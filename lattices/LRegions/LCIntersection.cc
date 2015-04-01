//# LCIntersection.cc: Make the intersection of 2 or more regions
//# Copyright (C) 1998
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


#include <casacore/lattices/LRegions/LCIntersection.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCIntersection::LCIntersection()
{}

LCIntersection::LCIntersection (const LCRegion& region1,
				const LCRegion& region2)
: LCRegionMulti (region1, region2)
{
    defineBox();
}

LCIntersection::LCIntersection (Bool takeOver, const LCRegion* region1,
				const LCRegion* region2,
				const LCRegion* region3,
				const LCRegion* region4,
				const LCRegion* region5,
				const LCRegion* region6,
				const LCRegion* region7,
				const LCRegion* region8,
				const LCRegion* region9,
				const LCRegion* region10)
: LCRegionMulti (takeOver, region1, region2, region3, region4, region5,
		 region6, region7, region8, region9, region10)
{
    defineBox();
}

LCIntersection::LCIntersection (Bool takeOver,
				const PtrBlock<const LCRegion*>& regions)
: LCRegionMulti (takeOver, regions)
{
    defineBox();
}

LCIntersection::LCIntersection (const LCIntersection& other)
: LCRegionMulti (other),
  itsOffsets    (other.itsOffsets)
{}

LCIntersection::~LCIntersection()
{}

LCIntersection& LCIntersection::operator= (const LCIntersection& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
	itsOffsets = other.itsOffsets;
    }
    return *this;
}

Bool LCIntersection::operator== (const LCRegion& other) const
{
    return LCRegionMulti::operator== (other);
}
 
    
LCRegion* LCIntersection::cloneRegion() const
{
    return new LCIntersection (*this);
}

LCRegion* LCIntersection::doTranslate (const Vector<Float>& translateVector,
				       const IPosition& newLatticeShape) const
{
    PtrBlock<const LCRegion*> regions;
    multiTranslate (regions, translateVector, newLatticeShape);
    return new LCIntersection (True, regions);
}

String LCIntersection::className()
{
    return "LCIntersection";
}

String LCIntersection::type() const
{
   return className();
}

TableRecord LCIntersection::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

LCIntersection* LCIntersection::fromRecord (const TableRecord& rec,
					    const String& tableName)
{
    PtrBlock<const LCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new LCIntersection (True, regions);
}

void LCIntersection::defineBox()
{
    uInt i;
    // Get the intersection of blc and trc.
    const IPosition& shape = latticeShape();
    uInt nrdim = shape.nelements();
    IPosition blc(nrdim, 0);
    IPosition trc(shape - 1);
    uInt nr = regions().nelements();
    itsOffsets.resize (nr, True);
    for (i=0; i<nr; i++) {
	const IPosition& regblc = regions()[i]->boundingBox().start();
	const IPosition& regtrc = regions()[i]->boundingBox().end();
	for (uInt j=0; j<nrdim; j++) {
	    if (regtrc(j) < blc(j)  ||  regblc(j) > trc(j)) {
	        throw (AipsError ("LCIntersection::LCIntersection - "
				  "regions do not overlap"));
	    }
	    if (regblc(j) > blc(j)) {
	        blc(j) = regblc(j);
	    }
	    if (regtrc(j) < trc(j)) {
	        trc(j) = regtrc(j);
	    }
	}
    }
    // Set the bounding box in the parent object.
    setBoundingBox (Slicer(blc, trc, Slicer::endIsLast));
    // Now determine where bounding box starts in constituting regions.
    itsOffsets.resize (nr);
    for (i=0; i<nr; i++) {
        itsOffsets[i] = blc - regions()[i]->boundingBox().start();
    }
    // Fill the hasMask switch.
    fillHasMask();
}


void LCIntersection::multiGetSlice (Array<Bool>& buffer,
				    const Slicer& section)
{
    // Get the required part from the first region.
    // Note this getSlice version ensures that the result is not
    // referencing some internal Lattice array.
    Array<Bool> tmp = regions()[0]->getSlice
                              (Slicer(section.start()+itsOffsets[0],
				      section.length(),
				      section.stride()));
    buffer.reference(tmp);
    Bool deleteBuf, deleteTmp;
    Bool* buf = buffer.getStorage (deleteBuf);
    Bool* bufend = buf + section.length().product();
    Array<Bool> tmpbuf (buffer.shape());
    uInt nr = regions().nelements();
    for (uInt i=1; i<nr; i++) {
        LCRegion* reg = (LCRegion*)(regions()[i]);
        reg->doGetSlice (tmpbuf, Slicer(section.start()+itsOffsets[i],
					section.length(),
					section.stride()));
        const Bool* tmp = tmpbuf.getStorage (deleteTmp);
	const Bool* tmpptr = tmp;
	Bool* bufptr = buf;
	// Take the 'and' of all elements.
	while (bufptr < bufend) {
	    if (*bufptr) {
	        *bufptr = *tmpptr;
	    }
	    bufptr++;
	    tmpptr++;
	}
	tmpbuf.freeStorage (tmp, deleteTmp);
    }
    buffer.putStorage (buf, deleteBuf);
}

} //# NAMESPACE CASACORE - END

