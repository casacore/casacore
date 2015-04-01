//# LCUnion.cc: Make the union of 2 or more regions
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


#include <casacore/lattices/LRegions/LCUnion.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCUnion::LCUnion()
{}

LCUnion::LCUnion (const LCRegion& region1,
                  const LCRegion& region2)
: LCRegionMulti (region1, region2)
{
    defineBox();
}

LCUnion::LCUnion (Bool takeOver, const LCRegion* region1,
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

LCUnion::LCUnion (Bool takeOver,
		  const PtrBlock<const LCRegion*>& regions)
: LCRegionMulti (takeOver, regions)
{
    defineBox();
}

LCUnion::LCUnion (const LCUnion& other)
: LCRegionMulti (other)
{}

LCUnion::~LCUnion()
{}

LCUnion& LCUnion::operator= (const LCUnion& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
    }
    return *this;
}

Bool LCUnion::operator== (const LCRegion& other) const
{
   return LCRegionMulti::operator== (other);
}

LCRegion* LCUnion::cloneRegion() const
{
    return new LCUnion (*this);
}

LCRegion* LCUnion::doTranslate (const Vector<Float>& translateVector,
				const IPosition& newLatticeShape) const
{
    PtrBlock<const LCRegion*> regions;
    multiTranslate (regions, translateVector, newLatticeShape);
    return new LCUnion (True, regions);
}

String LCUnion::className()
{
    return "LCUnion";
}

String LCUnion::type() const
{
   return className();
}

TableRecord LCUnion::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

LCUnion* LCUnion::fromRecord (const TableRecord& rec,
			      const String& tableName)
{
    PtrBlock<const LCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new LCUnion (True, regions);
}

void LCUnion::defineBox()
{
    uInt i;
    // Get the union of blc and trc.
    const IPosition& shape = latticeShape();
    uInt nrdim = shape.nelements();
    IPosition blc (regions()[0]->boundingBox().start());
    IPosition trc (regions()[0]->boundingBox().end());
    uInt nr = regions().nelements();
    for (i=1; i<nr; i++) {
	const IPosition& regblc = regions()[i]->boundingBox().start();
	const IPosition& regtrc = regions()[i]->boundingBox().end();
	for (uInt j=0; j<nrdim; j++) {
	    if (regblc(j) < blc(j)) {
		blc(j) = regblc(j);
	    }
	    if (regtrc(j) > trc(j)) {
		trc(j) = regtrc(j);
	    }
	}
    }
    // Set the bounding box in the parent object.
    setBoundingBox (Slicer(blc, trc, Slicer::endIsLast));
}


void LCUnion::multiGetSlice (Array<Bool>& buffer,
			     const Slicer& section)
{
    buffer.resize (section.length());
    uInt nrdim = buffer.ndim();
    buffer = False;
    IPosition stbuf(nrdim);
    IPosition endbuf(nrdim);
    IPosition streg(nrdim);
    IPosition endreg(nrdim);
    const IPosition& inc = section.stride();
    uInt nr = regions().nelements();
    for (uInt i=0; i<nr; i++) {
        if (findAreas (stbuf, endbuf, streg, endreg, section, i)) {
	    Array<Bool> tmpbuf;
	    LCRegion* reg = (LCRegion*)(regions()[i]);
	    reg->doGetSlice (tmpbuf, Slicer(streg, endreg, inc,
					    Slicer::endIsLast));
	    Array<Bool> bufreg = buffer(stbuf,endbuf);
	    DebugAssert (bufreg.shape() == tmpbuf.shape(), AipsError);
	    // Make pixel in buffer True when tmpbuf has a True pixel.
	    Bool deleteBuf, deleteTmp;
	    Bool* buf = bufreg.getStorage (deleteBuf);
	    Bool* bufptr = buf;
	    Bool* bufend = buf + bufreg.nelements();
	    const Bool* tmp = tmpbuf.getStorage (deleteTmp);
	    const Bool* tmpptr = tmp;
	    while (bufptr < bufend) {
	      if (*tmpptr++) {
	        *bufptr = True;
	      }
	      bufptr++;
	    }
	    bufreg.putStorage (buf, deleteBuf);
	    tmpbuf.freeStorage (tmp, deleteTmp);
	}
    }
}

} //# NAMESPACE CASACORE - END

