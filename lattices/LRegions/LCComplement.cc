//# LCComplement.cc: Make the complement of a region
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


#include <casacore/lattices/LRegions/LCComplement.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCComplement::LCComplement()
{}

LCComplement::LCComplement (const LCRegion& region)
: LCRegionMulti (False, &region)
{
    defineBox();
}

LCComplement::LCComplement (Bool takeOver,
			    const PtrBlock<const LCRegion*>& regions)
: LCRegionMulti (takeOver, regions)
{
    defineBox();
}

LCComplement::LCComplement (const LCComplement& other)
: LCRegionMulti (other)
{}

LCComplement::~LCComplement()
{}

LCComplement& LCComplement::operator= (const LCComplement& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
    }
    return *this;
}

Bool LCComplement::operator== (const LCRegion& other) const
{
    return LCRegionMulti::operator== (other);
}


LCRegion* LCComplement::cloneRegion() const
{
    return new LCComplement (*this);
}

LCRegion* LCComplement::doTranslate (const Vector<Float>& translateVector,
				     const IPosition& newLatticeShape) const
{
    PtrBlock<const LCRegion*> regions;
    multiTranslate (regions, translateVector, newLatticeShape);
    return new LCComplement (True, regions);
}

String LCComplement::className()
{
    return "LCComplement";
}

String LCComplement::type() const
{   
   return className();
}
 

TableRecord LCComplement::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

LCComplement* LCComplement::fromRecord (const TableRecord& rec,
					const String& tableName)
{
    PtrBlock<const LCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new LCComplement (True, regions);
}

void LCComplement::defineBox()
{
    const IPosition& shape = latticeShape();
    // The bounding box is the full lattice.
    setBoundingBox (Slicer(IPosition(shape.nelements(),0), shape));
}


void LCComplement::multiGetSlice (Array<Bool>& buffer,
				  const Slicer& section)
{
    buffer.resize (section.length());
    // Initialize to all true.
    buffer = True;
    // Determine which part to get from the region (which is region 0).
    // Get and store negation in buffer when anything found.
    const IPosition& shape = buffer.shape();
    uInt nrdim = shape.nelements();
    IPosition stbuf(nrdim);
    IPosition endbuf(nrdim);
    IPosition streg(nrdim);
    IPosition endreg(nrdim);
    const IPosition& inc = section.stride();
    if (findAreas (stbuf, endbuf, streg, endreg, section, 0)) {
        Array<Bool> tmpbuf;
	((LCRegion*)(regions()[0]))->doGetSlice
                       (tmpbuf, Slicer(streg, endreg, inc, Slicer::endIsLast));
	buffer(stbuf,endbuf) = !tmpbuf;
    }
}

} //# NAMESPACE CASACORE - END

