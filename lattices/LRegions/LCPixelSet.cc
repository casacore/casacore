//# LCPixelSet.cc: Class to define a rectangular mask of interest
//# Copyright (C) 1998,1999,2001
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

#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCPixelSet::LCPixelSet()
{}

LCPixelSet::LCPixelSet (const Array<Bool>& mask, const LCBox& box)
: LCRegionFixed (box.latticeShape()),
  itsBox        (box)
{
    if (! mask.shape().isEqual (itsBox.shape())) {
	throw (AipsError ("LCPixelSet::LCPixelSet - "
			  "shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    setMask (mask);
}

LCPixelSet::LCPixelSet (const LCPixelSet& that)
: LCRegionFixed (that),
  itsBox        (that.itsBox)
{}

LCPixelSet::~LCPixelSet()
{}

LCPixelSet& LCPixelSet::operator= (const LCPixelSet& that)
{
    if (this != &that) {
	LCRegionFixed::operator= (that);
	itsBox = that.itsBox;
    }
    return *this;
}

Bool LCPixelSet::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionFixed::operator== (other)) {
	return False;
    }
    const LCPixelSet& that = (const LCPixelSet&)other;
    // Check the box and mask.
    return  (itsBox == that.itsBox  &&  masksEqual (that));
}
 

LCRegion* LCPixelSet::cloneRegion() const
{
    return new LCPixelSet(*this);
}

LCRegion* LCPixelSet::doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const
{
    LCBox* boxPtr = (LCBox*)(itsBox.translate (translateVector,
					       newLatticeShape));
    LCPixelSet* regPtr = new LCPixelSet (maskArray(), *boxPtr);
    delete boxPtr;
    return regPtr;
}

String LCPixelSet::className()
{
    return "LCPixelSet";
}

String LCPixelSet::type() const
{
    return className();
}

TableRecord LCPixelSet::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.define ("mask", maskArray());
    rec.defineRecord ("box", itsBox.toRecord (tableName));
    return rec;
}

LCPixelSet* LCPixelSet::fromRecord (const TableRecord& rec,
				    const String& tableName)
{
    LCBox* boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"),
						   tableName));
    LCPixelSet* regPtr = new LCPixelSet (rec.toArrayBool ("mask"), *boxPtr);
    delete boxPtr;
    return regPtr;
}

} //# NAMESPACE CASACORE - END

