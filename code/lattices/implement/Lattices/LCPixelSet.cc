//# LCMask.cc: Class to define a rectangular mask of interest
//# Copyright (C) 1997,1998
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

#include <trial/Lattices/LCMask.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> lcmask_gppbug1;


LCMask::LCMask()
{}

LCMask::LCMask (const Array<Bool>& mask, const LCBox& box)
: LCRegionFixed (box.latticeShape()),
  itsBox        (box)
{
    if (! mask.shape().isEqual (itsBox.shape())) {
	throw (AipsError ("LCMask::LCMask - "
			  "shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    setMask (mask);
}

LCMask::LCMask (const LCMask& that)
: LCRegionFixed (that),
  itsBox        (that.itsBox)
{}

LCMask::~LCMask()
{}

LCMask& LCMask::operator= (const LCMask& that)
{
    if (this != &that) {
	LCRegionFixed::operator= (that);
	itsBox = that.itsBox;
    }
    return *this;
}

Bool LCMask::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionFixed::operator== (other)) {
	return False;
    }
    const LCMask& that = (const LCMask&)other;
    // Check the box and mask.
    return ToBool (itsBox == that.itsBox  &&  masksEqual (that));
}
 

LCRegion* LCMask::cloneRegion() const
{
    return new LCMask(*this);
}

LCRegion* LCMask::doTranslate (const Vector<Float>& translateVector,
			       const IPosition& newLatticeShape) const
{
    LCBox* boxPtr = (LCBox*)(itsBox.translate (translateVector,
					       newLatticeShape));
    LCMask* regPtr = new LCMask (maskArray(), *boxPtr);
    delete boxPtr;
    return regPtr;
}

String LCMask::className()
{
    return "LCMask";
}

String LCMask::type() const
{
    return className();
}

TableRecord LCMask::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.define ("mask", maskArray());
    rec.defineRecord ("box", itsBox.toRecord (tableName));
    return rec;
}

LCMask* LCMask::fromRecord (const TableRecord& rec, const String& tableName)
{
    LCBox* boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"),
						   tableName));
    LCMask* regPtr = new LCMask (rec.asArrayBool ("mask"), *boxPtr);
    delete boxPtr;
    return regPtr;
}
