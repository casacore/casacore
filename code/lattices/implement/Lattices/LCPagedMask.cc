//# LCPagedMask.cc: Class to define a rectangular mask of interest
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

#include <trial/Lattices/LCPagedMask.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> lcpagedmask_gppbug1;


LCPagedMask::LCPagedMask()
{}

LCPagedMask::LCPagedMask (const TiledShape& latticShape,
			  const String& tableName)
: LCRegionSingle (latticShape.shape()),
  itsBox (IPosition(latticShape.shape().nelements(), 0),
	  latticShape.shape()-1, latticShape.shape())
{
    setBoundingBox (itsBox.boundingBox());
    itsMask = PagedArray<Bool> (latticShape, tableName);
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (const TiledShape& maskShape,
			  const LCBox& box,
			  const String& tableName)
: LCRegionSingle (box.latticeShape()),
  itsBox (box)
{
    // Check if box shape and mask shape are equal.
    if (itsBox.shape() != maskShape.shape()) {
	throw (AipsError ("LCPagedMask::LCPagedMask- "
			  "shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    itsMask = PagedArray<Bool> (maskShape, tableName);
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (PagedArray<Bool>& mask,
			  const LCBox& box)
: LCRegionSingle (box.latticeShape()),
  itsBox (box)
{
    // Check if box shape and mask shape are equal.
    if (itsBox.shape() != mask.shape()) {
	throw (AipsError ("LCPagedMask::LCPagedMask- "
			  "shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    itsMask = mask;
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (const LCPagedMask& other)
: LCRegionSingle (other),
  itsBox (other.itsBox),
  itsMask(other.itsMask)
{
    setMaskPtr (itsMask);
}

LCPagedMask::~LCPagedMask()
{}

LCPagedMask& LCPagedMask::operator= (const LCPagedMask& that)
{
    if (this != &that) {
	LCRegionSingle::operator= (that);
	itsBox  = that.itsBox;
	itsMask = that.itsMask;
	setMaskPtr (itsMask);
    }
    return *this;
}

Bool LCPagedMask::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionSingle::operator== (other)) {
	return False;
    }
    const LCPagedMask& that = (const LCPagedMask&)other;
    // Check the box and mask.
    return ToBool (itsBox == that.itsBox  &&  masksEqual (that));
}


LCRegion* LCPagedMask::cloneRegion() const
{
    return new LCPagedMask(*this);
}

LCRegion* LCPagedMask::doTranslate (const Vector<Float>&,
				    const IPosition&) const
{
    // A PagedLCMask cannot be translated.
    throw (AipsError ("LCPagedMask::translate is not supported"));
    return 0;
}

String LCPagedMask::className()
{
    return "LCPagedMask";
}

String LCPagedMask::type() const
{
   return className();
}

TableRecord LCPagedMask::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineTable ("mask", Table(itsMask.tableName()));
    rec.defineRecord ("box", itsBox.toRecord (tableName));
    return rec;
}

LCPagedMask* LCPagedMask::fromRecord (const TableRecord& rec,
				      const String& tableName)
{
    Table table (rec.asTable ("mask"));
    PagedArray<Bool> mask(table);
    LCBox* boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"),
						   tableName));
    LCPagedMask* regPtr = new LCPagedMask (mask, *boxPtr);
    delete boxPtr;
    return regPtr;
}

Bool LCPagedMask::isWritable() const
{
    return itsMask.isWritable();
}
