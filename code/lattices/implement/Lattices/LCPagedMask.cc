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
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> lcpagedmask_gppbug1;


LCPagedMask::LCPagedMask()
{}

LCPagedMask::LCPagedMask (const TiledShape& latticeShape,
			  const String& tableName)
: LCBox (IPosition(latticeShape.shape().nelements(), 0),
	 latticeShape.shape()-1, latticeShape.shape())
{
    itsMask = PagedArray<Bool> (latticeShape, tableName);
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (const TiledShape& maskShape,
			  const String& tableName,
			  const IPosition& blc,
			  const IPosition& latticeShape)
: LCBox (blc, blc+maskShape.shape()-1, latticeShape)
{
    itsMask = PagedArray<Bool> (maskShape, tableName);
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (PagedArray<Bool>& mask,
			  const IPosition& blc,
			  const IPosition& latticeShape)
: LCBox (blc, blc+mask.shape()-1, latticeShape),
  itsMask (mask)
{
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (const LCPagedMask& that)
: LCBox (that)
{}

LCPagedMask::~LCPagedMask()
{}

LCPagedMask& LCPagedMask::operator= (const LCPagedMask& that)
{
    if (this != &that) {
	LCBox::operator= (that);
    }
    return *this;
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

TableRecord LCPagedMask::toRecord (const String&) const
{
    TableRecord rec;
    rec.define ("name", className());
    rec.define ("blc", box().start().asVector());
    rec.defineTable ("mask", Table(itsMask.tableName()));
    rec.define ("shape", latticeShape().asVector());
    return rec;
}

LCPagedMask* LCPagedMask::fromRecord (const TableRecord& rec,
				      const String&)
{
    Table table (rec.asTable ("mask"));
    PagedArray<Bool> mask(table);
    return new LCPagedMask (mask,
			    Vector<Int>(rec.asArrayInt ("blc")),
			    Vector<Int>(rec.asArrayInt ("shape")));
}

Bool LCPagedMask::isWritable() const
{
    return itsMask.isWritable();
}
