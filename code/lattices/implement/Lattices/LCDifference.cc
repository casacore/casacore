//# LCDifference.cc: Make the difference of 2 region
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


#include <trial/Lattices/LCDifference.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


LCDifference::LCDifference()
{}

LCDifference::LCDifference (const LCRegion& region1, const LCRegion& region2)
: LCRegionMulti (False, &region1, &region2)
{
    defineBox();
}

LCDifference::LCDifference (Bool takeOver,
			    const PtrBlock<const LCRegion*>& regions)
: LCRegionMulti (takeOver, regions)
{
    defineBox();
}

LCDifference::LCDifference (const LCDifference& other)
: LCRegionMulti (other)
{}

LCDifference::~LCDifference()
{}

LCDifference& LCDifference::operator= (const LCDifference& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
    }
    return *this;
}

LCRegion* LCDifference::cloneRegion() const
{
    return new LCDifference (*this);
}

LCRegion* LCDifference::doTranslate (const Vector<Float>& translateVector,
				     const IPosition& newLatticeShape) const
{
    PtrBlock<const LCRegion*> regions;
    multiTranslate (regions, translateVector, newLatticeShape);
    return new LCDifference (True, regions);
}

String LCDifference::className()
{
    return "LCDifference";
}

TableRecord LCDifference::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

LCDifference* LCDifference::fromRecord (const TableRecord& rec,
					const String& tableName)
{
    PtrBlock<const LCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new LCDifference (True, regions);
}

void LCDifference::defineBox()
{
    // The bounding box is the bounding box of the first lattice.
    setBox (regions()[0]->box());
}


void LCDifference::multiGetSlice (Array<Bool>& buffer,
				  const Slicer& section)
{
    // Get the required part from region1.
    // Note this getSlice version ensures that the result is not
    // referencing some internal Lattice array.
    Array<Bool> tmp = regions()[0]->getSlice(section);
    buffer.reference(tmp);
    // Determine which part to get from region2.
    // Get and store negation in buffer when anything found.
    const IPosition& shape = buffer.shape();
    uInt nrdim = shape.nelements();
    IPosition stbuf(nrdim);
    IPosition endbuf(nrdim);
    IPosition streg(nrdim);
    IPosition endreg(nrdim);
    if (findAreas (stbuf, endbuf, streg, endreg, section, 1)) {
        Array<Bool> tmpbuf;
	LCRegion* reg = (LCRegion*)(regions()[1]);
	reg->doGetSlice (tmpbuf, Slicer(streg, endreg, Slicer::endIsLast));
	Array<Bool> bufreg = buffer(stbuf,endbuf);
	DebugAssert (bufreg.shape() == tmpbuf.shape(), AipsError);
	// Make pixel in buffer False when tmpbuf has a True pixel.
	Bool deleteBuf, deleteTmp;
	Bool* buf = bufreg.getStorage (deleteBuf);
	Bool* bufptr = buf;
	Bool* bufend = buf + bufreg.nelements();
	const Bool* tmp = tmpbuf.getStorage (deleteTmp);
	const Bool* tmpptr = tmp;
	while (bufptr < bufend) {
	    if (*tmpptr++) {
	        *bufptr = False;
	    }
	    bufptr++;
	}
	bufreg.putStorage (buf, deleteBuf);
	tmpbuf.freeStorage (tmp, deleteTmp);
    }
}
