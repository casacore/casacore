//# LCBox.cc: Class to define a rectangular box of interest
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

#include <trial/Lattices/LCBox.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> lcbox_gppbug1;


LCBox::LCBox()
{}

LCBox::LCBox (const Slicer& box, const IPosition& latticeShape)
: LCRegionFixed (latticeShape)
{
    // Make sure no stride is given.
    AlwaysAssert (box.stride()==1, AipsError);
    // When the slicer is fixed (i.e. blc and trc explicitly given),
    // it is possible that it partly exceeds the lattice boundaries.
    if (box.isFixed()) {
        setSlicerBox (box.start(), box.end());
    } else {
      setBox (box);
    }
}

// Construct from the IPosition's defining the bottom-left and
// top-right corner of the box.
LCBox::LCBox (const IPosition& blc, const IPosition& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape)
{
    setSlicerBox (blc, trc);
}

LCBox::LCBox (const LCBox& that)
: LCRegionFixed (that)
{}

LCBox::~LCBox()
{}

LCBox& LCBox::operator= (const LCBox& that)
{
    if (this != &that) {
	LCRegionFixed::operator= (that);
    }
    return *this;
}

LCRegion* LCBox::cloneRegion() const
{
    return new LCBox(*this);
}

LCRegion* LCBox::doTranslate (const Vector<Float>& translateVector,
			      const IPosition& newLatticeShape) const
{
    uInt ndim = latticeShape().nelements();
    if (translateVector.nelements() != ndim
    ||  newLatticeShape.nelements() != ndim) {
        throw (AipsError ("LCBox::translate - dimensionalities mismatch"));
    }
    IPosition blc = box().start();
    for (uInt i=0; i<ndim; i++) {
        blc(i) += Int(translateVector(i));
    }
    return new LCBox (Slicer(blc, box().length()), newLatticeShape);
}

String LCBox::className()
{
    return "LCBox";
}

TableRecord LCBox::toRecord (const String&) const
{
    TableRecord rec;
    rec.define ("name", className());
    rec.define ("blc", box().start().asVector());
    rec.define ("trc", box().end().asVector());
    rec.define ("shape", latticeShape().asVector());
    return rec;
}

LCBox* LCBox::fromRecord (const TableRecord& rec, const String&)
{
    return new LCBox (Slicer (Vector<Int>(rec.asArrayInt ("blc")),
			      Vector<Int>(rec.asArrayInt ("trc")),
			      Slicer::endIsLast),
		      Vector<Int>(rec.asArrayInt ("shape")));
}

void LCBox::setSlicerBox (const IPosition& blc, const IPosition& trc)
{
    const IPosition& shape = latticeShape();
    uInt ndim = shape.nelements();
    AlwaysAssert (blc.nelements() == ndim  &&  trc.nelements() == ndim,
		  AipsError);
    IPosition bl(blc);
    IPosition tr(trc);
    for (uInt i=0; i<ndim; i++) {
        if (bl(i) < 0) {
	    bl(i) = 0;
	}
	if (tr(i) >= shape(i)) {
	    tr(i) = shape(i) - 1;
	}
	AlwaysAssert (blc(i) <= trc(i), AipsError);
    }
    setBox (Slicer(bl, tr, Slicer::endIsLast));
}
