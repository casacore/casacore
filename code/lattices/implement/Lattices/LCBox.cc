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
    // Fill the blc and trc vectors.
    fillBlcTrc();
}

// Construct from the IPosition's defining the bottom-left and
// top-right corner of the box.
LCBox::LCBox (const IPosition& blc, const IPosition& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape)
{
    setSlicerBox (blc, trc);
    fillBlcTrc();
}

LCBox::LCBox (const Vector<Float>& blc, const Vector<Float>& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsBlc        (blc.copy()),
  itsTrc        (trc.copy())
{
    uInt i;
    IPosition bl(blc.nelements());
    for (i=0; i<blc.nelements(); i++) {
	bl(i) = Int(blc(i) + 0.5);
    }
    IPosition tr(trc.nelements());
    for (i=0; i<trc.nelements(); i++) {
	tr(i) = Int(trc(i) + 0.5);
    }
    setSlicerBox (bl, tr);
}

LCBox::LCBox (const Vector<Double>& blc, const Vector<Double>& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsBlc        (blc.nelements()),
  itsTrc        (trc.nelements())
{
    uInt i;
    IPosition bl(blc.nelements());
    for (i=0; i<blc.nelements(); i++) {
	itsBlc(i) = blc(i);
	bl(i) = Int(blc(i) + 0.5);
    }
    IPosition tr(trc.nelements());
    for (i=0; i<trc.nelements(); i++) {
	itsTrc(i) = trc(i);
	tr(i) = Int(trc(i) + 0.5);
    }
    setSlicerBox (bl, tr);
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
    Vector<Float> blc (itsBlc.copy());
    Vector<Float> trc (itsTrc.copy());
    for (uInt i=0; i<ndim; i++) {
        blc(i) += translateVector(i);
        trc(i) += translateVector(i);
    }
    return new LCBox (blc, trc, newLatticeShape);
}

String LCBox::className()
{
    return "LCBox";
}

TableRecord LCBox::toRecord (const String&) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.define ("blc", itsBlc);
    rec.define ("trc", itsTrc);
    rec.define ("shape", latticeShape().asVector());
    return rec;
}

LCBox* LCBox::fromRecord (const TableRecord& rec, const String&)
{
    return new LCBox (Vector<Float>(rec.asArrayFloat ("blc")),
		      Vector<Float>(rec.asArrayFloat ("trc")),
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

void LCBox::fillBlcTrc()
{
    const Slicer& sl = box();
    uInt nd = sl.ndim();
    itsBlc.resize (nd);
    itsTrc.resize (nd);
    for (uInt i=0; i<nd; i++) {
	itsBlc(i) = sl.start()(i);
	itsTrc(i) = sl.end()(i);
    }
}
