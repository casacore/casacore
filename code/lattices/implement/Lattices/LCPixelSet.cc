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

LCMask::LCMask (const IPosition& blc, const Array<Bool>& mask,
		const IPosition& latticeShape)
: LCBox (blc, blc+mask.shape()-1, latticeShape)
{
    // When the mask exceeded the lattice, the bounding box has been adapted.
    // If so, take only the appropriate part of the mask.
    if (box().length() == mask.shape()) {
        setMask (mask);
    } else {
        Array<Bool> refmask(mask);
        IPosition bl (box().start() - blc);
	setMask (refmask(bl, bl+box().length()-1));
    }
}

LCMask::LCMask (const LCMask& that)
: LCBox (that)
{}

LCMask::~LCMask()
{}

LCMask& LCMask::operator= (const LCMask& that)
{
    if (this != &that) {
	LCBox::operator= (that);
    }
    return *this;
}

LCRegion* LCMask::cloneRegion() const
{
    return new LCMask(*this);
}

LCRegion* LCMask::doTranslate (const Vector<Float>& translateVector,
			       const IPosition& newLatticeShape) const
{
    uInt ndim = latticeShape().nelements();
    if (translateVector.nelements() != ndim
    ||  newLatticeShape.nelements() != ndim) {
        throw (AipsError ("LCMask::translate - dimensionalities mismatch"));
    }
    IPosition blc = box().start();
    for (uInt i=0; i<ndim; i++) {
        blc(i) += Int(translateVector(i));
    }
    return new LCMask (blc, maskArray(), newLatticeShape);
}

String LCMask::className()
{
    return "LCMask";
}

TableRecord LCMask::toRecord (const String&) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.define ("blc", box().start().asVector());
    rec.define ("mask", maskArray());
    rec.define ("shape", latticeShape().asVector());
    return rec;
}

LCMask* LCMask::fromRecord (const TableRecord& rec, const String&)
{
    return new LCMask (Vector<Int>(rec.asArrayInt ("blc")),
		       rec.asArrayBool ("mask"),
		       Vector<Int>(rec.asArrayInt ("shape")));
}

Bool LCMask::isWritable() const
{
    return True;
}
