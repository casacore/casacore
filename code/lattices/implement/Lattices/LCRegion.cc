//# LCRegion.cc: Abstract base class to define a region of interest
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

#include <trial/Lattices/LCRegion.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCEllipsoid.h>
#include <trial/Lattices/LCPolygon.h>
#include <trial/Lattices/LCMask.h>
#include <trial/Lattices/LCPagedMask.h>
#include <trial/Lattices/LCIntersection.h>
#include <trial/Lattices/LCUnion.h>
#include <trial/Lattices/LCComplement.h>
#include <trial/Lattices/LCDifference.h>
#include <trial/Lattices/LCExtension.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


LCRegion::LCRegion()
{}

LCRegion::LCRegion (const IPosition& latticeShape)
: itsShape (latticeShape)
{}

LCRegion::LCRegion (const LCRegion& that)
: itsShape (that.itsShape),
  itsBox   (that.itsBox)
{}

LCRegion& LCRegion::operator= (const LCRegion& that)
{
    if (this != &that) {
	itsShape.resize (that.itsShape.nelements());
	itsShape = that.itsShape;
	itsBox   = that.itsBox;
    }
    return *this;
}

LCRegion::~LCRegion()
{}

Lattice<Bool>* LCRegion::clone() const
{
    return cloneRegion();
}

LCRegion* LCRegion::translate (const IPosition& translateVector,
		     const IPosition& newLatticeShape) const
{
    uInt nr = translateVector.nelements();
    Vector<Float> vec (nr);
    for (uInt i=0; i<nr; i++) {
        vec(i) = translateVector(i);
    }
    return doTranslate (vec, newLatticeShape);
}

void LCRegion::setBox (const Slicer& box)
{
    IPosition blc, trc, inc;
    box.inferShapeFromSource (itsShape, blc, trc, inc);
    itsBox = Slicer (blc, trc, inc, Slicer::endIsLast);
}

Slicer LCRegion::expand (const Slicer& slicer) const
{
    IPosition blc, trc, inc;
    IPosition shape = slicer.inferShapeFromSource (itsBox.length(),
						   blc, trc, inc);
    const IPosition& start = itsBox.start();
    uInt ndim = itsShape.nelements();
    for (uInt i=0; i<ndim; i++) {
	blc(i) += start(i);
    }
    return Slicer(blc, shape, inc);
}
IPosition LCRegion::expand (const IPosition& index) const
{
    uInt ndim = itsShape.nelements();
    DebugAssert (index.nelements() == ndim, AipsError);
    IPosition result (ndim);
    const IPosition& start = itsBox.start();
    for (uInt i=0; i<ndim; i++) {
	DebugAssert (index(i) < itsBox.length()(i), AipsError);
	result(i) = start(i) + index(i);
    }
    return result;
}

LCRegion* LCRegion::fromRecord (const TableRecord& rec,
				const String& tableName)
{
    const String& name = rec.asString ("name");
    if (name == LCBox::className()) {
	return LCBox::fromRecord (rec, tableName);
    } else if (name == LCEllipsoid::className()) {
	return LCEllipsoid::fromRecord (rec, tableName);
    } else if (name == LCPolygon::className()) {
      	return LCPolygon::fromRecord (rec, tableName);
    } else if (name == LCMask::className()) {
      	return LCMask::fromRecord (rec, tableName);
    } else if (name == LCPagedMask::className()) {
      	return LCPagedMask::fromRecord (rec, tableName);
    } else if (name == LCIntersection::className()) {
      	return LCIntersection::fromRecord (rec, tableName);
    } else if (name == LCUnion::className()) {
      	return LCUnion::fromRecord (rec, tableName);
    } else if (name == LCComplement::className()) {
      	return LCComplement::fromRecord (rec, tableName);
    } else if (name == LCDifference::className()) {
      	return LCDifference::fromRecord (rec, tableName);
    } else if (name == LCExtension::className()) {
        return LCExtension::fromRecord (rec, tableName);
    } else {
	throw (AipsError ("LCRegion::fromRecord - " + name +
			  " is unknown derived LCRegion class"));
    }
    return 0;
}


IPosition LCRegion::shape() const
{
    return itsBox.length();
}

Bool LCRegion::isWritable() const
{
    return False;
}

void LCRegion::doPutSlice (const Array<Bool>&, const IPosition&,
			   const IPosition&)
{
    throw (AipsError ("LCRegion::putSlice is not possible"));
}
void LCRegion::set (const Bool&)
{
    throw (AipsError ("LCRegion: set is not possible"));
}
void LCRegion::apply (Bool (*)(Bool))
{
    throw (AipsError ("LCRegion: apply is not possible"));
}
void LCRegion::apply (Bool (*)(const Bool&))
{
    throw (AipsError ("LCRegion: apply is not possible"));
}
void LCRegion::apply (const Functional<Bool,Bool>&)
{
    throw (AipsError ("LCRegion: apply is not possible"));
}
void LCRegion::putAt (const Bool&, const IPosition&)
{
    throw (AipsError ("LCRegion: putAt is not possible"));
}
void LCRegion::copyData (const Lattice<Bool>&)
{
    throw (AipsError ("LCRegion: copyData is not possible"));
}
