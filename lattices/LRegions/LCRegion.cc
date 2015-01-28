//# LCRegion.cc: Abstract base class to define a region of interest
//# Copyright (C) 1997,1998,1999,2000
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

#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCRegion::LCRegion()
{}

LCRegion::LCRegion (const IPosition& latticeShape)
: itsShape (latticeShape)
{}

LCRegion::LCRegion (const LCRegion& other)
: Lattice<Bool>(),
  itsShape       (other.itsShape),
  itsBoundingBox (other.itsBoundingBox),
  itsComment     (other.itsComment)
{}

LCRegion& LCRegion::operator= (const LCRegion& other)
{
    if (this != &other) {
	itsShape.resize (other.itsShape.nelements());
	itsShape       = other.itsShape;
	itsBoundingBox = other.itsBoundingBox;
	itsComment     = other.itsComment;
    }
    return *this;
}

Bool LCRegion::operator== (const LCRegion& other) const
{

    // Type check.
    if (type() != other.type()) {
	return False;
    }
    // Compare bounding boxes, which also takes care of dimensionality.
    if (! itsBoundingBox.length().isEqual (other.itsBoundingBox.length())
    ||  ! itsBoundingBox.start().isEqual (other.itsBoundingBox.start())) {
	return False;
    }
    return itsShape.isEqual (other.itsShape);
}


LCRegion::~LCRegion()
{}

Lattice<Bool>* LCRegion::clone() const
{
    return cloneRegion();
}

void LCRegion::handleDelete()
{}
void LCRegion::handleRename (const String&, Bool)
{}

LCRegion* LCRegion::translate (const IPosition& translateVector,
			       const IPosition& newLatticeShape) const
{
    uInt nr = translateVector.nelements();
    Vector<Float> vec (nr);
    for (uInt i=0; i<nr; i++) {
        vec(i) = translateVector(i);
    }
    return translate (vec, newLatticeShape);
}
LCRegion* LCRegion::translate (const Vector<Float>& translateVector,
			       const IPosition& newLatticeShape) const
{
    if (translateVector.nelements() != newLatticeShape.nelements()) {
	throw (AipsError ("LCRegion::translate - "
			  "translateVector and newLatticeShape vectors "
			  "do not have same length"));
    }
    if (newLatticeShape.nelements() < latticeShape().nelements()) {
	throw (AipsError ("LCRegion::translate - "
			  "length of newLatticeShape vector less than "
			  "dimensionality of region"));
    }
    return doTranslate (translateVector, newLatticeShape);
}

void LCRegion::setBoundingBox (const Slicer& box)
{
    IPosition blc, trc, inc;
    box.inferShapeFromSource (itsShape, blc, trc, inc);
    itsBoundingBox = Slicer (blc, trc, inc, Slicer::endIsLast);
}
void LCRegion::setShapeAndBoundingBox (const IPosition& latticeShape,
				       const Slicer& boundingBox)
{
    AlwaysAssert (latticeShape.nelements() == boundingBox.ndim(), AipsError);
    itsShape.resize (latticeShape.nelements());
    itsShape = latticeShape;
    setBoundingBox (boundingBox);
}

Slicer LCRegion::expand (const Slicer& slicer) const
{
    IPosition blc, trc, inc;
    IPosition shape = slicer.inferShapeFromSource (itsBoundingBox.length(),
						   blc, trc, inc);
    const IPosition& start = itsBoundingBox.start();
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
    const IPosition& start = itsBoundingBox.start();
    for (uInt i=0; i<ndim; i++) {
	DebugAssert (index(i) < itsBoundingBox.length()(i), AipsError);
	result(i) = start(i) + index(i);
    }
    return result;
}

void LCRegion::defineRecordFields (RecordInterface& record,
				   const String& className) const
{
    record.define ("isRegion", Int(RegionType::LC));
    record.define ("name", className);
    record.define ("comment", itsComment);
}


uInt LCRegion::ndim() const
{
    return itsShape.nelements();
}

IPosition LCRegion::shape() const
{
    return itsBoundingBox.length();
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

} //# NAMESPACE CASACORE - END

