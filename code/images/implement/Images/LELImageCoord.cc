//# LELImageCoord.cc: The letter class for image coordinates
//# Copyright (C) 1998,1999,2000
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


#include <trial/Images/LELImageCoord.h>
#include <trial/Images/ImageExpr.h>
#include <trial/Images/SubImage.h>
#include <trial/Lattices/LattRegionHolder.h>
#include <trial/Lattices/LatticeRegion.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Exceptions/Error.h>


LELImageCoord::LELImageCoord()
{}

LELImageCoord::LELImageCoord (const CoordinateSystem& coordinates)
: coords_p (new CoordinateSystem(coordinates))
{}

LELImageCoord::~LELImageCoord()
{}

const CoordinateSystem& LELImageCoord::coordinates() const
{
    return *coords_p;
}

Bool LELImageCoord::hasCoordinates() const
{
    return True;
}

String LELImageCoord::classname() const
{
    return "LELImageCoord";
}

Bool LELImageCoord::conform (const LELLattCoordBase& other) const
{
// Call the virtual doConform function to be able to compare
// two LELImageCoord objects.

    return other.doConform (*this);
}

Bool LELImageCoord::doConform (const LELImageCoord& other) const
{
// This is the real conformance checker.

    return coordinates().near (other.coordinates());
}

LatticeExprNode LELImageCoord::makeSubLattice
                                    (const LatticeExprNode& expr,
				     const LattRegionHolder& region) const
{
    switch (expr.dataType()) {
/// case TpBool:
///     return SubImage<Bool> (ImageExpr<Bool>
///                        (LatticeExpr<Bool>(expr), ""), region);
    case TpFloat:
        return SubImage<Float> (ImageExpr<Float>
                           (LatticeExpr<Float>(expr), ""), region);
/// case TpDouble:
///     return SubImage<Double> (ImageExpr<Double>
///                        (LatticeExpr<Double>(expr, "")), region);
    case TpComplex:
        return SubImage<Complex> (ImageExpr<Complex>
                           (LatticeExpr<Complex>(expr), ""), region);
/// case TpDComplex:
///     return SubImage<DComplex> (ImageExpr<DComplex>
///                        (LatticeExpr<DComplex>(expr), ""), region);
    default:
        throw (AipsError ("LELImageCoord::makeSubLattice - unknown datatype"));
    }
    return LatticeExprNode();
}
