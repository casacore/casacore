//# LELLattCoord.cc: The base letter class for lattice coordinates
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


#include <trial/Lattices/LELLattCoord.h>
#include <trial/Lattices/LattRegionHolder.h>
#include <trial/Lattices/LatticeRegion.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Exceptions/Error.h>


LELLattCoord::LELLattCoord()
{}

LELLattCoord::~LELLattCoord()
{}

Bool LELLattCoord::hasCoordinates() const
{
    return False;
}

String LELLattCoord::classname() const
{
    return "LELLattCoord";
}

Bool LELLattCoord::conform (const LELLattCoordBase&) const
{
    return True;
}

Bool LELLattCoord::doConform (const LELImageCoord&) const
{
    return True;
}

LatticeExprNode LELLattCoord::makeSubLattice
                                    (const LatticeExprNode& expr,
				     const LattRegionHolder& region) const
{
    LatticeRegion latReg (region.toLatticeRegion (expr.shape()));
    switch (expr.dataType()) {
    case TpBool:
        return SubLattice<Bool> (LatticeExpr<Bool>(expr), latReg);
    case TpFloat:
        return SubLattice<Float> (LatticeExpr<Float>(expr), latReg);
    case TpDouble:
        return SubLattice<Double> (LatticeExpr<Double>(expr), latReg);
    case TpComplex:
        return SubLattice<Complex> (LatticeExpr<Complex>(expr), latReg);
    case TpDComplex:
        return SubLattice<DComplex> (LatticeExpr<DComplex>(expr), latReg);
    default:
        throw (AipsError ("LELLattCoord::makeSubLattice - unknown datatype"));
    }
    return LatticeExprNode();
}
