//# LELLattCoord.cc: The base letter class for lattice coordinates
//# Copyright (C) 1998,1999,2000,2001
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


#include <casacore/lattices/LEL/LELLattCoord.h>
#include <casacore/lattices/LRegions/LattRegionHolder.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/RebinLattice.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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

Int LELLattCoord::compare (const LELLattCoordBase&) const
{
  return 0;
}

Int LELLattCoord::doCompare (const LELImageCoord&) const
{
  return 0;
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

LatticeExprNode LELLattCoord::makeExtendLattice
                                    (const LatticeExprNode&,
				     const IPosition&,
				     const LELLattCoordBase&) const
{
  throw AipsError ("LELCoordinates::getSpectralInfo - "
		   "cannot extend lattice without coordinates");
  return LatticeExprNode();
}

LatticeExprNode LELLattCoord::makeRebinLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& binning) const
{
  switch (expr.dataType()) {
  case TpFloat:
    return RebinLattice<Float> (LatticeExpr<Float>(expr), binning);
  case TpDouble:
    return RebinLattice<Double> (LatticeExpr<Double>(expr), binning);
  case TpComplex:
    return RebinLattice<Complex> (LatticeExpr<Complex>(expr), binning);
  case TpDComplex:
    return RebinLattice<DComplex> (LatticeExpr<DComplex>(expr), binning);
  default:
    throw (AipsError ("LELLattCoord::makeRebinLattice - invalid datatype"));
  }
  return LatticeExprNode();
}

uInt LELLattCoord::getSpectralInfo (Vector<Double>&, const IPosition&) const
{
  throw AipsError ("LELCoordinates::getSpectralInfo - "
		   "no spectral coordinates available");
  return 0;
}

} //# NAMESPACE CASACORE - END

