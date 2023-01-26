//# LELImageCoord.cc: The letter class for image coordinates
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


#include <casacore/images/Images/LELImageCoord.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/ExtendImage.h>
#include <casacore/images/Images/RebinImage.h>
#include <casacore/lattices/LRegions/LattRegionHolder.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LELImageCoord::LELImageCoord()
{}

LELImageCoord::LELImageCoord (const CoordinateSystem& coordinates,
			      const ImageInfo& imageInfo,
			      const Unit& unit,
			      const RecordInterface& miscInfo)
: coords_p    (new CoordinateSystem(coordinates)),
  imageInfo_p (imageInfo),
  unit_p      (unit),
  miscInfo_p  (miscInfo)
{}

LELImageCoord::~LELImageCoord()
{}

bool LELImageCoord::hasCoordinates() const
{
  return true;
}

String LELImageCoord::classname() const
{
  return "LELImageCoord";
}

uint32_t LELImageCoord::getSpectralInfo (Vector<double>& worldCoordinates,
				     const IPosition& shape) const
{
  // Find the coordinate number of the spectral coordinate.
  const CoordinateSystem& csys = coordinates();
  int32_t which = csys.findCoordinate (Coordinate::SPECTRAL);
  if (which < 0) {
    throw AipsError ("LatticeExpr - no spectral coordinate found");
  }
  // Get the pixel axis of the spectral coordinate.
  Vector<int32_t> pixelAxes = csys.pixelAxes (which);
  AlwaysAssert (pixelAxes.nelements() == 1, AipsError);
  if (pixelAxes(0) < 0  ||  pixelAxes(0) >= int32_t(shape.nelements())) {
    // No pixel axis, so there is a replacement value for this axis.
    // We can only get that by converting a pixel position to world.
    Vector<double> worlds;
    AlwaysAssert (csys.toWorld (worlds, IPosition(shape.nelements(), 0)),
		  AipsError);
    Vector<int32_t> worldAxes = csys.worldAxes (which);
    AlwaysAssert (worldAxes.nelements() == 1, AipsError);
    worldCoordinates.resize (1);
    worldCoordinates(0) = worlds(worldAxes(0));
  } else {
    // Get the world values for the entire spectral axis.
    uint32_t length = shape(pixelAxes(0));
    const SpectralCoordinate& crd = csys.spectralCoordinate (which);
    worldCoordinates.resize (length);
    for (uint32_t i=0; i<length; i++) {
      AlwaysAssert (crd.toWorld (worldCoordinates(i), double(i)), AipsError);
    }
  }
  return pixelAxes(0);
}

int32_t LELImageCoord::compare (const LELLattCoordBase& other) const
{
  // Call the virtual doCompare function to be able to compare
  // two LELImageCoord objects.
  return other.doCompare (*this);
}

int32_t LELImageCoord::doCompare (const LELImageCoord& other) const
{
  return CoordinateUtil::compareCoordinates (other.coordinates(),
					     coordinates());
}


LatticeExprNode LELImageCoord::makeSubLattice
                                    (const LatticeExprNode& expr,
				     const LattRegionHolder& region) const
{
  switch (expr.dataType()) {
  case TpFloat:
    return SubImage<float>
                 (ImageExpr<float> (LatticeExpr<float>(expr), ""),
		  region);
  case TpDouble:
    return SubImage<double>
                 (ImageExpr<double> (LatticeExpr<double>(expr), ""),
		  region);
  case TpComplex:
    return SubImage<Complex>
                 (ImageExpr<Complex> (LatticeExpr<Complex>(expr), ""),
		  region);
  case TpDComplex:
    return SubImage<DComplex>
                 (ImageExpr<DComplex> (LatticeExpr<DComplex>(expr), ""),
		  region);
  case TpBool:
    return SubImage<bool>
                 (ImageExpr<bool> (LatticeExpr<bool>(expr), ""),
		  region);
  default:
    throw (AipsError ("LELImageCoord::makeSubLattice - unknown datatype"));
  }
  return LatticeExprNode();
}


LatticeExprNode LELImageCoord::makeExtendLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& newShape,
				     const LELLattCoordBase& newCoord) const
{
  // Get new coordinate system.
  const LELImageCoord* cptr = dynamic_cast<const LELImageCoord*>(&newCoord);
  AlwaysAssert (cptr != 0, AipsError);
  const CoordinateSystem& newCsys = cptr->coordinates();
  switch (expr.dataType()) {
  case TpFloat:
    return ExtendImage<float>
                (ImageExpr<float>(LatticeExpr<float>(expr), ""),
		 newShape, newCsys);
  case TpDouble:
    return ExtendImage<double>
                (ImageExpr<double>(LatticeExpr<double>(expr), ""),
		 newShape, newCsys);
  case TpComplex:
    return ExtendImage<Complex>
                (ImageExpr<Complex>(LatticeExpr<Complex>(expr), ""),
		 newShape, newCsys);
  case TpDComplex:
    return ExtendImage<DComplex>
                (ImageExpr<DComplex>(LatticeExpr<DComplex>(expr), ""),
		 newShape, newCsys);
  case TpBool:
    return ExtendImage<bool>
                (ImageExpr<bool>(LatticeExpr<bool>(expr), ""),
		 newShape, newCsys);
  default:
    throw (AipsError ("LELImageCoord::makeExtendLattice - unknown datatype"));
  }
  return LatticeExprNode();
}

LatticeExprNode LELImageCoord::makeRebinLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& binning) const
{
  switch (expr.dataType()) {
  case TpFloat:
    return RebinImage<float>
                 (ImageExpr<float> (LatticeExpr<float>(expr), ""),
		  binning);
  case TpDouble:
    return RebinImage<double>
                 (ImageExpr<double> (LatticeExpr<double>(expr), ""),
		  binning);
  case TpComplex:
    return RebinImage<Complex>
                 (ImageExpr<Complex> (LatticeExpr<Complex>(expr), ""),
		  binning);
  case TpDComplex:
    return RebinImage<DComplex>
                 (ImageExpr<DComplex> (LatticeExpr<DComplex>(expr), ""),
		  binning);
  default:
    throw (AipsError ("LELLattCoord::makeRebinLattice - invalid datatype"));
  }
  return LatticeExprNode();
}


} //# NAMESPACE CASACORE - END

