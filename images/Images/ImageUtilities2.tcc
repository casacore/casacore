//# ImageUtilities2.cc:  Implement templates functions
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//

#include <images/Images/ImageUtilities.h>

#include <casa/Arrays/MaskedArray.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <coordinates/Coordinates/TabularCoordinate.h>
#include <casa/Exceptions/Error.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/TempImage.h>
#include <images/Images/RebinImage.h>
#include <lattices/Lattices/TiledShape.h>
#include <lattices/Lattices/TempLattice.h>
#include <casa/Utilities/Assert.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template <typename T, typename U> 
void ImageUtilities::copyMiscellaneous (ImageInterface<T>& out,
                                        const ImageInterface<U>& in,
                                        Bool copyImageInfo)
{
    out.setMiscInfo(in.miscInfo());
    if (copyImageInfo) {
        out.setImageInfo(in.imageInfo());
    }
    out.setUnits(in.units());
    out.appendLog(in.logger());
    copyAttributes (out.attrHandler(True), in.roAttrHandler());
}


template <typename T> 
void ImageUtilities::bin (MaskedArray<T>& out, Coordinate& coordOut,
                          const MaskedArray<T>& in, const Coordinate& coordIn,
                          uInt axis, uInt bin)
{

// Check

   AlwaysAssert(coordIn.nPixelAxes()==1 && coordIn.nWorldAxes()==1, AipsError);
   AlwaysAssert(coordOut.nPixelAxes()==1 && coordOut.nWorldAxes()==1, AipsError);
//
   AlwaysAssert(coordIn.type()==coordOut.type(),AipsError);
   Coordinate::Type type = coordIn.type();
   AlwaysAssert(type==Coordinate::LINEAR || type==Coordinate::SPECTRAL ||
                type==Coordinate::TABULAR, AipsError);
//  
   const IPosition shapeIn = in.shape();
   const uInt nDim = shapeIn.nelements();
   AlwaysAssert(axis<nDim, AipsError);

// Create CS

   CoordinateSystem cSysIn;
   LinearCoordinate linCoord;
   for (uInt i=0; i<nDim; i++) {

      if (i==axis) {
         cSysIn.addCoordinate(coordIn);
      } else {
         cSysIn.addCoordinate(linCoord);
      }
   }

// Make Image

   TiledShape tShapeIn(shapeIn);
   TempImage<T> im(tShapeIn, cSysIn);

// Set data

   im.put(in.getArray());
   TempLattice<Bool> pixelMask(shapeIn);
   pixelMask.put(in.getMask());
   im.attachMask(pixelMask);

// Create binner

   IPosition factors(nDim,1);
   factors(axis) = bin;
   RebinImage<T> binIm(im, factors);

// Assign output MA

   MaskedArray<T> tmp(binIm.get(), binIm.getMask());
   out = tmp;

// Handle coordinate.  

   const CoordinateSystem cSysOut = binIm.coordinates();
   if (type==Coordinate::LINEAR) {
      const LinearCoordinate& cIn = cSysOut.linearCoordinate(axis);
      LinearCoordinate& cOut = dynamic_cast<LinearCoordinate&>(coordOut);
      cOut = cIn;
   } else if (type==Coordinate::SPECTRAL) { 
      const SpectralCoordinate& cIn = cSysOut.spectralCoordinate(axis);
      SpectralCoordinate& cOut = dynamic_cast<SpectralCoordinate&>(coordOut);
      cOut = cIn;
   } else if (type==Coordinate::TABULAR) {
      const TabularCoordinate& cIn = cSysOut.tabularCoordinate(axis);
      TabularCoordinate& cOut = dynamic_cast<TabularCoordinate&>(coordOut);
      cOut = cIn;
   }
}

} //# NAMESPACE CASA - END

