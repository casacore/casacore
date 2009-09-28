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
#include <images/Images/ImageInfo.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/TempImage.h>
#include <images/Images/RebinImage.h>
#include <images/Images/ImageFit1D.h>
#include <lattices/Lattices/TiledShape.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/TiledLineStepper.h>
#include <lattices/Lattices/MaskedLatticeIterator.h>
#include <components/SpectralComponents/SpectralElement.h>
#include <casa/System/ProgressMeter.h>
#include <casa/Logging/LogIO.h>
#include <casa/Quanta/Unit.h>
#include <casa/Utilities/Assert.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template <typename T, typename U> 
void ImageUtilities::copyMiscellaneous (ImageInterface<T>& out,
                                        const ImageInterface<U>& in)
{
    out.setMiscInfo(in.miscInfo());
    out.setImageInfo(in.imageInfo());
    out.setUnits(in.units());
    out.appendLog(in.logger());
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
   


template <typename T> 
void ImageUtilities::fitProfiles (ImageInterface<T>*& pFit,
                                  ImageInterface<T>*& pResid,
                                  const ImageInterface<T>& inImage,
                                  ImageInterface<T>*& pWeight,
                                  uInt axis, uInt nGauss, Int poly,
                                  Bool showProgress)
{

// Check shapes

   IPosition inShape = inImage.shape();
   if (pFit) {
      AlwaysAssert(inShape.isEqual(pFit->shape()), AipsError);
   }
   if (pResid) {
      AlwaysAssert(inShape.isEqual(pResid->shape()), AipsError);
   }
   if (pWeight) {   
      AlwaysAssert(inShape.isEqual(pWeight->shape()), AipsError);
   }

// Check axis

   const uInt nDim = inImage.ndim();
   AlwaysAssert(axis<nDim, AipsError);

// Progress Meter

   ProgressMeter* pProgressMeter = 0;
   if (showProgress) {
     Double nMin = 0.0;
     Double nMax = 1.0;
     for (uInt i=0; i<inShape.nelements(); i++) {
        if (i!=axis) {
           nMax *= inShape(i);
        }
     }
     ostringstream oss;
     oss << "Fit profiles on axis " << axis+1;
     pProgressMeter = new ProgressMeter(nMin, nMax, String(oss),
                                        String("Fits"),
                                        String(""), String(""),
                                        True, max(1,Int(nMax/20)));
   }

// Make fitter

   ImageFit1D<T> fitter (inImage, axis);

// Set up iterator

   IPosition inTileShape = inImage.niceCursorShape();
   TiledLineStepper stepper (inImage.shape(), inTileShape, axis);
   RO_MaskedLatticeIterator<Float> inIter(inImage, stepper);

// Get hold of masks

   Lattice<Bool>* pFitMask = 0;
   if (pFit && pFit->hasPixelMask() && pFit->pixelMask().isWritable()) {
      pFitMask = &(pFit->pixelMask());
   }
   Lattice<Bool>* pResidMask = 0;
   if (pResid && pResid->hasPixelMask() && pResid->pixelMask().isWritable()) {
      pResidMask = &(pResid->pixelMask());
   }
//
   IPosition sliceShape(nDim,1);
   sliceShape(axis) = inShape(axis);
   Array<T> failData(sliceShape);
   failData = 0.0;
   Array<Bool> failMask(sliceShape);
   failMask = False;
   Array<T> resultData(sliceShape);
   Array<Bool> resultMask(sliceShape);

// Since we write the fits out to images, fitting in pixel space is fine

   typename ImageFit1D<T>::AbcissaType abcissaType(ImageFit1D<T>::PIXEL);
//
   SpectralElement polyEl(poly);
   Bool ok(False);
   uInt nFail = 0;
   uInt nConv = 0;
   uInt nProfiles = 0;
   for (inIter.reset(); !inIter.atEnd(); inIter++,nProfiles++) {
      const IPosition& curPos = inIter.position();
      fitter.clearList();

// Set data

      ok = fitter.setData (curPos, abcissaType, True);

// Make Gaussian estimate (could try to reuse previous fit as estimate)
// Could use some cutoff criteria

      if (ok) ok = fitter.setGaussianElements (nGauss);

// Add polynomial 

      if (ok && poly>=0) fitter.addElement (polyEl);
//
      if (ok) {
         try {
            ok = fitter.fit();                // ok == False means no convergence
            if (!ok) nConv++;
         } catch (AipsError x) {
            ok = False;                       // Some other error
            nFail++;
         }
      }

// Evaluate and fill

      if (ok) {
         Array<Bool> resultMask = fitter.getTotalMask().reform(sliceShape);
         if (pFit) {
            Array<T> resultData = fitter.getFit().reform(sliceShape);
            pFit->putSlice (resultData, curPos);
            if (pFitMask) pFitMask->putSlice(resultMask, curPos);
         }
         if (pResid) {
            Array<T> resultData = fitter.getResidual().reform(sliceShape);
            pResid->putSlice (resultData, curPos);
            if (pResidMask) pResidMask->putSlice(resultMask, curPos);
         }
      } else {
         if (pFit) {
            pFit->putSlice (failData, curPos);
            if (pFitMask) pFitMask->putSlice(failMask, curPos);
         }
         if (pResid) {
            pResid->putSlice (failData, curPos);
            if (pResidMask) pResidMask->putSlice(failMask, curPos);
         }
      }
//
      if (showProgress) pProgressMeter->update(Double(nProfiles));
    }
    if (pProgressMeter) delete pProgressMeter;
//
    cerr << "Number of profiles   = " << nProfiles << endl;
    cerr << "Number converged     = " << nProfiles - nFail << endl;
    cerr << "Number not converged = " << nConv << endl;
    cerr << "Number failed        = " << nFail << endl;
}

} //# NAMESPACE CASA - END

