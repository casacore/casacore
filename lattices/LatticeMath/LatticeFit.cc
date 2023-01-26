//# LatticeFit.cc: Fit every line of pixels parallel to any axis in a Lattice.
//# Copyright (C) 1994,1995,1999,2000,2001,2002,2003
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

#include <casacore/lattices/LatticeMath/LatticeFit.h>

#include <casacore/scimath/Functionals/Function.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TiledLineStepper.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/System/ProgressMeter.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

uint32_t LatticeFit::fitProfiles (Lattice<float> &outImage,
                              Vector<float> &fittedParameters,
                              LinearFit<float> &fitter, 
                              const Lattice<float> &inImage,
                              uint32_t whichAxis,
                              const Vector<bool> &fitMask,
                              bool returnResiduals)
{
    IPosition outShape = outImage.shape();
    IPosition inShape = inImage.shape();

    if (outShape != inShape) {
	throw(AipsError("::baselineFit - outImage.shape() != inImage.shape()"));
    }

    if (whichAxis >= outImage.ndim()) {
	throw(AipsError("::baselineFit - whichAxis does not exist in image"));
    }
    if (int32_t(fitMask.nelements()) != outShape(whichAxis)) {
	throw(AipsError("::baselineFit - improperly specified mask"));
    }

    // These selections etc will get easier when masked arrays are available.
    int32_t nPointsToFit = fitMask.nelements();

    // Set up x and sigma
    Vector<float> x(nPointsToFit);
    Vector<float> y(nPointsToFit);
    Vector<float> sigma(nPointsToFit);

    int32_t count, i;

    // data points with sigma = -1.0 are ignored in fitting
    for (count = 0, i = 0; i < nPointsToFit; i++) {
      if (fitMask(i)) {
	x(i) = count;
	count++;
	sigma(i) = 1.0;
      } else {
	sigma(i) = -1.0;
      }
    }

    // For simplicity this now just iterates through the cube "line by line".
    // It might be considerably more efficient to iterate through plane by
    // plane though (earlier versions of the code did this, however it has
    // been changed to get it working quickly).

    IPosition cursorShape(outShape.nelements());
    cursorShape = 1;
    cursorShape(whichAxis) = inShape(whichAxis);

    LatticeIterator<float>    outIter(outImage, cursorShape);
    RO_LatticeIterator<float> inIter(inImage, cursorShape);

    Vector<float> xall(inShape(whichAxis));
    indgen(xall);
    Vector<float> solution(xall.nelements());
    Vector<float> yall(xall.nelements());

    count = 0;
    fittedParameters.resize(0);
    for (inIter.reset(), outIter.reset(); 
	 ! inIter.atEnd(); inIter++, outIter++, count++) {
        yall = inIter.vectorCursor();
	fittedParameters=fitter.fit(x, yall, sigma);
	for (uint32_t ii=0; ii < solution.nelements(); ii++) {
	    solution(ii) = (*fitter.fittedFunction())(xall(ii)).value();
	}
	if (returnResiduals) {
	    outIter.woVectorCursor() = (yall - solution);
	} else {
	    outIter.woVectorCursor() = solution;
	}
    }

    return count;
}


uint32_t LatticeFit::fitProfiles (MaskedLattice<float>* pFit,
                             MaskedLattice<float>* pResid,
                             MaskedLattice<float>& in,
                             Lattice<float>* pSigma,
                             LinearFit<float>& fitter,
                             uint32_t axis, bool showProgress)
{
   LogIO os(LogOrigin("LatticeFit", "fitProfiles"));
//
   IPosition inShape = in.shape();
   if (pFit!=0) {
      AlwaysAssert(inShape.isEqual(pFit->shape()), AipsError);
   }
   if (pResid!=0) {
      AlwaysAssert(inShape.isEqual(pResid->shape()), AipsError);
   }

// Setup iterators

   IPosition inTileShape = in.niceCursorShape();
   TiledLineStepper stepper (in.shape(), inTileShape, axis);
   RO_MaskedLatticeIterator<float> inIter(in, stepper);
//
   LatticeIterator<float>* pFitIter = 0;
   LatticeIterator<bool>* pFitMaskIter = 0;
   LatticeIterator<float>* pResidIter = 0;
   LatticeIterator<bool>* pResidMaskIter = 0;
//
   if (pFit) {
      pFitIter = new LatticeIterator<float>(*pFit, stepper);
      if (pFit->hasPixelMask()) {
         pFitMaskIter = new LatticeIterator<bool>(pFit->pixelMask(), stepper);
      }
   }
   if (pResid) {
      pResidIter = new LatticeIterator<float>(*pResid, stepper);
      if (pResid->hasPixelMask()) {
         pResidMaskIter = new LatticeIterator<bool>(pResid->pixelMask(), stepper);
      }
   }
//
   int32_t nProfiles = inShape.product()/inIter.vectorCursor().nelements();
   ProgressMeter* pProgress = 0;
   double meterValue = 0.0;
   if (showProgress) {
      pProgress = new ProgressMeter(0.0, double(nProfiles), "Profile fitting", "Profiles fitted",
                                     "", "", true, max(1,int32_t(nProfiles/20)));
   }
//
   const uint32_t n = inShape(axis);
   Vector<float> x(n);
   Vector<float> y(n);
   for (uint32_t i=0; i<x.nelements(); i++) x[i] = i;
   const Function<FunctionTraits<float>::DiffType, FunctionTraits<float>::DiffType>* pFunc = fitter.fittedFunction();
//
   Vector<bool> inMask;
   Vector<float> inSigma;
   bool ok = false;
   uint32_t nFail = 0;
//
   while (!inIter.atEnd()) {  
            
// Get data and mask (reflects pixelMask and region mask of SubImage)

      const Vector<float>& data = inIter.vectorCursor();
      inMask = inIter.getMask(true);
//
      ok = true;
      Vector<float> sol;
      if (pSigma) {
         inSigma = pSigma->getSlice(inIter.position(),
                                    inIter.cursorShape(), true);
         try {
            sol.assign(fitter.fit(x, data, inSigma, &inMask));
         } catch (std::exception& x) {
            ok = false;
         }

      } else {
         try {
            sol.assign(fitter.fit(x, data, &inMask));
         } catch (std::exception& x) {
            ok = false;
         }
      }
      for (Vector<float>::const_iterator iter=sol.begin(); iter!=sol.end(); iter++) {
    	  if (isNaN(*iter)) {
    		  ok = false;
    	  }
      }

// Evaluate
      if (ok) {
         if (pFit) {
            for (uint32_t i=0; i<n;  i++) {
               pFitIter->rwVectorCursor()[i] = (*pFunc)(x(i)).value();
            }
         }
         if (pFitMaskIter) {
            pFitMaskIter->rwVectorCursor() = inMask;
         }
         if (pResid) {   
            if (pFit) {
               pResidIter->rwVectorCursor() = data - pFitIter->rwVectorCursor();
            } else {
               for (uint32_t i=0; i<n;  i++) {
                  pResidIter->rwVectorCursor()[i] = data[i] - (*pFunc)(x(i)).value();
               }
            }
         }
         if (pResidMaskIter) {
            pResidMaskIter->rwVectorCursor() = inMask;
         }
      } else {
         nFail++;
         if (pFit) {
            pFitIter->rwVectorCursor() = 0.0;
         }
         if (pFitMaskIter) {
            pFitMaskIter->rwVectorCursor() = false;
         }
         if (pResid) {
            pResidIter->rwVectorCursor() = 0.0;
         }
         if (pResidMaskIter) {
            pResidMaskIter->rwVectorCursor() = false;
         }
      }                                  
//
      inIter++;
      if (pFitIter) (*pFitIter)++;
      if (pResidIter) (*pResidIter)++;
      if (pFitMaskIter) (*pFitMaskIter)++;    
      if (pResidMaskIter) (*pResidMaskIter)++;
//          
      if (pProgress) {
         meterValue += 1.0;
         pProgress->update(meterValue);
      }
    }     
//
    if (pFitIter) delete pFitIter;
    if (pResidIter) delete pResidIter;
    if (pFitMaskIter) delete pFitMaskIter;
    if (pResidMaskIter) delete pResidMaskIter;
    if (pProgress) delete pProgress;
//
    os << "Number of    profiles = " << nProfiles << LogIO::POST;
    os << "Number of   good fits = " << nProfiles - nFail << LogIO::POST;
    os << "Number of failed fits = " << nFail << LogIO::POST;
//
   return nFail;
}
  

} //# NAMESPACE CASACORE - END

