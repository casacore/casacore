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
//#
//# $Id$

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

uInt LatticeFit::fitProfiles (Lattice<Float> &outImage,
                              Vector<Float> &fittedParameters,
                              LinearFit<Float> &fitter, 
                              const Lattice<Float> &inImage,
                              uInt whichAxis,
                              const Vector<Bool> &fitMask,
                              Bool returnResiduals)
{
    IPosition outShape = outImage.shape();
    IPosition inShape = inImage.shape();

    if (outShape != inShape) {
	throw(AipsError("::baselineFit - outImage.shape() != inImage.shape()"));
    }

    if (whichAxis >= outImage.ndim()) {
	throw(AipsError("::baselineFit - whichAxis does not exist in image"));
    }
    if (Int(fitMask.nelements()) != outShape(whichAxis)) {
	throw(AipsError("::baselineFit - improperly specified mask"));
    }

    // These selections etc will get easier when masked arrays are available.
    Int nPointsToFit = fitMask.nelements();

    // Set up x and sigma
    Vector<Float> x(nPointsToFit);
    Vector<Float> y(nPointsToFit);
    Vector<Float> sigma(nPointsToFit);

    Int count, i;

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

    LatticeIterator<Float>    outIter(outImage, cursorShape);
    RO_LatticeIterator<Float> inIter(inImage, cursorShape);

    Vector<Float> xall(inShape(whichAxis));
    indgen(xall);
    Vector<Float> solution(xall.nelements());
    Vector<Float> yall(xall.nelements());

    count = 0;
    fittedParameters.resize(0);
    for (inIter.reset(), outIter.reset(); 
	 ! inIter.atEnd(); inIter++, outIter++, count++) {
        yall = inIter.vectorCursor();
	fittedParameters=fitter.fit(x, yall, sigma);
	for (uInt ii=0; ii < solution.nelements(); ii++) {
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


uInt LatticeFit::fitProfiles (MaskedLattice<Float>* pFit,
                             MaskedLattice<Float>* pResid,
                             MaskedLattice<Float>& in,
                             Lattice<Float>* pSigma,
                             LinearFit<Float>& fitter,
                             uInt axis, Bool showProgress)
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
   RO_MaskedLatticeIterator<Float> inIter(in, stepper);
//
   LatticeIterator<Float>* pFitIter = 0;
   LatticeIterator<Bool>* pFitMaskIter = 0;
   LatticeIterator<Float>* pResidIter = 0;
   LatticeIterator<Bool>* pResidMaskIter = 0;
//
   if (pFit) {
      pFitIter = new LatticeIterator<Float>(*pFit, stepper);
      if (pFit->hasPixelMask()) {
         pFitMaskIter = new LatticeIterator<Bool>(pFit->pixelMask(), stepper);
      }
   }
   if (pResid) {
      pResidIter = new LatticeIterator<Float>(*pResid, stepper);
      if (pResid->hasPixelMask()) {
         pResidMaskIter = new LatticeIterator<Bool>(pResid->pixelMask(), stepper);
      }
   }
//
   Int nProfiles = inShape.product()/inIter.vectorCursor().nelements();
   ProgressMeter* pProgress = 0;
   Double meterValue = 0.0;
   if (showProgress) {
      pProgress = new ProgressMeter(0.0, Double(nProfiles), "Profile fitting", "Profiles fitted",
                                     "", "", True, max(1,Int(nProfiles/20)));
   }
//
   const uInt n = inShape(axis);
   Vector<Float> x(n);
   Vector<Float> y(n);
   for (uInt i=0; i<x.nelements(); i++) x[i] = i;
   const Function<FunctionTraits<Float>::DiffType, FunctionTraits<Float>::DiffType>* pFunc = fitter.fittedFunction();
//
   Vector<Bool> inMask;
   Vector<Float> inSigma;
   Bool ok = False;
   uInt nFail = 0;
//
   while (!inIter.atEnd()) {  
            
// Get data and mask (reflects pixelMask and region mask of SubImage)

      const Vector<Float>& data = inIter.vectorCursor();
      inMask = inIter.getMask(True);
//
      ok = True;
      Vector<Float> sol;
      if (pSigma) {
         inSigma = pSigma->getSlice(inIter.position(),
                                    inIter.cursorShape(), True);
         try {
            sol.assign(fitter.fit(x, data, inSigma, &inMask));
         } catch (AipsError x) {
            ok = False;
         }

      } else {
         try {
            sol.assign(fitter.fit(x, data, &inMask));
         } catch (AipsError x) {
            ok = False;
         }
      }
      for (Vector<Float>::const_iterator iter=sol.begin(); iter!=sol.end(); iter++) {
    	  if (isNaN(*iter)) {
    		  ok = False;
    	  }
      }

// Evaluate
      if (ok) {
         if (pFit) {
            for (uInt i=0; i<n;  i++) {
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
               for (uInt i=0; i<n;  i++) {
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
            pFitMaskIter->rwVectorCursor() = False;
         }
         if (pResid) {
            pResidIter->rwVectorCursor() = 0.0;
         }
         if (pResidMaskIter) {
            pResidMaskIter->rwVectorCursor() = False;
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

