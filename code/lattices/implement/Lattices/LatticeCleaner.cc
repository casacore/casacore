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

#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>

#include <aips/Logging.h>
#include <aips/Logging/LogIO.h>
#include <aips/OS/File.h>
#include <aips/Containers/Record.h>

#include <trial/Lattices/LatticeCleaner.h>
#include <trial/Lattices/LatticeCleanerProgress.h>
#include <aips/Lattices/TiledLineStepper.h> 
#include <aips/Lattices/LatticeStepper.h> 
#include <aips/Lattices/LatticeNavigator.h> 
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/TempLattice.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/CopyLattice.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Arrays/Slicer.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeExprNode.h>


#include <aips/Tasking/AppInfo.h>
#include <trial/Tasking/ApplicationEnvironment.h>
#include <trial/Tasking/PGPlotter.h>
#include <trial/Tasking/ObjectController.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/VectorIter.h>

#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/Fallible.h>

#include <aips/Mathematics/Constants.h>

#include <aips/Logging/LogSink.h>
#include <aips/Logging/LogMessage.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Matrix.h>



template<class T> 
Bool LatticeCleaner<T>::validatePsf(const Lattice<T> & psf)
{
  LogIO os(LogOrigin("LatticeCleaner", "validatePsf()", WHERE));
  
  // Find the peak of the raw Psf
  AlwaysAssert(psf.shape().product() != 0, AipsError);
  T maxPsf=0;
  itsPositionPeakPsf=IPosition(psf.shape().nelements(), 0);
  findMaxAbsLattice(psf, maxPsf, itsPositionPeakPsf);
  os << "Peak of PSF = " << maxPsf << " at " << itsPositionPeakPsf+1
     << LogIO::POST;
  return True;
}
  

template<class T> 
LatticeCleaner<T>::LatticeCleaner(const Lattice<T> & psf,
				  const Lattice<T> &dirty):
  itsMask(0),
  itsScaleSizes(0),
  itsChoose(True),
  itsDoSpeedup(False),
  itsIgnoreCenterBox(False),
  itsSmallScaleBias(0.6),
  itsStopAtLargeScaleNegative(False),
  itsStopPointMode(-1),
  itsDidStopPointMode(False)
  
{
  AlwaysAssert(validatePsf(psf), AipsError);
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  AlwaysAssert(psf.shape().nelements() == dirty.shape().nelements(),
	       AipsError);
  AlwaysAssert(dirty.shape().product() != 0, AipsError);
  // looks OK so make the convolver
  
  // We need to guess the memory use. For the moment, we'll assume
  // that about 5 scales will be used, giving about 32 TempLattices
  // in all. Also we'll try not to take more that half of the memory

  // Ah, but when we are doing a mosaic, its actually worse than this!
  // So, we pass it in
  itsMemoryMB=Double(AppInfo::memoryInMB())/64.0;

  itsDirty = new TempLattice<T>(dirty.shape(), itsMemoryMB);
  itsDirty->copyData(dirty);
  itsXfr=new TempLattice<Complex>(psf.shape(), itsMemoryMB);
  convertLattice(*itsXfr,psf);
  LatticeFFT::cfft2d(*itsXfr, True);

  itsScales.resize(0);
  itsDirtyConvScales.resize(0);
  itsPsfConvScales.resize(0);
  itsScaleMasks.resize(0);
  itsScalesValid = False;
  itsStartingIter = 0;
}

template <class T> LatticeCleaner<T>::
LatticeCleaner(const LatticeCleaner<T> & other):
   itsCleanType(other.itsCleanType),
   itsDirty(other.itsDirty),
   itsXfr(other.itsXfr),
   itsMask(other.itsMask),
   itsScales(other.itsScales),
   itsPsfConvScales(other.itsPsfConvScales),
   itsDirtyConvScales(other.itsDirtyConvScales),
   itsScaleMasks(other.itsScaleMasks),
   itsStartingIter(other.itsStartingIter),
   itsIgnoreCenterBox(other.itsIgnoreCenterBox),
   itsSmallScaleBias(other.itsSmallScaleBias),
   itsStopAtLargeScaleNegative(other.itsStopAtLargeScaleNegative),
   itsStopPointMode(other.itsStopPointMode),
   itsDidStopPointMode(other.itsDidStopPointMode)
{
}

template<class T> LatticeCleaner<T> & LatticeCleaner<T>::
operator=(const LatticeCleaner<T> & other) {
  if (this != &other) {
    itsCleanType = other.itsCleanType;
    itsXfr = other.itsXfr;
    itsMask = other.itsMask;
    itsDirty = other.itsDirty;
    itsScales = other.itsScales;
    itsPsfConvScales = other.itsPsfConvScales;
    itsDirtyConvScales = other.itsDirtyConvScales;
    itsScaleMasks = other.itsScaleMasks;
    itsStartingIter = other.itsStartingIter;
    itsIgnoreCenterBox = other.itsIgnoreCenterBox;
    itsSmallScaleBias = other.itsSmallScaleBias;
    itsStopAtLargeScaleNegative = other.itsStopAtLargeScaleNegative;
    itsStopPointMode = other.itsStopPointMode;
    itsDidStopPointMode = other.itsDidStopPointMode;

  }
  return *this;
}

template<class T> LatticeCleaner<T>::
~LatticeCleaner()
{
  destroyScales();
  if(itsDirty) delete itsDirty;
  if(itsXfr) delete itsXfr;
  if(itsMask) delete itsMask; 
}


// add a mask image
template<class T> 
void LatticeCleaner<T>::setMask(Lattice<T> & mask) 
{
  IPosition maskShape = mask.shape();
  IPosition dirtyShape = itsDirty->shape();

  AlwaysAssert((mask.shape() == itsDirty->shape()), AipsError);

  itsMask = new TempLattice<T>(mask.shape(), itsMemoryMB);
  itsMask->copyData(mask);

  if (itsScalesValid) {
    makeScaleMasks();
  }

};



// Set up the control parameters
template <class T>
Bool LatticeCleaner<T>::setcontrol(CleanEnums::CleanType cleanType,
				   const Int niter,
				   const Float gain,
				   const Quantity& threshold,
				   const Bool choose)
{
  itsCleanType=cleanType;
  itsMaxNiter=niter;
  itsGain=gain;
  itsThreshold=threshold;
  itsChoose=choose;
  return True;
}

// Set up speedup parameters
template <class T>
void LatticeCleaner<T>::speedup(const Float nDouble)
{
  itsDoSpeedup=True;
  itsNDouble = nDouble;
};



// Do the clean as set up
template <class T>
Bool LatticeCleaner<T>::clean(Lattice<T>& model,
			      LatticeCleanerProgress<T>* progress)
{
  AlwaysAssert(model.shape()==itsDirty->shape(), AipsError);

  LogIO os(LogOrigin("LatticeCleaner", "clean()", WHERE));
  
  Int nScalesToClean=itsNscales;
  if (itsCleanType==CleanEnums::HOGBOM) {
    os << "Hogbom Clean algorithm" << LogIO::POST;
    nScalesToClean=1;
  }
  else if (itsCleanType==CleanEnums::MULTISCALE) {
    if (nScalesToClean==1) {
      os << "Multi-scale Clean with only one scale" << LogIO::POST;
    }
    else {
      os << "Multi-scale Clean algorithm" << LogIO::POST;
    }
  }

  Int scale;
  Vector<T> scaleBias(nScalesToClean);
  if (nScalesToClean > 1) {
    for (scale=0;scale<nScalesToClean;scale++) {
      scaleBias(scale) = 1 - itsSmallScaleBias *
	itsScaleSizes(scale)/itsScaleSizes(nScalesToClean-1);
    }
  } else {
    scaleBias(0) = 1.0;
  }
  AlwaysAssert(itsScalesValid, AipsError);

  // Find the peaks of the convolved Psfs
  Vector<T> maxPsfConvScales(nScalesToClean);
  for (scale=0;scale<nScalesToClean;scale++) {
    IPosition positionPeakPsfConvScales(model.shape().nelements(), 0);

    findMaxAbsLattice(*itsPsfConvScales[scale], maxPsfConvScales(scale),
		      positionPeakPsfConvScales);
    if(nScalesToClean==1) {
      os << "Peak of PSF " << maxPsfConvScales(scale)
	 << " at "   << positionPeakPsfConvScales+1 << LogIO::POST;
    }
    else {
      os << "Scale " << scale+1 << ", peak of PSF " << maxPsfConvScales(scale)
	 << " at "   << positionPeakPsfConvScales+1 << LogIO::POST;
    }
    if ( maxPsfConvScales(scale) < 0.0) {
      os << "As Peak of PSF is negative, you should setscales again with a smaller scale size" 
	 << LogIO::SEVERE;
      return False;
    }
  }


  // Define a subregion for the inner quarter
  IPosition blcDirty(model.shape().nelements(), 0);
  IPosition trcDirty(model.shape()-1);

  if (itsIgnoreCenterBox) {
    os << "Cleaning entire image as per MF/WF" << LogIO::POST;
  } else {
    os << "Cleaning inner quarter of image" << LogIO::POST;
    for (Int i=0;i<Int(model.shape().nelements());i++) {
      blcDirty(i)=model.shape()(i)/4;
      trcDirty(i)=blcDirty(i)+model.shape()(i)/2-1;
      if(trcDirty(i)<0) trcDirty(i)=1;
    }
  }
  LCBox centerBox(blcDirty, trcDirty, model.shape());

  PtrBlock<Lattice<T>* > scaleMaskSubs;
  if (itsMask)  {
    scaleMaskSubs.resize(itsNscales);
    for (Int is=0; is < itsNscales; is++) {
      scaleMaskSubs[is] = new SubLattice<T>(*(itsScaleMasks[is]), centerBox);
    }
  }

  // Start the iteration
  Vector<T> maxima(nScalesToClean);
  Block<IPosition> posMaximum(nScalesToClean);
  Vector<T> totalFluxScale(nScalesToClean); totalFluxScale=0.0;
  T totalFlux=0.0;
  Bool converged=False;
  Int stopPointModeCounter = 0;
  Int optimumScale=0;
  T strengthOptimum=0.0;
  IPosition positionOptimum(model.shape().nelements(), 0);
  os << "Starting iteration"<< LogIO::POST;

  itsIteration = itsStartingIter;
  for (Int ii=itsStartingIter; ii < itsMaxNiter; ii++) {
    itsIteration++;
    // Find the peak residual
    strengthOptimum = 0.0;
    optimumScale = 0;
    for (scale=0; scale<nScalesToClean; scale++) {
      // Find absolute maximum for the dirty image
      SubLattice<T> dirtySub(*itsDirtyConvScales[scale], centerBox);
      maxima(scale)=0;
      posMaximum[scale]=IPosition(model.shape().nelements(), 0);


      if (itsMask) {
	findMaxAbsMaskLattice(dirtySub, *(scaleMaskSubs[scale]),
			      maxima(scale), posMaximum[scale]);
      } else {
	findMaxAbsLattice(dirtySub, maxima(scale), posMaximum[scale]);
      }

      // Remember to adjust the position for the window and for 
      // the flux scale
      maxima(scale)/=maxPsfConvScales(scale);
      maxima(scale) *= scaleBias(scale);
      posMaximum[scale]+=blcDirty;
      if(abs(maxima(scale))>abs(strengthOptimum)) {
        optimumScale=scale;
        strengthOptimum=maxima(scale);
	positionOptimum=posMaximum[scale];
      }
    }

    AlwaysAssert(optimumScale<nScalesToClean, AipsError);

    // Now add to the total flux
    totalFlux += (strengthOptimum*itsGain);
    totalFluxScale(optimumScale) += (strengthOptimum*itsGain);


    // Various ways of stopping:
    //    1. stop if below threshold
    if(abs(strengthOptimum)<threshold() ) {
      os << "Reached stopping threshold " << threshold() << LogIO::POST;
      os << "Optimum flux is " << abs(strengthOptimum) << LogIO::POST;
      converged = True;
      break;
    }
    //    2. negatives on largest scale?
    if (itsStopAtLargeScaleNegative  && 
	optimumScale == (nScalesToClean-1) && 
	strengthOptimum < 0.0) {
      os << "Reached negative on largest scale" << LogIO::POST;
      converged = False;
      break;
    }
    if (itsStopPointMode > 0) {
      if (optimumScale == 0) {
	stopPointModeCounter++;
      } else {
	stopPointModeCounter = 0;
      }
      if (stopPointModeCounter >= itsStopPointMode) {
	os << "Cleaned " << stopPointModeCounter << 
	  " consecutive components from the smallest scale, stopping prematurely"
	   << LogIO::POST;
	itsDidStopPointMode = True;
	converged = False;
	break;
      }
    }



    if(progress) {
      progress->info(False, itsIteration, itsMaxNiter, model, maxima,
		     posMaximum, strengthOptimum,
		     optimumScale, positionOptimum,
		     totalFlux, totalFluxScale,
		     itsDirtyConvScales, (Bool)(itsIteration==itsStartingIter) );
    } else if ((itsIteration % 20) == 0) {
      if (itsIteration == 0) {
	os << "ItsIteration    Resid   CleanedFlux" << LogIO::POST;
      }
      os << itsIteration <<"      "<<strengthOptimum<<"      "<< totalFlux <<LogIO::POST ;
    }

    // Continuing: subtract the peak that we found from all dirty images
    // Define a subregion so that that the peak is centered
    IPosition blc(model.shape().nelements(), 0);
    IPosition trc(model.shape()-1);
    IPosition blcPsf(model.shape().nelements(), 0);
    IPosition trcPsf(model.shape()-1);
    for (Int i=0;i<Int(model.shape().nelements());i++) {
      blc(i)+=positionOptimum(i)-itsPositionPeakPsf(i);
      if(blc(i)<0) blc(i)=0;
      if(blc(i)>=model.shape()(i)) blc(i)=model.shape()(i)-1;
      trc(i)+=positionOptimum(i)-itsPositionPeakPsf(i);
      if(trc(i)>=model.shape()(i)) trc(i)=model.shape()(i)-1;
      if(trc(i)<0) trc(i)=0;
      blcPsf(i)-=positionOptimum(i)-itsPositionPeakPsf(i);
      if(blcPsf(i)<0) blcPsf(i)=0;
      if(blcPsf(i)>=model.shape()(i)) blcPsf(i)=model.shape()(i)-1;
      trcPsf(i)-=positionOptimum(i)-itsPositionPeakPsf(i);
      if(trcPsf(i)>=model.shape()(i)) trcPsf(i)=model.shape()(i)-1;
      if(trcPsf(i)<0) trcPsf(i)=0;
    }

    LCBox subRegion(blc, trc, model.shape());
    LCBox subRegionPsf(blcPsf, trcPsf, model.shape());
    SubLattice<T> modelSub(model, subRegion, True);
    SubLattice<T> scaleSub(*itsScales[optimumScale], subRegionPsf, True);

    // Now do the addition of this scale to the model image....
    T scaleFactor;
    scaleFactor=itsGain*strengthOptimum;
    LatticeExpr<T> add(modelSub+scaleFactor*scaleSub);
    modelSub.copyData(add);


    // and then subtract the effects of this scale from all the precomputed
    // dirty convolutions.
    for (scale=0;scale<nScalesToClean;scale++) {
      SubLattice<T> dirtySub(*itsDirtyConvScales[scale], subRegion, True);
      AlwaysAssert(itsPsfConvScales[index(scale,optimumScale)], AipsError);
      SubLattice<T> psfSub(*itsPsfConvScales[index(scale,optimumScale)],
			       subRegionPsf, True);
      LatticeExpr<T> sub(dirtySub-scaleFactor*psfSub);
      dirtySub.copyData(sub);
    }
  }
  // End of iteration

  if(itsMask) {
    for (Int is=0; is < itsNscales; is++) {
      delete scaleMaskSubs[is];
    }
    scaleMaskSubs.resize(0);
  }

  // Finish off the plot, etc.
  if(progress) {
    progress->info(True, itsIteration, itsMaxNiter, model, maxima, posMaximum,
		   strengthOptimum,
		   optimumScale, positionOptimum,
		   totalFlux, totalFluxScale,
		   itsDirtyConvScales);
  }

  if(!converged) {
    os << "Failed to reach stopping threshold" << LogIO::POST;
    return False;
  }
  else {
    return True;
  }
  return True;
}


template<class T>
Bool LatticeCleaner<T>::findMaxAbsLattice(const Lattice<T>& lattice,
					  T& maxAbs,
					  IPosition& posMaxAbs)
{

  posMaxAbs = IPosition(lattice.shape().nelements(), 0);
  maxAbs=0.0;
  const IPosition tileShape = lattice.niceCursorShape();
  TiledLineStepper ls(lattice.shape(), tileShape, 0);
  {
    RO_LatticeIterator<T> li(lattice, ls);
    for(li.reset();!li.atEnd();li++) {
      IPosition posMax=li.position();
      IPosition posMin=li.position();
      T maxVal=0.0;
      T minVal=0.0;
      minMax(minVal, maxVal, posMin, posMax, li.cursor());
      if(abs(minVal)>abs(maxAbs)) {
        maxAbs=minVal;
	posMaxAbs=li.position();
	posMaxAbs(0)=posMin(0);
      }
      if(abs(maxVal)>abs(maxAbs)) {
        maxAbs=maxVal;
	posMaxAbs=li.position();
	posMaxAbs(0)=posMax(0);
      }
    }
  }

  return True;
}




template<class T>
Bool LatticeCleaner<T>::findMaxAbsMaskLattice(const Lattice<T>& lattice,
					      const Lattice<T>& mask,
					      T& maxAbs,
					      IPosition& posMaxAbs)
{

  posMaxAbs = IPosition(lattice.shape().nelements(), 0);
  maxAbs=0.0;
  const IPosition tileShape = lattice.niceCursorShape();
  TiledLineStepper ls(lattice.shape(), tileShape, 0);
  {
    RO_LatticeIterator<T> li(lattice, ls);
    RO_LatticeIterator<T> mi(mask, ls);
    for(li.reset(),mi.reset();!li.atEnd();li++, mi++) {
      IPosition posMax=li.position();
      IPosition posMin=li.position();
      IPosition posMaxMask=li.position();
      IPosition posMinMask=li.position();
      T maxVal=0.0;
      T minVal=0.0;
      T maxMask=0.0;
      T minMask=0.0;
      
      minMaxMasked(minVal, maxVal, posMin, posMax, li.cursor(), mi.cursor());
      minMax(minMask, maxMask, posMinMask, posMaxMask, mi.cursor());
      if(abs(minVal)>abs(maxAbs)) {
        maxAbs=minVal;
	posMaxAbs=li.position();
	posMaxAbs(0)=posMin(0);
      }
      if(abs(maxVal)>abs(maxAbs)) {
        maxAbs=maxVal;
	posMaxAbs=li.position();
	posMaxAbs(0)=posMax(0);
      }
    }
  }

  return True;
}




template<class T>
Bool LatticeCleaner<T>::setscales(const Int nscales, const Float scaleInc)
{
  LogIO os(LogOrigin("deconvolver", "setcales()", WHERE));

  itsNscales=nscales;
  if(itsNscales<1) {
    os << "Using default of 5 scales" << LogIO::POST;
    itsNscales=5;
  }
  
  Vector<Float> scaleSizes(itsNscales);
  
  // Validate scales
  os << "Creating " << itsNscales << " scales" << LogIO::POST;
  scaleSizes(0) = 0.00001 * scaleInc;
  os << "scale 1 = 0.0 arcsec" << LogIO::POST;
  for (Int scale=1; scale<itsNscales;scale++) {
    scaleSizes(scale) =
      scaleInc * pow(10.0, (Float(scale)-2.0)/2.0);
    os << "scale " << scale+1 << " = " << scaleSizes(scale)
       << " arcsec" << LogIO::POST;
  }

  return setscales(scaleSizes);

}
  
// We calculate all the scales and the corresponding convolutions
// and cross convolutions.
template<class T>
Bool LatticeCleaner<T>::setscales(const Vector<Float>& scaleSizes)
{
  LogIO os(LogOrigin("deconvolver", "setscales()", WHERE));

  Int scale;

  if(itsScales.nelements()>0) {
    destroyScales();
  }

  itsNscales=scaleSizes.nelements();
  itsScaleSizes.resize(itsNscales);
  itsScaleSizes=scaleSizes;  // make a copy that we can call our own
  GenSort<Float>::sort(itsScaleSizes);

  itsScales.resize(itsNscales);
  itsDirtyConvScales.resize(itsNscales);
  itsScaleMasks.resize(itsNscales);
  itsPsfConvScales.resize((itsNscales+1)*(itsNscales+1));
  for(scale=0; scale<itsNscales;scale++) {
    itsScales[scale] = 0;
    itsDirtyConvScales[scale] = 0;
    itsScaleMasks[scale] = 0;
  }
  for(scale=0; scale<((itsNscales+1)*(itsNscales+1));scale++) {
    itsPsfConvScales[scale] = 0;
  }

  AlwaysAssert(itsDirty, AipsError);


  TempLattice<Complex> dirtyFT(itsDirty->shape(), itsMemoryMB);
  convertLattice(dirtyFT, *itsDirty);
  LatticeFFT::cfft2d(dirtyFT, True);

  PtrBlock<TempLattice<Complex> *> scaleXfr(itsNscales);

  for (scale=0; scale<itsNscales;scale++) {
    os << "Calculating image for scale " << scale+1 << LogIO::POST;
    itsScales[scale] = new TempLattice<T>(itsDirty->shape(),
					  itsMemoryMB);
    AlwaysAssert(itsScales[scale], AipsError);
    // First make the scale
    makeScale(*itsScales[scale], scaleSizes(scale));
    scaleXfr[scale] = new TempLattice<Complex> (itsScales[scale]->shape(),
						itsMemoryMB);
    // Now store the XFR
    // Following doesn't work under egcs
    //    scaleXfr[scale]->copyData(LatticeExpr<Complex>(*itsScales[scale], 0.0));
    convertLattice(*scaleXfr[scale], *itsScales[scale]);

    // Now FFT
    LatticeFFT::cfft2d(*scaleXfr[scale], True);
  }
    
  // Now we can do all the convolutions
  TempLattice<Complex> cWork(itsDirty->shape(), itsMemoryMB);
  for (scale=0; scale<itsNscales;scale++) {
    os << "Calculating convolutions for scale " << scale+1 << LogIO::POST;
    
    // PSF * scale
    LatticeExpr<Complex> ppsExpr( (*itsXfr)*(*scaleXfr[scale]));
    cWork.copyData(ppsExpr);
    LatticeFFT::cfft2d(cWork, False);
    itsPsfConvScales[scale] = new TempLattice<T>(itsDirty->shape(),
						 itsMemoryMB);
    AlwaysAssert(itsPsfConvScales[scale], AipsError);
    LatticeExpr<T> realWork(real(cWork));
    itsPsfConvScales[scale]->copyData(realWork);
    
    // Dirty * scale
    LatticeExpr<Complex> dpsExpr( (dirtyFT)*(*scaleXfr[scale]));
    cWork.copyData(dpsExpr);
    LatticeFFT::cfft2d(cWork, False);
    itsDirtyConvScales[scale] = new TempLattice<T>(itsDirty->shape(),
						   itsMemoryMB);
    AlwaysAssert(itsDirtyConvScales[scale], AipsError);

    LatticeExpr<T> realWork2(real(cWork));
    itsDirtyConvScales[scale]->copyData(realWork2);

    for (Int otherscale=scale;otherscale<itsNscales;otherscale++) {
      
      AlwaysAssert(index(scale, otherscale)<Int(itsPsfConvScales.nelements()),
		   AipsError);
      
      // PSF *  scale * otherscale
      LatticeExpr<Complex> ppsoExpr( (*itsXfr)*conj(*scaleXfr[scale])*(*scaleXfr[otherscale]));
      cWork.copyData(ppsoExpr);
      LatticeFFT::cfft2d(cWork, False);
      itsPsfConvScales[index(scale,otherscale)] =
	new TempLattice<T>(itsDirty->shape(), itsMemoryMB);
      AlwaysAssert(itsPsfConvScales[index(scale,otherscale)], AipsError);
      LatticeExpr<T> realWork3(real(cWork));
      itsPsfConvScales[index(scale,otherscale)]->copyData(realWork3);
    }
  }

  itsScalesValid=True;
  for(Int scale=0; scale<itsNscales;scale++) {
    if(scaleXfr[scale]) delete scaleXfr[scale];
    scaleXfr[scale] = 0;
  }

  if (itsMask) {
    makeScaleMasks();
  }

  return True;
}
  
// Make a single scale size image
template <class T> 
void LatticeCleaner<T>::makeScale(Lattice<T>& scale, const Float& scaleSize) 
{
  
  Int nx=scale.shape()(0);
  Int ny=scale.shape()(1);
  Matrix<T> iscale(nx, ny);
  iscale=0.0;
  
  Double refi=nx/2;
  Double refj=ny/2;
  
  if(scaleSize==0.0) {
    iscale(Int(refi), Int(refj)) = 1.0;
  }
  else {
    AlwaysAssert(scaleSize>0.0,AipsError);

    Int mini = max( 0, (Int)(refi-scaleSize));
    Int maxi = min(nx-1, (Int)(refi+scaleSize));
    Int minj = max( 0, (Int)(refj-scaleSize));
    Int maxj = min(ny-1, (Int)(refj+scaleSize));

    Float ypart=0.0;
    Float volume=0.0;
    for (Int j=minj;j<=maxj;j++) {
      ypart = square( (refj - (Double)(j)) / scaleSize );
      for (Int i=mini;i<=maxi;i++) {
	iscale(i,j) = max(0.0, (1.0 - ypart - square( (refi - (Double)(i)) / scaleSize ) ) );
	volume += iscale(i,j);
      }
    }
    iscale/=volume;
  }
  scale.putSlice(iscale, IPosition(scale.ndim(),0), IPosition(scale.ndim(),1));
}

// Calculate index into PsfConvScales
template<class T>
Int LatticeCleaner<T>::index(const Int scale, const Int otherscale) {
  if(otherscale>scale) {
    return scale + itsNscales*(otherscale+1);
  }
  else {
    return otherscale + itsNscales*(scale+1);
  }
}
  
template<class T>
Bool LatticeCleaner<T>::destroyScales()
{
  if(!itsScalesValid) return True;
  for(uInt scale=0; scale<itsScales.nelements();scale++) {
    if(itsScales[scale]) delete itsScales[scale];
    itsScales[scale]=0;
  }
  for(uInt scale=0; scale<itsDirtyConvScales.nelements();scale++) {
    if(itsDirtyConvScales[scale]) delete itsDirtyConvScales[scale];
    itsDirtyConvScales[scale]=0;
  }
  for(uInt scale=0; scale<itsPsfConvScales.nelements();scale++) {
    if(itsPsfConvScales[scale]) delete itsPsfConvScales[scale];
    itsPsfConvScales[scale] = 0;
  }
  destroyMasks();
  itsScales.resize(0);
  itsDirtyConvScales.resize(0);
  itsPsfConvScales.resize(0);
  itsScalesValid=False;
  return True;
}


template<class T>
Bool LatticeCleaner<T>::destroyMasks()
{
  for(uInt scale=0; scale<itsScaleMasks.nelements();scale++) {
    if(itsScaleMasks[scale]) delete itsScaleMasks[scale];
    itsScaleMasks[scale]=0;
  }
  itsScaleMasks.resize(0);
  return True;
};

template<class T>
Bool LatticeCleaner<T>::stopnow() {
  if(itsChoose) {
    LogIO os(LogOrigin("LatticeCleaner", "stopnow()", WHERE));
    Vector<String> choices(2);
    choices(0)="Continue";
    choices(1)="Stop Now";
    choices(2)="Don't ask again";
    String choice =
      ApplicationEnvironment::choice("Do you want to continue or stop?",
				     choices);
    if (choice==choices(0)) {
      return False;
    }
    else if (choice==choices(2)) {
      itsChoose=False;
      os << "Continuing: won't ask again" << LogIO::POST;
      return False;
    }
    else {
      os << "Lattice clean stopped at user request" << LogIO::POST;
      return True;
    }
  }
  else {
    return False;
  }
}



// Set up the masks for the various scales
// This really only works for well behaved (ie, non-concave) masks.
// with only 1.0 or 0.0 values, and assuming the Scale images have
// a finite extent equal to +/- itsScaleSizes(scale)
template <class T>
Bool LatticeCleaner<T>::makeScaleMasks()
{

  LogIO os(LogOrigin("deconvolver", "makeScaleMasks()", WHERE));
  AlwaysAssert( (itsNscales>0), AipsError);
  AlwaysAssert( (itsMask!=0), AipsError);
  
  destroyMasks();

  IPosition wholeShape(itsMask->shape());
  LatticeStepper ls(wholeShape, IPosition(4, wholeShape(0), wholeShape(1), 1, 1), IPosition(4,0,1,2,3));
  RO_LatticeIterator<T> mli(*itsMask, ls);
  IPosition ip(itsMask->shape());
  IPosition maskShape(itsMask->shape());
  ip(2) = 0;
  ip(3) = 0;
  maskShape(2) = 0;
  maskShape(3) = 0;

  Float point99 = 0.99;
  for (Int scale=0; scale<itsNscales; scale++){
    // get a sublattice of the scale image, for computational puproses
    IPosition blc = maskShape;
    IPosition trc = maskShape;
    blc(0) = maskShape(0)/2 - (Int)(itsScaleSizes(scale)+ point99);
    blc(1) = maskShape(1)/2 - (Int)(itsScaleSizes(scale)+ point99);
    trc(0) = maskShape(0)/2 + (Int)(itsScaleSizes(scale)+ point99);
    trc(1) = maskShape(1)/2 + (Int)(itsScaleSizes(scale)+ point99);

    LCBox centerSubRegion(blc, trc, wholeShape);
    SubLattice<T> subScale( *itsScales[scale], centerSubRegion, False);
    
    itsScaleMasks[scale] = new TempLattice<T>(itsMask->shape(),
					      itsMemoryMB);
    if (itsScaleSizes(scale) == 0.0) {
      itsScaleMasks[scale]->copyData(*itsMask);
    } else {
      itsScaleMasks[scale]->set(0.0);
      Bool isIn;
      Float zero = 0.000001;
      LatticeIterator<T> smli(*itsScaleMasks[scale], ls);
      
      // step through each tile; look to see if scale edge points
      // are within this tile; is so, check if they are within the mask --
      // If not within tile, use "getAt"
      for(mli.reset(), smli.reset();  !mli.atEnd();  mli++, smli++) {
	
	IPosition tShape(smli.latticeShape());
	IPosition loc(mli.position());
	for (Int iy=0; iy<tShape(1); iy++) {      
	  for (Int ix=0; ix<tShape(0); ix++) {
	    isIn = True;

	    if (mli.matrixCursor()(ix,iy) <= zero)   isIn = False;
	    // +X
	    if (isIn) {
	      if ( (ix + itsScaleSizes(scale)) < tShape(0) ) {
		if (mli.matrixCursor()(ix + (Int)(itsScaleSizes(scale)+point99),iy) <= zero )  isIn = False;
	      } else {
		ip(0) = loc(0) + ix + (Int)(itsScaleSizes(scale)+point99);
		ip(1) = loc(1) + iy;
		if (ip(0) >= wholeShape(0)) {
		  isIn = False;
		} else {
		  if (itsMask->getAt(ip) <= zero ) isIn = False;
		}
	      }
	    }
	    // -X
	    if (isIn) {
	      if ( (ix - itsScaleSizes(scale)) >= 0 ) {
		if (mli.matrixCursor()(ix - (Int)(itsScaleSizes(scale)+point99),iy) <= zero )  isIn = False;
	      } else {
		ip(0) = loc(0) + ix - (Int)(itsScaleSizes(scale)+point99);
		ip(1) = loc(1) + iy;
		if (ip(0) < 0) {
		  isIn = False;
		} else {
		  if (itsMask->getAt(ip) <= zero ) isIn = False;
		}
	      }
	    }
	    // +Y
	    if (isIn) {
	      if ( (iy + itsScaleSizes(scale)) < tShape(1) ) {
		if (mli.matrixCursor()(ix,iy+(Int)(itsScaleSizes(scale)+point99)) <= zero )  isIn = False;
	      } else {
		ip(0) = loc(0) + ix;
		ip(1) = loc(1) + iy + (Int)(itsScaleSizes(scale)+point99);
		if (ip(1) >= wholeShape(1)) {
		  isIn = False;
		} else {
		  if (itsMask->getAt(ip) <= zero ) isIn = False;
		}
	      }
	    }
	    // -Y
	    if (isIn) {
	      if ( (iy - itsScaleSizes(scale)) >= 0 ) {
		if (mli.matrixCursor()(ix, iy - (Int)(itsScaleSizes(scale)+point99)) <= zero )  isIn = False;
	      } else {
	        ip(0) = loc(0) + ix;
		ip(1) = loc(1) + iy - (Int)(itsScaleSizes(scale)+point99);
		if (ip(1) < 0) {
		  isIn = False;
		} else {
		  if (itsMask->getAt(ip) <= zero ) isIn = False;
		}
	      }
	    }
	    if (isIn) {
	      // check now to see if its REALLY in by multiplying the
	      // scale and the mask and seeing if the integral is very nearly 1.0
	      IPosition blc2 = blc;
	      IPosition trc2 = trc;
	      blc2(0) += ix - wholeShape(0)/2;
	      blc2(1) += iy - wholeShape(1)/2;
	      trc2(0) += ix - wholeShape(0)/2;
	      trc2(1) += iy - wholeShape(1)/2;
	      if (blc2(0) >=0 && blc2(1) >= 0 && 
		  trc2(0) < wholeShape(0) && trc2(1) < wholeShape(1) ) {
		// do the full test: subLattice itsMask, multiply by scale, and integrate
		LCBox shiftedSubRegion(blc2, trc2, wholeShape);
		SubLattice<T> subMask( *itsMask, shiftedSubRegion, False);

		/*  from debugging
		IPosition ssip = subScale.shape();
		IPosition smip = subMask.shape();
		if (ssip(0) != smip(0) ||  ssip(1) != smip(1) ) {
		  cerr << "subScale shape = " << ssip << endl;
		  cerr << "subMask shape = " << smip << endl;
		  cerr << " ix, iy = " << ix << iy << endl;
		  cerr << " blc2, trc2 = " << blc2 << trc2 << endl;
		  cerr << " wholeshape = " << wholeShape << endl;
		}
		*/
		
		LatticeExprNode LEN;
		LEN = sum( subScale * subMask );
		Float mysum = LEN.getFloat();
		if ( abs(mysum - 1.0) < 0.0001 ) {
		  smli.rwMatrixCursor()(ix,iy) = 1.0;
		}
	      }
	    }
	  }
	}
      }
    }

    LatticeExprNode LEN;
    LEN = sum( *itsScaleMasks[scale] );
    Float mysum = LEN.getFloat();
    if (mysum <= 0.1) {
      os << "Warning: scale " << scale << 
	" is too large to fit within the mask" << LogIO::POST;
    }
  }
  return True;
};




template<class T> 
Float LatticeCleaner<T>::threshold()
{
  if (! itsDoSpeedup) {
    return (itsThreshold.get("Jy").getValue());
  } else {
    Float factor = exp( (Float)( itsIteration - itsStartingIter )/ itsNDouble )
      / 2.7182818;
    return (factor * itsThreshold.get("Jy").getValue());
  }
};
