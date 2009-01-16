//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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

#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayMath.h>

#include <casa/Logging/LogIO.h>
#include <casa/OS/File.h>
#include <casa/Containers/Record.h>

#include <lattices/Lattices/LatticeCleaner.h>
#include <lattices/Lattices/LatticeCleanProgress.h>
#include <lattices/Lattices/TiledLineStepper.h> 
#include <lattices/Lattices/LatticeStepper.h> 
#include <lattices/Lattices/LatticeNavigator.h> 
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/LatticeFFT.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/SubLattice.h>
#include <lattices/Lattices/LCBox.h>
#include <casa/Arrays/Slicer.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/LatticeExprNode.h>

#include <casa/OS/HostInfo.h>
#include <casa/System/PGPlotter.h>
#include <casa/Arrays/ArrayError.h>
#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/VectorIter.h>

#include <casa/Utilities/GenSort.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/Fallible.h>

#include <casa/BasicSL/Constants.h>

#include <casa/Logging/LogSink.h>
#include <casa/Logging/LogMessage.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Matrix.h>



namespace casa { //# NAMESPACE CASA - BEGIN

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
LatticeCleaner<T>::LatticeCleaner():
  itsDirty(0),
  itsMask(0),
  itsXfr(0),
  itsScaleSizes(0),
  itsMaximumResidual(0.0),
  itsChoose(True),
  itsDoSpeedup(False),
  itsIgnoreCenterBox(False),
  itsSmallScaleBias(0.6),
  itsStopAtLargeScaleNegative(False),
  itsStopPointMode(-1),
  itsDidStopPointMode(False),
  itsJustStarting(True)
{
  itsMemoryMB=Double(HostInfo::memoryTotal()/1024)/16.0;
  itsScales.resize(0);
  itsScaleXfrs.resize(0);
  itsDirtyConvScales.resize(0);
  itsPsfConvScales.resize(0);
  itsScaleMasks.resize(0);
  itsScalesValid = False;
  itsStartingIter = 0;
}

 

template<class T> 
LatticeCleaner<T>::LatticeCleaner(const Lattice<T> & psf,
				  const Lattice<T> &dirty):
  itsMask(0),
  itsScaleSizes(0),
  itsMaximumResidual(0.0),
  itsChoose(True),
  itsDoSpeedup(False),
  itsIgnoreCenterBox(False),
  itsSmallScaleBias(0.6),
  itsStopAtLargeScaleNegative(False),
  itsStopPointMode(-1),
  itsDidStopPointMode(False),
  itsJustStarting(True)
{
  AlwaysAssert(validatePsf(psf), AipsError);
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  AlwaysAssert(psf.shape().nelements() == dirty.shape().nelements(),
	       AipsError);
  AlwaysAssert(dirty.shape().product() != 0, AipsError);
  // looks OK so make the convolver
  
  // We need to guess the memory use. For the moment, we'll assume
  // that about 4 scales will be used, giving about 32 TempLattices
  // in all. Also we'll try not to take more that half of the memory

  // Ah, but when we are doing a mosaic, its actually worse than this!
  // So, we pass it in
  itsMemoryMB=Double(HostInfo::memoryTotal()/1024)/16.0;

  itsDirty = new TempLattice<T>(dirty.shape(), itsMemoryMB);
  itsDirty->copyData(dirty);
  itsXfr=new TempLattice<Complex>(psf.shape(), itsMemoryMB);
  itsXfr->copyData(LatticeExpr<Complex>(toComplex(psf)));
  LatticeFFT::cfft2d(*itsXfr, True);

  itsScales.resize(0);
  itsScaleXfrs.resize(0);
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
   itsScaleXfrs(other.itsScaleXfrs),
   itsPsfConvScales(other.itsPsfConvScales),
   itsDirtyConvScales(other.itsDirtyConvScales),
   itsScaleMasks(other.itsScaleMasks),
   itsStartingIter(other.itsStartingIter),
   itsMaximumResidual(other.itsMaximumResidual),
   itsIgnoreCenterBox(other.itsIgnoreCenterBox),
   itsSmallScaleBias(other.itsSmallScaleBias),
   itsStopAtLargeScaleNegative(other.itsStopAtLargeScaleNegative),
   itsStopPointMode(other.itsStopPointMode),
   itsDidStopPointMode(other.itsDidStopPointMode),
   itsJustStarting(other.itsJustStarting)
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
    itsScaleXfrs = other.itsScaleXfrs;
    itsPsfConvScales = other.itsPsfConvScales;
    itsDirtyConvScales = other.itsDirtyConvScales;
    itsScaleMasks = other.itsScaleMasks;
    itsStartingIter = other.itsStartingIter;
    itsMaximumResidual = other.itsMaximumResidual;
    itsIgnoreCenterBox = other.itsIgnoreCenterBox;
    itsSmallScaleBias = other.itsSmallScaleBias;
    itsStopAtLargeScaleNegative = other.itsStopAtLargeScaleNegative;
    itsStopPointMode = other.itsStopPointMode;
    itsDidStopPointMode = other.itsDidStopPointMode;
    itsJustStarting = other.itsJustStarting;

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

template<class T> 
void LatticeCleaner<T>::update(const Lattice<T> &dirty)
{
  AlwaysAssert(dirty.shape()==itsDirty->shape(), AipsError);
  itsDirty->copyData(dirty);

  LogIO os(LogOrigin("LatticeCleaner", "clean()", WHERE));

  TempLattice<Complex> dirtyFT(itsDirty->shape(), itsMemoryMB);
  dirtyFT.copyData(LatticeExpr<Complex>(toComplex(*itsDirty)));
  LatticeFFT::cfft2d(dirtyFT, True);

  // Now we can redo the relevant convolutions
  TempLattice<Complex> cWork(itsDirty->shape(), itsMemoryMB);

  for (Int scale=0; scale<itsNscales;scale++) {
    // Dirty * scale
    os << "Updating dirty * scale image for scale " << scale+1 << LogIO::POST;

    LatticeExpr<Complex> dpsExpr( (dirtyFT)*(*itsScaleXfrs[scale]));
    cWork.copyData(dpsExpr);
    LatticeFFT::cfft2d(cWork, False);
    AlwaysAssert(itsDirtyConvScales[scale], AipsError);
    LatticeExpr<T> realWork2(real(cWork));
    itsDirtyConvScales[scale]->copyData(realWork2);
  }

}


// add a mask image
template<class T> 
void LatticeCleaner<T>::setMask(Lattice<T> & mask) 
{
  IPosition maskShape = mask.shape();
  IPosition dirtyShape = itsDirty->shape();

  AlwaysAssert((mask.shape() == itsDirty->shape()), AipsError);

  // This is not needed after the first steps
  itsMask = new TempLattice<T>(mask.shape(), itsMemoryMB);
  itsMask->copyData(mask);

  if (itsScalesValid) {
    makeScaleMasks();
  }

};

template <class T>
Bool LatticeCleaner<T>::setcontrol(CleanEnums::CleanType cleanType,
				   const Int niter,
				   const Float gain,
				   const Quantity& threshold,
				   const Bool choose)
{
  return setcontrol(cleanType, niter, gain, threshold, Quantity(0.0, "%"), choose);
}

// Set up the control parameters
template <class T>
Bool LatticeCleaner<T>::setcontrol(CleanEnums::CleanType cleanType,
				   const Int niter,
				   const Float gain,
				   const Quantity& aThreshold,
				   const Quantity& fThreshold,
				   const Bool choose)
{
  itsCleanType=cleanType;
  itsMaxNiter=niter;
  itsGain=gain;
  itsThreshold=aThreshold;
  itsFracThreshold=fThreshold;
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
			      LatticeCleanProgress* progress)
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
    os << "Scale biases =";
    for (scale=0;scale<nScalesToClean;scale++) {
      scaleBias(scale) = 1 - itsSmallScaleBias *
	itsScaleSizes(scale)/itsScaleSizes(nScalesToClean-1);
      if(scale) os << ",";
      os << " " << scaleBias(scale);
    }
    os << LogIO::POST;
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

  if(itsMask){
    os << "Cleaning using given mask" << LogIO::POST;
    
    Int nx=model.shape()(0);
    Int ny=model.shape()(1);
    
    
    AlwaysAssert(itsMask->shape()(0)==nx, AipsError);
    AlwaysAssert(itsMask->shape()(1)==ny, AipsError);
    
    LatticeStepper mls(itsMask->shape(),
		       IPosition(4, nx, ny, 1, 1),
		       IPosition(4, 0, 1, 3, 2));
    RO_LatticeIterator<Float> maskli(*itsMask, mls);
    maskli.reset(); 
    Int xbeg=nx-1;
    Int ybeg=ny-1;
    Int xend=0;
    Int yend=0;
    for (Int iy=0;iy<ny;iy++) {
      for (Int ix=0;ix<nx;ix++) {
	if(maskli.matrixCursor()(ix,iy)>0.000001) {
	  xbeg=min(xbeg,ix);
	  ybeg=min(ybeg,iy);
	  xend=max(xend,ix);
	  yend=max(yend,iy);
	}
      }
    }
    
    if (!itsIgnoreCenterBox) {
      if((xend - xbeg)>nx/2) {
	xbeg=nx/4-1; //if larger than quarter take inner of mask
	os << LogIO::WARN << "Mask span over more than half the x-axis: Considering inner half of the x-axis"  << LogIO::POST;
      } 
      if((yend - ybeg)>ny/2) { 
	ybeg=ny/4-1;
	os << LogIO::WARN << "Mask span over more than half the y-axis: Considering inner half of the y-axis" << LogIO::POST;
      }  
      xend=min(xend,xbeg+nx/2-1);
      yend=min(yend,ybeg+ny/2-1);
    }

    blcDirty(0)=xbeg;
    blcDirty(1)=ybeg;
    trcDirty(0)=xend;
    trcDirty(1)=yend;
  }
  else {
    if (itsIgnoreCenterBox) {
      os << "Cleaning entire image as per MF/WF" << LogIO::POST;
    }
    else {
      os << "Cleaning inner quarter of image" << LogIO::POST;
      for (Int i=0;i<Int(model.shape().nelements());i++) {
	blcDirty(i)=model.shape()(i)/4;
	trcDirty(i)=blcDirty(i)+model.shape()(i)/2-1;
	if(trcDirty(i)<0) trcDirty(i)=1;
      }
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

    if(ii==itsStartingIter) {
      itsMaximumResidual=abs(strengthOptimum);
      os << "Initial maximum residual is " << itsMaximumResidual
	 << LogIO::POST;
    }

    // Various ways of stopping:
    //    1. stop if below threshold
    if(abs(strengthOptimum)<threshold() ) {
      os << "Reached stopping threshold " << threshold() << LogIO::POST;
      os << "Optimum flux is " << abs(strengthOptimum) << LogIO::POST;
      converged = True;
      break;
    }
    //    2. negatives on largest scale?
    if ((nScalesToClean > 1) && itsStopAtLargeScaleNegative  && 
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
      progress->info(False, itsIteration, itsMaxNiter, maxima,
		     posMaximum, strengthOptimum,
		     optimumScale, positionOptimum,
		     totalFlux, totalFluxScale,
		     itsJustStarting );
      itsJustStarting = False;
    } else if ((itsIteration % (itsMaxNiter/10 > 0 ? itsMaxNiter/10 : 1)) == 0) {
      if (itsIteration == 0) {
	os << "ItsIteration    Resid   CleanedFlux" << LogIO::POST;
      }
      os << itsIteration <<"      "<<strengthOptimum<<"      "<< totalFlux <<LogIO::POST ;
    }

    T scaleFactor;
    scaleFactor=itsGain*strengthOptimum;

    // Continuing: subtract the peak that we found from all dirty images
    // Define a subregion so that that the peak is centered
    IPosition support(model.shape());
    support(0)=max(Int(itsScaleSizes(itsNscales-1)+0.5), support(0));
    support(1)=max(Int(itsScaleSizes(itsNscales-1)+0.5), support(1));

    IPosition inc(model.shape().nelements(), 1);
    
    IPosition blc(positionOptimum-support/2);
    IPosition trc(positionOptimum+support/2-1);
    LCBox::verify(blc, trc, inc, model.shape());
    
    IPosition blcPsf(blc+itsPositionPeakPsf-positionOptimum);
    IPosition trcPsf(trc+itsPositionPeakPsf-positionOptimum);
    LCBox::verify(blcPsf, trcPsf, inc, model.shape());

    makeBoxesSameSize(blc,trc,blcPsf,trcPsf);
    
    LCBox subRegion(blc, trc, model.shape());
    LCBox subRegionPsf(blcPsf, trcPsf, model.shape());
    
    SubLattice<T> modelSub(model, subRegion, True);
    SubLattice<T> scaleSub(*itsScales[optimumScale], subRegionPsf, True);
    
    // Now do the addition of this scale to the model image....
    LatticeExpr<T> add(scaleFactor*scaleSub);
    addTo(modelSub, add);

    // and then subtract the effects of this scale from all the precomputed
    // dirty convolutions.
    for (scale=0;scale<nScalesToClean;scale++) {
      SubLattice<T> dirtySub(*itsDirtyConvScales[scale], subRegion, True);
      AlwaysAssert(itsPsfConvScales[index(scale,optimumScale)], AipsError);
      SubLattice<T> psfSub(*itsPsfConvScales[index(scale,optimumScale)],
			   subRegionPsf, True);
      LatticeExpr<T> sub((-scaleFactor)*psfSub);
      addTo(dirtySub, sub);
    }
  }
  // End of iteration

  for (scale=0;scale<nScalesToClean;scale++) {
    os << "Scale " << scale+1 << ", total flux = "
       << totalFluxScale(scale) << LogIO::POST;
  }

  if(itsMask) {
    for (Int is=0; is < itsNscales; is++) {
      delete scaleMaskSubs[is];
    }
    scaleMaskSubs.resize(0);
  }

  // Finish off the plot, etc.
  if(progress) {
    progress->info(True, itsIteration, itsMaxNiter, maxima, posMaximum,
		   strengthOptimum,
		   optimumScale, positionOptimum,
		   totalFlux, totalFluxScale);
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
  LogIO os(LogOrigin("deconvolver", "setscales()", WHERE));


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

  destroyMasks();

  itsNscales=scaleSizes.nelements();

  // Residual, psf, and mask, plus cross terms
  // e.g. for 5 scales this is 45. for 6 it is 60.
  Int nImages=3*itsNscales+itsNscales*(itsNscales+1);
  os << "Expect to use "  << nImages << " scratch images" << LogIO::POST;

  // Now we can update the size of memory allocated
  itsMemoryMB=0.5*Double(HostInfo::memoryTotal()/1024)/Double(nImages);
  os << "Maximum memory allocated per image "  << itsMemoryMB << "MB" << LogIO::POST;

  itsScaleSizes.resize(itsNscales);
  itsScaleSizes=scaleSizes;  // make a copy that we can call our own
  GenSort<Float>::sort(itsScaleSizes);

  itsScales.resize(itsNscales);
  itsDirtyConvScales.resize(itsNscales);
  itsScaleMasks.resize(itsNscales);
  itsScaleXfrs.resize(itsNscales);
  itsPsfConvScales.resize((itsNscales+1)*(itsNscales+1));
  for(scale=0; scale<itsNscales;scale++) {
    itsScales[scale] = 0;
    itsDirtyConvScales[scale] = 0;
    itsScaleMasks[scale] = 0;
    itsScaleXfrs[scale] = 0;
  }
  for(scale=0; scale<((itsNscales+1)*(itsNscales+1));scale++) {
    itsPsfConvScales[scale] = 0;
  }

  AlwaysAssert(itsDirty, AipsError);

  TempLattice<Complex> dirtyFT(itsDirty->shape(), itsMemoryMB);
  dirtyFT.copyData(LatticeExpr<Complex>(toComplex(*itsDirty)));
  LatticeFFT::cfft2d(dirtyFT, True);

  for (scale=0; scale<itsNscales;scale++) {
    os << "Calculating scale image and Fourier transform for scale " << scale+1 << LogIO::POST;
    itsScales[scale] = new TempLattice<T>(itsDirty->shape(),
					  itsMemoryMB);
    AlwaysAssert(itsScales[scale], AipsError);
    // First make the scale
    makeScale(*itsScales[scale], scaleSizes(scale));
    itsScaleXfrs[scale] = new TempLattice<Complex> (itsScales[scale]->shape(),
						   itsMemoryMB);
    // Now store the XFR
    itsScaleXfrs[scale]->copyData(LatticeExpr<Complex>(toComplex(*itsScales[scale])));

    // Now FFT
    LatticeFFT::cfft2d(*itsScaleXfrs[scale], True);
  }
    
  // Now we can do all the convolutions
  TempLattice<Complex> cWork(itsDirty->shape(), itsMemoryMB);
  for (scale=0; scale<itsNscales;scale++) {
    os << "Calculating convolutions for scale " << scale+1 << LogIO::POST;
    
    // PSF * scale
    LatticeExpr<Complex> ppsExpr( (*itsXfr)*(*itsScaleXfrs[scale]));
    cWork.copyData(ppsExpr);
    LatticeFFT::cfft2d(cWork, False);
    itsPsfConvScales[scale] = new TempLattice<T>(itsDirty->shape(),
						  itsMemoryMB);
    AlwaysAssert(itsPsfConvScales[scale], AipsError);
    LatticeExpr<T> realWork(real(cWork));
    itsPsfConvScales[scale]->copyData(realWork);
    
    // Dirty * scale
    LatticeExpr<Complex> dpsExpr( (dirtyFT)*(*itsScaleXfrs[scale]));
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
      LatticeExpr<Complex> ppsoExpr( (*itsXfr)*conj(*itsScaleXfrs[scale])*(*itsScaleXfrs[otherscale]));
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
    Float rad2=0.0;
    Float rad=0.0;

    for (Int j=minj;j<=maxj;j++) {
      ypart = square( (refj - (Double)(j)) / scaleSize );
      for (Int i=mini;i<=maxi;i++) {
	rad2 =  ypart + square( (refi - (Double)(i)) / scaleSize );
	if (rad2 < 1.0) {
	  if (rad2 <= 0.0) {
	    rad = 0.0;
	  } else {
	    rad = sqrt(rad2);
	  }
	  iscale(i,j) = (1.0 - rad2) * spheroidal(rad);
	  volume += iscale(i,j);
	} else {
	  iscale(i,j) = 0.0;
	}
      }
    }
    iscale/=volume;
  }
  scale.putSlice(iscale, IPosition(scale.ndim(),0), IPosition(scale.ndim(),1));
}

// Calculate the spheroidal function
template<class T>
Float LatticeCleaner<T>::spheroidal(Float nu) {
  
  if (nu <= 0) {
    return 1.0;
  } else if (nu >= 1.0) {
    return 0.0;
  } else {
    uInt np = 5;
    uInt nq = 3;
    Matrix<float> p(np, 2);
    Matrix<float> q(nq, 2);
    p(0,0) = 8.203343e-2;
    p(1,0) = -3.644705e-1;
    p(2,0) =  6.278660e-1;
    p(3,0) = -5.335581e-1; 
    p(4,0) =  2.312756e-1;
    p(0,1) =  4.028559e-3;
    p(1,1) = -3.697768e-2; 
    p(2,1) = 1.021332e-1;
    p(3,1) = -1.201436e-1;
    p(4,1) = 6.412774e-2;
    q(0,0) = 1.0000000e0;
    q(1,0) = 8.212018e-1;
    q(2,0) = 2.078043e-1;
    q(0,1) = 1.0000000e0;
    q(1,1) = 9.599102e-1;
    q(2,1) = 2.918724e-1;
    uInt part = 0;
    Float nuend = 0.0;
    if (nu >= 0.0 && nu < 0.75) {
      part = 0;
      nuend = 0.75;
    } else if (nu >= 0.75 && nu <= 1.00) {
      part = 1;
      nuend = 1.0;
    }

    Float top = p(0,part);
    Float delnusq = pow(nu,2.0) - pow(nuend,2.0);
    uInt k;
    for (k=1; k<np; k++) {
      top += p(k, part) * pow(delnusq, (Float)k);
    }
    Float bot = q(0, part);
    for (k=1; k<nq; k++) {
      bot += q(k,part) * pow(delnusq, (Float)k);
    }
    
    if (bot != 0.0) {
      return (top/bot);
    } else {
      return 0.0;
    }
  }
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
  for(uInt scale=0; scale<itsScaleXfrs.nelements();scale++) {
    if(itsScaleXfrs[scale]) delete itsScaleXfrs[scale];
    itsScaleXfrs[scale]=0;
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

//# Removed on 8-Apr-2004 by GvD because it is not used and add Tasking
//# dependencies to Lattices
// template<class T>
// Bool LatticeCleaner<T>::stopnow() {
//   if(itsChoose) {
//     LogIO os(LogOrigin("LatticeCleaner", "stopnow()", WHERE));
//     Bool stop = ApplicationEnvironment::stop();
//     if(stop) {
//       os << "Lattice clean stopped at user request" << LogIO::POST;
//       return True;
//     }
//     Vector<String> choices(2);
//     choices(0)="Continue";
//     choices(1)="Stop Now";
//     choices(2)="Don't ask again";
//     String choice =
//       ApplicationEnvironment::choice("Do you want to continue or stop?",
// 				     choices);
//     if (choice==choices(0)) {
//       return False;
//     }
//     else if (choice==choices(2)) {
//       itsChoose=False;
//       os << "Continuing: won't ask again" << LogIO::POST;
//       return False;
//     }
//     else {
//       os << "Lattice clean stopped at user request" << LogIO::POST;
//       return True;
//     }
//   }
//   else {
//     return False;
//   }
// }



// Set up the masks for the various scales
// This really only works for well behaved (ie, non-concave) masks.
// with only 1.0 or 0.0 values, and assuming the Scale images have
// a finite extent equal to +/- itsScaleSizes(scale)
template <class T>
Bool LatticeCleaner<T>::makeScaleMasks()
{
  LogIO os(LogOrigin("deconvolver", "makeScaleMasks()", WHERE));
  Int scale;

  if(!itsScalesValid) {
    os << "Scales are not yet set - cannot set scale masks"
       << LogIO::EXCEPTION;
  }

  destroyMasks();

  AlwaysAssert(itsMask, AipsError);

  TempLattice<Complex> maskFT(itsMask->shape(), itsMemoryMB);
  maskFT.copyData(LatticeExpr<Complex>(toComplex(*itsMask)));
  LatticeFFT::cfft2d(maskFT, True);

  // Now we can do all the convolutions
  TempLattice<Complex> cWork(itsScaleXfrs[0]->shape(), itsMemoryMB);
  for (scale=0; scale<itsNscales;scale++) {
    AlwaysAssert(itsScaleXfrs[scale], AipsError);
    os << "Calculating mask convolution for scale " << scale+1 << LogIO::POST;
    
    // Mask * scale
    LatticeExpr<Complex> maskExpr((maskFT)*(*itsScaleXfrs[scale]));
    cWork.copyData(maskExpr);
    LatticeFFT::cfft2d(cWork, False);
    // Allow only 10% overlap
    LatticeExpr<T> maskWork(iif(real(cWork)>0.9,1.0,0.0));
    itsScaleMasks[scale] = new TempLattice<T>(itsMask->shape(),
					      itsMemoryMB);
    AlwaysAssert(itsScaleMasks[scale], AipsError);
    itsScaleMasks[scale]->copyData(maskWork);

    LatticeExprNode LEN;
    LEN = sum( *itsScaleMasks[scale] );
    Float mysum = LEN.getFloat();
    if (mysum <= 0.1) {
      os << LogIO::WARN << "Ignoring scale " << scale+1 << 
	" since it is too large to fit within the mask" << LogIO::POST;
    }
    
  }

  return True;
};




template<class T> 
Float LatticeCleaner<T>::threshold()
{
  if (! itsDoSpeedup) {
    return max(itsFracThreshold.get("%").getValue() * itsMaximumResidual /100.0,
	       itsThreshold.get("Jy").getValue());
  } else {
    Float factor = exp( (Float)( itsIteration - itsStartingIter )/ itsNDouble )
      / 2.7182818;
    return factor * max(itsFracThreshold.get("%").getValue() * itsMaximumResidual /100.0,
		       itsThreshold.get("Jy").getValue());
  }
};

template<class T>
void LatticeCleaner<T>::addTo(Lattice<T>& to, const Lattice<T>& add)
{
  // Check the lattice is writable.
  // Check the shape conformance.
  AlwaysAssert (to.isWritable(), AipsError);
  const IPosition shapeIn  = add.shape();
  const IPosition shapeOut = to.shape();
  AlwaysAssert (shapeIn.isEqual (shapeOut), AipsError);
  IPosition cursorShape = to.niceCursorShape();
  LatticeStepper stepper (shapeOut, cursorShape, LatticeStepper::RESIZE);
  LatticeIterator<T> toIter(to, stepper);
  RO_LatticeIterator<T> addIter(add, stepper);
  for (addIter.reset(), toIter.reset(); !addIter.atEnd();
       addIter++, toIter++) {
    toIter.rwCursor()+=addIter.cursor();
  }
}

template <class T>
void LatticeCleaner<T>::makeBoxesSameSize(IPosition& blc1, IPosition& trc1, 
                  IPosition &blc2, IPosition& trc2)
{
  const IPosition shape1 = trc1 - blc1;
  const IPosition shape2 = trc2 - blc2;

  AlwaysAssert(shape1.nelements() == shape2.nelements(), AipsError);
  
  if (shape1 == shape2) {
      return;
  }
  for (uInt i=0;i<shape1.nelements();++i) {
       Int minLength = shape1[i];
       if (shape2[i]<minLength) {
           minLength = shape2[i];
       }
       AlwaysAssert(minLength>=0, AipsError);
       //if (minLength % 2 != 0) {
           // if the number of pixels is odd, ensure that the centre stays 
           // the same by making this number even
           //--minLength; // this code is a mistake and should be removed
       //}
       const Int increment1 = shape1[i] - minLength;
       const Int increment2 = shape2[i] - minLength;
       blc1[i] += increment1/2;
       trc1[i] -= increment1/2 + (increment1 % 2 != 0 ? 1 : 0);
       blc2[i] += increment2/2;
       trc2[i] -= increment2/2 + (increment2 % 2 != 0 ? 1 : 0);
  }
}


} //# NAMESPACE CASA - END

