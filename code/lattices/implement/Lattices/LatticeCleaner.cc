//# Copyright (C) 1997,1998
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
#include <trial/Lattices/TiledLineStepper.h> 
#include <trial/Lattices/LatticeStepper.h> 
#include <trial/Lattices/LatticeNavigator.h> 
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/TempLattice.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/CopyLattice.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Lattices/Slicer.h>

#include <aips/Tasking/AppInfo.h>
#include <trial/Tasking/ApplicationEnvironment.h>
#include <trial/Tasking/PGPlotter.h>
#include <trial/Tasking/ObjectController.h>

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
  Float maxPsf=0;
  itsPositionPeakPsf=IPosition(psf.shape().nelements(), 0);
  findMaxAbsLattice(psf, maxPsf, itsPositionPeakPsf);
  os << "Peak of PSF = " << maxPsf << " at " << itsPositionPeakPsf+1
     << LogIO::POST;
  return True;
}
  

template<class T> 
LatticeCleaner<T>::LatticeCleaner(const Lattice<T> & psf,
				  const Lattice<T> &dirty):
  itsChoose(True)
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
  // in all.
  itsMemoryMB=AppInfo::memoryInMB()/32;

  itsDirty = new TempLattice<T>(dirty.shape(), itsMemoryMB);
  itsDirty->copyData(dirty);
  itsXfr=new TempLattice<Complex>(psf.shape(), itsMemoryMB);
  convertLattice(*itsXfr,psf);
  LatticeFFT::cfft(*itsXfr, True);

  itsScales.resize(0);
  itsDirtyConvScales.resize(0);
  itsPsfConvScales.resize(0);

}

template <class T> LatticeCleaner<T>::
LatticeCleaner(const LatticeCleaner<T> & other):
   itsCleanType(other.itsCleanType),
   itsDirty(other.itsDirty),
   itsXfr(other.itsXfr),
   itsScales(other.itsScales),
   itsPsfConvScales(other.itsPsfConvScales),
   itsDirtyConvScales(other.itsDirtyConvScales)
{
}

template<class T> LatticeCleaner<T> & LatticeCleaner<T>::
operator=(const LatticeCleaner<T> & other) {
  if (this != &other) {
    itsCleanType = other.itsCleanType;
    itsXfr = other.itsXfr;
    itsDirty = other.itsDirty;
    itsScales = other.itsScales;
    itsPsfConvScales = other.itsPsfConvScales;
    itsDirtyConvScales = other.itsDirtyConvScales;
  }
  return *this;
}

template<class T> LatticeCleaner<T>::
~LatticeCleaner()
{
  destroyScales();
  if(itsDirty) delete itsDirty;
  if(itsXfr) delete itsXfr;
}

// Set up the control parameters
template <class T>
Bool LatticeCleaner<T>::setcontrol(CleanEnums::CleanType cleanType,
				   const Int niter,
				   const Float gain,
				   const Quantity& threshold,
				   const Bool choose)
{
  itsCleanType=cleanType;
  itsNiter=niter;
  itsGain=gain;
  itsThreshold=threshold;
  itsChoose=choose;
  return True;
}

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
  }
  else if (itsCleanType==CleanEnums::MULTISCALE) {
    if (itsNscales==1) {
      os << "Multi-scale Clean with only one scale" << LogIO::POST;
    }
    else {
      os << "Multi-scale Clean algorithm" << LogIO::POST;
    }
  }

  AlwaysAssert(itsScalesValid, AipsError);

  // Find the peaks of the convolved Psfs
  Vector<T> maxPsfConvScales(nScalesToClean);
  for (Int scale=0;scale<nScalesToClean;scale++) {
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
  }

  // Define a subregion for the inner quarter
  IPosition blcDirty(model.shape().nelements(), 0);
  IPosition trcDirty(model.shape()-1);
  for (Int i=0;i<Int(model.shape().nelements());i++) {
    blcDirty(i)=model.shape()(i)/4;
    trcDirty(i)=blcDirty(i)+model.shape()(i)/2-1;
    if(trcDirty(i)<0) trcDirty(i)=1;
  }
  LCBox centerBox(blcDirty, trcDirty, model.shape());

  // Start the iteration
  Vector<Float> maxima(itsNscales);
  Block<IPosition> posMaximum(itsNscales);
  Vector<Float> totalFluxScale(itsNscales); totalFluxScale=0.0;
  Float totalFlux=0.0;
  Bool converged=False;
  Int optimumScale=0;
  Float strengthOptimum=0.0;
  IPosition positionOptimum(model.shape().nelements(), 0);
  os << "Starting iteration"<< LogIO::POST;

  for (Int iteration=0; iteration < itsNiter; iteration++) {
    // Find the peak
    strengthOptimum = 0.0;
    optimumScale = 0;
    for (scale=0; scale<nScalesToClean; scale++) {
      // Find absolute maximum for the dirty image
      SubLattice<T> dirtySub(*itsDirtyConvScales[scale], centerBox);
      maxima(scale)=0;
      posMaximum[scale]=IPosition(model.shape().nelements(), 0);
      findMaxAbsLattice(dirtySub, maxima(scale), posMaximum[scale]);
      // Remember to adjust the position for the window and for 
      // the flux scale
      maxima(scale)/=maxPsfConvScales(scale);
      posMaximum[scale]+=blcDirty;
      if(abs(maxima(scale))>abs(strengthOptimum)) {
        optimumScale=scale;
        strengthOptimum=maxima(scale);
	positionOptimum=posMaximum[scale];
      }
    }

    totalFlux += strengthOptimum;
    totalFluxScale(optimumScale) += strengthOptimum;

    // Various ways of stopping:
    //    1. stop if below threshold
    if(abs(strengthOptimum)<itsThreshold.get("Jy").getValue()) {
      os << "Reached stopping threshold" << LogIO::POST;
      converged = True;
      break;
    }
    //    2. call back: the return value tells us to stop
    if(progress) {
      if(progress->info(False, iteration, itsNiter, model, maxima,
			posMaximum, strengthOptimum,
			optimumScale, positionOptimum,
			totalFlux, totalFluxScale,
			itsDirtyConvScales)) break;
    }
    //    3. try the stopnow method after every 10% of the iterations
    if(iteration%(itsNiter/10)==0) {
      if(stopnow()) break;
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
    Float scaleFactor;
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

  if(progress) {
    progress->info(True, iteration, itsNiter, model, maxima, posMaximum,
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
      Float maxVal=0.0;
      Float minVal=0.0;
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
      scaleInc * pow(10.0, (Float(scale)-Float(itsNscales/2))/2.0);
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

  itsNscales=scaleSizes.nelements();
  
  if(itsScales.nelements()>0) {
    destroyScales();
  }

  itsScales.resize(itsNscales);
  itsDirtyConvScales.resize(itsNscales);
  itsPsfConvScales.resize((itsNscales+1)*(itsNscales+1));

  AlwaysAssert(itsDirty, AipsError);

  TempLattice<Complex> dirtyFT(itsDirty->shape(), itsMemoryMB);
  convertLattice(dirtyFT, *itsDirty);
  LatticeFFT::cfft(dirtyFT, True);

  PtrBlock<TempLattice<Complex> *> scaleXfr(itsNscales);

  for (Int scale=0; scale<itsNscales;scale++) {
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
    LatticeFFT::cfft(*scaleXfr[scale], True);
  }
    
  // Now we can do all the convolutions
  TempLattice<Complex> cWork(itsDirty->shape(), itsMemoryMB);
  for (scale=0; scale<itsNscales;scale++) {
    os << "Calculating convolutions for scale " << scale+1 << LogIO::POST;
    
    // PSF * PSF * scale
    LatticeExpr<Complex> ppsExpr(conj(*itsXfr)*(*itsXfr)*(*scaleXfr[scale]));
    cWork.copyData(ppsExpr);
    LatticeFFT::cfft(cWork, False);
    itsPsfConvScales[scale] = new TempLattice<T>(itsDirty->shape(),  itsMemoryMB);
    AlwaysAssert(itsPsfConvScales[scale], AipsError);
    LatticeExpr<Float> realWork(real(cWork));
    itsPsfConvScales[scale]->copyData(realWork);
    
    // Dirty * PSF * scale
    LatticeExpr<Complex> dpsExpr(conj(*itsXfr)*(dirtyFT)*(*scaleXfr[scale]));
    cWork.copyData(dpsExpr);
    LatticeFFT::cfft(cWork, False);
    itsDirtyConvScales[scale] = new TempLattice<T>(itsDirty->shape(), itsMemoryMB);
    AlwaysAssert(itsDirtyConvScales[scale], AipsError);
    itsDirtyConvScales[scale]->copyData(realWork);

    for (Int otherscale=scale;otherscale<itsNscales;otherscale++) {
      
      AlwaysAssert(index(scale, otherscale)<Int(itsPsfConvScales.nelements()),
		   AipsError);
      
      // PSF * PSF * scale * otherscale
      LatticeExpr<Complex> ppsoExpr(conj(*itsXfr)*(*itsXfr)*conj(*scaleXfr[scale])*(*scaleXfr[otherscale]));
      cWork.copyData(ppsoExpr);
      LatticeFFT::cfft(cWork, False);
      itsPsfConvScales[index(scale,otherscale)] =
	new TempLattice<T>(itsDirty->shape(), itsMemoryMB);
      AlwaysAssert(itsPsfConvScales[index(scale,otherscale)], AipsError);
      itsPsfConvScales[index(scale,otherscale)]->copyData(realWork);
    }
  }
  itsScalesValid=True;
  for(scale=0; scale<itsNscales;scale++) {
    if(scaleXfr[scale]) delete scaleXfr[scale];
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
    Double sb=4.0*log(2.0)/square(scaleSize);
    Float volume=0.0;
    for (Int j=0;j<ny;j++) {
      for (Int i=0;i<nx;i++) {
	Double radius = sb*(square(i-refi) + square(j-refj));
	if (radius<20.) {
	  iscale(i,j) = exp(-radius);
	  volume+=iscale(i,j);
	}
      }
    }
    iscale.ac()/=volume;
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
  for(scale=0; scale<itsDirtyConvScales.nelements();scale++) {
    if(itsDirtyConvScales[scale]) delete itsDirtyConvScales[scale];
    itsDirtyConvScales[scale]=0;
  }
  for(scale=0; scale<itsPsfConvScales.nelements();scale++) {
    if(itsPsfConvScales[scale]) delete itsPsfConvScales[scale];
    itsPsfConvScales[scale]=0;
  }
  itsScales.resize(0);
  itsDirtyConvScales.resize(0);
  itsPsfConvScales.resize(0);
  itsScalesValid=False;
  return True;
}

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

