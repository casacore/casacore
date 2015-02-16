//# Convolver.cc:  this defines Convolver a class for doing convolution
//# Copyright (C) 1996,1997,1999,2003
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

#ifndef SCIMATH_CONVOLVER_TCC
#define SCIMATH_CONVOLVER_TCC

#include <casacore/scimath/Mathematics/Convolver.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class FType> Convolver<FType>::
Convolver(const Array<FType>& psf, Bool){
  //  if (cachePsf) thePsf = psf;
  thePsf = psf;
  valid = False;
  doFast_p=False;
}

template<class FType> Convolver<FType>::
Convolver(const Array<FType>& psf, 
	  const IPosition&,
	  Bool,
	  Bool){
  //  if (cachePsf) thePsf = psf;
  thePsf = psf;
  valid = False;
  doFast_p=False;
}

template<class FType> Convolver<FType>::
Convolver(const Convolver<FType>& other){
  thePsfSize = other.thePsfSize;
  theFFTSize = other.theFFTSize;
  theXfr = other.theXfr;
  thePsf = other.thePsf;
  theFFT = other.theFFT;
  theIFFT = other.theIFFT;
  doFast_p=False;
}

template<class FType> Convolver<FType> & 
Convolver<FType>::operator=(const Convolver<FType> & other){
  if (this != &other) {
    thePsfSize.resize(other.thePsfSize.nelements(), False);
    thePsfSize = other.thePsfSize;
    theFFTSize.resize(other.theFFTSize.nelements(), False);
    theFFTSize = other.theFFTSize;
    theXfr.resize(other.theXfr.shape());
    theXfr = other.theXfr;
    thePsf.resize(other.thePsf.shape());
    thePsf = other.thePsf;
    theFFT = other.theFFT;
    theIFFT = other.theIFFT;
    doFast_p=False;
  }
  return *this;
} 

template<class FType> Convolver<FType>::
~Convolver(){}

template<class FType>
void Convolver<FType>::validate() {
  if(!valid) {
    valid = True;
    makeXfr(thePsf, defaultShape(thePsf), False, False);
  }
}

template<class FType> IPosition Convolver<FType>::
defaultShape(const Array<FType>& psf){
  // If the user has not specified an image size assume that it is 
  // the same size as the psf. 
  return psf.shape().nonDegenerate();
}

template<class FType> IPosition Convolver<FType>::
extractShape(IPosition& psfSize, const IPosition& imageSize){
  // return an IPosition that has the same number of dimensions as the psf
  // but with the lengths of the image
  return imageSize.getFirst(psfSize.nonDegenerate().nelements());
}

template<class FType> void Convolver<FType>::
makeXfr(const Array<FType>& psf, 
	const IPosition& imageSize,
	Bool linear, Bool fullSize){

  const Array<FType> psfND1 = psf.nonDegenerate();
  Array<FType> psfND= psfND1.copy();
  thePsfSize = psfND.shape();
  IPosition imageNDSize = imageSize.nonDegenerate();
  uInt psfDim = thePsfSize.nelements();
  IPosition convImageSize = extractShape(thePsfSize, imageNDSize);
  theFFTSize.resize(psfDim);
  if (linear) 
    if (fullSize)
      theFFTSize = thePsfSize+extractShape(thePsfSize, imageNDSize);
    else
      for (uInt i = 0; i < psfDim; i++)
	theFFTSize(i) = std::max(thePsfSize(i), 
                                 convImageSize(i)+2*Int((thePsfSize(i)+3)/4));
  else 
    for (uInt i = 0; i < psfDim; i++)
      theFFTSize(i) = std::max(thePsfSize(i), convImageSize(i));
  {
    IPosition tmp = theXfr.shape();
    tmp = 0;
    theXfr.resize(tmp); // I am to lazy to work out the correct size
  }
  // Pad the psf (if necessary) 
  if (theFFTSize != thePsfSize){
    Array<FType> paddedPsf(theFFTSize);
    IPosition blc = theFFTSize/2-thePsfSize/2;
    IPosition trc = blc + thePsfSize - 1;
    paddedPsf = 0.;  
    paddedPsf(blc, trc) = psfND;
    // And do the fft
    if(doFast_p){
      //theFFT.flip(paddedPsf, True, False);
      theFFT.fft0(theXfr, paddedPsf, False);
    }
    else{
      theFFT.fft(theXfr, paddedPsf, False);
    }
  }
  else{
    if(doFast_p){
      //theFFT.flip(psfND, True, False);
      theFFT.fft0(theXfr, psfND);
    }
    else{
      theFFT.fft(theXfr, psfND);
    }

  }
}

template<class FType> void Convolver<FType>::
makePsf(Array<FType>& psf){
  validate();
  if (thePsf.nelements() == 0) {
    Array<FType> paddedPsf(theFFTSize);
    //    theIFFT.flip(paddedPsf, True, False);
    if(doFast_p){
      theIFFT.fft0(paddedPsf, theXfr, True);
      theIFFT.flip(paddedPsf, False, False);
    }
    else{
      theIFFT.fft(paddedPsf, theXfr, True);
    }
    IPosition trc, blc;
    blc = (theFFTSize-thePsfSize)/2;
    trc = blc + thePsfSize - 1;
    psf = paddedPsf(blc, trc);
  }
  else
    psf.reference(thePsf);
}

template<class FType> void Convolver<FType>::
linearConv(Array<FType>& result,
	   const Array<FType>& model,  
	   Bool fullSize) {
  validate();
  // Check the dimensions of the model are compatible with the current psf
  IPosition imageSize = extractShape(thePsfSize, model.shape());
  if (fullSize){
    if (imageSize+thePsfSize > theFFTSize){
      resizeXfr(imageSize, True, True);
    }
  }
  else {
    Bool doResize = False;
    for (uInt i = 0; i < thePsfSize.nelements(); i++) {
      if (theFFTSize < std::max(thePsfSize(i), 
                                imageSize(i)+2*Int((thePsfSize(i)+3)/4)))
	doResize=True;
    }
    if (doResize)
      resizeXfr(imageSize, True, False);
  }
  // Calculate to output array size
  IPosition resultSize = model.shape();
  if (fullSize)
    resultSize.setFirst(imageSize+thePsfSize-1);
  // create space in the output array to hold the data
  result.resize(resultSize);

  ReadOnlyArrayIterator<FType> from(model, thePsfSize.nelements());
  ArrayIterator<FType> to(result, thePsfSize.nelements());

  for (from.origin(), to.origin();
       (from.pastEnd() || to.pastEnd()) == False;
       from.next(), to.next()) {
    doConvolution(to.array(), from.array(), fullSize);
  }
}

template<class FType> void Convolver<FType>::
doConvolution(Array<FType>& result,
	      const Array<FType>& model,
	      Bool fullSize) {
  validate();
  IPosition modelSize = model.shape();
  Array<typename NumericTraits<FType>::ConjugateType> fftModel;
  if (theFFTSize != modelSize){
    // Pad the model
    Array<FType> paddedModel(theFFTSize);
    IPosition blc = (theFFTSize-modelSize)/2;
    IPosition trc = blc + modelSize - 1;
    paddedModel = 0.;
    paddedModel(blc, trc) = model;
    // And calculate its transform
    //    theFFT.flip(paddedModel, True, False);
    if(doFast_p){
      theFFT.fft0(fftModel, paddedModel);
    }
    else{
      theFFT.fft(fftModel, paddedModel);
    }
  }
  else{
    Array<FType> paddedModel=model;
    if(doFast_p){
      Array<FType> paddedModel=model;
      //    theFFT.flip(paddedModel, True, False);
      theFFT.fft0(fftModel, paddedModel);
    }
    else{
      theFFT.fft(fftModel, model);
    } 
  }
  // Multiply by the transfer function
  fftModel *= theXfr;

  // Do the inverse transform
  Array<FType> convolvedData(theFFTSize);
  if(doFast_p){
    theIFFT.fft0(convolvedData, fftModel);
    theIFFT.flip(convolvedData, False, False);
  }
  else{
    theIFFT.fft(convolvedData, fftModel);
  }
  // Extract the required part of the convolved data
  IPosition trc, blc; 
  if (fullSize) {
    blc = IPosition(thePsfSize.nelements(), 0);
    trc = thePsfSize + modelSize - 2;
  }
  else {
    blc = (theFFTSize-modelSize)/2;
    trc = blc + modelSize - 1;
  }
  result = convolvedData(blc, trc);
}
  
template<class FType> void Convolver<FType>::
setPsf(const Array<FType>& psf, Bool){
  thePsf.resize(psf.shape());
  thePsf = psf;
  valid=False;
  doFast_p=False;
}
  
template<class FType> void Convolver<FType>::
setPsf(const Array<FType>& psf, 
       IPosition, 
       Bool,
       Bool){
  thePsf.resize(psf.shape());
  thePsf = psf;
  valid=False;
  doFast_p=False;
}

template<class FType> void Convolver<FType>::
resizeXfr(const IPosition& imageSize, 
	  Bool linear,
	  Bool fullSize){
  Array<FType> psf;
  makePsf(psf);
  makeXfr(psf, imageSize, linear, fullSize);
}

template<class FType> void Convolver<FType>::
circularConv(Array<FType>& result, 
	     const Array<FType>& model){
  // Check the dimensions of the model are compatible with the current psf
  validate();
  IPosition imageSize = extractShape(thePsfSize, model.shape());
  if (max(imageSize.asVector(), 
	  thePsfSize.asVector()) 
      != theFFTSize){
    resizeXfr(model.shape(), False, False);
  }
  // create space in the output array to hold the data
  result.resize(model.shape());

  ReadOnlyArrayIterator<FType> from(model, thePsfSize.nelements());
  ArrayIterator<FType> to(result, thePsfSize.nelements());

  for (from.origin(), to.origin();
       (from.pastEnd() || to.pastEnd()) == False;
       from.next(), to.next()) {
    doConvolution(to.array(), from.array(), False);
  }
}

template<class FType> const Array<FType> Convolver<FType>::
getPsf(Bool cachePsf){
  validate();
  Array<FType> psf;
  makePsf(psf);
  if ((cachePsf == True) && (thePsf.nelements() == 0))
    thePsf.reference(psf);
  return psf;
}
template<class FType> void Convolver<FType>::
setFastConvolve(){
  doFast_p=True;

}

} //# NAMESPACE CASACORE - END


#endif
