//# Copyright (C) 1997,1998,1999
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

#include <trial/Lattices/LatticeConvolver.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/Assert.h>

template<class T> LatticeConvolver<T>::
LatticeConvolver()
  :itsPsfShape(IPosition(1,1)),
   itsModelShape(itsPsfShape),
   itsType(ConvEnums::CIRCULAR),
   itsFFTShape(IPosition(1,1)),
   itsXfr(itsFFTShape),
   itsPsf(),
   itsCachedPsf(False)
{
  itsXfr.set(NumericTraits<T>::ConjugateType(1));
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf)
  :itsPsfShape(psf.shape()),
   itsModelShape(itsPsfShape),
   itsType(ConvEnums::CIRCULAR),
   itsFFTShape(psf.ndim(), 0),
   itsPsf(),
   itsCachedPsf(False)
{
  AlwaysAssert(itsPsfShape.product() != 0, AipsError);
  makeXfr(psf);
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape) 
  :itsPsfShape(psf.shape()),
   itsModelShape(modelShape),
   itsType(ConvEnums::LINEAR),
   itsFFTShape(psf.ndim(), 0),
   itsPsf(),
   itsCachedPsf(False)
{
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  AlwaysAssert(itsPsfShape.nelements() == itsModelShape.nelements(),AipsError);
  AlwaysAssert(itsPsfShape.product() != 0, AipsError);
  AlwaysAssert(itsModelShape.product() != 0, AipsError);
  // looks OK so make the transfer function
  makeXfr(psf);
}

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape,
		 ConvEnums::ConvType type) 
  :itsPsfShape(psf.shape()),
   itsModelShape(modelShape),
   itsType(type),
   itsFFTShape(psf.ndim(), 0),
   itsPsf(),
   itsCachedPsf(False)
{
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  AlwaysAssert(itsPsfShape.nelements() == itsModelShape.nelements(),AipsError);
  AlwaysAssert(itsPsfShape.product() != 0, AipsError);
  AlwaysAssert(itsModelShape.product() != 0, AipsError);
  // looks OK so make the psf
  makeXfr(psf);
}

template<class T> LatticeConvolver<T>::
LatticeConvolver(const LatticeConvolver<T> & other)
  :itsPsfShape(other.itsPsfShape),
   itsModelShape(other.itsModelShape),
   itsType(other.itsType),
   itsFFTShape(other.itsFFTShape),
   itsXfr(other.itsXfr),
   itsPsf(other.itsPsf),
   itsCachedPsf(other.itsCachedPsf)
{
}

template<class T> LatticeConvolver<T> & LatticeConvolver<T>::
operator=(const LatticeConvolver<T> & other) {
  if (this != &other) {
    itsModelShape = other.itsModelShape;
    itsPsfShape = other.itsPsfShape;
    itsType = other.itsType;
    itsFFTShape = other.itsFFTShape;
    itsXfr = other.itsXfr;
    itsPsf = other.itsPsf;
    itsCachedPsf = other.itsCachedPsf;
  }
  return *this;
}

template<class T> LatticeConvolver<T>::
~LatticeConvolver()
{
}

template<class T> void LatticeConvolver<T>::
getPsf(Lattice<T> & psf) const {
  AlwaysAssert(psf.ndim() == itsPsfShape.nelements(), AipsError);
  AlwaysAssert(psf.shape() == itsPsfShape, AipsError);
  if (itsCachedPsf) { // used the cached Psf if possible
    itsPsf.copyDataTo(psf);
  } else { // reconstruct the psf from the transfer function
    makePsf(psf);
  }
}

template<class T> void LatticeConvolver<T>::
linear(Lattice<T> & result, const Lattice<T> & model) {
  resize(model.shape(), ConvEnums::LINEAR);
  convolve(result, model);
}

template<class T> void LatticeConvolver<T>::
linear(Lattice<T> & modelAndResult){
  linear(modelAndResult, modelAndResult);
}

template<class T> void LatticeConvolver<T>::
circular(Lattice<T> & result, const Lattice<T> & model) {
  resize(model.shape(), ConvEnums::CIRCULAR);
  convolve(result, model);
}

template<class T> void LatticeConvolver<T>::
circular(Lattice<T> & modelAndResult){
  circular(modelAndResult, modelAndResult);
}

template<class T> void LatticeConvolver<T>::
convolve(Lattice<T> & result, const Lattice<T> & model) const {
  const uInt ndim = itsFFTShape.nelements();
  AlwaysAssert(result.ndim() == ndim, AipsError);
  AlwaysAssert(model.ndim() == ndim, AipsError);
  const IPosition modelShape = model.shape();
  const IPosition resultShape = result.shape();
  AlwaysAssert(resultShape == modelShape, AipsError);
  AlwaysAssert(modelShape == itsModelShape, AipsError);
  // Create a lattice that will hold the transform. Do this before creating the
  // paddedModel TempLattice so that it is more likely to be memory based.
  IPosition XFRShape(itsFFTShape);
  XFRShape(0) = (XFRShape(0)+2)/2;
  TempLattice<NumericTraits<T>::ConjugateType> fftModel(XFRShape);
  // Copy the model into a larger Lattice that has the appropriate padding.
  // (if necessary)
  Bool doPadding = False;
  const Lattice<T> * modelPtr = &model;
  Lattice<T> * resultPtr = &result;
  if (!(itsFFTShape <= modelShape)) {
    doPadding = True;
    modelPtr = resultPtr = new TempLattice<T>(itsFFTShape);
    pad(*resultPtr, model);
  } 
  // Do the forward transform
  LatticeFFT::rcfft(fftModel.lc(), *modelPtr);
  { // Multiply the transformed model with the transfer function
    LatticeExpr<Complex> product(fftModel*itsXfr); 
    // The LatticeExpr temporary is needed to work around a bug in egcs-1.0.2
    fftModel.copyData(product);
  } 
  // Do the inverse transform
  LatticeFFT::crfft(*resultPtr, fftModel.lc());
  if (doPadding) { // Unpad the result
    unpad(result, *resultPtr);
    delete modelPtr;
    modelPtr = resultPtr = 0;
  }
}

template<class T> void LatticeConvolver<T>::
convolve(Lattice<T> & modelAndResult) const {
  convolve(modelAndResult, modelAndResult);
}

template<class T> void LatticeConvolver<T>::
resize(const IPosition & modelShape, ConvEnums::ConvType type) {
  const uInt ndim = itsXfr.ndim();
  AlwaysAssert(ndim == modelShape.nelements(), AipsError);
  itsType = type;
  itsModelShape = modelShape;
  {
    const IPosition newFFTShape = 
      calcFFTShape(itsPsfShape, modelShape, itsType);
    if (newFFTShape == itsFFTShape) return;
  }
  // need to know the psf.
  TempLattice<T> psf = itsPsf;
  if (itsCachedPsf == False) { // calculate the psf from the transfer function
    psf = TempLattice<T>(itsPsfShape);
    makePsf(psf);
  }
  makeXfr(psf);
}

template<class T> IPosition LatticeConvolver<T>::
shape() const {
  return itsModelShape;
}

template<class T> IPosition LatticeConvolver<T>::
psfShape() const {
  return itsPsfShape;
}

template<class T> IPosition LatticeConvolver<T>::
fftShape() const {
  return itsFFTShape;
}

template<class T> ConvEnums::ConvType LatticeConvolver<T>::
type() const {
  return itsType;
}

// copy the centre portion of the input Lattice to the padded Lattice. No
// assumptions are made about the padded Lattice except that it is the right
// shape (including the correct number of dimensions). 
template<class T> void LatticeConvolver<T>::
pad(Lattice<T> & paddedLat, const Lattice<T> & inLat) {
  paddedLat.set(T(0));
  const uInt ndim = inLat.ndim();
  const IPosition inLatShape = inLat.shape();
  const IPosition FFTShape = paddedLat.shape();
  IPosition inBlc(ndim, 0);
  IPosition patchShape(inLatShape);
  for (uInt k = 0; k < ndim; k++) {
    if (FFTShape(k) < inLatShape(k)) {
      inBlc(k) = inLatShape(k)/2 - FFTShape(k)/2;
      patchShape(k) = FFTShape(k);
    }
  }
  const Slicer inLatSlice(inBlc, patchShape);
  const SubLattice<T> inLatPatch(inLat, inLatSlice); 
  const IPosition outBlc = FFTShape/2 - patchShape/2;
  const Slicer paddedSlice(outBlc, patchShape);
  SubLattice<T> paddedPatch(paddedLat, paddedSlice, True); 
  paddedPatch.copyData(inLatPatch);
}

template<class T> void LatticeConvolver<T>::
unpad(Lattice<T> & result, const Lattice<T> & paddedResult) {
  const IPosition resultShape = result.shape();
  const IPosition inBlc = paddedResult.shape()/2 - resultShape/2;
  const Slicer paddedSlice(inBlc, resultShape);
  const SubLattice<T> resultPatch(paddedResult, paddedSlice); 
  result.copyData(resultPatch);
}

// Requires that the itsType, itsPsfShape and itsModelShape data members are
// initialised correctly and will initialise the itsFFTShape, itsXfr, itsPsf &
// itsCachedPsf data members.
template<class T> void LatticeConvolver<T>::
makeXfr(const Lattice<T> & psf) {
  DebugAssert(itsPsfShape == psf.shape(), AipsError);
  itsFFTShape = calcFFTShape(itsPsfShape, itsModelShape, itsType);
  { // calculate the transfer function
    IPosition XFRShape = itsFFTShape;
    XFRShape(0) = (XFRShape(0)+2)/2;
    itsXfr = TempLattice<NumericTraits<T>::ConjugateType>(XFRShape);
    if (itsFFTShape == itsPsfShape) { // no need to pad the psf
      LatticeFFT::rcfft(itsXfr, psf);
    } else { // need to pad the psf 
      TempLattice<T> paddedPsf(itsFFTShape);
      pad(paddedPsf, psf);
      LatticeFFT::rcfft(itsXfr, paddedPsf);
    }
  }
  // Only cache the psf if it cannot be reconstructed from the transfer
  // function.
  if (itsFFTShape < itsPsfShape) {
    itsPsf = TempLattice<T>(itsPsfShape, 1); // Prefer to put this on disk
    itsPsf.copyData(psf);
    itsCachedPsf = True;
  } else {
    itsPsf = TempLattice<T>();
    itsCachedPsf = False;
  }
}

// Construct a psf from the transfer function (itsXFR).
template<class T> void LatticeConvolver<T>::
makePsf(Lattice<T> & psf) const {
  DebugAssert(itsPsfShape == psf.shape(), AipsError);
  if (itsFFTShape == itsPsfShape) { // If the Transfer function has not been
                                    // padded so no unpadding is necessary 
    LatticeFFT::crfft(psf, itsXfr);
  } else { // need to unpad the transfer function
    TempLattice<T> paddedPsf(itsFFTShape);
    LatticeFFT::crfft(paddedPsf, itsXfr);
    unpad(psf, paddedPsf);
  }
}

// Calculate the minimum FFTShape necessary to do a convolution of the
// specified type with the supplied mode and psf shapes. Will try and avoid odd
// length FFT's.
template<class T> IPosition LatticeConvolver<T>::
calcFFTShape(const IPosition & psfShape, const IPosition & modelShape,
	     ConvEnums::ConvType type) {
  if (type == ConvEnums::CIRCULAR) {
    // All the books (eg Bracewell) only define circular convolution for two
    // Arrays that are the same length. So I always pad the smaller one to make
    // it the same size as the bigger one.
    return max(psfShape, modelShape);
  }
  // When doing linear convolution the formulae is more complicated.  In
  // general the shape is given by modelShape + psfShape - 1. But if we are
  // only to return an Array of size modelShape you can do smaller
  // transforms. I deduced the following formulae empirically.
  IPosition FFTShape = modelShape + psfShape/2;
  const uInt ndim = FFTShape.nelements();
  for (uInt i = 0; i < ndim; i++) {
    if (FFTShape(i) < psfShape(i)) {
      FFTShape(i) = 2 * modelShape(i) - 1;
    }
  }
  return FFTShape;
}

// Local Variables: 
// compile-command: "cd test; gmake OPTLIB=1 inst tLatticeConvolver"
// End: 
