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

#include <trial/Lattices/LatticeConvolver.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Tasking/AppInfo.h>

template<class T> LatticeConvolver<T>::
LatticeConvolver()
  :itsPsfShape(IPosition(1,1)),
   itsModelShape(IPosition(1,0)),
   itsFFTShape(IPosition(1,1)),
   itsXfr(itsFFTShape),
   itsPsf(),
   itsCachedPsf(False)
{
  itsXfr.set(NumericTraits<T>::ConjugateType(0));
  IPosition XFRShape = itsFFTShape/2;
  XFRShape(0) = 0;
  itsXfr.putAt(NumericTraits<T>::ConjugateType(1), XFRShape);
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf)
  :itsPsfShape(psf.shape()),
   itsModelShape(psf.ndim(), 0),
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
  const uInt ndim = itsFFTShape.nelements();
  AlwaysAssert(result.ndim() == ndim, AipsError);
  AlwaysAssert(model.ndim() == ndim, AipsError);
  // The following restrictions will be relaxed when the LatticeConvolver knows
  // how to resize itself.
  const IPosition modelShape = model.shape();
  AlwaysAssert(modelShape == itsModelShape, AipsError);
  const IPosition resultShape = result.shape();
  AlwaysAssert(resultShape == modelShape, AipsError);
  // Deteremine if we have enough free memoery to avoid disk based temporaries
  const uInt freeMem = AppInfo::availableMemoryInMB();
  const uInt reqMem = modelShape.product()*sizeof(T)/1024/1024;
  // Copy the model into a larger Lattice than has the appropriate padding.
  // If memory is short make this Lattice disk based and the Complex one that
  // will shortly be created memory based.
  TempLattice<T> paddedModel(itsFFTShape, freeMem-2*reqMem);
  pad(paddedModel, model);
  // Create a lattice that will hold the transform
  IPosition XFRShape(itsFFTShape);
  XFRShape(0) = (XFRShape(0)+2)/2;
  TempLattice<NumericTraits<T>::ConjugateType> fftModel(XFRShape, 
							freeMem-reqMem);
  // Do the forward transform
  LatticeFFT::rcfft(fftModel.lc(), paddedModel.lc());
  { // Multiply the transformed model with the transfer function
    LatticeExpr<Complex> product(fftModel*itsXfr);
    fftModel.copyData(product);
  } 
  // Do the inverse transform
  LatticeFFT::crfft(paddedModel.lc(), fftModel.lc());
  // Unpad the result
  unpad(result, paddedModel);
}

template<class T> void LatticeConvolver<T>::
linear(Lattice<T> & modelAndResult){
  linear(modelAndResult, modelAndResult);
}

template<class T> void LatticeConvolver<T>::
circular(Lattice<T> & result, const Lattice<T> & model) {
}

template<class T> void LatticeConvolver<T>::
circular(Lattice<T> & modelAndResult){
  circular(modelAndResult, modelAndResult);
}

template<class T> void LatticeConvolver<T>::
convolve(Lattice<T> & modelAndResult) const {
  
}

template<class T> void LatticeConvolver<T>::
resize(const IPosition & modelShape) {
  const uInt ndim = itsXfr.ndim();
  AlwaysAssert(ndim == modelShape.nelements(), AipsError);
  {
    const IPosition newFFTShape = calcFFTShape(itsPsfShape, modelShape);
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
fftShape() const {
  return itsFFTShape;
}

template<class T> IPosition LatticeConvolver<T>::
psfShape() const {
  return itsPsfShape;
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

template<class T> LatticeConvolver<T>::
LatticeConvolver(const LatticeConvolver<T> & other)
  :itsPsfShape(other.itsPsfShape),
   itsModelShape(other.itsModelShape),
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
    itsFFTShape = other.itsFFTShape;
    itsXfr = other.itsXfr;
    itsPsf = other.itsPsf;
    itsCachedPsf = other.itsCachedPsf;
  }
  return *this;
}

template<class T> void LatticeConvolver<T>::
makeXfr(const Lattice<T> & psf) {
  DebugAssert(itsPsfShape == psf.shape(), AipsError);
  itsFFTShape = calcFFTShape(itsPsfShape, itsModelShape);
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

template<class T> IPosition LatticeConvolver<T>::
calcFFTShape(const IPosition & psfShape, const IPosition & modelShape) {
  if (modelShape.product() == 0) return psfShape;
  // Now calculate the minimum fft size required. I spent a fair of of time
  // working out this formulae (empirically)!
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
