//# LatticeConvolver.cc:
//# Copyright (C) 1997
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
  :itsFFTShape(IPosition(1,1)),
   itsXfr(itsFFTShape)
{
  itsXfr.set(NumericTraits<T>::ConjugateType(0));
  IPosition XFRShape = itsFFTShape/2;
  XFRShape(0) = 0;
  itsXfr.putAt(NumericTraits<T>::ConjugateType(1), XFRShape);
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf)
  :itsFFTShape(psf.shape())
{
  IPosition XFRShape = itsFFTShape;
  XFRShape(0) = (XFRShape(0) + 2)/2;
  itsXfr = TempLattice<NumericTraits<T>::ConjugateType>(XFRShape);
  LatticeFFT::rcfft(itsXfr, psf);
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf, const IPosition & imageShape) {
  const uInt ndim = psf.ndim();
  const IPosition & psfShape = psf.shape();
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  AlwaysAssert(ndim == imageShape.nelements(), AipsError);
  AlwaysAssert(psfShape.product() != 0, AipsError);
  AlwaysAssert(imageShape.product() != 0, AipsError);
  // Now calculate the minimum fft size required. I spent a fair of of time
  // working out this formulae (empirically)!
  itsFFTShape = imageShape + psfShape/2;
  for (uInt i = 0; i < ndim; i++) {
    if (itsFFTShape(i) < psfShape(i)) {
      itsFFTShape(i) = 2 * imageShape(i) - 1;
    }
  }
  // Now copy the psf into the padded psf, prior to calculating the transfer
  // function. 
  TempLattice<T> paddedPsf;
  pad(paddedPsf, psf);
  IPosition XFRShape(itsFFTShape);
  XFRShape(0) = (XFRShape(0)+2)/2;
  itsXfr = TempLattice<NumericTraits<T>::ConjugateType>(XFRShape);
  LatticeFFT::rcfft(itsXfr, paddedPsf);
}

template<class T> LatticeConvolver<T>::
~LatticeConvolver()
{
}

template<class T> void LatticeConvolver<T>::
getPsf(Lattice<T> & psf) const {
  LatticeFFT::crfft(psf, itsXfr);
}

template<class T> void LatticeConvolver<T>::
linear(Lattice<T> & result, const Lattice<T> & model) {
  TempLattice<T> padded;
  pad(padded, model);
  IPosition XFRShape(itsFFTShape);
  XFRShape(0) = (XFRShape(0)+2)/2;
  TempLattice<NumericTraits<T>::ConjugateType> fftModel(XFRShape);
  LatticeFFT::rcfft(fftModel, padded);
  fftModel.copyData(fftModel*itsXfr);
  LatticeFFT::crfft(padded.lc(), fftModel.lc());
  unpad(result, padded);
}

template<class T> void LatticeConvolver<T>::
pad(TempLattice<T> & paddedModel, const Lattice<T> & model) const {
  const uInt ndim = model.ndim();
  const IPosition & modelShape = model.shape();
  IPosition inBlc(ndim, 0), patchShape(modelShape);
  for (uInt k = 0; k < ndim; k++) {
    if (itsFFTShape(k) < modelShape(k)) {
      inBlc(k) = modelShape(k)/2 - itsFFTShape(k)/2;
      patchShape(k) = itsFFTShape(k);
    }
  }
  const Slicer modelSlice(inBlc, patchShape);
  const SubLattice<T> modelPatch(model, modelSlice); 
  const IPosition outBlc = itsFFTShape/2 - patchShape/2;
  const Slicer paddedSlice(outBlc, patchShape);
  paddedModel = TempLattice<T>(itsFFTShape);
  paddedModel.set(0.0f);
  SubLattice<T> paddedPatch(paddedModel, paddedSlice, True); 
  paddedPatch.copyData(modelPatch);
}

template<class T> void LatticeConvolver<T>::
unpad(Lattice<T> & result, const TempLattice<T> & paddedResult) const {
  const IPosition & resultShape = result.shape();
  const IPosition inBlc = itsFFTShape/2 - resultShape/2;
  const Slicer paddedSlice(inBlc, resultShape);
  const SubLattice<T> resultPatch(paddedResult, paddedSlice); 
  result.copyData(resultPatch);
}
// Local Variables: 
// compile-command: "(cd test; gmake OPTLIB=1 tLatticeConvolver)"
// End: 
