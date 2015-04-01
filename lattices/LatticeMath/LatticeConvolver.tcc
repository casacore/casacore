// -*- C++ -*-
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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

#ifndef LATTICES_LATTICECONVOLVER_TCC
#define LATTICES_LATTICECONVOLVER_TCC

#include <casacore/lattices/LatticeMath/LatticeConvolver.h>
#include <casacore/lattices/LatticeMath/LatticeFFT.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/TileStepper.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

const Int maxLatSize = HostInfo::memoryTotal()/1024/8;

template<class T> LatticeConvolver<T>::
LatticeConvolver()
  :itsPsfShape(IPosition(1,1)),
   itsModelShape(itsPsfShape),
   itsType(ConvEnums::CIRCULAR),
   itsFFTShape(IPosition(1,1)),
   itsXfr(0),
   itsPsf(0),
   itsCachedPsf(False)
{
  itsXfr->set(typename NumericTraits<T>::ConjugateType(1));
  doFast_p=False;
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf, Bool doFast)
  :itsPsfShape(psf.shape()),
   itsModelShape(itsPsfShape),
   itsType(ConvEnums::CIRCULAR),
   itsFFTShape(psf.ndim(), 0),
   itsXfr(0),
   itsPsf(0),
   itsCachedPsf(False)
{
  DebugAssert(itsPsfShape.product() != 0, AipsError);
  doFast_p=doFast;
  makeXfr(psf);
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape, 
		 Bool doFast) 
  :itsPsfShape(psf.shape()),
   itsModelShape(modelShape),
   itsType(ConvEnums::LINEAR),
   itsFFTShape(psf.ndim(), 0),
   itsXfr(0),
   itsPsf(0),
   itsCachedPsf(False)
{
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  DebugAssert(itsPsfShape.nelements() == itsModelShape.nelements(),AipsError);
  DebugAssert(itsPsfShape.product() != 0, AipsError);
  DebugAssert(itsModelShape.product() != 0, AipsError);
  // looks OK so make the transfer function
  doFast_p=doFast;
  makeXfr(psf);
}

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape,
		 ConvEnums::ConvType type, Bool doFast) 
  :itsPsfShape(psf.shape()),
   itsModelShape(modelShape),
   itsType(type),
   itsFFTShape(psf.ndim(), 0),
   itsXfr(0),
   itsPsf(0),
   itsCachedPsf(False)
{
  // Check that everything is the same dimension and that none of the
  // dimensions is zero length.
  DebugAssert(itsPsfShape.nelements() == itsModelShape.nelements(),AipsError);
  DebugAssert(itsPsfShape.product() != 0, AipsError);
  DebugAssert(itsModelShape.product() != 0, AipsError);
  // looks OK so make the psf
  doFast_p=doFast;
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
    doFast_p=other.doFast_p;
  }
  return *this;
}

template<class T> LatticeConvolver<T>::
~LatticeConvolver()
{
  if(itsPsf) delete itsPsf; itsPsf=0;
  if(itsXfr) delete itsXfr; itsXfr=0;
}

template<class T> void LatticeConvolver<T>::
getPsf(Lattice<T> & psf) const {
  DebugAssert(psf.ndim() == itsPsfShape.nelements(), AipsError);
  DebugAssert(psf.shape() == itsPsfShape, AipsError);
  if (itsCachedPsf) { // used the cached Psf if possible
    itsPsf->copyDataTo(psf);
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
  //  cerr << "convolve: " << model.shape() << " " << itsXfr->shape() << endl;
  const uInt ndim = itsFFTShape.nelements();
  DebugAssert(result.ndim() == ndim, AipsError);
  DebugAssert(model.ndim() == ndim, AipsError);
  const IPosition modelShape = model.shape();
  DebugAssert(result.shape() == modelShape, AipsError);
  DebugAssert(modelShape == itsModelShape, AipsError);
  // Create a lattice that will hold the transform. Do this before creating the
  // paddedModel TempLattice so that it is more likely to be memory based.
  IPosition XFRShape(itsFFTShape);
  XFRShape(0) = (XFRShape(0)+2)/2;
  TempLattice<typename NumericTraits<T>::ConjugateType> fftModel(XFRShape,
								 maxLatSize);
  // Copy the model into a larger Lattice that has the appropriate padding.
  // (if necessary)
  Bool doPadding = False;
  const Lattice<T>* modelPtr = 0;
  Lattice<T>* resultPtr = 0;
  if (!(itsFFTShape <= modelShape)) {
    doPadding = True;
    resultPtr = new TempLattice<T>(itsFFTShape, maxLatSize);
    modelPtr = resultPtr;
  } 

  IPosition sliceShape(ndim,1);
  for (uInt n = 0; n < ndim; n++) {
    if (itsFFTShape(n) > 1) {
      sliceShape(n) = modelShape(n);
    }
  }
  LatticeStepper ls(modelShape, sliceShape);
  for (ls.reset(); !ls.atEnd(); ls++) {
    const Slicer sl(ls.position(), sliceShape);
    const SubLattice<Float> modelSlice(model, sl);
    SubLattice<Float> resultSlice(result, sl, True);
    if (doPadding) {
      pad(*resultPtr, modelSlice);
    } else {
      modelPtr = &modelSlice;
      resultPtr = &resultSlice;
    }
    // Do the forward transform

    LatticeFFT::rcfft(fftModel, *modelPtr, True, doFast_p);
    { // Multiply the transformed model with the transfer function
      IPosition tileShape(itsXfr->niceCursorShape());
      const IPosition otherTileShape(fftModel.niceCursorShape());
      for (uInt i = 0; i < ndim; i++) {
	if (tileShape(i) > otherTileShape(i)) tileShape(i) = otherTileShape(i);
      }
      TileStepper tiledNav(XFRShape, tileShape);
      RO_LatticeIterator<typename NumericTraits<T>::ConjugateType> 
	xfrIter(*itsXfr, tiledNav);
      LatticeIterator<typename NumericTraits<T>::ConjugateType> 
	fftModelIter(fftModel, tiledNav);
      for (xfrIter.reset(), fftModelIter.reset(); !fftModelIter.atEnd();
	   xfrIter++, fftModelIter++) {
	fftModelIter.rwCursor() *= xfrIter.cursor();
      }
    }
    // Do the inverse transform
    // We have done a fft with no shift to the psf and the incoming 
    // image to be convolved now we fft back and shift for the final
    // image.
    LatticeFFT::crfft(*resultPtr, fftModel, True, doFast_p);
    if (doPadding) { // Unpad the result
      unpad(resultSlice, *resultPtr);
    }

//     {
//       int kkk=0;

//       for (int i=0;i<resultSlice.shape()(0);i++)
// 	for (int j=0;j<resultSlice.shape()(1);j++)
// 	  if (resultSlice(IPosition(4,i,j,0,0)) != 0)
// 	    {kkk=1;break;}

//       if (kkk==1)
// 	{
// 	  for (int i=0;i<resultSlice.shape()(0);i++)
// 	    {
// 	      for (int j=0;j<resultSlice.shape()(1);j++)
// 		cout << "Res: " 
// 		     << resultSlice(IPosition(4,i,j,0,0)) << " "
// 		     << endl;
// 	      cout << endl;
// 	    }
// 	  exit(0);
// 	}
//     }

  }
  if (doPadding) { // cleanup the TempLattice used for padding.
    delete resultPtr;
    modelPtr = resultPtr = 0;
  }
  //  cerr << "convolve" << endl;
}

template<class T> void LatticeConvolver<T>::
convolve(Lattice<T> & modelAndResult) const {
  convolve(modelAndResult, modelAndResult);
}

template<class T> void LatticeConvolver<T>::
resize(const IPosition & modelShape, ConvEnums::ConvType type) {
  DebugAssert(itsXfr->ndim() == modelShape.nelements(), AipsError);
  itsType = type;
  itsModelShape = modelShape;
  {
    const IPosition newFFTShape = 
      calcFFTShape(itsPsfShape, modelShape, itsType);
    if (newFFTShape == itsFFTShape) return;
  }
  // need to know the psf.
  if (itsCachedPsf == False) { // calculate the psf from the transfer function
    TempLattice<T> psf(itsPsfShape, maxLatSize);
    makePsf(psf);
    makeXfr(psf);
  }
  else {
    makeXfr(*itsPsf);
  }
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
  //  cerr << "makeXfr" << endl;
  DebugAssert(itsPsfShape == psf.shape(), AipsError);
  itsFFTShape = calcFFTShape(itsPsfShape, itsModelShape, itsType);

//   for (int i=0;i<psf.shape()(0);i++)
//     {
//       for (int j=0;j<psf.shape()(1);j++)
// 	cout << "PSF: " 
// 	     << psf(IPosition(4,i,j,0,0)) << " "
// 						//	     << abs(itsXfr(IPosition(4,i,j,0,0))) << " "
// 						//	     << arg(itsXfr(IPosition(4,i,j,0,0))) << " "
// 	     << endl;
//       cout << endl;
//     }


  { // calculate the transfer function
    IPosition XFRShape = itsFFTShape;
    XFRShape(0) = (XFRShape(0)+2)/2;
    //    XFRShape(1) = (XFRShape(1)/2+1)*2;
    if(itsXfr) delete itsXfr; itsXfr=0;
    itsXfr = new TempLattice<typename NumericTraits<T>::ConjugateType>(XFRShape, 
								   maxLatSize);
    if (itsFFTShape == itsPsfShape) { // no need to pad the psf
      LatticeFFT::rcfft(*itsXfr, psf, True, doFast_p); 
    } else { // need to pad the psf 
      TempLattice<T> paddedPsf(itsFFTShape, maxLatSize);
      pad(paddedPsf, psf);
      LatticeFFT::rcfft(*itsXfr, paddedPsf, True, doFast_p); 
    }
  }
  // Only cache the psf if it cannot be reconstructed from the transfer
  // function.
  if (itsFFTShape < itsPsfShape) {
    if(itsPsf) delete itsPsf; itsPsf=0;
    itsPsf = new TempLattice<T>(itsPsfShape, 1); // Prefer to put this on disk
    itsPsf->copyData(psf);
    itsCachedPsf = True;
  } else {
    if(itsPsf) delete itsPsf; itsPsf=0;
    itsPsf = new TempLattice<T>();
    itsCachedPsf = False;
  }
  //  cerr << "makeXfr" << endl;
}

// Construct a psf from the transfer function (itsXFR).
template<class T> void LatticeConvolver<T>::
makePsf(Lattice<T> & psf) const {
  DebugAssert(itsPsfShape == psf.shape(), AipsError);
  if (itsFFTShape == itsPsfShape) { // If the Transfer function has not been
                                    // padded so no unpadding is necessary 
    LatticeFFT::crfft(psf, *itsXfr, True, doFast_p);
  } else { // need to unpad the transfer function
    TempLattice<T> paddedPsf(itsFFTShape, maxLatSize);
    LatticeFFT::crfft(paddedPsf, *itsXfr, True, doFast_p);
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
  // transforms. I deduced the following formulae empirically. If the length on
  // any axis is one for either the model or the psf you do not need to do an
  // FFT along this axis. All you need to do is iterate through it hence the
  // FFTShape on this axis is set to one. The iteration is done in the convolve
  // function.
  IPosition FFTShape = modelShape + psfShape/2;
  const uInt ndim = FFTShape.nelements();
  for (uInt i = 0; i < ndim; i++) {
    if (psfShape(i) == 1 || modelShape(i) == 1) {
      FFTShape(i) = 1; 
    } else if (FFTShape(i) < psfShape(i)) {
      FFTShape(i) = 2 * modelShape(i);
      //      FFTShape(i) = 2 * modelShape(i) - 1;
    }
  }
  return FFTShape;
}

template<class T> void LatticeConvolver<T>::
setFastConvolve(){
  doFast_p=True;
}

// Local Variables: 
// compile-command: "cd test; gmake OPTLIB=1 inst tLatticeConvolver"
// End: 

} //# NAMESPACE CASACORE - END


#endif
