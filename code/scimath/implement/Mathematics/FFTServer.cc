//# FFTServer.cc: A class with methods for Fast Fourier Transforms
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/Mathematics/FFTServer.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Mathematics/extern_fft.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>

template<class T, class S> FFTServer<T,S>::
FFTServer()
  :theSize(0),
   theComplexFlag(False),
   theWork(0),
   theBuffer(0)
{
}

template<class T, class S> FFTServer<T,S>::
FFTServer(const IPosition & fftSize, const Bool complexTransforms)
  :theSize(fftSize.nelements(), 0),
   theComplexFlag(complexTransforms),
   theWork(fftSize.nelements()),
   theBuffer(0)
{
  const uInt ndim = theSize.nelements();
  for (uInt i = 0; i < ndim; i++) {
    theWork[i] = new Block<T>;
  }
  resize(fftSize, complexTransforms);
}

template<class T, class S> FFTServer<T,S>::
FFTServer(const FFTServer<T,S> & other)
  :theSize(other.theSize),
   theComplexFlag(other.theComplexFlag),
   theWork(other.theSize.nelements()),
   theBuffer(other.theBuffer.nelements())
{
  const uInt ndim = theSize.nelements();
  for (uInt n = 0; n < ndim; n++)
    theWork[n] = new Block<T>(*(other.theWork[n]));
}

template<class T, class S> FFTServer<T,S>::
~FFTServer() {
  const uInt ndim = theWork.nelements();
  for (uInt n = 0; n < ndim; n++) {
    delete theWork[n];
    theWork[n] = 0;
  }
}

template<class T, class S> FFTServer<T,S> & FFTServer<T,S>::
operator=(const FFTServer<T,S> & other) {
  if (this != &other) {
    uInt n;
    const uInt curDim = theSize.nelements();
    for (n = 0; n < curDim; n++) {
      delete theWork[n];
      theWork[n] = 0;
    }
    theSize = other.theSize;
    theComplexFlag = other.theComplexFlag;
    const uInt newDim = theSize.nelements();
    for (n = 0; n < newDim; n++)
      theWork[n] = new Block<T>(*(other.theWork[n]));
    theBuffer.resize(other.theBuffer.nelements());
  }
  return *this;
}

template<class T, class S> void FFTServer<T,S>::
resize(const IPosition & fftSize, const Bool complexTransforms) {
  const uInt ndim = fftSize.nelements();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(fftSize.product() > 0, AipsError);

  // if the number of dimensions has changed then allocate/remove the work
  // arrays required/used.
  if (theSize.nelements() != ndim) {
    const uInt curDim = theSize.nelements();
    uInt k;
    theSize.resize(ndim);
    if (curDim < ndim) {
      theWork.resize(ndim);
      for (k = curDim; k < ndim; k++) {
	theWork[k] = new Block<T>;
	theSize(k) = 0;
      }
    }
    else {
      for (k = ndim; k < curDim; k++) {
	delete theWork[k];
	theWork[k] = 0;
      }
    }
  }
  // Now initialise the work arrays. There is one for each dimension.  Only
  // along the first dimension is a real <-> complex transform done so it is
  // treated separately.
  Int fftLen = fftSize(0);
  uInt workSize;
  uInt bufferLength = theBuffer.nelements();
  if (complexTransforms != theComplexFlag || theSize(0) != fftLen) {
    if (complexTransforms)
      workSize = 4 * fftLen + 15;
    else
      workSize = 2 * fftLen + 15;
    theWork[0]->resize(workSize); 
    T * workPtr = theWork[0]->storage();
    if (complexTransforms) {
      cffti(&fftLen, workPtr);
      bufferLength = max(bufferLength, (uInt) fftLen);
    }
    else
      rffti(&fftLen, workPtr);
    theComplexFlag = complexTransforms;
    theSize(0) = fftLen;
  }
  // Allocate the work arrays for the other dimensions.
  for (uInt n = 1; n < ndim; n++) {
    fftLen = fftSize(n);
    if (theSize(n) != fftLen) {
      workSize = 4 * fftLen + 15;
      theWork[n]->resize(workSize); 
      T * workPtr = theWork[n]->storage();
      cffti(&fftLen, workPtr);
      bufferLength = max(bufferLength, (uInt) fftLen);
      theSize(n) = fftLen;
    }
  }
  theBuffer.resize(bufferLength);
}

template<class T, class S> void FFTServer<T,S>::
fft(Array<S> & cResult, Array<T> & rData, const Bool constInput) {
  if (constInput) {
    Array<T> rCopy = rData.copy();
    flip(rCopy,True,False);
    fft0(cResult, rCopy, False);
  }
  else {
    flip(rData,True,False);
    fft0(cResult, rData, False);
  }
  flip(cResult,False,True);
}

template<class T, class S> void FFTServer<T,S>::
fft(Array<S> & cResult, const Array<T> & rData) {
 fft(cResult, (Array<T> &) rData, True);
}

template<class T, class S> void FFTServer<T,S>::
fft(Array<T> & rResult, Array<S> & cData, const Bool constInput) {
  if (constInput) {
    Array<S> cCopy = cData.copy();
    flip(cCopy, True, True);
    fft0(rResult, cCopy, False);
  }
  else {
    flip(cData, True, True);
    fft0(rResult, cData, False);
  }
  flip(rResult, False, False);
}

template<class T, class S> void FFTServer<T,S>::
fft(Array<T> & rResult, const Array<S> & cData) {
  fft(rResult, (Array<S> &) cData, True);
}

template<class T, class S> void FFTServer<T,S>::
fft(Array<S> & cValues, const Bool toFrequency=True) {
  flip(cValues, True, False);
  fft0(cValues, toFrequency);
  flip(cValues, False, False);
}

template<class T, class S> void FFTServer<T,S>::
fft(Array<S> & cResult, const Array<S> & cData, const Bool toFrequency=True) {
  if (cResult.nelements() != 0) {
    AlwaysAssert(cResult.conform(cData), AipsError);
  }
  else
    cResult.resize(cData.shape());
  cResult = cData;
  fft(cResult, toFrequency);
}

template<class T, class S> void FFTServer<T,S>::
fft0(Array<S> & cResult, Array<T> & rData, const Bool constInput) {
  // The constInput argument is not used as the input Array is never changed by
  // this function. But is put into the interface to in case this function
  // changes in the future and to maintain a consistant interface with the
  // other fft functions in this class. To supress the compiler warning I'll
  // use it here.
  if (constInput) {
  }

  const IPosition shape = rData.shape();
  // Initialise the work arrays
  if (!shape.isEqual(theSize) || theComplexFlag != False)
    resize(shape, False);
  // Ensure the output Array is the required size
  IPosition resultShape = shape;
  resultShape(0) = (shape(0)+2)/2;
  if (cResult.nelements() != 0) {
    AlwaysAssert(resultShape.isEqual(cResult.shape()), AipsError);
  }
  else
    cResult.resize(resultShape);

  // get a pointer to the array holding the result
  Bool resultIsAcopy;
  S * complexPtr = cResult.getStorage(resultIsAcopy);
  // Do real to complex transforms along all the rows
  uInt fftLen;
  {
    Bool dataIsAcopy;
    const T * dataPtr = rData.getStorage(dataIsAcopy);
    T * workPtr = theWork[0]->storage();
    T * resultPtr = (T *) complexPtr;
    fftLen = shape(0);
    Bool even = True;
    if (fftLen%2 == 1)
      even = False;
    uInt resultRowLen = resultShape(0)*2;
    const T * inputRowPtr = dataPtr;
    T * resultRowPtr = resultPtr;
    const uInt nrows = shape.product()/fftLen;
    // Iterate over all the rows
    for (uInt r = 0; r < nrows; r++) {
      // Copy data to the complex array
      objcopy(resultRowPtr, inputRowPtr, fftLen);
      // Do the Real->Complex row transforms
      rfftf( (Int *) &fftLen, resultRowPtr, workPtr);
      // Shuffle elements along
      if (fftLen > 1)
	objmove(resultRowPtr+2, resultRowPtr+1, fftLen-1);
      // put zero into imaginary part of the first element
      *(resultRowPtr+1) = T(0.0);
      if (even) {
	// Stick zero into imaginary part of the nyquist sample
	*(resultRowPtr+resultRowLen-1) = T(0.0);
      } 
      // Increment the pointers
      inputRowPtr += fftLen;
      resultRowPtr += resultRowLen;
    }
    // We have finished with the input data array
    rData.freeStorage(dataPtr, dataIsAcopy);
  }
  
  // Do complex to complex transforms along all the remaining axes.
  const uInt ndim = shape.nelements();
  if (ndim > 1) {
    T * workPtr = 0;
    S * buffPtr = 0;
    S * rowPtr = 0;
    const uInt cElements = resultShape.product();
    uInt nffts, r, stride = resultShape(0);
    for (uInt n = 1; n < ndim; n++) {
      fftLen = resultShape(n);
      nffts = cElements/fftLen;
      workPtr = theWork[n]->storage();
      buffPtr = theBuffer.storage();
      rowPtr = complexPtr;
      r = 0;
      while (r < nffts) {
	// Copy the data into a temporary buffer. This makes it contigious and
	// hence it is more likely to fit into cache. With current computers
	// this speeds up access to the data by a factors of about ten!
	objcopy(buffPtr, rowPtr, fftLen, 1u, stride);
	// Do the transform
	cfftf( (Int *) &fftLen, (T *) buffPtr, workPtr);
	// copy the data back
	objcopy(rowPtr, buffPtr, fftLen, stride, 1u);
	// indexing calculations
	r++;
	rowPtr++;
	if (r%stride == 0)
	  rowPtr += stride*(fftLen-1);
      }
      stride *= fftLen;
    }
  }
  cResult.putStorage(complexPtr, resultIsAcopy);
}
  
template<class T, class S> void FFTServer<T,S>::
fft0(Array<S> & cResult, const Array<T> & rData) {
  fft0(cResult, (Array<T> &) rData, True);
}

template<class T, class S> void FFTServer<T,S>::
fft0(Array<T> & rResult, Array<S> & cData, const Bool constInput) {
  Array<S> cCopy;
  if (constInput)
    cCopy = cData;
  else
    cCopy.reference(cData);

  const IPosition cShape = cCopy.shape();
  const IPosition rShape = determineShape(rResult.shape(), cCopy);
  rResult.resize(rShape);
  if (!rShape.isEqual(theSize) || theComplexFlag != False)
    resize(rShape, False);

  const uInt ndim = rShape.nelements();
  uInt fftLen;
  Bool dataIsAcopy;
  
  S * dataPtr;
  dataPtr = cCopy.getStorage(dataIsAcopy);
  T * workPtr = 0;

  // Do complex to complex transforms along all other dimensions
  if (ndim > 1) {
    S * buffPtr = theBuffer.storage();
    S * rowPtr = 0;
    const uInt cElements = cShape.product();
    uInt n, r, nffts, stride = cShape(0);
    for (n = 1; n < ndim; n++) {
      workPtr = theWork[n]->storage();
      rowPtr = dataPtr;
      fftLen = rShape(n);
      nffts = cElements/fftLen;
      r = 0;
      while (r < nffts) {
	// Copy the data into a temporary buffer. This makes it contigious and
	// hence it is more likely to fit into cache. With current computers
	// this speeds up access to the data by a factors of about ten!
	objcopy(buffPtr, rowPtr, fftLen, 1u, stride);
	// Do the FFT
	cfftb( (Int *) &fftLen, (T *) buffPtr, workPtr);
	// copy the data back
	objcopy(rowPtr, buffPtr, fftLen, stride, 1u);
	// indexing calculations
	r++;
	rowPtr++;
	if (r%stride == 0)
	  rowPtr += stride*(fftLen-1);
      }
      stride *= fftLen;
    }
  }
  // Do complex to real transforms along all the rows
  Bool resultIsAcopy;
  T * resultPtr = rResult.getStorage(resultIsAcopy);
  T * realDataPtr = (T *) dataPtr;
  workPtr = theWork[0]->storage();

  T * resultRowPtr = resultPtr;
  const uInt cStride = cShape(0)*2;
  fftLen = rShape(0);
  const uInt nffts = rShape.product()/fftLen;
  // Iterate over all the rows
  for (uInt r = 0; r < nffts; r++) {
    // Copy the data to the real array
    *resultRowPtr = *realDataPtr;
    objcopy(resultRowPtr+1, realDataPtr+2, fftLen-1);
    // Do the Complex->Real row transform
      rfftb( (Int *) &fftLen, resultRowPtr, workPtr);
    // Increment the pointers
    realDataPtr += cStride;
    resultRowPtr += fftLen;
  }
  // We have finished with the input data array
  cCopy.freeStorage(dataPtr, dataIsAcopy);
  // While we have a raw pointer handy do the scaling
  uInt nelem = rResult.nelements();
  T scale = T(1)/T(nelem);
  T * endPtr = resultPtr + nelem;
  for (resultRowPtr = resultPtr; resultRowPtr < endPtr; resultRowPtr++)
    *resultRowPtr *= scale;
  // We have finished with the output data array
  rResult.putStorage(resultPtr, resultIsAcopy);
}

template<class T, class S> void FFTServer<T,S>::
fft0(Array<T> & rResult, const Array<S> & cData) {
  fft0(rResult, (Array<S> &) cData, True);
}

template<class T, class S> void FFTServer<T,S>::
fft0(Array<S> & cValues, const Bool toFrequency) {
  const IPosition shape = cValues.shape();
  if (!shape.isEqual(theSize) || theComplexFlag != True)
    resize(shape, True);

  const uInt ndim = shape.nelements();
  uInt fftLen;
  Bool valuesIsAcopy;
  S * dataPtr = cValues.getStorage(valuesIsAcopy);
  T * workPtr = 0;

  // Do complex to complex transforms along all the dimensions
  S * buffPtr = theBuffer.storage();
  T * realBuffPtr = 0;
  T * endRowPtr = 0;
  S * rowPtr = 0;
  const uInt nElements = shape.product();
  const T scale = T(1)/T(nElements);
  const uInt shape0t2 = shape(0) * 2;
  uInt n, r, nffts, stride = 1u;
  for (n = 0; n < ndim; n++) {
    workPtr = theWork[n]->storage();
    rowPtr = dataPtr;
    fftLen = shape(n);
    nffts = nElements/fftLen;
    r = 0;
    if (n != 0) 
      realBuffPtr = (T *) buffPtr;
    while (r < nffts) {
      // Copy the data into a temporary buffer. This makes it contigious and
      // hence it is more likely to fit into cache. With current computers
      // this speeds up access to the data by a factors of about ten!
      if (n != 0)
	objcopy(buffPtr, rowPtr, fftLen, 1u, stride);
      else
	realBuffPtr = (T *) rowPtr;
      // Do the FFT
      if (toFrequency == True)
	cfftf( (Int *) &fftLen, realBuffPtr, workPtr);
      else {
	cfftb( (Int *) &fftLen, realBuffPtr, workPtr);
	if (n == 0) // Scale by 1/N while things are (hopefully) in cache
	  for (endRowPtr = realBuffPtr+shape0t2; 
	       realBuffPtr < endRowPtr; realBuffPtr++)
	    *realBuffPtr *= scale;
      }
      
      // copy the data back
      if (n != 0)
	objcopy(rowPtr, buffPtr, fftLen, stride, 1u);
      // indexing calculations
      r++;
      rowPtr++;
      if (r%stride == 0)
	rowPtr += stride*(fftLen-1);
    }
    stride *= fftLen;
  }
}

template<class T, class S> void FFTServer<T,S>::
fft0(Array<S> & cResult, const Array<S> & cData, const Bool toFrequency=True) {
  if (cResult.nelements() != 0) {
    AlwaysAssert(cResult.conform(cData), AipsError);
  }
  else
    cResult.resize(cData.shape());
  cResult = cData;
  fft0(cResult, toFrequency);
}

template<class T, class S> IPosition FFTServer<T,S>::
determineShape(const IPosition & rShape, const Array<S> & cData){
  const IPosition cShape=cData.shape();
  const uInt cDim = cShape.nelements();
  DebugAssert(cDim > 0, AipsError);
  // If rShape is non-zero then it must match one of the two possible shapes
  if (rShape.product() != 0) {
    DebugAssert(cDim == rShape.nelements(), AipsError);
    IPosition reqShape(cShape);
    reqShape(0) = 2*cShape(0)-2;
    if (reqShape.isEqual(rShape))
      return reqShape;
    reqShape(0) += 1;
    if (reqShape.isEqual(rShape))
      return reqShape;
    throw(AipsError("FFTServer<T,S>::determineShape() -"
		    " output array has the wrong shape"));
  }
  // Scan the imaginary components of the last samples on the first axis in
  // the cData to see if there are any non-zero terms. If so the output array
  // must be odd length in its first axis.
  {
    VectorIterator<S> iter((Array<S> &) cData);
    uInt lastElem = cShape(0)-1;
    while (!iter.pastEnd()) {
      if (!near(iter.vector()(lastElem).imag(), 0.0f)) {
	IPosition oddLength(cShape);
	oddLength(0) = cShape(0)*2-1;
	return oddLength;
      }
      iter.next();
    }
  }
  // See if the FFTServer size can be used to guess the output Array size;
  if (theSize.nelements() == cDim) {
    Bool match = True;
    for (uInt i = 1; i < cDim; i++)
      if (theSize(i) != cShape(i))
	match = False;
    if (match == True && 
	((theSize(0) == 2*cShape(0) - 2) || (theSize(0) == 2*cShape(0) - 1)))
      return theSize;
  }
  IPosition defShape(cShape);
  defShape(0) = 2*cShape(0) - 2;
  return defShape;
};

template<class T, class S> void FFTServer<T,S>::
flip(Array<S> & cData, const Bool toZero, const Bool isHermitian) {
  const IPosition shape = cData.shape();
  const uInt ndim = shape.nelements();
  const uInt nElements = shape.product();
  AlwaysAssert(nElements != 0, AipsError);
  {
    Int buffLen = theBuffer.nelements();
    for (uInt i = 0; i < ndim; i++)
      buffLen = max(buffLen, shape(i));
    theBuffer.resize(buffLen, False, False);
  }
  Bool dataIsAcopy;
  S * dataPtr = cData.getStorage(dataIsAcopy);
  S * buffPtr = theBuffer.storage();
  S * rowPtr = 0;
  S * rowPtr2 = 0;
  S * rowPtr2o = 0;
  uInt rowLen, rowLen2, rowLen2o;
  uInt nFlips;
  uInt stride = 1;
  uInt r;
  uInt n=0;
  if (isHermitian) {
    n = 1;
    stride = shape(0);
  }
  for (; n < ndim; n++) {
    rowLen = shape(n);
    rowLen2 = rowLen/2;
    rowLen2o = (rowLen+1)/2;
    nFlips = nElements/rowLen;
    rowPtr = dataPtr;
    r = 0;
    while (r < nFlips) {
      rowPtr2 = rowPtr + stride * rowLen2;
      rowPtr2o = rowPtr + stride * rowLen2o;
      if (toZero) {
	objcopy(buffPtr, rowPtr2, rowLen2o, 1u, stride);
	objcopy(rowPtr2o, rowPtr, rowLen2, stride, stride);
	objcopy(rowPtr, buffPtr, rowLen2o, stride, 1u);
      }
      else {
	objcopy(buffPtr, rowPtr, rowLen2o, 1u, stride);
	objcopy(rowPtr, rowPtr2o, rowLen2, stride, stride);
	objcopy(rowPtr2, buffPtr, rowLen2o, stride, 1u);
      }
      r++;
      rowPtr++;
      if (r%stride == 0)
	rowPtr += stride*(rowLen-1);
    }
    stride *= rowLen;
  }
  cData.putStorage(dataPtr, dataIsAcopy);
}

template<class T, class S> void FFTServer<T,S>::
flip(Array<T> & rData, const Bool toZero, const Bool isHermitian) {
  const IPosition shape = rData.shape();
  const uInt ndim = shape.nelements();
  const uInt nElements = shape.product();
  AlwaysAssert(nElements != 0, AipsError);
  {
    Int buffLen = theBuffer.nelements();
    for (uInt i = 0; i < ndim; i++)
      buffLen = max(buffLen, shape(i)/2);
    theBuffer.resize(buffLen, False, False);
  }
  Bool dataIsAcopy;
  T * dataPtr = rData.getStorage(dataIsAcopy);
  T * buffPtr = (T *) theBuffer.storage();
  T * rowPtr = 0;
  T * rowPtr2 = 0;
  T * rowPtr2o = 0;
  uInt rowLen, rowLen2, rowLen2o;
  uInt nFlips;
  uInt stride = 1;
  uInt r;
  uInt n=0;
  if (isHermitian) {
    n = 1;
    stride = shape(0);
  }
  for (; n < ndim; n++) {
    rowLen = shape(n);
    rowLen2 = rowLen/2;
    rowLen2o = (rowLen+1)/2;
    nFlips = nElements/rowLen;
    rowPtr = dataPtr;
    r = 0;
    while (r < nFlips) {
      rowPtr2 = rowPtr + stride * rowLen2;
      rowPtr2o = rowPtr + stride * rowLen2o;
      if (toZero) {
	objcopy(buffPtr, rowPtr2, rowLen2o, 1u, stride);
	objcopy(rowPtr2o, rowPtr, rowLen2, stride, stride);
	objcopy(rowPtr, buffPtr, rowLen2o, stride, 1u);
      }
      else {
	objcopy(buffPtr, rowPtr, rowLen2o, 1u, stride);
	objcopy(rowPtr, rowPtr2o, rowLen2, stride, stride);
	objcopy(rowPtr2, buffPtr, rowLen2o, stride, 1u);
      }
      r++;
      rowPtr++;
      if (r%stride == 0)
	rowPtr += stride*(rowLen-1);
    }
    stride *= rowLen;
  }
  rData.putStorage(dataPtr, dataIsAcopy);
}
// Local Variables: 
// compile-command: "cd test; gmake OPTLIB=1 inst"
// End: 
