//# FFTServer.h: A class with methods for Fast Fourier Transforms
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

#if !defined(AIPS_FFTSERVER_H)
#define AIPS_FFTSERVER_H

#if defined(_AIX)
#pragma implementation ("FFTServer.cc")
#endif

#include <aips/aips.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Containers/Block.h>

template <class T> class Array;

// <summary>A class with methods for Fast Fourier Transforms</summary>

// <prerequisite> 
// <li> <linkto module=Arrays>The Arrays module</linkto>
// </prerequisite>

// <etymology> The FFTServer class, can do Fast Fourier Transforms  of
// any length and dimensionality. 
// </etymology>


// <synopsis>

// The FFTServer class provides methods for performing n-dimensional
// Fast Fourier Transforms with real and complex Array objects of
// abitrary size.


// </synopsis>

// <templating arg=T>
// The T argument must be of type float or double. 
// </templating>
//
// <templating arg=S>
// The S argument must be of type Complex or DComplex
// </templating>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
template<class T, class S> class FFTServer
{
public:
  // The default constructor. Initiliases the server to do transforms of length
  // 1! 
  FFTServer();

  // Initialise the Server to do transforms on Arrays of the specified
  // shape. See the resize function for a description of the
  // complexTransforms flag.
  FFTServer(const IPosition & fftSize, const Bool complexTransforms=False);
  
  // copy constructor. The copied server is initialses to do transforms of the
  // same length as the other server. Uses copy (and not reference) semantics
  // so that changing the transform length of one server does not affect the
  // other server.
  FFTServer(const FFTServer<T,S> & other);

  // destructor
  ~FFTServer();
  
  // The assignment operator which does the same thing as the copy
  // constructor.
  FFTServer<T,S> & operator=(const FFTServer<T,S> & other);

  // Modify the FFTServer object to do transforms of the supplied shape. If
  // complexTransforms is True then it is assumed that complex to complex
  // transforms are going to be done. Otherwise it is assumed that Real to
  // comlex transforms (and viceversa) will be done. The shape is the shape of
  // the real array (or complex one if complex to complex transforms are being
  // done). In general it is not necessary to use this function as all the fft
  // & fft0 functions will automatically resize the server, if necessary, to
  // match their input arguments
  void resize(const IPosition & fftSize,
		 const Bool complexTransforms=False);

  // Real to complex fft. The origin of the transform is in the centre of the
  // Array. Because of the Hermitian property the output Array only contains
  // half of the complex result. Its size must either be zero, ie. have no
  // elements, or a size that is appropriate to the input Array size,
  // ie. <src>shape = [(nx+2)/2, ny, nz,...]</src>.  Otherwise an AipsError is
  // thrown. 
  // If constInput is False then the input Array may be scrambled after calling
  // this function. Setting constInput to True will guarantee this does not
  // happen by making an internal copy of the input Array.
  void fft(Array<S> & cResult, Array<T> & rData, const Bool constInput=False);

  // Complex to real fft. The origin of the transform is in the centre of the
  // Array. Because of the Hermiton property the input Array only contains half
  // of the complex values. The output Array's size must either be zero, ie
  // have no elements, or a size that is appropriate to the input Array size,
  // ie. either <src>shape = [2*cx-2, cy, cz,...]</src> or 
  // <src>shape = [2*cx-1, cy, cz,...]</src>.  Otherwise an AipsError is
  // thrown.

  // To decide on whether the first axis of the output array is odd or even the
  // following algorithm is used.
  // <ul>
  // <li> If the size of the output Array is non-zero:<br>
  //      Then its first dimension must be either either 2*cx-1 or 2*cx-2 (and
  //      the other dimensions must be the same as the input array). Otherwise
  //      an AipsError is thrown.
  // <li> If the number of elements in the output Array is zero:<br>
  //      Then scan the imaginary components of the last points on the first
  //      axis of the input Array. If any are not zero then the first axis of
  //      the output array is 2*cx-1. Otherwise look at the current size of the
  //      FFTServer. If its first dimension is either 2*cx-1 or 2*cx-2 then use
  //      that dimension. Otherwise assume the first diumension is 2*cx-2, ie
  //      an even length.
  // </ul>

  // If constInput is False then the input Array may be scrambled after calling
  // this function. Setting constInput to True will guarantee this does not
  // happen by making an internal copy of the input Array.
  void fft(Array<T> & rResult, Array<S> & cData, const Bool constInput=False);


  // Complex to complex inplace fft. The origin of the transform is in the
  // centre of the Array. The direction of the transform is controlled by the
  // toFrequency variable. If True then a forward, or time to frequency,
  // transform is performed. If False a backward or frequency to time transform
  // is done. Scaling by 1/N, where N is the number of elements in the Array,
  // is always done on the backward transform.
  void fft(Array<S> & cValues, const Bool toFrequency=True);

  // Complex to complex fft. The origin of the transform is in the centre of
  // the Array. The direction of the transform is controlled by the toFrequency
  // variable. If True then a forward, or time to frequency, transform is
  // performed. If False a backward or frequency to time transform is
  // done. Scaling by 1/N, where N is the number of elements in the Array, is
  // always done on the backward transform. The size of the output Array must
  // either be zero , ie one dimensional but with no elements, or the same as
  // the input Array, ie. <src>shape = [cx, cy, cz,...]</src>.  Otherwise an
  // AipsError is thrown.
  void fft(Array<S> & cResult, const Array<S> & cData,
	   const Bool toFrequency=True);

  // The following functions are equivalent to the <src>fft</src> functions
  // described above except that the origin of the transform is the first
  // element of the Array, ie. [0,0,0...], rather than the centre element, ie
  // [nx/2, ny/2, nz/2, ...]. As the underlying FORTRAN functions assume that
  // the origin of the transform is the first element these routines are in
  // general faster than the equivalent ones with the origin at the centre of
  // the Array.
  // <group>
  void fft0(Array<S> & cResult, Array<T> & rData, const Bool constInput=False);
  void fft0(Array<T> & rResult, Array<S> & cData, const Bool constInput=False);
  void fft0(Array<S> & cValues, const Bool toFrequency=True);
  void fft0(Array<S> & cResult, const Array<S> & cData,
	    const Bool toFrequency=True);
  // </group>
private:
  //# flips the quadrants in a complex Array so that the point at
  //# cData.shape()/2 moves to the origin. This moves, for example, the point 
  //# at [8,3] to the origin ([0,0]) in an array of shape [16,7].
  //# With an Array with an odd length two flips do NOT restore the data to its
  //# original state. So the when toZero=False this routine does an unflip
  //# operation that does restore the data to its original state for odd length
  //# arrays.
  //# When passed a Hermition Array where half the complex plane is implicit 
  //# (eg as produced by a real->complex Transform) it is not necessary to flip
  //# the first dimension of the Array. In this case the isHermition flag
  //# should be set to True. For complex<->complex transforms this should be
  //# False.
  // <group>
  void flip(Array<T> & rData, const Bool toZero, const Bool isHermition);
  void flip(Array<S> & cData, const Bool toZero, const Bool isHermition);
  // </group>
  //# finds the shape of the output array when doing complex -> real transforms
  IPosition determineShape(const IPosition & rShape, const Array<S> & cData);
  //# The size of the last FFT done by this object
  IPosition theSize;
  //# Whether the last FFT was complex <-> complex or not
  Bool theComplexFlag;
  //# twiddle factors and factorisations used by fftpack
  PtrBlock<Block<T> *> theWork;
  //# buffer for copying non-contigious arrays to contigious ones. This is done
  //# so that the FFT's have a better chance of fitting into cache and hence
  //# going faster. 
  //# This buffer is also used as temporary storage when flipping the data.
  Block<S> theBuffer;
};
#endif
