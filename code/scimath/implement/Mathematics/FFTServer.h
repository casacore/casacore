//# FFTServer.h: A class with methods for Fast Fourier Transforms
//# Copyright (C) 1994,1995,1996
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

#include <aips/aips_exit.h>
#include <aips/aips.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Typeinfo.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Mathematics/FourierTool.h>
#include <aips/Mathematics/extern_fft.h>
#include <aips/Arrays/Vector.h>

// <summary> Error class for <linkto class=FFTServer>FFTServer</linkto> class </summary>

// <synopsis>
// Error class for <linkto class=FFTServer>FFTServer</linkto> class.
// </synopsis>

class FFTError : public AipsError
{
public:
  FFTError() : AipsError("FFTError") {}
  FFTError(const Char *m)   : AipsError(m) {}
  FFTError(const String &m) : AipsError(m) {}
};


// <summary> Parameters for <linkto class=FFTServer>FFTServer</linkto> class </summary>

// <synopsis>
// Parameters for <linkto class=FFTServer>FFTServer</linkto> class.
// </synopsis>

//############################################################################
//# NOTE:  Delete the following listing of the enumerations when enumerations
//#        are handled properly by the documentation extractor.
//############################################################################

// <note role=caution>
// The following enum documentation is currently extracted by hand, and thus
// could be out of date if this documentation was not updated when the
// enum was modified.
// </note>
//
// This flag says that an array does not have nyquist
// data at end of each column
//
// <srcblock>
// enum {DEF_IMAGE_TYPE = 0};   
// </srcblock>
//
// <todo asof="1996/04/10">
//   <li> The complex->Real transforms produce incorrect results
//        when the length of the real array is odd
// </todo>

class fftparms {
public:
  //#     If you change this, change it in the documentation above.
  //
  // This flag says that an array does not have nyquist
  // data at end of each column
  enum {DEF_IMAGE_TYPE = 0};   
};


// <summary> A class with methods for Fast Fourier Transforms </summary>

// <prerequisite> 
// <li> <linkto class=FourierTool>FourierTool</linkto>
// </prerequisite>

// <etymology> The FFTServer class, in addition to providing direct
// interfaces to FORTRAN FFT routines, manipulates the extra storage
// required during the transformation of Hermitian objects.
// </etymology>


// <synopsis>

// The FFTServer class provides methods for performing n-dimensional
// Fast Fourier Transforms with real and complex Array objects of
// abitrary size.

// The term uv grid is used to denote any frequency domain object,
// whether two dimensional or not. A forward transform is a transform
// from the time to frequency domain; a reverse or backward transform is
// from frequency to time.

// Methods are provided to transform both real and complex arrays; the
// Hermitian property of a transformed real object is used to reduce the
// required storage by half. The Nyquist component in the first dimension
// is maintained in a separate array. The FourierTool base class provides
// methods for Nyquist manipulation, and grid packing. A packed format is
// used to store all frequency domain data in the transformed array.

// The methods fft, rrfft, and rndfft perform real to real transforms and
// use the Nyquist storage.

// The methods nyfft, rcnyfft, crnyfft, rndnyfft perform real to real and
// real to complex or complex to real transforms, and expect an object of
// dimension <src> (n1+2, n2, n3, ...) </src>. If <src> n1 </src> is even,
// the last two columns in
// each row store the Nyquist component. If <src> n1 </src> is odd, the last two
// columns in each row store the real and imaginary components of the
// last element. These methods set but do not read the Nyquist storage.

// The methods rcfft, crfft, rcnyfft, and crnyfft, in addition to
// performing transforms, convert the object between real and complex
// representations.

// The methods cxfft, cndfft, and rcndfft perform complex to complex
// transforms, and never use the Nyquist storage.

// The methods fft, nyfft, cxfft, rcfft, rcnyfft, crfft, crnyfft, and
// rrfft assume that the phase center of an image is the center pixel,
// and not the zero pixel. Thus the image will be flipped before a
// transform to bring the phase center to the origin in the array, and
// flipped after the transform to bring the origin of the array to the
// phase center. By convention, the phase center in a uv grid for any
// dimension of odd size is the middle pixel; i.e. if dimension 2 has
// rank 7 and pixels are numbered <src> 0,...,6 </src>, then pixel
// number 3 is the
// phase center. For a dimension of even size, the center pixel is the
// <src> N/2 </src> pixel; i.e. if dimension 3 has rank 6 and pixels are numbered
// <src> 1,...,6 </src>, then pixel number 4 is the phase center.

// The flipImage and exchangeUV methods are used to move the phase center
// of the object to its origin. The parity parameter is used to specify
// the phase center; if it is 1, then the above described rules apply. If
// it is zero, then if a dimension is of odd size, the phase center is
// the middle pixel + 1. This is required to make flipImage and
// exchangeUV invertible operations. For example, applying the method
// flipImage to the Vector <src> [0,1,2,3,4] </src> produces
// <src> [2,3,4,0,1] </src>; applying
// flipImage again yields <src> [4,0,1,2,3] </src>. Setting parity to 0 before
// the second flipImage will yield <src> [0,1,2,3,4] </src>.

// Applying the flipImage and exchangeUV operations to an Array object is
// equivalent to shifting the object through the n-space that is the
// periodic extension of the object. flipImage shifts by <src> N/2 </src> in all
// dimensions; exchangeUV shifts by <src> N/2 </src> in all dimensions but the
// first.  For example, the discrete transform of a sample of size
// <src> N </src> is periodic with period <src> N </src>. flipImage is
//  equivalent to shifting by <src> N/2 </src>.

// For an Vector, flipImage exchanges halfs. For a Matrix, it exchanges
// quadrants; for a cube, octants. exchangeUV is equivalent to shifting
// by <src> N/2 </src> in all but the first dimension; it should be used
// to permute a
// real object that is the representation of a complex-valued Hermitian
// transform, since the phase reference will be at the <src> u=0 </src>
// Position in the array.

// The methods rndfft, rndnyfft, cndfft, and rcndfft are provided for
// users who do not require image flipping.

// The methods cndfft and rcndfft provide a direct interface to
// multi-dimensional FORTRAN FFT routines. No image flipping or grid
// manipulation is performed. These methods should be used by users
// requiring an optimal DFT.

// In an object where the first dimension is an odd size, Nyquist storage
// is a misnomer, since there will be no sample at the Nyquist frequency
// in <src> u </src>.
// In this case, the Nyquist component is used to store the real
// and imaginary components of the last element in each row. The last
// real element is duplicated.  When the first dimension is even,
// the Nyquist storage contains the real and imaginary components at the
// Nyquist frequency in <src> u </src>.

// Nyquist storage always has first dimension of size 2. The expand
// method always adds two columns to the array. The method nyfft expects
// that the real component of the last element is duplicated in the
// <src> u-3 </src> and <src> u-2 </src> elements, where
// <src> u-1 </src> is the last element in a row.

// In the case of rcfft or rcnyfft, an array with an odd first dimension
// is first expanded by 1, then converted to a complex array, since the
// conversion expects a sequence of real/imaginary pairs. When calling
// crfft or crnyfft, the parameter shrinkodd must be nonzero to ensure
// invertibility. If shrinkodd is nonzero, the base method shrinkby1 will
// be invoked on the converted real array before the transform.

// Since the FFTServer class manages the Nyquist component of a
// transformed object, an FFTServer object is loosely associated with
// a specific Array object. The size of the Nyquist storage is
// determined by the size of the Array. After performing a forward
// transform with a real to real method, the Nyquist array contains
// the Nyquist component of the transformed array, or the real and
// imaginary components of the last elements in each row if the first
// dimension is odd.

// The association is loose because a single FFTServer object can be
// used to transform different Array objects of the same size,
// when the Nyquist component is unneeded. The methods fft, rcfft,
// crfft, rrfft, and rndfft read from and write to the Nyquist array,
// so caution should be used when transforming different Array
// objects. The methods nyfft, rcnyfft, crnyfft, rndnyfft write to but
// do not read from the Nyquist array. Thus, different objects of the
// same size can always be exchanged safely, but the Nyquist array
// will change. The methods cxfft, cndfft, and rcndfft never touch the
// Nyquist array, so an abitrary Array object can be transformed with
// these methods. In general, the Nyquist array will be written to
// during a forward transform, an unpack operation, and a shrink
// operation. The Nyquist array will be read from during a backward
// transform, a pack operation, and an expand operation.

// In most methods, the object is normalized after a backward transform
// (all elements are multiplied by <src> 1.0 / N </src>, where
// <src> N </src> is the number of
// complex elements.)

// In all cases, the dir parameter specifies the direction of the
// transform. If <src> dir > 0 </src>, then the transform is forward.
// If <src> dir <=0 </src>, then
// the transform is backward. The case <src> dir==0 </src> is used in
// some transforms
// to signify that the object should not be normalized after a backward
// transform.

// FORTRAN FFT routines from the Netlib package fftpak and from code
// originally written by R.C. Singleton are used.

// The two template parameters are used to associate real and complex 
// types; see examples below.

// The shift method exploits multi-dimensional shifting properties of
// Fourier Transforms.  If <src> f(t) </src> is a function of time and
// <src> F(v) </src> its
// transform, then <src> f(t-T) <-> exp(-i*2_pi*(T(v/N)))F(v) </src>,
// where <src> N </src> is the
// number of samples and <src> T </src> is a real valued shift. A version of the
// inverse shift property is <src> exp(i*2*pi(vo/N)t(f(t) <-> F(v-vo) </src>.
// This property generalizes to <src> n </src> dimensions.
// The parameter factor is an
// additional factor in the multiplication, and shift is a real valued
// vector to shift by (the constant <src> T </src> in this case,
// not <src> -T </src>). If timeshift
// is nonzero, then the exponent is negative (we're performing a
// timeshift, so multiply in the frequency domain). If timeshift is zero,
// then the exponent is positive.

// For a complete description of the methods associated with this class
// see note 157: <em>The AIPS++ FFTServer Class</em>.

// </synopsis>

// <templating arg=T>
// The T argument must be of type float or double. 
// </templating>
//
// <templating arg=S>
// The S argument must be of type Complex or DComplex
// </templating>
//
// <templating arg=S, T>
// The S argument must be of type Complex or DComplex
// </templating>
//
// <note role=warning>
// The FORTRAN routines for performing
// multidimensional transforms expect arrays in FORTRAN order. A
// complex array is expected to be stored as sequential real imaginary
// pairs: real, imaginary, real, imaginary.  To ensure optimal
// performance, the FFTServer class uses the getStorage and putStorage
// Array class methods to gain access to the Array's underlying
// storage. This class depends on the advertised storage policy of
// real and complex Array objects.
// </note>

// <example>
// <srcblock>

// #include <aips/FFTServer.h>
// #include <aips/Mathematics/math.h>

// void examples() {
//
//  // perform ffts on a 5 by 4 matrix
// Matrix<float> m(5,4);
// FFTServer<float,Complex> ffts(m);
// m = 0;
// m(0,0)=1;
//  // forward transform
// ffts.fft(m, 1);
//  // m is now packed; perform reverse transform
// ffts.fft(m, -1);

//  // perform forward transform without image flipping
// ffts.rndfft(m, 1);
//  // reverse
// ffts.rndfft(m, -1);

// Cube<Complex> c(4,5,4);
//  // okay to use ffts for cxfft
//  // reverse, don't scale
// ffts.cxfft(c, 0);
//  // do a flipImage to bring phase center to origin
// ffts.flipImage(c);
//  // do a flipImage to bring center + 1 to origin
// ffts.flipImage(c, fftparms::DEF_IMAGE_TYPE, 0);

//  // use separate real and imaginary components
// Vector<Double> re(4);
// Vector<Double> im(4);
// re = 0;
// im = 0;
// re(0) = 1;
//  // okay to use ffts
// ffts.rcndfft(re, im, 1);
//  //don't scale on reverse transform
// ffts.rcndfft(re, im, -1, 0);
//  // re(0) should now be 4

//  // now must Set ffts with re, or use new FFTServer object
// ffts.Set(re);
// Vector<DComplex> cv;
// cv = rcfft(re);

// Vector<double> dv(7);
// Vector<double> dvr(7);
// FFTServer<double,DComplex> fftt(dv);

//  // assume dv is a Vector of size 5 with two Nyquist components
//  // at end
// cv = rcnyfft(dv);
//  // cv is now a Vector of 4 DComplex objects
//  // we must set shrinkodd to 1 on crfft
// dvr = crnyfft(cv, 1, 1);
//  // perform an image flip on re
// fftt.flipImage(dv);
//
// }

// </srcblock>
// </example>
//



// For all transform functions, if <src> dir >0 </src>, then it is a
// forward transform. If <src> dir <= 0 </src>, it is a backward transform.
// <src> dir==0 </src> has method-specific meaning.

template<class T, class S> class FFTServer: 
  public FourierTool<T, S>
{
public:
  // default constructor
  FFTServer(); 

  // copy constructor
  FFTServer(const FFTServer<T,S> &); 

  // Other Constructors
  FFTServer(const Array<T> &);		   
  FFTServer(const Array<S> &, int has_ny = 0);		   
  FFTServer(const IPosition &);		  
  
  // destructor
  ~FFTServer();
  
  // operator =
  FFTServer<T,S> &operator=(const FFTServer<T,S>&);

  // <group>
  // Modify the parameters of an FFTServer object.
  void Set(const Array<T> &);		   
  void Set(const Array<S> &, int has_ny = 0);		   
  void Set(const IPosition &);		  
  // </group>


  // Real to real packed ffts. If dir==0, do
  // not normalize after backward transform.
  void fft(Array <T>&rdata, int dir); 

// Real to real fft with nyquist expected as last two columns in
// rdata. If dir == 0, do not normalize after backward transform.
  void nyfft(Array<T>&rdata, int dir);         

// Complex to complex fft if dir == 0, do not normalize after backward
// transform.
  void cxfft(Array<S>&, int dir, int center=1);         


// <note role=caution> As currently implemented, the transforms rcfft,
// crfft, rcnyfft, and crnyfft copy the entire array several
// times.</note>

// <group>

// In rcfft and rcnyfft, if the first dimension is of odd size, the
// array is expanded by one before being converted to a complex array.
// shrinkodd must be set correctly when performing a reverse
// transform; if the original array has a first dimension of odd size,
// then shrinkodd should be nonzero.  Real to complex fft; nyquist
// data packed in complex result.
  Array<S>
    rcfft(const Array<T> &);   		       

  // Real to complex fft; nyquist data added to end of output complex array
  Array<S>
    rcnyfft(const Array<T> &);   		       

// If shrinkodd is nonzero, then the base method shrinkby1 is called with
// the converted real object before the transformation.
// <group>

  // Complex to real fft. Nyquist data packed in complex input.
  Array<T>
    crfft(Array<S> &, int do_scale=1, int shrinkodd=0);    

  // Complex to real fft. Nyquist data required at end of complex
  // input as 1 complex element at the end of each row
  Array<T>
    crnyfft(Array<S> &, int do_scale=1, int shrinkodd=0);    
// </group>

// </group>


// If do_scale is 0, the array will
// not be normalized after a backward transform.
// <group>

  // Real to real transform; perform shifting of phase reference
  // to zero pixel before and after.
  void rrfft(Array <T>&rdata, int dir, int do_scale=1); 

  // Real to real transform; do not do image shifting.
  void rndfft(Array<T>&, int dir, int do_scale=1);

  // Real to real transform; first dimension of size n+2
  // last two columns are Nyquist data for n even;
  // otherwise, they are the real and imaginary data for
  // the last column.
  void rndnyfft(Array<T>&, int dir, int do_scale=1);
// </group>


// <note role=tip> Direct interface to FORTRAN FFT. Provides optimal
// DFT. </note>
// <group>

  // Complex to complex transform
  void cndfft(Array<S>&, int dir, int do_scale=1);

  // Complex to complex transform, real and imaginary
  // components passed as separate arrays
  void rcndfft(Array<T> &rdata, Array<T> &cdata, int dir, int
	       do_scale=1);
// </group>


  //flip image quadrants
  void flipImage(Array<T> &, int image_type = fftparms::DEF_IMAGE_TYPE, int parity=1);
  void flipImage(Array<S> &, int image_type = fftparms::DEF_IMAGE_TYPE, int parity=1);

  // order uv frequencies
  void exchangeUV(Array<T> &, int parity=1);       	
  void exchangeUV(Array<S> &, int parity=1);      

  // move weights to a UV grid
  void uvassign(Array<T> &, Array<T> &);   
  void uvassign(Array<S> &, Array<T> &);  

  // move weights for full complex fft 
  void cxUVassign(Array<S> &, Array<T> &);

  // sum up weights
  T wtsum(Array<T> &);            	   

  // sum of weights for weight array generated by 'grid' method
  T imWtsum(Array<T> &);            	   

  // sum up weights for full complex fft
  T cxWtsum(Array<T> &);            	   

// Perform complex multiplication of the array equivalent to frequency
// or time shifting. A shift in the frequency domain is equivalent to
// calling this function with the time domain array, the desired
// shift, and timeshift set to 0, then transforming (forward
// transform). A shift in the time domain is equivalent to calling
// this function with the frequency domain array, the desired shift,
// and timeshift set to 1, then transforming to the time domain
// (reverse transform). Alternatively, one may transform first, then
// call shift to perform the equivalent of a pre-transform shift.
// All elements will be multiplied by factor for convenience.
// The value of timeshift determines the sign of the complex exponent; if
// timeshift !=0, the exponent is -1, and we're multiplying in the
// frequency domain. Otherwise, the exponent is 1, and we're multiplying
// in the time domain.
// shift is the desired real-valued shift.
  void shift(Array<S>&, S factor, int timeshift, const Vector<T> &shift);

  // Return the value by which an FFT'd image was scaled
  T scaleFactor(void);
private:

  // A simplified version of the shift method. When the shift
  // is half the size of the array and the dimension are even,
  // the multipliers reduce to +/- 1.
  void flipsignsfact(Array<S> &cdata, S factor);

  // a vector used for temporary copies
  Vector<T> temp;       

  // work array for storage of sines and cosines
  Vector<T> work;              

  // scaling factor for reverse fft
  T scale;		 	
};


#endif //AIPS_FFTSERVER_H
