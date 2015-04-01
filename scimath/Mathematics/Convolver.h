//# Convolver.h: this defines Convolver a class for doing convolution
//# Copyright (C) 1996,1999,2001,2002,2003
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
//#
//# $Id$

#ifndef SCIMATH_CONVOLVER_H
#define SCIMATH_CONVOLVER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/scimath/Mathematics/FFTServer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Forward Declarations
template <class FType> class Convolver;

// Typedefs
typedef Convolver<Float> FloatConvolver;
typedef Convolver<Double> DoubleConvolver;

// <summary>
// A class for doing multi-dimensional convolution
// </summary>

// <use visibility=export>

// <reviewed reviewer="Brian Glendenning " date="1996/05/27" 
// tests="tConvolution">
// </reviewed>

// <prerequisite>
// <li> The mathematical concept of convolution
// </prerequisite>
//
// <etymology>
// The convolver class performs convolution!
// </etymology>
//
// <synopsis>
// This class will perform linear or circular convolution on arrays. 
//
// The dimension of the convolution done is determined by the dimension of
// the point spread function (psf), so for example, if the psf is a Vector,
// one dimensional convolution will be done. The dimension of the model
// that is to be convolved must be at least the same as the point
// spread function, but it can be larger. If it is then the convolution will
// be repeated for each row or plane of the model. 
// <note role=tip> 
// This class strips all degenerate axes when determining the dimensionality
// of the psf or model. So a psf with shapes of [1,1,16] or [16,1,1] is
// treated the same as a Vector of length 16, and will result in one
// dimensional convolutions along the first non-degenerate axis of the
// supplied model.
// </note>

// Repeated convolution can only be done along the fastest moving axes of
// the supplied image. For example, if a one dimensional psf is used (so
// that one dimensional convolution is being done), and a cube of data is
// supplied then the convolution will be repeated for each row in the
// cube. It is not currently possible to have this class do repeated one
// dimensional convolution along all the columns or along the z
// axis. To do this you need to use an iterator external to the class to
// successively feed in the appropriate slices of your Array. 

// The difference between linear and circular convolution can best be
// explained with a one dimensional example.
// Suppose the psf and data to be convolved are: 
// <srcblock>
// psf = [0 .5 1 .1];  data = [1  0  0  0  0  0]
// </srcblock>
// then their linear and circular convolutions are: 
// <srcblock>
// circular convolution =         [1 .1  0  0  0 .5]
//   linear convolution =         [1 .1  0  0  0  0]    (fullSize == False)
//   linear convolution =   [0 .5  1 .1  0  0  0  0  0] (fullSize == True)
// </srcblock>
// The circular convolution "wraps around" whereas the linear one does not.
// Usage of the fullSize option is explained below. As can be seen from the
// above example this class does not normalise the convolved result by any
// factor that depends on the psf, so if the "beam area" is not unity the
// flux scales will vary.

// The "centre" of the convolution is at the point (NX/2, NY/2) (assuming a
// 2 dimensional psf)  where the first point in the psf is at (0,0) and the
// last is at (NX-1, NY-1). This means that a psf that is all zero except
// for 1 at the "centre" pixel will when convolved with any model leave the
// model unchanged. 

// The convolution is done in the Fourier domain and the transform of the
// psf (the transfer function) is cached by this class. If the cached
// transfer function is the wrong size for a given model it will be
// automatically be recomputed to the right size (this will involve two
// FFT's)

// Each convolution requires two Fourier transforms which dominate the
// computational load. Hence the computational expense is 
// <em> n Log(n) </em> for 1 dimensional and 
// <em> n^2 Log(n) </em> for 2 dimensional convolutions.

// The size of the convolved result is always the same as the input model
// unless linear convolution is done with the fullSize option set to True.
// In this case the result will be larger than the model and include the
// full linear convolution (resultSize = psfSize+modelSize-1), rather than
// the central portion.

// If the convolver is constructed with an expected model size (as in the
// example below) then the cached transfer function will be computed to a
// size appropriate for linear convolution of models of that size.  If no
// model size is given then the cached transfer function will be computed
// with a size appropriate for circular convolution. These guidelines also
// apply when using the setPsf functions. 

// <note role=tip> 
// If you are intending to do 'fullsize' linear convolutions
// you should also set the fullsize option to True as the cached transfer
// function is a different size for fullsize linear convolutions.
// </note>

// For linear convolution the psf can be larger, the same size or smaller
// than the model but for circular convolution the psf must be smaller or the
// same size. 

// The size of the cached transfer function (and also the length of the
// FFT's calculated) depends on the sizes of the psf and the model, as well
// as whether you are doing linear or circular convolution and the fullSize
// option. It is always advantageous to use the smallest possible psf
// (ie. do not pad the psf prior to supplying it to this class). Be aware
// that using odd length images will lead to this class doing odd length
// FFT's, which are less computationally effecient (particularly is the
// length of the transform is a prime number) in general than even length
// transforms.

// There are only two valid template types namely, 
// <ol>
// <li>FType=Float or
// <li>FType=Double
// </ol>
// and the user may prefer to use the following typedef's: 
// <srcblock>
// FloatConvolver (= Convolver<Float>) or
// DoubleConvolver (= Convolver<Double>)  
// </srcblock>
// rather than explicitly specifying the template arguements. 
// <note role=tip> 
// The typedefs need to be redeclared when using the gnu compiler making
// them essentially useless.  
// </note>

// When this class is constructed you may choose to have the psf
// explicitly stored by the class (by setting cachePsf=True). This will
// allow speedy access to the psf when using the getPsf function. However
// the getPsf function can still be called even if the psf has not been
// cached. Then the psf will be computed by FFT'ing the transfer
// function, and the psf will also then be cached (unless
// cachePsf=Flase). Cacheing the psf is also a good idea if you will be
// switching between different sized transfer functions (eg. mixing
// linear and circular convolution) as it will save one of the two
// FFT's. Note that even though the psf is returned as a const Array, it
// is possible to inadvertently modify it using the array copy constructor
// as this uses reference symantics. Modifying the psf is NOT
// recommended. eg. 
// <srcblock>
// DoubleConvolver conv();
// {
//   Matrix<Double> psf(20,20); 
//   conv.setPsf(psf);
// }
// Matrix<Double> convPsf = conv.getPsf(); // Get the psf used by the convolver
// convPsf(0,0) = -100;                    // And modify it. This modifies
//                                         // This internal psf used by the 
//                                         // convolver also! (unless it is
//                                         // not caching the psf)
// </srcblock> 
//
// </synopsis>
//
// <example>
// Calculate the convolution of two Matrices (psf and model);
// <srcblock>
// Matrix<Float> psf(4,4), model(12,12);
// ...put meaningful values into the above two matrices...
// FloatConvolver conv(psf, model.shape());
// conv.linearConv(result, model); // result = Convolution(psf, model)
// </srcblock> 
// </example>
//
// <motivation>
// I needed to do linear convolution to write a clean algorithm. It
// blossomed into this class.
// </motivation>
//
// <thrown>
// <li> AipsError: if psf has more dimensions than the model. 
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> the class should detect if the psf or image is small and do the
//        convolution directly rather than use the Fourier domain
//   <li> Add a lattice interface, and more flexible iteration scheme
//   <li> Allow the psf to be specified with a
//   	 <linkto class=Function>Function</linkto>. 
// </todo>

template<class FType> class Convolver
{
public:
  // When using the default constructor the psf MUST be specified using the
  // setPsf function prior to doing any convolution. 
  // <group>
  Convolver(){}
  // </group>
  // Create the cached Transfer function assuming that circular convolution
  // will be done
  // <group>
  Convolver(const Array<FType>& psf, Bool cachePsf=False);
  // </group>
  // Create the cached Transfer function assuming that linear convolution
  // with an array of size imageSize will be done. 
  // <group>
  Convolver(const Array<FType>& psf, const IPosition& imageSize, 
	    Bool fullSize=False, Bool cachePsf=False);
  // </group>

  // The copy constructor and the assignment operator make copies (and not 
  // references) of all the internal data arrays, as this object could get
  // really screwed up if the private data was silently messed with.
  // <group>
  Convolver(const Convolver<FType>& other);
  Convolver<FType> & operator=(const Convolver<FType> & other); 
  // </group>

  // The destructor does nothing!
  // <group>
  ~Convolver();
  // </group>

  // Perform linear convolution of the model with the previously
  // specified psf. Return the answer in result. Set fullSize to True if you
  // want the full convolution, rather than the central portion (the same
  // size as the model) returned.
  // <group>
  void linearConv(Array<FType>& result, 
		  const Array<FType>& model, 
		  Bool fullSize=False);
  // </group>

  // Perform circular convolution of the model with the previously
  // specified psf. Return the answer in result.
  // <group>
  void circularConv(Array<FType>& result,
		    const Array<FType>& model);
  // </group>

  // Set the transfer function for future convolutions to psf. 
  // Assume circular convolution will be done
  // <group>
  void setPsf(const Array<FType>& psf, Bool cachePsf=False);
  // </group>
  // Set the transfer function for future convolutions to psf. 
  // Assume linear convolution with a model of size imageSize
  // <group>
  void setPsf(const Array<FType>& psf, 
	      IPosition imageShape, Bool fullSize=False, 
	      Bool cachePsf=False); 
  // </group>
  // Get the psf currently used by this convolver
  // <group>
  const Array<FType> getPsf(Bool cachePsf=True); 
  // </group>

  // Set to use convolution with lesser flips
  // <group>
  void setFastConvolve(); 
  // </group>

private:
  IPosition thePsfSize;
  IPosition theFFTSize;
  Array<typename NumericTraits<FType>::ConjugateType> theXfr;
  Array<FType> thePsf;
  FFTServer<FType, typename NumericTraits<FType>::ConjugateType> theFFT;
  FFTServer<FType, typename NumericTraits<FType>::ConjugateType> theIFFT;

  void makeXfr(const Array<FType>& psf, const IPosition& imageSize,
	       Bool linear, Bool fullSize);
  void makePsf(Array<FType>& psf);
  IPosition defaultShape(const Array<FType>& psf);
  IPosition extractShape(IPosition& psfSize, const IPosition& imageSize);
  void doConvolution(Array<FType>& result, 
		     const Array<FType>& model, 
		     Bool fullSize);
  void resizeXfr(const IPosition& imageShape, Bool linear, Bool fullSize);
//#   void padArray(Array<FType>& paddedArr, const Array<FType>& origArr, 
//# 		const IPosition & blc);
  Bool valid;
  Bool doFast_p;
  void validate();
};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/Convolver.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
