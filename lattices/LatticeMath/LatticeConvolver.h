//# Convolver.h: this defines Convolver a class for doing convolution
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef LATTICES_LATTICECONVOLVER_H
#define LATTICES_LATTICECONVOLVER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
//template <class T> class LatticeConvolver;
class IPosition;

// <summary>Lists the different types of Convolutions that can be done</summary>
// <synopsis>This enumerator is brought out as a separate class because g++
// currently cannot handle enumerators in a templated class. When it can this
// class will go away and this enumerator moved into the Convolver
// class</synopsis>
class ConvEnums {
public:
  enum ConvType {
    // Linear convolution
    LINEAR,
    // Circular Convolution
    CIRCULAR
    //# Assume the point spread function is symmetric
    //#REALSYMMETRIC
  };
};

// <summary>A class for doing multi-dimensional convolution</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeConvolver">
// </reviewed>

// <prerequisite>
//  <li> The mathematical concept of convolution
// </prerequisite>
//
// <etymology>
// The LatticeConvolver class will convolve Lattices. This class
// complements the Convolver class which will convolve Arrays.  
// </etymology>
//
// <synopsis>
// This class performs linear or circular convolution on Lattices. See the
// <linkto class="Convolver">Convolver</linkto> class description of the
// difference between linear and circular convolution. 

// This class does convolutions by multiplying the Fourier transforms of the
// supplied Lattices and returning the inverse transform of the product. This
// is the best algorithm to use when the point spread function is large. This
// class does all the padding with zeros necessary to implement this
// algorithm. Hence the 

// </synopsis>
//
// <example>
// <srcblock>
// 
// </srcblock> 
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
// <li> AipsError: if psf and model have a differing numbers of dimensions
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> the class should detect if the psf or image is small and do the
//        convolution directly rather than use the Fourier domain
//   <li> Allow the psf to be specified with a
//   	 <linkto class=Function>Function</linkto>. 
// </todo>

template<class T> class LatticeConvolver
{
public:
  // The default constructor creates a LatticeConvolver that will convolve your
  // data with a point spread function (psf) that zero everywhere except at the
  // centre where it is one. Convolving with this psf will not change your
  // data.
  LatticeConvolver();

  // Create a convolver that is initialised to do circular convolution with the
  // specified point spread function. It is assumed that the supplied model
  // will be the same shape as the point spread function.
  LatticeConvolver(const Lattice<T> & psf, Bool doFast=False);

  // Create a convolver that is initialised to do linear convolution with the
  // specified point spread function. The size of the model you will convolve
  // with must be specified.
  LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape, 
		   Bool doFast=False);

  // Create a convolver that is initialised to do the specified type of
  // convolution with the specified point spread function. The size of the
  // model you expect to convolve with must be specified.
  LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape,
  		   ConvEnums::ConvType type, Bool doFast=False);

  // The copy constructor uses reference semantics
  LatticeConvolver(const LatticeConvolver<T> & other);

  // The assignment operator also uses reference semantics
  LatticeConvolver<T> & operator=(const LatticeConvolver<T> & other); 

  // The destructor does nothing special.
  ~LatticeConvolver();

  // Perform linear convolution of the model with the previously specified
  // psf. The supplied Lattices must be the same shape.
  void linear(Lattice<T> & result, const Lattice<T> & model);

  // Perform in-place linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the
  // model. 
  void linear(Lattice<T> & modelAndResult);

  // Perform circular convolution of the model with the previously
  // specified psf. Return the answer in result.
  void circular(Lattice<T> & result, const Lattice<T> & model);

  // Perform in-place linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the model.
  void circular(Lattice<T> & modelAndResult);

  // Perform convolution on the specified model using the currently initialised
  // convolution type (linear or circular). These functions will not resize the
  // LatticeConvolver if the supplied Lattice is the wrong shape.
  //
  // If the LatticeConvolver is setup for circular Convolution then the size of
  // the supplied model must be less than or equal to the shape returned by the
  // fftshape() function, which is usually the same as the shape of the psf.
  //
  // If the LatticeConvolver is setup to do linear convolution the the
  // input and output Lattices must have the same shape as the result from the
  // shape() member function. The convolution may be either in-place or not.
  // <group>
  void convolve(Lattice<T> & modelAndResult) const;
  void convolve(Lattice<T> & result, const Lattice<T> & model) const;
  // </group>

  // Return the psf currently used by this convolver. The supplied Lattice must
  // be the correct shape ie., the same as returned by the psfShape member
  // function.
  void getPsf(Lattice<T> & psf) const;

  // Resize the LatticeConvolver to do convolutions of the specified type and
  // shape. The supplied function must always have the same number of
  // dimensions as the internal point spread function (which can be found using
  // the shape member function). The LatticeConvolver will be set up to do
  // circular or linear convolutions depending on the supplied type
  void resize(const IPosition & modelShape, ConvEnums::ConvType type);

  // Returns the shape of the Lattices that the convolver will convolve. This
  // shape will always have as many dimensions as the psf that was used to
  // initialise the LatticeConvolver. If the LatticeConvolver is setup to do
  // circular convolutions then every axis of the returned IPosition will be
  // zero length. If the LatticeConvolver is setup to do linear convolutions
  // then the returned IPosition will have a positive values on each axis that
  // indicate the expected shape of the input model.
  IPosition shape() const;

  // Returns the shape of the point spread function that the LatticeConvolver
  // was initialised with.
  IPosition psfShape() const;

  // Returns the type of convolution the LatticeConvolver is currently set up
  // to do.
  ConvEnums::ConvType type() const;

  // Returns the shape of the FFT's that the LatticeConvolver will do when
  // performing the convolution. Not really useful except as a diagnostic
  // tool. If the shape contains a lot of poorly factorisable lengths then the
  // convolution will be slow.
  IPosition fftShape() const;

  // Set usage of fast convolve with lesser flips
  void setFastConvolve();

private:
  //# The following functions are used in various places in the code and are
  //# documented in the .cc file. Static functions are used when the functions
  //# do not use the object state. They ensure that implicit assumptions
  //# about the current state and implicit side-effects are not possible
  //# because all information must be suplied in the input arguments
  static void pad(Lattice<T> & paddedLat, const Lattice<T> & inLat);
  static void unpad(Lattice<T> & result, const Lattice<T> & paddedResult);
  void makeXfr(const Lattice<T> & psf);
  void makePsf(Lattice<T> & psf) const;
  static IPosition calcFFTShape(const IPosition & psfShape, 
				const IPosition & modelShape,
				ConvEnums::ConvType type);

  IPosition itsPsfShape;
  IPosition itsModelShape;
  ConvEnums::ConvType itsType;
  IPosition itsFFTShape;
  TempLattice<typename NumericTraits<T>::ConjugateType>* itsXfr;
  TempLattice<T>* itsPsf;
  Bool itsCachedPsf;
  Bool doFast_p;
};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeConvolver.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

