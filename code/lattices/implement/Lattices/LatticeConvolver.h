//# Convolver.h: this defines Convolver a class for doing convolution
//# Copyright (C) 1996,1997,1998
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

#if !defined(AIPS_LATTICECONVOLVER_H)
#define AIPS_LATTICECONVOLVER_H

//# Includes
#include <aips/aips.h>
#include <aips/Mathematics/NumericTraits.h>
#include <trial/Lattices/TempLattice.h>
#include <aips/Lattices/IPosition.h>

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

// <prerequisite> The mathematical concept of convolution </prerequisite>
//
// <etymology>
// The convolver class performs convolution!
// </etymology>
//
// <synopsis>
// This class will perform linear or circular convolution on arrays. 
//
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock> 
// </example>
//
// <motivation>
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
//   <li> Allow the psf to be specified with a FunctionND. 
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
  // specified point spread function.
  LatticeConvolver(const Lattice<T> & psf);

  // Create a convolver that is initialised to do linear convolution with the
  // specified point spread function. The size of the model you will convolve
  // with must be specified.
  LatticeConvolver(const Lattice<T> & psf, const IPosition & modelShape);

  // The destructor does nothing special.
  ~LatticeConvolver();

  // Perform linear convolution of the model with the previously specified
  // psf. The supplied Lattices must be the same shape and currently this must
  // be the same as the model shape specified when the class was
  // constructed. In the future this restriction will be relaxed and the class
  // will automatically adjust if the Lattices are different sizes.
  void linear(Lattice<T> & result, const Lattice<T> & model);

  // Perform in-place linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the
  // model. Currently the supplied Lattice must be the same as the model shape
  // specified when the class was constructed. In the future this restriction
  // will be relaxed and the class will automatically adjust if the Lattices
  void linear(Lattice<T> & modelAndResult);

  // Perform circular convolution of the model with the previously
  // specified psf. Return the answer in result.
  void circular(Lattice<T> & result, const Lattice<T> & model);

  // Perform in-place linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the model.
  void circular(Lattice<T> & modelAndResult);

  // Perform convolution on the specified model using the currently initialised
  // convolution type (linear or circular) and model for linear convolution the
  // initialised input model shape. These functions will not resize the
  // LatticeConvolver if the supplied Lattice is the wrong shape. If the
  // LatticeConvolver is setup for circular Convolution then any size Lattice
  // (with the same number of dimensions as the specified psf) can be used. If
  // the LatticeConvolver is setup to do linear convolution the the input and
  // output Lattices must have the same shape as the result from the shape()
  // member function. The convolution may be either in-place or not.
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
  // circular convolutions if the supplied IPosition has zero length on all
  // axes. The LatticeConvolver will be set up to do linear convolutions if the
  // supplied IPosition has positive values on all axes. Then the supplied
  // shape is the shape of the model Lattices (eg., used in the linear member
  // function).
  void resize(const IPosition & modelShape);

  // Returns the shape of the Lattices that the convolver will convolve. This
  // shape will always have as many dimensions as the psf that was used to
  // initialise the LatticeConvolver. If the LatticeConvolver is setup to do
  // circular convolutions then every axis of the returned IPosition will be
  // zero length. If the LatticeConvolver is setup to do linear convolutions
  // then the returned IPosition will have a positive values on each axis that
  // indicate the expected shape of the input model.
  IPosition shape() const;

  // Returns the shape of the FFT's that the LatticeConvolver will do when
  // performing the convolution. Not really useful except as a diagnostic
  // tool. If the shape contains a lot of poorly factorisable lengths then the
  // convolution will be slow.
  IPosition fftShape() const;

  // Returns the shape of the point spread function that the LatticeConvolver
  // was initialised with.
  IPosition psfShape() const;

private:
  //# The following functions are defined to avoid default versions being
  //# generated by the compiler. They are made private to prevent users from
  //# using them.
  //# <group>
  LatticeConvolver(const LatticeConvolver<T> & other);
  LatticeConvolver<T> & operator=(const LatticeConvolver<T> & other); 
  //# </group>

  //# The following functions are used in various places in the code and are
  //# documented in the .cc file. Static functions are used when the functions
  //# do not modify the object state. They ensure that implicit assumptions
  //# about the current state and implicit side-effects are not possible
  //# because all information must be supliied in the input arguments
  static void pad(Lattice<T> & paddedLat, const Lattice<T> & inLat);
  static void unpad(Lattice<T> & result, const Lattice<T> & paddedResult);
  void makeXfr(const Lattice<T> & psf);
  void makePsf(Lattice<T> & psf) const;
  static IPosition calcFFTShape(const IPosition & psfShape, 
				const IPosition & modelShape);

  IPosition itsPsfShape;
  IPosition itsModelShape;
  IPosition itsFFTShape;
  TempLattice<NumericTraits<T>::ConjugateType> itsXfr;
  TempLattice<T> itsPsf;
  Bool itsCachedPsf;
  
//   void makeXfr(const Lattice<T> & psf, const IPosition & imageSize,
// 	       const Bool & linear, const Bool & fullSize);
};
#endif
