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
    CIRCULAR,
    // Real to Real transforms with symmetric Arrays.
    REALSYMMETRIC
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
  // The default constructor creates a Convolver that will convolve your data
  // with a point spread function (psf) that zero everywhere except at the
  // centre where it is one. Convolving with this psf will not change your
  // data.
  LatticeConvolver();

  // Create a convolver that is initialised to do circular convolution with the
  // specified point spread function.
  LatticeConvolver(const Lattice<T> & psf);

  // Create a convolver that is initialised to do linear convolution with the
  // specified transfer function. The size of the image you will convolve with
  // must be specified. 
  LatticeConvolver(const Lattice<T> & psf, const IPosition & imageSize);

  // The copy constructor 
  LatticeConvolver(const LatticeConvolver<T> & other);

  // The destructor does nothing.
  ~LatticeConvolver();

  // The assignment operator
  LatticeConvolver<T> & operator=(const LatticeConvolver<T> & other); 

  // Perform linear convolution of the model with the previously
  // specified psf. Return the answer in result. 
  void linear(Lattice<T> & result, const Lattice<T> & model);

  // Perform in-place linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the model.
  void linear(Lattice<T> & modelAndResult);

  // Perform circular convolution of the model with the previously
  // specified psf. Return the answer in result. 
  void circular(Lattice<T> & result, const Lattice<T> & model);

  // Perform in-place linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the model.
  void circular(Lattice<T> & modelAndResult);

  // Perform convolution on the specified model using the currently initialised
  // convolution type (linear or circular) and image for linear convolution the
  // initilaised input model shape.
  // The convolution may be either in-place or not.
  // <group>
  //  void convolve(Lattice<T> & modelAndResult) const;
  //  void convolve(Lattice<T> & result, const Lattice<T> & model) const;
  // </group>

  // Return the psf currently used by this convolver. The Lattice must be the
  // correct shape.
  void getPsf(Lattice<T> & psf) const;

private:
  void pad(TempLattice<T> & paddedModel, const Lattice<T> & model) const;
  void unpad(Lattice<T> & result, const TempLattice<T> & paddedResult) const;

  IPosition itsFFTShape;
  TempLattice<NumericTraits<T>::ConjugateType> itsXfr;

//   void makeXfr(const Lattice<T> & psf, const IPosition & imageSize,
// 	       const Bool & linear, const Bool & fullSize);
};
#endif
