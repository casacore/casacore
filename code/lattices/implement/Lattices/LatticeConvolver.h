//# Convolver.h: this defines Convolver a class for doing convolution
//# Copyright (C) 1996,1997
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

//# Forward Declarations
template <class T> class LatticeConvolver;
class IPosition;

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
  // must be specified. See the synopsis for a description of the fullSize
  // flag. 
  LatticeConvolver(const Lattice<T> & psf, const IPosition & imageSize, 
		   Bool fullSize=False);

  // The copy constructor makes copies (and not references) of all the internal
  // data.
  LatticeConvolver(const LatticeConvolver<T> & other);

  // The destructor does nothing.
  ~LatticeConvolver();

  // The assignment operator make copies (and not references) of all the  // internal data.
  LatticeConvolver<T> & operator=(const LatticeConvolver<T> & other); 

  // Perform linear convolution of the model with the previously
  // specified psf. Return the answer in result. See the synopsis for a
  // description of the fullSize flag.
  void linear(Lattice<T> & result, const Lattice<T> & model, 
	      const Bool fullSize=False);

  // Perform inplace linear convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the model. As the
  // lattice is not resized hence the fullSize flag is always False.
  void linear(Lattice<T> & modelAndResult);

  // Perform circular convolution of the model with the previously
  // specified psf. Return the answer in result.
  void circular(Lattice<T> & result, const Lattice<T> & model);

  // Perform inplace circular convolution of the model with the previously
  // specified psf. Return the result in the same Lattice as the model.
  void circular(Lattice<T> & modelAndResult);

  // Set the point spread function for future convolutions to psf.  Assume that
  // circular convolution will be done
  void setPsf(const Lattice<T> & psf);

  // Set the transfer function for future convolutions to psf.  Assume that
  // linear convolution with a model of size imageSize will be done. See the
  // synopsis for a description of the fullSize flag.
  void setPsf(const Lattice<T> & psf, 
	      const IPosition & imageShape, const Bool fullSize=False); 

  // Return the psf currently used by this convolver. The Lattice will be
  // resized to the required size regardless of its current one.
  getPsf(const Lattice<T> & psf);

private:
  //  IPosition itsPsfSize;
  //  IPosition itsFFTSize;
  TempLattice<NumericTraits<T>::ConjugateType> itsXfr;

//   void makeXfr(const Lattice<T> & psf, const IPosition & imageSize,
// 	       const Bool & linear, const Bool & fullSize);
};
#endif
