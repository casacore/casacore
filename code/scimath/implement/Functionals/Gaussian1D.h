//# NQGaussian1D.h: A one-dimensional Gaussian class
//# Copyright (C) 1995,1996,1997,2001
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

#if !defined(AIPS_NQGAUSSIAN1D_H)
#define AIPS_NQGAUSSIAN1D_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/NQGaussian1DParam.h>
#include <aips/Functionals/NQFunction1D.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffMath.h>

//# Forward declarations

// <summary> A one dimensional Gaussian class.</summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tNQGaussian1D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="NQGaussian1DParam">NQGaussian1DParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A NQGaussian1D functional is designed exclusively for calculating a
// Gaussian (or Normal) distribution in one dimension. Other classes exist
// for calculating these functions in two
// (<linkto class=NQGaussian2D>NQGaussian2D</linkto>) and N 
// (<linkto class=NQGaussianND>NQGaussianND</linkto>) dimensions.
// </etymology>

// <synopsis> 
// A <src>NQGaussian1D</src> is described by a height, center, and width. Its
// fundamental operation is evaluating itself at some <src>x</src>.
// The parameters (height, center and width) may be changed at run time. 
//
// The width of the Gaussian (for the constructors or the <src>setWidth
// </src> function) is always specified in terms of the full width at half
// maximum (FWHM). It is always positive and attempts to set a non-positive
// width will throw an assertion when in debug mode.
//
// The peak height of the Gaussian can be specified at construction time or
// by using the <src> setHeight </src> function. Alternatively the <src>
// setFlux </src> function can be used to implicitly set the peak height by
// specifying the integrated area under the Gaussian. The height (or flux)
// can be positive, negative or zero, as this class makes no assumptions on
// what quantity the height represents.
//
// <note role=tip> Changing the width of the Gaussian will not affect
// its peak height but will change its flux. So you should always set the
// width before setting the flux. </note>
//
// The parameter interface (see 
// <linkto class="NQGaussian1DParam">NQGaussian1DParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// There are 3 parameters that are used to describe the Gaussian:
// <ol>
// <li> The height of the Gaussian. This is identical to the value 
//      returned using the <src>height()</src> member function.
// <li> The center of the Gaussian in the x direction. This is identical to
//      the value returned using the <src>center()</src> member function. 
// <li> The width (FWHM) of the Gaussian. To aid convergence of
//      the non-linear fitting routines this parameter is allowed to be
//      negative. This does not affect the shape of the Gaussian as the
//	square of the width is used when evaluating the function.
// </ol>
//
// An enumeration for the <src>HEIGHT</src>, <src>WIDTH</src> and
// <src>CENTER</src> parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The 
// <src>mask()</src> methods can be used to check and set the parameter masks.
//
// </synopsis>

// <example>
// <srcblock>
//    Gaussian<Double> gf(5.0, 25.0, 7);
//    gf(25);            // = 5.0
//    gf[HEIGHT](1.0);
//    gf.setWidth(2.0);                
//    gf[CENTER](0.0);
//    gf(1);             // = 0.5*height = 0.5
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
//    <li> Assertion in debug mode if operator(Vector<>) with empty Vector
// </thrown>

// <todo asof="2001/08/19">
//   <li> Gaussians that know about their DFT's could be required eventually.
// </todo>

template<class T> class NQGaussian1D : public NQGaussian1DParam<T> {
 public:
  //# Enumerations
  
  //# Constructors
  // Constructs the one dimensional Gaussians. Defaults:
  // height=1, center=0, width(FWHM)=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  NQGaussian1D() : NQGaussian1DParam<T>() {};
  explicit NQGaussian1D(const T &height) : NQGaussian1DParam<T>(height) {};
  NQGaussian1D(const T &height, const T &center) :
    NQGaussian1DParam<T>(height, center) {};
  NQGaussian1D(const T &height, const T &center, const T &width) :
    NQGaussian1DParam<T>(height, center, width) {};
  // </group>

  // Copy constructor (deep copy)
  NQGaussian1D(const NQGaussian1D<T> &other) : NQGaussian1DParam<T>(other) {};

  // Copy assignment (deep copy)
  NQGaussian1D<T> &operator=(const NQGaussian1D<T> &other) {
    NQGaussian1DParam<T>::operator=(other); return *this; };
    
  // Destructor
  virtual ~NQGaussian1D() {};

  //# Operators    
  // Evaluate the Gaussian at <src>x</src>.
  // <group>
  virtual T eval(typename NQFunction1D<T>::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<T> *clone() const { return new NQGaussian1D<T>(*this); };
  // </group>

};

#define NQGaussian1D_PS NQGaussian1D

// <summary> Partial specialization of NQGaussian1D for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>NQGaussian1D_PS</src> is only for cxx2html
// documentation problems. Use <src>NQGaussian1D</src> in your code.</note>
// </synopsis>

template <class T> class NQGaussian1D_PS<AutoDiff<T> > : 
public NQGaussian1DParam<AutoDiff<T> > {
 public:
  //# Constructors
  // Constructs one dimensional Gaussians.
  // <group>
  NQGaussian1D_PS() : NQGaussian1DParam<AutoDiff<T> >() {};
  explicit NQGaussian1D_PS(const AutoDiff<T> &height) :
    NQGaussian1DParam<AutoDiff<T> >(height) {};
  NQGaussian1D_PS(const AutoDiff<T> &height, const AutoDiff<T> &center) :
    NQGaussian1DParam<AutoDiff<T> >(height, center) {};
  NQGaussian1D_PS(const AutoDiff<T> &height, const AutoDiff<T> &center,
		  const AutoDiff<T> &width) :
    NQGaussian1DParam<AutoDiff<T> >(height, center, width) {};;
  // </group>

  // Copy constructor (deep copy)
  NQGaussian1D_PS(const NQGaussian1D_PS &other) :
    NQGaussian1DParam<AutoDiff<T> >(other) {};

  // Copy assignment (deep copy)
  NQGaussian1D_PS<AutoDiff<T> > &
    operator=(const NQGaussian1D_PS<AutoDiff<T> > &other) {
    NQGaussian1DParam<AutoDiff<T> >::operator=(other); return *this; };
    
  // Destructor
  virtual ~NQGaussian1D_PS() {};

  //# Operators    
  // Evaluate the Gaussian and its derivatives at <src>x</src>.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new NQGaussian1D<AutoDiff<T> >(*this); };
  // </group>

};

#undef NQGaussian1D_PS

#endif
