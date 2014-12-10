//# Gaussian1D.h: A one-dimensional Gaussian class
//# Copyright (C) 1995,1996,1997,2001,2002,2005
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

#ifndef SCIMATH_GAUSSIAN1D_H
#define SCIMATH_GAUSSIAN1D_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Gaussian1DParam.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional Gaussian class.</summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tGaussian1D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Gaussian1DParam">Gaussian1DParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A Gaussian1D functional is designed exclusively for calculating a
// Gaussian (or Normal) distribution in one dimension. Other classes exist
// for calculating these functions in two
// (<linkto class=Gaussian2D>Gaussian2D</linkto>) and N 
// (<linkto class=GaussianND>GaussianND</linkto>) dimensions.
// </etymology>

// <synopsis> 
// A <src>Gaussian1D</src> is described by a height, center, and width. Its
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
// <linkto class="Gaussian1DParam">Gaussian1DParam</linkto> class), 
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

template<class T> class Gaussian1D : public Gaussian1DParam<T> {
public:
  //# Enumerations
  
  //# Constructors
  // Constructs the one dimensional Gaussians. Defaults:
  // height=1, center=0, width(FWHM)=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  Gaussian1D() : Gaussian1DParam<T>() {}
  explicit Gaussian1D(const T &height) : Gaussian1DParam<T>(height) {}
  Gaussian1D(const T &height, const T &center) :
    Gaussian1DParam<T>(height, center) {}
  Gaussian1D(const T &height, const T &center, const T &width) :
    Gaussian1DParam<T>(height, center, width) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Gaussian1D(const Gaussian1D<T> &other) : Gaussian1DParam<T>(other) {}
  template <class W>
    Gaussian1D(const Gaussian1D<W> &other) : Gaussian1DParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  Gaussian1D<T> &operator=(const Gaussian1D<T> &other) {
    Gaussian1DParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~Gaussian1D() {}

  //# Operators    
  // Evaluate the Gaussian at <src>x</src>.
  // <group>
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<T> *clone() const { return new Gaussian1D<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new Gaussian1D<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new Gaussian1D<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using Gaussian1DParam<T>::param_p;
public:
  using Gaussian1DParam<T>::HEIGHT;
  using Gaussian1DParam<T>::CENTER;
  using Gaussian1DParam<T>::WIDTH;
  using Gaussian1DParam<T>::fwhm2int;
};


#define Gaussian1D_PS Gaussian1D

// <summary> Partial specialization of Gaussian1D for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>Gaussian1D_PS</src> is only for cxx2html
// documentation problems. Use <src>Gaussian1D</src> in your code.</note>
// </synopsis>

template <class T> class Gaussian1D_PS<AutoDiff<T> > : 
public Gaussian1DParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs one dimensional Gaussians.
  // <group>
  Gaussian1D_PS() : Gaussian1DParam<AutoDiff<T> >() {}
  explicit Gaussian1D_PS(const AutoDiff<T> &height) :
    Gaussian1DParam<AutoDiff<T> >(height) {}
  Gaussian1D_PS(const AutoDiff<T> &height, const AutoDiff<T> &center) :
    Gaussian1DParam<AutoDiff<T> >(height, center) {}
  Gaussian1D_PS(const AutoDiff<T> &height, const AutoDiff<T> &center,
		  const AutoDiff<T> &width) :
    Gaussian1DParam<AutoDiff<T> >(height, center, width) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Gaussian1D_PS(const Gaussian1D_PS &other) :
    Gaussian1DParam<AutoDiff<T> >(other) {}
  template <class W>
  Gaussian1D_PS(const Gaussian1D_PS<W> &other) :
    Gaussian1DParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  Gaussian1D_PS<AutoDiff<T> > &
    operator=(const Gaussian1D_PS<AutoDiff<T> > &other) {
    Gaussian1DParam<AutoDiff<T> >::operator=(other); return *this; }
    
  // Destructor
  virtual ~Gaussian1D_PS() {}

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
    return new Gaussian1D<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new Gaussian1D<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new Gaussian1D<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using Gaussian1DParam<AutoDiff<T> >::param_p;
public:
  using Gaussian1DParam<AutoDiff<T> >::HEIGHT;
  using Gaussian1DParam<AutoDiff<T> >::CENTER;
  using Gaussian1DParam<AutoDiff<T> >::WIDTH;
  using Gaussian1DParam<AutoDiff<T> >::fwhm2int;
};

#undef Gaussian1D_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Gaussian1D.tcc>
#include <casacore/scimath/Functionals/Gaussian1D2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
