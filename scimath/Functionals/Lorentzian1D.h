//# Lorentzian1D.h: A one-dimensional Lorentzian class
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

#ifndef SCIMATH_LORENTZIAN1D_H
#define SCIMATH_LORENTZIAN1D_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Lorentzian1DParam.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional Lorentzian class.</summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tLorentzian1D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lorentzian1DParam">Lorentzian1DParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A Lorentzian1D functional is designed exclusively for calculating a
// Lorentzian (or Normal) distribution in one dimension.
//# Other classes exist (not yet!)
//# for calculating these functions in two
//# (<linkto class=Lorentzian2D>Lorentzian2D</linkto>) and N 
//# (<linkto class=LorentzianND>LorentzianND</linkto>) dimensions.
// </etymology>

// <synopsis> 
// A <src>Lorentzian1D</src> is described by a height, center, and width. Its
// fundamental operation is evaluating itself at some <src>x</src>.
// The parameters (height, center and width) may be changed at run time. 
//
// The width of the Lorentzian (for the constructors or the <src>setWidth
// </src> function) is always specified in terms of the full width at half
// maximum (FWHM). It is always positive and attempts to set a non-positive
// width will throw an assertion when in debug mode.
//
// The peak height of the Lorentzian can be specified at construction time or
// by using the <src> setHeight </src> function. Alternatively the <src>
// setFlux </src> function can be used to implicitly set the peak height by
// specifying the integrated area under the Lorentzian. The height (or flux)
// can be positive, negative or zero, as this class makes no assumptions on
// what quantity the height represents.
//
// <note role=tip> Changing the width of the Lorentzian will not affect
// its peak height but will change its flux. So you should always set the
// width before setting the flux. </note>
//
// The parameter interface (see 
// <linkto class="Lorentzian1DParam">Lorentzian1DParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// There are 3 parameters that are used to describe the Lorentzian:
// <ol>
// <li> The height of the Lorentzian. This is identical to the value 
//      returned using the <src>height()</src> member function.
// <li> The center of the Lorentzian in the x direction. This is identical to
//      the value returned using the <src>center()</src> member function. 
// <li> The width (FWHM) of the Lorentzian. To aid convergence of
//      the non-linear fitting routines this parameter is allowed to be
//      negative. This does not affect the shape of the Lorentzian as the
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
//    Lorentzian<Double> gf(5.0, 25.0, 7);
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
//   <li> Lorentzians that know about their DFT's could be required eventually.
// </todo>

template<class T> class Lorentzian1D : public Lorentzian1DParam<T> {
public:
  //# Enumerations
  
  //# Constructors
  // Constructs the one dimensional Lorentzians. Defaults:
  // height=1, center=0, width(FWHM)=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  Lorentzian1D() : Lorentzian1DParam<T>() {};
  explicit Lorentzian1D(const T &height) : Lorentzian1DParam<T>(height) {};
  Lorentzian1D(const T &height, const T &center) :
    Lorentzian1DParam<T>(height, center) {};
  Lorentzian1D(const T &height, const T &center, const T &width) :
    Lorentzian1DParam<T>(height, center, width) {};
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Lorentzian1D(const Lorentzian1D<T> &other) : Lorentzian1DParam<T>(other) {};
  template <class W>
    Lorentzian1D(const Lorentzian1D<W> &other) : Lorentzian1DParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  Lorentzian1D<T> &operator=(const Lorentzian1D<T> &other) {
    Lorentzian1DParam<T>::operator=(other); return *this; };
    
  // Destructor
  virtual ~Lorentzian1D() {};

  //# Operators    
  // Evaluate the Lorentzian at <src>x</src>.
  // <group>
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<T> *clone() const { return new Lorentzian1D<T>(*this); };
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new Lorentzian1D<typename FunctionTraits<T>::DiffType>(*this); };
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new Lorentzian1D<typename FunctionTraits<T>::BaseType>(*this); };
  // </group>

  //# Make members of parent classes known.
protected:
  using Lorentzian1DParam<T>::param_p;
public:
  using Lorentzian1DParam<T>::HEIGHT;
  using Lorentzian1DParam<T>::CENTER;
  using Lorentzian1DParam<T>::WIDTH;
  using Lorentzian1DParam<T>::fwhm2int;
};


#define Lorentzian1D_PS Lorentzian1D

// <summary> Partial specialization of Lorentzian1D for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>Lorentzian1D_PS</src> is only for cxx2html
// documentation problems. Use <src>Lorentzian1D</src> in your code.</note>
// </synopsis>

template <class T> class Lorentzian1D_PS<AutoDiff<T> > : 
public Lorentzian1DParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs one dimensional Lorentzians.
  // <group>
  Lorentzian1D_PS() : Lorentzian1DParam<AutoDiff<T> >() {};
  explicit Lorentzian1D_PS(const AutoDiff<T> &height) :
    Lorentzian1DParam<AutoDiff<T> >(height) {};
  Lorentzian1D_PS(const AutoDiff<T> &height, const AutoDiff<T> &center) :
    Lorentzian1DParam<AutoDiff<T> >(height, center) {};
  Lorentzian1D_PS(const AutoDiff<T> &height, const AutoDiff<T> &center,
		  const AutoDiff<T> &width) :
    Lorentzian1DParam<AutoDiff<T> >(height, center, width) {};
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Lorentzian1D_PS(const Lorentzian1D_PS &other) :
    Lorentzian1DParam<AutoDiff<T> >(other) {};
  template <class W>
  Lorentzian1D_PS(const Lorentzian1D_PS<W> &other) :
    Lorentzian1DParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  Lorentzian1D_PS<AutoDiff<T> > &
    operator=(const Lorentzian1D_PS<AutoDiff<T> > &other) {
    Lorentzian1DParam<AutoDiff<T> >::operator=(other); return *this; };
    
  // Destructor
  virtual ~Lorentzian1D_PS() {};

  //# Operators    
  // Evaluate the Lorentzian and its derivatives at <src>x</src>.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new Lorentzian1D<AutoDiff<T> >(*this); };
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new Lorentzian1D<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); };
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new Lorentzian1D<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); };
  // </group>

  //# Make members of parent classes known.
protected:
  using Lorentzian1DParam<AutoDiff<T> >::param_p;
public:
  using Lorentzian1DParam<AutoDiff<T> >::HEIGHT;
  using Lorentzian1DParam<AutoDiff<T> >::CENTER;
  using Lorentzian1DParam<AutoDiff<T> >::WIDTH;
  using Lorentzian1DParam<AutoDiff<T> >::fwhm2int;
};

#undef Lorentzian1D_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include "Lorentzian1D.tcc"
#include "Lorentzian1D2.tcc"
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
