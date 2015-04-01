//# Polynomial.h: A one dimensional polynomial class
//# Copyright (C) 1994,1995,1996,2001,2002,2005
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

#ifndef SCIMATH_POLYNOMIAL_H
#define SCIMATH_POLYNOMIAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/PolynomialParam.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional polynomial class
// </summary>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis> 
// A Polynomial<T> contains a set of coefficients; its fundamental operations
// is evaluating itself at some "x". The number of coefficients is the order
// of the polynomial plus one, so is the number of available parameters. 
//
// <note role=tip>
// The present implementation merely stores the coefficients in a Block. In the
// unlikely case that we need to deal with polynomials with many zero
// coefficients, a more efficient representation would be possible.
// </note>
// </synopsis> 
//
// <example>
// <srcblock>
//  Polynomial<Float> pf(3); // Third order polynomial - coeffs 0 by default
//  pf.setCoefficient(1, 1.0);
//  pf[2] = 2.0;
//  pf.setCoefficient(3, 3.0);  // 3x^3 + 2x^2 + x
//  pf(2); // == 34
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to address incorrect
//		coefficients
// </thrown>

// <todo asof="1995/08/25">
//   <li> Global functions to make various ``special'' polynomials of various
//   orders will be useful eventually.
// </todo>

template<class T> class Polynomial: public PolynomialParam<T> {
public:
  //# Enumerations
  
  //# Constructors
  // Constructs a zero'th order polynomial, with a coeficcient of 0.0.
  Polynomial() : PolynomialParam<T>() {}
  // Makes a polynomial of the given order, with all coeficcients set to
  // zero. 
  explicit Polynomial(uInt order) : PolynomialParam<T>(order) {}
  // Copy constructor/assignment (deep copy)
  // <group>
  Polynomial(const Polynomial<T> &other) : PolynomialParam<T>(other) {}
  template <class W>
  Polynomial(const Polynomial<W> &other) : PolynomialParam<T>(other) {}
  Polynomial<T> &operator=(const Polynomial<T> &other) {
    PolynomialParam<T>::operator=(other); return *this; }
  // </group>
  
  // Destructor
  virtual ~Polynomial() {}
  
  //# Operators    
  // Evaluate the polynomial at <src>x</src>.
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  
  //# Member functions
  // Return the polynomial which is the derivative of this one. <em>e.g.,</em>
  // <src> 2+4x+5x^2 --> 0+4+10x </src>.
  Polynomial<T> derivative() const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new Polynomial<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new Polynomial<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new Polynomial<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using PolynomialParam<T>::param_p;
public:
  using PolynomialParam<T>::nparameters;
  using PolynomialParam<T>::order;

};

#define Polynomial_PS Polynomial

// <summary> Partial specialization of Polynomial for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>Polynomial_PS</src> is only for cxx2html
// documentation problems. Use <src>Polynomial</src> in your code.</note>
// </synopsis>

template <class T> class Polynomial_PS<AutoDiff<T> > : 
public PolynomialParam<AutoDiff<T> > {
public:
  //# Constructors
  // Constructs one dimensional Polynomials.
  // <group>
  Polynomial_PS() : PolynomialParam<AutoDiff<T> >() {}
  explicit Polynomial_PS(uInt order) :
    PolynomialParam<AutoDiff<T> >(order) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Polynomial_PS(const Polynomial_PS<AutoDiff<T> > &other) :
    PolynomialParam<AutoDiff<T> >(other) {}
  template <class W>
  Polynomial_PS(const Polynomial_PS<W> &other) :
    PolynomialParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  Polynomial_PS<AutoDiff<T> > &
    operator=(const Polynomial_PS<AutoDiff<T> > &other) {
    PolynomialParam<AutoDiff<T> >::operator=(other); return *this; }
    
  // Destructor
  virtual ~Polynomial_PS() {}

  //# Operators    
  // Evaluate the polynomial and its derivatives at <src>x</src> <em>wrt</em>
  // to the coefficients.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new Polynomial<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new Polynomial<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new Polynomial<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using PolynomialParam<AutoDiff<T> >::param_p;
public:
  using PolynomialParam<AutoDiff<T> >::nparameters;
  using PolynomialParam<AutoDiff<T> >::order;
};

#undef Polynomial_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Polynomial.tcc>
#include <casacore/scimath/Functionals/Polynomial2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
