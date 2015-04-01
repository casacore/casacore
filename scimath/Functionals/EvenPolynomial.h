//# EvenPolynomial.h: A one dimensional even polynomial class
//# Copyright (C) 2002,2005
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

#ifndef SCIMATH_EVENPOLYNOMIAL_H
#define SCIMATH_EVENPOLYNOMIAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/EvenPolynomialParam.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional odd polynomial class
// </summary>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tSpecialPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis> 
// An EvenPolynomial<T> contains a set of coefficients;
// its fundamental operation is evaluating itself at some "x".
// The number of coefficients is the order of the polynomial divided by two,
// plus one, so is the number of available parameters. 
//
// </synopsis> 
//
// <example>
// <srcblock>
//  EvenPolynomial<Float> pf(3);  // Second order polynomial - coeffs 0 by default
//  pf.setCoefficient(0, 1.0);
//  pf[1] = 2.0;		// 2x^2 + 1x^0
//  pf(2); // == 8
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

// <todo asof="2002/02/26">
//   <li> Nothing I know of
// </todo>

template<class T> class EvenPolynomial: public EvenPolynomialParam<T>
{
public:
  //# Enumerations
  
  //# Constructors
  // Constructs a zeroth order polynomial, with a coeficcient of 0.0.
  EvenPolynomial() : EvenPolynomialParam<T>() {}
  // Makes a polynomial of the given order, with all coeficcients set to
  // zero. 
  explicit EvenPolynomial(uInt order) : EvenPolynomialParam<T>(order) {}
  // Copy constructor/assignment (deep copy)
  // <group>
  EvenPolynomial(const EvenPolynomial<T> &other) :
    EvenPolynomialParam<T>(other) {}
  template <class W>
    EvenPolynomial(const EvenPolynomial<W> &other) :
    EvenPolynomialParam<T>(other) {}
  EvenPolynomial<T> &operator=(const EvenPolynomial<T> &other) {
    EvenPolynomialParam<T>::operator=(other); return *this; }
  // </group>
  
  // Destructor
  virtual ~EvenPolynomial() {}
  
  //# Operators    
  // Evaluate the polynomial at <src>x</src>.
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new EvenPolynomial<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new EvenPolynomial<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new EvenPolynomial<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using EvenPolynomialParam<T>::param_p;
public:
  using EvenPolynomialParam<T>::nparameters;
};

#define EvenPolynomial_PS EvenPolynomial

// <summary> Partial specialization of EvenPolynomial for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>EvenPolynomial_PS</src> is only for cxx2html
// documentation problems. Use <src>EvenPolynomial</src> in your code.</note>
// </synopsis>

template <class T> class EvenPolynomial_PS<AutoDiff<T> > : 
public EvenPolynomialParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs one dimensional EvenPolynomials.
  // <group>
  EvenPolynomial_PS() : EvenPolynomialParam<AutoDiff<T> >() {}
  explicit EvenPolynomial_PS(uInt order) :
    EvenPolynomialParam<AutoDiff<T> >(order) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  EvenPolynomial_PS(const EvenPolynomial_PS<AutoDiff<T> > &other) :
    EvenPolynomialParam<AutoDiff<T> >(other) {}
  template <class W>
    EvenPolynomial_PS(const EvenPolynomial_PS<W> &other) :
    EvenPolynomialParam<AutoDiff<T> >(other) {}
  // </group>
  // Copy assignment (deep copy)
  EvenPolynomial_PS<AutoDiff<T> > &
    operator=(const EvenPolynomial_PS<AutoDiff<T> > &other) {
    EvenPolynomialParam<AutoDiff<T> >::operator=(other); return *this; }

  // Destructor
  virtual ~EvenPolynomial_PS() {}

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
    return new EvenPolynomial<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new EvenPolynomial<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new EvenPolynomial<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using EvenPolynomialParam<AutoDiff<T> >::param_p;
public:
  using EvenPolynomialParam<AutoDiff<T> >::nparameters;
};

#undef EvenPolynomial_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/EvenPolynomial.tcc>
#include <casacore/scimath/Functionals/EvenPolynomial2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
