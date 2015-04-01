//# OddPolynomial.h: A one dimensional odd polynomial class
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

#ifndef SCIMATH_ODDPOLYNOMIAL_H
#define SCIMATH_ODDPOLYNOMIAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/OddPolynomialParam.h>
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
// A OddPolynomial<T> contains a set of coefficients;
// its fundamental operation is evaluating itself at some "x".
// The number of coefficients is the order of the polynomial divided by two,
// plus one, so is the number of available parameters. 
//
// </synopsis> 
//
// <example>
// <srcblock>
//  OddPolynomial<Float> pf(3);  // Third order polynomial - coeffs 0 by default
//  pf.setCoefficient(0, 1.0);
//  pf[1] = 2.0;		// 2x^3 + 1x^1
//  pf(2); // == 18
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
//   <li> Nothing I know off
// </todo>

template<class T> class OddPolynomial: public OddPolynomialParam<T>
{
public:
  //# Enumerations
  
  //# Constructors
  // Constructs a first order polynomial, with a coeficcient of 0.0.
  OddPolynomial() : OddPolynomialParam<T>() {}
  // Makes a polynomial of the given order, with all coeficcients set to
  // zero. 
  explicit OddPolynomial(uInt order) : OddPolynomialParam<T>(order) {}
  // Copy constructor/assignment (deep copy)
  // <group>
  OddPolynomial(const OddPolynomial<T> &other) :
    OddPolynomialParam<T>(other) {}
  template <class W>
    OddPolynomial(const OddPolynomial<W> &other) :
    OddPolynomialParam<T>(other) {}
  OddPolynomial<T> &operator=(const OddPolynomial<T> &other) {
    OddPolynomialParam<T>::operator=(other); return *this; }
  // </group>
  
  // Destructor
  virtual ~OddPolynomial() {}
  
  //# Operators    
  // Evaluate the polynomial at <src>x</src>.
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new OddPolynomial<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new OddPolynomial<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new OddPolynomial<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using OddPolynomialParam<T>::param_p;
public:
  using OddPolynomialParam<T>::nparameters;
};

#define OddPolynomial_PS OddPolynomial

// <summary> Partial specialization of OddPolynomial for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>OddPolynomial_PS</src> is only for cxx2html
// documentation problems. Use <src>OddPolynomial</src> in your code.</note>
// </synopsis>

template <class T> class OddPolynomial_PS<AutoDiff<T> > : 
public OddPolynomialParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs one dimensional OddPolynomials.
  // <group>
  OddPolynomial_PS() : OddPolynomialParam<AutoDiff<T> >() {}
  explicit OddPolynomial_PS(uInt order) :
    OddPolynomialParam<AutoDiff<T> >(order) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  OddPolynomial_PS(const OddPolynomial_PS<AutoDiff<T> > &other) :
  OddPolynomialParam<AutoDiff<T> >(other) {}
  template <class W>
    OddPolynomial_PS(const OddPolynomial_PS<W> &other) :
    OddPolynomialParam<AutoDiff<T> >(other) {}
  // </group>
  // Copy assignment (deep copy)
  OddPolynomial_PS<AutoDiff<T> > &
    operator=(const OddPolynomial_PS<AutoDiff<T> > &other) {
    OddPolynomialParam<AutoDiff<T> >::operator=(other); return *this; }
    
  // Destructor
  virtual ~OddPolynomial_PS() {}

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
    return new OddPolynomial<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new OddPolynomial<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new OddPolynomial<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using OddPolynomialParam<AutoDiff<T> >::param_p;
public:
  using OddPolynomialParam<AutoDiff<T> >::nparameters;
};

#undef OddPolynomial_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/OddPolynomial.tcc>
#include <casacore/scimath/Functionals/OddPolynomial2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
