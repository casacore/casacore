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
//# $Id: Polynomial.h 21024 2011-03-01 11:46:18Z gervandiepen $

#ifndef SCIMATH_POWERLOGARITHMICPOLYNOMIAL_H
#define SCIMATH_POWERLOGARITHMICPOLYNOMIAL_H

//# Includes
#include <casa/aips.h>
#include <scimath/Functionals/PowerLogarithmicPolynomialParam.h>
#include <scimath/Functionals/Function1D.h>
#include <scimath/Mathematics/AutoDiff.h>
#include <scimath/Mathematics/AutoDiffMath.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward declarations

// <summary> A one dimensional power logarithmic polynomial class of form
// y = c_0 * x**( c_1 + c_2*ln(x) + c_3*ln(x)**2 + ... c_n*ln(x)**(n-1))
// </summary>

// <reviewed reviewer="" date="" tests="tPowerLogarithmicPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis> 
// A Power Logarithmic Polynomial<T> contains a set of coefficients; its fundamental operations
// is evaluating itself at some "x".
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

template<class T> class PowerLogarithmicPolynomial: public PowerLogarithmicPolynomialParam<T> {
public:
  
	// Constructs an empty PowerLogarithmicPolynomial
	PowerLogarithmicPolynomial() : PowerLogarithmicPolynomialParam<T>() {}

	// Makes a power logaritmic polynomial with the specified number of coefficients, all set to
	// zero.
	explicit PowerLogarithmicPolynomial(uInt n) : PowerLogarithmicPolynomialParam<T>(n) {}

	// Make a function with the specified params.
	PowerLogarithmicPolynomial(const vector<T>& parms) : PowerLogarithmicPolynomialParam<T>(parms) {}

	// Copy constructor/assignment (deep copy)
	// <group>
  PowerLogarithmicPolynomial(const PowerLogarithmicPolynomial<T> &other) : PowerLogarithmicPolynomialParam<T>(other) {}
  template <class W>
  PowerLogarithmicPolynomial(const PowerLogarithmicPolynomial<W> &other) : PowerLogarithmicPolynomialParam<T>(other) {}
  PowerLogarithmicPolynomial<T> &operator=(const PowerLogarithmicPolynomial<T> &other) {
	  PowerLogarithmicPolynomialParam<T>::operator=(other); return *this; }
  // </group>
  
  // Destructor
  virtual ~PowerLogarithmicPolynomial() {}
  
  //# Operators    
  // Evaluate the polynomial at <src>x</src>.
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new PowerLogarithmicPolynomial<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new PowerLogarithmicPolynomial<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new PowerLogarithmicPolynomial<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using PowerLogarithmicPolynomialParam<T>::param_p;
public:
  using PowerLogarithmicPolynomialParam<T>::nparameters;

};

#define PowerLogarithmicPolynomial_PS PowerLogarithmicPolynomial

// <summary> Partial specialization of PowerLogarithmicPolynomial for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>PowerLogarithmicPolynomial_PS</src> is only for cxx2html
// documentation problems. Use <src>PowerLogarithmicPolynomial</src> in your code.</note>
// </synopsis>

template <class T> class PowerLogarithmicPolynomial_PS<AutoDiff<T> > :
public PowerLogarithmicPolynomialParam<AutoDiff<T> > {
public:
  //# Constructors
  // Constructs one dimensional Polynomials.
  // <group>
	PowerLogarithmicPolynomial_PS() : PowerLogarithmicPolynomialParam<AutoDiff<T> >() {}
  explicit PowerLogarithmicPolynomial_PS(uInt n) :
		PowerLogarithmicPolynomialParam<AutoDiff<T> >(n) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  PowerLogarithmicPolynomial_PS(const PowerLogarithmicPolynomial_PS<AutoDiff<T> > &other) :
	  PowerLogarithmicPolynomialParam<AutoDiff<T> >(other) {}
  template <class W>
  PowerLogarithmicPolynomial_PS(const PowerLogarithmicPolynomial_PS<W> &other) :
  PowerLogarithmicPolynomialParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  PowerLogarithmicPolynomial_PS<AutoDiff<T> > &
    operator=(const PowerLogarithmicPolynomial_PS<AutoDiff<T> > &other) {
	  PowerLogarithmicPolynomialParam<AutoDiff<T> >::operator=(other); return *this; }
    
  // Destructor
  virtual ~PowerLogarithmicPolynomial_PS() {}

  //# Operators    
  // Evaluate the function and its derivatives at <src>x</src> <em>wrt</em>
  // to the coefficients.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new PowerLogarithmicPolynomial<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new PowerLogarithmicPolynomial<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new PowerLogarithmicPolynomial<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using PowerLogarithmicPolynomialParam<AutoDiff<T> >::param_p;
public:
  using PowerLogarithmicPolynomialParam<AutoDiff<T> >::nparameters;
};

#undef PowerLogarithmicPolynomial_PS


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Functionals/PowerLogarithmicPolynomial.tcc>
#include <scimath/Functionals/PowerLogarithmicPolynomial2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
