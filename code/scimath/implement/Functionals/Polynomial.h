//# NQPolynomial.h: A one dimensional polynomial class
//# Copyright (C) 1994,1995,1996,2001
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

#if !defined(AIPS_NQPOLYNOMIAL_H)
#define AIPS_NQPOLYNOMIAL_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/NQPolynomialParam.h>
#include <aips/Functionals/NQFunction1D.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffMath.h>

//# Forward declarations

// <summary> A one dimensional polynomial class
// </summary>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tNQPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis> 
// A NQPolynomial<T> contains a set of coefficients; its fundamental operations
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
//  NQPolynomial<Float> pf(3); // Third order polynomial - coeffs 0 by default
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

template<class T> class NQPolynomial: public NQPolynomialParam<T> {
 public:
  //# Enumerations
  
  //# Constructors
  // Constructs a zero'th order polynomial, with a coeficcient of 0.0.
  NQPolynomial() : NQPolynomialParam<T>() {};
  // Makes a polynomial of the given order, with all coeficcients set to
  // zero. 
  explicit NQPolynomial(uInt order) : NQPolynomialParam<T>(order) {};
  // Copy constructor/assignment (deep copy)
  // <group>
  NQPolynomial(const NQPolynomial<T> &other) : NQPolynomialParam<T>(other) {};
  NQPolynomial<T> &operator=(const NQPolynomial<T> &other) {
    NQPolynomialParam<T>::operator=(other); return *this; };
  // </group>
  
  // Destructor
  virtual ~NQPolynomial() {};
  
  //# Operators    
  // Evaluate the polynomial at <src>x</src>.
  virtual T eval(typename NQFunction1D<T>::FunctionArg x) const;
  
  //# Member functions
  // Return the polynomial which is the derivative of this one. <em>e.g.,</em>
  // <src> 2+4x+5x^2 --> 0+4+10x </src>.
  NQPolynomial<T> derivative() const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<T> *clone() const { return new NQPolynomial<T>(*this); };

private:

};

#define NQPolynomial_PS NQPolynomial

// <summary> Partial specialization of NQPolynomial for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>NQPolynomial_PS</src> is only for cxx2html
// documentation problems. Use <src>NQPolynomial</src> in your code.</note>
// </synopsis>

template <class T> class NQPolynomial_PS<AutoDiff<T> > : 
public NQPolynomialParam<AutoDiff<T> > {
 public:
  //# Constructors
  // Constructs one dimensional Polynomials.
  // <group>
  NQPolynomial_PS() : NQPolynomialParam<AutoDiff<T> >() {};
  explicit NQPolynomial_PS(uInt order) :
    NQPolynomialParam<AutoDiff<T> >(order) {};
  // </group>

  // Copy constructor (deep copy)
  NQPolynomial_PS(const NQPolynomial_PS<AutoDiff<T> > &other) :
    NQPolynomialParam<AutoDiff<T> >(other) {};

  // Copy assignment (deep copy)
  NQPolynomial_PS<AutoDiff<T> > &
    operator=(const NQPolynomial_PS<AutoDiff<T> > &other) {
    NQPolynomialParam<AutoDiff<T> >::operator=(other); return *this; };
    
  // Destructor
  virtual ~NQPolynomial_PS() {};

  //# Operators    
  // Evaluate the polynomial and its derivatives at <src>x</src> <em>wrt</em>
  // to the coefficients.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  virtual Function<AutoDiff<T> > *clone() const {
    return new NQPolynomial<AutoDiff<T> >(*this); };

};

#undef NQPolynomial_PS

#endif
