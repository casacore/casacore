//# SPolynomial.h: A one dimensional scaled polynomial class
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

#ifndef SCIMATH_SPOLYNOMIAL_H
#define SCIMATH_SPOLYNOMIAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/SPolynomialParam.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional scaled polynomial class
// </summary>

// <reviewed reviewer="" date="" tests="tSPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis> 
// A SPolynomial<T> contains a set of coefficients; its fundamental operations
// is evaluating itself at some "x". The number of coefficients is the order
// of the polynomial plus one, plus an additional 3 as height, center, width.
//
// </synopsis> 
//
// <example>
// <srcblock>
//  SPolynomial<Float> pf(3); // Third order polynomial - coeffs 0 by default
//  pf.setCoefficient(1, 1.0);
//  pf[5] = 2.0;
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

// <todo asof="2002/05/20">
//   <li> Nothing I know of
// </todo>

template<class T> class SPolynomial: public SPolynomialParam<T>
{
public:
  //# Enumerations
  
  //# Constructors
  // Constructs a zero'th order polynomial, with a coeficcient of 0.0.
  SPolynomial() : SPolynomialParam<T>() {}
  // Makes a polynomial of the given order, with all coeficcients set to
  // zero, and height, center, width to 1,0,1.
  explicit SPolynomial(uInt order) : SPolynomialParam<T>(order) {}
  // Copy constructor/assignment (deep copy)
  // <group>
  SPolynomial(const SPolynomial<T> &other) : SPolynomialParam<T>(other) {}
  template <class W>
    SPolynomial(const SPolynomial<W> &other) : SPolynomialParam<T>(other) {}
  SPolynomial<T> &operator=(const SPolynomial<T> &other) {
    SPolynomialParam<T>::operator=(other); return *this; }
  // </group>
  
  // Destructor
  virtual ~SPolynomial() {}
  
  //# Operators    
  // Evaluate the polynomial at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  //# Member functions
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new SPolynomial<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new SPolynomial<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new SPolynomial<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using SPolynomialParam<T>::param_p;
public:
  using SPolynomialParam<T>::nparameters;
  using SPolynomialParam<T>::mask;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/SPolynomial.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
