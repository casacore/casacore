//# SPolynomialParam.h: Parameter handling for scaled 1-D polynomials
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

#ifndef SCIMATH_SPOLYNOMIALPARAM_H
#define SCIMATH_SPOLYNOMIALPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template<class T> class Vector;

// <summary>  Parameter handling for scaled 1-D polynomials
// </summary>

// <reviewed reviewer="" date="" tests="tSPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>

// <etymology> 
// A 1-dimensional Scaled Polynomial's parameters.
// </etymology>
//
// <synopsis> 
// A <src>SPolynomial</src> is described by a set of coefficients;
// its fundamental operation is evaluating itself at some "x".
// The number of coefficients is the order of the polynomial plus one,
// plus three, describing a height, center and width. These three parameters
// are the first three, and have default values of 1, 0, 1. 
//
// Since the <src>SPolynomial</src> is a <src>Function</src>, the derivatives
// can be obtained as well. 
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// This class is in general used implicitly by the <src>SPolynomial</src>
// class only.
// </synopsis> 
//
// <example>
// <srcblock>
//  SPolynomial<Float> pf(3);  // Third order polynomial - coeffs 0 by default
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

template<class T> class SPolynomialParam: public Function<T>
{
public:
  //# Constructors
  // Constructs a zero'th order polynomial, with a coeficcient of 0.0.
  SPolynomialParam();
  
  // Makes a polynomial of the given order, with all coeficcients set to
  // zero. 
  explicit SPolynomialParam(uInt order);
  
  // Make this a copy of other (deep copy).
  // <group>
  SPolynomialParam(const SPolynomialParam<T> &other);
  template <class W>
    SPolynomialParam(const SPolynomialParam<W> &other) :
    Function<T>(other) {}
  SPolynomialParam<T> &operator=(const SPolynomialParam<T> &other);
  // </group>
  
  // Destructor
  ~SPolynomialParam();

  //# Operators  
  // Comparisons.  
  // SPolynomials are equal if they are of the same order
  // <group>
  Bool operator==(const SPolynomialParam<T> &other) const {
    return (param_p == other.param_p); }
  Bool operator!=(const SPolynomialParam<T> &other) const {
    return (param_p != other.param_p); }
  // </group>

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("spolynomial");
    return x; }

  // What is the order of the polynomial, i.e. maximum exponent of "x".
  uInt order() const { return param_p.nelements() - 4; }
  
  // What is the <em>which</em>'th coefficient of the polynomial. For an nth
  // degree polynomial, <em>which</em> varies between zero and n.
  T coefficient(uInt which) const {
    DebugAssert(which<=order(), AipsError); return param_p[which+3]; }
  
  // Return all the coefficients as a vector.
  Vector<T> coefficients() const;

  // Set the <em>which</em>'th coefficient to <em>value</em>. 
  void setCoefficient(uInt which, const T value) {
    DebugAssert(which<=order(), AipsError); param_p[which+3] = value; }
  
  // Set all the coefficients at once, throw away all existing coefficients.
  void setCoefficients(const Vector<T> &coefficients);

  // Returns the dimension of function
  virtual uInt ndim() const { return 1; }

  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
  using Function<T>::mask;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/SPolynomialParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
