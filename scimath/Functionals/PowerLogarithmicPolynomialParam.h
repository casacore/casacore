//# PolynomialParam.h: Parameter handling for one-dimensional polynomials
//# Copyright (C) 2001,2002,2005
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
//# $Id: PolynomialParam.h 21024 2011-03-01 11:46:18Z gervandiepen $

#ifndef SCIMATH_POWERLOGARITHMICPOLYNOMIALPARAM_H
#define SCIMATH_POWERLOGARITHMICPOLYNOMIALPARAM_H

//# Includes
#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Assert.h>
#include <scimath/Functionals/Function1D.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward declarations
template<class T> class Vector;

// <summary>  Parameter handling for one-dimensional power logarithmic polynomials
// </summary>

// <reviewed reviewer="" date="" tests="tPowerLogarithmicPolynomial"
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class=Function1D>Function1D</linkto>
// </prerequisite>

// <etymology> 
// A 1-dimensional power logaritmic olynomial's parameters.
// </etymology>
//
// <synopsis> 
// A <src>power logarithmic polynomial</src> is described by a set of coefficients;
// its fundamental operation is evaluating itself at some "x".
//
// Since the <src> power logarithmic olynomial</src> is a <src>Function</src>, the derivatives
// can be obtained as well. 
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// This class is in general used implicitly by the <src>PowerLogarithmicPolynomial</src>
// class only.
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


template<class T> class PowerLogarithmicPolynomialParam: public Function1D<T> {
public:
  //# Constructors
  // Constructs a function with two coefficients, both 1 (so y = x).
  PowerLogarithmicPolynomialParam();
  
  // Makes a polynomial of the specified number of coefficients, all set to zero.
  explicit PowerLogarithmicPolynomialParam(uInt n);
  
  PowerLogarithmicPolynomialParam(const vector<T>& parms);

  // Make this a copy of other (deep copy).
  // <group>
  PowerLogarithmicPolynomialParam(const PowerLogarithmicPolynomialParam<T> &other);
  template <class W>
    PowerLogarithmicPolynomialParam(const PowerLogarithmicPolynomialParam<W> &other) :
    Function1D<T>(other) {}
  PowerLogarithmicPolynomialParam<T> &operator=(const PowerLogarithmicPolynomialParam<T> &other);
  // </group>
  
  // Destructor
  virtual ~PowerLogarithmicPolynomialParam();

  //# Operators  
  // Comparisons.  
  // <group>
  Bool operator==(const PowerLogarithmicPolynomialParam<T> &other) const {
    return (param_p == other.param_p); }
  Bool operator!=(const PowerLogarithmicPolynomialParam<T> &other) const {
    return (param_p != other.param_p); }
  // </group>

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("power logarithmic polynomial");
    return x; }

  // What is the <em>which</em>'th coefficient of the polynomial. For an nth
  // degree polynomial, <em>which</em> varies between zero and n.
  T coefficient(uInt which) const {
    DebugAssert(which<=nparameters, AipsError); return param_p[which]; }
  
  // Return all the coefficients as a vector.
  const Vector<T> &coefficients() const;

  // Set the <em>which</em>'th coefficient to <em>value</em>. 
  void setCoefficient(uInt which, const T value) {
    DebugAssert(which<=nparameters, AipsError); param_p[which] = value; }
  
  // Set all the coefficients at once, throw away all existing coefficients.
  void setCoefficients(const Vector<T> &coefficients);

  //# Make members of parent classes known.
protected:
  using Function1D<T>::param_p;
public:
  using Function1D<T>::nparameters;
};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Functionals/PowerLogarithmicPolynomialParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
