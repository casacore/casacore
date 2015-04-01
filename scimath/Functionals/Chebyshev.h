//# Chebyshev.h  A function class that defines a Chebyshev polynomial
//# Copyright (C) 2000,2001,2002,2003,2005
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
//#! ========================================================================
//# $Id$

#ifndef SCIMATH_CHEBYSHEV_H
#define SCIMATH_CHEBYSHEV_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/ChebyshevParam.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> A function class that defines a Chebyshev polynomial
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="2001/11/12" tests="tChebyshev" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <etymology>
// This class is named after Chebyshev Type I polynomials 
// </etymology>
//
// <synopsis>
// This class allows one to form and evaluate a function as a
// Chebyshev series, a linear combination of so-called Chebyshev
// polynomials. 
//
// This class's implementation is split into two parts: 
// the parent class <linkto class="ChebyshevParam">ChebyshevParam&lt;T&gt;</linkto>,
// which manages the function's parameters, and this class, which handles
// how the function is evaluated.  Thus, be sure to also consult 
// <linkto class="ChebyshevParam">ChebyshevParam&lt;T&gt;</linkto> for 
// the full interface of this function.  
// 
// <ANCHOR NAME="Chebyshev:about">
// <H3>About Chebyshev Polynomials</H3></ANCHOR>
// 
// Chebyshev polynomials are a special type of ultraspheric polynomials 
// that are useful in such contexts as numerical analysis and circuit
// design. They form an orthogobnal set.
// A (type I) Chebyshev polynomial, T_n, is generated via the
// equation:
// <srcblock>
// T_n(x) = cos n(arccos x)
// </srcblock>
// Through clever use of trigometric identities, one can express T_n
// as a real polynomial expression of the form
// <srcblock>
//           n
// T_n(x) = SUM C_i t^i
//          i=0
// </srcblock>
// The low order polynomials look like this:
// <srcblock>
// T_0 = 1
// T_1 = x
// T_2 = 2x^2 - 1
// T_3 = 4x^3 - 3x
// T_4 = 8x^4 - 8x^2 + 1
// T_5 = 16x^5 - 20x^3 + 5x 
// </srcblock>
// Higher order polynomials satisfy the recurrance relation,
// <srcblock>
// T_(n+1) = 2xT_(n) - T_(n-1).
// </srcblock>
// 
// A common use of Chebyshev polynomials is in approximating
// functions.  In particular, any function that is approximated by
// a power series,
// <srcblock>
// f(x) ~ SUM P_i x^i,
// </srcblock>
// over the interval [-1, 1] can be approximated by a linear
// combination of Chebyshev polynomials:
// <srcblock>
// f(x) ~ SUM C_i T_i(x),
// </srcblock>
// where C_i is the set of so-called Chebyshev coefficients.
// 
// Approximating a function with Chebyshev polynomials has some
// important advantages.  For one, if the function is well approximated
// by a converging power series, one can obtain an equally accurate
// estimate using fewer terms of the corresponding Chebyshev series.
// More important, though, is the property over the interval [-1, 1],
// each polynomial has a domain of [-1, 1]; thus, the series is nicely
// bounded.  And because of this bounded property, approximations
// calculated from a Chebyshev series are less susceptible to machine
// rounding errors than the equivalent power series.  
//
// <ANCHOR NAME="Chebyshev:using">
// <H3>Using the Chebyshev Function class</H3></ANCHOR>
// 
// With a simple change of variable, it is possible to approximate a
// continuous function over any restricted interval using a
// Chebyshev series.  This documention refers to this interval as the
// <em>Chebyshev interval</em> (set with the
// <linkto class="ChebyshevParam">setInterval()</linkto> function).  The
// other important input parameters, of course, include the
// coefficients of the polynomials.  
//
// Like all Functions, the Chebyshev series is evaluated via the 
// function operator, <src>operator()</src>.  If the input value is 
// within the range set by
// <linkto class="ChebyshevParam">setInterval()</linkto>, it is 
// transformed to the range [-1, 1] via,
// <srcblock>
// y = x - (min + max)/2) / ((max - min)/2)
// </srcblock>
// The series is then evaluated with the coefficients set either at
// construction or via setCoefficients().  The value that is returned
// when the input value is outside the Chebyshev interval depends on
// the out-of-interval mode (set via 
// <linkto class="ChebyshevParam">setOutOfIntervalMode()</linkto>).  The
// default mode is to return a default value which can be set via
// <linkto class="ChebyshevParam">setDefault()</linkto>.  The supported 
// modes are identified by the
// enumeration OutOfIntervalMode; see the 
// <linkto class="ChebyshevParam">documentation for ChebyshevParam</linkto>
// for a detailed description of these modes.  In practice, though, it is 
// expected that this class will be configured for the interval of interest.  
// 
// The derivative of a Chebyshev series with respect to the independent 
// variable (i.e. the argument <src>x</src>) is easily calculated analytically 
// and can be expressed as another Chebyshev series; this is what the
// <src>derivative()</src> function returns.  However, the more general way to
// obtain derivatives is via the <linkto class="AutoDiff">AutoDiff</linkto> 
// templated type.  
//
// </synopsis>
//
// <example>
// In this example, a 2nd order Chebyshev polynomial series is
// created.
// <srcblock>
//   // set coeffs to desired values
//   Vector<Double> coeffs(3, 1);   
//
//   // configure the function   
//   Chebyshev<Double> cheb;
//   cheb.setInterval(-0.8, 7.2);
//   cheb.setDefault(1.0);
//   cheb.setCoefficients(coeffs);
//
//   // evaluate the function as necessary
//   Double z = cheb(-0.5);    // -0.5 is within range, z = 0.78625
//   z = cheb(4.2);            // 4.2 is within range, z = 0.375
//   z = cheb(-3);             // -3 is out of the interval, z = 1
// </srcblock>
//
// The next example illustrates how to use the 
// <linkto class="AutoDiff">AutoDiff</linkto> class to simultaneously
// calculate derivatives.  Here, we replace the Double type with 
// AutoDiff<Double>.
// <srcblock>
//   Chebyshev<AutoDiffA<Double> > cheb;
//   cheb.setDefault(AutoDiffA<Double>(1));
//   cheb.setInterval(AutoDiffA<Double>(-0.8), AutoDiffA<Double>(7.2));
//
//   // we'll track derivatives with respect to x and each of our
//   // coefficients; for a second-order series, this makes 4
//   // derivatives total.  x will be the first variable; the
//   // coefficients will the 2nd-4th variables
//   cheb.setCoefficient(0, AutoDiffA<Double>(3.1, 4, 1));   // c0 = 3.1
//   cheb.setCoefficient(1, AutoDiffA<Double>(2.4, 4, 2));   // c1 = 2.4
//   cheb.setCoefficient(2, AutoDiffA<Double>(0.5, 4, 3));   // c2 = 0.5
//   
//   // now evaluate the function
//   AutoDiffA<Double> x(1.2, 4, 0);    // x = 1.2
//   AutoDiffA<Double> y = cheb(x);     // y = 1.65
//   Double dydx = y.derivative(0);     // dy/dx = 0.35
//   Double dydc1 = y.derivative(2);    // dy/dc1 = -0.5
// </srcblock>
// </example>
//
// <motivation>
// This class was created to support systematic errors in the simulator tool.  
// It can be used by Jones matrix classes to vary gains in a predictable way,
// mimicing natural processes of the atmosphere or instrumental effects.  
// </motivation>

// <templating arg=T>
//  <li> T should have standard numerical operators. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to address incorrect
//		coefficients
// </thrown>
//
// <todo asof="2001/08/22">
//    <li> It would be helpful to be able to convert to and from the 
//         Polynomial<T> type; this would be supported via a function,
//         Polynomial<T> polynomial(), and constructor, 
//         Chebyshev(Polynomial<T>)
// </todo>

template<class T>
class Chebyshev : public ChebyshevParamModeImpl<T> {
public: 

    //# Constructors
    // create a zero-th order Chebyshev polynomial with the first coefficient
    // equal to zero.  The bounded domain is [T(-1), T(1)].  The 
    // OutOfDomainMode is CONSTANT, and the default value is T(0).
    Chebyshev() : ChebyshevParamModeImpl<T>() {}

    // create an n-th order Chebyshev polynomial with the coefficients
    // equal to zero.  The bounded domain is [T(-1), T(1)].  The 
    // OutOfDomainMode is CONSTANT, and the default value is T(0).
    explicit Chebyshev(const uInt n) : ChebyshevParamModeImpl<T>(n) {}

    // create a zero-th order Chebyshev polynomical with the first coefficient
    // equal to one.  
    //   min is the minimum value of its Chebyshev interval, and 
    //   max is the maximum value.  
    //   mode sets the behavior of the function outside the Chebyshev interval
    //      (see setOutOfIntervalMode() and OutOfIntervalMode enumeration 
    //      definition for details).  
    //   defval is the value returned when the function is evaluated outside
    //      the Chebyshev interval and mode=CONSTANT.
    Chebyshev(const T &min, const T &max,
	      const typename ChebyshevEnums::
	      OutOfIntervalMode mode=ChebyshevEnums::CONSTANT,
	      const T &defval=T(0)) :
	ChebyshevParamModeImpl<T>(min, max, mode, defval) {}
  
    // create a fully specified Chebyshev polynomial.  
    //   coeffs holds the coefficients of the Chebyshev polynomial (see 
    //      setCoefficients() for details).
    //   min is the minimum value of its canonical range, and 
    //   max is the maximum value.  
    //   mode sets the behavior of the function outside the Chebyshev interval
    //      (see setOutOfIntervalMode() and OutOfIntervalMode enumeration 
    //      definition for details).  
    //   defval is the value returned when the function is evaluated outside
    //      the canonical range and mode=CONSTANT.
    Chebyshev(const Vector<T> &coeffs, const T &min, const T &max, 
	      const typename ChebyshevEnums::
	      OutOfIntervalMode mode=ChebyshevEnums::CONSTANT,
	      const T &defval=T(0)) :
	ChebyshevParamModeImpl<T>(coeffs, min, max, mode, defval) {}

    // create a fully specified Chebyshev polynomial.
    //   config  is a record that contains the non-coefficient data 
    //             that configures this class.
    // The fields recognized by this class are those documented for the 
    // <linkto class="ChebyshevParam">ChebyshevPara::setMode()</linkto> 
    // function.
    // <group>
    Chebyshev(uInt order, const RecordInterface& mode) :
	ChebyshevParamModeImpl<T>(order, mode) { }
    Chebyshev(const Vector<T> &coeffs, const RecordInterface& mode) :
	ChebyshevParamModeImpl<T>(coeffs, mode) { }
    // </group>
  
    // create a deep copy of another Chebyshev polynomial
    // <group>
    Chebyshev(const Chebyshev &other) : ChebyshevParamModeImpl<T>(other) {}
    // </group>

    // make this instance a (deep) copy of another Chebyshev polynomial
    Chebyshev<T> &operator=(const Chebyshev<T> &other) {
	ChebyshevParam<T>::operator=(other); return *this; }
  
    // Destructor
    virtual ~Chebyshev() {}
  
    //# Operators    
    // Evaluate the Chebyshev at <src>x</src>.
    virtual T eval(const typename FunctionTraits<T>::ArgType *x) const;
  
    //# Member functions
    // Return the Chebyshev polynomial which is the derivative of this one
    // (with respect to the argument <src>x</src>).
    Chebyshev<T> derivative() const;
  
    // Create a new copy of this object.  The caller is responsible 
    // for deleting the pointer. 
    // <group>
    virtual Function<T> *clone() const { return new Chebyshev<T>(*this); }
  // </group>

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Chebyshev.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
