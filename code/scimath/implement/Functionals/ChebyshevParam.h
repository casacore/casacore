//# NQChebyshevParam.h: Parameter handling for NQChebyshev polynomial
//# Copyright (C) 2000,2001
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

#if !defined(AIPS_NQCHEBYSHEVPARAM_H)
#define AIPS_NQCHEBYSHEVPARAM_H

#include <aips/aips.h>
#include <aips/Functionals/NQFunction1D.h>

//# Forward Declarations
template<class T> class Vector;

// <summary> Parameter handling for NQChebyshev polynomial
// </summary>

// <use visibility=local>

// <reviewed reviewer="wbrouw" date="2001/11/12" tests="tNQChebyshev" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class=NQFunction1D>NQFunction1D</linkto>
// </prerequisite>
//
// <etymology>
// This class is named after NQChebyshev Type I polynomials 
// </etymology>
//
// <synopsis>
// This class allows one to form and evaluate a function as a
// NQChebyshev series, a linear combination of so-called NQChebyshev
// polynomials. 
// 
// NQChebyshev polynomials are a special type of ultraspheric polynomials 
// that are useful in such contexts as numerical analysis and circuit
// design. They form an orthogobnal set.
// A (type I) NQChebyshev polynomial, T_n, is generated via the
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
// A common use of NQChebyshev polynomials is in approximating
// functions.  In particular, any function that is approximated by
// a power series,
// <srcblock>
// f(x) ~ SUM P_i x^i,
// </srcblock>
// over the interval [-1, 1] can be approximated by a linear
// combination of NQChebyshev polynomials:
// <srcblock>
// f(x) ~ SUM C_i T_i(x),
// </srcblock>
// where C_i is the set of so-called NQChebyshev coefficients.
// 
// Approximating a function with NQChebyshev polynomials has some
// important advantages.  For one, if the function is well approximated
// by a converging power series, one can obtain an equally accurate
// estimate using fewer terms of the corresponding NQChebyshev series.
// More important, though, is the property over the interval [-1, 1],
// each polynomial has a domain of [-1, 1]; thus, the series is nicely
// bounded.  And because of this bounded property, approximations
// calculated from a NQChebyshev series are less susceptible to machine
// rounding errors than the equivalent power series.  
//
// With a simple change of variable, it is possible to approximate a
// continuous function over any restricted interval using a
// NQChebyshev series.  This documention refers to this interval as the
// <em>NQChebyshev interval</em> (set with the
// <src>setInterval()</src> function).  The
// other important input parameters, of course, include the
// coefficients of the polynomials.  
//
// Like all Functions, the NQChebyshev series is evaluated via the 
// operator().  If the input value is within the range set by
// setInterval(), it is transformed to the range [-1, 1] via,
// <srcblock>
// y = x - (min + max)/2) / ((max - min)/2)
// </srcblock>
// The series is then evaluated with the coefficients set either at
// construction or via setCoefficients().  The value that is returned
// when the input value is outside the NQChebyshev interval depends on
// the out-of-interval mode (set via setOutOfIntervalMode()).  The
// default mode is to return a default value which can be set via
// setDefault().  The supported modes are identified by the
// enumeration OutOfIntervalMode; see the documentation of this
// enumeration below for detailed description of these modes.  In
// practice, though, it is expected that this class will be configured
// for the interval of interest.  
// 
// 
// The derivative of a Chebyshev series <em>wrt</em> the argument <src>x</src>,
// is easily calculated analytically and
// can be expressed as another NQChebyshev series; this is what the
// derivative() function returns.  However, the more general way to
// obtain derivatives is via the <linkto class="AutoDiff">AutoDiff</linkto> 
// templated type.  
//
// </synopsis>
//
// <example>
// In this example, a 2nd order NQChebyshev polynomial series is
// created.
// <srcblock>
//   // set coeffs to desired values
//   Vector<Double> coeffs(3, 1);   
//
//   // configure the function   
//   NQChebyshev<Double> cheb;
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
//   NQChebyshev<AutoDiff<Double> > cheb;
//   cheb.setDefault(AutoDiff<Double>(1));
//   cheb.setInterval(AutoDiff<Double>(-0.8), AutoDiff<Double>(7.2));
//
//   // we'll track derivatives with respect to x and each of our
//   // coefficients; for a second-order series, this makes 4
//   // derivatives total.  x will be the first variable; the
//   // coefficients will the 2nd-4th variables
//   cheb.setCoefficient(0, AutoDiff<Double>(3.1, 4, 1));   // c0 = 3.1
//   cheb.setCoefficient(1, AutoDiff<Double>(2.4, 4, 2));   // c1 = 2.4
//   cheb.setCoefficient(2, AutoDiff<Double>(0.5, 4, 3));   // c2 = 0.5
//   
//   // now evaluate the function
//   AutoDiff<Double> x(1.2, 4, 0);    // x = 1.2
//   AutoDiff<Double> y = cheb(x);     // y = ??
//   Double dydx = y.derivative(0);    // dy/dx = ??
//   Double dydc1 = y.derivative(2);   // dy/dc1 = ??
// </srcblock>
// </example>
//
// <motivation>
// This class was created to support systematic errors in the simulator tool.  
// It can be used by Jones matrix classes to vary gains in a predictable way,
// mimicing natural processes of the atmosphere or instrumental effects.  
// </motivation>
//
// <templating arg=T>
//  <li> T should have standard numerical operators. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>
//
// <thrown>
//    <li> Assertion if indices out-of-range
// </thrown>
//
// <todo asof="2001/08/22">
//    <li> It would be helpful to be able to convert to and from the 
//         Polynomial<T> type; this would be supported via a function,
//         Polynomial<T> polynomial(), and constructor, 
//         NQChebyshev(Polynomial<T>)
// </todo>

template<class T>
class  NQChebyshevParam : public NQFunction1D<T> {
 public: 
  //# Enumerators
  // modes that identify how this function behaves outside its NQChebyshev
  // interval (see setInterval()).
  enum OutOfIntervalMode {
    // return a constant, default value.  The value returned is 
    // set with setDefault().
    CONSTANT,
    // return a constant value equal to the zero-th order coefficient
    ZEROTH,
    // evaluate the polynomial based on its coefficients just as it
    // would be inside the interval.  Thus, the function's range is not
    // guaranteed to remain within the characteristic bounds of the
    // NQChebyshev interval.  
    EXTRAPOLATE,
    // evaluate the function as if the range is cyclic, repeating the
    // range values from its canonical domain.  The period of the cycle
    // will be equal to getIntervalMax()-getIntervalMin().  When the 
    // function is evaluated outside this interval, the input value will 
    // shifted an integer number of periods until it falls within the 
    // NQChebyshev interval; the value returned is the polynomial evaluated 
    // at the shifted (x-axis) value.  Obviously, this mode is most 
    // expensive computationally when evaluating outside the range.
    CYCLIC,
    // evaluate the function at nearest interval edge
    EDGE,
    // number of enumerators
    NOutOfIntervalModes };

  //# Constructors
  // create a zero-th order NQChebyshev polynomial with the first coefficient
  // equal to zero.  The bounded domain is [T(-1), T(1)].  The 
  // OutOfDomainMode is CONSTANT, and the default value is T(0).
  NQChebyshevParam();

  // create an n-th order NQChebyshev polynomial with the coefficients
  // equal to zero.  The bounded domain is [T(-1), T(1)].  The 
  // OutOfDomainMode is CONSTANT, and the default value is T(0).
  explicit NQChebyshevParam(const uInt n);

  // create a zero-th order NQChebyshev polynomical with the first coefficient
  // equal to one.  
  //   min is the minimum value of its NQChebyshev interval, and 
  //   max is the maximum value.  
  //   mode sets the behavior of the function outside the NQChebyshev interval
  //      (see setOutOfIntervalMode() and OutOfIntervalMode enumeration 
  //      definition for details).  
  //   defval is the value returned when the function is evaluated outside
  //      the NQChebyshev interval and mode=CONSTANT.
  NQChebyshevParam(const T &min, const T &max,
		   const OutOfIntervalMode mode=CONSTANT,
		   const T &defval=T(0));
  
  // create a fully specified NQChebyshev polynomial.  
  //   coeffs holds the coefficients of the NQChebyshev polynomial (see 
  //      setCoefficients() for details).
  //   min is the minimum value of its canonical range, and 
  //   max is the maximum value.  
  //   mode sets the behavior of the function outside the NQChebyshev interval
  //      (see setOutOfIntervalMode() and OutOfIntervalMode enumeration 
  //      definition for details).  
  //   defval is the value returned when the function is evaluated outside
  //      the canonical range and mode=CONSTANT.
  NQChebyshevParam(const Vector<T> &coeffs, const T &min, const T &max, 
		   const OutOfIntervalMode mode=CONSTANT, const T &defval=T(0));
  
  // create a deep copy of another NQChebyshev polynomial
  NQChebyshevParam(const  NQChebyshevParam &other);
  
  // make a (deep) copy of another NQChebyshev polynomial
  NQChebyshevParam<T> &operator=(const NQChebyshevParam<T> &other);
  
  // Destructor
  virtual ~NQChebyshevParam();
  
  // set the NQChebyshev coefficients.  
  //   coeffs holds the coefficients in order, beginning with the zero-th 
  //      order term.  The order of the polynomial, then, would be the size 
  //      of the Vector minus one.  
  void setCoefficients(const Vector<T> &coeffs);

  // set a particular NQChebyshev coefficient.
  //   which is the coefficient order (i.e. 0 refers to the constant offset).
  //   value is the coefficient value.
  // If which is larger than current order of the function, the order will
  // be increased to the value of which, and that coefficient is set to 
  // value; missing coefficients less than this value will be set to zero.
  // Thus, the order can be increased with this function; however, it cannot
  // be decreased (even if the highest order coefficient is set to zero).
  // To lower the order, use setCoefficients() with a Vector having the 
  // desired number of coefficients.
  void setCoefficient(const uInt which, const T &value);
  
  // return the current set of coefficients into a given Vector.  
  const Vector<T> &getCoefficients() const;
  
  // return a particular coefficient.  
  //   which is the coefficient order (i.e. 0 refers to the constant offset).
  //     If which is out of range, zero is returned.
  T getCoefficient(const uInt which) const {
    return ((which < nparameters()) ? param_p[which] : T(0)); };
  
  // return the number of coeefficients currently loaded.  This does not
  // guarantee that the coefficients are non-zero
  uInt nCoefficients() const { return nparameters(); }
    
  // set the Chebyshev interval for this function.  The function will 
  // be scaled and shifted to such that the central bounded range of the 
  // NQChebyshev polynomials ([-1, 1] in untransformed space) spans the 
  // given range.  
  //   min is the minimum value for the interval, and 
  //   max is the maximum value.  See setOutOfIntervalMode() for the behavior 
  //      of this function outside the set range.
  void setInterval(T xmin, T xmax) {
    if (xmin < xmax) { minx_p = xmin; maxx_p = xmax;
    } else { minx_p = xmax; maxx_p = xmin; }; };   

  // return the minimum value for the currently Chebyshev interval.
  // See setInterval() for additional details.
  T getIntervalMin() const { return minx_p; }

  // return the maximum value for the currently Chebyshev interval.
  // See setInterval() for additional details.
  T getIntervalMax() const { return maxx_p; }

  // set the behavior of this function when it is evaluated outside its
  // NQChebyshev interval
  void setOutOfIntervalMode(OutOfIntervalMode mode) { mode_p = mode; }

  // return the behavior of this function when it is evaluated outside of 
  // its NQChebyshev interval.
  OutOfIntervalMode getOutOfIntervalMode() const { return mode_p; }

  // set the default value of this function.  This value is used when 
  // the getOutOfIntervalMode() returns NQChebyshev::CONSTANT; it is returned
  // when the a value outside of the NQChebyshev interval is passed to 
  // the () operator.
  void setDefault(const T &val) { def_p = val; }

  // return the currently set default value.  See setDefault() for details 
  // on the use of this value.
  const T &getDefault() const { return def_p; }

  // return the order of this polynomial.  This returns the value of 
  // nCoefficients()-1;
  uInt order() const { return param_p.nelements() - 1; };

  // transform a set of NQChebyshev polynomial coefficients into a set 
  // representing the series' derivative.  coeffs should be assuming
  // an interval of [-1, 1].  xmin and xmax can be provided to transform
  // the series to another interval.
  static void derivativeCoeffs(Vector<T> &coeffs, const T &xmin=T(-1),
			       const T &xmax=T(1));
  
  // convert a set of NQChebyshev polynomial coefficients to power series
  // coefficients.  The values passed in coeffs are taken to 
  // be chebyshev coefficients; these values will be replaced with the 
  // power series coefficients.  They should be ordered beginning
  // with the zero-th order coefficient.  
  static void chebyshevToPower(Vector<T> &coeffs);

  // convert a set of power series coefficients to NQChebyshev
  // polynomial coefficients.  The values passed in coeffs are taken to 
  // be power series coefficients; these values will be replaced with the 
  // NQChebyshev polynomial coefficients.  They should be ordered beginning
  // with the zero-th order coefficient.  
  static void powerToNQChebyshev(Vector<T> &coeffs);

 protected:
  // Default value if outside interval
  T def_p;
  // Lowest interval bound
  T minx_p;
  // Highest inetrval bound
  T maxx_p;
  // Out-of-interval handling type
  OutOfIntervalMode mode_p;

};

#endif


