//# LinearFit.h: Class for linear least-squares fit.
//#
//# Copyright (C) 1995,1999,2000,2001,2002,2004
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

#ifndef SCIMATH_LINEARFIT_H
#define SCIMATH_LINEARFIT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Fitting/GenericL2Fit.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> Class for linear least-squares fit.
// </summary>
//
// <reviewed reviewer="wbrouw" date="2004/06/15" tests="tLinearFitSVD.cc"
//	 demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Functional">Functional</linkto> 
//   <li> <linkto class="Function">Function</linkto> 
//   <li> <linkto module="Fitting">Fitting</linkto>
// </prerequisite>
//
// <etymology>
// A set of data point is fit with some functional equation.
// The equations solved are linear equations.  The functions 
// themselves however can be wildly nonlinear.
// </etymology>
//
// <synopsis>
// NOTE: Constraints added. Documentation out of date at moment, check
// the tLinearFitSVD and tNonLinearFirLM programs for examples.
//
// The following is a brief summary of the linear least-squares fit problem.
// See module header, <linkto module="Fitting">Fitting</linkto>,
// for a more complete description.  
//
// Given a set of N data points (measurements), (x(i), y(i)) i = 0,...,N-1, 
// along with a set of standard deviations, sigma(i), for the data points, 
// and M specified functions, f(j)(x) j = 0,...,M-1, we form a linear 
// combination of the functions: 
// <srcblock>
// z(i) = a(0)f(0)(x(i)) + a(1)f(1)(x(i)) + ... + a(M-1)f(M-1)(x(i)),
// </srcblock>
// where a(j) j = 0,...,M-1 are a set of parameters to be determined.
// The linear least-squares fit tries to minimize
// <srcblock>
// chi-square = [(y(0)-z(0))/sigma(0)]^2 + [(y(1)-z(1))/sigma(1)]^2 + ... 
//              + [(y(N-1)-z(N-1))/sigma(N-1)]^2.
// </srcblock>
// by adjusting {a(j)} in the equation. 
//
// For complex numbers, <code>[(y(i)-z(i))/sigma(i)]^2</code> in chi-square 
// is replaced by
// <code>[(y(i)-z(i))/sigma(i)]*conjugate([(y(i)-z(i))/sigma(i)])</code>
//
// For multidimensional functions, x(i) is a vector, and
// <srcblock> 
// f(j)(x(i)) = f(j)(x(i,0), x(i,1), x(i,2), ...)
// </srcblock>
//
// Normally, it is necessary that N > M for the solutions to be valid, since 
// there must be more data points than model parameters to be solved.
//
// If the measurement errors (standard deviation sigma) are not known 
// at all, they can all be set to one initially.  In this case, we assume all 
// measurements have the same standard deviation, after minimizing
// chi-square, we recompute
// <srcblock>  
// sigma^2 = {(y(0)-z(0))^2 + (y(1)-z(1))^2 + ... 
//           + (y(N-1)-z(N-1))^2}/(N-M) = chi-square/(N-M).
// </srcblock> 
//
// A statistic weight can also be assigned to each measurement if the 
// standard deviation is not available.  sigma can be calculated from
// <srcblock>
// sigma = 1/ sqrt(weight)
// </srcblock>
// Alternatively a 'weight' switch can be set with <src>asWeight()</src>.
// For best arithmetic performance, weight should be normalized to a maximum
// value of one. Having a large weight value can sometimes lead to overflow
// problems.
//
// The function to be fitted to the data can be given as an instance of the
// <linkto class="Function">Function</linkto> class.
// One can also form a sum of functions using the
// <linkto class="CompoundFunction">CompoundFunction</linkto>.  
//
// For small datasets the usage of the calls is:
// <ul>
//  <li> Create a functional description of the parameters
//  <li> Create a fitter: LinearFit<T> fitter();
//  <li> Set the functional representation: fitter.setFunction()
//  <li> Do the fit to the data: fitter.fit(x, data, sigma)
//  	(or do a number of calls to buildNormalMatrix(x, data, sigma)
//	and finish of with fitter.fit() or fitter.sol())
//  <li> if needed the covariance; residuals; chiSquared, parameter errors
//	 can all be obtained
// </ul>
// Note that the fitter is reusable. An example is given in the following.
//
// The solution of a fit always produces the total number of parameters given 
// to the fitter. I.e. including any parameters that were fixed. In the
// latter case the solution returned will be the fixed value.
// 
// <templating arg=T>
// <li> Float
// <li> Double
// <li> Complex
// <li> DComplex   
// </templating>
//
// If there are a large number of unknowns or a large number of data points
// machine memory limits (or timing reasons) may not allow a complete
// in-core fitting to be performed.  In this case one can incrementally
// build the normal equation (see buildNormalMatrix()).
//
// The normal operation of the class tests for real inversion problems
// only. If tests are needed for almost collinear columns in the
// solution matrix, the collinearity can be set as the square of the sine of
// the minimum angle allowed.
//
// Singular Value Decomposition is supported by the
// <linkto class=LinearFitSVD>LinearFitSVD</linkto> class,
// which has a behaviour completely identical to this class (apart from a
// default collinearity of 1e-8). 
//
// Other information (see a.o. <linkto class=LSQFit>LSQFit</linkto>) can
// be set and obtained as well.
// </synopsis>
//
// <motivation>
// The creation of this class was driven by the need to write code
// to perform baseline fitting or continuum subtraction.
// </motivation>

// <example>
//# /// redo example
// In the following a polynomial is fitted through the first 20 prime numbers.
// The data is given in the x vector (1 to 20) and in the primesTable
// (2, 3, ..., 71) (see tLinearFitSVD test program). In the following
// all four methods to calculate a polynomial through the data is used
// <srcblock>
//    	// The list of coordinate x-values
//    	Vector<Double> x(nPrimes);
//    	indgen((Array<Double>&)x, 1.0);  // 1, 2, ...
//    	Vector<Double> primesTable(nPrimes);
//    	for (uInt i=1; i < nPrimes; i++) {
//        primesTable(i) =
//	   Primes::nextLargerPrimeThan(Int(primesTable(i-1)+0.01));
//      };   
//	Vector<Double> sigma(nPrimes);
//	sigma = 1.0;
//	// The fitter
//  	LinearFit<Double> fitter;
//	Polynomial<AutoDiff<Double> > combination(2);
//	// Get the solution
//	fitter.setFunction(combination);
//    	Vector<Double> solution = fitter.fit(x, primesTable, sigma);
//	// create a special function (should probably at beginning)
//	static void myfnc(Vector<Double> &y, const Double x) {
//  	y(0) = 1; for (uInt i=1; i<y.nelements(); i++) y(i) = y(i-1)*x; };
//    	fitter.setFunction(3, &myfnc);
//    	solution = fitter.fit(x, primesTable, sigma);
//	// Create the direct coefficients table
//    	fitter.setFunction(3);
//    	Matrix<Double> xx(nPrimes, 3);
//    	for (uInt i=0; i<nPrimes; i++) {
//        xx(i,0) = 1;
//        for (uInt j=1; j<3; j++) xx(i,j) = xx(i,j-1)*Double(i+1);
//      };
//      solution = fitter.fit(xx, primesTable, sigma);
// </srcblock>
// In the test program examples are given on how to get the other
// information, and other examples.
// </example>

template<class T> class LinearFit : public GenericL2Fit<T>
{
public: 
  //# Constructors
  // Create a fitter: the normal way to generate a fitter object. Necessary
  // data will be deduced from the Functional provided with
  // <src>setFunction()</src>
  LinearFit();
  // Copy constructor (deep copy)
  LinearFit(const LinearFit &other);
  // Assignment (deep copy)
  LinearFit &operator=(const LinearFit &other);

  // Destructor
  virtual ~LinearFit();
  
  //# Member functions

protected:
  //#Data

  //# Member functions
  // Generalised fitter
  virtual Bool fitIt
    (Vector<typename FunctionTraits<T>::BaseType> &sol,
     const Array<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
     const Vector<Bool> *const mask=0);

private:
  //# Data

  //# Member functions

protected:
  //# Make members of parent classes known.
  using GenericL2Fit<T>::pCount_p;
  using GenericL2Fit<T>::ptr_derive_p;
  using GenericL2Fit<T>::sol_p;
  using GenericL2Fit<T>::solved_p;
  using GenericL2Fit<T>::nr_p;
  using GenericL2Fit<T>::svd_p;
  using GenericL2Fit<T>::condEq_p;
  using GenericL2Fit<T>::err_p;
  using GenericL2Fit<T>::errors_p;
  using GenericL2Fit<T>::COLLINEARITY;
  using GenericL2Fit<T>::buildConstraint;
  using GenericL2Fit<T>::fillSVDConstraints;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/LinearFit.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif















