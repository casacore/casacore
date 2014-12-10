//# NonLinearFit.h: Class for non-linear least-squares fit.
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2002,2004
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

#ifndef SCIMATH_NONLINEARFIT_H
#define SCIMATH_NONLINEARFIT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Fitting/GenericL2Fit.h>
namespace casacore { //# begin namesapce casa
//# Forward declarations

//
// <summary> Class for non-linear least-squares fit.
// </summary>
//
// <reviewed reviewer="wbrouw" date="2006/06/15" tests="tNonLinearFitLM.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Functional">Functional</linkto> 
//   <li> <linkto class="Function">Function</linkto> 
//   <li> <linkto module="Fitting">Fitting</linkto>
// </prerequisite>
//
// <etymology>
// A nonlinear function is used to fit a set of data points.  
// </etymology>
//
// <synopsis>
// NOTE: Constraints added. Documentation out of date at moment, check
// the tLinearFitSVD and tNonLinearFirLM programs for examples.
//
// The following is a brief summary of the non-linear least-squares fit
// problem.
// See module header, <linkto module="Fitting">Fitting</linkto>,
// for a more complete description.  
//
// Given a set of N data points (measurements), (x(i), y(i)) i = 0,...,N-1, 
// along with a set of standard deviations, sigma(i), for the data points, 
// and a specified non-linear function, f(x;a) where a = a(j) j = 0,...,M-1 
// are a set of parameters to be 
// determined, the non-linear least-squares fit tries to minimize
// <srcblock>
// chi-square = [(y(0)-f(x(0);a)/sigma(0)]^2 + [(y(1)-f(x(1);a)/sigma(1)]^2 + 
//              ... + [(y(N-1)-f(x(N-1);a))/sigma(N-1)]^2.
// </srcblock>
// by adjusting {a(j)} in the equation.
//
// For multidimensional functions, x(i) is a vector, and
// <srcblock> 
// f(x(i);a) = f(x(i,0), x(i,1), x(i,2), ...;a)
// </srcblock>
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
// A statistic weight can be also be assigned to each measurement if the 
// standard deviation is not available. sigma can be calculated from
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
//  <li> Create a fitter: NonLinearFit<T> fitter();
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
// Singular Value Decomposition is supported by setting the 'svd' switch,
// which has a behaviour completely identical to, apart from a
// default collinearity check of 1e-8. 
//
// Other information (see a.o. <linkto class=LSQaips>LSQaips</linkto>) can
// be set and obtained as well.
// </synopsis>
//
// <motivation>
// The creation of the class module was driven by the need to write code
// to fit Gaussian functions to data points.
// </motivation>
//
// <example>
// </example>

template<class T> class NonLinearFit : public GenericL2Fit<T>
{
public: 
  //# Constants
  // Default maximum number of iterations (30)
  static const uInt MAXITER = 30;
  // Default convergence criterium (0.001)
  static const Double CRITERIUM;

  //# Constructors
  // Create a fitter: the normal way to generate a fitter object. Necessary
  // data will be deduced from the Functional provided with
  // <src>setFunction()</src>.
  // Create optionally a fitter with SVD behaviour specified.
  explicit NonLinearFit(Bool svd=False);
  // Copy constructor (deep copy)
  NonLinearFit(const NonLinearFit &other);
  // Assignment (deep copy)
  NonLinearFit &operator=(const NonLinearFit &other);

  // Destructor
  virtual ~NonLinearFit();

  // setMaxIter() sets the maximum number of iterations to do before stopping.
  // Default value is 30.
  void setMaxIter(uInt maxIter=MAXITER);

  // getMaxIter() queries what the maximum number of iterations currently is
  uInt getMaxIter() const { return maxiter_p; };

  // currentIteration() queries what the current iteration is
  uInt currentIteration() const { return maxiter_p - curiter_p; };

  // setCriteria() sets the convergence criteria. The actual value and
  // its interpretation depends on the derived class used to do the
  // actual iteration. Default value is 0.001.
  void setCriteria(const Double criteria=CRITERIUM) {criterium_p = criteria; };

  // getCriteria() queries the current criteria
  Double getCriteria() const { return criterium_p; };

  // Check to see if the fit has converged
  Bool converged() const { return converge_p; };

protected:
  //#Data
  // Maximum number of iterations
  uInt maxiter_p;
  // Current iteration number
  uInt curiter_p;
  // Convergence criteria
  Double criterium_p;
  // Has fit converged
  Bool converge_p;

  //# Member functions
  // Generalised fitter
  virtual Bool fitIt
    (Vector<typename FunctionTraits<T>::BaseType> &sol, 
     const Array<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
     const Vector<Bool> *const mask=0) = 0;
  
private: 
  //# Data
  
  //# Member functions
  
protected:
  //# Make members of parent classes known.
  using GenericL2Fit<T>::pCount_p;
  using GenericL2Fit<T>::ptr_derive_p;
  using GenericL2Fit<T>::arg_p;
  using GenericL2Fit<T>::sol_p;
  using GenericL2Fit<T>::fsol_p;
  using GenericL2Fit<T>::solved_p;
  using GenericL2Fit<T>::nr_p;
  using GenericL2Fit<T>::svd_p;
  using GenericL2Fit<T>::condEq_p;
  using GenericL2Fit<T>::fullEq_p;
  using GenericL2Fit<T>::err_p;
  using GenericL2Fit<T>::ferr_p;
  using GenericL2Fit<T>::errors_p;
  using GenericL2Fit<T>::valder_p;
  using GenericL2Fit<T>::set;
  using GenericL2Fit<T>::buildConstraint;
  using GenericL2Fit<T>::fillSVDConstraints;
  using GenericL2Fit<T>::isReady;
};

} //# End namespace casacore
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/NonLinearFit.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
