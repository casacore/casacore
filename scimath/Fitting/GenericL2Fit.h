//# GenericL2Fit.h: Generic base class for least-squares fit.
//#
//# Copyright (C) 2001,2002,2004,2005
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

#ifndef SCIMATH_GENERICL2FIT_H
#define SCIMATH_GENERICL2FIT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/scimath/Fitting/LSQaips.h>
#include <casacore/scimath/Fitting/LSQTraits.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/FunctionTraits.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>

namespace casacore { // begin namespace casa

//# Forward declarations
template <class T> class Array;
template <class T, class U> class Function;

// <summary> Generic base class for least-squares fit.
// </summary>
//
// <reviewed reviewer="wbrouw" date="2004/06/14" tests="tLinearFitSVD.cc"
// 	 demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> 
//   <li> <linkto module="Fitting">Fitting</linkto>
// </prerequisite>
//
// <etymology>
// A set of data point is fit with some functional equation.
// The class acts as a generic base class for <src>L2</src> type
// fits.
// </etymology>
//
// <synopsis>
// NOTE: Constraints added. Documentation out of date at moment, check
// the tLinearFitSVD and tNonLinearFitLM programs for examples.
//
// The class acts as a base class for L2-type (least-squares) fitting. 
// Actual classes (se e.g. <linkto class=LinearFit>LinearFit</linkto> and
// <linkto class=NonLinearFit>NonLinearFit</linkto>.
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
//  <li> Create a fitter: GenericL2Fit<T> fitter();
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
// <li> The following data types can be used to instantiate the GenericL2Fit 
//      templated class:
//      Known classes for FunctionTraits. I.e simple numerical like
//	<src>Float</src>, <src>Double</src>, <src>Complex</src>,
//	 <src>DComplex</src>; and the <src>AutoDiff<></src> versions.
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
// <em> asSVD()</em> (which will also set the
// default collinearity to 1e-8).
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
// In the following a polynomial is fitted through the first 20 prime numbers.
// The data is given in the x vector (1 to 20) and in the primesTable
// (2, 3, ..., 71) (see tLinearFitSVD test program). In the following
// all four methods to calculate a polynomial through the data is used
// <srcblock>
//    	// The list of coordinate x-values
//    	Vector<Double> x(nPrimes);
//    	indgen(x, 1.0);  // 1, 2, ...
//    	Vector<Double> primesTable(nPrimes);
//    	for (uInt i=1; i < nPrimes; i++) {
//        primesTable(i) =
//	   Primes::nextLargerPrimeThan(Int(primesTable(i-1)+0.01));
//      }   
//	Vector<Double> sigma(nPrimes);
//	sigma = 1.0;
//	// The fitter
//  	LinearFit<Double> fitter;
//	// Linear combination of functions describing 1 + x + x*x
//    	combination.setCoefficient(0, 1.0);   // 1
//    	combination.setCoefficient(1, 1.0);     // x
//	combination.setCoefficient(2, 1.0);     // x^2
//	// Get the solution
//	fitter.setFunction(combination);
//    	Vector<Double> solution = fitter.fit(x, primesTable, sigma);
//	// Try with a function with automatic derivatives (note that default 
//	// polynomial has zero first guess)
//  	LinearFit<AutoDiffA<Double> > fitad;
//    	Polynomial<AutoDiffA<Double> > sqre(2);
//    	fitad.setFunction(sqre);
//    	solution = fitad.fit(x, primesTable, sigma);
// </srcblock>
// In the test program examples are given on how to get the other
// information, and other examples.
// </example>

template<class T> class GenericL2Fit : public LSQaips {
 public: 
  //# Constants
  // Default collinearity test for SVD
  const Double COLLINEARITY;

  //# Constructors
  // Create a fitter: the normal way to generate a fitter object. Necessary
  // data will be deduced from the Functional provided with
  // <src>setFunction()</src>
  GenericL2Fit();
  // Copy constructor (deep copy)
  GenericL2Fit(const GenericL2Fit &other);
  // Assignment (deep copy)
  GenericL2Fit &operator=(const GenericL2Fit &other);

  // Destructor
  virtual ~GenericL2Fit();

  // Sets the function to be fitted.  Upon entry, the argument function object 
  // is cloned.  The cloned copy is used in the later fitting process.
  // A valid function should be an instance of the
  // <linkto class="Function">Function</linkto> class,
  // so that derivatives with respect to the adjustable parameters
  // can be calculated.  The current values of the "available" parameters
  // of the function are taken as the initial guess for the non-linear fitting.
  template <class U>  
    void setFunction(const Function<U,U> &function) { resetFunction();
    ptr_derive_p = function.cloneAD(); setFunctionEx(); }

  // Set the possible constraint functions. The <src>addConstraint</src>
  // will add one; the <src>setConstraint</src> will [re-]set the
  // <src>n</src>th constraint. If unsucessful, False returned.<br>
  // Constraint functional can only be set when the function to be fitted
  // has been set. It should have the same number of parameters as the function
  // to be fitted. The <src>x</src> should have the correct dimension.
  // <group>
  template <class U>
    Bool setConstraint(const uInt n,
		       const Function<U,U> &function,
		       const Vector<typename FunctionTraits<T>::BaseType> &x,
		       const typename FunctionTraits<T>::BaseType y=
		       typename FunctionTraits<T>::BaseType(0)) {
    if (n >= constrFun_p.nelements() ||
	!ptr_derive_p ||
	ptr_derive_p->nparameters() != function.nparameters() ||
	function.ndim() != x.nelements()) return False;
    delete constrFun_p[n]; constrFun_p[n] = 0;
    constrFun_p[n] = function.cloneAD(); return setConstraintEx(n, x, y); }
  Bool setConstraint(const uInt n,
		     const Vector<typename FunctionTraits<T>::BaseType> &x,
		     const typename FunctionTraits<T>::BaseType y=
		     typename FunctionTraits<T>::BaseType(0));
  Bool setConstraint(const uInt n,
		     const typename FunctionTraits<T>::BaseType y=
		     typename FunctionTraits<T>::BaseType(0));
  Bool addConstraint(const Function<typename FunctionTraits<T>::DiffType,
		     typename FunctionTraits<T>::DiffType> &function,
		     const Vector<typename FunctionTraits<T>::BaseType> &x,
		     const typename FunctionTraits<T>::BaseType y=
		     typename FunctionTraits<T>::BaseType(0));
  Bool addConstraint(const Vector<typename FunctionTraits<T>::BaseType> &x,
		     const typename FunctionTraits<T>::BaseType y=
		     typename FunctionTraits<T>::BaseType(0));
  Bool addConstraint(const typename FunctionTraits<T>::BaseType y=
		     typename FunctionTraits<T>::BaseType(0));
  // </group>
  // Set the collinearity factor as the square of the sine of the
  // minimum angle allowed between input vectors (default zero for non-SVD,
  // 1e-8 for SVD)
  void setCollinearity(const Double cln);

  // Set sigma values to be interpreted as weight (i.e. 1/sigma/sigma).
  // A value of zero or -1 will be skipped. The switch will stay in effect
  // until set False again explicitly. Default is False.
  void asWeight(const Bool aswgt) { asweight_p = aswgt; }

  // Set the use of SVD or not (default). When set the default collinearity
  // is set as well.
  void asSVD(const Bool svd);

  // Return a pointer to the function being fitted.  Should
  // never delete this pointer.
  // <group>
  Function<typename FunctionTraits<T>::DiffType,
    typename FunctionTraits<T>::DiffType> *fittedFunction() {
    return ptr_derive_p; }
  const Function<typename FunctionTraits<T>::DiffType,
                 typename FunctionTraits<T>::DiffType>*
    fittedFunction() const { return ptr_derive_p; }
  // </group>
  // Return the number of fitted parameters
  uInt fittedNumber() const { return aCount_ai; }

  // Return the number of constraints, and pointers to constraint functions.
  // A <src>0-pointer</src> will be returned if no such constraint present.
  // This pointer should never be destroyed.
  // <group>
  uInt NConstraints() { return constrFun_p.nelements(); }
  Function<typename FunctionTraits<T>::DiffType,
    typename FunctionTraits<T>::DiffType> *getConstraint(const uInt n) {
    return (n >= constrFun_p.nelements() ? 0 : constrFun_p[n]); }
  // </group>

  // Return the nth constraint equation derived from SVD
  // Note that the number present will be given by <src>getDeficiency()</src>
  Vector<typename LSQTraits<typename FunctionTraits<T>::
    BaseType>::base> getSVDConstraint(uInt n);
  // Set the parameter values. The input is a vector of parameters; all
  // or only the masked ones' values will be set, using the input values
  // <group>
  void setParameterValues
    (const Vector<typename FunctionTraits<T>::BaseType> &parms);
  void setMaskedParameterValues
    (const Vector<typename FunctionTraits<T>::BaseType> &parms);
  // </group>
  
  // Fit the function to the data. If no sigma provided, all ones assumed.
  // In the case of no x,y,sigma the fitting equations are supposed to be
  // generated by previous calls to buildNormalMatrix. Note that the ones
  // with a scalar sigma will assume sigma=1 (overloading problem). The mask
  // assumes that if present, points with False will be skipped.
  // <thrown>
  //  <li> AipsError if unmatched array sizes given
  //  <li> AipsError if equations cannot be inverted (not in SVD case and in
  //		the case of the Bool versions.)
  // </thrown>
  // <group>
  Vector<typename FunctionTraits<T>::BaseType>
    fit(const Vector<typename FunctionTraits<T>::BaseType> &x, 
	const Vector<typename FunctionTraits<T>::BaseType> &y,
	const Vector<typename FunctionTraits<T>::BaseType> &sigma,
	const Vector<Bool> *const mask=0);
  Vector<typename FunctionTraits<T>::BaseType>
    fit(const Matrix<typename FunctionTraits<T>::BaseType> &x, 
	const Vector<typename FunctionTraits<T>::BaseType> &y,
	const Vector<typename FunctionTraits<T>::BaseType> &sigma,
	const Vector<Bool> *const mask=0);
  Vector<typename FunctionTraits<T>::BaseType>
    fit(const Vector<typename FunctionTraits<T>::BaseType> &x, 
	const Vector<typename FunctionTraits<T>::BaseType> &y,
	const Vector<Bool> *const mask=0);
  Vector<typename FunctionTraits<T>::BaseType>
    fit(const Matrix<typename FunctionTraits<T>::BaseType> &x, 
	const Vector<typename FunctionTraits<T>::BaseType> &y,
	const Vector<Bool> *const mask=0);
  Vector<typename FunctionTraits<T>::BaseType>
    fit(const Vector<Bool> *const mask=0); 
  Bool fit(Vector<typename FunctionTraits<T>::BaseType> &sol,
	   const Vector<typename FunctionTraits<T>::BaseType> &x, 
	   const Vector<typename FunctionTraits<T>::BaseType> &y,
	   const Vector<typename FunctionTraits<T>::BaseType> &sigma,	
	   const Vector<Bool> *const mask=0);
  Bool fit(Vector<typename FunctionTraits<T>::BaseType> &sol,
	   const Matrix<typename FunctionTraits<T>::BaseType> &x, 
	   const Vector<typename FunctionTraits<T>::BaseType> &y,
	   const Vector<typename FunctionTraits<T>::BaseType> &sigma,
	   const Vector<Bool> *const mask=0);
  Bool fit(Vector<typename FunctionTraits<T>::BaseType> &sol,
	   const Vector<typename FunctionTraits<T>::BaseType> &x, 
	   const Vector<typename FunctionTraits<T>::BaseType> &y,
	   const typename FunctionTraits<T>::BaseType &sigma,
	   const Vector<Bool> *const mask=0);
  Bool fit(Vector<typename FunctionTraits<T>::BaseType> &sol,
	   const Matrix<typename FunctionTraits<T>::BaseType> &x, 
	   const Vector<typename FunctionTraits<T>::BaseType> &y,
	   const typename FunctionTraits<T>::BaseType &sigma,
	   const Vector<Bool> *const mask=0);
  Bool fit(Vector<typename FunctionTraits<T>::BaseType> &sol,
	   const Vector<Bool> *const mask=0);
  // </group>

  // Obtain the chi squared. It has already been calculated during the
  // fitting process.
  // <group>
  Double chiSquare() const { return getChi(); }
  // </group>

  // Get the errors on the solved values
  // <thrown>
  //  <li> AipsError if none present (or Bool returned)
  // </thrown>
  // <group>
  const Vector<typename FunctionTraits<T>::BaseType> &errors() const;
  Bool errors(Vector<typename FunctionTraits<T>::BaseType> &err) const;
  // </group>

  // Get covariance matrix
  // <group>
  Matrix<Double> compuCovariance();
  void compuCovariance(Matrix<Double> &cov);
  // </group>

  // Generate the normal equations by one or more calls to the
  // buildNormalMatrix(), before calling a fit() without arguments.
  // The arguments are the same as for the fit(arguments) function.
  // A False is returned if the Array sizes are unmatched.
  // <group>
  void buildNormalMatrix
    (const Vector<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> &sigma,
     const Vector<Bool> *const mask=0);
  void buildNormalMatrix
    (const Matrix<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> &sigma,
     const Vector<Bool> *const mask=0);
  void buildNormalMatrix
    (const Vector<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<Bool> *const mask=0);
  void buildNormalMatrix
    (const Matrix<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<Bool> *const mask=0);
  // </group>
  // Return the residual after a fit in y. x can 
  // be a vector (if 1D function) or a matrix (ND functional), as in the 
  // fit() methods. If sol is given, it is the solution derived from
  // a fit and its value will be used; otherwise  only the parameters
  // in the fitted functional will be used.
  // If <src>model</src> is given as <src>True</src>, the model, rather
  // the residual <src><data>-<model></src> will be returned in <src>y</src>.
  // False is returned if residuals cannot be calculated.
  // <thrown>
  // <li> Aipserror if illegal array sizes
  // </thrown>
  // <group>
  Bool residual(Vector<typename FunctionTraits<T>::BaseType> &y,
		const Array<typename FunctionTraits<T>::BaseType> &x,
		const Vector<typename FunctionTraits<T>::BaseType> &sol,
		const Bool model=False);
  Bool residual(Vector<typename FunctionTraits<T>::BaseType> &y,
		const Array<typename FunctionTraits<T>::BaseType> &x,
		const Bool model=False);
  // </group>
  // Get the rank of the solution (or zero of no fit() done yet). A 
  // valid solution will have the same rank as the number of unknowns (or
  // double that number in the complex case). For SVD solutions the
  // rank could be less.
  uInt getRank() const {
    return (solved_p ? nUnknowns()-getDeficiency() : 0); }

 protected:
  //#Data
  // Adjustable
  uInt aCount_ai;
  // SVD indicator
  Bool svd_p;
  // Function to use in evaluating condition equation
  Function<typename FunctionTraits<T>::DiffType,
    typename FunctionTraits<T>::DiffType> *ptr_derive_p;
  // List of functions describing the possible constraint equations
  // e.g. The sum of 3 angles w`could be described by a
  // <src>HyperPlane(3)</src> function with <src>[1,1,1]</src>
  // as parameters; giving <src>[1,1,1]</src> as argument vector and
  // <src>3.1415</src> as value.
  // <group>
  PtrBlock<Function<typename FunctionTraits<T>::DiffType,
    typename FunctionTraits<T>::DiffType>*> constrFun_p;
  // List of vectors describing the constraint equations' arguments
  PtrBlock<Vector<typename FunctionTraits<T>::BaseType>*> constrArg_p;
  // List of values describing the constraint equations' value
  PtrBlock<typename FunctionTraits<T>::BaseType *> constrVal_p;
  // </group>
  // Number of available parameters
  uInt pCount_p;
  // Number of dimensions of input data
  uInt ndim_p;
  // No normal equations yet.
  Bool needInit_p;
  // Have solution
  Bool solved_p;
  // Have errors
  Bool errors_p;
  mutable Bool ferrors_p;
  // Interpret as weights rather than as sigma the given values.
  Bool asweight_p;
  // The rank of the solution
  uInt nr_p;
  // Condition equation parameters (for number of adjustable parameters)
  mutable Vector<typename FunctionTraits<T>::BaseType> condEq_p;
  // Equation for all available parameters
  mutable Vector<typename FunctionTraits<T>::BaseType> fullEq_p;
  // Contiguous argument areas
  // <group>
  mutable Vector<typename FunctionTraits<T>::ArgType> arg_p;
  mutable Vector<typename FunctionTraits<T>::ArgType> carg_p;
  // </group>
  // Local solution area
  // <group>
  mutable Vector<typename FunctionTraits<T>::BaseType> sol_p;
  mutable Vector<typename FunctionTraits<T>::BaseType> fsol_p;
  // </group>
  // Local error area
  // <group>
  mutable Vector<typename FunctionTraits<T>::BaseType> err_p;
  mutable Vector<typename FunctionTraits<T>::BaseType> ferr_p;
  // </group>
  // Local value and derivatives
  mutable typename FunctionTraits<T>::DiffType valder_p;
  // Local SVD constraints
  mutable Vector<Vector<typename LSQTraits<typename FunctionTraits<T>::
    BaseType>::base> > consvd_p;
  //# Member functions
  // Generalised fitter
  virtual Bool fitIt
    (Vector<typename FunctionTraits<T>::BaseType> &sol,
     const Array<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
     const Vector<Bool> *const mask=0) = 0;
  // Build the normal matrix
  void buildMatrix(const Array<typename FunctionTraits<T>::BaseType> &x, 
		   const Vector<typename FunctionTraits<T>::BaseType> &y,
		   const Vector<typename FunctionTraits<T>::BaseType>
		   *const sigma,
		   const Vector<Bool> *const mask=0);
  // Build the constraint equations
  void buildConstraint();
  // Get the SVD constraints
  void fillSVDConstraints();
  // Calculate residuals
  Bool buildResidual(Vector<typename FunctionTraits<T>::BaseType> &y,
		     const Array<typename FunctionTraits<T>::BaseType> &x,
		     const Vector<typename FunctionTraits<T>::BaseType>
		     *const sol, const Bool model=False);
  // Function to get evaluated functional value
  typename FunctionTraits<T>::BaseType
    getVal_p(const Array<typename FunctionTraits<T>::BaseType> &x,
	     uInt j, uInt i) const;
  // Initialise the fitter with number of solvable parameters
  void initfit_p(uInt parcnt);
  // Return number of condition equations and check sizes x, y, sigma
  // <thrown>
  //  <li> Aipserror if size inconsistencies 
  // </thrown>
  uInt testInput_p
    (const Array<typename FunctionTraits<T>::BaseType> &x,
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> *const sigma);
  // Reset all the input
  void resetFunction();

 private:
  //# Data

  //# Member functions
  // Set function properties
  void setFunctionEx();
  // Set Constraint properties
  Bool setConstraintEx(const uInt n,
		       const Vector<typename FunctionTraits<T>::BaseType> &x,
		       const typename FunctionTraits<T>::BaseType y);
};

} //# End namespace casacore
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/GenericL2Fit.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
