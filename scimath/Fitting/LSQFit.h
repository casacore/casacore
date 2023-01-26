//# LSQFit.h: Basic class for least squares fitting
//# Copyright (C) 1999-2001,2004-2008
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

#ifndef SCIMATH_LSQFIT_H
#define SCIMATH_LSQFIT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/RecordTransformable.h>
#include <casacore/scimath/Fitting/LSQMatrix.h>
#include <casacore/scimath/Fitting/LSQTraits.h>
#include <complex>
#include <string>
#include <utility>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
 
// <summary> Basic class for the least squares fitting </summary>
// <reviewed reviewer="Neil Killeen" date="2000/06/01" tests="tLSQFit"
//	 demos="">
// </reviewed>

// <prerequisite>
//   <li> Some knowledge of Matrix operations
//   <li> The background information provided in 
//        <a href="../notes/224.html">Note 224</a>.
//   <li> <linkto module="Fitting">Fitting module</linkto>
// </prerequisite>
//
// <etymology>
// From Least SQuares and Fitting
// </etymology>
//
// <synopsis>
// The LSQFit class contains the basic functions to do all the fitting
// described in the 
// <a href="../notes/224.html">Note</a>
// about fitting.
// It handles real, and complex equations;<br>
// linear and non-linear (Levenberg-Marquardt) solutions;<br>
// regular (with optional constraints) or Singular Value Decomposition
// (<src>SVD</src>).<br>
// In essence they are a set of routines to generate normal equations
// (<src>makeNorm()</src>) in triangular form from a set of condition
// equations;<br>
// to do a Cholesky-type decomposition of the normal
// equations (either regular or <src>SVD</src>) and test its rank
// (<src>invert()</src>);<br>
// to do a quasi inversion of the decomposed equations (<src>solve()</src>) to
// obtain the solution and/or the errors.
//
// All calculations are done in place.
// Methods to obtain additional information about the fitting process are
// available.
//
// This class can  be used as a stand-alone class outside of the Casacore
// environment.  In that case the aips.h include file
// can be replaced if necessary by appropriate typedefs for double, float and 
// uint32_t.<br> 
// The interface to the methods have standard data or standard STL iterator
// arguments only. They can be used with any container having an STL
// random-access iterator interface. Especially they can be used with
// <src>carrays</src> (necessary templates provided),
// Casacore Vectors (necessary templates
// provided in <src>LSQaips</src>),
// standard random access STL containers (like <src>std::vector</src>).
//
// The normal operation of the class consists of the following steps:
// <ul> 
// <li> Create an LSQFit object.
//      The information that can be provided in the constructor of the object,
// either directly, or indirectly using the <src>set()</src> commands, is
// (see <a href="../notes/224.html">Note 224</a>):
// <ul>
//  <li> The number of unknowns that have to be solved for (mandatory)
//  <li> The number of constraint equations you want to use explicitly
//		(defaults to 0, but can be changed on-the-fly)
// </ul>
// Separately settable are:
// <ul>
//  <li> A collinearity test factor (defaults to 1e-8)
//  <note role=warning>
// The collinearity factor is the square of the sine of the angle between
// a column in the normal equations, and the hyper-plane through
// all the other columns. In special cases (e.g. fitting a polynomial
// in a very narrow bandwidth window, it could be advisable to set this
// factor to zero if you want a solution (whatever the truth of it maybe). 
// </note>
//  <li> A Levenberg-Marquardt adjustment factor (if appropriate,
//        defaults to 1e-3)
// </ul>
//
// <li>Create the normal equations used in solving the set of condition
// equations of the user, by using the <src>makeNorm()</src> methods.
// Separate <src>makenorm()</src> methods are provided for sparse condition
// equations (e.g. if data for 3 antennas are provided, rather than for all 64)
//
// <li>If there are user provided constraints, either limiting constraints like
// the sum of the angles in a triangle is 180 degrees, or constraints to add
// missing information if e.g. only differences between parameters have been
// measured, they can be added to the normal
// equations with the <src>setConstraint()</src> or
// the <src>addConstraint()</src> methods. Lagrange multipliers will be used to
// solve the extended normal equations.
//
// <li>The normal equations are triangu;arised (using the collinearity factor
// as a check for solvability) with the <src>invert()</src> method. If the
// normal equations are non-solvable an error is returned, or a switch to
// an SVD solution is made if indicated in the <src>invert</src> call.
//
// <li>The solutions and adjustment errors are obtained with the
// <src>solve()</src> method.
// A non-linear loop in a Levenberg-Marquardt adjustment can be obtained
// (together with convergence information), with the <src>solveLoop()</src>
// method (see below) replacing the combination of
// <src>invert</src> and <src>solve</src>.
//
// <li>Non-linear loops are done by looping through the data using
// <src>makeNorm()</src> calls, and upgrade the solution with the
// <src>solveLoop()</src> method.
// The normal equations are upgraded by changing LM factors. Upgrade depends
// on the 'balanced' factor. The LM factor is either added in some way to all
// diagonal elements (if balanced) or all diagonal elements are multiplied by
// <src>(1+factor)</src> After each loop convergence can be tested
// by the <src>isReady()</src> call; which will return <src>false</src> or
// a non-zero code indicating the ready reason. Reasons for stopping can be:
// <ul>
// <li> SOLINCREMENT: the relative change in the norm of the parameter
// solutions is less than 
// (a settable, <src>setEpsValue()</src>, default 1e-8) value.
// <li> DERIVLEVEL: the inf-norm of the known vector of the equations to be
// solved is less than the settable, <src>setEpsDerivative()</src>, default
// 1e-8, value.
// <li> MAXITER: maximum number of iterations reached (only possible if a
// number is explicitly set)
// <li> NOREDUCTION: if the Levenberg-Marquardt correction factor goes towards
// infinity. I.e. if no Chi2 improvement seems possible. Have to redo the
// solution with a different start condition for the unknowns.
// <li> SINGULAR: can only happen due to numeric rounding, since the LM
// equations are always positive-definite. Best solution is to indicate SVD
// needed in the <src>solveLoop</src> call, which is cost-free
// </ul> 
//
// <li>Covariance information in various forms can be obtained with the 
// <src>getCovariance(), getErrors()</src>, <src>getChi()</src> 
// (or <src>getChi2</src>), <src>getSD</src> and <src>getWeightedSD</src>
// methods after a <src>solve()</src> or after the final loop in a non-linear
// solution (of course, when necessary only).
// </ul>
//
// An LSQFit object can be re-used by issuing the <src>reset()</src> command,
// or <src>set()</src> of new
// values. If an unknown has not been used in the condition equations at all,
// the <src>doDiagonal()</src> will make sure a proper solution is obtained,
// with missing unknowns zeroed.
//
// Most of the calculations are done in place; however, enough data is saved
// that it is possible to continue
// with the same (partial) normal equations after e.g. an interim solution.
//
// If the normal equations are produced in separate partial sets (e.g.
// in a multi-processor environment) a <src>merge()</src> method can combine
// them.
// <note role=tip>
// It is suggested to add any possible constraint equations after the merge.
// </note>
//
// A <src>debugIt()</src> method provides read access to all internal
// information.
//
// The member definitions are split over three files. The second
// one contains the templated member function definitions, to bypass the
// problem of duplicate definitions of non-templated members when 
// pre-compiling them. The third contains methods for saving objects as
// Records or through AipsIO.
//
// <note role=warning> No boundary checks on input and output containers
// is done for faster execution. In general these tests should be done at
// the higher level routines, like the
// <linkto class=LinearFit>LinearFit</linkto> and
// <linkto class=NonLinearFitLM>NonLinearFit</linkto> classes which should be
// checked for usage of LSQFit.
// </note>
//
// The contents can be saved in a record (<src>toRecord</src>), 
// and an object can be created from a record (<src>fromRecord</src>).
// The record identifier is 'lfit'.
// <br>The object can also be saved or restored using AipsIO.
// </synopsis>
//
// <example>
// See the tLSQFit.cc and tLSQaips.cc program for extensive examples.
//
// The following example will first create 2 condition equations for 
// 3 unknowns (the third is degenerate). It will first create normal equations
// for a 2 unknown solution and solve; then it will create normal equations
// for a 3 unknown solution, and solve (note that the degenerate will be
// set to 0. The last one will use SVD and one condition equation.r 
// <srcblock>
//   #include <casacore/casa/aips.h>
//   #include <casacore/scimath/Fitting/LSQFit.h>
//   #include <iostream>
//   
//   int main() {
//     // Condition equations for x+y=2; x-y=4;
//     double ce[2][3] = {{1, 1, 0}, {1, -1, 0}};
//     double m[2] = {2, 4};
//     // Solution and error area
//     double sol[3];
//     double sd, mu;
//     uint32_t rank;
//     bool ok;
//   
//     // LSQ object
//     LSQFit fit(2);
//   
//     // Make normal equation
//     for (uint32_t i=0; i<2; i++) fit.makeNorm(ce[i], 1.0, m[i]);
//     // Invert(decompose) and show
//     ok = fit.invert(rank);
//     cout << "ok? " << ok << "; rank: " << rank << endl;
//     // Solve and show
//     if (ok) {
//       fit.solve(sol, &sd, &mu);
//       for (uint32_t i=0; i<2; i++) cout << "Sol" << i << ": " << sol[i] << endl;
//       cout << "sd: "<< sd << "; mu: " << mu << endl;
//     };
//     cout << "----------" << endl; 
//   
//     // Retry with 3 unknowns: note auto fill of unmentioned one
//     fit.set(uint32_t(3));
//     for (uint32_t i=0; i<2; i++) fit.makeNorm(ce[i], 1.0, m[i]);
//     ok = fit.invert(rank);
//     cout << "ok? " << ok << "; rank: " << rank << endl;
//     if (ok) {
//       fit.solve(sol, &sd, &mu);
//       for (uint32_t i=0; i<3; i++) cout << "Sol" << i << ": " << sol[i] << endl;
//       cout << "sd: "<< sd << "; mu: " << mu << endl; 
//     };
//     cout << "----------" << endl; 
//   
//     // Retry with 3 unknowns; but 1 condition equation and use SVD
//     fit.reset();
//     for (uint32_t i=0; i<1; i++) fit.makeNorm(ce[i], 1.0, m[i]);
//     ok = fit.invert(rank, true);
//     cout << "ok? " << ok << "; rank: " << rank << endl;
//     if (ok) {
//       fit.solve(sol, &sd, &mu);
//       for (uint32_t i=0; i<3; i++) cout << "Sol" << i << ": " << sol[i] << endl;
//       cout << "sd: "<< sd << "; mu: " << mu << endl; 
//     };
//     cout << "----------" << endl; 
//   
//     // Without SVD it would be:
//     fit.reset();
//     for (uint32_t i=0; i<1; i++) fit.makeNorm(ce[i], 1.0, m[i]);
//     ok = fit.invert(rank);
//     cout << "ok? " << ok << "; rank: " << rank << endl;
//     if (ok) {
//       fit.solve(sol, &sd, &mu);
//       for (uint32_t i=0; i<3; i++) cout << "Sol" << i << ": " << sol[i] << endl;
//       cout << "sd: "<< sd << "; mu: " << mu << endl; 
//     };
//     cout << "----------" << endl; 
//   
//     exit(0);
//   }
// </srcblock>
// Which will produce the output:
// <srcblock>
//   ok? 1; rank: 2
//   Sol0: 3
//   Sol1: -1
//   sd: 0; mu: 0
//   ----------
//   ok? 1; rank: 3
//   Sol0: 3
//   Sol1: -1
//   Sol2: 0
//   sd: 0; mu: 0
//   ----------
//   ok? 1; rank: 2
//   Sol0: 1
//   Sol1: 1
//   Sol2: 0
//   sd: 0; mu: 0
//   ----------
//   ok? 0; rank: 2
//   ----------
// </srcblock>
// </example>
//
// <motivation>
// The class was written to be able to do complex, real standard and SVD
// solutions in a simple and fast way.
// </motivation>
//
// <todo asof="2006/04/02">
//   <li> a thorough check if all loops are optimal in the makeNorm() methods
//   <li> input of condition equations with cross covariance
// </todo>

class LSQFit {
 public:
  // Simple classes to overload templated memberfunctions
  struct Real      { enum normType { REAL }; };
  struct Complex   { enum normType { COMPLEX }; };
  struct Separable { enum normType { SEPARABLE }; };
  struct AsReal    { enum normType { ASREAL }; };
  struct Conjugate { enum normType { CONJUGATE }; };
  // And values to use
  static Real      REAL;
  static Complex   COMPLEX;
  static Separable SEPARABLE;
  static AsReal    ASREAL;
  static Conjugate CONJUGATE;

  //# Public enums
  // State of the non-linear solution
  enum ReadyCode {
    NONREADY=0,
    SOLINCREMENT,
    DERIVLEVEL,
    MAXITER,
    NOREDUCTION,
    SINGULAR,
    N_ReadyCode
  };
  // Offset of fields in error_p data area.
  enum ErrorField {
    // Number of condition equations
    NC,
    // Sum weights of condition equations
    SUMWEIGHT,
    // Sum known terms squared
    SUMLL,
    // Calculated chi^2
    CHI2,
    // Number of error fields
    N_ErrorField
  };
  //# Constructors
  // Construct an object with the number of unknowns and
  // constraints, using the default collinearity factor and the
  // default Levenberg-Marquardt adjustment factor.
  // <group>
  // Assume real
  explicit LSQFit(uint32_t nUnknowns, uint32_t nConstraints=0);
  // Allow explicit Real specification
  LSQFit(uint32_t nUnknowns, const LSQReal &, uint32_t nConstraints=0);
  // Allow explicit Complex specification
  LSQFit(uint32_t nUnknowns, const LSQComplex &, uint32_t nConstraints=0);
  // </group>
  // Default constructor (empty, only usable after a <src>set(nUnknowns)</src>)
  LSQFit();
  // Copy constructor (deep copy)
  LSQFit(const LSQFit &other);
  // Assignment (deep copy)
  LSQFit &operator=(const LSQFit &other);

  //# Destructor
  ~LSQFit();

  //# Operators

  //# General Member Functions
  // Triangularize the normal equations and determine
  // the rank <src>nRank</src> of the normal equations and, in the case of
  // an <src>SVD</src> solution,  the constraint
  // equations. The collinearity factor is used
  // to determine if the system can be solved (in essence it is the square
  // of the sine of the angle between a column in the normal equations and
  // the plane suspended by the other columns: if too
  // parallel, the equations are degenerate).
  // If <src>doSVD</src> is given as false, false is returned if rank not
  // maximal, else an <src>SVD</src> solution is done.
  bool invert(uint32_t &nRank, bool doSVD=false);
  // Copy date from beg to end; converting if necessary to complex data
  // <group>
  template <class U>
    void copy(const double *beg, const double *end, U &sol, LSQReal); 
  template <class U>
    void copy(const double *beg, const double *end, U &sol, LSQComplex); 
  template <class U>
    void copy(const double *beg, const double *end, U *sol, LSQReal); 
  template <class U>
    void copy(const double *beg, const double *end, U *sol, LSQComplex); 
  template <class U>
    void uncopy(double *beg, const double *end, U &sol, LSQReal); 
  template <class U>
    void uncopy(double *beg, const double *end, U &sol, LSQComplex); 
  template <class U>
    void uncopy(double *beg, const double *end, U *sol, LSQReal); 
  template <class U>
    void uncopy(double *beg, const double *end, U *sol, LSQComplex); 
  template <class U>
    void copyDiagonal(U &errors, LSQReal);
  template <class U>
    void copyDiagonal(U &errors, LSQComplex);
  // </group>
  // Solve normal equations.
  // The solution will be given in <src>sol</src>.
  // <group>
  template <class U>
    void solve(U *sol);
  template <class U>
    void solve(std::complex<U> *sol);
  template <class U>
    void solve(U &sol);
  // </group>
  // Solve a loop in a non-linear set.
  // The methods with the  <src>fit</src> argument are deprecated. Use
  // the combination without the 'fit' parameter, and the <src>isReady()</src>
  // call. The 'fit' parameter returns
  // for each loop a goodness
  // of fit indicator. If it is >0; more loops are necessary.
  // If it is negative,
  // and has an absolute value of say less than .001, it is probably ok, and
  // the iterations can be stopped.
  // Other arguments are as for <src>solve()</src> and <src>invert()</src>.
  // The <src>sol</src> is used for both input (parameter guess) and output.
  // <group>
  template <class U>
    bool solveLoop(uint32_t &nRank,
		   U *sol,
		   bool doSVD=false);
  template <class U>
    bool solveLoop(uint32_t &nRank,
		   std::complex<U> *sol,
		   bool doSVD=false);
  template <class U>
    bool solveLoop(uint32_t &nRank,
		   U &sol,
		   bool doSVD=false);
  template <class U>
    bool solveLoop(double &fit, uint32_t &nRank,
		   U *sol,
		   bool doSVD=false);
  template <class U>
    bool solveLoop(double &fit, uint32_t &nRank,
		   std::complex<U> *sol,
		   bool doSVD=false);
  template <class U>
    bool solveLoop(double &fit, uint32_t &nRank,
		   U &sol,
		   bool doSVD=false);
  // </group>
  // Make normal equations using the <src>cEq</src> condition equation (cArray)
  // (with <src>nUnknowns</src> elements) and a weight <src>weight</src>,
  // given the known observed value <src>obs</src>.
  //
  // <src>doNorm</src> and <src>doKnown</src> can be used
  // to e.g. re-use existing normal equations, i.e. the condition equations,
  // but make a new known side (i.e. new observations).
  //
  // The versions with <src>cEqIndex[]</src> indicate which of the 
  // <src>nUnknowns</src> are actually present in the condition equation
  // (starting indexing at 0); the other terms are supposed to be zero. E.g.
  // if a 12-telescope array has an equation only using telescopes 2 and 4,
  // the lengths of <src>cEqIndex</src> and <src>cEq</src> will be both 2,
  // and the index will contain 1 and 3 (when telescope numbering starts at 1)
  // or 2 and 4 (when telescope numbering starts at 0. The index is given
  // as an iterator (and hence can be a raw pointer)
  //
  // The complex versions can have different interpretation of the inputs,
  // where the complex number can be seen either as a complex number; as two
  // real numbers, or as coefficients of equations with complex conjugates.
  // See  <a href="../notes/224.html">Note 224</a>)
  // for the details.
  //
  // Versions with <em>pair</em> assume that the pairs are created by the
  // <em>SparseDiff</em> automatic differentiation class. The pair is an index
  // and a value. The indices are assumed to be sorted.
  //
  // Special (<em>makeNormSorted</em>) indexed versions exist which assume
  // that the given indices are sorted (which is the case for the
  // LOFAR BBS environment).
  //
  // Some versions exist with two sets of equations (<em>cEq2, obs2</em>).
  // If two simultaneous equations are created they will be faster.
  //
  // Note that the
  // use of <src>const U &</src> is due to a float->double conversion problem
  // on Solaris. Linux was ok.
  // <group>
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight, const U &obs,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight, const U &obs,
		  LSQFit::Real,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Complex,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Separable,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::AsReal,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Conjugate,
		  bool doNorm=true, bool doKnown=true);
  //
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight, const U &obs,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const V &cEq2,
		  const U &weight, const U &obs, const U &obs2,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight, const U &obs,
		  LSQFit::Real,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Complex,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Separable,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::AsReal,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
    void makeNorm(uint32_t nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Conjugate,
		  bool doNorm=true, bool doKnown=true);
  //
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight, const U &obs,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight, const U &obs,
		  LSQFit::Real,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight,
		  const std::complex<U> &obs,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Complex,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Separable,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::AsReal,
		  bool doNorm=true, bool doKnown=true);
  template <class U, class V>
    void makeNorm(const std::vector<std::pair<uint32_t, V> > &cEq,
		  const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Conjugate,
		  bool doNorm=true, bool doKnown=true);
  //
  template <class U, class V, class W>
  void makeNormSorted(uint32_t nIndex, const W &cEqIndex,
		      const V &cEq, const U &weight,
		      const U &obs,
		      bool doNorm=true, bool doKnown=true);
  template <class U, class V, class W>
  void makeNormSorted(uint32_t nIndex, const W &cEqIndex,
		      const V &cEq, const V &cEq2, const U &weight,
		      const U &obs, const U &obs2,
		      bool doNorm=true, bool doKnown=true);
  // </group>
  // Get the <src>n-th</src> (from 0 to the rank deficiency, or missing rank,
  // see e.g. <src>getDeficiency()</src>)
  // constraint equation as determined by <src>invert()</src> in SVD-mode in
  // <src> cEq[nUnknown]</src>. false returned for illegal n. Note
  // that nMissing will be equal to the number of unknowns
  // (<src>nUnknowns</src>, or double that for the complex case) minus the
  // rank as returned from the <src>invert()</src> method.
  // <group> 
  template <class U> 
    bool getConstraint(uint32_t n, U *cEq) const;
  template <class U>
    bool getConstraint(uint32_t n, std::complex<U> *cEq) const;
  template <class U>
    bool getConstraint(uint32_t n, U &cEq) const;
  // </group>
  // Add a new constraint equation (updating nConstraints); or set a
  // numbered constraint equation (0..nConstraints-1). false if illegal
  // number n. The constraints are equations with <src>nUnknowns</src> terms, 
  // and a constant value. E.g. measuring three angles of a triangle
  // could lead to equation <src>[1,1,1]</src> with obs as
  // <src>3.1415</src>. Note that each complex constraint will be
  // converted into two real constraints (see 
  // <a href="../notes/224.html">Note 224</a>).
  // <group>
  template <class U, class V>
    bool setConstraint(uint32_t n, const V &cEq, const U &obs);
  template <class U, class V>
    bool setConstraint(uint32_t n, const V &cEq,
		       const std::complex<U> &obs);
  template <class U, class V, class W>
    bool setConstraint(uint32_t n, uint32_t nIndex, const W &cEqIndex,
		       const V &cEq, const U &obs);
  template <class U, class V, class W>
    bool setConstraint(uint32_t n, uint32_t nIndex, const W &cEqIndex,
		       const V &cEq,
		       const std::complex<U> &obs);
  template <class U, class V>
    bool addConstraint(const V &cEq, const U &obs);
  template <class U, class V>
    bool addConstraint(const V &cEq,
		       const std::complex<U> &obs);
  template <class U, class V, class W>
    bool addConstraint(uint32_t nIndex, const W &cEqIndex,
		       const V &cEq, const U &obs);
  template <class U, class V, class W>
    bool addConstraint(uint32_t nIndex, const W &cEqIndex,
		       const V &cEq,
		       const std::complex<U> &obs);
  // </group>
  // Merge other <src>LSQFit</src> object (i.e. the normal equation and
  // related information) into <src>this</src>. Both objects must have the
  // same number of unknowns, and be pure normal equations (i.e. no
  // <src>invert(), solve(), solveLoop()</src> or statistics calls
  // should have been made). If merging cannot be done, <src>false</src>
  // is returned. The index case (the index is an iterator) assumes that
  // the normal equations to be merged are a sparse subset of the complete
  // matrix. The index 'vector' specifies which unknowns are present. An index
  // outside the scope of the final equations will be skipped.
  // <note role=tip> For highest numerical precision in the case of a larger
  // number of partial normal equations to be merged, it is best to merge
  // them in pairs (and repeat).
  // </note>
  // <group>
  bool merge(const LSQFit &other);
  bool merge(const LSQFit &other, uint32_t nIndex, const uint32_t *nEqIndex) {
    return mergeIt(other, nIndex, nEqIndex); }
  bool merge(const LSQFit &other, uint32_t nIndex,
	     const std::vector<uint32_t> &nEqIndex) {
    return mergeIt(other, nIndex, &nEqIndex[0]); }
  template <class W>
    bool merge(const LSQFit &other, uint32_t nIndex, const W &nEqIndex) {
    std::vector<uint32_t> ix(nIndex);
    for (uint32_t i=0; i<nIndex; ++i) ix[i] = nEqIndex[i];
    return mergeIt(other, nIndex, &ix[0]); }
  // </group>
  // Reset status to empty
  void reset();
  // Set new sizes (default is for Real)
  // <group>
  void set(uint32_t nUnknowns, uint32_t nConstraints=0);
  void set(int32_t nUnknowns, int32_t nConstraints=0) { 
    set (static_cast<uint32_t>(nUnknowns), static_cast<uint32_t>(nConstraints));
  };
  void set(uint32_t nUnknowns, const LSQReal &, uint32_t nConstraints=0) {
    set (nUnknowns, nConstraints);
  };
  void set(int32_t nUnknowns, const LSQReal &, int32_t nConstraints=0) { 
    set (nUnknowns, nConstraints);
  };
  void set(uint32_t nUnknowns, const LSQComplex &, uint32_t nConstraints=0); 
  void set(int32_t nUnknowns, const LSQComplex &, int32_t nConstraints=0) { 
    set (static_cast<uint32_t>(nUnknowns), LSQComplex(),
	 static_cast<uint32_t>(nConstraints));
  };
  // </group>
  // Set new factors (collinearity <src>factor</src>, and Levenberg-Marquardt
  // <src>LMFactor</src>)
  void set(double factor=1e-6, double LMFactor=1e-3);
  // Set new value solution test
  void setEpsValue(double epsval=1e-8) {epsval_p = epsval; };
  // Set new derivative test
  void setEpsDerivative(double epsder=1e-8) {epsder_p = epsder; };
  // Set maximum number of iterations
  void setMaxIter(uint32_t maxiter=0) { maxiter_p = maxiter; };
  // Get number of iterations done
  uint32_t nIterations() const { return (maxiter_p>0 ? maxiter_p-niter_p : 0); };
  // Set the expected form of the normal equations
  void setBalanced(bool balanced=false) { balanced_p = balanced; };
  // Ask the state of the non-linear solutions
  // <group>
  LSQFit::ReadyCode isReady() const { return ready_p; };
  const std::string &readyText() const;
  // </group>  
// Get the covariance matrix (of size <src>nUnknowns * nUnknowns</src>)
  // <group>
  template <class U>
  bool getCovariance(U *covar);
  template <class U>
    bool getCovariance(std::complex<U> *covar);
  // </group>
  // Get main diagonal of covariance function (of size <src>nUnknowns</src>)
  // <group>
  template <class U>
    bool getErrors(U *errors);
  template <class U>
    bool getErrors(std::complex<U> *errors);
  template <class U>
    bool getErrors(U &errors);
  // </group>
  // Get the number of unknowns
  uint32_t nUnknowns() const { return nun_p; };
  // Get the number of constraints
  uint32_t nConstraints() const { return ncon_p; };
  // Get the rank deficiency <note role=warning>Note that the number is
  // returned assuming real values. For complex values it has to be halved
  // </note>
  uint32_t getDeficiency() const { return n_p-r_p; };
  // Get chi^2 (both are identical); the standard deviation (per observation)
  // and the standard deviation per weight unit.
  // <group>
  double getChi() const;
  double getChi2() const { return getChi(); };
  double getSD() const;
  double getWeightedSD() const;
  // </group>
  // Debug:
  // <ul>
  // <li> <src>nun    = </src> number of unknowns
  // <li> <src>np     = </src> total number of solved unknowns (nun+ncon)
  // <li> <src>ncon   = </src> number of constraint equations
  // <li> <src>ner    = </src> number of elements in chi<sup>2</sup> vector
  // <li> <src>rank   = </src> rank)
  // <li> <src>nEq    = </src> normal equation (nun*nun as triangular matrix)
  // <li> <src>known  = </src> known vector (np)
  // <li> <src>constr = </src> constraint matrix (ncon*nun)
  // <li> <src>er     = </src> error info vector (ner)
  // <li> <src>piv    = </src> pivot vector (np)
  // <li> <src>sEq    = </src> normal solution equation (np*np triangular)
  // <li> <src>sol    = </src> internal solution vector (np)
  // <li> <src>prec   = </src> collinearity precision
  // <li> <src>nonlin = </src> current Levenberg factor-1
  // </ul>
  // Note that all pointers may be 0.
  void debugIt(uint32_t &nun, uint32_t &np, uint32_t &ncon, uint32_t &ner, uint32_t &rank,
	       double *&nEq, double *&known, double *&constr, double *&er,
	       uint32_t *&piv, double *&sEq, double *&sol,
	       double &prec, double &nonlin) const;
  //
  // Create an LSQFit object from a record.
  // An error message is generated, and false
  // returned if an invalid record is given. A valid record will return true.
  // Error messages are postfixed to error.
  // <group>
  bool fromRecord(String &error, const RecordInterface &in);
  // </group>
  // Create a record from an LSQFit object.
  // The return will be false and an error
  // message generated only if the object does not contain a valid object.
  // Error messages are postfixed to error.
  bool toRecord(String &error, RecordInterface &out) const;
  // Get identification of record
  const String &ident() const;
  //
  // Save or restore using AipsIO.
  // <group>
  void toAipsIO (AipsIO&) const;
  void fromAipsIO (AipsIO&);
  // </group>
  //
 protected:
  //# enum
  // Bits that can be set/referenced
  enum StateBit {
    // Inverted matrix present
    INVERTED = 1,
    // Triangularised
    TRIANGLE = 2*INVERTED,
    // Non-linear solution
    NONLIN = 2*TRIANGLE,
    // Filler for cxx2html
    N_StateBit
  };

  // Record field names
  // <group>
  static const String recid;
  static const String state;
  static const String nun;
  static const String ncon;
  static const String prec;
  static const String startnon;
  static const String nonlin;
  static const String rank;
  static const String nnc;
  static const String piv;
  static const String constr;
  static const String known;
  static const String errors;
  static const String sol;
  static const String lar;
  static const String wsol;
  static const String wcov;
  static const String nceq;
  static const String nar;
  // </group>  

  //# Data
  // Bits set to indicate state
  uint32_t state_p;
  // Number of unknowns
  uint32_t nun_p;
  // Number of constraints
  uint32_t ncon_p;
  // Matrix size (will be n_p = nun_p + ncon_p)
  uint32_t n_p;
  // Rank of normal equations (normally n_p)
  uint32_t r_p;
  // Collinearity precision
  double prec_p;
  // Levenberg start factor
  double startnon_p;
  // Levenberg current factor
  double nonlin_p;
  // Levenberg step factor
  double stepfactor_p;
  // Test value for [incremental] solution in non-linear loop.
  // The <src>||sol increment||/||sol||</src> is tested
  double epsval_p;
  // Test value for known vector in non-linear loop.
  // ||known||<sub>inf</sub> is tested
  double epsder_p;
  // Indicator for a well balanced normal equation. A balanced equation is
  // one with similar values in the main diagonal.
  bool balanced_p;
  // Maximum number of iterations for non-linear solution. If a non-zero 
  // maximum number of iterations is set, the value is tested in non-linear
  // loops
  uint32_t maxiter_p;
  // Iteration count for non-linear solution
  uint32_t niter_p; 
  // Indicate the non-linear state. A non-zero code indicates that non-linear
  // looping is ready.
  ReadyCode ready_p; 
 
  // Pivot table (n_p)
  uint32_t *piv_p;
  // Normal equations (triangular nun_p * nun_p)
  LSQMatrix *norm_p;
  // Current length nceq_p
  uint32_t nnc_p;
  // Normal combined with constraint equations for solutions
  // (triangular nnc_p*nnc_p)
  LSQMatrix *nceq_p;
  // Known part equations (n_p)
  double *known_p;
  // Counts for errors (N_ErrorField)
  double *error_p;
  // Constraint equation area (nun_p*ncon_p))
  double *constr_p;
  // Solution area (n_p)
  double *sol_p;
  // Save area for non-linear case (size determined internally)
  LSQFit *nar_p;
  // Save area for non-symmetric (i.e. with constraints) (n_p * n_p)
  double *lar_p;
  // Work areas for interim solutions and covariance
  // <group>
  double *wsol_p;
  double *wcov_p;
  // </group>

  //# Member functions
  // Get pointer in rectangular array
  // <group>
  double *rowrt(uint32_t i) const { return &lar_p[n_p*i]; };
  double *rowru(uint32_t i) const { return &lar_p[nun_p*i]; };
  // </group>
  // Calculate the real or imag part of <src>x*conj(y)</src>
  // <group>
  static double realMC(const std::complex<double> &x,
		       const std::complex<double> &y) {
    return (x.real()*y.real() + x.imag()*y.imag()); };
  static double imagMC(const std::complex<double> &x,
		       const std::complex<double> &y) {
    return (x.imag()*y.real() - x.real()*y.imag()); };
  static float realMC(const std::complex<float> &x,
		       const std::complex<float> &y) {
    return (x.real()*y.real() + x.imag()*y.imag()); };
  static float imagMC(const std::complex<float> &x,
		       const std::complex<float> &y) {
    return (x.imag()*y.real() - x.real()*y.imag()); };
  // </group>
  // Initialise areas
  void init();
  // Clear areas
  void clear();
  // De-initialise area
  void deinit();
  // Solve normal equations
  void solveIt();
  // One non-linear LM loop
  bool solveItLoop(double &fit, uint32_t &nRank, bool doSVD=false);
  // Solve missing rank part
  void solveMR(uint32_t nin);
  // Invert rectangular matrix (i.e. when constraints present)
  bool invertRect();
  // Get the norm of the current solution vector
  double normSolution(const double *sol) const;
  // Get the infinite norm of the known vector
  double normInfKnown(const double *known) const;
  // Merge sparse normal equations
  bool mergeIt(const LSQFit &other, uint32_t nIndex, const uint32_t *nEqIndex);
  // Save current status (or part)
  void save(bool all=true);
  // Restore current status
  void restore(bool all=true);
  // Copy data. If all false, only the relevant data for non-linear 
  // solution are copied (normal equations, knows and errors).
  void copy(const LSQFit &other, bool all=true);
  // Extend the constraint equation area to the specify number of
  // equations.
  void extendConstraints(uint32_t n);
  // Create the solution equation area nceq_p and fill it.
  void createNCEQ();
  // Get work areas for solutions, covariance
  // <group>
  void getWorkSOL();
  void getWorkCOV();
  // </group>
  //
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/LSQFit2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
