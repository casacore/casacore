//# LSQaips.h: Interface for aips++ Vectors in least squares fitting
//# Copyright (C) 1999,2000,2001,2004
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

#if !defined(AIPS_LSQAIPS_H)
#define AIPS_LSQAIPS_H

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/VectorSTLIterator.h>
#include <aips/Fitting/LSQFit.h>

//# Forward Declarations

// <summary>
// Interface for aips++ Vectors in least squares fitting
// </summary>
// <reviewed reviewer="" date="2004/04/01" tests=""
//	 demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LSQ>LSQFit</linkto> class
// </prerequisite>
//
// <etymology>
// From Least SQuares and aips++
// </etymology>
//
// <synopsis>
// The interface used in the <src>LSQaips</src> class is in terms of
// aips++ Vectors directly, rather than an STL iterator (like 
// <linkto class=VectorSTLIterator>VectorSTLIterator</linkto>) based
// on it.
//
// Its functionality is identical to that of the
// <linkto class=LSQ>LSQFit</linkto> class, although it will be faster to use
// the iterator interface directly, since constructing of temporary iterators
// can be avoided
// </synopsis>
//
// <example>
// See the <src>tLSQaips.cc</src> program for extensive examples.
// Note: this class is in an interim state.
// </example>
//
// <motivation>
// The class was written to enable easy tranistion from the current Vector
// to the Vector::iterator interface.
// </motivation>
//
// <todo asof="2004/04/02">
//   <li> create all the method interfaces
// </todo>

class LSQaips : public LSQFit {
 public:
  
  //# Constructors
  // Construct an object with the number of unknown, knowns and
  // constraints, and type, using the default collinearity factor and the
  // default Levenberg-Marquardt adjustment factor
  // <group>
  // Assume real
  LSQaips(uInt nUnknowns, uInt nConstraints=0);
  // Allow explicit complex/real specification
  // <group>
  LSQaips(uInt nUnknowns, const LSQReal &, uInt nConstraints=0);
  LSQaips(uInt nUnknowns, const LSQComplex &, uInt nConstraints=0);
  // </group>
  // </group>
  // Default constructor (empty, real, only usable after a set(nUnknowns))
  LSQaips();
  // Copy constructor (deep copy)
  LSQaips(const LSQaips &other);
  // Assignment (deep copy)
  LSQaips &operator=(const LSQaips &other);
  
  //# Destructor
  ~LSQaips();
  
  //# Operators
  
  //# General Member Functions
  // Make normal equations using the <src>cEq</src> condition equations
  // (with <src>nUnknowns</src> elements) and a weight <src>weight</src>,
  // given the known observed value <src>obs</src>.
  //
  // <src>doNorm</src> and <src>doKnown</src> can be used
  // to e.g. re-use existing normal equations, but make a new known side.
  // 
  // The versions with <src>cEqIndex[]</src> indicate which of the 
  // <src>nUnknowns</src> are actually present in the condition equation
  // (starting indexing at 0); the other terms are supposed to be zero. E.g.
  // if a 12-telescope array has an equation only using telescopes 2 and 4,
  // the lengths of <cEqIndex</src> and <src>cEq</src> will be both 2,
  // and the index will contain 1 and 3 (when telescope numbering starts at 1)
  // or 2 and 4 (when telescope numbering starts at 0.
  // <group>
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight, const U &obs,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, doNorm, doKnown); };
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight, const U &obs,
		  LSQFit::Real,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, doNorm, doKnown); };
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, doNorm, doKnown); };
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Complex,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, doNorm, doKnown); };
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Separable,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, LSQFit::SEPARABLE, doNorm, doKnown); };
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::AsReal,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, LSQFit::ASREAL, doNorm, doKnown); };
  template <class U, class V>
    void makeNorm(const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Conjugate,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(cEq, weight, obs, LSQFit::CONJUGATE, doNorm, doKnown); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight, const U &obs,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight, const U &obs,
		  LSQFit::Real,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Complex,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs, doNorm, doKnown); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::AsReal,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs,
		     LSQFit::ASREAL, doNorm, doKnown); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Separable,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs,
		     LSQFit::SEPARABLE, doNorm, doKnown); };
  template <class U, class V, class W>
    void makeNorm(uInt nIndex, const W &cEqIndex,
		  const V &cEq, const U &weight,
		  const std::complex<U> &obs,
		  LSQFit::Conjugate,
		  Bool doNorm=True, Bool doKnown=True) {
    LSQFit::makeNorm(nIndex, cEqIndex, cEq, weight, obs,
		     LSQFit::CONJUGATE, doNorm, doKnown); };
  // </group>
  // Get the <src>nMissing</src> (the rank deficiency, or missing rank)
  // constraint equations as <src> cEq[nUnknowns][nMissing]</src>. Note
  // that nMissing will be equal to the number of unknowns
  // (<src>nUnknowns</src>, or double that for the complex case) minus the
  // rank as returned from the <src>invert()</src> method. 
  // <group>
  template <class U>
    void getConstraint(uInt n, U *cEq) const {
    LSQFit::getConstraint(n, cEq); };
  template <class U>
    void getConstraint(uInt n, U &cEq) const {
    LSQFit::getConstraint(n, cEq); };
  // </group>
  
  // Add a new constraint equation (updating nConstraints); or add a
  // numbered constraint equation (0..nConstraints-1). False if illegal
  // number n. The constraints are equations with nUnknowns terms, 
  // and a constant value. E.g. measuring three angles of a triangle
  // could lead to equation <src>[1,1,1]</src> with obs as
  // <src>3.1415</src>. Note that each complex constraint will be
  // converted into two real constraints (see 
  // <a href="../../../notes/224">Note 224</a>).
  // <group>
  template <class U, class V>
    Bool setConstraint(uInt n, const V &cEq, const U &obs) {
    return LSQFit::setConstraint(n, cEq, obs); };
  template <class U, class V>
    Bool setConstraint(uInt n, const V &cEq,
		       const std::complex<U> &obs) {
    return LSQFit::setConstraint(n, cEq, obs); };
  template <class U, class V, class W>
    Bool setConstraint(uInt n, uInt nIndex, const W &cEqIndex,
		       const V &cEq, const U &obs) {
    return LSQFit::setConstraint(n, nIndex, cEqIndex, cEq, obs); };
  template <class U, class V, class W>
    Bool setConstraint(uInt n, uInt nIndex, const W &cEqIndex,
		       const V &cEq,
		       const std::complex<U> &obs) {
    return LSQFit::setConstraint(n, nIndex, cEqIndex, cEq, obs); };
  template <class U, class V>
    Bool addConstraint(const V &cEq, const U &obs) {
    return LSQFit::addConstraint(cEq, obs); };
  template <class U, class V>
    Bool addConstraint(const V &cEq,
		       const std::complex<U> &obs) {
    return LSQFit::addConstraint(cEq, obs); };
  template <class U, class V, class W>
    Bool addConstraint(uInt nIndex, const W &cEqIndex,
		       const V &cEq, const U &obs) {
    return LSQFit::addConstraint(nIndex, cEqIndex, cEq, obs); };
  template <class U, class V, class W>
    Bool addConstraint(uInt nIndex, const W &cEqIndex,
		       const V &cEq,
		       const std::complex<U> &obs) {
    return LSQFit::addConstraint(nIndex, cEqIndex, cEq, obs); };
  // </group>

  // Solve normal equations.
  // The solution will be given in <src>sol</src>, with the
  // adjustment error <src>mu</src>, and the standard
  // deviation <swrc>sd</src>. I.e. <src>mu</src> is per unit weight,
  // <src>sd</src> per observation. In the cases where the solution is
  // returned in a Vector, the Array given as input will be resized and
  // reshaped (if necessary), to the properly sized Vector.
  // <group>
  template <class U>
    void solve(U sol[]) { LSQFit::solve(sol); };
  template <class U>
    void solve(std::complex<U> sol[]) { LSQFit::solve(sol); };
  template <class U>
    void solve(Vector<U> &sol) {
    sol.resize(n_p); LSQFit::solve(sol.data()); };
  template <class U>
    void solve(Vector<std::complex<U> > &sol) {
    sol.resize(n_p); LSQFit::solve(sol.data()); };
  // </group>
  // Solve a Levenberg-Marquardt loop. Note that the solution <src>sol</src>
  // is used both and input and output. No check on the size is done.
  // <group>
  template <class U>
    Bool solveLoop(Double &fit, uInt &nRank,
		   U sol[], Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol, doSVD); };
  template <class U>
    Bool solveLoop(Double &fit, uInt &nRank,
		   std::complex<U> sol[],
		   Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol, doSVD); };
  template <class U>
    Bool solveLoop(Double &fit, uInt &nRank,
		   Vector<U> &sol,
		   Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol.data(), doSVD); };
  template <class U>
    Bool solveLoop(Double &fit, uInt &nRank,
		   Vector<std::complex<U> > &sol,
		   Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol, doSVD); };
  // </group>
  // Get the covariance matrix. False if an error occurred
  // (of size <src>nUnknowns * nUnknowns</src>)
  // <group>
  template <class U>
    Bool getCovariance(U *covar) {
    return LSQFit::getCovariance(covar); };
  template <class U>
    Bool getCovariance(Array<U> &covar);
  // </group>  

 private:

  //# Data

};

#endif
