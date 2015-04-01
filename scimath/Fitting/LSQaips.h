//# LSQaips.h: Interface for Casacore Vectors in least squares fitting
//# Copyright (C) 1999,2000,2001,2004,2006
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

#ifndef SCIMATH_LSQAIPS_H
#define SCIMATH_LSQAIPS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/scimath/Fitting/LSQFit.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary>
// Interface for Casacore Vectors in least squares fitting
// </summary>
// <reviewed reviewer="" date="2004/04/01" tests=""
//	 demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LSQFit>LSQFit</linkto> class
// </prerequisite>
//
// <etymology>
// From Least SQuares and aips++ (now Casacore)
// </etymology>
//
// <synopsis>
// The interface used in the <src>LSQaips</src> class is in terms of
// Casacore Vectors directly, rather than an STL iterator (like 
// <linkto class=VectorSTLIterator>VectorSTLIterator</linkto>) based
// on it.
//
// Its functionality is identical to that of the
// <linkto class=LSQFit>LSQFit</linkto> class, although it will be faster to use
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
  LSQaips(uInt nUnknowns, uInt nConstraints=0)
  : LSQFit(nUnknowns, nConstraints) {;}
  // Allow explicit complex/real specification
  // <group>
  LSQaips(uInt nUnknowns, const LSQReal &, uInt nConstraints=0)
  : LSQFit(nUnknowns, LSQReal(), nConstraints) {;}
  LSQaips(uInt nUnknowns, const LSQComplex &, uInt nConstraints=0)
  : LSQFit(nUnknowns, LSQComplex(), nConstraints) {;}
  // </group>
  // </group>
  // Default constructor (empty, real, only usable after a set(nUnknowns))
  LSQaips() : LSQFit() {;}
  // Copy constructor (deep copy)
  LSQaips(const LSQaips &other) : LSQFit(other) {;}
  // Assignment (deep copy)
  LSQaips &operator=(const LSQaips &other) {
  if (this != &other) LSQFit::operator=(other);
  return *this; }
  
  //# Destructor
  ~LSQaips() {;}
  
  //# Operators
  
  //# General Member Functions
  // Solve normal equations.
  // The solution will be given in <src>sol</src>.
  // <group>
  template <class U>
  void solve(U *sol) { LSQFit::solve(sol); }
  template <class U>
  void solve(std::complex<U> *sol) { LSQFit::solve(sol); }
  template <class U>
  void solve(U &sol) { LSQFit::solve(sol); }
  template <class U>
  void solve(Vector<U> &sol) {
    sol.resize(nUnknowns()/LSQTraits<U>::size);
    LSQFit::solve(sol.data()); }
  // </group>
  // Solve a Levenberg-Marquardt loop. Note that the solution <src>sol</src>
  // is used both and input and output. No check on the size is done.
  // <group>
  template <class U>
  Bool solveLoop(uInt &nRank,
		 U *sol, Bool doSVD=False) {
    return LSQFit::solveLoop(nRank, sol, doSVD); }
  template <class U>
  Bool solveLoop(uInt &nRank,
		 std::complex<U> *sol, Bool doSVD=False) {
    return LSQFit::solveLoop(nRank, sol, doSVD); }
  template <class U>
  Bool solveLoop(uInt &nRank,
		 U &sol, Bool doSVD=False) {
    return LSQFit::solveLoop(nRank, sol, doSVD); }
  template <class U>
  Bool solveLoop(uInt &nRank,
		 Vector<U> &sol, Bool doSVD=False);
  template <class U>
  Bool solveLoop(Double &fit, uInt &nRank,
		 U *sol, Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol, doSVD); }
  template <class U>
  Bool solveLoop(Double &fit, uInt &nRank,
		 std::complex<U> *sol, Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol, doSVD); }
  template <class U>
  Bool solveLoop(Double &fit, uInt &nRank,
		 U &sol, Bool doSVD=False) {
    return LSQFit::solveLoop(fit, nRank, sol, doSVD); }
  template <class U>
  Bool solveLoop(Double &fit, uInt &nRank,
		 Vector<U> &sol, Bool doSVD=False);
  // </group>
  // Get the covariance matrix. False if an error occurred
  // (of size <src>nUnknowns * nUnknowns</src>)
  // <group>
  template <class U>
  Bool getCovariance(U *covar) {
    return LSQFit::getCovariance(covar); }
  template <class U>
  Bool getCovariance(std::complex<U> *covar) {
    return LSQFit::getCovariance(covar); }
  template <class U>
  Bool getCovariance(Array<U> &covar);
  // </group>  
  // Get main diagonal of covariance function (of size <src>nUnknowns</src>)
  // <group>
  template <class U>
  Bool getErrors(U *errors) {
    return LSQFit::getErrors(errors); }
  template <class U>
  Bool getErrors(std::complex<U> *errors) {
    return LSQFit::getErrors(errors); }
  template <class U>
  Bool getErrors(U &errors) {
    return LSQFit::getErrors(errors); }
  template <class U>
  Bool getErrors(Vector<U> &errors) {
    errors.resize(nUnknowns()/LSQTraits<U>::size);
    return LSQFit::getErrors(errors.data()); }
  // </group>
  
private:

  //# Data

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/LSQaips.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
