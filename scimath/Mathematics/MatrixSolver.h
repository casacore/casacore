//# MatrixSolver.h: the base class for solvers of AX=B
//# Copyright (C) 1994,1995,1999
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

#ifndef SCIMATH_MATRIXSOLVER_H
#define SCIMATH_MATRIXSOLVER_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>

#include <casacore/casa/Logging/LogSink.h>
#include <casacore/casa/Logging/LogMessage.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

typedef Float FType;  // floating type (Float, Double)

//<summary>
// MatrixSolver.h: the base class for solvers of linear equations AX=B
//</summary>

// <use visibility=local>

// <reviewed reviewer="" date="",tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Matrix, Vector
// </prerequisite>
//
// <etymology>
// The MatrixSolver class name reflects its use as the base class for solving
// Linear Equations of the form AX=B. This class is purely virtual
// and provides the essential implementation for derived solvers
// classes.  
// </etymology>
//
// <synopsis> 
// The MatrixSolver class is a purely virtual base class.  The programmer needs
// to define the following functions in a class derived from MatrixSolver:
// <ol>
//  <li> the derived destructor.
//  <li> <src>void setImageAndPsf(const Array<FType> & image, const 
//  Array<FType> & psf);</src> Set the image and the Point Spread Function 
//  (beam).  Setting this should reset the internal state, e.g. 
//  CurrentIter()==0.
//  <li> <src>Bool solve();</src>  Perform solution of AX=B.
//       Returns True if algorithm has converged or stop criterium reached.
// </ol>
// </synopsis> 
//
// <todo asof="">
// </todo>

class MatrixSolver {
public:

  // Default Constructor
  MatrixSolver();
  
  // Copy Constructor
  MatrixSolver(const MatrixSolver & other);
  
  // Create a MatrixSolver from a matrix A and a Vector B
  // <note role=warning> A and B are accessed by reference, so do not 
  // modify them during the lifetime of the MatrixSolver </note>
  MatrixSolver(const Matrix<FType> & A, const Vector<FType> & B);
  
  // Virtual destructor: calls all derived class destructors
  virtual ~MatrixSolver();
  
  // Assignment operator: uses reference semantics, i.e., it 
  // references the internal arrays of other
  MatrixSolver & operator=(const MatrixSolver & other);

  // Set A matrix and B vector
  void setAB(const Matrix<FType> & A, const Vector<FType> & B);

  // Set initial value of X
  void setX(const Vector<FType> & X);
  
  // Solve for the X vector.
  virtual Bool solve();

  // Is the current solution good enough?
  Bool accurateSolution();

  // Return residual vector B-AX
  const Vector<FType> & getResidual();
  
  // Return solution vector
  const Vector<FType> & getSolution();

  // Set the tolerance for solution
  void setTolerance(FType tol);

  // Return the tolerance for solution
  FType Tolerance();
  
  // Set the maximum number of iterations.
  void setMaxIters(uInt maxiters);
  
  // Return the maximum number of iterations.
  uInt MaxIters();

  // Set the gain for solution
  void setGain(FType g);

  // Return the gain for solution
  FType Gain();

  // Set status of solution
  void setSolved(Bool s);

  // Return status of solution
  Bool Solved();

  // Return norm of solution i.e. ||B-AX||
  FType getNorm();
  
protected:

  LogSink logSink_p;
  virtual LogSink& logSink() {return logSink_p;}

  // the A matrix data member
  Matrix<FType> AMatrix;

  // the constraint vector data member
  Vector<FType> BVector;

  // The residual vector data member
  Vector<FType> RVector;

  // The solution vector data member
  Vector<FType> XVector;

  // The solution norm i.e. ||B-AX||
  FType RNorm;

  // The data norm i.e. ||B||
  FType BNorm;

private:

  // Tolerance for solution i.e. ||B-AX||/||B|| must be less than this
  FType SolTolerance; 
 
  // Maximum number of iterations
  uInt MaxIterations;

  // Has a solution been found?
  Bool solved;

  // Gain
  FType gain;

};

inline void MatrixSolver::setTolerance(FType tol) 
{SolTolerance=tol;}

inline FType MatrixSolver::Tolerance() 
{return SolTolerance;}

inline void MatrixSolver::setMaxIters(uInt maxiters) 
{MaxIterations = maxiters;}

inline uInt MatrixSolver::MaxIters() 
{return MaxIterations;}

inline void MatrixSolver::setGain(FType g) 
{gain=g;}

inline FType MatrixSolver::Gain() 
{return gain;}

inline void MatrixSolver::setSolved(Bool s) 
{solved=s;}

inline Bool MatrixSolver::Solved() 
{return solved;}

inline FType MatrixSolver::getNorm()
{return RNorm;}


} //# NAMESPACE CASACORE - END

#endif
