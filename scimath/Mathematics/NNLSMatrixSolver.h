//# NNLSMatrixSolver.h: the base class for NNLS solvers of AX=B
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

#ifndef SCIMATH_NNLSMATRIXSOLVER_H
#define SCIMATH_NNLSMATRIXSOLVER_H


#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/MatrixSolver.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//<summary>
// NNLSMatrixSolver.h: the base class for NNLS solvers of linear equations AX=B
//</summary>

// <use visibility=local>

// <reviewed reviewer="" date="",tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Matrix, Vector
// </prerequisite>
//
// <etymology>
// NNLS stands for Projection Onto Convex Sets. The idea is very simple: to
// find a solution to AX=B simply take the residual vector B-AX and operate
// on it to keep only the bits that obey some constraint e.g. positivity.
// Add this part to the current estimate of the solution vector and iterate.
// Both CLEAN and Gerchberg-Saxon are NNLS algorithms. If the projection 
// Operators are convex then the process is guaranteed to converge (Youla, 1970).
// </etymology>
//
// <synopsis> 
// NNLSMatrixSolver is a complete class. To use it, simply add Operators
// <ol>
// <li> I do not know how to do this yet but it should look something like
// <src>NNLSMatrixSolver NNLS(amatrix, bvector);NNLS.addOperator(foo);</src>
// </ol>
// </synopsis> 
//
// <todo asof="">
// <li> Add list of operators
// </todo>

class NNLSMatrixSolver : public MatrixSolver {
public:

  // Default Constructor
  NNLSMatrixSolver();
  
  // Copy Constructor
  NNLSMatrixSolver(const NNLSMatrixSolver & other);
  
  // Create a NNLSMatrixSolver from a matrix A and a Vector B
  // <note role=warning> A and B are accessed by reference, so don't 
  // modify them during the lifetime of the NNLSMatrixSolver </note>
  NNLSMatrixSolver(const Matrix<FType> & A, const Vector<FType> & B);
  
  // Destructor
  ~NNLSMatrixSolver();
  
  // Solve for the X vector.
  Bool solve();
  
protected:

private:

};


} //# NAMESPACE CASACORE - END

#endif
