//# MatrixMath.cc: The Casacore linear algebra functions
//# Copyright (C) 1994,1995,1996,1998,2001,2002
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

#ifndef SCIMATH_MATRIXMATHLA_TCC
#define SCIMATH_MATRIXMATHLA_TCC

#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
Matrix<T> invert(const Matrix<T> &in){
  Matrix<T> out;
  T det;
  invert(out, det, in);
  return out;
}

template<class T> 
T determinate(const Matrix<T> &in){
  Matrix<T> out;
  T det;
  invert(out, det, in);
  return det;
}

template<class T> 
void invert(Matrix<T> &out, T& det, const Matrix<T> &in)
{
  AlwaysAssert(in.nrow() == in.ncolumn(), AipsError);
  
  Int m = in.nrow();
  Int lda = m; Int n = m;               // m, n, lda
  
  out.resize(in.shape());
  out = in;
  Bool deleteIt;
  T *a = out.getStorage(deleteIt);      // a
  
  Block<Int> ipiv(m);                   // ipiv
  Int info;                             // info

  getrf(&m, &n, a, &lda, ipiv.storage(), &info);

  if (info == 0) { // LU decomposition worked! 
    // Calculate the determinate
    // It is just the product of the diagonal elements
    det = out(0,0);
    for (Int i = 1; i < n; i++)
      det *= out(i,i);

    // Calculate the inverse using back substitution
    Int lwork = 32 * n; // Lazy - we should really get this from ilaenv
    Block<T> work(lwork);
    getri(&m, a, &lda, ipiv.storage(), work.storage(), &lwork, &info);
  }
  out.putStorage(a, deleteIt); 
  AlwaysAssert(info >= 0, AipsError); // illegal argument to *getri or *getrf
  if (info > 0) {
    out.resize(0,0);
  }
}


template<class T> Matrix<T> invertSymPosDef(const Matrix<T> &in)
{
  Int i, j, k, n;
  n = in.nrow();

  Vector<T> diag(n);
  Vector<T> b(n);
  Matrix<T> tmp(n,n);
  Matrix<T> out(n,n);

  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      tmp(i,j) = in(i,j);
    }
  }

  // Cholesky decomposition: A = L*trans(L)
  CholeskyDecomp(tmp, diag);

  // Solve inverse of A by forward and backward substitution.  The right
  // hand side is a unit matrix, the solution is thus the inverse of A.
  for(j = 0; j < n; j++) {
    // one column at a time
    for(k = 0; k < n; k++) {
      b(k) = T(0.0);
    }
    b(j) = T(1.0);
    CholeskySolve(tmp, diag, b, b);
    for(k = 0; k < n; k++) {
      out(k,j) = b(k);
    }
  }
  return out;
}


template<class T> void invertSymPosDef(Matrix<T> & out, T& determinate, 
				       const Matrix<T> &in)
{
  // Resize out to match in
  out.resize(in.shape());

  Int i, j, k, n;
  n = in.nrow();

  Vector<T> diag(n);
  Vector<T> b(n);
  Matrix<T> tmp(n,n);

  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      tmp(i,j) = in(i,j);
    }
  }

  // Cholesky decomposition: A = L*trans(L)
  CholeskyDecomp(tmp, diag);
  

  // Is the following correct?
  determinate = diag(0)*diag(0);
  for(k = 1; k < n; k++) determinate = determinate*diag(k)*diag(k);

  // Solve inverse of A by forward and backward substitution.  The right
  // hand side is a unit matrix, the solution is thus the inverse of A.
  for(j = 0; j < n; j++) {
    // one column at a time
    for(k = 0; k < n; k++) {
      b(k) = T(0.0);
    }
    b(j) = T(1.0);
    CholeskySolve(tmp, diag, b, b);
    for(k = 0; k < n; k++) {
      out(k,j) = b(k);
    }
  }
}

template<class T> void CholeskyDecomp(Matrix<T> &A, Vector<T> &diag)
{
  // This function performs Cholesky decomposition.
  // A is a positive-definite symmetric matrix. Only the upper triangle of
  // A is needed on input. On output, the lower triangle of A contains the
  // Cholesky factor L.  The diagonal elements of L are returned in vector
  // diag.

  Int i, j, k, n;
  T sum;
  n = A.nrow();
  // Cholesky decompose A = L*trans(L)
  for(i = 0; i < n; i++) {
    for(j = i; j < n; j++) {
      sum = A(i,j);
      for(k = i-1; k >=0; k--) {
	sum = sum - A(i,k)*A(j,k);
      }
      if(i == j) {
	if(sum <= T(0.0)) {
	  throw(AipsError("CholeskyDecomp: Matrix is"
			  "not positive definite"));
	}
	diag(i) = sqrt(sum);
      } else {
	A(j,i) = sum/diag(i);
      }
    }
  }
}

template<class T> void CholeskySolve(Matrix<T> &A, Vector<T> &diag, 
				     Vector<T> &b, Vector<T> &x)
{
  // Solve linear equation A*x = b, where A positive-definite symmetric.
  // On input, A contains Cholesky factor L in its low triangle except the
  // diagonal elements which are in vector diag.  On return x contains the
  // solution.  b and x can be the same vector to save memory space.

  Int i, k, n;
  T sum;

  n = A.nrow();

  // Ensure solution vector has same length as input vector
  x.resize(b.shape());
        
  // solve by forward and backward substitution.  
  // L*y = b
  for(i = 0; i < n; i++) {
    sum = b(i);
    for(k = i-1; k >=0; k--) {
      sum = sum - A(i,k)*x(k);
    }
    x(i) = sum/diag(i);
  }
  // trans(L)*x = y
  for(i = n-1; i >= 0; i--) {
    sum = x(i);
    for(k = i+1; k < n; k++) {
	sum = sum - A(k,i)*x(k);
      }
    x(i) = sum/diag(i);
  }
}

} //# NAMESPACE CASACORE - END


#endif
