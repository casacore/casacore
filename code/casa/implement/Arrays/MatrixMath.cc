//# MatrixMath.cc: The AIPS++ linear algebra functions
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

#include <aips/Mathematics/Complex.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/stdio.h>
#include <aips/math.h>

#include <aips/Arrays/ArrayIO.h>
                                      // the vector dot/scalar/inner product
template<class T> T innerProduct (const Vector<T> &A, const Vector<T> &B) {
                                   // check for correct dimensions
  if (A.conform(B) == False){
    throw(ArrayConformanceError("innerProduct - conform() error."));
  }
  T scalar = 0;   
  for (uInt i=0; i < A.nelements(); i++)
    scalar += A(i)*B(i);
  return scalar;
}

// Uncomment this if we ever want a templated Rot3D again.
// template <class T> Matrix<T> Rot3D(Int axis, T angle) {
//     if (axis<0 || axis>2) 
// 	throw (ArrayError("Rot3D(axis, angle): axis has to be 0 (x),"
// 			  " 1 (y) or 2 (z)."));
//    
// 	Matrix<T> Rot(3,3);
// 	Rot=0;
// 	T cosa=cos(angle); 
// 	T sina=sin(angle);
//
// 	Rot(axis,axis)=1;
// 	Rot((axis+1)%3,(axis+1)%3)=cosa;
// 	Rot((axis+2)%3,(axis+1)%3)=sina;
// 	Rot((axis+1)%3,(axis+2)%3)=-sina;
// 	Rot((axis+2)%3,(axis+2)%3)=cosa;
// 	return Rot;
// }

                                  // the 3-space cross/vector product
template <class T>
Vector<T> crossProduct (const Vector<T> &A, const Vector<T> &B) {
                                      // check for correct dimensions
  if (!A.conform(B)){
    throw (ArrayConformanceError("crossProduct - conform() error."));
  } else {
    if (A.nelements() != 3) 
      throw (ArrayConformanceError("crossProduct - Vector not in 3-space"));
  }
  Vector<T> result(3);
  result(0) = A(1)*B(2) - A(2)*B(1);
  result(1) = A(2)*B(0) - A(0)*B(2);
  result(2) = A(0)*B(1) - A(1)*B(0);
  return result;
}

                                 // matrix multiplication or cayley product
template <class T>
Vector<T> product (const Matrix<T> &A, const Vector<T> &x) {
  if (A.ncolumn() != x.nelements())
    throw (ArrayError("product - multiplication of" 
                                    " these matrices shapes is undefined"));
  Vector<T> result(A.nrow());
  for (uInt i = 0; i < A.nrow(); i++) {
      result(i) = T(0);
      for (uInt k = 0; k < A.ncolumn(); k++) result(i) += A(i,k) * x(k);
  }
  return result;
}


template <class T> 
Vector<T> directProduct(const Vector<T>& x, const Vector<T>& y) 
{
  Int nx=x.nelements(), ny=y.nelements();
  Vector<T> res(nx*ny);
  for (uInt i=0; i<nx*ny; i++) {
      res(i) = x(i/ny) * y(i%ny);
  }
  return res;
}


template <class T>
Vector<T> outerProduct (const Matrix<T> &A, const Vector<T> &x) {
  static int count = 0;
  if(count==0) 
    cerr << "outerProduct(Matrix, Vector) is deprecated; use product(...)\n";
  return product(A,x);
}

template <class T> 
Matrix<T> product (const Vector<T> &x, const Matrix<T> &yT) {
  if (yT.nrow()!= 1) 
    throw (ArrayError("product - multiplication of" 
                                    " these matrices shapes is undefined"));
  Matrix<T> A(x.nelements(),1u);
  A.column(0) = x;

  return product(A,yT);
}

template <class T>
Matrix<T> outerProduct (const Vector<T> &x, const Matrix<T> &yT) {
  static int count = 0;
  if(count==0) 
    cerr << "outerProduct(Vector, Matrix) is deprecated; use product(...)\n";
  return product(x, yT);
}

                                 // matrix multiplication or cayley product
template <class T> 
Matrix<T> product (const Matrix<T> &A, const Matrix<T> &B) {
  if (A.ncolumn() != B.nrow())
    throw (ArrayError("product - multiplication of" 
                                    " these matrices shapes is undefined"));
  Matrix<T> result(A.nrow(), B.ncolumn());
  for (uInt i = 0; i < A.nrow(); i++) 
      for (uInt j = 0; j < B.ncolumn(); j++) {
	  result(i,j) = T(0);
	  for (uInt k = 0; k < A.ncolumn(); k++) result(i,j) += A(i,k) * B(k,j);
      }
  return result;
}

template <class T>
Matrix<T> cayleyProduct (const Matrix<T> &A, const Matrix<T> &B) {
  static int count = 0;
  if(count==0) 
    cerr << "cayleyProduct(Matrix, Matrix) is deprecated; use product(...)\n";
  return product(A,B);
}

template <class T> Matrix<T> transpose (const Matrix<T> &A) {
  Matrix<T> aT(A.ncolumn(), A.nrow());
  for (uInt i=0; i<A.nrow(); i++)
    for (uInt j=0; j<A.ncolumn(); j++) aT(j,i) = A(i,j);
  return aT;
}

template <class T> 
Matrix<T> directProduct(const  Matrix<T> &A, const Matrix<T> &B) {
    Int ncB = B.ncolumn(), nrB = B.nrow();
    Matrix<T> dpAB(A.ncolumn()*B.ncolumn(),A.nrow()*B.nrow());
    for (uInt i=0; i<dpAB.ncolumn(); i++) {
	for (uInt j=0; j<dpAB.nrow(); j++) {
	    dpAB(i,j) = A(i/ncB,j/nrB)*B(i%ncB,j%nrB);
	}
    }
    return dpAB;
}

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
	  throw(AipsError("invertSymPosDef(const Matrix<T> &in): Matrix is"
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
