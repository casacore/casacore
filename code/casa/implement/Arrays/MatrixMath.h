//# MatrixMath.h: The AIPS++ linear algebra functions
//# Copyright (C) 1994,1995,1996
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

#if !defined(AIPS_MATRIXMATH_H)
#define AIPS_MATRIXMATH_H

#if defined(_AIX)
#pragma implementation ("MatrixMath.cc")
#pragma implementation ("Matrix2Math.cc")
#endif

#include <aips/aips_exit.h>
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Complex.h>


//<summary>
//    Linear algebra functions on Vectors and Matrices.
// </summary>
//
// <reviewed reviewer="" date="" tests="tLinAlgebra">
//
// <linkfrom anchor="Linear Algebra" classes="Vector Matrix">
//    <here>Linear Algebra</here> -- Linear algebra functions
//     on Vectors and Matrices.
// </linkfrom>
//
//<group name="Linear Algebra">

//
// The scalar/dot/inner product of two equal length vectors.
//
//<group>
template <class T> T innerProduct (const Vector<T> &x, const Vector<T> &y);
Complex innerProduct (const Vector<Complex> &x, const Vector<Complex> &y);
DComplex innerProduct (const Vector<DComplex> &x, const Vector<DComplex> &y);
//</group>

//
// The magnitude/norm of a vector.
//<group>
Int norm (const Vector<Int> &x);
Float norm (const Vector<Float> &x);
Double norm (const Vector<Double> &x);
Float norm (const Vector<Complex> &x);
Double norm (const Vector<DComplex> &x);
//</group>

//
// The vector/cross product of two 3-space vectors.
//
template <class T> 
   Vector<T> crossProduct (const Vector<T> &x, const Vector<T> &y);

//
// The matrix/outer product of a vector and a transposed vector. 
// <note> The function's second argument is actually a transposed vector
// stored as the only row in a 1xN matrix. </note>
//
template <class T>
   Matrix<T> product (const Vector<T> &x, const Matrix<T> &yT);

//
// The matrix/outer product of a vector and a transposed vector. 
// <note> The function's second argument is actually a transposed vector
// stored as the only row in a 1xN matrix. </note> 
// <note role=warning> This is deprecated - use "product(Vector, Matrix)" 
// </note>
//
template <class T>
   Matrix<T> outerProduct (const Vector<T> &x, const Matrix<T> &yT);

//
// The vector/outer product of an MxN matrix and an N-length vector.
//
template <class T>
   Vector<T> product (const Matrix<T> &A, const Vector<T> &x);

//
// The vector/outer product of an MxN matrix and an N-length vector.
// <note role=warning> This is deprecated - use "product(Matrix, Vector)" 
// </note>
template <class T>
   Vector<T> outerProduct (const Matrix<T> &A, const Vector<T> &x);

// 
// The direct product of two vectors.
// The resulting vector contains for every element of x, the product of
// that element and Vector y. Thus the length of the output vector is
// the product of the input lengths.
//
template <class T> 
   Vector<T> directProduct(const Vector<T>& x, const Vector<T>& y);

//
// The matrix multiplication or cayley product of an MxN matrix and
// an NxP matrix.
//
template <class T> 
   Matrix<T> product (const Matrix<T> &A, const Matrix<T> &B);

//
// The matrix multiplication or cayley product of an MxN matrix and
// an NxP matrix.
// <note role=warning> This is deprecated - use "product(Matrix, Matrix)"
// </note>
template <class T> 
   Matrix<T> cayleyProduct (const Matrix<T> &A, const Matrix<T> &B);

//
// The infinity norm (or maximum value of the sum of the absolute values 
// of the rows members of a matrix)
// <group>
Int normI(const Matrix<Int> &A);
Float normI(const Matrix<Float> &A);
Double normI(const Matrix<Double> &A);
Float normI(const Matrix<Complex> &A);
Double normI(const Matrix<DComplex> &A);
// </group>

//
// The one norm (or maximum value of the sum of the absolute values 
// of the column members of a matrix)
//<group>
Int norm1(const Matrix<Int> &A);
Float norm1(const Matrix<Float> &A);
Double norm1(const Matrix<Double> &A);
Float norm1(const Matrix<Complex> &A);
Double norm1(const Matrix<DComplex> &A);
//</group>

//
// The NxM transpose of an MxN matrix.
//
template <class T> Matrix<T> transpose (const Matrix<T> &A);

// Create a 3D rotation matrix (3x3).
// Axis is 0,1,2 for x,y,z; angle is in radians.
// <group>
template <class T> Matrix<T> Rot3D(Int axis, T angle);
Matrix<Double> Rot3D(Int axis, Double angle);
Matrix<Float> Rot3D(Int axis, Float angle);
// </group>

// 
// The direct product of two matrices.
// The resulting matrix contains for every element of A, the product of
// that element and Matrix B. Thus the shape of the output matrix is
// the (element by element) product of the input shapes.
//
template <class T> 
   Matrix<T> directProduct(const Matrix<T>& A, const Matrix<T>& B);

//
// The complex conjugate of the complex matrix A.
//
Matrix<Complex> conjugate (const Matrix<Complex> &A);

//
// The complex conjugate of the double precision complex matrix A.
//
Matrix<DComplex> conjugate (const Matrix<DComplex> &A);

//
// The conjugate/transpose or adjoint of the complex matrix A.
//
Matrix<Complex> adjoint (const Matrix<Complex> &A);
Matrix<DComplex> adjoint (const Matrix<DComplex> &A);

// define the adjoint operator as a plain old transpose when the Matrix is 
// not complex valued. (for templating purposes)
Matrix<Int> adjoint (const Matrix<Int> &A);
Matrix<Float> adjoint (const Matrix<Float> &A);
Matrix<Double> adjoint (const Matrix<Double> &A);

//
// The product of a Complex Matrix and a Real Vector
//
  Vector<Complex> product(const Matrix<Complex>&, const Vector<Float>&);

//
// The real part of a product of a Complex Matrix and a Complex Vector
//
  Vector<Float> rproduct(const Matrix<Complex>&, const Vector<Complex>&);

//
// The real part of a product of a Complex Matrix and a Complex Matrix
//
  Matrix<Float> rproduct (const Matrix<Complex>&, const Matrix<Complex>&);

// Routines which calculate the inverse of a matrix.  The inverse is very
// often the worst way to do a calculation.  Nevertheless it is often
// convenient. The present implementation uses LU decomposition implemented
// by LAPACK. The determinate can be calculated "for free" as it is the
// product of the diagonal terms after decomposition.  If the input matrix is
// singular, a matrix with no rows or columns is returned.  <src>in</src>
// must be a square matrix.  
// <note role="warning">This function will only work for complex types if
// Complex and DComplex map onto their FORTRAN counterparts.</note>
//# We could special case small matrices for efficiency. 
//<group>
template<class T> void invert(Matrix<T> & out, T& determinate, 
			      const Matrix<T> &in);
template<class T> Matrix<T> invert(const Matrix<T> &in);
template<class T> T determinate(const Matrix<T> &in);
//</group>

// This function inverts a symmetric positive definite matrix.  It is
// written in C++, so it should work with any data type for which
// operators +, -, *, /, =, and sqrt are defined.  The function uses
// the Cholesky decomposition method to invert the matrix.  Cholesky
// decomposition is about a factor of 2 better than LU decomposition
// where symmetry is ignored. 
template<class T> void invertSymPosDef(Matrix<T> & out, T& determinate, 
				       const Matrix<T> &in);
template<class T> Matrix<T> invertSymPosDef(const Matrix<T> &in);

//</group>

//# These are actually used by invertSymPosDef. They will not
//# normally be called by the end user.

//# This function performs Cholesky decomposition.
//# A is a positive-definite symmetric matrix. Only the upper triangle of
//# A is needed on input. On output, the lower triangle of A contains the
//# Cholesky factor L.  The diagonal elements of L are returned in vector
//# diag.
template<class T> void CholeskyDecomp(Matrix<T> &A, Vector<T> &diag);

//# Solve linear equation A*x = b, where A positive-definite symmetric.
//# On input, A contains Cholesky factor L in its low triangle except the
//# diagonal elements which are in vector diag.  On return x contains the
//# solution.  b and x can be the same vector to save memory space.
template<class T> void CholeskySolve(Matrix<T> &A, Vector<T> &diag, 
				     Vector<T> &b, Vector<T> &x);


//# These are the LAPACK routines actually used by invert. They will not
//# normally be called by the end user.

#define NEED_FORTRAN_UNDERSCORES

#if defined(NEED_FORTRAN_UNDERSCORES)
#define sgetrf sgetrf_
#define dgetrf dgetrf_
#define cgetrf cgetrf_
#define zgetrf zgetrf_
#define sgetri sgetri_
#define dgetri dgetri_
#define cgetri cgetri_
#define zgetri zgetri_
#endif

extern "C" {
      void sgetrf(const int *m, const int *n, float *a, const int *lda,
		  int *ipiv, int *info);
      void dgetrf(const int *m, const int *n, double *a, const int *lda,
		  int *ipiv, int *info);
      void cgetrf(const int *m, const int *n, Complex *a, const int *lda,
		  int *ipiv, int *info);
      void zgetrf(const int *m, const int *n, DComplex *a, const int *lda,
		  int *ipiv, int *info);
      void sgetri(const int *m, float *a, const int *lda, const int *ipiv,
		  float *work, const int *lwork, int *info);
      void dgetri(const int *m, double *a, const int *lda, const int *ipiv,
		  double *work, const int *lwork, int *info);
      void cgetri(const int *m, Complex *a, const int *lda, const int *ipiv,
		  Complex *work, const int *lwork, int *info);
      void zgetri(const int *m, DComplex *a, const int *lda, const int *ipiv,
		  DComplex *work, const int *lwork, int *info);
};

//# Overloaded versions of the above to make templating work more easily
inline void getrf(const int *m, const int *n, float *a, const int *lda,
		  int *ipiv, int *info)
   { sgetrf(m, n, a, lda, ipiv, info); }
inline void getrf(const int *m, const int *n, double *a, const int *lda,
		  int *ipiv, int *info)
   { dgetrf(m, n, a, lda, ipiv, info); }
inline void getrf(const int *m, const int *n, Complex *a, const int *lda,
		  int *ipiv, int *info)
   { cgetrf(m, n, a, lda, ipiv, info); }
inline void getrf(const int *m, const int *n, DComplex *a, const int *lda,
		  int *ipiv, int *info)
   { zgetrf(m, n, a, lda, ipiv, info); }
inline void getri(const int *m, float *a, const int *lda, const int *ipiv,
		  float *work, const int *lwork, int *info)
   { sgetri(m, a, lda, ipiv, work, lwork, info); }
inline void getri(const int *m, double *a, const int *lda, const int *ipiv,
		  double *work, const int *lwork, int *info)
   { dgetri(m, a, lda, ipiv, work, lwork, info); }
inline void getri(const int *m, Complex *a, const int *lda, const int *ipiv,
		  Complex *work, const int *lwork, int *info)
   { cgetri(m, a, lda, ipiv, work, lwork, info); }
inline void getri(const int *m, DComplex *a, const int *lda, const int *ipiv,
		  DComplex *work, const int *lwork, int *info)
   { zgetri(m, a, lda, ipiv, work, lwork, info); }


#endif

