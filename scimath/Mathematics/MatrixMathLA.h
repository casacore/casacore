//# MatrixMath.h: The Casacore linear algebra functions
//# Copyright (C) 1994,1995,1996,1999,2000,2002
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

#ifndef SCIMATH_MATRIXMATHLA_H
#define SCIMATH_MATRIXMATHLA_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicSL/Complex.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//<summary>
//    Linear algebra functions on Vectors and Matrices.
// </summary>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLinAlgebra">
//
// <linkfrom anchor="Linear Algebra" classes="Vector Matrix">
//    <here>Linear Algebra</here> -- Linear algebra functions
//     on Vectors and Matrices.
// </linkfrom>
//
//<group name="Linear Algebra">

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

#if !defined(NEED_FORTRAN_UNDERSCORES)
#define NEED_FORTRAN_UNDERSCORES 1
#endif

#if NEED_FORTRAN_UNDERSCORES
#define sgetrf sgetrf_
#define dgetrf dgetrf_
#define cgetrf cgetrf_
#define zgetrf zgetrf_
#define sgetri sgetri_
#define dgetri dgetri_
#define cgetri cgetri_
#define zgetri zgetri_
#define sposv sposv_
#define dposv dposv_
#define cposv cposv_
#define zposv zposv_
#define spotri spotri_
#define dpotri dpotri_
#define cpotri cpotri_
#define zpotri zpotri_
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


      void sposv(const char *uplo, const int *n, const int* nrhs, float *a, 
		 const int *lda, float *b, const int *ldb, int *info);
      void dposv(const char *uplo, const int *n, const int* nrhs, double *a, 
		 const int *lda, double *b, const int *ldb, int *info);
      void cposv(const char *uplo, const int *n, const int* nrhs, Complex *a, 
		 const int *lda, Complex *b, const int *ldb, int *info);
      void zposv(const char *uplo, const int *n, const int* nrhs, DComplex *a, 
		 const int *lda, DComplex *b, const int *ldb, int *info);


      void spotri(const char *uplo, const int *n, float *a, 
		  const int *lda, int *info);
      void dpotri(const char *uplo, const int *n, double *a, 
 		  const int *lda, int *info);
      void cpotri(const char *uplo, const int *n, Complex *a, 
		  const int *lda, int *info);
      void zpotri(const char *uplo, const int *n, DComplex *a, 
		  const int *lda, int *info);

}

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

inline void posv(const char *uplo, const int *n, const int* nrhs, float *a, 
		 const int *lda, float *b, const int *ldb, int *info)
   { sposv(uplo, n, nrhs, a, lda, b, ldb, info); }  
inline void posv(const char *uplo, const int *n, const int* nrhs, double *a, 
		 const int *lda, double *b, const int *ldb, int *info)
   { dposv(uplo, n, nrhs, a, lda, b, ldb, info); }  
inline void posv(const char *uplo, const int *n, const int* nrhs, Complex *a, 
		 const int *lda, Complex *b, const int *ldb, int *info)
   { cposv(uplo, n, nrhs, a, lda, b, ldb, info); }  
inline void posv(const char *uplo, const int *n, const int* nrhs, DComplex *a, 
		 const int *lda, DComplex *b, const int *ldb, int *info)
   { zposv(uplo, n, nrhs, a, lda, b, ldb, info); }  

inline void potri(const char *uplo, const int *n, float *a, 
		  const int *lda, int *info)
   { spotri(uplo, n, a, lda, info); }  
inline void potri(const char *uplo, const int *n, double *a, 
		  const int *lda, int *info)
   { dpotri(uplo, n, a, lda, info); }  
inline void potri(const char *uplo, const int *n, Complex *a, 
		  const int *lda, int *info)
   { cpotri(uplo, n, a, lda, info); }  
inline void potri(const char *uplo, const int *n, DComplex *a, 
		  const int *lda, int *info)
   { zpotri(uplo, n, a, lda, info); }  



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/MatrixMathLA.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
