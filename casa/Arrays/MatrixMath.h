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

#ifndef CASA_MATRIXMATH_2_H
#define CASA_MATRIXMATH_2_H

#include "Vector.h"
#include "Matrix.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//<summary>
//    Linear algebra functions on Vectors and Matrices.
// </summary>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLinAlgebra">
//</reviewed>
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
std::complex<float> innerProduct (const Vector<std::complex<float>> &x, const Vector<std::complex<float>> &y);
std::complex<double> innerProduct (const Vector<std::complex<double>> &x, const Vector<std::complex<double>> &y);
//</group>

//
// The magnitude/norm of a vector.
//<group>
int norm (const Vector<int> &x);
float norm (const Vector<float> &x);
double norm (const Vector<double> &x);
float norm (const Vector<std::complex<float>> &x);
double norm (const Vector<std::complex<double>> &x);
//</group>

//
// The vector/cross product of two 3-space vectors.
//
template <class T> 
   Vector<T> crossProduct (const Vector<T> &x, const Vector<T> &y);

// Magnitude of cross product of two 2-space vectors, x[0]*y[1] - x[1]*y[0]. 
template <class T> T crossProduct2D(const Vector<T> &x, const Vector<T> &y);
//
// The matrix/outer product of a vector and a transposed vector. 
// <note> The function's second argument is actually a transposed vector
// stored as the only row in a 1xN matrix. </note>
//
template <class T>
   Matrix<T> product (const Vector<T> &x, const Matrix<T> &yT);

//
// The vector/outer product of an MxN matrix and an N-length vector.
//
template <class T>
   Vector<T> product (const Matrix<T> &A, const Vector<T> &x);

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
// The infinity norm (or maximum value of the sum of the absolute values 
// of the rows members of a matrix)
// <group>
int normI(const Matrix<int> &A);
float normI(const Matrix<float> &A);
double normI(const Matrix<double> &A);
float normI(const Matrix<std::complex<float>> &A);
double normI(const Matrix<std::complex<double>> &A);
// </group>

//
// The one norm (or maximum value of the sum of the absolute values 
// of the column members of a matrix)
//<group>
int norm1(const Matrix<int> &A);
float norm1(const Matrix<float> &A);
double norm1(const Matrix<double> &A);
float norm1(const Matrix<std::complex<float>> &A);
double norm1(const Matrix<std::complex<double>> &A);
//</group>

//
// The NxM transpose of an MxN matrix.
//
template <class T> Matrix<T> transpose (const Matrix<T> &A);

// Create a 3D rotation matrix (3x3).
// Axis is 0,1,2 for x,y,z; angle is in radians.
// <group>
template <class T> Matrix<T> Rot3D(int axis, T angle);
Matrix<double> Rot3D(int axis, double angle);
Matrix<float> Rot3D(int axis, float angle);
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
// The conjugate/transpose or adjoint of the complex matrix A.
//
Matrix<std::complex<float>> adjoint (const Matrix<std::complex<float>> &A);
Matrix<std::complex<double>> adjoint (const Matrix<std::complex<double>> &A);

// define the adjoint operator as a plain old transpose when the Matrix is 
// not complex valued. (for templating purposes)
Matrix<int> adjoint (const Matrix<int> &A);
Matrix<float> adjoint (const Matrix<float> &A);
Matrix<double> adjoint (const Matrix<double> &A);

//
// The product of a std::complex<float> Matrix and a Real Vector
//
  Vector<std::complex<float>> product(const Matrix<std::complex<float>>&, const Vector<float>&);

//
// The real part of a product of a std::complex<float> Matrix and a std::complex<float> Vector
//
  Vector<float> rproduct(const Matrix<std::complex<float>>&, const Vector<std::complex<float>>&);

//
// The real part of a product of a std::complex<float> Matrix and a std::complex<float> Matrix
//
  Matrix<float> rproduct (const Matrix<std::complex<float>>&, const Matrix<std::complex<float>>&);

// </group>


} //# NAMESPACE CASACORE - END

#include "MatrixMath.tcc"

#endif

