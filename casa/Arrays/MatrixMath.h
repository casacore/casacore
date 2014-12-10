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

#ifndef CASA_MATRIXMATH_H
#define CASA_MATRIXMATH_H


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

// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/MatrixMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

