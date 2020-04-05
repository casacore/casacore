//# Matrix2Math.cc: The Casacore linear algebra functions (non-templated)
//# Copyright (C) 1993,1994,1995,1996,1999,2002
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

#include "Vector.h"
#include "Matrix.h"
#include "MatrixMath.h"
#include "ArrayLogical.h"
#include "ArrayMath.h"
#include "ArrayError.h"

#include <complex>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  Matrix<double> Rot3D(int axis, double angle) 
{
  if (axis<0 || axis>2) 
    throw (ArrayError("Rot3D(axis, angle): axis has to be 0 (x),"
		      " 1 (y) or 2 (z)."));
  
  Matrix<double> Rot(3,3);
  Rot=0;
  double cosa=std::cos(angle); 
  double sina=std::sin(angle);
  
  Rot(axis,axis)=1;
  Rot((axis+1)%3,(axis+1)%3)=cosa;
  Rot((axis+2)%3,(axis+1)%3)=sina;
  Rot((axis+1)%3,(axis+2)%3)=-sina;
  Rot((axis+2)%3,(axis+2)%3)=cosa;
  return Rot;
}

Matrix<float> Rot3D(int axis, float angle) 
{
  Matrix<double> tmp = Rot3D(axis, double(angle));

  Matrix<float> retval(tmp.shape());
  convertArray(retval, tmp);

  return retval;
}

// In order to create a specific std::complex<float> and Dcomplex function (with the
// little nuances associated with complex space) this file carries some
// non-template copies of the templated Matrix math stuff.

// the vector dot/scalar/inner product
std::complex<float> innerProduct (const Vector<std::complex<float>> &A, const Vector<std::complex<float>> &B) {
  // check for correct dimensions
  if (!(A.conform(B)))
    throw (ArrayConformanceError("innerProduct - conform() error."));
  std::complex<float> scalar = 0;
  for (size_t i = 0; i < A.nelements(); i++)
    scalar += A(i)*conj(B(i));
  return scalar;
}
 
Matrix<std::complex<float>> conjugate(const Matrix<std::complex<float>> &A)
{
  return conj(A);
}

Matrix<std::complex<float>> adjoint(const Matrix<std::complex<float>> &A)
{
  return transpose(Matrix<std::complex<float>>(conj(A)));
}

// ----------------std::complex<double> subroutines---------------------------

// the vector dot/scalar/inner product
std::complex<double> innerProduct (const Vector<std::complex<double>> &A, const Vector<std::complex<double>> &B) 
{
  // check for correct dimensions
  if (!(A.conform(B)))
    throw (ArrayConformanceError("innerProduct - conform() error."));
  std::complex<double> scalar = 0;
  for (size_t i = 0; i < A.nelements(); i++)
    scalar += A(i)*conj(B(i));
  return scalar;
}
 

Matrix<std::complex<double>> conjugate(const Matrix<std::complex<double>> &A)
{
  return conj(A);
}

Matrix<std::complex<double>> adjoint (const Matrix<std::complex<double>> &A)
{
  return transpose(Matrix<std::complex<double>>(conj(A)));
}

// int Vector magnitude/norm
int norm (const Vector<int> &A) 
{
  return int(std::sqrt(double(innerProduct(A,A))));
}

// float Vector magnitude/norm
float norm (const Vector<float> &A) 
{
  return std::sqrt(innerProduct(A,A));
}

// double Vector magnitude/norm
double norm (const Vector<double> &A) 
{
  return std::sqrt(innerProduct(A,A));
}

// std::complex<float> vector magnitude/norm
float norm (const Vector<std::complex<float>> &A) 
{
  return std::sqrt(real(innerProduct(A,A)));
}

// std::complex<double> vector magnitude/norm
double norm (const Vector<std::complex<double>> &A) 
{
  return std::sqrt(real(innerProduct(A,A)));
}

// The infinity norm of a matrix
int normI (const Matrix<int> &A)
{
  int output = 0;
  if( A.nelements()!=0) {
    int hold = 0;
    size_t M = A.nrow();
    for (size_t I = 0; I < M; I++)
      {
	hold = sum(abs(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The infinity norm of a matrix
float normI (const Matrix<float> &A)
{
  float output = 0;
  if( A.nelements()!=0) {
    float hold = 0;
    size_t M = A.nrow();
    for (size_t I = 0; I < M; I++)
      {
	hold = sum(abs(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The infinity norm of a matrix
double normI (const Matrix<double> &A)
{
  double output = 0;
  if( A.nelements()!=0) {
    double hold = 0;
    size_t M = A.nrow();
    for (size_t I = 0; I < M; I++)
      {
	hold = sum(abs(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}
                                    
// the infinite norm of a matrix
float normI (const Matrix<std::complex<float>> &A)
{
  float output = 0;
  if( A.nelements()!=0) {
    float hold = 0;
    size_t M = A.nrow();
    for (size_t I = 0; I < M; I++)
      {
	hold = sum(amplitude(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// the infinite norm of a matrix
double normI (const Matrix<std::complex<double>> &A)
{
  double output = 0;
  if( A.nelements()!=0) {
    double hold = 0;
    size_t M = A.nrow();
    for (size_t I = 0; I < M; I++)
      {
	hold = sum(amplitude(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
int norm1 (const Matrix<int> &A)
{
  int output = 0;
  if (A.nelements()!=0) {
    int hold = 0;
    size_t N = A.ncolumn();
    for (size_t I = 0; I < N; I++)
      {
	hold = sum(abs(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
float norm1 (const Matrix<float> &A)
{
  float output = 0;
  if (A.nelements()!=0) {
    float hold = 0;
    size_t N = A.ncolumn();
    for (size_t I = 0; I < N; I++)
      {
	hold = sum(abs(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
double norm1 (const Matrix<double> &A)
{
  double output = 0;
  if (A.nelements()!=0) {
    double hold = 0;
    size_t N = A.ncolumn();
    for (size_t I = 0; I < N; I++)
      {
	hold = sum(abs(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
float norm1 (const Matrix<std::complex<float>> &A)
{
  float output = 0;
  if( A.nelements()!=0) {
    float hold = 0;
    size_t N = A.ncolumn();
    for (size_t I = 0; I < N; I++)
      {
	hold = sum(amplitude(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
double norm1 (const Matrix<std::complex<double>> &A)
{
  double output = 0;
  if( A.nelements()!=0) {
    double hold = 0;
    size_t N = A.ncolumn();
    for (size_t I = 0; I < N; I++)
      {
	hold = sum(amplitude(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

Matrix<float> rproduct (const Matrix<std::complex<float>> &A, const Matrix<std::complex<float>> &B) {
  if (A.ncolumn() != B.nrow())
    throw (ArrayError("product - multiplication of" 
		      " these matrices shapes is undefined"));
  Matrix<float> result(A.nrow(), B.ncolumn());
  for (size_t i = 0; i < A.nrow(); i++) 
    for (size_t j = 0; j < B.ncolumn(); j++) {
      result(i,j) = 0.0;
      for (size_t k = 0; k < A.ncolumn(); k++) result(i,j) += 
		      real(A(i, k) * B(k, j));
    }
  return result;
}

Vector<float> rproduct(const Matrix<std::complex<float>> &A, const Vector<std::complex<float>> &x) {
  if (A.ncolumn() != x.nelements())
    throw (ArrayError("product - multiplication of" 
		      " these matrices shapes is undefined"));
  Vector<float> result(A.nrow());
  for (size_t i = 0; i < A.nrow(); i++) {
    result(i) = 0.0;
    for (size_t k = 0; k < A.ncolumn(); k++) result(i) += 
		    real(A(i, k) * x(k));
  }
  return result;
}

Vector<std::complex<float>> product(const Matrix<std::complex<float>> &A, const Vector<float> &x) {
  if (A.ncolumn() != x.nelements())
    throw (ArrayError("product - multiplication of" 
		      " these matrices shapes is undefined"));
  Vector<std::complex<float>> result(A.nrow());
  for (size_t i = 0; i < A.nrow(); i++) {
    result(i) = std::complex<float>(0);
    for (size_t k = 0; k < A.ncolumn(); k++) result(i) += 
		    A(i, k) * x(k);
  }
  return result;
}

Matrix<int> adjoint (const Matrix<int> &A){
  return transpose(A);}
Matrix<float> adjoint (const Matrix<float> &A){
  return transpose(A);}
Matrix<double> adjoint (const Matrix<double> &A){
  return transpose(A);}

} //# NAMESPACE CASACORE - END

