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

#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Matrix<Double> Rot3D(Int axis, Double angle) 
{
  if (axis<0 || axis>2) 
    throw (ArrayError("Rot3D(axis, angle): axis has to be 0 (x),"
		      " 1 (y) or 2 (z)."));
  
  Matrix<Double> Rot(3,3);
  Rot=0;
  Double cosa=std::cos(angle); 
  Double sina=std::sin(angle);
  
  Rot(axis,axis)=1;
  Rot((axis+1)%3,(axis+1)%3)=cosa;
  Rot((axis+2)%3,(axis+1)%3)=sina;
  Rot((axis+1)%3,(axis+2)%3)=-sina;
  Rot((axis+2)%3,(axis+2)%3)=cosa;
  return Rot;
}

Matrix<Float> Rot3D(Int axis, Float angle) 
{
  Matrix<Double> tmp = Rot3D(axis, Double(angle));

  Matrix<Float> retval(tmp.shape());
  convertArray(retval, tmp);

  return retval;
}

// In order to create a specific Complex and Dcomplex function (with the
// little nuances associated with complex space) this file carries some
// non-template copies of the templated Matrix math stuff.

// the vector dot/scalar/inner product
Complex innerProduct (const Vector<Complex> &A, const Vector<Complex> &B) {
  // check for correct dimensions
  if (!(A.conform(B)))
    throw (ArrayConformanceError("innerProduct - conform() error."));
  Complex scalar = 0;
  for (uInt i = 0; i < A.nelements(); i++)
    scalar += A(i)*conj(B(i));
  return scalar;
}
 
Matrix<Complex> conjugate(const Matrix<Complex> &A)
{
  cout << "MatrixMath::conjugate is deprecated, use ArrayMath::conj." << endl;
  return conj(A);
}

Matrix<Complex> adjoint(const Matrix<Complex> &A)
{
  return transpose(Matrix<Complex>(conj(A)));
}

// ----------------DComplex subroutines---------------------------

// the vector dot/scalar/inner product
DComplex innerProduct (const Vector<DComplex> &A, const Vector<DComplex> &B) 
{
  // check for correct dimensions
  if (!(A.conform(B)))
    throw (ArrayConformanceError("innerProduct - conform() error."));
  DComplex scalar = 0;
  for (uInt i = 0; i < A.nelements(); i++)
    scalar += A(i)*conj(B(i));
  return scalar;
}
 

Matrix<DComplex> conjugate(const Matrix<DComplex> &A)
{
  cout << "MatrixMath::conjugate is deprecated, use ArrayMath::conj." << endl;
  return conj(A);
}

Matrix<DComplex> adjoint (const Matrix<DComplex> &A)
{
  return transpose(Matrix<DComplex>(conj(A)));
}

// Int Vector magnitude/norm
Int norm (const Vector<Int> &A) 
{
  return Int(std::sqrt(Double(innerProduct(A,A))));
}

// Float Vector magnitude/norm
Float norm (const Vector<Float> &A) 
{
  return std::sqrt(innerProduct(A,A));
}

// Double Vector magnitude/norm
Double norm (const Vector<Double> &A) 
{
  return std::sqrt(innerProduct(A,A));
}

// Complex vector magnitude/norm
Float norm (const Vector<Complex> &A) 
{
  return std::sqrt(real(innerProduct(A,A)));
}

// DComplex vector magnitude/norm
Double norm (const Vector<DComplex> &A) 
{
  return std::sqrt(real(innerProduct(A,A)));
}

// The infinity norm of a matrix
Int normI (const Matrix<Int> &A)
{
  Int output = 0;
  if( A.nelements()!=0) {
    Int hold = 0;
    uInt M = A.nrow();
    for (uInt I = 0; I < M; I++)
      {
	hold = sum(abs(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The infinity norm of a matrix
Float normI (const Matrix<Float> &A)
{
  Float output = 0;
  if( A.nelements()!=0) {
    Float hold = 0;
    uInt M = A.nrow();
    for (uInt I = 0; I < M; I++)
      {
	hold = sum(abs(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The infinity norm of a matrix
Double normI (const Matrix<Double> &A)
{
  Double output = 0;
  if( A.nelements()!=0) {
    Double hold = 0;
    uInt M = A.nrow();
    for (uInt I = 0; I < M; I++)
      {
	hold = sum(abs(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}
                                    
// the infinite norm of a matrix
Float normI (const Matrix<Complex> &A)
{
  Float output = 0;
  if( A.nelements()!=0) {
    Float hold = 0;
    uInt M = A.nrow();
    for (uInt I = 0; I < M; I++)
      {
	hold = sum(amplitude(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// the infinite norm of a matrix
Double normI (const Matrix<DComplex> &A)
{
  Double output = 0;
  if( A.nelements()!=0) {
    double hold = 0;
    uInt M = A.nrow();
    for (uInt I = 0; I < M; I++)
      {
	hold = sum(amplitude(A.row(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
Int norm1 (const Matrix<Int> &A)
{
  Int output = 0;
  if (A.nelements()!=0) {
    Int hold = 0;
    uInt N = A.ncolumn();
    for (uInt I = 0; I < N; I++)
      {
	hold = sum(abs(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
Float norm1 (const Matrix<Float> &A)
{
  Float output = 0;
  if (A.nelements()!=0) {
    Float hold = 0;
    uInt N = A.ncolumn();
    for (uInt I = 0; I < N; I++)
      {
	hold = sum(abs(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
Double norm1 (const Matrix<Double> &A)
{
  Double output = 0;
  if (A.nelements()!=0) {
    Double hold = 0;
    uInt N = A.ncolumn();
    for (uInt I = 0; I < N; I++)
      {
	hold = sum(abs(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
Float norm1 (const Matrix<Complex> &A)
{
  Float output = 0;
  if( A.nelements()!=0) {
    Float hold = 0;
    uInt N = A.ncolumn();
    for (uInt I = 0; I < N; I++)
      {
	hold = sum(amplitude(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

// The one norm of a matrix
Double norm1 (const Matrix<DComplex> &A)
{
  double output = 0;
  if( A.nelements()!=0) {
    double hold = 0;
    uInt N = A.ncolumn();
    for (uInt I = 0; I < N; I++)
      {
	hold = sum(amplitude(A.column(I)));
	output = (output >= hold) ? output : hold;
      }
  }
  return output;
}

Matrix<Float> rproduct (const Matrix<Complex> &A, const Matrix<Complex> &B) {
  if (A.ncolumn() != B.nrow())
    throw (ArrayError("product - multiplication of" 
		      " these matrices shapes is undefined"));
  Matrix<Float> result(A.nrow(), B.ncolumn());
  for (uInt i = 0; i < A.nrow(); i++) 
    for (uInt j = 0; j < B.ncolumn(); j++) {
      result(i,j) = 0.0;
      for (uInt k = 0; k < A.ncolumn(); k++) result(i,j) += 
		      real(A(i, k) * B(k, j));
    }
  return result;
}

Vector<Float> rproduct(const Matrix<Complex> &A, const Vector<Complex> &x) {
  if (A.ncolumn() != x.nelements())
    throw (ArrayError("product - multiplication of" 
		      " these matrices shapes is undefined"));
  Vector<Float> result(A.nrow());
  for (uInt i = 0; i < A.nrow(); i++) {
    result(i) = 0.0;
    for (uInt k = 0; k < A.ncolumn(); k++) result(i) += 
		    real(A(i, k) * x(k));
  }
  return result;
}

Vector<Complex> product(const Matrix<Complex> &A, const Vector<Float> &x) {
  if (A.ncolumn() != x.nelements())
    throw (ArrayError("product - multiplication of" 
		      " these matrices shapes is undefined"));
  Vector<Complex> result(A.nrow());
  for (uInt i = 0; i < A.nrow(); i++) {
    result(i) = Complex(0);
    for (uInt k = 0; k < A.ncolumn(); k++) result(i) += 
		    A(i, k) * x(k);
  }
  return result;
}

Matrix<Int> adjoint (const Matrix<Int> &A){
  return transpose(A);}
Matrix<Float> adjoint (const Matrix<Float> &A){
  return transpose(A);}
Matrix<Double> adjoint (const Matrix<Double> &A){
  return transpose(A);}

} //# NAMESPACE CASACORE - END

