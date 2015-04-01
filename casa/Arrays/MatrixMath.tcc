//# MatrixMath.tcc: The Casacore linear algebra functions
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

#ifndef CASA_MATRIXMATH_TCC
#define CASA_MATRIXMATH_TCC

#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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

template <class T>
T crossProduct2D (const Vector<T> &A, const Vector<T> &B) {
                                      // check for correct dimensions
  if (!A.conform(B)){
    throw (ArrayConformanceError("crossProduct2D - conform() error."));
  } else {
    if (A.nelements() != 2) 
      throw (ArrayConformanceError("crossProduct2D - Vector not in 2-space"));
  }
  return A[0]* B[1] - A[1]*B[0];
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
  uInt nx=x.nelements(), ny=y.nelements();
  Vector<T> res(nx*ny);
  for (uInt i=0; i<nx*ny; i++) {
      res(i) = x(i/ny) * y(i%ny);
  }
  return res;
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

} //# NAMESPACE CASACORE - END


#endif
