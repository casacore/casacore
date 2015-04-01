//# SquareMatrix.h: Fast Square Matrix class with fixed (templated) size
//# Copyright (C) 1996,1997,1999,2001
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
//#
//# $Id$
 
#ifndef SCIMATH_SQUAREMATRIX_H
#define SCIMATH_SQUAREMATRIX_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations
template <class T, Int n> class RigidVector;

// <summary>
// Fast Square Matrix class with fixed (templated) size
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// <prerequisite>
//   <li> Complex
//   <li> Matrix
// </prerequisite>
//
// <etymology>
// SquareMatrix is a specialized class for small (<5x5) square matrices.
// </etymology>
//
// <synopsis>
// SquareMatrix provides operations similar to the Matrix class, but it is
// much faster for small arrays. One important difference is that operators *=
// and * do matrix products for SquareMatrices instead of element by element
// multiplication. SquareMatrices also optimize operations internally for 
// scalar identity matrices (diagonal matrix with all elements equal) and
// diagonal matrices. The different types of SquareMatrix are created by
// constructors and operator= taking either a scalar, a vector or a full
// matrix.
// </synopsis>
//
// <example>
// <srcblock>
// // create two SquareMatrices
// SquareMatrix<Float,2> sm1(3.0); // a scalar identity matrix
// Vector<Float> vec(2); vec(0)=2.0; vec(1)=3.0;
// SquareMatrix<Float,2> sm2(vec); // a diagonal matrix
// // multiply the matrices
// // Note: A*=B is equivalent to A=A*B where '*' is matrix multiplication
// sm1*=sm2; // sm1 now diagonal
// </srcblock>
// </example>
//
// <motivation>
// The basic Matrix classes are rather inefficient for small sizes,
// new and delete tend to dominate the execution time for computationally
// intensive code. The SquareMatrix classes circumvent this by having a
// compile-time fixed size c-array internally. The SquareMatrix class have
// fixed zero origin and no increments, this allows fast indexing,
// copying and math operations. As mentioned in the synopsis, the SquareMatrix
// classes also avoid unnecessary operations for simple matrices 
// (scalar-identity and diagonal).
// </motivation>
//
// <templating arg=T>
//    <li> real() is called for T=Complex/DComplex
// </templating>
//
// <thrown>
//    <li> No exceptions
// </thrown>
//
// <todo asof="1996/11/06">
//   <li> when the Sun native compiler improves, some explicit instantiations
//        can be replaced by their templated equivalent and two constructor
//        calls with for loops can be moved out of line.
//   <li> not all operators and math functions available for Matrix are
//        implemented yet, add on as-needed basis.
// </todo>
 
template <class T, Int n> class SquareMatrix {
    // Friends currently need to be explicit (non templated) type to work.
    friend class RigidVector<T,n>;
    //# friend class SquareMatrix<Complex,n>; // for real()
    //    friend class SquareMatrix<T,n*n>;// Sun native does not accept this
    //    friend class SquareMatrix<Complex,4>; // for directProduct of 2x2
    // Global friend function for product of Complex matrix and Float 4-vector
    friend RigidVector<Complex,4> operator*(const SquareMatrix<Complex,4>& m,
    const RigidVector<Float,4>& v);
    // Global friend function to calculate direct product
    friend SquareMatrix<Complex,4>& 
	directProduct(SquareMatrix<Complex,4>& result,
		      const SquareMatrix<Complex,2>& left, 
		      const SquareMatrix<Complex,2>& right);
public:
    // Enum used internally to optimize operations.
    enum {General, Diagonal, ScalarId};
    // Destructor
    ~SquareMatrix() {}
    // Default constructor - creates a unity matrix at present, this may not
    // be what we want (non-intuitive?)
    SquareMatrix() : type_p(ScalarId) {a_p[0][0]=T(1);}
    // Create a matrix of a given type, no initialization
    SquareMatrix(int itype) : type_p(itype) {}
    // Copy construct a SquareMatrix, a true copy is made.
    SquareMatrix(const SquareMatrix<T,n>& m) {operator=(m);}
    // Construct from c-style matrix (by copying elements).
    SquareMatrix(const T a[n][n]) {operator=(a);}
    // Construct from Matrix.
    SquareMatrix(const Matrix<T>& mat) {operator=(mat);}
    // Construct from c-style vector, creates a diagonal matrix.
    SquareMatrix(const T vec[n]){operator=(vec);}
    // Construct from Vector, creates a diagonal matrix.
    SquareMatrix(const Vector<T>& vec) {operator=(vec);}
    // Construct from scalar, creates a scalar-identity matrix
    SquareMatrix(const T& scalar) : type_p(ScalarId) { a_p[0][0]=scalar; }
    // Assignment, uses copy semantics.
    SquareMatrix<T,n>& operator=(const SquareMatrix<T,n>& m);
    // Assign a c-style matrix, creates a general matrix.
    SquareMatrix<T,n>& operator=(const T a[n][n]) {
	type_p=General;
	const T* pa=&a[0][0];
	T* pa_p=&a_p[0][0];
	for (Int i=0; i<n*n; i++) *pa_p++=*pa++;
	return *this;
    }	
    // Assign a Matrix, creates a general matrix.
    SquareMatrix<T,n>& operator=(const Matrix<T>& m);
    // Assign a c-style vector, creates a diagonal matrix
    SquareMatrix<T,n>& operator=(const T vec[n]) {
	type_p=Diagonal;
	for (Int i=0; i<n; i++) a_p[i][i]=vec[i];
	return *this;
    } 
    // Assign a Vector, creates a diagonal matrix
    SquareMatrix<T,n>& operator=(const Vector<T>& v);
    // Assign a scalar, creates a scalar-identity matrix
    SquareMatrix<T,n>& operator=(T val) {
	type_p=ScalarId; a_p[0][0]=val; return *this;
    }
    // Add two SquareMatrices, element by element.
    SquareMatrix<T,n>& operator+=(const SquareMatrix<T,n>& other);
    // Matrix product of 'this' SquareMatrix with other,
    // i.e., A*=B; is equivalent with A=A*B where '*' is matrix multiplication.
    SquareMatrix<T,n>& operator*=(const SquareMatrix<T,n>& other);
    // Scalar multiplication
    SquareMatrix<T,n>& operator*=(Float f);
    // Indexing, only const indexing is allowed. You cannot change the
    // matrix via indexing. No bounds checking.
    T operator()(Int i, Int j) const { 
      switch (type_p) {
      case ScalarId: return (i==j) ? a_p[0][0] : T();
	break;
      case Diagonal: return (i==j) ? a_p[i][i] : T();
	break;
      }
      return a_p[i][j];
    }
    // Non const indexing, throws exception if you try to change an element
    // which would require a type change of the matrix
    T& operator()(Int i, Int j) {
      switch (type_p) {
      case ScalarId: return (i==j) ? a_p[0][0] : throwInvAccess();
	break;
      case Diagonal: return (i==j) ? a_p[i][i] : throwInvAccess();
	break;
      }
      return a_p[i][j];
    }

    //# The following does not compile with Sun native, replaced by explicit
    //# global function.
    //# direct product : dp= this (directproduct) other
    //#    SquareMatrix<T,n*n>& 
    //#	directProduct(SquareMatrix<T,n*n>& dp, 
    //#		      const SquareMatrix<T,n>& other) const;
    // For a <src>SquareMatrix<Complex,n></src>: 
    // set the argument result to the real part of the matrix 
    // (and return result by reference to allow use in
    // expressions without creating temporary).
    //# SquareMatrix<Float,n>& real(SquareMatrix<Float,n>& result) const;
    // For a <src>SquareMatrix<Complex,n></src>: 
    // return the real part of the matrix.
    //# SquareMatrix<Float,n> real() const {
    //# SquareMatrix<Float,n> result;
    //# return real(result);
    //# }
    // Conjugate the matrix in place(!).
    SquareMatrix<T,n>& conj();
    // Tranpose and conjugate the matrix in place(!).
    SquareMatrix<T,n>& adjoint(); 
    // Conjugate the matrix, return it in result (and by ref)
    SquareMatrix<T,n>& conj(SquareMatrix<T,n>& result);
    // Tranpose and conjugate the matrix, return it in result (and by ref)
    SquareMatrix<T,n>& adjoint(SquareMatrix<T,n>& result);
    // Compute the inverse of the matrix and return it in result (also
    // returns result by reference).
    SquareMatrix<T,n>& inverse(SquareMatrix<T,n>& result) const;
    // Return the inverse of the matrix by value.
    SquareMatrix<T,n> inverse() const
	{ SquareMatrix<T,n> result; return inverse(result);}
    // Assign 'this' to the Matrix result, also return result by reference.
    Matrix<T>& matrix(Matrix<T>& result) const;
    // Convert the SquareMatrix to a Matrix.
    Matrix<T> matrix() const
	{ Matrix<T> result(n,n); return matrix(result);}

private:
    T& throwInvAccess();
    T a_p[n][n];
    Int type_p;
};


//# the following does not compile with Sun native but should...
//# expanded by hand for types and sizes needed
//#template<class T, Int n> 
//#ostream& operator<<(ostream& os, const SquareMatrix<T,n>& m) {
//#   return os<<m.matrix();
//#}
//#
//#template<class T, Int n> inline SquareMatrix<T,n> operator+(const SquareMatrix<T,n>& left, 
//#						   const SquareMatrix<T,n>& right) {
//#    SquareMatrix<T,n> result(left);
//#    return result+=right;
//#}
//#template<class T, Int n> inline SquareMatrix<T,n> operator*(const SquareMatrix<T,n>& left, 
//#						   const SquareMatrix<T,n>& right) 
//#{
//#    SquareMatrix<T,n> result(left);
//#    return result*=right;
//#}
//#
//#template<class T, Int n> inline SquareMatrix<T,n*n> directProduct(const SquareMatrix<T,n>& left, 
//#							 const SquareMatrix<T,n>& right)
//#{
//#    SquareMatrix<T,n*n> result;
//#    return left.directProduct(result,right);
//#}
//#
//#template<class T, Int n> inline SquareMatrix<T,n*n>& 
//#directProduct(SquareMatrix<T,n*n>& result,
//#	      const SquareMatrix<T,n>& left, 
//#	      const SquareMatrix<T,n>& right)
//#{
//#    return left.directProduct(result,right);
//#}
//#template<class T, Int n> inline SquareMatrix<T,n> conj(
//#    const SquareMatrix<T,n>& m) {
//#    SquareMatrix<T,n> result(m);
//#    return result.conj();
//#}
//#
//#template<class T, Int n> inline SquareMatrix<T,n> adjoint(
//#    const SquareMatrix<T,n>& m) {
//#    SquareMatrix<T,n> result(m);
//#    return result.adjoint();
//#}
//#

// <summary>
// Various global math and IO functions.
// </summary>
// <group name=SqM_global_functions>

// Calculate direct product of two SquareMatrices.
SquareMatrix<Complex,4> 
directProduct(const SquareMatrix<Complex,2>& left, 
	      const SquareMatrix<Complex,2>& right);

// Return conjugate of SquareMatrix.
SquareMatrix<Complex,2> conj(const SquareMatrix<Complex,2>& m);

// Return conjugate of SquareMatrix.
SquareMatrix<Complex,4> conj(const SquareMatrix<Complex,4>& m);

// Return adjoint of SquareMatrix.
SquareMatrix<Complex,2> adjoint(const SquareMatrix<Complex,2>& m);

// Return adjoint of SquareMatrix.
SquareMatrix<Complex,4> adjoint(const SquareMatrix<Complex,4>& m);

// Write SquareMatrix to output, uses Matrix to do the work.
ostream& operator<<(ostream& os, const SquareMatrix<Complex,2>& m);
ostream& operator<<(ostream& os, const SquareMatrix<Complex,4>& m);
ostream& operator<<(ostream& os, const SquareMatrix<Float,2>& m);
ostream& operator<<(ostream& os, const SquareMatrix<Float,4>& m);
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/SquareMatrix.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
