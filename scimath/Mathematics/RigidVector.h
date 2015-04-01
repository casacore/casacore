//# RigidVector.h: Fast Vector classes with fixed (templated) length
//# Copyright (C) 1996,1999,2001
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
 
#ifndef SCIMATH_RIGIDVECTOR_H
#define SCIMATH_RIGIDVECTOR_H
  
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward
template <class T, Int n> class SquareMatrix;
// <summary> Fast Vector classes with fixed (templated) length </summary>
 
// <use visibility=export>
 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
 
// <prerequisite>
//   <li> Vector
//   <li> Complex
// </prerequisite>
//
// <etymology>
// RigidVector is a vector with a size fixed at compile time, i.e. Rigid
// as compared to the normal Vector class.
// </etymology>
//
// <synopsis>
// RigidVector is a specialized Vector class for short (<25 elements) vectors.
// It has a size fixed at compile time, avoids new and delete and uses 
// copy semantics throughout. 
// Unlike Vectors, RigidVectors have fixed zero origin and no strides,
// allowing fast indexing.
// The more common mathematical operations are defined for RigidVector, 
// allowing element by element arithmetic, innerproduct and matrix
// multiplication (by a SquareMatrix). Conversion to and from normal
// vectors is provided.
// </synopsis>
//
// <example>
// <srcblock> 
// // Create two RigidVectors
// RigidVector<Float,3> rv1(1.0,2.0,3.0),rv2(3.0,2.0,1.0);
// // Compute sum
// RigidVector<Float,3> rv3=rv1+rv2;
// // Compute innerproduct
// Float inprod=rv1*rv2;
// // Write out results
// cout << "rv1+rv2="<< rv3 <<", rv1*rv2="<< inprod<<endl;
// </srcblock> 
// </example>
//
// <motivation>
// The standard Vector class is rather inefficient for short vectors, this
// class is designed for speed and simplicity.
// </motivation>
//
// <templating arg=T>
//    <li> this class is meant for computation and assumes operators
//          +,-,* to be defined.
// </templating>
//
// <thrown>
//    <li> no exceptions
// </thrown>
//
// <todo asof="1996/11/07">
//   <li> not all operations defined for Vectors are defined 
//   <li> default implementation of innerProduct is wrong for Complex vectors
// </todo>

template <class T, Int n> class RigidVector {

    //# friends (could be out of line if compiler accepted that)
    // Add two RigidVectors.
    friend RigidVector<T,n> operator+(const RigidVector<T,n>& l,
    const RigidVector<T,n>& r) {
	RigidVector<T,n> result=l;
	return result+=r;
    }
    // Subtract two RigidVectors.
    friend RigidVector<T,n> operator-(const RigidVector<T,n>& l,
    const RigidVector<T,n>& r) {
	RigidVector<T,n> result=l;
	return result-=r;
    }
    // The innerproduct of 2 RigidVectors.
    friend T operator*(const RigidVector<T,n>& l,
		       const RigidVector<T,n>& r) {
	T sum=T(0);
	for (Int i=0; i<n; i++) sum+=l.v_p[i]*r.v_p[i];
	return sum;
    }
    // Multiply a RigidVector by a scalar.
    friend RigidVector<T,n> operator*(const T& f, const RigidVector<T,n>& v) {
	RigidVector<T,n> r(v);
	return r*=f;
    }
    // Multiply a RigidVector by a scalar.
    friend RigidVector<T,n> operator*(const RigidVector<T,n>& v, const T& f) {
	RigidVector<T,n> r(v);
	return r*=f;
    }
    // Write out a RigidVector using the Vector output method.
    friend ostream& operator<<(ostream& os, const RigidVector<T,n>& v) {
	os << v.vector();
	return os;
    }
    // Special matrix multiply of Complex matrix * Float vector.
    friend RigidVector<Complex,4> operator*(const SquareMatrix<Complex,4>& m,
					    const RigidVector<Float,4>& v);
public:
  //    RigidVector(Int dummy) {
  //      for (Int i=0; i<n; i++) v_p[i]=T(0);
  //    }
    // Default constructor
    RigidVector() {
      for (Int i=0; i<n; i++) v_p[i]=T(0);
    }
    // Construct from scalar, sets all elements to c
    RigidVector(const T& c) {
	for (Int i=0; i<n; i++) v_p[i]=c;
    }
    // Construct a 2-element vector, fails for wrong size vectors.
    RigidVector(const T& v0, const T& v1) {
	if (n!=2) exit(1);
	v_p[0]=v0; v_p[1]=v1;
    }
    // Construct a 3-element vector, fails for wrong size vectors.
    RigidVector(const T& v0, const T& v1, const T& v2) {
	if (n!=3) exit(1);
	v_p[0]=v0; v_p[1]=v1; v_p[2]=v2;
    }
    // Construct a 4-element vector, fails for wrong size vectors.
    RigidVector(const T& v0, const T& v1, const T& v2, const T& v3) {
	if (n!=4) exit(1);
	v_p[0]=v0; v_p[1]=v1; v_p[2]=v2; v_p[3]=v3;
    }
    // Construct a 5-element vector, fails for wrong size vectors.
    RigidVector(const T& v0, const T& v1, const T& v2, const T& v3,
                const T& v4) {
        if (n!=5) exit(1);
        v_p[0]=v0; v_p[1]=v1; v_p[2]=v2; v_p[3]=v3; v_p[4]=v4;
    }
    // Construct a 6-element vector, fails for wrong size vectors.
    RigidVector(const T& v0, const T& v1, const T& v2, const T& v3,
                const T& v4, const T& v5) {
        if (n!=6) exit(1);
        v_p[0]=v0; v_p[1]=v1; v_p[2]=v2; v_p[3]=v3; v_p[4]=v4; v_p[5]=v5;
    }
    // Construct from a c-array (copy semantics)
    RigidVector(const T v[n]) {
	for (Int i=0; i<n; i++) v_p[i]=v[i];
    }
    // Construct from a Vector.
    RigidVector(const Vector<T> & v) {
	for (Int i=0; i<n; i++) v_p[i]=v(i);
    }
    // Copy constructor, copy semantics.
    RigidVector(const RigidVector<T,n>& v) {
	for (Int i=0; i<n; i++) v_p[i]=v.v_p[i];
    }
    // Assign from a RigidVector.
    RigidVector<T,n>& operator=(const RigidVector<T,n>& v) {
	for (Int i=0; i<n; i++) v_p[i]=v.v_p[i];
	return *this;
    }
    // Assign from a Vector.
    RigidVector<T,n>& operator=(const Vector<T>& v) {
	for (Int i=0; i<n; i++) v_p[i]=v(i);
	return *this;
    }
    // Assign a scalar, sets all elements to c.
    RigidVector<T,n>& operator=(const T& c) {
	for (Int i=0; i<n; i++) v_p[i]=c;
	return *this;
    }
    // Negation
    RigidVector<T,n>& operator-() {
	for (Int i=0; i<n ;i++) v_p[i]=-v_p[i];
	return *this;
    }
    // Addition 
    RigidVector<T,n>& operator+=(const RigidVector<T,n>& v) {
	for (Int i=0; i<n; i++) v_p[i]+=v.v_p[i];
	return *this;
    }
    RigidVector<T,n>& operator*=(const RigidVector<T,n>& v) {
	for (Int i=0; i<n; i++) v_p[i]*=v.v_p[i];
	return *this;
    }
    // Subtraction
    RigidVector<T,n>& operator-=(const RigidVector<T,n>& v) {
	for (Int i=0; i<n; i++) v_p[i]-=v.v_p[i];
	return *this;
    }
    // Multiplication by scalar.
    RigidVector<T,n>& operator*=(const T& val) {
	for (Int i=0; i<n; i++) v_p[i]*=val;
	return *this;
    }
    // Multiply vector by matrix: v*=M is equivalent to v=M*v;
    RigidVector<T,n>& operator*=(const SquareMatrix<T,n>& m); 

    // Indexing by reference
    T& operator()(Int i) { return v_p[i];}
    // Indexing by const reference
    const T& operator()(Int i) const { return v_p[i];}
    //# Get const access to the underlying c-array
    //#const T*& cArray() const { return v_p;}
    // Convert to a regular Vector
    Vector<T> vector() const {
	Vector<T> v(n);
	for (Int i=0; i<n; i++) v(i)=v_p[i];
	return v;
    }
    // Square Root
    RigidVector<T,n> sqrt(const RigidVector<T,n>& v);

// // The following are needed for Image<RigidVector>

//   static IPosition shape() {return IPosition(1,n);}

//   static void* newCopyInfo (const TableRecord& record,
// 			    const IPosition& sourceElementShape);

//   static void deleteCopyInfo (void*);

//   static void set (void* copyInfo, void* out,
// 		   const Array<T>& in,
// 		   const IPosition& shape);
//   static void get (void* copyInfo, Array<T>& out,
// 		   const void* in,
// 		   const IPosition& shape);

protected:
    T v_p[n];
};

// <summary> Mathematical operations involving RigidVectors </summary>

// <group name=math>
//#Fails to compile
//#// Multiply vector by matrix.
//#template <class T, Int n>
//#inline RigidVector<T,n> operator*(const SquareMatrix<T,n>& m,
//#				  const RigidVector<T,n>& v) {
//#    RigidVector<T,n> result(v);
//#    return result*=m;
//#}
// Multiply vector by matrix.
inline RigidVector<Float,4> operator*(const SquareMatrix<Float,4>& m,
				      const RigidVector<Float,4>& v) {
    RigidVector<Float,4> result(v);
    return result*=m;
}
// Multiply vector by matrix.
inline RigidVector<Complex,4> operator*(const SquareMatrix<Complex,4>& m,
					const RigidVector<Complex,4>& v) {
    RigidVector<Complex,4> result(v);
    return result*=m;
}
// </group>

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/RigidVector.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
