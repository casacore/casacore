//# StokesVector.h: A fast RigidVector with Stokes conversions
//# Copyright (C) 1996,1999,2001,2003
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#ifndef MSVIS_STOKESVECTOR_H
#define MSVIS_STOKESVECTOR_H

#include <casa/aips.h>
#include <casa/IO/AipsIO.h>
//#include <tables/Tables/TableRecord.h>
#include <casa/BasicSL/Complex.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/MatrixMath.h>
#include <scimath/Mathematics/RigidVector.h>
#include <scimath/Mathematics/SquareMatrix.h>
#include <casa/Arrays/IPosition.h>
#include <casa/BasicMath/Math.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class StokesVector;
// <summary>
// Two specialized 4-vector classes for polarization handling
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// <prerequisite>
//   <li> RigidVector
//   <li> Stokes
// </prerequisite>
//
// <etymology>
// StokesVector and CStokesVector (Complex StokesVector) are two classes
// designed to handle a 4-vector of polarization values (I,Q,U,V or
// e.g., RR,RL,LR,LL).
// </etymology>
//
// <synopsis>
// StokesVectors are RigidVectors of length 4. They have a few special
// member functions to do polarization conversions efficiently.
// CStokesVector also has a real() member function to get at the real
// part of the components.
// </synopsis>
//
// <example>
// <srcblock>
// // Create a real valued I,Q,U,V StokesVector
// StokesVector pixel(4.0,2.0,1.0,0.1);
// // convert to a Complex valued vector of linear polarizations
// CStokesVector cohVec=applySlin(pixel);
// // convert back to I,Q,U,V
// cohVec.applySlinInv();
// // Write out the real part
// cout<< cohVec.real() <<endl;
// </srcblock>
// </example>
//
// <motivation>
// Full polarization processing of interferometry data uses real and complex
// valued 4-vectors. The StokesVector specialization handles this more 
// efficiently than a standard Vector of size 4.
// </motivation>
//
// <thrown>
//    <li> No exceptions
// </thrown>
//
// <todo asof="1996/11/07">
//   <li> we may want to add Stokesvector Eigenvalues
// </todo>


class CStokesVector:public RigidVector<Complex,4> {
public:
  static String dataTypeId() {return "CStokesVector";};
  //  CStokesVector(Int n):RigidVector<Complex,4>(n) {} 
  // The Complex data members are automatically initialized to 0.
  CStokesVector():RigidVector<Complex,4>() {} 
  // Construct from scalar, setting all values to a constant.
  CStokesVector(const Complex& c):RigidVector<Complex,4>(c) {}
  // Construct with four values specified.
  CStokesVector(const Complex& v0, const Complex& v1, 
		const Complex& v2, const Complex& v3):
    RigidVector<Complex,4>(v0,v1,v2,v3) {}
  // Construct from c-array
  CStokesVector(const Complex v[4]):RigidVector<Complex,4>(v) {}
  // Construct from Vector (should have length 4)
//  CStokesVector(const Vector<Complex> & v):RigidVector<Complex,4>(v) {}
  // Copy constructor with copy semantics.
  CStokesVector(const CStokesVector& v):RigidVector<Complex,4>(v){}
  // Construct from RigidVector
//  CStokesVector(const RigidVector<Complex,4>& v):RigidVector<Complex,4>(v) {}
  // Assignment
  CStokesVector& operator=(const CStokesVector& v) {
    RigidVector<Complex,4>::operator=(v); return *this;
  }
  // Assign from a Vector
  CStokesVector& operator=(const Vector<Complex>& v) {
    RigidVector<Complex,4>::operator=(v); return *this;
  }
  // Assign from a scalar, setting all values to a constant
  CStokesVector& operator=(const Complex& c) {
    RigidVector<Complex,4>::operator=(c); return *this;
  }
  // Negation
  CStokesVector& operator-() {
    RigidVector<Complex,4>::operator-(); return *this;
  }
  // Addition
  CStokesVector& operator+=(const CStokesVector& v) {
    RigidVector<Complex,4>::operator+=(v); return *this;
  }
  // Subtraction
  CStokesVector& operator-=(const CStokesVector& v) {
    RigidVector<Complex,4>::operator-=(v); return *this;
  }
  CStokesVector& operator*=(const CStokesVector& v) {
    RigidVector<Complex,4>::operator*=(v); return *this;
  }
  // Matrix multiplication - v*=m is equivalent to v=m*v
  CStokesVector& operator*=(const SquareMatrix<Complex,4>& m) {
    RigidVector<Complex,4>::operator*=(m); return *this;
  }
  CStokesVector& operator*=(Float f) {
    v_p[0]*=f; v_p[1]*=f; v_p[2]*=f; v_p[3]*=f; return *this;
  }
  // Equality
  Bool operator==(const CStokesVector& v) const {
    return (v_p[0]==v.v_p[0] && v_p[1]==v.v_p[1] &&
		  v_p[2]==v.v_p[2] && v_p[3]==v.v_p[3]);
  }
  // Inequality
  Bool operator!=(const CStokesVector& v) const {
    return (v_p[0]!=v.v_p[0] || v_p[1]!=v.v_p[1] ||
		  v_p[2]!=v.v_p[2] || v_p[3]!=v.v_p[3]);
  }

  // Apply conversion matrix from Stokes to linear, in place.
  CStokesVector& applySlin() {
    Complex i=v_p[0],q=v_p[1], u=v_p[2],iv=v_p[3]*Complex(0,1);
    v_p[0]=(i+q);  v_p[1]=(u+iv); 
    v_p[2]=(u-iv); v_p[3]=(i-q);
    return *this;
  }
  // Apply conversion matrix from Stokes to circular, in place
  CStokesVector& applyScirc() {
    Complex i=v_p[0],q=v_p[1],iu=v_p[2]*Complex(0,1),v=v_p[3];
    v_p[0]=(i+v);  v_p[1]=(q+iu); 
    v_p[2]=(q-iu); v_p[3]=(i-v);
    return *this;
  }
  // Apply conversion matrix from linear to Stokes, in place
  CStokesVector& applySlinInv() {
    Complex xx=v_p[0],xy=v_p[1],yx=v_p[2],yy=v_p[3];
    v_p[0]=(xx+yy)/2; v_p[1]=(xx-yy)/2;
    v_p[2]=(xy+yx)/2; v_p[3]=Complex(0,1)*(yx-xy)/2;
    return *this;
  }
  // Apply conversion matrix from circular to Stokes, in place
  CStokesVector& applyScircInv() {
    Complex rr=v_p[0],rl=v_p[1],lr=v_p[2],ll=v_p[3];
    v_p[0]=(rr+ll)/2; v_p[3]=(rr-ll)/2;
    v_p[1]=(rl+lr)/2; v_p[2]=Complex(0,1)*(lr-rl)/2;
    return *this;
  }
  // Return a StokesVector with the real part.
//  StokesVector real();
  // inner product of two complex vectors
  friend Complex innerProduct(const CStokesVector& l,
			      const CStokesVector& r) {
    return l.v_p[0]*conj(r.v_p[0])+ l.v_p[1]*conj(r.v_p[1])+
      l.v_p[2]*conj(r.v_p[2])+ l.v_p[3]*conj(r.v_p[3]);
  }
  friend double norm(const CStokesVector& l) {
    return ::sqrt(square(l.v_p[0].real())+square(l.v_p[0].imag())+
		  square(l.v_p[1].real())+square(l.v_p[1].imag())+
		  square(l.v_p[2].real())+square(l.v_p[2].imag())+
		  square(l.v_p[3].real())+square(l.v_p[3].imag()));
}
  // Write out a CStokesVector using the Vector output method.
  friend ostream& operator<<(ostream& os, const CStokesVector& v) {
    os << v.vector();
    return os;
  }
};

// Multiplication of CStokesVector by a Complex SquareMatrix
inline CStokesVector operator*(const SquareMatrix<Complex,4>& m,
			       const CStokesVector& v) {
  CStokesVector result(v);
  return result*=m;
}

inline void defaultValue(CStokesVector& v) {
  v=Complex(0.0,0.0);
}

class StokesVector:public RigidVector<Float,4> {
  
public:
  static String dataTypeId() {return "StokesVector";};
  //  StokesVector(Int n):RigidVector<Float,4>(n) {} 
  // Default constructor zeroes vector.
  StokesVector():RigidVector<Float,4>() {}
  // Construct from scalar, setting all values to a constant.
  StokesVector(Float f):RigidVector<Float,4>(f) {};
  // Construct with four values specified.
  StokesVector(Float v0, Float v1, Float v2, Float v3): RigidVector<Float,4>(v0,v1,v2,v3){}
  // Construct from Vector (should have length 4)
//  StokesVector(const Vector<Float> & v):RigidVector<Float,4>(v) {}
  // Copy constructor with copy semantics.
  StokesVector(const StokesVector& v):RigidVector<Float,4>(v) {}
  // Construct from RigidVector
//  StokesVector(const RigidVector<Float,4>& v):RigidVector<Float,4>(v) {}
  // Assignment
  StokesVector& operator=(const StokesVector& v) {
    RigidVector<Float,4>::operator=(v); return *this;
  }
  // Assign from a Vector
  StokesVector& operator=(const Vector<Float>& v) {
    RigidVector<Float,4>::operator=(v); return *this;
  }
  // Assign from a scalar, setting all values to a constant
  StokesVector& operator=(Float f) {
    RigidVector<Float,4>::operator=(f); return *this;
  }
  // Negation
  StokesVector& operator-() {
    RigidVector<Float,4>::operator-(); return *this;
  }
  // Addition
  StokesVector& operator+=(const StokesVector& v) {
    RigidVector<Float,4>::operator+=(v); return *this;
  }
  // Subtraction
  StokesVector& operator-=(const StokesVector& v) {
    RigidVector<Float,4>::operator-=(v); return *this;
  }
  StokesVector& operator*=(Float f) {
    RigidVector<Float,4>::operator*=(f); return *this;
  }
  StokesVector& operator*=(const StokesVector& v) {
    RigidVector<Float,4>::operator*=(v); return *this;
  }
  // Matrix multiplication - v*=m is equivalent to v=m*v
  StokesVector& operator*=(const SquareMatrix<Float,4>& m) {
    RigidVector<Float,4>::operator*=(m); return *this;
  }
  // Equality
  Bool operator==(const StokesVector& v) const {
    return (v_p[0]==v.v_p[0] && v_p[1]==v.v_p[1] &&
		  v_p[2]==v.v_p[2] && v_p[3]==v.v_p[3]);
  }
  // Inequality
  Bool operator!=(const StokesVector& v) const {
    return (v_p[0]!=v.v_p[0] || v_p[1]!=v.v_p[1] ||
		  v_p[2]!=v.v_p[2] || v_p[3]!=v.v_p[3]);
  }
  // Compute the maximum EigenValue
  Float maxEigenValue() const;
  // Compute the minimum EigenValue
  Float minEigenValue() const;
  // Compute the determinant of the coherence matrix
  Float determinant() const;

  // The innerproduct of 2 StokesVectors
  friend Float innerProduct(const StokesVector& l, const StokesVector& r) {
    return l.v_p[0]*r.v_p[0]+ l.v_p[1]*r.v_p[1]+
      l.v_p[2]*r.v_p[2]+ l.v_p[3]*r.v_p[3];
  }
  // Multiplication of StokesVector by a Complex SquareMatrix
  friend CStokesVector operator*(const SquareMatrix<Complex,4>& m,
				 const StokesVector& v) {
//    return m * (RigidVector<Float,4>&)v;
    return m * v;
  }
  // Write out a StokesVector using the Vector output method.
  friend ostream& operator<<(ostream& os, const StokesVector& v) {
    os << v.vector();
    return os;
  }

};

inline void defaultValue(StokesVector& v) {
  v=0.0f;
}

// Multiply by a scalar
inline StokesVector operator*(Float f, const StokesVector& v) {
  StokesVector r(v);
  return r*=f;
}
// Multiply by a scalar
inline StokesVector operator*(const StokesVector& v, Float f) {
  StokesVector r(v);
  return r*=f;
}

// Multiplication of StokesVector by a SquareMatrix
inline StokesVector operator*(const SquareMatrix<Float,4>& m,
			      const StokesVector& v) {
  StokesVector result(v);
  return result*=m;
}

// Apply conversion matrix from Stokes to linear(returns result)
inline CStokesVector& applySlin(CStokesVector& result, 
				const StokesVector& v) {
  Complex t=Complex(0.,v(3)); 
  result(0)=v(0)+v(1);
  result(1)=v(2)+t;
  result(2)=v(2)-t;
  result(3)=v(0)-v(1);
  return result;
}
// Apply conversion matrix from Stokes to linear.
inline CStokesVector applySlin(const StokesVector& v) {
  CStokesVector result;
  return applySlin(result,v);
}
// Apply conversion matrix from Stokes to circular.
inline CStokesVector& applyScirc(CStokesVector& result,
				 const StokesVector& v) {
  Complex t=Complex(0.,1.0)*v(2);
  result(0)=v(0)+v(3);
  result(1)=v(1)+t;
  result(2)=v(1)-t;
  result(3)=v(0)-v(3);
  return result;
}
// Apply conversion matrix from Stokes to circular.
inline CStokesVector applyScirc(const StokesVector& v) {
  CStokesVector result;
  return applyScirc(result,v);
}

// Apply conversion matrix from linear to Stokes.
inline StokesVector& applySlinInv(StokesVector& result, const CStokesVector& v) {
  result(0)=real(v(0)+v(3))/2;
  result(1)=real(v(0)-v(3))/2;
  result(2)=real(v(1)+v(2))/2;
  result(3)=real(Complex(0.,1.0)*(v(2)-v(1))/2);
  return result;
}

// Apply conversion matrix from linear to Stokes.
inline StokesVector applySlinInv(const CStokesVector& v) {
  StokesVector result;
  return applySlinInv(result,v);
}

// Apply conversion matrix from circular to Stokes.
inline StokesVector& applyScircInv(StokesVector& result, const CStokesVector& v) {
  result(0)=real(v(0)+v(3))/2;
  result(1)=real(v(1)+v(2))/2;
  result(2)=real(Complex(0.,1.0)*(v(2)-v(1))/2);
  result(3)=real(v(0)-v(3))/2;
  return result;
}

// Apply conversion matrix from circular to Stokes.
inline StokesVector applyScircInv(const CStokesVector& v) {
  StokesVector result;
  return applyScircInv(result,v);
}

// The following are needed until Image no longer has
// sigma images
//StokesVector& sqrt(const StokesVector& v);

//CStokesVector& sqrt(const CStokesVector& v);


} //# NAMESPACE CASA - END

#endif
