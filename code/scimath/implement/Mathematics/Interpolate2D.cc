//# Interpolate2D.cc:  this implements Interpolate2D
//# Copyright (C) 1996,1997,1998,1999,2000
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
 
#include <trial/Mathematics/Interpolate2D.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Array.h>
#include <aips/Exceptions/Error.h>

template<class T> 
Interpolate2D<T>::Interpolate2D(Interpolate2D<T>::Method method)
: itsMethod(method)
{
   initArrays();
//
   ok();
};

template<class T> 
Interpolate2D<T>::Interpolate2D(const Interpolate2D& other)
: itsMethod(other.itsMethod)
{
   initArrays();
   ok();
};

template<class T> 
Interpolate2D<T>::~Interpolate2D()
{
};

template<class T> 
Interpolate2D<T>& Interpolate2D<T>::operator=(const Interpolate2D& other)
{
   if (this != &other) {
      itsMethod = other.itsMethod;
   }
   return *this;
};


template<class T> 
Bool Interpolate2D<T>::ok()
{
  Bool ok = True;
  return ok;
};
  
template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
                              const Vector<Double>& where, 
			      const Array<T>& data)
{
  AlwaysAssert( (data.ndim() == 2 ), AipsError);
  return (interp(result, where, Matrix<T>(data)));
};


template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
			      const Vector<Double>& where, 
                              const Matrix<T>& data)
{
  if (itsMethod==Interpolate2D<T>::LINEAR) {
    return (interpLinear(result, where, data));
  } else if (itsMethod==Interpolate2D<T>::CUBIC) {
    return (interpCubic(result, where, data));
  } else if (itsMethod==Interpolate2D<T>::NEAREST) {
    return (interpNearest(result, where, data));
  }
  return True;
};

template<class T> 
Bool Interpolate2D<T>::interpNearest(T& result, 
                              const Vector<Double>& where, 
                              const Matrix<T>& data)
//
// nearest
//
{
  Bool ok = True;
  if(check(where, data)) {
    Int i = static_cast<Int>(floor(where(0)));
    Int j = static_cast<Int>(floor(where(1)));
//
    Double dx = where(0) - Double(i);
    Double dy = where(1) - Double(j);
    Int xoff = static_cast<Int>(floor(dx+0.5));
    Int yoff = static_cast<Int>(floor(dy+0.5));
//
    result = data(i+xoff,j+yoff);
  } else {
    ok = False;
  }
  return ok;
}

template<class T> 
Bool Interpolate2D<T>::interpLinear(T& result, 
                              const Vector<Double>& where, 
                              const Matrix<T>& data)
//
// bi-linear interpolation
//
{
  if(check(where, data) ) {
    its1I = static_cast<Int>(where(0));
    its1J = static_cast<Int>(where(1));
//
    its1T = where(0) - its1I;
    its1U = where(1) - its1J;
//
    result = (1.0-its1T)*(1.0-its1U)*data(its1I,its1J) +
             its1T*(1.0-its1U)*data(its1I+1,its1J) +
	     its1T*its1U*data(its1I+1,its1J+1) +
             (1.0-its1T)*its1U*data(its1I,its1J+1);
    return True;
  } else {
    return False;
  }
};


template<class T> 
Bool Interpolate2D<T>::interpCubic(T& result, 
                               const Vector<Double>& where, 
			       const Matrix<T>& data)
//
// bi-cubic interpolation
//
{
  if (! check(where, data) ) {
    return False;
  }
  Double d1 = 1.0;
  Double d2 = 1.0;
//
  its2I =  static_cast<Int>(where(0));    //  its2I, its2J is to the lower left of "where"
  its2J =  static_cast<Int>(where(1));
//
  its2T = where(0) - its2I;
  its2U = where(1) - its2J;

// define values of function and its derivatives on the
// square of points bounding "where"

  itsY(0) = data(its2I,  its2J);
  itsY(1) = data(its2I+1,its2J);
  itsY(2) = data(its2I+1,its2J+1);
  itsY(3) = data(its2I,  its2J+1);
//
  itsY1(0) = data(its2I+1, its2J)   - data(its2I-1, its2J);
  itsY1(1) = data(its2I+2, its2J)   - data(its2I,   its2J);
  itsY1(2) = data(its2I+2, its2J+1) - data(its2I,   its2J+1);
  itsY1(3) = data(its2I+1, its2J+1) - data(its2I-1, its2J+1);
  
  itsY2(0) = data(its2I,   its2J+1) - data(its2I, its2J-1);
  itsY2(1) = data(its2I+1, its2J+1) - data(its2I+1, its2J-1);
  itsY2(2) = data(its2I+1, its2J+2) - data(its2I+1, its2J);
  itsY2(3) = data(its2I,   its2J+2) - data(its2I, its2J);
//
  itsY12(0) =  data(its2I+1, its2J+1) + data(its2I-1, its2J-1) -
               data(its2I-1, its2J+1) - data(its2I+1, its2J-1);
  itsY12(1) =  data(its2I+2, its2J+1) + data(its2I, its2J-1) -
               data(its2I, its2J+1) - data(its2I+2, its2J-1);
  itsY12(2) =  data(its2I+2, its2J+2) + data(its2I, its2J) -
               data(its2I, its2J+2) - data(its2I+2, its2J);
  itsY12(3) =  data(its2I+1, its2J+2) + data(its2I-1, its2J) - 
               data(its2I-1, its2J+2) - data(its2I+1, its2J);
//
  bcucof(itsY, itsY1, itsY2, itsY12, d1, d2, itsC);
//  
  result = 0.0;
  for (Int i=3; i>=0; i--) {
    result = its2T*result + ((itsC(i,3)*its2U + itsC(i,2))*its2U + itsC(i,1))*its2U + itsC(i,0);
  }
//
  return True;
};


template<class T>  
void Interpolate2D<T>::bcucof (Vector<T> y, Vector<T> y1, Vector<T> y2, Vector<T> y12,
                               Double d1, Double d2, Matrix<T> c)
//
// Numerical recipes 3.6 (p99)
//
{
  static const int wt[16][16] =
  {{1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
   {0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0},
   {-3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0},
   {2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0},
   {0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},
   {0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0},
   {0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1},
   {0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1},
   {-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0},
   {0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0},
   {9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2},
   {-6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2},
   {2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0},
   {0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0},
   {-6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1},
   {4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1}};
//
  Int l, k, i, j;
  Double  xx, d1d2; 
  d1d2 = d1*d2;

// Pack temporary

  for (i=0;i<4;i++) {
    itsX(i) = y(i);
    itsX(i+4) = y1(i)*d1;
    itsX(i+8) = y2(i)*d2;
    itsX(i+12) = y12(i)*d1d2;
  }

 // Matrix multiply the stored table

  for (i=0;i<=15;i++) {
    xx = 0.0;
    for (k=0;k<=15;k++) {
       j = (15 * i) + k;
       xx += wt[i][k] * itsX(k);
    }
    itsCL(i) = xx;
  }

// Unpack the result inot the output table

  l = 0;
  for (i=0;i<4;i++) {
    for (j=0;j<4;j++) {
      c(i,j) = itsCL(l++);
    }
  }
};



template<class T> 
Bool Interpolate2D<T>::check(const Vector<Double>& where, 
                             const Matrix<T>& data)
//
// Check to make sure that there is enough space around "where" in the Matrix
// to get an interpolation
//
{
  Bool ok = True;
  AlwaysAssert(where.nelements()==2, AipsError);
  Int i = static_cast<Int>(floor(where(0)));
  Int j = static_cast<Int>(floor(where(1)));
//
  const IPosition& shape = data.shape();
  if (itsMethod==Interpolate2D<T>::NEAREST) {

// 2x2 starting from [i,j]

     ok = (i>=0 && (i+1)<=(shape(0)-1) &&
           j>=0 && (j+1)<=(shape(1)-1));
  } else if (itsMethod==Interpolate2D<T>::LINEAR) {

// 2x2 starting from [i,j]

     ok = (i>=0 && (i+1)<=(shape(0)-1) &&
           j>=0 && (j+1)<=(shape(1)-1));
  } else if (itsMethod==Interpolate2D<T>::CUBIC) {

// 4x4 starting from [i-1,j-1]

     ok = ((i-1)>=0 && (i+2)<=(shape(0)-1) &&
           (j-1)>=0 && (j+2)<=(shape(1)-1));
  }
  return ok;
};


template<class T> 
void Interpolate2D<T>::initArrays()
{
   itsY.resize(4);
   itsY1.resize(4);
   itsY2.resize(4);
   itsY12.resize(4);
   itsC.resize(4,4);
   itsCL.resize(16);
   itsX.resize(16);
}


template<class T> 
Interpolate2D<T>::Method Interpolate2D<T>::stringToMethod (const String& method)
{
   String typeU = method;
   typeU.upcase();
   Interpolate2D<T>::Method method2;
   if (typeU.contains("NEA")) {
      method2 = Interpolate2D<T>::NEAREST;
   } else if (typeU.contains("LIN")) {
      method2 = Interpolate2D<T>::LINEAR;
   } else if (typeU.contains("CUB")) {
      method2 = Interpolate2D<T>::CUBIC;
   } else {
      throw(AipsError("Illegal method"));
   }
   return method2;
}

