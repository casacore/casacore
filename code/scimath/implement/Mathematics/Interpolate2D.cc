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
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>

template<class T> 
Interpolate2D<T>::Interpolate2D()
:  itsMaskPtr(0)
{
}

template<class T> 
Interpolate2D<T>::Interpolate2D(const Interpolate2D& other)
:  itsMaskPtr(0)
{
}

template<class T> 
Interpolate2D<T>::~Interpolate2D()
{
};

template<class T> 
Interpolate2D<T>& Interpolate2D<T>::operator=(const Interpolate2D& other)
{
   if (this != &other) {
      itsMaskPtr = 0;
   }
   return *this;
};

  
template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
                              const Vector<Double>& where, 
			      const Array<T>& data,
                              Interpolate2D<T>::Method method)
{
  AlwaysAssert( (data.ndim() == 2 ), AipsError);
  const Matrix<T>& data2 = dynamic_cast<const Matrix<T>&>(data);
  return interp(result, where, data2, method);
};


template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
                              const Vector<Double>& where, 
			      const Array<T>& data,
			      const Array<Bool>& mask,
                              Interpolate2D<T>::Method method)
{
  AlwaysAssert( (data.ndim() == 2 ), AipsError);
  const Matrix<T>& data2 = dynamic_cast<const Matrix<T>&>(data);
  const Matrix<Bool>& mask2 = dynamic_cast<const Matrix<Bool>&>(mask);
  return interp(result, where, data2, mask2, method);
};


template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
			      const Vector<Double>& where, 
                              const Matrix<T>& data,
                              Interpolate2D<T>::Method method)
{
  itsMaskPtr = 0;
  if (method==Interpolate2D<T>::LINEAR) {
    return interpLinear(result, where, data);
  } else if (method==Interpolate2D<T>::CUBIC) {
    return interpCubic(result, where, data);
  } else if (method==Interpolate2D<T>::NEAREST) {
    return interpNearest(result, where, data);
  }
  return True;
};


template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
			      const Vector<Double>& where, 
                              const Matrix<T>& data,
                              const Matrix<Bool>& mask,
                              Interpolate2D<T>::Method method)
{
  itsMaskPtr = &mask;
  if (method==Interpolate2D<T>::LINEAR) {
    return interpLinear(result, where, data);
  } else if (method==Interpolate2D<T>::CUBIC) {
    return interpCubic(result, where, data);
  } else if (method==Interpolate2D<T>::NEAREST) {
    return interpNearest(result, where, data);
  }
  return True;
};


template<class T> 
Bool Interpolate2D<T>::interpNearest(T& result, 
                                     const Vector<Double>& where, 
                                     const Matrix<T>& data)
{
  static Int i,j;
  Bool ok = check(where, data.shape(), Interpolate2D<T>::NEAREST);
  if (itsMaskPtr!=0 && ok) {
     ok = !anyBadMaskPixels();
  }
//
  if(ok) {
    i = Int(where(0)+0.5);
    j = Int(where(1)+0.5);
    result = data(i,j);
//
// needed for debug function "location" only
/*
    itsII = i;
    itsJJ = j;
*/
  }
//
  return ok;
}

template<class T> 
Bool Interpolate2D<T>::interpLinear(T& result, 
                                    const Vector<Double>& where, 
                                    const Matrix<T>& data)
{
  static Double TT, UU; 
//
  Bool ok = check(where, data.shape(), Interpolate2D<T>::LINEAR);
  if (itsMaskPtr!=0 && ok) {
     ok = !anyBadMaskPixels();
  }
//
  if(ok) {
    TT = where(0) - itsI;
    UU = where(1) - itsJ;
//
    result = (1.0-TT)*(1.0-UU)*data(itsI,itsJ) +
             TT*(1.0-UU)*data(itsI+1,itsJ) +
	     TT*UU*data(itsI+1,itsJ+1) +
             (1.0-TT)*UU*data(itsI,itsJ+1);
  }
  return ok;
};


template<class T> 
Bool Interpolate2D<T>::interpCubic(T& result, 
                                   const Vector<Double>& where, 
                                   const Matrix<T>& data)
//
// bi-cubic interpolation
//
{

// Temporaries

  static Double TT, UU, d1, d2;
  static Vector<T> Y(4), Y1(4), Y2(4), Y12(4);
  static Matrix<T> C(4,4);
//
  Bool ok = check(where, data.shape(), Interpolate2D<T>::CUBIC);
  if (itsMaskPtr!=0 && ok) {
     ok = !anyBadMaskPixels();
  }
  if (!ok) return False;
//
  d1 = 1.0;
  d2 = 1.0;
//
  TT = where(0) - itsI;
  UU = where(1) - itsJ;

// define values of function and its derivatives on the
// square of points bounding "where"

  Y(0) = data(itsI,  itsJ);
  Y(1) = data(itsI+1,itsJ);
  Y(2) = data(itsI+1,itsJ+1);
  Y(3) = data(itsI,  itsJ+1);
//
  Y1(0) = data(itsI+1, itsJ)   - data(itsI-1, itsJ);
  Y1(1) = data(itsI+2, itsJ)   - data(itsI,   itsJ);
  Y1(2) = data(itsI+2, itsJ+1) - data(itsI,   itsJ+1);
  Y1(3) = data(itsI+1, itsJ+1) - data(itsI-1, itsJ+1);
  
  Y2(0) = data(itsI,   itsJ+1) - data(itsI,   itsJ-1);
  Y2(1) = data(itsI+1, itsJ+1) - data(itsI+1, itsJ-1);
  Y2(2) = data(itsI+1, itsJ+2) - data(itsI+1, itsJ);
  Y2(3) = data(itsI,   itsJ+2) - data(itsI,   itsJ);
//
  Y12(0) =  data(itsI+1, itsJ+1) + data(itsI-1, itsJ-1) -
               data(itsI-1, itsJ+1) - data(itsI+1, itsJ-1);
  Y12(1) =  data(itsI+2, itsJ+1) + data(itsI, itsJ-1) -
               data(itsI, itsJ+1) - data(itsI+2, itsJ-1);
  Y12(2) =  data(itsI+2, itsJ+2) + data(itsI, itsJ) -
               data(itsI, itsJ+2) - data(itsI+2, itsJ);
  Y12(3) =  data(itsI+1, itsJ+2) + data(itsI-1, itsJ) - 
               data(itsI-1, itsJ+2) - data(itsI+1, itsJ);
//
  bcucof(Y, Y1, Y2, Y12, d1, d2, C);
//  
  result = 0.0;
  for (Int i=3; i>=0; i--) {
    result = TT*result + ((C(i,3)*UU + C(i,2))*UU + C(i,1))*UU + C(i,0);
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
  static Vector<T> X(16), CL(16);
//
  Int l, k, i, j;
  Double  xx, d1d2; 
  d1d2 = d1*d2;

// Pack temporary

  for (i=0;i<4;i++) {
    X(i) = y(i);
    X(i+4) = y1(i)*d1;
    X(i+8) = y2(i)*d2;
    X(i+12) = y12(i)*d1d2;
  }

 // Matrix multiply the stored table

  for (i=0;i<=15;i++) {
    xx = 0.0;
    for (k=0;k<=15;k++) {
       j = (15 * i) + k;
       xx += wt[i][k] * X(k);
    }
    CL(i) = xx;
  }

// Unpack the result inot the output table

  l = 0;
  for (i=0;i<4;i++) {
    for (j=0;j<4;j++) {
      c(i,j) = CL(l++);
    }
  }
}


template<class T>  
Bool Interpolate2D<T>::check(const Vector<Double>& where,
                             const IPosition& shape,
                             Interpolate2D<T>::Method method)
//
// Check to make sure that there is enough space around "where" in the Matrix
// to get an interpolation
//
{
  Bool ok = True;
  AlwaysAssert(where.nelements()==2, AipsError);
  itsI = Int(where(0));
  itsJ = Int(where(1));
//
  if (method==Interpolate2D<T>::NEAREST) {

// 2x2 starting from [i,j]

     itsMinI = itsI;
     itsMaxI = itsI + 1;
     itsMinJ = itsJ;
     itsMaxJ = itsJ + 1;
//
     ok = (itsMinI>=0 && (itsMaxI)<=(shape(0)-1) &&
           itsMinJ>=0 && (itsMaxJ)<=(shape(1)-1));
  } else if (method==Interpolate2D<T>::LINEAR) {

// 2x2 starting from [i,j]

     itsMinI = itsI;
     itsMaxI = itsI + 1;
     itsMinJ = itsJ;
     itsMaxJ = itsJ + 1;
//
     ok = (itsMinI>=0 && (itsMaxI)<=(shape(0)-1) &&
           itsMinJ>=0 && (itsMaxJ)<=(shape(1)-1));
  } else if (method==Interpolate2D<T>::CUBIC) {

// 4x4 starting from [i-1,j-1]

     itsMinI = itsI - 1;
     itsMaxI = itsI + 2;
     itsMinJ = itsJ - 1;
     itsMaxJ = itsJ + 2;
     ok = (itsMinI>=0 && itsMaxI<=(shape(0)-1) &&
           itsMinJ>=0 && itsMaxJ<=(shape(1)-1));

  }
  return ok;
}





template<class T> 
Interpolate2D<T>::Method Interpolate2D<T>::stringToMethod (const String& method)
{
   String typeU = method;
   typeU.upcase();
   String tmp = String(typeU.at(0, 1));
   Interpolate2D<T>::Method method2;
   if (tmp==String("N")) {
      method2 = Interpolate2D<T>::NEAREST;
   } else if (tmp==String("L")) {
      method2 = Interpolate2D<T>::LINEAR;
   } else if (tmp==String("C")) {
      method2 = Interpolate2D<T>::CUBIC;
   } else {
      throw(AipsError("Illegal method"));
   }
   return method2;
}


template<class T> 
Bool Interpolate2D<T>::anyBadMaskPixels ()
{
   for (Int j=itsMinJ; j<itsMaxJ; j++) {
      for (Int i=itsMinI; i<itsMaxI; i++) {   
         if (! (*itsMaskPtr)(i,j)) return True;
       }
    }
    return False;
}

/*
template<class T> 
void Interpolate2D<T>::location (IPosition& nearLoc, IPosition& loc, IPosition& min, IPosition& max) const
//
// A debugging function
//
{
   nearLoc.resize(2);
   nearLoc(0) = itsII;
   nearLoc(1) = itsJJ;
//
   loc.resize(2);
   loc(0) = itsI;
   loc(1) = itsJ;
//
   min.resize(2);
   min(0) = itsMinI;
   min(1) = itsMinJ;
//
   max.resize(2);
   max(0) = itsMaxI;
   max(1) = itsMaxJ;
}
*/
