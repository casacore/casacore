//# Interpolate2D.cc:  this implements Interpolate2D
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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
{
   itsY.resize(4);
   itsY1.resize(4);
   itsY2.resize(4);
   itsY12.resize(4);
   itsC.resize(4,4);
}

template<class T> 
Interpolate2D<T>::Interpolate2D(const Interpolate2D& other)
{}

template<class T> 
Interpolate2D<T>::~Interpolate2D()
{}

template<class T> 
Interpolate2D<T>& Interpolate2D<T>::operator=(const Interpolate2D& other)
{
   return *this;
};

  
template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
                              const Vector<Double>& where, 
			      const Array<T>& data,
                              Interpolate2D<T>::Method method)  const
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
                              Interpolate2D<T>::Method method)  const
{
  AlwaysAssert(data.ndim()==2, AipsError);
  const Matrix<T>& data2 = dynamic_cast<const Matrix<T>&>(data);
  const Matrix<Bool>& mask2 = dynamic_cast<const Matrix<Bool>&>(mask);
  return interp(result, where, data2, mask2, method);
};


template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
			      const Vector<Double>& where, 
                              const Matrix<T>& data,
                              Interpolate2D<T>::Method method) const
{
  const Matrix<Bool>* maskPtr(0);
  if (method==Interpolate2D<T>::LINEAR) {
    return interpLinear(result, where, data, maskPtr);
  } else if (method==Interpolate2D<T>::CUBIC) {
    return interpCubic(result, where, data, maskPtr);
  } else if (method==Interpolate2D<T>::NEAREST) {
    return interpNearest(result, where, data, maskPtr);
  }
  return True;
}


template<class T> 
Bool Interpolate2D<T>::interp(T& result, 
			      const Vector<Double>& where, 
                              const Matrix<T>& data,
                              const Matrix<Bool>& mask,
                              Interpolate2D<T>::Method method)  const
{
  const Matrix<Bool>* maskPtr = &mask;
  if (method==Interpolate2D<T>::LINEAR) {
    return interpLinear(result, where, data, maskPtr);
  } else if (method==Interpolate2D<T>::CUBIC) {
    return interpCubic(result, where, data, maskPtr);
  } else if (method==Interpolate2D<T>::NEAREST) {
    return interpNearest(result, where, data, maskPtr);
  }
  return True;
};


template<class T> 
Bool Interpolate2D<T>::interpNearest(T& result, 
                                     const Vector<Double>& where, 
                                     const Matrix<T>& data,
                                     const Matrix<Bool>*& maskPtr) const
{
  AlwaysAssert(where.nelements()==2, AipsError);
  const IPosition& shape = data.shape();

// Find nearest pixel; (i,j) = centre

  Int i = Int(where(0)+0.5);
  Int j = Int(where(1)+0.5);
  Bool ok = False;
//
  if (i >= 0 && i <= shape(0)-1 && j >= 0 && j <= shape(1)-1) {
    if (maskPtr==0) {
       result = data(i,j);
       ok = True;
    } else {
       if ((*maskPtr)(i,j)) {
          result = data(i,j);
          ok = True;
       }
    }
  }    
//
  return ok;
}

template<class T> 
Bool Interpolate2D<T>::interpLinear(T& result, 
                                    const Vector<Double>& where, 
                                    const Matrix<T>& data,
                                    const Matrix<Bool>*& maskPtr) const
{
   AlwaysAssert(where.nelements()==2, AipsError);
   const IPosition& shape = data.shape();

// Find nearest pixel; (i,j) = centre

   Int i = Int(where(0)+0.5);
   Int j = Int(where(1)+0.5);

// Handle edge. Just move start left/down by one,

   if (i==shape(0)-1) i--;
   if (j==shape(1)-1) j--;

// 2x2 starting from [i,j]

   Bool doit = False;
   if (i >= 0 && i+1 <= shape(0)-1 &&  j >= 0 && j+1 <= shape(1)-1) {
      doit = True;
   }
//
   Bool ok = False;
   if (doit && maskPtr!=0) {
      doit = (*maskPtr)(i,j) &&
              (*maskPtr)(i+1,j) &&
              (*maskPtr)(i,j+1) &&
              (*maskPtr)(i+1,j+1);
   }
//
   if (doit) {
      Double TT = where(0) - i;
      Double UU = where(1) - j;
//
      result = (1.0-TT)*(1.0-UU)*data(i,j) +
               TT*(1.0-UU)*data(i+1,j) +
               TT*UU*data(i+1,j+1) +
               (1.0-TT)*UU*data(i,j+1);
      ok = True;
  }
//
  return ok;
}


template<class T> 
Bool Interpolate2D<T>::interpCubic(T& result, 
                                   const Vector<Double>& where, 
                                   const Matrix<T>& data,
                                   const Matrix<Bool>*& maskPtr) const
//
// bi-cubic interpolation
//
{
   AlwaysAssert(where.nelements()==2, AipsError);
   const IPosition& shape = data.shape();

// Find nearest pixel; (i,j) = centre

   Int i = Int(where(0)+0.5);
   Int j = Int(where(1)+0.5);

// Interpolation grid is 4x4 :  [i-1,j-1] -> [i+2,j+2]
// Handle edge (and beyond) by using linear.

   if (i<=0 || i>=shape(0)-2 || j<=0 || j>=shape(1)-2) {
      return interpLinear(result, where, data, maskPtr);
   }
//
   if (anyBadMaskPixels(maskPtr)) return False;

// Do it

   Double d1 = 1.0;
   Double d2 = 1.0;
//
   Double TT = where(0) - i;
   Double UU = where(1) - j;

// define values of function and its derivatives on the
// square of points bounding "where"

   itsY(0) = data(i,  j);
   itsY(1) = data(i+1,j);
   itsY(2) = data(i+1,j+1);
   itsY(3) = data(i,  j+1);
//
   itsY1(0) = data(i+1, j)   - data(i-1, j);
   itsY1(1) = data(i+2, j)   - data(i,   j);
   itsY1(2) = data(i+2, j+1) - data(i,   j+1);
   itsY1(3) = data(i+1, j+1) - data(i-1, j+1);
//  
   itsY2(0) = data(i,   j+1) - data(i,   j-1);
   itsY2(1) = data(i+1, j+1) - data(i+1, j-1);
   itsY2(2) = data(i+1, j+2) - data(i+1, j);
   itsY2(3) = data(i,   j+2) - data(i,   j);
//
   itsY12(0) =  data(i+1, j+1) + data(i-1, j-1) -
                data(i-1, j+1) - data(i+1, j-1);
   itsY12(1) =  data(i+2, j+1) + data(i, j-1) -
                data(i, j+1) - data(i+2, j-1);
   itsY12(2) =  data(i+2, j+2) + data(i, j) -
                data(i, j+2) - data(i+2, j);
   itsY12(3) =  data(i+1, j+2) + data(i-1, j) - 
                data(i-1, j+2) - data(i+1, j);
//
   bcucof(itsC, itsY, itsY1, itsY2, itsY12, d1, d2);
//  
   result = 0.0;
   for (Int i=3; i>=0; i--) {
     result = TT*result + ((itsC(i,3)*UU + itsC(i,2))*UU + 
                            itsC(i,1))*UU + itsC(i,0);
   }
//
   return True;
};


template<class T>  
void Interpolate2D<T>::bcucof (Matrix<T>& c, const Vector<T>& y, const Vector<T>& y1, 
                               const Vector<T>& y2, const Vector<T>& y12,
                               Double d1, Double d2) const
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
  uInt l, k, i, j;
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

// Unpack the result into the output table

  l = 0;
  for (i=0;i<4;i++) {
    for (j=0;j<4;j++) {
      c(i,j) = CL(l++);
    }
  }
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
Bool Interpolate2D<T>::anyBadMaskPixels (const Matrix<Bool>*& maskPtr) const
{
   if (maskPtr==0) return False;
//
   Bool deleteIt;
   const Bool* p = maskPtr->getStorage(deleteIt);
   for (uInt i=0; i<maskPtr->nelements(); i++)  {
      if (!(p[i])) return True;
   }
   maskPtr->freeStorage(p, deleteIt);
   return False;
}  
