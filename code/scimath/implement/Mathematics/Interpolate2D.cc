//# Interpolate2D.cc:  this implements Interpolate2D
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

Interpolate2D::Interpolate2D(Interpolate2D::Method method)
{
   itsY.resize(4);
   itsY1.resize(4);
   itsY2.resize(4);
   itsY12.resize(4);
   itsC.resize(4,4);

// Set up function pointers to correct method

  if (method==Interpolate2D::LINEAR) {
    itsFuncPtrFloat = &Interpolate2D::interpLinearFloat;
    itsFuncPtrDouble = &Interpolate2D::interpLinearDouble;
    itsFuncPtrBool = &Interpolate2D::interpLinearBool;
  } else if (method==Interpolate2D::CUBIC) {
    itsFuncPtrFloat = &Interpolate2D::interpCubicFloat;
    itsFuncPtrDouble = &Interpolate2D::interpCubicDouble;
    itsFuncPtrBool = &Interpolate2D::interpCubicBool;
  } else if (method==Interpolate2D::NEAREST) {
    itsFuncPtrFloat = &Interpolate2D::interpNearestFloat;
    itsFuncPtrDouble = &Interpolate2D::interpNearestDouble;
    itsFuncPtrBool = &Interpolate2D::interpNearestBool;
  }   
}

Interpolate2D::Interpolate2D(const Interpolate2D &other)
: itsFuncPtrFloat(0),
  itsFuncPtrDouble(0),
  itsFuncPtrBool(0)
{}

Interpolate2D::~Interpolate2D()
{}

Interpolate2D &Interpolate2D::operator=(const Interpolate2D &other)
{
   itsFuncPtrFloat = other.itsFuncPtrFloat;
   itsFuncPtrDouble = other.itsFuncPtrDouble;
   itsFuncPtrBool = other.itsFuncPtrBool;
   return *this;
};



// Float Versions

Bool Interpolate2D::interp(Float &result, 
                           const Vector<Double> &where, 
                           const Matrix<Float> &data) const
{
  const Matrix<Bool>* maskPtr(0);
  return ((*this).*itsFuncPtrFloat)(result, where, data, maskPtr);
}


Bool Interpolate2D::interp(Float &result, 
                           const Vector<Double> &where, 
                           const Matrix<Float> &data,
                           const Matrix<Bool> &mask) const
{
  const Matrix<Bool>* maskPtr = &mask;
  return ((*this).*itsFuncPtrFloat)(result, where, data, maskPtr);
}


// Double versions


Bool Interpolate2D::interp(Double &result, 
                           const Vector<Double> &where, 
                           const Matrix<Double> &data) const {
  const Matrix<Bool>* maskPtr(0);
  return ((*this).*itsFuncPtrDouble)(result, where, data, maskPtr);
}


Bool Interpolate2D::interp(Double &result, 
                           const Vector<Double> &where, 
                           const Matrix<Double> &data,
                           const Matrix<Bool> &mask) const {
  const Matrix<Bool>* maskPtr = &mask;
  return ((*this).*itsFuncPtrDouble)(result, where, data, maskPtr);
}

// Double version with two identicals and mask

Bool Interpolate2D::interp(Double &resultI, Double &resultJ, 
                           const Vector<Double> &where, 
                           const Matrix<Double> &dataI,
                           const Matrix<Double> &dataJ,
                           const Matrix<Bool> &mask) const {
  return interpLinearDouble2(resultI, resultJ, where, dataI, dataJ, mask);
}



// Bool versions


Bool Interpolate2D::interp(Bool &result, 
                           const Vector<Double> &where, 
                           const Matrix<Bool> &data) const
{
  return ((*this).*itsFuncPtrBool)(result, where, data);
}



// Private functions


Bool Interpolate2D::interpNearestFloat (Float &result, 
                                        const Vector<Double> &where,
                                        const Matrix<Float> &data,
                                        const Matrix<Bool>* &maskPtr) const
{
  // definition of the 'neighborhood' of outer edge data elements.
  static const Double half= .5001;

  const IPosition &shape = data.shape();

  Double imax = shape(0) - 1.;
  Double wi = where[0];
  if(wi < 0. - half || wi > imax + half || imax < 0) return False;

  Double jmax = shape(1) - 1.;
  Double wj = where[1];
  if(wj < 0 - half || wj > jmax + half || jmax < 0) return False;

  uInt i = (wi <= 0.)?		0
	 : (wi >= imax)?	uInt(imax)
	 :			uInt(wi + .5);
      
  uInt j = (wj <= 0.)?		0
	 : (wj >= jmax)?	uInt(jmax)
	 :			uInt(wj + .5);
      
  Bool dataValid = !maskPtr || (*maskPtr)(i,j);
  if (dataValid) result = data(i,j);
  return dataValid;
}

Bool Interpolate2D::interpNearestDouble (Double &result, 
                                         const Vector<Double> &where,
                                         const Matrix<Double> &data,
                                         const Matrix<Bool>* &maskPtr) const
{
  // definition of the 'neighborhood' of outer edge data elements.
  static const Double half= .5001;

  const IPosition &shape = data.shape();

  Double imax = shape(0) - 1.;
  Double wi = where[0];
  if(wi < 0. - half || wi > imax + half || imax < 0) return False;

  Double jmax = shape(1) - 1.;
  Double wj = where[1];
  if(wj < 0 - half || wj > jmax + half || jmax < 0) return False;

  uInt i = (wi <= 0.)?		0
	 : (wi >= imax)?	uInt(imax)
	 :			uInt(wi + .5);
      
  uInt j = (wj <= 0.)?		0
	 : (wj >= jmax)?	uInt(jmax)
	 :			uInt(wj + .5);
      
  Bool dataValid = !maskPtr || (*maskPtr)(i,j);
  if (dataValid) result = data(i,j);
  return dataValid;
}


Bool Interpolate2D::interpLinearFloat(Float &result, 
				      const Vector<Double> &where, 
				      const Matrix<Float> &data,
				      const Matrix<Bool>* &maskPtr) const {
  const IPosition &shape = data.shape();

  // We find 4 points surrounding the one of interest.
  // Negatives will give big positive value for the uInt 
  // and these will fail the  shape test below.
  // Make sure we don't access i+1 or j+1 because the 
  // big positive plus 1 may become 0 and then we will spuriously
  // pass the shape test
  uInt i = Int(where[0]);               // Assuming Int does (1.2 -> 1)
  uInt j = Int(where[1]);
  uInt si = uInt(shape(0)-1);
  uInt sj = uInt(shape(1)-1);

  // Handle edge. Just move start left/down by one,
  if (i==si) --i;
  if (j==sj) --j;

  // 2x2 starting from [i,j]
  // mask==True is a good pixel
  if (i < si && j < sj) {                   
    if (maskPtr) {
      if (!(*maskPtr)(i,j) || !(*maskPtr)(i+1,j) ||
	  !(*maskPtr)(i,j+1) || !(*maskPtr)(i+1,j+1)) return False;
    };
    Double TT = where[0] - i;
    Double UU = where[1] - j;
    result = (1.0-TT)*(1.0-UU)*data(i,j) +
      TT*(1.0-UU)*data(i+1,j) +
      TT*UU*data(i+1,j+1) +
      (1.0-TT)*UU*data(i,j+1);
    return True;
  } else return False;
}

Bool Interpolate2D::interpLinearDouble(Double &result, 
				       const Vector<Double> &where, 
				       const Matrix<Double> &data,
				       const Matrix<Bool>* &maskPtr) const {
  const IPosition &shape = data.shape();

// We find 4 points surrounding the one of interest.
// Negatives will give big positive value for the uInt 
// and these will fail the  shape test below.
// Make sure we don't access i+1 or j+1 because the 
// big positive plus 1 may become 0 and then we will spuriously
// pass the shape test

  uInt i = Int(where[0]);               // Assuming Int does (1.2 -> 1)
  uInt j = Int(where[1]);
  uInt si = uInt(shape(0)-1);
  uInt sj = uInt(shape(1)-1);

  // Handle edge. Just move start left/down by one,
  if (i==si) --i;
  if (j==sj) --j;

  // 2x2 starting from [i,j]
  // mask==True is a good pixel
  if (i < si && j < sj) {                   
    if (maskPtr) {
      if (!(*maskPtr)(i,j) || !(*maskPtr)(i+1,j) ||
	  !(*maskPtr)(i,j+1) || !(*maskPtr)(i+1,j+1)) return False;
    };
    Double TT = where[0] - i;
    Double UU = where[1] - j;
    result = (1.0-TT)*(1.0-UU)*data(i,j) +
      TT*(1.0-UU)*data(i+1,j) +
      TT*UU*data(i+1,j+1) +
      (1.0-TT)*UU*data(i,j+1);
    return True;
  } else return False;
}

Bool Interpolate2D::interpLinearDouble2(Double &resultI, Double &resultJ, 
                                        const Vector<Double> &where, 
                                        const Matrix<Double> &dataI,
                                        const Matrix<Double> &dataJ,
                                        const Matrix<Bool> &mask) const {
  const IPosition &shape = mask.shape();

  // We find 4 points surrounding the one of interest.
  // Negatives will give big positive value for the uInt 
  // and these will fail the  shape test below.
  // Make sure we don't access i+1 or j+1 because the 
  // big positive plus 1 may become 0 and then we will spuriously
  // pass the shape test

  uInt i = Int(where[0]);               // Assuming Int does (1.2 -> 1)
  uInt j = Int(where[1]);
  uInt si = uInt(shape[0]-1);
  uInt sj = uInt(shape[1]-1);
  // Handle edge. Just move start left/down by one,
  if (i==si) --i;
  if (j==sj) --j;
  // 2x2 starting from [i,j]
  // mask==True is a good pixel
  if (i < si && j < sj) {
    uInt k0 = dataI.steps()[0];
    uInt k1 = dataI.steps()[1];
    const Bool *m = &mask(i,j);
    if ( !*m || !*(m+k0) || !*(m+k1) || !*(m+k0+k1)) return False;
    Double TT = where[0] - i;
    Double UU = where[1] - j;
    Double x1 = (1.0-TT);
    Double y1 = (1.0-UU);
    Double x = x1*y1;
    const Double *dI = &dataI(i,j);
    const Double *dJ = &dataJ(i,j);
    resultI = x * *dI;
    resultJ = x * *dJ;
    x = TT*y1;
    resultI += x * *(dI+k0);
    resultJ += x * *(dJ+k0);
    x = TT*UU;;;
    resultI += x * *(dI+k0+k1);
    resultJ += x * *(dJ+k0+k1);
    x = x1*UU;;;
    resultI += x * *(dI+k1);
    resultJ += x * *(dJ+k1);
    return True;
  } else return False;
}

Bool Interpolate2D::interpCubicFloat (Float &result, 
                                      const Vector<Double> &where, 
                                      const Matrix<Float> &data,
                                      const Matrix<Bool>* &maskPtr) const
//
// bi-cubic interpolation
//
{
   const IPosition &shape = data.shape();

// We find 4 points surrounding the one of interest.
// Points are labelled:
//
//   1   2
//   0   3
//
// where point 0 is [i,j].
//
// we use points in a 4 x 4 grid in total (to get derivatives)
// [i-1,j-1] -> [i+2,j+2]

   Int i = Int(where[0]);
   Int j = Int(where[1]);

// Handle edge (and beyond) by using linear.

   if (i<=0 || i>=shape(0)-2 || j<=0 || j>=shape(1)-2) {
      return interpLinearFloat(result, where, data, maskPtr);
   }

// Handle mask

   if (anyBadMaskPixels(maskPtr, i-1, i+2, j-1, j+2)) return False;

// Do it

   Double d1 = 1.0;
   Double d2 = 1.0;
   Double TT = where[0] - i;
   Double UU = where[1] - j;

//
// define values of function and its derivatives on the
// square of points bounding "where"

   itsY(0) = data(i,  j);
   itsY(1) = data(i+1,j);
   itsY(2) = data(i+1,j+1);
   itsY(3) = data(i,  j+1);

// x-derivatives (points 0->3)

   itsY1(0) = data(i+1, j)   - data(i-1, j);
   itsY1(1) = data(i+2, j)   - data(i,   j);
   itsY1(2) = data(i+2, j+1) - data(i,   j+1);
   itsY1(3) = data(i+1, j+1) - data(i-1, j+1);

// y-derivatives (points 0->3)

   itsY2(0) = data(i,   j+1) - data(i,   j-1);
   itsY2(1) = data(i+1, j+1) - data(i+1, j-1);
   itsY2(2) = data(i+1, j+2) - data(i+1, j);
   itsY2(3) = data(i,   j+2) - data(i,   j);

// cross derivatives (points 0->3)

   itsY12(0) =  data(i+1, j+1) + data(i-1, j-1) -
                data(i-1, j+1) - data(i+1, j-1);
   itsY12(1) =  data(i+2, j+1) + data(i, j-1) -
                data(i, j+1) - data(i+2, j-1);
   itsY12(2) =  data(i+2, j+2) + data(i, j) -
                data(i, j+2) - data(i+2, j);
   itsY12(3) =  data(i+1, j+2) + data(i-1, j) - 
                data(i-1, j+2) - data(i+1, j);

// Get result

   bcucof(itsC, itsY, itsY1, itsY2, itsY12, d1, d2);
   result = 0.0;
   for (Int i=3; i>=0; i--) {
     result = TT*result + ((itsC(i,3)*UU + itsC(i,2))*UU + 
                            itsC(i,1))*UU + itsC(i,0);
   }
//
   return True;
}


Bool Interpolate2D::interpCubicDouble (Double &result, 
                                       const Vector<Double> &where, 
                                       const Matrix<Double> &data,
                                       const Matrix<Bool>* &maskPtr) const
//
// bi-cubic interpolation
//
{
   const IPosition &shape = data.shape();

// We find 4 points surrounding the one of interest.
// Points are labelled:
//
//   1   2
//   0   3
//
// where point 0 is [i,j].
//
// we use points in a 4 x 4 grid in total (to get derivatives)
// [i-1,j-1] -> [i+2,j+2]

   Int i = Int(where[0]);
   Int j = Int(where[1]);

// Handle edge (and beyond) by using linear.

   if (i<=0 || i>=shape(0)-2 || j<=0 || j>=shape(1)-2) {
      return interpLinearDouble(result, where, data, maskPtr);
   }

// Handle mask

   if (anyBadMaskPixels(maskPtr, i-1, i+2, j-1, j+2)) return False;

// Do it

   Double d1 = 1.0;
   Double d2 = 1.0;
   Double TT = where[0] - i;
   Double UU = where[1] - j;

//
// define values of function and its derivatives on the
// square of points bounding "where"

   itsY(0) = data(i,  j);
   itsY(1) = data(i+1,j);
   itsY(2) = data(i+1,j+1);
   itsY(3) = data(i,  j+1);

// x-derivatives (points 0->3)

   itsY1(0) = data(i+1, j)   - data(i-1, j);
   itsY1(1) = data(i+2, j)   - data(i,   j);
   itsY1(2) = data(i+2, j+1) - data(i,   j+1);
   itsY1(3) = data(i+1, j+1) - data(i-1, j+1);

// y-derivatives (points 0->3)

   itsY2(0) = data(i,   j+1) - data(i,   j-1);
   itsY2(1) = data(i+1, j+1) - data(i+1, j-1);
   itsY2(2) = data(i+1, j+2) - data(i+1, j);
   itsY2(3) = data(i,   j+2) - data(i,   j);

// cross derivatives (points 0->3)

   itsY12(0) =  data(i+1, j+1) + data(i-1, j-1) -
                data(i-1, j+1) - data(i+1, j-1);
   itsY12(1) =  data(i+2, j+1) + data(i, j-1) -
                data(i, j+1) - data(i+2, j-1);
   itsY12(2) =  data(i+2, j+2) + data(i, j) -
                data(i, j+2) - data(i+2, j);
   itsY12(3) =  data(i+1, j+2) + data(i-1, j) - 
                data(i-1, j+2) - data(i+1, j);

// Get result

   bcucof(itsC, itsY, itsY1, itsY2, itsY12, d1, d2);
   result = 0.0;
   for (Int i=3; i>=0; i--) {
     result = TT*result + ((itsC(i,3)*UU + itsC(i,2))*UU + 
                            itsC(i,1))*UU + itsC(i,0);
   }
//
   return True;
};



Bool Interpolate2D::interpNearestBool (Bool &result, 
                                       const Vector<Double> &where, 
                                       const Matrix<Bool> &data) const
{
  AlwaysAssert(where.nelements()==2, AipsError);
  const IPosition &shape = data.shape();

// Find nearest pixel; (i,j) = centre

  Int i = Int(where[0]+0.5);
  Int j = Int(where[1]+0.5);
  Bool ok = False;
  if (i >= 0 && i <= shape(0)-1 && j >= 0 && j <= shape(1)-1) {
    result = data(i,j);
    ok = True;
  }    
//
  return ok;
}


Bool Interpolate2D::interpLinearBool (Bool &result, 
                                      const Vector<Double> &where, 
                                      const Matrix<Bool> &data) const
{
   AlwaysAssert(where.nelements()==2, AipsError);
   const IPosition &shape = data.shape();

// Find nearest pixel; (i,j) = centre

   Int i = Int(where[0]+0.5);
   Int j = Int(where[1]+0.5);

// Handle edge. Just move start left/down by one,

   if (i==shape(0)-1) i--;
   if (j==shape(1)-1) j--;

// 2x2 starting from [i,j]

   Bool ok = False;
   if (i >= 0 && i+1 <= shape(0)-1 && j >= 0 && j+1 <= shape(1)-1) {
      result = !(!data(i,j) || !data(i+1,j) || !data(i,j+1) || !data(i+1,j+1));
      ok = True;
  }
//
  return ok;
}


Bool Interpolate2D::interpCubicBool (Bool &result, 
                                     const Vector<Double> &where, 
                                     const Matrix<Bool> &data) const
//
// bi-cubic interpolation
//
{
   AlwaysAssert(where.nelements()==2, AipsError);
   const IPosition &shape = data.shape();

// Find nearest pixel; (i,j) = centre

   Int i = Int(where[0]+0.5);
   Int j = Int(where[1]+0.5);

// Interpolation grid is 4x4 :  [i-1,j-1] -> [i+2,j+2]
// Handle edge (and beyond) by using linear.

   if (i<=0 || i>=shape(0)-2 || j<=0 || j>=shape(1)-2) {
      return interpLinearBool(result, where, data);
   }
//
   const Matrix<Bool>* p = &data;
   result = !(anyBadMaskPixels(p, i-1, i+2, j-1, j+2));
   return True;
}






void Interpolate2D::bcucof (Matrix<Double> &c, const Vector<Double> &y, const Vector<Double> &y1, 
                            const Vector<Double> &y2, const Vector<Double> &y12,
                            Double d1, Double d2) const
//
// Numerical recipes 3.6 (p99)
//
{
  static const double wt[16][16] =
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
  static Vector<Double> X(16), CL(16);
//
  uInt l, k, i, j;
  Double  xx, d1d2; 
  d1d2 = d1*d2;

// Pack temporary

  for (i=0;i<4;i++) {
    X[i] = y[i];
    X[i+4] = y1[i]*d1;
    X[i+8] = y2[i]*d2;
    X[i+12] = y12[i]*d1d2;
  }

 // Matrix multiply the stored table

  for (i=0;i<=15;i++) {
    xx = 0.0;
    for (k=0;k<=15;k++) {
       j = (15 * i) + k;
       xx += wt[i][k] * X[k];
    }
    CL[i] = xx;
  }

// Unpack the result into the output table

  l = 0;
  for (i=0;i<4;i++) {
    for (j=0;j<4;j++) {
      c(i,j) = CL[l++];
    }
  }
}



Interpolate2D::Method Interpolate2D::stringToMethod (const String &method)
{
   String typeU = method;
   typeU.upcase();
   String tmp = String(typeU.at(0, 1));
   Interpolate2D::Method method2;
   if (tmp==String("N")) {
      method2 = Interpolate2D::NEAREST;
   } else if (tmp==String("L")) {
      method2 = Interpolate2D::LINEAR;
   } else if (tmp==String("C")) {
      method2 = Interpolate2D::CUBIC;
   } else {
      throw(AipsError("Illegal method"));
   }
   return method2;
}


Bool Interpolate2D::anyBadMaskPixels (const Matrix<Bool>* &maskPtr,
                                      Int i1, Int i2, Int j1, Int j2) const
{
   if (maskPtr==0) return False;
   for (Int j=j1; j<=j2; j++) {
      for (Int i=i1; i<=i2; i++) {
         if (!(*maskPtr)(i,j)) return True;
       }
    }
    return False;
}  

