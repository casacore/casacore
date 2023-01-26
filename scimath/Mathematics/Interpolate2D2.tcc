//# Interpolate2D2.cc:  this implements Interpolate2D templates
//# Copyright (C) 2004
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

#ifndef SCIMATH_INTERPOLATE2D2_TCC
#define SCIMATH_INTERPOLATE2D2_TCC
 
#include <casacore/scimath/Mathematics/Interpolate2D.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Constants.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <typename T>
bool Interpolate2D::interpNearest(T &result, 
				  const Vector<double> &where,
				  const Matrix<T> &data,
				  const Matrix<bool>* &maskPtr) const {
  // definition of the 'neighborhood' of outer edge data elements.
  static const double half= .5001;

  const IPosition &shape = data.shape();

  double imax = shape(0) - 1.;
  double wi = where[0];
  if(wi < 0. - half || wi > imax + half || imax < 0) return false;

  double jmax = shape(1) - 1.;
  double wj = where[1];
  if(wj < 0 - half || wj > jmax + half || jmax < 0) return false;

  uint32_t i = (wi <= 0.)?		0
	 : (wi >= imax)?	uint32_t(imax)
	 :			uint32_t(wi + .5);
      
  uint32_t j = (wj <= 0.)?		0
	 : (wj >= jmax)?	uint32_t(jmax)
	 :			uint32_t(wj + .5);
      
  bool dataValid = !maskPtr || (*maskPtr)(i,j);
  if (dataValid) result = data(i,j);
  return dataValid;
}

template <typename T>
bool Interpolate2D::interpLinear(T &result, 
				 const Vector<double> &where, 
				 const Matrix<T> &data,
				 const Matrix<bool>* &maskPtr) const {
  const IPosition &shape = data.shape();

  // We find 4 points surrounding the one of interest.
  // Negatives will give big positive value for the uint32_t 
  // and these will fail the  shape test below.
  // Make sure we don't access i+1 or j+1 because the 
  // big positive plus 1 may become 0 and then we will spuriously
  // pass the shape test
  uint32_t i = int32_t(where[0]);               // Assuming int32_t does (1.2 -> 1)
  uint32_t j = int32_t(where[1]);
  uint32_t si = uint32_t(shape(0)-1);
  uint32_t sj = uint32_t(shape(1)-1);

  // Handle edge. Just move start left/down by one,
  if (i==si) --i;
  if (j==sj) --j;

  // 2x2 starting from [i,j]
  // mask==true is a good pixel
  if (i < si && j < sj) {                   
    if (maskPtr) {
      if (!(*maskPtr)(i,j) || !(*maskPtr)(i+1,j) ||
	  !(*maskPtr)(i,j+1) || !(*maskPtr)(i+1,j+1)) return false;
    }
    double TT = where[0] - i;
    double UU = where[1] - j;
    result = (1.0-TT)*(1.0-UU)*data(i,j) +
      TT*(1.0-UU)*data(i+1,j) +
      TT*UU*data(i+1,j+1) +
      (1.0-TT)*UU*data(i,j+1);
    return true;
  } else return false;
}

template <typename T>
bool Interpolate2D::interpLinear2(T &resultI, T &resultJ, 
				  const Vector<double> &where, 
				  const Matrix<T> &dataI,
				  const Matrix<T> &dataJ,
				  const Matrix<bool> &mask) const {
  const IPosition &shape = mask.shape();

  // We find 4 points surrounding the one of interest.
  // Negatives will give big positive value for the uint32_t 
  // and these will fail the  shape test below.
  // Make sure we don't access i+1 or j+1 because the 
  // big positive plus 1 may become 0 and then we will spuriously
  // pass the shape test

  uint32_t i = int32_t(where[0]);               // Assuming int32_t does (1.2 -> 1)
  uint32_t j = int32_t(where[1]);
  uint32_t si = uint32_t(shape[0]-1);
  uint32_t sj = uint32_t(shape[1]-1);
  // Handle edge. Just move start left/down by one,
  if (i==si) --i;
  if (j==sj) --j;
  // 2x2 starting from [i,j]
  // mask==true is a good pixel
  if (i < si && j < sj) {
    uint32_t k0 = dataI.steps()[0];
    uint32_t k1 = dataI.steps()[1];
    const bool *m = &mask(i,j);
    if ( !*m || !*(m+k0) || !*(m+k1) || !*(m+k0+k1)) return false;
    double TT = where[0] - i;
    double UU = where[1] - j;
    double x1 = (1.0-TT);
    double y1 = (1.0-UU);
    double x = x1*y1;
    const T *dI = &dataI(i,j);
    const T *dJ = &dataJ(i,j);
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
    return true;
  } else return false;
}

template <typename T>
bool Interpolate2D::interpCubic(T &result, 
				const Vector<double> &where, 
				const Matrix<T> &data,
				const Matrix<bool>* &maskPtr) const {
  //
  // bi-cubic interpolation
  //
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
   
   int32_t i = int32_t(where[0]);
   int32_t j = int32_t(where[1]);
   
   // Handle edge (and beyond) by using linear.
   
   if (i<=0 || i>=shape[0]-2 || j<=0 || j>=shape[1]-2) {
     return interpLinear<T>(result, where, data, maskPtr);
   }
   
   // Handle mask
   
   if (anyBadMaskPixels(maskPtr, i-1, i+2, j-1, j+2)) return false;
   
   // Do it
   
   double TT = where[0] - i;
   double UU = where[1] - j;
   
   double itsY[4];
   double itsY1[4];
   double itsY2[4];
   double itsY12[4];
   double itsC[4][4];
   //
   // define values of function and its derivatives on the
   // square of points bounding "where"
   
   itsY[0] = data(i,  j);
   itsY[1] = data(i+1,j);
   itsY[2] = data(i+1,j+1);
   itsY[3] = data(i,  j+1);
   
   // x-derivatives (points 0->3)
   
   itsY1[0] = data(i+1, j)   - data(i-1, j);
   itsY1[1] = data(i+2, j)   - data(i,   j);
   itsY1[2] = data(i+2, j+1) - data(i,   j+1);
   itsY1[3] = data(i+1, j+1) - data(i-1, j+1);
   
   // y-derivatives (points 0->3)
   
   itsY2[0] = data(i,   j+1) - data(i,   j-1);
   itsY2[1] = data(i+1, j+1) - data(i+1, j-1);
   itsY2[2] = data(i+1, j+2) - data(i+1, j);
   itsY2[3] = data(i,   j+2) - data(i,   j);
   
   // cross derivatives (points 0->3)
   
   itsY12[0] =  data(i+1, j+1) + data(i-1, j-1) -
     data(i-1, j+1) - data(i+1, j-1);
   itsY12[1] =  data(i+2, j+1) + data(i, j-1) -
     data(i, j+1) - data(i+2, j-1);
   itsY12[2] =  data(i+2, j+2) + data(i, j) -
     data(i, j+2) - data(i+2, j);
   itsY12[3] =  data(i+1, j+2) + data(i-1, j) - 
     data(i-1, j+2) - data(i+1, j);
   for (uint32_t i=0; i<4; ++i) {
     itsY1[i]  /= 2.0;
     itsY2[i]  /= 2.0;
     itsY12[i] /= 4.0;
   }
   
   // Get result
   
   bcucof(itsC, itsY, itsY1, itsY2, itsY12);
   result = 0.0;
   for (int32_t i=3; i>=0; --i) {
     result = TT*result + ((itsC[i][3]*UU + itsC[i][2])*UU + 
			   itsC[i][1])*UU + itsC[i][0];
   }
   //
   return true;
}

template <typename T>
bool Interpolate2D::interpLanczos(T &result, 
        const Vector<double> &where, 
        const Matrix<T> &data,
        const Matrix<bool>* &maskPtr) const {
    //
    // Lanczos 2D interpolation
    //

    // Hardcoded kernel size
    const double a = 3;

    const IPosition& shape = data.shape();
    const double x = where[0];
    const double y = where[1];
    const T floorx = std::floor(x);
    const T floory = std::floor(y);

    // Handle mask
    if (anyBadMaskPixels(maskPtr, x-a+1, x+a, y-a+1, y+a)) return false;

    // Where we can't sum over the full support of the kernel due to proximity
    // to the edge, set the pixel value to zero. This is just one way of
    // dealing with edge effects, another could be to revert to linear
    // interpolation.
    if (floorx < a || floorx >= shape[0] - a || floory < a || floory >= shape[1] - a) {
        result = 0;
        return true;
    }

    // Interpolate
    result = 0;
    for (T i = floorx - a + 1; i <= floorx + a; ++i) {
        for (T j = floory - a + 1; j <= floory + a; ++j) {
            result += data(i, j) * L(x - i, a) * L(y - j, a);
        }
    }

    return true;
}

// Lanczos interpolation: helper function
template <typename T>
T Interpolate2D::sinc(const T x) const {
    if (x == 0) {
        return 1;
    }
    return sin(C::pi * x) / (C::pi * x);
}

// Lanczos interpolation: helper function
template <typename T>
T Interpolate2D::L(const T x, const int32_t a) const {
    if (-a < x && x < a) {
        return sinc(x) * sinc (x/a);
    }
    return 0;
}

} //# NAMESPACE CASACORE - END


#endif
