//# Interpolate2D.cc:  this implements Interpolate2D
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2004
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
 
#include <casacore/scimath/Mathematics/Interpolate2D.h>

#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Interpolate2D::Interpolate2D(Interpolate2D::Method method) {

// Set up function pointers to correct method

  if (method==Interpolate2D::LINEAR) {
    itsFuncPtrFloat = &Interpolate2D::interpLinear<float>;
    itsFuncPtrDouble = &Interpolate2D::interpLinear<double>;
    itsFuncPtrBool = &Interpolate2D::interpLinearBool;
  } else if (method==Interpolate2D::CUBIC) {
    itsFuncPtrFloat = &Interpolate2D::interpCubic<float>;
    itsFuncPtrDouble = &Interpolate2D::interpCubic<double>;
    itsFuncPtrBool = &Interpolate2D::interpCubicBool;
  } else if (method==Interpolate2D::NEAREST) {
    itsFuncPtrFloat = &Interpolate2D::interpNearest<float>;
    itsFuncPtrDouble = &Interpolate2D::interpNearest<double>;
    itsFuncPtrBool = &Interpolate2D::interpNearestBool;
  } else if (method==Interpolate2D::LANCZOS) {
    itsFuncPtrFloat = &Interpolate2D::interpLanczos<float>;
    itsFuncPtrDouble = &Interpolate2D::interpLanczos<double>;
    itsFuncPtrBool = &Interpolate2D::interpLanczosBool;
  }
}

Interpolate2D::Interpolate2D(const Interpolate2D &other)
: itsFuncPtrFloat (other.itsFuncPtrFloat),
  itsFuncPtrDouble(other.itsFuncPtrDouble),
  itsFuncPtrBool  (other.itsFuncPtrBool)
{}

Interpolate2D::~Interpolate2D()
{}

Interpolate2D &Interpolate2D::operator=(const Interpolate2D &other)
{
   itsFuncPtrFloat  = other.itsFuncPtrFloat;
   itsFuncPtrDouble = other.itsFuncPtrDouble;
   itsFuncPtrBool   = other.itsFuncPtrBool;
   return *this;
}



// float Versions

bool Interpolate2D::interp(float &result, 
                           const Vector<double> &where, 
                           const Matrix<float> &data) const
{
  const Matrix<bool>* maskPtr(0);
  return ((*this).*itsFuncPtrFloat)(result, where, data, maskPtr);
}


bool Interpolate2D::interp(float &result, 
                           const Vector<double> &where, 
                           const Matrix<float> &data,
                           const Matrix<bool> &mask) const
{
  const Matrix<bool>* maskPtr = &mask;
  return ((*this).*itsFuncPtrFloat)(result, where, data, maskPtr);
}


// double versions

bool Interpolate2D::interp(double &result, 
                           const Vector<double> &where, 
                           const Matrix<double> &data) const {
  const Matrix<bool>* maskPtr(0);
  return ((*this).*itsFuncPtrDouble)(result, where, data, maskPtr);
}


bool Interpolate2D::interp(double &result, 
                           const Vector<double> &where, 
                           const Matrix<double> &data,
                           const Matrix<bool> &mask) const {
  const Matrix<bool>* maskPtr = &mask;
  return ((*this).*itsFuncPtrDouble)(result, where, data, maskPtr);
}

// Complex versions
bool Interpolate2D::interp(
    Complex &result, const Vector<double> &where,
    const Matrix<Complex> &data
) const {
    float realRes, imagRes;
    Matrix<float> realData = (Matrix<float>)real(data);
    Matrix<float> imagData = (Matrix<float>)imag(data);
    const Matrix<bool>* maskPtr(0);
    bool realFunc = ((*this).*itsFuncPtrFloat)(realRes, where, realData, maskPtr);
    if (! realFunc) {
        return false;
    }
    bool imagFunc = ((*this).*itsFuncPtrFloat)(imagRes, where, imagData, maskPtr);
    if (! imagFunc) {
        return false;
    }
    result = Complex(realRes, imagRes);
    return true;
}

bool Interpolate2D::interp(
    Complex &result, const Vector<double> &where,
    const Matrix<Complex> &data, const Matrix<bool> &mask
) const {
    float realRes, imagRes;
    Matrix<float> realData = (Matrix<float>)real(data);
    Matrix<float> imagData = (Matrix<float>)imag(data);
    const Matrix<bool>* maskPtr = &mask;
    bool realFunc = ((*this).*itsFuncPtrFloat)(realRes, where, realData, maskPtr);
    if (! realFunc) {
        return false;
    }
    bool imagFunc = ((*this).*itsFuncPtrFloat)(imagRes, where, imagData, maskPtr);
    if (! imagFunc) {
        return false;
    }
    result = Complex(realRes, imagRes);
    return true;
}

// DComplex versions
bool Interpolate2D::interp(
    DComplex &result, const Vector<double> &where,
    const Matrix<DComplex> &data
) const {
    double realRes, imagRes;
    Matrix<double> realData = (Matrix<double>)real(data);
    Matrix<double> imagData = (Matrix<double>)imag(data);
    const Matrix<bool>* maskPtr(0);
    bool realFunc = ((*this).*itsFuncPtrDouble)(realRes, where, realData, maskPtr);
    if (! realFunc) {
        return false;
    }
    bool imagFunc = ((*this).*itsFuncPtrDouble)(imagRes, where, imagData, maskPtr);
    if (! imagFunc) {
        return false;
    }
    result = DComplex(realRes, imagRes);
    return true;
}

bool Interpolate2D::interp(
    DComplex &result, const Vector<double> &where,
    const Matrix<DComplex> &data, const Matrix<bool> &mask
) const {
    double realRes, imagRes;
    Matrix<double> realData = (Matrix<double>)real(data);
    Matrix<double> imagData = (Matrix<double>)imag(data);
    const Matrix<bool>* maskPtr = &mask;
    bool realFunc = ((*this).*itsFuncPtrDouble)(realRes, where, realData, maskPtr);
    if (! realFunc) {
        return false;
    }
    bool imagFunc = ((*this).*itsFuncPtrDouble)(imagRes, where, imagData, maskPtr);
    if (! imagFunc) {
        return false;
    }
    result = DComplex(realRes, imagRes);
    return true;
}

// double version with two identicals and mask

bool Interpolate2D::interp(double &resultI, double &resultJ, 
                           const Vector<double> &where, 
                           const Matrix<double> &dataI,
                           const Matrix<double> &dataJ,
                           const Matrix<bool> &mask) const {
  return interpLinear2<double>(resultI, resultJ, where, dataI, dataJ, mask);
}



// bool versions


bool Interpolate2D::interp(bool &result, 
                           const Vector<double> &where, 
                           const Matrix<bool> &data) const
{
  return ((*this).*itsFuncPtrBool)(result, where, data);
}



// Private functions

bool Interpolate2D::interpNearestBool(bool &result, 
				      const Vector<double> &where, 
				      const Matrix<bool> &data) const {
  AlwaysAssert(where.nelements()==2, AipsError);
  const IPosition &shape = data.shape();
  
  // Find nearest pixel; (i,j) = centre
  
  int32_t i = int32_t(where[0]+0.5);
  int32_t j = int32_t(where[1]+0.5);
  bool ok = false;
  if (i >= 0 && i <= shape(0)-1 && j >= 0 && j <= shape(1)-1) {
    result = data(i,j);
    ok = true;
  }    
  //
  return ok;
}

bool Interpolate2D::interpLinearBool(bool &result, 
				     const Vector<double> &where, 
				     const Matrix<bool> &data) const {
  AlwaysAssert(where.nelements()==2, AipsError);
  const IPosition &shape = data.shape();
  
  // Find nearest pixel; (i,j) = centre
  
  int32_t i = int32_t(where[0]+0.5);
  int32_t j = int32_t(where[1]+0.5);
  
  // Handle edge. Just move start left/down by one,
  
  if (i==shape(0)-1) i--;
  if (j==shape(1)-1) j--;
  
  // 2x2 starting from [i,j]
  
  bool ok = false;
  if (i >= 0 && i+1 <= shape(0)-1 && j >= 0 && j+1 <= shape(1)-1) {
    result = !(!data(i,j) || !data(i+1,j) || !data(i,j+1) || !data(i+1,j+1));
    ok = true;
  }
  //
  return ok;
}

bool Interpolate2D::interpCubicBool(bool &result, 
				    const Vector<double> &where, 
				    const Matrix<bool> &data) const {
  //
  // bi-cubic interpolation
  //
  AlwaysAssert(where.nelements()==2, AipsError);
  const IPosition &shape = data.shape();
  
  // Find nearest pixel; (i,j) = centre
  
  int32_t i = int32_t(where[0]+0.5);
  int32_t j = int32_t(where[1]+0.5);
  
  // Interpolation grid is 4x4 :  [i-1,j-1] -> [i+2,j+2]
  // Handle edge (and beyond) by using linear.
  
  if (i<=0 || i>=shape(0)-2 || j<=0 || j>=shape(1)-2) {
    return interpLinearBool(result, where, data);
  }
  //
  const Matrix<bool>* p = &data;
  result = !(anyBadMaskPixels(p, i-1, i+2, j-1, j+2));
  return true;
}

bool Interpolate2D::interpLanczosBool(bool &/*result*/,
        const Vector<double> &/*where*/, 
        const Matrix<bool> &/*data*/) const {
    throw(AipsError("Interpolate2D::interpLanczosBool() is not implemented"));
}

void Interpolate2D::bcucof (double c[4][4], const double y[4],
			    const double y1[4], 
                            const double y2[4],
			    const double y12[4]) const {
  //
  // Numerical recipes 3.6 (p99)
  //
  static const double wt[16][16] =
  { {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
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
    {4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1} };
  static double X[16], CL[16];
  
  // Pack temporary
  for (uint32_t i=0; i<4; ++i) {
    X[i] = y[i];
    X[i+4] = y1[i];
    X[i+8] = y2[i];
    X[i+12] = y12[i];
  }
  
  // Matrix multiply the stored table
  
  for (uint32_t i=0; i<16; ++i) {
    CL[i] = 0.0;
    for (uint32_t k=0; k<16; ++k) CL[i] += wt[i][k] * X[k];
  }
  
  // Unpack the result into the output table
  
  for (uint32_t i=0, l=0; i<4; ++i)
    for (uint32_t j=0; j<4; ++j) c[i][j] = CL[l++];
}



Interpolate2D::Method Interpolate2D::stringToMethod (const String &method) {
  String typeU = method;
  typeU.upcase();
  String tmp = String(typeU.at(0, 1));
  Interpolate2D::Method method2;
  if (tmp==String("N")) {
    method2 = Interpolate2D::NEAREST;
  } else if (tmp==String("L")) {
    String tmp2 = String(typeU.at(1, 1));
    if (tmp2==String("A")) {
      method2 = Interpolate2D::LANCZOS;
    } else {
      method2 = Interpolate2D::LINEAR;
    }
  } else if (tmp==String("C")) {
    method2 = Interpolate2D::CUBIC;
  } else if (tmp==String("Z")) {
    method2 = Interpolate2D::LANCZOS;
  } else {
    throw AipsError("Unknown interpolation method " + method);
  }
  return method2;
}


bool Interpolate2D::anyBadMaskPixels (const Matrix<bool>* &maskPtr,
                                      int32_t i1, int32_t i2, int32_t j1, int32_t j2) const {
  if (maskPtr) {
    for (int32_t j=j1; j<=j2; ++j)
      for (int32_t i=i1; i<=i2; ++i) if (!(*maskPtr)(i,j)) return true;
  }
  return false;
}  

} //# NAMESPACE CASACORE - END

