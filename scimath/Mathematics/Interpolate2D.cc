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
//#
//# $Id$
 
#include <casacore/scimath/Mathematics/Interpolate2D.h>

#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Interpolate2D::Interpolate2D(Interpolate2D::Method method) {

// Set up function pointers to correct method

  if (method==Interpolate2D::LINEAR) {
    itsFuncPtrFloat = &Interpolate2D::interpLinear<Float>;
    itsFuncPtrDouble = &Interpolate2D::interpLinear<Double>;
    itsFuncPtrBool = &Interpolate2D::interpLinearBool;
  } else if (method==Interpolate2D::CUBIC) {
    itsFuncPtrFloat = &Interpolate2D::interpCubic<Float>;
    itsFuncPtrDouble = &Interpolate2D::interpCubic<Double>;
    itsFuncPtrBool = &Interpolate2D::interpCubicBool;
  } else if (method==Interpolate2D::NEAREST) {
    itsFuncPtrFloat = &Interpolate2D::interpNearest<Float>;
    itsFuncPtrDouble = &Interpolate2D::interpNearest<Double>;
    itsFuncPtrBool = &Interpolate2D::interpNearestBool;
  } else if (method==Interpolate2D::LANCZOS) {
    itsFuncPtrFloat = &Interpolate2D::interpLanczos<Float>;
    itsFuncPtrDouble = &Interpolate2D::interpLanczos<Double>;
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
  return interpLinear2<Double>(resultI, resultJ, where, dataI, dataJ, mask);
}



// Bool versions


Bool Interpolate2D::interp(Bool &result, 
                           const Vector<Double> &where, 
                           const Matrix<Bool> &data) const
{
  return ((*this).*itsFuncPtrBool)(result, where, data);
}



// Private functions

Bool Interpolate2D::interpNearestBool(Bool &result, 
				      const Vector<Double> &where, 
				      const Matrix<Bool> &data) const {
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

Bool Interpolate2D::interpLinearBool(Bool &result, 
				     const Vector<Double> &where, 
				     const Matrix<Bool> &data) const {
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

Bool Interpolate2D::interpCubicBool(Bool &result, 
				    const Vector<Double> &where, 
				    const Matrix<Bool> &data) const {
  //
  // bi-cubic interpolation
  //
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

Bool Interpolate2D::interpLanczosBool(Bool &/*result*/,
        const Vector<Double> &/*where*/, 
        const Matrix<Bool> &/*data*/) const {
    throw(AipsError("Interpolate2D::interpLanczosBool() is not implemented"));
}

void Interpolate2D::bcucof (Double c[4][4], const Double y[4],
			    const Double y1[4], 
                            const Double y2[4],
			    const Double y12[4]) const {
  //
  // Numerical recipes 3.6 (p99)
  //
  static const Double wt[16][16] =
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
  static Double X[16], CL[16];
  
  // Pack temporary
  for (uInt i=0; i<4; ++i) {
    X[i] = y[i];
    X[i+4] = y1[i];
    X[i+8] = y2[i];
    X[i+12] = y12[i];
  }
  
  // Matrix multiply the stored table
  
  for (uInt i=0; i<16; ++i) {
    CL[i] = 0.0;
    for (uInt k=0; k<16; ++k) CL[i] += wt[i][k] * X[k];
  }
  
  // Unpack the result into the output table
  
  for (uInt i=0, l=0; i<4; ++i)
    for (uInt j=0; j<4; ++j) c[i][j] = CL[l++];
}



Interpolate2D::Method Interpolate2D::stringToMethod (const String &method) {
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
  } else if (tmp==String("Z")) {
    method2 = Interpolate2D::LANCZOS;
  } else {
    throw AipsError("Unknown interpolation method " + method);
  }
  return method2;
}


Bool Interpolate2D::anyBadMaskPixels (const Matrix<Bool>* &maskPtr,
                                      Int i1, Int i2, Int j1, Int j2) const {
  if (maskPtr) {
    for (Int j=j1; j<=j2; ++j)
      for (Int i=i1; i<=i2; ++i) if (!(*maskPtr)(i,j)) return True;
  }
  return False;
}  

} //# NAMESPACE CASACORE - END

