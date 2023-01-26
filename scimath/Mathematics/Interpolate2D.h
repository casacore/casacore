//# Interpolate2D.h: this defines the Interpolate2D class
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

#ifndef SCIMATH_INTERPOLATE2D_H
#define SCIMATH_INTERPOLATE2D_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class String;

// <summary>
// A two dimension interpolator for Matrices or Arrays
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="2004/05/26" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=Array>Arrays</linkto>
// </prerequisite>
//
// <etymology>
// This class is called Interpolate2D because it does 2 dimensional interpolations
// </etymology>
//
// <synopsis>
// Given a regular Array or Matrix and a vector of pixel
// coordinates, interpolate the values of that array/matrix onto those
// pixel coordinates.
//
// Absolutely no checking of the consistency of the input data
// is done in order to preserve maximum speed.   The coordinate vector
// *must* have at least 2 elements (others will be ignored). If 
// you supply data and mask, those arrays *must* be the same shape.
// Failure to follow these rules will result in your program 
// crashing.
// </synopsis>
//
// <example>
// <srcblock>
//
// Matrix<float> matt(10,10);
// Vector<double> where(2);
// where(0) = 3.452;  where(1) = 6.1;
// Interpolate2D myInterp(Interpolate2D::LINEAR);
// float result;
// bool ok = myInterp.interp(result, where, matt);
//
// </srcblock> 
// </example>
//
// <motivation>
// 2-D interpolation is required in geometry transformation routines
// such as in ImageRegrid.
// </motivation>
//
//
// <todo asof="1998/08/02">
//   <li> Alternative approach: instantiate with an Array, take a block of
//        vector locations, return a block of interpolation results
// </todo>


class Interpolate2D {
 public:

  enum Method {

    // Nearest neighbour
    NEAREST,  
    
    // Bilinear 
    LINEAR, 
    
    // Bicubic 
    CUBIC,

    // Lanczos
    LANCZOS};
  
  // Constructor
  Interpolate2D(Interpolate2D::Method method=Interpolate2D::LINEAR);
  
  // Copy constructor (copy semantics)
  Interpolate2D(const Interpolate2D &other);
  
  // destructor
  ~Interpolate2D();
  
  // Assignment operator (copy semantics)
  Interpolate2D &operator=(const Interpolate2D &other);
  
  // Do one float interpolation, supply Matrix and mask (true is good),
  // and pixel coordinate.  Returns false if coordinate out of range or data 
  // are masked.  No shape integrity checking is done (see above).
  // <group>
  bool interp (float &result, 
               const Vector<double> &where,
               const Matrix<float> &data) const;
  bool interp (float &result, 
               const Vector<double> &where,
               const Matrix<float> &data, 
               const Matrix<bool> &mask) const;
  // </group>
  
  // Do one double interpolation, supply Matrix/Array and mask (true is good),
  // and pixel coordinate.  Returns false if coordinate out of range or data 
  // are masked.  No shape integrity checking is done (see above).
  // <group>
  bool interp (double &result, 
               const Vector<double> &where,
               const Matrix<double> &data) const;
  bool interp (double &result, 
               const Vector<double> &where,
               const Matrix<double> &data, 
               const Matrix<bool> &mask) const;
  // </group>

  // Do one Complex interpolation, supply Matrix/Array and mask (true is good),
  // and pixel coordinate.  Returns false if coordinate out of range or data
  // are masked.  No shape integrity checking is done (see above). The real
  // and imaginary parts are treated independently (see CAS-11375).
  // <group>
  bool interp (Complex &result,
               const Vector<double> &where,
               const Matrix<Complex> &data) const;
  bool interp (Complex &result,
               const Vector<double> &where,
               const Matrix<Complex> &data,
               const Matrix<bool> &mask) const;
  // </group>

  // Do one DComplex interpolation, supply Matrix/Array and mask (true is good),
  // and pixel coordinate.  Returns false if coordinate out of range or data
  // are masked.  No shape integrity checking is done (see above). The real
  // and imaginary parts are treated independently (see CAS-11375).
  // <group>
  bool interp (DComplex &result,
               const Vector<double> &where,
               const Matrix<DComplex> &data) const;
  bool interp (DComplex &result,
               const Vector<double> &where,
               const Matrix<DComplex> &data,
               const Matrix<bool> &mask) const;
  // </group>

  // Do two linear interpolations simultaneously. The second call is direct.
  // The first call transfers to the second call. It is assumed that the
  // structure (shape, steps) of the mask and data files are the same.
  // <group>
  bool interp(double &resultI, double &resultJ, 
	      const Vector<double> &where, 
	      const Matrix<double> &dataI,
	      const Matrix<double> &dataJ,
	      const Matrix<bool> &mask) const;
  template <typename T>
  bool interpLinear2(T &resultI, T &resultJ, 
		     const Vector<double> &where, 
		     const Matrix<T> &dataI,
		     const Matrix<T> &dataJ,
		     const Matrix<bool> &mask) const;
  // </group>
  
  // Do one interpolation, supply boolean Matrix (true is good),
  // and pixel coordinate.  Returns false if coordinate
  // out of range. The result is false if any data value in the interpolation
  // grid are false (bad), else true.  No shape integrity checking is done.
  // <group>
  bool  interp (bool &result, 
                const Vector<double> &where,
                const Matrix<bool> &data) const;
  // </group>
  
  // Convert string ("nearest", "linear", "cubic", "lanczos") to interpolation
  // method. The match is case insensitive.
  static Interpolate2D::Method stringToMethod(const String &method);
  
 private:
  
  // Are any of the mask pixels bad ? Returns false if no mask.
  bool anyBadMaskPixels (const Matrix<bool>* &mask, int32_t i1, int32_t i2,
			 int32_t j1, int32_t j2) const;
  
  // nearest neighbour interpolation
  template <typename T>
  bool interpNearest(T &result, const Vector<double> &where,
		     const Matrix<T> &data,
		     const Matrix<bool>* &maskPtr) const;
  bool interpNearestBool(bool &result, const Vector<double> &where,
			 const Matrix<bool> &data) const;

  // bi-linear interpolation 
  template <typename T>
  bool interpLinear(T &result, const Vector<double> &where,
		    const Matrix<T> &data,
		    const Matrix<bool>* &maskPtr) const;
  bool interpLinearBool(bool &result, const Vector<double> &where,
			const Matrix<bool> &data) const;
  
  // bi-cubic interpolation
  template <typename T>
    bool interpCubic(T &result, const Vector<double> &where,
		     const Matrix<T> &data,
		     const Matrix<bool>* &maskPtr) const;
  bool interpCubicBool(bool &result, const Vector<double> &where,
		       const Matrix<bool> &data) const;

  // Lanczos interpolation
  template <typename T>
  bool interpLanczos(T &result, const Vector<double> &where,
		     const Matrix<T> &data,
		     const Matrix<bool>* &maskPtr) const;
  bool interpLanczosBool(bool &result, const Vector<double> &where,
		       const Matrix<bool> &data) const;
  // Lanczos interpolation: helper functions
  template <typename T>
  T sinc(const T x) const;
  template <typename T>
  T L(const T x, const int32_t a) const;

  // helping routine from numerical recipes
  void bcucof (double c[4][4], const double y[4],
	       const double y1[4], 
               const double y2[4], const double y12[4]) const;

  // Typedefs for function pointers
  typedef bool(Interpolate2D::*FuncPtrFloat)
    (float &result, 
     const Vector<double> &where, 
     const Matrix<float> &data,
     const Matrix<bool>* &maskPtr) const;
  typedef bool(Interpolate2D::*FuncPtrDouble)
    (double &result, 
     const Vector<double> &where, 
     const Matrix<double> &data,
     const Matrix<bool>* &maskPtr) const;
  typedef bool(Interpolate2D::*FuncPtrBool)
    (bool &result, 
     const Vector<double> &where, 
     const Matrix<bool> &data) const;
  //
  FuncPtrFloat itsFuncPtrFloat;
  FuncPtrDouble itsFuncPtrDouble;
  FuncPtrBool itsFuncPtrBool;

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/Interpolate2D2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

