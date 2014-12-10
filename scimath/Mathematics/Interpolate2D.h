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
//#
//# $Id$

#ifndef SCIMATH_INTERPOLATE2D_H
#define SCIMATH_INTERPOLATE2D_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template <typename T> class Vector;
template <typename T> class Matrix;
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
// Matrix<Float> matt(10,10);
// Vector<Float> where(2);  
// where(0) = 3.452;  where(1) = 6.1;
// Interpolate2D myInterp(Interpolate2D::LINEAR);
// Float result;
// Bool ok = myInterp(result, where, matt);
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
//   <li> Now that there are float/double/bool versions, the class should
//        be templated and specialized versions made as needed. The
//        code duplucation in the Float/Double versions is pretty awful presently.
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
  
  // Do one Float interpolation, supply Matrix and mask (True is good),
  // and pixel coordinate.  Returns False if coordinate out of range or data 
  // are masked.  No shape integrity checking is done (see above).
  // <group>
  Bool interp (Float &result, 
               const Vector<Double> &where,
               const Matrix<Float> &data) const;
  Bool interp (Float &result, 
               const Vector<Double> &where,
               const Matrix<Float> &data, 
               const Matrix<Bool> &mask) const;
  // </group>
  
  // Do one Double interpolation, supply Matrix/Array and mask (True is good),
  // and pixel coordinate.  Returns False if coordinate out of range or data 
  // are masked.  No shape integrity checking is done (see above).
  // <group>
  Bool interp (Double &result, 
               const Vector<Double> &where,
               const Matrix<Double> &data) const;
  Bool interp (Double &result, 
               const Vector<Double> &where,
               const Matrix<Double> &data, 
               const Matrix<Bool> &mask) const;
  // </group>
  // Do two linear interpolations simultaneously. The second call is direct.
  // The first call transfers to the second call. It is assumed that the
  // structure (shape, steps) of the mask and data files are the same.
  // <group>
  Bool interp(Double &resultI, Double &resultJ, 
	      const Vector<Double> &where, 
	      const Matrix<Double> &dataI,
	      const Matrix<Double> &dataJ,
	      const Matrix<Bool> &mask) const;
  template <typename T>
  Bool interpLinear2(T &resultI, T &resultJ, 
		     const Vector<Double> &where, 
		     const Matrix<T> &dataI,
		     const Matrix<T> &dataJ,
		     const Matrix<Bool> &mask) const;
  // </group>
  
  // Do one interpolation, supply boolean Matrix (True is good),
  // and pixel coordinate.  Returns False if coordinate
  // out of range. The result is False if any data value in the interpolation
  // grid are False (bad), else True.  No shape integrity checking is done.
  // <group>
  Bool  interp (Bool &result, 
                const Vector<Double> &where,
                const Matrix<Bool> &data) const;
  // </group>
  
  // Recover interpolation method
  Method interpolationMethod() const {return itsMethod;}
  
  // Convert string ("nearest", "linear", "cubic") to interpolation method
  // Minimum match will do.
  static Interpolate2D::Method stringToMethod(const String &method);
  
 private:
  
  // Are any of the mask pixels bad ? Returns False if no mask.
  Bool anyBadMaskPixels (const Matrix<Bool>* &mask, Int i1, Int i2,
			 Int j1, Int j2) const;
  
  // nearest neighbour interpolation
  template <typename T>
  Bool interpNearest(T &result, const Vector<Double> &where,
		     const Matrix<T> &data,
		     const Matrix<Bool>* &maskPtr) const;
  Bool interpNearestBool(Bool &result, const Vector<Double> &where,
			 const Matrix<Bool> &data) const;

  // bi-linear interpolation 
  template <typename T>
  Bool interpLinear(T &result, const Vector<Double> &where,
		    const Matrix<T> &data,
		    const Matrix<Bool>* &maskPtr) const;
  Bool interpLinearBool(Bool &result, const Vector<Double> &where,
			const Matrix<Bool> &data) const;
  
  // bi-cubic interpolation
  template <typename T>
    Bool interpCubic(T &result, const Vector<Double> &where,
		     const Matrix<T> &data,
		     const Matrix<Bool>* &maskPtr) const;
  Bool interpCubicBool(Bool &result, const Vector<Double> &where,
		       const Matrix<Bool> &data) const;

  // Lanczos interpolation
  template <typename T>
  Bool interpLanczos(T &result, const Vector<Double> &where,
		     const Matrix<T> &data,
		     const Matrix<Bool>* &maskPtr) const;
  Bool interpLanczosBool(Bool &result, const Vector<Double> &where,
		       const Matrix<Bool> &data) const;
  // Lanczos interpolation: helper functions
  template <typename T>
  T sinc(const T x) const;
  template <typename T>
  T L(const T x, const Int a) const;

  // helping routine from numerical recipes
  void bcucof (Double c[4][4], const Double y[4],
	       const Double y1[4], 
               const Double y2[4], const Double y12[4]) const;
  //
  Interpolate2D::Method itsMethod;
  
  // Typedefs for function pointers
  typedef Bool(Interpolate2D::*FuncPtrFloat)
    (Float &result, 
     const Vector<Double> &where, 
     const Matrix<Float> &data,
     const Matrix<Bool>* &maskPtr) const;
  typedef Bool(Interpolate2D::*FuncPtrDouble)
    (Double &result, 
     const Vector<Double> &where, 
     const Matrix<Double> &data,
     const Matrix<Bool>* &maskPtr) const;
  typedef Bool(Interpolate2D::*FuncPtrBool)
    (Bool &result, 
     const Vector<Double> &where, 
     const Matrix<Bool> &data) const;
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

