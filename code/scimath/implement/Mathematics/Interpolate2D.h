//# Interpolate2D.h: this defines the Interpolate2D class
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
//# $Id$

#if !defined(AIPS_INTERPOLATE2D_H)
#define AIPS_INTERPOLATE2D_H

//# Includes
#include <aips/aips.h>
#include <aips/Lattices/Lattice.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Array.h>


// <summary>
// A two dimension interpolator for Lattices, Matrices, or Arrays
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> Lattices
// </prerequisite>
//
// <etymology>
// This class is called Interpolate2D because it does 2 dimensional interpolations
// </etymology>
//
// <synopsis>
// Given a regular Array, Matrix, or Lattices and a vector of pixel
// coordinates, interpolate the values of that array/matrix/lattice onto those
// pixel coordinates.
// </synopsis>
//
// <example>
// <srcblock>
//
// Matrix matt(10,10);
// Vector where(2);  
// where(0) = 3.452;  where(1) = 6.1;
// Interpolate2D myInterp(iorder);
// Float result = myInterp(matt, where);
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


template<class T> class Interpolate2D
{
 public:

  enum Method {

// Nearest neighbour
    NEAREST,  

// Bilinear 
    LINEAR, 

// Bicubic 
    CUBIC};
 
  // Constructor
  Interpolate2D();

  // Copy constructor (copy semantics)
  Interpolate2D(const Interpolate2D& other);
 
  // destructor
  ~Interpolate2D();

  // Assignment operator (copy semantics)
  Interpolate2D& operator=(const Interpolate2D& other);

  // Interpolation methods return status of interpolation
  // do one interpolation, supply Matrix, pixel coordinate
  // and method
  // <group>
  Bool  interp(T& result, 
               const Vector<Double>& where,
               const Matrix<T>& data, 
               Interpolate2D<T>::Method method=Interpolate2D<T>::LINEAR) const;

  // do one interpolation, supply Matrix and mask (True is good),
  // pixel coordinate and method
  Bool  interp(T& result, 
               const Vector<Double>& where,
               const Matrix<T>& data, 
               const Matrix<Bool>& mask,
               Interpolate2D<T>::Method method=Interpolate2D<T>::LINEAR) const;

  // do one interpolation, supply Array, pixel coordinate and method
  Bool interp(T& result, 
              const Vector<Double>& where, 
              const Array<T>& data, 
              Interpolate2D<T>::Method method=Interpolate2D<T>::LINEAR) const;

  // do one interpolation, supply Array and mask (True is good), pixel
  // coordinate and method
  Bool interp(T& result, 
              const Vector<Double>& where, 
              const Array<T>& data, 
              const Array<Bool>& mask,
              Interpolate2D<T>::Method method=Interpolate2D<T>::LINEAR) const;
  //</group>

// Convert string ("nearest", "linear", "cubic") to Method
// Minimum match will do.
  static Interpolate2D<T>::Method stringToMethod(const String& method);

private:

  // Are any of the mask pixels bad ? Returns False if no mask.
  Bool anyBadMaskPixels (const Matrix<Bool>*& mask) const;

  // nearest neighbour interpolation
  Bool interpNearest(T& result, const Vector<Double>& where, const Matrix<T>& data,
                     const Matrix<Bool>*& maskPtr) const;

  // bi-linear interpolation
  Bool interpLinear(T& result, const Vector<Double>& where, const Matrix<T>& data,
                    const Matrix<Bool>*& maskPtr) const;

  // bi-cubic interpolation
  Bool interpCubic(T& result, const Vector<Double>& where, const Matrix<T>& data,
                    const Matrix<Bool>*& maskPtr) const;

  // helping routine from numerical recipes
  void bcucof (Matrix<T>& c, const Vector<T>& y, const Vector<T>& y1, 
               const Vector<T>& y2, const Vector<T>& y12,
	       Double d1, Double d2) const;
//
  Interpolate2D<T>::Method itsMethod;
//
  Int itsI, itsJ;
  Int itsMinI, itsMaxI, itsMinJ, itsMaxJ;
  Int itsII, itsJJ;

// These are private temporaries for cubic interpolation

  mutable Vector<T> itsY, itsY1, itsY2, itsY12;
  mutable Matrix<T> itsC;
};

#endif

