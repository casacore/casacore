//# Interpolate2D.h: this defines the Interpolate2D class
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
  Interpolate2D(Interpolate2D::Method method);

  // Copy constructor (copy semantics)
  Interpolate2D(const Interpolate2D& other);
 
  // destructor
  ~Interpolate2D();

  // Assignment operator (copy semantics)
  Interpolate2D& operator=(const Interpolate2D& other);

  // Interpolation methods return status of interpolation
  // <group>
  // do one interpolation, supply Matrix
  Bool  interp(T& result, const Vector<Double>& where, 
               const Matrix<T>& data);

  // do one interpolation, supply Array
  Bool interp(T& result, const Vector<Double>& where, const Array<T>& data);

  // do many interpolations, supply Matrix
//Bool interp(Vector<T>& result,  Interpolate2D::Method method, 
//              const Block<Vector<Double> >& where, const Matrix<T>& data);

  // do many interpolations, supply Array
//Bool interp(Vector<T>& result, Interpolate2D::Method method, 
//              const Block< Vector<Double> >& where, const Array<T>& data);
  // </group>

// Convert string ("nearest", "linear", "cubic") to Method
// The first 3 letters will do.
  static Interpolate2D<T>::Method stringToMethod(const String& method);


private:

  Bool ok();
  
  // checks that "where" is sufficiently within data, considering itsOrder;
  // If not, return false
  Bool check(const Vector<Double>& where, const Matrix<T>& data);


  // nearest neighbour interpolation
  Bool interpNearest(T& result, const Vector<Double>& where, const Matrix<T>& data);
  // bi-linear interpolation
  Bool interpLinear(T& result, const Vector<Double>& where, const Matrix<T>& data);

  // bi-cubic interpolation
  Bool interpCubic(T& result, const Vector<Double>& where, const Matrix<T>& data);

  // helping routine from numerical recipes
  void bcucof (Vector<T> y, Vector<T> y1, Vector<T> y2, Vector<T> y12,
	       Double d1, Double d2, Matrix<T> c);

  // initialize temporarys
  void initArrays();

//
  Interpolate2D<T>::Method itsMethod;
  mutable Int its1I, its1J;                      // interp1
  mutable Double its1T, its1U;                   // interp1
  mutable Int its2I, its2J;                      // interp2
  mutable Double its2T, its2U;                   // interp2
  mutable Vector<T> itsY, itsY1, itsY2, itsY12;  // interp2
  mutable Matrix<T> itsC;                        // interp2
  mutable Vector<T> itsCL, itsX;                 // bcucof 

};

#endif
