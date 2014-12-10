//# MVPosition.h: A 3D vector in space
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

#ifndef CASA_MVPOSITION_H
#define CASA_MVPOSITION_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RotMatrix;

//# Constants (SUN compiler does not accept non-simple default arguments)

// <summary> A 3D vector in space </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasValue>MeasValue</linkto>
//   <li> <linkto class=Vector>Vector</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Position
// </etymology>
//
// <synopsis>
// A MVPosition is a 3-vector of positions in a rectangular frame with
// internal units of m.<br>
// It can be constructed with:
// <ul>
//   <li> MVPosition() creates point at origin (0,0,0)
//   <li> MVPosition(MVPosition) creates a copy
//   <li> MVPosition(Double, Double, Double) creates (x,y,z) with
//		specified values
//   <li> MVPosition(Quantity length,Double, Double) creates a MVPosition assuming
//		that the two values are (in radians) angle along 'equator' 
//		and towards 'pole'. A length of zero will be made 1um.
//   <li> MVPosition(Quantity length, Quantity, Quantity) creates a MVPosition 
//		assuming angles as in previous, or positions
//   <li> <src>MVPosition(Quantity, Quantum<Vector<Double> >)</src> creates a 
//		MVPosition from angle vector, using first two angles, and 
//		assuming second as zero if not present, and pole if length 0.
//   <li> <src>MVPosition(Quantum<Vector<Double> ></src> creates from
//		angles or positions, depending on the units in the
//		quantum vector. In the angle case,
//		the data derived can be scaled with the readjust() function. If
//		the unit of the quantum vector is length, position is
//		assumed.
//    <li> <src>MVPosition(Vector<Double></src> creates from angles (less than
//		or equal to two elements) or x,y,z (3 elements).
//    <li> <src>MVPosition(Vector<Quantity></src> creates from length+angles,
//		angles, or x,y,z, depending on units.
// </ul>
// A void adjust(Double) function normalises the vector to a length of 1;
// a get() returns as a
// Double 3-vector the length and angles of the position vector;
// a getAngle() returns a Quantum 2-vector, (uInt) returns the indicated 
// element, and getValue returns the vector.<br>
// Positions can be added and subtracted.<br>
// The multiplication of two positions produces the in-product.<br>
// </synopsis>
//
// <example>
// See <linkto class=MPosition>Mposition</linkto> class.
// </example>
//
// <motivation>
// To do coordinate transformations
// </motivation>
//
// <todo asof="1996/02/04">
//	<li> See if not better to have a direction + length
// </todo>

class MVPosition : public MeasValue {	

public:
  //# Constants
  // Internal limts codes for negative height
  // <group>
  static const Double loLimit;
  static const Double hiLimit;
  // </group>
  //# Friends
  
  //# Constructors
  // Default constructor generates a (0,0,0) position
  MVPosition();
  // Copy constructor
  MVPosition(const MVPosition &other);
  // Creates a specified vector
  MVPosition(Double in0, Double in1, Double in2);
  // Creates a vector with specified length towards pole
  // <group>
  explicit MVPosition(Double in0);
  MVPosition(const Quantity &l);
  // </group>
  // Creates the position from specified (azimuth,elevation) angles and length
  MVPosition(const Quantity &l, Double angle0, Double angle1);
  // Creates the position from specified angles and length. or positions
  // <thrown>
  //    <li> AipsError if quantities not in angle format
  // </thrown>
  // <group>
  MVPosition(const Quantity &l, const Quantity &angle0, 
	     const Quantity &angle1);
  // If not enough angles: pole assumed (if none), or elevation =0 (if 1)
  MVPosition(const Quantum<Vector<Double> > &angle);
  MVPosition(const Quantity &l, const Quantum<Vector<Double> > &angle);
  // </group>
  // Create from specified length and/or angles and/or position
  // <group>
  explicit MVPosition(const Vector<Double> &other);
  MVPosition(const Vector<Quantity> &other);
  // </group>
  // Copy assignment
  MVPosition &operator=(const MVPosition &other);
  
  // Destructor
  ~MVPosition();
  
  //# Operators
  // Multiplication defined as in-product
  // <group>
  Double operator*(const MVPosition &other) const;
  // </group>
  
  // Equality comparisons
  // <group>
  Bool operator== (const MVPosition &other) const;
  Bool operator!= (const MVPosition &other) const;
  Bool near(const MVPosition &other, Double tol=1e-13) const;
  Bool near(const MVPosition &other, Quantity tol) const;
  Bool nearAbs(const MVPosition &other, Double tol=1e-13) const;
  // </group>
  
  // Addition and subtraction
  // <group>
  MVPosition operator-() const;
  MVPosition &operator+=(const MVPosition &right);
  MVPosition operator+(const MVPosition &right) const;
  MVPosition &operator-=(const MVPosition &right);
  MVPosition operator-(const MVPosition &right) const;
  // </group>
  
  // Multiplication with rotation matrix (see also global functions)
  // <group>
  MVPosition &operator*=(const RotMatrix &right);
  // </group>
  
  // Multiplication with constant
  // <group>
  MVPosition &operator*=(Double right);
  // </group>
  
  // Obtain an element
  // <group>
  Double &operator()(uInt which);
  const Double &operator()(uInt which) const;
  // </group>
  
  //# General Member Functions
  
  // Tell me your type
  // <group>
  virtual uInt type() const;
  static void assure(const MeasValue &in);
  // </group>
  
  // Normalise direction aspects by adjusting the length to 1
  // <group>
  // For position no adjustment; for direction adjustment
  virtual void adjust();
  // Adjustment with returned factor
  virtual void adjust(Double &res);
  // Re-adjust using factor given
  virtual void readjust(Double res);
  // </group>
  // Get radius of position
  virtual Double radius();
  // Generate a 3-vector of coordinates (length(m), angles(rad))
  Vector<Double> get() const;
  // Generate a 3-vector of x,y,z in m
  const Vector<Double> &getValue() const;
  // Generate angle 2-vector (in rad)
  Quantum<Vector<Double> > getAngle() const;
  // and with specified units
  Quantum<Vector<Double> > getAngle(const Unit &unit) const;
  // Get the longitudinal angle (in radians)
  Double getLong() const;
  // and with specified units
  Quantity getLong(const Unit &unit) const;
  // Get the latitude angle (rad)
  Double getLat() const;
  // and with specified units
  Quantity getLat(const Unit &unit) const;
  // Generate the length
  Quantity getLength() const;
  // and generate it with the specified units
  Quantity getLength(const Unit &unit) const;
  // Get the position angle between the directions. I.e. the angle between
  // the direction from one to the pole, and from one to the other.
  // <group>
  Double positionAngle(const MVPosition &other) const;
  Quantity positionAngle(const MVPosition &other, 
			 const Unit &unit) const;
  // </group>
  // Get the angular separation between two directions.
  // <group>
  Double separation(const MVPosition &other) const;
  Quantity separation(const MVPosition &other, 
		      const Unit &unit) const;
  // </group>
  // Produce the cross product
  MVPosition crossProduct(const MVPosition &other) const;
  
  // Print data
  virtual void print(ostream &os) const;
  // Clone
  virtual MeasValue *clone() const;

  // Get the value in internal units
  virtual Vector<Double> getVector() const;
  // Set the value from internal units (set 0 for empty vector)
  virtual void putVector(const Vector<Double> &in);
  // Get the internal value as a <src>Vector<Quantity></src>. Usable in
  // records. The getXRecordValue() gets additional information for records.
  // Note that the Vectors could be empty.
  // <group>
  virtual Vector<Quantum<Double> > getRecordValue() const;
  virtual Vector<Quantum<Double> > getXRecordValue() const;
  virtual Vector<Quantum<Double> > getTMRecordValue() const {
    return getXRecordValue(); } ;
  // </group>
  // Set the internal value if correct values and dimensions
  virtual Bool putValue(const Vector<Quantum<Double> > &in);
  
protected:
  //# Member functions
  // Get the latitude assuming length is given
  Double getLat(Double ln) const;
  //# Data
  // Position vector (in m)
  Vector<Double> xyz;
};

//# Global functions
// Rotate a position vector with rotation matrix and other multiplications
// <group>
MVPosition operator*(const RotMatrix &left, const MVPosition &right);
MVPosition operator*(const MVPosition &left, const RotMatrix &right);
MVPosition operator*(Double left, const MVPosition &right);
MVPosition operator*(const MVPosition &left, Double right);
Double operator*(const Vector<Double> &left, const MVPosition &right);
Double operator*(const MVPosition &left, const Vector<Double> &right);
// </group>


} //# NAMESPACE CASACORE - END

#endif
