//# MVDirection.h: Vector of three direction cosines
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

#ifndef CASA_MVDIRECTION_H
#define CASA_MVDIRECTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/MVPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward De

// <summary> Vector of three direction cosines </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MVPosition>MVPosition</linkto>
//   <li> <linkto class=Vector>Vector</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Direction
// </etymology>
//
// <synopsis>
// An MVDirection is a 3-vector of direction cosines. It is based on the
// MVposition class. The main difference is that the length of the
// vector will be adjusted (normalised) to a length of 1 in all operations.
// It can be constructed with:
// <ul>
//   <li> MVDirection() creates direction cosines for pole: (0,0,1)
//   <li> MVDirection(MVDirection) creates a copy
//   <li> MVDirection(Double, Double, Double) creates with
//		specified values and adjust to length of 1.
//   <li> MVDirection(Double, Double) creates a MVDirection assuming that the two
//		values are (in radians) angle along 'equator' and towards 'pole'.
//   <li> MVDirection(Quantity, Quantity) creates a MVDirection assuming angles
//		as in previous
//   <li> <src>MVDirection(Quantum<Vector<Double> >)</src> creates an MVDirection
//		from angle vector, assuming
// 		second as zero if not present, and pole if length 0. Assumes
//		a direction cosine if 3 elements
//   <li> <src>MVDirection(Vector<Double>)</src> creates an MVDirection with
//		the same restrictions as previous one
//   <li> <src>MVDirection(Vector<Quantum<Double> >)</src> creates an 
//		MVDirection with the same rstrictions as previous one; but
//		with unit check.
// </ul>
// A void adjust() function normalises the vector to a length of 1;
// a get() returns as a
// Double 2-vector the angles of the direction cosines; a getAngle() returns
// a Quantum 2-vector, (uInt) returns the indicated element, and getValue
// returns the direction cosine vector.<br>
// Direction cosines can be added and subtracted: the result will be 
// adjusted to a length of 1.<br>
// The multiplication of two direction cosines produces the inner product.<br>
// shift() methods are available to shift in angular coordinates. E.g.
// shift(Quantity(5, "arcsec"), Quantity(-7, "arcsec")) will shift 5 arcsec
// in longitude, and -7 arcsec in latitude. They have a trueAngle switch
// to shift in latitude and perpendicular (along a great circle) to it.
// </synopsis>
//
// <example>
// See <linkto class=MDirection>MDirection</linkto>
// </example>
//
// <motivation>
// To aid coordinate transformations
// </motivation>
//
// <todo asof="1998/04/22">
//   <li> check if true shifts can be done faster
// </todo>

class MVDirection : public MVPosition {

public:

  //# Friends
  
  //# Constructors
  // Default constructor generates a direction to the pole (i.e. (0,0,1))
  MVDirection();
  // Copy constructor
  MVDirection(const MVPosition &other);
  // Constructs with elevation = 0.
  // <group>
  MVDirection(Double in0);
  MVDirection(const Quantity &angle0);
  // </group>
  // Creates a specified vector
  MVDirection(Double in0, Double in1, Double in2);
  // Creates the direction cosines from specified angles along equator (azimuth)
  // and towards pole (,elevation).
  MVDirection(Double angle0, Double angle1);
  // Creates the direction cosines from specified angles
  // <thrown>
  //    <li> AipsError if quantities not in angle format
  // </thrown>
  // <group>
  MVDirection(const Quantity &angle0, const Quantity &angle1);
  // If not enough angles: pole (=(0,0,1)) assumed (if none), or elevation =0 (if 1);
  // direction cosines assumed (if 3).
  // <thrown>
  //  <li> AipsError if more than 3 values or incorrect units
  // </thrown>
  MVDirection(const Quantum<Vector<Double> > &angle);
  // </group>
  // Create from Vector. Assumes angles if less than or equal than 2 elements.
  // Assumes direction cosines if 3 elements.
  // <thrown>
  //  <li> AipsError if more than 3 elements
  // </thrown>
  // <group>
  MVDirection(const Vector<Double> &other);
  MVDirection(const Vector<Quantity> &other);
  // </group>
  // Copy assignment
  MVDirection &operator=(const MVDirection &other);
  
  // Destructor
  ~MVDirection();
  //# Operators
  // Addition and subtraction
  // <group>
  MVDirection &operator+=(const MVDirection &right);
  MVDirection operator+(const MVDirection &right) const;
  MVDirection &operator-=(const MVDirection &right);
  MVDirection operator-(const MVDirection &right) const;
  // </group>
  
  //# General Member Functions
  
  // Tell me your type
  // <group>
  virtual uInt type() const;
  static void assure(const MeasValue &in);
  // </group>
  
  // Adjust the direction cosines to a length of 1
  virtual void adjust();
  // Adjust the direction cosines to a length of 1 and return the length value
  virtual void adjust(Double &res);
  // Re-adjust : taken from MVPosition.
  //
  // Clone data
  virtual MeasValue *clone() const;
  // Generate a 2-vector of angles (in rad)
  Vector<Double> get() const;
  // Get the latitude angle (rad)
  Double getLat() const;
  // and with specified units
  Quantity getLat(const Unit &unit) const;
  // Get the position angle between the directions. I.e. the angle between
  // the direction from one to the pole, and from one to the other.
  // <group>
  Double positionAngle(const MVPosition &other) const;
  Double positionAngle(const MVDirection &other) const;
  Quantity positionAngle(const MVPosition &other, 
			 const Unit &unit) const;
  Quantity positionAngle(const MVDirection &other, 
			 const Unit &unit) const;
  // </group>
  // Get the angular separation between two directions.
  // <group>
  Double separation(const MVPosition &other) const;
  Double separation(const MVDirection &other) const;
  Quantity separation(const MVPosition &other, 
		      const Unit &unit) const;
  Quantity separation(const MVDirection &other, 
		      const Unit &unit) const;
  // </group>
  // Produce the cross product
  MVDirection crossProduct(const MVDirection &other) const;
  // Get the internal value as a <src>Vector<Quantity></src>. Usable in
  // records. The getXRecordValue() gets additional information for records.
  // Note that the Vectors could be empty.
  // <group>
  virtual Vector<Quantum<Double> > getRecordValue() const;
  virtual Vector<Quantum<Double> > getXRecordValue() const;
  virtual Vector<Quantum<Double> > getTMRecordValue() const;
  // </group>
  // Set the internal value if correct values and dimensions
  virtual Bool putValue(const Vector<Quantum<Double> > &in);
  // Set the internal value, using the longitude and latitude (in rad) given
  void setAngle(Double angle0, Double angle1);
  // Shift the direction in longitude (radians if Double) and/or latitude.
  // If the trueAngle switch is True, the longitude shift will be in
  // angular units perpendicular to the direction to the pole at the shifted
  // latitude, along a great circle.
  // <group>
  void shift(const Quantum<Double> &lng,
	     const Quantum<Double> &lat, Bool trueAngle=False);
  void shift(Double lng, Double lat, Bool trueAngle=False);
  void shiftLongitude(const Quantity &lng, Bool trueAngle=False);
  void shiftLongitude(Double lng, Bool trueAngle=False);
  void shiftLatitude(const Quantum<Double> &lat, Bool trueAngle=False);
  void shiftLatitude(Double lat, Bool trueAngle=False);
  void shift(const MVDirection &shft, Bool trueAngle=False);
  // </group>
  // Shift over an angle off in the direction pa. pa is measured from North,
  // in the direction of increasing longitude.
  // <group>
  void shiftAngle(const Quantum<Double> &off,
		  const Quantum<Double> &pa);
  void shiftAngle(Double off, Double pa);
  // </group>
  
protected:
  //# Data
};

//# Global functions
// Rotate a position vector
MVDirection operator*(const RotMatrix &left, const MVDirection&right);
MVDirection  operator*(const MVDirection &left, const RotMatrix &right);


} //# NAMESPACE CASACORE - END

#endif
