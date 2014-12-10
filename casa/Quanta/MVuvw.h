//# MVuvw.h: A 3D vector on Earth
//# Copyright (C) 1998,2000
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

#ifndef CASA_MVUVW_H
#define CASA_MVUVW_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/MVPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MVDirection;
class MVBaseline;

// <summary> A 3D vector on Earth </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMuvw" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasValue>MeasValue</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and uvw
// </etymology>
//
// <synopsis>
// A MVuvw is a 3-vector of uvws in a rectangular frame with
// internal units of m.<br>
// It can be constructed with:
// <ul>
//   <li> MVuvw() creates point at origin (0,0,0)
//   <li> MVuvw(MVuvw) creates a copy
//   <li> MVuvw(Double, Double, Double) creates (x,y,z) with
//		specified values (assuming meters)
//   <li> MVuvw(Quantity length,Double, Double) creates a MVuvw assuming
//		that the two values are (in radians) angle along 'equator' 
//		and towards 'pole'.
//   <li> MVuvw(Quantity length, Quantity, Quantity) creates a MVuvw 
//		assuming angles as in previous, or uvws
//   <li> <src>MVuvw(Quantity, Quantum<Vector<Double> >)</src> creates a 
//		MVuvw from angle vector, using first two angles, and 
//		assuming second as zero if not present.
//   <li> <src>MVuvw(Quantum<Vector<Double> ></src> creates from
//		angles or uvws, depending on the units in the
//		quantum vector. In the angle case,
//		the data derived can be scaled with the readjust() function. If
//		the unit of the quantum vector is length, uvw is
//		assumed.
//    <li> <src>MVuvw(Vector<Double></src> creates from angles (less than
//		or equal to two elements) or x,y,z (3 elements).
//    <li> <src>MVuvw(Vector<Quantity></src> creates from length+angles,
//		angles, or x,y,z, depending on units.
//    <li> <src>MVuvw(MVBaseline, MVDirection)</src> creates a uvw
//		in the specified reference direction (in same reference frame)
// </ul>
// A void adjust(Double) function normalises the vector to a length of 1;
// a get() returns as a
// Double 3-vector the length and angles of the uvw vector;
// a getAngle() returns a Quantum 2-vector, (uInt) returns the indicated 
// element, and getValue returns the vector.<br>
// uvws can be added and subtracted.<br>
// The multiplication of two uvws produces the in-product.<br>
// </synopsis>
//
// <example>
// See <linkto class=Muvw>Muvw</linkto> class.
// </example>
//
// <motivation>
// To do coordinate transformations
// </motivation>
//
// <todo asof="1998/04/20">
//	<li> Implement for EW
//	<li> Get sign (especially of V) correct
//	<li> Let it handle Vectors of UVW
//	<li> Add some rotation matrix history for speed
// </todo>

class MVuvw : public MVPosition {	

public:

  //# Friends
  
  //# Constructors
  // Default constructor generates a (0,0,0) uvw
  MVuvw();
  // Copy constructor
  MVuvw(const MVPosition &other);
  // Creates a specified vector
  MVuvw(Double in0, Double in1, Double in2);
  // Creates a vector with specified length towards pole
  // <group>
  explicit MVuvw(Double in0);
  MVuvw(const Quantity &l);
  // </group>
  // Creates the uvw from specified (azimuth,elevation) angles and length
  MVuvw(const Quantity &l, Double angle0, Double angle1);
  // Creates the uvw from specified angles and length. or uvws
  // <thrown>
  //    <li> AipsError if quantities not in angle format
  // </thrown>
  // <group>
  MVuvw(const Quantity &l, const Quantity &angle0, 
	     const Quantity &angle1);
  // If not enough angles: pole assumed (if none), or elevation =0 (if 1)
  MVuvw(const Quantum<Vector<Double> > &angle);
  MVuvw(const Quantity &l, const Quantum<Vector<Double> > &angle);
  // </group>
  // Create from specified length and/or angles and/or uvw
  // <group>
  MVuvw(const Vector<Double> &other);
  MVuvw(const Vector<Quantity> &other);
  // </group>
  // uvw from a baseline and a reference direction (in same frame)
  // <group>
  MVuvw(const MVBaseline &pos, const MVDirection &dr, Bool ew=False);
  // </group>
  // Copy assignment
  MVuvw &operator=(const MVuvw &other);
  
  // Destructor
  ~MVuvw();
  
  //# Operators
  // Multiplication defined as in-product
  // <group>
  Double operator*(const MVuvw &other) const;
  // </group>
  
  // Equality comparisons
  // <group>
  Bool operator== (const MVuvw &other) const;
  Bool operator!= (const MVuvw &other) const;
  Bool near(const MVuvw &other, Double tol=1e-13) const;
  Bool near(const MVuvw &other, Quantity tol) const;
  Bool nearAbs(const MVuvw &other, Double tol=1e-13) const;
  // </group>
  
  // Addition and subtraction
  // <group>
  MVuvw operator-() const;
  MVuvw &operator+=(const MVuvw &right);
  MVuvw operator+(const MVuvw &right) const;
  MVuvw &operator-=(const MVuvw &right);
  MVuvw operator-(const MVuvw &right) const;
  // </group>
  
  //# General Member Functions
  
  // Tell me your type
  // <group>
  virtual uInt type() const;
  static void assure(const MeasValue &in);
  // </group>
  
  // Normalise direction aspects by adjusting the length to 1
  // <group>
  virtual void adjust();
  virtual void adjust(Double &res);
  virtual void readjust(Double res);
  // </group>
  // Get radius(i.e. length of vector, in m) of uvw
  virtual Double radius();
  // Generate a 3-vector of coordinates (length(m), angles(rad))
  Vector<Double> get() const;
  // Generate a 3-vector of x,y,z in m
  const Vector<Double> &getValue() const;
  // Generate angle 2-vector (in rad)
  Quantum<Vector<Double> > getAngle() const;
  // and with specified units
  Quantum<Vector<Double> > getAngle(const Unit &unit) const;
  // Generate the length
  Quantity getLength() const;
  // and generate it with the specified units
  Quantity getLength(const Unit &unit) const;
  // Get the uvw angle between the directions. I.e. the angle between
  // the direction from one to the pole, and from one to the other.
  // <group>
  Double uvwAngle(const MVuvw &other) const;
  Quantity uvwAngle(const MVuvw &other, 
			 const Unit &unit) const;
  // </group>
  // Get the angular separation between two directions.
  // <group>
  Double separation(const MVuvw &other) const;
  Quantity separation(const MVuvw &other, 
		      const Unit &unit) const;
  // </group>
  // Produce the cross product
  MVuvw crossProduct(const MVuvw &other) const;
  
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
  
};

//# Global functions
// Rotate a uvw vector with rotation matrix and other multiplications
// <group>
MVuvw operator*(const RotMatrix &left, const MVuvw &right);
MVuvw operator*(const MVuvw &left, const RotMatrix &right);
MVuvw operator*(Double left, const MVuvw &right);
MVuvw operator*(const MVuvw &left, Double right);
Double operator*(const Vector<Double> &left, const MVuvw &right);
Double operator*(const MVuvw &left, const Vector<Double> &right);
Double operator*(const MVPosition &left, const MVuvw &right);
Double operator*(const MVuvw &left, const MVPosition &right);
// </group>


} //# NAMESPACE CASACORE - END

#endif
