//# MVBaseline.h: A 3D vector on Earth
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

#ifndef CASA_MVBASELINE_H
#define CASA_MVBASELINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/MVPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> A 3D vector on Earth </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMBaseline" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasValue>MeasValue</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Baseline
// </etymology>
//
// <synopsis>
// A MVBaseline is a 3-vector of Baselines in a rectangular frame with
// internal units of m.<br>
// It can be constructed with:
// <ul>
//   <li> MVBaseline() creates point at origin (0,0,0)
//   <li> MVBaseline(MVBaseline) creates a copy
//   <li> MVBaseline(Double, Double, Double) creates (x,y,z) with
//		specified values (assuming meters)
//   <li> MVBaseline(Quantity length,Double, Double) creates a MVBaseline assuming
//		that the two values are (in radians) angle along 'equator' 
//		and towards 'pole'.
//   <li> MVBaseline(Quantity length, Quantity, Quantity) creates a MVBaseline 
//		assuming angles as in previous, or Baselines
//   <li> <src>MVBaseline(Quantity, Quantum<Vector<Double> >)</src> creates a 
//		MVBaseline from angle vector, using first two angles, and 
//		assuming second as zero if not present.
//   <li> <src>MVBaseline(Quantum<Vector<Double> ></src> creates from
//		angles or Baselines, depending on the units in the
//		quantum vector. In the angle case,
//		the data derived can be scaled with the readjust() function. If
//		the unit of the quantum vector is length, Baseline is
//		assumed.
//    <li> <src>MVBaseline(Vector<Double></src> creates from angles (less than
//		or equal to two elements) or x,y,z (3 elements).
//    <li> <src>MVBaseline(Vector<Quantity></src> creates from length+angles,
//		angles, or x,y,z, depending on units.
//    <li> <src>MVBaseline(MVPosition, MVPosition)</src> creates a baseline
//		pointing from second to first MVPosition
//    <li> <src>MVBaseline(MVPosition)</src> creates a baseline as defined by the
//		position given (e.g. as derived from an offset MPosition)
// </ul>
// A void adjust(Double) function normalises the vector to a length of 1;
// a get() returns as a
// Double 3-vector the length and angles of the Baseline vector;
// a getAngle() returns a Quantum 2-vector, (uInt) returns the indicated 
// element, and getValue returns the vector.<br>
// Baselines can be added and subtracted.<br>
// The multiplication of two Baselines produces the in-product.<br>
// </synopsis>
//
// <example>
// See <linkto class=MBaseline>MBaseline</linkto> class.
// </example>
//
// <motivation>
// To do coordinate transformations
// </motivation>
//
// <todo asof="1998/04/20">
//	<li> Nothing I know of
// </todo>

class MVBaseline : public MVPosition {	

public:

  //# Friends
  
  //# Constructors
  // Default constructor generates a (0,0,0) Baseline
  MVBaseline();
  // Copy constructor
  MVBaseline(const MVPosition &other);
  // Creates a specified vector
  MVBaseline(Double in0, Double in1, Double in2);
  // Creates a vector with specified length towards pole
  // <group>
  explicit MVBaseline(Double in0);
  MVBaseline(const Quantity &l);
  // </group>
  // Creates the Baseline from specified (azimuth,elevation) angles and length
  MVBaseline(const Quantity &l, Double angle0, Double angle1);
  // Creates the Baseline from specified angles and length. or Baselines
  // <thrown>
  //    <li> AipsError if quantities not in angle format
  // </thrown>
  // <group>
  MVBaseline(const Quantity &l, const Quantity &angle0, 
	     const Quantity &angle1);
  // If not enough angles: pole assumed (if none), or elevation =0 (if 1)
  MVBaseline(const Quantum<Vector<Double> > &angle);
  MVBaseline(const Quantity &l, const Quantum<Vector<Double> > &angle);
  // </group>
  // Create from specified length and/or angles and/or Baseline
  // <group>
  MVBaseline(const Vector<Double> &other);
  MVBaseline(const Vector<Quantity> &other);
  // </group>
  // Baseline as difference between positions (first - second (default(0,0,0))
  // <group>
  MVBaseline(const MVPosition &pos, const MVPosition &base);
  // </group>
  // Copy assignment
  MVBaseline &operator=(const MVBaseline &other);
  
  // Destructor
  ~MVBaseline();
  
  //# Operators
  // Multiplication defined as in-product
  // <group>
  Double operator*(const MVBaseline &other) const;
  // </group>
  
  // Equality comparisons
  // <group>
  Bool operator== (const MVBaseline &other) const;
  Bool operator!= (const MVBaseline &other) const;
  Bool near(const MVBaseline &other, Double tol=1e-13) const;
  Bool near(const MVBaseline &other, Quantity tol) const;
  Bool nearAbs(const MVBaseline &other, Double tol=1e-13) const;
  // </group>
  
  // Addition and subtraction
  // <group>
  MVBaseline operator-() const;
  MVBaseline &operator+=(const MVBaseline &right);
  MVBaseline operator+(const MVBaseline &right) const;
  MVBaseline &operator-=(const MVBaseline &right);
  MVBaseline operator-(const MVBaseline &right) const;
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
  // Get radius of Baseline
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
  // Get the Baseline angle between the directions. I.e. the angle between
  // the direction from one to the pole, and from one to the other.
  // <group>
  Double BaselineAngle(const MVBaseline &other) const;
  Quantity BaselineAngle(const MVBaseline &other, 
			 const Unit &unit) const;
  // </group>
  // Get the angular separation between two directions.
  // <group>
  Double separation(const MVBaseline &other) const;
  Quantity separation(const MVBaseline &other, 
		      const Unit &unit) const;
  // </group>
  // Produce the cross product
  MVBaseline crossProduct(const MVBaseline &other) const;
  
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
// Rotate a Baseline vector with rotation matrix and other multiplications
// <group>
MVBaseline operator*(const RotMatrix &left, const MVBaseline &right);
MVBaseline operator*(const MVBaseline &left, const RotMatrix &right);
MVBaseline operator*(Double left, const MVBaseline &right);
MVBaseline operator*(const MVBaseline &left, Double right);
Double operator*(const Vector<Double> &left, const MVBaseline &right);
Double operator*(const MVBaseline &left, const Vector<Double> &right);
Double operator*(const MVPosition &left, const MVBaseline &right);
Double operator*(const MVBaseline &left, const MVPosition &right);
// </group>


} //# NAMESPACE CASACORE - END

#endif
