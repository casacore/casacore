//# MVEarthMagnetic.h: A 3D Earth magnetic field vector
//# Copyright (C) 1996,1997,1998,2000
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

#ifndef CASA_MVEARTHMAGNETIC_H
#define CASA_MVEARTHMAGNETIC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/MVPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary> A 3D Earth magnetic field vector </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasValue>MeasValue</linkto>
//   <li> <linkto class=Vector>Vector</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Earth Magnetic field
// </etymology>
//
// <synopsis>
// A MVEarthMagnetic is a 3-vector of the Earth's magnetic flux density in a
// rectangular frame with the z-axis to astronomical North pole, and x-axis
// towards longitude zero, in internal Units of nano tesla (== 0.00001 G).<br>
// It can be constructed with:
// <ul>
//   <li> MVEarthMagnetic() creates  (0,0,0)
//   <li> MVEarthMagnetic(MVEarthMagnetic) creates a copy
//   <li> MVEarthMagnetic(Double, Double, Double) creates (x,y,z) with
//		specified values in tesla
//   <li> MVEarthMagnetic(Quantity length,Double, Double) creates an
//		 MVEarthMagnetic assuming
//		that the two values are (in radians) angle along 'equator' 
//		and towards 'pole'.
//   <li> MVEarthMagnetic(Quantity length, Quantity, Quantity) creates an
//		 MVEarthMagnetic 
//		assuming angles as in previous, or (x,y,z) fields
//   <li> <src>MVEarthMagnetic(Quantity, Quantum<Vector<Double> >)</src> creates a 
//		MVEarthMagnetic from angle vector, using first two angles, and 
//		assuming second as zero if not present, and pole if length 0.
//   <li> <src>MVEarthMagnetic(Quantum<Vector<Double> ></src> creates from
//		angles or fields, depending on the units in the
//		quantum vector. In the angle case,
//		the data derived can be scaled with the readjust() function. If
//		the unit of the quantum vector is magnetic flux density,
//		magnetic field components are assumed.
//    <li> <src>MVEarthMagnetic(Vector<Double></src> creates from angles (less than
//		or equal to two elements) or x,y,z (3 elements).
//    <li> <src>MVEarthMagnetic(Vector<Quantity></src> creates from length+angles,
//		angles, or x,y,z, depending on units.
// </ul>
// A void adjust(Double) function normalises the vector to a length of 1;
// a get() returns as a
// Double 3-vector the length and angles of the EarthMagnetic vector;
// a getAngle() returns a Quantum 2-vector, (uInt) returns the indicated 
// element, and getValue returns the vector.<br>
// EarthMagnetics can be added and subtracted.<br>
// The multiplication of two EarthMagnetics produces the in-product.<br>
// </synopsis>
//
// <example>
// See <linkto class=MEarthMagnetic>MEarthMagnetic</linkto> class.
// </example>
//
// <motivation>
// To use in ionospheric effect calculations
// </motivation>
//
// <todo asof="1998/05/19">
//	<li> nothing I know of
// </todo>

class MVEarthMagnetic : public MVPosition {	

public:

  //# Friends
  
  //# Constructors
  // Default constructor generates a (0,0,0) EarthMagnetic
  MVEarthMagnetic();
  // Copy constructor
  MVEarthMagnetic(const MVPosition &other);
  // Creates a specified vector
  MVEarthMagnetic(Double in0, Double in1, Double in2);
  // Creates a vector with specified length towards pole
  // <group>
  explicit MVEarthMagnetic(Double in0);
  MVEarthMagnetic(const Quantity &l);
  // </group>
  // Creates the EarthMagnetic from specified (azimuth,elevation) angles and length
  MVEarthMagnetic(const Quantity &l, Double angle0, Double angle1);
  // Creates the EarthMagnetic from specified angles and length. or EarthMagnetics
  // <thrown>
  //    <li> AipsError if quantities not in angle format
  // </thrown>
  // <group>
  MVEarthMagnetic(const Quantity &l, const Quantity &angle0, 
		  const Quantity &angle1);
  // If not enough angles: pole assumed (if none), or elevation =0 (if 1)
  MVEarthMagnetic(const Quantum<Vector<Double> > &angle);
  MVEarthMagnetic(const Quantity &l, const Quantum<Vector<Double> > &angle);
  // </group>
  // Create from specified length and/or angles and/or EarthMagnetic
  // <group>
  MVEarthMagnetic(const Vector<Double> &other);
  MVEarthMagnetic(const Vector<Quantity> &other);
  // </group>
  // Copy assignment
  MVEarthMagnetic &operator=(const MVEarthMagnetic &other);
  
  // Destructor
  ~MVEarthMagnetic();
  
  //# Operators
  // Multiplication defined as in-product
  // <group>
  Double operator*(const MVEarthMagnetic &other) const;
  // </group>
  
  // Equality comparisons
  // <group>
  Bool operator== (const MVEarthMagnetic &other) const;
  Bool operator!= (const MVEarthMagnetic &other) const;
  Bool near(const MVEarthMagnetic &other, Double tol=1e-13) const;
  Bool near(const MVEarthMagnetic &other, Quantity tol) const;
  Bool nearAbs(const MVEarthMagnetic &other, Double tol=1e-13) const;
  // </group>
  
  // Addition and subtraction
  // <group>
  MVEarthMagnetic operator-() const;
  MVEarthMagnetic &operator+=(const MVEarthMagnetic &right);
  MVEarthMagnetic operator+(const MVEarthMagnetic &right) const;
  MVEarthMagnetic &operator-=(const MVEarthMagnetic &right);
  MVEarthMagnetic operator-(const MVEarthMagnetic &right) const;
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
  // Get modulus of EarthMagnetic
  virtual Double radius();
  // Generate a 3-vector of coordinates (length(T), angles(rad))
  Vector<Double> get() const;
  // Generate a 3-vector of x,y,z in tesla
  const Vector<Double> &getValue() const;
  // Generate angle 2-vector (in rad)
  Quantum<Vector<Double> > getAngle() const;
  // and with specified units
  Quantum<Vector<Double> > getAngle(const Unit &unit) const;
  // Generate the length
  Quantity getLength() const;
  // and generate it with the specified units
  Quantity getLength(const Unit &unit) const;
  // Get the EarthMagnetic angle between the directions. I.e. the angle between
  // the direction from one to the pole, and from one to the other.
  // <group>
  Double earthMagneticAngle(const MVEarthMagnetic &other) const;
  Quantity earthMagneticAngle(const MVEarthMagnetic &other, 
			      const Unit &unit) const;
  // </group>
  // Get the angular separation between two directions.
  // <group>
  Double separation(const MVEarthMagnetic &other) const;
  Quantity separation(const MVEarthMagnetic &other, 
		      const Unit &unit) const;
  // </group>
  // Produce the cross product
  MVEarthMagnetic crossProduct(const MVEarthMagnetic &other) const;
  
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
  // </group>
  // Set the internal value if correct values and dimensions
  virtual Bool putValue(const Vector<Quantum<Double> > &in);

};

//# Global functions
// Rotate a EarthMagnetic vector with rotation matrix and other multiplications
// <group>
MVEarthMagnetic operator*(const RotMatrix &left, const MVEarthMagnetic &right);
MVEarthMagnetic operator*(const MVEarthMagnetic &left, const RotMatrix &right);
MVEarthMagnetic operator*(Double left, const MVEarthMagnetic &right);
MVEarthMagnetic operator*(const MVEarthMagnetic &left, Double right);
Double operator*(const Vector<Double> &left, const MVEarthMagnetic &right);
Double operator*(const MVEarthMagnetic &left, const Vector<Double> &right);
Double operator*(const MVPosition &left, const MVEarthMagnetic &right);
Double operator*(const MVEarthMagnetic &left, const MVPosition &right);
// </group>


} //# NAMESPACE CASACORE - END

#endif
