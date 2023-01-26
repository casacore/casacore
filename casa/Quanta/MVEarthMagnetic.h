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
//   <li> MVEarthMagnetic(MVPosition) creates (x,y,z) from the given position
//   <li> MVEarthMagnetic(double, double, double) creates (x,y,z) with
//		specified values in tesla
//   <li> MVEarthMagnetic(Quantity length,double, double) creates an
//		 MVEarthMagnetic assuming
//		that the two values are (in radians) angle along 'equator' 
//		and towards 'pole'.
//   <li> MVEarthMagnetic(Quantity length, Quantity, Quantity) creates an
//		 MVEarthMagnetic 
//		assuming angles as in previous, or (x,y,z) fields
//   <li> <src>MVEarthMagnetic(Quantity, Quantum<Vector<double> >)</src> creates a 
//		MVEarthMagnetic from angle vector, using first two angles, and 
//		assuming second as zero if not present, and pole if length 0.
//   <li> <src>MVEarthMagnetic(Quantum<Vector<double> ></src> creates from
//		angles or fields, depending on the units in the
//		quantum vector. In the angle case,
//		the data derived can be scaled with the readjust() function. If
//		the unit of the quantum vector is magnetic flux density,
//		magnetic field components are assumed.
//    <li> <src>MVEarthMagnetic(Vector<double></src> creates from angles (less than
//		or equal to two elements) or x,y,z (3 elements).
//    <li> <src>MVEarthMagnetic(Vector<Quantity></src> creates from length+angles,
//		angles, or x,y,z, depending on units.
// </ul>
// A void adjust(double) function normalises the vector to a length of 1;
// a get() returns as a
// double 3-vector the length and angles of the EarthMagnetic vector;
// a getAngle() returns a Quantum 2-vector, (uint32_t) returns the indicated 
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
  // Creates from an MVPosition
  MVEarthMagnetic(const MVPosition &other);
  // Creates a specified vector
  MVEarthMagnetic(double in0, double in1, double in2);
  // Creates a vector with specified length towards pole
  // <group>
  explicit MVEarthMagnetic(double in0);
  MVEarthMagnetic(const Quantity &l);
  // </group>
  // Creates the EarthMagnetic from specified (azimuth,elevation) angles and length
  MVEarthMagnetic(const Quantity &l, double angle0, double angle1);
  // Creates the EarthMagnetic from specified angles and length. or EarthMagnetics
  // <thrown>
  //    <li> AipsError if quantities not in angle format
  // </thrown>
  // <group>
  MVEarthMagnetic(const Quantity &l, const Quantity &angle0, 
		  const Quantity &angle1);
  // If not enough angles: pole assumed (if none), or elevation =0 (if 1)
  MVEarthMagnetic(const Quantum<Vector<double> > &angle);
  MVEarthMagnetic(const Quantity &l, const Quantum<Vector<double> > &angle);
  // </group>
  // Create from specified length and/or angles and/or EarthMagnetic
  // <group>
  MVEarthMagnetic(const Vector<double> &other);
  MVEarthMagnetic(const Vector<Quantity> &other);
  // </group>
  
  //# Operators
  // Multiplication defined as in-product
  // <group>
  double operator*(const MVEarthMagnetic &other) const;
  // </group>
  
  // Equality comparisons
  // <group>
  bool operator== (const MVEarthMagnetic &other) const;
  bool operator!= (const MVEarthMagnetic &other) const;
  bool near(const MVEarthMagnetic &other, double tol=1e-13) const;
  bool near(const MVEarthMagnetic &other, Quantity tol) const;
  bool nearAbs(const MVEarthMagnetic &other, double tol=1e-13) const;
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
  static void assure(const MeasValue &in);
  // </group>
  
  // Normalise direction aspects by adjusting the length to 1
  // <group>
  virtual void adjust();
  virtual void adjust(double &res);
  virtual void readjust(double res);
  // </group>
  // Get modulus of EarthMagnetic
  virtual double radius();
  // Generate a 3-vector of coordinates (length(T), angles(rad))
  Vector<double> get() const;
  // Generate a 3-vector of x,y,z in tesla
  const Vector<double> &getValue() const;
  // Generate angle 2-vector (in rad)
  Quantum<Vector<double> > getAngle() const;
  // and with specified units
  Quantum<Vector<double> > getAngle(const Unit &unit) const;
  // Generate the length
  Quantity getLength() const;
  // and generate it with the specified units
  Quantity getLength(const Unit &unit) const;
  // Get the EarthMagnetic angle between the directions. I.e. the angle between
  // the direction from one to the pole, and from one to the other.
  // <group>
  double earthMagneticAngle(const MVEarthMagnetic &other) const;
  Quantity earthMagneticAngle(const MVEarthMagnetic &other, 
			      const Unit &unit) const;
  // </group>
  // Get the angular separation between two directions.
  // <group>
  double separation(const MVEarthMagnetic &other) const;
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
  virtual Vector<double> getVector() const;
  // Set the value from internal units (set 0 for empty vector)
  virtual void putVector(const Vector<double> &in);
  // Get the internal value as a <src>Vector<Quantity></src>. Usable in
  // records. The getXRecordValue() gets additional information for records.
  // Note that the Vectors could be empty.
  // <group>
  virtual Vector<Quantum<double> > getRecordValue() const;
  // </group>
  // Set the internal value if correct values and dimensions
  virtual bool putValue(const Vector<Quantum<double> > &in);

};

//# Global functions
// Rotate a EarthMagnetic vector with rotation matrix and other multiplications
// <group>
MVEarthMagnetic operator*(const RotMatrix &left, const MVEarthMagnetic &right);
MVEarthMagnetic operator*(const MVEarthMagnetic &left, const RotMatrix &right);
MVEarthMagnetic operator*(double left, const MVEarthMagnetic &right);
MVEarthMagnetic operator*(const MVEarthMagnetic &left, double right);
double operator*(const Vector<double> &left, const MVEarthMagnetic &right);
double operator*(const MVEarthMagnetic &left, const Vector<double> &right);
double operator*(const MVPosition &left, const MVEarthMagnetic &right);
double operator*(const MVEarthMagnetic &left, const MVPosition &right);
// </group>


} //# NAMESPACE CASACORE - END

#endif
