//# Euler.h: Vector of Euler rotation angles
//# Copyright (C) 1995,1996,1997,1998,1999
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

#ifndef CASA_EULER_H
#define CASA_EULER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Vector of Euler rotation angles
// </summary>

// <use visibility=local>

// <reviewed reviewer="tcornwel" date="1996/02/15" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Vector>Vector</linkto> class
//   <li> <linkto class=Quantum>Quantum</linkto> class for units
//   <li> <linkto class=RotMatrix>RotMatrix</linkto> class for usage
// </prerequisite>
//
// <etymology>
// Euler angles describe the rotation of a coordinate system
// </etymology>
//
// <synopsis>
// The Euler class is a vector of three angles, together with a vector of
// three signed integers. The angles describe the rotation around an axis of a
// coordinate system, the integers the actual axis around which to rotate.
// The integer can be 0 (do not use this angle) or 1,2,3 to indicate the
// axis.
// Given angles (a1,a2,a3) and axes (i1,i2,i3), the actual rotation matrix
// constructed will be:<br>
// R = R<sub>i3</sub>(a3).R<sub>i2</sub>(a2).R<sub>i1</sub>(a1) <br>
// It has the following constructors:
// <ul>
//   <li> Euler() creates a zero filled vector of length 3. Axes: (1,2,3)
//   <li> Euler(Euler) creates a copy
//   <li> Euler(Double, uInt, Double=0, uInt=0, Double=0, uInt=0) creates an 
//		Euler with specified values
//   <li> Euler(Double, Double=0, Double=0) creates an Euler with (1,2,3)
//   <li> Euler(Quantity, uInt, Quantity=0, uInt=0, Quantity=0, uInt=0) creates
//		 an Euler with specified values
//   <li> Euler(Quantity, Quantity=0, Quantity=0) creates an Euler with
//		interpretation of angle units in the Quantities
//   <li> Euler(<src>Quantum<Vector<Double> ></src>) creates a zero expanded
//		Euler from at most the first three elements of Quantity
//		vector; with (1,2,3) 
//   <li> Euler(<src>Quantum<Vector<Double> >, Vector<uInt></src>) creates a 
//		zero expanded Euler with given values
// </ul>
// It has a unary minus operator, which reverses the sign and order of the
// three angles, and the order of the axes, to produce the Euler angles
// for a rotation with opposite signs, so that <src>RotMatrix(-Euler)</src>
// will generate the inverse rotation matrix as compared with
// <src>RotMatrix(Euler)</src>.<br>
// getAngle() functions return the Euler angles as a Quantum vector.<br>
// Eulers have addition and subtraction (on the angles). Note that this
// produces the correct angles for a combined rotation only if the
// axes are identical.<br>
// A (which) operator returns the indicated angle. Set/get functions
// manipulate the axes.
// </synopsis>
//
// <example>
// <srcblock>
//	Quantity angle(25,"deg");	// 25 degrees
//	Euler eul(angle.get().getValue(),2); // rotate over axis 2 (radians)
//	RotMatrix rot(eul); 		// generates rotation matrix
// </srcblock>  
// </example>
//
// <motivation>
// To use generated precession and nutation results
// </motivation>
//
// <todo asof="1995/09/04">
// </todo>

class Euler
{	
    public:
//# Friends
// Output Euler angles
    friend ostream &operator<<(ostream &os, const Euler &eul);

//# Constructors
// Default constructor generates zero filled Double vector of length 3, with
// (1,2,3) axes
    Euler();
// Copy constructor
    Euler(const Euler &other);
// Copy assignment
    Euler &operator=(const Euler &other);
// Constructs an Euler with specified angles and (1,2,3) axes
    Euler(Double in0, Double in1 = 0, Double in2 = 0);
// Constructs an Euler with specified angles and axes
    Euler(Double in0, uInt ax0, Double in1 = 0, uInt ax1=0, Double in2 = 0,
	  uInt ax2=0);
// <thrown>
//    <li> AipsError if non-angle units used
// </thrown>
// Constructs an Euler from specified angle quantities
// <group>
    Euler(const Quantity &in0);
    Euler(const Quantity &in0, const Quantity &in1);
    Euler(const Quantity &in0, const Quantity &in1, 
	  const Quantity &in2);
    Euler(const Quantity &in0, uInt ax0);
    Euler(const Quantity &in0, uInt ax0, const Quantity &in1, uInt ax1=0);
    Euler(const Quantity &in0, uInt ax0, const Quantity &in1, uInt ax1,
	  const Quantity &in2, uInt ax2=0);
// Constructs an Euler (zero filled) from elements of Quantity vector
// <group>
    Euler(const Quantum<Vector<Double> > &in);
    Euler(const Quantum<Vector<Double> > &in, const Vector<uInt> &ax);
// </group>
// </group>

// Destructor
    ~Euler();

//# Operators
// The unary minus reverses the sign and order of the Euler angles
    Euler operator-() const;
// Addition and subtraction
// <group>
    Euler &operator+=(const Euler &right);
    Euler operator+(const Euler &right) const;
    Euler &operator-=(const Euler &right);
    Euler operator-(const Euler &right) const;
// </group>
// Return the which' angle
// <group>
    Double &operator()(uInt which);
    const Double &operator()(uInt which) const;
// </group>

//# General Member Functions
// with the optional conversion units.
// <group>
    Quantum<Vector<Double> > getAngle() const;
    Quantum<Vector<Double> > getAngle(const Unit &unit) const;
// </group>

// Set an axis
    void set(uInt which, uInt ax);

// Set all axes
    void set(uInt ax0, uInt ax1, uInt ax2);

// Get an axis
    Int get(uInt which) const;

private:
//# Data
// Actual vector with 3 Euler angles
    Vector<Double> euler;
// Axes
    Vector<Int> axes;

//# Private Member Functions
// The makeRad functions check and convert the input Quantities to radians
// <group>
    static Double makeRad(const Quantity &in);
    static Vector<Double> makeRad(const Quantum<Vector<Double> > &in);
// </group>
};


} //# NAMESPACE CASACORE - END

#endif


