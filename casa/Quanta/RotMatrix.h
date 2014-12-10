//# RotMatrix.h: a 3x3 rotation matrix
//# Copyright (C) 1995,1996,1997,1999,2000,2001
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

#ifndef CASA_ROTMATRIX_H
#define CASA_ROTMATRIX_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Euler;

//# Constants (SUN compiler does not accept non-simple default arguments)

// <summary>
// A 3x3 rotation matrix
// </summary>

// <use visibility=local>

// <reviewed reviewer="tcornwel" date="1996/02/15" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Matrix>Matrix</linkto>
//   <li> <linkto class=Euler>Euler</linkto>
// </prerequisite>
//
// <etymology>
// From Rotation and Matrix
// </etymology>
//
// <synopsis>
// A rotation matrix is a 3x3 matrix, which can be used to rotate a coordinate
// system, notably the direction cosines in 
// <linkto class=MVDirection>MVDirection</linkto>.<br>
// A RotMatrix can be constructed by the default constructor (which will
// set the diagonal to 1), a copy constructor, and from a set of
// Euler angles with <src>RotMatrix(Euler)</src>.<br>
// Multiplication can be done (by *= and *) of two rotation matrices.<br>
// The (uInt, uInt) operator returns the indicated element. 
// </synopsis>
//
// <example>
//  See <linkto class=Euler>Euler</linkto>
// </example>
//
// <motivation>
// To use in nutation and other coordinate calculations
// </motivation>
//
// <todo asof="1996/02/15">
// </todo>

class RotMatrix
{	
    public:
//# Friends
// Output a rotation matrix as a matrix
    friend ostream &operator<< (ostream &os, const RotMatrix &rot);
//# Constructors
// Default constructor generates a unit 3x3 matrix.
    RotMatrix();
// The copy constructor copies
    RotMatrix(const RotMatrix &other);
// Make from an Euler
    RotMatrix(const Euler &other);
// Make from an Euler around specified axes
    RotMatrix(const Euler &other, Int ax0, Int ax1, Int ax2);
// Copy assignment
    RotMatrix &operator=(const RotMatrix &other);

// Destructor
    ~RotMatrix();

//# Operators
// The multiplication operations generate matrix products
// <group>
    RotMatrix &operator*=(const RotMatrix &other);
    RotMatrix operator*(const RotMatrix &other) const;
// </group>

// Return the indicated element
// <group>
    Double &operator()(uInt row, uInt column);
    const Double &operator()(uInt row, uInt column) const;
// </group>

  //# Methods
// Get as Matrix
    Matrix<Double> get() const;

// Transpose the rotation matrix
     void transpose();

// Fill Rotation matrix from Matrix
     void set(const Matrix<Double> &in);

// Fill Rotation matrix from 3 (row) vectors
     void set(const Vector<Double> &in0, const Vector<Double> &in1,
	      const Vector<Double> &in2);

    private:
//# Data
// The rotation matrix (3x3)
    Double rotat[3][3];

//# Member functions
// Apply to a rotation matrix a further rotation of angle around the specified
// axis which (0 or 1 or 2).
    void applySingle(Double angle, Int which);
};


} //# NAMESPACE CASACORE - END

#endif
