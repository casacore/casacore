//# LinearXform.h: Perform a linear transform between input and output vectors
//# Copyright (C) 1997,1998,1999
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
//#
//# $Id$

#if !defined(AIPS_LINEAR_XFORM_H)
#define AIPS_LINEAR_XFORM_H

#include <aips/aips.h>

template<class T> class Vector;
template<class T> class Matrix;
class String;
struct linprm;

// <summary>
// Perform a linear transform between input and output vectors
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> General knowledge of AIPS++ Arrays
//   <li> Knowledge of FITS terminology in coordinate transformations
// </prerequisite>
//
// <synopsis>
// This class represents the common linear part of a FITS coordinate 
// transformation. In particular it does the following:
// <srcblock>
// world = cdelt * PC * (pixel - crpix)
// </srcblock>
// Where PC is an NxN matrix, pixel, crpix and world are length N vectors, and
// cdelt is an NxN diagonal matrix, represented as a length N vector.
//
// Normally this class isn't used directly, rather it is used indirectly through
// a class like <linkto class=LinearCoordinate>LinearCoordinate</linkto>.
// 
// The actual computations are performed by WCSLIB, written by Mark Calabretta
// of the ATNF.
// </synopsis>
//
// <example>
// See the test program tLinearXform.cc
// </example>
//
// <motivation>
// Factor out the common linear part of coordinate transformations.
// </motivation>
//
// <todo asof="1997/01/13">
//   <li> Allow different numbers of pixel and world axes.
// </todo>

class LinearXform
{
public:
    // Construct with "naxis" axes, CRPIX of 0, CDELT of 1, and a 
    // diagonal PC matrix.
    LinearXform(uInt naxis=1);

    // Construct the linear transformation from the supplied crpix and cdelt. The
    // PC matrix is the unit matrix. crpix and cdelt must have the same number
    // of elements.
    LinearXform(const Vector<Double> &crpix, const Vector<Double> &cdelt);

    // Construct a linear transformation, supplying all of crpix, cdelt, and pc.
    // The vectors must be of the same length "n", and the number of rows and
    // columns in the matrix must also be n.
    LinearXform(const Vector<Double> &crpix, const Vector<Double> &cdelt,
                 const Matrix<Double> &pcMatrix);

    // Copy constructor (copy sematics)
    LinearXform(const LinearXform &other);

    // Assignment (copy sematics)
    LinearXform &operator=(const LinearXform &other);

    // Destructor
    ~LinearXform();

    // Returns the number or world axes, which for this class is also the
    // number of pixel axes.
    uInt nWorldAxes() const;

    // Convert world coordinates to pixel coordinates (forward), or pixel 
    // coordinates to world (reverse). If the conversion works True is returned,
    // otherwise False is returned and errorMsg is set.  The output vectors
    // are resized appropriately.
    // <group>
    Bool forward(const Vector<Double> &world,
			 Vector<Double> &pixel, String &errorMsg) const;
    Bool reverse(Vector<Double> &world,
			 const Vector<Double> &pixel, String &errorMsg) const;
    // </group>

    // Retrieve the value of crpix, cdelt, and pc.
    // <group>
    Vector<Double> crpix() const;
    Vector<Double> cdelt() const;
    Matrix<Double> pc() const;
    // </group>

    // Set the value of crpix, cdelt, and pc. Note that since you can only
    // set one of them, you cannot change the dimensionality of the transform
    // using these functions. Instead use assignment on a temporary, i.e.:
    // <src> linxform = LinearXform (crpix,crval,pc); </src>
    // <group>
    void crpix(const Vector<Double> &newvals);
    void cdelt(const Vector<Double> &newvals);
    void pc(const Matrix<Double> &newvals);
    // </group>

    // Comparison function. Any private Double data members are compared
    // with the specified fractional tolerance.  
    // <group>
    Bool near(const LinearXform& other,
              Double tol=1e-6) const;
    Bool near(const LinearXform& other,
              const Vector<Int>& excludeAxes,
              Double tol=1e-6) const;
    // </group>


private:
    // A WCSLIB C-structure.
    linprm *linprm_p;
};

#endif


