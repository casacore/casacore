//# LinearXform.h: Perform a linear transform between input and output vectors
//# Copyright (C) 1997-2003
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

#ifndef COORDINATES_LINEARXFORM_H
#define COORDINATES_LINEARXFORM_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>

#include <wcslib/lin.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;

// <summary>
// Perform a linear transform between input and output vectors
// </summary>

// <use visibility=local>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tLinearXform">
// </reviewed>

// <prerequisite>
//   <li> General knowledge of Casacore Arrays
//   <li> Knowledge of FITS terminology in coordinate transformations
// </prerequisite>
//
// <synopsis>
// This class represents the common linear part of a FITS coordinate
// transformation. In particular it does the following:
// <srcblock>
// world = cdelt * PC * (pixel - crpix)
// </srcblock>
// Where PC is an NxN matrix; pixel, crpix (reference pixel) and world are
// length N vectors; and cdelt (increment) is an NxN diagonal matrix,
// represented as a length N vector.
//
// Normally this class isn't used directly, rather it is used indirectly through
// a class like <linkto class=LinearCoordinate>LinearCoordinate</linkto>.
//
// The actual computations are performed by WCSLIB, written by Mark Calabretta
// of the ATNF.
// </synopsis>
//
// <example>
//   Let's make a LinearXform housing two axes with a unit
//   diagonal PC matrix and convert from pixel to world
//
// <srcblock>
//    Vector<double> crpix(2), cdelt(2);
//    crpix(0) = 10.0; crpix(1) = 20.0;
//    cdelt(0) = 1.0; cdelt(1) = -1.0;
//    LinearXform lxf(crpix, cdelt);
//
//    String errMsg;
//    Vector<double> world, pixel(2);
//    pixel = 10.0;
//    bool ok = lxf.reverse(world, pixel, errMsg);
//    if (ok) {
//       cerr << "pixel, world = " << pixel << world << endl;
//    } else {
//       cerr << "Error : " << errMsg << endl;
//    }
// </srcblock>
// The answer should be a world vector with values 0 and -10.
// </example>
//
// <motivation>
// Factor out the common linear part of coordinate transformations.
// </motivation>
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="1997/01/13">
//   <li> Allow different numbers of pixel and world axes.
// </todo>
//


class LinearXform
{
public:
    // Construct with specified number of axes.  The reference pixel is
    // assumed to be 0, and the increment is assumed to be unity, and the
    // PC matrix is assumed to be diagonal.
    LinearXform(uint32_t naxis=1);

    // Construct the linear transformation from the supplied reference pixel
    // and increment. The PC matrix is the unit matrix.
    // <src>crpix</src> and <src>cdelt</src> must have the same number
    // of elements.
    LinearXform(const Vector<double> &crpix, const Vector<double> &cdelt);

    // Construct a linear transformation, supplying all of the reference pixel,
    // increment and PC matrix.
    // The vectors must be of the same length ("n") and the number of rows and
    // columns in the matrix must also be n.
    LinearXform(const Vector<double> &crpix, const Vector<double> &cdelt,
                const Matrix<double> &pc);

    // Copy constructor (copy sematics)
    LinearXform(const LinearXform &other);

    // Assignment (copy sematics)
    LinearXform &operator=(const LinearXform &other);

    // Destructor
    ~LinearXform();

    // Returns the number of world axes, which for this class is also the
    // number of pixel axes.
    uint32_t nWorldAxes() const;

    // Convert world coordinates to pixel coordinates (forward), or pixel
    // coordinates to world (reverse). If the conversion works true is returned,
    // otherwise false is returned and errorMsg is set.  The output vectors
    // are resized appropriately.
    // <group>
    bool forward(Vector<double> &pixel, const Vector<double> &world,
                        String &errorMsg) const;
    bool reverse(Vector<double> &world, const Vector<double> &pixel,
                        String &errorMsg) const;
    // </group>

    // Retrieve the value of crpix, cdelt, and pc.
    // <group>
    Vector<double> crpix() const;
    Vector<double> cdelt() const;
    Matrix<double> pc() const;
    // </group>

    // Set the value of crpix, cdelt, and pc. Note that since you can only
    // set one of them, you cannot change the dimensionality of the transform
    // using these functions. Instead use assignment on a temporary, i.e.:
    // <src> linxform = LinearXform (crpix,crval,pc); </src>
    // <group>
    void crpix(const Vector<double> &newvals);
    void cdelt(const Vector<double> &newvals);
    void pc(const Matrix<double> &newvals);
    // </group>

    // Invert the LinearXform ready for use in a Fourier Transformed Coordinate.
    // It is the callers responsibility to delete the pointer. If it fails
    // the pointer is 0 and an error message is provided
    LinearXform* fourierInvert (String& errMsg, const Vector<bool>& axes,
                               const Vector<double>& crpix,
                               const Vector<double>& scale) const;

    // Comparison function. Any private double data members are compared
    // with the specified fractional tolerance.  You can specify axes to
    // exclude from the comparison if you wish.
    // <group>
    bool near(const LinearXform& other,
              double tol=1e-6) const;
    bool near(const LinearXform& other,
              const Vector<int32_t>& excludeAxes,
              double tol=1e-6) const;
    // </group>

private:
    // A WCSLIB C-structure.
    mutable linprm linprm_p;

    bool isPCDiagonal_p;
    void set_linprm();
};

} //# NAMESPACE CASACORE - END

#endif
