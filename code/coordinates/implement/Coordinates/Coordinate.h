//# Coordinate.h: Interface for converting between world and pixel coordinates
//# Copyright (C) 1997
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

#if !defined(AIPS_COORDINATE_H)
#define AIPS_COORDINATE_H

#include <aips/aips.h>
#include <aips/Utilities/String.h>

template<class T> class Vector;
template<class T> class Matrix;
class RecordInterface;

// <summary>
// Interface for converting between world and pixel coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="dCoordinates">
// </reviewed>

// <prerequisite>
//   <li> Knowledge of astronomical coordinate conversions in general. Probably 
//        the most single useful document is "Representations of celestial 
//        coordinates in FITS" Eric W. Greisen and Mark Calabretta. This is the 
//        "WCS" paper, and is currently in late draft, available via anonymous 
//        ftp from fits.cv.nrao.edu.
//   <li> Generic AIPS++ classes; especially those in the 
//        <linkto module=Arrays>Arrays</linkto> module.
//   <li> Perhaps some of the information in the
//        <linkto module=Measures>Measures</linkto> module.
// </prerequisite>
//
// <synopsis>
// The Coordinate class defines the generic interface whereby a pixel position
// is converted to a world (sky, frequency, stokes, ...) position and vice
// versa. The pixel position and worl coordinate value are in general
// multi-dimensional values. In general there need not be the same number of
// pixel and world axes, although this will normally be the case.
//
// The fundamental model is that a pixel is first turned into a relative
// physical coordinate by:
// <ol>
//    <li> Subtracting a reference pixel value from the pixel location; then
//    <li> Multiplying this offset by a general transformation matrix (usually
//         to account for rotation, but any matrix is allowd); then
//    <li> Multiplying this product by an increment in physical units.
// </ol>
// After this linear stage, the final coordinate value is computed from this
// relative physical unit and a reference value, and possibly some other 
// parameters. In the case of a sky position, these latter include at least the
// projection type. In the case of a purely linar coordinate, the reference value
// is merely added to the relative physical coordinate. The interface also
// allows the axes to be assigned names (reasonable defaults will be selected),
// and for physical units.
// </synopsis>
//
// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>.
// </example>
//
// <motivation>
// Encapsulate the common interface to coordinate conversion so that it may
// be used polymorphically.
// </motivation>
//
// <todo asof="1997/1/13">
//   <li> Perhaps common FITS related interfaces should go in this class.
// </todo>

class Coordinate
{
public:
    // This enum lists the types of the derived classes. It is primarly used
    // in the CoordinateSystem class.
    enum Type { 
	// Linear axes.
	LINEAR, 
	// A direction. Usually RA/DEC.
	DIRECTION, 
	// A spectral axis.
	SPECTRAL, 
	// A Stokes axis.
	STOKES, 
	// A coordinate system (a collection of coordinates).
	COORDSYS };

    virtual ~Coordinate();

    // List the type of this Coordinate object. Generally you shouldn't have
    // to call this function, it is used mostly in the CoordinateSystem class.
    virtual Type type() const = 0;

    // How many world/pixel axes are there in this coordinate? While the number
    // of world and pixel axes will generally be the same, it is not a 
    // requirement. For example, in CoordinateSystem you could remove a pixel
    // axis and leave the corresponding world axes. Also, if we ever implement
    // a "SlicedCoordinate" class then there would be more world than pixel
    // coordinates (the pixel coordinate would be a pixel number along the slice,
    // whereas the world axes would continue to be RA/DEC).
    // <group>
    virtual uInt nPixelAxes() const = 0;
    virtual uInt nWorldAxes() const = 0;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const = 0;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const = 0;
    // </group>


    // Batch up a lot of transformation. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Return the number of failures.
    // The failues array will be at least as long as the returned number of 
    // failures, and contains the indicies of the failed transformations.
    // error() will be set to the error from the FIRST failure. A default
    // implementation is provided that works with the "single" version of
    // toWorld and toPixel, but for maximum efficiency these should be
    // overridden. If failures is longer than the return value, the value
    // in the excess locations is undefined.
    // <group>
    virtual uInt toWorldMany(Matrix<Double> &world, 
			      const Matrix<Double> &pixel, 
			      Vector<Int> &failures) const;
    virtual uInt toPixelMany(Matrix<Double> &pixel, 
			      const Matrix<Double> &world, 
			      Vector<Int> &failures) const;
    // </group>

    // Report the value of the requested attributed.
    // <group>
    virtual Vector<String> worldAxisNames() const = 0;
    virtual Vector<Double> referencePixel() const = 0;
    virtual Matrix<Double> linearTransform() const = 0;
    virtual Vector<Double> increment() const = 0;
    virtual Vector<Double> referenceValue() const = 0;
    virtual Vector<String> worldAxisUnits() const = 0;
    // </group>

    // Set the value of the requested attribute.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names) = 0;
    virtual Bool setReferencePixel(const Vector<Double> &refPix) = 0;
    virtual Bool setLinearTransform(const Matrix<Double> &xform) = 0;
    virtual Bool setIncrement(const Vector<Double> &inc)  = 0;
    virtual Bool setReferenceValue(const Vector<Double> &refval)  = 0;
    // </group>

    // Change the units. If <src>adjust</src> is True, adjust the increment and
    // reference value by the ratio of the old and new units. This implies that
    // the units must be known <linkto class=Unit>Unit</linkto> strings, and that
    // they must be compatible, e.g. they can't change from time to length.
    //
    // A default implementation is available which does everything except set
    // the units vector, which must be done in the derived class.
    virtual Bool setWorldAxisUnits(const Vector<String> &units,
				   Bool adjust = True) = 0;

    // If the last conversion to world or pixel coordinates resulted in an
    // error, report that error. If the last conversion succeeded, it is
    // undefined what this will return (it might well contain the last error
    // message).
    const String &errorMessage() const;

    // Provide a common interface to getting formatted representations of
    // coordinate values out. The default implementation merely turns
    // the number into a string using operator<<(Double). Derived classes
    // might, e.g., use an hms representation. sigDigits <=1 means make
    // your best guess.
    virtual String format(Double worldValue, uInt worldAxis, 
			  Int sigDigits = -1) const;

    // Used for persistence. Derived classes will have similar static
    // restore methods. It will typically only return False if fieldName
    // has already been defined.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const = 0;

    // Make a copy of ourself. This pointer has been allocated with
    // <src>new</src> and must be deleted by the caller.
    virtual Coordinate *clone() const = 0;
protected:
    void set_error(const String &errorMsg) const;
    Bool find_scale_factor(String &error, Vector<Double> &factor, 
			   const Vector<String> &units, 
			   const Vector<String> &oldUnits);
private:
    String error_p;
};

//###### Inlines

inline const String &Coordinate::errorMessage() const
{
    return error_p;
}

#endif


