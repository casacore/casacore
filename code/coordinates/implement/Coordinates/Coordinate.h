//# Coordinate.h: Interface for converting between world and pixel coordinates
//# Copyright (C) 1997,1999
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
// <note role=caution>
// All pixels coordinates are zero relative.
// </note>
//
// <example>
// This is a base class so there is no direct example, but
// see the example in <linkto module=Coordinates>Coordinates.h</linkto>
// for use of the derived classes.
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
	// A one-dimensional Cooordinate system, usually created from a table
        // although it can also be purely linear.
	TABULAR,
	// A coordinate system (a collection of coordinates).
	COORDSYS };

    // This enum is used for formatting world values into strings
    enum formatType {
       // Default; formatter decides
       DEFAULT,
       // Scientific format (e.g. -1.2397E+03)
       SCIENTIFIC,
       // Fixed floating format (e.g. 12.134)
       FIXED,
       // DDD:MM:SS.SSS style formatting 
       TIME };

    // Destructor
    virtual ~Coordinate();

    // List the type of this Coordinate object. Generally you shouldn't have
    // to call this function, it is used mostly in the CoordinateSystem class.
    // <group>
    virtual Type type() const = 0;
    virtual String showType() const = 0;
    // </group>

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

    // Convert a pixel position to a world position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and method
    // errorMessage contains an error message.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const = 0;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const = 0;
    // </group>


    // Batch up a lot of transformation. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Return the number of failures.
    // The failures array will be at least as long as the returned number of 
    // failures, and contains the indicies of the failed transformations.
    // errorMessage() will be set to the error from the FIRST failure. A default
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

    // Return the requested attributed.
    // <group>
    virtual Vector<String> worldAxisNames() const = 0;
    virtual Vector<Double> referencePixel() const = 0;
    virtual Matrix<Double> linearTransform() const = 0;
    virtual Vector<Double> increment() const = 0;
    virtual Vector<Double> referenceValue() const = 0;
    virtual Vector<String> worldAxisUnits() const = 0;
    // </group>

    // Set the requested attribute.  Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names) = 0;
    virtual Bool setReferencePixel(const Vector<Double> &refPix) = 0;
    virtual Bool setLinearTransform(const Matrix<Double> &xform) = 0;
    virtual Bool setIncrement(const Vector<Double> &inc)  = 0;
    virtual Bool setReferenceValue(const Vector<Double> &refval)  = 0;
    // </group>

    // Change the units. If adjust is True, adjust the increment and
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

    // Comparison to fractional tolerance (for floating point values). 
    // Don't compare on specified axes in coordinate. If the comparison
    // returns False, errorMessage() contains a message.
    // <group>
    virtual Bool near(const Coordinate* pOther, 
                      Double tol) const = 0;
    virtual Bool near(const Coordinate* pOther, 
                      const Vector<Int>& excludeAxes,
                      Double tol) const = 0;
    // </group>

    // Provide a common interface to getting formatted representations of
    // coordinate values.    Different derived coordinate types are formatted
    // in different ways.  For example, an RA/DEC  DirectionCoordinate
    // uses an HMS.SS/DMS.SS representation. A Galactic Lat/Long DirectionCoordinate
    // uses floating format in degrees.  Other derived coordinates are formatted with 
    // scientific format or floating format. The derived class format functions
    // provide this functionality.   
    // 
    // You may specify the format with the format argument and a value
    // from the enum Coordinate::formatType.  If 
    // you give it the value Coordinate::DEFAULT then a default
    // is taken.
    //
    // A mechanism for specifying the precision number of significant digits after 
    // decimal point) is provided.  You can specify the precision directly when 
    // calling format if it is unambiguous how the derived Coordinate is 
    // going to be formatted.  For example, a LinearCoordinate is always formatted with 
    // scientific format.  However, if you are using these classes polymorphically, you 
    // don't want to have to know this and some derived Coordinates may be formatted
    // in multiple ways (such as the DirectionCoordinate examples above).
    // Therefore, the function getPrecision enables 
    // you to set default precisions for the different styles of formatting 
    // used variously in the base and derived classes.   This function chooses the 
    // precision from these default values, according to the type of derived 
    // Coordinate that your object is and what value for format that
    // you give (refer to the derived classes for details on this).
    // 
    // Some derived classes will format differently depending upon whether
    // you want to format an absolute or offset world value input via 
    // absolute (e.g. DirectionCoordinates).
    //
    // Some derived classes will format in units different from that which
    // currently reflect the state of the CoordinateSystem.  The units of
    // the formatted number are returned in <src>units</src>.
    //
    // The default implementation here in this base class is to format only
    // with scientific or fixed formats.  absolute is ignored.
    // If precision is negative, a the default precision is used.
    //
    //<group>
    virtual void getPrecision(Int &precision,
                              Coordinate::formatType& format,
                              const Bool absolute,
                              const Int defPrecScientific,
                              const Int defPrecFixed,
                              const Int defPrecTime) const;
    virtual String format(String& units,
                          const Coordinate::formatType format, 
                          const Double worldValue, 
                          const uInt worldAxis, 
                          const Bool absolute,
 			  const Int precision = -1) const;
    //</group>

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

    // Check format type
    void checkFormat(Coordinate::formatType& format,         
                     const Bool absolute) const;

};

//###### Inlines

inline const String &Coordinate::errorMessage() const
{
    return error_p;
}

#endif


