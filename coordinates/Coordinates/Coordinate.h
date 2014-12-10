//# Coordinate.h: Interface for converting between world and pixel coordinates
//# Copyright (C) 1997,1999,2000,2001,2002,2003,2004
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


#ifndef COORDINATES_COORDINATE_H
#define COORDINATES_COORDINATE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>
#include <wcslib/wcs.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


template<class T> class Quantum;
template<class T> class Matrix;
class IPosition;
class RecordInterface;
class Projection;

// <summary>
// Interface for converting between world and pixel coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24">
// </reviewed>

// <prerequisite>
//   <li> Knowledge of astronomical coordinate conversions in general. Probably the
//        best documents are the papers by Mark Calabretta and Eric Greisen.
//        The initial draft from 1996 can be found at
//        http://www.atnf.csiro.au/~mcalabre.  It is this draft that the
//        Coordinate classes are based upon.  Since then, this paper has evolved
//        into three which can be found at the above address, and will be published in the
//        Astronomy and Astrophysics Supplement Series (probably in 2000).
//        The design has changed since the initial draft.  When these papers
//        are finalized, and the IAU has ratified the new standards, WCSLIB
//        (Mark Calabretta's implementation of these conventions) will be
//        revised for the new designs.  At that time, the Coordinate classes
//        may also be revised.
//   <li> Generic Casacore classes; especially those in the 
//        <linkto module=Arrays>Arrays</linkto> module.
//   <li> Perhaps some of the information in the
//        <linkto module=Measures>Measures</linkto> module.
// </prerequisite>
//
// <synopsis>
// The Coordinate class defines the generic interface whereby a pixel position
// is converted to a world (sky, frequency, stokes, ...) position and vice
// versa. The pixel and world coordinates are in general
// multi-dimensional values. In general there need not be the same number of
// pixel and world axes, although this will normally be the case.
//
// The fundamental model is that a pixel is first converted into a relative
// physical coordinate by:
// <ol>
//    <li> Subtracting a reference pixel value from the pixel location; then
//    <li> Multiplying this offset by a general transformation matrix (usually
//         to account for rotation, but any matrix is allowed); then
//    <li> Multiplying this product by an increment in physical units.
// </ol>
// After this linear stage, the final coordinate value is computed from this
// relative physical unit and a reference value, and possibly some other 
// parameters. In the case of a sky position, these latter include at least the
// projection type. In the case of a purely linear coordinate, the reference value
// is merely added to the relative physical coordinate. The interface also
// allows the axes to be assigned names (reasonable defaults will be selected),
// and for physical units.
// 
// Both absolute and relative coordinates are supported.  The main
// interface supports conversion between absolute pixel
// and absolute world coordinate.  There are then functions to
// convert absolute coordinates to relative and vice versa.
// A relative pixel coordinate is defined according to 
// 
// relative = absolute - reference
//
// A relative world coordinate is similar, although there may
// be deviations from this formula (e.g. for DirectionCoordinate
// a cos(latitude) term is incorporated and for StokesCoordinate
// relative world coordinates  are defined to be the same as
// absolute world coordinates.
//
// </synopsis>
//
// <note role=caution>
// All absolute pixels coordinates are zero relative.
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
// <thrown>
//   <li>  AipsError
//   <li>  AllocError
// </thrown>
//
// <todo asof="1997/1/13">
//   <li> Perhaps common FITS related interfaces should go in this class.
// </todo>
//

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
	// to mark DATA and ERROR values
	QUALITY,
	// A CoordinateSystem (a collection of Coordinates).
	COORDSYS };

    // This enum is used for formatting world values into Strings
    enum formatType {
       // Default; formatter decides
       DEFAULT,
       // Scientific format (e.g. -1.2397E+03)
       SCIENTIFIC,
       // Fixed floating format (e.g. 12.134)
       FIXED,
       // Either scientific or floating point, auto-selected by the C++
       // STL formatting routines.  May not be available for all Coordinate
       // types.
       MIXED,
       // HHH:MM:SS.SSS style formatting
       TIME };

    // Destructor.  Needs to be public so the user can delete Coordinate* objects
    virtual ~Coordinate();

    // List the type of this Coordinate object. 
    // <group>
    virtual Type type() const = 0;
    virtual String showType() const = 0;
    static String typeToString (Coordinate::Type type);
    // </group>

    // How many world/pixel axes are there in this Coordinate? While the number
    // of world and pixel axes will generally be the same, it is not a 
    // requirement. For example, in CoordinateSystem you could remove a pixel
    // axis and leave the corresponding world axis. Also, if we ever implement
    // a "SlicedCoordinate" class then there would be more world than pixel
    // coordinates (the pixel coordinate would be a pixel number along the slice,
    // whereas the world axes would continue to be RA/DEC).
    // <group>
    virtual uInt nPixelAxes() const = 0;
    virtual uInt nWorldAxes() const = 0;
    // </group>

    // Convert an absolute pixel position to an absolute world position or vice 
    // versa. Returns True
    // if the conversion succeeds, otherwise it returns False and method
    // errorMessage contains an error message. The input vector must be of length
    // <src>nPixelAxes</src> or <src>nWorldAxes</src>.  The output vector
    // is resized appropriately.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const = 0;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const = 0;
    // </group>

    // Mixed absolute pixel/world coordinate conversion.
    // worldIn and worldAxes are vectors of length <src>nWorldAxes</src>.
    // <src>pixelIn</src> and <src>pixelAxes</src> are of length <src>nPixelAxes</src>.
    // <src>worldAxes(i) = True</src> specifies you have given a world
    // value in <src>worldIn(i)</src> to convert to pixel.
    // <src>pixelAxes(i)=True</src> specifies you have given a pixel 
    // value in <src>pixelIn(i)</src> to convert to world.
    // You cannot specify the same axis via <src>worldAxes</src>
    // and <src>pixelAxes</src>.
    // Values in <src>pixelIn</src> are converted to world and
    // put into <src>worldOut</src> in the appropriate world axis
    // location.  Values in <src>worldIn</src> are copied to
    // <src>worldOut</src>.
    // Values in <src>worldIn</src> are converted to pixel and
    // put into <src>pixelOut</src> in the appropriate pixel axis
    // location.  Values in <src>pixelIn</src> are copied to
    // <src>pixelOut</src>.
    // <src>worldMin</src> and <src>worldMax</src> specify the range of the world
    // coordinate (in the world axis units of that world axis
    // in the CoordinateSystem) being solved for in a mixed calculation
    // for each world axis. They are only actually needed for DirectionCoordinates
    // and for all other Coordinates the relevant elements   
    // can be undefined.   If you don't know, use -180 to 180
    // degrees for longitude, and -90 to 90 for latitude.
    // Removed axes are handled (for example, a removed pixel
    // axis with remaining corresponding world axis will
    // correctly be converted to world using the replacement
    // value).
    // Returns True if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message. The output vectors
    // are resized.
    virtual Bool toMix(Vector<Double>& worldOut,
                       Vector<Double>& pixelOut,
                       const Vector<Double>& worldIn,
                       const Vector<Double>& pixelIn,
                       const Vector<Bool>& worldAxes,   
                       const Vector<Bool>& pixelAxes,
                       const Vector<Double>& worldMin,
                       const Vector<Double>& worldMax) const;

    // Set the world min and max ranges, for use in function <src>toMix</src>, for 
    // a lattice of the given shape for this coordinate. The default implementation
    // here sets the range for pixels dangling 25% off the image.
    // Returns False if fails with a reason  in <src>errorMessage()</src>.
    // setDefaultWorldMixRanges sets the range for each axis to +/-1e99
    // The ranges remain zero length vectors until you explicitly
    // initialize them.
    // <group>
    virtual Bool setWorldMixRanges (const IPosition& shape);
    virtual void setDefaultWorldMixRanges ();
    Vector<Double> worldMixMin () const {return worldMin_p;};
    Vector<Double> worldMixMax () const {return worldMax_p;};
    //</group>


    // Batch up a lot of transformations. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Returns False if any conversion
    // failed  and  <src>errorMessage()</src> will hold a message.
    // The <src>failures</src> array (True for fail, False for success)
    // is the length of the number of conversions and
    // holds an error status for each conversion.  The default
    // implementation is provided that works with the "single" version of
    // <src>toWorld</src> and <src>toPixel</src>, but for maximum efficiency these should be
    // overridden.
    // <group>
    virtual Bool toWorldMany(Matrix<Double>& world, 
                             const Matrix<Double>& pixel, 
                             Vector<Bool>& failures) const;
    virtual Bool toPixelMany(Matrix<Double>& pixel, 
                             const Matrix<Double>& world, 
                             Vector<Bool>& failures) const;
    // </group>

    // Make absolute coordinates relative and vice-versa (with
    // respect to the reference value).
    // Vectors must be length <src>nPixelAxes()</src> or
    // <src>nWorldAxes()</src> or memory access errors will occur
    // <group>
    virtual void makePixelRelative (Vector<Double>& pixel) const;
    virtual void makePixelAbsolute (Vector<Double>& pixel) const;
    virtual void makeWorldRelative (Vector<Double>& world) const;
    virtual void makeWorldAbsolute (Vector<Double>& world) const;
    // </group>

    // Make absolute coordinates relative and vice versa with respect
    // to the given reference value.  Add the other functions in this grouping
    // as needed. Vectors must be length <src>nPixelAxes()</src> or
    // <src>nWorldAxes()</src> or memory access errors will occur
    // <group>
    virtual void makeWorldAbsoluteRef (Vector<Double>& world,
                                       const Vector<Double>& refVal) const;
    // </group>


    // Batch up a lot of absolute/relative transformations. 
    // Parameters as above  for 
    // <src>toWorldMany</src> and <src>toPixelMany</src>
    // <group>
    virtual void makePixelRelativeMany (Matrix<Double>& pixel) const;
    virtual void makePixelAbsoluteMany (Matrix<Double>& pixel) const;
    virtual void makeWorldRelativeMany (Matrix<Double>& world) const;
    virtual void makeWorldAbsoluteMany (Matrix<Double>& world) const;
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

    // Change the units. Adjust the increment and
    // reference value by the ratio of the old and new units. This implies that
    // the units must be known <linkto class=Unit>Unit</linkto> strings, and that
    // they must be compatible, e.g. they can't change from time to length.
    //
    // A default implementation is available which does everything except set
    // the units vector, which must be done in the derived class.
    virtual Bool setWorldAxisUnits(const Vector<String> &units) = 0;

    // Find the Coordinate for when we Fourier Transform ourselves.  This pointer 
    // must be deleted by the caller. Axes specifies which axes of the Coordinate
    // you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the Coordinate. Currently the
    // output reference pixel is always shape/2.
    virtual Coordinate* makeFourierCoordinate (const Vector<Bool>& axes,
                                               const Vector<Int>& shape) const;

    // If the last conversion to world or pixel coordinates resulted in an
    // error, report that error. If the last conversion succeeded, it is
    // undefined what this will return (it might well contain the last error
    // message).
    const String& errorMessage() const;

    // Comparison to fractional tolerance (for floating point values). 
    // Don't compare on specified axes in Coordinate. If the comparison
    // returns False, <src>errorMessage()</src> contains a message.
    // <group>
    virtual Bool near(const Coordinate& other, 
                      Double tol=1.0e-6) const = 0;
    virtual Bool near(const Coordinate& other, 
                      const Vector<Int>& excludeAxes,
                      Double tol=1.0e-6) const = 0;
    // </group>


    // Provide a common interface to getting formatted representations of
    // coordinate values.    Different derived Coordinate types are formatted
    // in different ways.  For example, an RA/DEC  DirectionCoordinate
    // uses an HMS.SS/DMS.SS representation. A Galactic Lat/Long DirectionCoordinate
    // uses floating format in degrees.  Other derived Coordinates are formatted with 
    // scientific format or floating format. The derived class format functions
    // provide this functionality.   
    // 
    // You may specify the format with the format argument and a value
    // from the enum <src>Coordinate::formatType</src>. If you give it the value 
    // <src>Coordinate::DEFAULT</src> then a sensible default is used.
    //
    // A mechanism for specifying the precision number of significant digits after 
    // decimal point is provided.  You can specify the precision directly when 
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
    // The provided <src>worldValue</src> must be in the native units
    // of the Coordinate.  It may be an absolute (<src>isAbsolute=True</src>)
    // or relative (<src>isAbsolute=False</src>) value.   You may choose to
    // format the world value as absolute (<src>showAsAbsolute=True</src>) or
    // relative (<src>showAsAbsolute=False</src>).  <src>axis</src>
    // specifies which axis of the Coordinate this value belongs to.
    //
    // <src>units</src> specifies the units in which the input world value
    // will be formatted.  
    // If <src>units</src> is empty, the native unit for the given axis 
    // is used.
    //
    // Some derived classes will format in units different from the 
    // native unit of the Coordinate. The units of
    // the formatted number are returned in <src>units</src>.
    // If the <src>units</src> string is provided, the unit must be
    // consistent with the native unit of the coordinate.  The input
    // world value will be converted to this unit.
    //
    // You can also use the Quantum interface.  The units of the Quantum 
    // can then be anything  consistent with the Coordinate.
    // 
    // The default implementation here is to format only
    // with scientific or fixed formats. If precision is negative, a 
    // the default precision is used.
    //
    //<group>
    virtual void getPrecision(Int &precision,
                              Coordinate::formatType& format,
                              Bool showAsAbsolute,
                              Int defPrecScientific,
                              Int defPrecFixed,
                              Int defPrecTime) const;
    virtual String format(
    	String& units,
    	Coordinate::formatType format,
    	Double worldValue,
    	uInt axis,
    	Bool isAbsolute=True,
    	Bool showAsAbsolute=True,
    	Int precision=-1,
    	Bool usePrecForMixed=False
    ) const;

    String formatQuantity(String& units,
                          Coordinate::formatType format, 
                          const Quantum<Double>& worldValue, 
                          uInt axis, 
                          Bool isAbsolute=True,
                          Bool showAsAbsolute=True,
        		  Int precision=-1);
    //</group>

    // Used for persistence. Derived classes will have similar static
    // restore methods. It will typically only return False if fieldName
    // has already been defined.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const = 0;

    // Make a copy of ourself. This pointer has been allocated with
    // <src>new</src> and must be deleted by the caller.
    virtual Coordinate *clone() const = 0;

    // Comparison only made for specified axes in this and other Coordinate 
    // The default implementation should be ok for all Coordinate types
    // except Stokes and Quality...
    virtual Bool doNearPixel (const Coordinate& other, 
                              const Vector<Bool>&  thisAxes,
                              const Vector<Bool>& otherAxes,
                              Double tol=1.0e-6) const;

    // return the result of rotating the coordinate clockwise through the specified angle.
    // Rotation occurs about the reference pixel.
    // Coordinate must have exactly two pixel axes. The return type is the same
    // as the input type. It is the caller's responsibility to delete the returned pointer
    // when done with it to prevent a memory leak.
    // This method ultimately just changes the input coordinate's linear transform matrix.
    virtual Coordinate* rotate(const Quantum<Double>& angle) const;

protected:
    // Default constructor. Make an empty coordinate.  Used by derived classes.
    Coordinate();

    // Copy constructor (copy semantics)
    Coordinate(const Coordinate& other);

    // Assignment (copy semantics) 
    Coordinate& operator=(const Coordinate& other);

    // Set error message
    void set_error(const String &errorMsg) const;

    //
    Bool find_scale_factor(String &error, Vector<Double> &factor, 
			   const Vector<String> &units, 
			   const Vector<String> &oldUnits);


    // Tries to find a canonical unit for input unit (e.g.  GHz -> Hz), and
    // tells you the output name and unit for the Fourier coordinate 
    // pairing with the canonical unit
    void fourierUnits (String& nameOut, String& unitOut, String& unitInCanon,
                       Coordinate::Type type, Int axis, 
                       const String& unitIn, 
                       const String& nameIn) const;

   // Functions to interconvert pixel<->world via wcs.  These functions are called 
   // explicitly by the to{world,Pixel} functions in the appropriate wcs-based derived
   // classes. 
   // <group>
   Bool toWorldWCS (Vector<Double> &world, const Vector<Double> &pixel, wcsprm& wcs) const;
   Bool toPixelWCS(Vector<Double> &pixel,  const Vector<Double> &world, wcsprm& wcs) const;
   Bool toWorldManyWCS (Matrix<Double>& world, const Matrix<Double>& pixel,
                        Vector<Bool>& failures, wcsprm& wcs) const;
   Bool toPixelManyWCS (Matrix<Double>& pixel, const Matrix<Double>& world,
                        Vector<Bool>& failures, wcsprm& wcs) const;

   // Functions for handling conversion between the current units and
   // the wcs units. These are called explicitly by the appropriate 
   // derived class.
   // <src>convertFrom</src>
   // <group>
   void toCurrentMany (Matrix<Double>& world, const Vector<Double>& toCurrentFactors) const;
   void fromCurrentMany(Matrix<Double>& world, const Vector<Double>& toCurrentFactors) const;
   // </group>


   // Functions for handling conversion between the current reference frame 
   // and the native one. The default implementations do nothing.  They
   // should be over-ridden in the derived classes.
   // <group>
   virtual void convertTo (Vector<Double>&) const
     {}
   virtual void convertFrom (Vector<Double>&) const
     {}
   // </group>

   // Functions for handling conversion between the current reference frame 
   // and the native one for many conversions.  These functions just
   // call the virtual functions for single conversions.
   // <group>
   void convertToMany (Matrix<Double>& world) const;
   void convertFromMany (Matrix<Double>& world) const;
   // </group>

   // Interconvert between wcs PC cards and Matrix xForm format 
   void pcToXform (Matrix<Double>& xForm, const wcsprm& wcs) const;
   void xFormToPC (wcsprm& wcs, const Matrix<Double>& xForm) const;
   // </group>

   // Call wcsset on the wcs structure
   void set_wcs (wcsprm& wcs);

    // toMix ranges.  Should be set by derived class.
    Vector<Double> worldMin_p, worldMax_p;

private:
    mutable String error_p;

    // Check format type
    void checkFormat(Coordinate::formatType& format,         
                     const Bool absolute) const;

    void makeWorldAbsRelMany (Matrix<Double>& value, Bool toAbs) const; 
    void makePixelAbsRelMany (Matrix<Double>& value, Bool toAbs) const; 


};

//###### Inlines

inline const String& Coordinate::errorMessage() const
{
    return error_p;
}

} //# NAMESPACE CASACORE - END

#endif
