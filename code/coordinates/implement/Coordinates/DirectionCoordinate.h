//# DirectionCoordinate.h: Interconvert pixel positions and directions (e.g.,RA/DEC)
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_DIRECTION_COORDINATE_H)
#define AIPS_DIRECTION_COORDINATE_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/LinearXform.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Arrays/Vector.h>

class celprm;
class prjprm;

// <summary>
// Interconvert pixel positions and directions (e.g.,RA/DEC).
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto> defines the fundamental
//        interface to coordinate conversions.
//   <li> <linkto class=MDirection>MDirection</linkto> defines the types of
//        directions (J2000 etc.) which are defined. The measures machinery
//        also implements "astronomical" conversions which are outside the
//        scope of these coordinates (for example, <src>J2000</src> to
//        <src>B1950</src>).
//   <li> <linkto class=Projection>Projection</linkto> defines the types of
//        celestial projections which are available.
// </prerequisite>
//
// <synopsis>
// This class implements pixel to world coordinate conversions. It is important
// to understand that this class implements the only geometric conversions
// (e.g., SIN projection). Astronomical conversions (<src>RA/DEC <--> l,b</src>)
// are the responsibliy of the <linkto module=Measures>Measures</linkto> module.
// Of course the <linkto class=MDirection>MDirection</linkto> object you can
// obtain from this class would be the prime input for that conversion.
//
// The actual computations are carried out in WCSLIB, written by Mark Calabretta
// of the ATNF.
// </synopsis>
//
// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>.
// </example>
//
// <motivation>
// Directions in the sky are fundamental to astronomy.
// </motivation>
//
// <todo asof="1997/01/13">
//   <li> Add "wcsmix" capability.
// </todo>

class DirectionCoordinate : public Coordinate
{
public:
    // The default constructor creates a J2000 direction coordinate with a
    // CARtesion projection with longitude,latitude 0,0 at pixel 0,0 and an
    // increment of +1 radian per pixel on both axes.
    DirectionCoordinate();

    // Define the direction coordinate transformation. refLong and refLat will
    // normally the the RA/DEC of the pixel described by refX/refY. incLat/incLong
    // are the increment per pixel (RA is usually negative!), and the xform
    // matrix is usually the unit diagonal matrix unless you have a rotation or
    // some other linear transformation between the pixel and world axes.
    // 
    // Note that the units are RADIANS initially! You can change it to degrees
    // or something else with <src>setWorldAxisUnits()</src> later if you want.
    DirectionCoordinate(MDirection::Types directionType,
 			const Projection &projection,
			Double refLong, Double refLat,
			Double incLong, Double incLat,
			const Matrix<Double> &xform,
 			Double refX, Double refY);

    // Overwrite this DirectionCoordinate with the value of other (copy
    // semantics).
    // <group>
    DirectionCoordinate(const DirectionCoordinate &other);
    DirectionCoordinate &operator=(const DirectionCoordinate &other);
    // </group>

    virtual ~DirectionCoordinate();

    // Return <src>Coordinate::DIRECTION</src>
    virtual Coordinate::Type type() const;

    // Returns "Direction"
    virtual String showType() const;

    // Always 2.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message.   The output 
    // vectors are appropriately resized.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    // </group>

    // A convenient way to turn the world vector into an MDirection for further
    // processing in the Measures system.
    // 
    // We could improve the performance of this if it would be useful, however I
    // expect that normally you would just call this once to get a template
    // MDirection, and then call the vector versions. A similar toPixel is also
    // possible, but probably not needed.
    Bool toWorld(MDirection &world, const Vector<Double> &pixel) const;

    // Report the value of the requested attributed.
    // <group>
    MDirection::Types directionType() const;
    Projection projection() const;
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<String> worldAxisUnits() const;
    virtual Vector<Double> referenceValue() const;
    virtual Vector<Double> increment() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> referencePixel() const;
    // </group>

    // Set the value of the requested attribute.  Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names);
    virtual Bool setReferencePixel(const Vector<Double> &refPix);
    virtual Bool setLinearTransform(const Matrix<Double> &xform);
    virtual Bool setIncrement(const Vector<Double> &inc);
    virtual Bool setReferenceValue(const Vector<Double> &refval);
    // </group>

    // If <src>adjust</src> is True, the units must be compatible with
    // angle. The units are initially "rad" (radians).
    virtual Bool setWorldAxisUnits(const Vector<String> &units,
				   Bool adjust = True);

    // I think this should be in the MDirection class, but Wim
    // disagrees. Leave it here for now.
    static Vector<String> axisNames(MDirection::Types type, 
				    Bool FITSName = False);

    // Comparison function. Any private Double data members are compared    
    // with the specified fractional tolerance.  Don't compare on the specified     
    // axes in the Coordinate.  If the comparison returns False, 
    // <src>errorMessage()</src> contains a message about why.
     // <group>
    Bool near(const Coordinate* pOther, 
              Double tol=1e-6) const;
    Bool near(const Coordinate* pOther, 
              const Vector<Int>& excludeAxes,
              Double tol=1e-6) const;
    // </group>


    // Format a DirectionCoordinate coordinate world value nicely through the
    // common format interface.  Formatting types that are allowed are
    // <src>Coordinate::SCIENTIFIC, Coordinate::FIXED, Coordinate::TIME</src>
    // If you ask for format type <src>Coordinate::DEFAULT</src> then the
    // selected format depends upon what the value of the enum 
    // <src>MDirection::GlobalTypes</src> is for this DirectionCoordinate.
    // For example, if it is <src>GRADEC</src> or <src>GHADEC</src> you would
    // get <src>Coordinate::TIME</src> style formatting (DD:MM:SS.SS), otherwise
    // you would get <src>Coordinate::FIXED</src> formatting by default.
    //
    // <src>worldAxis</src> says which axis in this Coordinate we are formatting.  
    // We have to know this because we may format, say, RA and DEC differently.  
    // For <src>Coordinate::TIME</src> style formatting, <src>precision</src>
    // refers to the places after the decimal in the SS field.
    //<group>
    virtual void getPrecision (Int& precision, 
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

    // Save ourself into the supplised record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a coordinate system.
    static DirectionCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of ourself using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;
private:
    // Direction type
    MDirection::Types type_p;
    // Projection parameters
    Projection projection_p;
    // WCS structures
    // <group>
    celprm *celprm_p;
    prjprm *prjprm_p;
    // </group>

    // Performs the linear part of the transformation.
    LinearXform linear_p;

    // WCS computes in degrees - use this to convert back and forth between
    // degrees and the cuurently requested units.
    Double to_degrees_p[2];

    // Axis names.
    Vector<String> names_p;

    // Current units.
    Vector<String> units_p;

    // Interconvert between degrees and the current angular unit
    // <group>
    void toDegrees(Vector<Double> &other) const;
    void toOther(Vector<Double> &degrees) const;
    // </group>

    // Check formatting types
    void checkFormat(Coordinate::formatType& format,
                     const Bool absolute) const;
};

#endif
