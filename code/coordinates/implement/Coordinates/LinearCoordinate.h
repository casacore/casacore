//# LinearCoordinate.h: Assume a general linear relation between pixel and world axes.
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

#if !defined(AIPS_LINEAR_COORDINATE_H)
#define AIPS_LINEAR_COORDINATE_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/LinearXform.h>
#include <aips/Arrays/Vector.h>

// <summary>
// Assume a general linear relation between pixel and world axes.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto> defines the fundamental
//        interface to coordinate conversions.
// </prerequisite>
//
// <synopsis>
// The LinearCoordinate class ties pixel and world axes together through a general
// linear transformation. 
//
// <srcblock>
// world = (cdelt * PC * (pixel - crpix)) + crval
// </srcblock>
// Where PC is an NxN matrix, pixel, crval, crpix and world are length N vectors, and
// cdelt is an NxN diagonal matrix, represented as a length N vector.
//
// The actual computations are carried out in WCSLIB, written by Mark Calabretta
// of the ATNF.
//
// </synopsis>
//
// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>.
// </example>
//
// <motivation>
// This class is intended for use for axes which do not have specific coordinate
// types. A "time" axis would be a good example.
// </motivation>
//
// <todo asof="1997/01/14">
//   <li> Allow differing numbers of world and pixel axes. Requres a change in
//        WCS or use of a different library.
// </todo>

class LinearCoordinate : public Coordinate
{
public:
    // The default constructor just copies the pixel coordinate through to the
    // world coordinate and vice versa.
    LinearCoordinate(uInt naxis = 1);
    // Set up the full linear transformation.
    LinearCoordinate(const Vector<String> &names,
		     const Vector<String> &units,
		     const Vector<Double> &refVal,
		     const Vector<Double> &inc,
		     const Matrix<Double> &xform,
		     const Vector<Double> &refPix);

    // Overwrite this linear coordinate with "other" (copy semantics).
    // <group>
    LinearCoordinate(const LinearCoordinate &other);
    LinearCoordinate &operator=(const LinearCoordinate &other);
    // </group>

    virtual ~LinearCoordinate();

    // Return Coordinate::LINEAR.
    virtual Coordinate::Type type() const;

    //Returns "Linear"
    virtual String showType() const;

    // Returns the number of pixel/world axes. The number of axes is arbitrary,
    // however the number or world and pixel axes must at present be the same.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message.  The output 
    // vectors are appropriately resized.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    // </group>

    // Report the value of the requested attributed.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<Double> referenceValue() const;
    virtual Vector<Double> increment() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> referencePixel() const;
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the value of the requested attributed. Note that these just
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

    // Comparison function. Any private Double data members are compared    
    // with the specified fractional tolerance.  Don't compare on the specified     
    // axes in the Coordinate.  If the comparison returns False, 
    // <src>errorMessage()</src> contains a message about why.
    // <group>
    virtual Bool near(const Coordinate* pOther, 
                      Double tol=1e-6) const;
    virtual Bool near(const Coordinate* pOther, 
                      const Vector<Int>& excludeAxes,
                      Double tol=1e-6) const;
    // </group>

    // Format a LinearCoordinate world value with the common format
    // interface (refer to the base class <linkto class=Coordinate>Coordinate</linkto>
    // for more details on this interface, particularly with regards polymorphic use).
    // A LinearCoordinate can be formatted in either <src>Coordinate::SCIENTIFIC</src>
    // or <src>Coordinate::FIXED</src> formats only.  The argument <src>absolute</src>
    // is ignored.
    //<group>
    virtual void getPrecision(Int& precision,
                              Coordinate::formatType& format,
                              const Bool absolute,
                              const Int defPrecSci, 
                              const Int defPrecFloat,
                              const Int defPrecRADEC) const;
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
    static LinearCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of ourself using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;
private:
    // An interface to the WCSLIB linear transformation routines.
    LinearXform transform_p;

    // Names and units.
    // <group>
    Vector<String> names_p;
    Vector<String> units_p;
    // </group>

    // The reference value.
    Block<Double> crval_p;

    // Check format type
    void checkFormat(Coordinate::formatType& format,
                     const Bool absolute) const;

};

#endif
