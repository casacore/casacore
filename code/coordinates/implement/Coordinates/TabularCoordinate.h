//# TabularCoordinate.h: Table lookup 1-D coordinate, with interpolation
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

#if !defined(AIPS_TABULAR_COORDINATE_H)
#define AIPS_TABULAR_COORDINATE_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>

template<class Domain, class Range> class Interpolate1D;
class LogIO;

// <summary>
// Table lookup 1-D coordinate, with interpolation.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
// </prerequisite>
//
// <synopsis>
// This class is used where the world and pixel values are determined by a
// table. Off the pixel-values, a linear interpolation is used. The values
// returned by, e.g., increment, are based on the average of the whole table,
// i.e. the first position in the table to the last position. At present,
// the values must either increase or decrease monotonically.
// </synopsis>
//
// <example>
// </example>
//
// <todo asof="1997/07/12">
// <li> Allow interpolations other than linear.
// </todo>

class TabularCoordinate : public Coordinate
{
public:
    // Equivalent to TabularCoordinate(0,1,0, "", "Tabular");
    TabularCoordinate();
    // Create a pure linear coordinate system: 
    // <src>world = refval + inc*(pixel-refpix)</src>
    TabularCoordinate(Double refval, Double inc, Double refpix,
		      const String &unit, const String &axisName);

    // Construct a TabularCoordinate with the specified world values. The
    // increments and related functions return the <src>average</src> values
    // calculated from the first and last world values. The number of pixel
    // and world values must be the same. Normally the pixel values will be
    // 0,1,2,..., but this is not required.
    //
    // A linear interpolation/extrapolation is used for channels which are not
    // supplied. The refrence channel (pixel) is chosen to be 0.  The
    // frequencies must increase or decreas monotonically (otherwise the
    // toPixel lookup would not be possible).
    TabularCoordinate(const Vector<Double> &pixelValues,
		      const Vector<Double> &worldValues,
		      const String &unit, const String &axisName);
    

    // Overwrite this TabularCoordinate with other (copy semantics).
    // <group>
    TabularCoordinate(const TabularCoordinate &other);
    TabularCoordinate &operator=(const TabularCoordinate &other);
    // </group>

    virtual ~TabularCoordinate();

    // Always returns Coordinate::TABULAR
    virtual Coordinate::Type type() const;

    // Always returns "Tabular"
    virtual String showType() const;

    // Always returns 1.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    Bool toWorld(Double &world, Double pixel) const;
    Bool toPixel(Double &pixel, Double world) const;
    // </group>

    // Report the value of the requested attributed.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<Double> referencePixel() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> increment() const;
    virtual Vector<Double> referenceValue() const;
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the value of the requested attributed.  Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names);
    virtual Bool setReferencePixel(const Vector<Double> &refPix);
    virtual Bool setLinearTransform(const Matrix<Double> &xform);
    virtual Bool setIncrement(const Vector<Double> &inc) ;
    virtual Bool setReferenceValue(const Vector<Double> &refval);
    // </group>

    // Set the unit. If adjust is True, the unit must be compatible with
    // frequency.
    virtual Bool setWorldAxisUnits(const Vector<String> &units,
				   Bool adjust = True);

    // Get the table, i.e. the pixel and world values. The length of these
    // Vectors will be zero if this axis is pure linear.
    // <group>
    Vector<Double> pixelValues() const;
    Vector<Double> worldValues() const;
    // </group>

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

    // Save ourself into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a coordinate system.
    static TabularCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of ourself using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;
private:
    Double crval_p, cdelt_p, crpix_p;
    Double matrix_p;
    String unit_p;
    String name_p;

    // Channel_True = channel_corrections_p(Channel_average).
    // <group>
    Interpolate1D<Double,Double> *channel_corrector_p;
    Interpolate1D<Double,Double> *channel_corrector_rev_p;
    // </group>

    // Comon for assignment operator and destructor
    void clear_self();
    // Common code for copy ctor and assignment operator
    void copy(const TabularCoordinate &other);
};


#endif
