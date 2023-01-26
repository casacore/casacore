//# TabularCoordinate.h: Table lookup 1-D coordinate, with interpolation
//# Copyright (C) 1997,1998,1999,2000,2001,2003,2004
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


#ifndef COORDINATES_TABULARCOORDINATE_H
#define COORDINATES_TABULARCOORDINATE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class Domain, class Range> class Interpolate1D;
template<class T> class Quantum;
class LogIO;



// <summary>
// Table lookup 1-D coordinate, with interpolation.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tTabularCoordinate">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
// </prerequisite>
//
// <synopsis>
// This class is used where the world and pixel values are determined by a
// lookup table. For fractional pixel values, a linear interpolation is used. 
// The values returned for, e.g., the increment, are based on 
// the average of the whole table.  At present,
// the values must either increase or decrease monotonically.
// </synopsis>
//
// <note role=caution>
// All pixels coordinates are zero relative.
// </note>
//
// <example>
// Let's make a non-linear TabularCoordinate  and convert a pixel
// value to world (which will use linear interpolation)
// <srcblock>
//    Vector<double> pixelValues(3); 
//    Vector<double> worldValues(3); 
//    pixelValues(0) = 122.0;
//    pixelValues(1) = 300.0;
//    pixelValues(2) = 6524.0;
//    worldValues(0) = 1.1e6;
//    worldValues(1) = 2.1e6;
//    worldValues(2) = 2.2e6;
//
//    String unit("km");
//    String axisName("length");
//
//    TabularCoordinate tc(pixelValues, worldValues, unit, axisName);
//
//    double world, pixel;
//    pixel = 200.12;
//    if (!tc.toWorld(world, pixel)) {
//      cerr << "Error : " << tc.errorMessage() << endl;
//    } else {
//      cerr << "pixel, world = " << pixel << ", " << world << endl;
//    }
// </srcblock>
// </example>
//
// <motivation>
// This class was motivated by the need for an irregular axis, such as a collection
// of frequencies.    For example, the SpectralCoordinate class contains a TabularCoordinate.
// </motivation>
//
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="1997/07/12">
// <li> Allow interpolations other than linear.
// </todo>


class TabularCoordinate : public Coordinate
{
public:
    // Default constructor.  It is equivalent to 
    // TabularCoordinate(0,1,0, "", "Tabular");
    TabularCoordinate();

    // Create a linear TabularCoordinate where 
    // <src>world = refval + inc*(pixel-refpix)</src>
    TabularCoordinate(double refval, double inc, double refpix,
		      const String &unit, const String &axisName);

    // Create a linear TabularCoordinate with a Quantum-based interface where 
    // <src>world = refval + inc*(pixel-refpix)</src>.  The units of the 
    // increment (<src>inc</src>) will be converted to
    // those of the reference value (<src>refVal</src>) which will
    // then serve as the units of the Coordinate.
    TabularCoordinate(const Quantum<double>& refval, 
                      const Quantum<double>& inc, 
                      double refpix, const String& axisName);

    // Construct a TabularCoordinate with the specified world values. The
    // increments and related functions return the average values
    // calculated from the first and last world values. The number of pixel
    // and world values must be the same. Normally the pixel values will be
    // 0,1,2,..., but this is not required.
    //
    // A linear interpolation/extrapolation is used for channels which are not
    // supplied. The reference channel (pixel) is chosen to be 0.  The
    // frequencies must increase or decrease monotonically (otherwise the
    // toPixel lookup would not be possible).
    TabularCoordinate(const Vector<double> &pixelValues,
		      const Vector<double> &worldValues,
		      const String &unit, const String &axisName);

    // Construct a TabularCoordinate with the specified world values
    // via the Quantum-based interface.  All comments for the
    // previous constructor apply
    TabularCoordinate(const Vector<double>& pixelValues,
                      const Quantum<Vector<double> >& worldValues,
                      const String &axisName);

    // Copy constructor (copy semantics).
    TabularCoordinate(const TabularCoordinate &other);

    // Assignment (copy semantics).
    TabularCoordinate &operator=(const TabularCoordinate &other);

    // Destructor.
    virtual ~TabularCoordinate();

    // Returns Coordinate::TABULAR.
    virtual Coordinate::Type type() const;

    // Always returns the String "Tabular".
    virtual String showType() const;

    // Always returns 1.
    // <group>
    virtual uint32_t nPixelAxes() const;
    virtual uint32_t nWorldAxes() const;
    // </group>

    // Convert a pixel position to a world position or vice versa. Returns true
    // if the conversion succeeds, otherwise it returns false and method
    // errorMessage contains an error message.  The output
    // vectors are appropriately resized.
    // The bool parameter in toWorld() has no effect as this coordinate does
    // not support a conversion layer frame.
    // <group>
    virtual bool toWorld(Vector<double> &world, 
			 const Vector<double> &pixel, bool=true) const;
    virtual bool toPixel(Vector<double> &pixel, 
			 const Vector<double> &world) const;
    bool toWorld(double &world, double pixel) const;
    bool toPixel(double &pixel, double world) const;
    // </group>

    // Batch up a lot of transformations. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Returns false if any conversion
    // failed  and  <src>errorMessage()</src> will hold a message.
    // The <src>failures</src> array (true for fail, false for success)
    // is the length of the number of conversions and
    // holds an error status for each conversion.  
    // <group>
    virtual bool toWorldMany(Matrix<double> &world,
                             const Matrix<double> &pixel,
                             Vector<bool>& failures) const;
    virtual bool toPixelMany(Matrix<double>& pixel,
                             const Matrix<double>& world,
                             Vector<bool>& failures) const;
    // </group>


    // Make absolute coordinates relative and vice-versa (with
    // respect to the referencfe value).
    // Vectors must be length <src>nPixelAxes()</src> or
    // <src>nWorldAxes()</src> or memory access errors will occur
    // <group>
    virtual void makePixelRelative (Vector<double>& pixel) const {pixel -= crpix_p;};
    virtual void makePixelAbsolute (Vector<double>& pixel) const {pixel += crpix_p;};
    virtual void makeWorldRelative (Vector<double>& world) const {world -= crval_p;};
    virtual void makeWorldAbsolute (Vector<double>& world) const {world += crval_p;};
    // </group>

    // Return the requested attribute.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<double> referencePixel() const;
    virtual Matrix<double> linearTransform() const;
    virtual Vector<double> increment() const;
    virtual Vector<double> referenceValue() const;
    // </group>

    // Set the value of the requested attribute.  Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual bool setWorldAxisNames(const Vector<String> &names);
    virtual bool setReferencePixel(const Vector<double> &refPix);
    virtual bool setLinearTransform(const Matrix<double> &xform);
    virtual bool setIncrement(const Vector<double> &inc) ;
    virtual bool setReferenceValue(const Vector<double> &refval);
    // </group>

    // Set/get the axis unit. Adjust the increment and
    // reference value by the ratio of the old and new units.
    // The unit must be compatible with the current units.
    // <group>
    virtual bool setWorldAxisUnits(const Vector<String> &units);
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Overwrite the world axis units with no compatibility
    // checks or adjustment.
    bool overwriteWorldAxisUnits(const Vector<String> &units);

    // Get the table, i.e. the pixel and world values. The length of these
    // Vectors will be zero if this axis is pure linear.
    // <group>
    Vector<double> pixelValues() const;
    Vector<double> worldValues() const;
    // </group>

    // Comparison function. Any private double data members are compared    
    // with the specified fractional tolerance.  Don't compare on the specified     
    // axes in the Coordinate.  If the comparison returns false, method
    // errorMessage() contains a message about why.
    // <group>
    virtual bool near(const Coordinate& other, 
                      double tol=1e-6) const;
    virtual bool near(const Coordinate& other, 
                      const Vector<int32_t>& excludeAxes,
                      double tol=1e-6) const;
    // </group>

    // Find the Coordinate for when we Fourier Transform ourselves.  This pointer
    // must be deleted by the caller. Axes specifies which axes of the Coordinate
    // you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the Coordinate.   Currently the
    // output reference pixel is always shape/2. If the pointer returned is 0, 
    // it failed with a message in <src>errorMessage</src>
    virtual Coordinate* makeFourierCoordinate (const Vector<bool>& axes,
                                               const Vector<int32_t>& shape) const;

    // Save the TabularCoordinate into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>false</src> is returned.
    virtual bool save(RecordInterface &container, const String &fieldName) const;

    // Recover the TabularCoordinate from a record.
    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a CoordinateSystem.
    static TabularCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of the TabularCoordinate using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;

private:
    double crval_p, cdelt_p, crpix_p;
    double matrix_p;
    String unit_p;
    String name_p;

    // Channel_True = channel_corrections_p(Channel_average).
    // <group>
    Interpolate1D<double,double> *channel_corrector_p;
    Interpolate1D<double,double> *channel_corrector_rev_p;
    // </group>

    // Common for assignment operator and destructor.
    void clear_self();

    // Common code for copy ctor and assignment operator.
    void copy(const TabularCoordinate &other);

    void makeNonLinearTabularCoordinate(const Vector<double> &pixelValues,
                                        const Vector<double> &worldValues);
};

} //# NAMESPACE CASACORE - END


#endif
