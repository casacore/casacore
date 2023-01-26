//# QualityCoordinate.h: Interconvert between pixel number and Quality value.
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003,2004
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


#ifndef COORDINATES_QUALITYCOORDINATE_H
#define COORDINATES_QUALITYCOORDINATE_H

#include <casacore/casa/aips.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/measures/Measures/Quality.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN



// <summary>
// Interconvert between pixel and Quality value.
// </summary>

// <use visibility=export>

// <reviewed tests="tQualityCoordinate">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
//   <li> <linkto class=Quality>Quality</linkto>
// </prerequisite>
//
// <synopsis>
// </synopsis>
//
// <note role=caution>
// All pixel coordinates are zero relative.
// </note>
//
// <example>
// In this example we create a QualityCoordinate containing 'DATA'
// and 'ERROR'
// <srcblock>
//   Vector<int32_t> newQuality(2);
//   newQuality(0) = Quality::DATA;
//   newQuality(1) = Quality::ERROR;
//   qual = QualityCoordinate(newQuality);
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//

class QualityCoordinate : public Coordinate
{
public:

    // The length of whichQuality is the length of the axis, and the values
    // define which quality are in which axis value. Often the vector will be of
    // length 2 and will contain Quality::DATA, and ERROR.
    explicit QualityCoordinate(const Vector<int32_t> &whichQuality);

    // Copy constructor (copy semantics)
    QualityCoordinate(const QualityCoordinate &other);

    // Assignment (copy semantics)
    QualityCoordinate &operator=(const QualityCoordinate &other);

    // Destructor.
    virtual ~QualityCoordinate();

    // Returns Coordinates::QUALITY.
    virtual Coordinate::Type type() const;

    // Always returns the String "Quality".
    virtual String showType() const;

    // Always returns 1.
    // <group>
    virtual uint32_t nPixelAxes() const;
    virtual uint32_t nWorldAxes() const;
    // </group>

    // Convert a pixel to a world coordinate or vice versa. Returns true
    // if the conversion succeeds, otherwise it returns false and method
    // <src>errorMessage</src> returns an error message.
    // The output vectors are appropriately resized before use.
    // The bool parameter in toWorld() is ignored as this coordinate does not
    // support a conversion layer frame.
    // <group>
    virtual bool toWorld(Vector<double> &world, 
                                const Vector<double> &pixel, bool=true) const;
    virtual bool toPixel(Vector<double> &pixel, 
                                const Vector<double> &world) const;
    // </group>

    // Interconvert between pixel and world as a Quality type.
    // It returns false if no conversion could be done.
    // <group>
    bool toPixel(int32_t &pixel, Quality::QualityTypes quality) const;
    bool toWorld(Quality::QualityTypes &quality, int32_t pixel) const;
    // </group>

    // Interconvert between world stored as a double and world stored as
    // a Quality type.  Since these functions are static, any valid
    // Quality type can be used.  The second function returns
    // Quality::Undefined if world is illegal.
    // <group>
    static double toWorld (Quality::QualityTypes quality);
    static Quality::QualityTypes toWorld (double world);
    // </group>

    // Make absolute coordinates relative and vice-versa. 
    // For the QualityCoordinate relative world coordinates are defined to be the
    // same as absolute world coordinates.  Relative pixels do have meaning
    // and are implemented (rel = abs - refPix)
    // <group>
    virtual void makePixelRelative (Vector<double>& pixel) const;
    virtual void makePixelAbsolute (Vector<double>& pixel) const;
    virtual void makeWorldRelative (Vector<double>& world) const;
    virtual void makeWorldAbsolute (Vector<double>& world) const;
    // </group>
 

    // Get the Quality values (Quality::QualityType) that we constructed
    // with into a vector
    Vector<int32_t> quality() const;

    // Set a new vector of Quality values (a vector of Quality::QualityType)
    void setQuality (const Vector<int32_t> &whichQuality);
   
    // Report the value of the requested attribute.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<double> referencePixel() const;
    virtual Matrix<double> linearTransform() const;
    virtual Vector<double> increment() const;
    virtual Vector<double> referenceValue() const;
    // </group>

    // Set the value of the requested attribute.  For the QualityCoordinate,
    // these have no effect (always return true) except for setWorldAxisNames.
    // <group>
    virtual bool setWorldAxisNames(const Vector<String> &names);
    virtual bool setReferencePixel(const Vector<double> &refPix);
    virtual bool setLinearTransform(const Matrix<double> &xform);
    virtual bool setIncrement(const Vector<double> &inc) ;
    virtual bool setReferenceValue(const Vector<double> &refval) ;
    // </group>

    // The set function has no effect as the units must be empty for a QualityCoordinate
    // Always returns true
    // <group>
    virtual bool setWorldAxisUnits(const Vector<String> &units);
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the world min and max ranges, for use in function <src>toMix</src>,
    // for  a lattice of the given shape (for this coordinate).
    // The implementation here gives world coordinates at the start 
    // and end of the Quality axis.
    // The output vectors are resized.  Returns false if fails (and
    // then <src>setDefaultWorldMixRanges</src> generates the ranges)
    // with a reason in <src>errorMessage()</src>.
    // The <src>setDefaultWorldMixRanges</src> function
    // gives you [-1e99->1e99]. 
    // <group>
    virtual bool setWorldMixRanges (const IPosition& shape);
    virtual void setDefaultWorldMixRanges ();
    //</group>

    // Format a QualityCoordinate world value with the common format
    // interface (refer to the base class <linkto class=Coordinate>Coordinate</linkto>
    // for basics.
    //
    // A QualityCoordinate is formatted differently from other Coordinate
    // types.  The world value is converted to the character representation
    // as defined by the enum <src>QualityTypes</src> in the class
    // <linkto class=Quality>Quality</linkto>.
    //
    // Thus, all other arguments to do with formatting and precision are ignored.
    virtual String format(String& units,
                          Coordinate::formatType format,
                          double worldValue,
                          uint32_t worldAxis,
                          bool isAbsolute=true,
                          bool showAsAbsolute=true,
                          int32_t precision = -1, bool usePrecForMixed=false) const;

    // Comparison function. Any private double data members are compared    
    // with the specified fractional tolerance.  Don't compare on the specified     
    // axes in the Coordinate.  If the comparison returns false,  method
    // errorMessage returns a message about why.
    // <group>
    virtual bool near(const Coordinate& other,  
                      double tol=1e-6) const;
    virtual bool near(const Coordinate& other,  
                      const Vector<int32_t>& excludeAxes,
                      double tol=1e-6) const;
    // </group>

    // Save the QualityCoordinate into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>false</src> is returned.
    virtual bool save(RecordInterface &container,
		    const String &fieldName) const;

    // Recover the QualityCoordinate from a record.
    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a CoordinateSystem.
    static QualityCoordinate* restore(const RecordInterface &container,
                                     const String &fieldName);

    // Make a copy of the QualityCoordinate using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;


    // Comparison only made for specified axes in this and other Coordinate
    virtual bool doNearPixel (const Coordinate& other,
                              const Vector<bool>&  thisAxes,
                              const Vector<bool>& otherAxes,
                              double tol=1.0e-6) const; 

private:

    bool toWorld(double& world,  const double pixel) const;
    bool toPixel(double& pixel,  const double world) const;
//
    Block<int32_t> values_p;

    // Keep these for subimaging purposes.
    double crval_p, crpix_p, matrix_p, cdelt_p;
    String name_p;
    String unit_p;
    int32_t nValues_p;

    // Undefined and inaccessible
    QualityCoordinate();
};

} //# NAMESPACE CASACORE - END


#endif

