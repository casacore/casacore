//# LinearCoordinate.h: Assume a general linear relation between pixel and world axes.
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
//#
//#
//# $Id$


#ifndef COORDINATES_LINEARCOORDINATE_H
#define COORDINATES_LINEARCOORDINATE_H

#include <casacore/casa/aips.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/casa/Arrays/Vector.h>
#include <wcslib/wcs.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Quantum;


// <summary>
// Interconvert between pixel and a linear world coordinate.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tLinearCoordinate"> 
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto> defines the fundamental
//        interface to coordinate conversions.
// </prerequisite>
//
// <synopsis>
// The LinearCoordinate class ties pixel and world axes together through 
// a general linear transformation. 
//
// <srcblock>
// world = (cdelt * PC * (pixel - crpix)) + crval
// </srcblock>
// Where PC is an NxN matrix; pixel, crval, crpix and world are length N 
// vectors, and cdelt is an NxN diagonal matrix, represented as a length 
// N vector.
//
// The LinearCoordinate can contain several uncoupled axes (similar to the way
// in which the DirectionCoordinate contains two axes).
// </synopsis>
//
// <note role=caution>
// All pixels coordinates are zero relative.
// </note>
//
// <example>
// Let's make a LinearCoordinate with just one axis containing
// a coordinate describing length.
// <srcblock>
//    Vector<Double> crpix(1); crpix = 0.0;
//    Vector<Double> crval(1); crval = 100.0;
//    Vector<Double> cdelt(1); cdelt = -10.0;
//    Matrix<Double> pc(1,1); pc= 0; pc.diagonal() = 1.0;
//    Vector<String> name(1);  name = "length";
//    Vector<String> units(1); units = "km";
//
//    LinearCoordinate lin(names, units, crval, cdelt, pc, crpix);
// </srcblock>
//
// Now do a coordinate conversion
//
// <srcblock>
//   Vector<Double> world, pixel(1);
//   pixel = 2.0;
//   if (!lin.toWorld(world, pixel)) {
//      cerr << "Error : " << lin.errorMessage() << endl;
//   } else {
//      cerr << "pixel, world = " << pixel << world << endl;
//   }
// </srcblock>
// The answer should of course be -20km.
// </example>
//
// <motivation>
// This class is intended for use with axes which do not have specific coordinate
// types. A "time" axis would be a good example.
// </motivation>
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="2000/01/01">
//   <li> Allow differing numbers of world and pixel axes. Requires a change in
//        WCS or use of a different library.
// </todo>
//


class LinearCoordinate : public Coordinate
{
public:
    // The default constructor makes a LinearCoordinate for which pixel 
    // and world coordinates are equal.  <src>naxes</src> gives the number
    // of axes in the Coordinate.
    LinearCoordinate(uInt naxes = 1);

    // Construct the LinearCoordinate
    LinearCoordinate(const Vector<String> &names,
		     const Vector<String> &units,
		     const Vector<Double> &refVal,
		     const Vector<Double> &inc,
		     const Matrix<Double> &pc,
		     const Vector<Double> &refPix);

    // Construct LinearCoordinate with Quantum-based interface.
    // The units of the increment (<src>inc</src>) will be converted to
    // those of the reference value (<src>refVal</src>) which will
    // then serve as the units of the Coordinate.
    LinearCoordinate(const Vector<String> &names,
                     const Vector<Quantum<Double> >& refVal,
                     const Vector<Quantum<Double> >& inc,
                     const Matrix<Double> &pc,
                     const Vector<Double> &refPix);

    // Constructor from WCS structure; must hold ONLY a linear wcs structure
    // Specify whether the absolute pixel coordinates in the wcs structure
    // are 0- or 1-relative.  The coordinate is always constructed with 0-relative    
    // pixel coordinates
    LinearCoordinate(const wcsprm& wcs, Bool oneRel=True);

    // Copy constructor (copy semantics).
    LinearCoordinate(const LinearCoordinate &other);

    // Assignment  (copy semantics).
    LinearCoordinate &operator=(const LinearCoordinate &other);

    // Destructor.
    virtual ~LinearCoordinate();

    // Returns Coordinate::LINEAR.
    virtual Coordinate::Type type() const;

    // Returns the String "Linear".
    virtual String showType() const;

    // Returns the number of pixel/world axes. The number of axes is arbitrary,
    // however the number of world and pixel axes must at present be the same.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and method
    // errorMessage returns an error message.  The output 
    // vectors are appropriately resized.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    // </group>


    // Return the requested attribute
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
    virtual Bool setLinearTransform(const Matrix<Double> &pc);
    virtual Bool setIncrement(const Vector<Double> &inc);
    virtual Bool setReferenceValue(const Vector<Double> &refval);
    // </group>

    // Set the world axis units. Adjust the increment and
    // reference value by the ratio of the old and new units.  
    // The units must be compatible with the current units.
    virtual Bool setWorldAxisUnits(const Vector<String> &units);

    // Overwrite the world axis units with no compatibility 
    // checks or adjustment.
    Bool overwriteWorldAxisUnits(const Vector<String> &units);

    // Comparison function. Any private Double data members are compared    
    // with the specified fractional tolerance.  Don't 
    // compare on the specified     
    // axes in the Coordinate.  If the comparison returns False, method
    // errorMessage contains a message about why.
    // <group>
    virtual Bool near(const Coordinate& other, 
                      Double tol=1e-6) const;
    virtual Bool near(const Coordinate& other, 
                      const Vector<Int>& excludeAxes,
                      Double tol=1e-6) const;
    // </group>

    // Find the Coordinate for when we Fourier Transform ourselves.  This pointer
    // must be deleted by the caller. Axes specifies which axes of the Coordinate
    // you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the Coordinate.   Currently the
    // output reference pixel is always shape/2.  If the pointer returned is 0, 
    // it failed with a message in <src>errorMessage</src>
    virtual Coordinate* makeFourierCoordinate (const Vector<Bool>& axes,
                                               const Vector<Int>& shape) const;

    // Save the LinearCoordinate into the supplied record using the supplied field name.
    // The field must not already exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container, const String &fieldName) const;

    // Restore the LinearCoordinate from a record.
    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a CoordinateSystem.
    static LinearCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of the LinearCoordinate using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;

private:

// An interface to the WCSLIB linear transformation routines.
    mutable ::wcsprm wcs_p;

// Copy private data
   void copy (const LinearCoordinate& other);

// Make wcs structure
   void makeWCS (wcsprm& wcs, uInt naxis, const Vector<Double>& refPix,
                 const Vector<Double>& refVal,
                 const Vector<Double>& incr,  
                 const Matrix<Double>& pc,  
                 const Vector<String>& units,
                 const Vector<String>& names);
};

} //# NAMESPACE CASACORE - END


#endif

