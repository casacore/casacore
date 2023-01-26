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
//    Vector<double> crpix(1); crpix = 0.0;
//    Vector<double> crval(1); crval = 100.0;
//    Vector<double> cdelt(1); cdelt = -10.0;
//    Matrix<double> pc(1,1); pc= 0; pc.diagonal() = 1.0;
//    Vector<String> name(1);  name = "length";
//    Vector<String> units(1); units = "km";
//
//    LinearCoordinate lin(names, units, crval, cdelt, pc, crpix);
// </srcblock>
//
// Now do a coordinate conversion
//
// <srcblock>
//   Vector<double> world, pixel(1);
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
    LinearCoordinate(uint32_t naxes = 1);

    // Construct the LinearCoordinate
    LinearCoordinate(const Vector<String> &names,
		     const Vector<String> &units,
		     const Vector<double> &refVal,
		     const Vector<double> &inc,
		     const Matrix<double> &pc,
		     const Vector<double> &refPix);

    // Construct LinearCoordinate with Quantum-based interface.
    // The units of the increment (<src>inc</src>) will be converted to
    // those of the reference value (<src>refVal</src>) which will
    // then serve as the units of the Coordinate.
    LinearCoordinate(const Vector<String> &names,
                     const Vector<Quantum<double> >& refVal,
                     const Vector<Quantum<double> >& inc,
                     const Matrix<double> &pc,
                     const Vector<double> &refPix);

    // Constructor from WCS structure; must hold ONLY a linear wcs structure
    // Specify whether the absolute pixel coordinates in the wcs structure
    // are 0- or 1-relative.  The coordinate is always constructed with 0-relative    
    // pixel coordinates
    LinearCoordinate(const wcsprm& wcs, bool oneRel=true);

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
    virtual uint32_t nPixelAxes() const;
    virtual uint32_t nWorldAxes() const;
    // </group>

    // Convert a pixel position to a worl position or vice versa. Returns true
    // if the conversion succeeds, otherwise it returns false and method
    // errorMessage returns an error message.  The output 
    // vectors are appropriately resized. The value of the bool parameter passed
    // to toWorld() has no effect as this type of coordinate does not support a
    // conversion layer frame.
    // <group>
    virtual bool toWorld(Vector<double> &world, 
			 const Vector<double> &pixel, bool=true) const;
    virtual bool toPixel(Vector<double> &pixel, 
			 const Vector<double> &world) const;
    // </group>


    // Return the requested attribute
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<double> referenceValue() const;
    virtual Vector<double> increment() const;
    virtual Matrix<double> linearTransform() const;
    virtual Vector<double> referencePixel() const;
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the value of the requested attributed. Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual bool setWorldAxisNames(const Vector<String> &names);
    virtual bool setReferencePixel(const Vector<double> &refPix);
    virtual bool setLinearTransform(const Matrix<double> &pc);
    virtual bool setIncrement(const Vector<double> &inc);
    virtual bool setReferenceValue(const Vector<double> &refval);
    // </group>

    // Set the world axis units. Adjust the increment and
    // reference value by the ratio of the old and new units.  
    // The units must be compatible with the current units.
    virtual bool setWorldAxisUnits(const Vector<String> &units);

    // Overwrite the world axis units with no compatibility 
    // checks or adjustment.
    bool overwriteWorldAxisUnits(const Vector<String> &units);

    // Comparison function. Any private double data members are compared    
    // with the specified fractional tolerance.  Don't 
    // compare on the specified     
    // axes in the Coordinate.  If the comparison returns false, method
    // errorMessage contains a message about why.
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
    // output reference pixel is always shape/2.  If the pointer returned is 0, 
    // it failed with a message in <src>errorMessage</src>
    virtual Coordinate* makeFourierCoordinate (const Vector<bool>& axes,
                                               const Vector<int32_t>& shape) const;

    // Save the LinearCoordinate into the supplied record using the supplied field name.
    // The field must not already exist, otherwise <src>false</src> is returned.
    virtual bool save(RecordInterface &container, const String &fieldName) const;

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
   void makeWCS (wcsprm& wcs, uint32_t naxis, const Vector<double>& refPix,
                 const Vector<double>& refVal,
                 const Vector<double>& incr,  
                 const Matrix<double>& pc,  
                 const Vector<String>& units,
                 const Vector<String>& names);
};

} //# NAMESPACE CASACORE - END


#endif

