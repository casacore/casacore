//# StokesCoordinate.h: Interconvert between pixel number and Stokes value.
//# Copyright (C) 1997,1998,1999,2000
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

#if !defined(AIPS_STOKES_COORDINATE_H)
#define AIPS_STOKES_COORDINATE_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>
#include <aips/Measures/Stokes.h>
#include <aips/Containers/Block.h>


// <summary>
// Interconvert between pixel and Stokes value.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tStokesCoordinate">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
//   <li> <linkto class=Stokes>Stokes</linkto>
// </prerequisite>
//
// <synopsis>
// Although not really a "coordinate", an axis where pixel numbers are used
// for different Stokes values is in wide use.     The StokesCoordinate
// is a somewhat poor fit to the Coordinate polymorphic model.
// You will probably find that if you try to use the Coordinate
// classes polymorphically, that StokesCoordinate will cause you
// to break the polymorphism.  I.e. you may have to deal with
// a specific StokesCoordinate)
//
// In particular, the StokesCoordinate really just maintains
// a list (given in the constructor) of possibly non-monotonic
// Stokes values.    A simple equation is used to convert between 
// an index of that list, and  workld value.  The formula
// is index = (pixel - crpix)*cdelt*linear + crval
// Now you could use the set* functions to change crval or cdelt etc
// However, you run the risk of breaking your StokesCoordinate
// if you do so.   This is because you may cause the computed
// index to be outside of the range of the Stokes actually
// contained in the StokesCoordinate and pixel/world conversion
// will fail.  This is quite different behaviour to the other 
// Coordinates, where you can change these coordinate descriptors
// as you wish.     So use these functions very carefully, if
// you must.
//
// </synopsis>
//
// <note role=caution>
// All pixels coordinates are zero relative.
// </note>
//
// </note>
//
// <example>
// In this example we create a StokesCoordinate housing IQUV
// <srcblock>
//    Vector<Int> iquv(4);
//    iquv(0) = Stokes::I; iquv(1) = Stokes::Q;
//    iquv(2) = Stokes::U; iquv(3) = Stokes::V;
//    StokesCoordinate stokes(iquv);           
// </srcblock>
// </example>
//
// <motivation>
// It is conventional to make a pseudo-axis of Stokes parameters.
// Another approach is to make an image type called Stokes.
// </motivation>
//
// <todo asof="1997/1/14">
//   <li> This could probably be generalized into an "enumeration axis" class.
// </todo>
// 

class StokesCoordinate : public Coordinate
{
public:

    // The length of whichStokes is the length of the axis, and the values
    // define which stokes are in which axis value. Often the vector will be of
    // length 4 and will contain Stokes::I, Q, U and V, however any valid value
    // from the stokes enum may be used. The values may not repeat however, e.g.
    // only one axis position may contain "I".
    explicit StokesCoordinate(const Vector<Int> &whichStokes);

    // Copy constructor (copy semantics)
    StokesCoordinate(const StokesCoordinate &other);

    // Assignment (copy semantics)
    StokesCoordinate &operator=(const StokesCoordinate &other);

    // Destructor.
    virtual ~StokesCoordinate();

    // Returns Coordinates::STOKES.
    virtual Coordinate::Type type() const;

    // Always returns the String "Stokes".
    virtual String showType() const;

    // Always returns 1.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>

    // Convert a pixel to a world coordinate or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and method
    // <src>errorMessage</src> returns an error message.
    //
    // The calculation that takes place in going from pixel to world is the
    // following.
    // <srcblock>
    // index = (pixel - crpix)*cdelt*linear + crval
    // </srcblock>
    // Where <src>pixel</src> is the pixel position, <src>crpix</src> is the
    // reference pixel, <src>cdelt</src> is the increment, <src>linear</src> is
    // the (1-pixel!) linear transformation matrix, and <src>crval</src> is the
    // reference value. Usually <src>crpix</src> and <src>crval</src> are 0.0, 
    // and <src>cdelt</src> and <src>linear</src> are 1.0. Once 
    // the index is calculated, it is used to look up the world
    // (Stokes) value from the internal list of stokes values (which is the
    // same as the vector which was given at construction time). The nearest
    // integer is taken, and values less than zero or beyond the end of the
    // vector result in an error. For world to pixel the reverse operation
    // happens.  The output vectors are appropriately resized before use.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    // </group>

    // Interconvert between pixel number and Stokes type.
    // <group>
    Bool toPixel(Int &pixel, Stokes::StokesTypes stokes) const;
    Bool toWorld(Stokes::StokesTypes &stokes, Int pixel) const;
    // </group>

    // Get the Stokes values (Stokes::StokesType) that we constructed 
   // with into a vector
    Vector<Int> stokes() const;

    // Report the value of the requested attribute.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<Double> referencePixel() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> increment() const;
    virtual Vector<Double> referenceValue() const;
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the value of the requested attribute.  Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names);
    virtual Bool setReferencePixel(const Vector<Double> &refPix);
    virtual Bool setLinearTransform(const Matrix<Double> &xform);
    virtual Bool setIncrement(const Vector<Double> &inc) ;
    virtual Bool setReferenceValue(const Vector<Double> &refval) ;
    // </group>

    // The units must be an empty string as there is no actual unit
    // for the Stokes axis.
    virtual Bool setWorldAxisUnits(const Vector<String> &units);

    // Format a StokesCoordinate world value with the common format 
    // interface (refer to the base class <linkto class=Coordinate>Coordinate</linkto>
    // for more details on this interface, particularly with regards to polymorphic use).  
    //
    // A StokesCoordinate is formatted differently from other Coordinate
    // types.  The world value is converted to the character representation
    // as defined by the enum <src>StokesTypes</src> in the class
    // <linkto class=Stokes>Stokes</linkto>.
    //
    // Thus, all arguments to do with formatting and precision are ignored.
    virtual String format(String& units,
                          Coordinate::formatType format,
                          Double worldValue,
                          uInt worldAxis,
                          Bool absolute,
                          Int precision = -1,
                          Bool native=True) const;

    // Comparison function. Any private Double data members are compared    
    // with the specified fractional tolerance.  Don't compare on the specified     
    // axes in the Coordinate.  If the comparison returns False,  method
    // errorMessage returns a message about why.
    // <group>
    virtual Bool near(const Coordinate*,  Double tol=1e-6) const;
    virtual Bool near(const Coordinate* pOther,  const Vector<Int>& excludeAxes,
                      Double tol=1e-6) const;
    // </group>
 
    // Save the StokesCoordinate into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // Recover the StokesCoordinate from a record.
    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a CoordinateSystem.
    static StokesCoordinate* restore(const RecordInterface &container,
                                     const String &fieldName);

    // Make a copy of the StokesCoordinate using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;

private:
    Block<Int> values_p;
    // Keep these for subimaging purposes.
    Double crval_p, crpix_p, matrix_p, cdelt_p;
    String name_p;
    String unit_p;

    // Undefined and inaccessible
    StokesCoordinate();
};

#endif
