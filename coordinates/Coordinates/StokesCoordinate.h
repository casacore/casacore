//# StokesCoordinate.h: Interconvert between pixel number and Stokes value.
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


#ifndef COORDINATES_STOKESCOORDINATE_H
#define COORDINATES_STOKESCOORDINATE_H

#include <casacore/casa/aips.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN



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
// is a poor fit to the Coordinate polymorphic model.
// You will probably find that if you try to use the Coordinate
// classes polymorphically, that StokesCoordinate will cause you
// to break the polymorphism.  I.e. you may have to deal with
// a specific StokesCoordinate)
//
// The StokesCoordinate just maintains a list (given in the constructor) of 
// possibly non-monotonic  Stokes values.   The values of the list are the
// world values (they are values from the Stokes::StokesTypes enum).
// Thus  world = list[pixel]  where pixel is the pixel coordinate (0,1, ...)
// 
// This means that concepts such as reference pixel, reference value,
// increment etc are meaningless for StokesCoordinate.  You can recover these
// these attributes, but the functions to set these attributes have no effect.
//
// Note also that for the StokesCoordinate relative world coordinates are defined to be the 
// as absolute, since there is no meaning for a relative Stokes world value (e.g.
// what is XX -RL ??   Relative pixel coordinates are defined.
//
// </synopsis>
//
// <note role=caution>
// All pixel coordinates are zero relative.
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
    // The output vectors are appropriately resized before use.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
                                const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
                                const Vector<Double> &world) const;
    // </group>

    // Interconvert between pixel and world as a Stokes type.
    // It returns False if no conversion could be done.
    // <group>
    Bool toPixel(Int &pixel, Stokes::StokesTypes stokes) const;
    Bool toWorld(Stokes::StokesTypes &stokes, Int pixel) const;
    // </group>

    // Interconvert between world stored as a Double and world stored as
    // a Stokes type.  Since these functions are static, any valid
    // Stokes type can be used.  The second function returns 
    // Stokes::Undefined if world is illegal.
    // <group>
    static Double toWorld (Stokes::StokesTypes stokes);
    static Stokes::StokesTypes toWorld (Double world);
    // </group>

    // Make absolute coordinates relative and vice-versa. 
    // For the StokesCoordinate relative world coordinates are defined to be the 
    // same as absolute world coordinates.  Relative pixels do have meaning
    // and are implemented (rel = abs - refPix)
    // <group>
    virtual void makePixelRelative (Vector<Double>& pixel) const;
    virtual void makePixelAbsolute (Vector<Double>& pixel) const;
    virtual void makeWorldRelative (Vector<Double>& world) const;
    virtual void makeWorldAbsolute (Vector<Double>& world) const;
    // </group>
 

    // Get the Stokes values (Stokes::StokesType) that we constructed 
    // with into a vector
    Vector<Int> stokes() const;

    // Get the stokes string representations
    Vector<String> stokesStrings() const;

    // Set a new vector of Stokes values (a vector of Stokes::StokesType) 
    void setStokes (const Vector<Int> &whichStokes);
   
    // Report the value of the requested attribute.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<Double> referencePixel() const;
    virtual Matrix<Double> linearTransform() const;
    virtual Vector<Double> increment() const;
    virtual Vector<Double> referenceValue() const;
    // </group>

    // Set the value of the requested attribute.  For the StokesCoordinate,
    // these have no effect (always return True) except for setWorldAxisNames.
    // <group>
    virtual Bool setWorldAxisNames(const Vector<String> &names);
    virtual Bool setReferencePixel(const Vector<Double> &refPix);
    virtual Bool setLinearTransform(const Matrix<Double> &xform);
    virtual Bool setIncrement(const Vector<Double> &inc) ;
    virtual Bool setReferenceValue(const Vector<Double> &refval) ;
    // </group>

    // The set function has no effect as the units must be empty for a StokesCoordinate
    // Always returns True
    // <group>
    virtual Bool setWorldAxisUnits(const Vector<String> &units);
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Set the world min and max ranges, for use in function <src>toMix</src>,
    // for  a lattice of the given shape (for this coordinate).
    // The implementation here gives world coordinates at the start 
    // and end of the Stokes axis.
    // The output vectors are resized.  Returns False if fails (and
    // then <src>setDefaultWorldMixRanges</src> generates the ranges)
    // with a reason in <src>errorMessage()</src>.
    // The <src>setDefaultWorldMixRanges</src> function
    // gives you [-1e99->1e99]. 
    // <group>
    virtual Bool setWorldMixRanges (const IPosition& shape);
    virtual void setDefaultWorldMixRanges ();
    //</group>

    // Format a StokesCoordinate world value with the common format 
    // interface (refer to the base class <linkto class=Coordinate>Coordinate</linkto>
    // for basics.
    //
    // A StokesCoordinate is formatted differently from other Coordinate
    // types.  The world value is converted to the character representation
    // as defined by the enum <src>StokesTypes</src> in the class
    // <linkto class=Stokes>Stokes</linkto>.
    //
    // Thus, all other arguments to do with formatting and precision are ignored.
    virtual String format(String& units,
                          Coordinate::formatType format,
                          Double worldValue,
                          uInt worldAxis,
                          Bool isAbsolute=True,
                          Bool showAsAbsolute=True,
                          Int precision = -1, Bool usePrecForMixed=False) const;

    // Comparison function. Any private Double data members are compared    
    // with the specified fractional tolerance.  Don't compare on the specified     
    // axes in the Coordinate.  If the comparison returns False,  method
    // errorMessage returns a message about why.
    // <group>
    virtual Bool near(const Coordinate& other,  
                      Double tol=1e-6) const;
    virtual Bool near(const Coordinate& other,  
                      const Vector<Int>& excludeAxes,
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


    // Comparison only made for specified axes in this and other Coordinate
    virtual Bool doNearPixel (const Coordinate& other,
                              const Vector<Bool>&  thisAxes,
                              const Vector<Bool>& otherAxes,
                              Double tol=1.0e-6) const; 

private:

    Bool toWorld(Double& world,  const Double pixel) const;
    Bool toPixel(Double& pixel,  const Double world) const;
//
    Block<Int> values_p;

    // Keep these for subimaging purposes.
    Double crval_p, crpix_p, matrix_p, cdelt_p;
    String name_p;
    String unit_p;
    Int nValues_p;

    // Undefined and inaccessible
    StokesCoordinate();
};

} //# NAMESPACE CASACORE - END


#endif

