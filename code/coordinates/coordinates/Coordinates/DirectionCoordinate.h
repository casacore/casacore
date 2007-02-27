//# DirectionCoordinate.h: Interconvert pixel positions and directions (e.g. RA/DEC)
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
//# $Id: 


#ifndef COORDINATES_DIRECTIONCOORDINATE_H
#define COORDINATES_DIRECTIONCOORDINATE_H

#include <casa/aips.h>
#include <coordinates/Coordinates/Coordinate.h>
#include <coordinates/Coordinates/Projection.h>
#include <casa/Arrays/Vector.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MeasConvert.h>
#include <casa/Quanta/RotMatrix.h>
#include <wcslib/wcs.h>

class celprm;
class prjprm;
class wcsprm;

namespace casa { //# NAMESPACE CASA - BEGIN

class MVDirection;
class MVAngle;
class LogIO;
template<class T> class Quantum;


// <summary>
// Interconvert pixel positions and directions (e.g. RA/DEC).
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tDirectionCoordinate"> 
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
// This class implements pixel to world coordinate conversions. This class
// implements geometric conversions (e.g. SIN projection) via the WCS library
// and also provides an interface to astronomical conversions (RA/DEC <--> l,b)
// via the <linkto module=Measures>Measures</linkto> module.
// </synopsis>
//
//
// <note role=caution>
// All absolute pixels coordinates are zero relative.
// </note>
//
// <example>
// Let's make a DirectionCoordinate --- used to represent a direction,
// usually an RA/DEC, but it could also be, e.g., an AZ/EL pair.
// <srcblock>
//    Matrix<Double> xform(2,2);                                    // 1
//    xform = 0.0; xform.diagonal() = 1.0;                          // 2
//    DirectionCoordinate radec(MDirection::J2000,                  // 3 
//                            Projection(Projection::SIN),          // 4 
//                            135*C::pi/180.0, 60*C::pi/180.0,      // 5
//                            -1*C::pi/180.0, 1*C::pi/180,          // 6
//                            xform,                                // 7
//                            128, 128);                            // 8   
// </srcblock>
// <ul>
//    <li> <i>1-2:</i>Here we set up a diagonal transformation matrix.      
//         Normally this matrix should be diagonal, however if you wanted
//         to introduce a rotation or skew, you would do it through this
//         matrix.
//    <li> <i>3:</i>This defines the astronomical type of the world
//         coordinate. Most of the time it will probably be J2000   
//         or B1950, but many other possibilities are possible as listed
//         in the <linkto class=MDirection>MDirection</linkto> class
//         header.
//    <li> <i>4:</i>The <linkto class=Projection>Projection</linkto> class
//         defines the "geometry" that is used to map <src>xy<-->world</src>. SIN
//         is the most common projection for radio interferometers. Note that
//         SIN can optionally take parameters as defined in Calabretta and Greisen.
//         If not provided, they default to 0.0, which is the "old" SIN
//         convention.
//    <li> <i>5:</i>Set the reference position to RA=135, DEC=60 degrees.
//         Note that the native units of a Direction is radians.
//    <li> <i>6:</i> Set the increments to -1 degree in RA, and +1 degree
//         in DEC.
//    <li> <i>7:</i> Set the previously defined transformation matrix.
//    <li> <i>8:</i> Set the zero-relative reference pixel. Note that it does
//         not have to be incremental. At the reference pixel, the world 
//         coordinate has the reference value.
// </ul>
// 
// In this example is is more convenient to change the units to degrees. This can
// be accomplished as follows:
// <srcblock>
//    Vector<String> units(2); units = "deg";                       //  9
//    radec.setWorldAxisUnits(units);                               // 10
// </srcblock>
// The increment and reference value are updated appropriately.  
// 
// Set up a couple of vectors to use the world and pixel coordinate values.  
// <srcblock>
//    Vector<Double> world(2), pixel(2);                            // 11
//    pixel = 138.0;                                                // 12
// </srcblock>
// We use 138 as an arbitrary pixel position which is near the reference pixel
// so we can tell if the answers look foolish or not.
// We can actually perform a transformation like this as follows. If
// it succeeds we print the value of the world coordinate.
// <srcblock>
//    Bool ok = radec.toWorld(world, pixel);                        // 13
//    if (!ok) {                                                    // 14 
//      cout << "Error: " << radec.errorMessage() << endl;          // 15
//      return 1;                                                   // 16
//    }                                                             // 17
//    cout << world << " <--- " << pixel << endl;         // 18
// </srcblock>
// There is an overloaded "toWorld" function that produces an MDirection
// in case you want to, e.g., find out what the position in B1950 coordinates
// would be.
//                              
// The reverse transformation takes place similarly:
// <srcblock>
//    ok = radec.toPixel(pixel, world);                             // 19   
// </srcblock>
// </example>
//
// <example>
// We could also have made the above DirectionCoordinate using the Quantum-based
// constructor, which is a little more elegant if you want to use degrees.
//
//    Matrix<Double> xform(2,2);                
//    xform = 0.0; xform.diagonal() = 1.0;                 
//    Quantum<Double> refLon(135.0, "deg");
//    Quantum<Double> refLat(60.0, "deg");
//    Quantum<Double> incLon(-1.0, "deg");
//    Quantum<Double> incLat(1.0, "deg");
//    DirectionCoordinate radec(MDirection::J2000,         
//                            Projection(Projection::SIN), 
//                            refLon, refLat,
//                            incLon, incLat,
//                            xform,      
//                            128, 128);  
//
// But note that the constructor will have converted the native units
// of the DirectionCoordinate to radians.  So the Double-based toWorld and
// toPixel functions will be in terms of radians.   If you want the native
// units to be degrees, then again you can use 
//
// <srcblock>
//    Vector<String> units(2); units = "deg";         
//    radec.setWorldAxisUnits(units);                 
// </srcblock>
// and thereafter degrees are the native units.
// </example>
//
// <motivation>
// Directions in the sky are fundamental to astronomy.
// </motivation>
//
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="2000/01/01">
//   <li> Nothing
// </todo>


class DirectionCoordinate : public Coordinate
{
public:
    // The default constructor creates a J2000 DirectionCoordinate with a
    // CARtesion projection with longitude,latitude 0,0 at pixel 0,0 and an
    // increment of +1 radian per pixel on both axes.
    DirectionCoordinate();

    // Define the DirectionCoordinate transformation. <src>refLong</src> and 
    // <src>refLat</src> will normally the the RA/DEC of the pixel described by 
    // <src>refX/refY</src>. <src>incLat/incLong</src>
    // are the increments per pixel (RA is usually negative), and the <src>xform</src>
    // matrix is usually the unit diagonal matrix unless you have a rotation or
    // some other linear transformation between the pixel and world axes.
    // 
    // Note that the units are radians initially. You can change it to degrees
    // or something else with the <src>setWorldAxisUnits</src> method later if you want.
    // 
    // longPole and latPole are defined by Calabretta and Greisen (these
    // are reference points not at the native pole).  In general
    // you can leave these out and the default values will cause them
    // to be computed appropriately.  However, when reading from FITS
    // the LONPOLE and LATPOLE keywords are passed along here.
    DirectionCoordinate(MDirection::Types directionType,
 			const Projection &projection,
			Double refLong, Double refLat,
			Double incLong, Double incLat,
			const Matrix<Double> &xform,
 			Double refX, Double refY,
                        Double longPole=999.0, Double latPole=999.0);

    // Create DirectionCoordinate with Quantum-based interface. 
    // Parameters are the same as above.
    // Regardless of the units of the quanta, the initial units
    // of the DirectionCoordinate will be converted radians.
    // You can change it to degrees or something else with the 
    // setWorldAxisUnits method later if you want.
    //
    // longPole and latPole are defined by Calabretta and Greisen (these
    // are reference points not at the native pole).  In general
    // you can leave these out and the default values will cause them
    // to be computed appropriately.  However, when reading from FITS
    // the LONPOLE and LATPOLE keywords are passed along here.
    // To get the default the 999.0 value should be used (units
    // are irrelevant in that case)
    DirectionCoordinate(MDirection::Types directionType,
                        const Projection &projection,
                        const Quantum<Double>& refLong, 
                        const Quantum<Double>& refLat,
                        const Quantum<Double>& incLong, 
                        const Quantum<Double>& incLat,
                        const Matrix<Double> &xform,
                        Double refX, Double refY,
                        const Quantum<Double>& longPole=Quantum<Double>(999.0,Unit("rad")),
                        const Quantum<Double>& latPole=Quantum<Double>(999.0,Unit("rad")));

    // Constructor from WCS structure; must hold ONLY a celestial wcs structure
    // Specify whether the absolute pixel coordinates in the wcs structure
    // are 0- or 1-relative.  The coordinate is always constructed with 0-relative
    // pixel coordinates
    DirectionCoordinate(MDirection::Types directionType,
                        const ::wcsprm& wcs, Bool oneRel=True);

    // Copy constructor (copy semantics)
    DirectionCoordinate(const DirectionCoordinate &other);

    // Assignment (copy semantics).
    DirectionCoordinate &operator=(const DirectionCoordinate &other);

    // Destructor
    virtual ~DirectionCoordinate();

    // Return Coordinate::DIRECTION
    virtual Coordinate::Type type() const;

    // Always returns the String "Direction".
    virtual String showType() const;

    // Always returns 2.
    // <group>
    virtual uInt nPixelAxes() const;
    virtual uInt nWorldAxes() const;
    // </group>


    // Set extra conversion type.  Whenever a conversion from pixel to world is done,
    // the world value is then further converted to this MDirection::Types value.
    // For example, your DirectionCoordinate may be defined in J2000.
    // You can use this to get the world values out in say GALACTIC.
    // Similarly, whenever you convert from world to pixel, the world
    // value is assumed to be that appropriate to the conversionDirectionType.
    // It is first converted to the MDirection::Types with which the
    // DirectionCoordinate was constructed and from there to pixel.
    // If you don't call this function, or you set the same type
    // for which the DirectionCoordinate was constructed, no extra
    // conversions occur.   Some conversions will fail.  These are the
    // ones that require extra frame information (epoch, position) such
    // as to AZEL from J2000 etc.  This will be added later.
    //
    // In the mixed pixel/world conversion routine <src>toMix</src>
    // the implementation is only partial.  See the comments for this
    // function below.
    // <group>
    void setReferenceConversion (MDirection::Types type);
    void getReferenceConversion (MDirection::Types& type) const
       {type=conversionType_p;};
    // </group>

    // Convert a pixel position to a world position or vice versa. Returns True
    // if the conversion succeeds, otherwise it returns False and method
    // errorMessage returns an error message.   The output 
    // vectors are appropriately resized.
    // <group>
    virtual Bool toWorld(Vector<Double> &world, 
			 const Vector<Double> &pixel) const;
    virtual Bool toPixel(Vector<Double> &pixel, 
			 const Vector<Double> &world) const;
    // </group>

    // Mixed pixel/world coordinate conversion.
    // <src>worldIn</src> and <src>worldAxes</src> are of length 
    // nWorldAxes. 
    // <src>pixelIn</src> and <src>pixelAxes</src> are of length nPixelAxes.
    // <src>worldAxes(i)=True</src> specifies you have given a world
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
    //
    // <src>worldMin</src> and <src>worldMax</src> specify the range of the world
    // coordinate (in the world axis units of that world axis
    // in the CoordinateSystem) being solved for in a mixed calculation
    // for each world axis.    Some mixed solutions can be degenerate, whereupon you
    // you must say which one you want.  Use functions <src>setWorldMixRanges</src>
    // and <src>worldMixMin, worldMixMax</src> to set these ranges,
    // If you don't know, use the defaults (function <src>setDefaultWorldMixRanges</src>.
    // Removed axes are handled (for example, a removed pixel
    // axis with remaining corresponding world axis will
    // correctly be converted to world using the replacement
    // value).
    // Returns True if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message. The output vectors
    // are resized.
    //
    // If you actually request a pure pixel to world or world to pixel
    // via <src>toMix</src>, then the functions <src>toWorld</src> or <src>toPixel</src>
    // will be invoked directly (see above) and the extra conversion layer
    // invoked through function <src>setReferenceConversion</src> will be active.  
    // However, if you request a true mixed pixel/world conversion,
    // the extra conversion layer is not activated (because of the nature of mixed
    // conversions).  This situation may change in the future
    // with a partial implementation added.
    virtual Bool toMix(Vector<Double>& worldOut,
                       Vector<Double>& pixelOut,
                       const Vector<Double>& worldIn,
                       const Vector<Double>& pixelIn,
                       const Vector<Bool>& worldAxes,
                       const Vector<Bool>& pixelAxes,
                       const Vector<Double>& worldMin,
                       const Vector<Double>& worldMax) const; 

    // Compute and retrieve the world min and max ranges, for use in function <src>toMix</src>, 
    // for  a lattice of the given shape (for this coordinate).   Using these
    // ranges in <src>toMix</src> should speed it up and help avoid ambiguity.
    // If the shape is negative, that indicates that the shape is unknown
    // for that axis.  The default range is used for that axis.  This situation
    // arises in a CoordinateSystem for which a pixel, but not a world axis
    // has been removed.
    // The output vectors are resized.  Returns False if fails (and
    // then <src>setDefaultWorldMixRanges</src> generates the ranges)
    // with a reason in <src>errorMessage()</src>.
    // The <src>setDefaultWorldMixRanges</src> function
    // just gives you [-90->90], [-180,180] (in appropriate units) 
    // <group>
    virtual Bool setWorldMixRanges (const IPosition& shape);
    virtual void setDefaultWorldMixRanges ();
    // </group>

    // Non-virtual function.  When <src>which</src> is T, use the 
    // world value as the center for the mix world range.
    void setWorldMixRanges (const Vector<Bool>& which,
                            const Vector<Double>& world);

    // A convenient way to turn the world vector into an MDirection or MVDirection 
    // for further processing in the Measures system.  
    //
    // We could improve the performance of this if it would be useful, however I
    // expect that normally you would just call this once to get a template
    // MDirection, and then call the vector versions. 
    // <group>
    Bool toWorld(MDirection &world, const Vector<Double> &pixel) const;
    Bool toPixel(Vector<Double> &pixel, const MDirection &world) const;
    Bool toWorld(MVDirection &world, const Vector<Double> &pixel) const;
    Bool toPixel(Vector<Double> &pixel, const MVDirection &world) const;
     //</group>

    // Batch up a lot of transformations. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Returns False if any conversion
    // failed  and  <src>errorMessage()</src> will hold a message.
    // The <src>failures</src> array is the length of the number of conversions
    // (True for failure, False for success)
    // <group>
    virtual Bool toWorldMany(Matrix<Double> &world,
                             const Matrix<Double> &pixel,
                             Vector<Bool> &failures) const;
    virtual Bool toPixelMany(Matrix<Double> &pixel,
                             const Matrix<Double> &world,
                             Vector<Bool> &failures) const;
    // </group>
  

    // Make absolute world coordinates relative and vice-versa (relative to
    // the reference value).  Note that these functions are independent 
    // of the MDirection::Types  (set either at construction or by function
    // <src>setReferenceConversion</src>).  The vectors must be
    // of length <src>nWorldAxes</src> or memory access errors will occur
    //<group>
    virtual void makeWorldRelative (Vector<Double>& world) const;
    virtual void makeWorldRelative (MDirection& world) const;
    virtual void makeWorldAbsolute (Vector<Double>& world) const;
    virtual void makeWorldAbsolute (MDirection& world) const;
    //</group>

    // Make absolute coordinates relative and vice versa with respect
    // to the given reference value.  Add the other functions in this grouping
    // as needed.
    //<group>
    virtual void makeWorldAbsoluteRef (Vector<Double>& world,
                                       const Vector<Double>& refVal) const;
    //</group>

    // Recover the requested attribute.
    // <group>
    MDirection::Types directionType(Bool showConversion=False) const;    
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

    // Change the world axis units.  Adjust the increment and
    // reference value by the ratio of the old and new units. 
    // The units must be compatible with
    // angle. The units are initially "rad" (radians).
    virtual Bool setWorldAxisUnits(const Vector<String> &units);

    // Return canonical axis names for the given MDirection type,
    // giving FITS names if desired.
    // BEG think this should be in the MDirection class, but WNB
    // disagrees. Leave it here for now.
    static Vector<String> axisNames(MDirection::Types type, 
				    Bool FITSName = False);

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


    // Format a DirectionCoordinate coordinate world value nicely through the
    // common format interface.  See <linkto class=Coordinate>Coordinate</linkto>
    // for basics.
    //
    // Formatting types that are allowed are SCIENTIFIC, FIXED, MIXED, and TIME
    // If you ask for format type Coordinate::DEFAULT then the
    // selected format depends upon what the value of the enum 
    // MDirection::GlobalTypes is for this DirectionCoordinate.
    // For example, if it is GRADEC or GHADEC you would
    // get Coordinate::TIME style formatting (DD:MM:SS.SS), otherwise
    // you would get Coordinate::FIXED formatting by default.
    //
    // <src>axis</src> says which axis in this Coordinate we are formatting.  
    // We have to know this because we may format Longitude and Latitude differently.  
    // For Coordinate::TIME style formatting, precision
    // refers to the places after the decimal in the SS field.
    //
    // If you leave <src>units</src> empty, then it makes up a nice unit for you.
    //<group>
    virtual void getPrecision (Int& precision, 
                               Coordinate::formatType& format,
                               Bool showAsAbsolute, 
                               Int defPrecScientific,
                               Int defPrecFixed,
                               Int defPrecTime) const;
    virtual String format(String& units,
                          Coordinate::formatType format, 
                          Double worldValue, 
                          uInt axis, 
                          Bool isAbsolute,
                          Bool showAsAbsolute,
                          Int precision=-1);
    //</group>

    // Fix cylindrical coordinates to put the longitude in [-180,180] range.
    // If False returned, it failed an an error is in <src>errorMessage</src>
    // This fix is not done automatically internally because of the dependence
    // on the image shape.  It should be called for any foreign image
    // (such as FITS) that is imported
    Bool cylindricalFix (Int shapeLong, Int shapeLat);

    // Find the Coordinate for when we Fourier Transform ourselves.  This pointer
    // must be deleted by the caller. Axes specifies which axes of the Coordinate
    // you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the Coordinate.   Currently the
    // output reference pixel is always shape/2. If the pointer returned is 0, 
    // it failed with a message in <src>errorMessage</src>
    virtual Coordinate* makeFourierCoordinate (const Vector<Bool>& axes,
                                               const Vector<Int>& shape) const;

    // Save the DirectionCoordinate into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // Recover the DirectionCoordinate from a record.
    // A null pointer means that the restoration did not succeed.
    static DirectionCoordinate *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of the DirectionCoordinate using new. The caller 
    // is responsible for calling delete.
    virtual Coordinate *clone() const;

    // Fish out the ref and non-native poles (refLong, refLat, longPole, latPole)
    // Not for general use.  Units are degrees.
    Vector<Double> longLatPoles() const;

private:
    // Direction type
    MDirection::Types type_p, conversionType_p;

    // Projection parameters
    Projection projection_p;

    // WCS structure.  This is mutable because the wcs functions
    // that do toPixel and toWorld (which have const signature)
    // require a non const wcs structure.  so either all of these
    // virtual functions lose their const or we use mutable...
    mutable ::wcsprm wcs_p;

    // WCS computes in degrees - use this to convert back and forth between
    // current DirectionCoordinate units and degrees or radians
    Vector<Double> to_degrees_p;           // From current units to degrees
    Vector<Double> to_radians_p;           // From current units to radians

    // Axis names.
    Vector<String> names_p;

    // Current units.
    Vector<String> units_p;

    // Rotation matrix used to handle relative coordinates
    RotMatrix rot_p;

    // Conversion machines.  
    // "To"   handles type_p -> conversionType_p
    // "From" handles conversionType_p -> type_p;
    mutable MDirection::Convert* pConversionMachineTo_p;
    mutable MDirection::Convert* pConversionMachineFrom_p;

    // Interconvert between the current units and wcs units (degrees)
    // <group>
    void toCurrent(Vector<Double>& degrees) const;
    void fromCurrent(Vector<Double>& current) const;
    // </group>

    // Check formatting types.
    void checkFormat(Coordinate::formatType& format,
                     Bool absolute) const;

    // Format a latitude.
    String formatLatitude (String& units, MVAngle& mVA,
			   Bool absolute, 
			   Coordinate::formatType form,
			   Int prec) const;
    // Format a longitude.
    String formatLongitude (String& units, MVAngle& mVA,
                            MDirection::GlobalTypes gtype,
                            Bool absolute, 
                            Coordinate::formatType form,
                            Int prec) const;

    // Mixed pixel/world coordinate conversion.  Vector in must
    // be length nWorldAxes (2).  Specify whether longitude
    // (in(0)) or latitude (in(1)) is the world coordinate . It is
    // assumed that the other value is the pixel coordinate.
    Bool toMix2(Vector<Double>& out, const Vector<Double>& in,
                const Vector<Double>& minWorld, const Vector<Double>& maxWorld,
                Bool longIsWorld) const;

    // Initialize unit conversion vectors and units
    void initializeFactors ();

    // Helper functions interfacing to WCS.
    // <group>
    void makeDirectionCoordinate(MDirection::Types directionType,
                                 const Projection& proj, Double refLong, Double refLat,
                                 Double incLong, Double incLat,
                                 const Matrix<Double> &xform,
                                 Double refX, Double refY, 
                                 Double longPole, Double latPole);
//
    void makeWCS(::wcsprm& wcs,  const Matrix<Double>& xform,
                 const Projection& proj, MDirection::Types directionType,
                 Double refPixLong, Double refPixLat,
                 Double refLong, Double refLat,
                 Double incLong, Double incLat,
                 Double longPole, Double latPole);
    // </group>

   Double putLongInPiRange (Double lon, const String& unit) const;

   // Set up conversion machine
   void makeConversionMachines();

   // Convert from type_p -> conversionType_p
   // <group>
   virtual void convertTo (Vector<Double>& world) const;
   virtual void convertFrom (Vector<Double>& world) const;
   // </group>

   // Copy private data
   void copy (const DirectionCoordinate& other);

   // Set up the offset coordinate rotation matrix.  Units
   // of long and lat are current world units
   // <group>
   void setRotationMatrix ();
   void setRotationMatrix (RotMatrix& rot, Double lon, Double lat) const;
   // </group>

   // Return unit conversion vector for converting to current units
   const Vector<Double> toCurrentFactors () const;
};

} //# NAMESPACE CASA - END


#endif

