//# DirectionCoordinate.h: Interconvert pixel positions and directions (e.g. RA/DEC)
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

#if !defined(AIPS_DIRECTION_COORDINATE_H)
#define AIPS_DIRECTION_COORDINATE_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/LinearXform.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Arrays/Vector.h>
#include <wcslib/wcs.h>

class celprm;
class prjprm;
class MVDirection;
class MVAngle;
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
// This class implements pixel to world coordinate conversions. It is important
// to understand that this class implements only the geometric conversions
// (e.g. SIN projection). Astronomical conversions (RA/DEC <--> l,b)
// are the responsibility of the <linkto module=Measures>Measures</linkto> module.
// Of course the <linkto class=MDirection>MDirection</linkto> object you can
// obtain from this class would be the prime input for that conversion.
//
// The actual computations are carried out with the WCS library.
// </synopsis>
//
//
// <note role=caution>
// All pixels coordinates are zero relative.
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
    DirectionCoordinate(MDirection::Types directionType,
 			const Projection &projection,
			Double refLong, Double refLat,
			Double incLong, Double incLat,
			const Matrix<Double> &xform,
 			Double refX, Double refY);

    // Create DirectionCoordinate with Quantum-based interface. 
    // Parameters are the same as above.
    // Regardless of the units of the quanta, the initial units
    // of the DirectionCoordinate will be converted radians.
    // You can change it to degrees or something else with the 
    // setWorldAxisUnits method later if you want.
    DirectionCoordinate(MDirection::Types directionType,
                        const Projection &projection,
                        const Quantum<Double>& refLong, 
                        const Quantum<Double>& refLat,
                        const Quantum<Double>& incLong, 
                        const Quantum<Double>& incLat,
                        const Matrix<Double> &xform,
                        Double refX, Double refY);

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
    // <src>pixelOut</src>
    // <src>worldMin</src> and <src>worldMax</src> specify the range of the world
    // coordinate (in the world axis units of that world axis
    // in the CoordinateSystem) being solved for in a mixed calculation
    // for each world axis. They are only actually needed for DirectionCoordinates
    // and for all other Coordinates the relevant elements
    // can be undefined.   If you don't know, use -180 to 180
    // degrees for longitude, and -90 to 90 for latitude. 
    // Some mixed solutions can be degenerate, whereupon you
    // you must say which one you want.
    // Removed axes are handled (for example, a removed pixel
    // axis with remaining corresponding world axis will
    // correctly be converted to world using the replacement
    // value).
    // Returns True if the conversion succeeds, otherwise it returns False and
    // <src>errorMessage()</src> contains an error message. The output vectors
    // are resized.
    virtual Bool toMix(Vector<Double>& worldOut,
                       Vector<Double>& pixelOut,
                       const Vector<Double>& worldIn,
                       const Vector<Double>& pixelIn,
                       const Vector<Bool>& worldAxes,
                       const Vector<Bool>& pixelAxes,
                       const Vector<Double>& worldMin,
                       const Vector<Double>& worldMax) const; 

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

    // Recover the requested attribute.
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
    // common format interface.  Formatting types that are allowed are
    // Coordinate::SCIENTIFIC, Coordinate::FIXED, Coordinate::TIME
    // If you ask for format type Coordinate::DEFAULT then the
    // selected format depends upon what the value of the enum 
    // MDirection::GlobalTypes is for this DirectionCoordinate.
    // For example, if it is GRADEC or GHADEC you would
    // get Coordinate::TIME style formatting (DD:MM:SS.SS), otherwise
    // you would get Coordinate::FIXED formatting by default.
    //
    // axis says which axis in this Coordinate we are formatting.  
    // We have to know this because we may format, say, RA and DEC differently.  
    // For Coordinate::TIME style formatting, precision
    // refers to the places after the decimal in the SS field.
    //
    // The world value should have the units currently
    // set in the state of the DirectionCoordinate.  The units of
    // the formatted number are returned in <src>units</src>.
    // You can also use the Quantum interface (see base class Coordinate). 
    // The units can then be anything consistent with DirectionCoordinate units.
    //
    // native only applies to FIXED and SCIENTIFIC formatting.  If true
    // you get radians, else degrees.
    //<group>
    virtual void getPrecision (Int& precision, 
                               Coordinate::formatType& format,
                               Bool absolute, 
                               Int defPrecScientific,
                               Int defPrecFixed,
                               Int defPrecTime) const;
    virtual String format(String& units,
                          Coordinate::formatType format, 
                          Double worldValue, 
                          uInt axis, 
                          Bool absolute,
                          Int precision=-1,
                          Bool native=False) const;
    //</group>

    // Find the Coordinate for when we Fourier Transform ourselves.  This pointer
    // must be deleted by the caller. Axes specifies which axes of the Coordinate
    // you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the Coordinate.   Currently the
    // output reference pixel is always shape/2.
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

private:
    // Direction type
    MDirection::Types type_p;

    // Projection parameters
    Projection projection_p;

    // WCS structures
    // <group>
    celprm* celprm_p;
    prjprm* prjprm_p;
    wcsprm* wcs_p;
    char c_ctype_p[2][9];
    double c_crval_p[2];
    // </group>

    // Performs the linear part of the transformation.
    LinearXform linear_p;

    // WCS computes in degrees - use this to convert back and forth between
    // degrees and the currently requested units.
    Double to_degrees_p[2];
    Double to_radians_p[2];

    // Axis names.
    Vector<String> names_p;

    // Current units.
    Vector<String> units_p;

    // Some kinds of DirectionCoordinate cannot be used with
    // the toMix function (e.g. one made with MDirection::SUN)
    // We find this out at construction time (things are set
    // up early and cached) so we cart about the flag and
    // error message until such time as it is needed.  Yuck.
    Bool canDoToMix_p;
    String canDoToMixErrorMsg_p;

    // Interconvert between degrees and the current angular unit.
    // <group>
    void toDegrees(Vector<Double> &other) const;
    void toOther(Vector<Double> &degrees) const;
    // </group>

    // Check formatting types.
    void checkFormat(Coordinate::formatType& format,
                     Bool absolute) const;

    // Format a latitude.
    String DirectionCoordinate::formatLatitude (String& units, MVAngle& mVA,
                                                Bool absolute, Bool native,
                                                Coordinate::formatType form,
                                                Int prec) const;
    // Format a longitude.
    String formatLongitude (String& units, MVAngle& mVA,
                            MDirection::GlobalTypes gtype,
                            Bool absolute, Bool native,
                            Coordinate::formatType form,
                            Int prec) const;

    // Mixed pixel/world coordinate conversion.  Vector in must
    // be length nWorldAxes (2).  Specify whether longitude
    // (in(0)) or latitude (in(1)) is the world coordinate . It is
    // assumed that the other value is the pixel coordinate.
    Bool toMix2(Vector<Double>& out, const Vector<Double>& in,
                const Vector<Double>& minWorld, const Vector<Double>& maxWorld,
                Bool longIsWorld) const;

    // Helper functions interfacing to WCS.
    // <group>
    void makeDirectionCoordinate(Double refLong, Double refLat,
                                 Double incLong, Double incLat,
                                 const Matrix<Double> &xform,
                                 Double refX, Double refY);

    void make_celprm_and_prjprm(Bool& canDoToMix, String& canDoToMixErrorMsg,
                                celprm* &pCelPrm, prjprm* &pPrjPrm, wcsprm* &pWcs,
                                char c_ctype[2][9], double c_crval[2],
                                const Projection& proj,
                                MDirection::Types type,
                                Double refLong, Double refLat,
                                Double longPole, Double latPole) const;

    void copy_celprm_and_prjprm(celprm* &pToCel, prjprm* &pToPrj,
                                   wcsprm* &pToWcs,
                                   char toctype[2][9], double tocrval[2],
                                   const celprm *pFromCel, const prjprm *pFromPrj,
                                   const wcsprm *pFromWcs,
                                   const char fromctype[2][9], const double fromcrval[2]) const;
    // </group>

};

#endif

