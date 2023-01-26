//# CoordinateSystem.h: Interconvert pixel and image coordinates.
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


#ifndef COORDINATES_COORDINATESYSTEM_H
#define COORDINATES_COORDINATESYSTEM_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/coordinates/Coordinates/ObsInfo.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class DirectionCoordinate;
class LinearCoordinate;
class SpectralCoordinate;
class StokesCoordinate;
class QualityCoordinate;
class TabularCoordinate;
class IPosition;
class LogIO;


// <summary>
// Interconvert pixel and world coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/12/24" tests="tCoordinateSystem">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
// </prerequisite>

// <synopsis>
// CoordinateSystem is the normal interface to coordinate systems,
// typically attached to an 
// <linkto class=ImageInterface>ImageInterface</linkto>, however the
// coordinate system can be manipulated on its own. CoordinateSystem
// is in turn composed from various classes derived from the base class
// <linkto class=Coordinate>Coordinate</linkto>.
// <p>
// The fundamental operations available to the user of a 
// CoordinateSystem are:
// <ol>
//   <li> Transform a world (physical) coordinate to a pixel coordinate 
//        or vice versa via the methods toWorld and toPixel.
//   <li> Compose a CoordinateSystem from one or more independent groups,
//        typically the sky-plane transformation will be one group, and the
//        spectral axis will be another group. Each group consists of a linear
//        transformation (in FITS terms, apply <src>CRPIX, PC, CDELT</src>)
//        to turn the pixel coordinates into relative world coordinates, 
//        followed by a (possibly) nonlinear projection to world coordinates 
//        (i.e. apply <src>CTYPE and CRVAL</src>), typically a sky projection
//        or a frequency to velocity conversion. Note that an arbitrary rotation
//        or linear transformation can be applied by changing the
//        matrix.
//   <li> Transpose the world and/or pixel axes.
//   <li> One or more pixel or world axes may be removed. You are encouraged to
//        leave all the world axes if you remove a pixel axis.
//        Removing a world axis also removes the corresponding pixel axis.
//   <li> Calculate the CoordinateSystem that results from a subimage
//        operation.
// </ol>
//
// Note that all the knowledge to do with removing and transposing axes is
// maintained by the CoordinateSystem.  The individual Coordinates, of which it
// is made, know nothing about this.
// <p>
// Although the CoordinateSystem exists in the absence of an image, the usual
// place you will find one is attached to an object derived from ImageInterface 
// such as PagedImage. When you do so, the physical (or pixel) axes in the image
// map one to one with the pixel axes contained in the CoordinateSystem.
// It cannot be any other way as when you create a PagedImage, it is checked
// that there are equal numbers of image and CoordinateSystem pixel axes.
// It is up to the creator of the PagedImage to make sure that they are
// in the correct order.
// <p>
// However, the CoordinateSystem may have more world axes than pixel axes
// because it is possible to remove a pixel axis but not its associated
// world axis (for example for a moment image).   Now, if you use
// the CoordinateSystem functions 
// referencePixel and referenceValue, you will find the vector of reference
// values will have more values than the vector of reference pixels,
// if a pixel axis has been removed but not the world axis.  You 
// must use the ancilliary functions provided
// to find out what is where.   
// <p>
// Let's consider an example where a CoordinateSystem consisted of
// a DirectionCoordinate and a SpectralCoordinate.  Let us say that
// the first two pixel axes of the image associate (roughly of course
// because lines of constant RA and DEC are not parallel with
// the pixel coordinates) with the DirectionCoordinate (RA and DEC say) 
// and the third pixel axis is the SpectralCoordinate.
// Now imagine we collapse the image along the second pixel axis (roughly,
// the DEC axis).  For the output image, we remove the second pixel axis
// from the CoordinateSystem, but leave the world axis intact.  This enables
// us to still be able to make coordinate conversions for the first (roughly RA)
// pixel axis.  Thus, CoordinateSystem::referenceValue would return a Vector of
// length 3 (for RA, DEC and spectral), but CoordinateSystem::referencePixel
// would return a vector length 2 (for RA and spectral).  
// <p>
// Now this CoordinateSystem has two Coordinates, a DirectionCoordinate and
// a SpectralCoordinate, and let us state that that is the order in which
// they exist in the CoordinateSystem (you can change them about if you wish);
// they are coordinates number 0 and 1. The DirectionCoordinate has two axes
// (RA and DEC) and the SpectralCoordinate has one axis. Only the
// CoordinateSystem knows about removed axes, the DirectionCoordinate
// itself is ignorant that it has been bisected. If you want to find
// out what axis in the Coordinate system is where, you can use
// the functions findPixelAxis or findWorldAxis.
//
// If we asked the former to find pixel axis 0, it would tell us that the
// Coordinate number was 0 (the DirectionCoordinate) and that the axis in
// that coordinate was 0 (the first axis in a DirectionCoordinate
// is always longitude, the second always latitude).  If we asked it to find
// pixel axis 1, it would tell us that the coordinate number was 1
// (the SpectralCoordinate) and that the axis in that coordinate was 0
// (there is only one axis in a SpectralCoordinate). If we asked for
// pixelAxis 2 that would generate an error because our squashed image
// only has 2 pixel axes. 
//
// Now, if we asked findWorldAxis similar questions,
// it would tell us that worldAxis 0 in the CoordinateSystem can be found in
// coordinate 0 (the DirectionCoordinate) in axis 0 of that DirectionCoordinate.
// Similarly, worldAxis 1 in the CoordinateSystem (which has not been removed)
// is in coordinate 0 (the DirectionCoordinate) in axis 1 of that 
// Finally, worldAxis 2 in the CoordinateSystem is in coordinate 1 
// (the SpectralCoordinate) in axis 0 of that SpectralCoordinate.
// <p>
// Other handy functions are pixelAxes and worldAxes.
// These list the pixel and world axes in
// the CoordinateSystem for the specified coordinate. Thus, if we asked
// pixelAxes to find the pixel axes for coordinate 0 (the  DirectionCoordinate)
// in the CoordinateSystem it would return a vector [0, -1]  indicating
// the second axis of  the DirectionCoordinate has been removed.  However, 
// the worldAxes function would return [0,1] as no world axis has been removed.
// Similarly, if operated on coordinate 1 (the SpectralCoordinate), pixelAxes
// would return [1] and worldAxes would return [2].
//
// Because you can transpose the CoordinateSystem about, you should NEVER ASSUME
// ANYTHING except that the pixel axes of the CoordinateSystem map to the pixel
// axes of the image when you first construct the image.
//
// <p>
// SpectralCoordinate and DirectionCoordinate both have a (non-virtual) function
// called <src>setReferenceConversion</src>.  This enables an extra conversion
// layer so that conversion between pixel and world can go to a reference frame
// other than the construction reference.    When you use the function
// <src>convert</src>, these layers are active, but ONLY if the 
// requested conversion is purely between pixel and world. For 
// a SpectralCoordinate this must always be true (only has one axis)
// but for the DirectionCoordinate  you might request a mixed
// pixel/world conversion. In this case, the extra conversion layer
// is ill-defined and not active (for the DirectionCoordinate part of it).
// </synopsis>

// <note role=caution>
// All pixels coordinates are zero relative.
// </note>

// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>
// and tCoordinateSystem.cc
// </example>

// <motivation>
// Coordinate systems for images.
// </motivation>
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="1997/01/13">
//   <li> Undelete individual removed axes.
//   <li> Non-integral pixel shifts/decimations in subimage operations?
//   <li> Copy-on-write for efficiency?
//   <li> Check if the classes are thread safe in general
// </todo>
//


class CoordinateSystem : public Coordinate
{
public:
    // Default constructor.  This is an empty CoordinateSystem.
    CoordinateSystem();

    // Copying constructor (copy semantics)
    CoordinateSystem(const CoordinateSystem &other);

    // Assignment (copy semantics).
    CoordinateSystem &operator=(const CoordinateSystem &other);

    // Destructor
    virtual ~CoordinateSystem();

    // Add another Coordinate to this CoordinateSystem. This addition is done
    // by copying, so that if coord changes the change is NOT
    // reflected in the CoordinateSystem.
    void addCoordinate(const Coordinate &coord);

    // Transpose the CoordinateSystem so that world axis 0 is
    // newWorldOrder(0) and so on for all the other axes.
    // newPixelOrder works similarly. Normally you will give the
    // same transformation vector for both the world and pixel transformations,
    // however this is not required.
    void transpose(const Vector<int32_t> &newWorldOrder,
                   const Vector<int32_t> &newPixelOrder);

    // Find the world and pixel axis mappings to the supplied CoordinateSystem
    // from the current coordinate system. <src>false</src> is 
    // returned if either the supplied or current coordinate system, 
    // has no world axes (and a message recoverable with function
    // errorMessage indicating why).  Otherwise <src>true</src> is returned.
    // worldAxisMap(i) is the location of world axis <src>i</src> (from the
    // supplied CoordinateSystem, cSys, in the current CoordinateSystem.
    // worldAxisTranspose(i) is the location of world axis 
    // <src>i</src> (from the current CoordinateSystem) in the supplied 
    // CoordinateSystem, cSys.  The output vectors
    // are resized appropriately by this function.  A value of  -1 
    // in either vector means that the axis could not be found in the other
    // CoordinateSystem.  The vector <src>refChange</src> says
    // if the types are the same, is there a reference type change
    // (e.g. TOPO versus LSR for the SpectralCoordinate, 
    // or J2000 versus GALACTIC for DirectionCoordinate). Thus
    // if refChange(i) is true, it means world axis i in the
    // current CoordinateSystem was matched, but has a different
    // reference type to that of the supplied CoordinateSystem.
    // <group>
    bool worldMap (Vector<int32_t>& worldAxisMap,
		   Vector<int32_t>& worldAxisTranspose,
		   Vector<bool>& refChange,
		   const CoordinateSystem& cSys) const;
    bool pixelMap (Vector<int32_t>& pixelAxisMap,
		   Vector<int32_t>& pixelAxisTranspose,
		   const CoordinateSystem& cSys) const;
    // </group>

    // Remove a world or pixel axis. When its value is required for forward or
    // backwards transformations, use <src>replacement</src>
    // <br>
    // When a world axis is removed, the corresponding pixel axis is removed
    // too, because it makes no sense having a pixel axis without world
    // coordinates.
    // <br>
    // Removing a pixel axis without removing the corresponding world axis
    // is, however, possible and meaningful. It can be used when e.g. a
    // frequency plane is taken from a cube. The plane has 2 pixel axes, but
    // the 3rd world axis can still describe the frequency coordinate.
    // See also the functions in  <linkto class=CoordinateUtil>CoordinateUtil</linkto>
    // for removing lists of pixel/world axes (tricky because they shift down)
    //
    // false is returned (an error in <src>errorMessage()</src> will be set)
    // if the axis is illegal, else returns true.
    // <group>
    bool removeWorldAxis(uint32_t axis, double replacement);
    bool removePixelAxis(uint32_t axis, double replacement);
    // </group>

    // Return a CoordinateSystem appropriate for a shift of origin
    // (the shift is subtracted from the reference pixel)
    // and change of increment (the increments are multipled
    // by the factor). Both vectors should be of length nPixelAxes(). 
    //
    // The newShape vector is only needed for the StokesCoordinate,
    // if any.  If this vector is of length zero, the new StokesCoordinate
    // is formed from all of the available input Stokes after application
    // of the shift and increment factor.    Otherwise,
    // the new Stokes axis length is equal to that specified after
    // appliction of the shift and increment and excess values 
    // discarded.    In addition, for any StokesCoordinate, the
    // shift and factor must be integer.  So <src>int32_t(value+0.5)</src>
    // is taken before they are used.
    // <group>
    CoordinateSystem subImage(const Vector<float> &originShift,
			      const Vector<float> &incrFac,
                              const Vector<int32_t>& newShape) const;
    void subImageInSitu (const Vector<float> &originShift,
                         const Vector<float> &incrFac,
                         const Vector<int32_t>& newShape);
    // </group>

    // Untranspose and undelete all axes. Does not undo the effects of
    // subimaging.
    void restoreOriginal();

    // Returns the number of Coordinates that this CoordinateSystem contains.
    // The order might be unrelated to the axis order through the results of
    // transposing and removing axes.
    uint32_t nCoordinates() const;

    // For a given Coordinate say where its world and pixel axes are in
    // this CoordinateSystem. The position in the returned Vector is its
    // axis number in the Coordinate, and its value is the axis
    // number in the CoordinateSystem. If the value is less than zero the axis
    // has been removed from this CoordinateSystem.
    //  <group>
    Vector<int32_t> worldAxes(uint32_t whichCoord) const;
    Vector<int32_t> pixelAxes(uint32_t whichCoord) const;
    // </group> 

    // Return the type of the given Coordinate.
    Coordinate::Type type(uint32_t whichCoordinate) const;

    // Returns the type of the given Coordinate as a string.
    String showType(uint32_t whichCoordinate) const;

    // Return the given Coordinate as a reference to the base
    // class object.
    const Coordinate& coordinate(uint32_t which) const;

    // Return the given Coordinate.
    // Throws an exception if retrieved as the wrong type.
    // The versions which take no parameters will return the
    // first (or in most cases only) coordinate of the requested type.
    // If no such coordinate exists, an exception is thrown.
    // <group>
    const LinearCoordinate    &linearCoordinate(uint32_t which) const;
    const DirectionCoordinate &directionCoordinate() const;
    const DirectionCoordinate &directionCoordinate(uint32_t which) const;

    const SpectralCoordinate &spectralCoordinate(uint32_t which) const;
    const SpectralCoordinate &spectralCoordinate() const;
    const StokesCoordinate  &stokesCoordinate() const;

    const StokesCoordinate  &stokesCoordinate(uint32_t which) const;
    const QualityCoordinate &qualityCoordinate(uint32_t which) const;
    const TabularCoordinate &tabularCoordinate(uint32_t which) const;
    // </group>

    // Replace one Coordinate with another. The mapping of the coordinate axes
    // to the CoordinateSystem axes is unchanged, therefore the number of world
    // and pixel axes must not be changed. You can, somewhat dangerously,
    // change the type of the coordinate however. For example, replace a 
    // SpectralCoordinate with a 1-D Linearcoordinate.  It is dangerous because
    // the world replacement values (see removeWorldAxis) have to be scaled.
    // The algorithm tries to find a scale factor between the old and new
    // units and applies it to the replacement values.  If it can't find
    // a scale factor (non-conformant units) then the reference value is
    // used for any world replacement values.  If the latter occurs,
    // it returns false, else true is returned.
    bool replaceCoordinate(const Coordinate &newCoordinate, uint32_t whichCoordinate);

    // Find the Coordinate number that corresponds to the given type.
    // Since there might be more than one Coordinate of a given type you
    // can call this multiple times setting <src>afterCoord</src> to
    // the last value found. Returns -1 if a Coordinate of the desired
    // type is not found.
    int32_t findCoordinate(Coordinate::Type type, int32_t afterCoord = -1) const;

    // Given an axis number (pixel or world) in the CoordinateSystem,
    // find the corresponding coordinate number and axis in that Coordinate. 
    // The returned values are set to -1 if the axis does not exist.
    // <group>
    void findWorldAxis(int32_t &coordinate, int32_t &axisInCoordinate, 
		       uint32_t axisInCoordinateSystem) const;
    void findPixelAxis(int32_t &coordinate, int32_t &axisInCoordinate, 
		       uint32_t axisInCoordinateSystem) const;
    // </group>

    // Find the world axis for the given pixel axis in a CoordinateSystem.
    // Returns -1 if the world axis is unavailable (e.g. if it has been
    // removed).  
    int32_t pixelAxisToWorldAxis(uint32_t pixelAxis) const;

    // Find the pixel axis for the given world axis in a CoordinateSystem.
    // Returns -1 if the pixel axis is unavailable (e.g. if it has been
    // removed). 
    int32_t worldAxisToPixelAxis(uint32_t worldAxis) const;

    // Return the name of the record field in which the coordinate is stored.
    String coordRecordName(uint32_t which) const;

    // Returns <src>Coordinate::COORDSYS</src>
    virtual Coordinate::Type type() const;

    // Always returns "System"
    virtual String showType() const;

    // Sums the number of axes in the Coordinates that the CoordinateSystem
    // contains, allowing for removed axes.
    // <group>
    virtual uint32_t nPixelAxes() const;
    virtual uint32_t nWorldAxes() const;
    // </group>


    // Convert a pixel position to a world position or vice versa. Returns true
    // if the conversion succeeds, otherwise it returns <src>false</src> and
    // <src>errorMessage()</src> contains an error message. 
    // The input vector must be of length <src>nPixelAxes</src> or
    // <src>nWorldAxes</src>.  The output vector  is resized appropriately.
    // if <src>useConversionFrame</src>, if the coordinate has a conversion layer frame
    // (such as can be present in spectral and direction coordinates), it
    // is used. Else, the native frame is used for the conversion.
    // <group>
    virtual bool toWorld(Vector<double> &world, 
			 const Vector<double> &pixel, bool useConversionFrame=true) const;
    // This one throws an exception rather than returning false. After all, that's
    // what exceptions are for.
    virtual Vector<double> toWorld(const Vector<double> &pixel) const;
    virtual bool toPixel(Vector<double> &pixel, 
			 const Vector<double> &world) const;
    // This one throws an exception rather than returning false.
    virtual Vector<double> toPixel(const Vector<double> &world) const;
    // </group>

    // convert a pixel "length" to a world "length"
    virtual Quantity toWorldLength(
    	const double nPixels,
    	const uint32_t pixelAxis
    ) const;

    // This is provided as a convenience since it is a very commonly desired
    // operation through CoordinateSystem.  The output vector is resized.   
    bool toWorld(Vector<double> &world, const IPosition &pixel) const;
    Vector<double> toWorld(const IPosition& pixel) const;

    // Batch up a lot of transformations. The first (most rapidly varying) axis
    // of the matrices contain the coordinates. Returns false if any conversion
    // failed  and  <src>errorMessage()</src> will hold a message.
    // The <src>failures</src> array (true for fail, false for success)
    // is the length of the number of conversions and
    // holds an error status for each conversion.  
    // <group>
    virtual bool toWorldMany(Matrix<double>& world,
                             const Matrix<double>& pixel,
                             Vector<bool>& failures) const;
    virtual bool toPixelMany(Matrix<double>& pixel,
                             const Matrix<double>& world,
                             Vector<bool>& failures) const;
    // </group>


    // Mixed pixel/world coordinate conversion.
    // <src>worldIn</src> and <src>worldAxes</src> are of length n<src>worldAxes</src>.
    // <src>pixelIn</src> and <src>pixelAxes</src> are of length nPixelAxes.
    // <src>worldAxes(i)=true</src> specifies you have given a world
    // value in <src>worldIn(i)</src> to convert to pixel.
    // <src>pixelAxes(i)=true</src> specifies you have given a pixel 
    // value in <src>pixelIn(i)</src> to convert to world.
    // You cannot specify the same axis via <src>worldAxes</src>
    // and pixelAxes.
    // Values in <src>pixelIn</src> are converted to world and
    // put into <src>worldOut</src> in the appropriate world axis
    // location.  Values in <src>worldIn</src> are copied to
    // <src>worldOut</src>.   
    // Values in <src>worldIn</src> are converted to pixel and
    // put into <src>pixelOut</src> in the appropriate pixel axis
    // location.  Values in <src>pixelIn</src> are copied to
    // <src>pixelOut</src>.  Vectors
    // <src>worldMin</src> and <src>worldMax</src> specify the range of the world
    // coordinate (in the world axis units of that world axis
    // in the coordinate system) being solved for in a mixed calculation 
    // for each world axis. They are only actually used for DirectionCoordinates
    // and for all other coordinates the relevant elements are ignored.
    // Functions <src>setWorldMixRanges, worldMixMin, worldMixMax</src> can be
    // used to compute and recover the world ranges.  If you don't know 
    // the values, use  functions <src>setDefaultWorldMixRanges, worldMixMin, worldMixMax</src>.
    // Removed axes are handled (for example, a removed pixel
    // axis with remaining corresponding world axis will
    // correctly be converted to world using the replacement
    // value).
    // Returns true if the conversion succeeds, otherwise it returns <src>false</src> and
    // <src>errorMessage()</src> contains an error message. The output vectors
    // are resized.
    virtual bool toMix(Vector<double>& worldOut,
                       Vector<double>& pixelOut,
                       const Vector<double>& worldIn,
                       const Vector<double>& pixelIn,
                       const Vector<bool>& worldAxes,                
                       const Vector<bool>& pixelAxes,
                       const Vector<double>& worldMin,
                       const Vector<double>& worldMax) const; 

    // Compute and recover the world min and max ranges, for use in function <src>toMix</src>,
    // for  a lattice of the given shape (must be of length <src>nPixelAxes()</src>). 
    // Removed pixel axes (with remaining world axes are handled).  With
    // the retrieval functions, the output vectors are resized.  They return 
    // false if they fail (and then <src>setDefaultWorldMixRanges</src> generates the ranges)
    // with a reason in <src>errorMessage()</src>.
    // The <src>setDefaultWorldMixRanges</src> function
    // gives you  a useful default range if you don't know the shape.
    // The only Coordinate type for which these ranges are actually
    // used in <src>toMix</src> is DirectionCoordinate (because its coupled). For
    // the rest the functionality is provided but never used
    // by toMix.
    //<group>
    virtual bool setWorldMixRanges (const IPosition& shape);
    virtual void setDefaultWorldMixRanges ();
    virtual Vector<double> worldMixMin () const;
    virtual Vector<double> worldMixMax () const;
    //</group>

    // Make absolute coordinates relative and vice-versa (relative
    // to the reference pixel/value).  The vectors must be of length
    // <src>nPixelAxes()</src> or <src>nWorldAxes()</src>
    //<group>
    virtual void makePixelRelative (Vector<double>& pixel) const;
    virtual void makePixelAbsolute (Vector<double>& pixel) const;
    virtual void makeWorldRelative (Vector<double>& world) const;
    virtual void makeWorldAbsolute (Vector<double>& world) const;
    //</group>

    // Make absolute coordinates relative and vice versa with respect
    // to the given reference value.  Add the other functions in this grouping
    // as needed.    The vectors must be of length
    // <src>nPixelAxes()</src> or <src>nWorldAxes()</src>
    //<group>
    virtual void makeWorldAbsoluteRef (Vector<double>& world,
                                       const Vector<double>& refVal) const;
    //</group>

    // Batch up a lot of absolute/relative transformations. 
    // Parameters as above  for 
    // <src>toWorldMany</src> and <src>toPixelMany</src>
    // <group>
    virtual void makePixelRelativeMany (Matrix<double>& pixel) const;
    virtual void makePixelAbsoluteMany (Matrix<double>& pixel) const;
    virtual void makeWorldRelativeMany (Matrix<double>& world) const;
    virtual void makeWorldAbsoluteMany (Matrix<double>& world) const;
    // </group>


    // General coordinate conversion.  Only works if no axes
    // have been removed and no axis reordering has occurred.
    // That is pixel axes and world axes are the same.
    //
    // Specify the input coordinate values, input units,
    // whether value is absolute (or relative). For output
    // specify units and abs/rel.  Units may be 'pix' and velocity consistent
    // units (e.g. m/s).  Specify doppler types if velocities
    // involved.   The pixel offsets allow for the input
    // and output pixel coordinates to be something other than 0-rel.  
    // If your pixel coordinates are 1-rel input and output, set the 
    // offsets to -1 and 1
    //
    // The Matrix interface lets you do many conversions efficiently.
    // Use <src>Matrix(nAxes, nConversions) </src> and 
    // <src>Matrix.column()=coordinate</src> or
    // <src>Matrix(axis, iConversion)</src> to get the order right.  
    //
    // These functions invoke <src>toMix</src>
    // so make sure you call <src>setWorldMixRanges</src>
    // first to set up the world ranges. 
    // <group>
    bool convert (Vector<double>& coordOut, 
                  const Vector<double>& coordin,
                  const Vector<bool>& absIn,
                  const Vector<String>& unitsIn,
                  MDoppler::Types dopplerIn,
                  const Vector<bool>& absOut,
                  const Vector<String>& unitsOut,
                  MDoppler::Types dopplerOut,
                  double pixInOffset = 0.0,
                  double pixOutOffset = 0.0);
    bool convert (Matrix<double>& coordOut, 
                  const Matrix<double>& coordIn,
                  const Vector<bool>& absIn,
                  const Vector<String>& unitsIn,
                  MDoppler::Types dopplerIn,
                  const Vector<bool>& absOut,
                  const Vector<String>& unitsOut,
                  MDoppler::Types dopplerOut,
                  double pixInOffset = 0.0,
                  double pixOutOffset = 0.0);
    // </group>

    // Return the requested attribute.
    // <group>
    virtual Vector<String> worldAxisNames() const;
    virtual Vector<double> referencePixel() const;
    virtual Matrix<double> linearTransform() const;
    virtual Vector<double> increment() const;
    virtual Vector<double> referenceValue() const;
    // </group>

    // Set the requested attribute.  Note that these just
    // change the internal values, they do not cause any recomputation.
    // <group>
    virtual bool setWorldAxisNames(const Vector<String> &names);
    virtual bool setReferencePixel(const Vector<double> &refPix);
    virtual bool setLinearTransform(const Matrix<double> &xform);
    virtual bool setIncrement(const Vector<double> &inc);
    virtual bool setReferenceValue(const Vector<double> &refval);
    // </group>

    // Set/get the units. Adjust the increment and
    // reference value by the ratio of the old and new units. This implies that
    // the units must be known <linkto class=Unit>Unit</linkto> strings, and
    // that they must be compatible, e.g. they can't change from time to
    // length. If <src>throwException=true</src>, throw an exception rather than
    // returning false on failure.
    // <group>
    virtual bool setWorldAxisUnits(const Vector<String> &units);
    bool setWorldAxisUnits(const Vector<String> &units,
                           bool throwException);
    virtual Vector<String> worldAxisUnits() const;
    // </group>

    // Comparison function. Any private double data members are compared
    // with the specified fractional tolerance.  Don't compare on the specified 
    // pixel axes in the CoordinateSystem.  If the comparison returns
    // <src>false</src>, errorMessage() contains a message about why.
    // <group>
    virtual bool near(const Coordinate& other, double tol=1e-6) const;
    virtual bool near(const Coordinate& other, 
                      const Vector<int32_t>& excludePixelAxes,
                      double tol=1e-6) const;
    // </group>

    // This function compares this and the other coordinate system,
    // but ONLY for the non-removed pixel axes.   It is less strict
    // than near, which, for example, insists the number of coordinates
    // is the same in each CS
    bool nearPixel (const CoordinateSystem& other, double tol=1e-6) const;


    // Format a world value nicely through the
    // common format interface.  See <linkto class=Coordinate>Coordinate</linkto>
    // for basics.
    //
    // You specify a world value and its corresponding world axis in
    // the CoordinateSystem.   
    //
    // For the specified worldAxis, the coordinate
    // number in the CoordinateSystem is found and the actual derived Coordinate
    // class object for that number is created.  The arguments to the formatting 
    // function are then passed on to the formatter for that Coordinate. So
    // refer to the other derived Coordinate classes for specifics on the
    // formatting.
    virtual String format(
    	String& units,
    	Coordinate::formatType format,
    	double worldValue,
    	uint32_t worldAxis,
    	bool isAbsolute=true,
    	bool showAsAbsolute=true,
    	int32_t precision=-1, bool usePrecForMixed=false
    ) const;

    // Miscellaneous information related to an observation, for example the
    // observation date.
    // <group>
    ObsInfo obsInfo() const;
    void setObsInfo(const ObsInfo &obsinfo);
    // </group>

    // Find the CoordinateSystem (you can safely caste the pointer to a CoordinateSystem)
    // for when we Fourier Transform ourselves.  This pointer 
    // must be deleted by the caller. Axes specifies which pixel axes of the Coordinate
    // System you wish to transform.   Shape specifies the shape of the image
    // associated with all the axes of the CoordinateSystem.  Currently you have
    // no control over the reference pixel, it is always shape/2.
    virtual Coordinate* makeFourierCoordinate (const Vector<bool>& axes,
                                               const Vector<int32_t>& shape) const;


    // Save the CoordinateSystem into the supplied record using the supplied field name.
    // The field must not exist, otherwise <src>false</src> is returned.
    // If the CoordinateSystem is empty  <src>false</src> is also returned.
    // If <src>false</src> is returned, errorMessage() contains a message about why.   
    virtual bool save(RecordInterface &container,
		    const String &fieldName) const;

    // Restore the CoordinateSystem from a record.  The <src>fieldName</src>
    // can be empty, in which case the CoordinateSystem is restored 
    // directly from the Record, rather than a subrecord of it.
    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a CoordinateSystem.
    static CoordinateSystem *restore(const RecordInterface &container,
 				   const String &fieldName);

    // Make a copy of the CoordinateSystem using new. The caller is responsible for calling
    // delete.
    virtual Coordinate* clone() const;

    // Convert a CoordinateSystem to FITS, i.e. fill in ctype etc. In the record
    // the keywords are vectors, it is expected that the actual FITS code will
    // split them into scalars and upcase the names. Returns false if one of the
    // keywords is already taken.
    // 
    // If writeWCS is true, attempt to write the WCS convention (Greisen and
    // Calabretta "Representation of celestial coordinates in FITS"). 
    // Use <src>oneRelative=true</src> to convert zero-relative pixel coordinates to
    // one-relative FITS coordinates.
    //
    // prefix gives the prefix for the FITS keywords. E.g.,
    // if prefix="c" then crval, cdelt etc. 
    // if prefix="d" then drval, ddelt etc. 
    //# Much of the work in to/from fits should be moved to the individual
    //# classes.
    bool toFITSHeader(RecordInterface &header, 
		      IPosition &shape,
		      bool oneRelative, 
		      char prefix = 'c', bool writeWCS=true,
		      bool preferVelocity=true, 
		      bool opticalVelocity=true,
		      bool preferWavelength=false,
		      bool airWavelength=false) const;

    // Probably even if we return false we should set up the best linear
    // coordinate that we can.
    // Use oneRelative=true to convert one-relative FITS pixel coordinates to
    // zero-relative Casacore coordinates.
    // On output, <src>stokesFITSValue</src>
    // holds the FITS value of any unofficial Stokes (beam, optical depth,
    // spectral index) for the last unofficial value accessed (-1 if none).
    // The idea is that if the Stokes axis is of length one and holds an
    // unofficial value, you should drop the STokes axis and convert that
    // value to <src>ImageInfo::ImageTypes</src> with
    // <src>ImageInfo::imageTypeFromFITSValue</src>.
    // If on input, <src>stokesFITSValue</src> is positive, then a warning
    // is issued if any unofficial values are encountered.
    // Otherwise no warning is issued.
    //# cf comment in toFITS.
    static bool fromFITSHeader(int32_t& stokesFITSValue, 
                               CoordinateSystem &coordsys, 
                               RecordInterface& recHeader,
                               const Vector<String>& header,
                               const IPosition& shape,
                               uint32_t which=0);

// List all header information.  By default, the reference
// values and pixel increments are converted to a "nice" unit before 
// formatting (e.g. RA is  shown as HH:MM:SS.S).  
// For spectral axes, both frequency and velocity information is listed. You
// can specify what velocity definition you want with <src>velocityType</src>
// If you wish, you can specify two shapes; a lattice and tile shape
// (perhaps an image from which the CoordinateSystem came)
// If you give (both of) these, they are included in the listing.  If you pass
// in zero length <src>IPositions</src> then they are not included in
// the listing.   If <src>postlocally=true</src> the formatted summary lines 
// are written locally only to the sink, and then returned by the return value 
// vector.
   Vector<String> list(LogIO& os, MDoppler::Types doppler,
                       const IPosition& latticeShape,
                       const IPosition& tileShape, bool postLocally=false) const;

   // Does this coordinate system have a spectral axis?
   bool hasSpectralAxis() const;

   // What number is the spectral axis?
   // If doWorld=true, the world axis number is returned.
   // Otherwise, the pixel axis number is returned.
   // Returns -1 if the spectral axis (world c.q. pixel) does not exist.
   int32_t spectralAxisNumber(bool doWorld=false) const;

   // what number is the spectral coordinate?
    // Returns -1 if no spectral coordinate exists.
   int32_t spectralCoordinateNumber() const;


   // does this coordinate system have a polarizaion/stokes coordinate?
   bool hasPolarizationCoordinate() const;
   bool hasPolarizationAxis() const
     { return hasPolarizationCoordinate(); }

   // Given a stokes or polarization parameter, find the pixel location.
   // Note the client is responsible for any boundedness checks
   // (eg finite number of stokes in an image).
   int32_t stokesPixelNumber(const String& stokesString) const;

   // what is the number of the polarization/stokes coordinate?
   // Returns -1 if no stokes coordinate exists.
   int32_t polarizationCoordinateNumber() const;

   // What is the number of the polarization/stokes axis?
   // If doWorld=true, the world axis number is returned.
   // Otherwise, the pixel axis number is returned.
   // Returns -1 if the stokes axis (world c.q. pixel) does not exist.
   int32_t polarizationAxisNumber(bool doWorld=false) const;

   // Does this coordinate system have a quality axis?
   bool hasQualityAxis() const;

   // what number is the quality axis? Returns -1 if no quality axis exists.
   int32_t qualityAxisNumber() const;

   // what is the number of the quality coordinate?
   // Returns -1 if no quality coordinate exists.
   int32_t qualityCoordinateNumber() const;

   // Given a quality parameter, find the pixel location.
   // Note the client is responsible for any boundedness checks
   // (eg finite number of quality in an image).
   int32_t qualityPixelNumber(const String& qualityString) const;

   String qualityAtPixel(const uint32_t pixel) const;

   int32_t directionCoordinateNumber() const;

   bool hasDirectionCoordinate() const;

   // Get the pixel axis numbers of the direction coordinate in this object.
   // The order of the returned axis numbers is always longitude axis first,
   // latitude axis second.
   Vector<int32_t> directionAxesNumbers() const;

   String stokesAtPixel(const uint32_t pixel) const;

   int32_t linearCoordinateNumber() const;

   bool hasLinearCoordinate() const;

   Vector<int32_t> linearAxesNumbers() const;

   // Get the 0 based order of the minimal match strings specified in <src>order</src>.
   // If <src>requireAll</src> is true, checks are done to ensure that all axes in
   // the coordinate system are uniquely specified in <src>order</src>.
   // If <src>allowFriendlyNames</src> is true, the following (fully specified) strings
   // will match the specified axes:
   // "spectral" matches both "frequency" and "velocity".
   // "ra" matches "right ascension".
   Vector<int32_t> getWorldAxesOrder(Vector<String>& myNames, bool requireAll,
                                 bool allowFriendlyNames=false) const;

   // Is the abscissa in the DirectionCoordinate the longitude axis?
   // Throws exception if there is no DirectionCoordinate or if either of
   // the direction pixel axes have been removed.
   // For a normal direction coordinate, this will return true.
   bool isDirectionAbscissaLongitude() const;

    // Set Spectral conversion layer of SpectralCoordinate in CoordinateSystem
    // so that pixel<->world go to the specified frequency system (a valid
    // MFrequency::Types string).  Returns false if frequency system invalid
    // or if no DirectionCoordinate or if cant get Date/Epoch.
    // <group>
    bool setSpectralConversion (String& errorMsg, const String frequencySystem);
    // This version throws an exception rather than returning false.
    void setSpectralConversion (const String frequencySystem);
    //</group>
 
    // Set rest frequency of SpectralCoordinate in CoordinateSystem.
    // Unit must be consistent with Hz or m.
    // Returns false if invalid inputs (and CS not changed) and an error message.
    bool setRestFrequency (String& errorMsg, const Quantity& freq);

private:
    // Where we store copies of the coordinates we are created with.
    PtrBlock<Coordinate *> coordinates_p;
    
    // For coordinate[i] axis[j], 
    //    world_maps_p[i][j], if >=0 gives the location in the
    //                        input vector that maps to this coord/axis,
    //                        <0 means that the axis has been removed
    //    world_tmp_p[i] a temporary vector length coord[i]->nworldAxes()
    //    replacement_values_p[i][j] value to use for this axis if removed
    PtrBlock<Block<int32_t> *>     world_maps_p;
    PtrBlock<Vector<double> *> world_tmps_p;
    PtrBlock<Vector<double> *> world_replacement_values_p;

    // Same meanings as for the world*'s above.
    PtrBlock<Block<int32_t> *>     pixel_maps_p;
    PtrBlock<Vector<double> *> pixel_tmps_p;
    PtrBlock<Vector<double> *> pixel_replacement_values_p;

    // These temporaries all needed for the toMix function
    PtrBlock<Vector<bool> *> worldAxes_tmps_p;
    PtrBlock<Vector<bool> *> pixelAxes_tmps_p;
    PtrBlock<Vector<double> *> worldOut_tmps_p;
    PtrBlock<Vector<double> *> pixelOut_tmps_p;
    PtrBlock<Vector<double> *> worldMin_tmps_p;
    PtrBlock<Vector<double> *> worldMax_tmps_p;

    // Miscellaneous information about the observation associated with this
    // Coordinate System.
    ObsInfo obsinfo_p;

    const static String _class;
    static std::mutex _mapInitMutex;
    static std::map<String, String> _friendlyAxisMap;

    static void _initFriendlyAxisMap();

    // Helper functions to group common code.
    bool mapOne(Vector<int32_t>& worldAxisMap, 
                Vector<int32_t>& worldAxisTranspose, 
                Vector<bool>& refChange,
                const CoordinateSystem& cSys,
                const CoordinateSystem& cSys2,
                const uint32_t coord, const uint32_t coord2) const;

    void copy(const CoordinateSystem &other);
    void clear();
    bool checkAxesInThisCoordinate(const Vector<bool>& axes, uint32_t which) const;

   // Delete some pointer blocks
   void cleanUpSpecCoord (PtrBlock<SpectralCoordinate*>&  in,
                          PtrBlock<SpectralCoordinate*>&  out);

   // Delete temporary maps
   void deleteTemps (const uint32_t which);

    // Many abs/rel conversions
    // <group>
    void makeWorldAbsRelMany (Matrix<double>& value, bool toAbs) const;
    void makePixelAbsRelMany (Matrix<double>& value, bool toAbs) const;
    // </group>

    // Do subImage for Stokes
    StokesCoordinate stokesSubImage(const StokesCoordinate& sc, int32_t originShift, int32_t pixincFac,
                                    int32_t newShape) const;

    // Do subImage for Quality
    QualityCoordinate qualitySubImage(const QualityCoordinate& qc, int32_t originShift, int32_t pixincFac,
    		                        int32_t newShape) const;

    // Strip out coordinates with all world and pixel axes removed
    CoordinateSystem stripRemovedAxes (const CoordinateSystem& cSys) const;

    //  All these functions are in support of the <src>list</src> function
    // <group>
    void listDirectionSystem(LogIO& os) const; 
    void listFrequencySystem(LogIO& os, MDoppler::Types velocityType) const;
    void listPointingCenter (LogIO& os) const;
    void getFieldWidths (LogIO& os, uint32_t& widthAxis, uint32_t& widthCoordType, 
                         uint32_t& widthCoordNumber, uint32_t& widthName,
                         uint32_t& widthProj, uint32_t& widthShape,
                         uint32_t& widthTile, uint32_t& widthRefValue,
                         uint32_t& widthRefPixel, uint32_t& widthInc,
                         uint32_t& widthUnits, int32_t& precRefValSci,
                         int32_t& precRefValFloat,  int32_t& precRefValRADEC,
                         int32_t& precRefPixFloat, int32_t& precIncSci, String& nameAxis,
                         String& nameCoordType, String& nameCoordNumber, String& nameName, String& nameProj,
                         String& nameShape, String& nameTile,
                         String& nameRefValue, String& nameRefPixel,
                         String& nameInc, String& nameUnits,
                         MDoppler::Types velocityType,
                         const IPosition& latticeShape, const IPosition& tileShape) const;

    void listHeader (LogIO& os, Coordinate* pc, uint32_t& widthAxis, uint32_t& widthCoordType,  uint32_t& widthCoordNumber,
                     uint32_t& widthName, uint32_t& widthProj,
                     uint32_t& widthShape, uint32_t& widthTile, uint32_t& widthRefValue,
                     uint32_t& widthRefPixel, uint32_t& widthInc, uint32_t& widthUnits,     
                     bool findWidths, int32_t coordinate, int32_t axisInCoordinate, int32_t pixelAxis, 
                     int32_t precRefValSci, int32_t precRefValFloat, int32_t precRefValRADEC, int32_t precRefPixFloat,
                     int32_t precIncSci, const IPosition& latticeShape, const IPosition& tileShape) const;
    void listVelocity (LogIO& os,  Coordinate* pc, uint32_t widthAxis, 
                       uint32_t widthCoordType, uint32_t widthCoordNumber,
                       uint32_t& widthName, uint32_t widthProj,
                       uint32_t widthShape, uint32_t widthTile, uint32_t& widthRefValue,
                       uint32_t widthRefPixel, uint32_t& widthInc,  uint32_t& widthUnits,
                       bool findWidths, int32_t axisInCoordinate, int32_t pixelAxis, 
                       MDoppler::Types velocityType, int32_t precRefValSci, int32_t precRefValFloat,
                       int32_t precRefValRADEC, int32_t precRefPixFloat, int32_t precIncSci) const;
    void clearFlags (LogIO& os) const;
    bool velocityIncrement(double& velocityInc,  SpectralCoordinate& sc,
                           MDoppler::Types velocityType, const String& velUnits) const;
    // </group>

    void _downcase(Vector<String>& vec) const
      { for (uint32_t i=0; i<vec.size(); ++i) vec[i].downcase(); }

};

} //# NAMESPACE CASACORE - END

#endif

