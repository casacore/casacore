//# CoordinateSystem.h: Interconvert pixel and image coordinates.
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_COORDINATE_SYSTEM_H)
#define AIPS_COORDINATE_SYSTEM_H

#include <aips/aips.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/ObsInfo.h>

#include <aips/Containers/Block.h>

class DirectionCoordinate;
class LinearCoordinate;
class SpectralCoordinate;
class StokesCoordinate;
class TabularCoordinate;
class IPosition;

// <summary>
// Interconvert pixel and image coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Coordinate>Coordinate</linkto>
// </prerequisite>

// <synopsis>
// <src>CoordinateSystem</src> is the normal interface to coordinate systems,
// typically attached to an 
// <linkto class=ImageInterface>ImageInterface</linkto>, however the
// coordinate system can be manipulated on its own. <src>CoordinateSystem</src>
// is in turn composed from various classes derived from
// <linkto class=Coordinate>Coordinate</linkto>.
// <p>
// The fundamental operations available to the user of a 
// <src>CoordinateSystem</src> are:
// <ol>
//   <li> Transform a world (physical) coordinate system to a pixel coordinate or 
//        vice versa via <src>toWorld()</src> and <src>toPixel()</src>.
//   <li> Compose a coordinate system from one or more independent groups,
//        typically the sky-plane transformation will be one group, and the
//        spectral axis will be another group. Each group consists of a linear
//        transformation (in FITS terms, apply <src>CRPIX, PC, CDELT</src>)
//        to turn the pixel coordinates into relative physical coordinates, 
//        followed by a (possibly) nonlinear projection to world coordinates 
//        (i.e. apply <src>CTYPE and CRVAL</src>), typically a sky projection
//        or a frequency to velocity conversion. Note that an arbitrary rotation
//        or linear transformation can be applied by changing the
//        matrix.
//   <li> Transpose the world and/or pixel axes.
//   <li> One or more pixel or world axes may be removed. You are encouraged to
//        leave all the world axes if you remove a pixel axis.
//        Removing a world axis also removes the corresponding pixel axis.
//   <li> Calculate the coordinate system that results from a subimage
//        operation.
//   <li> Various convenience functions to create "standard" coordinate systems.
// </ol>
//
// Note that all the knowledge to do with removing and transposing axes is
// maintained by the CoordinateSystem.  The individual Coordinates, of which it
// is made, know nothing about this.
// <p>
// Although the CoordinateSystem exists in the absence of an image, the usual
// time you will find one is attached to an object derived from ImageInterface 
// such as PagedImage. When you do so, the physical (or pixel) axes in the image
// map one to one with the pixel axes contained in the CoordinateSystem.
// It cannot be any other way as when you create a PagedImage, it is checked
// that there are equal numbers of image and CoordinateSystem pixel axes.
// It is up to the creator of the PagedImage to make sure that they are
// in the correct order.
// <p>
// However, the CoordinateSystem may have more world axes than pixel axes
// because it is possible to remove a pixel axis but not its associated
// world axis.   Now, if you use the CoordinateSystem functions 
// referencePixel and referenceValue to get these vectors, the latter
// will have more values than the former if a pixel axis has been removed,
// but not the world axis.  You must use the ancilliary functions provided
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
// Now this CoordinateSystem has two coordinates, a DirectionCoordinate and
// a SpectralCoordinate, and let us state that that is the order in which
// they exist in the CoordinateSystem (you can change them about if you wish);
// they are coordinates number 0 and 1. The DirectionCoordinate has two axes
// (RA and DEC) and the SpectralCoordinate has one axis. Only the
// CoordinateSystem knows about removed axes, the DirectionCoordinate
// itself is ignorant that it has been bisected. If you want to find
// out what axis in the Coordinate system is where, you can use
// CoordinateSystem::findPixelAxis or CoordinateSystem::findWorldAxis.
// If we asked the former to find pixel axis 0, it would tell us that the
// Coordinate number was 0 (the DirectionCoordinate) and that the axis in
// that coordinate was 0 (the first axis in a DirectionCoordinate
// is always longitude, the second always latitude).  If we asked it to find
// pxiel axis 1, it would tell us that the Coordinate number was 1
// (the SpectralCoordinate) and that the axis in that coordinate was 0
// (there is only one axis in a SpectralCoordinate). If we asked for
// pixelAxis 2 that would generate an error because out squashed image
// only has 2 pixel axes. Now, if we asked findWorldAxis similar questions,
// it would tell us that worldAxis 0 in the CoordinateSystem can be found in
// coordinate 0 (the DirectionCoordinate) in axis 0 of that DirectionCoordinate.
// Similarly, worldAxis 1 in the CoordinateSystem (which has not been removed)
// is in coordinate 0 (the DirectionCoordinate) in axis 1 of that 
// Finally, worldAxis 2 in the CoordinateSystem is in coordinate 1 
// (the SpectralCoordinate) in axis 0 of that SpectralCoordinate.
// <p>
// Other handy functions are CoordinateSystem::pixelAxes and
// CoordinateSystem::worldAxes These list the pixel and world axes in
// the CoordinateSystem for the specified coordinate. Thus, if we asked
// pixelAxes to find the pixel axes for coordinate 0 (the  DirectionCoordinate)
// in the CoordinateSystem it would return a vector [0, -1]  indicating
// the second axis of  the DirectionCoordinate has been removed.  However, 
// the worldAxes function would return [0,1] as no world axis has been removed.
// Similarly, if operated on coordinate 1 (the SpectralCoordinate), pixelAxes
// would return [1] and worldAxes would return [2].
//
// Because you can transpose the CoordinateSystem about, you should never assume
// anything except that the pixel axes of the CoordinateSystem map to the pixel
// axes of the image when you first construct the image.
// </synopsis>

// <example>
// See the example in <linkto module=Coordinates>Coordinates.h</linkto>.
// </example>

// <motivation>
// Coordinate systems for images.
// </motivation>

// <todo asof="1997/01/13">
//   <li> Add an equivalent of wcsmix() - either here or at a higher level.
//   <li> Undelete individual removed axes.
//   <li> Non-integral pixel shifts/decimations in subimage operations?
//   <li> Copy-on-write for efficiency?
// </todo>


class CoordinateSystem : public Coordinate
{
public:
    CoordinateSystem();

    // Copying and assignment use copy semantics.
    // <group>
    CoordinateSystem(const CoordinateSystem &other);
    CoordinateSystem &operator=(const CoordinateSystem &other);
    // </group>

    ~CoordinateSystem();

    // Add another coordinate to this coordinate system. This addition is done
    // by copying, so that if <src>coord</src> changes the change is NOT
    // reflected in the coordinate system.
    void addCoordinate(const Coordinate &coord);

    // Transpose the coordinate system so that world axis 0 is
    // <src>newWorldOrder(0)</src> and so on for all the other axes.
    // <src>newPixelOrder</src> works similarly. Normally you will give the
    // same transformation vector for both the world and pixel transformations,
    // however this is not required.
    void transpose(const Vector<Int> &newWorldOrder,
                   const Vector<Int> &newPixelOrder);

    // Find the world axis mapping to the supplied <src>CoordinateSystem</src>
    // from the current <src>CoordinateSystem</src>. <src>False</src> is 
    // returned if either the supplied or current <src>CoordinateSystem</src>, 
    // has no world axes (and a message recoverable with function
    // <src>errorMessage</src> indicating why).  Othwerwise <src>True</src> is returned.
    // <src>worldAxisMap(i)</src> is the location of world axis <src>i</src> (from the
    // supplied <src>CoordinateSystem</src>, <src>cSys</src>,
    // in the current <src>CoordinateSystem</src>.
    // <src>worldAxisTranspose(i)</src> is the location of world axis 
    // <src>i</src> (from the current <src>CoordinateSystem</src>) in the supplied 
    // <src>CoordinateSystem</src>, <src>cSys</src>.  The output vectors
    // are resized appropriately by this function.  A value of  -1 
    // in either vector means that it could not be found in the other
    // <src>CoordinateSystem</src>.  Note that two world axes of the
    // same coordinate type (e.g. <src>SpectralCoordinate</src>)
    // will be considered to not  match if their specific types are 
    // different (e.g. TOPO versus LSR for the <src>SpectralCoordinate</src>, 
    // or J2000 versus GALACTIC for <src>DirectionCoordinate</src>).
    Bool worldMap (Vector<Int>& worldAxisMap,
		   Vector<Int>& worldAxisTranspose,
		   const CoordinateSystem& cSys) const;

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
    // <group>
    void removeWorldAxis(uInt axis, Double replacement);
    void removePixelAxis(uInt axis, Double replacement);
    // </group>

    // Both vectors should be of length nPixelAxes(). At present this will throw
    // an exception if <src>nPixelAxes() != nWorldAxes()</src>
    CoordinateSystem subImage(const Vector<Int> &originShift,
			      const Vector<Int> &pixinc) const;

    // Untranspose and undelete all axes. Does not undo the effects of
    // subimaging.
    void restoreOriginal();

    // Returns the number of coordinates that this coordinate system contains.
    // The order might be unrelated to the axis order through the results of
    // transposing and removing axes.
    uInt nCoordinates() const;

    // For a given coordinate say where its world and coordinate axes are in
    // this coordinate system. The position in the returned Vector is its
    // axis number in the Coordinate system, and its value is the axis
    // number in the CoordinateSystem. If the value is less than zero the axis
    // has been removed from this CoordinateSystem.  <group>
    Vector<Int> worldAxes(uInt whichCoord) const;
    Vector<Int> pixelAxes(uInt whichCoord) const;
    // </group> 

    // Return the type of the given coordinate.
    Coordinate::Type type(uInt whichCoordinate) const;

    // Returns the type of the given coordinate
    String showType(uInt whichCoordinate) const;

    // Return the given coordinate.
    const Coordinate &coordinate(uInt which) const;

    // Return the given coordinate.
    // Throws an exception if retrieved as the wrong type.
    // <group>
    const LinearCoordinate &linearCoordinate(uInt which) const;
    const DirectionCoordinate &directionCoordinate(uInt which) const;
    const SpectralCoordinate &spectralCoordinate(uInt which) const;
    const StokesCoordinate &stokesCoordinate(uInt which) const;
    const TabularCoordinate &tabularCoordinate(uInt which) const;
    // </group>

    // Replace one coordinate with another. The mapping of the coordinate axes
    // to the coordinate system axes is unchanged, therefore the number of world
    // and pixel axes must not be changed. You can change the type of the
    // coordinate however. For example, replace a SpectralCoordinate with a 1-D
    // Linearcoordinate.
    void replaceCoordinate(const Coordinate &newCoordinate, uInt whichCoordinate);

    // Find the coordinate number that corresponds to the given type.
    // Since there might be more than one coordinate of a given type you
    // can call this multiple times setting <src>afterCoord</src> to
    // the last value found. Returns -1 if a coordinate of the desired
    // type is not found.
    Int findCoordinate(Coordinate::Type type, Int afterCoord = -1) const;

    // Turn an axis number in the coordinate system into the coordinate number
    // and axis in that coordinate. That is, this function undoes the effects
    // of transposing and removing axes. The returned values are set to -1 if
    // the axis does not exist.
    // <group>
    void findWorldAxis(Int &coordinate, Int &axisInCoordinate, 
		       uInt axisInCoordinateSystem) const;
    void findPixelAxis(Int &coordinate, Int &axisInCoordinate, 
		       uInt axisInCoordinateSystem) const;
    // </group>

    // Find the world axis for the given pixel axis in a coordinate system
    // Returns -1 if the world axis is unavailable (e.g. if it has been
    // removed).
    Int pixelAxisToWorldAxis(uInt pixelAxis) const;

    // Find the pixel axis for the given world axis in a coordinate system
    // Returns -1 if the pixel axis is unavailable (e.g. if it has been
    // removed). 
    Int worldAxisToPixelAxis(uInt pixelAxis) const;

    // Returns <src>Coordinate::COORDSYS</src>
    virtual Coordinate::Type type() const;

    // Always returns "System"
    virtual String showType() const;

    // Sums the number of axes in the coordinates that the coordinate system
    // contains, correcting for removed axes.
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
    // </group>

    // This is provided as a convenience since it is a very commonly desired
    // operation through CoordinateSystem.
    Bool toWorld(Vector<Double> &world, const IPosition &pixel) const;

    // Report the value of the requested attributed.
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
    virtual Bool setIncrement(const Vector<Double> &inc);
    virtual Bool setReferenceValue(const Vector<Double> &refval);
    // </group>

    // Change the units. If <src>adjust</src> is True, adjust the increment and
    // reference value by the ratio of the old and new units. This implies that
    // the units must be known <linkto class=Unit>Unit</linkto> strings, and
    // that they must be compatible, e.g. they can't change from time to
    // length.
    virtual Bool setWorldAxisUnits(const Vector<String> &units,
				   Bool adjust = True);

    // Comparison function. Any private Double data members are compared
    // with the specified fractional tolerance.  Don't compare on the specified 
    // pixel axes in the CoordinateSystem.  If the comparison returns
    // False, <src>errorMessage()</src> contains a message about why.
    virtual Bool near(const Coordinate* pOther, Double tol=1e-6) const;
    virtual Bool near(const Coordinate* pOther, 
                      const Vector<Int>& excludePixelAxes,
                      Double tol=1e-6) const;

    // Format a world value with the common format interface (refer to the base 
    // class <linkto class=Coordinate>Coordinate</linkto> for more details on this 
    // interface).   For the specified <src>worldAxis</src>, the coordinate
    // number in the CoordinateSystem is found and the actual derived Coordinate
    // class object for that number is created.  The arguments to the formatting 
    // function are then passed on to the formatter for that Coordinate. So
    // refer to the other derived Coordinate classes for specifics on the
    // formatting.
    virtual String format(String& units,
                          const Coordinate::formatType format,
                          const Double worldValue,
                          const uInt worldAxis,
                          const Bool absolute,
                          const Int precision = -1) const;

    // Miscellaneous information related to an observation, for example the
    // observation date.
    // <group>
    ObsInfo obsInfo() const;
    void setObsInfo(const ObsInfo &obsinfo);
    // </group>

    // Save ourself into the supplised record using the supplied field name.
    // The field must not exist, otherwise <src>False</src> is returned.
    virtual Bool save(RecordInterface &container,
		    const String &fieldName) const;

    // A null pointer means that the restoration did not succeed - probably 
    // because fieldName doesn't exist or doesn't contain a coordinate system.
    static CoordinateSystem *restore(const RecordInterface &container,
				   const String &fieldName);

    // Make a copy of ourself using new. The caller is responsible for calling
    // delete.
    virtual Coordinate *clone() const;

    // Convert a CoordinateSystem to FITS, i.e. fill in ctype etc. In the record
    // the keywords are vectors, it is expected that the actual FITS code will
    // split them into scalars and upcase the names. Returns False if one of the
    // keywords is already taken.
    // 
    // If writeWCS is True, attempt to write the WCS convention (Greisen and
    // Calabretta "Representation of celestial coordinates in FITS"). This is
    // a DRAFT convention evolving rapidly. It is not recommended that you
    // write this convention in general.
    //# Much of the work in to/from fits should be moved to the individual
    //# classes.
    Bool toFITSHeader(RecordInterface &header, 
		      IPosition &shape,
		      Bool oneRelative, 
		      char prefix = 'c', Bool writeWCS=False,
		      Bool preferVelocity=True, 
		      Bool opticalVelocity=True) const;


    // Probably even if we return False we should set up the best linear
    // coordinate that we can.
    //# cf comment in toFITS.
    static Bool fromFITSHeader(CoordinateSystem &coordsys, 
			       const RecordInterface &header,
			       Bool oneRelative,
			       char prefix = 'c');

private:
    // Where we store copies of the coordinates we are created with.
    PtrBlock<Coordinate *> coordinates_p;
    
    // For coordinate[i] axis[j], 
    //    world_maps_p[i][j], if >=0 gives the location in the
    //                        input vector that maps to this coord/axis,
    //                        <= means that the axis has been removed
    //    world_tmp_p[i] a temporary vector length coord[i]->nWorldAxes()
    //    replacement_values_p[i][j] value to use for this axis if removed
    PtrBlock<Block<Int> *>     world_maps_p;
    PtrBlock<Vector<Double> *> world_tmps_p;
    PtrBlock<Vector<Double> *> world_replacement_values_p;

    // Same meanings as for the world*'s above.
    PtrBlock<Block<Int> *>     pixel_maps_p;
    PtrBlock<Vector<Double> *> pixel_tmps_p;
    PtrBlock<Vector<Double> *> pixel_replacement_values_p;

    // Miscellaneous information about the observation associated with this
    // Coordinate System.
    ObsInfo obsinfo_p;

    // Helper functions to group common code.
    Bool mapOne(Vector<Int>& worldAxisMap, 
                Vector<Int>& worldAxisTranspose, 
                const CoordinateSystem& cSys,
                const CoordinateSystem& cSys2,
                const uInt coord, const uInt coord2) const;

    void copy(const CoordinateSystem &other);
    void clear();
};

#endif
