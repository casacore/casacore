//# CoordinateUtils.h: static functions dealing with coordinates
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2004
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
//# $Id$

#ifndef COORDINATES_COORDINATEUTIL_H
#define COORDINATES_COORDINATEUTIL_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>

#include <casacore/measures/Measures/MDirection.h>        //# For enums
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class CoordinateSystem;
class DirectionCoordinate;
class ObsInfo;
class String;
class LogIO;
class MEpoch;
class MPosition;
class Unit;


// <summary>Functions for creating default CoordinateSystems</summary>
// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="CoordinateSystem">CoordinateSystem</linkto>
// </prerequisite>
//
// <etymology> 
// CoordinateUtils follows the Casacore naming convention for static functions
// that are associated with a class.
// </etymology>
//
// <synopsis>
// This file contains declarations for static functions that manipulate
// coordinate systems. It currently contains functions for:
// <ul>
// <li> Adding default axes to a CoordinateSystem
// <li> Creating a default CoordinateSystem
// <li> Finding specified axes in a CoordinateSystem
// </ul>
// 
// The functions for adding default axes to a CoordinateSystem can add
// either a RA/DEC pair of axes, a Polarisation Axis, or a Spectral Axis to
// a user supplied coordinate system. The default values for these functions
// are:
// <ul>
// <li> <src>addDirAxes</src> this adds a DirectionCoordinate with a
// reference pixel of (0,0) corresponding to an RA/DEC of (0,0) in a
// J2000 reference frame. The pixel increment is 1 arc-minute.
// <li> <src>addIQUVAxis</src> this adds a polarization axis with four
// elements corresponding to the Stokes (I,Q,U,V) components.
// <li> <src>addIAxis</src> this adds a polarization axis with one
// element corresponding to the Stokes I component only
// <li> <src>addFreqAxis</src> this adds a spectral axis with a reference
// frequency of 1.415GHz on channel 0. The channel bandwidth (pixel
// increment) is 1kHz, and the reference frame is the kinematical Local Standard of
// rest (<linkto class="MFrequency">MFrequency</linkto>::LSRK). 
// </ul>
//
// The <src>defaultCoords</src> functions, create from scratch a
// CoordinateSystem using the above described <src>addXXXAxis</src>
// functions to add the required number of dimensions to the
// CoordinateSystem. Only 2, 3 or 4 dimensional coordinate systems can be
// constructed using these functions. The coordinate systems always have
// RA/Dec axes. Three dimensional Systems add a spectral axis and
// four-dimensional systems add an IQUV  polarization axis. An exception
// (AipsError) is thrown if <src>defaultCoords(uInt)</src> is called with a
// parameter that is not 2, 3, or 4. 
//
// The <src>defaultCoordsXX</src> functions return the coordinate system by
// value (which involves a copy of the CoordinateSystem) and hence are not
// as effcient as the <src>addXXXAxis</src> functions.
//
// If the default axes provided by these functions are not quite what is
// required it is possible to use member functions of the 
// <linkto class="CoordinateSystem">CoordinateSystem</linkto>
// and <linkto class="Coordinate">Coordinate</linkto> classes 
// (<linkto class="DirectionCoordinate">DirectionCoordinate</linkto>,
// <linkto class="StokesCoordinate">StokesCoordinate</linkto>,
// <linkto class="SpectralCoordinate">SpectralCoordinate</linkto> etc.)
// to tweak the appropriate parameters of the specified axis.
//
// Now we turn to the functions for finding axes in a CoordinateSystem. With
// a CoordinateSystem object it is not required that the first Coordinate
// axis in the the CoordinateSystem map to the first pixel axis in an
// image. Hence it is necessary to determine which pixel axis corresponds to a
// specified Coordinate and this can be done using these functions. Some
// coordinate types, in particular DirectionCoordinate, usually map to more
// than one pixel axis (DirectionsCoordinates are inherently two-dimensional).
// 
// This group contains declarations for static functions that search
// CoordinateSystem's for a coordinate of the specified type. It returns the
// pixel axis (zero relative) of the specified coordinate type. If the supplied
// Coordinate system does not contain the specified coordinate type the
// returned value is function specific (but usually -1). If the supplied   
// CoordinateSystem contains two or more of the specified coordinateType then
// an exception (AipsError) is thrown.
//
// Finally functions are provided for removing lists of pixel/world axes
// from a CoordinateSystem.
// This process is made a little awkward by the fact that when you
// remove one axis, all the rest shuffle down one, so it is
// provided here.  Generally, one only needs to remove one axis
// (in which case you should use the CoordinateSystem::removeWorldAxis and
// CoordinateSystem::removcePixelAxis functions), but on occaision,
// the multiple need is there.
// </synopsis>
//
// <example>
// I use these functions when creating test images. 
// <srcblock>
// PagedImage(IPosition(4,256,256,4,32), CoordinateUtil::defaultCoords4D(),
//            String("test.image"));
// </srcblock>
// </example>
//
// <example>
// Functions are needed to handle images without specifying a canonical
// coordinate order. For example suppose we want to find the spectral aixs
// of a PagedImage object.
//     
// <srcblock>
//   const Int spectralAxis = CoordinateUtil::findSpectralAxis(image.coordinates());
//   cout << "The spectral axis is of shape " << image.shape()(spectralAxis) << endl;
// </srcblock>
// </example>
//
// <example>
// Here we remove the first and last world axes, and their associated
// pixel axes from a 3D CoordinateSystem.  The reference values and
// reference pixels are used for the replacement values.
//     
// <srcblock>
//   CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
//   Vector<Int> worldAxes(2);
//   worldAxes(0) = 0; worldAxes(1) = cSys.nWorldAxes()-1;
//   Vector<Double> worldRep;
//   Bool ok = CoordinateUtil::removeAxes(cSys, worldRep, worldAxes, True);
//   cout << "For world axes used " << worldRep << " for replacement" << endl;
// </srcblock>
// </example>
//
//
// <motivation>
// I got fed up writing small functions to create and find coordinates when writing
// test programs involving Images and ComponentModels.
// </motivation>
//
// <thrown>
//    <li> AipsError
// </thrown>
//
// <todo asof="1997/01/23">
//   Many of these methods belong in the CoordinateSystem class,
//   eg all the add* methods, and in fact CoordinateSystem already has analogs
//   for many of them. The factory methods which create a CoordinateSystem
//   could also arguably go in CoordinateSystem as static methods. Having a separate
//   utility class that really just has methods that operate on or create CoordinateSystem
//   objects makes no sense. CoordinateUtil is the antithesis of object oriented design,
//   and we need to endeavor to expunge it from our system.
// </todo>

//  <linkfrom anchor=defaultAxes classes="CoordinateSystem">
//      Static functions for creating <here>default</here> coordinate systems
//  </linkfrom>

class CoordinateUtil
{
public: 

// Add a RA/DEC pair of direction axes (ie. a DirectionCoordinate) to the
// user supplied CoordinateSystem. See the synopsis above for the current
// default values.
static void addDirAxes(CoordinateSystem& coords);

// Add a Stokes I,Q,U,V axis to the user supplied CoordinateSystem.
static void addIQUVAxis(CoordinateSystem& coords);

// Add a Stokes I (only) axis to the user supplied CoordinateSystem.
static void addIAxis(CoordinateSystem& coords);

// Add a Stokes axis of length 1 to 4 selected from I,Q,U,V
// E.g. if shape=2 you get IQ.   Returns False if shape
// is not in the range 1 to 4
static Bool addStokesAxis(CoordinateSystem& coords, uInt shape);

// Add Linear axes.  The LinearCoordinate can have > 1 axes (like
// the DirectionCoordinate has 2).  The number of axes is given
// by the length of the names argument.   If you supply a shape,
// it will be used to set the reference pixel to 1/2 the shape.
// If the shape does not have the same number of elements as
// the names variable, the reference pixel will be 0
static void addLinearAxes (CoordinateSystem & coords,
                           const Vector<String>& names,
                           const IPosition& shape);

// Add a spectral axis to the user supplied CoordinateSystem. See the
// synopsis above for the current default values.
static void addFreqAxis(CoordinateSystem& coords);


// Add one axis for each of the specified coordinate types.
// Returns the number of axes added.
// If silent==True, existing axes are silently ignored.
// This should really be a method of CoordinateSystem, but the
// code was moved from ImageUtilities which makes heavy use
// of CoordUtil methods (which aren't available to CoordinateSystem)
static uInt addAxes (
	CoordinateSystem& csys,
	Bool direction,
	Bool spectral, const String& stokes,
	Bool linear, Bool tabular,
	Bool silent=False
);

// Return a 2-dimensional coordinate system with RA/DEC axes only. 
static CoordinateSystem defaultCoords2D();

// Return a 3-dimensional coordinate system with RA/DEC axes and a spectral axis.
static CoordinateSystem defaultCoords3D();

// Return a 4-dimensional coordinate system with RA/DEC axes, an IQUV
// polarisation axis  and a spectral axis.
static CoordinateSystem defaultCoords4D();

// Calls one of the above three functions depending of the arguement. An
// AipsError is thrown if dims is not 2, 3, or 4.
static CoordinateSystem defaultCoords(uInt dims);

// If doLinear=False, Tries to make a standard RA/DEC/Stokes/Frequency CoordinateSystem
// depending upon the shape.   The shape for the Stokes axis
// must be <= 4.   If axis 2 can't be Stokes it will be a Spectral
// axis instead.  AFter the standard types, the rest (if any)
// of the CoordinateSystem consists of LinearCoordinates.
// If doLinear=True, then you just get a linear coordinate system
static CoordinateSystem makeCoordinateSystem(const IPosition& shape,
                                             Bool doLinear=False);

//
// Find which pixel axis in the CoordinateSystem corresponds to the
// SpectralCoordinate. If there is no SpectralCoordinate in the coordinate
// system then return -1.
static Int findSpectralAxis(const CoordinateSystem & coords);

// Find the SpectralCoordinate in the CoordinateSystem, and then
// return the most general description of where it is.  
// If there is no SpectralCoordinate in the CoordinateSystem then return 
// -1 for coordinate.  If the world or pixel axis has been removed,
// return -1 for that value. 
static void findSpectralAxis(Int& pixelAxis, Int& worldAxis, Int& coordinate,
                             const CoordinateSystem & coords);

// Find which pixel axes correspond to the DirectionCoordinate in the 
// supplied coordinate system and return this as a Vector. If there is no 
// DirectionCoordinate in the CoordinateSystem then return a Vector of zero 
// length. Normally the returned Vector will have a length of two.  
// However, if the pixel axis has been removed, then the resultant
// vector will take the value -1 for that axis.
static Vector<Int> findDirectionAxes(const CoordinateSystem & coords);

// Find which pixel axes correspond to the DirectionCoordinate in the supplied coordinate
// system and return the most general description of where it is. If there is 
// no DirectionCoordinate then coordinate is returned with value -1.
// Values of -1 in the returned vectors indicate an axis has been removed.
static void findDirectionAxes(Vector<Int>& pixelAxes, Vector<Int>& worldAxes,
                              Int& coordinate, const CoordinateSystem & coords);

// Find which pixel axis is the polarisation axis in the supplied
// CoordinateSystem and return this. If there is no StokesCoordinate in the
// CoordinateSystem return a negative number. The actual polarisations on the
// returned pixel axis are returned in the whichPols Vector. Each element of
// this Vector is a Stokes::StokesTypes enumerator and the length of the Vector
// is the same as the length of the polarisation axis. If there is no
// polarisation axis the whichPols returns a unit length Vector containing
// Stokes::I
static Int findStokesAxis(Vector<Stokes::StokesTypes>& whichPols, 
			  const CoordinateSystem& coords);

// Find the StokesCoordinate in the CoordinateSystem, and then
// return the most general description of where it is.  
// If there is no StokesCoordinate in the CoordinateSystem then return 
// -1 for coordinate.  If the world or pixel axis has been removed,
// return -1 for that value. 
static void findStokesAxis(Int& pixelAxis, Int& worldAxis, Int& coordinate,
                           const CoordinateSystem & coords);

// Find Coordinate type for this pixel or world axis
// <group>
static Coordinate::Type findPixelAxis (const CoordinateSystem& cSys, Int axis);
static Coordinate::Type findWorldAxis (const CoordinateSystem& cSys, Int axis);
// </group>

// Remove a list of world axes and their associated
// pixel axes from a <src>CoordinateSystem</src>. The list of world
// axes to be removed is derived from a list giving either axes to remove, 
// or axes to keep (controlled by whether <src>remove</src> 
// is <src>True</src> or <src>False</src>.  The replacement values (see functions 
// <src>CoordinateSystem::removeWorldAxis</src>) for the world axes
// can be given.  For the associated pixel axes, the pixel replacement
// coordinate is found by converting the world coordinate 
// to a pixel coordinate. If the length of the replacement value 
// vector is not the number of world axes to be removed then
// the reference values will be used (e.g. use zero length
// vectors).
static Bool removeAxes(CoordinateSystem& cSys,
                       Vector<Double>& worldReplacement,
                       const Vector<Int>& worldAxes,
                       const Bool remove);

// Remove a list of pixel axes but not their associated
// world axes from a <src>CoordinateSystem</src>. 
// The list of pixel axes to be removed is derived from a 
// list giving either axes to remove, 
// or axes to keep (controlled by whether <src>remove</src> 
// is <src>True</src> or <src>False</src>.  The replacement values (see functions 
// <src>CoordinateSystem::removePixelAxis</src>) for the pixel axes
// can be given.  If the length of the replacement value 
// vector is not the number of pixel axes to be removed then
// the reference pixel will be used (e.g. use zero length
// vectors).
static Bool removePixelAxes(CoordinateSystem& cSys,
                            Vector<Double>& pixelReplacement,
                            const Vector<Int>& pixelAxes,
                            const Bool remove);

// Physically (nont just virtually) drop coordinates from the CoordinateSystem
// if all axes are fully removed. For coordinates with axes partially removed
// (world/pixel) preserve that removal state in the output CS.  No effort
// is made to deal in any way with transposed systems, unless perserveAxesOrder
// is True, and then the ordering of the axes of the output coordinate system
// will be the same as the input cSysIn (sans dropped axes of course).
static Bool dropRemovedAxes (
	CoordinateSystem& cSysOut, const CoordinateSystem& cSysIn,
	Bool preserveAxesOrder=False
);

// Setup Measures conversion machine for MDirections.
// Returns True if the machine was needed and set.  Returns False
// if the machine was not needed and not set.  
  static Bool makeDirectionMachine(LogIO& os, MDirection::Convert& machine,
                                   const DirectionCoordinate& dirCoordTo,
                                   const DirectionCoordinate& dirCoordFrom,
                                   const ObsInfo& obsTo,
                                   const ObsInfo& obsFrom);

// Setup Measures conversion machines for MFrequencies. 
// Returns False if a trial conversion failed, else returns True.
// There must be both a Direction and a Spectral
// Coordinate in the CoordinateSystem when making the Frequency machine,
// else an exception occurs.
   static Bool makeFrequencyMachine(LogIO& os, MFrequency::Convert& machine, 
                                    Int coordinateTo, Int coordinateFrom, 
                                    const CoordinateSystem& coordsTo, 
				    const CoordinateSystem& coordsFrom,
                                    const Unit& unit=Unit(String("Hz")));

// Setup Measures conversion machines for MFrequencies.
// Returns False if a trial conversion failed, else returns True.
   static Bool makeFrequencyMachine(LogIO& os, MFrequency::Convert& machine,
                                    MFrequency::Types typeTo, MFrequency::Types typeFrom,
                                    const MDirection& dirTo, const MDirection& dirFrom,
                                    const MEpoch& epochTo, const MEpoch& epochFrom,
                                    const MPosition& posTo, const MPosition& posFrom,
                                    const Unit& unit=Unit(String("Hz")));

// Find the Sky in the CoordinateSystem. Assumes only one DirectionCoordinate.
// <src>pixelAxes</src> and <src>worldAxes</src>  say where
// in the CS the DirectionCoordinate axes are (long then lat).
// Returns False and an error message if it can't find the sky.
   static Bool findSky(String& errorMessage, Int& dirCoord, Vector<Int>& pixelAxes,
                       Vector<Int>& worldAxes, const CoordinateSystem& cSys);

// Do the specified axes hold the sky ?  Returns False if no DirectionCoordinate
// or if only one axis of the DirectionCoordinate is held or the specified
// pixel axes don't pertain to the DirectionCoordinate.  
   static Bool holdsSky (Bool& holdsOneSkyAxis, const CoordinateSystem& cSys, 
                         Vector<Int> pixelAxes);


// Find the Stokes for the specified pixel. If there is no Stokes in the
// CoordinateSystem, returns Stokes::I
   static Stokes::StokesTypes findSingleStokes (LogIO& os, const CoordinateSystem& cSys,
                                                uInt pixel=0);

// Set the world axis units in the CS to 'deg' for Direction. For Spectral
// set the velocity handling to use 'km/s' units.  Other coordinates
// are not touched.
   static void setNiceAxisLabelUnits(CoordinateSystem& cSys);

// Set world axis units for specific Coordinate.  Returnd False if fails to set units
// with error in cSys.errorMessage().  
   static Bool setCoordinateUnits (CoordinateSystem& cSys, const Vector<String>& units,
                                   uInt which);

// Set a unit for all unremoved world axes in the DirectionCoordinate in the
// CS.  Returns False if fails to set unit with error in cSys.  If no DC
// returns True
   static Bool setDirectionUnit (CoordinateSystem& cSys, const String& unit, Int which=-1);

// Set Direction conversion layer of DirectionCoordinate in CoordinateSystem
// so that pixel<->world go to the specified direction system (a valid
// MDirection::Types string).  Returns False with error if direction
// system invalid.  If no DirectionCoordinate returns True
   static Bool setDirectionConversion (String& errorMsg, CoordinateSystem& cSys,
                                      const String directionSystem);

// Set spectral state of SpectralCoordinate in CoordinateSystem.
// Unit must be consistent with Hz or m/s and the doppler a valid MDoppler string.
// For no change, leave either String empty.
// Returns False if invalid inputs (and CS not changed) and an error message. 
   static Bool setSpectralState (String& errorMsg, CoordinateSystem& cSys, 
                                 const String& unit, const String& spcquant);

// Set velocity state of SpectralCoordinate in CoordinateSystem.
// Unit must be consistent m/s and the doppler a valid MDoppler string.
// For no change, leave either String empty.
// Returns False if invalid inputs (and CS not changed) and an error message. 
   static Bool setVelocityState (String& errorMsg, CoordinateSystem& cSys, 
                                 const String& unit, const String& spcquant);

  //#/// Kept setRestFrequency for CASA-4.2
// Does the CoordinateSystem hold just the sky?
// Returns True if CS pixel axis 0 is the longitude and 1 latitude  
// else returns False
   static Bool isSky (LogIO& os, const CoordinateSystem& cSys);

  //#/// Kept setRestFrequency for CASA-4.2
// Set rest frequency of SpectralCoordinate in CoordinateSystem.
// Unit must be consistent with Hz or m.
// Returns False if invalid inputs (and CS not changed) and an error message.
   static Bool setRestFrequency (String& errorMsg, CoordinateSystem& cSys,
                                 const String& unit,
                                 const Double& value);

  //#/// Kept setSpectralConversion for old casarest
// Set Spectral conversion layer of SpectralCoordinate in CoordinateSystem
// so that pixel<->world go to the specified frequency system (a valid
// MFrequency::Types string).  Returns False if frequency system invalid
// or if no DirectionCoordinate or if cant get Date/Epoch
   static Bool setSpectralConversion (String& errorMsg, CoordinateSystem& cSys,
                                      const String frequencySystem);

// Set default format unit and doppler velocity state of SpectralCoordinate in CoordinateSystem.
// Unit can be consistent with Hz or m/s
// Returns False if invalid inputs (and CS not changed) and an error message. 
   static Bool setSpectralFormatting (String& errorMsg, CoordinateSystem& cSys, 
                                      const String& unit, const String& spcquant);

// Convert an absolute pixel coordinate to world and format with 
// default Coordinate formatting
// <group>
   static String formatCoordinate(const IPosition& pixel, const CoordinateSystem& cSys, Int precision = -1);
   static String formatCoordinate(const Vector<Double>& pixel, const CoordinateSystem& cSys, Int precision = -1);
// </group>

// Generate axis label String from coordinate. Specify coordinate axis,
// whether world or pixel labels required, whether absolute or
// relative.   For spectral coordinates, doVel says if you want to 
// use the velocity information contained in it to generate the label
   static String axisLabel (const Coordinate& coord, uInt axisInCoordinate=0,
                            Bool doWorld=True, Bool doAbs=True, Bool doVel=False);

  // <group name=Coordinate comparison>
  // Check how the coordinates of this and that compare.
  // The return value tells how they compare.
  // <br>-1: left is subset
  // <br>0: equal 
  // <br>1: left is superset
  // <br>9: invalid (mismatch)
  static Int compareCoordinates (const CoordinateSystem& thisCsys,
				 const CoordinateSystem& thatCsys);

  // Convert the world axes map given in worldAxes to a pixel axes map.
  static Vector<Int> toPixelAxes (const CoordinateSystem& thisCsys,
				  const CoordinateSystem& thatCsys,
				  const Vector<Int>& worldAxes);

  // Check if the axes in the pixel axes map are in ascending order.
  static Bool checkOrder (const Vector<Int>& pixelAxes);

  // Find the new and stretch axes when comparing the old and new
  // coordinates and shapes (helper for ExtendImage).
  static Bool findExtendAxes (IPosition& newAxes,
			      IPosition& stretchAxes,
			      const IPosition& newShape,
			      const IPosition& oldShape,
			      const CoordinateSystem& newCsys,
			      const CoordinateSystem& oldCsys);
  // </group>

  // Fix up Cylindrical parameters in any DirectionCoordinate for when the longitude 
  // is outside of [-180,180] range.  If it returns False, it failed and an error 
  // message is returned as well.  This function should be called on any
  // CS made from an imported image like FITS
  static Bool cylindricalFix (CoordinateSystem& cSys, String& errorMessage, const IPosition& shape);

  // Apply the binning factors to the CS and create a new one reflecting the binning
  // You can optionally throw an exception if factors is non-unit for any Stokes axis
  static CoordinateSystem makeBinnedCoordinateSystem (const IPosition& factors,
                                                      const CoordinateSystem& cSysIn,
                                                      Bool failOnStokes=False);
private:
  // Sets pos to the position found for tel in the database, or
  // raises an exception + error message.
  static void findObservatoryOrRaiseException(LogIO& os, MPosition& pos,
					      const String& tel);
};


} //# NAMESPACE CASACORE - END

#endif
