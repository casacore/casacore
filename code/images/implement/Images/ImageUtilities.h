//# ImageUtilities.h: Some utility functions handy for accessing images
//# Copyright (C) 1996,1997,1999,2000,2001
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
#if !defined(AIPS_IMAGEUTILITIES_H)
#define AIPS_IMAGEUTILITIES_H


#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>
#include <aips/Measures/Stokes.h>
template <class T> class ImageInterface;
template <class T> class Vector;
template <class T> class Quantum;
class CoordinateSystem;
class SkyComponent;
class ImageInfo;
class String;
class IPosition;
class LogIO;
class Unit;

//
// <summary>
// Utility functions for Image manipulation
// </summary>
//   
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> IPosition
//   <li> Arrays
//   <li> Lattice
// </prerequisite>
//
// <synopsis>
// Some helpful static functions that are common to some of my image
// analysis application  classes.
// </synopsis>
//
// <motivation>
// I needed some bits and pieces.  My goal isto move this rag-tag bunch
// out of here into other classes as time goes on.  So far
// I have eliminated 80% of the original !
// </motivation>
//
// <todo asof="1996/11/27">
//  <li>  
// </todo>
 

class ImageUtilities
{
public:
// Define the possible image types.
   enum ImageTypes {
     // AIPS++
     AIPSPP,
     // FITS
     FITS,
     // Miriad
     MIRIAD,
     // Gipsy
     GIPSY,
     // Unknown
     UNKNOWN
   };

// Return the type of an image with the given name.
   static ImageTypes imageType (const String& fileName);

// Copy MiscInfo, ImageInfo, brightness unit and logger (history) from in to out
// SHould template this function.
   static void copyMiscellaneous (ImageInterface<Float>& out,
                                  const ImageInterface<Float>& in);

// This function converts pixel coordinates to world coordinates. You
// specify a vector of pixel coordinates (<src>pixels</src>) for only one 
// axis, the <src>pixelAxis</src>.    For the other pixel axes in the
// <src>CoordinateSystem</src>, if a pixel axis "i" is  found in the 
// <src>CursorAxes</src> vector, its pixel coordinate is set to 
// the average of the selected region from the image for that axis
// (<src>(blc(i)+trc(i))/2)</src>), otherwise it is set to the reference pixel.   
// The vector of world coordinates for <src>pixelAxis</src> is returned as formatted 
// Strings.  If for some reason it can't make the conversion, the element
// element is returned as "?"    Returns </src>False</src> if the lengths of
// <<src>blc</src> and <src>trc</src> are not equal to the number of pixel axes
// in the coordinate system.
   static Bool pixToWorld (Vector<String>& sWorld,
                           CoordinateSystem& cSys,
                           const Int& pixelAxis,
                           const Vector<Int>& cursorAxes,
                           const IPosition& blc,
                           const IPosition& trc,
                           const Vector<Double>& pixels,
                           const Int& prec);

// Convert long axis names "Right Ascension", "Declination", "Frequency" and
// "Velocity" to "RA", "Dec", "Freq", "Vel" respectively.  Unknown strings
// are returned as given.
   static String shortAxisName (const String& axisName);
//
// This function takes a vector of doubles holding SkyComponent values
// (the output from SkyComponent::toPixel) and converts them to a 
// SkyComponent.   The coordinate values are in the 'x' and 'y' frames (
// It is possible that the x and y axes of the pixel array are
// lat/long (xIsLong=False)  rather than  long/lat.
//
//   pars(0) = FLux     image units
//   pars(1) = x cen    abs pix
//   pars(2) = y cen    abs pix
//   pars(3) = major    pix
//   pars(4) = minor    pix
//   pars(5) = pa radians (pos +x -> +y)
//
//  5 values for ComponentType::Gaussian, CT::Disk.  3 values for CT::Point.
//
   static SkyComponent encodeSkyComponent(LogIO& os, Double& fluxRatio,
                                          const ImageInfo& ii,
                                          const CoordinateSystem& cSys,
                                          const Unit& brightnessUnit,
                                          ComponentType::Shape type,
                                          const Vector<Double>& parameters,
                                          Stokes::StokesTypes stokes,
                                          Bool xIsLong);

// Convert 2d sky shape (parameters=major axis, minor axis, position angle) 
// from pixels to world at reference pixel. pixelAxes describes which
// 2 pixel axes of the coordinate system our 2D shape is in.
// On input pa is positive for +x -> +y in pixel frame
// On output pa is positive N->E
// Returns True if major/minor exchanged themselves on conversion to world.
   static Bool skyPixelWidthsToWorld (LogIO& os,
                                      Vector<Quantum<Double> >& wParameters,
                                      const CoordinateSystem& cSys,
                                      const Vector<Double>& pParameters,
                                      const IPosition& pixelAxes);

//
// Convert 2d shape (parameters=major axis, minor axis, position angle)
// from world to pixel.  Can handle quantum units 'pix'.  If one width is 
// in pixel units both must be in pixel units.  pixelAxes describes which
// 2 pixel axes of the coordinate system our 2D shape is in.
// If axes are not from the same coordinate type units must be pixels.
//
// On input, pa is N->E (at ref pix) for celestial planes.
// Otherwise pa is in pixel coordinate system +x -> +y
// On output, pa (radians) is positive +x -> +y in pixel frame
   static void worldWidthsToPixel (LogIO& os, Vector<Double>& dParameters,
                                   const Vector<Quantum<Double> >& parameters,
                                   const CoordinateSystem& cSys,
                                   const IPosition& pixelAxes);

   private:

// Convert a length and position angle in world units (for a non-coupled 
// coordinate) to pixels. The length is in some 2D plane in the 
// CoordinateSystem specified  by pixelAxes.
   static Double worldWidthToPixel (LogIO& os, Double positionAngle,
                                    const Quantum<Double>& length,
                                    const CoordinateSystem& cSys,
                                    const IPosition& pixelAxes);
};


#endif

