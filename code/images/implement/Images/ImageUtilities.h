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
template <class T> class Vector;
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
};


#endif

