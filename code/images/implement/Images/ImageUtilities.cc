//# ImageUtilities.cc:  Helper class for accessing images
//# Copyright (C) 1996,1997,1998,1999
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
//

#include <trial/Images/ImageUtilities.h>

#include <aips/Utilities/String.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <trial/Coordinates/CoordinateSystem.h>

  

Bool ImageUtilities::pixToWorld (Vector<String>& sWorld,
                                 const CoordinateSystem& cSysIn,
                                 const Int& pixelAxis,
                                 const Vector<Int>& cursorAxes,
                                 const IPosition& blc,
                                 const IPosition& trc,
                                 const Vector<Double>& pixels,
                                 const Int& prec)
//
// This function converts pixel coordinates to world coordinates.
// You specify pixel coordinates for only  one axis, the pixel axis,
// and you specify a Vector of pixels for conversion.   For the
// other pixel axes,  if a pixel axis is found in the CursorAxes
// vector, its pixel coordinate is set to the average pixel coordinate
// in the specified region ((blc(i)+trc(i))/2), otherwise it
// is set to the reference pixel. The Vector of world coordinates
// for the pixel axis is returned as formatted Strings.  If for some
// reason it can't make the conversion, a string is returned as "?"
// 
// Inputs
//   cSysIn        The CoordinateSystem associated with the image
//   pixelAxis     The pixel axis whose coordinates we are interested in.
//   cursorAxes    If any of the pixel axes, i, in the image are found this
//                 vector, assign their pixel coordinate to 
//                 (blc(i) + trc(i)) / 2  otherwise they get the 
//                 reference pixel
//   blc,trc       The region of the image being accessed. The average
//                 pixel coordinate in this region is used for the axes
//                 found in CursorAxes.  These must be of the same 
//                 dimension as the no. of pixel axes in teh 
//                 CoordinateSystem
//   pixels        Vector of pixel coordinates (0 rel) to transform
//                 for the pixel axis of interest. 
//   prec          Precision to format scientific output
// Outputs
//   sWorld        Vector of formatted strings of world coordinates
//                 for the pixel axis
//
{

// CHeck blc,trc

   if (blc.nelements()!=cSysIn.nPixelAxes() || trc.nelements()!=cSysIn.nPixelAxes()) return False;

// Create pixel and world vectors for all pixel axes. Initialize pixel values
// to reference pixel, but if an axis is a cursor axis (whose coordinate is
// essentially being averaged) set the pixel to the mean pixel.

   Vector<Double> pix(cSysIn.nPixelAxes());
   Vector<Double> world(cSysIn.nPixelAxes());
   pix = cSysIn.referencePixel(); 
   Bool found;
   uInt i;
   for (i=0; i<pix.nelements(); i++) {
     if (linearSearch(found, cursorAxes, Int(i), cursorAxes.nelements()) != -1) {
        pix(i) = Double(blc(i) + trc(i)) / 2.0;
     }
   }
         
            
// Find the world axis for this pixel axis 
            
   const Int worldAxis = cSysIn.pixelAxisToWorldAxis(pixelAxis);

          
// Convert to world and format 

   String formatUnits;
   Bool absolute = True;

   const uInt n1 = pixels.nelements();
   sWorld.resize(n1);

// Loop over list of pixel coordinates and convert to world
         
   for (i=0; i<n1; i++) {
      pix(pixelAxis) = pixels(i);
      if (cSysIn.toWorld(world,pix)) {
         sWorld(i) = cSysIn.format(formatUnits, Coordinate::DEFAULT, world(pixelAxis), 
                                 worldAxis, absolute, prec);
      } else {
         sWorld(i) = "?";
      }
   }

   return True;
}




String ImageUtilities::shortAxisName (const String& axisName)
//
// Look for "Right Ascension", "Declination", "Velocity",
// and "Frequency" in an axis string name and return "RA", 
// "Dec", "Vel", "Freq", respectively
// if these are found.  Anything else is returned as is.
// This will go away when I do something in the Coordinates
// classes to return long, short and FITS names
//
{
   String temp = axisName;
   temp.upcase();
   if (temp.contains("RIGHT ASCENSION")) {
     temp = "RA";
   } else if (temp.contains("DECLINATION")) {
     temp = "Dec";
   } else if (temp.contains("VELOCITY")) {
     temp = "Vel";
   } else if (temp.contains("FREQUENCY")) {
     temp = "Freq";
   } else {
     temp = axisName;
   }
   return temp;
}


Bool ImageUtilities::verifyRegion (IPosition& blc,
                                   IPosition& trc,
                                   IPosition& inc,
                                   const IPosition& imageShape)
//
// Make sure a region specification is within an image.  Currently,
// just a hyper cube can be set in image pixels (zero relative).
// 
// Returns
//  Bool      Retruns True if any of the inout objects are
//            changed on output, else False
// Input/output
//   blc,trc  The blc and trc.  Any illegal or missing blc values
//            are set to 0.  Any illegal or missing trc values
//            are set to imageShape.  If blc and trc are legal
//            then blc must be <= trc
//   inc      The increment through which to step through the image. 
//            Any missing values are set to unity.  If inc
//            is bigger than the selected region it is set to 1
//   
// Input
//   imageShape 
//         The image shape
//
{
   IPosition inBlc(blc);
   IPosition inTrc(trc);
   IPosition inInc(inc);
   const Int nDim = imageShape.nelements();

   const Int blcDim = blc.nelements();
   blc.resize(nDim,True);
   if (blcDim == 0) {
      blc = 0;
   } else {
      for (Int i=0; i<nDim; i++) {
         if (i > blcDim-1) {
            blc(i) = 0;
         } else {
            if (blc(i) < 0 || blc(i) > imageShape(i)-1) blc(i) = 0;
         }
      }
   }

   const Int trcDim = trc.nelements();
   trc.resize(nDim,True);
   if (trcDim == 0) {
      trc = imageShape - 1;
   } else {
      for (Int i=0; i<nDim; i++) {
         if (i > trcDim-1) {
            trc(i) = imageShape(i) - 1;
         } else {
            if (trc(i) < 0 || trc(i) > imageShape(i)-1) {
               trc(i) = imageShape(i) - 1;
            }
         }
      }
   }

   const Int incDim = inc.nelements();
   inc.resize(nDim,True);
   if (incDim == 0) {
      inc = 1;
   } else {
      for (Int i=0; i<nDim; i++) {
         if (i > incDim-1) {
            inc(i) = 1;
         } else {
            if (inc(i) < 1 || inc(i) > trc(i)-blc(i)+1) inc(i) = 1;
         }
      }
   }

   for (Int i=0; i<nDim; i++) {
      if (blc (i) > trc(i)) {
         blc(i) = 0;
         trc(i) = imageShape(i) - 1;
      }
   }

   Bool changed = ToBool(blc.nelements()!=inBlc.nelements() || 
                         trc.nelements()!=inTrc.nelements() || 
                         inc.nelements()!=inInc.nelements());
   if (!changed) changed = ToBool(blc!=inBlc || trc!=inTrc || inc!=inInc);
   return changed;
}

