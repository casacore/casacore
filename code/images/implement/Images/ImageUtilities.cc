//# ImageUtilities.cc:  Helper class for accessing images
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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
#include <aips/Utilities/Regex.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Arrays/Vector.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableInfo.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/Images/ImageInfo.h>
#include <aips/Logging/LogIO.h>
#include <aips/Measures/Stokes.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Unit.h>
#include <aips/OS/RegularFile.h>
#include <aips/IO/RegularFileIO.h>


ImageUtilities::ImageTypes ImageUtilities::imageType (const String& name)
{
  File file(name);
  if (file.isDirectory()) {
    if (Table::isReadable(name)) {
      TableInfo info = Table::tableInfo (name);
      if (info.type() == TableInfo::type(TableInfo::PAGEDIMAGE)) {
	return AIPSPP;
      }
    } else {
      if (File(name + "/header").isRegular()  &&
	  File(name + "/image").isRegular()) {
	return MIRIAD;
      }
    }
  } else if (file.isRegular()) {
    // Find file type.
    String base = file.path().baseName();
    Int i;
    for (i=base.length()-1; i>0; i--) {
      if (base[i] == '.') {
	break;
      }
    }
    if (i > 0  &&  base.after(i) == "image") {
      String descName = file.path().dirName() + '/' +
	                base.before(i) + ".descr";
      if (File(descName).isRegular()) {
	return GIPSY;
      }
    }
    RegularFileIO fio((RegularFile(file)));
    char buf[2880];
    Int nread = fio.read (2880, buf, False);
    if (nread == 2880) {
      String str(buf, 80);
      if (str.matches (Regex("^SIMPLE *= *T.*"))) {
	return FITS;
      }
    }
  }
  return UNKNOWN;
}
  

Bool ImageUtilities::pixToWorld (Vector<String>& sWorld,
                                 CoordinateSystem& cSysIn,
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
   const uInt n1 = pixels.nelements();
   sWorld.resize(n1);

// Loop over list of pixel coordinates and convert to world
         
   for (i=0; i<n1; i++) {
      pix(pixelAxis) = pixels(i);
      if (cSysIn.toWorld(world,pix)) {
         sWorld(i) = cSysIn.format(formatUnits, Coordinate::DEFAULT, world(pixelAxis), 
                                   worldAxis, True, True, prec);
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


SkyComponent ImageUtilities::encodeSkyComponent(LogIO& os, Double& fluxRatio,
                                                const ImageInfo& ii,
                                                const CoordinateSystem& cSys,
                                                const Unit& brightnessUnit,
                                                ComponentType::Shape type,
                                                const Vector<Double>& parameters,
                                                Stokes::StokesTypes stokes,
                                                Bool xIsLong)
{
   SkyComponent sky;
  
// Account for the fact that 'x' could be longitude or latitude.  Urk.

   Vector<Double> pars = parameters.copy();
   if (!xIsLong) {
      Double tmp = pars(0);
      pars(0) = pars(1);
      pars(1) = tmp;
//
      Double pa0 = pars(5);
      MVAngle pa(pa0 + C::pi_2);
      pa();                         // +/- pi
      pars(5) = pa.radian();
   }
//
   Vector<Quantum<Double> > beam = ii.restoringBeam();
   sky.fromPixel(fluxRatio, pars, brightnessUnit, beam, cSys, type, stokes);
   return sky;
} 
