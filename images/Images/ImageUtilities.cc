//# ImageUtilities.cc:  Helper class for accessing images
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/images/Images/ImageUtilities.h>
#include <casacore/images/Images/ImageOpener.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Regions/RegionHandler.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


Bool ImageUtilities::pixToWorld (
	Vector<String>& sWorld,	const CoordinateSystem& cSysIn,
	const Int& pixelAxis, const Vector<Int>& cursorAxes,
	const IPosition& blc, const IPosition& trc,
	const Vector<Double>& pixels,
	const Int& prec, const Bool usePrecForMixed
)
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
                                   worldAxis, True, True, prec, usePrecForMixed);
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


GaussianBeam ImageUtilities::makeFakeBeam(
		LogIO& logIO, const CoordinateSystem& csys, Bool suppressWarnings
	) {
    Int dirCoordinate = csys.findCoordinate(Coordinate::DIRECTION);
    if (dirCoordinate==-1) {
        logIO << "CoordinateSystem does not contain "
            << "a DirectionCoordinate" << LogIO::EXCEPTION;
    }
    const DirectionCoordinate& dirCoord = csys.directionCoordinate(dirCoordinate);

    Vector<Double> inc = dirCoord.increment();
    Quantity majAx(abs(inc[0]), "rad");
    Quantity minAx(abs(inc[1]), "rad");
    Quantity pa(0,"rad");
    if (! suppressWarnings) {
    	logIO << LogIO::WARN
    			<< "No restoring beam defined even though the "
    			<< "image brightness units contain a beam. Assuming "
    			<< "the restoring beam is one pixel. To avoid this non-fatal message "
    			<< "and subsequent related messages, add a restoring beam to your image's "
    			<< "header."
    			<< LogIO::POST;
    }
    return GaussianBeam(majAx, minAx, pa);
}


void ImageUtilities::writeImage(
		const TiledShape& mapShape,
		const CoordinateSystem& coordinateInfo,
		const String& imageName,
		const Array<Float>& pixels, LogIO& log,
		const Array<Bool>& maskPixels
) {
	// using pattern from ImageProxy
	if (!maskPixels.empty()) {
		if (! maskPixels.shape().isEqual(mapShape.shape())) {
			log << "Requested image shape differs from pixel mask shape"
				<< LogIO::EXCEPTION;
		}
	}
	PagedImage<Float> *newImage = new PagedImage<Float>(
			mapShape, coordinateInfo, imageName
	);
	if (newImage == 0) {
		log << "Failed to create image "
			 << imageName << LogIO::EXCEPTION;
	}
	newImage->put(pixels);
	if (! maskPixels.empty()) {
		newImage->makeMask("mask0", True, True).asMask().put(maskPixels);
	}

	log << LogIO::NORMAL << "Created image "
		<< imageName << LogIO::POST;
	delete newImage;
}

void ImageUtilities::getUnitAndDoppler(
	String& xUnit, String& doppler,
	const uInt axis, const CoordinateSystem& csys
) {
    xUnit = csys.worldAxisUnits()[axis];
    doppler = "";
	Int specCoordIndex = csys.findCoordinate(Coordinate::SPECTRAL);
    if (
    	specCoordIndex >= 0
    	&& axis == (uInt)csys.pixelAxes(specCoordIndex)[0]
    	&& ! csys.spectralCoordinate(specCoordIndex).velocityUnit().empty()
    ) {
    	SpectralCoordinate specCoord = csys.spectralCoordinate(specCoordIndex);
    	xUnit = specCoord.velocityUnit();
    	doppler = MDoppler::showType(
    		specCoord.velocityDoppler()
    	);
    }
}

void ImageUtilities::copyAttributes (ImageAttrHandler& out,
                                     ImageAttrHandler& in)
{
  Vector<String> groupNames = in.groupNames();
  for (uInt i=0; i<groupNames.size(); ++i) {
    ImageAttrGroup& inGroup  = in.openGroup (groupNames[i]);
    ImageAttrGroup& outGroup = out.createGroup (groupNames[i]);
    Vector<String> attrNames = inGroup.attrNames();
    for (uInt rownr=0; rownr<inGroup.nrows(); ++rownr) {
      for (uInt j=0; j<attrNames.size(); ++j) {
        outGroup.putData (attrNames[j], rownr,
                          inGroup.getData (attrNames[j], rownr),
                          inGroup.getUnit (attrNames[j]),
                          inGroup.getMeasInfo (attrNames[j]));
      }
    }
    in.closeGroup (groupNames[i]);
    out.closeGroup (groupNames[i]);
  }
}
 

} //# NAMESPACE CASACORE - END
