//# ImageUtilities.h: Some utility functions handy for accessing images
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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
#ifndef IMAGES_IMAGEUTILITIES_H
#define IMAGES_IMAGEUTILITIES_H


#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <images/Images/MaskSpecifier.h>
#include <images/Images/ImageFit1D.h>
#include <measures/Measures/Stokes.h>
#include <casa/Utilities/PtrHolder.h>
#include <casa/Containers/SimOrdMap.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Vector;
template <class T> class Quantum;
template <class T> class MaskedArray;
class LatticeBase;
class CoordinateSystem;
class Coordinate;
class SkyComponent;
class ImageInfo;
class String;
class IPosition;
class LogIO;
class Unit;
class AxesSpecifier;
class ImageAttrHandler;


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
// Open disk image (can be any registered image).  Exception
// if fileName empty or file does not exist or file is not
// of legal image type.   For aips++ images, the default mask is
// applied.
//  <group>
   static void openImage (PtrHolder<ImageInterface<Float> >& image,
                          const String& fileName, LogIO& os);
   static void openImage (ImageInterface<Float>*& image,
                          const String& fileName, LogIO& os);
//  </group>

// Copy MiscInfo, ImageInfo, brightness unit, attributes, and logger (history)
// from in to out
   template <typename T, typename U>
   static void copyMiscellaneous (ImageInterface<T>& out,
                                  const ImageInterface<U>& in);

// Copy a mask from one image to another
   static void copyMask (ImageInterface<Float>& out,
                         const ImageInterface<Float>& in,
                         const String& maskOut, const String& maskIn,
                         AxesSpecifier axesSpecifier);

  // Copy the attributes from one image to another.
  static void copyAttributes (ImageAttrHandler& out,
                              ImageAttrHandler& in);

// Add one degenerate axis for each of the specified coordinate types.
// If the outfile string is given the new image is a PagedImage.
// If the outfile string is empty, the new image is a TempImage.
   static void addDegenerateAxes (LogIO& os,
                                  PtrHolder<ImageInterface<Float> >& outImage,
                                  ImageInterface<Float>& inImage,
                                  const String& outFile, Bool direction, 
                                  Bool spectral, const String& stokes, 
                                  Bool linear, Bool tabular, Bool overwrite);

// Function to bin up (average data) one axis of an N-D MaskedArray. The interface
// is pretty specific to a particular application. It's here because
// its implemented with ImageRebin.  On input, the output MA *must*
// have zero shape.   The input and output Coordinates must have the
// same type and have only one axis (Linear,Spectral & Tabular).
// The output coordinate is adjusted for the binning.   The binning
// factor does not have to fit integrally into the shape of the specified
// axis.
   template <typename T>
   static void bin (MaskedArray<T>& out, Coordinate& coordOut,
                    const MaskedArray<T>& in, const Coordinate& coordIn,
                    uInt axis, uInt bin);

   // Fit all profiles in image.  The output images must be already
   // created; if the pointer is 0, that image won't be filled.
   // The mask from the input image is transferred to the output
   // images.    If the weights image is pointer is non-zero, the
   // values from it will be used to weight the data points in the
   // fit.  You can fit some combination of gaussians and a polynomial
   // (-1 means no polynomial).  Initial estimates are not required.
   // Fits are done in image space to provide astronomer friendly results,
   // but pixel space is better for the fitter when fitting polynomials.
   // Thus, atm, callers should be aware that fitting polynomials may
   // fail even when the data lie exactly on a polynomial curve.
   // This will probably be fixed in the future by doing the fits
   // in pixel space here and requiring the caller to deal with converting
   // to something astronomer friendly if it so desires.

   template <typename T>
   static Vector<ImageFit1D<T> > fitProfiles (
	   ImageInterface<T>* &pFit,
       ImageInterface<T>* &pResid, String& xUnit,
       const ImageInterface<T>& inImage,
       const uInt axis, const uInt nGauss=1,
       const Int poly=-1, const Bool showProgress=False
   );

// This function converts pixel coordinates to world coordinates. You
// specify a vector of pixel coordinates (<src>pixels</src>) for only one 
// axis, the <src>pixelAxis</src>.    For the other pixel axes in the
// <src>CoordinateSystem</src>, if a pixel axis "i" is  found in the 
// <src>CursorAxes</src> vector, its pixel coordinate is set to 
// the average of the selected region from the image for that axis
// (<src>(blc(i)+trc(i))/2)</src>), otherwise it is set to the reference pixel.   
// The vector of world coordinates for <src>pixelAxis</src> is returned as formatted 
// Strings.  If for some reason it can't make the conversion, the element
// element is returned as "?"    Returns <src>False</src> if the lengths of
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

// These functions convert between a vector of doubles holding SkyComponent values
// (the output from SkyComponent::toPixel) and a SkyComponent.   The coordinate 
// values are in the 'x' and 'y' frames.  It is possible that the x and y axes of 
// the pixel array are lat/long (xIsLong=False)  rather than  long/lat.  
// facToJy converts the brightness units from whatevers per whatever
// to Jy per whatever (e.g. mJy/beam to Jy/beam).  It is unity if it
// can't be done and you get a warning.  In the SkyComponent the flux
// is integral.  In the parameters vector it is peak.
//
//   pars(0) = FLux     image units (e.g. Jy/beam).
//   pars(1) = x cen    abs pix
//   pars(2) = y cen    abs pix
//   pars(3) = major    pix
//   pars(4) = minor    pix
//   pars(5) = pa radians (pos +x -> +y)
//
//  5 values for ComponentType::Gaussian, CT::Disk.  3 values for CT::Point.
//
// <group>
   static SkyComponent encodeSkyComponent(LogIO& os, Double& facToJy,
                                          const ImageInfo& ii,
                                          const CoordinateSystem& cSys,
                                          const Unit& brightnessUnit,
                                          ComponentType::Shape type,
                                          const Vector<Double>& parameters,
                                          Stokes::StokesTypes stokes,
                                          Bool xIsLong);

    // for some reason, this method was in ImageAnalysis but also belongs here.
    // Obviously hugely confusing to have to methods with the same name and
    // which presumably are for the same thing in two different classes. I'm
    // moving ImageAnalysis' method here and also moving that implamentation to
    // here as well and also being consistent regarding callers (ie those that
    // called the ImageAnalysis method will now call this method). I couldn't
    // tell you which of the two implementations is the better one to use
    // for new code, but this version does call the version that already existed
    // in ImageUtilities, so this version seems to do a bit more.
    // I also hate having a class with anything like Utilities in the name,
    // but I needed to move this somewhere and can only tackle one issue
    // at a time.
    static casa::SkyComponent encodeSkyComponent(
        casa::LogIO& os, casa::Double& fluxRatio,
        const casa::ImageInterface<casa::Float>& im, 
        casa::ComponentType::Shape modelType,
        const casa::Vector<casa::Double>& parameters,
        casa::Stokes::StokesTypes stokes,
        casa::Bool xIsLong, casa::Bool deconvolveIt
    );

    // Deconvolve SkyComponent from beam
    // moved from ImageAnalysis. this needs to be moved to a more appropriate class at some point
    static casa::SkyComponent deconvolveSkyComponent(
        casa::LogIO& os, const casa::SkyComponent& skyIn,
        const casa::Vector<casa::Quantum<casa::Double> >& beam,
        const casa::DirectionCoordinate& dirCoord
    );

    // moved from ImageAnalysis. this needs to be moved to a more appropriate class at some point
    // Put beam into +x -> +y frame
    static casa::Vector<casa::Quantum<casa::Double> >putBeamInXYFrame (
        const casa::Vector<casa::Quantum<casa::Double> >& beam,
        const casa::DirectionCoordinate& dirCoord
    );

    // moved from ImageAnalysis. this needs to be moved to a more appropriate class at some point
    static Vector<Double> decodeSkyComponent (
        const SkyComponent& sky, const ImageInfo& ii,
        const CoordinateSystem& cSys, const Unit& brightnessUnit,
        Stokes::StokesTypes stokes, Bool xIsLong
    );
// </group>

    // moved from ImageAnalysis. this needs to be moved to a more appropriate class at some point
    // Deconvolve from beam
    // Returns True if the deconvolved source is a point source, False otherwise.
    // The returned value of successFit will be True if the deconvolved size could be determined, False otherwise.
    static Bool deconvolveFromBeam(
        Quantity& majorFit, casa::Quantity& minorFit,
        Quantity& paFit, Bool& successFit, LogIO& os,
        const Vector<Quantity>& beam, const Bool verbose=True
    );

    static Bool deconvolveFromBeam(
        Quantity& majorFit, Quantity& minorFit,
        Quantity& paFit, casa::Bool& successFit, casa::LogIO& os,
        const Vector<Quantity>& sourceIn, const Vector<Quantity>& beam,
        const Bool verbose=True
    );

//
// Convert 2d shape from world (world parameters=x, y, major axis, 
// minor axis, position angle) to pixel (major, minor, pa).  
// Can handle quantum units 'pix'.  If one width is 
// in pixel units both must be in pixel units.  pixelAxes describes which
// 2 pixel axes of the coordinate system our 2D shape is in.
// If axes are not from the same coordinate type units must be pixels.
// If doRef is True, then x and y are taken from the reference
// value rather than the parameters vector.
//
// On input, pa is N->E (at ref pix) for celestial planes.
// Otherwise pa is in pixel coordinate system +x -> +y
// On output, pa (radians) is positive +x -> +y in pixel frame
   static void worldWidthsToPixel (LogIO& os, Vector<Double>& dParameters,
                                   const Vector<Quantum<Double> >& parameters,
                                   const CoordinateSystem& cSys,
                                   const IPosition& pixelAxes,
                                   Bool doRef=False); 


// Convert 2d shape  from pixels (parameters=x,y, major axis, 
// minor axis, position angle) to world (major, minor, pa)
// at specified location. pixelAxes describes which
// 2 pixel axes of the coordinate system our 2D shape is in.
// If doRef is True, then x and y are taken from the reference
// pixel rather than the paraneters vector.
//
// On input pa is positive for +x -> +y in pixel frame
// On output pa is positive N->E
// Returns True if major/minor exchanged themselves on conversion to world.
   static Bool pixelWidthsToWorld (LogIO& os,
                                   Vector<Quantum<Double> >& wParameters,
                                   const Vector<Double>& pParameters,
                                   const CoordinateSystem& cSys,
                                   const IPosition& pixelAxes,
                                   Bool doRef=False); 

   // write the specified image and add the specified pixels to it.
   // Currently no checks are done to ensure the pixel array size and
   // mapShape are compatible; the caller is responsible for this check.
   static void writeImage(
   		const TiledShape& mapShape,
   		const CoordinateSystem& coordinateInfo,
   		const String& imageName,
   		const Array<Float>& pixels, LogIO& log
   );

   static Vector<Quantity> makeFakeBeam(LogIO& logIO, const CoordinateSystem& csys, Bool suppressWarnings = False);

   static void getUnitAndDoppler(
	   String& xUnit, String& doppler,
	   const uInt axis, const CoordinateSystem& csys
   );

private:

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
                                      const IPosition& pixelAxes, Bool doRef);

// Convert a length and position angle in world units (for a non-coupled 
// coordinate) to pixels. The length is in some 2D plane in the 
// CoordinateSystem specified  by pixelAxes.
   static Double worldWidthToPixel (LogIO& os, Double positionAngle,
                                    const Quantum<Double>& length,
                                    const CoordinateSystem& cSys,
                                    const IPosition& pixelAxes);



   static Quantum<Double> pixelWidthToWorld (LogIO& os, Double positionAngle,
                                             Double length,
                                             const CoordinateSystem& cSys,
                                             const IPosition& pixelAxes);
};



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageUtilities2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
