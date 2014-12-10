//# ImageSummary.h: List descriptive information from an image 
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#ifndef IMAGES_IMAGESUMMARY_H
#define IMAGES_IMAGESUMMARY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/ObsInfo.h>
#include <casacore/images/Images/ImageInfo.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Vector;
class IPosition;
class Unit;
class LogIO;
class Coordinate;


// <summary>
// Provides and lists information about the header of an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto module=Coordinates>Coordinates</linkto> 
// </prerequisite>

// <etymology>
// This class lists the ancilliary descriptive information from an image
// </etymology>

// <synopsis>
// Images consist of pixel values and descriptive information.
// This information describes the coordinate system, the image 
// units etc.  This class enables you to 
// retrieve the descriptive information and/or list it.
// <p>
// The functions that retrieve specific coordinate information in vectors 
// (e.g. <src>referenceValues</src>) return it in the order of the (pixel) axes of 
// the image.  Note that this can be different from the order in which 
// the <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// functions of similar name might return them.   This is because the
// order of the coordinates in the CoordinateSystem is not necessarily 
// the same order as the pixel axes in the associated image, although 
// of course there is a known association.
//
// <note role=tip>
// This class lists information about the coordinates in the image.
// The Coordinates classes can maintain the information in a variety
// of units.  For example, angular quantities are by default in radians,
// but the manipulator of a <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// may have converted to some other unit such as arcseconds.  This
// means that when this class fetches coordinate information and returns
// it to you (such as the <src>referenceValues()</src> function, 
// the information is returned to you in whatever units the coordinates
// are currently in.  It does not convert it.
// </note>
// </synopsis>

// <example>
// <srcBlock>
//     PagedImage<Float> inImage(fileName);
//     ImageSummary<Float> summary(inImage);
//     LogOrigin or("myClass", "myFunction(...)", WHERE);
//     LogIO os(or);
//     summary.list(os);
// </srcBlock>
// A <src>PagedImage</src> object is constructed and then logged to the 
// supplied <src>LogIO</src> object.
// </example>

// <motivation>
// The viewing of the descriptive image information is a basic capability.
// </motivation>

//# <todo asof="1998/08/31">
//#  None that I know of.
//# </todo>
 

template <class T> class ImageSummary
{
public:
// Constructor
   ImageSummary (const ImageInterface<T>&);

// Copy constructor
   ImageSummary (const ImageSummary<T> &other);

// Destructor
  ~ImageSummary();

// Assignment operator
   ImageSummary<T> &operator=(const ImageSummary<T> &other);

// Retrieve number of image dimension
   Int ndim () const;
 
// Retrieve image shape
   IPosition shape () const;

// Retrieve tile shape with which image is stored on disk
   IPosition tileShape () const;

// Retrieve axis names in pixel or world axis order.
   Vector<String> axisNames (Bool pixelOrder=True) const;

// Retrieve reference pixels (0 or 1 rel)
   Vector<Double> referencePixels (Bool oneRel=True) const;

// Retrieve reference values in pixel or world axis order. 
   Vector<Double> referenceValues (Bool pixelOrder=True) const;

// Retrieve axis increments in pixel or world axis order.  
   Vector<Double> axisIncrements (Bool pixelOrder=True) const;

// Retrieve axis units in pixel or world axis order.
   Vector<String> axisUnits(Bool pixelOrder=True) const;

// Retrieve image units
   Unit units () const;
 
// Retrieve image name.  Any prepended path is stripped off.
   String name() const;

// Retrieve observer name
   String observer() const;

// Return epoch of observation as MEpoch or formatted string
   String obsDate(MEpoch& date) const;

// Return telescope
   String telescope() const;

// Return rest frequency.  Returns False if none.
   Bool restFrequency(String& restFreqString, Quantum<Double>& restFreq) const;

// Return frequency system.  Returns False if none.
   Bool frequencySystem(String& freqTypeString, MFrequency::Types& freqType) const;

// Return direction system.  Returns False if none.
   Bool directionSystem(String& dirTypeString, MDirection::Types& dirType) const;

// Retrieve whether image has mask or not
   Bool hasAMask () const;

// Retrieve mask names
   Vector<String> maskNames() const;

// Retrieve region names
   Vector<String> regionNames() const;

// Retrieve default mask name.  Empty if none
   String defaultMaskName() const;


// Retrieve image type
   String imageType () const;

// List all header information.  By default, the reference
// values and pixel increments are converted to a "nice" unit before 
// formatting (e.g. RA is  shown as HH:MM:SS.S).  
// For spectral axes, both frequency and velocity information is listed. You
// can specify what velocity definition you want with <src>velocityType</src>
// If postLocally is True, the formatted strings are returned in the return value
   Vector<String> list(
		   LogIO& os, const MDoppler::Types velocityType=MDoppler::RADIO,
		   Bool postLocally=False, const Bool verbose=False
   );

// Set a new image
   Bool setNewImage (const ImageInterface<T>& image);


private:
   CoordinateSystem cSys_p;
   ObsInfo obsInfo_p;
   ImageInfo imageInfo_p;
   const ImageInterface<T>* pImage_p;

    String makeMasksString() const;
    String makeRegionsString() const;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageSummary.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
