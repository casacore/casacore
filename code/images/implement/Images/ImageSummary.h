//# ImageSummary.h: List descriptive information from an image 
//# Copyright (C) 1996,1997,1998
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
//
#if !defined(AIPS_IMAGESUMMARY_H)
#define AIPS_IMAGESUMMARY_H

#if defined(_AIX)
#pragma implementation ("ImageSummary.cc")
#endif

#include <aips/aips.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MDoppler.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/ObsInfo.h>

template <class T> class MaskedImage;
template <class T> class Vector;
class IPosition;
class Unit;
class LogIO;
class Coordinate;


// <summary> Provides and lists information about the header of an image </summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class=MaskedImage>ImageInterface</linkto>
//   <li> <linkto module=Coordinates>Coordinates</linkto> 
// </prerequisite>
//
// <etymology>
// This class lists the ancilliary descriptive information from an image
// </etymology>
//
// <synopsis>
// Images consist of pixel values and descriptive information.
// This information describes the coordinate system, the image 
// units etc.  This class enables you to 
// retrieve the descriptive information and/or list it.
//
// The functions that retrieve specific coordinate information in vectors 
// (e.g. <src>referenceValues</src>) return it in the order of the (pixel) axes of 
// the image.  Note that this can be different from the order in which 
// the <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// functions of similar name might return them.   This is because the
// order of the coordinates in the CoordinateSystem is not necessarily 
// the same order as the pixel axes in the associated image, although 
// of course there is a known association.
// 
// </synopsis>
//
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
//
// <note role=caution>
// Note that if the <src>PagedImage</src> goes out of scope, this
// class will retrieve rubbish as it just maintains a pointer
// to the image.
// </note>
//
// <motivation>
// The viewing of the descriptive image information is a basic capability.
// </motivation>
//
// <todo asof="1998/08/31">
//  None that I know of.
// </todo>
 

template <class T> class ImageSummary
{
public:
// Constructor
   ImageSummary (const MaskedImage<T>&);

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

// Retrieve axis names in pixel axis order.
   Vector<String> axisNames () const;

// Retrieve reference pixels
   Vector<Double> referencePixels () const;

// Retrieve reference values in pixel axis order. 
   Vector<Double> referenceValues () const;

// Retrieve axis increments in pixel axis order.  
   Vector<Double> axisIncrements () const;

// Retrieve axis units in pixel axis order.
   Vector<String> axisUnits() const;

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

// Retrieve whether image has mask or not
   Bool hasAMask () const;

// List all header information.  By default, the reference
// values and pixel increments are converted to a "nice" unit before 
// formatting (e.g. RA is  shown as HH:MM:SS.S).  If <src>nativeFormat</src> 
// is <src>True</src> then the values are formatted in their native format.
// For spectral axes, both frequency and velocity information is listed. You
// can specify what velocity definition you want with <src>velocityType</src>
   void list(LogIO& os, 
             const MDoppler::Types velocityType=MDoppler::RADIO,
             const Bool nativeFormat=False);

// Set a new image
   Bool setNewImage (const MaskedImage<T>& image);


private:

   CoordinateSystem cSys_p;
   ObsInfo obsInfo_p;
   const MaskedImage<T>* pImage_p;

   void getFieldWidths (uInt& widthName, 
                        uInt& widthProj,
                        uInt& widthShape,
                        uInt& widthTile,
                        uInt& widthRefValue,
                        uInt& widthRefPixel,
                        uInt& widthInc,
                        uInt& widthUnits,
                        Int& precRefValSci,
                        Int& precRefValloat,
                        Int& precRefValRADEC,   
                        Int& precRefPixFloat,
                        Int& precIncSci,
                        String& nameName,
                        String& nameProj,
                        String& nameShape,
                        String& nameTile,
                        String& nameRefValue,
                        String& nameRefPixel,
                        String& nameInc,
                        String& nameUnits,
                        const Bool& nativeFormat,
                        const CoordinateSystem& cSys,
                        const MDoppler::Types velocityType) const;


// List axis descriptors
   void listHeader (LogIO& os,
                    Coordinate* pc,
                    uInt& widthName,
                    uInt& widthProj,
                    uInt& widthShape,
                    uInt& widthTile,
                    uInt& widthRefValue,
                    uInt& widthRefPixel,
                    uInt& widthInc,
                    uInt& widthUnits,
                    const Bool findWidths,
                    const Int axisInCoordinate,
                    const Int pixelAxis,
                    const Bool nativeFormat,
                    const Int precRefValSci,
                    const Int precRefValFloat,
                    const Int precRefValRADEC,
                    const Int precRefPixFloat,
                    const Int precIncSci) const;

// List axis descriptors
   void listVelocity (LogIO& os,
                    Coordinate* pc,
                    uInt& widthName,
                    uInt& widthProj,
                    uInt& widthShape,
                    uInt& widthTile,
                    uInt& widthRefValue,
                    uInt& widthRefPixel,
                    uInt& widthInc,
                    uInt& widthUnits,
                    const Bool findWidths,
                    const Int axisInCoordinate,
                    const Int pixelAxis,
                    const MDoppler::Types velocityType,
                    const Int precRefValSci,
                    const Int precRefValFloat,
                    const Int precRefValRADEC,
                    const Int precRefPixFloat,
                    const Int precIncSci) const;

Bool pixelToVelocity(Double& velocity,
                     const Double pixel,
                     const SpectralCoordinate* sc,
                     const MDoppler::Types velocityType,
                     const String& velUnits) const;

Bool velocityIncrement(Double& velocityInc,
                       const SpectralCoordinate* sc,
                       const MDoppler::Types velocityType,
                       const String& velUnits) const;

// Clear formatting flags
   void clearFlags (LogIO& os) const;

};

#endif
