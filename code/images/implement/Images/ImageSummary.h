//# ImageSummary.h: Helper class for applications listing an image header
//# Copyright (C) 1996,1997
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
#if !defined(AIPS_IMAGESUMMARY_H)
#define AIPS_IMAGESUMMARY_H

#if defined(_AIX)
#pragma implementation ("ImageSummary.cc")
#endif

#include <aips/aips.h>

template <class T> class ImageInterface;
template <class T> class Vector;
class IPosition;
class Unit;
class LogIO;
class DirectionCoordinate;
class SpectralCoordinate;
class StokesCoordinate;
class LinearCoordinate;

// <summary> Provides and lists information about the header of an image </summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=Coordinates>Coordinates</linkto> 
// </prerequisite>
//
// <etymology>
// This class lists the ancilliary or header information from an image
// </etymology>
//
// <synopsis>
// Images consist of pixels and descriptive information stored in what
// is loosely termed the header. This is information describing the
// coordinate system, the image units etc.  This class enables you to
// retrieve the descriptive header information and/or list it.
// </synopsis>
//
// <example>
// <srcBlock>
//     PagedImage<Float> inImage(fileName);
//     ImageSummary<Float> header(inImage);
//     LogOrigin or("myClass", "myFunction(...)", WHERE);
//     LogIO os(or);
//     header.list(os);
// </srcBlock>
// A <src>PagedImage</src> object is constructed and then logged to the 
// supplied <src>LogIO</src> object.
// </example>
//
// <note role=caution>
// Note that if the <src>PagedImage</src> goes out of scope, this
// class will retrieve rubbish as it just maintains a pointer
// to the image.
// </note>
//
// <motivation>
// The viewing of the image header is a basic capability that is
// commonly required.
// </motivation>
//
// <todo asof="1997/02/17">
//  There will be more things to add as ImageInterface becomes richer
// </todo>
 

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

// Retrieve axis names
   Vector<String> axisNames () const;

// Retrieve reference pixels
   Vector<Double> referencePixels () const;

// Retrieve reference values.  These are returned without
// conversion to some other units.  E.g., angles are returned
// in radians.
   Vector<Double> referenceValues() const;

// Retrieve axis increments. These are returned without
// conversion to some other units.  E.g., angles are returned
// in radians.
   Vector<Double> axisIncrements () const;

// Retrieve axis units
   Vector<String> axisUnits() const;

// Retrieve image units
   Unit units () const;
 
// Retrieve image name.  Any prepended path is stripped off.
   String name() const;

// Retrieve whether image has mask or not
   Bool hasAMask () const;

// List all header information.  By default, the reference
// values and pixel increments are converted to a "nice"
// unit before formatting (e.g. RA is shown as HH:MM:SS.S).
// If <src>nativeFormat</src> is <src>True</src> then
// the values are formatted in their native format (e.g.
// RA would be listed in exponential format in radians).
   void list (LogIO& os, Bool nativeFormat=False);

// Set a new image
   Bool setNewImage (const ImageInterface<T>& image);


private:
   const ImageInterface<T>* pImage_p;

// These are format controllers used by the list() function.
// I should probably write these as a little format class
// but I can't be bothered !

   uInt widthName_p;
   uInt widthProj_p;
   uInt widthNPix_p;
   uInt widthTile_p;
   uInt widthRefValue_p;
   uInt widthAxUnits_p;
   Int precRefValueSci_p;
   Int precRefValueFixed_p;
   uInt widthRefPixel_p;
   Int precRefPixel_p;
   uInt widthInc_p;
   Int precInc_p;

// List DirectionCoordinate axis descriptors
   void listDirection (LogIO& os, 
                       const DirectionCoordinate& coord,
                       const Int& axisInCoordinate,
                       const Int& pixelAxis, 
                       const Bool& nativeFormat) const;

// List SpectralCoordinate axis descriptors
   void listSpectral  (LogIO& os, 
                       const SpectralCoordinate& coord,
                       const Int& axisInCoordinate,
                       const Int& pixelAxis) const;

// List LinearCoordinate axis descriptors
   void listLinear    (LogIO& os, 
                       const LinearCoordinate& coord,
                       const Int& axisInCoordinate,
                       const Int& pixelAxis) const;

// List StokesCoordinate axis descriptors
   void listStokes    (LogIO& os, 
                       const StokesCoordinate& coord,
                       const Int& axisInCoordinate,
                       const Int& pixelAxis,
                       const Bool& nativeFormat) const;

// Clear formatting flags
   void clearFlags (LogIO& os) const;

};

#endif

