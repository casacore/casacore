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

// Destructor
  ~ImageSummary();

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

// Retrieve reference values
   Vector<Double> referenceValues() const;

// Retrieve axis increments
   Vector<Double> axisIncrements () const;

// Retrieve axis units
   Vector<String> axisUnits() const;

// Retrieve image units
   Unit units () const;
 
// Retrieve image name
   String name() const;

// Retrieve whether image has mask or not
   Bool hasAMask () const;

// List all header information.
   void list (LogIO& os) const;

// Set a new image
   Bool setNewImage (const ImageInterface<T>& image);


private:
   const ImageInterface<T>* pImage;

// List DirectionCoordinate axis descriptors
   void listDirection (LogIO& os, 
                       const DirectionCoordinate& coord,
                       const uInt& widthName,
                       const uInt& widthProj,
                       const uInt& widthNPix,
                       const uInt& widthTile,
                       const uInt& widthRefValue,
                       const uInt& widthRefPixel,
                       const uInt& widthInc,
                       const uInt& worldAxis, 
                       const Int& axisInCoordinate) const;

// List SpectralCoordinate axis descriptors
   void listSpectral  (LogIO& os, 
                       const SpectralCoordinate& coord,
                       const uInt& widthName,
                       const uInt& widthProj,
                       const uInt& widthNPix,
                       const uInt& widthTile,
                       const uInt& widthRefValue,
                       const uInt& widthRefPixel,
                       const uInt& widthInc,
                       const uInt& worldAxis, 
                       const Int& axisInCoordinate) const;

// List LinearCoordinate axis descriptors
   void listLinear    (LogIO& os, 
                       const LinearCoordinate& coord,
                       const uInt& widthName,
                       const uInt& widthProj,
                       const uInt& widthNPix,
                       const uInt& widthTile,
                       const uInt& widthRefValue,
                       const uInt& widthRefPixel,
                       const uInt& widthInc,
                       const uInt& worldAxis, 
                       const Int& axisInCoordinate) const;

// List StokesCoordinate axis descriptors
   void listStokes    (LogIO& os, 
                       const StokesCoordinate& coord,
                       const uInt& widthName,
                       const uInt& widthProj,
                       const uInt& widthNPix,
                       const uInt& widthTile,
                       const uInt& widthRefValue,
                       const uInt& worldAxis, 
                       const Int& axisInCoordinate) const;

// Clear formatting flags
   void clearFlags (LogIO& os) const;

};

#endif

