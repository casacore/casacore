//# ImageSummary.cc:  Helper class for applications listing an image header
//# Copyright (C) 1995,1996,1997
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
#include <aips/aips.h>
#include <aips/Arrays.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Measures/Unit.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/Stokes.h>

#include <trial/Coordinates.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageSummary.h>

#include <iomanip.h>
#include <iostream.h>

template <class T> ImageSummary<T>::ImageSummary (const ImageInterface<T>& image)
//
// Constructor assigns pointer.  If ImageInterface goes out of scope you
// will get rubbish.
//
{
   pImage = &image;
}

template <class T> ImageSummary<T>::~ImageSummary ()
//
// Destructor does nothing
//
{}

template <class T> Int ImageSummary<T>::ndim () const
//
// Retrieve number of image dimension 
//
{
   return pImage->ndim();
}


template <class T> IPosition ImageSummary<T>::shape () const
//
// Get image shape
//
{
   return pImage->shape();
}

template <class T> IPosition ImageSummary<T>::tileShape () const
//
// Get image tile shape
//
{
   return pImage->niceCursorShape(pImage->maxPixels());
}

typedef Vector<String> gpp_VS;
template <class T> gpp_VS ImageSummary<T>::axisNames () const
// 
// Get axis names
//
{
   return pImage->coordinates().worldAxisNames();
}


template <class T> Vector<Double> ImageSummary<T>::referencePixels () const
// 
// Get reference pixels
//
{
   return pImage->coordinates().referencePixel().ac() + 1.0;
}


template <class T> Vector<Double> ImageSummary<T>::referenceValues () const
// 
// Get reference values
//
{
   return pImage->coordinates().referenceValue().ac();
}


template <class T> Vector<Double> ImageSummary<T>::axisIncrements () const
// 
// Get axis increments
//
{
   return pImage->coordinates().increment().ac();
}

template <class T> Vector<String> ImageSummary<T>::axisUnits () const
// 
// Get axis units
//
{
   return pImage->coordinates().worldAxisUnits().ac();
}


template <class T> Unit ImageSummary<T>::units () const
//
// Get image units
//
{
   return pImage->units();
}


template <class T> String ImageSummary<T>::name () const
//
// Get image name
//
{
   return pImage->name();
}


template <class T> Bool ImageSummary<T>::hasAMask () const
//
// See if image has a mask
//
{
   return pImage->isMasked();
}


template <class T> void ImageSummary<T>::list (LogIO& os) const
//
// List information about an image to the logger
//
{

   os << LogIO::NORMAL << endl;

// List random things

   os << "Image name  : " << this->name() << endl;
   if (this->hasAMask()) {
      os << "Image mask  : Present" << endl;
   } else {
      os << "Image mask  : Absent" << endl;
   }
   if (!this->units().getName().empty()) 
      os << "Image units : " << this->units().getName() << endl;
   os << endl;


// Obtain CoordinateSystem

   CoordinateSystem cSys = pImage->coordinates();
   Vector<String> names = cSys.worldAxisNames();


// Maximum width of names fields

   uInt widthName = 0;
   for (uInt i=0; i<names.nelements(); i++) {
     widthName = max(widthName,cSys.worldAxisNames()(i).length());
   }

// Set up headers

   os.output().fill(' ');
   os.output().setf(ios::left, ios::adjustfield);

   os.output().width(widthName);
   os << "Type";

   os.output().setf(ios::right, ios::adjustfield);
   uInt widthProj = 5;
   os.output().width(widthProj);
   os << "Proj";

   uInt widthNPix = 7;
   os.output().width(widthNPix);
   os << "Pixels";

   uInt widthTile = 5;
   os.output().width(widthTile);
   os << "Tile";

   uInt widthRefValue = 14;
   os.output().width(widthRefValue);
   os << "Coord Value";

   uInt widthRefPixel = 9;
   os.output().width(widthRefPixel);
   os << "at Pixel";

   uInt widthInc = 13;
   os.output().width(widthInc);
   os << "Coord Incr";

   os << " Units" << endl;

   uInt widthAxUnits = 8;
   uInt totWidth = widthName + widthProj + widthNPix + widthTile + widthRefValue +
                   widthRefPixel + widthInc + widthAxUnits;
   os.output().fill('-');
   os.output().width(totWidth);
   os.output().setf(ios::right, ios::adjustfield);
   os << " " << endl;
   os.output() << setfill(' ');

// Loop over the number of world axes in the coordinate system (this better 
// correspond to the number of axes in the image) and find out what the
// coordinate number for that axis is, then find out its type and what the 
// axis in that coordinate is and list all the good stuff.

   uInt worldAxis;
   Int coordinate, axisInCoordinate;
   for (worldAxis=0; worldAxis<cSys.nWorldAxes(); worldAxis++) {
 
      cSys.findWorldAxis(coordinate, axisInCoordinate, worldAxis);

      if (cSys.type(coordinate) == Coordinate::DIRECTION) { 
        const DirectionCoordinate dCoord = cSys.directionCoordinate(coordinate);
        listDirection (os, dCoord, widthName, widthProj, widthNPix,
                       widthTile, widthRefValue, widthRefPixel, 
                       widthInc, worldAxis, axisInCoordinate);
     } else if (cSys.type(coordinate) == Coordinate::SPECTRAL) { 
        const SpectralCoordinate sCoord = cSys.spectralCoordinate(coordinate);
        listSpectral (os, sCoord, widthName, widthProj, widthNPix,
                      widthTile, widthRefValue, widthRefPixel, 
                      widthInc, worldAxis, axisInCoordinate);
     } else if (cSys.type(coordinate) == Coordinate::LINEAR) { 
        const LinearCoordinate lCoord = cSys.linearCoordinate(coordinate);
        listLinear (os, lCoord, widthName, widthProj, widthNPix,
                    widthTile, widthRefValue, widthRefPixel, 
                    widthInc, worldAxis, axisInCoordinate);
     } else if (cSys.type(coordinate) == Coordinate::STOKES) { 
        const StokesCoordinate sCoord = cSys.stokesCoordinate(coordinate);
        listStokes (os, sCoord, widthName, widthProj, widthNPix, 
                    widthTile, widthRefValue, worldAxis, axisInCoordinate);
     }
   }
 

// Post it

   os.post();

}


template <class T> Bool ImageSummary<T>::setNewImage (const ImageInterface<T>& image)
//
// Reassign pointer.  
//
{
   const ImageInterface<T>* pTemp;
   pTemp = &image;
   if (pTemp == 0) {
      return False;
   } else {
      pImage = pTemp;
      return True;
   }
}



template <class T> void ImageSummary<T>::listDirection (LogIO& os, 
                                                        const DirectionCoordinate& coord,
                                                        const uInt& widthName,
                                                        const uInt& widthProj, 
                                                        const uInt& widthNPix, 
                                                        const uInt& widthTile,
                                                        const uInt& widthRefValue, 
                                                        const uInt& widthRefPixel, 
                                                        const uInt& widthInc, 
                                                        const uInt& worldAxis,
                                                        const Int& axisInCoordinate) const
//
// Output the descriptors for a DIRECTION axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The direction coordinate
//     width*           The width of the fields
//     worldAxis        The axis in the image (increments for every axis in each coordinate)
//     axisIncoordinate The axis number in this coordinate for this worldAxis and coordinate
//           
{

// Clear flags

   clearFlags(os);


// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName);
   os << coord.worldAxisNames()(axisInCoordinate);

// Projection

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj);
   os << coord.projection().name();

// Number of pixels


   os.output().width(widthNPix);
   os << this->shape()(worldAxis);

// Tile shape

   os.output().width(widthTile);
   os << this->tileShape()(worldAxis);


// Reference value

   MVAngle mVA(coord.referenceValue()(axisInCoordinate));
   String tString = coord.worldAxisNames()(axisInCoordinate);
   tString.upcase();
   if (tString.contains("RIGHT ASCENSION")) {
      os.output().width(widthRefValue);
      os << mVA.string(MVAngle::TIME,8);
   } else if (tString.contains("DECLINATION")) {
      os.output().width(widthRefValue);
      os << mVA.string(MVAngle::DIG2,8);
   } else {
      os.output().setf(ios::fixed, ios::floatfield);
      os.output().width(widthRefValue);
      os.output().precision(3);
      os << mVA.degree();
   }

// Reference pixel

   os.output().setf(ios::fixed, ios::floatfield);
   os.output().width(widthRefPixel);
   os.output().precision(2);
   os << coord.referencePixel()(axisInCoordinate) + 1.0;

// Increment

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthInc);
   os.output().precision(5);
   if (tString.contains("RIGHT ASCENSION") ||   
       tString.contains("DECLINATION")) {
      os << coord.increment()(axisInCoordinate) * 3600.0 * 180.0 / C::pi;
   } else {
      os << coord.increment()(axisInCoordinate) * 180.0 / C::pi;
   }

// Increment units
  
   os.output().setf(ios::left, ios::adjustfield);
   if (tString.contains("RIGHT ASCENSION") ||
       tString.contains("DECLINATION")) {
      os << " arcsec";
   } else {
      os << " degrees";
   }

   os << endl;    

}



template <class T> void ImageSummary<T>::listSpectral (LogIO& os, 
                                                       const SpectralCoordinate& coord,
                                                       const uInt& widthName,
                                                       const uInt& widthProj, 
                                                       const uInt& widthNPix, 
                                                       const uInt& widthTile,
                                                       const uInt& widthRefValue, 
                                                       const uInt& widthRefPixel, 
                                                       const uInt& widthInc, 
                                                       const uInt& worldAxis,
                                                       const Int& axisInCoordinate) const
//
// Output the descriptors for a SPECTRAL axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The spectral coordinate
//     width*           The width of the fields
//     worldAxis        The axis in the image (increments for every axis in each coordinate)
//     axisIncoordinate The axis number in this coordinate for this worldAxis and coordinate
//           
{

// Clear flags

   clearFlags(os);

// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName);
   os << coord.worldAxisNames()(axisInCoordinate);

// Projection (none)

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj);
   os << " ";

// Number of pixels

   os.output().width(widthNPix);
   os << this->shape()(worldAxis);

// Tile shape

   os.output().width(widthTile);
   os << this->tileShape()(worldAxis);

// Reference value

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthRefValue);
   os.output().precision(6);
   os << coord.referenceValue()(axisInCoordinate);

// Reference pixel

   os.output().setf(ios::fixed, ios::floatfield);
   os.output().width(widthRefPixel);
   os.output().precision(2);
   os << coord.referencePixel()(axisInCoordinate) + 1.0;

// Increment

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthInc);
   os.output().precision(5);
   os << coord.increment()(axisInCoordinate);

// Increment units
  
   os.output().setf(ios::left, ios::adjustfield);
   os << " " << coord.worldAxisUnits()(axisInCoordinate);

   os << endl; 
}


template <class T> void ImageSummary<T>::listLinear (LogIO& os, 
                                                     const LinearCoordinate& coord,
                                                     const uInt& widthName,
                                                     const uInt& widthProj, 
                                                     const uInt& widthNPix, 
                                                     const uInt& widthTile,
                                                     const uInt& widthRefValue, 
                                                     const uInt& widthRefPixel, 
                                                     const uInt& widthInc, 
                                                     const uInt& worldAxis,
                                                     const Int& axisInCoordinate) const
//
// Output the descriptors for a LINEAR axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The linear coordinate
//     width*           The width of the fields
//     worldAxis        The axis in the image (increments for every axis in each coordinate)
//     axisIncoordinate The axis number in this coordinate for this worldAxis and coordinate
//           
{

// Clear flags

   clearFlags(os);


// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName);
   os << coord.worldAxisNames()(axisInCoordinate);


// Projection (none)

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj);
   os << " ";

// Number of pixels

   os.output().width(widthNPix);
   os << this->shape()(worldAxis);

// Tile shape

   os.output().width(widthTile);
   os << this->tileShape()(worldAxis);

// Reference value

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthRefValue);
   os.output().precision(6);
   os << coord.referenceValue()(axisInCoordinate);

// Reference pixel

   os.output().setf(ios::fixed, ios::floatfield);
   os.output().width(widthRefPixel);
   os.output().precision(2);
   os << coord.referencePixel()(axisInCoordinate) + 1.0;

// Increment

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthInc);
   os.output().precision(5);
   os << coord.increment()(axisInCoordinate);

// Increment units
  
   os.output().setf(ios::left, ios::adjustfield);
   os << " " << coord.worldAxisUnits()(axisInCoordinate);

   os << endl; 
}




template <class T> void ImageSummary<T>::listStokes (LogIO& os, 
                                                     const StokesCoordinate& coord,
                                                     const uInt& widthName,
                                                     const uInt& widthProj,
                                                     const uInt& widthNPix, 
                                                     const uInt& widthTile,
                                                     const uInt& widthRefValue, 
                                                     const uInt& worldAxis,
                                                     const Int& axisInCoordinate) const
//
// Output the descriptors for a STOKES axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The stokes coordinate
//     width*           The width of the fields
//     worldAxis        The axis in the image (increments for every axis in each coordinate)
//     axisIncoordinate The axis number in this coordinate for this worldAxis and coordinate
//
{

// Clear flags

   clearFlags(os);

// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName);
   os << coord.worldAxisNames()(axisInCoordinate);


// Projection (none)

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj);
   os << " ";

// Number of pixels

   os.output().width(widthNPix);
   os << this->shape()(worldAxis);

// Tile shape

   os.output().width(widthTile);
   os << this->tileShape()(worldAxis);

// Reference value

   os.output().width(widthRefValue);
   os.output().precision(6);

   String sName;
   for (Int i=0; i<this->shape()(worldAxis); i++) {
      Stokes::StokesTypes iStokes;
      Int pixel;
      Bool ok = coord.toWorld(iStokes, i);
      sName += Stokes::name(Stokes::type(iStokes));
   }
   os << sName;

   os << endl; 
}


template <class T> void ImageSummary<T>::clearFlags(LogIO& os) const
//
// Clear all the formatting flags
//
{
   os.output().unsetf(ios::left);
   os.output().unsetf(ios::right);
   os.output().unsetf(ios::internal);

   os.output().unsetf(ios::dec);
   os.output().unsetf(ios::oct);
   os.output().unsetf(ios::hex);

   os.output().unsetf(ios::showbase | ios::showpos | ios::uppercase | ios::showpoint);

   os.output().unsetf(ios::scientific);
   os.output().unsetf(ios::fixed);

}




