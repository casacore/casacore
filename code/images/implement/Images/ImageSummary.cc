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

template <class T> 
ImageSummary<T>::ImageSummary (const ImageInterface<T>& image)
//
// Constructor assigns pointer.  If ImageInterface goes out of scope you
// will get rubbish.
//
{
   pImage_p = &image;
}

template <class T> 
ImageSummary<T>::ImageSummary (const ImageSummary<T> &other)
                   : widthName_p(other.widthName_p),
                     widthProj_p(other.widthProj_p),
                     widthNPix_p(other.widthNPix_p),
                     widthTile_p(other.widthTile_p),
                     widthRefValue_p(other.widthRefValue_p),
                     widthAxUnits_p(other.widthAxUnits_p),
                     precRefValueSci_p(other.precRefValueSci_p),
                     precRefValueFixed_p(other.precRefValueFixed_p),
                     widthRefPixel_p(other.widthRefPixel_p),
                     precRefPixel_p(other.precRefPixel_p),
                     widthInc_p(other.widthInc_p),
                     precInc_p(other.precInc_p)

//
// Copy constructor
//
{
   pImage_p = other.pImage_p;
}

template <class T> 
ImageSummary<T>::~ImageSummary ()
//
// Destructor does nothing
//
{}

template <class T>
ImageSummary<T> &ImageSummary<T>::operator=(const ImageSummary<T> &other)
// 
// Assignment operator
//
{
   pImage_p = other.pImage_p;
   widthName_p = other.widthName_p;
   widthProj_p = other.widthProj_p;
   widthNPix_p = other.widthNPix_p;
   widthTile_p = other.widthTile_p;
   widthRefValue_p = other.widthRefValue_p;
   widthAxUnits_p = other.widthAxUnits_p;
   precRefValueSci_p = other.precRefValueSci_p;
   precRefValueFixed_p = other.precRefValueFixed_p;
   widthRefPixel_p = other.widthRefPixel_p;
   precRefPixel_p = other.precRefPixel_p;
   widthInc_p = other.widthInc_p;
   precInc_p = other.precInc_p;
}

template <class T> 
Int ImageSummary<T>::ndim () const
//
// Retrieve number of image dimension 
//
{
   return pImage_p->ndim();
}


template <class T> 
IPosition ImageSummary<T>::shape () const
//
// Get image shape
//
{
   return pImage_p->shape();
}

template <class T> 
IPosition ImageSummary<T>::tileShape () const
//
// Get image tile shape
//
{
   return pImage_p->niceCursorShape(pImage_p->maxPixels());
}



typedef Vector<String> gpp_VS;
template <class T> 
gpp_VS ImageSummary<T>::axisNames () const
// 
// Get axis names for the pixel axes
//
{
//   return pImage_p->coordinates().worldAxisNames();

   CoordinateSystem cSys = pImage_p->coordinates();
   Int coordinate, axisInCoordinate;
   Vector<String> names(cSys.nPixelAxes());

   for (Int pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
      Int worldAxis = cSys.worldAxes(coordinate)(axisInCoordinate);
      names(pixelAxis) = cSys.worldAxisNames()(worldAxis);
   }
   return names;
}




template <class T> 
Vector<Double> ImageSummary<T>::referencePixels () const
// 
// Get reference pixels for the pixel axes
//
{
//   return pImage_p->coordinates().referencePixel().ac() + 1.0;

   CoordinateSystem cSys = pImage_p->coordinates();
   Int coordinate, axisInCoordinate;
   Vector<Double> refPix(cSys.nPixelAxes());
 
   for (Int pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
      refPix(pixelAxis) = cSys.referencePixel()(pixelAxis) + 1.0;
   }
   return refPix;
}


template <class T> 
Vector<Double> ImageSummary<T>::referenceValues () const
// 
// Get reference values for the pixel axes
//
{
//   return pImage_p->coordinates().referenceValue().ac();

   CoordinateSystem cSys = pImage_p->coordinates();
   Int coordinate, axisInCoordinate;
   Vector<Double> refVals(cSys.nPixelAxes());
 
   for (Int pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
      Int worldAxis = cSys.worldAxes(coordinate)(axisInCoordinate);
      refVals(pixelAxis) = cSys.referenceValue()(worldAxis);
   }
   return refVals;
}


template <class T> 
Vector<Double> ImageSummary<T>::axisIncrements () const
// 
// Get axis increments for the pixel axes
//
{
//   return pImage_p->coordinates().increment().ac();


   CoordinateSystem cSys = pImage_p->coordinates();
   Int coordinate, axisInCoordinate;
   Vector<Double> incs(cSys.nPixelAxes());
 
   for (Int pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
      Int worldAxis = cSys.worldAxes(coordinate)(axisInCoordinate);
      incs(pixelAxis) = cSys.increment()(worldAxis);
   }
   return incs;
}

template <class T> 
Vector<String> ImageSummary<T>::axisUnits () const
// 
// Get axis units for the pixel axes
//
{
//   return pImage_p->coordinates().worldAxisUnits().ac();

   CoordinateSystem cSys = pImage_p->coordinates();
   Int coordinate, axisInCoordinate;
 
   Vector<String> units(cSys.nPixelAxes());
 
   for (Int pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
      Int worldAxis = cSys.worldAxes(coordinate)(axisInCoordinate);
      units(pixelAxis) = cSys.worldAxisNames()(worldAxis);
   }
   return units;
}



template <class T> 
Unit ImageSummary<T>::units () const
//
// Get image units
//
{
   return pImage_p->units();
}


template <class T> 
String ImageSummary<T>::name () const
//
// Get image name
//
{
   const Bool stripPath = True;
   return pImage_p->name(stripPath);
}


template <class T> 
Bool ImageSummary<T>::hasAMask () const
//
// See if image has a mask
//
{
   return pImage_p->isMasked();
}


template <class T> 
void ImageSummary<T>::list (LogIO& os,
                            Bool nativeFormat) 
//
// List information about an image to the logger
//
// Input:
//   nativeFormat  If True, reference values and axis increments
//                 are formatted in their native format.  E.g.
//                 RA and DEC in radians.  Otherwise, they are
//                 possibly converted to some other unit and
//                 formatted nicely (e.g. HH:MM:SS.S)
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

   CoordinateSystem cSys = pImage_p->coordinates();

// Maximum width of names fields 

   widthName_p = 0;
   for (uInt i=0; i<cSys.worldAxisNames().nelements(); i++) {
     widthName_p = max(widthName_p,cSys.worldAxisNames()(i).length());
   }


// Set up headers

   os.output().fill(' ');
   os.output().setf(ios::left, ios::adjustfield);

   os.output().width(widthName_p);
   os << "Type";
   widthName_p = max(String("Type").length(), widthName_p);

   os.output().setf(ios::right, ios::adjustfield);
   widthProj_p = 5;
   os.output().width(widthProj_p);
   os << "Proj";

   widthNPix_p = 7;
   os.output().width(widthNPix_p);
   os << "Pixels";

   widthTile_p = 5;
   os.output().width(widthTile_p);
   os << "Tile";

   widthRefValue_p = 14;
   precRefValueSci_p = 6;
   precRefValueFixed_p = 3;
   os.output().width(widthRefValue_p);
   os << "Coord Value";

   widthRefPixel_p = 9;
   precRefPixel_p = 2;
   os.output().width(widthRefPixel_p);
   os << "at Pixel";

   widthInc_p = 14;
   precInc_p = 6;
   os.output().width(widthInc_p);
   os << "Coord Incr";

   os << " Units" << endl;

   widthAxUnits_p = 8;
   uInt totWidth = widthName_p + widthProj_p + widthNPix_p + widthTile_p + widthRefValue_p +
                   widthRefPixel_p + widthInc_p + widthAxUnits_p;
   os.output().fill('-');
   os.output().width(totWidth);
   os.output().setf(ios::right, ios::adjustfield);
   os << " " << endl;
   os.output() << setfill(' ');


// Loop over the number of pixel axes in the coordinate system (same
// as number of axes in image) 

   Int pixelAxis;
   Int coordinate, axisInCoordinate;
   for (pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {


// Find coordinate number for this pixel axis
 
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);


// List according to coordinate type

      if (cSys.type(coordinate) == Coordinate::DIRECTION) { 
        const DirectionCoordinate dCoord = cSys.directionCoordinate(coordinate);
        listDirection (os, dCoord, axisInCoordinate, pixelAxis, 
                       nativeFormat);
      } else if (cSys.type(coordinate) == Coordinate::SPECTRAL) { 
        const SpectralCoordinate sCoord = cSys.spectralCoordinate(coordinate);
        listSpectral (os, sCoord, axisInCoordinate, pixelAxis);
      } else if (cSys.type(coordinate) == Coordinate::LINEAR) { 
        const LinearCoordinate lCoord = cSys.linearCoordinate(coordinate);
        listLinear (os, lCoord, axisInCoordinate, pixelAxis);
      } else if (cSys.type(coordinate) == Coordinate::STOKES) { 
        const StokesCoordinate sCoord = cSys.stokesCoordinate(coordinate);
        listStokes (os, sCoord, axisInCoordinate, pixelAxis,
                    nativeFormat);
      }
   }
   os << endl;


// Now find those pixel axes that have been removed and list their
// associated coordinate information.

   Int worldAxis;
   for (worldAxis=0; worldAxis<cSys.nWorldAxes(); worldAxis++) {


// Find coordinate number for this pixel axis
 
      cSys.findWorldAxis(coordinate, axisInCoordinate, worldAxis);


// See if this world axis has an associated removed pixel axis
      
      Vector<Int> pixelAxes = cSys.pixelAxes(coordinate);
      if (pixelAxes(axisInCoordinate) == -1) {


// List according to coordinate type

        if (cSys.type(coordinate) == Coordinate::DIRECTION) { 
          const DirectionCoordinate dCoord = cSys.directionCoordinate(coordinate);
          listDirection (os, dCoord, axisInCoordinate, -1, nativeFormat);
        } else if (cSys.type(coordinate) == Coordinate::SPECTRAL) { 
          const SpectralCoordinate sCoord = cSys.spectralCoordinate(coordinate);
          listSpectral (os, sCoord, axisInCoordinate, -1);
        } else if (cSys.type(coordinate) == Coordinate::LINEAR) { 
          const LinearCoordinate lCoord = cSys.linearCoordinate(coordinate);
          listLinear (os, lCoord, axisInCoordinate, -1);
        } else if (cSys.type(coordinate) == Coordinate::STOKES) { 
          const StokesCoordinate sCoord = cSys.stokesCoordinate(coordinate);
          listStokes (os, sCoord, axisInCoordinate, -1, nativeFormat);
        }
     }
   }
 

// Post it

   os.post();

}


template <class T> 
Bool ImageSummary<T>::setNewImage (const ImageInterface<T>& image)
//
// Reassign pointer.  
//
{
   const ImageInterface<T>* pTemp;
   pTemp = &image;
   if (pTemp == 0) {
      return False;
   } else {
      pImage_p = pTemp;
      return True;
   }
}



template <class T> 
void ImageSummary<T>::listDirection (LogIO& os, 
                                     DirectionCoordinate& coord,
                                     const Int& axisInCoordinate,
                                     const Int& pixelAxis,
                                     const Bool& nativeFormat) const
//
// Output the descriptors for a DIRECTION axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The direction coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//     nativeFormat     If true don't convert any units
//           
{

// Clear flags

   clearFlags(os);

// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName_p);
   os << coord.worldAxisNames()(axisInCoordinate);

// Projection

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj_p);
   os << coord.projection().name();

// Number of pixels
   
   os.output().width(widthNPix_p);
   if (pixelAxis != -1) {
      os << this->shape()(pixelAxis);
   } else {
      os << " ";
   }

// Tile shape

   os.output().width(widthTile_p);
   if (pixelAxis != -1) {
      os << this->tileShape()(pixelAxis);
   } else {
      os << " ";
   }


// Remember units

   Vector<String> oldUnits(2);
   oldUnits = coord.worldAxisUnits();
   Vector<String> units(2);


// Reference value

   String tString = coord.worldAxisNames()(axisInCoordinate);
   tString.upcase();
   os.output().width(widthRefValue_p);
   if (nativeFormat) {
      os.output().setf(ios::scientific, ios::floatfield);
      os.output().precision(precRefValueSci_p);
      os << coord.referenceValue()(axisInCoordinate);
   } else {

// Convert to radians as MVAngle wants radians

      units = "rad";
      coord.setWorldAxisUnits(units);

      MVAngle mVA(coord.referenceValue()(axisInCoordinate));
      if (tString.contains("RIGHT ASCENSION")) {
         os << mVA.string(MVAngle::TIME,8);
      } else if (tString.contains("DECLINATION")) {
         os << mVA.string(MVAngle::DIG2,8);
      } else {
         os.output().setf(ios::fixed, ios::floatfield);
         os.output().precision(precRefValueFixed_p);
         os << mVA.degree();
      }
   }

// Reference pixel

   os.output().setf(ios::fixed, ios::floatfield);
   os.output().width(widthRefPixel_p);
   os.output().precision(precRefPixel_p);
   if (pixelAxis != -1) {
      os << coord.referencePixel()(axisInCoordinate) + 1.0;
   } else {
      os << " ";
   }


// Increment

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthInc_p);
   os.output().precision(precInc_p);
   if (pixelAxis != -1) {
      if (nativeFormat) {
         os << coord.increment()(axisInCoordinate);
      } else { 
         if (tString.contains("RIGHT ASCENSION") ||   
             tString.contains("DECLINATION")) {
            units = "''";
            coord.setWorldAxisUnits(units);
            os << coord.increment()(axisInCoordinate);
         } else {
            os << coord.increment()(axisInCoordinate);
         }
      }
   } else {
      os << " ";
   }



// Increment units

   os.output().setf(ios::left, ios::adjustfield);
   if (pixelAxis != -1) {
      os << " " << coord.worldAxisUnits()(axisInCoordinate);
   }

   os << endl;    


// Put the units back to what they were

   coord.setWorldAxisUnits(oldUnits);

}


template <class T> 
void ImageSummary<T>::listSpectral (LogIO& os, 
                                    const SpectralCoordinate& coord,
                                    const Int& axisInCoordinate,
                                    const Int& pixelAxis) const
//
// Output the descriptors for a SPECTRAL axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The spectral coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//           
{

// Clear flags

   clearFlags(os);

// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName_p);
   os << coord.worldAxisNames()(axisInCoordinate);

// Projection (none)

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj_p);
   os << " ";

// Number of pixels
   
   os.output().width(widthNPix_p);
   if (pixelAxis != -1) {
      os << this->shape()(pixelAxis);
   } else {
      os << " ";
   }

// Tile shape

   os.output().width(widthTile_p);
   if (pixelAxis != -1) {
      os << this->tileShape()(pixelAxis);
   } else {
      os << " ";
   }


// Reference value

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthRefValue_p);
   os.output().precision(precRefValueSci_p);
   os << coord.referenceValue()(axisInCoordinate);

// Reference pixel

   os.output().setf(ios::fixed, ios::floatfield);
   os.output().width(widthRefPixel_p);
   os.output().precision(precRefPixel_p);
   if (pixelAxis != -1) {
      os << coord.referencePixel()(axisInCoordinate) + 1.0;
   } else {
      os << " ";
   }
      

// Increment

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthInc_p);
   os.output().precision(precInc_p);
   if (pixelAxis != -1) {
      os << coord.increment()(axisInCoordinate);
   } else {
      os << " ";
   }


// Increment units
  
   os.output().setf(ios::left, ios::adjustfield);
   if (pixelAxis != -1) {
      os << " " << coord.worldAxisUnits()(axisInCoordinate);
   }


   os << endl; 
}


template <class T> 
void ImageSummary<T>::listLinear (LogIO& os, 
                                  const LinearCoordinate& coord,
                                  const Int& axisInCoordinate,
                                  const Int& pixelAxis) const
//
// Output the descriptors for a LINEAR axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The linear coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//           
{

// Clear flags

   clearFlags(os);

// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName_p);
   os << coord.worldAxisNames()(axisInCoordinate);

// Projection (none)

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj_p);
   os << " ";


// Number of pixels
   
   os.output().width(widthNPix_p);
   if (pixelAxis != -1) {
      os << this->shape()(pixelAxis);
   } else {
      os << " ";
   }

// Tile shape

   os.output().width(widthTile_p);
   if (pixelAxis != -1) {
      os << this->tileShape()(pixelAxis);
   } else {
      os << " ";
   }


// Reference value

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthRefValue_p);
   os.output().precision(precRefValueSci_p);
   os << coord.referenceValue()(axisInCoordinate);

// Reference pixel

   os.output().setf(ios::fixed, ios::floatfield);
   os.output().width(widthRefPixel_p);
   os.output().precision(precRefPixel_p);
   if (pixelAxis != -1) {
      os << coord.referencePixel()(axisInCoordinate) + 1.0;
   } else {
      os << " ";
   }


// Increment

   os.output().setf(ios::scientific, ios::floatfield);
   os.output().width(widthInc_p);
   os.output().precision(precInc_p);
   if (pixelAxis != -1) {
      os << coord.increment()(axisInCoordinate);
   } else {
      os << " ";
   }


// Increment units
  
   os.output().setf(ios::left, ios::adjustfield);
   if (pixelAxis != -1) {
      os << " " << coord.worldAxisUnits()(axisInCoordinate);
   }

   os << endl; 
}




template <class T> 
void ImageSummary<T>::listStokes (LogIO& os, 
                                  const StokesCoordinate& coord,
                                  const Int& axisInCoordinate,
                                  const Int& pixelAxis,
                                  const Bool& nativeFormat) const
//
// Output the descriptors for a STOKES axis
//
//  Input:
//     os               The LogIO to write to
//     coord            The stokes coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//     nativeFormat     If true don't convert any units
//
{

// Clear flags

   clearFlags(os);

// Axis name

   os.output().setf(ios::left, ios::adjustfield);
   os.output().width(widthName_p);
   os << coord.worldAxisNames()(axisInCoordinate);

// Projection (none)

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj_p);
   os << " ";

// Number of pixels
   
   os.output().width(widthNPix_p);
   if (pixelAxis != -1) {
      os << this->shape()(pixelAxis);
   } else {
      os << " ";
   }

// Tile shape

   os.output().width(widthTile_p);
   if (pixelAxis != -1) {
      os << this->tileShape()(pixelAxis);
   } else {
      os << " ";
   }


// Reference value, reference pixel, increment

   if (nativeFormat) {
      os.output().setf(ios::scientific, ios::floatfield);
      os.output().width(widthRefValue_p);
      os.output().precision(precRefValueFixed_p);
      os << coord.referenceValue()(axisInCoordinate);

      os.output().setf(ios::fixed, ios::floatfield);
      os.output().width(widthRefPixel_p);
      os.output().precision(precRefPixel_p);
      if (pixelAxis != -1) {
         os << coord.referencePixel()(axisInCoordinate) + 1.0;
      } else {
         os << " ";
      }

      os.output().setf(ios::scientific, ios::floatfield);
      os.output().width(widthInc_p);
      os.output().precision(precInc_p);
      if (pixelAxis != -1) {
         os << coord.increment()(axisInCoordinate);
      } else {
         os << " ";
      }

      os.output().setf(ios::left, ios::adjustfield);
      if (pixelAxis != -1) {
         os << " " << coord.worldAxisUnits()(axisInCoordinate);
      } else {
         os << " ";
      }
   } else {

// We write here the names of each pixel on the Stokes axes such as
// IQUV or XXYYXYYX etc   It is possible that this will run into trouble
// if for some reason the Stokes axis is very funny and the names overflow
// the width of the field "widthRefValue_p".  This is pretty remote
// except in test cases.

      os.output().width(widthRefValue_p);
      if (pixelAxis != -1) {
         String sName;
         for (Int i=0; i<this->shape()(pixelAxis); i++) {
            Stokes::StokesTypes iStokes;
            Bool ok = coord.toWorld(iStokes, i);

// SOme protections against funny axes

            String temp;
            if (ok) {
               temp = Stokes::name(Stokes::type(iStokes));
            } else {
               temp = "?";
            }
            if (sName.length()+temp.length() < widthRefValue_p) sName += temp;
         }
         os << sName;
      } else {
         String sName;
         Stokes::StokesTypes iStokes;
         Int i = Int(coord.referencePixel()(axisInCoordinate));
         Bool ok = coord.toWorld(iStokes, i);

         if (ok) {
            sName = Stokes::name(Stokes::type(iStokes));
         } else {
            sName = "?";
         }
         os << sName;
      }
   }


   os << endl; 

}


template <class T> 
void ImageSummary<T>::clearFlags(LogIO& os) const
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




