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
//# $Id$
//
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
   if (this != &other) pImage_p = other.pImage_p;
   return *this;
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


// Determine the widths for all the fields that we want to list

   uInt widthName, widthProj, widthShape, widthTile, widthRefValue;
   uInt widthRefPixel, widthInc, widthUnits, totWidth;

   String nameName, nameProj, nameShape, nameTile, nameRefValue;
   String nameRefPixel, nameInc, nameUnits;
   
   Int precRefValSci, precRefValFloat, precRefValRADEC;
   Int precRefPixFloat, precIncSci;

   getFieldWidths (widthName, widthProj, widthShape, widthTile,
                   widthRefValue, widthRefPixel, widthInc,
                   widthUnits, totWidth, precRefValSci, 
                   precRefValFloat, precRefValRADEC, precRefPixFloat,
                   precIncSci, nameName, nameProj, nameShape, 
                   nameTile, nameRefValue, nameRefPixel, nameInc, 
                   nameUnits, nativeFormat, cSys);


// Write headers

   os.output().fill(' ');
   os.output().setf(ios::left, ios::adjustfield);

   os.output().width(widthName);
   os << nameName;

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj);
   os << nameProj;

   os.output().width(widthShape);
   os << nameShape;

   os.output().width(widthTile);
   os << nameTile;

   os.output().width(widthRefValue);
   os << nameRefValue;

   os.output().width(widthRefPixel);
   os << nameRefPixel;

   os.output().width(widthInc);
   os << nameInc;

   os << nameUnits << endl;

   totWidth = widthName + widthProj + widthShape + widthTile +
              widthRefValue + widthRefPixel + widthInc + widthUnits;
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

// List it

      Coordinate* pc = cSys.coordinate(coordinate).clone();
      listHeader(os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 False, axisInCoordinate, pixelAxis, nativeFormat,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci);
      delete pc;
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

// List it

        Coordinate* pc = cSys.coordinate(coordinate).clone();
        listHeader(os, pc, widthName, widthProj, widthShape, 
                   widthTile, widthRefValue, widthRefPixel, 
                   widthInc, widthUnits, False, axisInCoordinate, 
                   -1, nativeFormat, precRefValSci, precRefValFloat, 
                   precRefValRADEC, precRefPixFloat, precIncSci);
        delete pc;
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
void ImageSummary<T>::getFieldWidths (uInt& widthName, 
                                      uInt& widthProj, 
                                      uInt& widthShape,
                                      uInt& widthTile,
                                      uInt& widthRefValue, 
                                      uInt& widthRefPixel, 
                                      uInt& widthInc,
                                      uInt& widthUnits, 
                                      uInt& totWidth, 
                                      Int& precRefValSci, 
                                      Int& precRefValFloat, 
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
                                      const CoordinateSystem& cSys) const
//
// ALl these silly format and precision things shoudl really be
// in  a little class, but I can't be bothered !
{

// Precision for scientific notation, floating notation,
// HH:MM:SS.SSS and sDD:MM:SS.SSS for the reference value formatting.
// Precision for the reference pixel and increment formatting.

   precRefValSci = 6;
   precRefValFloat = 3;
   precRefValRADEC = 3;
   precRefPixFloat = 2;
   precIncSci = 6;   


// Header names for fields

   nameName = "Name";
   nameProj = "Proj";
   nameShape = "Shape";
   nameTile = "Tile";
   nameRefValue = "Coord value";
   nameRefPixel = "at pixel";
   nameInc = "Coord incr";
   nameUnits = " Units";

// Initialize (logger will never be used)

   widthName = widthProj = widthShape = widthTile = widthRefValue = 0;
   widthRefPixel = widthInc = widthUnits = 0;
   LogIO os(LogOrigin("ImageSummary", "getFieldWidths()", WHERE));

// Loop over number of pixel axes

   Int pixelAxis;
   Int coordinate, axisInCoordinate;
   for (pixelAxis=0; pixelAxis<cSys.nPixelAxes(); pixelAxis++) {


// Find coordinate number for this pixel axis
 
      cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);


// Update widths of fields

      Coordinate* pc = cSys.coordinate(coordinate).clone();
      listHeader (os, pc,  widthName, widthProj, widthShape, widthTile, 
                  widthRefValue, widthRefPixel, widthInc, widthUnits,
                  True, axisInCoordinate, pixelAxis,
                  nativeFormat, precRefValSci, precRefValFloat,
                  precRefValRADEC, precRefPixFloat, precIncSci);
      delete pc;
   }


// Compare with header widths

   widthName = max(nameName.length(), widthName) + 1;
   widthProj = max(nameProj.length(), widthProj) + 1;
   widthShape = max(nameShape.length(), widthShape) + 1;
   widthTile = max(nameTile.length(), widthTile) + 1;
   widthRefValue = max(nameRefValue.length(), widthRefValue) + 1;
   widthRefPixel = max(nameRefPixel.length(), widthRefPixel) + 1;
   widthInc = max(nameInc.length(), widthInc) + 1;
   widthUnits = max(nameUnits.length(), widthUnits);
}


template <class T> 
void ImageSummary<T>::listHeader (LogIO& os, Coordinate* pc,
                                  uInt& widthName, uInt& widthProj,
                                  uInt& widthShape, uInt& widthTile,
                                  uInt& widthRefValue, uInt& widthRefPixel, 
                                  uInt& widthInc, uInt& widthUnits, const Bool& findWidths,
                                  const Int& axisInCoordinate, const Int& pixelAxis,
                                  const Bool& nativeFormat, const Int& precRefValSci, 
                                  const Int& precRefValFloat, const Int& precRefValRADEC, 
                                  const Int& precRefPixFloat, const Int& precIncSci) const
//
// List all the good stuff
//
//  Input:
//     os               The LogIO to write to
//     pc               Pointer to the coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//     nativeFormat     If true don't convert any units
//           
{

// Clear flags

   if (!findWidths) clearFlags(os);

// Axis name

   String string = pc->worldAxisNames()(axisInCoordinate);
   if (findWidths) {
      widthName = max(widthName, string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthName);
      os << string;
   }


// Projection

   if (pc->type() == Coordinate::DIRECTION) {
      DirectionCoordinate* dc = (DirectionCoordinate*)pc;
      string = dc->projection().name();
   } else {
      string = " ";
   }
   if (findWidths) {
      widthProj = max(widthProj, string.length());
   } else {
      os.output().setf(ios::right, ios::adjustfield);
      os.output().width(widthProj);
      os << string;
   }


// Number of pixels
   
   if (pixelAxis != -1) {
      ostrstream oss;
      oss << this->shape()(pixelAxis);
      string = String(oss);
   } else {
      string = " ";
   }
   if (findWidths) {
      widthShape = max(widthShape, string.length());
   } else {
      os.output().width(widthShape);
      os << string;
   }


// Tile shape

   if (pixelAxis != -1) {
      ostrstream oss;
      oss << this->tileShape()(pixelAxis);
      string = String(oss);
   } else {
      string = " ";
   }
   if (findWidths) {
      widthTile = max(widthTile, string.length());
   } else {
      os.output().width(widthTile);
      os << string;
   }


// Remember units

   Vector<String> oldUnits(pc->nWorldAxes());
   oldUnits = pc->worldAxisUnits();
   Vector<String> units(pc->nWorldAxes());


// Reference value

   if (nativeFormat && pc->type() != Coordinate::STOKES) {
      ostrstream oss;
      oss.setf(ios::scientific, ios::floatfield);
      oss.precision(precRefValSci);
      oss << pc->referenceValue()(axisInCoordinate);
      string = String(oss);
   } else {
      Coordinate::formatType form;
      Bool absolute = True;
      String listUnits;
      Int prec;

      if (pc->type() == Coordinate::STOKES) {
         Vector<Double> world(1);
         Vector<Double> pixel(1);
         String sName;
         form = Coordinate::DEFAULT;

         if (pixelAxis != -1) {
            for (Int i=0; i<this->shape()(pixelAxis); i++) {
               pixel(0) = Double(i);
               Bool ok = pc->toWorld(world, pixel);
               String temp;
               if (ok) {
                  temp = pc->format(listUnits, form, world(0), 
                                    axisInCoordinate, absolute, -1);
               } else {
                  temp = "?";
               }
               sName += temp;
            }
         } else {
            pixel(0) =pc->referencePixel()(axisInCoordinate);
            Bool ok = pc->toWorld(world, pixel);
            if (ok) {
               sName = pc->format(listUnits, form, world(0), 
                                  axisInCoordinate, absolute, -1);
            } else {
               sName = "?";
            }
         }
         string = sName;
      } else {
         if (pc->type() == Coordinate::DIRECTION) {

// Convert to radians for formatting for DirectionCoordinate

            units = "rad";
            pc->setWorldAxisUnits(units);
         }
         form = Coordinate::DEFAULT;
         pc->getPrecision(prec, form, absolute, precRefValSci, 
                          precRefValFloat, precRefValRADEC);
         string = pc->format(listUnits, form, 
                            pc->referenceValue()(axisInCoordinate),
                            axisInCoordinate, absolute, prec);
      }
   }
   if (findWidths) {
      widthRefValue = max(widthRefValue,string.length());
   } else {
      os.output().width(widthRefValue);
      os << string;
   }

// Reference pixel

   if (pc->type() != Coordinate::STOKES) {
      if (pixelAxis != -1) {
         ostrstream oss;
         oss.setf(ios::fixed, ios::floatfield);
         oss.precision(precRefPixFloat);
         oss << pc->referencePixel()(axisInCoordinate) + 1.0;
         string = String(oss);
      } else {
         string = " ";
      }
      if (findWidths) {
         widthRefPixel = max(widthRefPixel,string.length());
      } else {
         os.output().width(widthRefPixel);
         os << string;
      }
   }


// Increment

   String incUnits;
   if (pc->type() != Coordinate::STOKES) {
      if (pixelAxis != -1) {
         if (nativeFormat) {
            ostrstream oss;
            oss.setf(ios::scientific, ios::floatfield);
            oss.precision(precIncSci);
            oss << pc->increment()(axisInCoordinate);
            string = String(oss);
            incUnits = pc->worldAxisUnits()(axisInCoordinate);
         } else {
            Coordinate::formatType form;
            const Bool absolute = False;
            String listUnits;
            Int prec;

            form = Coordinate::DEFAULT;
 
            pc->getPrecision(prec, form, absolute, precRefValSci, 
                             precRefValFloat, precRefValRADEC);
            string = pc->format(incUnits, form, 
                                pc->increment()(axisInCoordinate),
                                axisInCoordinate, absolute, prec);
         }
      } else {
         string = " ";
      }
      if (findWidths) {
         widthInc = max(widthInc,string.length());
      } else {
         os.output().width(widthInc);
         os << string;
      }
   }


// Increment units

   if (pc->type()!= Coordinate::STOKES) {
      if (pixelAxis != -1) {
/*
         ostrstream oss;
         oss << " " << pc->worldAxisUnits()(axisInCoordinate);
         string = String(oss);
*/
         string = " " + incUnits;
      } else {
         string = " ";
      }
      if (findWidths) {
         widthUnits = max(widthUnits,string.length());
      } else {
         os.output().setf(ios::left, ios::adjustfield);
         os << string;
      }
   }

   if (!findWidths) os << endl;    
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

