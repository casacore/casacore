//# ImageSummary.cc:  list an image header
//# Copyright (C) 1995,1996,1997,1998
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
#include <trial/Coordinates.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/MaskedImage.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Quanta/Unit.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/VelocityMachine.h>
#include <aips/Utilities/ValType.h>

#include <iomanip.h>
#include <iostream.h>

#include <trial/Images/ImageSummary.h>

template <class T>
ImageSummary<T>::ImageSummary (const MaskedImage<T>& image)
//
// Constructor assigns pointer.  If MaskedImage goes out of scope you
// will get rubbish.
//
{
   cSys_p = image.coordinates();
   obsInfo_p = cSys_p.obsInfo();
   pImage_p = &image;
}

template <class T> 
ImageSummary<T>::ImageSummary (const ImageSummary<T> &other)
//
// Copy constructor
//
{
   cSys_p = other.cSys_p;
   obsInfo_p = other.obsInfo_p;
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
   if (this != &other) {
      cSys_p = other.cSys_p;
      obsInfo_p = other.obsInfo_p;
      pImage_p = other.pImage_p;
   }
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
   Vector<String> names(cSys_p.nPixelAxes());
   Vector<String> worldNames(cSys_p.worldAxisNames());

   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      if (worldAxis != -1) {
         names(pixelAxis) = worldNames(worldAxis);
      } else {
         names(pixelAxis) = "removed";
      }
   }
   return names;
}




template <class T> 
Vector<Double> ImageSummary<T>::referencePixels () const
// 
// Get reference pixels for the pixel axes
//
{
   return cSys_p.referencePixel().ac()+Vector<Double>(cSys_p.nPixelAxes(),1.0).ac();
}


template <class T> 
Vector<Double> ImageSummary<T>::referenceValues () const
// 
// Get reference values for the pixel axes
//
{
   Vector<Double> refVals(cSys_p.nPixelAxes());
   Vector<Double> refValues(cSys_p.referenceValue());
// 
   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      if (worldAxis != -1) {
         refVals(pixelAxis) = refValues(worldAxis);
      } else {
         refVals(pixelAxis) = ValType::undefDouble();
      }
   }
   return refVals;
}


template <class T> 
Vector<Double> ImageSummary<T>::axisIncrements () const
// 
// Get axis increments for the pixel axes
//
{
   Vector<Double> incs(cSys_p.nPixelAxes());
   Vector<Double> worldIncs(cSys_p.increment());

   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      if (worldAxis != -1) {
         incs(pixelAxis) = worldIncs(worldAxis);
      } else {
         incs(pixelAxis) = ValType::undefDouble();
      }
   }
   return incs;
}

template <class T> 
Vector<String> ImageSummary<T>::axisUnits () const
// 
// Get axis units for the pixel axes
//
{
   Vector<String> units(cSys_p.nPixelAxes());
   Vector<String> worldUnits(cSys_p.worldAxisUnits());

   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      if (worldAxis != -1) {
         units(pixelAxis) = worldUnits(worldAxis);
      } else {
         units(pixelAxis) = "removed";
      }
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
String ImageSummary<T>::observer() const
//
// Get observer name
//
{
   return obsInfo_p.observer();
}


template <class T> 
String ImageSummary<T>::obsDate(MEpoch& epoch) const
//
// Get epoch 
//
{
   epoch = obsInfo_p.obsDate();
   MVTime time = MVTime(epoch.getValue());
   return time.string(MVTime::YMD);   
}


template <class T> 
String ImageSummary<T>::telescope() const
//
// Get telescope
//
{
   return obsInfo_p.telescope();
}

template <class T> 
Bool ImageSummary<T>::restFrequency(String& restFreqString, 
                                    Quantum<Double>& restFreq) const
{
   Bool ok;
   Int spectralAxis = CoordinateUtil::findSpectralAxis(cSys_p);
   if (spectralAxis >= 0) {
      Int coordinate, axisInCoordinate;
      cSys_p.findPixelAxis (coordinate, axisInCoordinate, spectralAxis);
//
      restFreq.setValue(cSys_p.spectralCoordinate(coordinate).restFrequency());
      restFreq.setUnit(cSys_p.spectralCoordinate(coordinate).worldAxisUnits()(axisInCoordinate));
      ostrstream oss;
//      oss.output().setf(ios::scientific, ios::floatfield);
//      oss.output().precision(8);
      oss << restFreq << endl;
      restFreqString= String(oss);
      ok = True;
   } else {
      restFreq.setValue(0.0);
      restFreq.setUnit("Hz");
      restFreqString = "";
      ok = False;
   }
   return ok;
}



template <class T> 
Bool ImageSummary<T>::frequencySystem(String& freqTypeString, 
                                      MFrequency::Types& freqType) const
{
   Bool ok;
   Int spectralAxis = CoordinateUtil::findSpectralAxis(cSys_p);
   if (spectralAxis >= 0) {
      Int coordinate, axisInCoordinate;
      cSys_p.findPixelAxis (coordinate, axisInCoordinate, spectralAxis);
//
      freqType = cSys_p.spectralCoordinate(uInt(coordinate)).frequencySystem();
      freqTypeString = MFrequency::showType(freqType);
      ok = True;
   } else {
      freqTypeString = "";
      ok = False;
   }
   return ok;
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
                            const MDoppler::Types velocityType,
                            const Bool nativeFormat) 
//
// List information about an image to the logger
//
// Input:
//   velocityType  Speciy velocity definition
//   nativeFormat  If True, reference values and axis increments
//                 are formatted in their native format.  E.g.
//                 RA and DEC in radians.  Otherwise, they are
//                 possibly converted to some other unit and
//                 formatted nicely (e.g. HH:MM:SS.S)
//
{

   os << LogIO::NORMAL << endl;
   MEpoch epoch;
   obsDate(epoch);

// List random things
   
   os << "Image name       : " << name() << endl;
   if (telescope() != "UNKNOWN") {
      os << "Telescope        : " << telescope() << endl;
   }
   if (observer() != "UNKNOWN") {
      os << "Observer         : " << observer() << endl;
   }
   if (epoch.getValue().getDay() != Double(0.0)) { 
      os << "Date observation : " << obsDate(epoch) << endl;
   }
   if (hasAMask()) {
      os << "Image mask       : Present" << endl;
   } else {
      os << "Image mask       : Absent" << endl;
   }
   if (!units().getName().empty()) 
      os << "Image units      : " << this->units().getName() << endl << endl;


// List DirectionCoordinate type from the first DirectionCoordinate we find

   Vector<Int> pixelAxes, worldAxes;
   Int coordinate;
   CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys_p);
   if (coordinate >= 0) {           
      os << "Direction system : " 
         << MDirection::showType(cSys_p.directionCoordinate(uInt(coordinate)).directionType()) << endl;
   }

// List rest frequency and reference frame from the first spectral axis we find

   String freqTypeString;
   MFrequency::Types freqType;
   if (frequencySystem(freqTypeString, freqType)) {
      os << "Frequency system : " << freqTypeString << endl;
      os << "Velocity  system : " << MDoppler::showType(velocityType) << endl;
   }
//
   Quantum<Double> restFreq;
   String restFreqString;
   if (restFrequency(restFreqString, restFreq)) {
      os << "Rest Frequency   : " << restFreqString << endl;
   } else {
      os << "Rest Frequency   : Absent" << endl;
   }
   os << endl;
      


// Determine the widths for all the fields that we want to list

   uInt widthName, widthProj, widthShape, widthTile, widthRefValue;
   uInt widthRefPixel, widthInc, widthUnits, totWidth;

   String nameName, nameProj, nameShape, nameTile, nameRefValue;
   String nameRefPixel, nameInc, nameUnits;
   
   Int precRefValSci, precRefValFloat, precRefValRADEC;
   Int precRefPixFloat, precIncSci;

   getFieldWidths (widthName, widthProj, widthShape, widthTile,
                   widthRefValue, widthRefPixel, widthInc,
                   widthUnits, precRefValSci, precRefValFloat, 
                   precRefValRADEC, precRefPixFloat,
                   precIncSci, nameName, nameProj, nameShape, 
                   nameTile, nameRefValue, nameRefPixel, nameInc, 
                   nameUnits, nativeFormat, cSys_p, velocityType);


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


   uInt pixelAxis;
   Int axisInCoordinate;
   for (pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {

// Find coordinate number for this pixel axis
 
      cSys_p.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);

// List it

      Coordinate* pc = cSys_p.coordinate(coordinate).clone();
//      cout << "type = " << pc->type() << endl;

      listHeader(os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 False, axisInCoordinate, pixelAxis, nativeFormat,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci);

// If the axis is spectral, we might like to see it as
// velocity as well as frequency.  Since the listing is row
// based, we have to do it like this.  Urk.

      if (pc->type() == Coordinate::SPECTRAL) {
         listVelocity (os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 False, axisInCoordinate, pixelAxis, velocityType,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci);

      }
      delete pc;
   }
   os << endl;


// Now find those pixel axes that have been removed and list their
// associated coordinate information.

   uInt worldAxis;
   for (worldAxis=0; worldAxis<cSys_p.nWorldAxes(); worldAxis++) {


// Find coordinate number for this pixel axis
 
      cSys_p.findWorldAxis(coordinate, axisInCoordinate, worldAxis);


// See if this world axis has an associated removed pixel axis
      
      Vector<Int> pixelAxes = cSys_p.pixelAxes(coordinate);
      if (pixelAxes(axisInCoordinate) == -1) {

// List it

        Coordinate* pc = cSys_p.coordinate(coordinate).clone();
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
Bool ImageSummary<T>::setNewImage (const MaskedImage<T>& image)
//
// Reassign pointer.  
//
{
   const MaskedImage<T>* pTemp;
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
                                      const CoordinateSystem& cSys,
                                      const MDoppler::Types velocityType) const
//
// All these silly format and precision things should really be
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

// Initialize (logger will never be actually used in this function)

   widthName = widthProj = widthShape = widthTile = widthRefValue = 0;
   widthRefPixel = widthInc = widthUnits = 0;
   LogIO os(LogOrigin("ImageSummary", "getFieldWidths()", WHERE));

// Loop over number of pixel axes

   uInt pixelAxis;
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


      if (pc->type() == Coordinate::SPECTRAL) {
         listVelocity (os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 True, axisInCoordinate, pixelAxis, velocityType,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci);
      }

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
void ImageSummary<T>::listHeader (LogIO& os, 
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
                                  const Int precIncSci) const
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
void ImageSummary<T>::listVelocity (LogIO& os, 
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
                                    const Int precIncSci) const
//
// List all the good stuff
//
//  Input:
//     os               The LogIO to write to
//     pc               Pointer to the coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//           
{

// Clear flags

   if (!findWidths) clearFlags(os);


// Axis name

   String string("Velocity");
   if (findWidths) {
      widthName = max(widthName, string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthName);
      os << string;
   }

// Projection

   if (!findWidths) {
     os.output().setf(ios::right, ios::adjustfield);
      os.output().width(widthProj);
      string = " ";
      os << string;
   }

// Number of pixels
   

   if (!findWidths) {
      os.output().width(widthShape);
      os << string;
   }

// Tile shape

   if (!findWidths) {   
      os.output().width(widthTile);
      os << string;
   }


// Caste the coordinate to a spectral coordinate

   SpectralCoordinate* sc = (SpectralCoordinate*)pc;

// Remember units

   Vector<String> oldUnits(pc->nWorldAxes());
   oldUnits = pc->worldAxisUnits();
   Vector<String> units(pc->nWorldAxes());


// Convert reference pixel it to a velocity and format 

   Double velocity;
   String velUnits("km/s");
   Bool ok = pixelToVelocity(velocity, sc->referencePixel()(axisInCoordinate),
                             sc, velocityType, velUnits);
   if (!ok) {
      string = "Fail";
   } else {
      Coordinate::formatType form;
      Bool absolute = True;
      String listUnits;
      Int prec;
      form = Coordinate::DEFAULT;
      pc->getPrecision(prec, form, absolute, precRefValSci, 
                       precRefValFloat, precRefValRADEC);
      string = sc->format(listUnits, form, velocity,
                          axisInCoordinate, absolute, prec);
   }
   if (findWidths) {
      widthRefValue = max(widthRefValue,string.length());
   } else {
      os.output().width(widthRefValue);
      os << string;
   }


// Reference pixel

   if (pixelAxis != -1) {
      ostrstream oss;
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(precRefPixFloat);
      oss << sc->referencePixel()(axisInCoordinate) + 1.0;
      string = String(oss);
   } else {
      string = " ";
   }
   if (!findWidths) {
      os.output().width(widthRefPixel);
      os << string;
   }
  

// Increment

   if (pixelAxis != -1) {
     Double velocityInc;
     if (!velocityIncrement(velocityInc, sc, velocityType, velUnits)) {
        string = "Fail";
     } else {
        ostrstream oss;
        oss.setf(ios::scientific, ios::floatfield);
        oss.precision(precIncSci);
        oss << velocityInc;
        string = String(oss);
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
 

// Increment units

   if (pixelAxis != -1) {
      string = " " + velUnits;
   } else {
      string = " ";
   }
   if (findWidths) {
      widthUnits = max(widthUnits,string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os << string;
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


template <class T> 
Bool ImageSummary<T>::pixelToVelocity(Double& velocity,
                                      const Double pixel,
                                      const SpectralCoordinate* sc,
                                      const MDoppler::Types velocityType,
                                      const String& velUnits) const

{

// Find rest frequency.  Assumes only one axis in SpectralCoordinate

   if (sc->nWorldAxes() != 1) return False;
   if (sc->restFrequency() <= 0) return False;

   Quantum<Double> restFreq;
   restFreq.setValue(sc->restFrequency());
   restFreq.setUnit(sc->worldAxisUnits()(0));

// Convert pixel to MFrequency

   MFrequency frequency;
   if (!sc->toWorld(frequency, pixel)) return False;


// Create velocity machine

   MDoppler::Ref ref;
   ref = velocityType;
   VelocityMachine m(sc->frequencySystem(), Unit("Hz"),
                     MVFrequency(restFreq),
   		     ref, Unit(velUnits));
   velocity = m(frequency.get("Hz")).getValue();

   return True;
}

         
template <class T> 
Bool ImageSummary<T>::velocityIncrement(Double& velocityInc,
                                        const SpectralCoordinate* sc,
                                        const MDoppler::Types velocityType,
                                        const String& velUnits) const
{

// DO this the hard way for now until Wim gives me spectralMachine

   if (sc->nWorldAxes() != 1) return False;
   Double refPix = sc->referencePixel()(0);

// Find world values at refPix +/- 0.5 and take difference

   Double pixel;
   pixel = refPix + 0.5;
   Double velocity1;
   if (!pixelToVelocity(velocity1, pixel, sc, velocityType, velUnits)) return False;

//

   pixel = refPix - 0.5;
   Double velocity2;
   if (!pixelToVelocity(velocity2, pixel, sc, velocityType, velUnits)) return False;


// Return increment
   
   velocityInc = velocity1 - velocity2;

   return True;
}


