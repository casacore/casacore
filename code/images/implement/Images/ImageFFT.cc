//# ImageFFT.cc: FFT an image
//# Copyright (C) 1995,1997,1998,1999
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


#include <trial/Images/ImageFFT.h>


#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/Assert.h>

#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/TempImage.h>



ImageFFT::ImageFFT()
: itsTempImagePtr(0),
  itsInImagePtr(0),
  itsDone(False)
{
}


ImageFFT::~ImageFFT()
{
   if (itsTempImagePtr!=0) {
      delete itsTempImagePtr;
      itsTempImagePtr = 0;
      delete itsInImagePtr;
      itsInImagePtr = 0;
   }
}

ImageFFT::ImageFFT(const ImageFFT& other)
: itsTempImagePtr(0),
  itsInImagePtr(0),
  itsDone(False)
{
  if (this != &other) {
     if (other.itsTempImagePtr!=0) {
        itsTempImagePtr = other.itsTempImagePtr->cloneII();
     }
//
     if (other.itsInImagePtr!=0) {
        itsInImagePtr = other.itsInImagePtr->cloneII();
     }
//
     itsDone = other.itsDone;
   }
}

ImageFFT& ImageFFT::operator=(const ImageFFT& other)
{
  if (this != &other) {
     delete itsTempImagePtr;
     itsTempImagePtr = 0;
     if (other.itsTempImagePtr!=0) {
        itsTempImagePtr = other.itsTempImagePtr->cloneII();
     }
//
     delete itsInImagePtr;
     itsInImagePtr = 0;
     if (other.itsInImagePtr!=0) {
        itsInImagePtr = other.itsInImagePtr->cloneII();
     }
//
     itsDone = other.itsDone;
   }
   return *this;
}



void ImageFFT::fftsky(const ImageInterface<Float>& in) 
{
   delete itsInImagePtr;
   itsInImagePtr = 0;
   itsInImagePtr = in.cloneII();
//
   delete itsTempImagePtr;
   itsTempImagePtr = 0;
   itsTempImagePtr = new TempImage<Complex>(in.shape(), in.coordinates());   

// Do complex FFT
  
   ImageFFT::fftsky2(*itsTempImagePtr, in);

// Set new coordinate system into TempImage

   LogIO os(LogOrigin("ImageFFT", "fft()", WHERE));
   fiddleCoordinates (os);

// 
   itsDone = True;
}


const ImageInterface<Complex>& ImageFFT::getComplex()  const
{
   return *itsTempImagePtr;
}



void ImageFFT::getComplex(ImageInterface<Complex>& out)  const
{
   LogIO os(LogOrigin("ImageFFT", "complex(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(*itsTempImagePtr);
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output real image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getReal(ImageInterface<Float>& out)  const
{
   LogIO os(LogOrigin("ImageFFT", "real(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(real(*itsTempImagePtr)));
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output real image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getImaginary(ImageInterface<Float>& out) const
{
   LogIO os(LogOrigin("ImageFFT", "imag(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(imag(*itsTempImagePtr)));
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output imaginary image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getAmplitude(ImageInterface<Float>& out) const
{
   LogIO os(LogOrigin("ImageFFT", "amp(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(abs(*itsTempImagePtr)));
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output amplitude image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getPhase(ImageInterface<Float>& out) const
{
   LogIO os(LogOrigin("ImageFFT", "phase(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(arg(*itsTempImagePtr)));
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output phase image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
   out.setUnits(Unit("deg"));
}


// Private functions


void ImageFFT::fftsky2(ImageInterface<Complex>& out, 
                       const ImageInterface<Float>& in)
{
   LogIO os(LogOrigin("ImageFFT", "skyfft(...)", WHERE));
   if (!out.shape().isEqual(in.shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }

// Find the sky

   Vector<Int> pixelAxes, worldAxes;
   CoordinateSystem cSysIn = in.coordinates();
   Int dC;
   findSky(os, dC, pixelAxes, worldAxes, cSysIn);

// Do the FFT.  Use in place complex because it does
// all the unscrambling for me.  Replace masked values
// by zero and then convert to Complex.  LEL is a marvel.

   Float zero = 0.0;
   LatticeExpr<Complex> expr(toComplex(replace(in,zero)));
   out.copyData(expr);
//
   Vector<Bool> whichAxes(in.ndim(), False);
   whichAxes(pixelAxes(0)) = True;
   whichAxes(pixelAxes(1)) = True;
   LatticeFFT::cfft(out, whichAxes, True);
}


void ImageFFT::copyMiscellaneous (ImageInterface<Float>& out,
                                  const ImageInterface<Float>& in) const
{
    out.setMiscInfo(in.miscInfo());
    out.setImageInfo(in.imageInfo());
    out.setUnits(in.units());
    out.mergeTableLogSink(in);
}


void ImageFFT::copyMiscellaneous (ImageInterface<Complex>& out,
                                  const ImageInterface<Float>& in) const
{
    out.setMiscInfo(in.miscInfo());
    out.setImageInfo(in.imageInfo());
    out.setUnits(in.units());
//    out.mergeTableLogSink(in);
}

void ImageFFT::fiddleCoordinates (LogIO& os)
{

// Find the input CoordinateSystem

   CoordinateSystem cSys = itsInImagePtr->coordinates();

// Find the sky from the input image

   Vector<Int> pixelAxes, worldAxes;
   Int dC;
   findSky(os, dC, pixelAxes, worldAxes, cSys);

// Set the axis units in radians
   
   DirectionCoordinate dCoord(cSys.directionCoordinate(dC));
   Vector<String> unitsIn = dCoord.worldAxisUnits();
   unitsIn.set("rad");
   if (!dCoord.setWorldAxisUnits(unitsIn)) {
     os << "Failed to set DirectionCoordinate units to radians" << LogIO::EXCEPTION;
   }
   Vector<Double> incIn = dCoord.increment();
   IPosition shape = itsTempImagePtr->shape();

// Fiddle about with coordinates.  We replace the DirectionCoordinate
// by a LinearCoordinate.  The mapping in the CoordinateSYStem of world
// axes etc remains the same.

   Vector<String> names(2), unitsOut(2);
   Vector<Double> refVal(2), refPix(2), incOut(2);
   Matrix<Double> xform(2,2);
   xform = 0.0; xform.diagonal() = 1.0;
//
   refVal.set(0.0);
   refPix(0) = Int(shape(pixelAxes(0))/2);
   refPix(1) = Int(shape(pixelAxes(1))/2);
   incOut(0) = 1.0 / (shape(pixelAxes(0))*incIn(worldAxes(0)));
   incOut(1) = 1.0 / (shape(pixelAxes(1))*incIn(worldAxes(1)));
//
   names(0) = String("UU");
   names(1) = String("VV");
   unitsOut(0) = String("lambda");
   unitsOut(1) = String("lambda");
//
   LinearCoordinate lCoord(names, unitsOut, refVal, 
                           incOut, xform, refPix);

// Replace TempImage CS with the fiddled one

   cSys.replaceCoordinate(lCoord, dC);
   if (!itsTempImagePtr->setCoordinateInfo(cSys)) {
      os << "Could not replace Coordinate System in internal complex image" << LogIO::EXCEPTION;
   }
}



void ImageFFT::findSky(LogIO& os, Int& dC, Vector<Int>& pixelAxes, 
                       Vector<Int>& worldAxes, const CoordinateSystem& cSys)
{
   CoordinateUtil::findDirectionAxes (pixelAxes, worldAxes, dC, cSys);
   if (dC<0 || pixelAxes.nelements()!=2 || worldAxes.nelements()!=2) {
      os << "Image does not have 2 sky coordinate axes" << LogIO::EXCEPTION;
   }
   for (uInt i=0; i<2; i++) {
      if (pixelAxes(i)==-1 || worldAxes(i)==-1) {
        os << "Image does not have 2 sky coordinate axes" << LogIO::EXCEPTION;
      }
   }
}
