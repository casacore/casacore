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
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/TempImage.h>



ImageFFT::ImageFFT()
: itsTempImagePtr(0),
  itsInImagePtr(0),
  itsDone(False),
  itsCoordPtr(0)
{
}


ImageFFT::~ImageFFT()
{
   delete itsTempImagePtr;
   itsTempImagePtr = 0;
//
   delete itsInImagePtr;
   itsInImagePtr = 0;
//
   delete itsCoordPtr;
   itsCoordPtr = 0;
}

ImageFFT::ImageFFT(const ImageFFT& other)
: itsTempImagePtr(0),
  itsInImagePtr(0),
  itsDone(False),
  itsCoordPtr(0)
{

// We don't need to do anything with itsCoordPtr
// as its just used as a temporary

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

// We don't need to do anything with itsCoordPtr
// as its just used as a temporary

     delete itsCoordPtr;
//
     itsDone = other.itsDone;
   }
   return *this;
}



void ImageFFT::fftsky(const ImageInterface<Float>& in) 
{
   LogIO os(LogOrigin("ImageFFT", "fftsky()", WHERE));

// Try and find the sky first.   Exception if not there

   Int dC;
   Vector<Int> pixelAxes, worldAxes;
   findSky(os, dC, pixelAxes, worldAxes, in.coordinates(), True);

// Set pointer for the input image

   delete itsInImagePtr;
   itsInImagePtr = 0;
   itsInImagePtr = in.cloneII();

// Create TempImage

   delete itsTempImagePtr;
   itsTempImagePtr = 0;
   itsTempImagePtr = new TempImage<Complex>(in.shape(), in.coordinates());   

// Set new coordinate system in TempImage

   setCoordinates (os, *itsTempImagePtr, *itsInImagePtr,
                   pixelAxes, worldAxes, dC);

// Do complex FFT
  
   fftsky2(*itsTempImagePtr, *itsInImagePtr, pixelAxes);
//
   itsDone = True;
}



void ImageFFT::fft(const ImageInterface<Float>& in,
                   const Vector<Bool>& axes)
{
   LogIO os(LogOrigin("ImageFFT", "fft(,,)", WHERE));

// Check axes are ok 

   checkAxes (in, axes);

// Set pointer for the input image

   delete itsInImagePtr;
   itsInImagePtr = 0;
   itsInImagePtr = in.cloneII();

// Create TempImage

   delete itsTempImagePtr;
   itsTempImagePtr = 0;
   itsTempImagePtr = new TempImage<Complex>(in.shape(), in.coordinates());   

// Set new coordinate system in TempImage

   setCoordinates2 (os, *itsTempImagePtr, *itsInImagePtr, axes);

// Do complex FFT
  
   fft2(*itsTempImagePtr, *itsInImagePtr, axes);
// 
   itsDone = True;
}


const ImageInterface<Complex>& ImageFFT::getComplex()  const
{
   if (!itsDone) {
      LogIO os(LogOrigin("ImageFFT", "getComplex()", WHERE));
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }

   return *itsTempImagePtr;
}



void ImageFFT::getComplex(ImageInterface<Complex>& out)  const
{
   LogIO os(LogOrigin("ImageFFT", "getComplex()", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(*itsTempImagePtr);
   copyMask(out, *itsInImagePtr);
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output real image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getReal(ImageInterface<Float>& out)  const
{
   LogIO os(LogOrigin("ImageFFT", "getReal()", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(real(*itsTempImagePtr)));
   copyMask(out, *itsInImagePtr);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output real image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getImaginary(ImageInterface<Float>& out) const
{
   LogIO os(LogOrigin("ImageFFT", "getImaginary(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(imag(*itsTempImagePtr)));
   copyMask(out, *itsInImagePtr);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output imaginary image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getAmplitude(ImageInterface<Float>& out) const
{
   LogIO os(LogOrigin("ImageFFT", "getAmplitude(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(abs(*itsTempImagePtr)));
   copyMask(out, *itsInImagePtr);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output amplitude image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
}


void ImageFFT::getPhase(ImageInterface<Float>& out) const
{
   LogIO os(LogOrigin("ImageFFT", "getPhase(,,)", WHERE));
   if (!itsDone) {
      os << "You must call function fft first" << LogIO::EXCEPTION;
   }
   if (!out.shape().isEqual(itsTempImagePtr->shape())) {
      os << "Input and output images have inconsistent shapes" << LogIO::EXCEPTION;
   }
//
   out.copyData(LatticeExpr<Float>(arg(*itsTempImagePtr)));
   copyMask(out, *itsInImagePtr);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output phase image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out, *itsInImagePtr);
   out.setUnits(Unit("deg"));
}


// Private functions

void ImageFFT::checkAxes(const ImageInterface<Float>& in, const Vector<Bool>& axes)
{
   LogIO os(LogOrigin("ImageFFT", "checkAxes(,)", WHERE));
//
   CoordinateSystem cSys = in.coordinates();
   if (axes.nelements() != in.ndim()) {
      os << "The length of the axes vector must be the number of image dimensions" << LogIO::EXCEPTION;
   }

// See if we have the sky.  If the user wishes to FFT the sky, they
// must have both sky axes in their list

   Int dC;
   Vector<Int> pixelAxes, worldAxes;
   Bool haveSky = findSky(os, dC, pixelAxes, worldAxes, cSys, False);
   if (haveSky) {
      if (axes(pixelAxes(0)) || axes(pixelAxes(1))) {
         if (! (axes(pixelAxes(0)) && axes(pixelAxes(1)))) {
            os << "You must specify both the DirectionCoordinate (sky) axes to FFT" << LogIO::EXCEPTION;
         }
      }
   }  
//

}

void ImageFFT::copyMask (ImageInterface<Float>& out, 
                         const ImageInterface<Float>& in) const
{
   if (in.isMasked()) {
      if (!out.isMasked() || !out.isMaskWritable()) {
         LogIO os(LogOrigin("ImageFFT", "copyMask(...)", WHERE));
         os << LogIO::WARN << "The input image is masked but the output image does" << endl;
         os << "not have a writeable mask.  Therefore no mask will be transferred" << LogIO::POST;
         return;
      }

// Use the same stepper for input and output.

      IPosition cursorShape = out.niceCursorShape();
      LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);
  
// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.
      
      LatticeIterator<Float> dummyIter(out);
      RO_LatticeIterator<Float> iter(in, stepper);
      for (iter.reset(); !iter.atEnd(); iter++) { 
         out.putMaskSlice(in.getMaskSlice(iter.position(), iter.cursorShape()),
                          iter.position());
      }
   }
}


void ImageFFT::copyMask (ImageInterface<Complex>& out,
                         const ImageInterface<Float>& in) const
{
   if (in.isMasked()) {
      if (!out.isMasked() || !out.isMaskWritable()) {
         LogIO os(LogOrigin("ImageFFT", "copyMask(...)", WHERE));
         os << LogIO::WARN << "The input image is masked but the output image does" << endl;
         os << "not have a writeable mask.  Therefore no mask will be transferred" << LogIO::POST;
         return;
      }

// Use the same stepper for input and output.

      IPosition cursorShape = out.niceCursorShape();
      LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);
  
// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.
      
      LatticeIterator<Complex> dummyIter(out);
      RO_LatticeIterator<Float> iter(in, stepper);
      for (iter.reset(); !iter.atEnd(); iter++) { 
         out.putMaskSlice(in.getMaskSlice(iter.position(), iter.cursorShape()),
                          iter.position());
      }
   }
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


void ImageFFT::fftsky2(ImageInterface<Complex>& out, 
                       const ImageInterface<Float>& in,
                       const Vector<Int>& pixelAxes)
{

// Do the FFT.  Use in place complex because it does
// all the unscrambling for me.  Replace masked values
// by zero and then convert to Complex.  LEL is a marvel.

   if (in.isMasked()) {
      Float zero = 0.0;
      LatticeExpr<Complex> expr(toComplex(replace(in,zero)));
      out.copyData(expr);
   } else {
      LatticeExpr<Complex> expr(toComplex(in));
      out.copyData(expr);
   }    
//
   Vector<Bool> whichAxes(in.ndim(), False);
   whichAxes(pixelAxes(0)) = True;
   whichAxes(pixelAxes(1)) = True;
   LatticeFFT::cfft(out, whichAxes, True);
}


void ImageFFT::fft2(ImageInterface<Complex>& out, 
                    const ImageInterface<Float>& in,
                    const Vector<Bool>& axes)
{

// Do the FFT.  Use in place complex because it does
// all the unscrambling for me.  Replace masked values
// by zero and then convert to Complex.  LEL is a marvel.

   Float zero = 0.0;
   LatticeExpr<Complex> expr(toComplex(replace(in,zero)));
   out.copyData(expr);
//
   LatticeFFT::cfft(out, axes, True);
}


void ImageFFT::setCoordinates (LogIO& os,
                               ImageInterface<Complex>& out,
                               const ImageInterface<Float>& in,
                               const Vector<Int>& pixelAxes,
                               const Vector<Int>& worldAxes,
                               const Int dC)
//
// dC, pixelAxes and worldAxes tell us where the sky is in the CS
//
{

// Find the input CoordinateSystem

   CoordinateSystem cSys = in.coordinates();
   uInt which(dC);
   Vector<Bool> axes(in.ndim(), True);
   setInverseCoordinate (cSys, which, in.shape(), axes);

// Replace TempImage CS with the setd one

   if (!out.setCoordinateInfo(cSys)) {
      os << "Could not replace Coordinate System in internal complex image" << LogIO::EXCEPTION;
   }
}

void ImageFFT::setCoordinates2 (LogIO& os,
                                ImageInterface<Complex>& out,
                                const ImageInterface<Float>& in,
                                const Vector<Bool>& axes)
{
   CoordinateSystem cSys = in.coordinates();
   CoordinateSystem cSys2(cSys);
//
   Vector<Bool> doneCoord(cSys.nCoordinates(), False);
   Int coord, axisInCoord;
   for (uInt i=0; i<in.ndim(); i++) {
      if (axes(i)) {
         cSys.findPixelAxis(coord, axisInCoord, i);
         if (coord >=0) {
            if (!doneCoord(coord)) {
               setInverseCoordinate (cSys2, coord, in.shape(), axes);
               doneCoord(coord) = True;
            }
         } else {
            os << "The specified axis has been removed" << LogIO::EXCEPTION;
         }
      }
   }

// Replace TempImage CS with the fiddled one

   if (!out.setCoordinateInfo(cSys2)) {
      os << "Could not replace Coordinate System in internal complex image" << LogIO::EXCEPTION;
   }
}


Bool ImageFFT::findSky(LogIO& os, Int& dC, Vector<Int>& pixelAxes, 
                       Vector<Int>& worldAxes, const CoordinateSystem& cSys,
                       Bool throwIt)
//
// Assumes only one DirectionCoordinate .   {pixel,world}Axes says where 
// in the CS the DirectionCoordinate axes are
{
   CoordinateUtil::findDirectionAxes (pixelAxes, worldAxes, dC, cSys);
   if (dC<0 || pixelAxes.nelements()!=2 || worldAxes.nelements()!=2) {
      if (throwIt) {
         os << "Image does not have 2 sky coordinate axes" << LogIO::EXCEPTION;
      } else {
         return False;
      }
   }
//
   for (uInt i=0; i<2; i++) {
      if (pixelAxes(i)==-1 || worldAxes(i)==-1) {
        if (throwIt) {
           os << "Image does not have 2 sky coordinate axes" << LogIO::EXCEPTION;
        } else {
           return False;
        }
      }
   }
}



void ImageFFT::setInverseCoordinate (CoordinateSystem& cSys, uInt which, 
                                     const IPosition& shape, 
                                     const Vector<Bool>& axes)
//
// axes is the list of all pixel axes in the image to be FFTs.  
// It has already been checked valid. For DirectionCoordinates,
// both axes have been checked to be there
//
// This stuff should move to the Coordinates classes as virtual 
// functions.  Let's see if it works first.
//
// I make a big fudge here currently. I assume xform is a unit
// diagonal matrix.  This is not the case for rotated coordinates
// Have to do some mathematics to work out what to do...
// Currently we don't write out anything other than non-rotated
// coordinates, but this will change one day
//
{
   LogIO os(LogOrigin("ImageFFT", "setInverseCoordinate(,)", WHERE));
//
   delete itsCoordPtr;
   itsCoordPtr = 0;
//
   if (cSys.type(which)==Coordinate::LINEAR) {
      LinearCoordinate coord(cSys.linearCoordinate(which));
      Vector<Int> pixelAxes = cSys.pixelAxes(which);
      AlwaysAssert(pixelAxes.nelements()==coord.nPixelAxes(), AipsError);

// The coordinate doesn't know anything about silly pixel/world orderings
// Pixels and world are the same order

      Vector<String> units = coord.worldAxisUnits();
      Vector<String> names = coord.worldAxisNames();
      Vector<Double> inc = coord.increment();
      Vector<Double> refVal = coord.referenceValue();      
      Vector<Double> refPix = coord.referencePixel();

// Loop over the axes for this coordinate and see if its in
// the users list

      for (uInt i=0; i<pixelAxes.nelements(); i++) {
         if (axes(pixelAxes(i))) {

// User wants to FFT this pixel axis 
 
            String tmp = String("1/") + units(i);
            units(i) = tmp;
            refVal(i) = 0.0;
            refPix(i) = Int(shape(pixelAxes(i))/2);
            inc(i) = 1.0 / (shape(pixelAxes(i))*inc(i));
            names(i) = String("Inverse(") + names(i) + String(")");
         }
      }
      Matrix<Double> xform(pixelAxes.nelements(),pixelAxes.nelements());
      xform = 0.0; xform.diagonal() = 1.0;
      itsCoordPtr = new LinearCoordinate(names, units, refVal, 
                                         inc, xform, refPix);
   } else if (cSys.type(which)==Coordinate::DIRECTION) {
      DirectionCoordinate coord(cSys.directionCoordinate(which));

// Get increment in radians

      Vector<String> units = coord.worldAxisUnits();
      units.set("rad");
      if (!coord.setWorldAxisUnits(units)) {
        os << "Failed to set DirectionCoordinate units to radians" << LogIO::EXCEPTION;
      }
      Vector<Double> inc = coord.increment();

// Fiddle about with coordinates.  We replace the DirectionCoordinate
// by a LinearCoordinate.  The mapping in the CoordinateSYStem of world
// axes etc remains the same.

      Vector<String> names = coord.worldAxisNames();
      Vector<Double> refVal = coord.referenceValue();      
      Vector<Double> refPix = coord.referencePixel();
//      
      Matrix<Double> xform(2,2);
      xform = 0.0; xform.diagonal() = 1.0;
//
      Vector<Int> pixelAxes = cSys.pixelAxes(which);
      Vector<Int> worldAxes = cSys.worldAxes(which);
      AlwaysAssert(worldAxes.nelements()==2, AipsError);  // Should not happen
      AlwaysAssert(pixelAxes.nelements()==2, AipsError);
//
      refVal.set(0.0);
      refPix(0) = Int(shape(pixelAxes(0))/2);
      refPix(1) = Int(shape(pixelAxes(1))/2);
      inc(0) = 1.0 / (shape(pixelAxes(0))*inc(0));
      inc(1) = 1.0 / (shape(pixelAxes(1))*inc(1));
//
      names(0) = String("UU");
      names(1) = String("VV");
      units(0) = String("lambda");
      units(1) = String("lambda");
//
      itsCoordPtr = new LinearCoordinate(names, units, refVal, 
                                         inc, xform, refPix);
   } else if (cSys.type(which)==Coordinate::SPECTRAL) {

// Spectal Coordinate only has one pixel axis

      SpectralCoordinate coord(cSys.spectralCoordinate(which));
      Vector<Int> pixelAxes = cSys.pixelAxes(which);
      AlwaysAssert(pixelAxes.nelements()==coord.nPixelAxes(), AipsError);
      AlwaysAssert(pixelAxes.nelements()==1, AipsError);

// The coordinate doesn't know anything about silly pixel/world orderings
// Pixels and world are the same order

      Vector<String> units = coord.worldAxisUnits();
      Vector<String> names = coord.worldAxisNames();
      Vector<Double> refVal = coord.referenceValue();      
      Vector<Double> refPix = coord.referencePixel();

// Set the axis units to Hz

      units.set("Hz");
      if (!coord.setWorldAxisUnits(units)) {
        os << "Failed to set SpectralCoordinate units to Hz" << LogIO::EXCEPTION;
      }
      Vector<Double> inc = coord.increment();


// Loop over the axes for this coordinate and see if its in
// the users list

      Unit tmp2;
      if (axes(pixelAxes(0))) {

// User wants to FFT this pixel axis 
 
         units(0) = "s";
         refVal(0) = 0.0;
         refPix(0) = Int(shape(pixelAxes(0))/2);
         inc(0) = 1.0 / (shape(pixelAxes(0))*inc(0));
         names(0) = String("Time");
      }
      Matrix<Double> xform(1, 1);
      xform = 0.0; xform.diagonal() = 1.0;
      itsCoordPtr = new LinearCoordinate(names, units, refVal, 
                                         inc, xform, refPix);
   } else if (cSys.type(which)==Coordinate::STOKES) {
      os << "You cannot Fourier Transform the Stokes axis" << LogIO::EXCEPTION;
   } else if (cSys.type(which)==Coordinate::TABULAR) {
      os << "You cannot Fourier Transform a Tabular axis" << LogIO::EXCEPTION;
   } else {
      os << "Unrecognized Coordinate type" << LogIO::EXCEPTION;
   }

// Replace coordinate 

   cSys.replaceCoordinate(*itsCoordPtr, which);
}
