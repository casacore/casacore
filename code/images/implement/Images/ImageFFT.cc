//# ImageFFT.cc: FFT an image
//# Copyright (C) 1995,1997,1998,1999,2000
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
#include <aips/Lattices/LatticeStepper.h>
#include <aips/Lattices/LatticeIterator.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/TempImage.h>



ImageFFT::ImageFFT()
: itsTempImagePtr(0),
  itsInImagePtrFloat(0),
  itsInImagePtrComplex(0),
  itsDone(False)
{
}


ImageFFT::~ImageFFT()
{
   delete itsTempImagePtr;
   itsTempImagePtr = 0;
//
   delete itsInImagePtrFloat;
   itsInImagePtrFloat = 0;
//
   delete itsInImagePtrComplex;
   itsInImagePtrComplex = 0;
}

ImageFFT::ImageFFT(const ImageFFT& other)
: itsTempImagePtr(0),
  itsInImagePtrFloat(0),
  itsInImagePtrComplex(0),
  itsDone(False)
{
  if (this != &other) {
     if (other.itsTempImagePtr!=0) {
        itsTempImagePtr = other.itsTempImagePtr->cloneII();
     }
//
     if (other.itsInImagePtrFloat!=0) {
        itsInImagePtrFloat = other.itsInImagePtrFloat->cloneII();
     }
//
     if (other.itsInImagePtrComplex!=0) {
        itsInImagePtrComplex = other.itsInImagePtrComplex->cloneII();
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
     delete itsInImagePtrFloat;
     itsInImagePtrFloat = 0;
     if (other.itsInImagePtrFloat!=0) {
        itsInImagePtrFloat = other.itsInImagePtrFloat->cloneII();
     }
//
     delete itsInImagePtrComplex;
     itsInImagePtrComplex = 0;
     if (other.itsInImagePtrComplex!=0) {
        itsInImagePtrComplex = other.itsInImagePtrComplex->cloneII();
     }
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

   delete itsInImagePtrFloat;
   delete itsInImagePtrComplex;
   itsInImagePtrFloat = 0;
   itsInImagePtrComplex = 0;
//
   itsInImagePtrFloat = in.cloneII();

// Create TempImage

   delete itsTempImagePtr;
   itsTempImagePtr = 0;
   itsTempImagePtr = new TempImage<Complex>(in.shape(), in.coordinates());   

// Set new coordinate system in TempImage

   uInt dC2 = dC;
   setSkyCoordinates (os, *itsTempImagePtr, *itsInImagePtrFloat, dC2);

// Do complex FFT
  
   fftsky2(*itsTempImagePtr, *itsInImagePtrFloat, pixelAxes);
//
   itsDone = True;
}


void ImageFFT::fft(const ImageInterface<Float>& in,
                   const Vector<Bool>& axes)
{
   LogIO os(LogOrigin("ImageFFT", "fft(,,)", WHERE));

// Check axes are ok 

   checkAxes (in.coordinates(), in.ndim(), axes);

// Set pointer for the input image

   delete itsInImagePtrFloat;
   delete itsInImagePtrComplex;
   itsInImagePtrFloat = 0;
   itsInImagePtrComplex = 0;
   itsInImagePtrFloat = in.cloneII();

// Create TempImage

   delete itsTempImagePtr;
   itsTempImagePtr = 0;
   itsTempImagePtr = new TempImage<Complex>(in.shape(), in.coordinates());   

// Set new coordinate system in TempImage

   setCoordinates (os, *itsTempImagePtr, itsInImagePtrFloat->coordinates(), 
                   axes, in.shape());

// Do complex FFT
  
   fft2(*itsTempImagePtr, *itsInImagePtrFloat, axes);
// 
   itsDone = True;
}



void ImageFFT::fft(const ImageInterface<Complex>& in,
                   const Vector<Bool>& axes)
{
   LogIO os(LogOrigin("ImageFFT", "fft(,,)", WHERE));

// Check axes are ok 

   checkAxes (in.coordinates(), in.ndim(), axes);

// Set pointer for the input image

   delete itsInImagePtrFloat;
   delete itsInImagePtrComplex;
   itsInImagePtrFloat = 0;
   itsInImagePtrComplex = 0;
   itsInImagePtrComplex = in.cloneII();

// Create TempImage

   delete itsTempImagePtr;
   itsTempImagePtr = 0;
   itsTempImagePtr = new TempImage<Complex>(in.shape(), in.coordinates());   

// Set new coordinate system in TempImage

   setCoordinates (os, *itsTempImagePtr, itsInImagePtrComplex->coordinates(), 
                   axes, in.shape());

// Do complex FFT
  
   fft3(*itsTempImagePtr, *itsInImagePtrComplex, axes);
// 
   itsDone = True;
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
   copyMask(out);
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output real image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out);
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
   copyMask(out);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output real image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out);
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
   copyMask(out);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output imaginary image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out);
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
   copyMask(out);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output amplitude image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out);
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
   copyMask(out, *itsInImagePtrFloat);
//
   if (!out.setCoordinateInfo(itsTempImagePtr->coordinates())) {
      os << "Could not replace CoordinateSystem in output phase image" << LogIO::EXCEPTION;
   }
//
   copyMiscellaneous(out);
   out.setUnits(Unit("deg"));
}


// Private functions

void ImageFFT::checkAxes(const CoordinateSystem& cSys, uInt ndim, const Vector<Bool>& axes)
{
   LogIO os(LogOrigin("ImageFFT", "checkAxes(,)", WHERE));
//
   if (axes.nelements() != ndim) {
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

void ImageFFT::copyMask (ImageInterface<Float>& out) const
{
   if (itsInImagePtrFloat!=0) {
      copyMask(out, *itsInImagePtrFloat);
   } else {
      copyMask(out, *itsInImagePtrComplex);
   }
}

void ImageFFT::copyMask (ImageInterface<Complex>& out) const
{
   if (itsInImagePtrFloat!=0) {
      copyMask(out, *itsInImagePtrFloat);
   } else {
      copyMask(out, *itsInImagePtrComplex);
   }
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

void ImageFFT::copyMask (ImageInterface<Float>& out,
                         const ImageInterface<Complex>& in) const
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
      RO_LatticeIterator<Complex> iter(in, stepper);
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

void ImageFFT::copyMask (ImageInterface<Complex>& out,
                         const ImageInterface<Complex>& in) const
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
      RO_LatticeIterator<Complex> iter(in, stepper);
      for (iter.reset(); !iter.atEnd(); iter++) {
         out.putMaskSlice(in.getMaskSlice(iter.position(), iter.cursorShape()),
                          iter.position());
      }
   }
}



void ImageFFT::copyMiscellaneous (ImageInterface<Float>& out) const
{
   if (itsInImagePtrFloat!=0) {
      out.setMiscInfo(itsInImagePtrFloat->miscInfo());
      out.setImageInfo(itsInImagePtrFloat->imageInfo());
      out.setUnits(itsInImagePtrFloat->units());
      out.mergeTableLogSink(itsInImagePtrFloat->logSink());
   } else {
      out.setMiscInfo(itsInImagePtrComplex->miscInfo());
      out.setImageInfo(itsInImagePtrComplex->imageInfo());
      out.setUnits(itsInImagePtrComplex->units());
      out.mergeTableLogSink(itsInImagePtrComplex->logSink());
   }
}


void ImageFFT::copyMiscellaneous (ImageInterface<Complex>& out) const
{
   if (itsInImagePtrFloat!=0) {
      out.setMiscInfo(itsInImagePtrFloat->miscInfo());
      out.setImageInfo(itsInImagePtrFloat->imageInfo());
      out.setUnits(itsInImagePtrFloat->units());
      out.mergeTableLogSink(itsInImagePtrFloat->logSink());
   } else {
      out.setMiscInfo(itsInImagePtrComplex->miscInfo());
      out.setImageInfo(itsInImagePtrComplex->imageInfo());
      out.setUnits(itsInImagePtrComplex->units());
      out.mergeTableLogSink(itsInImagePtrComplex->logSink());
   }
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

void ImageFFT::fft3(ImageInterface<Complex>& out, 
                    const ImageInterface<Complex>& in,
                    const Vector<Bool>& axes)
{

// Do the FFT.  Use in place complex because it does
// all the unscrambling for me.  Replace masked values
// by zero.  LEL is a marvel.

   Complex zero(0.0,0.0);
   LatticeExpr<Complex> expr(replace(in,zero));
   out.copyData(expr);
//
   LatticeFFT::cfft(out, axes, True);
}


void ImageFFT::setSkyCoordinates (LogIO& os,
                                  ImageInterface<Complex>& out,
                                  const ImageInterface<Float>& in,
                                  uInt dC)
//
// dC is the DC coordinate number
//
{
// Find the input CoordinateSystem

   CoordinateSystem cSys = in.coordinates();
   Vector<Int> pixelAxes = cSys.pixelAxes(dC);
   AlwaysAssert(pixelAxes.nelements()==2,AipsError);

// Set the DirectionCoordinate axes to True

   Vector<Bool> axes(cSys.nPixelAxes(), False);
   axes(pixelAxes(0)) = True;
   axes(pixelAxes(1)) = True;

// FT the CS

   Coordinate* pC = cSys.makeFourierCoordinate(axes, in.shape().asVector());

// Replace TempImage CS with the new one

   CoordinateSystem* pC2 = (CoordinateSystem*)(pC);
   if (!out.setCoordinateInfo(*pC2)) {
      os << "Could not replace Coordinate System in internal complex image" << LogIO::EXCEPTION;
   }
//
   delete pC; pC = 0;
   pC2 = 0;
}

void ImageFFT::setCoordinates (LogIO& os,
                               ImageInterface<Complex>& out,
                               const CoordinateSystem& cSys,
                               const Vector<Bool>& axes,
                               const IPosition& shape)
{
// FT CS

   Coordinate* pC = cSys.makeFourierCoordinate(axes, shape.asVector());

// Replace TempImage CS with the fiddled one

   CoordinateSystem* pCS = (CoordinateSystem*)(pC);
   if (!out.setCoordinateInfo(*pCS)) {
      os << "Could not replace Coordinate System in internal complex image" << LogIO::EXCEPTION;
   }

//
   delete pC;
   pC = 0;
   pCS = 0;
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
