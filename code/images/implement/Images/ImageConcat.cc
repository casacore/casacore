//# ImageConcat.cc: concatenate images
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
//# $Id: 

#include <trial/Images/ImageConcat.h>


#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/Assert.h>

#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Images/ImageSummary.h>




template<class T>
ImageConcat<T>::ImageConcat()
: LatticeConcat(0, False),
  itsWarnAxisNames(True),
  itsWarnAxisUnits(True),
  itsWarnImageUnits(True),
  itsWarnRefPix(True),
  itsWarnRefVal(True),
  itsWarnInc(True)
{
}

template<class T>
ImageConcat<T>::ImageConcat(uInt axis, Bool showProgress)
: LatticeConcat(axis, showProgress),
  itsWarnAxisNames(True),
  itsWarnAxisUnits(True),
  itsWarnImageUnits(True),
  itsWarnContig(True),
  itsWarnRefPix(True),
  itsWarnRefVal(True),
  itsWarnInc(True)
{
}

template<class T>
ImageConcat<T>::ImageConcat (const ImageConcat<T>&other) 
: LatticeConcat<T>(other),
  itsWarnAxisNames(other.itsWarnAxisNames),
  itsWarnAxisUnits(other.itsWarnAxisUnits),
  itsWarnImageUnits(other.itsWarnImageUnits),
  itsWarnContig(other.itsWarnContig),
  itsWarnRefPix(other.itsWarnRefPix),
  itsWarnRefVal(other.itsWarnRefVal),
  itsWarnInc(other.itsWarnInc)
{
}

template<class T>
ImageConcat<T>::~ImageConcat()
{
}

template<class T>
ImageConcat<T>& ImageConcat<T>::operator= (const ImageConcat<T>& other)
{
  if (this != &other) {
     LatticeConcat<T>::operator=(other);
     itsWarnAxisNames  = other.itsWarnAxisNames;
     itsWarnAxisUnits  = other.itsWarnAxisUnits;
     itsWarnImageUnits = other.itsWarnImageUnits;
     itsWarnContig = other.itsWarnContig;
     itsWarnRefPix = other.itsWarnRefPix;
     itsWarnRefVal = other.itsWarnRefVal;
     itsWarnInc = other.itsWarnInc;
  }
  return *this;
}


template<class T>
void ImageConcat<T>::setAxis(uInt pixelAxis, Bool relax)
{


// Changes state of LC object as needed

   LatticeConcat<T>::setAxis(pixelAxis);


// Recheck images for consistency with new axis.
// Since we don't know what value of relax was used
// last time, we always make these checks

   const uInt n = itsLattices.nelements();
//
   if (n > 1) {
      LogIO os(LogOrigin("ImageConcat", "setImage(...)", WHERE));    
//
      for (uInt j=1; j<n; j++) {
   
// Compare coordinates at end of last image and start of current image

         const ImageInterface<T>* pIm1 = (ImageInterface<T>*)(itsLattices[j-1]);
         const ImageInterface<T>* pIm2 = (ImageInterface<T>*)(itsLattices[j]);
         checkContiguous (itsWarnContig, pIm1->shape(), pIm1->coordinates(), 
                          pIm2->coordinates(), os, itsAxis, relax);

// Compare coordinate descriptors not on concatenation axis

         ImageSummary<Float> sum1(*pIm1);
         ImageSummary<Float> sum2(*pIm2);
         checkCoordinates (itsWarnRefPix, itsWarnRefVal, itsWarnInc, 
                           os, sum1, sum2, itsAxis, relax);
      }
   } 
}

template<class T>
void ImageConcat<T>::setImage(ImageInterface<T>& image, Bool relax)
{
    LogIO os(LogOrigin("ImageConcat", "setImage(...)", WHERE));

// How many images have we set so far ?

   const uInt nIm = itsLattices.nelements();

// Do Lattice relevant things. This makes shape checks and
// sets the lattice pointers

   LatticeConcat<T>::setLattice(image);


// Do the extra image checks. 

   if (nIm>1) {

// Caste the pointer of the last image so we can get at ImageInterface things

      const ImageInterface<T>* pIm0 = (ImageInterface<T>*)(itsLattices[0]);
      const CoordinateSystem& cSys0 = pIm0->coordinates();
//
      const CoordinateSystem& cSys = image.coordinates();
//
      if (cSys.nCoordinates() != cSys0.nCoordinates()) {
         os << "Images have inconsistent numbers of coordinates" 
            << LogIO::EXCEPTION;
      }
      if (!allEQ(cSys.worldAxisNames(), cSys0.worldAxisNames())) {
         if (relax) {
            if (itsWarnAxisNames) {
               os << LogIO::WARN
                  << "Image axis names differ" << LogIO::POST;
               itsWarnAxisNames = False;
            }
         } else {
           os <<  "Image axis names differ" << LogIO::EXCEPTION;
         }  
      }
      if (!allEQ(cSys.worldAxisUnits(),cSys0.worldAxisUnits())) {
         if (relax) {
            if (itsWarnAxisUnits) {
               os << LogIO::WARN
                  << "Image axis units differ" << LogIO::POST;
               itsWarnAxisUnits = False;
            }
         } else {
            os <<  "Image axis units differ" << LogIO::EXCEPTION;
         }  
      }
      if (image.units() != pIm0->units() && itsWarnImageUnits) {
         os << LogIO::WARN
            << "Image units differ" << LogIO::POST;

         itsWarnImageUnits = False;
      }

// Compare coordinates at end of last image and start of new image

      const ImageInterface<T>* pImLast = (ImageInterface<T>*)(itsLattices[nIm-1]);
      const CoordinateSystem& cSysLast = pImLast->coordinates();
      checkContiguous (itsWarnContig, pImLast->shape(), cSysLast, cSys, 
                       os, itsAxis, relax);

// Compare coordinate descriptors not on concatenation axis

      ImageSummary<Float> sum1(image);
      ImageSummary<Float> sum2(*pIm0);
      checkCoordinates (itsWarnRefPix, itsWarnRefVal, itsWarnInc, 
                        os, sum1, sum2, itsAxis, relax);
   }
} 

template<class T>
void ImageConcat<T>::reset()
{
   LatticeConcat<T>::reset();
//
   itsWarnAxisNames = True;
   itsWarnAxisUnits = True;
   itsWarnImageUnits = True;
   itsWarnRefPix = True;
   itsWarnRefVal = True;
   itsWarnInc = True;
} 


template<class T>
void ImageConcat<T>::copyData(ImageInterface<T>& image)
{
// Copy the pixels and mask.  Throws an exception if no
// images do concatenate

   LatticeConcat<T>::copyData(image);

// Copy the imagy things from the first image

   ImageInterface<T>* pIm = (ImageInterface<T>*)(itsLattices[0]);
//   
   image.setCoordinateInfo(pIm->coordinates());
   image.setMiscInfo(pIm->miscInfo());
   image.setUnits(pIm->units());
   image.setImageInfo(pIm->imageInfo());

// Copy history

   for (uInt i=0; i<itsLattices.nelements(); i++) {
     image.mergeTableLogSink(*pIm);
   }
} 


template<class T>
void ImageConcat<T>::checkContiguous (Bool& warnContig, const IPosition& shape1,
                                      const CoordinateSystem& cSys1,
                                      const CoordinateSystem& cSys2,
                                      LogIO& os, uInt axis, Bool relax) 
//
// cSys1 from last image
// cSys2 from current image
//
// Find out the coordinate of the concatenation axis at the location
// of the last pixel from the previous image and compare.
//
{
   Int worldAxis;
   Double axisVal1 = coordConvert(worldAxis, os, cSys1, axis, Double(shape1(axis)-1));
   Double axisVal2 = coordConvert(worldAxis, os, cSys2, axis, Double(-1.0));
//
   Double inc = cSys1.increment()(worldAxis);
   if (abs(axisVal2-axisVal1) > 0.01*abs(inc)) {
      if (relax) {
         if (warnContig) {
            os << LogIO::WARN
               << "Images are not contiguous along the concatenation axis"
               << LogIO::POST;
            warnContig = False;
        }
      } else {
        os << "Images are not contiguous along the concatenation axis" 
           << LogIO::EXCEPTION;
      }
   }
}


template<class T>
Double ImageConcat<T>::coordConvert(Int& worldAxis, LogIO& os,
                                    const CoordinateSystem& cSys, 
                                    uInt axis, Double pixelCoord) const
{
   Vector<Double> pixel(cSys.nPixelAxes());
   Vector<Double> world(cSys.nWorldAxes());
//
   pixel = cSys.referencePixel();
   pixel(axis) = pixelCoord;
   if (!cSys.toWorld(world, pixel)) {
      os << "Coordinate conversion failed" << LogIO::EXCEPTION;
   }
   worldAxis = cSys.pixelAxisToWorldAxis(axis);
   if (worldAxis==-1) {
     os << "Concatenation pixel axis has no world axis" << LogIO::EXCEPTION;
   } 
   return world(worldAxis);
}



template<class T>
void ImageConcat<T>::checkCoordinates (Bool& warnRefPix, Bool& warnRefVal,
                                       Bool& warnInc, LogIO& os, 
                                       const ImageSummary<T>& sum1,
                                       const ImageSummary<T>& sum2,
                                       uInt axis, Bool relax)
                                       
//  
// Check coordinate descriptors for each non-concatenation axis
// The ImageSummary objects gives us the descriptors in pixel
// axis order
{
   const uInt dim = sum1.ndim();
   Vector<Double> refPix1 = sum1.referencePixels();
   Vector<Double> refPix2 = sum2.referencePixels();
   Vector<Double> refVal1 = sum1.referenceValues();
   Vector<Double> refVal2 = sum2.referenceValues();
   Vector<Double> inc1 = sum1.axisIncrements();
   Vector<Double> inc2 = sum2.axisIncrements();
//
   for (uInt j=0; j<dim; j++) {
      if (j!= axis) {
         if (!near(refPix1(j), refPix2(j))) {
            if (relax) {
               if (warnRefPix) {
                   os << LogIO::WARN
                      << "Image reference pixels are different on non-concatenation axis "
                      << j+1 << LogIO::POST;
                   warnRefPix = False;
               }
            } else {
               os << "Image reference pixels are different on non-concatenation axis "
                  << j+1 << LogIO::EXCEPTION;
            }
         }
//
         if (!near(refVal1(j), refVal2(j))) {
            if (relax) {
               if (warnRefVal) {
                  os << LogIO::WARN
                     << "Image reference values are different on non-concatenation axis "
                     << j+1 << LogIO::POST;
                  warnRefVal = False;
               }
            } else {
               os << "Image reference values are different on non-concatenation axis "
                  << j+1 << LogIO::EXCEPTION;
            }
         }
//
         if (!near(inc1(j), inc2(j))) {
            if (relax) {
               if (warnInc) {
                  os << LogIO::WARN
                     << "Image increments are different on non-concatenation axis "
                     << j+1 << LogIO::POST;
                  warnInc = False;
               }
            } else {
               os << "Image increments are different on non-concatenation axis "
                  << j+1 << LogIO::EXCEPTION;
            }
         }
      }
   }
}
