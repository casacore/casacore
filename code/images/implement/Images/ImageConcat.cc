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
//# $Id$


#include <trial/Images/ImageConcat.h>


#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>

#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/TabularCoordinate.h>
#include <trial/Images/ImageSummary.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageInfo.h>
#include <trial/Images/LELImageCoord.h>
#include <trial/Lattices/LatticeConcat.h>
#include <trial/Lattices/LELCoordinates.h>
#include <trial/Lattices/MaskedLattice.h>


template<class T>
ImageConcat<T>::ImageConcat()
: warnAxisNames_p(True),
  warnAxisUnits_p(True),
  warnImageUnits_p(True),
  warnRefPix_p(True),
  warnRefVal_p(True),
  warnInc_p(True),
  isContig_p(True)
{
}

template<class T>
ImageConcat<T>::ImageConcat(uInt axis)
: latticeConcat_p(axis),
  warnAxisNames_p(True),
  warnAxisUnits_p(True),
  warnImageUnits_p(True),
  warnContig_p(True),
  warnRefPix_p(True),
  warnRefVal_p(True),
  warnInc_p(True),
  isContig_p(True)
{
}

template<class T>
ImageConcat<T>::ImageConcat (const ImageConcat<T>&other) 
: ImageInterface<T>(other),
  latticeConcat_p(other.latticeConcat_p),
  unit_p(other.unit_p),
  warnAxisNames_p(other.warnAxisNames_p),
  warnAxisUnits_p(other.warnAxisUnits_p),
  warnImageUnits_p(other.warnImageUnits_p),
  warnContig_p(other.warnContig_p),
  warnRefPix_p(other.warnRefPix_p),
  warnRefVal_p(other.warnRefVal_p),
  warnInc_p(other.warnInc_p),
  isContig_p(other.isContig_p),
  rec_p(other.rec_p)
{
  isImage_p.resize(other.isImage_p.nelements());
  isImage_p = other.isImage_p;
}

template<class T>
ImageConcat<T>::~ImageConcat()
{
}

template<class T>
ImageConcat<T>& ImageConcat<T>::operator= (const ImageConcat<T>& other)
{
  if (this != &other) {
     ImageInterface<T>::operator= (other);
     latticeConcat_p = other.latticeConcat_p;
     unit_p = other.unit_p;
     warnAxisNames_p  = other.warnAxisNames_p;
     warnAxisUnits_p  = other.warnAxisUnits_p;
     warnImageUnits_p = other.warnImageUnits_p;
     warnContig_p = other.warnContig_p;
     warnRefPix_p = other.warnRefPix_p;
     warnRefVal_p = other.warnRefVal_p;
     warnInc_p = other.warnInc_p;
     isContig_p = other.isContig_p;
     rec_p = other.rec_p;
     isImage_p.resize(other.isImage_p.nelements());
     isImage_p = other.isImage_p;
  }
  return *this;
}


// Public functions
template<class T>
void ImageConcat<T>::setImage(ImageInterface<T>& image, Bool relax)
{
   LogIO os(LogOrigin("ImageConcat", "setImage(...)", WHERE));

// How many images have we set so far ?

   const uInt nIm = latticeConcat_p.nlattices();

// LatticeConcat allows the dimensionality to increase by
// one, but ImageConcat can't do that yet - so an extra
// test here.

   if (latticeConcat_p.axis() >= image.ndim()) {
      throw(AipsError("Axis number and image dimension are inconsistent"));
   }


// Do Lattice relevant things. This makes shape checks and
// sets the lattice pointers

   latticeConcat_p.setLattice(image);

// Do the extra image stuff.  Most of it is coordinate rubbish.
// The ImageInfo comes from the first image only

   isImage_p.resize(nIm+1,True);
   isImage_p(nIm) = True;
   if (nIm==0) {
      ImageInterface<T>::setCoordinateInfo(image.coordinates());
      unit_p = image.units();
      imageInfo_p = image.imageInfo();
   } else {

// Compare the coordinates of this image with the current private coordinates

      const CoordinateSystem& cSys = image.coordinates();
      if (cSys.nCoordinates() != coords_p.nCoordinates()) {
         os << "Images have inconsistent numbers of coordinates" 
            << LogIO::EXCEPTION;
      }
      if (!allEQ(cSys.worldAxisNames(), coords_p.worldAxisNames())) {
         if (relax) {
            if (warnAxisNames_p) {
               os << LogIO::WARN
                  << "Image axis names differ" << LogIO::POST;
               warnAxisNames_p = False;
            }
         } else {
           os <<  "Image axis names differ" << LogIO::EXCEPTION;
         }  
      }
      if (!allEQ(cSys.worldAxisUnits(),coords_p.worldAxisUnits())) {
         if (relax) {
            if (warnAxisUnits_p) {
               os << LogIO::WARN
                  << "Image axis units differ" << LogIO::POST;
               warnAxisUnits_p = False;
            }
         } else {
            os <<  "Image axis units differ" << LogIO::EXCEPTION;
         }  
      }
      if (image.units() != unit_p && warnImageUnits_p) {
         os << LogIO::WARN
            << "Image units differ" << LogIO::POST;

         warnImageUnits_p = False;
      }

// Compare coordinates at end of last image and start of new image

      const ImageInterface<T>* pImLast = (ImageInterface<T>*)(latticeConcat_p.lattice(nIm-1));
      const CoordinateSystem& cSysLast = pImLast->coordinates();
      checkContiguous (isContig_p, warnContig_p, pImLast->shape(), cSysLast, cSys, 
                       os, latticeConcat_p.axis(), relax);

// Compare coordinate descriptors not on concatenation axis

      ImageSummary<Float> sum1(image);
      const ImageInterface<T>* pIm0 = (ImageInterface<T>*)(latticeConcat_p.lattice(0));
      ImageSummary<Float> sum2(*pIm0);
      checkCoordinates (warnRefPix_p, warnRefVal_p, warnInc_p, 
                        os, sum1, sum2, latticeConcat_p.axis(), relax);

// Update the coordinates now we are happy all is well

      setCoordinates();
   }
} 


template<class T>
void ImageConcat<T>::setLattice(MaskedLattice<T>& lattice)
{
   LogIO os(LogOrigin("ImageConcat", "setLattice(...)", WHERE));

// How many images have we set so far ?

   const uInt nIm = latticeConcat_p.nlattices();

// Must have already set an image before we can set a lattice

   if (nIm==0) {
      throw(AipsError("You must call setImage before you can call setLattice"));
    }


// LatticeConcat allows the dimensionality to increase by
// one, but ImageConcat can't do that yet - so an extra
// test here.

   if (latticeConcat_p.axis() >= lattice.ndim()) {
      throw(AipsError("Axis number and lattice dimension are inconsistent"));
   }


// Do Lattice relevant things. This makes shape checks and
// sets the lattice pointers

   latticeConcat_p.setLattice(lattice);

// Because the Lattice has no coordinates, we signal
// a non-contiguity situation.  Function setCoordinates
// will make up a coordinate for this lattice

   isImage_p.resize(nIm+1,True);
   isImage_p(nIm) = False;
   isContig_p = False;
//
   setCoordinates();
} 



// Public virtual functions


template <class T>
void ImageConcat<T>::resize(const TiledShape&)
{  
   throw (AipsError ("ImageConcat::resize - an ImageConcat is not writable"));
}

template <class T>
Bool ImageConcat<T>::doGetSlice(Array<T>& buffer,
                              const Slicer& section)
{
   return latticeConcat_p.doGetSlice(buffer, section);
}

template <class T>
void ImageConcat<T>::doPutSlice (const Array<T>& buffer, const IPosition& where,
                                 const IPosition& stride)
{
   latticeConcat_p.doPutSlice(buffer, where, stride);
}

template <class T>
Bool ImageConcat<T>::doGetMaskSlice (Array<Bool>& buffer, const Slicer& section)
{
   return latticeConcat_p.doGetMaskSlice (buffer, section);
}

template <class T>
void ImageConcat<T>::doPutMaskSlice (const Array<Bool>& buffer, const IPosition& where,
                                     const IPosition& stride)
{
   latticeConcat_p.doPutMaskSlice(buffer, where, stride);
}



template<class T> 
Bool ImageConcat<T>::setUnits(const Unit& unit)
{
   unit_p = unit;
   return True;
}

template<class T> 
Unit ImageConcat<T>::units() const
{
  return unit_p;
}

template <class T>
Bool ImageConcat<T>::setCoordinateInfo(const CoordinateSystem& cSys)
{
   return ImageInterface<T>::setCoordinateInfo(cSys);
}


template <class T>
LELCoordinates ImageConcat<T>::lelCoordinates() const
{
    return LELCoordinates (new LELImageCoord (coords_p));
}

template<class T> 
const RecordInterface& ImageConcat<T>::miscInfo() const
{
   const uInt n = latticeConcat_p.nlattices();
   TableRecord x;
   for (uInt i=0; i<n; i++) {
      if (isImage_p(i)) {
         const ImageInterface<T>* pIm = (ImageInterface<T>*)(latticeConcat_p.lattice(i));
         x.defineRecord(i, pIm->miscInfo());
      }
   }
   rec_p.defineRecord(0,x);
   return rec_p.asRecord(0);
}
   
template <class T>
Bool ImageConcat<T>::setMiscInfo(const RecordInterface&)
{
   LogIO os(LogOrigin("ImageConcat", "setMiscInfo(...)", WHERE));
   os << LogIO::WARN << "Cannot set MiscInfo in ImageConcat objects" << LogIO::POST;
   return False;
}


template <class T>
IPosition ImageConcat<T>::doNiceCursorShape (uInt maxPixels) const
{  
   return latticeConcat_p.niceCursorShape(maxPixels);
}
 
   
template <class T>
Bool ImageConcat<T>::ok() const
{
   return True;   
}
 

template <class T>
LatticeIterInterface<T>* ImageConcat<T>::makeIter(const LatticeNavigator &navigator) const
{
  return latticeConcat_p.makeIter(navigator);
} 



// Private functions

template<class T>
void ImageConcat<T>::checkContiguous (Bool& isContig, Bool& warnContig, const IPosition& shape1,
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
               << "Images are not contiguous along the concatenation axis" << endl;
            os << "For this axis, a non-regular TabularCoordinate will be made"
               << LogIO::POST;
            warnContig = False;
        }
      } else {
        os << "Images are not contiguous along the concatenation axis" 
           << LogIO::EXCEPTION;
      }
      isContig = False;
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


template<class T>
void ImageConcat<T>::setCoordinates()
{

// If the images are not contiguous along the concatenation axis,
// make an irregular TabularCoordinate

   CoordinateSystem cSys = coords_p;
   const uInt axis = latticeConcat_p.axis();
   Int coord, axisInCoord;
   cSys.findPixelAxis(coord, axisInCoord,  axis);
   if (coord<0) {
      throw(AipsError("The coordinate for the concatenation axis has been removed"));        
   } 
//
   if (!isContig_p) {
      const uInt nIm = latticeConcat_p.nlattices();
      Vector<Double> pixelValues;
      Vector<Double> worldValues;
      uInt off = 0;
      String unit, name;

// Loop over images

      for (uInt i=0; i<nIm; i++) {
         const uInt l = pixelValues.nelements();
         const uInt shape = latticeConcat_p.lattice(i)->shape()(axis);
//
         if (isImage_p(i)) {
            ImageInterface<T>* pIm2 = (ImageInterface<T>*)(latticeConcat_p.lattice(i));
            const CoordinateSystem& cSys2 = pIm2->coordinates();
//
            Vector<Double> p = cSys2.referencePixel();
            Vector<Double> w = cSys2.referenceValue();
            Int worldAxis = cSys2.pixelAxisToWorldAxis(axis);
//
// For each pixel in concatenation axis for this image, find world 
// and pixel values 

            pixelValues.resize(l+shape, True);
            worldValues.resize(l+shape, True);
            for (uInt j=0; j<shape; j++) {        
               p(axis) = Double(j);
               if (cSys2.toWorld(w, p)) {
                  pixelValues(off+j) = p(axis) + off;
                  worldValues(off+j) = w(worldAxis);
               } else {
                  throw(AipsError(String("Coordinate conversion failed because")+cSys2.errorMessage()));        
               }
            }

// First lattice must be an image

            if (i==0) {
              unit = cSys2.worldAxisUnits()(worldAxis);
              name = cSys2.worldAxisNames()(worldAxis);
            }
         } else {
            Double winc;
            if (l==1) {
               winc = worldValues(0) / 10.0;
            } else {
               winc = worldValues(l-1)-worldValues(l-2);
            }
//
            pixelValues.resize(l+shape, True);
            worldValues.resize(l+shape, True);
            Double ww = worldValues(l-1) + winc;
//
            for (uInt j=0; j<shape; j++) {        
               pixelValues(off+j) = Double(j) + off;
               worldValues(off+j) = ww;
               ww += winc;
            }
         }
//
         off += shape;
      }

// Make TabularCoordinate and replace it.    If it's not monotonic, we
// can't make the TC, so fall back to CS from first image

      Bool ok = True;
      String msg;
      try {
         TabularCoordinate tc(pixelValues, worldValues, unit, name);
         cSys.replaceCoordinate(tc, uInt(coord));
         if (!ImageInterface<T>::setCoordinateInfo(cSys)) {
            throw(AipsError("Failed to save new CoordinateSystem with TabularCoordinate"));        
         }
      } catch (AipsError x) {
         ok = False;
         msg = x.getMesg();
      }
      if (!ok) {
         LogIO os(LogOrigin("ImageConcat", "setCoordinates(...)", WHERE));
         os << LogIO::WARN << "Could not create TabularCoordinate because " << msg << LogIO::POST;
         os << LogIO::WARN << "CoordinateSystem set to that of first image set instead" << LogIO::POST;
      }
   }
} 
