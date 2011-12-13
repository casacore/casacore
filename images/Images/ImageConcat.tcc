//# ImageConcat.cc: concatenate images
//# Copyright (C) 1995,1997,1998,1999,2000,2001,2002,2003
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


#include <images/Images/ImageConcat.h>

#include <casa/OS/Timer.h>

#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Containers/Block.h>
#include <casa/Containers/Record.h>
#include <casa/Exceptions/Error.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Logging/LogIO.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>

#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/TabularCoordinate.h>
#include <coordinates/Coordinates/StokesCoordinate.h>
#include <images/Images/ImageSummary.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/ImageInfo.h>
#include <images/Images/LELImageCoord.h>
#include <lattices/Lattices/LatticeConcat.h>
#include <lattices/Lattices/MaskedLattice.h>
#include <lattices/Lattices/LELCoordinates.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
ImageConcat<T>::ImageConcat()
: latticeConcat_p(),
  warnAxisNames_p(True),
  warnAxisUnits_p(True),
  warnImageUnits_p(True),
  warnRefPix_p(True),
  warnRefVal_p(True),
  warnInc_p(True),
  warnTab_p(True),
  isContig_p(True)
{}

template<class T>
ImageConcat<T>::ImageConcat (uInt axis, Bool tempClose)
: latticeConcat_p(axis, tempClose),
  warnAxisNames_p(True),
  warnAxisUnits_p(True),
  warnImageUnits_p(True),
  warnContig_p(True),
  warnRefPix_p(True),
  warnRefVal_p(True),
  warnInc_p(True),
  warnTab_p(True),
  isContig_p(True)
{}

template<class T>
ImageConcat<T>::ImageConcat (const ImageConcat<T>& other) 
: ImageInterface<T>(other),
  latticeConcat_p(other.latticeConcat_p),
  warnAxisNames_p(other.warnAxisNames_p),
  warnAxisUnits_p(other.warnAxisUnits_p),
  warnImageUnits_p(other.warnImageUnits_p),
  warnContig_p(other.warnContig_p),
  warnRefPix_p(other.warnRefPix_p),
  warnRefVal_p(other.warnRefVal_p),
  warnInc_p(other.warnInc_p),
  warnTab_p(other.warnTab_p),
  isContig_p(other.isContig_p),
  pixelValues_p(other.pixelValues_p.copy()),
  worldValues_p(other.worldValues_p.copy()),
  originalAxisType_p(other.originalAxisType_p)
{
  isImage_p.resize(other.isImage_p.nelements());
  isImage_p = other.isImage_p;
}

template<class T>
ImageConcat<T>::~ImageConcat()
{}

template<class T>
ImageConcat<T>& ImageConcat<T>::operator= (const ImageConcat<T>& other)
{
  if (this != &other) {
     ImageInterface<T>::operator= (other);
     latticeConcat_p = other.latticeConcat_p;
     warnAxisNames_p  = other.warnAxisNames_p;
     warnAxisUnits_p  = other.warnAxisUnits_p;
     warnImageUnits_p = other.warnImageUnits_p;
     warnContig_p = other.warnContig_p;
     warnRefPix_p = other.warnRefPix_p;
     warnRefVal_p = other.warnRefVal_p;
     warnInc_p = other.warnInc_p;
     warnTab_p = other.warnTab_p;
     isContig_p = other.isContig_p;
     isImage_p.resize(other.isImage_p.nelements());
     isImage_p = other.isImage_p;
     pixelValues_p.resize(other.pixelValues_p.nelements());
     pixelValues_p = other.pixelValues_p;
     worldValues_p.resize(other.worldValues_p.nelements());
     worldValues_p = other.worldValues_p;
     originalAxisType_p = other.originalAxisType_p;
  }
  return *this;
}

template<class T>
ImageInterface<T>* ImageConcat<T>::cloneII() const
{
  return new ImageConcat(*this);
}

template<class T>
String ImageConcat<T>::imageType() const
{
  return "ImageConcat";
}


// Public functions
template<class T>
void ImageConcat<T>::setImage (ImageInterface<T>& image, Bool relax)
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
// The ImageInfo comes from the first image only. 
// The miscInfo is merged from all images.

   isImage_p.resize(nIm+1,True);
   isImage_p(nIm) = True;
   if (nIm==0) {
      ImageInterface<T>::setCoordinateInfo(image.coordinates());
      this->setUnitMember (image.units());
      this->setImageInfoMember (image.imageInfo());
      setMiscInfoMember (image.miscInfo());
      setCoordinates();
   } else {
      TableRecord rec = miscInfo();
      rec.merge (image.miscInfo(), RecordInterface::RenameDuplicates);
      setMiscInfoMember (rec);

// Compare the coordinates of this image with the current private coordinates

      const CoordinateSystem& cSys0 = coordinates();
      const CoordinateSystem& cSys = image.coordinates();
      if (cSys.nCoordinates() != cSys0.nCoordinates()) {
         os << "Images have inconsistent numbers of coordinates" 
            << LogIO::EXCEPTION;
      }
//
      Int coord0, axisInCoordinate0;
      Int coord, axisInCoordinate;
      cSys0.findPixelAxis (coord0, axisInCoordinate0,
                           latticeConcat_p.axis());
      cSys.findPixelAxis(coord, axisInCoordinate, latticeConcat_p.axis());
      if (coord0<0 || coord<0) {
         os << "Pixel axis has been removed for concatenation axis" << LogIO::EXCEPTION;
      }
      if (cSys.pixelAxisToWorldAxis(latticeConcat_p.axis())<0 ||
          coordinates().pixelAxisToWorldAxis(latticeConcat_p.axis())<0) {
         os << "World axis has been removed for concatenation axis" << LogIO::EXCEPTION;
      }

// This could be cleverer.  E.g. allow some mixed types (Tabular/Linear)
// Because the CoordinateSystem may change (e.g. -> Tabular) we hang onto
// the Coordinate type of the original coordinate system for the first image

      if (cSys.type(coord0)!=originalAxisType_p) {
         os << "Coordinate types for concatenation axis are inconsistent" << LogIO::EXCEPTION;
      }
//
      if (!allEQ(cSys.worldAxisNames(), cSys0.worldAxisNames())) {
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
//
      if (!allEQ(cSys.worldAxisUnits(),cSys0.worldAxisUnits())) {
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
//
      if (image.units() != units() && warnImageUnits_p) {
         os << LogIO::WARN
            << "Image units differ" << LogIO::POST;
         warnImageUnits_p = False;
      }

// Compare coordinates at end of last image and start of new image

      if (latticeConcat_p.isTempClose()) latticeConcat_p.reopen(nIm-1);
      const ImageInterface<T>* pImLast = dynamic_cast<const ImageInterface<T>*>(latticeConcat_p.lattice(nIm-1));
      const CoordinateSystem& cSysLast = pImLast->coordinates();
      if (latticeConcat_p.isTempClose()) latticeConcat_p.tempClose(nIm-1);
      checkContiguous (isContig_p, warnContig_p, pImLast->shape(), cSysLast, cSys, 
                       os, latticeConcat_p.axis(), relax);

// Compare coordinate descriptors not on concatenation axis

      checkNonConcatAxisCoordinates (warnRefPix_p, warnRefVal_p, warnInc_p, 
                                     os, image, relax);

// Update the coordinates in the ImageCOncat object now we are happy all is well

      setCoordinates();
   }

// Add parent history

   logger().addParent (image.logger());
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


// Public non-virtual over-ridden functions from ImageInterface



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
void ImageConcat<T>::doPutSlice (const Array<T>& buffer,
				 const IPosition& where,
                                 const IPosition& stride)
{
   latticeConcat_p.doPutSlice(buffer, where, stride);
}

template <class T>
Bool ImageConcat<T>::doGetMaskSlice (Array<Bool>& buffer,
				     const Slicer& section)
{
   return latticeConcat_p.doGetMaskSlice (buffer, section);
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
LatticeIterInterface<T>* ImageConcat<T>::makeIter (const LatticeNavigator& nav,
						   Bool useRef) const
{
  return latticeConcat_p.makeIter(nav, useRef);
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

// For Stokes axis we must do something different, because you can't
// convert pixel -1 to Stokes.  Bloody Stokes.  coord already checked
// to be consistent

   Int coord, axisInCoordinate;
   cSys2.findPixelAxis(coord, axisInCoordinate, axis);
   if (cSys2.type(coord)==Coordinate::STOKES) {

// See if we can make a Stokes coordinate from all the previous
// Stokes and the new Stokes.  If we can, its ok

      Vector<Int> stokes = makeNewStokes(coordinates().stokesCoordinate(coord).stokes(),
                                         cSys2.stokesCoordinate(coord).stokes());
//
      if (stokes.nelements()==0) {
         if (relax) {
            if (warnContig) {
               os << LogIO::WARN
                  << "Images are not contiguous along the concatenation axis" << endl;
               os << "For this axis, a non-regular TabularCoordinate will be made"
                  << LogIO::POST;
               warnContig = False;
            }
         } else {
           os << "Images are not contiguous along the concatenation axis - consider relax=T" 
              << LogIO::EXCEPTION;
         }
         isContig = False;
      }
   } else {
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
           os << "Images are not contiguous along the concatenation axis - consider relax=T" 
              << LogIO::EXCEPTION;
         }
         isContig = False;
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
      os << "Coordinate conversion failed because " << cSys.errorMessage() << LogIO::EXCEPTION;
   }
   worldAxis = cSys.pixelAxisToWorldAxis(axis);
   if (worldAxis==-1) {
     os << "Concatenation pixel axis has no world axis" << LogIO::EXCEPTION;
   } 
   return world(worldAxis);
}



template<class T>
void ImageConcat<T>::checkNonConcatAxisCoordinates (Bool& warnRefPix, Bool& warnRefVal,
                                                    Bool& warnInc, LogIO& os, 
                                                    const ImageInterface<T>& imageIn,
                                                    Bool relax)
//  
// Check coordinate descriptors for each non-concatenation axis
// for the current image being set and the coordinates currently
// set. The ImageSummary objects gives us the descriptors in pixel
// axis order
{
   const uInt axis = latticeConcat_p.axis();
   ImageSummary<T> sumIn(imageIn);
//
   if (latticeConcat_p.isTempClose()) latticeConcat_p.reopen(0);
   const ImageInterface<T>* pIm0 = 
     dynamic_cast<const ImageInterface<T>*>(latticeConcat_p.lattice(0));
   ImageSummary<T> sum0(*pIm0);
   if (latticeConcat_p.isTempClose()) latticeConcat_p.tempClose(0);
//
   Bool pixelOrder = True;
   const uInt dim = sumIn.ndim();
   Vector<Double> refPix = sumIn.referencePixels();
   Vector<Double> refPix0 = sum0.referencePixels();
   Vector<Double> refVal = sumIn.referenceValues(pixelOrder);
   Vector<Double> refVal0 = sum0.referenceValues(pixelOrder);
   Vector<Double> inc = sumIn.axisIncrements(pixelOrder);
   Vector<Double> inc0 = sum0.axisIncrements(pixelOrder);
//
   for (uInt j=0; j<dim; j++) {
      if (j!= axis) {
         if (!near(refPix(j), refPix0(j))) {
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
         if (!near(refVal(j), refVal0(j))) {
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
         if (!near(inc(j), inc0(j))) {
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
//
// Updates the CoordinateSystem in the ImageConcat image. The first lattice must 
// be an image.  The first lattice is contiguous by definition.  The Coordinate 
// System for the first image must be set before calling this function. For
// the first image, this function just sets up worldValues and pixelValues
// 
// 
{
    LogIO os(LogOrigin("ImageConcat", "setCoordinates(...)", WHERE));

// If the images are not contiguous along the concatenation axis,
// make an irregular TabularCoordinate.  As usual Stokes demands
// different handling

   CoordinateSystem cSys = coordinates();
   const uInt axis = latticeConcat_p.axis();
//
   Int coord, axisInCoord;
   cSys.findPixelAxis(coord, axisInCoord,  axis);
//
   const uInt nIm = latticeConcat_p.nlattices();
   const uInt iIm = nIm - 1;
//
   const uInt shapeNew = latticeConcat_p.lattice(iIm)->shape()(axis);
//
   Vector<Int> stokes;
//
   if (iIm==0) {

// Must set the worldValues and pixelValues for the first
// image.  The first image is always contiguous.

      pixelValues_p.resize(shapeNew);
      worldValues_p.resize(shapeNew);
//
      Vector<Double> p = cSys.referencePixel();
      Vector<Double> w = cSys.referenceValue();
//
      Int worldAxis = cSys.pixelAxisToWorldAxis(axis);
      for (uInt j=0; j<shapeNew; j++) {        
         p(axis) = Double(j);
         if (cSys.toWorld(w, p)) {
            pixelValues_p(j) = p(axis);
            worldValues_p(j) = w(worldAxis);
         } else {
            throw(AipsError(String("Coordinate conversion failed because")+cSys.errorMessage()));        
         }
      }

// Hang on to the type of coordinate of the concat axis for the first image 

      originalAxisType_p = cSys.coordinate(coord).type();
      return;
   }
//
   if (isContig_p) {
      if (latticeConcat_p.isTempClose()) latticeConcat_p.reopen(iIm);
      if (cSys.type(coord)==Coordinate::STOKES) {
         if (isImage_p(iIm)) {
            const ImageInterface<T>* pIm = dynamic_cast<const ImageInterface<T>*>(latticeConcat_p.lattice(iIm));
            stokes = makeNewStokes(cSys.stokesCoordinate(coord).stokes(),
                                   pIm->coordinates().stokesCoordinate(coord).stokes());
         } else {

// This is unlikely to work.  We make a Stokes axis starting from the
// last Stokes already in coordinates() + 1.  WIll only work
// if results in a useable Stokes axis
                    
            Vector<Int> stokes1 = coordinates().stokesCoordinate(coord).stokes();
            Int last = stokes1(stokes1.nelements()-1);
            const uInt shape = latticeConcat_p.lattice(nIm-1)->shape()(axis);
            Vector<Int> stokes2 (shape,0);
            indgen(stokes2, last+1, 1);
            stokes = makeNewStokes(stokes1, stokes2);
         }

// If Stokes ok, make new StokesCoordinate, replace it and set it

        if (stokes.nelements()==0) {
            os << "Cannot concatenate this Lattice with previous images as concatenation" << endl;
            os << "axis is Stokes and result would be illegal" << LogIO::EXCEPTION;
         } else {
            StokesCoordinate tmp(stokes);
            cSys.replaceCoordinate(tmp, uInt(coord));
            if (!ImageInterface<T>::setCoordinateInfo(cSys)) {
               os << "Failed to save new CoordinateSystem with StokesCoordinate" << LogIO::EXCEPTION;
            }
         } 
      }
      if (latticeConcat_p.isTempClose()) latticeConcat_p.tempClose(iIm);
   } else {
      const uInt nPixelsOld = pixelValues_p.nelements();
      pixelValues_p.resize(nPixelsOld+shapeNew, True);
      worldValues_p.resize(nPixelsOld+shapeNew, True);
//
      if (isImage_p(iIm)) {
         if (latticeConcat_p.isTempClose()) latticeConcat_p.reopen(iIm);
         const ImageInterface<T>* pIm = 
            dynamic_cast<const ImageInterface<T>*>(latticeConcat_p.lattice(iIm));
         const CoordinateSystem& cSys2 = pIm->coordinates();
         if (latticeConcat_p.isTempClose()) latticeConcat_p.tempClose(iIm);
//
         Vector<Double> p = cSys2.referencePixel();
         Vector<Double> w = cSys2.referenceValue();
//
// For each pixel in concatenation axis for this image, find world 
// and pixel values 

         Int worldAxis = cSys2.pixelAxisToWorldAxis(axis);
         for (uInt j=0; j<shapeNew; j++) {        
            p(axis) = Double(j);
            if (cSys2.toWorld(w, p)) {
               pixelValues_p(nPixelsOld+j) = p(axis) + nPixelsOld;
               worldValues_p(nPixelsOld+j) = w(worldAxis);
            } else {
               throw(AipsError(String("Coordinate conversion failed because")+cSys2.errorMessage()));        
            }
         }
      } else {

// iIm = 0 will always be an image.  Make up world values for lattices based on interpolation

         Double winc;
         if (iIm==1) {
            winc = worldValues_p(0) / 10.0;
         } else {
            winc = worldValues_p(iIm-1)-worldValues_p(iIm-2);
         }
//
         Double ww = worldValues_p(iIm-1) + winc;
         for (uInt j=0; j<shapeNew; j++) {        
            pixelValues_p(nPixelsOld+j) = Double(j) + nPixelsOld;
            worldValues_p(nPixelsOld+j) = ww;
            ww += winc;
         }
      }

// The first lattice is enforced to be an image.  Always use its units and names

      String unit, name;
      Int worldAxis = cSys.pixelAxisToWorldAxis(axis);
      unit = cSys.worldAxisUnits()(worldAxis);
      name = cSys.worldAxisNames()(worldAxis);

// Make TabularCoordinate and replace it.    If it's not monotonic, we
// can't make the TC, so fall back to CS from first image

      Bool ok = True;
      String msg;
      try {
         TabularCoordinate tc(pixelValues_p, worldValues_p, unit, name);
         cSys.replaceCoordinate(tc, uInt(coord));
         if (!ImageInterface<T>::setCoordinateInfo(cSys)) {
            throw(AipsError("Failed to save new CoordinateSystem with TabularCoordinate"));        
         }
      } catch (AipsError x) {
         ok = False;
         msg = x.getMesg();
      } 
      if (!ok) {
         if (warnTab_p) {
            os << LogIO::WARN << "Could not create TabularCoordinate because " << msg << LogIO::POST;
            os << LogIO::WARN << "CoordinateSystem set to that of first image instead" << LogIO::POST;
            warnTab_p = False;
         }
      }
   }
} 


template <class T>
Vector<Int> ImageConcat<T>::makeNewStokes(const Vector<Int>& stokes1,
                                          const Vector<Int>& stokes2)
{
   Vector<Int> stokes = concatenateArray(stokes1, stokes2);
   Bool ok = True;
   try {
      StokesCoordinate tmp(stokes);
   } catch (AipsError x) {
      ok = False;
   } 
//
   if (ok) {
      return stokes;
   } else {
      Vector<Int> tmp;
      return tmp;
   }
}


template <class T>
Bool ImageConcat<T>::lock (FileLocker::LockType type, uInt nattempts) 
{
  return latticeConcat_p.lock(type, nattempts);
}
template <class T>
void ImageConcat<T>::unlock()
{
  latticeConcat_p.unlock();
}
template <class T>
Bool ImageConcat<T>::hasLock (FileLocker::LockType type) const
{
  return latticeConcat_p.hasLock(type);
}
template <class T>
void ImageConcat<T>::resync()
{
  latticeConcat_p.resync();
}
template <class T>
void ImageConcat<T>::flush()
{
  latticeConcat_p.flush();
}
template <class T>
void ImageConcat<T>::tempClose()
{
  latticeConcat_p.tempClose();
}
template <class T>
void ImageConcat<T>::reopen()
{
  latticeConcat_p.reopen();
}

template<class T>
String ImageConcat<T>::name (Bool) const
{
  return String("Concatenation :");
}

template<class T>
Bool ImageConcat<T>::isMasked() const
{
  return latticeConcat_p.isMasked();
}

template<class T>
Bool ImageConcat<T>::hasPixelMask() const
{
  return latticeConcat_p.hasPixelMask();
}

template<class T>
const Lattice<Bool>& ImageConcat<T>::pixelMask() const
{
  return latticeConcat_p.pixelMask();
}
template<class T>
Lattice<Bool>& ImageConcat<T>::pixelMask()
{
  return latticeConcat_p.pixelMask();
}
  
template<class T>
const LatticeRegion* ImageConcat<T>::getRegionPtr() const
{
  return latticeConcat_p.getRegionPtr();
}

template<class T>
Bool ImageConcat<T>::isWritable() const
{
  return latticeConcat_p.isWritable();
}
   
template<class T>
IPosition ImageConcat<T>::shape() const
{
  return latticeConcat_p.shape();
}

} //# NAMESPACE CASA - END

