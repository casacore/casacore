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

#ifndef IMAGES_IMAGECONCAT_TCC
#define IMAGES_IMAGECONCAT_TCC


#include <casacore/images/Images/ImageConcat.h>
#include <casacore/images/Images/ImageOpener.h>

#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/OS/Path.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/images/Images/ImageSummary.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/LELImageCoord.h>
#include <casacore/lattices/Lattices/LatticeConcat.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LEL/LELCoordinates.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
  fileName_p(other.fileName_p),
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
     fileName_p = other.fileName_p;
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
ImageConcat<T>::ImageConcat (AipsIO& aio, const String& fileName)
: latticeConcat_p(),
  warnAxisNames_p(True),
  warnAxisUnits_p(True),
  warnImageUnits_p(True),
  warnContig_p(True),
  warnRefPix_p(True),
  warnRefVal_p(True),
  warnInc_p(True),
  warnTab_p(True),
  isContig_p(True),
  fileName_p(fileName)
{
  // This must be the opposite of function save.
  AlwaysAssert (aio.getstart ("ImageConcat") == 1, AipsError);
  uInt axis, nlatt;
  Bool tmpClose;
  String name;
  aio >> axis >> tmpClose >> nlatt;
  latticeConcat_p.setTempClose (tmpClose);
  for (uInt i=0; i<nlatt; ++i) {
    aio >> name;
    LatticeBase* latt = ImageOpener::openImage (name);
    ImageInterface<T>* img = dynamic_cast<ImageInterface<T>*>(latt);
    if (img == 0) {
      delete latt;
      throw AipsError ("ImageConcat " + fileName +
                       " contains image " + name +
                       " of another data type");
    }
    setImage (*img, True);
    delete img;
  }
  aio.getend();
}

template<class T>
void ImageConcat<T>::save (const String& fileName) const
{
  // Check that all images used are persistent.
  for (uInt i=0; i<latticeConcat_p.nlattices(); ++i) {
    if (! latticeConcat_p.lattice(i)->isPersistent()) {
      throw AipsError ("ImageConcat cannot be made persistent, because one of "
                       "its images is not persistent");
    }
  }
  // Create the AipsIO file.
  AipsIO aio(fileName, ByteIO::New);
  // Start with a general header (used by ImageOpener)
  // and the data type of the image.
  aio.putstart ("CompoundImage-Conc", 0);
  aio << Int(this->dataType());
  // Now save all relevant info.
  aio.putstart ("ImageConcat", 1);
  aio << latticeConcat_p.axis() << latticeConcat_p.isTempClose();
  aio << latticeConcat_p.nlattices();
  for (uInt i=0; i<latticeConcat_p.nlattices(); ++i) {
    aio << latticeConcat_p.lattice(i)->name(False);
  }
  aio.putend();
  aio.putend();
  fileName_p = fileName;
}

template<class T>
Bool ImageConcat<T>::isPersistent() const
{
  return ! fileName_p.empty();
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
  LogIO os(LogOrigin("ImageConcat", __func__, WHERE));
  // How many images have we set so far ?
  const uInt nIm = latticeConcat_p.nlattices();
  IPosition oldShape = nIm > 0 ? this->shape() : IPosition();

  // LatticeConcat allows the dimensionality to increase by
  // one, but ImageConcat can't do that yet - so an extra
  // test here.
  if (latticeConcat_p.axis() >= image.ndim()) {
    throw(AipsError("Axis number and image dimension are inconsistent"));
  }

  // Do Lattice relevant things. This does shape checks and
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
    this->setImageInfo (image.imageInfo());
    this->setMiscInfoMember (image.miscInfo());
    this->setCoordinates();
  } else {
    TableRecord rec = miscInfo();
    rec.merge (image.miscInfo(), RecordInterface::RenameDuplicates);
    this->setMiscInfoMember (rec);

    // Combine the beams if possible.
    // Should be done before the coordinates are merged.
    this->rwImageInfo().combineBeams (image.imageInfo(),
                                      oldShape, image.shape(),
                                      this->coordinates(), image.coordinates(),
                                      latticeConcat_p.axis(), relax, os);
    // Compare the coordinates of this image with the current private
    // coordinates
    const CoordinateSystem& cSys0 = coordinates();
    const CoordinateSystem& cSys = image.coordinates();
    ThrowIf(
    	cSys.nCoordinates() != cSys0.nCoordinates(),
    	"Images have inconsistent numbers of coordinates"
    );
    Int coord0, axisInCoordinate0;
    Int coord, axisInCoordinate;
    cSys0.findPixelAxis (coord0, axisInCoordinate0,
                         latticeConcat_p.axis());
    cSys.findPixelAxis(coord, axisInCoordinate, latticeConcat_p.axis());
    ThrowIf(
    	coord0<0 || coord<0,
    	"Pixel axis has been removed for concatenation axis"
    );
    ThrowIf(
    	cSys.pixelAxisToWorldAxis(latticeConcat_p.axis()) < 0
    	|| coordinates().pixelAxisToWorldAxis(latticeConcat_p.axis()) < 0,
    	"World axis has been removed for concatenation axis"
    );

    // This could be cleverer.  E.g. allow some mixed types (Tabular/Linear)
    // Because the CoordinateSystem may change (e.g. -> Tabular) we hang onto
    // the Coordinate type of the original coordinate system for the first image
    if (cSys.type(coord0)!=originalAxisType_p) {
      os << "Coordinate types for concatenation axis are inconsistent"
    	  << LogIO::EXCEPTION;
    }
    if (!allEQ(cSys.worldAxisNames(), cSys0.worldAxisNames())) {
      ImageInfo::logMessage (warnAxisNames_p, os, relax,
                             "Image axis names differ");
    }
    if (!allEQ(cSys.worldAxisUnits(),cSys0.worldAxisUnits())) {
      ImageInfo::logMessage (warnAxisUnits_p, os, relax,
                             "Image axis units differ");
    }
    if (image.units().getName() != this->units().getName()) {
      ImageInfo::logMessage (warnAxisUnits_p, os, True,
                             "Image units differ. "
                             "Image units of the first image (" +
                             this->units().getName() +
                             ") will be used for the output image");
    }

    // Compare coordinates at end of last image and start of new image
    if (latticeConcat_p.isTempClose()) latticeConcat_p.reopen(nIm-1);
    const ImageInterface<T>* pImLast =
      dynamic_cast<const ImageInterface<T>*>(latticeConcat_p.lattice(nIm-1));
    const CoordinateSystem& cSysLast = pImLast->coordinates();
    if (latticeConcat_p.isTempClose()) latticeConcat_p.tempClose(nIm-1);
    // once a lattice has been added that is not contiguous, the output coordinate
    // system will not be contiguous no matter what type of additional coordinate
    // systems are concatenated.
    if (isContig_p) {
    	_checkContiguous (
    		pImLast->shape(), cSysLast, cSys,
    		os, latticeConcat_p.axis(), relax
    	);
    }
    else {
    	ThrowIf(
    		! relax,
    		"A previously added image was not contiguous, so the only way"
    		"the current image may be added is if relax=True"
    	);
    }
    // Compare coordinate descriptors not on concatenation axis
    checkNonConcatAxisCoordinates (os, image, relax);

    // Update the coordinates in the ImageConcat object now we are happy
    // all is well
    this->setCoordinates();
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
   this->setCoordinates();
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
void ImageConcat<T>::_checkContiguous (
	const IPosition& shape1,
	const CoordinateSystem& cSys1,
	const CoordinateSystem& cSys2,
	LogIO& os, uInt axis, Bool relax
) {
		//
		// cSys1 from last image
		// cSys2 from current image
		//
		// Find out the coordinate of the concatenation axis at the location
		// of the last pixel from the previous image and compare.
		//

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

		if (stokes.nelements()==0) {
			String coordType = cSys1.spectralAxisNumber() == (Int)axis
					? "Spectral" : "Tabular";
			ImageInfo::logMessage (warnContig_p, os, relax,
					"Images are not contiguous along the "
					"concatenation axis",
					"For this axis, a non-regular " +
					coordType + " coordinate will be made");
			isContig_p = False;
		}
	}
	else {
		Int worldAxis;
		Double axisVal1 = coordConvert(worldAxis, os, cSys1, axis, Double(shape1(axis)-1));
		Double axisVal2 = coordConvert(worldAxis, os, cSys2, axis, Double(-1.0));

		Double inc = cSys1.increment()(worldAxis);
		if (abs(axisVal2-axisVal1) > 0.01*abs(inc)) {
			String coordType = cSys1.spectralAxisNumber() == (Int)axis
					? "Spectral" : "Tabular";
			ImageInfo::logMessage (warnContig_p, os, relax,
					"Images are not contiguous along the "
					"concatenation axis",
					"For this axis, a non-regular " +
					coordType + " coordinate will be made");
			isContig_p = False;
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
void ImageConcat<T>::checkNonConcatAxisCoordinates (LogIO& os,
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
           ImageInfo::logMessage (warnRefPix_p, os, relax,
                                  "Image reference pixels are different on "
                                  "non-concatenation axis " +
                                  String::toString(j+1));
         }
         if (!near(refVal(j), refVal0(j))) {
           ImageInfo::logMessage (warnRefVal_p, os, relax,
                                  "Image reference values are different on "
                                  "non-concatenation axis " +
                                  String::toString(j+1));
         }
         if (!near(inc(j), inc0(j))) {
           ImageInfo::logMessage (warnInc_p, os, relax,
                                  "Image increments are different on "
                                  "non-concatenation axis " +
                                  String::toString(j+1));
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
    LogIO os(LogOrigin("ImageConcat", __func__, WHERE));

// If the images are not contiguous along the concatenation axis,
// make an irregular TabularCoordinate.  As usual Stokes demands
// different handling

    CoordinateSystem cSys = coordinates();
    const uInt axis = latticeConcat_p.axis();

    Int coord, axisInCoord;
    cSys.findPixelAxis(coord, axisInCoord,  axis);

    const uInt nIm = latticeConcat_p.nlattices();
    const uInt iIm = nIm - 1;
    Vector<Int> stokes;
    // we always must update the world values, because even if
    // the currently added image is contiguous, the next image to be added might not
    // be
    _updatePixelAndWorldValues(iIm);

    if (iIm==0) {
    	// Hang on to the type of coordinate of the concat axis for the first image
    	originalAxisType_p = cSys.coordinate(coord).type();
    	return;
   }
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
   }
   else {
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
    	  if (originalAxisType_p == Coordinate::SPECTRAL) {
    		  SpectralCoordinate origSpCoord = cSys.spectralCoordinate();
    		  SpectralCoordinate newSp(
    				  origSpCoord.frequencySystem(False), worldValues_p,
    				  origSpCoord.restFrequency()
    		  );
    		  cSys.replaceCoordinate(newSp, uInt(coord));
    	  }
    	  else {
    		  TabularCoordinate tc(pixelValues_p, worldValues_p, unit, name);
    		  cSys.replaceCoordinate(tc, uInt(coord));

    	  }
    	  if (!ImageInterface<T>::setCoordinateInfo(cSys)) {
    		  String ctype = originalAxisType_p == Coordinate::SPECTRAL
    				  ? "Spectral" : "Tabular";
    		  os << "Failed to save new CoordinateSystem with "
    				  << ctype << "Coordinate" << LogIO::EXCEPTION;
    	  }
      }
      catch (const AipsError& x) {
         ok = False;
         msg = x.getMesg();
      } 
      if (!ok) {
    	  ImageInfo::logMessage (
    	      warnTab_p, os, True,
    	      "Could not create Coordinate because " + msg,
    	      "CoordinateSystem set to that of first image "
    	      "instead"
    	  );
      }
   }
} 

template <class T>
void ImageConcat<T>::_updatePixelAndWorldValues(uInt iIm) {
	const uInt nPixelsOld = pixelValues_p.nelements();
	uInt axis = latticeConcat_p.axis();
	const uInt shapeNew = latticeConcat_p.lattice(iIm)->shape()(axis);
	pixelValues_p.resize(nPixelsOld+shapeNew, True);
	worldValues_p.resize(nPixelsOld+shapeNew, True);
	if (isImage_p(iIm)) {
		if (latticeConcat_p.isTempClose()) {
			latticeConcat_p.reopen(iIm);
		}
		const ImageInterface<T>* pIm =
				dynamic_cast<const ImageInterface<T>*>(latticeConcat_p.lattice(iIm));
		const CoordinateSystem& cSys2 = pIm->coordinates();
		if (latticeConcat_p.isTempClose()) {
			latticeConcat_p.tempClose(iIm);
		}
		Vector<Double> p = cSys2.referencePixel();
		Vector<Double> w = cSys2.referenceValue();
		// For each pixel in concatenation axis for this image, find world
		// and pixel values

		Int worldAxis = cSys2.pixelAxisToWorldAxis(axis);
		for (uInt j=0; j<shapeNew; j++) {
			p(axis) = Double(j);
			if (cSys2.toWorld(w, p)) {
				pixelValues_p(nPixelsOld+j) = p(axis) + nPixelsOld;
				worldValues_p(nPixelsOld+j) = w(worldAxis);
			}
			else {
				ThrowCc(
					"Coordinate conversion failed because"
					+ cSys2.errorMessage()
				);
			}
		}
	}
	else {
		// iIm = 0 will always be an image.  Make up world values for lattices based on interpolation
		Double winc;
		if (iIm==1) {
			winc = worldValues_p(0) / 10.0;
		}
		else {
			winc = worldValues_p(iIm-1)-worldValues_p(iIm-2);
		}
		Double ww = worldValues_p(iIm-1) + winc;
		for (uInt j=0; j<shapeNew; j++) {
			pixelValues_p(nPixelsOld+j) = Double(j) + nPixelsOld;
			worldValues_p(nPixelsOld+j) = ww;
			ww += winc;
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
String ImageConcat<T>::name (Bool stripPath) const
{
  if (fileName_p.empty()) {
    return "Concatenation :";
  }
  Path path(fileName_p);
  if (!stripPath) {
    return path.absoluteName();
  } 
  return path.baseName();
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

} //# NAMESPACE CASACORE - END


#endif
