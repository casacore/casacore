//# ImageSummary.cc:  list an image header
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef IMAGES_IMAGESUMMARY_TCC
#define IMAGES_IMAGESUMMARY_TCC
//
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/coordinates/Coordinates.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/casa/Utilities/ValType.h>

#include <casacore/casa/iomanip.h>
#include <casacore/casa/iostream.h>

#include <casacore/images/Images/ImageSummary.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
ImageSummary<T>::ImageSummary (const ImageInterface<T>& image)
: cSys_p(image.coordinates()),
  obsInfo_p(cSys_p.obsInfo()),
  imageInfo_p(image.imageInfo()),
  pImage_p(image.cloneII())
{}

template <class T> 
ImageSummary<T>::ImageSummary (const ImageSummary<T> &other)
: cSys_p(other.cSys_p),
  obsInfo_p(other.obsInfo_p),
  imageInfo_p(other.imageInfo_p),
  pImage_p(other.pImage_p->cloneII())
{}

template <class T> 
ImageSummary<T>::~ImageSummary ()
{ 
   delete pImage_p;
}

template <class T>
ImageSummary<T> &ImageSummary<T>::operator=(const ImageSummary<T> &other)
// 
// Assignment operator
//
{
   if (this != &other) {
      cSys_p = other.cSys_p;
      obsInfo_p = other.obsInfo_p;
      imageInfo_p = other.imageInfo_p;
      delete pImage_p;
      pImage_p = other.pImage_p->cloneII();
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
   return pImage_p->niceCursorShape();
}



template <class T> 
Vector<String> ImageSummary<T>::axisNames (Bool pixelOrder) const
{
   Vector<String> tmp(cSys_p.worldAxisNames());
   if (!pixelOrder) return tmp.copy();
//
// Every pixel axs must have a world axis, so don't check for removal
//
   Vector<String> tmp2(cSys_p.nPixelAxes());
   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      tmp2(pixelAxis) = tmp(worldAxis); 
   }
   return tmp2;
}




template <class T> 
Vector<Double> ImageSummary<T>::referencePixels (Bool oneRel) const
// 
// Get reference pixels for the pixel axes
//
{
   Vector<Double> off(cSys_p.nPixelAxes(),0.0);
   if (oneRel) off.set(1.0);
   return cSys_p.referencePixel().copy()+off;
}


template <class T> 
Vector<Double> ImageSummary<T>::referenceValues (Bool pixelOrder) const
{
   Vector<Double> tmp(cSys_p.referenceValue());
   if (!pixelOrder) return tmp.copy();
//
// Every pixel axs must have a world axis, so don't check for removal
//
   Vector<Double> tmp2(cSys_p.nPixelAxes());
   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      tmp2(pixelAxis) = tmp(worldAxis); 
   }
   return tmp2;
}


template <class T> 
Vector<Double> ImageSummary<T>::axisIncrements (Bool pixelOrder) const
{ 
   Vector<Double> tmp(cSys_p.increment());
   if (!pixelOrder) return tmp.copy();
//
// Every pixel axs must have a world axis, so don't check for removal
//
   Vector<Double> tmp2(cSys_p.nPixelAxes());
   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      tmp2(pixelAxis) = tmp(worldAxis);
   }
   return tmp2;
}



template <class T> 
Vector<String> ImageSummary<T>::axisUnits (Bool pixelOrder) const
{ 
   Vector<String> tmp(cSys_p.worldAxisUnits());
   if (!pixelOrder) return tmp.copy();
//
// Every pixel axs must have a world axis, so don't check for removal
//
   Vector<String> tmp2(cSys_p.nPixelAxes());
   for (uInt pixelAxis=0; pixelAxis<cSys_p.nPixelAxes(); pixelAxis++) {
      Int worldAxis = cSys_p.pixelAxisToWorldAxis(pixelAxis);
      tmp2(pixelAxis) = tmp(worldAxis);
   }
   return tmp2;
}

template <class T> 
Unit ImageSummary<T>::units () const
{
   return pImage_p->units();
}


template <class T> 
String ImageSummary<T>::name () const
{
   const Bool stripPath = True;
   String name = pImage_p->name(stripPath);
   if (name.length()==0) {
      name = String("Temporary_image");
   }
   return name;
}


template <class T> 
String ImageSummary<T>::observer() const
{
   return obsInfo_p.observer();
}


template <class T> 
String ImageSummary<T>::obsDate(MEpoch& epoch) const
{
   epoch = obsInfo_p.obsDate();
   MVTime time = MVTime(epoch.getValue());
   return time.string(MVTime::YMD);   
}


template <class T> 
String ImageSummary<T>::telescope() const
{
   return obsInfo_p.telescope();
}

template <class T> 
Bool ImageSummary<T>::restFrequency(String& restFreqString, 
                                    Quantum<Double>& restFreq) const
{
   Bool ok = False;
   Int spectralAxis = CoordinateUtil::findSpectralAxis(cSys_p);
   if (spectralAxis >= 0) {
      Int coordinate, axisInCoordinate;
      cSys_p.findPixelAxis (coordinate, axisInCoordinate, spectralAxis);
//
      Double rf = cSys_p.spectralCoordinate(coordinate).restFrequency();
      if (rf > 0.0) {
         restFreq.setValue(rf);
         restFreq.setUnit(cSys_p.spectralCoordinate(coordinate).worldAxisUnits()(axisInCoordinate));
         ok = True;
      }
   }
   if (ok) {
      ostringstream oss;
//      oss.output().setf(ios::scientific, ios::floatfield);
//      oss.output().precision(8);
      oss << restFreq << endl;
      restFreqString= String(oss);
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
Bool ImageSummary<T>::directionSystem(String& dirTypeString, 
                                      MDirection::Types& dirType) const
{
   Bool ok;
   Vector<Int> pixelAxes, worldAxes;
   Int coordinate;
   CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys_p);
   if (coordinate >= 0) {
      ok = True;
      dirType = cSys_p.directionCoordinate(uInt(coordinate)).directionType();
      dirTypeString = MDirection::showType(dirType);
   } else {
      ok = False;
      dirTypeString = "";
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
Vector<String> ImageSummary<T>::maskNames() const
{
   return pImage_p->regionNames(RegionHandler::Masks);
}
   
template <class T> 
Vector<String> ImageSummary<T>::regionNames() const
{
   return pImage_p->regionNames(RegionHandler::Regions);
}
   
template <class T> 
String ImageSummary<T>::defaultMaskName() const
{
   return pImage_p->getDefaultMask();
}

template <class T> 
String ImageSummary<T>::imageType  () const
{
   return pImage_p->imageType();
}

template <class T> 
Vector<String> ImageSummary<T>::list (
	LogIO& os, const MDoppler::Types velocityType,
	Bool postLocally, const Bool verbose
) {
   os << LogIO::NORMAL << endl;
   MEpoch epoch;
   obsDate(epoch);

// List random things

   os << "Image name       : " << name() << endl;
   os << "Object name      : " << imageInfo_p.objectName() << endl;
   os << "Image type       : " << imageType() << endl;
   os << "Image quantity   : " << ImageInfo::imageType(imageInfo_p.imageType()) << endl;
//
   String list = makeMasksString();
   os << "Pixel mask(s)    : " << list << endl;
//
   list = makeRegionsString();
   os << "Region(s)        : " << list << endl;
//
   if (!units().getName().empty()) {
      os << "Image units      : " << this->units().getName() << endl;
   }

// Restoring beam

   if ( imageInfo_p.hasBeam()) {
	   if (imageInfo_p.hasSingleBeam()) {
		   GaussianBeam rb = imageInfo_p.restoringBeam();
		   Quantity majAx = rb.getMajor();
		   majAx.convert("deg");
		   Quantity minAx = rb.getMinor();
		   minAx.convert("deg");
		   if (majAx.getValue()<1.0 || minAx.getValue()<1.0) {
			   majAx.convert(Unit("arcsec"));
			   minAx.convert(Unit("arcsec"));
		   }
		   Quantity pa = rb.getPA(True);
		   pa.convert(Unit("deg"));
		   os.output() << "Restoring Beam   : " << majAx
			   << ", " << minAx << ", " << pa << endl;
	   }
	   else {
		   imageInfo_p.getBeamSet().summarize(
				   os, verbose, pImage_p->coordinates()
		   );
	   }
   }

   if (postLocally) {
      os.postLocally();
   } else {
      os.post();
   }

// List CoordinateSystem.  The messages that were posted locally will
// be still be stored in the sink and this function will fish them out.

   const Vector<String>& messages = cSys_p.list(os, velocityType, shape(), tileShape(), postLocally);
   return messages;
}

template <class T> 
Bool ImageSummary<T>::setNewImage (const ImageInterface<T>& image) {
	// FIXME this should be done using shared pointers
	pImage_p = &image;
	return True;
}
         
template <class T> 
String ImageSummary<T>::makeMasksString() const
{
   const String defaultMask = defaultMaskName();
   const Vector<String> masks = maskNames();
   const uInt nMasks = masks.nelements();
   if (nMasks==0) {
      if (hasAMask()) {
         return String("Parent is masked");
      } else {
         return String("None");
      }
   }
//
   ostringstream oss;
   if (!defaultMask.empty()) {
      oss << defaultMask;
      if (nMasks==1) {
         return String(oss);
      }
   }
//
   oss << " [";
   uInt j = 0;
   for (uInt i=0; i<nMasks; i++) {
      if (masks(i) != defaultMask) {
         if (j > 0) {
	    oss << ", ";
         }
	 oss << masks(i);
         j++;
      }
   } 
   oss << "]";
   return String(oss);
}


template <class T> 
String ImageSummary<T>::makeRegionsString() const
{
   const Vector<String> regions = regionNames();
   const uInt nRegions = regions.nelements();
   if (nRegions==0) return String("None");
//
   ostringstream oss;
   uInt j=0;
   for (uInt i=0; i<nRegions; i++) {
      if (j > 0) {
         oss << ", ";
      }
      oss << regions(i);
      j++;
   } 
   return String(oss);
}

} //# NAMESPACE CASACORE - END


#endif
