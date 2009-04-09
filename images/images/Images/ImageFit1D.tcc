//# ImageFit1D.cc: Class to fit Spectral components to vectors in an image
//# Copyright (C) 2004
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
//#   $Id$

#include <images/Images/ImageFit1D.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Exceptions/Error.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/SubImage.h>
#include <images/Regions/ImageRegion.h>
#include <lattices/Lattices/LatticeUtilities.h>
#include <components/SpectralComponents/SpectralEstimate.h>
#include <components/SpectralComponents/SpectralElement.h>
#include <casa/Utilities/Assert.h>

namespace casa {

template <class T> 
ImageFit1D<T>::ImageFit1D()
 : itsImagePtr(0),
   itsWeightPtr(0),
   itsAxis(0)
{
   checkType();
}

template <class T> 
ImageFit1D<T>::ImageFit1D(const ImageInterface<T>& image, uInt pixelAxis)
 : itsImagePtr(0),
   itsWeightPtr(0),
   itsAxis(0)
{
   checkType();
   setImage(image, pixelAxis);
}

template <class T> 
ImageFit1D<T>::ImageFit1D(const ImageInterface<T>& image, 
                          const ImageInterface<T>& weights,
                          uInt pixelAxis)
 : itsImagePtr(0),
   itsWeightPtr(0),
   itsAxis(0)
{
   checkType();
   setImage(image, pixelAxis);
   setWeightsImage (weights);
}


template <class T> 
ImageFit1D<T>::ImageFit1D(const ImageFit1D<T>& other)
 : itsImagePtr(0),
   itsWeightPtr(0),
   itsAxis(0)
{
   checkType();
   copy(other);
}

template <class T> 
ImageFit1D<T>& ImageFit1D<T>::operator=(const ImageFit1D<T>& other)
{
  if (this != &other) {
     copy(other);
  }
  return *this;
}


template <class T> 
ImageFit1D<T>::~ImageFit1D()
{
   if (itsImagePtr) delete itsImagePtr;
   if (itsWeightPtr) delete itsWeightPtr;
}


template <class T> 
void ImageFit1D<T>::setImage (const ImageInterface<T>& image, uInt pixelAxis)
{
   AlwaysAssert(pixelAxis < image.ndim(), AipsError);
//
   itsImagePtr = image.cloneII();
   itsAxis = pixelAxis;
   itsCS = itsImagePtr->coordinates();
}


template <class T> 
void ImageFit1D<T>::setImage (const ImageInterface<T>& image,
                              const ImageInterface<T>& weights,
                              uInt pixelAxis)
{
   setImage(image, pixelAxis);
   setWeightsImage(weights);
}


template <class T> 
Bool ImageFit1D<T>::setData (const IPosition& pos, 
                             ImageFit1D<T>::AbcissaType abcissaType,
                             Bool doAbs)
{
   const uInt nDim = itsImagePtr->ndim();
   AlwaysAssert (pos.nelements()==nDim, AipsError);
//
   IPosition start(nDim);
   IPosition shape(itsImagePtr->shape());
   start(itsAxis) = 0;
   for (uInt i=0; i<nDim; i++) {
      if (i!=itsAxis) {
        start(i) = pos(i);
        shape(i) = 1;
      }
   }

// Get ordinate data

   Bool remDeg = True;
   Vector<T> y;
   y = itsImagePtr->getSlice(start, shape, remDeg);

// Mask

   Vector<Bool> mask;
   mask = itsImagePtr->getMaskSlice(start, shape, remDeg);

// Weights

   Vector<T> weights(y.nelements());
   weights = 1.0;
   if (itsWeightPtr) weights = itsWeightPtr->getSlice(start, shape, remDeg);

// Generate Abcissa

   Vector<Double> x;
   if (!makeAbcissa(x, abcissaType, doAbs)) return False;
  
// Set data in fitter; we need to use a Double fitter at present

   Vector<FitterType> y2(y.shape());
   convertArray(y2, y);
//
   Vector<Double> w2(weights.shape());
   convertArray(w2, weights);
//
   if (!itsFitter.setData (x, y2, mask, w2)) {
      itsError = itsFitter.errorMessage();
      return False;
   }
//
   return True;
}


template <class T> 
Bool ImageFit1D<T>::setData (const ImageRegion& region, 
                             ImageFit1D<T>::AbcissaType abcissaType,
                             Bool doAbs)
{

// Make SubImage

   const SubImage<T> subImage(*itsImagePtr, region, False);

// Average over non-profile axes 

   const uInt nDim = subImage.ndim();
   IPosition axes = IPosition::otherAxes(nDim, IPosition(1,itsAxis));
   Bool dropDeg = True;
//
   Vector<T> y;
   Vector<Bool> mask;
   LatticeUtilities::collapse (y, mask, axes, subImage, dropDeg);

// Weights

   Vector<T> weights(y.nelements());
   weights = 1.0;
   if (itsWeightPtr) {
      LatticeUtilities::collapse (weights, axes, *itsWeightPtr, dropDeg);
   }

// Generate Abcissa

   Vector<Double> x;
   if (!makeAbcissa(x, abcissaType, doAbs)) return False;
  
// Set data in fitter; we need to use a Double fitter at present

   Vector<FitterType> y2(y.shape());
   convertArray(y2, y);
//
   Vector<Double> w2(weights.shape());
   convertArray(w2, weights);
//
   if (!itsFitter.setData (x, y2, mask, w2)) {
      itsError = itsFitter.errorMessage();
      return False;
   }
// 
   return True;
}



template <class T> 
Bool ImageFit1D<T>::setGaussianElements (uInt nGauss)
{
   if (nGauss==0) return True;
//
   check();
//
   if (!itsFitter.setGaussianElements (nGauss)) {
      itsError = itsFitter.errorMessage();
      return False;
   }
//
   return True;
}


template <class T> 
Bool ImageFit1D<T>::fit ()
{
   check();
//
   return itsFitter.fit();
}

template <class T> 
Bool ImageFit1D<T>::setAbcissaState (String& errMsg, ImageFit1D<T>::AbcissaType& type,
                                     CoordinateSystem& cSys, const String& xUnit,
                                     const String& doppler, uInt pixelAxis)
{
   Unit unitKMS(String("km/s"));
//
   if (xUnit.contains(String("pix"))) {
      type = ImageFit1D<T>::PIXEL;
      return True;
   }

// Is the axis Spectral ?

   Int pAxis, wAxis, sCoord;
   CoordinateUtil::findSpectralAxis (pAxis, wAxis, sCoord, cSys);
   Bool isSpectral = (Int(pixelAxis)==pAxis);

// Defer unit making until now as 'pix' not a valid unit

   Bool ok(False);
   Unit unit(xUnit);
   if (unit==unitKMS && isSpectral) {
      ok = CoordinateUtil::setSpectralState (errMsg, cSys, xUnit, doppler);
      type = ImageFit1D<T>::VELOCITY;
   } else {
      Vector<String> units = cSys.worldAxisUnits().copy();
      units(pixelAxis) = xUnit;
      ok = cSys.setWorldAxisUnits(units);
      if (!ok) errMsg = cSys.errorMessage();
      type = ImageFit1D<T>::IM_NATIVE;
   }
//
   return ok;
}



// Private functions

template <class T> 
Bool ImageFit1D<T>::makeAbcissa (Vector<Double>& x, 
                                 ImageFit1D<T>::AbcissaType type, 
                                 Bool doAbs)
{
   const uInt n = itsImagePtr->shape()(itsAxis);
   x.resize(n);
//
   Double refPix = itsCS.referencePixel()(itsAxis);
   if (type==PIXEL) {
      indgen(x);
      if (!doAbs) x -= refPix;      
      return True;
   }

// Find the pixel axis

   Int coord, axisInCoord;
   itsCS.findPixelAxis (coord, axisInCoord, itsAxis);
//
   if (type==VELOCITY) {
      AlwaysAssert(itsCS.type(coord)==Coordinate::SPECTRAL, AipsError);
      const SpectralCoordinate& sCoord = itsCS.spectralCoordinate(coord);
      Double world;
      for (uInt i=0; i<n; i++) {
         if (!sCoord.pixelToVelocity (world, Double(i))) {
            itsError = sCoord.errorMessage();
            return False;
         } else {
            if (doAbs) {
               x[i] = world;
            } else {
               Double worldRefVal;
               sCoord.pixelToVelocity (worldRefVal, refPix);
               world -= worldRefVal;
            }
         }
      }
   } else if (type==IM_NATIVE) {
      const Coordinate& gCoord = itsCS.coordinate(coord);      
      Vector<Double> pixel(gCoord.referencePixel().copy());
      Vector<Double> world;
//
      for (uInt i=0; i<n; i++) {
         pixel(axisInCoord) = i;
         if (!gCoord.toWorld(world, pixel)) {
            itsError = gCoord.errorMessage();
            return False;
         }
//
         if (!doAbs) gCoord.makeWorldRelative(world);
         x[i] = world(axisInCoord);
      }
   } else {
      itsError = String("Unrecognized abcissa type");
      return False;
   }
//
   return True;
}


template <class T> 
void ImageFit1D<T>::check() const
{
   if (!itsImagePtr) {
      throw(AipsError("Image has not been set"));
   }
}


template <class T> 
void ImageFit1D<T>::setWeightsImage (const ImageInterface<T>& image)
{
   AlwaysAssert (itsImagePtr->shape().isEqual(image.shape()), AipsError);
   itsWeightPtr = image.cloneII();
}


template <class T> 
void ImageFit1D<T>::copy(const ImageFit1D<T>& other)
{

// Lattices are reference counted

   if (itsImagePtr) delete itsImagePtr;
   itsImagePtr = other.itsImagePtr->cloneII();
//
   if (itsWeightPtr) delete itsWeightPtr;
   if (other.itsWeightPtr) itsWeightPtr = other.itsWeightPtr->cloneII();

// These things are copies

   itsCS = other.itsCS;
   itsAxis = other.itsAxis;  
//
   itsFitter = other.itsFitter;
   itsError = other.itsError;
}


template <class T> 
void ImageFit1D<T>::checkType() const
//
// At this point, ProfileFitter and SpectralFitter
// take the *Same* template type for X and Y
// To avoid precision problems we do it all in Double
// at the moment.  Later X<T> and Y<T> can be separated
 //
{
   FitterType* p;
   DataType tp = whatType(p);
   AlwaysAssert(tp==TpDouble, AipsError);
}


template <class T> 
Vector<T> ImageFit1D<T>::getEstimate (Int which) const 
{
   Vector<FitterType> e = itsFitter.getEstimate(which);
   Vector<T> t(e.shape());
   convertArray (t, e);
   return t;
}


template <class T> 
Vector<T> ImageFit1D<T>::getFit (Int which) const 
{
   Vector<FitterType> f = itsFitter.getFit(which);
   Vector<T> t(f.shape());
   convertArray (t, f);
   return t;
}

template <class T> 
Vector<T> ImageFit1D<T>::getResidual(Int which, Bool fit) const 
{
   Vector<FitterType> r = itsFitter.getResidual(which, fit);
   Vector<T> t(r.shape());
   convertArray (t, r);
   return t;
}

} //#End casa namespace
