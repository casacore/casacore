//# ImageTwoPtCorr.cc: Compute the two point correlation function of an image
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


#include <images/Images/ImageTwoPtCorr.h>

#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Utilities/Assert.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/ImageInfo.h>
#include <lattices/Lattices/LatticeTwoPtCorr.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> 
ImageTwoPtCorr<T>::ImageTwoPtCorr()
{}


template <class T> 
ImageTwoPtCorr<T>::~ImageTwoPtCorr()
{}

template <class T> 
ImageTwoPtCorr<T>::ImageTwoPtCorr(const ImageTwoPtCorr& other)
{}


template <class T> 
ImageTwoPtCorr<T>& ImageTwoPtCorr<T>::operator=(const ImageTwoPtCorr& other)
{
  if (this != &other) {
  }
  return *this;
}


template <class T> 
void ImageTwoPtCorr<T>::autoCorrelation (ImageInterface<T>& imOut,
                                         const ImageInterface<T>& imIn,
                                         const IPosition& axes, 
                                         typename LatticeTwoPtCorr<T>::Method method,
                                         Bool progress) const
{
   AlwaysAssert(imIn.shape().nelements()==imOut.shape().nelements(), AipsError);
   LogIO os(LogOrigin("ImageTwoPtCorr", "structureFunction(...)", WHERE));

// Deal with axes

   CoordinateSystem cSysIn = imIn.coordinates();
   IPosition axes2 = setUpAxes (axes, cSysIn);

// Compute

   LatticeTwoPtCorr<T> twoPt;
   twoPt.autoCorrelation (imOut, imIn, axes2, method, progress);

// Overwrite output Coordinate System

   setCoordinateSystem (imOut, imIn, axes2);

// Copy Miscellaneous stuff

   copyMiscellaneous(imOut, imIn);

// Set Output Units

   setUnit(imOut);
}

template <class T> 
void ImageTwoPtCorr<T>::autoCorrelation (ImageInterface<T>& imOut,
                                         const ImageInterface<T>& imIn, 
                                         typename LatticeTwoPtCorr<T>::Method method,
                                         Bool progress) const
{
   IPosition axes;
   autoCorrelation (imOut, imIn, axes, method, progress);
}

// Private functions


template <class T> 
void ImageTwoPtCorr<T>::copyMiscellaneous (ImageInterface<T>& out,
                                        const ImageInterface<T>& in) const
{
   out.setMiscInfo(in.miscInfo());
   out.setImageInfo(in.imageInfo());
   out.appendLog(in.logger());
}

template <class T> 
void ImageTwoPtCorr<T>::setUnit (ImageInterface<T>& im) const
{

// Set image type to undefined

   ImageInfo info = im.imageInfo();
   info.setImageType (ImageInfo::Undefined);
   im.setImageInfo(info);

// Set unit to 'count'

   im.setUnits (Unit("count"));
}

template <class T> 
void ImageTwoPtCorr<T>::setCoordinateSystem (ImageInterface<T>& out,
                                          const ImageInterface<T>& in,
                                          const IPosition& axes) const
//
// Set the centre of the image to be refval=0 and the reference pixel
// 
{
   CoordinateSystem cSys = in.coordinates();
   IPosition shape = out.shape();
//
   Bool holdsOne;
   if (CoordinateUtil::holdsSky (holdsOne, cSys, axes.asVector())) {

// Replace DC by a LC

      String error;
      Int dC;
      Vector<Int> pixelAxes, worldAxes;
      CoordinateUtil::findSky (error, dC, pixelAxes, worldAxes, cSys);
      DirectionCoordinate dCoord = cSys.directionCoordinate(dC);
//
      Vector<String> names(2);
      names(0) = String("Lag-") + dCoord.worldAxisNames()(0);
      names(1) = String("Lag-") + dCoord.worldAxisNames()(1);
//
      Vector<String> units(dCoord.worldAxisUnits().copy());
//
      Vector<Double> refVal(2);
      refVal = 0.0;
//
      Vector<Double> inc(dCoord.increment().copy());
      Matrix<Double> pc(2,2); 
      pc= 0.0; 
      pc.diagonal() = 1.0;
//
      Vector<Double> refPix(2);
      refPix(0) = (shape(pixelAxes(0))-1) / 2;       // shape is odd (n = 2*lmax+1)
      refPix(1) = (shape(pixelAxes(1))-1) / 2;
//
      LinearCoordinate lCoord (names, units, refVal, inc, pc, refPix);
      cSys.replaceCoordinate (lCoord, dC);
   } else {
      Vector<Double> refVal(cSys.referenceValue());    // Refererence 
      refVal(axes(0)) = 0.0;
      refVal(axes(1)) = 0.0;
//
      Vector<Double> refPix(cSys.referencePixel());      
      refPix(axes(0)) = (shape(axes(0))-1) / 2;       // shape is odd (n = 2*lmax+1)
      refPix(axes(1)) = (shape(axes(1))-1) / 2;
  }

// Set in output image

   out.setCoordinateInfo(cSys);
}


template <class T> 
IPosition ImageTwoPtCorr<T>::setUpAxes (const IPosition& axes, 
                                  const CoordinateSystem& cSys) 
{
   LogIO os(LogOrigin("ImageTwoPtCorr", "setUpAxes(...)", WHERE));
   IPosition axes2(2);

// Find the Sky

   String error;
   Int dC;
   Vector<Int> pixelAxes, worldAxes;
   Bool foundSky = CoordinateUtil::findSky(error, dC, pixelAxes, worldAxes, cSys);
//
   if (axes.nelements()==0) {
      os << LogIO::NORMAL << "Selected Sky axes" << LogIO::POST;
      if (foundSky) {
         axes2(0) = pixelAxes(0);
         axes2(1) = pixelAxes(1);
      } else {
         axes2(0) = 0;
         axes2(1) = 1;
      }
   } else if (axes.nelements()==2) {
      os << LogIO::NORMAL << "Selected first two axes" << LogIO::POST;
      axes2(0) = axes(0);
      axes2(1) = axes(1);
   } else {
      os << "The axes argument must be of length 0 or 2" << LogIO::EXCEPTION;
   }

// Make sure we have all of the sky.  We do this because it makes it
// much easier to replace the DC by a LC in the CoordinateSystem.
// In principle we can code around this limitation.

   if (axes.nelements()==2) {
     if (foundSky) {
       Bool bad = (axes2(0)==pixelAxes(0) && axes2(1)!=pixelAxes(1)) ||
                  (axes2(0)==pixelAxes(1) && axes2(1)!=pixelAxes(0));
       if (bad) {
          os << "You cannot specify just one of the DirectionCoordinate (sky) axes" << LogIO::EXCEPTION;
       }
     }
   }
//
   return axes2;
}

template <class T> 
IPosition ImageTwoPtCorr<T>::setUpShape (const IPosition& inShape, const IPosition& axes)
{
   return LatticeTwoPtCorr<T>::setUpShape (inShape, axes);
}


} //# NAMESPACE CASA - END

