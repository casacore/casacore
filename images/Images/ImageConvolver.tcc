//# ImageConvolver.cc:  convolution of an image by given Array
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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
//   
#include <images/Images/ImageConvolver.h>
//
#include <casa/aips.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/String.h>
#include <images/Images/PagedImage.h>
#include <images/Images/TempImage.h>
#include <images/Regions/RegionHandler.h>
#include <images/Regions/ImageRegion.h>
#include <images/Images/ImageUtilities.h>
#include <lattices/Lattices/ArrayLattice.h>
#include <lattices/Lattices/LatticeConvolver.h>
#include <lattices/Lattices/LatticeUtilities.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/LatticeExprNode.h>
#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> 
ImageConvolver<T>::ImageConvolver ()
{}

template <class T>
ImageConvolver<T>::ImageConvolver(const ImageConvolver<T> &other)
{
   operator=(other);
}

template <class T> 
ImageConvolver<T>::~ImageConvolver ()
{}


template <class T>
ImageConvolver<T> &ImageConvolver<T>::operator=(const ImageConvolver<T> &other)
{

// There is no state

   if (this != &other) {
   }
   return *this;
}


template <class T>
void ImageConvolver<T>::convolve(LogIO& os,  
                                 ImageInterface<T>& imageOut,
                                 ImageInterface<T>& imageIn,
                                 const ImageInterface<T>& kernel,
                                 ScaleTypes scaleType, Double scale,
                                 Bool copyMiscellaneous, Bool warnOnly)
{
// Check Coordinates

    checkCoordinates (os, imageIn.coordinates(), kernel.coordinates(),
                      warnOnly);

// Convolve

    convolve (os, imageOut, imageIn, kernel,
              scaleType, scale, copyMiscellaneous);
}

template <class T>
void ImageConvolver<T>::convolve(LogIO& os,  
                                 ImageInterface<T>& imageOut,
                                 ImageInterface<T>& imageIn,
                                 const Array<T>& kernel,
                                 ScaleTypes scaleType, Double scale,
                                 Bool copyMiscellaneous)
{
    ArrayLattice<T> kernelLattice(kernel);
    convolve (os, imageOut, imageIn, kernelLattice, 
              scaleType, scale, copyMiscellaneous);
}


template <class T>
void ImageConvolver<T>::convolve(LogIO& os,  
                                 ImageInterface<T>& imageOut,
                                 ImageInterface<T>& imageIn,
                                 const Lattice<T>& kernel,
                                 ScaleTypes scaleType, Double scale,
                                 Bool copyMiscellaneous)
{

// Check

   const IPosition& inShape = imageIn.shape();
   const IPosition& outShape = imageOut.shape();
   if (!inShape.isEqual(outShape)) {
      os << "Input and output images must have same shape" << LogIO::EXCEPTION;
   }
// 
    if (kernel.ndim() > imageIn.ndim()) {
        os << "Kernel lattice has more axes than the image!" << LogIO::EXCEPTION;
    }

// Add degenerate axes if needed

    Lattice<T>* pNewKernel = 0;
    LatticeUtilities::addDegenerateAxes (pNewKernel, kernel, inShape.nelements());

// Normalize kernel.  
  
    LatticeExprNode node;
    if (scaleType==AUTOSCALE) {
       node = LatticeExprNode((*pNewKernel) / sum(*pNewKernel));
    } else if (scaleType==SCALE) {
       T t = static_cast<T>(scale);
       node = LatticeExprNode(t * (*pNewKernel));
    } else if (scaleType==NONE) {
       node = LatticeExprNode(*pNewKernel);
    }
    LatticeExpr<T> kernelExpr(node);

// Create convolver

    LatticeConvolver<T> lc(kernelExpr, imageIn.shape(),  ConvEnums::LINEAR);
//
    if (imageIn.isMasked()) {

// Generate output mask if needed

      makeMask(imageOut, os);

// Copy input mask to output.  Copy input pixels
// and set to zero where masked

      LatticeUtilities::copyDataAndMask(os, imageOut, imageIn, True);

// Convolve in situ

      lc.convolve(imageOut);
    } else {

// Convolve to output

      lc.convolve(imageOut, imageIn);
    }

// Overwrite output CoordinateSystem 

   imageOut.setCoordinateInfo (imageIn.coordinates());

// Copy miscellaneous things across as required

    if (copyMiscellaneous) ImageUtilities::copyMiscellaneous(imageOut, imageIn);

// Delete the restoring beam (should really check that the beam is in the
// plane of convolution)

    ImageInfo ii = imageOut.imageInfo();
    ii.removeRestoringBeam();
    imageOut.setImageInfo (ii);

// Clean up

    delete pNewKernel;
}

template <class T>
void ImageConvolver<T>::makeMask(ImageInterface<T>& out, LogIO& os)  const
{
   if (out.canDefineRegion()) {   
    
// Generate mask name 
      
      String maskName = out.makeUniqueRegionName(String("mask"), 0);
    
// Make the mask if it does not exist
      
      if (!out.hasRegion (maskName, RegionHandler::Masks)) {
         out.makeMask(maskName, True, True, False, True);
         os << LogIO::NORMAL << "Created mask `" << maskName << "'" << LogIO::POST;
      }
   } else {
      os << LogIO::WARN << "Cannot make requested mask for this type of image" << endl;
   }
}


template <class T>
void ImageConvolver<T>::checkCoordinates (LogIO& os, const CoordinateSystem& cSysImage,
                                          const CoordinateSystem& cSysKernel,
                                          Bool warnOnly) const
{
   const uInt nPixelAxesK = cSysKernel.nPixelAxes();
   const uInt nPixelAxesI = cSysImage.nPixelAxes();
   if (nPixelAxesK > nPixelAxesI) {
        os << "Kernel has more pixel axes than the image" << LogIO::EXCEPTION;
    }
//
   const uInt nWorldAxesK = cSysKernel.nWorldAxes();
   const uInt nWorldAxesI = cSysImage.nWorldAxes();
   if (nWorldAxesK > nWorldAxesI) {
        os << "Kernel has more world axes than the image" << LogIO::EXCEPTION;
    }
//
   const Vector<Double>& incrI = cSysImage.increment();
   const Vector<Double>& incrK = cSysKernel.increment();
   const Vector<String>& unitI = cSysImage.worldAxisUnits();
   const Vector<String>& unitK = cSysKernel.worldAxisUnits();
//
   for (uInt i=0; i<nWorldAxesK; i++) {

// Compare Coordinate types and reference

      if (CoordinateUtil::findWorldAxis(cSysImage, i) != 
          CoordinateUtil::findWorldAxis(cSysKernel, i)) {
         if (warnOnly) {
            os << LogIO::WARN << "Coordinate types are not the same for axis " << i+1 << LogIO::POST;
         } else {
            os << "Coordinate types are not the same for axis " << i+1 << LogIO::EXCEPTION;
         }
      }

// Compare units 

      Unit u1(unitI[i]);
      Unit u2(unitK[i]);
      if (u1 != u2) {
         if (warnOnly) {
            os << LogIO::WARN << "Axis units are not consistent for axis " << i+1 << LogIO::POST;
         } else {
            os << "Axis units are not consistent for axis " << i+1 << LogIO::EXCEPTION;
         }
      }

// Compare increments ; this is not a very correct test as there may be
// values in the LinearTransform inside the coordinate.  Should really
// convert some values...  See how we go.

      Quantum<Double> q2(incrK[i], u2);
      Double val2 = q2.getValue(u1);
      if (!near(incrI[i], val2, 1.0e-6)) {
         if (warnOnly) {
            os << LogIO::WARN << "Axis increments are not consistent for axis " << i+1 << LogIO::POST;
         } else {
            os << "Axis increments are not consistent for axis " << i+1 << LogIO::EXCEPTION;
         } 
     }
   }
}

} //# NAMESPACE CASA - END

