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
#include <trial/Images/ImageConvolver.h>
//
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/String.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/RegionHandler.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/ImageUtilities.h>
#include <aips/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeConvolver.h>
#include <trial/Lattices/LatticeUtilities.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <aips/iostream.h>


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
