//# SepImageConvolver.cc:  separable convolution of an image
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
//   

#include <images/Images/SepImageConvolver.h>

#include <casa/aips.h>

#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Block.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <scimath/Mathematics/Convolver.h>
#include <casa/Quanta/UnitMap.h>
#include <casa/Quanta/Quantum.h>
#include <casa/BasicSL/String.h>

#include <coordinates/Coordinates/CoordinateSystem.h>
#include <images/Images/PagedImage.h>
#include <images/Regions/ImageRegion.h>
#include <images/Images/SubImage.h>
#include <images/Images/TempImage.h>
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/LatticeUtilities.h>
#include <lattices/Lattices/TiledLineStepper.h>
#include <scimath/Mathematics/VectorKernel.h>
#include <casa/System/ProgressMeter.h>

#include <casa/sstream.h>
#include <casa/iomanip.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> 
SepImageConvolver<T>::SepImageConvolver (ImageInterface<T>& image,
                                         LogIO &os, Bool showProgress)
: itsImagePtr(image.cloneII()),
  itsOs(os),
  itsAxes(0),
  itsVectorKernels(0),
  itsShowProgress(showProgress)
{
}

template <class T>
SepImageConvolver<T>::SepImageConvolver(const SepImageConvolver<T> &other)
: itsImagePtr(0)
{
   operator=(other);
}


template <class T> 
SepImageConvolver<T>::~SepImageConvolver ()
{
   delete itsImagePtr;
   itsImagePtr = 0;
   const uInt n = itsVectorKernels.nelements();
   for (uInt i=0; i<n; i++) {
      delete itsVectorKernels[i];
      itsVectorKernels[i] = 0;
   }
}


template <class T>
SepImageConvolver<T> &SepImageConvolver<T>::operator=(const SepImageConvolver<T> &other)
{
   if (this != &other) {
      if (itsImagePtr!=0) delete itsImagePtr;
      itsImagePtr = other.itsImagePtr->cloneII();
//
      itsOs = other.itsOs;
      itsAxes = other.itsAxes.copy();
      itsShowProgress = other.itsShowProgress;
//
      const uInt n = itsVectorKernels.nelements();
      for (uInt i=0; i<n; i++) {
         delete itsVectorKernels[i];
         itsVectorKernels[i] = 0;
      }
//
      const uInt n2 = other.itsVectorKernels.nelements();
      itsVectorKernels.resize(n2);
      for (uInt i=0; i<n2; i++) {      
         itsVectorKernels[i] = new Vector<T>((other.itsVectorKernels[i])->copy());
      }
   }
   return *this;
}



template <class T>
void SepImageConvolver<T>::setKernel(uInt axis, const Vector<T>& kernel)
{
   checkAxis(axis);
//
   uInt n = itsVectorKernels.nelements() + 1;
   itsVectorKernels.resize(n, True);
   itsVectorKernels[n-1] = new Vector<T>(kernel.copy());
   itsAxes.resize(n, True);
   itsAxes(n-1) = axis;
}

template <class T>
void SepImageConvolver<T>::setKernel(uInt axis, VectorKernel::KernelTypes kernelType,  
                                     const Quantum<Double>& width, Bool autoScale,
                                     Bool useImageShapeExactly, Double scale)
{
// Catch pixel units

   UnitMap::putUser("pix",UnitVal(1.0), "pixel units");
   String sunit = width.getFullUnit().getName();
   if (sunit==String("pix")) {
      setKernel (axis, kernelType, width.getValue(), autoScale, useImageShapeExactly, scale);
      itsOs.output() << "Axis " << axis+1<< " : setting width "  << width << endl;
      itsOs << LogIO::NORMAL;
      return;
   }
//
   checkAxis(axis);
//
// Convert width to pixels
//
   CoordinateSystem cSys = itsImagePtr->coordinates();
   Int worldAxis = cSys.pixelAxisToWorldAxis(axis);
   Double inc = cSys.increment()(worldAxis);
//
   Unit unit = Unit(cSys.worldAxisUnits()(worldAxis));
   if (width.getFullUnit()!=unit) {
      itsOs << "Specified width units (" << width.getUnit() 
            << ") are inconsistent with image axis unit (" 
            << unit.getName() << LogIO::EXCEPTION;
   }
   Double width2 = abs(width.getValue(unit)/inc);
//
   itsOs.output() << "Axis " << axis+1<< " : setting width "
         << width << " = " << width2 << " pixels" << endl;
   itsOs <<  LogIO::NORMAL;
   setKernel(axis, kernelType, width2, autoScale, useImageShapeExactly, scale);
}

template <class T>
void SepImageConvolver<T>::setKernel(uInt axis, VectorKernel::KernelTypes kernelType,  
                                     Double width, Bool autoScale, 
                                     Bool useImageShapeExactly, Double scale)
{
   checkAxis(axis);
//
// T can only be Float or Double
//
   Bool peakIsUnity = !autoScale;
   uInt shape = itsImagePtr->shape()(axis);
   Vector<T> x = VectorKernel::make(kernelType, T(width), shape, 
                                    useImageShapeExactly, peakIsUnity);
   if (!autoScale && !near(scale,1.0)) x *= Float(scale);
   uInt n = itsVectorKernels.nelements() + 1;
   itsVectorKernels.resize(n, True);
   itsVectorKernels[n-1] = new Vector<T>(x.copy());
//
   itsAxes.resize(n, True);
   itsAxes(n-1) = axis;
}


template <class T>
Vector<T> SepImageConvolver<T>::getKernel(uInt axis) 
{
   for (uInt i=0; i<itsAxes.nelements(); i++) {
      if (axis==itsAxes(i)) {
         return *(itsVectorKernels[i]);
      }
   }
   itsOs << "There is no kernel for the specified axis" << LogIO::EXCEPTION;
//
   return Vector<T>(0);
}

template <class T>
uInt SepImageConvolver<T>::getKernelShape(uInt axis) 
{
   for (uInt i=0; i<itsAxes.nelements(); i++) {
      if (axis==itsAxes(i)) {
         return itsVectorKernels[i]->nelements();
      }
   }
   itsOs << "There is no kernel for the specified axis" << LogIO::EXCEPTION;
//
   return 0;
}


  

template <class T>
void SepImageConvolver<T>::convolve(ImageInterface<T>& imageOut)
{
   const uInt nAxes = itsAxes.nelements();
   if (nAxes==0) {
      itsOs << "You haven't specified any axes to convolve" << LogIO::EXCEPTION;
   }

// Some checks

   IPosition shape = itsImagePtr->shape();
   if (!shape.isEqual(imageOut.shape())) {
      itsOs << "Image shapes are different" << LogIO::EXCEPTION;
   }
   CoordinateSystem cSys = itsImagePtr->coordinates();
   if (!cSys.near(imageOut.coordinates())) {
      itsOs << LogIO::WARN << "Image CoordinateSystems differ - this may be unwise"
            << LogIO::POST;
   }

// Give the output image a mask if needed and make it the default

   if (itsImagePtr->isMasked() && !imageOut.isMasked()) {
      if (imageOut.canDefineRegion()) {
         String maskName = imageOut.makeUniqueRegionName (String("mask"), 0);
         imageOut.makeMask(maskName, True, True);
         itsOs << LogIO::NORMAL << "Created mask " << maskName 
               << " and make it the default" << LogIO::POST;
      } else {
         itsOs << LogIO::NORMAL << "Cannot create a mask for this output image" << LogIO::POST;
      }
   }

// First copy input to output. We must replace masked pixels by zeros.  These reflect 
// both the pixel mask and the region mask.  We also set the output mask to the input mask
 
   LatticeUtilities::copyDataAndMask(itsOs, imageOut, *itsImagePtr, True);

// Smooth in situ.  
      
   IPosition niceShape = imageOut.niceCursorShape();
   uInt axis = 0;
   for (uInt i=0; i<nAxes; i++) {
      axis = itsAxes(i);
      itsOs << LogIO::NORMAL << "Convolving axis " << axis+1 << LogIO::POST;
      const Int n = shape(axis)/niceShape(axis);
      if (n*niceShape(axis)!=shape(axis)) {
         itsOs << LogIO::WARN 
               << "The tile shape is not integral along this axis, performance may degrade" 
               << LogIO::POST;
      }
      smoothProfiles (imageOut, axis, *(itsVectorKernels[i]));
   }  
}


template <class T>
void SepImageConvolver<T>::convolve()
{
   const uInt nAxes = itsAxes.nelements();
   if (nAxes==0) {
      itsOs << "You haven't specified any axes to convolve" << LogIO::EXCEPTION;
   }
   IPosition shape = itsImagePtr->shape();

// We must replace masked pixels by zeros.  These reflect 
// both the pixel mask and the region mask.  
 
   zero();

// Smooth in situ.  
      
   IPosition niceShape = itsImagePtr->niceCursorShape();
   uInt axis = 0;
   for (uInt i=0; i<nAxes; i++) {
      axis = itsAxes(i);
      itsOs << LogIO::NORMAL << "Convolving axis " << axis+1 << LogIO::POST;
      const Int n = shape(axis)/niceShape(axis);
      if (n*niceShape(axis)!=shape(axis)) {
         itsOs << LogIO::WARN 
               << "The tile shape is not integral along this axis, performance may degrade" 
               << LogIO::POST;
      }
      smoothProfiles (*itsImagePtr, axis, *(itsVectorKernels[i]));
   }  
}

 
template <class T>
void SepImageConvolver<T>::zero()
{
   if (itsImagePtr->isMasked()) {
      itsOs << LogIO::NORMAL << "Zero masked pixels" << LogIO::POST;
//
      LatticeIterator<T> iter(*itsImagePtr);
      Bool deleteData, deleteMask;
      IPosition shape = iter.rwCursor().shape();
      Array<T> data(shape);
      Array<Bool> mask(shape);
//       
      for (iter.reset(); !iter.atEnd(); iter++) {
         shape = iter.rwCursor().shape();
         if (!data.shape().isEqual(shape)) data.resize(shape);
         if (!mask.shape().isEqual(shape)) mask.resize(shape);
//
         itsImagePtr->getSlice(data, iter.position(), shape);
         itsImagePtr->getMaskSlice(mask, iter.position(), shape);
//
         T* pData = data.getStorage(deleteData);
         const Bool* pMask = mask.getStorage(deleteMask);
//
         for (Int i=0; i<shape.product(); i++) {
            if (!pMask[i]) pData[i] = 0.0;
         }
//
         data.putStorage(pData, deleteData);
         mask.freeStorage(pMask, deleteMask);
      }
   }     
}        
 
template <class T>
void SepImageConvolver<T>::smoothProfiles (ImageInterface<T>& in,
                                           const Int& axis,
                                           const Vector<T>& psf)
{
  ProgressMeter* pProgressMeter = 0;
  if (itsShowProgress) {
     Double nMin = 0.0;
     Double nMax = 1.0;
     for (Int i=0; i<Int(in.shape().nelements()); i++) {
        if (i!=axis) {
           nMax *= in.shape()(i);
        }
     }
     ostringstream oss;
     oss << "Convolve Image Axis " << axis+1;
     pProgressMeter = new ProgressMeter(nMin, nMax, String(oss),
                                        String("Spectrum Convolutions"),
                                        String(""), String(""),
                                        True, max(1,Int(nMax/20)));   
  }
// 
  TiledLineStepper navIn(in.shape(),
                         in.niceCursorShape(),
                         axis);
  LatticeIterator<T> inIt(in, navIn);
  Vector<T> result(in.shape()(axis));
  IPosition shape(1, in.shape()(axis));
  Convolver<T> conv(psf, shape);

  uInt i = 0;
  while (!inIt.atEnd()) {
    conv.linearConv(result, inIt.vectorCursor());
    inIt.woVectorCursor() = result;
//
    if (itsShowProgress) pProgressMeter->update(Double(i));
    inIt++;
    i++;
  }
  if (itsShowProgress) delete pProgressMeter;
}

template <class T>
void SepImageConvolver<T>::checkAxis(uInt axis) 
{
   if (axis>itsImagePtr->ndim()-1) {
      itsOs << "Given pixel axis " << axis 
            << " is greater than the number of axes in the image" << LogIO::EXCEPTION;
   }
   const uInt n = itsAxes.nelements();
   for (uInt i=0; i<n; i++) {
      if (axis==itsAxes(i)) {
         itsOs << "You have already given this axis to be convolved" << LogIO::EXCEPTION;
      }
   }
}

template <class T>
Bool SepImageConvolver<T>::isTempImage (const ImageInterface<Float>* pIm) const
{
   Bool isTemp = False;
   const TempImage<Float>* tmp = dynamic_cast<const TempImage<Float>*>(pIm);
   if (tmp!=0) isTemp = True;
   return isTemp;
}


} //# NAMESPACE CASA - END

