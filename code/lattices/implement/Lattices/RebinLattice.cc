//# RebinLattice.cc: rebin a lattice
//# Copyright (C) 2003
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

#include <trial/Lattices/RebinLattice.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Lattices/ArrayLattice.h>
#include <aips/Lattices/LatticeStepper.h>
#include <trial/Lattices/MaskedLatticeIterator.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h> 


template<class T>
RebinLattice<T>::RebinLattice ()
: itsLatticePtr(0),
  itsAllUnity(False)
{}



template<class T>
RebinLattice<T>::RebinLattice (const MaskedLattice<T>& lattice,
                               const Vector<uInt>& bin)
: itsLatticePtr(lattice.cloneML())
{
   LogIO os(LogOrigin("RebinLattice", "RebinLattice(...)", WHERE));
   const uInt nDim = lattice.ndim();
   if (bin.nelements() != nDim) {
      os << "Binning vector and lattice must have same dimension" << LogIO::EXCEPTION;
   }
//
   itsBin.resize(bin.nelements());
   const IPosition& shapeIn = lattice.shape();
   itsAllUnity = True;
   for (uInt i=0; i<bin.nelements(); i++) {
      if (bin[i]==0)  {
         os << "Binning factors vector values must be positive integers" << LogIO::EXCEPTION;
      }
//
      itsBin(i) = bin[i];
      if (Int(bin[i])>shapeIn(i)) {
         os << LogIO::WARN << "Truncating bin to lattice shape for axis " << i+1 << LogIO::POST;
         itsBin(i) = shapeIn(i);
      }
      if (bin(i) != 1) itsAllUnity = False;
   }
}

template<class T>
RebinLattice<T>::RebinLattice (const RebinLattice<T>& other)
: itsLatticePtr(0)
{
  operator= (other);
}

template<class T>
RebinLattice<T>::~RebinLattice()
{
   if (itsLatticePtr) {
      delete itsLatticePtr;
      itsLatticePtr = 0;
   }
}

template<class T>
RebinLattice<T>& RebinLattice<T>::operator=(const RebinLattice<T>& other)
{
  if (this != &other) {
    delete itsLatticePtr;
    itsLatticePtr = other.itsLatticePtr->cloneML();
//
    itsData.resize();
    itsData = other.itsData.copy();
    itsMask.resize();
    itsMask = other.itsMask.copy();
//
    itsBin.resize(0);
    itsBin = other.itsBin;
    itsAllUnity = other.itsAllUnity;
  }
  return *this;
}

template<class T>
MaskedLattice<T>* RebinLattice<T>::cloneML() const
{
  return new RebinLattice<T> (*this);
}


template<class T>
Bool RebinLattice<T>::isMasked() const
{
  return itsLatticePtr->isMasked();
}

template<class T>
Bool RebinLattice<T>::isPaged() const
{
  return itsLatticePtr->isPaged();
}

template<class T>
Bool RebinLattice<T>::isWritable() const
{
  return False;
}

template<class T>
Bool RebinLattice<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return itsLatticePtr->lock (type, nattempts);
}

template<class T>
void RebinLattice<T>::unlock()
{
  itsLatticePtr->unlock();
}

template<class T>
Bool RebinLattice<T>::hasLock (FileLocker::LockType type) const
{
  return itsLatticePtr->hasLock (type);
}

template<class T>
void RebinLattice<T>::resync()
{
  itsLatticePtr->resync();
}

template<class T>
void RebinLattice<T>::flush()
{
  itsLatticePtr->flush();
}

template<class T>
void RebinLattice<T>::tempClose()
{
  itsLatticePtr->tempClose();
}

template<class T>
void RebinLattice<T>::reopen()
{
  itsLatticePtr->reopen();
}


template<class T>
const LatticeRegion* RebinLattice<T>::getRegionPtr() const
{
  return 0;
}

template<class T>
IPosition RebinLattice<T>::shape() const
{
   return doShape(itsLatticePtr->shape());
}



template<class T>
String RebinLattice<T>::name (Bool stripPath) const
{
  return itsLatticePtr->name(stripPath);
}

template<class T>
Bool RebinLattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{

// If we already have the result for this section don't get it again

   if (section==itsSlicer && itsData.nelements()>0) {
//      buffer.reference(itsData);
      buffer = itsData;
      return False;
   }

// Get input data

   Bool isRef = False;
   if (itsAllUnity) {
      isRef = getDataAndMask (itsData, itsMask, section);
      buffer.resize(itsData.shape());
//      buffer.reference(itsData);
      buffer = itsData;
   } else {
      Array<T> dataIn;
      Array<Bool> maskIn;
      getDataAndMask (dataIn, maskIn, section);

// Bin it up

      const IPosition& shapeOut = section.length();
      itsData.resize(shapeOut);
      itsMask.resize(shapeOut);
//
      bin (itsData, itsMask, dataIn, maskIn);
//      buffer.reference(itsData);
      buffer.resize(shapeOut);
      buffer = itsData;
      isRef = False;
   }
//
   itsSlicer = section;
   return isRef;
}



template<class T>
void RebinLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
                                  const IPosition& where, 
                                  const IPosition& stride)
{
  throw (AipsError ("RebinLattice::putSlice - non-writable lattice"));
}


template<class T>
uInt RebinLattice<T>::advisedMaxPixels() const
{
  return itsLatticePtr->advisedMaxPixels();
}

template<class T>
Bool RebinLattice<T>::doGetMaskSlice (Array<Bool>& buffer,
                                      const Slicer& section)
{
// If we already have the result for this section don't get it again

   if (section==itsSlicer && itsMask.nelements()>0) {
//      buffer.reference(itsMask);
      buffer = itsMask;
      return False;
   }
//
   Bool isRef;
   const IPosition& shapeOut = section.length();
   if (itsLatticePtr->isMasked()) {
      if (itsAllUnity) {
         isRef = getDataAndMask (itsData, itsMask, section);
         buffer.resize(itsMask.shape());
//         buffer.reference(itsMask);
         buffer = itsMask;
      } else {

// Get input data

         Array<T> dataIn;
         Array<Bool> maskIn;
         getDataAndMask (dataIn, maskIn, section);

// Bin it up

         itsData.resize(shapeOut);
         itsMask.resize(shapeOut);
         bin (itsData, itsMask, dataIn, maskIn);
//         buffer.reference(itsMask);
         buffer.resize(shapeOut);
         buffer = itsMask;
         isRef = True;
      }
   } else {
       itsMask.resize(shapeOut);
       itsMask = True;
//       buffer.reference(itsMask);
       buffer.resize(shapeOut);
       buffer = itsMask;
       isRef = False;
   }
//
   itsSlicer = section;
   return isRef;
}


template <class T>
Bool RebinLattice<T>::ok() const
{
  return itsLatticePtr->ok();
}



template<class T>
Bool RebinLattice<T>::getDataAndMask (Array<T>& data, Array<Bool>& mask, const Slicer& section)
{

// Work out the slicer for the input Lattice given the slicer for
// the binned Lattice

   Slicer sectionIn = findOriginalSlicer (section);

// Fetch

   Bool isRef = itsLatticePtr->getSlice(data, sectionIn);
   if (itsLatticePtr->isMasked()) {
      itsLatticePtr->getMaskSlice(mask, sectionIn);    // Assume isRef same for data/mask
   }
   return isRef;
}


template <class T>
Bool RebinLattice<T>::bin(Array<T>& dataOut, Array<Bool>& maskOut,
                          const Array<T>& dataIn, const Array<Bool>& maskIn) const
{

// Make Lattice from Array to get decent iterators

   ArrayLattice<T> latDataIn (dataIn);
   ArrayLattice<Bool> latMaskIn(maskIn);
   SubLattice<T> latIn(latDataIn);
//
   Bool doInMask  = maskIn.nelements() > 0;
   if (doInMask) latIn.setPixelMask(latMaskIn, False);
   Bool doOutMask = maskOut.nelements() > 0;

// Make Lattice iterators

   const uInt nDim = latIn.ndim();
   IPosition cursorShape(latIn.shape());
   LatticeStepper stepper (latIn.shape(), itsBin, LatticeStepper::RESIZE);
   RO_MaskedLatticeIterator<T> inIter(latIn, stepper);

// Do it

   IPosition outPos(nDim);
   T sumData;
   uInt nSum = 0;
//
   for (inIter.reset(); !inIter.atEnd(); inIter++) {
      const Array<T>& cursor(inIter.cursor());
      const uInt n = cursor.nelements();
//
      if (doInMask) {
         nSum = 0;
         sumData = 0;
         const Array<Bool>& cursorMask(inIter.getMask());

// Iterate through cursor with STL iterators

         typename Array<T>::const_iterator dataIter;
         typename Array<Bool>::const_iterator maskIter;
         for (dataIter=cursor.begin(),maskIter=cursorMask.begin();
              dataIter!=cursor.end(); ++dataIter,++maskIter) {
            if (*maskIter) {
               sumData += *dataIter;
               nSum++;
            }
         }
      } else {
         nSum = n;
         sumData = sum(cursor);
      }
      if (nSum>0) sumData /= nSum;

// Write output (perhaps could redo this with an iterator)

      const IPosition& inPos = inIter.position();
      outPos = inPos / itsBin;
//
      dataOut(outPos) = sumData;
      if (doOutMask) {
         maskOut(outPos) = True;
         if (nSum==0) maskOut(outPos) = False;
      }
   }
//
   return False;
}


template<class T>
IPosition RebinLattice<T>::doShape(const IPosition& inShape) const
{
   const uInt nDim = inShape.nelements();
   IPosition outShape(nDim);
   for (uInt i=0; i<nDim; i++) {
      Int n = inShape(i) / itsBin(i);
      Int rem = inShape(i) - n*itsBin(i);
      if (rem > 0) n += 1;                   // Allow last bin to be non-integral
      outShape(i) = n;
   }
   return outShape;
}


template<class T>
Slicer RebinLattice<T>::findOriginalSlicer (const Slicer& section) const
//
// For a slicer for the RebinLattice, find the Slicer for the original
// Lattice from which we must get data to then rebin 
//
{
   const uInt nDim = itsLatticePtr->ndim();
   const IPosition& shapeOrig = itsLatticePtr->shape();
//
   const IPosition& blc = section.start();
   const IPosition& trc = section.end();
   const IPosition& stride = section.stride();
//
   IPosition blcOrig(blc);
   IPosition trcOrig(trc);
   for (uInt i=0; i<nDim; i++) {
      if (stride(i) != 1) {
         throw (AipsError("Slices with non-unit stride are not yet supported"));
      }
//
      blcOrig(i) = blc(i) * itsBin(i);
      trcOrig(i) = trc(i) * itsBin(i) + (itsBin(i) - 1);
//
      blcOrig(i) = max(0, min(blcOrig(i), shapeOrig(i)-1));
      trcOrig(i) = max(0, min(trcOrig(i), shapeOrig(i)-1));
   }
//
   IPosition strideOrig(nDim,1);
   Slicer sliceOrig(blcOrig, trcOrig, strideOrig, Slicer::endIsLast);
   return sliceOrig;
}

