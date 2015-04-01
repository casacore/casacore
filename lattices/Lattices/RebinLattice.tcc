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

#ifndef LATTICES_REBINLATTICE_TCC
#define LATTICES_REBINLATTICE_TCC

#include <casacore/lattices/Lattices/RebinLattice.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
RebinLattice<T>::RebinLattice ()
: itsLatticePtr (0),
  itsAllUnity   (False)
{}



template<class T>
RebinLattice<T>::RebinLattice (const MaskedLattice<T>& lattice,
                               const IPosition& bin)
: itsLatticePtr(lattice.cloneML())
{
   LogIO os(LogOrigin("RebinLattice", "RebinLattice(...)", WHERE));
   const uInt nDim = lattice.ndim();
   if (bin.nelements() != nDim) {
      os << "Binning vector and lattice must have same dimension" << LogIO::EXCEPTION;
   }
//
   itsBin.resize(bin.nelements());
   const IPosition shapeIn = lattice.shape();
   itsAllUnity = True;
   for (uInt i=0; i<bin.nelements(); i++) {
      if (bin[i]==0)  {
         os << "Binning vector values must be positive integers" << LogIO::EXCEPTION;
      }
//
      itsBin[i] = bin[i];
      if (bin[i] > shapeIn[i]) {
         os << LogIO::WARN << "Truncating bin to lattice shape for axis " << i+1 << LogIO::POST;
         itsBin[i] = shapeIn[i];
      }
      if (bin[i] != 1) itsAllUnity = False;
   }
}

template<class T>
RebinLattice<T>::RebinLattice (const RebinLattice<T>& other)
: MaskedLattice<T>(),
  itsLatticePtr(0)
{
  operator= (other);
}

template<class T>
RebinLattice<T>::~RebinLattice()
{
  delete itsLatticePtr;
}

template<class T>
RebinLattice<T>& RebinLattice<T>::operator=(const RebinLattice<T>& other)
{
  if (this != &other) {
    delete itsLatticePtr;
    itsLatticePtr = 0;
    if (other.itsLatticePtr) {
      itsLatticePtr = other.itsLatticePtr->cloneML();
    }
// Clear the cache.
    itsData.resize();
    itsMask.resize();
    itsSlicer = Slicer();
//
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
   return rebinShape(itsLatticePtr->shape(), itsBin);
}



template<class T>
String RebinLattice<T>::name (Bool stripPath) const
{
  return itsLatticePtr->name(stripPath);
}


template<class T>
Bool RebinLattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{

// If all unity, access the lattice directly.

   if (itsAllUnity) {
      return itsLatticePtr->doGetSlice (buffer, section);
   }

// Get the result for this section if not in cache

   if (!(section == itsSlicer)) {
      getDataAndMask (section);
   }
   buffer.reference(itsData);
   return True;
}



template<class T>
void RebinLattice<T>::doPutSlice (const Array<T>&,
                                  const IPosition&, 
                                  const IPosition&)
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

// If not masked, simply fill the buffer

  if (!itsLatticePtr->isMasked()) {
     buffer.resize (section.length());
     buffer = True;
     return False;
  }

// If all unity, access the lattice directly.

   if (itsAllUnity) {
      return itsLatticePtr->doGetMaskSlice (buffer, section);
   }

// Get the result for this section if not in cache

   if (!(section == itsSlicer)) {
      getDataAndMask (section);
   }
   buffer.reference(itsMask);
   return True;
}


template <class T>
Bool RebinLattice<T>::ok() const
{
  return itsLatticePtr->ok();
}




template<class T>
void RebinLattice<T>::getDataAndMask (const Slicer& section)
{

// Work out the slicer for the input Lattice given the slicer for
// the binned Lattice

   Slicer sectionIn = findOriginalSlicer (section);

// Fetch

   Array<T> data;
   Array<Bool> mask;
   itsData.resize (section.length());
   itsLatticePtr->getSlice(data, sectionIn);
   if (itsLatticePtr->isMasked()) {
      itsLatticePtr->getMaskSlice(mask, sectionIn);
      itsMask.resize (section.length());
      bin (data, mask);
   } else {
      bin (data);
   }     

// Remember what is in cache
   itsSlicer = section;
}



template <class T>
void RebinLattice<T>::bin (const Array<T>& dataIn)
{

// Make Lattice from Array to get decent iterators

   const uInt nDim = dataIn.ndim();
   LatticeStepper stepper (dataIn.shape(), itsBin, LatticeStepper::RESIZE);
   ArrayLattice<T> latIn (dataIn);
   RO_LatticeIterator<T> inIter(latIn, stepper);

// Do it

   IPosition outPos(nDim);
//
   for (inIter.reset(); !inIter.atEnd(); inIter++) {
      const Array<T>& cursor(inIter.cursor());
      const uInt nSum = cursor.nelements();
      T sumData = sum(cursor);
      if (nSum>0) sumData /= nSum;

// Write output

      const IPosition& inPos = inIter.position();
      outPos = inPos / itsBin;
      itsData(outPos) = sumData;
   }
}


template <class T>
void RebinLattice<T>::bin (const Array<T>& dataIn, const Array<Bool>& maskIn)
{

// Make Lattice from Array to get decent iterators

   const uInt nDim = dataIn.ndim();
   ArrayLattice<T> latIn (dataIn);
   Array<Bool> maskInRef(maskIn);

// Make Lattice iterators

   LatticeStepper stepper (latIn.shape(), itsBin, LatticeStepper::RESIZE);
   RO_LatticeIterator<T> inIter(latIn, stepper);

// Do it

   IPosition outPos(nDim);
   Array<Bool> cursorMask;
//
   for (inIter.reset(); !inIter.atEnd(); inIter++) {
      const Array<T>& cursor(inIter.cursor());
      Array<Bool> cursorMask (maskInRef(inIter.position(),
                                        inIter.endPosition()));

// Iterate through cursor with STL iterators

      T sumData = 0;
      Int nSum = 0;
      typename Array<T>::const_iterator dataIterEnd = cursor.end();
      typename Array<T>::const_iterator dataIter;
      typename Array<Bool>::const_iterator maskIter;
      for (dataIter=cursor.begin(),maskIter=cursorMask.begin();
           dataIter!=dataIterEnd; ++dataIter,++maskIter) {
         if (*maskIter) {
            sumData += *dataIter;
            nSum++;
         }
      }
      if (nSum>0) sumData /= nSum;

// Write output (perhaps could redo this with an iterator)

      const IPosition& inPos = inIter.position();
      outPos = inPos / itsBin;
      itsData(outPos) = sumData;
      itsMask(outPos) = nSum>0;
   }
}


template<class T>
IPosition RebinLattice<T>::rebinShape (const IPosition& inShape,
                                       const IPosition& bin)
{
   AlwaysAssert(inShape.nelements()==bin.nelements(), AipsError);
//
   const uInt nDim = inShape.nelements();
   IPosition outShape(nDim);
   for (uInt i=0; i<nDim; i++) {
      Int n = inShape[i] / bin[i];
      Int rem = inShape[i] - n*bin[i];
      if (rem > 0) n += 1;               // Allow last bin to be non-integral
      outShape[i] = n;
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
   const IPosition shapeOrig = itsLatticePtr->shape();
//
   const IPosition& blc = section.start();
   const IPosition& trc = section.end();
   const IPosition& stride = section.stride();
//
   IPosition blcOrig(blc);
   IPosition trcOrig(trc);
   for (uInt i=0; i<nDim; i++) {
      if (stride[i] != 1) {
         throw (AipsError("RebinLattice: Slices with non-unit stride are not yet supported"));
      }
//
      blcOrig[i] = blc[i] * itsBin[i];
      trcOrig[i] = trc[i] * itsBin[i] + (itsBin[i] - 1);
//
      blcOrig[i] = std::max(ssize_t(0), std::min(blcOrig[i], shapeOrig[i]-1));
      trcOrig[i] = std::max(ssize_t(0), std::min(trcOrig[i], shapeOrig[i]-1));
   }
//
   IPosition strideOrig(nDim,1);
   return Slicer(blcOrig, trcOrig, strideOrig, Slicer::endIsLast);
}


} //# NAMESPACE CASACORE - END


#endif
