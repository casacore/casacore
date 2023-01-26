//# LatticeConcat.cc: concatenate lattices
//# Copyright (C) 1995,1997,1998,1999,2000,2003
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

#ifndef LATTICES_LATTICECONCAT_TCC
#define LATTICES_LATTICECONCAT_TCC


#include <casacore/lattices/Lattices/LatticeConcat.h>

#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
LatticeConcat<T>::LatticeConcat()
: axis_p(0),
  shape_p(IPosition(0)),
  isMasked_p(false),
  dimUpOne_p(false),
  tempClose_p(true),
  pPixelMask_p(0)
{
}

template<class T>
LatticeConcat<T>::LatticeConcat(uint32_t axis, bool tempClose)
: axis_p(axis),
  shape_p(IPosition(0)),
  isMasked_p(false),
  dimUpOne_p(false),
  tempClose_p(tempClose),
  pPixelMask_p(0)
{
}

template<class T>
LatticeConcat<T>::LatticeConcat (const LatticeConcat<T>&other) 
: MaskedLattice<T>(),
  lattices_p(other.lattices_p.nelements()),
  axis_p (other.axis_p),
  shape_p(other.shape_p),
  isMasked_p(other.isMasked_p),
  dimUpOne_p(other.dimUpOne_p),
  tempClose_p(other.tempClose_p),
  pPixelMask_p(0)
{
   const uint32_t n = lattices_p.nelements();
   for (uint32_t i=0; i<n; i++) {
      lattices_p[i] = other.lattices_p[i]->cloneML();
      if (tempClose_p) lattices_p[i]->tempClose();
   }
   if (other.pPixelMask_p!=0) {
      pPixelMask_p = other.pPixelMask_p->cloneML();
   }
}

template<class T>
LatticeConcat<T>::~LatticeConcat()
{
   const uint32_t n = lattices_p.nelements();
   for (uint32_t i=0; i<n; i++) {
      delete lattices_p[i];
      lattices_p[i] = 0;
   }
   delete pPixelMask_p;
}

template<class T>
LatticeConcat<T>& LatticeConcat<T>::operator= (const LatticeConcat<T>& other)
{
  if (this != &other) {
    axis_p         = other.axis_p;
    shape_p        = other.shape_p;
    isMasked_p     = other.isMasked_p;
    dimUpOne_p     = other.dimUpOne_p;
    tempClose_p    = other.tempClose_p;
//
    uint32_t n = lattices_p.nelements();
    for (uint32_t j=0; j<n; j++) {
       delete lattices_p[j];
       lattices_p[j] = 0;
    }
//
    lattices_p.resize(other.lattices_p.nelements(), true);
    n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       lattices_p[i] = other.lattices_p[i]->cloneML();
       if (tempClose_p) lattices_p[i]->tempClose();
    }
//
    delete pPixelMask_p;
    pPixelMask_p = 0;
    if (other.pPixelMask_p!=0) {
       pPixelMask_p = other.pPixelMask_p->cloneML();
    }
  }
//   
  return *this;
}



template<class T>
void LatticeConcat<T>::setLattice(MaskedLattice<T>& lattice)
{
   const uint32_t n = lattices_p.nelements();
   const uint32_t ndim = lattice.ndim();
   dimUpOne_p  = (axis_p==ndim);
//
// Check for consistency
//
   if (n==0) {
      checkAxis(axis_p, ndim);
//
      if (dimUpOne_p) {

// Increasing dimensionality by one

         IPosition shape = lattice.shape();
         shape_p = IPosition(ndim+1);
         shape_p.setFirst(shape);
         shape_p(ndim) = 1;
      } else {
         shape_p = lattice.shape();
      }
   } else {
      if (dimUpOne_p) {

// Increasing dimensionality by one

         IPosition shape = shape_p.getFirst(ndim);
         if (!shape.isEqual(lattice.shape())) {
            throw (AipsError("Lattice shapes inconsistent"));
         }
         shape_p(ndim) += 1;
      } else {

// Dimensionality the same

         if (shape_p.nelements() != ndim) {
            throw(AipsError("Lattice dimensions are inconsistent"));
         }
//
         const IPosition shape = lattice.shape();
         for (uint32_t i=0; i<shape.nelements(); i++) {
            if (i!=axis_p && shape(i) != shape_p(i)) {
               throw (AipsError("Lattice shapes inconsistent"));
            }
         }

// Update concatenated shape

         shape_p(axis_p) += shape(axis_p);
      }
   }

// Assign lattice

   lattices_p.resize(n+1, true);
   lattices_p[n] = lattice.cloneML();

// If any lattice is masked, the whole thing is masked

   if (lattice.isMasked()) isMasked_p = true;

// Handle pixelMask.
// If a Lattice has a pixelmask, insert pixelmasks (i.e. LCBox-s)
// for lattices not having pixelmasks.
// Note that this makes the pixelmask readonly.

   if (lattice.hasPixelMask()) {
      if (pPixelMask_p == 0) {
	 pPixelMask_p = new LatticeConcat<bool>(axis_p, tempClose_p);
	 for (uint32_t i=0; i<n; i++) {
	    SubLattice<bool> tmp = LCBox (lattices_p[i]->shape());
	    pPixelMask_p->setLattice (tmp);
	 }
      }
      SubLattice<bool> tmp(lattice.pixelMask(), true);
      pPixelMask_p->setLattice (tmp);
   } else {
      if (pPixelMask_p != 0) {
	 SubLattice<bool> tmp = LCBox (lattice.shape());
	 pPixelMask_p->setLattice (tmp);
      }
   }

// Close this lattice
 
   if (tempClose_p) lattices_p[n]->tempClose();
} 


template <class T>
uint32_t LatticeConcat<T>::latticeDim() const
{
   if (dimUpOne_p) {
      return shape_p.nelements();
   } else {
      return shape_p.nelements() - 1;
   }
}



//Public virtual functions

template <class T>
String LatticeConcat<T>::name (bool) const
{
  return "Concatenation :";
}

template <class T>
LatticeConcat<T>* LatticeConcat<T>::cloneML() const
{
   return new LatticeConcat(*this);
}

template<class T>
bool LatticeConcat<T>::isMasked() const
{
   return isMasked_p;
} 


template <class T>
const LatticeRegion* LatticeConcat<T>::getRegionPtr() const
{
   return 0;
}


template <class T>
bool LatticeConcat<T>::isWritable() const
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       if (!lattices_p[i]->isWritable()) return false;
    }
    return true;
}

template<class T>
bool LatticeConcat<T>::hasPixelMask() const
{
   return pPixelMask_p != 0;
}
  
template<class T>
const Lattice<bool>& LatticeConcat<T>::pixelMask() const
{
  if (pPixelMask_p == 0) {
    throw (AipsError ("LatticeConcat::pixelMask - no mask attached"));
  }
  return (*pPixelMask_p);
}


template<class T>
Lattice<bool>& LatticeConcat<T>::pixelMask()
{  
  if (pPixelMask_p == 0) {
    throw (AipsError ("LatticeConcat::pixelMask - no mask attached"));
  }
  return (*pPixelMask_p);
} 


template<class T>
IPosition LatticeConcat<T>::shape() const
{
   return shape_p;
} 


template <class T>
IPosition LatticeConcat<T>::doNiceCursorShape (uint32_t) const 
//
// This isn't very meaningful  for a LatticeConcat
// object since it isn't on disk !  But if you do
// copy it out, this is what you should use..
//
{
   TiledShape ts(shape());
   return ts.tileShape();
}



template <class T>
bool LatticeConcat<T>::doGetSlice (Array<T>& buffer,
                                   const Slicer& section)
{
   const uint32_t nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }
//
   bool ok = false;
   if (dimUpOne_p) {

// Increase dimensionality by one

     ok = getSlice1 (buffer, section, nLattices);
   } else {

// No dimensionality increase

     ok = getSlice2(buffer, section, nLattices);
   }
//
   return ok;
}
 

template <class T>
bool LatticeConcat<T>::doGetMaskSlice (Array<bool>& buffer,
                                       const Slicer& section)
{
   const uint32_t nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }
//
   bool ok = false;
   if (isMasked_p) {
      if (dimUpOne_p) {

// Increase dimensionality by one

        ok = getMaskSlice1 (buffer, section, nLattices);
      } else {

// No dimensionality increase

        ok = getMaskSlice2 (buffer, section, nLattices);
      }
   } else {
      buffer.resize (section.length());
      buffer = true;
      ok = true;
   }
//
   return ok;   
}


template <class T>
void LatticeConcat<T>::doPutSlice (const Array<T>& buffer, const IPosition& where,
                                   const IPosition& stride)
{      
   const uint32_t nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }
//
   if (!isWritable()) {
      throw(AipsError("Some of the underlying lattices are not writable"));
   }
//
   if (dimUpOne_p) {

// Increase dimensionality by one

     putSlice1 (buffer, where, stride, nLattices);
   } else {

// No dimensionality increase

     putSlice2 (buffer, where, stride, nLattices);
   }
}        



template <class T>
bool LatticeConcat<T>::lock (FileLocker::LockType type, uint32_t nattempts)
{
    const uint32_t n = lattices_p.nelements();
    Vector<bool> hadReadLock(n);
    Vector<bool> hadWriteLock(n);
//
    for (uint32_t i=0; i<n; i++) {
       hadReadLock(i) = lattices_p[i]->hasLock(FileLocker::Read);
       hadWriteLock(i) = lattices_p[i]->hasLock(FileLocker::Write);
       if (!lattices_p[i]->lock(type, nattempts)) {

// Try to put things back how they were if fails

          for (uint32_t j=0; j<i; j++) {
             if (hadReadLock(j)) {
                lattices_p[j]->lock(FileLocker::Read, 1);
             } else if (hadWriteLock(j)) {
                lattices_p[j]->lock(FileLocker::Write, 1);
             } else {
                lattices_p[j]->unlock();
             }
             if (tempClose_p) lattices_p[j]->tempClose();
          }
          if (tempClose_p) lattices_p[i]->tempClose();
          return false;
       }
       if (tempClose_p) lattices_p[i]->tempClose();
    }
    return true;
}

template <class T>
void LatticeConcat<T>::unlock()
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       lattices_p[i]->unlock();
    }
}


template <class T>
bool LatticeConcat<T>::hasLock (FileLocker::LockType type) const
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       if (lattices_p[i]->hasLock(type)) {
          return true;
       }
    }
    return false;
}
 
template <class T>
void LatticeConcat<T>::resync()
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       lattices_p[i]->resync();
    }
}

template <class T>
void LatticeConcat<T>::flush()
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       lattices_p[i]->flush();
    }
}

template <class T>
void LatticeConcat<T>::tempClose()
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       lattices_p[i]->tempClose();
    }
}

template <class T>
void LatticeConcat<T>::reopen()
{
    const uint32_t n = lattices_p.nelements();
    for (uint32_t i=0; i<n; i++) {
       lattices_p[i]->reopen();
    }
}

template <class T>
void LatticeConcat<T>::tempClose(uint32_t which)
{
    AlwaysAssert (which<lattices_p.nelements(), AipsError);
    lattices_p[which]->tempClose();
}

template <class T>
void LatticeConcat<T>::reopen(uint32_t which)
{
    AlwaysAssert (which<lattices_p.nelements(), AipsError);
    lattices_p[which]->reopen();
}


// Private functions



template<class T>
void LatticeConcat<T>::checkAxis(uint32_t axis, uint32_t ndim) const
{

// Allow the possibility to add one higher dimension.

   if (axis > ndim) {
      throw(AipsError("Axis number and lattice dimension are inconsistent"));
   }
}


template <class T>
void LatticeConcat<T>::setup1 (IPosition& blc, IPosition& trc, IPosition& stride,
                               IPosition& blc2, IPosition& trc2, 
                               IPosition& blc3, IPosition& trc3, IPosition& stride3, 
                               const Slicer& section)
{
// The Slicer section for the whole concatenated lattice

   blc = section.start();
   trc = section.end();
   stride = section.stride();

// The Slicer section for an individual lattice contribution
// in coordinates of that lattice

   blc2 = blc;
   trc2 = trc;

// The Slicer section for an individual lattice contribution
// in the output Array coordinates

   blc3 = blc;
   blc3 = 0;
   trc3 = section.length() - 1;
   stride3 = stride;
   stride3 = 1;
}

template<class T>
Slicer LatticeConcat<T>::setup2 (bool& first, IPosition& blc2, IPosition& trc2, 
                                 int32_t shape2, int32_t axis, const IPosition& blc, 
                                 const IPosition& trc, const IPosition& stride, int32_t start)
{

// This lattice contributes to the slice.  Find section
// in local lattice[i] coordinates 

   blc2(axis) = max(0,blc(axis)-start);
   trc2(axis) = min(trc(axis)-start,shape2-1);

// Adjust blc for stride if not first lattice

   if (!first) {
      blc2(axis) += (start-blc(axis))%stride(axis);
   }
   first = false;
//
   return Slicer(blc2, trc2, stride, Slicer::endIsLast);
}

template <class T>
bool LatticeConcat<T>::getSlice1 (Array<T>& buffer,
                                  const Slicer& section,
                                  uint32_t nLattices)
{
   const uint32_t dimIn = axis_p;

// The concatenated lattice section

   if (section.end()(axis_p)+1 > int32_t(nLattices)) {
      throw(AipsError("Number of lattices and requested slice are inconsistent"));      
   }
   IPosition blc3(dimIn+1,0);
   IPosition trc3(section.length()-1);
   IPosition stride3(dimIn+1,1);


// The underlying lattice section - it never changes

   Slicer section2(section.start().getFirst(dimIn), section.end().getFirst(dimIn), 
                   section.stride().getFirst(dimIn), Slicer::endIsLast);
//
   buffer.resize(section.length());

// We are looping over the last axis of the concatenated lattice
// Each input lattice contributes just one pixel to that axis

   uint32_t k = 0;
   for (int32_t i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
       Array<T> buf = lattices_p[i]->getSlice(section2);
//
       blc3(axis_p) = k;
       trc3(axis_p) = k;
       buffer(blc3, trc3, stride3) = buf.addDegenerate(1);
       if (tempClose_p) lattices_p[i]->tempClose();
       k++;
   }

// Result is a copy

   return false;
}


template <class T>
bool LatticeConcat<T>::getSlice2 (Array<T>& buffer,
                                  const Slicer& section,
                                  uint32_t nLattices)
{
//cout << "blc, trc, stride=" << section.start() << section.end() << section.stride() << endl;

// Setup positions

   IPosition blc, trc, stride;   
   IPosition blc2, trc2;
   IPosition blc3, trc3, stride3;
   setup1 (blc, trc, stride, blc2, trc2, blc3, trc3, stride3, section);
//
   buffer.resize(section.length());
//cout << "Buffer shape = " << buffer.shape() << endl;
//
   int32_t start = 0;
   bool first = true;
   Slicer section2;
//
   for (uint32_t i=0; i<nLattices; i++) {

//cout << "Lattice shape = " << lattices_p[i]->shape() << endl;

// Find start and end of this lattice inside the concatenated lattice

      int32_t shape2 = lattices_p[i]->shape()(axis_p);
      int32_t end = start + shape2 - 1;
//
      if (! (blc(axis_p)>end || trc(axis_p)<start)) {

// Find section of input Lattice to copy

         section2 = setup2(first, blc2, trc2, shape2, axis_p, blc, trc, 
                           stride, start);

// Put it in output buffer

//cout << "blc2, trc2, stride2, shape = " 
//     << section2.start() << section2.end() << section2.stride() << section2.length() << endl;

         trc3(axis_p) = blc3(axis_p) + section2.length()(axis_p) - 1;

//IPosition sh(Slicer(blc3, trc3, stride3, Slicer::endIsLast).length());
//cout << "blc3, trc3, stride3, shape = " << blc3 << trc3 << stride3  << sh << endl << endl;

         buffer(blc3, trc3, stride3) = lattices_p[i]->getSlice(section2);
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
      if (tempClose_p) lattices_p[i]->tempClose();
   }

// Result is a copy

   return false;
}


template <class T>
bool LatticeConcat<T>::putSlice1 (const Array<T>& buffer, const IPosition& where,
                                  const IPosition& stride, uint32_t nLattices)
{
   const uint32_t dimIn = axis_p;

// The concatenated Lattice section

   Slicer section(where, buffer.shape(), stride, Slicer::endIsLength);
   if (section.end()(axis_p)+1 > int32_t(nLattices)) {
      throw(AipsError("Number of lattices and given data buffer are inconsistent"));
   }
//      
   IPosition blc3(dimIn+1,0);
   IPosition trc3(section.length()-1);
   IPosition stride3(dimIn+1,1);

// The underlying Lattice section. It never changes
   
   Slicer section2(section.start().getFirst(dimIn), section.end().getFirst(dimIn),
                   section.stride().getFirst(dimIn), Slicer::endIsLast);

// We are looping over the last axis of the concatenated lattice
// Each input lattice contributes just one pixel to that axis
                                  
   uint32_t k = 0;
   for (int32_t i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
      blc3(axis_p) = k;
      trc3(axis_p) = k;
      Array<T> buf0(buffer);
      lattices_p[i]->putSlice(buf0(blc3, trc3, stride3).nonDegenerate(axis_p-1), 
                              section2.start(), section2.stride());
      if (tempClose_p) lattices_p[i]->tempClose();
      k++;
   }
//  
   return true;
}


template <class T>
bool LatticeConcat<T>::putSlice2 (const Array<T>& buffer, const IPosition& where,
                                  const IPosition& strider, uint32_t nLattices)
{

// Make a Slicer for the region to be put so we can reuse the functions
// for setting locations used in the getSlice functions
// Objects blc and stride will duplicate where and strider respectively

//cout << "Buffer shape = " << buffer.shape() << endl;

   Slicer section(where, buffer.shape(), strider, Slicer::endIsLength);

//cout << "Section = " << section << endl;

// Setup positions

   IPosition blc, trc, stride;   
   IPosition blc2, trc2;
   IPosition blc3, trc3, stride3;
   setup1 (blc, trc, stride, blc2, trc2, blc3, trc3, stride3, section);
//
   int32_t start = 0;
   bool first = true;
   Slicer section2;
//
   for (uint32_t i=0; i<nLattices; i++) {
//cout << "Lattice " << i << " shape = " << lattices_p[i]->shape() << endl;

// Find start and end of this lattice inside the concatenated lattice

      int32_t shape2 = lattices_p[i]->shape()(axis_p);
      int32_t end = start + shape2 - 1;
//
      if (! (blc(axis_p)>end || trc(axis_p)<start)) {

//cout << "Lattice included" << endl;

// Find section of input Lattice to put in coordinates of that lattice

         section2 = setup2(first, blc2, trc2, shape2, axis_p, blc, trc, 
                           stride, start);

// Put it in lattice

//cout << "blc2, trc2, stride2, shape = " 
//     << section2.start() << section2.end() << section2.stride() << section2.length() << endl;

         trc3(axis_p) = blc3(axis_p) + section2.length()(axis_p) - 1;

//IPosition sh(Slicer(blc3, trc3, stride3, Slicer::endIsLast).length());
//cout << "blc3, trc3, stride3, shape = " << blc3 << trc3 << stride3  << sh << endl << endl;

         Array<T> buf(buffer);
         lattices_p[i]->putSlice(buf(blc3, trc3, stride3), blc2, stride);
         if (tempClose_p) lattices_p[i]->tempClose();
//
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
   }
//
   return true;
}


template <class T>
bool LatticeConcat<T>::getMaskSlice1 (Array<bool>& buffer,
                                      const Slicer& section,
                                      uint32_t nLattices)
{
   const uint32_t dimIn = axis_p;

// The concatenated lattice section

   if (section.end()(axis_p)+1 > int32_t(nLattices)) {
      throw(AipsError("Number of lattices and requested slice are inconsistent"));
   }
   IPosition blc3(dimIn+1,0);
   IPosition trc3(section.length()-1);
   IPosition stride3(dimIn+1,1);


// The underlying lattice section - it never changes
   
   Slicer section2(section.start().getFirst(dimIn), section.end().getFirst(dimIn),
                   section.stride().getFirst(dimIn), Slicer::endIsLast);
//
   buffer.resize(section.length());

// We are looping over the last axis of the concatenated lattice
// Each input lattice contributes just one pixel to that axis
                                  
   uint32_t k = 0;
   for (int32_t i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
       blc3(axis_p) = k;
       trc3(axis_p) = k;
       Array<bool> buf = lattices_p[i]->getMaskSlice(section2);
       buffer(blc3, trc3, stride3) = buf.addDegenerate(1);
       if (tempClose_p) lattices_p[i]->tempClose();
       k++;
   }

// Result is a copy

   return false;
}
 


template <class T>
bool LatticeConcat<T>::getMaskSlice2 (Array<bool>& buffer,
                                      const Slicer& section,
                                      uint32_t nLattices)
{

// Setup positions

   IPosition blc, trc, stride;   
   IPosition blc2, trc2;
   IPosition blc3, trc3, stride3;
   setup1 (blc, trc, stride, blc2, trc2, blc3, trc3, stride3, section);
//
   buffer.resize(section.length());
//
   int32_t start = 0;
   bool first = true;
   Slicer section2;
//
   for (uint32_t i=0; i<nLattices; i++) {

// Find start and end of this lattice inside the concatenated lattice

      int32_t shape2 = lattices_p[i]->shape()(axis_p);
      int32_t end = start + shape2 - 1;
//
      if (! (blc(axis_p)>end || trc(axis_p)<start)) {

// Whack slice into the output

         section2 = setup2(first, blc2, trc2, shape2, axis_p, blc, trc, 
                           stride, start);
//
         trc3(axis_p) = blc3(axis_p) + section2.length()(axis_p) - 1;
         buffer(blc3, trc3, stride3) = lattices_p[i]->getMaskSlice(section2);
         if (tempClose_p) lattices_p[i]->tempClose();
//
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
   }

// Result is a copy

   return false;
}

} //# NAMESPACE CASACORE - END


#endif
