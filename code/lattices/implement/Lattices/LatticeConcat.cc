//# LatticeConcat.cc: concatenate lattices
//# Copyright (C) 1995,1997,1998,1999,2000
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


#include <trial/Lattices/LatticeConcat.h>

#include <aips/Exceptions/Error.h>
#include <aips/IO/FileLocker.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/LatticeStepper.h>
#include <trial/Lattices/MaskedLattice.h>
#include <aips/Lattices/TiledShape.h>


template<class T>
LatticeConcat<T>::LatticeConcat()
: axis_p(0),
  shape_p(IPosition(0)),
  isMasked_p(False)
{
}

template<class T>
LatticeConcat<T>::LatticeConcat(uInt axis)
: axis_p(axis),
  shape_p(IPosition(0)),
  isMasked_p(False)
{
}

template<class T>
LatticeConcat<T>::LatticeConcat (const LatticeConcat<T>&other) 
: lattices_p(other.lattices_p.nelements()),
  axis_p (other.axis_p),
  shape_p(other.shape_p),
  isMasked_p(other.isMasked_p)
{
   const uInt n = lattices_p.nelements();
   for (uInt i=0; i<n; i++) {
      lattices_p[i] = other.lattices_p[i]->cloneML();
   }
}

template<class T>
LatticeConcat<T>::~LatticeConcat()
{
   const uInt n = lattices_p.nelements();
   for (uInt i=0; i<n; i++) {
      delete lattices_p[i];
   }
}

template<class T>
LatticeConcat<T>& LatticeConcat<T>::operator= (const LatticeConcat<T>& other)
{
  if (this != &other) {
    axis_p         = other.axis_p;
    shape_p        = other.shape_p;
    isMasked_p     = other.isMasked_p;
//
    uInt n = lattices_p.nelements();
    for (uInt j=0; j<n; j++) {
       delete lattices_p[j];
       lattices_p[j] = 0;
    }
//
    lattices_p.resize(other.lattices_p.nelements(), True);
    n = lattices_p.nelements();
    for (uInt i=0; i<n; i++) {
       lattices_p[i] = other.lattices_p[i]->cloneML();
    }
  }
  return *this;
}



template<class T>
void LatticeConcat<T>::setLattice(MaskedLattice<T>& lattice)
{
   const uInt n = lattices_p.nelements();
   const uInt ndim = lattice.ndim();
   const Bool dimUpOne = (axis_p==ndim);
//
// Check for consistency
//
   if (n==0) {
      checkAxis(axis_p, ndim);
//
      if (dimUpOne) {

// Increasing dimensionality by one

         IPosition shape = lattice.shape();
         shape_p = IPosition(ndim+1);
         shape_p.setFirst(shape);
         shape_p(ndim) = 1;
      } else {
         shape_p = lattice.shape();
      }
   } else {
      if (dimUpOne) {

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
         for (uInt i=0; i<shape.nelements(); i++) {
            if (i!=axis_p && shape(i) != shape_p(i)) {
               throw (AipsError("Lattice shapes inconsistent"));
            }
         }

// Update concatenated shape

         shape_p(axis_p) += shape(axis_p);
      }
   }
//
// Assign lattice
//
   lattices_p.resize(n+1, True);
   lattices_p[n] = lattice.cloneML();
   if (lattice.isMasked()) isMasked_p = True;
} 


template <class T>
uInt LatticeConcat<T>::latticeDim() const
{
   uInt n = 0;
   if (lattices_p.nelements()>0) n = lattices_p[0]->ndim();
   return n;
}



//Public virtual functions

template <class T>
MaskedLattice<T>* LatticeConcat<T>::cloneML() const
{
   return new LatticeConcat(*this);
}

template<class T>
Bool LatticeConcat<T>::isMasked() const
{
   return isMasked_p;
} 


template <class T>
const LatticeRegion* LatticeConcat<T>::getRegionPtr() const
{
   return 0;
}


template <class T>
Bool LatticeConcat<T>::isWritable() const
{
    const uInt n = lattices_p.nelements();
    for (uInt i=0; i<n; i++) {
       if (!lattices_p[i]->isWritable()) return False;
    }
    return True;
}

template <class T>
Bool LatticeConcat<T>::isMaskWritable() const
{
    const uInt n = lattices_p.nelements();
    for (uInt i=0; i<n; i++) {
       if (!lattices_p[i]->isMaskWritable()) return False;
    }
    return True;
}

template<class T>
IPosition LatticeConcat<T>::shape() const
{
   return shape_p;
} 


template <class T>
IPosition LatticeConcat<T>::doNiceCursorShape (uInt) const 
//
// This isn't very meaningful  for a LatticeConcat
// object since it isn't on disk !  But if you do
// copy it out, this is what you should use...
//
{
   TiledShape ts(shape());
   return ts.tileShape();
}



template <class T>
Bool LatticeConcat<T>::doGetSlice (Array<T>& buffer,
                                   const Slicer& section)
{
   const uInt nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }

   Bool ok = False;
   if (axis_p==lattices_p[0]->ndim()) {

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
Bool LatticeConcat<T>::doGetMaskSlice (Array<Bool>& buffer,
                                       const Slicer& section)
{
   const uInt nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }

   Bool ok = False;
   if (isMasked_p) {
      if (axis_p==lattices_p[0]->ndim()) {

// Increase dimensionality by one

        ok = getMaskSlice1 (buffer, section, nLattices);
      } else {

// No dimensionality increase

        ok = getMaskSlice2 (buffer, section, nLattices);
      }
   } else {
      buffer.resize (section.length());
      buffer = True;
      ok = True;
   }
//
   return ok;   
}


template <class T>
void LatticeConcat<T>::doPutSlice (const Array<T>& buffer, const IPosition& where,
                                   const IPosition& stride)
{      
   const uInt nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }
//
   if (!isWritable()) {
      throw(AipsError("Some of the underlying lattices are not writable"));
   }
//
   Bool ok = False;
   if (axis_p==lattices_p[0]->ndim()) {

// Increase dimensionality by one

     ok = putSlice1 (buffer, where, stride, nLattices);
   } else {

// No dimensionality increase

     ok = putSlice2 (buffer, where, stride, nLattices);
   }
}        


template <class T>
void LatticeConcat<T>::doPutMaskSlice (const Array<Bool>& buffer, const IPosition& where,
                                       const IPosition& stride)
{      
   const uInt nLattices = lattices_p.nelements();
   if (nLattices==0) {
      throw (AipsError("No lattices set - use function setLattice"));
   }
//
   if (!isMaskWritable()) {
      throw(AipsError("Some of the underlying lattices do not have writable masks"));
   }
//
   Bool ok = False;
   if (axis_p==lattices_p[0]->ndim()) {

// Increase dimensionality by one

     ok = putMaskSlice1 (buffer, where, stride, nLattices);
   } else {

// No dimensionality increase

     ok = putMaskSlice2 (buffer, where, stride, nLattices);
   }
}        



template <class T>
Bool LatticeConcat<T>::lock (FileLocker::LockType type, uInt nattempts)
{
    const uInt n = lattices_p.nelements();
    Vector<Bool> hadReadLock(n);
    Vector<Bool> hadWriteLock(n);
//
    for (uInt i=0; i<n; i++) {
       hadReadLock(i) = lattices_p[i]->hasLock(FileLocker::Read);
       hadWriteLock(i) = lattices_p[i]->hasLock(FileLocker::Write);
       if (!lattices_p[i]->lock(type, nattempts)) {

// Try to put things back how they were if fails

          for (uInt j=0; j<i; j++) {
             if (hadReadLock(j)) {
                lattices_p[j]->lock(FileLocker::Read, 1);
             } else if (hadWriteLock(j)) {
                lattices_p[j]->lock(FileLocker::Write, 1);
             } else {
                lattices_p[j]->unlock();
             }
          }
          return False;
       }
    }
    return True;
}

template <class T>
void LatticeConcat<T>::unlock()
{
    const uInt n = lattices_p.nelements();
    for (uInt i=0; i<n; i++) {

// No Bool return so can't check if successful

       lattices_p[i]->unlock();
    }
}


template <class T>
Bool LatticeConcat<T>::hasLock (FileLocker::LockType type) const
{
    const uInt n = lattices_p.nelements();
    for (uInt i=0; i<n; i++) {
       if (!lattices_p[i]->hasLock(type)) return False;
    }
    return True;
}
 

// Private functions



template<class T>
void LatticeConcat<T>::checkAxis(uInt axis, uInt ndim) const
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
Slicer LatticeConcat<T>::setup2 (Bool& first, IPosition& blc2, IPosition& trc2, 
                                 Int shape2, Int axis, const IPosition& blc, 
                                 const IPosition& trc, const IPosition& stride, Int start)
{

// This lattice contributes to the slice.  Find section
// in local lattice[i] coordinates 

   blc2(axis) = max(0,blc(axis)-start);
   trc2(axis) = min(trc(axis)-start,shape2-1);

// Adjust blc for stride if not first lattice

   if (!first) {
      blc2(axis) += (start-blc(axis))%stride(axis);
   }
   first = False;
//
   return Slicer(blc2, trc2, stride, Slicer::endIsLast);
}

template <class T>
Bool LatticeConcat<T>::getSlice1 (Array<T>& buffer,
                                  const Slicer& section,
                                  uInt nLattices)
{
   const uInt dimIn = axis_p;

// The concatenated lattice section

   if (section.end()(axis_p)+1 > Int(nLattices)) {
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

   uInt k = 0;
   for (Int i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
       Array<T> buf = lattices_p[i]->getSlice(section2);
       blc3(axis_p) = k;
       trc3(axis_p) = k;
       buffer(blc3, trc3, stride3) = buf.addDegenerate(1);
       k++;
   }

// Result is a copy

   return False;
}


template <class T>
Bool LatticeConcat<T>::getSlice2 (Array<T>& buffer,
                                  const Slicer& section,
                                  uInt nLattices)
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
   Int start = 0;
   Bool first = True;
   Slicer section2;
//
   for (uInt i=0; i<nLattices; i++) {
//cout << "Lattice shape = " << lattices_p[i]->shape() << endl;

// Find start and end of this lattice inside the concatenated lattice

      Int shape2 = lattices_p[i]->shape()(axis_p);
      Int end = start + shape2 - 1;
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
//
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
   }

// Result is a copy

   return False;
}


template <class T>
Bool LatticeConcat<T>::putSlice1 (const Array<T>& buffer, const IPosition& where,
                                  const IPosition& stride, uInt nLattices)
{
   const uInt dimIn = axis_p;

// The concatenated Lattice section

   Slicer section(where, buffer.shape(), stride, Slicer::endIsLength);
   if (section.end()(axis_p)+1 > Int(nLattices)) {
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
                                  
   uInt k = 0;
   for (Int i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
       blc3(axis_p) = k;
       trc3(axis_p) = k;
       Array<Float> buf0(buffer);
       lattices_p[i]->putSlice(buf0(blc3, trc3, stride3).nonDegenerate(axis_p-1), 
                                section2.start(), section2.stride());
//
       k++;
   }
//  
   return True;
}


template <class T>
Bool LatticeConcat<T>::putSlice2 (const Array<T>& buffer, const IPosition& where,
                                  const IPosition& strider, uInt nLattices)
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
   Int start = 0;
   Bool first = True;
   Slicer section2;
//
   for (uInt i=0; i<nLattices; i++) {
//cout << "Lattice " << i << " shape = " << lattices_p[i]->shape() << endl;

// Find start and end of this lattice inside the concatenated lattice

      Int shape2 = lattices_p[i]->shape()(axis_p);
      Int end = start + shape2 - 1;
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
//
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
   }
//
   return True;
}


template <class T>
Bool LatticeConcat<T>::getMaskSlice1 (Array<Bool>& buffer,
                                      const Slicer& section,
                                      uInt nLattices)
{
   const uInt dimIn = axis_p;

// The concatenated lattice section

   if (section.end()(axis_p)+1 > Int(nLattices)) {
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
                                  
   uInt k = 0;
   for (Int i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
       blc3(axis_p) = k;
       trc3(axis_p) = k;
       Array<Bool> buf = lattices_p[i]->getMaskSlice(section2);
       buffer(blc3, trc3, stride3) = buf.addDegenerate(1);
       k++;
   }

// Result is a copy

   return False;
}
 


template <class T>
Bool LatticeConcat<T>::getMaskSlice2 (Array<Bool>& buffer,
                                      const Slicer& section,
                                      uInt nLattices)
{

// Setup positions

   IPosition blc, trc, stride;   
   IPosition blc2, trc2;
   IPosition blc3, trc3, stride3;
   setup1 (blc, trc, stride, blc2, trc2, blc3, trc3, stride3, section);
//
   buffer.resize(section.length());
//
   Int start = 0;
   Bool first = True;
   Slicer section2;
//
   for (uInt i=0; i<nLattices; i++) {

// Find start and end of this lattice inside the concatenated lattice

      Int shape2 = lattices_p[i]->shape()(axis_p);
      Int end = start + shape2 - 1;
//
      if (! (blc(axis_p)>end || trc(axis_p)<start)) {

// Whack slice into the output

         section2 = setup2(first, blc2, trc2, shape2, axis_p, blc, trc, 
                           stride, start);
//
         trc3(axis_p) = blc3(axis_p) + section2.length()(axis_p) - 1;
         buffer(blc3, trc3, stride3) = lattices_p[i]->getMaskSlice(section2);
//
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
   }

// Result is a copy

   return False;
}



template <class T>
Bool LatticeConcat<T>::putMaskSlice1 (const Array<Bool>& buffer, const IPosition& where,
                                      const IPosition& stride, uInt nLattices)
{
   const uInt dimIn = axis_p;

// The concatenated Lattice section

   Slicer section(where, buffer.shape(), stride, Slicer::endIsLength);
   if (section.end()(axis_p)+1 > Int(nLattices)) {
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
                                  
   uInt k = 0;
   for (Int i=section.start()(axis_p); i<=section.end()(axis_p); i+=section.stride()(axis_p)) {
       blc3(axis_p) = k;
       trc3(axis_p) = k;
       Array<Bool> buf0(buffer);
       lattices_p[i]->putMaskSlice(buf0(blc3, trc3, stride3).nonDegenerate(axis_p-1), 
                                    section2.start(), section2.stride());
//
       k++;
   }
//  
   return True;
}


template <class T>
Bool LatticeConcat<T>::putMaskSlice2 (const Array<Bool>& buffer, const IPosition& where,
                                      const IPosition& strider, uInt nLattices)
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
   Int start = 0;
   Bool first = True;
   Slicer section2;
//
   for (uInt i=0; i<nLattices; i++) {
//cout << "Lattice " << i << " shape = " << lattices_p[i]->shape() << endl;

// Find start and end of this lattice inside the concatenated lattice

      Int shape2 = lattices_p[i]->shape()(axis_p);
      Int end = start + shape2 - 1;
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

         Array<Bool> buf(buffer);
         lattices_p[i]->putMaskSlice(buf(blc3, trc3, stride3), blc2, stride);
//
         blc3(axis_p) += section2.length()(axis_p);
      }
      start += shape2;
   }
//
   return True;
}

