//# TempLattice.cc: A Lattice that can be used for temporary storage
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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

#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/PagedArray.h>
#include <lattices/Lattices/ArrayLattice.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/TableDesc.h>
#include <casa/Arrays/IPosition.h>
#include <casa/System/AppInfo.h>
#include <casa/OS/HostInfo.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
TempLattice<T>::TempLattice() 
: itsTablePtr (0),
  itsIsClosed (False)
{
  itsLatticePtr = new ArrayLattice<T>;
}

template<class T>
TempLattice<T>::TempLattice (const TiledShape& shape, Int maxMemoryInMB)
: itsTablePtr (0),
  itsIsClosed (False)
{
  init (shape, Double(maxMemoryInMB));
}

template<class T>
TempLattice<T>::TempLattice (const TiledShape& shape, Double maxMemoryInMB)
: itsTablePtr (0),
  itsIsClosed (False)
{
  init(shape, maxMemoryInMB);
}

template<class T>
TempLattice<T>::TempLattice (const TempLattice<T>& other)
: Lattice<T>(),
  itsTablePtr (0),
  itsIsClosed (False)
{
  operator= (other);
}

template<class T>
TempLattice<T>::~TempLattice()
{
  // Reopen to make sure that temporary table gets deleted.
  tempReopen();
  delete itsTablePtr;
}

template<class T>
TempLattice<T>& TempLattice<T>::operator= (const TempLattice<T>& other)
{
  if (this != &other) {
    // Reopen to make sure that temporary table gets deleted.
    tempReopen();
    delete itsTablePtr;
    itsTablePtr   = other.itsTablePtr;
    if (itsTablePtr != 0) {
      itsTablePtr = new Table(*itsTablePtr);
    }
    itsLatticePtr = other.itsLatticePtr;
    itsTableName  = other.itsTableName;
    itsIsClosed   = other.itsIsClosed;
  }
  return *this;
}

template<class T>
Lattice<T>* TempLattice<T>::clone() const
{
  return new TempLattice<T> (*this);
}

template<class T>
void TempLattice<T>::init (const TiledShape& shape, Double maxMemoryInMB) 
{
  Double memoryReq = Double(shape.shape().product()*sizeof(T))/(1024.0*1024.0);
  Double memoryAvail;
  // maxMemoryInMb = 0.0 forces disk.
  if (maxMemoryInMB < 0.0) {
    memoryAvail = Double(HostInfo::memoryFree()/1024) / 2.0;
  } else {
    memoryAvail = maxMemoryInMB;
  }
  if (memoryReq > memoryAvail) {
    // Create a table with a unique name in a work directory.
    // We can use exclusive locking, since nobody else should use the table.
    itsTableName = AppInfo::workFileName (Int(memoryReq), "TempLattice");
    SetupNewTable newtab (itsTableName, TableDesc(), Table::Scratch);
    itsTablePtr = new Table (newtab, TableLock::PermanentLockingWait);
    itsLatticePtr = new PagedArray<T> (shape, *itsTablePtr);
  } else {
    itsLatticePtr = new ArrayLattice<T> (shape.shape());
  }
}

template<class T>
void TempLattice<T>::flush()
{
  if (itsTablePtr != 0) {
    itsTablePtr->flush();
  }
}

template<class T>
void TempLattice<T>::tempClose()
{
  if (itsTablePtr != 0 && isPaged()) {
    // Take care that table does not get deleted, otherwise we cannot reopen.
    itsTablePtr->unmarkForDelete();
    delete itsTablePtr;
    itsTablePtr = 0;
    itsLatticePtr = 0;           // CountedPtr does delete of pointer
    itsIsClosed = True;
  }
}

template<class T>
void TempLattice<T>::reopen()
{
  doReopen();
}

template<class T>
void TempLattice<T>::tempReopen() const
{
  if (itsIsClosed && isPaged()) {
    itsTablePtr = new Table (itsTableName,
			     TableLock(TableLock::PermanentLockingWait),
			     Table::Update);
    itsLatticePtr = new PagedArray<T> (*itsTablePtr);
    itsIsClosed = False;
  }
  if (itsTablePtr != 0) {
    itsTablePtr->markForDelete();
  }
}

template<class T>
Bool TempLattice<T>::isPaged() const
{
  return  (! itsTableName.empty());
}

template <class T>
Bool TempLattice<T>::canReferenceArray() const
{
  return  (itsTableName.empty());
}

template<class T>
Bool TempLattice<T>::isWritable() const
{
  return True;
}

template<class T>
IPosition TempLattice<T>::shape() const
{
  doReopen();
  return itsLatticePtr->shape();
}

template<class T>
Bool TempLattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{
  doReopen();
  return itsLatticePtr->doGetSlice (buffer, section);
}

template<class T>
void TempLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
				 const IPosition& where, 
				 const IPosition& stride)
{
  doReopen();
  itsLatticePtr->putSlice (sourceBuffer, where, stride);
}

template<class T>
void TempLattice<T>::set (const T& value)
{
  doReopen();
  itsLatticePtr->set (value);
}

template<class T>
void TempLattice<T>::apply (T (*function)(T))
{
  doReopen();
  itsLatticePtr->apply (function);
}

template<class T>
void TempLattice<T>::apply (T (*function)(const T&))
{
  doReopen();
  itsLatticePtr->apply (function);
}

template<class T>
void TempLattice<T>::apply (const Functional<T,T>& function)
{
  doReopen();
  itsLatticePtr->apply (function);
}

template<class T>
uInt TempLattice<T>::advisedMaxPixels() const
{
  doReopen();
  return itsLatticePtr->advisedMaxPixels();
}

template<class T>
IPosition TempLattice<T>::doNiceCursorShape (uInt maxPixels) const
{
  doReopen();
  return itsLatticePtr->niceCursorShape (maxPixels);
}


template<class T>
uInt TempLattice<T>::maximumCacheSize() const
{
  return itsLatticePtr->maximumCacheSize();
}

template<class T>
void TempLattice<T>::setMaximumCacheSize (uInt howManyPixels)
{
  itsLatticePtr->setMaximumCacheSize (howManyPixels);
}

template<class T>
void TempLattice<T>::setCacheSizeFromPath (const IPosition& sliceShape,
					   const IPosition& windowStart,
					   const IPosition& windowLength,
					   const IPosition& axisPath)
{
  itsLatticePtr->setCacheSizeFromPath (sliceShape, windowStart, windowLength,
				       axisPath);
}

template<class T>
void TempLattice<T>::setCacheSizeInTiles (uInt howManyTiles)
{
  itsLatticePtr->setCacheSizeInTiles (howManyTiles);
}

template<class T>
void TempLattice<T>::clearCache()
{
  itsLatticePtr->clearCache();
}

template<class T>
void TempLattice<T>::showCacheStatistics (ostream& os) const
{
  itsLatticePtr->showCacheStatistics (os);
}


template<class T>
T TempLattice<T>::getAt (const IPosition& where) const
{
  doReopen();
  return itsLatticePtr->getAt (where);
}

template<class T>
void TempLattice<T>::putAt (const T& value, const IPosition& where)
{
  doReopen();
  itsLatticePtr->putAt (value, where);
}

template<class T>
Bool TempLattice<T>::ok() const
{
  doReopen();
  return itsLatticePtr->ok();
}

template<class T>
LatticeIterInterface<T>* TempLattice<T>::makeIter (const LatticeNavigator& nav,
						   Bool useRef) const
{
  doReopen();
  return itsLatticePtr->makeIter (nav, useRef);
}

} //# NAMESPACE CASA - END

