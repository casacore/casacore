//# TempLattice.cc:
//# Copyright (C) 1997,1998
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

#include <trial/Lattices/TempLattice.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/TiledShape.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Tasking/AppInfo.h>

template<class T>
TempLattice<T>::TempLattice() 
{
  itsLatticePtr = new ArrayLattice<T>;
}

template<class T>
TempLattice<T>::TempLattice (const IPosition& shape, Int maxMemoryInMB) 
{
  uInt memoryReq = shape.product()*sizeof(T);
  uInt memoryAvail;
  if (maxMemoryInMB > 0) {
    memoryAvail = maxMemoryInMB;
  } else {
    memoryAvail = AppInfo::memoryInMB() / 4;
  }
  memoryAvail *= 1024*1024;

  if (memoryReq > memoryAvail) {
    itsLatticePtr = new PagedArray<T>(TiledShape(shape));
  } else {
    itsLatticePtr = new ArrayLattice<T>(shape);
  }
}

template<class T>
TempLattice<T>::TempLattice (const TempLattice<T>& other)
: itsLatticePtr (other.itsLatticePtr)
{}

template<class T>
TempLattice<T>::~TempLattice()
{}

template<class T>
TempLattice<T>& TempLattice<T>::operator= (const TempLattice<T>& other)
{
  if (this != &other) {
    itsLatticePtr = other.itsLatticePtr;
  }
  return *this;
}

template<class T>
Lattice<T>* TempLattice<T>::clone() const
{
  return new TempLattice<T> (*this);
}

template<class T>
Bool TempLattice<T>::isPaged() const
{
  return itsLatticePtr->isPaged();
}

template<class T>
Bool TempLattice<T>::isWritable() const
{
  return itsLatticePtr->isWritable();
}

template<class T>
IPosition TempLattice<T>::shape() const
{
  return itsLatticePtr->shape();
}

template<class T>
uInt TempLattice<T>::ndim() const
{
  return itsLatticePtr->ndim();
}

template<class T>
uInt TempLattice<T>::nelements() const
{
  return itsLatticePtr->nelements();
}

template<class T>
Bool TempLattice<T>::conform(const Lattice<T>& other) const
{
  return itsLatticePtr->conform(other);
}

template<class T>
Bool TempLattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{
  return itsLatticePtr->doGetSlice (buffer, section);
}

template<class T>
void TempLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
				 const IPosition& where, 
				 const IPosition& stride)
{
  itsLatticePtr->putSlice (sourceBuffer, where, stride);
}

template<class T>
void TempLattice<T>::set (const T& value)
{
  itsLatticePtr->set (value);
}

template<class T>
void TempLattice<T>::apply(T (*function)(T))
{
  itsLatticePtr->apply (function);
}

template<class T>
void TempLattice<T>::apply (T (*function)(const T&))
{
  itsLatticePtr->apply (function);
}

template<class T>
void TempLattice<T>::apply (const Functional<T,T>& function)
{
  itsLatticePtr->apply (function);
}

template<class T>
uInt TempLattice<T>::maxPixels() const
{
  return itsLatticePtr->maxPixels();
}

template<class T>
IPosition TempLattice<T>::doNiceCursorShape (uInt maxPixels) const
{
  return itsLatticePtr->niceCursorShape (maxPixels);
}

template<class T>
T TempLattice<T>::getAt (const IPosition& where) const
{
  return itsLatticePtr->getAt (where);
}

template<class T>
void TempLattice<T>::putAt (const T& value, const IPosition& where)
{
  itsLatticePtr->putAt (value, where);
}

template<class T>
Bool TempLattice<T>::ok() const
{
  return itsLatticePtr->ok();
}

template<class T>
LatticeIterInterface<T>* TempLattice<T>::makeIter
                                   (const LatticeNavigator& navigator) const
{
  return itsLatticePtr->makeIter (navigator);
}
