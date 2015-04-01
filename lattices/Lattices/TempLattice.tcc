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

#ifndef LATTICES_TEMPLATTICE_TCC
#define LATTICES_TEMPLATTICE_TCC

#include <casacore/lattices/Lattices/TempLattice.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
TempLattice<T>::~TempLattice()
{}

template<class T>
Lattice<T>* TempLattice<T>::clone() const
{
  return new TempLattice<T> (*this);
}

template<class T>
void TempLattice<T>::flush()
{
  itsImpl->flush();
}

template<class T>
void TempLattice<T>::tempClose()
{
  itsImpl->tempClose();
}

template<class T>
void TempLattice<T>::reopen()
{
  itsImpl->doReopen();
}

template<class T>
Bool TempLattice<T>::isPaged() const
{
  return itsImpl->isPaged();
}

template <class T>
Bool TempLattice<T>::canReferenceArray() const
{
  return itsImpl->canReferenceArray();
}

template<class T>
Bool TempLattice<T>::isWritable() const
{
  return itsImpl->isWritable();
}

template<class T>
IPosition TempLattice<T>::shape() const
{
  return itsImpl->shape();
}

template<class T>
Bool TempLattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{
  return itsImpl->doGetSlice (buffer, section);
}

template<class T>
void TempLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
				 const IPosition& where, 
				 const IPosition& stride)
{
  itsImpl->doPutSlice (sourceBuffer, where, stride);
}

template<class T>
void TempLattice<T>::set (const T& value)
{
  itsImpl->set (value);
}

template<class T>
void TempLattice<T>::apply (T (*function)(T))
{
  itsImpl->apply (function);
}

template<class T>
void TempLattice<T>::apply (T (*function)(const T&))
{
  itsImpl->apply (function);
}

template<class T>
void TempLattice<T>::apply (const Functional<T,T>& function)
{
  itsImpl->apply (function);
}

template<class T>
uInt TempLattice<T>::advisedMaxPixels() const
{
  return itsImpl->advisedMaxPixels();
}

template<class T>
IPosition TempLattice<T>::doNiceCursorShape (uInt maxPixels) const
{
  return itsImpl->doNiceCursorShape (maxPixels);
}


template<class T>
uInt TempLattice<T>::maximumCacheSize() const
{
  return itsImpl->maximumCacheSize();
}

template<class T>
void TempLattice<T>::setMaximumCacheSize (uInt howManyPixels)
{
  itsImpl->setMaximumCacheSize (howManyPixels);
}

template<class T>
void TempLattice<T>::setCacheSizeFromPath (const IPosition& sliceShape,
					   const IPosition& windowStart,
					   const IPosition& windowLength,
					   const IPosition& axisPath)
{
  itsImpl->setCacheSizeFromPath (sliceShape, windowStart, windowLength,
                                 axisPath);
}

template<class T>
void TempLattice<T>::setCacheSizeInTiles (uInt howManyTiles)
{
  itsImpl->setCacheSizeInTiles (howManyTiles);
}

template<class T>
void TempLattice<T>::clearCache()
{
  itsImpl->clearCache();
}

template<class T>
void TempLattice<T>::showCacheStatistics (ostream& os) const
{
  itsImpl->showCacheStatistics (os);
}


template<class T>
T TempLattice<T>::getAt (const IPosition& where) const
{
  return itsImpl->getAt (where);
}

template<class T>
void TempLattice<T>::putAt (const T& value, const IPosition& where)
{
  itsImpl->putAt (value, where);
}

template<class T>
Bool TempLattice<T>::ok() const
{
  return itsImpl->ok();
}

template<class T>
LatticeIterInterface<T>* TempLattice<T>::makeIter (const LatticeNavigator& nav,
						   Bool useRef) const
{
  return itsImpl->makeIter (nav, useRef);
}

} //# NAMESPACE CASACORE - END

#endif
