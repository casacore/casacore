//# TempImage.cc: defines the TempImage class
//# Copyright (C) 1998,1999
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

#include <trial/Images/TempImage.h>
#include <trial/Lattices/TempLattice.h>
#include <aips/Quanta/Unit.h>
#include <aips/Arrays/Array.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


template <class T>
TempImage<T>::TempImage()
: mapPtr_p (new TempLattice<T>)
{} 
 
template <class T>
TempImage<T>::TempImage (const TiledShape& mapShape,
			 const CoordinateSystem& coordinateInfo,
			 Double maxMemoryInMb)
: mapPtr_p (new TempLattice<T> (mapShape, maxMemoryInMb))
{
    setCoordinateInfo (coordinateInfo);
}

template <class T>
TempImage<T>::TempImage (const TiledShape& mapShape,
			 const CoordinateSystem& coordinateInfo,
			 Int maxMemoryInMb)
: mapPtr_p (new TempLattice<T> (mapShape, maxMemoryInMb))
{
    setCoordinateInfo (coordinateInfo);
}

template <class T>
TempImage<T>::TempImage (const TempImage<T>& other)
: ImageInterface<T> (other),
  mapPtr_p          (new TempLattice<T> (*other.mapPtr_p)),
  unit_p            (other.unit_p),
  misc_p            (other.misc_p)
{}
 
template <class T>
TempImage<T>& TempImage<T>::operator=(const TempImage<T>& other)
{
    if (this != &other) {
	delete mapPtr_p;
	mapPtr_p = 0;
	ImageInterface<T>::operator= (other);
	mapPtr_p = new TempLattice<T> (*other.mapPtr_p);
	unit_p   = other.unit_p;
	misc_p   = other.misc_p;
    }
    return *this;
} 
 
template <class T>
TempImage<T>::~TempImage()
{
    delete mapPtr_p;
}


template <class T>
Lattice<T>* TempImage<T>::clone() const
{
    return new TempImage (*this);
}   
template <class T>
ImageInterface<T>* TempImage<T>::cloneII() const
{
    return new TempImage (*this);
}


template <class T>
Bool TempImage<T>::isPaged() const
{
    return mapPtr_p->isPaged();
}

template <class T>
Bool TempImage<T>::isWritable() const
{  
    return mapPtr_p->isWritable();
}

template <class T>
IPosition TempImage<T>::shape() const  
{ 
    return mapPtr_p->shape();
}

template <class T>
void TempImage<T>::resize (const TiledShape& newShape)
{
    delete mapPtr_p;
    mapPtr_p = new TempLattice<T> (newShape);
}

template <class T>
Bool TempImage<T>::doGetSlice (Array<T>& buffer,
			       const Slicer& section)
{
    return mapPtr_p->doGetSlice (buffer, section);
} 
   

template <class T>
void TempImage<T>::doPutSlice (const Array<T>& buffer,
			       const IPosition& where,
			       const IPosition& stride)
{
    mapPtr_p->doPutSlice (buffer, where, stride);
}

template<class T> 
Bool TempImage<T>::setUnits (const Unit& unit)
{
    unit_p = unit;
    return True;
}
   
template<class T>
Unit TempImage<T>::units() const
{  
    return unit_p;
}


template <class T> 
String TempImage<T>::name (const Bool) const
{
    return "";
}


template<class T> 
const RecordInterface& TempImage<T>::miscInfo() const
{
    return misc_p;
}
 
template<class T> 
Bool TempImage<T>::setMiscInfo (const RecordInterface& info)
{
    misc_p = info;
    return True;
}
 

template<class T>
void TempImage<T>::set (const T& value)
{
    mapPtr_p->set (value);
}

template<class T>
void TempImage<T>::apply (T (*function)(T))
{
    mapPtr_p->apply (function);
}

template<class T>
void TempImage<T>::apply (T (*function)(const T&))
{
    mapPtr_p->apply (function);
}

template<class T>
void TempImage<T>::apply (const Functional<T,T>& function)
{
    mapPtr_p->apply (function);
}

template<class T>
uInt TempImage<T>::maxPixels() const
{
    return mapPtr_p->maxPixels();
}

template<class T>
IPosition TempImage<T>::doNiceCursorShape (uInt maxPixels) const
{
    return mapPtr_p->niceCursorShape (maxPixels);
}

template<class T>
T TempImage<T>::getAt (const IPosition& where) const
{
    return mapPtr_p->getAt (where);
}

template<class T>
void TempImage<T>::putAt (const T& value, const IPosition& where)
{
    mapPtr_p->putAt (value, where);
}


template <class T>
Bool TempImage<T>::ok() const
{
    return mapPtr_p->ok();
}  


template <class T>
LatticeIterInterface<T>* TempImage<T>::makeIter
                                (const LatticeNavigator& navigator) const
{
    return mapPtr_p->makeIter (navigator);
}

