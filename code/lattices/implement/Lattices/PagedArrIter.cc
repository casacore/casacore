//# PagedArrIter.cc: a concrete iterator for use with PagedArray's.
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#include <aips/Lattices/PagedArrIter.h>
#include <aips/Lattices/LatticeStepper.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Utilities/DefaultValue.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


template<class T>
PagedArrIter<T>::PagedArrIter (const PagedArray<T>& data,
			       const LatticeNavigator& nav)
: LatticeIterInterface<T> (data, nav),
  itsData (data)
{
  setupTileCache();
}

template<class T>
PagedArrIter<T>::PagedArrIter (const PagedArrIter<T>& other)
: LatticeIterInterface<T> (other),
  itsData (other.itsData)
{}

template<class T>
PagedArrIter<T>::~PagedArrIter()
{
  itsData.clearCache();
}

template<class T>
PagedArrIter<T>& PagedArrIter<T>::operator= (const PagedArrIter<T>& other)
{
  if (this != &other) {
    rewriteData();
    itsData.clearCache();
    LatticeIterInterface<T>::operator= (other);
    itsData = other.itsData;
  }
  return *this;
}

template<class T>
LatticeIterInterface<T>* PagedArrIter<T>::clone() const
{
    return new PagedArrIter<T> (*this);
}

template<class T>
void PagedArrIter<T>::setupTileCache()
{
  uInt cacheSize = itsNavPtr->calcCacheSize (&(itsData.accessor()),
					     itsData.rowNumber());
  itsData.setCacheSizeInTiles (cacheSize);
}
