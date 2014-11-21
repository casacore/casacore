//# PagedArrIter.cc: a concrete iterator for use with PagedArray's.
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2003
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

#include <casacore/lattices/Lattices/PagedArrIter.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/DefaultValue.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
PagedArrIter<T>::PagedArrIter (const PagedArray<T>& data,
			       const LatticeNavigator& nav,
			       Bool useRef)
: LatticeIterInterface<T> (data, nav, useRef),
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
  const ROTiledStManAccessor& acc = itsData.accessor();
  uInt rownr = itsData.rowNumber();
  uInt cacheSize = itsNavPtr->calcCacheSize (acc.hypercubeShape(rownr),
                                             acc.tileShape(rownr),
                                             acc.maximumCacheSize(),
                                             acc.bucketSize(rownr));
  itsData.setCacheSizeInTiles (cacheSize);
}

} //# NAMESPACE CASA - END

