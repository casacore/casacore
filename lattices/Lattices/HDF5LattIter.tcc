//# HDF5LattIter.cc: a concrete iterator for use with HDF5Lattices.
//# Copyright (C) 2009
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

#ifndef LATTICES_HDF5LATTITER_TCC
#define LATTICES_HDF5LATTITER_TCC

#include <casacore/lattices/Lattices/HDF5LattIter.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/DefaultValue.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
HDF5LattIter<T>::HDF5LattIter (const HDF5Lattice<T>& data,
			       const LatticeNavigator& nav,
			       Bool useRef)
: LatticeIterInterface<T> (data, nav, useRef),
  itsData (data)
{
  setupTileCache();
}

template<class T>
HDF5LattIter<T>::HDF5LattIter (const HDF5LattIter<T>& other)
: LatticeIterInterface<T> (other),
  itsData (other.itsData)
{}

template<class T>
HDF5LattIter<T>::~HDF5LattIter()
{
  itsData.clearCache();
}

template<class T>
HDF5LattIter<T>& HDF5LattIter<T>::operator= (const HDF5LattIter<T>& other)
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
LatticeIterInterface<T>* HDF5LattIter<T>::clone() const
{
    return new HDF5LattIter<T> (*this);
}

template<class T>
void HDF5LattIter<T>::setupTileCache()
{
  const IPosition& tileShape = itsData.niceCursorShape();
  uInt cacheSize = itsNavPtr->calcCacheSize (itsData.shape(),
                                             tileShape,
                                             0,
                                             tileShape.product());
  itsData.setCacheSizeInTiles (cacheSize);
}

} //# NAMESPACE CASACORE - END


#endif
