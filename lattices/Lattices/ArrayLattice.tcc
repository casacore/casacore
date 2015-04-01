//# ArrayLattice.cc: this defines the Lattice wrapper class for Arrays.
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
//#
//# $Id$

#ifndef LATTICES_ARRAYLATTICE_TCC
#define LATTICES_ARRAYLATTICE_TCC

#include <casacore/lattices/Lattices/ArrayLattice.h>
//#include <casacore/casa/Lattices/ArrLatticeIter.h>
#include <casacore/lattices/Lattices/LatticeIterInterface.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ArrayLattice<T>::ArrayLattice()
: itsWritable (False)
{
}

template<class T>
ArrayLattice<T>::ArrayLattice (const IPosition& shape) 
: itsData     (shape),
  itsWritable (True)
{
}

template<class T>
ArrayLattice<T>::ArrayLattice (Array<T>& array, Bool isWritable) 
: itsData     (array),
  itsWritable (isWritable)
{
}

template<class T>
ArrayLattice<T>::ArrayLattice (const Array<T>& array) 
: itsData     (array),
  itsWritable (False)
{
}

template<class T>
ArrayLattice<T>::ArrayLattice (const ArrayLattice<T>&other) 
: Lattice<T>(),
  itsData     (other.itsData),
  itsWritable (other.itsWritable)
{
}

template<class T>
ArrayLattice<T>::~ArrayLattice()
{}

template<class T>
ArrayLattice<T>& ArrayLattice<T>::operator= (const ArrayLattice<T>& other)
{
  if (this != &other) {
    itsData     = other.itsData;
    itsWritable = other.itsWritable;
  }
  return *this;
}

template<class T>
Lattice<T>* ArrayLattice<T>::clone() const
{
  return new ArrayLattice<T> (*this);
}

template <class T>
Bool ArrayLattice<T>::canReferenceArray() const
{
  return True;
}

template <class T>
Bool ArrayLattice<T>::isWritable() const
{
  return itsWritable;
}

template<class T>
IPosition ArrayLattice<T>::shape() const
{
  return itsData.shape();
} 

template<class T>
Bool ArrayLattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{
  Array<T> tmp = itsData(section.start(), section.end(), section.stride());
  buffer.reference (tmp);
  return True;
}

template<class T>
void ArrayLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
				  const IPosition& where, 
				  const IPosition& stride)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::putSlice - non-writable lattice"));
  }
  const uInt sdim = sourceBuffer.ndim();
  const uInt ldim = ndim();
  DebugAssert(ldim == where.nelements(), AipsError);
  DebugAssert(ldim == stride.nelements(), AipsError);
  if (sdim == ldim) {
    itsData(where, 
	    where + (sourceBuffer.shape()-1)*stride, 
	    stride) = sourceBuffer;
  } else {
    Array<T> allAxes(sourceBuffer.addDegenerate(ldim-sdim));
    itsData(where, 
	    where + (allAxes.shape()-1)*stride, 
	    stride) = allAxes;
  }
}

template<class T>
void ArrayLattice<T>::getIterSlice (Array<T>& buffer, const IPosition& start,
				    const IPosition& end, const IPosition& incr)
{
    Array<T> tmp (itsData(start, end, incr));
    buffer.reference (tmp);
}


template<class T>
void ArrayLattice<T>::set (const T& value)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::set - non-writable lattice"));
  }
  itsData.set(value);
}

template<class T>
T ArrayLattice<T>::getAt (const IPosition& where) const
{
  return itsData(where);
}

template<class T>
void ArrayLattice<T>::putAt (const T& value, const IPosition& where)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::putAt - non-writable lattice"));
  }
  itsData(where) = value;
}

template<class T>
uInt ArrayLattice<T>::advisedMaxPixels() const
{
  return itsData.nelements();
}

template<class T>
Array<T>& ArrayLattice<T>::asArray()
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::asArray - non-writable lattice"));
  }
  return itsData;
}

template<class T>
const Array<T>& ArrayLattice<T>::asArray() const
{
  return itsData;
}

// Check class invariants. 
template<class T>
Bool ArrayLattice<T>::ok() const
{
  return itsData.ok();
}

} //# NAMESPACE CASACORE - END


#endif
