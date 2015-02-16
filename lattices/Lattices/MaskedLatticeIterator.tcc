//# MaskedLatticeIterator.cc: defines the RO_MaskedLatticeIterator class
//# Copyright (C) 2003
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

#ifndef LATTICES_MASKEDLATTICEITERATOR_TCC
#define LATTICES_MASKEDLATTICEITERATOR_TCC

#include <casacore/casa/aips.h>

#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h> 
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
RO_MaskedLatticeIterator<T>::RO_MaskedLatticeIterator()
: itsMaskLattPtr (0)
{}

template <class T>
RO_MaskedLatticeIterator<T>::RO_MaskedLatticeIterator
                                          (const MaskedLattice<T>& mlattice,
					   Bool useRef)
: RO_LatticeIterator<T> (mlattice, useRef)
{
  fillPtr (mlattice);
}

template <class T>
RO_MaskedLatticeIterator<T>::RO_MaskedLatticeIterator
                                          (const MaskedLattice<T>& mlattice,
					   const LatticeNavigator& method,
					   Bool useRef)
: RO_LatticeIterator<T> (mlattice, method, useRef)
{
  fillPtr (mlattice);
}

template <class T>
RO_MaskedLatticeIterator<T>::RO_MaskedLatticeIterator
                                          (const MaskedLattice<T>& mlattice,
					   const IPosition& cursorShape,
					   Bool useRef)
: RO_LatticeIterator<T> (mlattice, cursorShape, useRef)
{
  fillPtr (mlattice);
}

template <class T>
RO_MaskedLatticeIterator<T>::RO_MaskedLatticeIterator
                                 (const RO_MaskedLatticeIterator<T>& other)
: RO_LatticeIterator<T> (other),
  itsMaskLattPtr (other.itsMaskLattPtr)
{}

template <class T>
RO_MaskedLatticeIterator<T>::RO_MaskedLatticeIterator
                                 (const RO_LatticeIterator<T>& other,
				  const RO_MaskedLatticeIterator<T>& otherm)
: RO_LatticeIterator<T> (other)
{
  if (!isNull()) {
    fillPtr (otherm.lattice());
  }
}

template <class T>
RO_MaskedLatticeIterator<T>::~RO_MaskedLatticeIterator()
{}

template <class T>
RO_MaskedLatticeIterator<T>& RO_MaskedLatticeIterator<T>::operator=
                                 (const RO_MaskedLatticeIterator<T>& other)
{
  if (this != &other) {
    RO_LatticeIterator<T>::operator= (other);
    itsMaskLattPtr = other.itsMaskLattPtr;
  }
  return *this;
}

template <class T>
RO_MaskedLatticeIterator<T> RO_MaskedLatticeIterator<T>::copy() const
{
  if (isNull()) {
    return RO_MaskedLatticeIterator<T>();
  }
  return RO_MaskedLatticeIterator<T>(RO_LatticeIterator<T>::copy(), *this);
}

template <class T>
void RO_MaskedLatticeIterator<T>::fillPtr (const MaskedLattice<T>& mlattice)
{
  Lattice<T>* lptr = &(RO_LatticeIterator<T>::lattice());
  MaskedLattice<T>* mptr = dynamic_cast<MaskedLattice<T>*>(lptr);
  if (mptr) {
    itsMaskLattPtr = CountedPtr<MaskedLattice<T> > (mptr, False);
  } else {
    itsMaskLattPtr = mlattice.cloneML();
  }
}

template <class T>
Array<Bool> RO_MaskedLatticeIterator<T>::getMask
                                         (Bool removeDegenerateAxes) const
{
  return itsMaskLattPtr->getMaskSlice (Slicer(position(),
					      endPosition(),
					      Slicer::endIsLast),
				       removeDegenerateAxes);
}

template <class T>
Bool RO_MaskedLatticeIterator<T>::getMask (COWPtr<Array<Bool> >& arr,
					   Bool removeDegenerateAxes) const
{
  return itsMaskLattPtr->getMaskSlice (arr, position(), cursorShape(),
				       removeDegenerateAxes);
}

template <class T>
Bool RO_MaskedLatticeIterator<T>::getMask (Array<Bool>& arr,
					   Bool removeDegenerateAxes) const
{
  return itsMaskLattPtr->getMaskSlice (arr, position(), cursorShape(),
				       removeDegenerateAxes);
}

} //# NAMESPACE CASACORE - END


#endif
