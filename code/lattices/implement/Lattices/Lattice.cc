//# Lattice.cc:  this defines Lattice.cc, a base for array-related classes
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <aips/aips.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <aips/Arrays/Array.h>
#include <aips/Functionals/Functional.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Mathematics/Math.h>

//-------------------------Lattice---------------------------------

// destructor
template <class T>
Lattice<T>::~Lattice()
{
  // does nothing
}

template <class T>
Bool Lattice<T>::isWritable() const
{
  return True;
}

// rvalue subscript operator for const objects
template <class T>
T Lattice<T>::operator() (const IPosition& where) const
{
  return getAt (where);
}

// returns the number of axes in this Lattice.
template <class T>
uInt Lattice<T>::ndim() const
{
  return shape().nelements();
}

// returns the total number of elements in this Lattice.
template <class T>
uInt Lattice<T>::nelements() const
{
  return shape().product(); 
}

// returns a value of "True" if this Lattice and 'other' have the same 
// shape, otherwise returns a value of "False"
template <class T>
Bool Lattice<T>::conform (const Lattice<T>& other) const
{
  return shape().isEqual (other.shape());
}

// returns a LattCoord object.
template <class T>
LatticeCoordinates Lattice<T>::latticeCoordinates() const
{
  return LatticeCoordinates();
}

template <class T>
void Lattice<T>::putSlice (const Array<T>& sourceBuffer,
			   const IPosition& where)
{
  putSlice (sourceBuffer, where, IPosition(where.nelements(), 1));
}

template<class T>
void Lattice<T>::set (const T& value)
{
  IPosition windowShape(niceCursorShape());
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.woCursor() = value;
  }
}

template<class T>
void Lattice<T>::apply (T (*function) (T))
{
  IPosition windowShape(niceCursorShape());
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.rwCursor().apply (function);
  }
}

template<class T>
void Lattice<T>::apply (T (*function) (const T&))
{
  IPosition windowShape(niceCursorShape());
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.rwCursor().apply(function);
  }
}

template<class T>
void Lattice<T>::apply (const Functional<T,T>& function)
{
  IPosition windowShape(niceCursorShape());
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.rwCursor().apply(function);
  }
}

template<class T>
T Lattice<T>::getAt (const IPosition& where) const
{
  // Use a temporary 1-element array with the correct dimensionality.
  // Casting the const away is harmless.
  T value;
  Array<T> tmp (IPosition(where.nelements(), 1), &value, SHARE);
  ((Lattice<T>*)this)->getSlice (tmp, Slicer(where));
  return value;
}

template<class T>
void Lattice<T>::putAt (const T& value, const IPosition& where)
{
  // Use a temporary 1-element array with the correct dimensionality.
  Array<T> tmp (IPosition(where.nelements(), 1), &value);
  putSlice (tmp, where);
}

template<class T>
void Lattice<T>::copyData (const Lattice<T>& from)
{
  from.copyDataTo (*this);
}

template<class T>
void Lattice<T>::copyDataTo (Lattice<T>& to) const
{
  // Check the lattice is writable.
  // Check the shape conformance.
  AlwaysAssert (to.isWritable(), AipsError);
  const IPosition shapeIn  = shape();
  const IPosition shapeOut = to.shape();
  AlwaysAssert (shapeIn.isEqual (shapeOut), AipsError);
  IPosition cursorShape = to.niceCursorShape();
  LatticeStepper stepper (shapeOut, cursorShape, LatticeStepper::RESIZE);
  // Create an iterator for the output to setup the cache.
  // It is not used, because using putSlice directly is faster and as easy.
  LatticeIterator<T> dummyIter(to, stepper);
  RO_LatticeIterator<T> iter(*this, stepper);
  for (iter.reset(); !iter.atEnd(); iter++) {
    to.putSlice (iter.cursor(), iter.position());
  }
}

template<class T>
LatticeIterInterface<T>* Lattice<T>::makeIter
                                       (const LatticeNavigator& nav) const
{
  return new LatticeIterInterface<T>(*this, nav);
}

template<class T>
uInt Lattice<T>::maxPixels() const
{
  // The returned number of pixels is always a power of two for unknown
  // reasons, and occupies between 4 and 8 MBytes
  return (uInt) pow (2.0, ceil(log(4.0*1024.0*1024.0/sizeof(T))/log(2.0)));
}

template<class T>
IPosition Lattice<T>::niceCursorShape (uInt maxPixels) const
{
  IPosition originalShape(shape());
  uInt ndim = originalShape.nelements();
  IPosition cursorShape(ndim);
  if (ndim > 0) {
    cursorShape = 1;
    cursorShape(0) = originalShape(0);
    for (uInt i=1;
	 i < ndim  &&  cursorShape.product()*originalShape(i) <= Int(maxPixels);
	 i++) {
      cursorShape(i) = originalShape(i);
    }
  }
  return cursorShape;
}

// Check class invariants. 
template <class T>
Bool Lattice<T>::ok() const
{
  return True;
}
