//# Lattice.cc:  this defines Lattice.cc, a base for array-related classes
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2003
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

#ifndef LATTICES_LATTICE_TCC
#define LATTICES_LATTICE_TCC

#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Functional.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// destructor
template <class T>
Lattice<T>::~Lattice()
{
  // does nothing
}

template <class T>
DataType Lattice<T>::dataType() const
{
  return whatType ((T*)0);
}


// rvalue subscript operator for const objects
template <class T>
T Lattice<T>::operator() (const IPosition& where) const
{
  return getAt (where);
}

template<class T>
Bool Lattice<T>::get (COWPtr<Array<T> >& buffer,
		      Bool removeDegenerateAxes) const
{
  uInt nd = ndim();
  return getSlice (buffer, Slicer(IPosition(nd,0), shape()),
		   removeDegenerateAxes);
}

template<class T>
Bool Lattice<T>::get (Array<T>& buffer,
		      Bool removeDegenerateAxes)
{
  uInt nd = ndim();
  return getSlice (buffer, Slicer(IPosition(nd,0), shape()),
		   removeDegenerateAxes);
}

template<class T>
Array<T> Lattice<T>::get (Bool removeDegenerateAxes) const
{
  uInt nd = ndim();
  return getSlice (Slicer(IPosition(nd,0), shape()),
		   removeDegenerateAxes);
}

template<class T>
Bool Lattice<T>::getSlice (COWPtr<Array<T> >& buffer, const IPosition& start, 
			   const IPosition& shape,
			   Bool removeDegenerateAxes) const
{
  return getSlice (buffer, Slicer(start, shape),
		   removeDegenerateAxes);
}

template<class T>
Bool Lattice<T>::getSlice (COWPtr<Array<T> >& buffer, const IPosition& start, 
			   const IPosition& shape,
			   const IPosition& stride,
			   Bool removeDegenerateAxes) const
{
  return getSlice (buffer, Slicer(start, shape, stride),
		   removeDegenerateAxes);
}

template<class T>
Bool Lattice<T>::getSlice (COWPtr<Array<T> >& buffer,
			   const Slicer& section,
			   Bool removeDegenerateAxes) const
{
  // Cast pointer to non-const.
  // This is safe, since the array is copied when needed by COWptr.
  Lattice<T>* This = (Lattice<T>*)this;
  // The COWPtr takes over the pointer to the array.
  Array<T>* arr = new Array<T>;
  Bool isARef = This->getSlice (*arr, section, removeDegenerateAxes);
  buffer = COWPtr<Array<T> > (arr, True, isARef);
  return False;
}

template<class T>
Bool Lattice<T>::getSlice (Array<T>& buffer, const IPosition& start,
			   const IPosition& shape,
			   Bool removeDegenerateAxes)
{
  return getSlice (buffer, Slicer(start, shape), removeDegenerateAxes);
}

template<class T>
Bool Lattice<T>::getSlice (Array<T>& buffer, const IPosition& start,
			   const IPosition& shape, const IPosition& stride,
			   Bool removeDegenerateAxes)
{
  return getSlice (buffer, Slicer(start, shape, stride),
		   removeDegenerateAxes);
}

template<class T>
Bool Lattice<T>::getSlice (Array<T>& buffer, const Slicer& section,
			   Bool removeDegenerateAxes)
{
  Bool isARef;
  // When the slicer is fixed, it can be used immediately.
  // Otherwise unspecified values are to be filled in.
  if (section.isFixed()) {
    IPosition shp(shape());
    if (section.ndim() != shp.nelements()  ||  section.end() >= shp) {
      throw AipsError ("Lattice::getSlice - section outside lattice");
    }
    isARef = doGetSlice (buffer, section);
  } else {
    IPosition blc,trc,inc;
    section.inferShapeFromSource (shape(), blc, trc, inc);
    isARef = doGetSlice (buffer, Slicer(blc,trc,inc,Slicer::endIsLast));
  }
  if (removeDegenerateAxes) {
    Array<T> tmp = buffer.nonDegenerate();
    buffer.reference (tmp);
  }
  return isARef;
}

template<class T>
Array<T> Lattice<T>::getSlice (const IPosition& start,
			       const IPosition& shape,
			       Bool removeDegenerateAxes) const
{
  return getSlice (Slicer(start,shape), removeDegenerateAxes);
}

template<class T>
Array<T> Lattice<T>::getSlice (const IPosition& start,
			       const IPosition& shape,
			       const IPosition& stride,
			       Bool removeDegenerateAxes) const
{
  return getSlice (Slicer(start,shape,stride), removeDegenerateAxes);
}

template<class T>
Array<T> Lattice<T>::getSlice (const Slicer& section,
			       Bool removeDegenerateAxes) const
{
  // Cast pointer to non-const.
  // This is safe, since the array is copied when needed.
  Lattice<T>* This = (Lattice<T>*)this;
  // Note that getSlice is used to be sure that section gets filled
  // when needed.
  Array<T> arr;
  Bool isARef = This->getSlice (arr, section, removeDegenerateAxes);
  // When not referenced, return it as such.
  // Otherwise make a copy.
  if (!isARef) {
    return arr;
  }
  Array<T> tmp;
  tmp = arr;
  return tmp;
}


template<class T>
void Lattice<T>::putSlice (const Array<T>& sourceBuffer,
			   const IPosition& where)
{
  doPutSlice (sourceBuffer, where, IPosition(where.nelements(),1));
}

template<class T>
void Lattice<T>::put (const Array<T>& sourceBuffer)
{
  uInt nd = ndim();
  doPutSlice (sourceBuffer, IPosition(nd,0), IPosition(nd,1));
}

template<class T>
void Lattice<T>::set (const T& value)
{
  LatticeIterator<T> iter(*this, True);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.woCursor() = value;
  }
}

template<class T>
void Lattice<T>::apply (T (*function) (T))
{
  LatticeIterator<T> iter(*this, True);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.rwCursor().apply (function);
  }
}

template<class T>
void Lattice<T>::apply (T (*function) (const T&))
{
  LatticeIterator<T> iter(*this, True);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.rwCursor().apply(function);
  }
}

template<class T>
void Lattice<T>::apply (const Functional<T,T>& function)
{
  LatticeIterator<T> iter(*this, True);
  for (iter.reset(); !iter.atEnd(); iter++) {
    iter.rwCursor().apply(function);
  }
}

template<class T>
T Lattice<T>::getAt (const IPosition& where) const
{
  // Casting the const away is harmless.
  Array<T> tmp;
  ((Lattice<T>*)this)->doGetSlice (tmp, Slicer(where));
  // Since the array contains 1 element only, getStorage does not
  // create a copy.
  Bool deleteIt;
  return *(tmp.getStorage(deleteIt));
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
void Lattice<T>::handleMath (const Lattice<T>& from, int oper)
{
  from.handleMathTo (*this, oper);
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
  RO_LatticeIterator<T> iter(*this, stepper, True);
  for (iter.reset(); !iter.atEnd(); iter++) {
    to.putSlice (iter.cursor(), iter.position());
  }
}

template<class T>
void Lattice<T>::handleMathTo (Lattice<T>& to, int oper) const
{
  // Check the lattice is writable.
  // Check the shape conformance.
  AlwaysAssert (to.isWritable(), AipsError);
  const IPosition shapeIn  = shape();
  const IPosition shapeOut = to.shape();
  AlwaysAssert (shapeIn.isEqual (shapeOut), AipsError);
  IPosition cursorShape = to.niceCursorShape();
  LatticeStepper stepper (shapeOut, cursorShape, LatticeStepper::RESIZE);
  // Create an iterator for the output.
  // If possible, use reference semantics in the iterators.
  LatticeIterator<T> toIter(to, stepper, True);
  RO_LatticeIterator<T> iter(*this, stepper, True);
  switch (oper) {
  case 0:
    for (iter.reset(); !iter.atEnd(); iter++, toIter++) {
      toIter.rwCursor() += iter.cursor();
    }
    break;
  case 1:
    for (iter.reset(); !iter.atEnd(); iter++) {
      toIter.rwCursor() -= iter.cursor();
    }
    break;
  case 2:
    for (iter.reset(); !iter.atEnd(); iter++) {
      toIter.rwCursor() *= iter.cursor();
    }
    break;
  case 3:
    for (iter.reset(); !iter.atEnd(); iter++) {
      toIter.rwCursor() /= iter.cursor();
    }
    break;
  default:
    throw AipsError ("Lattice::handleMathTo - Unknown operator");
  }
}

template<class T>
LatticeIterInterface<T>* Lattice<T>::makeIter (const LatticeNavigator& nav,
					       Bool useRef) const
{
  return new LatticeIterInterface<T>(*this, nav, useRef);
}

template<class T>
uInt Lattice<T>::advisedMaxPixels() const
{
  // The returned number of pixels is always a power of two for unknown
  // reasons, and occupies between 4 and 8 MBytes
  return (uInt) pow (2.0, ceil(log(4.0*1024.0*1024.0/sizeof(T))/log(2.0)));
}

} //# NAMESPACE CASACORE - END


#endif
