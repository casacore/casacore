//# Lattice.cc:  this defines Lattice.cc, a base for array-related classes
//# Copyright (C) 1994,1995,1996,1997
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
#include <aips/Arrays/Array.h>
#include <aips/Functionals/Functional.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Mathematics/Math.h>

//-------------------------Lattice---------------------------------

// destructor
template <class T> Lattice<T>::
~Lattice() {
  // does nothing
};

// returns the value at the IPosition location.
template <class T> 
LatticeValueRef<T> Lattice<T>::
operator()(const IPosition & where) {
  return LatticeValueRef<T>(this, where);
};

// rvalue subscript operator for const objects
template <class T> T Lattice<T>::
operator()(const IPosition & where) const {
  return getAt(where);
};

// returns the number of axes in this Lattice.
template <class T> uInt Lattice<T>::
ndim() const {
  return shape().nelements();
};

// returns the total number of elements in this Lattice.
template <class T> uInt Lattice<T>::
nelements() const {
  return shape().product(); 
};

// returns a value of "True" if this Lattice and 'other' have the same 
// shape, otherwise returns a value of "False"
template <class T> Bool Lattice<T>::
conform(const Lattice<T> & other) const {
  return shape().isEqual(other.shape());
};

template <class T> Bool Lattice<T>::
getSlice(COWPtr<Array<T> > & buffer, const IPosition & start,
	 const IPosition & shape, const IPosition & stride, 
	 Bool removeDegenerateAxes) const {
  Slicer section(start, shape, stride);
  return getSlice(buffer, section, removeDegenerateAxes);
};
 
template <class T> Bool Lattice<T>::
getSlice(Array<T> & buffer, const IPosition & start, const IPosition & shape, 
	 const IPosition & stride, Bool removeDegenerateAxes) {
  Slicer section(start, shape, stride);
  return getSlice(buffer, section, removeDegenerateAxes);
};

template <class T> void Lattice<T>::
putSlice(const Array<T> & sourceBuffer, const IPosition & where) {
  putSlice(sourceBuffer, where, IPosition(where.nelements(), 1));
};

template<class T> void Lattice<T>::
set(const T & value) {
  IPosition windowShape(niceCursorShape(maxPixels()));
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++)
    iter.cursor() = value;
};

template<class T> void Lattice<T>::
apply(T (*function) (T)) {
  IPosition windowShape(niceCursorShape(maxPixels()));
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++)
    iter.cursor().apply(function);
};

template<class T> void Lattice<T>::
apply(T (*function) (const T &)) {
  IPosition windowShape(niceCursorShape(maxPixels()));
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++)
    iter.cursor().apply(function);
};

template<class T> void Lattice<T>::
apply(const Functional<T,T> & function) {
  IPosition windowShape(niceCursorShape(maxPixels()));
  LatticeIterator<T> iter(*this, windowShape);
  for (iter.reset(); !iter.atEnd(); iter++)
    iter.cursor().apply(function);
};

template<class T> uInt Lattice<T>::
maxPixels() const {
  // The returned number of pixels is always a power of two for unknown
  // reasons, and occupies between 4 and 8 MBytes
  return (uInt) pow(2.0,ceil(log(4.0*1024.0*1024.0/sizeof(T))/log(2.0)));
};

template<class T> IPosition Lattice<T>::
niceCursorShape(uInt maxPixels) const {
  IPosition originalShape(shape());
  uInt ndim = originalShape.nelements();
  IPosition cursorShape(ndim);
  if (ndim > 0) {
    cursorShape = 1;
    cursorShape(0) = originalShape(0);
    for (uInt i=1; 
	 i < ndim && cursorShape.product()*originalShape(i) <= maxPixels;
	 i++)
      cursorShape(i) = originalShape(i);
  }
  return cursorShape;
};

// Check class invariants. 
template <class T> Bool Lattice<T>::
ok() const {
  return True;
};

//----------------------LatticeValueRef-----------------------------

// constructor
template <class T> LatticeValueRef<T>::
LatticeValueRef(Lattice<T> * source, const IPosition & where) 
  :source_p(source),
   where_p(where) {
  // does nothing
};

// assignment operator
template <class T> 
LatticeValueRef<T> & LatticeValueRef<T>::
operator=(const T & val) {
  source_p->putAt(val,where_p);
  return *this;
};

// conversion operator
template <class T> LatticeValueRef<T>::
operator T() {
  return (source_p->getAt(where_p));
};






