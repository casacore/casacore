//# ArrayLattice.h: this defines the Lattice wrapper class for Arrays.
//# Copyright (C) 1995
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

#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/ArrLatticeIter.h>
#include <trial/Lattices/LatticeIterInterface.h>
#include <trial/Lattices/LatticeNavigator.h>

#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/COWPtr.h>

#include <iostream.h>

// default ctor
template<class T> ArrayLattice<T>::ArrayLattice() 
{
    // Nothing
};

// shape ctor
template<class T> ArrayLattice<T>::ArrayLattice(const IPosition &shape) 
: array_p(shape)
{
    // Nothing
};

// ctor
template<class T> ArrayLattice<T>::ArrayLattice(const Array<T> &array) 
: array_p(array)
{
    // Nothing
};

// copy ctor (reference semantics)
template<class T> ArrayLattice<T>::ArrayLattice(const ArrayLattice<T> &other) 
: array_p(other.array_p)
{
    // Nothing
};

template<class T> ArrayLattice<T>::~ArrayLattice()
{
    // Nothing
};

// assignment (reference semantics)
template<class T> 
ArrayLattice<T> &ArrayLattice<T>::operator=(const ArrayLattice<T> &other) 
{
  if (this != &other) 
    array_p.reference(other.array_p);
  return *this;
};

// returns the shape of the Lattice.
template<class T> IPosition ArrayLattice<T>::shape() const
{
  return array_p.shape();
}; 

template<class T>
Bool ArrayLattice<T>::getSlice(COWPtr<Array<T> > &buffer, 
			       const IPosition &start, const IPosition &shape,
			       const IPosition &stride,
			       Bool removeDegenerateAxes) const
{
  Slicer slicer(start, shape, stride);
  return getSlice(buffer, slicer, removeDegenerateAxes);
};

template<class T>
Bool ArrayLattice<T>::getSlice(COWPtr<Array<T> > &buffer, 
			       const Slicer &theSlice, 
			       Bool removeDegenerateAxes) const
{
  // caste away the constness of the datamember with pointer copies
  Array<T> &ARRAY_P = (Array<T> &)array_p;
  // set the COWPtr to reference the copy.
  if (removeDegenerateAxes) 
    buffer.set(new Array<T>(ARRAY_P(theSlice.start(), theSlice.end(), 
				    theSlice.stride()).nonDegenerate()),
	       True, True);
  else 
    buffer.set(new Array<T>(ARRAY_P(theSlice.start(), theSlice.end(), 
				    theSlice.stride())), True, True);
  return False;
};

template<class T>
Bool ArrayLattice<T>::getSlice(Array<T> &buffer, const IPosition &start, 
			       const IPosition &shape, const IPosition &stride,
			       Bool removeDegenerateAxes)
{  
  Slicer theSlice(start, shape, stride);
  return getSlice(buffer, theSlice, removeDegenerateAxes);
};

template<class T>
Bool ArrayLattice<T>::getSlice(Array<T> &buffer, const Slicer &theSlice, 
			       Bool removeDegenerateAxes)
{
  if (removeDegenerateAxes) {
    buffer.reference(array_p(theSlice.start(),theSlice.end(),
				    theSlice.stride()).nonDegenerate());
  } else { 
    buffer.reference(array_p(theSlice.start(),theSlice.end(),
				    theSlice.stride()));
  }
  return True;
};

template<class T>
void ArrayLattice<T>::putSlice(const Array<T> &sourceBuffer, 
			       const IPosition &where, const IPosition &stride)
{
  array_p(where, where + sourceBuffer.shape() - 1, stride) = sourceBuffer;
};

template<class T>
void ArrayLattice<T>::set(const T &value)
{
  array_p.set(value);
};

template <class T> T ArrayLattice<T>::getAt(const IPosition &where) const
{
  return array_p(where);
};

template <class T> 
void ArrayLattice<T>::putAt(const T &value, const IPosition &where)
{
  array_p(where) = value;
};

template <class T> 
RO_LatticeIterInterface<T> *ArrayLattice<T>::makeIter(
				            const LatticeNavigator &nav) const
{
  return new RO_ArrLatticeIter<T>(*this, nav);
};

template <class T> 
RO_LatticeIterInterface<T> *ArrayLattice<T>::makeIter(
					         const IPosition &shape) const 
{
  return new RO_ArrLatticeIter<T>(*this, shape);
};

template <class T> 
LatticeIterInterface<T> *ArrayLattice<T>::makeIter(const LatticeNavigator &nav)
{
  return new ArrLatticeIter<T>(*this, nav);
};

template <class T> 
LatticeIterInterface<T> *ArrayLattice<T>::makeIter(const IPosition &shape)
{
  return new ArrLatticeIter<T>(*this, shape);
};

template <class T> Array<T> &ArrayLattice<T>::asArray() 
{
  return array_p;
};

template <class T> const Array<T> &ArrayLattice<T>::asArray() const 
{
  return array_p;
};

// Check class invariants. 
template <class T> Bool ArrayLattice<T>::ok() const
{
  return array_p.ok();
};




