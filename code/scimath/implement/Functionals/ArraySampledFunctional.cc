//# ArraySampledFunctional.cc:  
//# Copyright (C) 1996
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

#include <aips/Functionals/ArraySampledFunctional.h>

template<class T> ArraySampledFunctional<T>::
ArraySampledFunctional()
  :refData(),
   origin(),
   slice(),
   lastAxis(0),
   nElements(0){
}

template<class T> ArraySampledFunctional<T>::
ArraySampledFunctional(const T & data) 
  :refData(data),
   origin(data.origin()),
   slice(data.end()),
   lastAxis(0),
   nElements(0)
{
  IPosition shape = data.shape();
  for (uInt i = 0; i < data.ndim(); i++)
    if (shape(i) > 1) lastAxis = i;
  slice(lastAxis) = 0;
  nElements = shape(lastAxis);
}

template<class T> ArraySampledFunctional<T>::
ArraySampledFunctional(ArraySampledFunctional<T> & other)
  :refData(other.refData),
   origin(other.origin),
   slice(other.slice),
   lastAxis(other.lastAxis),
   nElements(other.nElements){
}

template<class T> ArraySampledFunctional<T> & ArraySampledFunctional<T>::
operator=(ArraySampledFunctional<T> &other){
  if (this != &other){
    refData.reference(other.refData);
    origin = other.origin;
    slice = other.slice;
    lastAxis = other.lastAxis;
    nElements = other.nElements; 
  }
  return *this;
}

template<class T> T ArraySampledFunctional<T>::
operator()(const uInt &index) const {
  IPosition blc(origin); blc(lastAxis) += index;
  IPosition trc(slice); trc(lastAxis) += index;
  // Because refData is const I cannot use the operator() function as this
  // returns a reference. The way around this is to create a non const
  // pointer to the array, call the operator() function and then create a
  // copy (using the copy() function). 
  T *non_const_ptr = (T *) &refData;
  T subarray = non_const_ptr->operator()(blc, trc);
  return subarray.nonDegenerate(lastAxis);
}

template<class T> uInt ArraySampledFunctional<T>::
nelements() const {
  return nElements;
}

template<class T> ArraySampledFunctional<T>::
~ArraySampledFunctional(){
}
