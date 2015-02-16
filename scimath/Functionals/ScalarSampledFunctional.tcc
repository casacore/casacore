//# ScalarSampledFunctional.cc
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

#ifndef SCIMATH_SCALARSAMPLEDFUNCTIONAL_TCC
#define SCIMATH_SCALARSAMPLEDFUNCTIONAL_TCC

#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> ScalarSampledFunctional<T>::
ScalarSampledFunctional(){
}

template<class T> ScalarSampledFunctional<T>::
ScalarSampledFunctional(const Block<T> & data)
  :refData(data) {
}

template<class T> ScalarSampledFunctional<T>::
ScalarSampledFunctional(Vector<T> & data)
  :refData(data) {
}

template<class T> ScalarSampledFunctional<T>::
ScalarSampledFunctional(const Vector<T> & data)
  :refData(data.copy()){
}

template<class T> ScalarSampledFunctional<T>::
ScalarSampledFunctional(ScalarSampledFunctional<T> & other)
  : SampledFunctional<T>(other),
    refData(other.refData){
}

template<class T> ScalarSampledFunctional<T>::
ScalarSampledFunctional(const ScalarSampledFunctional<T> & other)
  : SampledFunctional<T>(other),
    refData(other.refData.copy()){
}

template<class T> ScalarSampledFunctional<T> & ScalarSampledFunctional<T>::
operator=(ScalarSampledFunctional<T> &other) {
  if (this != &other){
    refData.reference(other.refData);
  }
  return *this;
}

template<class T> ScalarSampledFunctional<T> & ScalarSampledFunctional<T>::
operator=(const ScalarSampledFunctional<T> &other) {
  if (this != &other){
    refData = other.refData;
  }
  return *this;
}

template<class T> T ScalarSampledFunctional<T>::
operator()(const uInt &index) const {
  return refData(index);
}

template<class T> uInt ScalarSampledFunctional<T>::
nelements() const {
  return refData.nelements();
}

template<class T> ScalarSampledFunctional<T>::
~ScalarSampledFunctional(){
}

} //# NAMESPACE CASACORE - END


#endif
