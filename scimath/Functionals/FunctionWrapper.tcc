//# FunctionWrapper.cc:  Construct function objects from C++ functions 
//# Copyright (C) 1995,1996,2001,2002
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
//#
//# $Id$

#ifndef SCIMATH_FUNCTIONWRAPPER_TCC
#define SCIMATH_FUNCTIONWRAPPER_TCC

//# Includes
#include <casacore/scimath/Functionals/FunctionWrapper.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/scimath/Functionals/WrapperData.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class T>
FunctionWrapper<T>::FunctionWrapper() :
  WrapperParam<T>(0),
  doit_p(0) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const T&), const Bool) : 
  WrapperParam<T>(0),
  doit_p(new WrapperData<T,T,T,False,True>(f)) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const Vector<T>&),
                                    const Bool) :
  WrapperParam<T>(0),
  doit_p(new WrapperData<T,T,Vector<T>,False,True>(f)) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)()) :
  WrapperParam<T>(0),
  doit_p(new WrapperData<T,T,T,False,False>(f)) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const T&)) :
  WrapperParam<T>(0),
  doit_p(new WrapperData<T,T,T,True,False>(f,1)) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const T&, const T&),
					const T &par) :
  WrapperParam<T>(1),
  doit_p(new WrapperData<T,T,T,True,True>(f,1)) {
  param_p[0] = par;
}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const T&, const Vector<T>&),
					const Vector<T> &par) :
  WrapperParam<T>(par),
  doit_p(new WrapperData<T,T,Vector<T>,True,True>(f,1)) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const Vector<T>&),
					const Int dim) :
  WrapperParam<T>(0),
  doit_p(new WrapperData<T,Vector<T>,T,True,False>(f,dim)) {}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const Vector<T>&, const T&),
					const T &par, const uInt dim) :
  WrapperParam<T>(1),
  doit_p(new WrapperData<T,Vector<T>,T,True,True>(f,dim)) {
  param_p[0] = par;
}

template <class T>
FunctionWrapper<T>::FunctionWrapper(T(*f)(const Vector<T>&,
					      const Vector<T>&),
					const Vector<T> &par,
					const uInt dim) :
  WrapperParam<T>(par),
  doit_p(new WrapperData<T,Vector<T>,Vector<T>,True,True>(f,dim)) {}

template <class T>
FunctionWrapper<T>::
FunctionWrapper(const FunctionWrapper<T> &other) :
  WrapperParam<T>(other), doit_p(other.doit_p) {} /// check if to clone

template <class T>
FunctionWrapper<T> &FunctionWrapper<T>::
operator=(const FunctionWrapper<T> &other) {
  if (this != &other) {
    WrapperParam<T>::operator=(other);
    doit_p = other.doit_p; /// check clone
  }
  return *this;
}

//# Operators    
template <class T>
T FunctionWrapper<T>::eval(typename Function<T>::FunctionArg x) const {
  if (doit_p) return doit_p->eval(x, param_p.getParameters());
  return T(0);
}

//# Member functions
template <class T>
uInt FunctionWrapper<T>::ndim() const {
  return (doit_p ? doit_p->ndim() : 0);
}

} //# NAMESPACE CASACORE - END


#endif
