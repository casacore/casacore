//# NQFunctionWrapper.cc:  Construct function objects from C++ functions 
//# Copyright (C) 1995,1996,2001
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

//# Includes
#include <aips/Functionals/NQFunctionWrapper.h>
#include <aips/Arrays/Vector.h>
#include <aips/Functionals/NQWrapperData.h>

//# Constructors
template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper() :
  NQWrapperParam<T>(0),
  doit_p(0) {}

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const T&), const Bool) : 
  NQWrapperParam<T>(0),
  doit_p(new NQWrapperData<T,T,T,False,True>(f)) {}

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const Vector<T>&),
					const Bool isPar) :
  NQWrapperParam<T>(0),
  doit_p(new NQWrapperData<T,T,Vector<T>,False,True>(f)) {}

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)()) :
  NQWrapperParam<T>(0),
  doit_p(new NQWrapperData<T,T,T,False,False>(f)) {}

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const T&)) :
  NQWrapperParam<T>(0),
  doit_p(new NQWrapperData<T,T,T,True,False>(f,1)) {}

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const T&, const T&),
					const T &par) :
  NQWrapperParam<T>(1),
  doit_p(new NQWrapperData<T,T,T,True,True>(f,1)) {
  param_p[0] = par;
}

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const T&, const Vector<T>&),
					const Vector<T> &par) :
  NQWrapperParam<T>(par),
  doit_p(new NQWrapperData<T,T,Vector<T>,True,True>(f,1)) {};

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const Vector<T>&),
					const Int dim) :
  NQWrapperParam<T>(0),
  doit_p(new NQWrapperData<T,Vector<T>,T,True,False>(f,dim)) {};

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const Vector<T>&, const T&),
					const T &par, const uInt dim) :
  NQWrapperParam<T>(1),
  doit_p(new NQWrapperData<T,Vector<T>,T,True,True>(f,dim)) {
  param_p[0] = par;
};

template <class T>
NQFunctionWrapper<T>::NQFunctionWrapper(T(*f)(const Vector<T>&,
					      const Vector<T>&),
					const Vector<T> &par,
					const uInt dim) :
  NQWrapperParam<T>(par),
  doit_p(new NQWrapperData<T,Vector<T>,Vector<T>,True,True>(f,dim)) {};

template <class T>
NQFunctionWrapper<T>::
NQFunctionWrapper(const NQFunctionWrapper<T> &other) :
  NQWrapperParam<T>(other), doit_p(other.doit_p) {} /// check if to clone

template <class T>
NQFunctionWrapper<T> &NQFunctionWrapper<T>::
operator=(const NQFunctionWrapper<T> &other) {
  if (this != &other) {
    NQWrapperParam<T>::operator=(other);
    doit_p = other.doit_p; /// check clone
  };
  return *this;
}

//# Operators    
template <class T>
T NQFunctionWrapper<T>::eval(typename Function<T>::FunctionArg x) const {
  if (doit_p) return doit_p->eval(x, param_p.getParameters());
  return T(0);
}

//# Member functions
template<class T>
Function<typename FunctionTraits<T>::DiffType>
*NQFunctionWrapper<T>::cloneAD() const {
  Function<typename FunctionTraits<T>::DiffType> *t =
    new NQFunctionWrapper<typename FunctionTraits<T>::DiffType>();
  for (uInt i=0; i<nparameters(); ++i) {
    (*t)[i] = typename FunctionTraits<T>::DiffType(param_p[i]);
  };
  return t;
}

template<class T>
Function<typename FunctionTraits<T>::BaseType>
*NQFunctionWrapper<T>::cloneBase() const {
  Function<typename FunctionTraits<T>::BaseType> *t =
    new NQFunctionWrapper<typename FunctionTraits<T>::BaseType>();
  for (uInt i=0; i<nparameters(); ++i) {
    (*t)[i] = typename FunctionTraits<T>::DiffType(param_p[i]).value();
  };
  return t;
}
