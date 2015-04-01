//# CompiledParam.cc:  Parameters for a compiled string Functions
//# Copyright (C) 2002
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

#ifndef SCIMATH_COMPILEDPARAM_TCC
#define SCIMATH_COMPILEDPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/CompiledParam.h>
#include <casacore/scimath/Functionals/FuncExpression.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
CompiledParam<T>::CompiledParam() : 
  Function<T>(), ndim_p(0), msg_p(), text_p(),
  functionPtr_p(0) {}

template <class T>
CompiledParam<T>::CompiledParam(const CompiledParam<T> &other) :
  Function<T>(other), ndim_p(other.ndim_p), msg_p(other.msg_p),
  text_p(other.text_p),
  functionPtr_p(new FuncExpression(*other.functionPtr_p)) {} 

template <class T>
CompiledParam<T>::~CompiledParam() {
  if(functionPtr_p)
     delete functionPtr_p;
  functionPtr_p = 0;
}

template <class T>
CompiledParam<T>& CompiledParam<T>::operator=(const CompiledParam<T> &other) {
  if (this != &other) {
    if(functionPtr_p)
       delete functionPtr_p;
    functionPtr_p = 0;
    ndim_p = other.ndim_p;
    msg_p = other.msg_p;
    text_p = other.text_p;
    functionPtr_p =  new FuncExpression(*other.functionPtr_p);
  }
  return *this;
}

template <class T>
Bool CompiledParam<T>::setFunction(const String &newFunction) {
  // Add the function
  if(functionPtr_p)
     delete functionPtr_p;
  functionPtr_p=0;
  functionPtr_p = new FuncExpression();
  ndim_p = 0;
  msg_p = "";
  text_p = "";
  if (!functionPtr_p->create(newFunction)) {
    this->param_p = FunctionParam<T>(0);
    msg_p = functionPtr_p->errorMessage();
    delete functionPtr_p; functionPtr_p=0;
    return False;
  }
  ndim_p = functionPtr_p->getNdim();
  this->param_p = FunctionParam<T>(functionPtr_p->getNpar());
  text_p = newFunction;
  return True;
}

} //# NAMESPACE CASACORE - END


#endif
