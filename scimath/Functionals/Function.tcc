//# Function.cc: Numerical functional interface class
//# Copyright (C) 2001,2002,2003,2005
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

#ifndef SCIMATH_FUNCTION_TCC
#define SCIMATH_FUNCTION_TCC

//# Includes
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T, class U>
U Function<T,U>::operator()(const Vector<ArgType> &x) const {
  DebugAssert(ndim()<=x.nelements(), AipsError);
  if (x.contiguousStorage() || ndim()<2) return this->eval(&(x[0]));
  uInt j=ndim();
  arg_p.resize(j);
  for (uInt i=0; i<j; ++i) arg_p[i] = x[i];
  return this->eval(&(arg_p[0]));
}

template<class T, class U>
U Function<T,U>::operator()(const ArgType &x, const ArgType &y) const {
  DebugAssert(ndim()==2, AipsError);
  arg_p.resize(ndim());
  arg_p[0] = x; arg_p[1] = y;
  return this->eval(&(arg_p[0]));
}

template<class T, class U>
U Function<T,U>::operator()(const ArgType &x, const ArgType &y,
			    const ArgType &z) const {
  DebugAssert(ndim()==3, AipsError);
  arg_p.resize(ndim());
  arg_p[0] = x; arg_p[1] = y; arg_p[2] = z;
  return this->eval(&(arg_p[0]));
} 

template<class T, class U>
const String &Function<T,U>::name() const {
  static String x("unknown");
  return x;
}

template<class T, class U>
void Function<T,U>::setMode(const RecordInterface&) { }

template<class T, class U>
void Function<T,U>::getMode(RecordInterface&) const { }

template<class T, class U>
Bool Function<T,U>::hasMode() const { return False; }

template<class T, class U>
Function<typename FunctionTraits<T>::DiffType> *
Function<T,U>::cloneAD() const {
  throw(AipsError(String("Function `") + name() +
		  "' has no cloneAD() method"));
}

template<class T, class U>
Function<typename FunctionTraits<T>::BaseType> *
Function<T,U>::cloneNonAD() const {
  throw(AipsError(String("Function `") + name() +
		  "' has no cloneNonAD() method"));
}

} //# NAMESPACE CASACORE - END


#endif
