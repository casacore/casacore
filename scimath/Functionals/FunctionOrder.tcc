//# FunctionOrder.cc: Container of function description details
//# Copyright (C) 2002,2003
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

#ifndef SCIMATH_FUNCTIONORDER_TCC
#define SCIMATH_FUNCTIONORDER_TCC

#include <casacore/scimath/Functionals/FunctionOrder.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
FunctionOrder<T>::FunctionOrder()
  : int_p(0), double_p(0), string_p(""), function_p(0),
    scale_p(0), center_p(0), width_p(0) {}

template<class T>
FunctionOrder<T>::FunctionOrder(const FunctionOrder<T> &other)
  : int_p(other.int_p.copy()), double_p(other.double_p.copy()),
    string_p(other.string_p), function_p(other.function_p.nelements()),
    scale_p(other.scale_p.copy()), center_p(other.center_p.copy()),
    width_p(other.width_p.copy()) {
  for (uInt i=0; i<function_p.nelements(); ++i) {
    function_p[i] = (*(other.function_p[i])).clone();
  }
}

template<class T>
FunctionOrder<T>::~FunctionOrder() {
  for (uInt i=0; i<function_p.nelements(); ++i) {
    delete function_p[i]; function_p[i] = 0;
  }
}

//# Operators
template<class T>
FunctionOrder<T> &FunctionOrder<T>::operator=(const FunctionOrder<T> &other) {
  if (this != &other) {
    int_p.resize(other.int_p.nelements());
    int_p = other.int_p;
    double_p.resize(other.double_p.nelements());
    double_p = other.double_p;
    string_p = other.string_p;
    scale_p.resize(other.scale_p.nelements());
    scale_p = other.scale_p;
    center_p.resize(other.center_p.nelements());
    center_p = other.center_p;
    width_p.resize(other.width_p.nelements());
    width_p = other.width_p;
    for (uInt i=0; i<function_p.nelements(); ++i) {
      delete function_p[i]; function_p[i] = 0;
    }
    function_p =  PtrBlock<Function<T> *>(other.function_p.nelements());
    for (uInt i=0; i<function_p.nelements(); ++i) {
      function_p[i] = (*(other.function_p[i])).clone();
    }
  }
  return *this;
}

//# Member functions
template<class T>
Int &FunctionOrder<T>::getInt(const uInt n) {
  if (n>=int_p.nelements()) int_p.resize(n+1, True);
  return int_p[n]; 
}

template<class T>
const Int &FunctionOrder<T>::getInt(const uInt n) const {
  return int_p[n];
}
 
template<class T>
T &FunctionOrder<T>::getPar(const uInt n) {
  if (n>=double_p.nelements()) double_p.resize(n+1, True);
  return double_p[n];
}

template<class T>
const T &FunctionOrder<T>::getPar(const uInt n) const {
  return double_p[n];
}
 
template<class T>
String &FunctionOrder<T>::getString() {
  return string_p;
}

template<class T>
const String &FunctionOrder<T>::getString() const {
  return string_p;
}

template<class T>
T &FunctionOrder<T>::getScale(const uInt n) {
  if (n>=scale_p.nelements()) scale_p.resize(n+1, True);
  return scale_p[n];
} 

template<class T>
const T &FunctionOrder<T>::getScale(const uInt n) const {
  return scale_p[n]; 
}

template<class T>
T &FunctionOrder<T>::getCenter(const uInt n) {
  if (n>=center_p.nelements()) center_p.resize(n+1, True);
  return center_p[n]; 
}
 
template<class T>
const T &FunctionOrder<T>::getCenter(const uInt n) const {
  return center_p[n]; 
}
 
template<class T>
T &FunctionOrder<T>::getWidth(const uInt n) {
  if (n>=width_p.nelements()) width_p.resize(n+1, True);
  return width_p[n];
}
 
template<class T>
const T &FunctionOrder<T>::getWidth(const uInt n) const {
  return width_p[n];
}

template<class T>
const Function<T> &FunctionOrder<T>::getFunction(const uInt n) const {
  return *(function_p[n]);
}

template<class T>
void FunctionOrder<T>::setFunction(const uInt n, Function<T> &other) {
  if (n>=function_p.nelements()) function_p.resize(n+1, True);
  delete function_p[n]; function_p[n] = other.clone();
}

//# Global functions
template<class T>
ostream &FunctionOrder<T>::print(ostream &os) const {
  os << "[";
  os << "[";
  for (uInt i=0; i<int_p.nelements(); ++i) {
    if (i!=0) os << ", ";
    os << int_p[i];
  }
  os << "], ";
  os << "[";
  for (uInt i=0; i<double_p.nelements(); ++i) {
    if (i!=0) os << ", ";
    os << double_p[i];
  }
  os << "], ";
  os << "\"";
  os << string_p;
  os << "\", ";
  os << function_p.nelements();
  os << ", ";
  os << "[";
  for (uInt i=0; i<scale_p.nelements(); ++i) {
    if (i!=0) os << ", ";
    os << scale_p[i];
  }
  os << "], ";
  os << "[";
  for (uInt i=0; i<center_p.nelements(); ++i) {
    if (i!=0) os << ", ";
    os << center_p[i];
  }
  os << "], ";
  os << "[";
  for (uInt i=0; i<width_p.nelements(); ++i) {
    if (i!=0) os << ", ";
    os << width_p[i];
  }
  os << "]";
  os << "]";
  return os;
}


template<class T>
Bool FunctionOrder<T>::fromRecord(String &, const RecordInterface &in) {
  if (in.isDefined(String("ord"))) in.get(RecordFieldId("ord"), int_p);
  if (in.isDefined(String("par"))) in.get(RecordFieldId("par"), double_p);
  if (in.isDefined(String("str"))) in.get(RecordFieldId("str"), string_p);
  if (in.isDefined(String("sca"))) in.get(RecordFieldId("sca"), scale_p);
  if (in.isDefined(String("cen"))) in.get(RecordFieldId("cen"), center_p);
  if (in.isDefined(String("wid"))) in.get(RecordFieldId("wid"), width_p);
  return True;
}

template<class T>
Bool FunctionOrder<T>::fromString(String &, const String &in) {
  string_p = in;
  return True;
}

template<class T>
Bool FunctionOrder<T>::toRecord(String &, RecordInterface &out) const {
  out.define(RecordFieldId("ord"), int_p);
  out.define(RecordFieldId("par"), double_p);
  out.define(RecordFieldId("str"), string_p);
  out.define(RecordFieldId("sca"), scale_p);
  out.define(RecordFieldId("cen"), center_p);
  out.define(RecordFieldId("wid"), width_p);
  /// Add the functionals!!
  return True;
}

template<class T>
const String &FunctionOrder<T>::ident() const {
  static String myid = "fncord";
  return myid;
}

} //# NAMESPACE CASACORE - END


#endif
