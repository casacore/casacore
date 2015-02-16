//# AipsrcValue.cc: Class to read values from the Aipsrc general resource files 
//# Copyright (C) 1995,1996,1997,1998,1999,2001,2002,2003
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

#ifndef CASA_AIPSRCVALUE_TCC
#define CASA_AIPSRCVALUE_TCC

//# Includes

#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Data
template <class T>
AipsrcValue<T> AipsrcValue<T>::myp_p;
template <class T>
Mutex AipsrcValue<T>::theirMutex;

//# Constructor
template <class T>
AipsrcValue<T>::AipsrcValue() : 
  tlst(0), ntlst(0) {}

//# Destructor
template <class T>
AipsrcValue<T>::~AipsrcValue() {}

template <class T>
Bool AipsrcValue<T>::find(T &value, const String &keyword) {
  String res;
  Bool x = Aipsrc::find(res, keyword, 0);
  if (x) {
    istringstream instr(res);
    instr >> value;
  }
  return x;
}

template <class T>
Bool AipsrcValue<T>::find(T &value, const String &keyword, 
			  const T &deflt) {
  return (find(value, keyword) ? True : (value = deflt, False));
}

template <class T>
Bool AipsrcValue<T>::find(T &value,
			  const String &keyword,
			  const Unit &defun, const Unit &resun) {
  String res;
  Bool x = Aipsrc::find(res, keyword, 0);
  if (x) {
    Quantum<Double> qres;
    istringstream instr(res);
    instr >> qres;
    if (qres.check(UnitVal::NODIM)) qres.setUnit(defun);
    value = (T) qres.getValue(resun);
  }
  return x;
}

template <class T>
Bool AipsrcValue<T>::find(T &value, const String &keyword, 
			  const Unit &defun, const Unit &resun,
			  const T &deflt) {
  return (find(value, keyword, defun, resun) ?
	  True : (value = deflt, False));
}

template <class T>
uInt AipsrcValue<T>::registerRC(const String &keyword,
				const T &deflt) {
  ScopedMutexLock lock(theirMutex);
  uInt n = Aipsrc::registerRC(keyword, myp_p.ntlst);
  myp_p.tlst.resize(n);
  find ((myp_p.tlst)[n-1], keyword, deflt);
  return n;
}

template <class T>
uInt AipsrcValue<T>::registerRC(const String &keyword,
				const Unit &defun, const Unit &resun,
				const T &deflt) {
  ScopedMutexLock lock(theirMutex);
  uInt n = Aipsrc::registerRC(keyword, myp_p.ntlst);
  myp_p.tlst.resize(n);
  find ((myp_p.tlst)[n-1], keyword, defun, resun, deflt);
  return n;
}

template <class T>
const T &AipsrcValue<T>::get(uInt keyword) {
  ScopedMutexLock lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  return (myp_p.tlst)[keyword-1];
}

template <class T>
void AipsrcValue<T>::set(uInt keyword, const T &deflt) {
  ScopedMutexLock lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  (myp_p.tlst)[keyword-1] = deflt;
}

template <class T>
void AipsrcValue<T>::save(uInt keyword) {
  ostringstream oss;
  {
    ScopedMutexLock lock(theirMutex);
    AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
    oss << (myp_p.tlst)[keyword-1];
  }
  // Unlock has to be done before save, because MVTime uses AipsrcValue.
  Aipsrc::save((myp_p.ntlst)[keyword-1], String(oss));
}

} //# NAMESPACE CASACORE - END


#endif
