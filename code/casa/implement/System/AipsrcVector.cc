//# AipsrcVector.cc: Read multiple values from the  Aipsrc resource files
//# Copyright (C) 1995,1996,1997,1998
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

//# Includes

#include <aips/Tasking/AipsrcVector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Quanta/Quantum.h>
#include <strstream.h>

//# Constructor
template <class T>
AipsrcVector<T>::AipsrcVector() : 
  ntlst(0), tlst(0) {}

//# Destructor
template <class T>
AipsrcVector<T>::~AipsrcVector() {}

template <class T>
Bool AipsrcVector<T>::find(Vector<T> &value,
			  const String &keyword) {
  String res;
  Bool x = Aipsrc::find(res, keyword, 0);
  if (x) {
    const Regex ws("[ 	]+");
    res.gsub(ws, " ");
    Int m = res.freq(" ") +1;
    String *nres = new String[m];
    m = split(res, nres, m, " ");
    value = Vector<T>(m);
    for (uInt i=0; i<m; i++) {
      istrstream instr((nres[i]).chars());
      instr >> value(i);
    };
    delete [] nres;
  };
  return x;
}

template <class T>
Bool AipsrcVector<T>::find(Vector<T> &value, const String &keyword, 
			   const Vector<T> &deflt) {
  return (find(value, keyword) ? True : (value = deflt, False));
}

template <class T>
Bool AipsrcVector<T>::find(Vector<T> &value,
			   const String &keyword,
			   const Unit &defun, const Unit &resun) {
  String res;
  Bool x = Aipsrc::find(res, keyword, 0);
  if (x) {
    const Regex ws("[ 	]+");
    res.gsub(ws, " ");
    Int m = res.freq(" ") +1;
    String *nres = new String[m];
    m = split(res, nres, m, " ");
    value = Vector<T>(m);
    Quantum<Double> qres;
    for (uInt i=0; i<m; i++) {
      istrstream instr((nres[i]).chars());
      instr >> qres;
      if (qres.check(UnitVal::NODIM)) qres.setUnit(defun);
      value(i) = (T) qres.getValue(resun);
    };
    delete [] nres;
  };
  return x;
}

template <class T>
Bool AipsrcVector<T>::find(Vector<T> &value, const String &keyword, 
			   const Unit &defun, const Unit &resun,
			   const Vector<T> &deflt) {
  return (find(value, keyword) ? True : (value = deflt, False));
}

template <class T>
uInt AipsrcVector<T>::registerRC(const String &keyword,
				 const Vector<T> &deflt) {
  AipsrcVector<T> &gcl = init();
  uInt n = Aipsrc::registerRC(keyword, gcl.ntlst);
  gcl.tlst.resize(n);
  find ((gcl.tlst)[n-1], keyword, deflt);
  return n;
}

template <class T>
uInt AipsrcVector<T>::registerRC(const String &keyword,
				 const Unit &defun, const Unit &resun,
				 const Vector<T> &deflt) {
  AipsrcVector<T> &gcl = init();
  uInt n = Aipsrc::registerRC(keyword, gcl.ntlst);
  gcl.tlst.resize(n);
  find ((gcl.tlst)[n-1], keyword, defun, resun, deflt);
  return n;
}

template <class T>
const Vector<T> &AipsrcVector<T>::get(uInt keyword) {
  AipsrcVector<T> &gcl = init();
  AlwaysAssert(keyword > 0 && keyword <= gcl.tlst.nelements(), AipsError);
  return (gcl.tlst)[keyword-1];
}

template <class T>
void AipsrcVector<T>::set(uInt keyword, const Vector<T> &deflt) {
  AipsrcVector<T> &gcl = init();
  AlwaysAssert(keyword > 0 && keyword <= gcl.tlst.nelements(), AipsError);
  (gcl.tlst)[keyword-1].resize(deflt.nelements());
  (gcl.tlst)[keyword-1] = deflt;
}

template <class T>
void AipsrcVector<T>::save(uInt keyword) {
  AipsrcVector<T> &gcl = init();
  AlwaysAssert(keyword > 0 && keyword <= gcl.tlst.nelements(), AipsError);
  ostrstream oss;
  Int n = ((gcl.tlst)[keyword-1]).nelements();
  for (Int i=0; i<n; i++) oss << " " << ((gcl.tlst)[keyword-1])(i);
  Aipsrc::save((gcl.ntlst)[keyword-1], String(oss));
}

// The following construction necessary since the gnu compiler does not (yet)
// support static templated data.
template <class T>
AipsrcVector<T> &AipsrcVector<T>::init() {
  static AipsrcVector<T> myp;
  return myp;
}
