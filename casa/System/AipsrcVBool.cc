//# AipsrcVBool.cc: Specialisation for AipsrcVector<bool>
//# Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003
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

//# Includes

#include <casacore/casa/System/AipsrcVector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Data
AipsrcVector<bool> AipsrcVector<bool>::myp_p;
std::mutex AipsrcVector<bool>::theirMutex;

//# Constructor
AipsrcVector<bool>::AipsrcVector() : 
  tlst(0), ntlst(0) {}

//# Destructor
AipsrcVector<bool>::~AipsrcVector() {}

bool AipsrcVector<bool>::find(Vector<bool> &value,
			      const String &keyword) {
  String res;
  bool x = Aipsrc::find(res, keyword, 0);
  if (x) {
    const Regex ws("[ 	]+");
    const Regex tTrue("^([tT]|[yY]|[1-9])");
    res.gsub(ws, " ");
    int32_t m = res.freq(" ") +1;
    String *nres = new String[m];
    m = split(res, nres, m, " ");
    value = Vector<bool>(m);
    for (int32_t i=0; i<m; i++) {
      value(i) = ((nres[i]).contains(tTrue));;
    }
    delete [] nres;
  }
  return x;
}

bool AipsrcVector<bool>::find(Vector<bool> &value,
			      const String &keyword, 
			      const Vector<bool> &deflt) {
  return (find(value, keyword) ? true : (value = deflt, false));
}

uint32_t AipsrcVector<bool>::registerRC(const String &keyword,
				    const Vector<bool> &deflt) {
  std::lock_guard<std::mutex> lock(theirMutex);
  uint32_t n = Aipsrc::registerRC(keyword, myp_p.ntlst);
  myp_p.tlst.resize(n);
  find ((myp_p.tlst)[n-1], keyword, deflt);
  return n;
}

const Vector<bool> &AipsrcVector<bool>::get(uint32_t keyword) {
  std::lock_guard<std::mutex> lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  return (myp_p.tlst)[keyword-1];
}

void AipsrcVector<bool>::set(uint32_t keyword,
			     const Vector<bool> &deflt) {
  std::lock_guard<std::mutex> lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  (myp_p.tlst)[keyword-1].resize(deflt.nelements());
  (myp_p.tlst)[keyword-1] = deflt;
}

void AipsrcVector<bool>::save(uint32_t keyword) {
  std::lock_guard<std::mutex> lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  ostringstream oss;
  int32_t n = ((myp_p.tlst)[keyword-1]).nelements();
  for (int32_t i=0; i<n; i++) {
    if (((myp_p.tlst)[keyword-1])(i)) {
      oss << " true";
    } else {
      oss << " false";
    }
  }
  Aipsrc::save((myp_p.ntlst)[keyword-1], String(oss));
}

} //# NAMESPACE CASACORE - END

