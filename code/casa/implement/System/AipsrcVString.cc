//# AipsrcVString.cc: Specialisation for AipsrcVector<String>
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

// This specialisation is necessary to make sure a list of Strings is read
// appropiately, and no Units are handled as in the standard Vector case.

//# Constructor
AipsrcVector<String>::AipsrcVector() : 
  tlst(0), ntlst(0) {}

//# Destructor
AipsrcVector<String>::~AipsrcVector() {}

Bool AipsrcVector<String>::find(Vector<String> &value,
				const String &keyword) {
  String res;
  Bool x = Aipsrc::find(res, keyword, 0);
  if (x) {
    const Regex ws("[ 	]+");
    res.gsub(ws, " ");
    Int m = res.freq(" ") +1;
    String *nres = new String[m];
    m = split(res, nres, m, " ");
    value = Vector<String>(m);
    for (Int i=0; i<m; i++) {
      value(i) = nres[i];
    };
    delete [] nres;
  };
  return x;
}

Bool AipsrcVector<String>::find(Vector<String> &value, const String &keyword, 
				const Vector<String> &deflt) {
  return (find(value, keyword) ? True : (value = deflt, False));
}


// The following construction necessary since the gnu compiler does not (yet)
// support static templated data.
// egcs 1.1.b requires it to be in front of its use.
AipsrcVector<String> &AipsrcVector<String>::init() {
  static AipsrcVector<String> myp;
  return myp;
}


uInt AipsrcVector<String>::registerRC(const String &keyword,
				      const Vector<String> &deflt) {
  AipsrcVector<String> &gcl = init();
  uInt n = Aipsrc::registerRC(keyword, gcl.ntlst);
  gcl.tlst.resize(n);
  find ((gcl.tlst)[n-1], keyword, deflt);
  return n;
}

const Vector<String> &AipsrcVector<String>::get(uInt keyword) {
  AipsrcVector<String> &gcl = init();
  AlwaysAssert(keyword > 0 && keyword <= gcl.tlst.nelements(), AipsError);
  return (gcl.tlst)[keyword-1];
}

void AipsrcVector<String>::set(uInt keyword, const Vector<String> &deflt) {
  AipsrcVector<String> &gcl = init();
  AlwaysAssert(keyword > 0 && keyword <= gcl.tlst.nelements(), AipsError);
  (gcl.tlst)[keyword-1].resize(deflt.nelements());
  (gcl.tlst)[keyword-1] = deflt;
}

void AipsrcVector<String>::save(uInt keyword) {
  AipsrcVector<String> &gcl = init();
  AlwaysAssert(keyword > 0 && keyword <= gcl.tlst.nelements(), AipsError);
  ostrstream oss;
  Int n = ((gcl.tlst)[keyword-1]).nelements();
  for (Int i=0; i<n; i++) oss << " " << ((gcl.tlst)[keyword-1])(i);
  Aipsrc::save((gcl.ntlst)[keyword-1], String(oss));
}
