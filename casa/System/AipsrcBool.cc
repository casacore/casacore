//# AipsrcBool.cc: Specialisation for AipsrcValue<Bool>
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
//#
//# $Id$

//# Includes

#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Data
AipsrcValue<Bool> AipsrcValue<Bool>::myp_p;
Mutex AipsrcValue<Bool>::theirMutex;

//# Constructor
AipsrcValue<Bool>::AipsrcValue() :
  tlst(0), ntlst(0) {}

//# Destructor
AipsrcValue<Bool>::~AipsrcValue() {}

Bool AipsrcValue<Bool>::find(Bool &value, const String &keyword) {
  String res;
  Bool x = Aipsrc::find(res, keyword, 0);
  if (x) value = (res.size() > 0  &&  (res[0]=='t' || res[0]=='T' || res[0]=='y' ||
				       res[0]=='Y' || (res[0]>='1' && res[0]<='9')));
  return x;
}

Bool AipsrcValue<Bool>::find(Bool &value, const String &keyword, 
			     const Bool &deflt) {
  return (find(value, keyword) ? True : (value = deflt, False));
}

uInt AipsrcValue<Bool>::registerRC(const String &keyword,
				   const Bool &deflt) {
  ScopedMutexLock lock(theirMutex);
  uInt n = Aipsrc::registerRC(keyword, myp_p.ntlst);
  myp_p.tlst.resize(n);
  find ((myp_p.tlst)[n-1], keyword, deflt);
  return n;
}

const Bool &AipsrcValue<Bool>::get(uInt keyword) {
  ScopedMutexLock lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  return (myp_p.tlst)[keyword-1];
}

void AipsrcValue<Bool>::set(uInt keyword, const Bool &deflt) {
  ScopedMutexLock lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  (myp_p.tlst)[keyword-1] = deflt;
}

void AipsrcValue<Bool>::save(uInt keyword) {
  ScopedMutexLock lock(theirMutex);
  AlwaysAssert(keyword > 0 && keyword <= myp_p.tlst.nelements(), AipsError);
  ostringstream oss;
  if ((myp_p.tlst)[keyword-1]) {
    oss << "true";
  } else {
    oss << "false";
  }
  Aipsrc::save((myp_p.ntlst)[keyword-1], String(oss));
}

} //# NAMESPACE CASACORE - END

