//# UnitVal.cc: defines the class describing a unit as a value and a dimension
//# Copyright (C) 1994-2001,2008
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

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/UnitName.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Utilities/Regex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

int UnitVal_static_initializer::initialized = 0;

//# Constants
UnitVal UnitVal::NODIM(		1.);
UnitVal UnitVal::UNDIM(		1., UnitDim::Dnon);
UnitVal UnitVal::LENGTH(	1., UnitDim::Dm);
UnitVal UnitVal::MASS(		1., UnitDim::Dkg);
UnitVal UnitVal::TIME(		1., UnitDim::Ds);
UnitVal UnitVal::CURRENT(	1., UnitDim::DA);
UnitVal UnitVal::TEMPERATURE(	1., UnitDim::DK);
UnitVal UnitVal::INTENSITY(	1., UnitDim::Dcd);
UnitVal UnitVal::MOLAR(		1., UnitDim::Dmol);
UnitVal UnitVal::ANGLE(		1., UnitDim::Drad);
UnitVal UnitVal::SOLIDANGLE(	1., UnitDim::Dsr);

void UnitVal::init(Double factor) {
  kindFactor = factor;
  kindDim.init();
}

void UnitVal::init(Double factor, Int pos) {
  kindFactor = factor;
  kindDim.init(pos);
}

UnitVal::UnitVal() :
  kindFactor(1.0),
  kindDim() {}

UnitVal::UnitVal(const UnitVal &other) :
  kindFactor(other.kindFactor),
  kindDim(other.kindDim) {}

UnitVal::UnitVal(Double factor, const String& s) :
  kindFactor(1.),
  kindDim() {
  if (UnitMap::getCache(s,*this)) {
    kindFactor *= factor;
  } else if (UnitVal::create(s,*this)) {
    UnitMap::putCache(s,*this);
    kindFactor *= factor;
  } else {
    throw (AipsError("UnitVal::UnitVal Illegal unit string '" +
		     s + "'"));
  }
}

UnitVal::~UnitVal() {}

UnitVal &UnitVal::operator=(const UnitVal &other) {
  if (this != &other) {
    kindFactor = other.kindFactor;
    kindDim = other.kindDim;
  }
  return *this;
}

UnitVal &UnitVal::operator*=(const UnitVal &other) {
  kindFactor *= other.kindFactor;
  kindDim   *= other.kindDim;
  return *this;
}

UnitVal operator*(const UnitVal &in, const UnitVal &other) {
  UnitVal result = in;
  result *= other;
  return result;
}

UnitVal &UnitVal::operator/=(const UnitVal &other) {
  kindFactor /= other.kindFactor;
  kindDim   /= other.kindDim;
  return *this;
}

UnitVal operator/(const UnitVal &in, const UnitVal &other) {
  UnitVal result = in;
  result /= other;
  return result;
}

Bool UnitVal::operator==(const UnitVal &other) const {
  return kindDim == other.kindDim;
}

Bool UnitVal::operator!=(const UnitVal &other) const {
  return kindDim != other.kindDim;
}

ostream& operator<< (ostream &os, const UnitVal &ku) {
  os << ku.kindFactor << ku.kindDim;
  return os;
}

UnitVal UnitVal::pow(Int p) {
  UnitVal loc;
  loc.kindFactor = ::pow(kindFactor,Double(p));
  loc.kindDim = kindDim.pow(p);
  return(loc);
}

UnitVal UnitVal::root(Int p) const {
  if (p==0) throw (AipsError("UnitVal::UnitVal Illegal root zero taken"));
  UnitVal loc;
  loc.kindDim = kindDim;
  for (Int i=0; i<UnitDim::Dnumber; i++) {
    if (kindDim.unitDim[i] % p == 0) loc.kindDim.unitDim[i] /= p;
    else throw (AipsError("UnitVal::UnitVal Illegal unit dimensions for root"));
  }
  loc.kindFactor = ::pow(kindFactor, 1.0/Double(p));
  return(loc);
}

UnitVal UnitVal::sqrt() const {
  return root(2);
}

const UnitDim &UnitVal::getDim() const {
  return kindDim;
}

Double UnitVal::getFac() const {
  return kindFactor;
}

Bool UnitVal::check(const String &s) {
  UnitVal loc;
  if (UnitMap::getCache(s,loc)) ;
  else if (UnitVal::create(s,loc)) UnitMap::putCache(s,loc);
  else return False;
  return True;
}

Bool UnitVal::check(const String &s, UnitVal &loc) {
  if (UnitMap::getCache(s,loc)) {
  } else if (UnitVal::create(s,loc)) {
    UnitMap::putCache(s,loc);
  } else {
    return False;
  }
  return True;
}

Bool UnitVal::create(const String &s, UnitVal &res) {
  MUString str(s);			// non-const copy
  return create(str, res);
}

Bool UnitVal::create(MUString &str, UnitVal &res) {
  UnitVal kind;
  Int ptr = str.getPtr();
  if (str.eos()) return True;
  Int ps = UnitVal::psign(str);	 	// power sign
  if (str.eos()) return True;
  if (str.testChar('(')) {
    if (!str.matchPair(')')) return False;
    if (!UnitVal::create(str.lastGet(), kind)) return False;
  } else {
    if (!UnitVal::field(str, kind)) return False;
  }
  ps *= UnitVal::power(str);			// full power
  if (str.getPtr() == ptr) return False;	// must have been error
  res *= kind.pow(ps);
  return UnitVal::create(str,res) ;		// add next part
}

Int UnitVal::psign(MUString& str) {
  static Regex sep("[ \\*\\./]");
  Int lc = 1;
  while (str.testChar(sep)) {
    if (str.testChar('/')) lc = -lc;
    str.skipChar();
  }
  return lc;
}

Int UnitVal::power(MUString &str) {
  if (str.testString("**")) str.skipString("**");
  if (str.testChar('^')) str.skipChar('^');
  Int lc = (Int) str.getSign();
  Int lp = str.getuInt();
  return (lp == 0 ? lc : lc * lp);
}

Bool UnitVal::field(MUString &str, UnitVal &res) {
  static Regex un1("[a-zA-Z_\"'$:%]");
  static Regex un2("[a-zA-Z_0\"'$:%]");
  UnitName loc;
  uInt wh(str.getPtr());
  res = UnitVal(); 		// Initial 1 value
  if (str.testChar(un1)) {
    Char prev = str.getChar();
    while (str.testChar(un2) || (str.testNum() && prev == '_'))
      prev = str.getChar();
  }
  String key = str.get(wh, str.getPtr());
  if (key.length() == 0) { res = loc.getVal(); return True;}
  if (UnitMap::getCache(key,res)) return True;
  if (UnitMap::getUnit(key,loc)) { res = loc.getVal(); return True;}
  if (key.length() > 1 && UnitMap::getPref(key(0,1), loc)) {
    UnitName loc1 = UnitName();
    if (UnitMap::getUnit(key.from(1), loc1)) {
      res = (loc.getVal() * loc1.getVal()); return True;
    } else if ( key.length() > 2 && UnitMap::getPref(key(0,2),loc)) {
      if (UnitMap::getUnit(key.from(2), loc1)) {
	res = (loc.getVal() * loc1.getVal()); return True;
      }
    }
  }
  return False;
}

} //# NAMESPACE CASACORE - END

