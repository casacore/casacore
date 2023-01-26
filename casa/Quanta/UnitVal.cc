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

void UnitVal::init(double factor) {
  kindFactor = factor;
  kindDim.init();
}

void UnitVal::init(double factor, int32_t pos) {
  kindFactor = factor;
  kindDim.init(pos);
}

UnitVal::UnitVal() :
  kindFactor(1.0),
  kindDim() {}

UnitVal::UnitVal(const UnitVal &other) :
  kindFactor(other.kindFactor),
  kindDim(other.kindDim) {}

UnitVal::UnitVal(double factor, const String& s, UMaps* maps) :
  kindFactor(1.),
  kindDim() {
  if (UnitMap::getCache(s,*this)) {
    kindFactor *= factor;
  } else if (UnitVal::create(s, *this, maps)) {
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

bool UnitVal::operator==(const UnitVal &other) const {
  return kindDim == other.kindDim;
}

bool UnitVal::operator!=(const UnitVal &other) const {
  return kindDim != other.kindDim;
}

ostream& operator<< (ostream &os, const UnitVal &ku) {
  os << ku.kindFactor << ku.kindDim;
  return os;
}

UnitVal UnitVal::pow(int32_t p) {
  UnitVal loc;
  loc.kindFactor = ::pow(kindFactor,double(p));
  loc.kindDim = kindDim.pow(p);
  return(loc);
}

UnitVal UnitVal::root(int32_t p) const {
  if (p==0) throw (AipsError("UnitVal::UnitVal Illegal root zero taken"));
  UnitVal loc;
  loc.kindDim = kindDim;
  for (int32_t i=0; i<UnitDim::Dnumber; i++) {
    if (kindDim.unitDim[i] % p == 0) loc.kindDim.unitDim[i] /= p;
    else throw (AipsError("UnitVal::UnitVal Illegal unit dimensions for root"));
  }
  loc.kindFactor = ::pow(kindFactor, 1.0/double(p));
  return(loc);
}

UnitVal UnitVal::sqrt() const {
  return root(2);
}

const UnitDim &UnitVal::getDim() const {
  return kindDim;
}

double UnitVal::getFac() const {
  return kindFactor;
}

bool UnitVal::check(const String &s) {
  UnitVal loc;
  if (UnitMap::getCache(s,loc)) ;
  else if (UnitVal::create(s,loc)) UnitMap::putCache(s,loc);
  else return false;
  return true;
}

bool UnitVal::check(const String &s, UnitVal &loc) {
  if (UnitMap::getCache(s,loc)) {
  } else if (UnitVal::create(s,loc)) {
    UnitMap::putCache(s,loc);
  } else {
    return false;
  }
  return true;
}

bool UnitVal::create(const String &s, UnitVal &res, UMaps* maps) {
  MUString str(s);			// non-const copy
  return create(str, res, maps);
}

bool UnitVal::create(MUString &str, UnitVal &res, UMaps* maps) {
  UnitVal kind;
  int32_t ptr = str.getPtr();
  if (str.eos()) return true;
  int32_t ps = UnitVal::psign(str);	 	// power sign
  if (str.eos()) return true;
  if (str.testChar('(')) {
    if (!str.matchPair(')')) return false;
    if (!UnitVal::create(str.lastGet(), kind, maps)) return false;
  } else {
    if (!UnitVal::field(str, kind, maps)) return false;
  }
  ps *= UnitVal::power(str);			// full power
  if (str.getPtr() == ptr) return false;	// must have been error
  res *= kind.pow(ps);
  return UnitVal::create(str, res, maps);       // add next part
}

int32_t UnitVal::psign(MUString& str) {
  static const Regex sep("[ \\*\\./]");
  int32_t lc = 1;
  while (str.testChar(sep)) {
    if (str.testChar('/')) lc = -lc;
    str.skipChar();
  }
  return lc;
}

int32_t UnitVal::power(MUString &str) {
  if (str.testString("**")) str.skipString("**");
  if (str.testChar('^')) str.skipChar('^');
  int32_t lc = (int32_t) str.getSign();
  int32_t lp = str.getuInt();
  return (lp == 0 ? lc : lc * lp);
}

bool UnitVal::field(MUString &str, UnitVal &res, UMaps* maps) {
  static const Regex un1("[a-zA-Z_\"'$:%]");
  static const Regex un2("[a-zA-Z_0\"'$:%]");
  UnitName loc;
  uint32_t wh(str.getPtr());
  res = UnitVal(); 		// Initial 1 value
  if (str.testChar(un1)) {
    char prev = str.getChar();
    while (str.testChar(un2) || (str.testNum() && prev == '_'))
      prev = str.getChar();
  }
  String key = str.get(wh, str.getPtr());
  if (key.length() == 0) { res = loc.getVal(); return true;}
  if (UnitMap::getCache(key,res)) return true;
  if (UnitMap::getUnit(key,loc,maps)) { res = loc.getVal(); return true;}
  if (key.length() > 1 && UnitMap::getPref(key(0,1), loc, maps)) {
    UnitName loc1 = UnitName();
    if (UnitMap::getUnit(key.from(1), loc1, maps)) {
      res = (loc.getVal() * loc1.getVal()); return true;
    } else if ( key.length() > 2 && UnitMap::getPref(key(0,2),loc)) {
      if (UnitMap::getUnit(key.from(2), loc1, maps)) {
	res = (loc.getVal() * loc1.getVal()); return true;
      }
    }
  }
  return false;
}

} //# NAMESPACE CASACORE - END

