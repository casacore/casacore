//# UnitMap.cc: defines the UnitMap class containing standard unit definitions
//# Copyright (C) 1994-2002,2007
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

#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

void UnitMap::initUM() {
  static Bool needInit = True;
  if (!needInit) return;
  needInit = False;

  // Initialise lists
  UnitMap::mapPref = 
    new map<String, UnitName>;
  UnitMap::mapDef =
    new map<String, UnitName>;
  UnitMap::mapSI =
    new map<String, UnitName>;
  UnitMap::mapCust =
    new map<String, UnitName>;
  UnitMap::mapUser =
    new map<String, UnitName>;
  UnitMap::mapCache =
    new map<String, UnitVal>;
  UnitMap::doneFITS = False;
  // Define the map
  // Known prefixes
  UnitMap::initUMPrefix();
  // Defining SI units
  UnitMap::initUMSI1();
  UnitMap::initUMSI2();
  // non-SI customary units
  UnitMap::initUMCust1();
  UnitMap::initUMCust2();
  UnitMap::initUMCust3();

  //# Start with clean cache
  UnitMap::mapCache->clear();
}

UnitMap::UnitMap() {}

UnitMap::~UnitMap() {
  releaseUM();
}

void UnitMap::releaseUM() {
  delete UnitMap::mapPref;	mapPref = 0;
  delete UnitMap::mapDef;	mapDef = 0;
  delete UnitMap::mapSI;	mapSI = 0;
  delete UnitMap::mapCust;	mapCust = 0;
  delete UnitMap::mapUser;	mapUser = 0;
  delete UnitMap::mapCache;	mapCache = 0;
}

Bool UnitMap::getCache(const String& s, UnitVal &val) {
  UnitMap::initUM();
  map<String, UnitVal>::iterator pos = mapCache->find(s);
  if (pos == mapCache->end()) {
    val = UnitVal();
    return False;
  }
  val = pos->second;
  return True;
}

Bool UnitMap::getPref(const String& s, UnitName &name) {
  UnitMap::initUM();
  map<String, UnitName>::iterator pos = mapPref->find(s);
  if (pos == mapPref->end()) {
    name = UnitName();
    return False;
  }
  name = pos->second;
  return True;
}

Bool UnitMap::getUnit(const String& s, UnitName &name) {
  UnitMap::initUM();
  map<String, UnitName>::iterator pos;
  if ((pos = mapUser->find(s)) != mapUser->end() ||
      (pos = mapCust->find(s)) != mapCust->end() ||
      (pos = mapSI->find(s)) != mapSI->end()) {
  } else {    
    name = UnitName();
    return False;
  }
  name = pos->second;
  return True;
}

void UnitMap::putCache(const String& s, const UnitVal& val) {
  UnitMap::initUM();
  if (! s.empty()) {
    if (mapCache->size() > 200) clearCache();
    mapCache->insert(map<String, UnitVal>::value_type(s,val));
  }
}

void UnitMap::putUser(const String& s, const UnitVal& val) {
  const String empty("");
  UnitMap::putUser(s, val, empty);
}

void UnitMap::putUser(const String& s, const UnitVal& val,
		      const String& name) {
  UnitName loc(s,val,name);
  UnitMap::putUser(loc);
}

void UnitMap::putUser(const UnitName& name) {
  UnitMap::initUM();
  map<String, UnitName>::iterator pos;
  if ((pos = mapUser->find(name.getName())) != mapUser->end() ||
      (pos = mapCust->find(name.getName())) != mapCust->end() ||
      (pos = mapSI->find(name.getName())) != mapSI->end()) clearCache();
  mapUser->insert(map<String, UnitName>::value_type(name.getName(), name));
}

void UnitMap::removeUser(const String& s) {
  UnitMap::initUM();
  map<String, UnitName>::iterator pos = mapUser->find(s);
  if (pos != mapUser->end()) {
    mapUser->erase(pos);
    clearCache();
  }
}

void UnitMap::removeUser(const UnitName& name) {
  UnitMap::removeUser(name.getName());
}

const String &UnitMap::getStringFITS(uInt which) {
  static String FITSstring[N_FITS] = {
    "beam",
    "d",
    "deg",
    "deg",
    "Hz",
    "Jy",
    "K",
    "K",
    "km",
    "m",
    "m",
    "Pa",
    "pixel",
    "s",
    "s",
    "s",
    "V",
    "a",
    "a"
  };
  return FITSstring[which];
}

Bool UnitMap::getNameFITS(UnitName *&name, uInt which) {
  static UnitName FITSunit[N_FITS] = {
    UnitName("BEAM",	UnitVal(1.0, getStringFITS(0)),	"dimensionless beam"),
    UnitName("DAYS", 	UnitVal(1.0, getStringFITS(1)),	"day"),
    UnitName("DEGREES", UnitVal(1.0, getStringFITS(2)),	"degree"),
    UnitName("DEG",	UnitVal(1.0, getStringFITS(3)),	"degree"),
    UnitName("HZ",	UnitVal(1.0, getStringFITS(4)),	"hertz"),
    UnitName("JY",	UnitVal(1.0, getStringFITS(5)),	"jansky"),
    UnitName("KELVINS",	UnitVal(1.0, getStringFITS(6)),	"kelvin"),
    UnitName("KELVIN",	UnitVal(1.0, getStringFITS(7)),	"kelvin"),
    UnitName("KM",	UnitVal(1.0, getStringFITS(8)),	"km"),
    UnitName("METERS",	UnitVal(1.0, getStringFITS(9)),	"meter"),
    UnitName("M",	UnitVal(1.0, getStringFITS(10)),"meter"),
    UnitName("PASCAL",	UnitVal(1.0, getStringFITS(11)),"pascal"),
    UnitName("PIXEL",   UnitVal(1.0, getStringFITS(12)),"dimensionless pixel"),
    UnitName("SECONDS",	UnitVal(1.0, getStringFITS(13)),"second"),
    UnitName("SEC",	UnitVal(1.0, getStringFITS(14)),"second"),
    UnitName("S",	UnitVal(1.0, getStringFITS(15)),"second"),
    UnitName("VOLTS",	UnitVal(1.0, getStringFITS(16)),"volt"),
    UnitName("YEARS",	UnitVal(1.0, getStringFITS(17)),"year"),
    UnitName("YEAR",	UnitVal(1.0, getStringFITS(18)),"year")
  };
  if (which >= N_FITS) {
    return False;
  }
  name = &FITSunit[which];
  return True;
}

void UnitMap::addFITS() {
  UnitMap::initUM();
  if (! UnitMap::doneFITS) {
    uInt cnt = 0;
    UnitName *Fname;
    while (UnitMap::getNameFITS(Fname, cnt)) {
      UnitMap::putUser(*Fname);
      cnt++;
    }
    UnitMap::doneFITS = True;
  }
}

void UnitMap::clearFITS() {
  UnitMap::initUM();
  if (UnitMap::doneFITS) {
    uInt cnt = 0;
    UnitName *Fname;
    while (UnitMap::getNameFITS(Fname, cnt)) {
      UnitMap::removeUser(*Fname);
      cnt++;
    }
    UnitMap::doneFITS = False;
  }
}

Unit UnitMap::fromFITS(const Unit &un) {
  static Regex sepa("[^a-zA-Z]");
  MUString mus(un.getName());
  String y;
  String z;
  UnitName *nam;
  while (!mus.eos()) {
    if (mus.testChar(sepa)) y += String(mus.getChar());
    else {
      z = mus.getAlpha();
      for (uInt i=0; i<N_FITS; i++) {
	getNameFITS(nam, i);
	if (z == nam->getName()) {
	  z =  getStringFITS(i);
	  break;
	}
      }
      y += z;
    }
  }
  return Unit(y);
}

Unit UnitMap::toFITS(const Unit &un) {
  static Regex sepa("[^a-zA-Z]");
  MUString mus(un.getName());
  String y;
  String z;
  UnitName *nam;
  while (!mus.eos()) {
    if (mus.testChar(sepa)) y += String(mus.getChar());
    else {
      z = mus.getAlpha();
      for (Int i=N_FITS-1; i>= 0; i--) {
	if (z == getStringFITS(i)) {
	  getNameFITS(nam, i);
	  z =  nam->getName();
	  break;
	}
      }
      y += z;
    }
  }
  return Unit(y);
}

void UnitMap::clearCache() {
  UnitMap::initUM();
  mapCache->clear();
}

void UnitMap::listPref() {
  listPref(cout);
}

void UnitMap::listPref(ostream &os) {
  UnitMap::initUM();
  for (map<String, UnitName>::iterator i=mapPref->begin();
       i != mapPref->end(); ++i) {
    os << "    " << i->second << endl;
  }
}

void UnitMap::listDef() {
  listDef(cout);
}

void UnitMap::listDef(ostream &os) {
  UnitMap::initUM();
  for (map<String, UnitName>::iterator i=mapDef->begin();
       i != mapDef->end(); ++i) {
    os << "    " << i->second << endl;
  }
}

void UnitMap::listSI() {
  listSI(cout);
}

void UnitMap::listSI(ostream &os) {
  UnitMap::initUM();
  for (map<String, UnitName>::iterator i=mapSI->begin();
       i != mapSI->end(); ++i) {
    os << "    " << i->second << endl;
  }
}

void UnitMap::listCust() {
  listCust(cout);
}

void UnitMap::listCust(ostream &os) {
  UnitMap::initUM();
  for (map<String, UnitName>::iterator i=mapCust->begin();
       i != mapCust->end(); ++i) {
    os << "    " << i->second << endl;
  }
}

void UnitMap::listUser() {
  listUser(cout);
}

void UnitMap::listUser(ostream &os) {
  UnitMap::initUM();
  for (map<String, UnitName>::iterator i=mapUser->begin();
       i != mapUser->end(); ++i) {
    os << "    " << i->second << endl;
  }
}

void UnitMap::list() {
  list(cout);
}

void UnitMap::list(ostream &os) {
  UnitMap::initUM();
  os  << "Prefix table (" << mapPref->size() << "):" << endl;
  listPref(os);
  os  << "Defining unit table (" << mapDef->size() << "):" << endl;
  listDef(os);
  os << "SI unit table (" << mapSI->size() << "):" << endl;
  listSI(os);
  os  << "Customary unit table (" << mapCust->size() << "):" << endl;
  listCust(os);
  os  << "User unit table (" << mapUser->size() << "):" << endl;
  listUser(os);
}

void UnitMap::listCache() {
  listCache(cout);
}

void UnitMap::listCache(ostream &os) {
  UnitMap::initUM();
  os  << "Cached unit table (" << mapCache->size() << "):" << endl;
  for (map<String, UnitVal>::iterator i=mapCache->begin();
       i != mapCache->end(); ++i) {
    os << "    " <<
      UnitName(i->first, i->second) << endl;
  }
}

const map<String, UnitName> &UnitMap::givePref() {
  UnitMap::initUM();
  return *mapPref;
}

const map<String, UnitName> &UnitMap::giveDef() {
  UnitMap::initUM();
  return *mapDef;
}
const map<String, UnitName> &UnitMap::giveSI() {
  UnitMap::initUM();
  return *mapSI;
}

const map<String, UnitName> &UnitMap::giveCust() {
  UnitMap::initUM();
  return *mapCust;
}

const map<String, UnitName> &UnitMap::giveUser() {
  UnitMap::initUM();
  return *mapUser;
}

const map<String, UnitVal> &UnitMap::giveCache() {
  UnitMap::initUM();
  return *mapCache;
}

map<String, UnitName> *UnitMap::mapPref;
map<String, UnitName> *UnitMap::mapDef;
map<String, UnitName> *UnitMap::mapSI;
map<String, UnitName> *UnitMap::mapCust;
map<String, UnitName> *UnitMap::mapUser;
map<String, UnitVal> *UnitMap::mapCache;
Bool UnitMap::doneFITS;

} //# NAMESPACE CASACORE - END

