//# UnitMap.cc: defines the UnitMap class containing standard unit definitions
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002
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

#include <aips/Quanta/UnitMap.h>
#include <aips/Quanta/MUString.h>
#include <aips/Utilities/Regex.h>
#include <aips/iostream.h>

void UnitMap::initUM() {
  static Bool needInit = True;
  if (!needInit) return;
  needInit = False;

  // Initialise lists
  // default undimensioned unit name value
  UnitName defbu;
  // default undimensioned unit value value
  UnitVal defbv;
  // Initialise lists
  UnitMap::mapPref = 
    new SimpleOrderedMap<String, UnitName> (defbu,20);
  UnitMap::mapDef =
    new SimpleOrderedMap<String, UnitName> (defbu,9);
  UnitMap::mapSI =
    new SimpleOrderedMap<String, UnitName> (defbu,50);
  UnitMap::mapCust =
    new SimpleOrderedMap<String, UnitName> (defbu,80);
  UnitMap::mapUser =
    new SimpleOrderedMap<String, UnitName> (defbu,10);
  UnitMap::mapCache =
    new SimpleOrderedMap<String, UnitVal> (defbv,100);
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

UnitMap::~UnitMap() {}

Bool UnitMap::getCache(const String& s, UnitVal &val) {
  UnitMap::initUM();
  UnitVal *loc = mapCache->isDefined(s);
  if (loc) { 
    val = *loc; return (True);
  } else {
    val = UnitVal();
    return (False);
  };
}

Bool UnitMap::getPref(const String& s, UnitName &name) {
  UnitMap::initUM();
  UnitName *loc;
  if ((loc = mapPref->isDefined(s))) {}
  else {
    name = UnitName();
    return (False);
  };
  name = *loc; return (True);
}

Bool UnitMap::getUnit(const String& s, UnitName &name) {
  UnitMap::initUM();
  UnitName *loc;
  if ((loc = mapUser->isDefined(s))) {}
  else if ((loc = mapCust->isDefined(s))) {}
  else if ((loc = mapSI->isDefined(s))) {}
  else {
    name = UnitName();
    return (False);
  };
  name = *loc; return (True);
}

void UnitMap::putCache(const String& s, const UnitVal& val) {
  UnitMap::initUM();
  if (! s.empty()) {
    if (mapCache->ndefined() > 200) clearCache();
    mapCache->define(s,val);
  };
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
  if (mapUser->isDefined(name.getName()) ||
      mapCust->isDefined(name.getName()) ||
      mapSI->isDefined(name.getName())) clearCache();
  mapUser->define(name.getName(),name);
}

void UnitMap::removeUser(const String& s) {
  UnitMap::initUM();
  if (mapUser->isDefined(s)) {
    mapUser->remove(s);
    clearCache();
  };
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
  };
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
    };
    UnitMap::doneFITS = True;
  };
}

void UnitMap::clearFITS() {
  UnitMap::initUM();
  if (UnitMap::doneFITS) {
    uInt cnt = 0;
    UnitName *Fname;
    while (UnitMap::getNameFITS(Fname, cnt)) {
      UnitMap::removeUser(*Fname);
      cnt++;
    };
    UnitMap::doneFITS = False;
  };
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
	};
      };
      y += z;
    };
  };
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
	};
      };
      y += z;
    };
  };
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
  uInt i;
  for (i=0; i < mapPref->ndefined(); i++) {
    os << "    " << mapPref->getVal(i) << endl;
  };
}

void UnitMap::listDef() {
  listDef(cout);
}

void UnitMap::listDef(ostream &os) {
  UnitMap::initUM();
  uInt i;
  for (i=0; i < mapDef->ndefined(); i++) {
    os << "    " << mapDef->getVal(i) << endl;
  };
}

void UnitMap::listSI() {
  listSI(cout);
}

void UnitMap::listSI(ostream &os) {
  UnitMap::initUM();
  uInt i;
  for (i=0; i < mapSI->ndefined(); i++) {
    os << "    " << mapSI->getVal(i) << endl;
  };
}

void UnitMap::listCust() {
  listCust(cout);
}

void UnitMap::listCust(ostream &os) {
  UnitMap::initUM();
  uInt i;
  for (i=0; i < mapCust->ndefined(); i++) {
    os << "    " << mapCust->getVal(i) << endl;
  };
}

void UnitMap::listUser() {
  listUser(cout);
}

void UnitMap::listUser(ostream &os) {
  UnitMap::initUM();
  uInt i;
  for (i=0; i < mapUser->ndefined(); i++) {
    os << "    " << mapUser->getVal(i) << endl;
  };
}

void UnitMap::list() {
  list(cout);
}

void UnitMap::list(ostream &os) {
  UnitMap::initUM();
  os  << "Prefix table (" << mapPref->ndefined() << "):" << endl;
  listPref(os);
  os  << "Defining unit table (" << mapDef->ndefined() << "):" << endl;
  listDef(os);
  os << "SI unit table (" << mapSI->ndefined() << "):" << endl;
  listSI(os);
  os  << "Customary unit table (" << mapCust->ndefined() << "):" << endl;
  listCust(os);
  os  << "User unit table (" << mapUser->ndefined() << "):" << endl;
  listUser(os);
}

void UnitMap::listCache() {
  listCache(cout);
}

void UnitMap::listCache(ostream &os) {
  UnitMap::initUM();
  uInt i;
  os  << "Cached unit table (" << mapCache->ndefined() << "):" << endl;
  for (i=0; i < mapCache->ndefined(); i++) {
    os << "    " << 
      UnitName(mapCache->getKey(i),mapCache->getVal(i)) << endl;
  };
}

const SimpleOrderedMap<String, UnitName> &UnitMap::givePref() {
  UnitMap::initUM();
  return *mapPref;
}

const SimpleOrderedMap<String, UnitName> &UnitMap::giveDef() {
  UnitMap::initUM();
  return *mapDef;
}
const SimpleOrderedMap<String, UnitName> &UnitMap::giveSI() {
  UnitMap::initUM();
  return *mapSI;
}

const SimpleOrderedMap<String, UnitName> &UnitMap::giveCust() {
  UnitMap::initUM();
  return *mapCust;
}

const SimpleOrderedMap<String, UnitName> &UnitMap::giveUser() {
  UnitMap::initUM();
  return *mapUser;
}

const SimpleOrderedMap<String, UnitVal> &UnitMap::giveCache() {
  UnitMap::initUM();
  return *mapCache;
}

SimpleOrderedMap<String, UnitName> *UnitMap::mapPref;
SimpleOrderedMap<String, UnitName> *UnitMap::mapDef;
SimpleOrderedMap<String, UnitName> *UnitMap::mapSI;
SimpleOrderedMap<String, UnitName> *UnitMap::mapCust;
SimpleOrderedMap<String, UnitName> *UnitMap::mapUser;
SimpleOrderedMap<String, UnitVal> *UnitMap::mapCache;
Bool UnitMap::doneFITS;
