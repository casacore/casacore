//# UnitMap.cc: defines the UnitMap class containing standard unit definitions
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <aips/Measures/UnitMap.h>

//# constants

// IAU definition of light time (s) unit distance to calculate IAU units
static const Double IAU_tauA=499.0047837;	

void UnitMap::initUM() {
  static Bool needInit = True;
  if (!needInit) return;
  needInit = False;

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

  // Make sure constants exist
  static C_init c_init;

  // Known prefixes

  UnitMap::mapPref->define("Y", UnitName("Y", C::yotta, "yotta"));
  UnitMap::mapPref->define("Z", UnitName("Z", C::zetta, "zetta"));
  UnitMap::mapPref->define("E", UnitName("E", C::exa,   "exa"));
  UnitMap::mapPref->define("P", UnitName("P", C::peta,  "peta"));
  UnitMap::mapPref->define("T", UnitName("T", C::tera,  "tera"));
  UnitMap::mapPref->define("G", UnitName("G", C::giga,  "giga"));
  UnitMap::mapPref->define("M", UnitName("M", C::mega,  "mega"));
  UnitMap::mapPref->define("k", UnitName("k", C::kilo,  "kilo"));
  UnitMap::mapPref->define("h", UnitName("h", C::hecto, "hecto"));
  UnitMap::mapPref->define("da",UnitName("da",C::deka,  "deka"));
  UnitMap::mapPref->define("d", UnitName("d", C::deci,  "deci"));
  UnitMap::mapPref->define("c", UnitName("c", C::centi, "centi"));
  UnitMap::mapPref->define("m", UnitName("m", C::milli, "milli"));
  UnitMap::mapPref->define("u", UnitName("u", C::micro, "micro"));
  UnitMap::mapPref->define("n", UnitName("n", C::nano,  "nano"));
  UnitMap::mapPref->define("p", UnitName("p", C::pico,  "pico"));
  UnitMap::mapPref->define("f", UnitName("f", C::femto, "femto"));
  UnitMap::mapPref->define("a", UnitName("a", C::atto,  "atto"));
  UnitMap::mapPref->define("z", UnitName("z", C::zepto, "zepto"));
  UnitMap::mapPref->define("y", UnitName("y", C::yocto, "yocto"));
  
  // Defining SI units
  for (Int i=0; i<UnitDim::Dnumber; i++) {
    UnitMap::mapDef->define(UnitDim::dimName(i),
			    UnitName(UnitDim::dimName(i),
				     UnitVal(1.0,i),
				     UnitDim::dimFull(i)));
    
    // SI units
    if (i != UnitDim::Dkg) {
      UnitMap::mapSI->define(UnitDim::dimName(i),
			     UnitName(UnitDim::dimName(i),
				      UnitVal(1.0,i),
				      UnitDim::dimFull(i)));
    }
  }
  
  UnitMap::mapSI->define("_",	UnitName("_",
					 UnitVal(1.),
					 "dimensionless"));
  UnitMap::mapSI->define("$",	UnitName("$",
					 UnitVal(1.),
					 "currency"));
  UnitMap::mapSI->define("%",	UnitName("%",
					 UnitVal(0.01),
					 "percent"));
  UnitMap::mapSI->define("%%",	UnitName("%%",
					 UnitVal(0.001),
					 "permille"));
  UnitMap::mapSI->define("g",	UnitName("g",
					 UnitVal(0.001,UnitDim::Dkg),
					 "gram"));
  UnitMap::mapSI->define("Bq",  UnitName("Bq",
					 UnitVal(1.,"s-1"),
					 "becquerel"));
  UnitMap::mapSI->define("Hz",  UnitName("Hz",
					 UnitVal(1.,"s-1"),
					 "hertz"));
  UnitMap::mapSI->define("C",   UnitName("C",
					 UnitVal(1.,"A.s"),
					 "coulomb"));
  UnitMap::mapSI->define("lm",  UnitName("lm",
					 UnitVal(1.,"cd.sr"),
					 "lumen"));
  UnitMap::mapSI->define("N",	UnitName("N",
					 UnitVal(1.,"kg.m.s-2"),
					 "newton"));
  UnitMap::mapSI->define("J",   UnitName("J",
					 UnitVal(1.,"N.m"),
					 "joule"));
  UnitMap::mapSI->define("W",   UnitName("W",
					 UnitVal(1.,"J.s-1"),
					 "watt"));
  UnitMap::mapSI->define("V",   UnitName("V",
					 UnitVal(1.,"W.A-1"),
					 "volt"));
  UnitMap::mapSI->define("F",   UnitName("F",
					 UnitVal(1.,"C.V-1"),
					 "farad"));
  UnitMap::mapSI->define("Gy",  UnitName("Gy",
					 UnitVal(1.,"J.kg-1"),
					 "gray"));
  UnitMap::mapSI->define("lx",  UnitName("lx",
					 UnitVal(1.,"lm.m-2"),
					 "lux"));
  UnitMap::mapSI->define("Ohm", UnitName("Ohm",
					 UnitVal(1.,"V.A-1"),
					 "ohm"));
  UnitMap::mapSI->define("Pa",  UnitName("Pa",
					 UnitVal(1.,"N.m-2"),
					 "pascal"));
  UnitMap::mapSI->define("S",   UnitName("S",
					 UnitVal(1.,"Ohm-1"),
					 "siemens"));
  UnitMap::mapSI->define("Sv",  UnitName("Sv",
					 UnitVal(1.,"J.kg-1"),
					 "sievert"));
  UnitMap::mapSI->define("Wb",  UnitName("Wb",
					 UnitVal(1.,"V.s"),
					 "weber"));
  UnitMap::mapSI->define("H",   UnitName("H",
					 UnitVal(1.,"Wb.A-1"),
					 "henry"));
  UnitMap::mapSI->define("T",   UnitName("T",
					 UnitVal(1.,"Wb.m-2"),
					 "tesla"));
  
  // non-metric SI units
  UnitMap::mapSI->define("deg",   	UnitName("deg",
						 UnitVal(C::degree,"rad"),
						 "degree"));
  UnitMap::mapSI->define("arcmin",  	UnitName("arcmin",
						 UnitVal(C::arcmin,"rad"),
						 "arcmin"));
  UnitMap::mapSI->define("arcsec",  	UnitName("arcsec",
						 UnitVal(C::arcsec,"rad"),
						 "arcsec"));
  UnitMap::mapSI->define("as", 		UnitName("as",
						 UnitVal(1.,"arcsec"),
						 "arcsec"));
  UnitMap::mapSI->define("L",   	UnitName("L",
						 UnitVal(1.,"dm3"),
						 "litre"));
  UnitMap::mapSI->define("l",   	UnitName("l",
						 UnitVal(1.,"L"),
						 "litre"));
  UnitMap::mapSI->define("d",   	UnitName("d",
						 UnitVal(C::day,"s"),
						 "day"));
  UnitMap::mapSI->define("h",   	UnitName("h",
						 UnitVal(C::hour,"s"),
						 "hour"));
  UnitMap::mapSI->define("min",   	UnitName("min",
						 UnitVal(C::minute,"s"),
						 "minute"));
  UnitMap::mapSI->define("a",   	UnitName("a",
						 UnitVal(24.*3600.*365.25,"s"),
						 "year"));
  UnitMap::mapSI->define("t",   	UnitName("t",
						 UnitVal(C::tonne,"kg"),
						 "tonne"));
  
  // Astronomical SI units
  UnitMap::mapSI->define("Jy",   	UnitName("Jy",
						 UnitVal(C::Jansky,"W/m2/Hz"),
						 "jansky"));
  UnitMap::mapSI->define("AU",   	UnitName("AU",
						 UnitVal(C::c * IAU_tauA,"m"),
						 "astronomical unit"));
  UnitMap::mapSI->define("UA",   	UnitName("UA",
						 UnitVal(1.,"AU"),
						 "astronomical unit"));
  UnitMap::mapSI->define("AE",   	UnitName("AE",
						 UnitVal(1.,"AU"),
						 "astronomical unit"));
  UnitMap::mapSI->define("S0",   	UnitName("S0",
						 UnitVal(IAU_k*IAU_k/C::Gravity,
							 "AU3/d2/(m3/kg/s2)"),
						 "solar mass"));
  UnitMap::mapSI->define("M0",   	UnitName("M0",
						 UnitVal(1.,"S0"),
						 "solar mass"));
  UnitMap::mapSI->define("pc",   	UnitName("pc",
						 UnitVal(1./C::arcsec,"AU"),
						 "parsec"));
  UnitMap::mapSI->define("cy",   	UnitName("cy",
						 UnitVal(24.*3600.*36525,"s"),
						 "century"));
  
  // non-SI customary units
  UnitMap::mapCust->define("sq_deg", 	UnitName("sq_deg",
						 UnitVal(C::square_degree,"sr"),
						 "square degree"));
  UnitMap::mapCust->define("sq_arcmin", UnitName("sq_arcmin",
						 UnitVal(C::square_arcmin,"sr"),
						 "square arcmin"));
  UnitMap::mapCust->define("sq_arcsec", UnitName("sq_arcsec",
						 UnitVal(C::square_arcsec,"sr"),
						 "square arcsec"));
  UnitMap::mapCust->define("deg_2",  	UnitName("deg_2",
						 UnitVal(C::square_degree,"sr"),
						 "square degree"));
  UnitMap::mapCust->define("arcmin_2",  UnitName("arcmin_2",
						 UnitVal(C::square_arcmin,"sr"),
						 "square arcmin"));
  UnitMap::mapCust->define("arcsec_2",  UnitName("arcsec_2",
						 UnitVal(C::square_arcsec,"sr"),
						 "square arcsec"));
  UnitMap::mapCust->define("'",   	UnitName("'",
						 UnitVal(C::arcmin,"rad"),
						 "arcmin"));
  UnitMap::mapCust->define("''",   	UnitName("''",
						 UnitVal(C::arcsec,"rad"),
						 "arcsec"));
  UnitMap::mapCust->define("\"",   	UnitName("\"",
						 UnitVal(C::arcsec,"rad"),
						 "arcsec"));
  UnitMap::mapCust->define("'_2",  	UnitName("'_2",
						 UnitVal(C::square_arcmin,"sr"),
						 "square arcmin"));
  UnitMap::mapCust->define("''_2",  	UnitName("''_2",
						 UnitVal(C::square_arcsec,"sr"),
						 "square arcsec"));
  UnitMap::mapCust->define("\"_2",  	UnitName("\"_2",
						 UnitVal(C::square_arcsec,"sr"),
						 "square arcsec"));
  UnitMap::mapCust->define(":",   	UnitName(":",
						 UnitVal(1.,"h"),
						 "hour"));
  UnitMap::mapCust->define("::",   	UnitName("::",
						 UnitVal(1.,"min"),
						 "minute"));
  UnitMap::mapCust->define(":::",   	UnitName(":::",
						 UnitVal(1.,"s"),
						 "second"));
  UnitMap::mapCust->define("FU",   	UnitName("FU",
						 UnitVal(1.,"Jy"),
						 "flux unit"));
  UnitMap::mapCust->define("fu",   	UnitName("fu",
						 UnitVal(1.,"FU"),
						 "flux unit"));
  UnitMap::mapCust->define("WU",   	UnitName("WU",
						 UnitVal(5.,"mJy"),
						 "WSRT flux unit"));
  UnitMap::mapCust->define("abA",   	UnitName("abA",
						 UnitVal(C::abAmpere,"A"),
						 "abampere"));
  UnitMap::mapCust->define("abC",   	UnitName("abC",
						 UnitVal(C::abCoulomb,"C"),
						 "abcoulomb"));
  UnitMap::mapCust->define("abF",   	UnitName("abF",
						 UnitVal(C::abFarad,"F"),
						 "abfarad"));
  UnitMap::mapCust->define("abH",   	UnitName("abH",
						 UnitVal(C::abHenry,"H"),
						 "abhenry"));
  UnitMap::mapCust->define("abOhm", 	UnitName("abOhm",
						 UnitVal(C::abOhm,"Ohm"),
						 "abohm"));
  UnitMap::mapCust->define("abV",   	UnitName("abV",
						 UnitVal(C::abVolt,"V"),
						 "abvolt"));
  UnitMap::mapCust->define("statA", 	UnitName("statA",
						 UnitVal(C::statAmpere,"A"),
						 "statampere"));
  UnitMap::mapCust->define("statC", 	UnitName("statC",
						 UnitVal(C::statCoulomb,"C"),
						 "statcoulomb"));
  UnitMap::mapCust->define("statF", 	UnitName("statF",
						 UnitVal(C::statFarad,"F"),
						 "statfarad"));
  UnitMap::mapCust->define("statH", 	UnitName("statH",
						 UnitVal(C::statHenry,"H"),
						 "stathenry"));
  UnitMap::mapCust->define("statOhm", 	UnitName("statOhm",
						 UnitVal(C::statOhm,"Ohm"),
						 "statohm"));
  UnitMap::mapCust->define("statV",   	UnitName("statV",
						 UnitVal(C::statVolt,"V"),
						 "statvolt"));
  UnitMap::mapCust->define("ac",   	UnitName("ac",
						 UnitVal(C::acre,"m2"),
						 "acre"));
  UnitMap::mapCust->define("Ah",   	UnitName("Ah",
						 UnitVal(1.,"A.h"),
						 "ampere hour"));
  UnitMap::mapCust->define("Angstrom", 	UnitName("Angstrom",
						 UnitVal(C::Angstrom,"m"),
						 "angstrom"));
  UnitMap::mapCust->define("atm",   	UnitName("atm",
						 UnitVal(C::atmosphere,"Pa"),
						 "standard atmosphere"));
  UnitMap::mapCust->define("ata",   	UnitName("ata",
						 UnitVal(C::g,"N.cm-2"),
						 "technical atmosphere"));
  UnitMap::mapCust->define("u",   	UnitName("u",
						 UnitVal(1.661e-27,"kg"),
						 "atomic mass unit"));
  UnitMap::mapCust->define("bar",   	UnitName("bar",
						 UnitVal(C::bar,"Pa"),
						 "bar"));
  UnitMap::mapCust->define("Btu",   	UnitName("Btu",
						 UnitVal(1055.056,"J"),
						 "British thermal unit (Int)"));
  UnitMap::mapCust->define("cal",   	UnitName("cal",
						 UnitVal(4.1868,"J"),
						 "calorie (Int)"));
  UnitMap::mapCust->define("Cal",   	UnitName("Cal",
						 UnitVal(1.,"kcal"),
						 "large calorie (Int)"));
  UnitMap::mapCust->define("CM",   	UnitName("CM",
						 UnitVal(C::carat,"kg"),
						 "metric carat"));
  UnitMap::mapCust->define("mHg",   	UnitName("mHg",
						 UnitVal(C::mmHg,"kPa"),
						 "metre of mercury"));
  UnitMap::mapCust->define("dyn",   	UnitName("dyn",
						 UnitVal(C::dyne,"N"),
						 "dyne"));
  UnitMap::mapCust->define("eV",   	UnitName("eV",
						 UnitVal(C::qe,"J"),
						 "electron volt"));
  UnitMap::mapCust->define("erg",   	UnitName("erg",
						 UnitVal(1.,"dyn/cm"),
						 "erg"));
  UnitMap::mapCust->define("fl_oz", 	UnitName("fl_oz",
						 UnitVal(C::fluid_ounce,"m3"),
						 "fluid ounce (Imp)"));
  UnitMap::mapCust->define("USfl_oz", 	UnitName("USfl_oz",
						 UnitVal(C::USfluid_ounce,"m3"),
						 "fluid ounce (US)"));
  UnitMap::mapCust->define("ft",   	UnitName("ft",
						 UnitVal(C::foot,"m"),
						 "foot"));
  UnitMap::mapCust->define("fur",   	UnitName("fur",
						 UnitVal(C::furlong,"m"),
						 "furlong"));
  UnitMap::mapCust->define("Gal",   	UnitName("Gal",
						 UnitVal(1.,"cm/s2"),
						 "gal"));
  UnitMap::mapCust->define("gal",   	UnitName("gal",
						 UnitVal(C::gallon,"m3"),
						 "gallon (Imp)"));
  UnitMap::mapCust->define("USgal", 	UnitName("USgal",
						 UnitVal(C::USgallon,"m3"),
						 "gallon (US)"));
  UnitMap::mapCust->define("G",   	UnitName("G",
						 UnitVal(C::Gauss,"T"),
						 "gauss"));
  UnitMap::mapCust->define("Gb",   	UnitName("Gb",
						 UnitVal(C::Gilbert,"A"),
						 "gilbert"));
  UnitMap::mapCust->define("ha",   	UnitName("ha",
						 UnitVal(1.,"hm2"),
						 "hectare"));
  UnitMap::mapCust->define("hp",   	UnitName("hp",
						 UnitVal(C::horsepower,"W"),
						 "horsepower"));
  UnitMap::mapCust->define("cwt",   	UnitName("cwt",
						 UnitVal(C::hundredweight,"kg"),
						 "hundredweight"));
  UnitMap::mapCust->define("in",   	UnitName("in",
						 UnitVal(C::inch,"m"),
						 "inch"));
  UnitMap::mapCust->define("kn",   	UnitName("kn",
						 UnitVal(C::knot,"m/s"),
						 "knot (Imp)"));
  UnitMap::mapCust->define("ly",   	UnitName("ly",
						 UnitVal(C::light_year,"m"),
						 "light year"));
  UnitMap::mapCust->define("Mx",   	UnitName("Mx",
						 UnitVal(C::Maxwell,"Wb"),
						 "maxwell"));
  UnitMap::mapCust->define("mile",  	UnitName("mile",
						 UnitVal(C::mile,"m"),
						 "mile"));
  UnitMap::mapCust->define("n_mile", 	UnitName("n_mile",
						 UnitVal(C::nautical_mile,"m"),
						 "nautical mile (Imp)"));
  UnitMap::mapCust->define("Oe",   	UnitName("Oe",
						 UnitVal(C::Oersted,"A/m"),
						 "oersted"));
  UnitMap::mapCust->define("oz",   	UnitName("oz",
						 UnitVal(C::ounce,"kg"),
						 "ounce (avoirdupois)"));
  UnitMap::mapCust->define("lb",   	UnitName("lb",
						 UnitVal(C::pound,"kg"),
						 "pound (avoirdupois)"));
  UnitMap::mapCust->define("R",   	UnitName("R",
						 UnitVal(2.58e-4,"C/kg"),
						 "mile"));
  UnitMap::mapCust->define("sb",   	UnitName("sb",
						 UnitVal(1e4,"cd/m2"),
						 "stilb"));
  UnitMap::mapCust->define("St",   	UnitName("St",
						 UnitVal(1.,"cm2/s"),
						 "stokes"));
  UnitMap::mapCust->define("Torr",  	UnitName("Torr",
						 UnitVal(C::torr,"Pa"),
						 "torr"));
  UnitMap::mapCust->define("yd",   	UnitName("yd",
						 UnitVal(C::yard,"m"),
						 "yard"));
  UnitMap::mapCust->define("yr",   	UnitName("yr",
						 UnitVal(24.*3600.*365.25,"s"),
						 "year"));

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
  }
}

Bool UnitMap::getPref(const String& s, UnitName &name) {
  UnitMap::initUM();
  UnitName *loc;
  if (loc = mapPref->isDefined(s)) {}
  else {
    name = UnitName();
    return (False);
  }
  name = *loc; return (True);
}

Bool UnitMap::getUnit(const String& s, UnitName &name) {
  UnitMap::initUM();
  UnitName *loc;
  if (loc = mapUser->isDefined(s)) {}
  else if (loc = mapCust->isDefined(s)) {}
  else if (loc = mapSI->isDefined(s)) {}
  else {
    name = UnitName();
    return (False);
  }
  name = *loc; return (True);
}

void UnitMap::putCache(const String& s, const UnitVal& val) {
  UnitMap::initUM();
  if (! s.empty()) {
    if (mapCache->ndefined() > 200) {
      clearCache();
    };
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
  if (mapUser->isDefined(name.getName())) {
    clearCache();
  };
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

Bool UnitMap::getNameFITS(UnitName *&name, uInt which) {
  const uInt N_FITS = 16;
  static UnitName FITSunit[N_FITS] = {
    UnitName("BEAM",	UnitVal(1.0, "_"),	"dimensionless beam"),
    UnitName("DAYS",	UnitVal(1.0, "d"),	"day"),
    UnitName("DEGREES",	UnitVal(1.0, "deg"),	"degree"),
    UnitName("DEG",	UnitVal(1.0, "deg"),	"degree"),
    UnitName("HZ",	UnitVal(1.0, "Hz"),	"hertz"),
    UnitName("JY",	UnitVal(1.0, "Jy"),	"jansky"),
    UnitName("KELVINS",	UnitVal(1.0, "K"),	"kelvin"),
    UnitName("KELVIN",	UnitVal(1.0, "K"),	"kelvin"),
    UnitName("METERS",	UnitVal(1.0, "m"),	"meter"),
    UnitName("M",	UnitVal(1.0, "m"),	"meter"),
    UnitName("PASCAL",	UnitVal(1.0, "Pa"),	"pascal"),
    UnitName("SECONDS",	UnitVal(1.0, "s"),	"second"),
    UnitName("SEC",	UnitVal(1.0, "s"),	"second"),
    UnitName("VOLTS",	UnitVal(1.0, "V"),	"volt"),
    UnitName("YEARS",	UnitVal(1.0, "a"),	"year"),
    UnitName("YEAR",	UnitVal(1.0, "a"),	"year")
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
    };
    UnitMap::doneFITS = False;
  }
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
  Int i;
  for (i=0; i < mapPref->ndefined(); i++) {
    os << "    " << mapPref->getVal(i) << endl;
  }
}

void UnitMap::listDef() {
  listDef(cout);
}

void UnitMap::listDef(ostream &os) {
  UnitMap::initUM();
  Int i;
  for (i=0; i < mapDef->ndefined(); i++) {
    os << "    " << mapDef->getVal(i) << endl;
  }
}

void UnitMap::listSI() {
  listSI(cout);
}

void UnitMap::listSI(ostream &os) {
  UnitMap::initUM();
  Int i;
  for (i=0; i < mapSI->ndefined(); i++) {
    os << "    " << mapSI->getVal(i) << endl;
  }
}

void UnitMap::listCust() {
  listCust(cout);
}

void UnitMap::listCust(ostream &os) {
  UnitMap::initUM();
  Int i;
  for (i=0; i < mapCust->ndefined(); i++) {
    os << "    " << mapCust->getVal(i) << endl;
  }
}

void UnitMap::listUser() {
  listUser(cout);
}

void UnitMap::listUser(ostream &os) {
  UnitMap::initUM();
  Int i;
  for (i=0; i < mapUser->ndefined(); i++) {
    os << "    " << mapUser->getVal(i) << endl;
  }
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
  Int i;
  os  << "Cached unit table (" << mapCache->ndefined() << "):" << endl;
  for (i=0; i < mapCache->ndefined(); i++) {
    os << "    " << 
      UnitName(mapCache->getKey(i),mapCache->getVal(i)) << endl;
  }
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
