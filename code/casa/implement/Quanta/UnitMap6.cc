//# UnitMap6.cc: Unit map custom units initialisation part 2
//# Copyright (C) 2001
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

// Initialise the maps
void UnitMap::initUMCust2() {
  UnitMap::mapCust->define("abA",   	UnitName("abA",
						 UnitVal(10.0,"A"),
						 "abampere"));
  UnitMap::mapCust->define("abC",   	UnitName("abC",
						 UnitVal(10.0,"C"),
						 "abcoulomb"));
  UnitMap::mapCust->define("abF",   	UnitName("abF",
						 UnitVal(1.0e+9,"F"),
						 "abfarad"));
  UnitMap::mapCust->define("abH",   	UnitName("abH",
						 UnitVal(1.0e-9,"H"),
						 "abhenry"));
  UnitMap::mapCust->define("abOhm", 	UnitName("abOhm",
						 UnitVal(1.0e-9,"Ohm"),
						 "abohm"));
  UnitMap::mapCust->define("abV",   	UnitName("abV",
						 UnitVal(1.0e-8,"V"),
						 "abvolt"));
  UnitMap::mapCust->define("statA", 	UnitName("statA",
						 UnitVal((0.1/C::c),"A"),
						 "statampere"));
  UnitMap::mapCust->define("statC", 	UnitName("statC",
						 UnitVal((0.1/C::c),"C"),
						 "statcoulomb"));
  UnitMap::mapCust->define("statF", 	UnitName("statF",
						 UnitVal(1.0/(3.0e+3*C::c),"F"),
						 "statfarad"));
  UnitMap::mapCust->define("statH", 	UnitName("statH",
						 UnitVal((3.0e+3*C::c),"H"),
						 "stathenry"));
  UnitMap::mapCust->define("statOhm", 	UnitName("statOhm",
						 UnitVal((3.0e+3*C::c),"Ohm"),
						 "statohm"));
  UnitMap::mapCust->define("statV",   	UnitName("statV",
						 UnitVal((C::c*1.0e-6),"V"),
						 "statvolt"));
  UnitMap::mapCust->define("ac",   	UnitName("ac",
						 UnitVal(4.0*40*16.5*12*2.54e-2*16.5*12*2.54e-2,"m2"),
						 "acre"));
  UnitMap::mapCust->define("Ah",   	UnitName("Ah",
						 UnitVal(1.,"A.h"),
						 "ampere hour"));
  UnitMap::mapCust->define("Angstrom", 	UnitName("Angstrom",
						 UnitVal(1.0e-10,"m"),
						 "angstrom"));
  UnitMap::mapCust->define("atm",   	UnitName("atm",
						 UnitVal(1.01325e+5,"Pa"),
						 "standard atmosphere"));
  UnitMap::mapCust->define("ata",   	UnitName("ata",
						 UnitVal(9.80665,"N.cm-2"),
						 "technical atmosphere"));
  UnitMap::mapCust->define("u",   	UnitName("u",
						 UnitVal(1.661e-27,"kg"),
						 "atomic mass unit"));
  UnitMap::mapCust->define("bar",   	UnitName("bar",
						 UnitVal(1.0e+5,"Pa"),
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
						 UnitVal((1e-3/5.0),"kg"),
						 "metric carat"));
  UnitMap::mapCust->define("mHg",   	UnitName("mHg",
						 UnitVal(13.5951*9.80665,"kPa"),
						 "metre of mercury"));
  UnitMap::mapCust->define("dyn",   	UnitName("dyn",
						 UnitVal(1.0e-5,"N"),
						 "dyne"));
  UnitMap::mapCust->define("eV",   	UnitName("eV",
						 UnitVal(1.60217733e-19,"J"),
						 "electron volt"));
  UnitMap::mapCust->define("erg",   	UnitName("erg",
						 UnitVal(1.,"dyn/cm"),
						 "erg"));
}
