//# UnitMap2.cc: Unit map prefix initialisation
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
void UnitMap::initUMPrefix() {
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
}
