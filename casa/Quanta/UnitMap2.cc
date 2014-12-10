//# UnitMap2.cc: Unit map prefix initialisation
//# Copyright (C) 2001,2002
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

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Initialise the maps
void UnitMap::initUMPrefix() {
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("Y", UnitName("Y", C::yotta, "yotta")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("Z", UnitName("Z", C::zetta, "zetta")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("E", UnitName("E", C::exa,   "exa")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("P", UnitName("P", C::peta,  "peta")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("T", UnitName("T", C::tera,  "tera")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("G", UnitName("G", C::giga,  "giga")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("M", UnitName("M", C::mega,  "mega")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("k", UnitName("k", C::kilo,  "kilo")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("h", UnitName("h", C::hecto, "hecto")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("da",UnitName("da",C::deka,  "deka")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("d", UnitName("d", C::deci,  "deci")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("c", UnitName("c", C::centi, "centi")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("m", UnitName("m", C::milli, "milli")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("u", UnitName("u", C::micro, "micro")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("n", UnitName("n", C::nano,  "nano")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("p", UnitName("p", C::pico,  "pico")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("f", UnitName("f", C::femto, "femto")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("a", UnitName("a", C::atto,  "atto")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("z", UnitName("z", C::zepto, "zepto")));
  UnitMap::mapPref->insert(map<String, UnitName>::value_type
			   ("y", UnitName("y", C::yocto, "yocto")));
}

} //# NAMESPACE CASACORE - END

