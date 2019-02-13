//# UnitMap4.cc: Unit map SI initialisation part 2
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

//# constants

// IAU definition of light time (s) unit distance to calculate IAU units
static const Double IAU_tauA=499.0047837;	

// Initialise the maps
void UnitMap::initUMSI2 (UMaps& maps) {
  map<String, UnitName>& mapSI = maps.mapSI;
  // non-metric SI units
  mapSI.insert(map<String, UnitName>::value_type
			 ("deg",   	UnitName("deg",
						 UnitVal(C::degree, "rad", &maps),
						 "degree")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("arcmin",  	UnitName("arcmin",
						 UnitVal(C::arcmin, "rad", &maps),
						 "arcmin")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("arcsec",  	UnitName("arcsec",
						 UnitVal(C::arcsec, "rad", &maps),
						 "arcsec")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("as", 		UnitName("as",
							 UnitVal(1., "arcsec", &maps),
							 "arcsec")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("L",   	UnitName("L",
						 UnitVal(1., "dm3", &maps),
						 "litre")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("l",   	UnitName("l",
						 UnitVal(1., "L", &maps),
						 "litre")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("d",   	UnitName("d",
						 UnitVal(C::day, "s", &maps),
						 "day")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("h",   	UnitName("h",
						 UnitVal(C::hour, "s", &maps),
						 "hour")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("min",   	UnitName("min",
						 UnitVal(C::minute, "s", &maps),
						 "minute")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("a",   	UnitName("a",
						 UnitVal(24.*3600.*365.25, "s", &maps),
						 "year")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("t",   	UnitName("t",
						 UnitVal(1000., "kg", &maps),
						 "tonne")));
  
  // Astronomical SI units
  mapSI.insert(map<String, UnitName>::value_type
			 ("Jy",   	UnitName("Jy",
						 UnitVal(1.0e-26, "W/m2/Hz", &maps),
						 "jansky")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("AU",   	UnitName("AU",
						 UnitVal(C::c * IAU_tauA, "m", &maps),
						 "astronomical unit")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("UA",   	UnitName("UA",
						 UnitVal(1., "AU", &maps),
						 "astronomical unit")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("AE",   	UnitName("AE",
						 UnitVal(1., "AU", &maps),
						 "astronomical unit")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("S0",   	UnitName("S0",
						 UnitVal(IAU_k*IAU_k/6.67259e-11,
							 "AU3/d2/(m3/kg/s2)", &maps),
						 "solar mass")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("M0",   	UnitName("M0",
						 UnitVal(1., "S0", &maps),
						 "solar mass")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("pc",   	UnitName("pc",
						 UnitVal(1./C::arcsec, "AU", &maps),
						 "parsec")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("cy",   	UnitName("cy",
						 UnitVal(24.*3600.*36525, "s", &maps),
						 "century")));
}

} //# NAMESPACE CASACORE - END

