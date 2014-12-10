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
void UnitMap::initUMSI2() {
  // non-metric SI units
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("deg",   	UnitName("deg",
						 UnitVal(C::degree,"rad"),
						 "degree")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("arcmin",  	UnitName("arcmin",
						 UnitVal(C::arcmin,"rad"),
						 "arcmin")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("arcsec",  	UnitName("arcsec",
						 UnitVal(C::arcsec,"rad"),
						 "arcsec")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("as", 		UnitName("as",
							 UnitVal(1.,"arcsec"),
							 "arcsec")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("L",   	UnitName("L",
						 UnitVal(1.,"dm3"),
						 "litre")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("l",   	UnitName("l",
						 UnitVal(1.,"L"),
						 "litre")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("d",   	UnitName("d",
						 UnitVal(C::day,"s"),
						 "day")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("h",   	UnitName("h",
						 UnitVal(C::hour,"s"),
						 "hour")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("min",   	UnitName("min",
						 UnitVal(C::minute,"s"),
						 "minute")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("a",   	UnitName("a",
						 UnitVal(24.*3600.*365.25,"s"),
						 "year")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("t",   	UnitName("t",
						 UnitVal(1000.,"kg"),
						 "tonne")));
  
  // Astronomical SI units
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Jy",   	UnitName("Jy",
						 UnitVal(1.0e-26,"W/m2/Hz"),
						 "jansky")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("AU",   	UnitName("AU",
						 UnitVal(C::c * IAU_tauA,"m"),
						 "astronomical unit")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("UA",   	UnitName("UA",
						 UnitVal(1.,"AU"),
						 "astronomical unit")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("AE",   	UnitName("AE",
						 UnitVal(1.,"AU"),
						 "astronomical unit")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("S0",   	UnitName("S0",
						 UnitVal(IAU_k*IAU_k/6.67259e-11,
							 "AU3/d2/(m3/kg/s2)"),
						 "solar mass")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("M0",   	UnitName("M0",
						 UnitVal(1.,"S0"),
						 "solar mass")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("pc",   	UnitName("pc",
						 UnitVal(1./C::arcsec,"AU"),
						 "parsec")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("cy",   	UnitName("cy",
						 UnitVal(24.*3600.*36525,"s"),
						 "century")));
}

} //# NAMESPACE CASACORE - END

