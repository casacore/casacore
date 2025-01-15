//# UnitMap3.cc: Unit map SI unit initialisation part 1
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes

#include <casacore/casa/Quanta/UnitMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# constants

// Initialise the maps
void UnitMap::initUMSI1(UMaps& maps) {
  map<String, UnitName>& mapDef = maps.mapDef;
  map<String, UnitName>& mapSI  = maps.mapSI;
  for (Int i=0; i<UnitDim::Dnumber; i++) {
    mapDef.insert(map<String, UnitName>::value_type
			    (UnitDim::dimName(i),
			     UnitName(UnitDim::dimName(i),
				      UnitVal(1.0, i),
				      UnitDim::dimFull(i))));
    
    // SI units
    if (i != UnitDim::Dkg) {
      mapSI.insert(map<String, UnitName>::value_type
			     (UnitDim::dimName(i),
			      UnitName(UnitDim::dimName(i),
				       UnitVal(1.0, i),
				       UnitDim::dimFull(i))));
    }
  }
  
  mapSI.insert(map<String, UnitName>::value_type
			 ("$",	UnitName("$",
					 UnitVal(1., UnitDim::Dnon),
					 "currency")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("%",	UnitName("%",
					 UnitVal(0.01),
					 "percent")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("%%",	UnitName("%%",
					 UnitVal(0.001),
					 "permille")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("g",	UnitName("g",
					 UnitVal(0.001, UnitDim::Dkg),
					 "gram")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Bq",  UnitName("Bq",
					  UnitVal(1., "s-1", &maps),
					  "becquerel")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Hz",  UnitName("Hz",
					  UnitVal(1., "s-1", &maps),
					  "hertz")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("C",   UnitName("C",
					  UnitVal(1.,"A.s", &maps),
					  "coulomb")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("lm",  UnitName("lm",
					  UnitVal(1., "cd.sr", &maps),
					  "lumen")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("N",	UnitName("N",
					 UnitVal(1., "kg.m.s-2", &maps),
					 "newton")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("J",   UnitName("J",
					  UnitVal(1., "N.m", &maps),
					  "joule")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("W",   UnitName("W",
					  UnitVal(1., "J.s-1", &maps),
					  "watt")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("V",   UnitName("V",
					  UnitVal(1., "W.A-1", &maps),
					  "volt")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("F",   UnitName("F",
					  UnitVal(1., "C.V-1", &maps),
					  "farad")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Gy",  UnitName("Gy",
					  UnitVal(1., "J.kg-1", &maps),
					  "gray")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("lx",  UnitName("lx",
					  UnitVal(1., "lm.m-2", &maps),
					  "lux")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Ohm", UnitName("Ohm",
					  UnitVal(1., "V.A-1", &maps),
					  "ohm")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Pa",  UnitName("Pa",
					  UnitVal(1., "N.m-2", &maps),
					  "pascal")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("S",   UnitName("S",
					  UnitVal(1., "Ohm-1", &maps),
					  "siemens")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Sv",  UnitName("Sv",
					  UnitVal(1., "J.kg-1", &maps),
					  "sievert")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("Wb",  UnitName("Wb",
					  UnitVal(1., "V.s", &maps),
					  "weber")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("H",   UnitName("H",
					  UnitVal(1., "Wb.A-1", &maps),
					  "henry")));
  mapSI.insert(map<String, UnitName>::value_type
			 ("T",   UnitName("T",
					  UnitVal(1., "Wb.m-2", &maps),
					  "tesla")));
}  

} //# NAMESPACE CASACORE - END

