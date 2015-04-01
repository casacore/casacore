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

// Initialise the maps
void UnitMap::initUMSI1() {
  for (Int i=0; i<UnitDim::Dnumber; i++) {
    UnitMap::mapDef->insert(map<String, UnitName>::value_type
			    (UnitDim::dimName(i),
			     UnitName(UnitDim::dimName(i),
				      UnitVal(1.0,i),
				      UnitDim::dimFull(i))));
    
    // SI units
    if (i != UnitDim::Dkg) {
      UnitMap::mapSI->insert(map<String, UnitName>::value_type
			     (UnitDim::dimName(i),
			      UnitName(UnitDim::dimName(i),
				       UnitVal(1.0,i),
				       UnitDim::dimFull(i))));
    }
  }
  
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("$",	UnitName("$",
					 UnitVal(1.,UnitDim::Dnon),
					 "currency")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("%",	UnitName("%",
					 UnitVal(0.01),
					 "percent")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("%%",	UnitName("%%",
					 UnitVal(0.001),
					 "permille")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("g",	UnitName("g",
					 UnitVal(0.001,UnitDim::Dkg),
					 "gram")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Bq",  UnitName("Bq",
					  UnitVal(1.,"s-1"),
					  "becquerel")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Hz",  UnitName("Hz",
					  UnitVal(1.,"s-1"),
					  "hertz")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("C",   UnitName("C",
					  UnitVal(1.,"A.s"),
					  "coulomb")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("lm",  UnitName("lm",
					  UnitVal(1.,"cd.sr"),
					  "lumen")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("N",	UnitName("N",
					 UnitVal(1.,"kg.m.s-2"),
					 "newton")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("J",   UnitName("J",
					  UnitVal(1.,"N.m"),
					  "joule")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("W",   UnitName("W",
					  UnitVal(1.,"J.s-1"),
					  "watt")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("V",   UnitName("V",
					  UnitVal(1.,"W.A-1"),
					  "volt")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("F",   UnitName("F",
					  UnitVal(1.,"C.V-1"),
					  "farad")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Gy",  UnitName("Gy",
					  UnitVal(1.,"J.kg-1"),
					  "gray")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("lx",  UnitName("lx",
					  UnitVal(1.,"lm.m-2"),
					  "lux")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Ohm", UnitName("Ohm",
					  UnitVal(1.,"V.A-1"),
					  "ohm")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Pa",  UnitName("Pa",
					  UnitVal(1.,"N.m-2"),
					  "pascal")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("S",   UnitName("S",
					  UnitVal(1.,"Ohm-1"),
					  "siemens")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Sv",  UnitName("Sv",
					  UnitVal(1.,"J.kg-1"),
					  "sievert")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("Wb",  UnitName("Wb",
					  UnitVal(1.,"V.s"),
					  "weber")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("H",   UnitName("H",
					  UnitVal(1.,"Wb.A-1"),
					  "henry")));
  UnitMap::mapSI->insert(map<String, UnitName>::value_type
			 ("T",   UnitName("T",
					  UnitVal(1.,"Wb.m-2"),
					  "tesla")));
}  

} //# NAMESPACE CASACORE - END

