//# UnitMap5.cc: Unit map custom units initialisation part 1
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
void UnitMap::initUMCust1() {
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
}
