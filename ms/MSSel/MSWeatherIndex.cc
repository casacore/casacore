//# MSWeatherIndex.cc:  this defined MSWeatherIndex
//# Copyright (C) 2000
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

#include <casacore/ms/MSSel/MSWeatherIndex.h>

#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/ms/MeasurementSets/MSWeather.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSWeatherIndex::MSWeatherIndex() 
    : MSTableIndex()
{;}

MSWeatherIndex::MSWeatherIndex(const MSWeather &weather)
    : MSTableIndex(weather, stringToVector("ANTENNA_ID"))
{ attachIds();}

MSWeatherIndex::MSWeatherIndex(const MSWeatherIndex &other)
    : MSTableIndex(other)
{ attachIds();}

MSWeatherIndex::~MSWeatherIndex()
{;}

MSWeatherIndex &MSWeatherIndex::operator=(const MSWeatherIndex &other)
{
    if (this != &other) {
	MSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void MSWeatherIndex::attach(const MSWeather &weather)
{
    MSTableIndex::attach(weather, stringToVector("ANTENNA_ID"));
    attachIds();
}

void MSWeatherIndex::attachIds()
{
    antennaId_p.attachToRecord(accessKey(), "ANTENNA_ID");
}


} //# NAMESPACE CASACORE - END

