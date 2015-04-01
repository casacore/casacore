//# MSDopplerIndex.cc:  this defined MSDopplerIndex
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

#include <casacore/ms/MSSel/MSDopplerIndex.h>

#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/ms/MeasurementSets/MSDoppler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSDopplerIndex::MSDopplerIndex() 
    : MSTableIndex()
{;}

MSDopplerIndex::MSDopplerIndex(const MSDoppler &doppler)
    : MSTableIndex(doppler, stringToVector("DOPPLER_ID,SOURCE_ID"))
{ attachIds();}

MSDopplerIndex::MSDopplerIndex(const MSDopplerIndex &other)
    : MSTableIndex(other)
{ attachIds();}

MSDopplerIndex::~MSDopplerIndex()
{;}

MSDopplerIndex &MSDopplerIndex::operator=(const MSDopplerIndex &other)
{
    if (this != &other) {
	MSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void MSDopplerIndex::attach(const MSDoppler &doppler)
{
    MSTableIndex::attach(doppler, stringToVector("DOPPLER_ID,SOURCE_ID"));
    attachIds();
}

void MSDopplerIndex::attachIds()
{
    dopplerId_p.attachToRecord(accessKey(), "DOPPLER_ID");
    sourceId_p.attachToRecord(accessKey(), "SOURCE_ID");
}


} //# NAMESPACE CASACORE - END

