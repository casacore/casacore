//# NewMSDopplerIndex.cc:  this defined NewMSDopplerIndex
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

#include <trial/MeasurementSets/NewMSDopplerIndex.h>

#include <aips/Arrays/ArrayUtil.h>
#include <aips/MeasurementSets/NewMSDoppler.h>

NewMSDopplerIndex::NewMSDopplerIndex() 
    : NewMSTableIndex()
{;}

NewMSDopplerIndex::NewMSDopplerIndex(const NewMSDoppler &doppler)
    : NewMSTableIndex(doppler, stringToVector("DOPPLER_ID,SOURCE_ID"))
{ attachIds();}

NewMSDopplerIndex::NewMSDopplerIndex(const NewMSDopplerIndex &other)
    : NewMSTableIndex(other)
{ attachIds();}

NewMSDopplerIndex::~NewMSDopplerIndex()
{;}

NewMSDopplerIndex &NewMSDopplerIndex::operator=(const NewMSDopplerIndex &other)
{
    if (this != &other) {
	NewMSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void NewMSDopplerIndex::attach(const NewMSDoppler &doppler)
{
    NewMSTableIndex::attach(doppler, stringToVector("DOPPLER_ID,SOURCE_ID"));
    attachIds();
}

void NewMSDopplerIndex::attachIds()
{
    dopplerId_p.attachToRecord(accessKey(), "DOPPLER_ID");
    sourceId_p.attachToRecord(accessKey(), "SOURCE_ID");
}

