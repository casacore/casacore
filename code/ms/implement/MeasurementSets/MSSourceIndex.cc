//# MSSourceIndex.cc:  this defined MSSourceIndex
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

#include <trial/MeasurementSets/MSSourceIndex.h>

#include <aips/Arrays/ArrayUtil.h>
#include <aips/MeasurementSets/MSSource.h>

MSSourceIndex::MSSourceIndex() 
    : MSTableIndex()
{;}

MSSourceIndex::MSSourceIndex(const MSSource &source)
    : MSTableIndex(source, stringToVector("SOURCE_ID,SPECTRAL_WINDOW_ID"))
{ attachIds();}

MSSourceIndex::MSSourceIndex(const MSSourceIndex &other)
    : MSTableIndex(other)
{ attachIds();}

MSSourceIndex::~MSSourceIndex()
{;}

MSSourceIndex &MSSourceIndex::operator=(const MSSourceIndex &other)
{
    if (this != &other) {
	MSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void MSSourceIndex::attach(const MSSource &source)
{
    MSTableIndex::attach(source, stringToVector("SOURCE_ID,SPECTRAL_WINDOW_ID"));
    attachIds();
}

void MSSourceIndex::attachIds()
{
    sourceId_p.attachToRecord(accessKey(), "SOURCE_ID");
    spwId_p.attachToRecord(accessKey(), "SPECTRAL_WINDOW_ID");
}

