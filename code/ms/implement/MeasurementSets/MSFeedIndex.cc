//# NewMSFeedIndex.cc:  this defined NewMSFeedIndex
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

#include <trial/MeasurementSets/NewMSFeedIndex.h>

#include <aips/Arrays/ArrayUtil.h>
#include <aips/MeasurementSets/NewMSFeed.h>

NewMSFeedIndex::NewMSFeedIndex() 
    : NewMSTableIndex()
{;}

NewMSFeedIndex::NewMSFeedIndex(const NewMSFeed &feed)
    : NewMSTableIndex(feed, stringToVector("ANTENNA_ID,FEED_ID,SPECTRAL_WINDOW_ID"))
{ attachIds();}

NewMSFeedIndex::NewMSFeedIndex(const NewMSFeedIndex &other)
    : NewMSTableIndex(other)
{ attachIds();}

NewMSFeedIndex::~NewMSFeedIndex()
{;}

NewMSFeedIndex &NewMSFeedIndex::operator=(const NewMSFeedIndex &other)
{
    if (this != &other) {
	NewMSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void NewMSFeedIndex::attach(const NewMSFeed &feed)
{
    NewMSTableIndex::attach(feed, stringToVector("ANTENNA_ID,FEED_ID,SPECTRAL_WINDOW_ID"));
    attachIds();
}

void NewMSFeedIndex::attachIds()
{
    antennaId_p.attachToRecord(accessKey(), "ANTENNA_ID");
    feedId_p.attachToRecord(accessKey(), "FEED_ID");
    spwId_p.attachToRecord(accessKey(), "SPECTRAL_WINDOW_ID");
}

