//# ObsInfo.cc: Miscellaneous information related to an observation
//# Copyright (C) 1998
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
//#
//# $Id$

#include <trial/Coordinates/ObsInfo.h>
#include <aips/Measures/MeasureHolder.h>
#include <aips/Containers/Record.h>
#include <trial/FITS/FITSUtil.h>

#include <iostream.h>

String ObsInfo::defaultTelescope()
{
    return "UNKNOWN";
}

String ObsInfo::defaultObserver()
{
    return defaultTelescope();
}

MEpoch ObsInfo::defaultObsDate()
{
    MEpoch tmp;
    return tmp;
}


ObsInfo::ObsInfo()
    : telescope_p(defaultTelescope()), observer_p(defaultObserver()), 
      obsdate_p(defaultObsDate())
{
    // Nothing
}

ObsInfo::~ObsInfo()
{
    // Nothing
}

void ObsInfo::copy_other(const ObsInfo &other)
{
    if (this != &other) {
	telescope_p = other.telescope_p;
	observer_p = other.observer_p;
	obsdate_p = other.obsdate_p;
    }
}

ObsInfo::ObsInfo(const ObsInfo &other)
{
    copy_other(other);
}

ObsInfo &ObsInfo::operator=(const ObsInfo &other)
{
    copy_other(other);
    return *this;
}

String ObsInfo::telescope() const
{
    return telescope_p;
}

ObsInfo& ObsInfo::setTelescope(const String &telescope)
{
    telescope_p = telescope;
    return *this;
}

String ObsInfo::observer() const
{
    return observer_p;
}

ObsInfo& ObsInfo::setObserver(const String &observer)
{
    observer_p = observer;
    return *this;
}

MEpoch ObsInfo::obsDate() const
{
    return obsdate_p;
}

ObsInfo &ObsInfo::setObsDate(const MEpoch &obsDate)
{
    obsdate_p = obsDate;
    return *this;
}

Bool ObsInfo::toRecord(String & error, RecordInterface & outRecord) const
{
    error = "";

    outRecord.define("telescope", telescope());
    outRecord.define("observer", observer());

    MeasureHolder mh(obsDate());
    Record obsrec;
    Bool ok = mh.toRecord(error, obsrec);
    if (ok) {
	outRecord.defineRecord("obsdate", obsrec);
    }
    
    return ok;
}

Bool ObsInfo::fromRecord(String & error, const RecordInterface & inRecord)
{
    error = "";

    ObsInfo tmp;
    (*this) = tmp; // Make sure we are "empty" first;

    Int field = inRecord.fieldNumber("telescope");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of telescope field is not String!";
	    return False;
	}
	setTelescope(inRecord.asString(field));
    }
    field = inRecord.fieldNumber("observer");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of observer field is not String!";
	    return False;
	}
	setObserver(inRecord.asString(field));
    }
    field = inRecord.fieldNumber("obsdate");
    if (field >= 0) {
	if (inRecord.type(field) != TpRecord) {
	    error = "Type of obsdate field is not Record!";
	    return False;
	}
	MeasureHolder mh;
	Bool ok = mh.fromRecord(error, inRecord.asRecord(field));
	if (!ok) {
	    return False;
	}
	if (!mh.isMEpoch()) {
	    error = "obsdate field is not an MEpoch!";
	    return False;
	}
	setObsDate(mh.asMEpoch());
    }
    return True;
}

Bool ObsInfo::toFITS(String & error, RecordInterface & outRecord) const
{
    error = "";

    String name = "telescop";
    if (telescope() != defaultTelescope()) {
	outRecord.define(name, telescope());
    } else {
	// Remove it if it already exists
	Int field = outRecord.fieldNumber(name);
	if (field >= 0 && !outRecord.isFixed()) {
	    outRecord.removeField(field);
	}
    }

    name = "observer";
    if (observer() != defaultObserver()) {
	outRecord.define(name, observer());
    } else {
	// Remove it if it already exists
	Int field = outRecord.fieldNumber(name);
	if (field >= 0 && !outRecord.isFixed()) {
	    outRecord.removeField(field);
	}
    }

    // Sort of yucky, but avoids a cast!
    name = "date-obs";
    MVTime time = obsDate().get("s");
    if (time != MVTime(defaultObsDate().get("s"))) {  // Roundoff problems?
	MEpoch::Types dtype;
	Bool ok = MEpoch::getType(dtype, MEpoch::showType(obsDate().type()));
	if (!ok) {
	    error = "Could not convert MEpoch to a type code! Cannot happen!";
	    return False;
	}
	String date, timesys;
	FITSDateUtil::toFITS(date, timesys, time, dtype);
	outRecord.define(name, date);
	outRecord.define("timesys", timesys);
    } else {
	// Remove it if it already exists
	Int field = outRecord.fieldNumber(name);
	if (field >= 0 && !outRecord.isFixed()) {
	    outRecord.removeField(field);
	}
	// Maybe we should also remove TIMESYS, but it is conceivably needed
	// for some other DATE field. FITS sure is yuck-o.
    }
						      
    return True;
}

Bool ObsInfo::fromFITS(String & error, const RecordInterface & inRecord)
{
    error = "";

    ObsInfo tmp;
    (*this) = tmp; // Make sure we are "empty" first;
    Int field = inRecord.fieldNumber("telescop");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of TELESCOP field is not String!";
	    return False;
	}
	setTelescope(inRecord.asString(field));
    }
    field = inRecord.fieldNumber("observer");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of OBSERVER field is not String!";
	    return False;
	}
	setObserver(inRecord.asString(field));
    }
    field = inRecord.fieldNumber("date-obs");
    if (field >= 0) {
	if (inRecord.type(field) != TpString) {
	    error = "Type of DATE-OBS field is not a String!";
	    return False;
	}
	Int field2 = inRecord.fieldNumber("timesys");
	String timesysstring;
	if (field2 >= 0 && inRecord.type(field2) == TpString) {
	    timesysstring = inRecord.asString(field2);
	}
	MVTime time; MEpoch::Types timesys;
	String datestring = inRecord.asString(field);
	Bool ok = 
	    FITSDateUtil::fromFITS(time, timesys, datestring, timesysstring);
	setObsDate(MEpoch(time.get(), timesys));
	if (!ok) {
	    error = "Could not decode FITS date format from keywords";
	    return False;
	}
    }
    return True;
}

Vector<String> ObsInfo::keywordNamesFITS()
{
    Vector<String> vs(4);
    vs(0) = "telescop";
    vs(1) = "observer";
    vs(2) = "date-obs";
    vs(3) = "timesys";
    return vs;
}

ostream &operator<<(ostream &os, const ObsInfo &info)
{
    os << "Telescope: " << info.telescope() << " Observer: " <<
	info.observer() << " Date Observed: " << info.obsDate();
    return os;
}

