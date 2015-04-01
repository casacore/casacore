//# FITSDateUtil.cc: this defines FITSDateUtil
//# Copyright (C) 2002,2003
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

#include <casacore/fits/FITS/FITSDateUtil.h>

#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>
#include <ctype.h>
#include <casacore/casa/stdlib.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

void FITSDateUtil::toFITS(String &date, String &timesys, const MVTime &time,
			  MEpoch::Types system, DateStyle style, uInt precision)
{
    date = "invalid";
    timesys = "invalid";

    // First get the DATE string.
    switch (style) {
    case OLD:
	{
	    Int month = time.month();
	    Int day = time.monthday();
	    Int year = time.year() - 1900; 
	    AlwaysAssert(year >= 0 && year<100, AipsError); // 20th century only
	    ostringstream out;
	    out << setfill('0') << setw(2) << day << "/" << setw(2) << month <<
		"/" << setw(2) << year;
	    date = out.str();
	}
	break;
    case NEW_DATEONLY:
	{
	    date = time.string(MVTime::FITS + MVTime::NO_TIME, 0);
	}
    break;
    case NEW_DATEANDTIME:
	{
	    date = time.string(MVTime::FITS, precision); // Full precision?
	    // double check by converting this string back to a time
	    Quantity q;
	    MVTime::read(q,date);
	    MVTime timeCheck = MVTime(q);
	    // within 1/2 day of each other
	    if (abs(time.day() - timeCheck.day()) > 0.5) {
		// send a SEVERE warning to the logger
		LogIO logger;
		logger << LogOrigin("FITSDateUtil", "toFITS", WHERE);
		logger << LogIO::SEVERE 
		       << "unexpected problem converting time to FITS string - "
		       << "the resulting string is off by more than a day - "
		       << LogIO::POST;
		logger << LogIO::SEVERE
		       << "The output date string is : " << date
		       << LogIO::POST;
		logger << LogIO::SEVERE
		       << "please report this bug using bug()."
		       << LogIO::POST;
	    }
	}
    break;
    case AUTO_PICK:
	{
	    style = precision > 0 ? NEW_DATEANDTIME : NEW_DATEONLY;
	    Time now;
	    if (time.year() < 1999 && now.year() < 1999) {
		style = OLD;
	    }
	    toFITS(date, timesys, time, system, style, precision); // recurse
	}
    break;
    default:
	AlwaysAssert(0, AipsError); // NOTREACHED
    }

    // And then timesys
    timesys = MEpoch::showType(system);
    // Canonicalize
    if (timesys == "IAT") {
	timesys = "TAI";
    } else if (timesys == "UT1") {
	timesys = "UT";
    }
}

Bool FITSDateUtil::fromFITS(MVTime &time, MEpoch::Types &system,
			    const String &date,
			    const String &timesys)
{
    Bool ok = True;
    time = MVTime(1900,1,1.0);
    system = MEpoch::UTC;

    // Let's carry on with the date
    if (date.contains("/")) {
	// Old style
	ok = date.length() >= 8;
	ok = ok && isdigit(date[0]);
	ok = ok && isdigit(date[1]);
	ok = ok && isdigit(date[3]);
	ok = ok && isdigit(date[4]);
	ok = ok && isdigit(date[6]);
	ok = ok && isdigit(date[7]);
	Int zero = '0';
	if (ok) {
	    Int year = (date[6]-zero)*10 + date[7]-zero;
	    year += 1900;
	    Int month = (date[3]-zero)*10 + date[4]-zero;
	    Double day = (date[0]-zero)*10 + date[1]-zero;
	    time = MVTime(year, month, day);
	}
    } else {
	// New style
	Quantity q;
	ok = MVTime::read(q, date);
	if (ok) {
	    time = MVTime(q);
	}
    }

    // Convert timesys first
    if (timesys == "") {
	system = MEpoch::UTC; // Default is UTC if not otherwise specified
    } else {
	if (timesys == "UTC") {
	    system = MEpoch::UTC;
	} else if (timesys == "UT") {   // Standard FITS starts here
	    system = MEpoch::UT;
	} else if (timesys == "TAI") {
	    system = MEpoch::TAI;
	} else if (timesys == "IAT") {
	    system = MEpoch::IAT;
	} else if (timesys == "ET") {
	    system = MEpoch::ET;
	} else if (timesys == "TT") {
	    system = MEpoch::TT;
	} else if (timesys == "TDT") {
	    system = MEpoch::TDT;
	} else if (timesys == "TDB") {
	    system = MEpoch::TDB;
	} else if (timesys == "TCG") {
	    system = MEpoch::TCG;
	} else if (timesys == "TCB") { 
	    system = MEpoch::TCB;
	} else if (timesys == "LAST") {   // Casacore extensions here
	    system = MEpoch::LAST;
	} else if (timesys == "LMST") { 
	    system = MEpoch::LMST;
	} else if (timesys == "GMST1") { 
	    system = MEpoch::GMST1;
	} else if (timesys == "GAST") { 
	    system = MEpoch::GAST;
	} else if (timesys == "UT1") { 
	    system = MEpoch::UT1;
	} else if (timesys == "UT2") { 
	    system = MEpoch::UT2;
	} else if (timesys == "GMST") { 
	    system = MEpoch::GMST;
	} else {
	    ok = False;
	}
    }

    return ok;
}

Bool FITSDateUtil::convertDateString(String &out, const String &in)
{
    MVTime time;
    MEpoch::Types system;
    Bool ok = FITSDateUtil::fromFITS(time, system, in, "");
    if (ok) {
	String sys;
	uInt precision = findPrecision(in);
	FITSDateUtil::toFITS(out, sys, time, 
			     MEpoch::UTC, AUTO_PICK, precision);
    }
    return ok;
}

uInt FITSDateUtil::findPrecision(const String &fitsDate)
{
    if (fitsDate.contains("/")) {
	return 0; // Old style has no time
    }

    // OK, new style
    uInt prec = 0;
    if (fitsDate.contains("T")) {
	prec += 6; // We are good at least to the second
	Int decimalpos = fitsDate.index('.');
	if (decimalpos > 0) {
	    // OK, we may have some decimal points, count 'em.
	    for (uInt i=decimalpos+1; 
		 i<fitsDate.length() && isdigit(fitsDate[i]); i++) {
		prec++;
	    }
	}
    }
    return prec;
}

} //# NAMESPACE CASACORE - END

