//# tFITSDateUtil.cc: Test program for FITSDateUtil
//# Copyright (C) 2002,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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
#include <casacore/fits/FITS/FITSDateUtil.h>

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
	// toFITS test
	String date;
	String timesys;
	MVTime timeIn;
	Quantity qtime;
	MVTime::read(qtime,"1990/03/31");
	timeIn = qtime;
	MEpoch::Types systemIn = MEpoch::UTC;
	// force old style
	FITSDateUtil::toFITS(date, timesys, timeIn, systemIn, 
			     FITSDateUtil::OLD);
	if (date != "31/03/90") {
	    throw(AipsError("FITSDateUtil::toFITS failed on conversion to OLD format"));
	}
	if (timesys != "UTC") {
	    throw(AipsError("FITSDateUtil::toFITS conversion to OLD produced unexpected timesys value"));
	}
	
	// AUTO pick will now always get the new format, use current date 
	// and time
	MVTime::read(qtime,"today");
	timeIn = qtime;
	FITSDateUtil::toFITS(date, timesys, timeIn, systemIn);
	
	// fromeFITS test
	// verify that the date and timesys produced above can reproduce 
	// timeIn and systemIn
	MVTime timeOut;
	MEpoch::Types systemOut;
	if (!FITSDateUtil::fromFITS(timeOut, systemOut, date, timesys))
	    throw(AipsError("unexpected failure of FITSDateUtil::fromFITS"));
	
	if (!near(Double(timeOut),Double(timeIn))) {
	    throw(AipsError("FITSDateUtil::fromFITS failed to convert back to original time"));
	}
	if (!(systemOut == systemIn)) {
	    throw(AipsError("FITSDateUtil::fromFITS failed to convert back to original time system"));
	}
	
	// test of a known problem date: 2001-09-26T00:00:00.00
	String problemDate("2001-09-26T00:00:00.00");
	if (!FITSDateUtil::fromFITS(timeOut, systemOut, problemDate, timesys))
	    throw(AipsError("unexpected failure of FITSDateUtil::fromFITS on problem date"));
	String probDateBack;
	FITSDateUtil::toFITS(probDateBack, timesys, timeOut, 
			     systemOut,FITSDateUtil::AUTO_PICK,8);
	if (problemDate != probDateBack) {
	    throw(AipsError("Problem converting to/from the problem date"));
	}
	
	// convertDateString test
	String in("31/03/90");
	String out;
	if (!FITSDateUtil::convertDateString(out, in)) {
	    throw(AipsError("unexpected failure of FITSDateUtil::convertDateString - actual conversion"));
	}
	if (out != "1990-03-31") {
	    throw(AipsError("FITSDateUtil::convertDateString failed to produce expected result"));
	}
	
	// pass-through conversion
	in = "2001-06-01";
	if (!FITSDateUtil::convertDateString(out, in)) {
	    throw(AipsError("unexpected failure of FITSDateUtil::convertDateString - pass through test"));
	}
	if (out != in) {
	    throw(AipsError("FITSDateUtil::convertDateString failed to pass through the value"));
	}
	
	// findPrecision test
	// old style returns 0
	if (FITSDateUtil::findPrecision("31/03/90") != 0) {
	    throw(AipsError("FITSDateUtil::findPrecision - old style didn't return 0"));
	}
	// no time also returns 0
	if (FITSDateUtil::findPrecision("2001-06-01") != 0) {
	    throw(AipsError("FITSDateUtil::findPrecision - no time didn't return 0"));
	}
	// seconds accuracy -> 6
	if (FITSDateUtil::findPrecision("2001-06-01T05:30:20")  != 6) {
	    throw(AipsError("FITSDateUtil::findPrecision - seconds accuracy didn't return 6"));
	}
	// should return 8
	if (FITSDateUtil::findPrecision("2001-06-01T05:30:20.25") != 8) {
	    throw(AipsError("FITSDateUtil::findPrecision - didn't return 8 as expected"));
	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    }
    return 0;
}
