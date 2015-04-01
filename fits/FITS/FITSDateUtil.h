//# FITSDateUtil.h: Class of static functions to help with FITS dates.
//# Copyright (C) 2002
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


#ifndef FITS_FITSDATEUTIL_H
#define FITS_FITSDATEUTIL_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;
class MVTime;

// <summary>
// A class with static functions to help deal with FITS dates
// </summary>

// <use visibility=export>

// <reviewed reviewer="Eric Sessoms" date="2002/08/19" tests="tFITSDateUtil.cc">
// </reviewed>

// <prerequisite>
//   <li> General knowledge of FITS, particularly FITS date keywords,
//        is assumed.
// </prerequisite>
//
// <etymology>
// This is a collection of static utility functions for creating and
// interpreting FITS date keywords (e.g. DATE-OBS).
// </etymology>
//
// <synopsis>
// Its never necessary to construct a FITSDateUtil, just use the 
// static functions to help handle FITS dates.
// </synopsis>
//
// <motivation>
// The strings that make up the value of FITS dates have a 
// precise format.  This class encompasses knowlege of the formats
// used and hopefully simplifies their creation and conversion
// to and from Casacore MVTimes.
// </motivation>
//

class FITSDateUtil
{
public:
    enum DateStyle {
	// dd/mm/yy
	OLD, 
	// yyyy-mm-dd
	NEW_DATEONLY,
	// yyyy-mm-ddThh:mm:ss[.ss...]
	NEW_DATEANDTIME,
	// OLD if the current year is before 1998 AND "time" is before 1998,
	// otherwise NEW_DATEANDTIME.
	AUTO_PICK};

    // Convert an MVTime to a FITS date string and timesys string.  The
    // time system must also be supplied.
    // Precision is only used when the time as well as the date is used
    // (NEW_DATEANDTIME or AUTO_PICK).
    // The default (16) gives 10^(-10) second accuracy, 6 gives second level
    // accuracy. Default is 10^(-6)s (1 micro-s) accuracy. 0 means date only,
    // no time (equivalent to NEW_DATEONLY).
    static void toFITS(String &date, String &timesys, const MVTime &time,
		       MEpoch::Types system = MEpoch::UTC,
		       DateStyle style= AUTO_PICK, uInt precision=12);

    // Convert a FITS date string and TIMESYS keyword value into an MVTime and system. 
    // Returns False if it can't decode date and timesys. It tries to convert as
    // much as possible, for example if it can't decode timesys it still
    // attempts to decode the time. It sets the date to Jan 1/1900 if it can't
    // decode the time, and UTC if it can't decode timesys. If timesys is the
    // empty string then UTC is assumed.
    static Bool fromFITS(MVTime &time, MEpoch::Types &system,
			 const String &date, 
			 const String &timesys);

    // Convert a FITS Date string to the current format. If the "in" format is
    // already correct it is just copied through.
    static Bool convertDateString(String &out, const String &in);


    // Determine the precision in a FITS date string.
    // Old style dates or no time returns 0, New style + time returns 6 + the number
    // of decimal points, i.e. if we have a time at all we assume it is at least
    // accurate to the second level. The result of this can be used in the call
    // to toFITS, i.e. it has the same meaning.
    //
    // This is mostly meant to be a helper function for convertDateString, but
    // it may be called by anyone.
    static uInt findPrecision(const String &fitsDate);
};


} //# NAMESPACE CASACORE - END

#endif
