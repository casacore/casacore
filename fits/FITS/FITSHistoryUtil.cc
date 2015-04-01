//# FITSHistoryUtil.cc: this defines FITSHistoryUtil
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

#include <casacore/fits/FITS/FITSHistoryUtil.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/Logging/LogSink.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt FITSHistoryUtil::getHistoryGroup(Vector<String> &strings, 
				      String &groupType,
				      ConstFitsKeywordList &in)
{
    LogIO os;
    os << LogOrigin("FITSHistoryUtil", "getHistoryGroup", WHERE);

    groupType = "";
    const Regex groupstart("^ *[Cc][Aa][Ss][Aa] *[Ss][Tt][Aa][Rr][Tt] *");
    const Regex groupend  ("^ *[Cc][Aa][Ss][Aa] *[Ed][Nn][Dd]");
    const Regex trailing(" *$");
    const String empty;

    // if in is at the top, strangely enough, this gets the first kw
    // if curr() is used, the first kw would be parsed twice
    const FitsKeyword *key = in.next();
    uInt nFound = 0;
    Bool foundStart = False;

    String tmp;
    while (key) {
	if (key->type() == FITS::NOVALUE && key->kw().name() == FITS::HISTORY) {
	    // Found a history card.
	    tmp = key->comm();
	    // Get rid of trailing spaces for all strings
	    tmp.gsub(trailing, empty);
	    if (tmp.contains(groupstart)) {
		// CASA START
		if (foundStart) {
		    os << LogIO::SEVERE << "Cannot handle nested CASA START"
			" history keywords. Ignoring" << LogIO::POST;
		} else if (nFound > 0) {
		    // OK, we are ending a normal HISTORY group
		    break;
		} else {
		    // OK, found a valid start of group.
		    foundStart = True;
		    tmp.gsub(groupstart, "");
		    tmp.gsub(" ", "");
		    groupType = tmp;
		}
	    } else if (tmp.contains(groupend)) {
		// CASA END
		if (foundStart) {
		    // OK, a normal end.
		    // Attempt to parse the TYPE in the END statement to see
		    // if it matches for debugging purposes.
		    tmp.gsub(groupend, "");
		    tmp.gsub(" ", "");
		    if (tmp != "") {
			if (tmp != groupType) {
			    os << LogIO::SEVERE << 
				"HISTORY START and END types do not match (" <<
				groupType << "," << tmp << ")" <<
				LogIO::POST;
			}
		    }
		    break;
		} else {
		    os << LogIO::SEVERE << "CASA END found in history without"
			" a corresponding START. Ignoring" << LogIO::POST;
		}
	    } else {
		// A string in the group.
		if (nFound == 0 || (tmp.length() > 0 && tmp[0] != '>')) {
		    nFound++;
		    if (nFound >= strings.nelements()) {
			// Exponentially resize for efficiency
			strings.resize(2*nFound + 1, True);
		    }
		    strings(nFound-1) = tmp;
		} else {
		    // continuation - strip out leading '>'
		    strings(nFound-1) += tmp(1, tmp.length()-1);
		}
	    }
	}
	key = in.next(); // Advance to next key
    }
    return nFound;
}

void FITSHistoryUtil::addHistoryGroup(FitsKeywordList &out,
				     const vector<String> &strings,
				     uInt nstrings, const String &groupType)
{
    LogIO os;
    os << LogOrigin("FITSHistoryUtil", "addHistoryGroup", WHERE);
    if (nstrings > strings.size()) {
	os << LogIO::SEVERE << "Asked to add more lines to history than there "
	    "are strings (adjusting)." << LogIO::POST;
	nstrings = strings.size();
    }

    if (groupType != "") {
	String tmp = String("CASA START ") + groupType;
	out.history(tmp.chars());
    }

    const Int maxlen = 72; // 80 - length('HISTORY ');

    String tmp;
    for (uInt i=0; i<nstrings; i++) {
	// Break at \n if any.
	Vector<String> lines = stringToVector(strings[i], '\n');
	for (uInt j=0; j<lines.nelements(); j++) {
	    if (Int(lines(j).length()) <= maxlen) {
		out.history(lines(j).chars());
	    } else {
		// Alas, we need to break the line. maxlen is effectively one
		// less here because we have to put in the leading '>'.
		// We have to turn trailing spaces on one line
		// into leading spaces on the next line - obviously this
		// doesn't work if there are more than 71 of them!
	        const int end = lines(j).length() - 1;
		int start=0, pos=0;
		Bool done = False;
		while (!done) {
		    pos = start + maxlen - 1;
		    if (pos >= end) {
			done = True;
			pos = end;
		    }
		    while (lines(j)[pos] == ' ' && pos > start) {
			pos--; // Backup to the first non-blank character
		    }
		    tmp = start == 0 ? "" : ">";
		    tmp += lines(j)(start, pos - start + 1);
		    out.history(tmp.chars());
		    start = pos;
		    start = pos+1;
		}
	    }
	}
    }	

    if (groupType != "") {
	out.history((String("CASA END ") + groupType).chars());
    }
}


void FITSHistoryUtil::fromHISTORY(LoggerHolder& logger, 
				  const Vector<String> &history, 
				  uInt nstrings, Bool aipsppFormat)
{
    LogIO os;
    os << LogOrigin("FITSHistoryUtil", "fromHistory", WHERE);
    LogSink& sink = logger.sink();
    AlwaysAssert(nstrings <= history.nelements(), AipsError);
//
    if (aipsppFormat  &&  (nstrings%2 == 0)) {

// OK, the first line is supposed to be DATE PRIORITY [SRCCODE] [OBJID]
// And the second one the message.

	Regex timePattern("^[^ ]*");
	Regex timeAndPriorityPattern("^[^ ]* *[^ ]*");
	Regex locationPattern("SRCCODE='.*'");
	Regex objectIDPattern("OBJID='.*'");
	MVTime time;
	MEpoch::Types timeSystem; // we don't care about this for log files!
	String date, priority, message, location, location2, objid, objid2;
	String tmp, msg;
        Double dtime;
	for (uInt i=0; i<nstrings/2; i++) {

// The message is the easy part.

            msg = history(2*i + 1);

// Get the TIME

	    tmp = history(2*i);
	    date = tmp.at(timePattern);
	    if (FITSDateUtil::fromFITS(time, timeSystem, date, "")) {
               dtime = Double(time)*86400.0;
	    } else {

// Maybe we should whinge if we couldn't decode the time?

               dtime = -1.0;
	    }

// PRIORITY

	    priority = tmp.at(timeAndPriorityPattern);
	    priority.gsub(timePattern, "");
	    // there is a leading space in priority at this point
	    priority = priority.after(0);

// LOCATION

	    location2 = tmp.at(locationPattern);
	    if (location2 == "") {
               location = location2;
	    } else {

// We need to strip the trailing '

               location2.gsub("SRCCODE='", "");
               location = location2.at(0, location2.length() - 1);
	    }

// ID
	    objid2 = tmp.at(objectIDPattern);
	    if (objid2 == "") {
                objid = objid2;
	    } else {

// We need to strip the trailing '

		objid2.gsub("OBJID='", "");
		objid = objid2.at(0, objid2.length() - 1);
	    }
//
	    try{
	      sink.writeLocally(dtime, msg, priority, location, objid);
	    }
	    catch (const AipsError& x) {
	      os << LogIO::WARN << "Problem while parsing image HISTORY: " << x.getMesg() 
		 << endl << "Will try to continue ... " << LogIO::POST; 
	    }
	}
    } else {

// Regular FITS HISTORY. 

	for (uInt i=0; i<nstrings; i++) {
            sink.writeLocally(-1.0, history(i), String(""), String(""), String(""));
        }
    }
}


uInt FITSHistoryUtil::toHISTORY(vector<String>& history, Bool& aipsppFormat,
				uInt& nstrings, uInt firstLine,
				const LoggerHolder& logger)
{
    String priority, message, location, id;
    Double timeInSec;
    history.resize(0);
    nstrings = 0;
    Bool thisLineFormat;
    String tmp1, tmp2;
// 
    uInt line = 0;
    for (LoggerHolder::const_iterator iter = logger.begin(); iter != logger.end(); iter++,line++) {
       if (line >= firstLine) {
          priority = iter->priority();
          message = iter->message();
          location = iter->location();
          id = iter->objectID();
          timeInSec = iter->time();
// In  each call  to toHistory, we process a group of contiguous records
// all in the fsame format (Casacore 2 [lines per logical line] or standard fits)

          thisLineFormat = (timeInSec>0.0 && priority!="");
          if (line == firstLine) {
             aipsppFormat = thisLineFormat;
          }
          if (aipsppFormat == thisLineFormat) {
             nstrings += aipsppFormat ? 2 : 1;
             if (aipsppFormat) {
                MVTime time(timeInSec/86400.0);
                FITSDateUtil::toFITS(tmp1, tmp2, time, MEpoch::UTC, 
				     FITSDateUtil::NEW_DATEANDTIME, 6);
                tmp1 += " ";
                tmp1 += priority;
                if (location != "") {
                   tmp1 += " SRCCODE='";
                   tmp1 += location;
                   tmp1 += "'";
                }
                if (id != "") {
                   tmp1 += " OBJID='";
                   tmp1 += id;
                   tmp1 += "'";
                }
                history.push_back(tmp1);
                history.push_back(message);

             }
             else {
            	 history.push_back(message);
             }
          }
       }
    }
    // If nstrings==0, aipsppFormat may not be set yet. So test it.
    if (nstrings > 0) {
      firstLine += aipsppFormat ? nstrings/2 : nstrings;
    }
    return firstLine;
}

} //# NAMESPACE CASACORE - END

