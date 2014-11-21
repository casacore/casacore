//# tFITSHistoryUtil.cc: Test program for FITSHistoryUtil
//# Copyright (C) 2002
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
#include <casacore/fits/FITS/FITSHistoryUtil.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/casa/Logging.h>
#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {	
	// LogSink to use in testing here - use a memory log sink
	LogMessage message(LogOrigin("testFITSHistoryUtil()",WHERE));
	message.message("this is a test").line(__LINE__);
	LoggerHolder logger(False);
	logger.sink().post(message);
	message.message("This is another LogMessage stored in the sink to be transfered to FITS");
	logger.sink().post(message);
	
	vector<String> history;
	Bool aipsppFormat = True;
	uInt nstrings, nread;
	nstrings = nread = 0;
	nread = FITSHistoryUtil::toHISTORY(history, aipsppFormat, nstrings,
					   uInt(0), logger);
	// there are 2 things inserted here, so nread should be 2
	AlwaysAssertExit(nread == 2);
	
	FitsKeywordList kwl;
	if (aipsppFormat) {
	    FITSHistoryUtil::addHistoryGroup(kwl, history, nstrings, 
					     "LOGTABLE");
	} else {
	    FITSHistoryUtil::addHistoryGroup(kwl, history, nstrings, "");
	}
	
	// add some other other history, to a different group
	vector<String> otherHistory(3);
	otherHistory[0] = "I like cats.";
	otherHistory[1] = "This is a longer history message to see how it handles that sort of thing.";
	otherHistory[2] = "This is part of the OTHER group.";
	FITSHistoryUtil::addHistoryGroup(kwl, otherHistory, 
					 otherHistory.size(), "OTHER");
	
	// and add some things without a group
	vector<String> moreHistory(4);
	moreHistory[0] = "More history.";
	moreHistory[1] = "Still more history.";
	moreHistory[2] = "And still more history.";
	moreHistory[3] =
	    "And the end of the more history, although this is another long message that should wrap";
	FITSHistoryUtil::addHistoryGroup(kwl, moreHistory, 
					 moreHistory.size(), "");
	
	// now retrieve stuff from kwl
	Vector<String> stringsOut;
	String groupType;
	uInt n;
	ConstFitsKeywordList ckwl(kwl);
	ckwl.first();
	while ((n = FITSHistoryUtil::getHistoryGroup(stringsOut, groupType, 
						     ckwl)) != 0) {
	    LoggerHolder logOut(False);
	    if (groupType == "LOGTABLE") {
		FITSHistoryUtil::fromHISTORY(logOut, stringsOut, n, True);
		Int iterCount = 0;
		LoggerHolder::const_iterator origIter = logger.begin();
		for (LoggerHolder::const_iterator iter = logOut.begin();
		     iter != logOut.end();
		     iter++) {
		    AlwaysAssertExit(origIter != logger.end());
		    iterCount++;
		    // I'm not sure what the precision of the stuff stored 
		    // in the FITS kwl is.  I think its 1s accuracy.
		    AlwaysAssertExit((origIter->time()-iter->time())<=1);
		    AlwaysAssertExit(origIter->priority()==iter->priority());
		    AlwaysAssertExit(origIter->message()==iter->message());
		    AlwaysAssertExit(origIter->location()==iter->location());
		    AlwaysAssertExit(origIter->objectID()==iter->objectID());
		    origIter++;
		}
		// we put 2 in, we should have gotten 2 out
		AlwaysAssertExit(iterCount==2);
		// the return value of n is 2* the number of log lines - 
		// one for the message and one for the time/priority/origin 
		// line
		AlwaysAssertExit(n==4);
	    } else if (groupType == "OTHER") {
		AlwaysAssertExit(n==otherHistory.size());
		for (uInt i=0;i<otherHistory.size();i++) {
		    AlwaysAssertExit(otherHistory[i] == stringsOut(i));
		}
	    } else {
		AlwaysAssertExit(n==moreHistory.size());
		for (uInt i=0;i<moreHistory.size();i++) {
		    AlwaysAssertExit(moreHistory[i] == stringsOut(i));
		}
	    }
	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    }
    return 0;
}

