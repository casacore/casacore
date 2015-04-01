//# Fitsmultitablex.h: View multiple FITS files as a single table
//# Copyright (C) 1995,1996,1997,1998,1999,2001
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

#include <casacore/casa/iostream.h>

#include <casacore/fits/FITS/FITSMultiTable.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/DirectoryIterator.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/GenSort.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Used for debug print statements
// #include <casacore/casa/Arrays/ArrayIO.h

FITSMultiTable::FITSMultiTable(const Vector<String> &fileNames,
			       FITSTabular* (*tabMaker)(const String&))
    : table_p(0), file_names_p(fileNames.copy()),  
      nfiles_p(fileNames.nelements()), which_file_p(0), 
      hasChanged_p(False), row_p(RecordInterface::Variable)
{
    AlwaysAssert(nfiles_p > 0, AipsError);
    for (uInt i=0;i<nfiles_p;i++) {
	if (tabMaker) 
	    table_p = tabMaker(fileNames(i));
	else 
	    table_p = defaultMaker(fileNames(i));
	AlwaysAssert(table_p, AipsError);    
	if (table_p->isValid()) {
	    which_file_p = i;
	    break;
	}
   } 

    row_p.restructure(table_p->description());
    row_p = table_p->currentRow();
}

FITSMultiTable::~FITSMultiTable()
{
    delete table_p;
    table_p = 0;
}

Bool FITSMultiTable::isValid() const
{
    return table_p->isValid();
}

const TableRecord &FITSMultiTable::keywords() const
{
    return table_p->keywords();
}

const RecordDesc &FITSMultiTable::description() const
{
    return table_p->description();
}

const Record &FITSMultiTable::units() const
{
    return table_p->units();
}

const Record &FITSMultiTable::displayFormats() const
{
    return table_p->displayFormats();
}

const Record &FITSMultiTable::nulls() const
{
    return table_p->nulls();
}

Bool FITSMultiTable::pastEnd() const
{
    return (which_file_p >= nfiles_p);
}

void FITSMultiTable::next()
{
    table_p->next();
    Bool status = True;
    uInt thisWhich = which_file_p;
    if (table_p->pastEnd()) {
	which_file_p++;
	RecordDesc oldDescription = table_p->description();
	status = False;
	if (which_file_p >= nfiles_p) status = True;
	while (which_file_p < nfiles_p && ! status) {
	    status = table_p->reopen(file_names_p(which_file_p));
	    if (!status) {
		cerr << "FITSMultiTable::next() - Problem opening : "
		    << file_names_p(which_file_p) << " - skipping this file " 
			<< endl;
		which_file_p++;
	    } else {
		if (oldDescription != description()) {
		    hasChanged_p = True;
		    row_p.restructure(table_p->description());
		}
	    }
	}
    }
    // if status is False
    // reopen previous successfully opened file
    if (!status) {
	table_p->reopen(file_names_p(thisWhich));
    }
    // if that failed, then that likely means that none of these will
    // work and we should probably throw the exception that will likely
    // happen here.
    row_p = table_p->currentRow();
}

const Record &FITSMultiTable::currentRow() const
{
    return row_p;
}

// This is truely bizare.  For CFRONT based compiler the
// following is required in order for this all to compile.
// If this code is placed in place in filesInTimeRange where it
// is used, both Centerline and Sun's CFRONT compilers complain
// about not being able to find operator << (class ostream, unsigned int)
// It seems to be tied to Vector<Double> in some sense since
// commenting out all occurences of them makes the compiler
// happy (but the code unusable, obviously).  Moving the output
// cout outside of filesInTimeRange makes the problem go away.
// It is obviously a complicated interacting involving other
// elements of this class as I am unable to reproduce it except
// in this class.

void timeRangeStatusMsg(uInt count) 
{
    cout << "Found " << count
	 << " files in specified time range."
	 << endl;
}

Vector<String> FITSMultiTable::filesInTimeRange(const String &directoryName, 
				const Time &startTime, const Time &endTime,
						Bool verboseErrors,
						Bool verboseStatus)
{
    Time t1(startTime), t2(endTime); // Should not be necessary
    Double timeRange = t2 - t1;
    // If the screwed up start and end, work anyway
    if (timeRange < 0) {
	return filesInTimeRange(directoryName, endTime, startTime,
				verboseStatus, verboseErrors);
    }

    File file(directoryName);
    if (! file.isDirectory()) {
	throw(AipsError(directoryName + " is not a directory"));
    }
    
    Directory dir(file);
    Path path(file.path());
    uInt nfiles = dir.nEntries();
    Vector<String> allfiles(nfiles);
    Vector<Double> allStartTimes(nfiles);
    uInt count = 0;
    // If this is still in use in the year 3xxx, it will fail!
    DirectoryIterator diriter(dir, 
			      String("^[12][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_"
				     "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]") + ".*" + 
			      String(".fits$"));

    while (!diriter.pastEnd()) {
	allfiles(count) = diriter.name();;
	allStartTimes(count) = timeFromFile(allfiles(count)) - startTime;
	count++;
	diriter++;
    }
    Vector<String> files(allfiles(Slice(0, count)));
    Vector<Double> startTimes(allStartTimes(Slice(0, count)));
    GenSort<String>::sort(files); // Sorted in ascending order
    GenSort<Double>::sort(startTimes); // should sort exactly the same as files
				// It would be nice if only a single sort were needed

    // Work out the end times, assume they may be as late as the start time
    // of the next file. Guard the end with a large number
    Vector<Double> endTimes(files.nelements());
    uInt i;
    for (i=0; i + 1< endTimes.nelements(); i++) {
	endTimes(i) = startTimes(i + 1);
    }
    if (endTimes.nelements() > 0) {
	endTimes(endTimes.nelements() - 1) = 1.0E+30; // + infinity
    }

    // Work out which files might have values in the appropriate range
    count = 0;
    for (i=0; i < files.nelements(); i++) {
	if (startTimes(i) <= timeRange &&  endTimes(i) >= 0.0) {
	    count++;
	}
    }
    Vector<String> foundFiles(count);
    count = 0;
    for (i=0; i < files.nelements(); i++) {
	if (startTimes(i) <= timeRange &&  endTimes(i) >= 0.0) {
	    foundFiles(count) = dir.path().originalName() + "/" + files(i);
	    count++;
	}
    }
    if (verboseStatus) {
	timeRangeStatusMsg(count);
    }
    return foundFiles;
}

FITSTabular* FITSMultiTable::defaultMaker(const String& fileName)
{
    return  new FITSTable(fileName);
}

Time FITSMultiTable::timeFromFile(const String &fileName)
{
    // try to extract time, assume everything is a number that should be ok in this usage
  // just make sure fileName is a basename 
  Path fpath(fileName);
  String fbase(fpath.baseName());
    const char zero = '0';
    uInt year = fbase[3] - zero + 
	10*(fbase[2] - zero) + 
	100*(fbase[1] - zero) +
	1000*(fbase[0] - zero);
    uInt month = fbase[6] - zero + 
	10*(fbase[5] - zero);
    uInt day = fbase[9] - zero + 
	10*(fbase[8] - zero);
    uInt hour = fbase[12] - zero + 
	10*(fbase[11] - zero);
    uInt minutes = fbase[15] - zero + 
	10*(fbase[14] - zero);
    uInt seconds = fbase[18] - zero + 
	10*(fbase[17] - zero);
    return Time(year, month, day, hour, minutes, seconds*1.0);
}

} //# NAMESPACE CASACORE - END

