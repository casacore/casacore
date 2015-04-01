//# FITSMultiTable.h: View multiple FITS files as a single table
//# Copyright (C) 1995,1996,1997
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

#ifndef FITS_FITSMULTITABLE_H
#define FITS_FITSMULTITABLE_H

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//  View multiple FITS files as a single table
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// A FITSMultiTable is used to view a collection of FITS files on disk as a 
// single Table. That is, when next() is called, when one Table ends the next
// is reopened until all files are exhausted. The FITS files must all have the
// same description. Something clever should be done about the keywords.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="yyyy/mm/dd">
// </todo>

class FITSMultiTable :  public FITSTabular
{
public:
    // The FITS files associated with the fileNames must all have the same
    // description, the second argument is a function to generate the
    // FITSTabular  If not specified, a generic FITSTable is assumed.
    // The returned pointer IS controlled by this object.
    FITSMultiTable(const Vector<String> &fileNames,
		   FITSTabular* (*tabMaker)(const String &) = 0);
    ~FITSMultiTable();

    virtual Bool isValid() const;
    virtual const TableRecord &keywords() const;
    virtual const RecordDesc &description() const;
    virtual const Record &units() const;
    virtual const Record &displayFormats() const;
    virtual const Record &nulls() const;

    virtual const String &name() const { return table_p->name(); }

    // Only returns True when all files are exhausted.
    virtual Bool pastEnd() const;
    // When end of data is hit on the current file, the next file is opened
    // automatically.
    virtual void next();
    virtual const Record &currentRow() const;

    // get the list of file names
    const Vector<String>& fileNames() const { return file_names_p;}

    // Has the descriptor changed from when the file was opened
    virtual Bool hasChanged() const { return hasChanged_p;}

    // set hasChanged to False - used after hasChanged has been checked
    void resetChangedFlag() { hasChanged_p = False;}

    // A helper function to generate a list of fileNames. This function returns
    // all the files in "directoryName" which have the form
    // yyyy_mm_dd_hh:mm:ss_*.fits and which are (even partially) 
    // in the time range specified by startTime and endTime. It is used to 
    // generate a set of file names for use in the FITSMultiTable constructor. 
    // If verboseStatus is True, some status messages appear on cout.
    // If verboseErrors is True improperly named files names (not matching the above 
    // pattern) are named on cerrt.
    static Vector<String> filesInTimeRange(const String &directoryName, 
				   const Time &startTime, const Time &endTime,
					   Bool verboseErrors = False,
					   Bool verboseStatus = False);
    // return the time as found in the given string using the form given above
    // There are no sanity checks in this subroutine
    static Time timeFromFile(const String &fileName);

private:
    // Undefined and inaccessible
    FITSMultiTable();
    FITSMultiTable(const FITSMultiTable &other);
    FITSMultiTable &operator=(const FITSMultiTable &other);

    FITSTabular *table_p;

    Vector<String> file_names_p;
    uInt nfiles_p;
    uInt which_file_p;
    Bool hasChanged_p;

    Record row_p;

    FITSTabular* defaultMaker(const String& fileName);
};


} //# NAMESPACE CASACORE - END

#endif
