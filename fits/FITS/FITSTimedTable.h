//# FITSTimedTable.h: A Table with a time column
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

#ifndef FITS_FITSTIMEDTABLE_H
#define FITS_FITSTIMEDTABLE_H

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> 
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// FITSTimedTable is used to look at FITS tables which have a time column. In
// particular, it peeks ahead, and knows the time of the currentRow and of the
// nextRow.
//
// It is constructed with a pointer to any FITSTabular. Presently, no memory 
// management is imposed to ensure that the pointer remains valid.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1995/06/01">
//   <li> 
// </todo>

class FITSTimedTable :  public FITSTabular
{
public:
    // This is not connected to any data, isValid always returns True,
    // keywords and description return the default versions
    // hasChanged returns False, name returns an empty string
    // pastEnd returns False and next does nothing.
    // setTime does nothing, currentRow returns an empty record
    // and currentTime returns 0.0
    // and ok returns True and nextTime returns 0.0
    FITSTimedTable();
    // Note, originalTable cannot be destructed, reopened, ...,during the
    // lifetime of this object.
    FITSTimedTable(FITSTabular *originalTable, uInt whichColumnIsTime=0);
    ~FITSTimedTable();

    virtual Bool isValid() const;
    virtual const TableRecord &keywords() const;
    virtual const RecordDesc &description() const;
    virtual const Record &units() const;
    virtual const Record &displayFormats() const;
    virtual const Record &nulls() const;

    virtual Bool hasChanged() const { return hasChanged_p;}
    virtual void resetChangedFlag() { hasChanged_p = False; }
    virtual const String &name() const { return table_p->name(); }
    virtual Bool pastEnd() const;
    virtual Bool pastEnd();
    virtual void next();
    // interpolate to the desired time which must be >= the currentTime()
    // This uses a linear interpolation between adjacent floating point values.
    // Non-floating point values are NOT interpolated but have the value of the
    // most recent actual row.  On the last row of the table, not interpolation
    // is done.
    virtual void setTime(Double time);
    virtual const Record &currentRow() const;
    virtual Record &currentRow();

    // What is the time of the current row?
    Double currentTime() const;

    // this is True if the last setTime() finished as expected
    // It is False only if the requested time is before the current time
    // and the timed table as just been opened
    Bool ok() const { return ok_p;}

    // What will the time of the next row be? Returns a very large number if
    // it is past the end of the table.
    Double nextTime();
private:
    Bool atStart_p;
    Bool ok_p;
    Bool hasChanged_p;
    Bool changePending_p;
    FITSTabular *table_p;
    Record *row_now_p;
    Record *row_next_p;
    RORecordFieldPtr<Double> time_now_p;
    RORecordFieldPtr<Double> time_next_p;
    RecordDesc rowDesc_p;
    Int how_past_end_p;
    uInt timeColumn_p;
    TableRecord dummyKeywords;
    Record dummyUnits;
    Record dummyDisps;
    Record dummyNulls;

    void initNowRecord(const RecordDesc& desc);
    void initNextRecord(const RecordDesc& desc);
};

} //# NAMESPACE CASACORE - END

#endif
