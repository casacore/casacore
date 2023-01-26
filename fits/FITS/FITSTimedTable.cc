//# FITSTimedTable.cc: A Table with a time column
//# Copyright (C) 1995,1996,1997,2001
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

#include <casacore/fits/FITS/FITSTimedTable.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

FITSTimedTable::FITSTimedTable()
    : atStart_p(true), hasChanged_p(false), changePending_p(false), 
      table_p(0), row_now_p(0), row_next_p(0), how_past_end_p(0),
      timeColumn_p(0)
{
    rowDesc_p.addField("Time", TpDouble);
    row_now_p = new Record(rowDesc_p);
    row_next_p = new Record(rowDesc_p);
    AlwaysAssert(row_now_p && row_next_p, AipsError);

    RecordFieldPtr<double> time_now(*row_now_p,"Time");
    *time_now = 0.0;
    RecordFieldPtr<double> time_next(*row_next_p,"Time");
    *time_next = 0.0;

    time_now_p.attachToRecord(*row_now_p, 0);
    time_next_p.attachToRecord(*row_next_p, 0);
    ok_p = true;
}

FITSTimedTable::FITSTimedTable(FITSTabular *originalTable, 
			       uint32_t whichColumnIsTime)
    : atStart_p(true), hasChanged_p(false), changePending_p(false), 
      table_p(originalTable), row_now_p(0), row_next_p(0), 
      rowDesc_p(table_p->description()), how_past_end_p(0), 
      timeColumn_p(whichColumnIsTime)
{
    AlwaysAssert(table_p, AipsError);

    if (!table_p->isValid()) {
      // as if this had been created with the default constructor
      table_p = 0;
      RecordDesc tmp;
      rowDesc_p = tmp;
      rowDesc_p.addField("Time", TpDouble);
      row_now_p = new Record(rowDesc_p);
      row_next_p = new Record(rowDesc_p);
      AlwaysAssert(row_now_p && row_next_p, AipsError);
      RecordFieldPtr<double> time_now(*row_now_p,"Time");
      *time_now = 0.0;
      RecordFieldPtr<double> time_next(*row_next_p,"Time");
      *time_next = 0.0;
      time_now_p.attachToRecord(*row_now_p, 0);
      time_next_p.attachToRecord(*row_next_p, 0);
      ok_p = true;
      timeColumn_p = 0;
    } else {
      initNowRecord(rowDesc_p);
      *row_now_p = table_p->currentRow();

      table_p->next();
      if (table_p->hasChanged()) {
	changePending_p = true;
	table_p->resetChangedFlag();
      }
      initNextRecord(table_p->description());
      *row_next_p = table_p->currentRow();
    }
}

FITSTimedTable::~FITSTimedTable()
{
    if (row_now_p) delete row_now_p;
    row_now_p = 0;
    if (row_next_p) delete row_next_p;
    row_next_p = 0;
}

bool FITSTimedTable::isValid() const
{
    if (!table_p) return true;
    return table_p->isValid();
}

const TableRecord &FITSTimedTable::keywords() const
{
    if (!table_p) return dummyKeywords;
    return table_p->keywords();
}

const Record &FITSTimedTable::units() const
{
    if (!table_p) return dummyUnits;
    return table_p->units();
}

const Record &FITSTimedTable::displayFormats() const
{
    if (!table_p) return dummyDisps;
    return table_p->displayFormats();
}

const Record &FITSTimedTable::nulls() const
{
    if (!table_p) return dummyNulls;
    return table_p->nulls();
}

const RecordDesc &FITSTimedTable::description() const
{
    return rowDesc_p;
}

bool FITSTimedTable::pastEnd() const
{
  // the constant version can not try and resync with underlying table
    return (!table_p || table_p->pastEnd());
}

bool FITSTimedTable::pastEnd()
{
    // if how_past_end_p indicates we've been past the end
    // but table_p->pastEnd() is false, resyncronize with table
    if (!table_p) return true;
    if (how_past_end_p && !table_p->pastEnd()) {
	how_past_end_p = 0;
	*row_now_p = table_p->currentRow();
	table_p->next();
	if (table_p->hasChanged()) {
	    rowDesc_p = table_p->description();
	    initNowRecord(rowDesc_p);
	    initNextRecord(rowDesc_p);
	    *row_now_p = table_p->currentRow();
	    hasChanged_p = true;
	    changePending_p = false;
	    table_p->resetChangedFlag();
	}
	*row_next_p = table_p->currentRow();
    }

    return (how_past_end_p > 1);
}

void FITSTimedTable::next()
{
    if (!table_p) return;

    if (changePending_p) {
	rowDesc_p = table_p->description();
	initNowRecord(rowDesc_p);
	changePending_p = false;
	hasChanged_p = true;
    }
    *row_now_p = table_p->currentRow();
    //    String curName = name();
    table_p->next();
    if (table_p->hasChanged()) {
	initNextRecord(table_p->description());
	changePending_p = true;
	table_p->resetChangedFlag();
    }
    *row_next_p = table_p->currentRow();

    // if nextTime() < currentTime() advance until that isn't true or
    // end of file is reached
    //    int32_t count = 0;
    while (!table_p->pastEnd() && (nextTime() < currentTime())) {
	table_p->next();
	if (table_p->hasChanged()) {
	    initNextRecord(table_p->description());
	    changePending_p = true;
	    table_p->resetChangedFlag();
	}
	*row_next_p = table_p->currentRow();
	//	if (curName != name()) {
	//	    cout << "File changed from " << curName << " to " << name() << endl;
	//	    curName = name();
	//	}
	//	count++;
    }
    //    if (count != 0) cout << "Skipped " << count << " rows" << endl;
    if (nextTime() < currentTime()) {
	*row_next_p = *row_now_p;
	how_past_end_p++;
	//	cout << "Still bad time" << endl;
    } else if (table_p->pastEnd()) {
	if (how_past_end_p == 0) {
	    *row_now_p = table_p->currentRow();
	}
	how_past_end_p++;
    }

    atStart_p = false;
    ok_p = true;
}

const Record &FITSTimedTable::currentRow() const
{
    return *row_now_p;
}

Record &FITSTimedTable::currentRow()
{
    return *row_now_p;
}

double FITSTimedTable::currentTime() const
{
    return *time_now_p;
}

double FITSTimedTable::nextTime()
{
    if (pastEnd() || how_past_end_p > 0) {
	return 1.0e+30;
    }
    return *time_next_p;
}

void FITSTimedTable::setTime(double time)
{
    if (!table_p) return;
    // time MUST be >= currentTime() unless this is the first row
    // in which case simply give the current row
    // and set ok flag to false
    
    if (time < currentTime()) {
	if (atStart_p) {
	    ok_p = false;
	    return;
	} else {
	    // just write out error message to cerr for now
	    cerr.precision(12);
	    cerr << "File : " << name() << endl;
	    cerr << "requested time " << time << endl;
	    cerr << "currentTime() " << currentTime() << endl;
	    throw(AipsError("FITSTimedTable::setTime(double time) - time is "
			"< currentTime() - can not currently back up in time."
			    "File : " + name()));
	}
    }

    ok_p = true;
    atStart_p = false;

    // step through file until end is reached or
    // time is >= currentTime and < nextTime
    while (! pastEnd() && time >= nextTime()) next();

    // simply return if pastEnd() or time == currentTime()
    if (pastEnd() || time == currentTime()) return;

    double fraction = (time - currentTime()) / (nextTime() - currentTime());

    // for now, we just interpolate TpFloat, TpDouble and TpArrayFloat and TpArrayDouble fields
    // But only when fraction is greater than 0.001
    // This clearly needs a better solution
    // watch for columns in row_now that are missing in row_next
    uint32_t nextNr;
    if (fraction > 0.001) {
	for (uint32_t i=0;i<rowDesc_p.nfields();i++) {
	    if (changePending_p) {
		nextNr = row_next_p->fieldNumber(rowDesc_p.name(i));
	    } else {
		nextNr = i;
	    }
	    switch (rowDesc_p.type(i)) {
	    case TpFloat:
	    {
		RecordFieldPtr<float> currField(*row_now_p, i);
		RecordFieldPtr<float> nextField(*row_next_p, nextNr);
		*currField = (*nextField-*currField) * fraction + *currField;
	    }
	    break;
	    case TpDouble:
	    {    
		RecordFieldPtr<double> currField(*row_now_p, i);
		RecordFieldPtr<double> nextField(*row_next_p, nextNr);
		*currField = (*nextField-*currField) * fraction + *currField;
	    }
	    break;
	    case TpArrayFloat:
	    {    
		RecordFieldPtr<Array<float> > currField(*row_now_p, i);
		RecordFieldPtr<Array<float> > nextField(*row_next_p, nextNr);
		*currField = (*nextField-*currField) * float(fraction) 
		    + *currField;
	    }
	    break;
	    case TpArrayDouble:
	    {    
		RecordFieldPtr<Array<double> > currField(*row_now_p, i);
		RecordFieldPtr<Array<double> > nextField(*row_next_p, nextNr);
		*currField = (*nextField-*currField) * fraction + *currField;
	    }
	    break;
	    default:
		// do nothing for all other types (including Complex and DComplex)
		;
		break;
	    }
	}
    }
}

void FITSTimedTable::initNowRecord(const RecordDesc& desc)
{
    if (row_now_p) delete row_now_p;
    row_now_p = new Record(desc);
    AlwaysAssert(row_now_p, AipsError);
    time_now_p.attachToRecord(*row_now_p, timeColumn_p);
}

void FITSTimedTable::initNextRecord(const RecordDesc& desc)
{
    if (row_next_p) delete row_next_p;
    row_next_p = new Record(desc);
    AlwaysAssert(row_next_p, AipsError);
    time_next_p.attachToRecord(*row_next_p, timeColumn_p);
}    

} //# NAMESPACE CASACORE - END

