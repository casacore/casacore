//# MSReader.h: read from a MS, coordinating all of the subtables
//# Copyright (C) 2000
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

#ifndef MS_MSREADER_H
#define MS_MSREADER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSSel/MSDopplerIndex.h>
#include <casacore/ms/MSSel/MSFeedIndex.h>
#include <casacore/ms/MSSel/MSFreqOffIndex.h>
#include <casacore/ms/MSSel/MSPointingIndex.h>
#include <casacore/ms/MSSel/MSSourceIndex.h>
#include <casacore/ms/MSSel/MSSysCalIndex.h>
#include <casacore/ms/MSSel/MSTableIndex.h>
#include <casacore/ms/MSSel/MSWeatherIndex.h>
#include <casacore/ms/MSOper/MSValidIds.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Read from an MS, coordinating all of the subtables in the process
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
class MSReader
{
public:
    // Attach to the indicated MeasurementSet
    MSReader(const MeasurementSet &ms);

    ~MSReader() {;}

    // Go to the indicated row in the MAIN table of the MS and point
    // at all of the appropriate rows in each of the subtables as
    // a result of going to this row.
    void gotoRow(uInt which);

    const Vector<String> &tables() const {return itsTableNames;}

    // Return the current row in the named table. Use rowNumber to
    // check to see that the most recent gotoRow actually found a matching 
    // row.
    const RecordInterface &tableRow(const String &name) const;

    // Return the current row number in the named table.  This returns
    // -1 if that table has no row as a result of the most recent gotoRow.
    Int rowNumber(const String &name) const;

    // Return a reference to the MS
    const MeasurementSet &ms() const {return itsMS;}

    // Return a reference to the named subtable
    const Table &table(const String &name) const;

    // this isn't what we need, right now just return an empty record
    const Record &units(const String &) const { return emptyRecord;}
private:
    MeasurementSet itsMS;
    ROMSColumns itsMSCols;

    // This possibly saves some time, Units of seconds
    Unit itsSecUnit;

    MSValidIds itsIds;

    // this maps table name to an index used throughout this class
    SimpleOrderedMap<String, Int> itsTabId;

    // the indexes for the NS subtables
    Block<MSTableIndex> itsIndexes;

    // specific indexes 
    MSDopplerIndex itsDopplerIndex;
    MSFeedIndex itsFeed1Index;
    MSFeedIndex itsFeed2Index;
    MSFreqOffIndex itsFreqOffIndex;
    MSPointingIndex itsPointing1Index;
    MSPointingIndex itsPointing2Index;
    MSSourceIndex itsSourceIndex;
    MSSysCalIndex itsSyscal1Index;
    MSSysCalIndex itsSyscal2Index;
    MSWeatherIndex itsWeather1Index;
    MSWeatherIndex itsWeather2Index;

    // table IDs for the standard tables
    Int itsMainId, itsAnt1Id, itsAnt2Id, itsDDId, itsDopplerId, itsFeed1Id, itsFeed2Id, itsFieldId,
	itsFlagCmdId, itsFreqOffsetId, itsObsId, itsPointing1Id, itsPointing2Id, itsPolId, itsProcId, 
	itsSourceId, itsSpwId, itsStateId, itsSyscal1Id, itsSyscal2Id, itsWeather1Id, itsWeather2Id;
    // the table rows
    Block<ROTableRow> itsTabRows;

    // What row number for each table is the most recent gotoRow call.  Set to
    // -1 if there was no matching row as a result of that call.
    Block<Int> itsRowNumbers;

    // this empty record is returned by tableRow when the name argument does not exist
    Record emptyRecord;

    // this empty table is returned by table when the name argument does not exist
    Table emptyTable;

    Vector<String> itsTableNames;

    // undefined and unavailable
    MSReader();
    MSReader(const MSReader &);
    MSReader& operator=(const MSReader &);
};


} //# NAMESPACE CASACORE - END

#endif

