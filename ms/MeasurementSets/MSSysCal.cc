//# MSSysCal.cc: The MeasurementSet SYSCAL Table
//# Copyright (C) 1996,1998,1999,2000
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
//# $Id$

#include <casacore/ms/MeasurementSets/MSSysCal.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// set hasBeenDestroyed to True to avoid validity check in destructor.
MSSysCal::MSSysCal():hasBeenDestroyed_p(True) { }

MSSysCal::MSSysCal(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(String &, TableOption) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(String &, String &, TableOption) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(const Table &) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(const MSSysCal &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSSysCal(const MSSysCal &) - "
			     "table is not a valid MSSysCal"));
}

MSSysCal::~MSSysCal()
{
// check to make sure that this MSSysCal is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSSysCal() - "
			 "Table written is not a valid MSSysCal"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSSysCal& MSSysCal::operator=(const MSSysCal &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSSysCal::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// ANTENNA_ID
	colMapDef(ANTENNA_ID, "ANTENNA_ID", TpInt,
		  "ID of antenna in this array","","");
	// FEED_ID
	colMapDef(FEED_ID,"FEED_ID",TpInt,
		  "Feed id","","");
	// INTERVAL
	colMapDef(INTERVAL,"INTERVAL",TpDouble,
		  "Interval for which this set of parameters is accurate",
		  "s","");
	// SPECTRAL_WINDOW_ID
	colMapDef(SPECTRAL_WINDOW_ID,"SPECTRAL_WINDOW_ID",TpInt,
		  "ID for this spectral window setup","","");
	// TIME
	colMapDef(TIME,"TIME",TpDouble,
		  "Midpoint of time for which this set of "
		  "parameters is accurate","s","Epoch");
	// PHASE_DIFF
	colMapDef(PHASE_DIFF,"PHASE_DIFF",TpFloat,
		  "Phase difference between receptor 2 and receptor 1",
		  "rad","");
	// PHASE_DIFF_FLAG
	colMapDef(PHASE_DIFF_FLAG,"PHASE_DIFF_FLAG",TpBool,
		  "Flag for PHASE_DIFF","","");
	// TANT
	colMapDef(TANT,"TANT",TpArrayFloat,
		  "Antenna temperature for each receptor","K","");
	// TANT_FLAG
	colMapDef(TANT_FLAG,"TANT_FLAG",TpBool,
		  "Flag for TANT","","");
	// TANT_SPECTRUM
	colMapDef(TANT_SPECTRUM,"TANT_SPECTRUM",TpArrayFloat,
		  "Antenna temperature for each channel and receptor","K","");
	// TANT_TSYS
	colMapDef(TANT_TSYS,"TANT_TSYS",TpArrayFloat,
		  "Ratio of Antenna & system temperature for each receptor",
		  "","");
	// TANT_TSYS_FLAG
	colMapDef(TANT_TSYS_FLAG,"TANT_TSYS_FLAG",TpBool,
		  "Flag for TANT_TSYS","","");
	// TANT_TSYS_SPECTRUM
	colMapDef(TANT_TSYS_SPECTRUM,"TANT_TSYS_SPECTRUM",TpArrayFloat,
		  "Ratio of Antenna & system temperature for each channel "
		  "and receptor","","");
	// TCAL
	colMapDef(TCAL,"TCAL",TpArrayFloat,
		  "Calibration temperature for each receptor","K","");
	// TCAL_FLAG
	colMapDef(TCAL_FLAG,"TCAL_FLAG",TpBool,
		  "Flag for TCAL","","");
	// TCAL_SPECTRUM
	colMapDef(TCAL_SPECTRUM,"TCAL_SPECTRUM",TpArrayFloat,
		  "Calibration temperature for each channel and receptor","K","");
	// TRX 
	colMapDef(TRX,"TRX",TpArrayFloat,
		  "Receiver temperature for each of the two receptors","K","");
	// TRX_FLAG
	colMapDef(TRX_FLAG,"TRX_FLAG",TpBool,
		  "Flag for TRX","","");
	// TRX_SPECTRUM
	colMapDef(TRX_SPECTRUM,"TRX_SPECTRUM",TpArrayFloat,
		  "Receiver temperature for each channel and receptor","K","");
	// TSKY 
	colMapDef(TSKY,"TSKY",TpArrayFloat,
		  "Sky temperature for each of the two receptors","K","");
	// TSKY_FLAG
	colMapDef(TSKY_FLAG,"TSKY_FLAG",TpBool,
		  "Flag for TSKY","","");
	// TSKY_SPECTRUM
	colMapDef(TSKY_SPECTRUM,"TSKY_SPECTRUM",TpArrayFloat,
		  "Sky temperature for each channel and receptor","K","");
	// TSYS
	colMapDef(TSYS,"TSYS",TpArrayFloat,
		  "System temp. for each of the two receptors","K","");
	// TSYS_FLAG
	colMapDef(TSYS_FLAG,"TSYS_FLAG",TpBool,
		  "Flag for TSYS","","");
	// TSYS_SPECTRUM
	colMapDef(TSYS_SPECTRUM,"TSYS_SPECTRUM",TpArrayFloat,
		  "System temperature for each channel and receptor","K","");

	// PredefinedKeywords

	// init requiredTableDesc
	TableDesc requiredTD;
	// all required keywords
	uInt i;
	for (i = UNDEFINED_KEYWORD+1;
	     i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
	    addKeyToDesc(requiredTD, PredefinedKeywords(i));
	}
	
	// all required columns 
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

MSSysCal MSSysCal::referenceCopy(const String& newTableName, 
				 const Block<String>& writableColumns) const
{
    return MSSysCal(MSTable<PredefinedColumns,PredefinedKeywords>::referenceCopy
		    (newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

