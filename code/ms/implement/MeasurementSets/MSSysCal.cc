//# NewMSSysCal.cc: The NewMeasurementSet SYSCAL Table
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

#include <aips/MeasurementSets/NewMSSysCal.h>
#include <aips/Utilities/String.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/ForwardCol.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>

// set hasBeenDestroyed to True to avoid validity check in destructor.
NewMSSysCal::NewMSSysCal():hasBeenDestroyed_p(True) { }

NewMSSysCal::NewMSSysCal(const String &tableName, TableOption option) 
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSSysCal(String &, TableOption) - "
			 "table is not a valid NewMSSysCal"));
}

NewMSSysCal::NewMSSysCal(const String& tableName, const String &tableDescName,
			       TableOption option)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSSysCal(String &, String &, TableOption) - "
			 "table is not a valid NewMSSysCal"));
}

NewMSSysCal::NewMSSysCal(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSSysCal(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid NewMSSysCal"));
}

NewMSSysCal::NewMSSysCal(const Table &table)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSSysCal(const Table &) - "
			 "table is not a valid NewMSSysCal"));
}

NewMSSysCal::NewMSSysCal(const NewMSSysCal &other)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("NewMSSysCal(const NewMSSysCal &) - "
			     "table is not a valid NewMSSysCal"));
}

NewMSSysCal::~NewMSSysCal()
{
// check to make sure that this NewMSSysCal is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~NewMSSysCal() - "
			 "Table written is not a valid NewMSSysCal"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


NewMSSysCal& NewMSSysCal::operator=(const NewMSSysCal &other)
{
    if (&other != this) {
	NewMSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void NewMSSysCal::init()
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

NewMSSysCal NewMSSysCal::referenceCopy(const String& newTableName, 
				 const Block<String>& writableColumns) const
{
    return NewMSSysCal(NewMSTable<PredefinedColumns,PredefinedKeywords>::referenceCopy
		    (newTableName,writableColumns));
}
