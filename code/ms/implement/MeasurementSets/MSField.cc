//# NewMSField.cc: The NewMeasurementSet FIELD Table
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

#include <aips/MeasurementSets/NewMSField.h>
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

NewMSField::NewMSField():hasBeenDestroyed_p(True) { }

NewMSField::NewMSField(const String &tableName, TableOption option) 
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSField(String &, TableOption) - "
			 "table is not a valid NewMSField"));
}

NewMSField::NewMSField(const String& tableName, const String &tableDescName,
			       TableOption option)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSField(String &, String &, TableOption) - "
			 "table is not a valid NewMSField"));
}

NewMSField::NewMSField(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSField(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid NewMSField"));
}

NewMSField::NewMSField(const Table &table)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSField(const Table &) - "
			 "table is not a valid NewMSField"));
}

NewMSField::NewMSField(const NewMSField &other)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("NewMSField(const NewMSField &) - "
			     "table is not a valid NewMSField"));
}

NewMSField::~NewMSField()
{
// check to make sure that this NewMSField is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~NewMSField() - "
			 "Table written is not a valid NewMSField"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


NewMSField& NewMSField::operator=(const NewMSField &other)
{
    if (&other != this) {
	NewMSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void NewMSField::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// CODE
	colMapDef(CODE, "CODE", TpString,
		  "Special characteristics of field, "
		  "e.g. Bandpass calibrator","","");
	// DELAY_DIR
	colMapDef(DELAY_DIR, "DELAY_DIR", TpArrayDouble,
		  "Direction of delay center (e.g. RA, DEC)" 
		  "as polynomial in time.","rad","Direction");
	// EPHEMERIS_ID
	colMapDef(EPHEMERIS_ID,"EPHEMERIS_ID", TpInt,
		  "Ephemeris id, pointer to EPHEMERIS table","","");
	// FLAG_ROW
	colMapDef(FLAG_ROW, "FLAG_ROW", TpBool,
		  "Row Flag","","");
	// NAME
	colMapDef(NAME, "NAME", TpString,
		  "Name of this field","","");
	// NUM_POLY
	colMapDef(NUM_POLY, "NUM_POLY", TpInt,
		  "Polynomial order of _DIR columns","","");
	// PHASE_DIR 
	colMapDef(PHASE_DIR, "PHASE_DIR", TpArrayDouble,
		  "Direction of phase center (e.g. RA, DEC).",
		  "rad","Direction");
	// REFERENCE_DIR 
	colMapDef(REFERENCE_DIR, "REFERENCE_DIR", TpArrayDouble,
		  "Direction of REFERENCE center (e.g. RA, DEC)."
		  "as polynomial in time.","rad","Direction");
	// SOURCE_ID
	colMapDef(SOURCE_ID, "SOURCE_ID", TpInt,
		  "Source id","","");
	// TIME
	colMapDef(TIME, "TIME", TpDouble,
		  "Time origin for direction and rate","s","Epoch");

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
	// First define the columns with known dimensionality
	addColumnToDesc(requiredTD, DELAY_DIR, 2);
	addColumnToDesc(requiredTD, PHASE_DIR, 2);
	addColumnToDesc(requiredTD, REFERENCE_DIR, 2);
	// Now define all other columns (duplicates are skipped)
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
NewMSField NewMSField::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return NewMSField(NewMSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}
