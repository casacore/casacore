//# NewMSPolarization.cc: The NewMeasurementSet POLARIZATION Table
//# Copyright (C) 1999,2000
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

#include <aips/MeasurementSets/NewMSPolarization.h>
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

NewMSPolarization::NewMSPolarization():hasBeenDestroyed_p(True) { }

NewMSPolarization::NewMSPolarization(const String &tableName, TableOption option) 
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSPolarization(String &, TableOption) - "
			 "table is not a valid NewMSPolarization"));
}

NewMSPolarization::NewMSPolarization(const String& tableName, const String &tableDescName,
			       TableOption option)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSPolarization(String &, String &, TableOption) - "
			 "table is not a valid NewMSPolarization"));
}

NewMSPolarization::NewMSPolarization(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSPolarization(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid NewMSPolarization"));
}

NewMSPolarization::NewMSPolarization(const Table &table)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSPolarization(const Table &) - "
			 "table is not a valid NewMSPolarization"));
}

NewMSPolarization::NewMSPolarization(const NewMSPolarization &other)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("NewMSPolarization(const NewMSPolarization &) - "
			     "table is not a valid NewMSPolarization"));
}

NewMSPolarization::~NewMSPolarization()
{
// check to make sure that this NewMSPolarization is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~NewMSPolarization() - "
			 "Table written is not a valid NewMSPolarization"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


NewMSPolarization& NewMSPolarization::operator=(const NewMSPolarization &other)
{
    if (&other != this) {
	NewMSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void NewMSPolarization::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// CORR_PRODUCT
	colMapDef(CORR_PRODUCT, "CORR_PRODUCT", TpArrayInt,
		  "Indices describing receptors of feed going into correlation","","");
	// CORR_TYPE
	colMapDef(CORR_TYPE, "CORR_TYPE", TpArrayInt,
		  "The polarization type for each correlation product,"
		  " as a Stokes enum.","","");
	// FLAG_ROW
	colMapDef(FLAG_ROW, "FLAG_ROW", TpBool,
		  "Row flag","","");
	// NUM_CORR
	colMapDef(NUM_CORR, "NUM_CORR", TpInt,
		  "Number of correlation products","","");
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
	addColumnToDesc(requiredTD, CORR_TYPE, 1);
	addColumnToDesc(requiredTD, CORR_PRODUCT, 2);
	// Now define all other columns (duplicates are skipped)
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
NewMSPolarization NewMSPolarization::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return NewMSPolarization(NewMSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}
