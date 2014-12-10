//# MSAntenna.cc: The MeasurementSet ANTENNA Table
//# Copyright (C) 1996,1998,2000
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

#include <casacore/ms/MeasurementSets/MSAntenna.h>

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

MSAntenna::MSAntenna():hasBeenDestroyed_p(True) { }

MSAntenna::MSAntenna(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSAntenna(String &, TableOption) - "
			 "table is not a valid MSAntenna"));
}

MSAntenna::MSAntenna(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSAntenna(String &, String &, TableOption) - "
			 "table is not a valid MSAntenna"));
}

MSAntenna::MSAntenna(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSAntenna(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSAntenna"));
}

MSAntenna::MSAntenna(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSAntenna(const Table &) - "
			 "table is not a valid MSAntenna"));
}

MSAntenna::MSAntenna(const MSAntenna &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSAntenna(const MSAntenna &) - "
			     "table is not a valid MSAntenna"));
}

MSAntenna::~MSAntenna()
{
// check to make sure that this MSAntenna is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSAntenna() - "
			 "Table written is not a valid MSAntenna"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSAntenna& MSAntenna::operator=(const MSAntenna &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSAntenna::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// DISH_DIAMETER
	colMapDef(DISH_DIAMETER, "DISH_DIAMETER", TpDouble,
		  "Physical diameter of dish","m","");
	// FLAG_ROW
	colMapDef(FLAG_ROW,"FLAG_ROW",TpBool,
		  "Flag for this row","","");
	// MOUNT
	colMapDef(MOUNT,"MOUNT",TpString,
		  "Mount type e.g. alt-az, equatorial, etc.","","");
	// NAME
	colMapDef(NAME,"NAME",TpString,
		  "Antenna name, e.g. VLA22, CA03","","");
	// OFFSET
	colMapDef(OFFSET,"OFFSET",TpArrayDouble,
		  "Axes offset of mount to FEED REFERENCE point",
		  "m","Position");
	// POSITION
	colMapDef(POSITION,"POSITION",TpArrayDouble,
		  "Antenna X,Y,Z phase reference position","m","Position");
	// STATION
	colMapDef(STATION,"STATION",TpString,
		  "Station (antenna pad) name","","");
	// TYPE
	colMapDef(TYPE,"TYPE", TpString,
		  "Antenna type (e.g. SPACE-BASED)","","");

	// Optional columns follow 
	// MEAN_ORBIT
	colMapDef(MEAN_ORBIT,"MEAN_ORBIT",TpArrayDouble,
		  "Mean Keplerian elements","","");
	// ORBIT_ID
	colMapDef(ORBIT_ID,"ORBIT_ID",TpInt,
		  "index into ORBIT table (ignore if<0)","","");
	// PHASED_ARRAY_ID
	colMapDef(PHASED_ARRAY_ID,"PHASED_ARRAY_ID",TpInt,
		  "index into PHASED_ARRAY table","","");
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
	// First define the columns with fixed size arrays
	IPosition shape(1,3);
	ColumnDesc::Option option=ColumnDesc::Direct;
	addColumnToDesc(requiredTD, OFFSET, shape, option);
	addColumnToDesc(requiredTD, POSITION, shape, option);
	// Now define all other columns (duplicates are skipped)
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
MSAntenna MSAntenna::referenceCopy(const String& newTableName, 
		    const Block<String>& writableColumns) const
{
    return MSAntenna(MSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

