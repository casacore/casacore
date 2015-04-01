//# MSDoppler.cc: The MeasurementSet DOPPLER Table
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

#include <casacore/ms/MeasurementSets/MSDoppler.h>

#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSDoppler::MSDoppler():hasBeenDestroyed_p(True) { }

MSDoppler::MSDoppler(const String &tableName, 
				     TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    addVelDef();
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDoppler(String &, TableOption) - "
			 "table is not a valid MSDoppler"));
}

void MSDoppler::addVelDef()
{
  // For a transition period: add the VELDEF column
  // silently if it is not there - 2000/09/12, remove next MS update.
  if (tableDesc().isColumn(columnName(TRANSITION_ID))) {
    // we probably have a MSDoppler table
    if (!tableDesc().isColumn(columnName(VELDEF))) {
      if (!isWritable()) {
	throw (AipsError("Missing VELDEF column in MSDoppler table -"
			"please open MS table R/W to have it added"));
      } else {
	TableDesc td; 
	addColumnToDesc(td,VELDEF);
	addColumn(td[0]);
	ScalarColumn<Double> velDef(*this,columnName(VELDEF));
	velDef.fillColumn(0);
      }
    }
  }
}

MSDoppler::MSDoppler(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    addVelDef();
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDoppler(String &, String &, TableOption) - "
			 "table is not a valid MSDoppler"));
}

MSDoppler::MSDoppler(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    addVelDef();
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDoppler(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSDoppler"));
}

MSDoppler::MSDoppler(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    addVelDef();
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDoppler(const Table &) - "
			 "table is not a valid MSDoppler"));
}

MSDoppler::MSDoppler(const MSDoppler &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
        addVelDef();
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSDoppler(const MSDoppler &) - "
			     "table is not a valid MSDoppler"));
}

MSDoppler::~MSDoppler()
{
// check to make sure that this MSDoppler is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSDoppler() - "
			 "Table written is not a valid MSDoppler"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSDoppler& MSDoppler::operator=(const MSDoppler &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSDoppler::init()
{
    if (! columnMap_p.ndefined()) {
        // the PredefinedColumns
        // 
	colMapDef(DOPPLER_ID,"DOPPLER_ID", TpInt,
		  "Doppler tracking id","","");
	// SOURCE_ID
	colMapDef(SOURCE_ID, "SOURCE_ID", TpInt,
		  "Pointer to SOURCE table","","");
	// TRANSITION_ID
	colMapDef(TRANSITION_ID,"TRANSITION_ID",TpInt,
		  "Pointer to list of transitions in SOURCE table","","");
	// VELDEF
	colMapDef(VELDEF, "VELDEF", TpDouble, 
		  "Velocity Definition for Doppler shift","m/s","Doppler");
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

	
MSDoppler MSDoppler::referenceCopy(const String& newTableName, 
		    const Block<String>& writableColumns) const
{
    return MSDoppler(MSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

