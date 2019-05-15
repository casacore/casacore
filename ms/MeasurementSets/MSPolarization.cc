//# MSPolarization.cc: The MeasurementSet POLARIZATION Table
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

#include <casacore/ms/MeasurementSets/MSPolarization.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSPolarization::MSPolarization():hasBeenDestroyed_p(True) { }

MSPolarization::MSPolarization(const String &tableName, TableOption option) 
    : MSTable<MSPolarizationEnums>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPolarization(String &, TableOption) - "
			 "table is not a valid MSPolarization"));
}

MSPolarization::MSPolarization(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSPolarizationEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPolarization(String &, String &, TableOption) - "
			 "table is not a valid MSPolarization"));
}

MSPolarization::MSPolarization(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<MSPolarizationEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPolarization(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSPolarization"));
}

MSPolarization::MSPolarization(const Table &table)
    : MSTable<MSPolarizationEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPolarization(const Table &) - "
			 "table is not a valid MSPolarization"));
}

MSPolarization::MSPolarization(const MSPolarization &other)
    : MSTable<MSPolarizationEnums>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSPolarization(const MSPolarization &) - "
			     "table is not a valid MSPolarization"));
}

MSPolarization::~MSPolarization()
{
// check to make sure that this MSPolarization is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSPolarization() - Table written is not a valid MSPolarization"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSPolarization& MSPolarization::operator=(const MSPolarization &other)
{
    if (&other != this) {
	MSTable<MSPolarizationEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSPolarization::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // CORR_PRODUCT
  colMapDef(maps, CORR_PRODUCT, "CORR_PRODUCT", TpArrayInt,
            "Indices describing receptors of feed going into correlation","","");
  // CORR_TYPE
  colMapDef(maps, CORR_TYPE, "CORR_TYPE", TpArrayInt,
            "The polarization type for each correlation product,"
            " as a Stokes enum.","","");
  // FLAG_ROW
  colMapDef(maps, FLAG_ROW, "FLAG_ROW", TpBool,
            "Row flag","","");
  // NUM_CORR
  colMapDef(maps, NUM_CORR, "NUM_CORR", TpInt,
            "Number of correlation products","","");

  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  // First define the columns with known dimensionality
  addColumnToDesc(maps, CORR_TYPE, 1);
  addColumnToDesc(maps, CORR_PRODUCT, 2);
  // Now define all other columns (duplicates are skipped)
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSPolarization MSPolarization::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MSPolarization(MSTable<MSPolarizationEnums>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

