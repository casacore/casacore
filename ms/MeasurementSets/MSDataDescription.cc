//# MSDataDescription.cc: The MeasurementSet DATA_DESCRIPTION Table
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

#include <casacore/ms/MeasurementSets/MSDataDescription.h>

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

MSDataDescription::MSDataDescription():hasBeenDestroyed_p(True) { }

MSDataDescription::MSDataDescription(const String &tableName, 
				     TableOption option) 
    : MSTable<MSDataDescriptionEnums>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDataDescription(String &, TableOption) - "
			 "table is not a valid MSDataDescription"));
}

MSDataDescription::MSDataDescription(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSDataDescriptionEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDataDescription(String &, String &, TableOption) - "
			 "table is not a valid MSDataDescription"));
}

MSDataDescription::MSDataDescription(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<MSDataDescriptionEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDataDescription(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSDataDescription"));
}

MSDataDescription::MSDataDescription(const Table &table)
    : MSTable<MSDataDescriptionEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSDataDescription(const Table &) - "
			 "table is not a valid MSDataDescription"));
}

MSDataDescription::MSDataDescription(const MSDataDescription &other)
    : MSTable<MSDataDescriptionEnums>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSDataDescription(const MSDataDescription &) - "
			     "table is not a valid MSDataDescription"));
}

MSDataDescription::~MSDataDescription()
{
// check to make sure that this MSDataDescription is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSDataDescription() - Table written is not a valid MSDataDescription"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSDataDescription& MSDataDescription::operator=(const MSDataDescription &other)
{
    if (&other != this) {
	MSTable<MSDataDescriptionEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSDataDescription::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // FLAG_ROW
  colMapDef(maps, FLAG_ROW,"FLAG_ROW", TpBool,
            "Flag this row","","");
  // LAG_ID
  colMapDef(maps, LAG_ID,"LAG_ID",TpInt,"The lag index","","");
  // POLARIZATION_ID
  colMapDef(maps, POLARIZATION_ID,"POLARIZATION_ID",TpInt,
            "Pointer to polarization table","","");
  // SPECTRAL_WINDOW_ID
  colMapDef(maps, SPECTRAL_WINDOW_ID, "SPECTRAL_WINDOW_ID", TpInt,
            "Pointer to spectralwindow table","","");

  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSDataDescription MSDataDescription::referenceCopy(const String& newTableName, 
		    const Block<String>& writableColumns) const
{
  return MSDataDescription(MSTable<MSDataDescriptionEnums>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

