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
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// set hasBeenDestroyed to True to avoid validity check in destructor.
MSSysCal::MSSysCal():hasBeenDestroyed_p(True) { }

MSSysCal::MSSysCal(const String &tableName, TableOption option) 
  : MSTable<MSSysCalEnums>(tableName, option),
    hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(String &, TableOption) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSSysCalEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(String &, String &, TableOption) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(SetupNewTable &newTab, rownr_t nrrow,
			       Bool initialize)
    : MSTable<MSSysCalEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(SetupNewTable &, rownr_t, Bool) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(const Table &table)
    : MSTable<MSSysCalEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSysCal(const Table &) - "
			 "table is not a valid MSSysCal"));
}

MSSysCal::MSSysCal(const MSSysCal &other)
    : MSTable<MSSysCalEnums>(other), 
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
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSSysCal() - Table written is not a valid MSSysCal"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSSysCal& MSSysCal::operator=(const MSSysCal &other)
{
    if (&other != this) {
	MSTable<MSSysCalEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSSysCal::initMaps()
{
  MSTableMaps maps;
  AlwaysAssert (maps.columnMap_p.empty(), AipsError);
  // the PredefinedColumns
  // ANTENNA_ID
  colMapDef(maps, ANTENNA_ID, "ANTENNA_ID", TpInt,
            "ID of antenna in this array","","");
  // FEED_ID
  colMapDef(maps, FEED_ID,"FEED_ID",TpInt,
            "Feed id","","");
  // INTERVAL
  colMapDef(maps, INTERVAL,"INTERVAL",TpDouble,
            "Interval for which this set of parameters is accurate",
            "s","");
  // SPECTRAL_WINDOW_ID
  colMapDef(maps, SPECTRAL_WINDOW_ID,"SPECTRAL_WINDOW_ID",TpInt,
            "ID for this spectral window setup","","");
  // TIME
  colMapDef(maps, TIME,"TIME",TpDouble,
            "Midpoint of time for which this set of "
            "parameters is accurate","s","Epoch");
  // PHASE_DIFF
  colMapDef(maps, PHASE_DIFF,"PHASE_DIFF",TpFloat,
            "Phase difference between receptor 2 and receptor 1",
            "rad","");
  // PHASE_DIFF_FLAG
  colMapDef(maps, PHASE_DIFF_FLAG,"PHASE_DIFF_FLAG",TpBool,
            "Flag for PHASE_DIFF","","");
  // TANT
  colMapDef(maps, TANT,"TANT",TpArrayFloat,
            "Antenna temperature for each receptor","K","");
  // TANT_FLAG
  colMapDef(maps, TANT_FLAG,"TANT_FLAG",TpBool,
            "Flag for TANT","","");
  // TANT_SPECTRUM
  colMapDef(maps, TANT_SPECTRUM,"TANT_SPECTRUM",TpArrayFloat,
            "Antenna temperature for each channel and receptor","K","");
  // TANT_TSYS
  colMapDef(maps, TANT_TSYS,"TANT_TSYS",TpArrayFloat,
            "Ratio of Antenna & system temperature for each receptor",
            "","");
  // TANT_TSYS_FLAG
  colMapDef(maps, TANT_TSYS_FLAG,"TANT_TSYS_FLAG",TpBool,
            "Flag for TANT_TSYS","","");
  // TANT_TSYS_SPECTRUM
  colMapDef(maps, TANT_TSYS_SPECTRUM,"TANT_TSYS_SPECTRUM",TpArrayFloat,
            "Ratio of Antenna & system temperature for each channel "
            "and receptor","","");
  // TCAL
  colMapDef(maps, TCAL,"TCAL",TpArrayFloat,
            "Calibration temperature for each receptor","K","");
  // TCAL_FLAG
  colMapDef(maps, TCAL_FLAG,"TCAL_FLAG",TpBool,
            "Flag for TCAL","","");
  // TCAL_SPECTRUM
  colMapDef(maps, TCAL_SPECTRUM,"TCAL_SPECTRUM",TpArrayFloat,
            "Calibration temperature for each channel and receptor","K","");
  // TRX 
  colMapDef(maps, TRX,"TRX",TpArrayFloat,
            "Receiver temperature for each of the two receptors","K","");
  // TRX_FLAG
  colMapDef(maps, TRX_FLAG,"TRX_FLAG",TpBool,
            "Flag for TRX","","");
  // TRX_SPECTRUM
  colMapDef(maps, TRX_SPECTRUM,"TRX_SPECTRUM",TpArrayFloat,
            "Receiver temperature for each channel and receptor","K","");
  // TSKY 
  colMapDef(maps, TSKY,"TSKY",TpArrayFloat,
            "Sky temperature for each of the two receptors","K","");
  // TSKY_FLAG
  colMapDef(maps, TSKY_FLAG,"TSKY_FLAG",TpBool,
            "Flag for TSKY","","");
  // TSKY_SPECTRUM
  colMapDef(maps, TSKY_SPECTRUM,"TSKY_SPECTRUM",TpArrayFloat,
            "Sky temperature for each channel and receptor","K","");
  // TSYS
  colMapDef(maps, TSYS,"TSYS",TpArrayFloat,
            "System temp. for each of the two receptors","K","");
  // TSYS_FLAG
  colMapDef(maps, TSYS_FLAG,"TSYS_FLAG",TpBool,
            "Flag for TSYS","","");
  // TSYS_SPECTRUM
  colMapDef(maps, TSYS_SPECTRUM,"TSYS_SPECTRUM",TpArrayFloat,
            "System temperature for each channel and receptor","K","");
  
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

MSSysCal MSSysCal::referenceCopy(const String& newTableName, 
				 const Block<String>& writableColumns) const
{
    return MSSysCal(MSTable<MSSysCalEnums>::referenceCopy
		    (newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

