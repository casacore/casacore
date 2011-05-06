//# MeasurementSet.cc:  the class that hold measurements from telescopes
//# Copyright (C) 1996,1997,1998,2000,2001,2002,2003
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

#include <ms/MeasurementSets/MeasurementSet.h>

#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Exceptions/Error.h>
#include <casa/Containers/Record.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MFrequency.h>
#include <ms/MeasurementSets/MSSelection.h>
#include <tables/Tables/TableInfo.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ColDescSet.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/StManAipsIO.h>
#include <tables/Tables/ForwardCol.h>
#include <tables/Tables/IncrementalStMan.h>
#include <tables/Tables/StandardStMan.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

MeasurementSet::MeasurementSet():hasBeenDestroyed_p(True) { }

MeasurementSet::MeasurementSet(const String &tableName,
			       TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    checkVersion();
    mainLock_p=TableLock(TableLock::AutoNoReadLocking);
    addCat(); 
    if (! validate(this->tableDesc()))
	throw (AipsError("MS(String &, TableOption) - "
			 "table is not a valid MS"));
    initRefs();
}

void MeasurementSet::addCat()
{
  // For a transition period: add the CATEGORY keyword to the FLAG_CATEGORY
  // column silently if it is not there - 2000/08/22, remove next MS update.
  if (!tableDesc().columnDesc(columnName(FLAG_CATEGORY)).
      keywordSet().isDefined("CATEGORY")) {
    if (!isWritable()) {
      throw (AipsError("Missing CATEGORY keyword in FLAG_CATEGORY column -"
		       "please open MS table R/W to have it added"));
    } else {
      ArrayColumn<Bool> fc(*this,columnName(FLAG_CATEGORY));
      fc.rwKeywordSet().define("CATEGORY",Vector<String>(0));
    }
  }
}


MeasurementSet::MeasurementSet(const String &tableName,
			       const TableLock& lockOptions,
			       TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, lockOptions, option), 
      hasBeenDestroyed_p(False)
{

  mainLock_p=lockOptions;
  // verify that the now opened table is valid
  checkVersion();
  addCat(); 
  if (! validate(this->tableDesc()))
    throw (AipsError("MS(String &, lockOptions, TableOption) - "
		     "table is not a valid MS"));
  initRefs();
}

MeasurementSet::MeasurementSet(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName, option),
      hasBeenDestroyed_p(False)
{

  mainLock_p=TableLock(TableLock::AutoNoReadLocking);
    // verify that the now opened table is valid 
    checkVersion();
    addCat(); 
    if (! validate(this->tableDesc()))
	throw (AipsError("MS(String &, String &, TableOption) - "
			 "table is not a valid MS"));
    initRefs();
}

MeasurementSet::MeasurementSet(const String& tableName, const String &tableDescName,
			       const TableLock& lockOptions, TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName, lockOptions, option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid 
  mainLock_p=lockOptions;
    checkVersion();
    addCat(); 
    if (! validate(this->tableDesc()))
	throw (AipsError("MS(String &, String &, TableOption) - "
			 "table is not a valid MS"));
    initRefs();
}

MeasurementSet::MeasurementSet(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{

  mainLock_p=TableLock(TableLock::AutoNoReadLocking);
    // verify that the now opened table is valid
    addCat(); 
    if (! validate(this->tableDesc()))
	throw (AipsError("MS(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MS"));
}

MeasurementSet::MeasurementSet(SetupNewTable &newTab,
			       const TableLock& lockOptions, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, lockOptions, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{

  mainLock_p=lockOptions;
    // verify that the now opened table is valid
    addCat(); 
    if (! validate(this->tableDesc()))
	throw (AipsError("MS(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MS"));
}

MeasurementSet::MeasurementSet(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{

  mainLock_p=TableLock(TableLock::AutoNoReadLocking);
    // verify that the now opened table is valid 
    checkVersion();
    addCat(); 
    if (! validate(this->tableDesc()))
	throw (AipsError("MS(const Table &) - "
			 "table is not a valid MS"));
    initRefs();
}

MeasurementSet::MeasurementSet(const MeasurementSet &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), hasBeenDestroyed_p(False)
{

  mainLock_p=TableLock(TableLock::AutoNoReadLocking);
    // verify that other is valid
    if (&other != this) {
        addCat(); 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MS(const MeasurementSet &) - "
			     "MeasurementSet is not a valid MS"));
    }
    if (!isNull()) initRefs();
}

MeasurementSet::~MeasurementSet()
{
// check to make sure that this MS is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MS() - Table written is not a valid MS"));
    }
    // Table::relinquishAutoLocks(True);
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}

MeasurementSet& MeasurementSet::operator=(const MeasurementSet &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
	// initRefs needs to be called even if the MS is null to ensure that
	// the subtable references, which may have contained non-null
	// subtables, are replaced with references to null-subtables.
	initRefs(True);
    }
    return *this;
}

void MeasurementSet::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// ANTENNA1
	colMapDef(ANTENNA1, "ANTENNA1", TpInt,
		"ID of first antenna in interferometer","","");
	// ANTENNA2
	colMapDef(ANTENNA2, "ANTENNA2", TpInt,
		  "ID of second antenna in interferometer","","");
	// ANTENNA3
	colMapDef(ANTENNA3, "ANTENNA3", TpInt,
		  "ID of third antenna in interferometer","","");
	// ARRAY_ID
	colMapDef(ARRAY_ID, "ARRAY_ID", TpInt, 
		  "ID of array or subarray","","");
	// BASELINE_REF
	colMapDef(BASELINE_REF,"BASELINE_REF",TpBool,
		  "Reference antenna for this baseline, True for ANTENNA1","",
		  "");
	// the CORRECTED_DATA column,
	colMapDef(CORRECTED_DATA,"CORRECTED_DATA",TpArrayComplex,
		  "The corrected data column","","");
	// the DATA columns,
	colMapDef(DATA,"DATA",TpArrayComplex,"The data column","","");
	// the DATA_DESC_ID
	colMapDef(DATA_DESC_ID,"DATA_DESC_ID",TpInt,
		  "The data description table index","","");
	// EXPOSURE
	colMapDef(EXPOSURE, "EXPOSURE", TpDouble,
		  "The effective integration time","s","");
	// FEED1
	colMapDef(FEED1, "FEED1", TpInt, "The feed index for ANTENNA1","","");
	// FEED2
	colMapDef(FEED2, "FEED2", TpInt, "The feed index for ANTENNA2","","");
	// FEED3
	colMapDef(FEED3, "FEED3", TpInt, "The feed index for ANTENNA3","","");
	// FIELD_ID
	colMapDef(FIELD_ID,"FIELD_ID", TpInt, "Unique id for this pointing","","");
	// FLAG
	colMapDef(FLAG,"FLAG", TpArrayBool, 
		  "The data flags, array of bools with same shape as data","","");
	// FLAG_CATEGORY
	colMapDef(FLAG_CATEGORY,"FLAG_CATEGORY", TpArrayBool, 
		  "The flag category, NUM_CAT flags for each datum","","");
	// FLAG_ROW
	colMapDef(FLAG_ROW,"FLAG_ROW", TpBool,
		  "Row flag - flag all data in this row if True","","");
	// FLOAT_DATA
	colMapDef(FLOAT_DATA,"FLOAT_DATA",TpArrayFloat,
		  "Floating point data - for single dish","","");
        // IMAGING_WEIGHT
        colMapDef(IMAGING_WEIGHT,"IMAGING_WEIGHT",TpArrayFloat,
                  "Weight set by imaging task (e.g. uniform weighting)","","");
	// INTERVAL
	colMapDef(INTERVAL, "INTERVAL", TpDouble, 
		  "The sampling interval","s","");
	// the LAG_DATA column,
	colMapDef(LAG_DATA,"LAG_DATA",TpArrayComplex,
		  "The lag data column","","");
	// the MODEL_DATA column,
	colMapDef(MODEL_DATA,"MODEL_DATA",TpArrayComplex,
		  "The model data column","","");
	// OBSERVATION_ID
	colMapDef(OBSERVATION_ID, "OBSERVATION_ID", TpInt,
		  "ID for this observation, index in OBSERVATION table","",""); 
	// PHASE_ID
	colMapDef(PHASE_ID,"PHASE_ID",TpInt,
		  "Id for phase switching","","");
	// PROCESSOR_ID
	colMapDef(PROCESSOR_ID,"PROCESSOR_ID",TpInt,
		  "Id for backend processor, index in PROCESSOR table","","");
	// PULSAR_BIN
	colMapDef(PULSAR_BIN, "PULSAR_BIN", TpInt,
		  "Pulsar pulse-phase bin for this DATA","",""); 
	// PULSAR_GATE_ID
	colMapDef(PULSAR_GATE_ID, "PULSAR_GATE_ID", TpInt,
		  "ID for this gate, index into PULSAR_GATE table","","");
	// SCAN_NUMBER
	colMapDef(SCAN_NUMBER, "SCAN_NUMBER", TpInt,
		  "Sequential scan number from on-line system","","");
	// STATE_ID
	colMapDef(STATE_ID,"STATE_ID",TpInt,
		  "ID for this observing state","","");
	// SIGMA
	colMapDef(SIGMA, "SIGMA", TpArrayFloat,
		  "Estimated rms noise for channel with unity bandpass response","","");
	// SIGMA_SPECTRUM
	colMapDef(SIGMA_SPECTRUM, "SIGMA_SPECTRUM", TpArrayFloat,
		  "Estimated rms noise for each data point","","");
	// TIME
	colMapDef(TIME, "TIME", TpDouble, "Modified Julian Day","s","Epoch");
	// TIME_CENTROID
	colMapDef(TIME_CENTROID, "TIME_CENTROID", TpDouble, 
		  "Modified Julian Day","s","Epoch");
	// TIME_EXTRA_PREC
	colMapDef(TIME_EXTRA_PREC, "TIME_EXTRA_PREC", TpDouble,
		  "Additional precision for TIME","s","");
	// UVW
	colMapDef(UVW, "UVW", TpArrayDouble, 
		  "Vector with uvw coordinates (in meters)","m","uvw");
	// UVW2
	colMapDef(UVW2,"UVW2",TpArrayDouble,
		  "uvw coordinates for second pair of triple corr product",
		  "m","uvw");
	// VIDEO_POINT
	colMapDef(VIDEO_POINT,"VIDEO_POINT",TpArrayComplex,
		  "zero frequency point, needed for transform to lag","","");
	// WEIGHT
	colMapDef(WEIGHT, "WEIGHT", TpArrayFloat,
		  "Weight for each polarization spectrum","","");
	// WEIGHT_SPECTRUM
	colMapDef(WEIGHT_SPECTRUM, "WEIGHT_SPECTRUM", TpArrayFloat,
		  "Weight for each data point","","");

	// PredefinedKeywords

	// ANTENNA
	keyMapDef(ANTENNA,"ANTENNA", TpTable,
		  "Antenna subtable. Antenna positions, mount-types etc.");
        // DATA_DESCRIPTION
	keyMapDef(DATA_DESCRIPTION,"DATA_DESCRIPTION", TpTable,
		  "DATA_DESCRIPTION subtable. Points to freq and pol layout"
		  "in subtables");
	// FEED
	keyMapDef(FEED,"FEED", TpTable,
		  "Feed subtable. Responses, offsets, beams etc.");
	// FIELD
	keyMapDef(FIELD,"FIELD",TpTable,
		  "Field subtable. Position etc. for each pointing.");
	// FLAG_CMD
	keyMapDef(FLAG_CMD,"FLAG_CMD",TpTable,
		  "Flag command subtable. Stores global flagging commands");
	// HISTORY
	keyMapDef(HISTORY,"HISTORY",TpTable,
		  "Observation and processing history");
	// MS_VERSION
	keyMapDef(MS_VERSION,"MS_VERSION",TpFloat,
		  "MS version number, i.e., 2.0");
	// OBSERVATION
	keyMapDef(OBSERVATION,"OBSERVATION",TpTable,
		  "Observation subtable. Project, observer, schedule.");
	// POINTING
	keyMapDef(POINTING,"POINTING",TpTable,
		  "Pointing subtable. Antenna pointing info.");
	// POLARIZATION
	keyMapDef(POLARIZATION,"POLARIZATION",TpTable,
		  "Polarization set up subtable");
	// PROCESSOR
	keyMapDef(PROCESSOR,"PROCESSOR",TpTable,
		  "Backend Processor information subtable");
	// SPECTRAL_WINDOW
	keyMapDef(SPECTRAL_WINDOW,"SPECTRAL_WINDOW",TpTable,
		  "Spectral window subtable. Frequencies, bandwidths,"
		  " polarizations");
	// STATE
	keyMapDef(STATE,"STATE",TpTable,
		  "State subtable. State information (cal, ref etc.)");
	// CAL_TABLES
	keyMapDef(CAL_TABLES,"CAL_TABLES",TpTable,
		  "Associated calibration tables, one per row");
	// DOPPLER
	keyMapDef(DOPPLER,"DOPPLER",TpTable,
		  "Doppler tracking info");
	// FREQ_OFFSET
	keyMapDef(FREQ_OFFSET,"FREQ_OFFSET",TpTable,
		  "Frequency offset information");
	// SORT_COLUMNS
	keyMapDef(SORT_COLUMNS,"SORT_COLUMNS",TpArrayString,
		  "Listing of sort columns for each sorted table");
	// SORT_ORDER
	keyMapDef(SORT_ORDER,"SORT_ORDER",TpArrayString,
		  "Listing of sort orders for each sorted table");
	// SORTED_TABLES
	keyMapDef(SORTED_TABLES,"SORTED_TABLES",TpTable,
		  "Sorted reference tables of the main table, first one is"
		  " main table");
	// SOURCE
	keyMapDef(SOURCE,"SOURCE",TpTable,
		  "Source subtable. Positions etc. for each source.");
	// SYSCAL
	keyMapDef(SYSCAL,"SYSCAL",TpTable,
		  "SysCal subtable. System calibration data (Tsys etc.).");
	// WEATHER
	keyMapDef(WEATHER,"WEATHER",TpTable,
		  "Weather subtable. Weather info for each antenna.");

	// define required keywords and columns
	TableDesc requiredTD;
	// all required keywords 
	uInt i;
	for (i = UNDEFINED_KEYWORD+1;
	     i <= NUMBER_REQUIRED_KEYWORDS; i++) {
	    addKeyToDesc(requiredTD, PredefinedKeywords(i));
	}
	// Set MS_VERSION number
	requiredTD.rwKeywordSet().define("MS_VERSION",Float(2.0));
	
	// all required columns 
	// First define the columns with fixed size arrays
	IPosition shape(1,3);
	ColumnDesc::Option option=ColumnDesc::Direct;
	addColumnToDesc(requiredTD, UVW, shape, option);
	// Also define columns with Arrays with their correct dimensionality
	addColumnToDesc(requiredTD, FLAG, 2);
	addColumnToDesc(requiredTD, FLAG_CATEGORY, 3);
	addColumnToDesc(requiredTD, WEIGHT, 1);
	addColumnToDesc(requiredTD, SIGMA, 1);
	// Now define all other columns (duplicates are skipped)
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
        // Add the column keyword for the FLAG_CATEGORY column
        requiredTD.rwColumnDesc("FLAG_CATEGORY").rwKeywordSet().
	  define("CATEGORY",Vector<String>(0));

	// init counted pointer to requiredTableDesc 
	requiredTD_p=new TableDesc(requiredTD);
    }
}
	
MeasurementSet MeasurementSet::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MeasurementSet(MSTable<PredefinedColumns,PredefinedKeywords>::
			  referenceCopy(newTableName,writableColumns));
}

String MeasurementSet::antennaTableName() const
{
  if (antenna_p.isNull()) {
    return tableName()+"/ANTENNA";
  }
  return antenna_p.tableName();
}
String MeasurementSet::dataDescriptionTableName() const
{
  if (dataDesc_p.isNull()) {
    return tableName()+"/DATA_DESCRIPTION";
  }
  return dataDesc_p.tableName();
}
String MeasurementSet::dopplerTableName() const
{
  if (doppler_p.isNull()) {
    return tableName()+"/DOPPLER";
  }
  return doppler_p.tableName();
}
String MeasurementSet::feedTableName() const
{
  if (feed_p.isNull()) {
    return tableName()+"/FEED";
  }
  return feed_p.tableName();
}
String MeasurementSet::fieldTableName() const
{
  if (field_p.isNull()) {
    return tableName()+"/FIELD";
  }
  return field_p.tableName();
}
String MeasurementSet::flagCmdTableName() const
{
  if (flagCmd_p.isNull()) {
    return tableName()+"/FLAG_CMD";
  }
  return flagCmd_p.tableName();
}
String MeasurementSet::freqOffsetTableName() const
{
  if (freqOffset_p.isNull()) {
    return tableName()+"/FREQ_OFFSET";
  }
  return freqOffset_p.tableName();
}
String MeasurementSet::historyTableName() const
{
  if (history_p.isNull()) {
    return tableName()+"/HISTORY";
  }
  return history_p.tableName();
}
String MeasurementSet::observationTableName() const
{
  if (observation_p.isNull()) {
    return tableName()+"/OBSERVATION";
  }
  return observation_p.tableName();
}
String MeasurementSet::pointingTableName() const
{
  if (pointing_p.isNull()) {
    return tableName()+"/POINTING";
  }
  return pointing_p.tableName();
}
String MeasurementSet::polarizationTableName() const
{
  if (polarization_p.isNull()) {
    return tableName()+"/POLARIZATION";
  }
  return polarization_p.tableName();
}
String MeasurementSet::processorTableName() const
{
  if (processor_p.isNull()) {
    return tableName()+"/PROCESSOR";
  }
  return processor_p.tableName();
}
String MeasurementSet::sourceTableName() const
{
  if (source_p.isNull()) {
    return tableName()+"/SOURCE";
  }
  return source_p.tableName();
}
String MeasurementSet::spectralWindowTableName() const
{
  if (spectralWindow_p.isNull()) {
    return tableName()+"/SPECTRAL_WINDOW";
  }
  return spectralWindow_p.tableName();
}
String MeasurementSet::stateTableName() const
{
  if (state_p.isNull()) {
    return tableName()+"/STATE";
  }
  return state_p.tableName();
}
String MeasurementSet::sysCalTableName() const
{
  if (sysCal_p.isNull()) {
    return tableName()+"/SYSCAL";
  }
  return sysCal_p.tableName();
}
String MeasurementSet::weatherTableName() const
{
  if (weather_p.isNull()) {
    return tableName()+"/WEATHER";
  }
  return weather_p.tableName();
}

void MeasurementSet::initRefs(Bool clear)
{
  if (isNull()||clear) {
    // clear subtable references
    antenna_p=MSAntenna();
    dataDesc_p=MSDataDescription();
    doppler_p=MSDoppler();
    feed_p=MSFeed();
    field_p=MSField();
    flagCmd_p=MSFlagCmd();
    freqOffset_p=MSFreqOffset();
    history_p=MSHistory();
    observation_p=MSObservation();
    pointing_p=MSPointing();
    polarization_p=MSPolarization();
    processor_p=MSProcessor();
    source_p=MSSource();
    spectralWindow_p=MSSpectralWindow();
    state_p=MSState();
    sysCal_p=MSSysCal();
    weather_p=MSWeather();
  }
  if (!isNull()) {
    // write the table info if needed
    if (this->tableInfo().type()=="") {
      String reqdType=this->tableInfo().type(TableInfo::MEASUREMENTSET);
      this->tableInfo().setType(reqdType);
      String reqdSubType=this->tableInfo().subType(TableInfo::MEASUREMENTSET);
      this->tableInfo().setSubType(reqdSubType);
      this->tableInfo().readmeAddLine("This is a MeasurementSet Table"
				      " holding measurements from a Telescope");
    }
    if(this->tableOption() != Table::Scratch){
      if (this->keywordSet().isDefined("ANTENNA"))
	antenna_p=MSAntenna(this->keywordSet().asTable("ANTENNA", mainLock_p));
      if (this->keywordSet().isDefined("DATA_DESCRIPTION"))
	dataDesc_p=MSDataDescription(this->keywordSet().
				     asTable("DATA_DESCRIPTION", mainLock_p));
      if (this->keywordSet().isDefined("DOPPLER"))
	doppler_p=MSDoppler(this->keywordSet().asTable("DOPPLER", mainLock_p));
      if (this->keywordSet().isDefined("FEED"))
	feed_p=MSFeed(this->keywordSet().asTable("FEED", mainLock_p));
      if (this->keywordSet().isDefined("FIELD"))
	field_p=MSField(this->keywordSet().asTable("FIELD", mainLock_p));
      if (this->keywordSet().isDefined("FLAG_CMD"))
	flagCmd_p=MSFlagCmd(this->keywordSet().asTable("FLAG_CMD", 
						       mainLock_p));
      if (this->keywordSet().isDefined("FREQ_OFFSET"))
	freqOffset_p=MSFreqOffset(this->keywordSet().
				  asTable("FREQ_OFFSET", mainLock_p));
      if (this->keywordSet().isDefined("HISTORY"))
	history_p=MSHistory(this->keywordSet().asTable("HISTORY", mainLock_p));
      if (this->keywordSet().isDefined("OBSERVATION"))
	observation_p=MSObservation(this->keywordSet().
				    asTable("OBSERVATION",mainLock_p));
      if (this->keywordSet().isDefined("POINTING"))
	pointing_p=MSPointing(this->keywordSet().
			      asTable("POINTING", mainLock_p));
      if (this->keywordSet().isDefined("POLARIZATION"))
	polarization_p=MSPolarization(this->keywordSet().
				      asTable("POLARIZATION",mainLock_p));
      if (this->keywordSet().isDefined("PROCESSOR"))
	processor_p=MSProcessor(this->keywordSet().
				asTable("PROCESSOR", mainLock_p));
      if (this->keywordSet().isDefined("SOURCE"))
	source_p=MSSource(this->keywordSet().asTable("SOURCE", mainLock_p));
      if (this->keywordSet().isDefined("SPECTRAL_WINDOW"))
	spectralWindow_p=MSSpectralWindow(this->keywordSet().
					  asTable("SPECTRAL_WINDOW",mainLock_p));
      if (this->keywordSet().isDefined("STATE"))
	state_p=MSState(this->keywordSet().asTable("STATE", mainLock_p));
      if (this->keywordSet().isDefined("SYSCAL"))
	sysCal_p=MSSysCal(this->keywordSet().asTable("SYSCAL", mainLock_p));
      if (this->keywordSet().isDefined("WEATHER"))
	weather_p=MSWeather(this->keywordSet().asTable("WEATHER", mainLock_p));
    }
    else{ //if its scratch...don't bother about the lock as 
          //We can't close and reopen subtables hence we can't use the version 
      //of astable that does that.
      if (this->keywordSet().isDefined("ANTENNA"))
	antenna_p=MSAntenna(this->keywordSet().asTable("ANTENNA"));
      if (this->keywordSet().isDefined("DATA_DESCRIPTION"))
	dataDesc_p=MSDataDescription(this->keywordSet().
				     asTable("DATA_DESCRIPTION"));
      if (this->keywordSet().isDefined("DOPPLER"))
	doppler_p=MSDoppler(this->keywordSet().asTable("DOPPLER"));
      if (this->keywordSet().isDefined("FEED"))
	feed_p=MSFeed(this->keywordSet().asTable("FEED"));
      if (this->keywordSet().isDefined("FIELD"))
	field_p=MSField(this->keywordSet().asTable("FIELD"));
      if (this->keywordSet().isDefined("FLAG_CMD"))
	flagCmd_p=MSFlagCmd(this->keywordSet().asTable("FLAG_CMD"));
      if (this->keywordSet().isDefined("FREQ_OFFSET"))
	freqOffset_p=MSFreqOffset(this->keywordSet().
				  asTable("FREQ_OFFSET"));
      if (this->keywordSet().isDefined("HISTORY"))
	history_p=MSHistory(this->keywordSet().asTable("HISTORY"));
      if (this->keywordSet().isDefined("OBSERVATION"))
	observation_p=MSObservation(this->keywordSet().
				    asTable("OBSERVATION"));
      if (this->keywordSet().isDefined("POINTING"))
	pointing_p=MSPointing(this->keywordSet().
			      asTable("POINTING"));
      if (this->keywordSet().isDefined("POLARIZATION"))
	polarization_p=MSPolarization(this->keywordSet().
				      asTable("POLARIZATION"));
      if (this->keywordSet().isDefined("PROCESSOR"))
	processor_p=MSProcessor(this->keywordSet().
				asTable("PROCESSOR"));
      if (this->keywordSet().isDefined("SOURCE"))
	source_p=MSSource(this->keywordSet().asTable("SOURCE"));
      if (this->keywordSet().isDefined("SPECTRAL_WINDOW"))
	spectralWindow_p=MSSpectralWindow(this->keywordSet().
					  asTable("SPECTRAL_WINDOW"));
      if (this->keywordSet().isDefined("STATE"))
	state_p=MSState(this->keywordSet().asTable("STATE"));
      if (this->keywordSet().isDefined("SYSCAL"))
	sysCal_p=MSSysCal(this->keywordSet().asTable("SYSCAL"));
      if (this->keywordSet().isDefined("WEATHER"))
	weather_p=MSWeather(this->keywordSet().asTable("WEATHER"));


    }
  }
}

void MeasurementSet::createDefaultSubtables(Table::TableOption option)
{
    SetupNewTable antennaSetup(antennaTableName(),
			       MSAntenna::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::ANTENNA),
			       Table(antennaSetup));
    SetupNewTable dataDescSetup(dataDescriptionTableName(),
			       MSDataDescription::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::DATA_DESCRIPTION), 
			       Table(dataDescSetup));
    SetupNewTable feedSetup(feedTableName(),
			       MSFeed::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::FEED), Table(feedSetup));
    SetupNewTable flagCmdSetup(flagCmdTableName(),
			       MSFlagCmd::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::FLAG_CMD), 
			       Table(flagCmdSetup));
    SetupNewTable fieldSetup(fieldTableName(),
			       MSField::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::FIELD), Table(fieldSetup));
    SetupNewTable historySetup(historyTableName(),
			       MSHistory::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::HISTORY), 
			       Table(historySetup));
    SetupNewTable observationSetup(observationTableName(),
			       MSObservation::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::OBSERVATION), 
			       Table(observationSetup));
    SetupNewTable pointingSetup(pointingTableName(),
			       MSPointing::requiredTableDesc(),option);
    // Pointing table can be large, set some sensible defaults for storageMgrs
    IncrementalStMan ismPointing ("ISMPointing");
    StandardStMan ssmPointing("SSMPointing",32768);
    pointingSetup.bindAll(ismPointing,True);
    pointingSetup.bindColumn(MSPointing::columnName(MSPointing::ANTENNA_ID),
			     ssmPointing);
    rwKeywordSet().defineTable(MS::keywordName(MS::POINTING),
			       Table(pointingSetup));
    SetupNewTable polarizationSetup(polarizationTableName(),
			       MSPolarization::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::POLARIZATION),
			       Table(polarizationSetup));
    SetupNewTable processorSetup(processorTableName(),
			       MSProcessor::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::PROCESSOR),
			       Table(processorSetup));
    SetupNewTable spectralWindowSetup(spectralWindowTableName(),
			       MSSpectralWindow::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::SPECTRAL_WINDOW),  
			       Table(spectralWindowSetup));
    SetupNewTable stateSetup(stateTableName(),
			       MSState::requiredTableDesc(),option);
    rwKeywordSet().defineTable(MS::keywordName(MS::STATE),  
			       Table(stateSetup));
    initRefs();
}

Bool MeasurementSet::makeComplexData()
{
  // for now we use an extremely simplistic implementation (should find out
  // storage managers and tiles and keep things the same)
  if (tableDesc().isColumn(MS::columnName(MS::DATA))) return False;
  if (!tableDesc().isColumn(MS::columnName(MS::FLOAT_DATA))) return False;

  // we have FLOAT_DATA but not DATA
  // add DATA
  addColumn(ArrayColumnDesc<Complex>("DATA",2));
  
  // now copy data across from FLOAT_DATA
  ArrayColumn<Float> floatData(*this,MS::columnName(MS::FLOAT_DATA));
  ArrayColumn<Complex> data(*this,MS::columnName(MS::DATA));
  for (uInt i=0; i<nrow(); i++) {
    Array<Float> floatArr(floatData(i));
    Array<Complex> dataArr(floatArr.shape());
    convertArray(dataArr,floatArr);
    data.put(i,dataArr);
  }
  return True;
}

Bool MeasurementSet::validateMeasureRefs()
{
  Bool ok=True;
  // check main table
  {
    Int nCol = tableDesc().ncolumn();
    for (Int i=0; i<nCol; i++) {
      Int fld = tableDesc()[i].keywordSet().fieldNumber("MEASINFO");
      if (fld>=0) {
	Int refFld = tableDesc()[i].keywordSet().asRecord(fld).
	  fieldNumber("Ref");
	if (refFld<0 || tableDesc()[i].keywordSet().asRecord(fld).
	    asString(refFld) == "") {
	  cerr << "Missing Measure reference for column "<<tableDesc()[i].name()
	       << endl;
	  ok = False;
	}
      }
    }
  }
  // check all subtables
  Int nKey = keywordSet().nfields();
  for (Int i=0; i<nKey; i++) {
    if (keywordSet().type(i)== TpTable) {
      Table tab = keywordSet().asTable(i);
      Int nCol = tab.tableDesc().ncolumn();
      for (Int i=0; i<nCol; i++) {
	Int fld = tab.tableDesc()[i].keywordSet().fieldNumber("MEASINFO");
	if (fld>=0) {
	  Int refFld = tab.tableDesc()[i].keywordSet().asRecord(fld).
	    fieldNumber("Ref");
	  if (refFld<0 || tab.tableDesc()[i].keywordSet().asRecord(fld).
	      asString(refFld) == "") {
	    cerr << "Missing Measure reference for column "
		 <<tab.tableDesc()[i].name()<<" in subtable "<<tab.tableName() 
		 << endl;
	    ok = False;
	  }
	}
      }
    }
  }
  return ok;
}

void MeasurementSet::flush(Bool sync) {
  MSTable<MSMainEnums::PredefinedColumns, MSMainEnums::PredefinedKeywords>::flush(sync);
  antenna_p.flush(sync);
  dataDesc_p.flush(sync);
  if (!doppler_p.isNull()) doppler_p.flush(sync);
  feed_p.flush(sync);
  field_p.flush(sync);
  flagCmd_p.flush(sync);
  if (!freqOffset_p.isNull())  freqOffset_p.flush(sync);
  history_p.flush(sync);
  observation_p.flush(sync);
  pointing_p.flush(sync);
  polarization_p.flush(sync);
  processor_p.flush(sync);
  if (!source_p.isNull())  source_p.flush(sync);
  spectralWindow_p.flush(sync);
  state_p.flush(sync);
  if (!sysCal_p.isNull())  sysCal_p.flush(sync);
  if (!weather_p.isNull())  weather_p.flush(sync);
}

void MeasurementSet::checkVersion()
{
  // Check that the MS is the latest version (2.0). Throw an
  // exception and advise the user to use the MS converter if it is not.
  //
  if (!keywordSet().isDefined("MS_VERSION") || 
      (keywordSet().isDefined("MS_VERSION") &&
       keywordSet().asFloat("MS_VERSION")!=2.0)) {
    throw(AipsError("These data are not in MSv2 format - use ms1toms2 to convert"));
  }
}

Record MeasurementSet::msseltoindex(const String& spw, const String& field, 
		      const String& baseline, const String& time, 
		      const String& scan, const String& uvrange, 
				    const String& taql){
  Record retval;
  MSSelection thisSelection;
  thisSelection.setSpwExpr(spw);
  thisSelection.setFieldExpr(field);
  thisSelection.setAntennaExpr(baseline);
  thisSelection.setTimeExpr(time);
  thisSelection.setScanExpr(scan);
  thisSelection.setUvDistExpr(uvrange);
  thisSelection.setTaQLExpr(taql);
  TableExprNode exprNode=thisSelection.toTableExprNode(this);
  Vector<Int> fieldlist=thisSelection.getFieldList();
  Vector<Int> spwlist=thisSelection.getSpwList();
  Vector<Int> scanlist=thisSelection.getScanList();
  Vector<Int> antenna1list=thisSelection.getAntenna1List();
  Vector<Int> antenna2list=thisSelection.getAntenna2List();
  Matrix<Int> chanlist=thisSelection.getChanList();
  Matrix<Int> baselinelist=thisSelection.getBaselineList();
  Vector<Int> ddIDList=thisSelection.getDDIDList();
  OrderedMap<Int, Vector<Int > > polMap=thisSelection.getPolMap();
  OrderedMap<Int, Vector<Vector<Int> > > corrMap=thisSelection.getCorrMap();

  retval.define("spw", spwlist);
  retval.define("field", fieldlist);
  retval.define("scan",scanlist);
  retval.define("antenna1", antenna1list);
  retval.define("antenna2", antenna2list);
  retval.define("baselines",baselinelist);
  retval.define("channel", chanlist);
  retval.define("dd",ddIDList);
  //  retval.define("polmap",polMap);
  // retrval.define("corrmap",corrMap);

  return retval;

}


} //# NAMESPACE CASA - END

