//# tMSColumns.cc : this program tests the MSColumns classes
//# Copyright (C) 1999,2000,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes

#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  try {
    {    
      // create a MeasurementSet with all predefined columns
      TableDesc td;
      for (uInt i = 1; i < MS::NUMBER_PREDEFINED_COLUMNS; i++) {
	MS::addColumnToDesc(td, MS::PredefinedColumns(i));
      }
      td.rwKeywordSet().define("MS_VERSION", Float(2.0));
      
      SetupNewTable newtab("tMSColumns_table.ms",td,Table::New);  
      
      MeasurementSet ms(newtab,1);
      
      // now add all subtables, each with all predefined columns
      
      TableDesc tdAntenna;
      for (uInt i = 1 ; i<=MSAntenna::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSAntenna::addColumnToDesc(tdAntenna, MSAntenna::PredefinedColumns(i));
      }
      SetupNewTable antennaSetup(ms.antennaTableName(),tdAntenna,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::ANTENNA),Table(antennaSetup));
      
      TableDesc tddataDescription;
      for (uInt i = 1 ; i<=MSDataDescription::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSDataDescription::addColumnToDesc(tddataDescription, MSDataDescription::PredefinedColumns(i));
      }
      SetupNewTable dataDescriptionSetup(ms.dataDescriptionTableName(),tddataDescription,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::DATA_DESCRIPTION),
				    Table(dataDescriptionSetup));
      
      TableDesc tdDoppler;
      for (uInt i = 1 ; i<=MSDoppler::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSDoppler::addColumnToDesc(tdDoppler, MSDoppler::PredefinedColumns(i));
      }
      SetupNewTable dopplerSetup(ms.dopplerTableName(),tdDoppler,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::DOPPLER),Table(dopplerSetup));
      
      TableDesc tdFeed;
      for (uInt i = 1 ; i<=MSFeed::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSFeed::addColumnToDesc(tdFeed, MSFeed::PredefinedColumns(i));
      }
      SetupNewTable feedSetup(ms.feedTableName(),tdFeed,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::FEED),Table(feedSetup));
      
      TableDesc tdField;
      for (uInt i = 1 ; i<=MSField::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSField::addColumnToDesc(tdField, MSField::PredefinedColumns(i));
      }
      SetupNewTable fieldSetup(ms.fieldTableName(),tdField,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::FIELD),Table(fieldSetup));
      
      TableDesc tdFlagCmd;
      for (uInt i = 1 ; i<=MSFlagCmd::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSFlagCmd::addColumnToDesc(tdFlagCmd, MSFlagCmd::PredefinedColumns(i));
      }
      SetupNewTable flagCmdSetup(ms.flagCmdTableName(),tdFlagCmd,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::FLAG_CMD),Table(flagCmdSetup));
      
      TableDesc tdFreqOffset;
      for (uInt i = 1 ; i<=MSFreqOffset::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSFreqOffset::addColumnToDesc(tdFreqOffset, MSFreqOffset::PredefinedColumns(i));
      }
      SetupNewTable freqOffsetSetup(ms.freqOffsetTableName(),tdFreqOffset,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::FREQ_OFFSET),Table(freqOffsetSetup));
      
      TableDesc tdHistory;
      for (uInt i = 1 ; i<=MSHistory::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSHistory::addColumnToDesc(tdHistory, MSHistory::PredefinedColumns(i));
      }
      SetupNewTable historySetup(ms.historyTableName(),tdHistory,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::HISTORY),Table(historySetup));
      
      TableDesc tdObservation;
      for (uInt i = 1 ; i<=MSObservation::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSObservation::addColumnToDesc(tdObservation, MSObservation::PredefinedColumns(i));
      }
      SetupNewTable observationSetup(ms.observationTableName(),tdObservation,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::OBSERVATION),Table(observationSetup));
      
      TableDesc tdPointing;
      for (uInt i = 1 ; i<=MSPointing::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSPointing::addColumnToDesc(tdPointing, MSPointing::PredefinedColumns(i));
      }
      SetupNewTable pointingSetup(ms.pointingTableName(),tdPointing,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::POINTING),Table(pointingSetup));
      
      TableDesc tdPolarization;
      for (uInt i = 1 ; i<=MSPolarization::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSPolarization::addColumnToDesc(tdPolarization, MSPolarization::PredefinedColumns(i));
      }
      SetupNewTable polarizationSetup(ms.polarizationTableName(),tdPolarization,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::POLARIZATION),Table(polarizationSetup));
      
      TableDesc tdProcessor;
      for (uInt i = 1 ; i<=MSProcessor::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSProcessor::addColumnToDesc(tdProcessor, MSProcessor::PredefinedColumns(i));
      }
      SetupNewTable processorSetup(ms.processorTableName(),tdProcessor,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::PROCESSOR),Table(processorSetup));
      
      TableDesc tdSource;
      for (uInt i = 1 ; i<=MSSource::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSSource::addColumnToDesc(tdSource, MSSource::PredefinedColumns(i));
      }
      SetupNewTable sourceSetup(ms.sourceTableName(),tdSource,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::SOURCE),Table(sourceSetup));
      
      TableDesc tdSpectralWindow;
      for (uInt i = 1 ; i<=MSSpectralWindow::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSSpectralWindow::addColumnToDesc(tdSpectralWindow, MSSpectralWindow::PredefinedColumns(i));
      }
      SetupNewTable spectralWindowSetup(ms.spectralWindowTableName(),tdSpectralWindow,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::SPECTRAL_WINDOW),Table(spectralWindowSetup));
      
      TableDesc tdState;
      for (uInt i = 1 ; i<=MSState::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSState::addColumnToDesc(tdState, MSState::PredefinedColumns(i));
      }
      SetupNewTable stateSetup(ms.stateTableName(),tdState,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::STATE),Table(stateSetup));
      
      TableDesc tdSysCal;
      for (uInt i = 1 ; i<=MSSysCal::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSSysCal::addColumnToDesc(tdSysCal, MSSysCal::PredefinedColumns(i));
      }
      SetupNewTable sysCalSetup(ms.sysCalTableName(),tdSysCal,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::SYSCAL),Table(sysCalSetup));
      
      TableDesc tdWeather;
      for (uInt i = 1 ; i<=MSWeather::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSWeather::addColumnToDesc(tdWeather, MSWeather::PredefinedColumns(i));
      }
      SetupNewTable weatherSetup(ms.weatherTableName(),tdWeather,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::WEATHER),Table(weatherSetup));
      
      // intialize the references to the subtables just added
      ms.initRefs();
    } // write the MS to disk

    //  now create the MSColumns and ROMSColumns objects, testing all
    // declarations and column definitions for type consistency
    {
      MeasurementSet ms("tMSColumns_table.ms",Table::Old);
      ROMSColumns romsc(ms);
    }
    {
      MeasurementSet ms("tMSColumns_table.ms",Table::Update);
      MSColumns msc(ms);
      // remove the table
      ms.markForDelete();
    }
    return 0;  
  } catch (AipsError x) {
    cerr << x.getMesg() <<endl;
    return 1;
  } 
}



