//# tNewMSColumns.cc : this program tests the NewMSColumns classes
//# Copyright (C) 1999,2000
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

#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables.h>

int main() {
  try {
    {    
      // create a NewMeasurementSet with all predefined columns
      TableDesc td;
      for (uInt i = 1; i < NewMS::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMS::addColumnToDesc(td, NewMS::PredefinedColumns(i));
      }
      
      SetupNewTable newtab("tNewMSColumns_table.ms",td,Table::New);  
      
      NewMeasurementSet ms(newtab,1);
      
      // now add all subtables, each with all predefined columns
      
      TableDesc tdAntenna;
      for (uInt i = 1 ; i<=NewMSAntenna::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSAntenna::addColumnToDesc(tdAntenna, NewMSAntenna::PredefinedColumns(i));
      }
      SetupNewTable antennaSetup(ms.antennaTableName(),tdAntenna,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::ANTENNA),Table(antennaSetup));
      
      TableDesc tddataDescription;
      for (uInt i = 1 ; i<=NewMSDataDescription::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSDataDescription::addColumnToDesc(tddataDescription, NewMSDataDescription::PredefinedColumns(i));
      }
      SetupNewTable dataDescriptionSetup(ms.dataDescriptionTableName(),tddataDescription,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::DATA_DESCRIPTION),
				    Table(dataDescriptionSetup));
      
      TableDesc tdDoppler;
      for (uInt i = 1 ; i<=NewMSDoppler::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSDoppler::addColumnToDesc(tdDoppler, NewMSDoppler::PredefinedColumns(i));
      }
      SetupNewTable dopplerSetup(ms.dopplerTableName(),tdDoppler,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::DOPPLER),Table(dopplerSetup));
      
      TableDesc tdFeed;
      for (uInt i = 1 ; i<=NewMSFeed::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSFeed::addColumnToDesc(tdFeed, NewMSFeed::PredefinedColumns(i));
      }
      SetupNewTable feedSetup(ms.feedTableName(),tdFeed,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::FEED),Table(feedSetup));
      
      TableDesc tdField;
      for (uInt i = 1 ; i<=NewMSField::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSField::addColumnToDesc(tdField, NewMSField::PredefinedColumns(i));
      }
      SetupNewTable fieldSetup(ms.fieldTableName(),tdField,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::FIELD),Table(fieldSetup));
      
      TableDesc tdFlagCmd;
      for (uInt i = 1 ; i<=NewMSFlagCmd::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSFlagCmd::addColumnToDesc(tdFlagCmd, NewMSFlagCmd::PredefinedColumns(i));
      }
      SetupNewTable flagCmdSetup(ms.flagCmdTableName(),tdFlagCmd,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::FLAG_CMD),Table(flagCmdSetup));
      
      TableDesc tdFreqOffset;
      for (uInt i = 1 ; i<=NewMSFreqOffset::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSFreqOffset::addColumnToDesc(tdFreqOffset, NewMSFreqOffset::PredefinedColumns(i));
      }
      SetupNewTable freqOffsetSetup(ms.freqOffsetTableName(),tdFreqOffset,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::FREQ_OFFSET),Table(freqOffsetSetup));
      
      TableDesc tdHistory;
      for (uInt i = 1 ; i<=NewMSHistory::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSHistory::addColumnToDesc(tdHistory, NewMSHistory::PredefinedColumns(i));
      }
      SetupNewTable historySetup(ms.historyTableName(),tdHistory,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::HISTORY),Table(historySetup));
      
      TableDesc tdObservation;
      for (uInt i = 1 ; i<=NewMSObservation::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSObservation::addColumnToDesc(tdObservation, NewMSObservation::PredefinedColumns(i));
      }
      SetupNewTable observationSetup(ms.observationTableName(),tdObservation,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::OBSERVATION),Table(observationSetup));
      
      TableDesc tdPointing;
      for (uInt i = 1 ; i<=NewMSPointing::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSPointing::addColumnToDesc(tdPointing, NewMSPointing::PredefinedColumns(i));
      }
      SetupNewTable pointingSetup(ms.pointingTableName(),tdPointing,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::POINTING),Table(pointingSetup));
      
      TableDesc tdPolarization;
      for (uInt i = 1 ; i<=NewMSPolarization::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSPolarization::addColumnToDesc(tdPolarization, NewMSPolarization::PredefinedColumns(i));
      }
      SetupNewTable polarizationSetup(ms.polarizationTableName(),tdPolarization,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::POLARIZATION),Table(polarizationSetup));
      
      TableDesc tdProcessor;
      for (uInt i = 1 ; i<=NewMSProcessor::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSProcessor::addColumnToDesc(tdProcessor, NewMSProcessor::PredefinedColumns(i));
      }
      SetupNewTable processorSetup(ms.processorTableName(),tdProcessor,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::PROCESSOR),Table(processorSetup));
      
      TableDesc tdSource;
      for (uInt i = 1 ; i<=NewMSSource::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSSource::addColumnToDesc(tdSource, NewMSSource::PredefinedColumns(i));
      }
      SetupNewTable sourceSetup(ms.sourceTableName(),tdSource,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::SOURCE),Table(sourceSetup));
      
      TableDesc tdSpectralWindow;
      for (uInt i = 1 ; i<=NewMSSpectralWindow::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSSpectralWindow::addColumnToDesc(tdSpectralWindow, NewMSSpectralWindow::PredefinedColumns(i));
      }
      SetupNewTable spectralWindowSetup(ms.spectralWindowTableName(),tdSpectralWindow,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::SPECTRAL_WINDOW),Table(spectralWindowSetup));
      
      TableDesc tdState;
      for (uInt i = 1 ; i<=NewMSState::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSState::addColumnToDesc(tdState, NewMSState::PredefinedColumns(i));
      }
      SetupNewTable stateSetup(ms.stateTableName(),tdState,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::STATE),Table(stateSetup));
      
      TableDesc tdSysCal;
      for (uInt i = 1 ; i<=NewMSSysCal::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSSysCal::addColumnToDesc(tdSysCal, NewMSSysCal::PredefinedColumns(i));
      }
      SetupNewTable sysCalSetup(ms.sysCalTableName(),tdSysCal,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::SYSCAL),Table(sysCalSetup));
      
      TableDesc tdWeather;
      for (uInt i = 1 ; i<=NewMSWeather::NUMBER_PREDEFINED_COLUMNS; i++) {
	NewMSWeather::addColumnToDesc(tdWeather, NewMSWeather::PredefinedColumns(i));
      }
      SetupNewTable weatherSetup(ms.weatherTableName(),tdWeather,Table::New);
      ms.rwKeywordSet().defineTable(NewMS::keywordName(NewMS::WEATHER),Table(weatherSetup));
      
      // intialize the references to the subtables just added
      ms.initRefs();
    } // write the NewMS to disk

    //  now create the NewMSColumns and RONewMSColumns objects, testing all
    // declarations and column definitions for type consistency
    {
      NewMeasurementSet ms("tNewMSColumns_table.ms",Table::Old);
      RONewMSColumns romsc(ms);
    }
    {
      NewMeasurementSet ms("tNewMSColumns_table.ms",Table::Update);
      NewMSColumns msc(ms);
      // remove the table
      ms.markForDelete();
    }
    return 0;  
  } catch (AipsError x) {
    cerr << x.getMesg() <<endl;
    return 1;
  } 
}



