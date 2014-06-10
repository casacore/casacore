//# tMSFieldEphem.cc : this program tests the ephemeris handling in MSField and MSFieldColumns
//# Copyright (C) 1995-1999,2000-2004
//# Associated Universities, Inc. Washington DC, USA
//# Copyright by ESO (in the framework of the ALMA collaboration)
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
//# Correspondence concerning CASA should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: CASA Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id: $

//# Includes

#include <ms/MeasurementSets/MSColumns.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <measures/Measures/MeasIERS.h>
#include <casa/Exceptions/Error.h>
#include <tables/Tables.h>
#include <casa/iostream.h>
#include <casa/OS/Directory.h>
#include <casa/OS/Path.h>
#include <casa/Utilities/Assert.h>

#include <casa/namespace.h>
int main() {
  try {
    {    
      // create MeasurementSet with all predefined columns
      TableDesc td;
      for (uInt i = 1; i < MS::NUMBER_PREDEFINED_COLUMNS; i++) {
	MS::addColumnToDesc(td, MS::PredefinedColumns(i));
      }
      td.rwKeywordSet().define("MS_VERSION", Float(2.0));
      
      SetupNewTable newtab("tMSFieldEphem_table.ms",td,Table::New);  
      
      MeasurementSet ms(newtab,1);
      
      // now add all compulsory subtables, each with all predefined columns
      
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
      
      TableDesc tdWeather;
      for (uInt i = 1 ; i<=MSWeather::NUMBER_PREDEFINED_COLUMNS; i++) {
	MSWeather::addColumnToDesc(tdWeather, MSWeather::PredefinedColumns(i));
      }
      SetupNewTable weatherSetup(ms.weatherTableName(),tdWeather,Table::New);
      ms.rwKeywordSet().defineTable(MS::keywordName(MS::WEATHER),Table(weatherSetup));
      
      // intialize the references to the subtables just added
      ms.initRefs();
    } // write the MS to disk

    //////////////////////////////////////////////////////////////////////
    // Test ephemeris table related features of MSField and MSFieldColumns
    //////////////////////////////////////////////////////////////////////
    {
      MeasurementSet ms("tMSFieldEphem_table.ms",Table::Old);
      MSFieldColumns msfc(ms.field());
      msfc.updateMeasComets();
      // use VGEO and VTOP as example ephemerides
      String tablePathName;
      {
	Table x; Table* y=0;
	MeasIERS::findTab(x, y, " ", " ", "VTOP");
	tablePathName = Path(x.tableName()).absoluteName();
        // cout << "Found " << tablePathName  << endl;
      }

      AlwaysAssertExit( ms.field().addEphemeris(0, tablePathName, "Venus") );

      String tablePathName2;
      {
	Table x2; Table* y2=0;
	MeasIERS::findTab(x2, y2, " ", " ", "VGEO");
	tablePathName2 = Path(x2.tableName()).absoluteName();
        // cout << "Found " << tablePathName2 << endl;
      }
      // add using non-absolute path
     {
	Directory vgeo(tablePathName2);
	vgeo.copy(Path(".").absoluteName()+"/VGEO", True);
      }
      AlwaysAssertExit( ms.field().addEphemeris(1, "./VGEO", "VenusGeo") );

      // test overwrite
      AlwaysAssertExit( ms.field().addEphemeris(0, "./VGEO", "Venus") );
      AlwaysAssertExit( ms.field().addEphemeris(0, tablePathName, "Venus") );
      // test error handling
      AlwaysAssertExit( ms.field().addEphemeris(1, "idontexist", "unknown") == False );      
      // test removal
      AlwaysAssertExit( ms.field().addEphemeris(2, "./VGEO", "Venus2") );
      AlwaysAssertExit( ms.field().removeEphemeris(2) );
      
      Directory vgeo2(Path(".").absoluteName()+"/VGEO");
      vgeo2.removeRecursive();

    }
    {
      MeasurementSet ms("tMSFieldEphem_table.ms",Table::Update);
      MSFieldColumns msfc(ms.field());
      Vector<Double> dir(2); dir(0)=0., dir(1)=0.;
      // add a row with default entries 
      ms.field().addRow();
      msfc.delayDir().put(0, dir);
      msfc.phaseDir().put(0, dir);
      msfc.referenceDir().put(0, dir);
      msfc.time().put(0,50802.708334*86400.); // start of the VTOP ephemeris
      msfc.ephemerisId().put(0,-1); // ephemeris id -1 
      msfc.updateMeasComets();
      {
	Int row = 0;
	Double mjds = 50802.75*86400.;
	MDirection dDir = msfc.delayDirMeas(row, mjds);
	// cout << "position for row " << row << ", MJD " << mjds/86400. << ": " << dDir.getAngle(Unit("deg")) << endl;
      }

      // add one row with ephemeris 
      ms.field().addRow();
      msfc.delayDir().put(1, dir);
      msfc.phaseDir().put(1, dir);
      msfc.referenceDir().put(1, dir);
      msfc.time().put(1,50802.708334*86400.); // start of the VTOP ephemeris
      msfc.ephemerisId().put(1,0); // ephemeris id 0
      msfc.updateMeasComets();
      {
	Int row = 1;
	Double mjds = 50802.75*86400.;
	MDirection dDir = msfc.delayDirMeas(row, mjds);
	// cout << "delaydir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << dDir.getAngle(Unit("deg")) << endl;
	MDirection pDir = msfc.phaseDirMeas(row, mjds);
	// cout << "phasedir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << pDir.getAngle(Unit("deg")) << endl;
	MDirection rDir = msfc.referenceDirMeas(row, mjds);
	// cout << "referencedir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << rDir.getAngle(Unit("deg")) << endl;

	MDirection expected(Quantity(-54.3855, "deg"), Quantity(-19.8873, "deg"), MDirection::APP);
	MVDirection expDir(expected.getAngle());

	AlwaysAssertExit(expDir.separation(MVDirection(dDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(dDir.type()==expected.type());
	AlwaysAssertExit(expDir.separation(MVDirection(pDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(pDir.type()==expected.type());
	AlwaysAssertExit(expDir.separation(MVDirection(rDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(rDir.type()==expected.type());
      }      

      Vector<Double> dirb(2); dirb(0)=Quantity(1.,"deg").getValue("rad"), dirb(1)=dirb(0)/2.;
      // add one row with ephemeris and non-zero offset 
      ms.field().addRow();
      msfc.delayDir().put(2, dirb);
      msfc.phaseDir().put(2, dirb);
      msfc.referenceDir().put(2, dirb);
      msfc.time().put(2,50802.708334*86400.); // start of the VTOP ephemeris
      msfc.ephemerisId().put(2,0); // ephemeris id 0
      msfc.updateMeasComets();
      {
	Int row = 2;
	Double mjds = 50802.75*86400.;
	MDirection dDir = msfc.delayDirMeas(row, mjds);
	// cout << "delaydir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << dDir.getAngle(Unit("deg")) << endl;
	MDirection pDir = msfc.phaseDirMeas(row, mjds);
	// cout << "phasedir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << pDir.getAngle(Unit("deg")) << endl;
	MDirection rDir = msfc.referenceDirMeas(row, mjds);
	// cout << "referencedir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << rDir.getAngle(Unit("deg")) << endl;

	MVDirection original(Quantity(305.6145129, "deg"),
			     Quantity(-19.8873316, "deg"));
	original.shift(dirb(0), dirb(1), True);
	MDirection expected(original, MDirection::TOPO);

 	// MDirection expected(Quantity(305.6145129 + dirb(0)*180./3.14159265/cos(-19.88733167*3.1415926/180.), "deg"),
 	//		    Quantity(-19.88733167+dirb(1)*180./3.14159265, "deg"), 
 	//		    MDirection::TOPO);
	MVDirection expDir(expected.getAngle());

	// cout << "separation " << expDir.separation(MVDirection(dDir.getAngle()), "deg") << endl;

	AlwaysAssertExit(expDir.separation(MVDirection(dDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(dDir.type()==expected.type());
	AlwaysAssertExit(expDir.separation(MVDirection(pDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(pDir.type()==expected.type());
	AlwaysAssertExit(expDir.separation(MVDirection(rDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));

	// cout << "types " << rDir.getRef() << " " << expected.getRef() << endl;

	AlwaysAssertExit(rDir.getRef().getType()  == expected.getRef().getType() );
      }      
      // add one row with GEO ephemeris 
      ms.field().addRow();
      msfc.delayDir().put(3, dir);
      msfc.phaseDir().put(3, dir);
      msfc.referenceDir().put(3, dir);
      msfc.time().put(3, 50802.708333*86400.); // start of the VGEO ephemeris
      msfc.ephemerisId().put(3,1); // ephemeris id 1
      msfc.updateMeasComets();
      {
	Int row = 3;
	Double mjds = 50802.75*86400.;
	MDirection dDir = msfc.delayDirMeas(row, mjds);
	// cout << "delaydir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << dDir.getAngle(Unit("deg")) << endl;
	MDirection pDir = msfc.phaseDirMeas(row, mjds);
	// cout << "phasedir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << pDir.getAngle(Unit("deg")) << endl;
	MDirection rDir = msfc.referenceDirMeas(row, mjds);
	// cout << "referencedir for row " << row << ", MJD-50802. " << mjds/86400.-50802. << ": " << rDir.getAngle(Unit("deg")) << endl;

	MVDirection original(Quantity(305.6095079, "deg"),
			     Quantity(-19.88256944, "deg"));

	MDirection expected(original, MDirection::APP);
	MVDirection expDir(expected.getAngle());

	// cout << "separation " << expDir.separation(MVDirection(dDir.getAngle()), "deg") << endl;

	AlwaysAssertExit(expDir.separation(MVDirection(dDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(dDir.type()==expected.type());
	AlwaysAssertExit(expDir.separation(MVDirection(pDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(pDir.type()==expected.type());
	AlwaysAssertExit(expDir.separation(MVDirection(rDir.getAngle()))<Quantity(1/3600., "deg").getValue("rad"));
	AlwaysAssertExit(rDir.getRef().getType() == expected.getRef().getType() );

	// test error handling

	Bool didThrow = False;

	try{
	  MDirection xDir = msfc.delayDirMeas(row, 12345.); // time outside validity range
	} catch (AipsError x) {
	  //cout <<  x.getMesg() <<endl;
	  didThrow = True;
	}
	AlwaysAssertExit(didThrow);

	didThrow = False;
	try{
	  MRadialVelocity xmradvel = msfc.radVelMeas(row, 12345.); // time outside validity range
	} catch (AipsError x) {
	  //cout <<  x.getMesg() <<endl;
	  didThrow = True;
	}
	AlwaysAssertExit(didThrow);

	didThrow = False;
	try{
	  MRadialVelocity xrho = msfc.rho(row, 12345.); // time outside validity range
	} catch (AipsError x) {
	  //cout <<  x.getMesg() <<endl;
	  didThrow = True;
	}
	AlwaysAssertExit(didThrow);

	// Finally test radial velocity and rho access

	MRadialVelocity mradvel = msfc.radVelMeas(row, mjds);
	// cout << "mradvel " << mradvel.get("AU/d") << endl;
	AlwaysAssertExit((mradvel.get("AU/d")-Quantity(-0.0057244046, "AU/d")).getValue("km/s")<0.01);

	Quantity rho =  msfc.rho(row, mjds);
	// cout << "rho " << rho.get("AU") << endl;
	AlwaysAssertExit((rho.get("AU") - Quantity(0.35214584, "AU")).getValue("km")<10.);

      }      
    }
    {
      MeasurementSet ms("tMSFieldEphem_table.ms",Table::Update);
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



