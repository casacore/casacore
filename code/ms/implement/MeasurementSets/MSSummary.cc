//# MSSummary.cc:  Helper class for applications listing a MeasurementSet
//# Copyright (C) 1998,1999,2000
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
//#
#include <aips/aips.h>
#include <aips/Arrays.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Quanta/Unit.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Measures/Stokes.h>
#include <aips/Tables/TableRecord.h>

#include <trial/Coordinates.h>
#include <aips/MeasurementSets.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <trial/MeasurementSets/MSSummary.h>

#include <iomanip.h>
#include <iostream.h>


//
// Constructor assigns pointer.  If MS goes out of scope you
// will get rubbish.  Also sets string to separate subtable output.
//
MSSummary::MSSummary (const MeasurementSet& ms)
  : pMS(&ms),
    dashlin1(replicate("-",80)),
    dashlin2(replicate("=",80))
{}


//
// Destructor does nothing
//
MSSummary::~MSSummary ()
{}


//
// Retrieve number of rows
//
Int MSSummary::nrow () const
{
   return pMS->nrow();
}


//
// Get ms name
//
String MSSummary::name () const
{
   return pMS->tableName();
}


//
// Reassign pointer.  
//
Bool MSSummary::setMS (const MeasurementSet& ms)
{
  const MeasurementSet* pTemp;
  pTemp = &ms;
  if (pTemp == 0) {
    return False;
  } else {
    pMS = pTemp;
    return True;
  }
}


//
// List information about an ms to the logger
//
void MSSummary::list (LogIO& os, Bool verbose) const
{
  // List a title for the Summary
  listTitle (os);

  // List the main table as well as the subtables in a useful order and format
  listWhere (os,verbose);
  listWhat (os,verbose);
  listHow (os,verbose);

  // These aren't really useful (yet?)
  //  listArray (os,verbose);
  //  listSource (os,verbose);
  //  listObsLog (os,verbose);
  //  listSysCal (os,verbose);
  //  listWeather (os,verbose);

  // List a summary of table sizes
  os << dashlin1 << endl << endl;
  listTables (os,verbose);
  os << dashlin2 << endl;

  // Post it
  os.post();

}


//
// List a title for the Summary
//
void MSSummary::listTitle (LogIO& os) const
{
  // Version number of the MS definition
  Float vers = 1.0;
  if (pMS->keywordSet().isDefined("MS_VERSION")) {
    vers = pMS->keywordSet().asFloat("MS_VERSION");
  }

  // List the MS name and version as the title of the Summary
  os << LogIO::NORMAL;
  os << dashlin2 << endl << "           MeasurementSet Name:  " << this->name()
     << "      MS Version " << vers << endl << dashlin2 << endl;
}


//
// Convenient table groupings
//
void MSSummary::listWhere (LogIO& os, Bool verbose) const
{
  listObservation (os,verbose);
}


void MSSummary::listWhat (LogIO& os, Bool verbose) const
{
  listField (os,verbose);
  listMain (os,verbose);
}


void MSSummary::listHow (LogIO& os, Bool verbose) const
{
  listSpectralWindow (os,verbose);
  listPolarization (os,verbose);
  listFeed (os,verbose);
  listAntenna (os,verbose);
}


//
// SUBTABLES
//
void MSSummary::listMain (LogIO& os, Bool verbose) const
{
  if (nrow()<=0) {
    os << "The MAIN table is empty: there are no data!!!" << endl;
  }
  else {
    // Make objects
    ROMSColumns msc(*pMS);
    Double startTime, stopTime;
    minMax(startTime, stopTime, msc.time().getColumn());
    Double exposTime = stopTime - startTime;
    //    Double exposTime = sum(msc.exposure().getColumn());
    MVTime startMVT(startTime/86400.0), stopMVT(stopTime/86400.0);

    if (verbose) {}	//null; always the same output

    // Output info
    os << "Data records: " << nrow() << "       Total integration time = "
       << exposTime << " seconds" << endl
       << "   Observed from   " << startMVT.string()
       << "   to   " << stopMVT.string() << endl << endl;

  }
  os << LogIO::POST;
}


void MSSummary::listAntenna (LogIO& os, Bool verbose) const 
{
  // Do nothing in terse mode
  if (verbose) {

    // Make a MS-antenna object
    MSAntenna antennaTable(pMS->antenna());

    if (antennaTable.nrow()<=0) {
      os << "The ANTENNA table is empty" << endl;
    }
    else {
      os << "Antennas: " << antennaTable.nrow() << endl;
      ROScalarColumn<String> antennaNames(antennaTable,
				MSAntenna::columnName(MSAntenna::NAME));
      String line, leader;
      Int last=-1;
      for (uInt row=0; row<antennaNames.nrow(); row++) {
	// Build the line
	line = line + " " + antennaNames(row);
	if (line.length()>65) {
	  // This line is finished, dump it after the line leader
	  leader = String::toString(last+2) +"-" +String::toString(row+1) +":";
	  os << "   " << leader << line << endl;
	  line = "";
	  last = row;
	}
      }
      if (line.length()>0) {
	// Prepend and dump last line
	leader =  String::toString(last+2) +"-"
		+ String::toString(antennaNames.nrow()) +":";
	os << "   " << leader << line << endl;
      }
    }
    os << LogIO::POST;
  }
}


void MSSummary::listArray (LogIO& os, Bool verbose) const 
{
  // Do nothing in terse mode
  if (verbose) {

    // Make a MS-observation object
    MSObservation obsTable(pMS->observation());

    if (obsTable.nrow()<=0) {
      os << "The OBSERVATION table is empty" << endl;
    }
    else {
      os << "Observations: " << obsTable.nrow() << endl;
      ROScalarColumn<String> arrayNames(obsTable,MSObservation::columnName
					(MSObservation::TELESCOPE_NAME));
      os << "   Array names:";
      for (uInt row=0; row<arrayNames.nrow(); row++) {
	os << " " << arrayNames(row);
      }
      os << endl;
    }
  }
  os<< LogIO::POST;
}


void MSSummary::listFeed (LogIO& os, Bool verbose) const 
{
  // Do nothing in terse mode
  if (verbose) {

    // Make a MS-feed-columns object
    ROMSFeedColumns msFC(pMS->feed());

    if (msFC.antennaId().nrow()<=0) {
      os << "The FEED table is empty" << endl;
    }
    else {
      os << "Feeds = " << msFC.antennaId().nrow();
      os << ": printing first row only";
      // Line is	FeedID SpWinID NumRecept PolTypes
      Int widthLead	=  3;
      Int widthAnt	= 10;
      Int widthSpWinId	= 20;
      Int widthNumRec	= 15;
      Int widthPolType	= 10;
      os << endl;
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthLead);	os << "   ";
      os.output().width(widthAnt);	os << "Antenna";
      os.output().width(widthSpWinId);	os << "Spectral Window";
      os.output().width(widthNumRec);	os << "# Receptors";
      os.output().width(widthPolType);	os << "Polarizations";
      os << endl;

      // loop through rows
      // for (uInt row=0; row<msFC.antennaId().nrow(); row++) {
      for (uInt row=0; row<1; row++) {
	os.output().setf(ios::left, ios::adjustfield);
	os.output().width(widthLead);	os << "   ";
	os.output().width(widthAnt);	os << (msFC.antennaId()(row)+1);
        Int spwId = msFC.spectralWindowId()(row);
	if (spwId >= 0) spwId = spwId + 1;
	os.output().width(widthSpWinId);os << spwId;
	os.output().width(widthNumRec);	os << msFC.numReceptors()(row);
	os.output().width(widthPolType);os << msFC.polarizationType()(row);
	os << endl;
      }
    }
  }
  os << LogIO::POST;
}


void MSSummary::listField (LogIO& os, Bool verbose) const 
{
  // Make a MS-field-columns object
  ROMSFieldColumns msFC(pMS->field());

  if (msFC.phaseDir().nrow()<=0) {
    os << "The FIELD table is empty" << endl;
  }
  else {
    os << "Fields: " << msFC.phaseDir().nrow();
    Int widthLead  =  3;	
    Int widthField =  5;	
    Int widthName  = 10;
    Int widthRA    = 17;
    Int widthDec   = 14;
    Int widthType  =  8;

    if (verbose) {
      os << endl;
      // Line is	ID Date Time Name RA Dec Type
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthLead);	os << "   ";
      os.output().width(widthField);	os << "ID";
      os.output().width(widthName);	os << "Name";
      os.output().width(widthRA);	os << "Right Ascension";
      os.output().width(widthDec);	os << "Declination";
      os.output().width(widthType);	os << "Type";
      os << endl;
    }

    // loop through rows
    for (uInt row=0; row<msFC.name().nrow(); row++) {
      MDirection mRaDec=msFC.phaseDirMeas(row);
      MVAngle mvRa = mRaDec.getAngle().getValue()(0);
      MVAngle mvDec= mRaDec.getAngle().getValue()(1);

      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthLead);	os << "   ";
      if (verbose) {
	os.output().width(widthField);	os << (row+1);
      }
      else {
	if (row==0) {os << "Name: ";}
	else {os << "         Name: ";}
      }
      os.output().width(widthName);	os << msFC.name()(row);
      if (!verbose) os << "RA: ";
      os.output().width(widthRA);	os << mvRa(0.0).string(MVAngle::TIME,8);
      if (!verbose) os << "Dec: ";
      os.output().width(widthDec);	os << mvDec.string(MVAngle::DIG2,8);
      if (!verbose) os << "(";
      os.output().width(widthType);
		    os << MDirection::showType(mRaDec.getRefPtr()->getType());
      if (!verbose) os << ")";
      os << endl;
    }
  }
  os << endl << LogIO::POST;
}

void MSSummary::listObservation (LogIO& os, Bool verbose) const 
{
  // Make objects
  ROMSColumns msc(*pMS);
  const ROMSObservationColumns& msOC(msc.observation());

  if (msOC.project().nrow()<=0) {
    os << "The OBSERVATION table is empty" << endl;
  }
  else {
    os << "   Observer: " << msOC.observer()(0) << "   "
       << "   Project: " << msOC.project()(0) << "   ";
//v2os << "   Obs Date: " << msOC.obsDate()(0) << endl
//v2   << "   Tel name: " << msOC.telName()(0);
    if (msc.observation().telescopeName().nrow()>0) {
      os<<endl << "   Array: " << msc.observation().telescopeName()(0);
    }
    if (!verbose) os << "(" << msc.antenna().name().nrow() << " antennas)";
    os << endl << endl;

    if (msOC.project().nrow()>1) {
      // for version 2 of the MS
      // Line is	TelName ObsDate Observer Project
      //Int widthLead =  3;
      //Int widthTel  = 10;
      //Int widthDate = 20;
      //Int widthObs  = 15;
      //Int widthProj = 15;
      //os.output().setf(ios::left, ios::adjustfield);
      //os.output().width(widthLead);	os << "   ";
      //os.output().width(widthTel);	os << "Telescope";
      //os.output().width(widthDate);	os << "Observation Date";
      //os.output().width(widthObs);	os << "Observer";
      //os.output().width(widthProj);	os << "Project";
      //os << endl;

      //for (uInt row=0;row<msOC.project().nrow();row++) {
	//os.output().setf(ios::left, ios::adjustfield);
        //os.output().width(widthLead);	os << "   ";
	//os.output().width(widthTel);	os << msOC.telName()(row);
	//os.output().width(widthDate);	os << msOC.obsDate()(row);
	//os.output().width(widthObs);	os << msOC.observer()(row);
	//os.output().width(widthProj);	os << msOC.project()(row);
	//os << endl;
      //}
    }
  }
  os << LogIO::POST;
}


void MSSummary::listObsLog (LogIO& os, Bool verbose) const 
{
  // Do nothing in terse mode
  if (verbose) {

    // Create a MS-obslog-columns object
    ROMSHistoryColumns msHis(pMS->history());

    if (msHis.time().nrow()<=0) {
      os << "The HISTORY table is empty" << endl;
    }
    else {
      os << "History entries: " << msHis.time().nrow() << endl;
      // os << "HISTORY table entries" << endl;
    }
  }
  os << LogIO::POST;
}


void MSSummary::listSource (LogIO& os, Bool verbose) const 
{
  // Check if optional SOURCE table is present:
  if (pMS->source().isNull()) {
    os << "The SOURCE table is absent: see the FIELD table" << endl;
    return;
  }

  // Create a MS-source-columns object
  ROMSSourceColumns msSC(pMS->source());

  if (msSC.name().nrow()<=0) {
    os << "The SOURCE table is empty: see the FIELD table" << endl;
  }
  else {
    os << "Sources: " << msSC.name().nrow() << endl;
    if (verbose) {
      //  Line is	Time Name RA Dec SysVel
      Int widthLead =  3;
      Int widthTime = 15;
      Int widthName = 10;
      Int widthRA   = 20;
      Int widthDec  = 20;
      Int widthVel  = 10;
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthLead);	os << "   ";
      os.output().width(widthTime);	os << "Time MidPt";
      os.output().width(widthName);	os << "Name";
      os.output().width(widthRA);	os << "RA";
      os.output().width(widthDec);	os << "Dec";
      os.output().width(widthVel);	os << "SysVel";
      os << endl;

      // Loop through rows
      for (uInt row=0; row<msSC.direction().nrow(); row++) {
      MDirection mRaDec=msSC.directionMeas()(row);
	MVAngle mvRa=mRaDec.getAngle().getValue()(0);
	MVAngle mvDec=mRaDec.getAngle().getValue()(1);
	os.output().setf(ios::left, ios::adjustfield);
	os.output().width(widthLead);	os<< "   ";
	os.output().width(widthTime);
				os<< MVTime(msSC.time()(row)/86400.0).string();
	os.output().width(widthName);	os<< msSC.name()(row);
	os.output().width(widthRA);	os<< mvRa(0.0).string(MVAngle::TIME,8);
	os.output().width(widthDec);	os<< mvDec.string(MVAngle::DIG2,8);
	os.output().width(widthVel);	os<< msSC.sysvel()(row) << "m/s";
	os << endl;
      }
    }

    // Terse mode
    else {
      os << "   ";
      for (uInt row=0; row<msSC.name().nrow(); row++) {
	os << msSC.name()(row) << " ";
      }
      os << endl;
    }
  }
  os << LogIO::POST;
}


void MSSummary::listSpectralWindow (LogIO& os, Bool verbose) const
{
  // Create a MS-spwin-columns object
  ROMSSpWindowColumns msSWC(pMS->spectralWindow());

  if (verbose) {}	//null; always the same output

  if (msSWC.refFrequency().nrow()<=0) {
    os << "The SPECTRAL_WINDOW table is empty: see the FEED table" << endl;
  }
  else {
    os << "Spectral Windows: " << msSWC.refFrequency().nrow() << endl;
    // The 8 columns below are all in the SpWin subtable of Version 1 of
    // the MS definition.  For Version 2, some info will appear in other
    // subtables, as indicated.
    // Line is (V1): RefFreq RestFreq Molecule Trans'n Resol BW Numch Correls
    // V2 subtable:		SOURCE		 SOURCE		      POLARIZ'N

    // Define the column widths
    Int widthLead	=  3;
    Int widthFreq	= 12;
    Int widthFrqNum	=  7;
    //Int widthMol	= 10;
    //Int widthTrans	= 12;
    Int widthNumChan	=  8;
    //    Int widthCorrTypes	= msPolC.corrType()(0).nelements()*4;
    //    Int widthCorrType	=  4;

    // Write the column headers
    os.output().setf(ios::left, ios::adjustfield);
    os.output().width(widthLead);	os << "   ";
    os.output().width(widthFreq);	os << "Ref.Freq";
    //os.output().width(widthFreq);	os << "RestFreq";
    //os.output().width(widthMol);	os << "Molecule";
    //os.output().width(widthTrans);	os << "Transition";
    os.output().width(widthNumChan);	os << "#Chans";
    os.output().width(widthFreq);	os << "Resolution";
    os.output().width(widthFreq);	os << "TotalBW";
    //    os.output().width(widthCorrTypes);os << "Correlations";
    os << endl;

    // For each row of the SpWin subtable, write the info
    for (uInt row=0; row<msSWC.refFrequency().nrow(); row++) {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthLead);		os << "   ";
      // 1st column: reference frequency
      os.output().width(widthFrqNum);
				os<< msSWC.refFrequency()(row)/1.0e6 <<"MHz  ";
      // 2nd column: rest frequency of a line, "continuum" otherwise
//      if (msSWC.restFrequency()(row)<1e3) {
//        os.output().width(widthFreq);		os << "continuum";
//      } else {
//        os.output().width(widthFrqNum);
//        os<< msSWC.restFrequency()(row)/1.0e6<<"MHz  ";
//      }
      // 3rd column: molecule formula
      //os.output().width(widthMol);		os << msSWC.molecule()(row);
      // 4th column: transition designation
      //os.output().width(widthTrans);		os << msSWC.transition()(row);
      // 5th column: number of channels in the spectral window
      os.output().width(widthNumChan);		os << msSWC.numChan()(row);
      // 6th column: channel resolution
      os.output().width(widthFrqNum);
      os << msSWC.resolution()(row)(IPosition(1,0))/1000<<"kHz  ";
      // 7th column: total bandwidth of the spectral window
      os.output().width(widthFrqNum);
      os<< msSWC.totalBandwidth()(row)/1000<<"kHz  ";
      // 8th column: the correlation type(s)
      //      for (uInt i=0; i<msSWC.corrType()(row).nelements(); i++) {
      //	os.output().width(widthCorrType);
      //	Int index = msSWC.corrType()(row)(IPosition(1,i));
      //	os << Stokes::name(Stokes::type(index));
      //      }
      os << endl;
    }
  }
  os << LogIO::POST;
}

void MSSummary::listPolarization (LogIO& os, Bool verbose) const
{
  // Create a MS-pol-columns object
  ROMSPolarizationColumns msPolC(pMS->polarization());

  if (verbose) {}	//null; always the same output

  uInt nRow = pMS->polarization().nrow();
  if (nRow<=0) {
    os << "The POLARIZATION table is empty: see the FEED table" << endl;
  }
  else {
    os << "Polarization setups: " << nRow << endl;

    // Define the column widths
    Int widthLead	=  3;
    Int widthCorrTypes	= msPolC.corrType()(0).nelements()*4;
    Int widthCorrType	=  4;

    // Write the column headers
    os.output().setf(ios::left, ios::adjustfield);
    os.output().width(widthLead);	os << "   ";
    os.output().width(widthCorrTypes); os << "Correlations";
    os << endl;

    // For each row of the Pol subtable, write the info
    for (uInt row=0; row<nRow; row++) {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthLead);		os << "   ";
      // 8th column: the correlation type(s)
      for (uInt i=0; i<msPolC.corrType()(row).nelements(); i++) {
      	os.output().width(widthCorrType);
      	Int index = msPolC.corrType()(row)(IPosition(1,i));
      	os << Stokes::name(Stokes::type(index));
      }
      os << endl;
    }
  }
  os << LogIO::POST;
}


void MSSummary::listSysCal (LogIO& os, Bool verbose) const 
{
  // Check for existence of optional SYSCAL table:
  if (pMS->sysCal().isNull()) {
    os << "The SYSCAL table is absent" << endl;
    return;
  }

  // Do nothing in terse mode
  if (verbose) {

    // Create a MS-syscal-columns object
    ROMSSysCalColumns msSCC(pMS->sysCal());

    if (msSCC.tsys().nrow()<=0) {
      os << "The SYSCAL table is empty" << endl;
    }
    else {
      os << "SysCal entries: " << msSCC.tsys().nrow() << endl;
      os << "   The average Tsys for all data is "
	 << sum(msSCC.tsys()(0))/msSCC.tsys()(0).nelements() << " K" << endl;
    }
  }
  os << LogIO::POST;
}


void MSSummary::listWeather (LogIO& os, Bool verbose) const 
{
  // Check for existence of optional WEATHER table:
  if (pMS->weather().isNull()) {
    os << "The WEATHER table is absent" << endl;
    return;
  }

  // Do nothing in terse mode
  if (verbose) {

    // Create a MS-weather-columns object
    ROMSWeatherColumns msWC(pMS->weather());

    if (msWC.H2O().nrow()<=0) {
      os << "The WEATHER table is empty" << endl;
    }
    else {
      os << "Weather entries: " << msWC.H2O().nrow() << endl;
      os << "   Average H2O column density = " << msWC.H2O()(0)
	 << " m**-2      Average air temperature = "
	 << msWC.temperature()(0) << " K" << endl;
    }
  }
  os<< LogIO::POST;
}


void MSSummary::listTables (LogIO& os, Bool verbose) const 
{
  // Get nrows for each table (=-1 if table absent)
  Vector<Int> tableRows(18);
  tableRows(0) = nrow();
  tableRows(1) = pMS->antenna().nrow();
  tableRows(2) = pMS->dataDescription().nrow();
  tableRows(3) = (pMS->doppler().isNull() ? -1 : (Int)pMS->doppler().nrow());
  tableRows(4) = pMS->feed().nrow();
  tableRows(5) = pMS->field().nrow();
  tableRows(6) = pMS->flagCmd().nrow();
  tableRows(7) = (pMS->freqOffset().isNull() ? -1 : (Int)pMS->freqOffset().nrow());
  tableRows(8) = pMS->history().nrow();
  tableRows(9) = pMS->observation().nrow();
  tableRows(10) = pMS->pointing().nrow();
  tableRows(11) = pMS->polarization().nrow();
  tableRows(12) = pMS->processor().nrow();
  tableRows(13) = (pMS->source().isNull() ? -1 : (Int)pMS->source().nrow());
  tableRows(14) = pMS->spectralWindow().nrow();
  tableRows(15) = pMS->state().nrow();
  tableRows(16) = (pMS->sysCal().isNull() ? -1 : (Int)pMS->sysCal().nrow());
  tableRows(17) = (pMS->weather().isNull() ? -1 : (Int)pMS->weather().nrow());

  Vector<String> rowStrings(18), tableStrings(18);
  rowStrings = " rows";
  tableStrings(0) = "MAIN";
  tableStrings(1) = "ANTENNA";
  tableStrings(2) = "DATA_DESCRIPTION";
  tableStrings(3) = "DOPPLER";
  tableStrings(4) = "FEED";
  tableStrings(5) = "FIELD";
  tableStrings(6) = "FLAG_CMD";
  tableStrings(7) = "FREQ_OFFSET";
  tableStrings(8) = "HISTORY";
  tableStrings(9) = "OBSERVATION";
  tableStrings(10)= "POINTING";
  tableStrings(11)= "POLARIZATION";
  tableStrings(12)= "PROCESSOR";
  tableStrings(13)= "SOURCE";
  tableStrings(14)= "SPECTRAL_WINDOW";
  tableStrings(15)= "STATE";
  tableStrings(16)= "SYSCAL";
  tableStrings(17)= "WEATHER";

  // Just to make things read better
  for (uInt i=0; i<18; i++) {
    if (tableRows(i)==1) rowStrings(i) = " row";
    // if table exists, but empty:
    if (tableRows(i)==0) {
      rowStrings(i) = " <empty>";
      if (tableStrings(i)=="SOURCE") rowStrings(i) += " (see FIELD)";
      if (tableStrings(i)=="SPECTRAL_WINDOW") rowStrings(i) += " (see FEED)";
    }
    // if table absent:
    if (tableRows(i)==-1) {
      rowStrings(i) = "<absent>";
      if (tableStrings(i)=="SOURCE") rowStrings(i) += " (see FIELD)";
    }
  }

  // This bit is a little messy, keeping track of the verbose and terse
  // forms of the output format, as well as the zero/nonzero tables
						// Do things on this side
						// whether verbose or not
						os << "Tables";
  if (!verbose) os << "(rows)";			os << ":";
  if (!verbose) os << "   (-1 = table absent)";
                                                os << endl;
  for (uInt i=0; i<18; i++) {
    if (verbose) {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(3);
    }						os << "   ";
    if (verbose) {
      os.output().width(20);
    }						os << tableStrings(i);
    if (verbose && tableRows(i)>0) {
	os.output().setf(ios::right, ios::adjustfield);
	os.output().width(8);
    }
    if (!verbose) os << "(";
    if (!verbose || tableRows(i)>0)		os << tableRows(i);
    if (!verbose) os << ")";
    if (verbose) {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(10);	os << rowStrings(i);
      os << endl;
    }
    else {if (i==5) os << endl;}
  }
  os << LogIO::POST;
}


//
// Clear all the formatting flags
//
void MSSummary::clearFlags(LogIO& os) const
{
  os.output().unsetf(ios::left);
  os.output().unsetf(ios::right);
  os.output().unsetf(ios::internal);

  os.output().unsetf(ios::dec);
  os.output().unsetf(ios::oct);
  os.output().unsetf(ios::hex);

  os.output().unsetf(ios::showbase | ios::showpos | ios::uppercase
		     | ios::showpoint);

  os.output().unsetf(ios::scientific);
  os.output().unsetf(ios::fixed);

}
