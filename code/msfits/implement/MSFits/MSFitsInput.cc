//# MSFitsInput:  uvfits (random group) to MeasurementSet filler
//# Copyright (C) 1996,1997,1998,1999,2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
//

#include <trial/MeasurementSets/MSFitsInput.h>

#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Containers/Record.h>
#include <aips/Exceptions/Error.h>
#include <aips/FITS/fitsio.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/MeasurementSets/MSAntennaColumns.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/MeasurementSets/MSDataDescColumns.h>
#include <aips/MeasurementSets/MSFeedColumns.h>
#include <aips/MeasurementSets/MSFieldColumns.h>
#include <aips/MeasurementSets/MSHistoryColumns.h>
#include <aips/MeasurementSets/MSObsColumns.h>
#include <aips/MeasurementSets/MSPolColumns.h>
#include <aips/MeasurementSets/MSSpWindowColumns.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/Stokes.h>
#include <aips/OS/File.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Utilities/Fallible.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Regex.h>
  
#include <trial/FITS/FITSUtil.h>
#include <trial/FITS/BinTable.h>
#include <trial/Tasking/NewFile.h>
#include <trial/Tasking/ProgressMeter.h>


// Returns the 0-based position of the key string in the map,
// which is a list of strings.  Looks for the "Which" occurrance
// of the key.
static Int getIndex(Vector<String>& map, const String& key, uInt which = 0)
{
  uInt count = 0;
  const uInt nMap = map.nelements();
  for (uInt i = 0; i < nMap; i++) {
    if (map(i) == key) {
      if (count == which) {
	return i;
      } else {
	count++;
      }
    }
  }
  return -1;
}

// Like getIndex, but only checks for containment, not exact identity
static Int getIndexContains(Vector<String>& map, const String& key, 
			    uInt which = 0)
{
  uInt count = 0;
  const uInt nMap = map.nelements();
  for (uInt i = 0; i < nMap; i++) {
    if (map(i).contains(key)) {
      if (count == which) {
	return i;
      } else {
	count++;
      }
    }
  }
  return -1;
}

MSPrimaryGroupHolder::MSPrimaryGroupHolder()
  :hdu_p(0),
   ps(0),
   pl(0),
   pf(0)
{}

MSPrimaryGroupHolder::MSPrimaryGroupHolder(FitsInput& infile)
  :ps(0),
   pl(0),
   pf(0)
{
  attach(infile);
}

void MSPrimaryGroupHolder::attach(FitsInput& infile)
{
  detach();
  switch (infile.datatype()) {
  case FITS::SHORT:
    ps = new PrimaryGroup<Short>(infile);
    hdu_p = ps;
    break;
  case FITS::LONG:
    pl = new PrimaryGroup<FitsLong>(infile);
    hdu_p = pl;
    break;
  case FITS::FLOAT:
    pf = new PrimaryGroup<Float>(infile);
    hdu_p = pf;
    break;
  default:
    throw(AipsError("PrimaryGroupHolder(infile): unhandled FITS datatype"));
  }
}

MSPrimaryGroupHolder::~MSPrimaryGroupHolder()
{
  detach();
}

void MSPrimaryGroupHolder::detach()
{
  if (ps) delete ps;
  if (pl) delete pl;
  if (pf) delete pf;
  ps = 0;
  pl = 0;
  pf = 0;
}

MSFitsInput::MSFitsInput(const String& msFile, const String& fitsFile)
  :infile_p(0),
   msc_p(0),
   itsLog(LogOrigin("MSFitsInput", "MSFitsInput"))
{
  // First, lets verify that fitsfile exists and that it appears to be a
  // FITS file.
  File f(fitsFile);
  if (!f.exists() || !f.isReadable()) {
    itsLog << "File " << fitsFile << " does not exist or is not readable" 
	   << LogIO::EXCEPTION;
  }
  // First attempt at validating that it's a FITS file
  if (!f.isRegular()) {
    itsLog << "File " << fitsFile 
	   << " is not a plain file (maybe a directory?)" << LogIO::EXCEPTION;
  }
  // We should probably look for SIMPLE = here

  String errmsg;
  NewFile fileOK(True);
  if (!fileOK.valueOK(msFile, errmsg)) {
    itsLog << "Error in output file: " << errmsg << LogIO::EXCEPTION;
  }

  msFile_p = msFile;

  itsLog << LogIO::NORMAL << "Converting FITS file '" << fitsFile 
	 << "' to MeasurementSet '" << msFile << "'" << LogIO::POST;

  // Open the FITS file for reading
  infile_p = new FitsInput(fitsFile, FITS::Disk);
  if (infile_p) {
    if (infile_p->err() == FitsIO::IOERR) {
      itsLog << "Error reading file " << fitsFile << LogIO::EXCEPTION;
    } else if (infile_p->err()) {
      itsLog << "Error reading initial record -- exiting."<<LogIO::EXCEPTION;
    } else {
      if (checkInput(*infile_p)) {
	priGroup_p.attach(*infile_p);
      }
    }
  } else {
    itsLog << "Error opening fits file "<< fitsFile << LogIO::EXCEPTION;
  }
}

Bool MSFitsInput::readFitsFile()
{
  itsLog << LogOrigin("MSFitsInput", "readFitsFile");
  Int nField=0, nSpW=0;

  getPrimaryGroupAxisInfo();

  Bool useTSM=True;
  setupMeasurementSet(msFile_p, useTSM);
          
  // fill the OBSERVATION table
  fillObsTables();
          
  // fill the main table
  fillMSMainTable(nField, nSpW);

  // now handle the BinaryTable extensions for the subtables
  Bool haveAn=False, haveField=False, haveSpW=False;

  while (infile_p->rectype() != FITS::EndOfFile && !infile_p->err()) {
    if (infile_p->hdutype() != FITS::BinaryTableHDU) {
      itsLog << LogIO::NORMAL << "Skipping unhandled extension" << LogIO::POST;
      infile_p->skip_hdu();
    } else {
      BinaryTable binTab(*infile_p);
      // see if we can recognize the type
      String type=binTab.extname();
      itsLog << LogIO::NORMAL << "Found binary table of type " << type 
	 << " following data" << LogIO::POST;
      //itsLog << binTab <<LogIO::POST;
      if (type.contains("AN") && !haveAn) {
	haveAn=True;
	fillAntennaTable(binTab);
      } else if (type.contains("FQ") && !haveSpW) {
	haveSpW=True;
	fillSpectralWindowTable(binTab, nSpW);
      } else if (type.contains("SU") && !haveField) {
	haveField=True;
	fillFieldTable(binTab, nField);
      } else {
	itsLog << LogIO::NORMAL 
	   << "Skipping table, duplicate or unrecognized type: "
	   << type << LogIO::POST;
	binTab.fullTable("", Table::Scratch); // infile.skip_hdu();
      }
    }
  }
  if (!haveSpW) {
    // single freq. case
    fillSpectralWindowTable();
  }

  if (!haveField) {
    // single source case
    fillFieldTable(nField);
  }
  fixEpochReferences();

  if (haveAn==False) {
    itsLog << "Cannot find an AN Table. This is required." << LogIO::EXCEPTION;
  }
  fillFeedTable();

  itsLog << LogIO::NORMAL << "Flushing MS to disk" << LogIO::POST;
  return True;
} 
 
MSFitsInput::~MSFitsInput() 
{
  delete infile_p;
  delete msc_p;
}

Bool MSFitsInput::checkInput(FitsInput& infile)
{
  itsLog << LogOrigin("MSFitsInput", "checkInput");
  // Check that we have a valid UV fits file
  if (infile.rectype() != FITS::HDURecord) {
    itsLog << "Error, file does not start with standard hdu record."
       << LogIO::EXCEPTION;
  }
  if (infile.hdutype() != FITS::PrimaryGroupHDU) {
    itsLog << "Error, no primary group found" << LogIO::EXCEPTION;
  }
  FITS::ValueType dataType = infile.datatype();
  if (dataType != FITS::FLOAT && dataType != FITS::SHORT && 
      dataType != FITS::LONG) {
    itsLog << "Error, this class handles only FLOAT, SHORT and LONG data "
       << "(BITPIX=-32,16,32) at present" << LogIO::EXCEPTION;
  }
  return True;
}

void MSFitsInput::getPrimaryGroupAxisInfo()
{
  itsLog << LogOrigin("MSFitsInput", "getPrimaryGroupAxisInfo");
  // Extracts the axis related info. from the PrimaryGroup object and 
  // returns them in the form of arrays.
  const Regex trailing(" *$"); // trailing blanks
  const Int nAxis = priGroup_p.dims();
  if (nAxis < 1) {
    itsLog << "Data has no axes!" << LogIO::EXCEPTION;
  }
  nPixel_p.resize(nAxis);
  refVal_p.resize(nAxis);
  refPix_p.resize(nAxis);
  delta_p.resize(nAxis);
  coordType_p.resize(nAxis);
  for (Int i = 0; i < nAxis; i++)  {
    nPixel_p(i) = priGroup_p.dim(i);
    if (nPixel_p(i) < 0) {
      itsLog << "Axes " << i << " cannot have a negative value" 
	     << LogIO::EXCEPTION;
    }
    coordType_p(i) = priGroup_p.ctype(i);
    coordType_p(i) = coordType_p(i).before(trailing);
    refVal_p(i) = static_cast<Double>(priGroup_p.crval(i));
    refPix_p(i) = static_cast<Double>(priGroup_p.crpix(i));
    delta_p(i) = static_cast<Double>(priGroup_p.cdelt(i));
  }
  // Check if required axes are there
  if (getIndex(coordType_p, "COMPLEX") < 0) {
    itsLog << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "STOKES") < 0) {
    itsLog << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "FREQ") < 0) {
    itsLog << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "RA") < 0) && 
      (getIndex(coordType_p, "RA---SIN") < 0) && 
      (getIndex(coordType_p, "RA---NCP") < 0) && 
      (getIndex(coordType_p, "RA---SCP") < 0)) {
    itsLog << "Data does not have a RA axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "DEC") < 0) && 
      (getIndex(coordType_p, "DEC--SIN") < 0) && 
      (getIndex(coordType_p, "DEC--NCP") < 0) && 
      (getIndex(coordType_p, "DEC--SCP") < 0)) {
    itsLog << "Data does not have a DEC axis" << LogIO::EXCEPTION;
  }
  
  // Sort out the order of the polarizations and find the sort indices
  // to put them in 'standard' order: PP,PQ,QP,QQ
  const uInt iPol = getIndex(coordType_p, "STOKES");
  const uInt numCorr = nPixel_p(iPol);
  corrType_p.resize(numCorr); 
  for (uInt i = 0; i < numCorr; i++) {
    // note: 1-based ref pix
    corrType_p(i) = ifloor(refVal_p(iPol) +
			   (i+1-refPix_p(iPol))*delta_p(iPol)+0.5);
    // convert AIPS-convention Stokes description to aips++ enum
    switch (corrType_p(i)) {
    case -8:
      corrType_p(i) = Stokes::YX; break;
    case -7:
      corrType_p(i) = Stokes::XY; break;
    case -6:
      corrType_p(i) = Stokes::YY; break;
    case -5:
      corrType_p(i) = Stokes::XX; break;
    case -4:
      corrType_p(i) = Stokes::LR; break;
    case -3:
      corrType_p(i) = Stokes::RL; break;
    case -2:
      corrType_p(i) = Stokes::LL; break;
    case -1:
      corrType_p(i) = Stokes::RR; break;
    default: 
      if (corrType_p(i) < 0) {
	itsLog << "Unknown Correlation type: " << corrType_p(i) 
	       << LogIO::EXCEPTION;
      }
    }
  }
  Vector<Int> tmp(corrType_p.copy());
  // Sort the polarizations to standard order. Could probably use
  // GenSortIndirect here.
  GenSort<Int>::sort(corrType_p);
  corrIndex_p.resize(numCorr);
  // Get the sort indices to rearrange the data to standard order
  for (uInt i = 0; i < numCorr; i++) {
    for (uInt j = 0; j < numCorr; j++) {
      if (corrType_p(j) == tmp(i)) corrIndex_p[i] = j;
    }
  }

  // Figure out the correlation products from the polarizations
  corrProduct_p.resize(2, numCorr); corrProduct_p = 0;
  for (uInt i = 0; i < numCorr; i++) {
    const Stokes::StokesTypes cType = Stokes::type(corrType_p(i));
    Fallible<Int> receptor = Stokes::receptor1(cType);
    if (receptor.isValid()) {
      corrProduct_p(0,i) = receptor;
    } else {
      itsLog << "Cannot deduce receptor 1 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
    receptor = Stokes::receptor2(cType);
    if (receptor.isValid()) {
      corrProduct_p(1,i) = receptor;
    } else {
      itsLog << "Cannot deduce receptor 2 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
  }
  // Save the object name, we may need it (for single source fits)
  const FitsKeyword* kwp;
  object_p = (kwp=priGroup_p.kw(FITS::OBJECT)) ? kwp->asString() : "unknown";
  object_p=object_p.before(trailing);
  // Save the array name
  array_p = (kwp=priGroup_p.kw(FITS::TELESCOP)) ? kwp->asString() : "unknown";
  array_p=array_p.before(trailing);
  // Save the RA/DEC epoch (for ss fits)
  epoch_p = (kwp=priGroup_p.kw(FITS::EPOCH)) ? kwp->asFloat() : 2000.0;

  // Get the spectral information
  freqsys_p = MFrequency::TOPO;
  restfreq_p = 0.0;
  Record header;
  Vector<String> ignore;
  Bool ok = FITSKeywordUtil::getKeywords(header, priGroup_p.kwlist(), ignore);
  if (ok) {
    Int spectralAxis;
    Double referenceChannel, referenceFrequency, deltaFrequency;
    Vector<Double> frequencies;
    MDoppler::Types velPref;
    // Many of the following aren't used since they have been obtained
    // in other ways.
    ok = FITSSpectralUtil::fromFITSHeader(spectralAxis,
					  referenceChannel,
					  referenceFrequency,
					  deltaFrequency,
					  frequencies,
					  freqsys_p,
					  velPref,
					  restfreq_p,
					  itsLog,
					  header);
  }
}

void MSFitsInput::setupMeasurementSet(const String& MSFileName, Bool useTSM) {
  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  Int nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
  nIF_p = getIndex(coordType_p,"IF");
  if (nIF_p>=0) {
    nIF_p=nPixel_p(nIF_p);
  } else {
    nIF_p=1;
  }
  
  // Make the MS table
  TableDesc td = MS::requiredTableDesc();
  
  // Even though we know the data is going to be the same shape throughout I'll
  // still create a column that has a variable shape as this will permit MS's
  // with other shapes to be appended.
  MS::addColumnToDesc(td, MS::DATA, 2);
  // add this optional column because random group fits has a
  // weight per visibility
  MS::addColumnToDesc(td, MS::WEIGHT_SPECTRUM, 1);
  
  if (useTSM) {
    td.defineHypercolumn("TiledData",3,
 			 stringToVector(MS::columnName(MS::DATA)));
    td.defineHypercolumn("TiledFlag",3,
 			 stringToVector(MS::columnName(MS::FLAG)));
    td.defineHypercolumn("TiledWeight",2,
 			 stringToVector(MS::columnName(MS::WEIGHT_SPECTRUM)));
    td.defineHypercolumn("TiledUVW",2,
 			 stringToVector(MS::columnName(MS::UVW)));
  }
  SetupNewTable newtab(MSFileName, td, Table::New);
  
  // Set the default Storage Manager to be the Incr one
  IncrementalStMan incrStMan ("ISMData");
  newtab.bindAll(incrStMan, True);
  // bind ANTENNA2 to the standardStMan as it changes every row
  StandardStMan aipsStMan;
  newtab.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan);
  
  if (useTSM) {
    Int tileSize=nChan/10+1;
    // make the tile about 128k big
    TiledShapeStMan tiledStMan1("TiledData",
 				IPosition(3,nCorr,tileSize,
 					  16384/nCorr/tileSize));
    TiledShapeStMan tiledStMan1f("TiledFlag",
 				 IPosition(3,nCorr,tileSize,
 					   16384/nCorr/tileSize));
    TiledShapeStMan tiledStMan2("TiledWeight",
 				IPosition(2,tileSize,
 					  8192/tileSize));
    TiledColumnStMan tiledStMan3("TiledUVW",
 				 IPosition(2,3,1024));
    // Bind the DATA, FLAG & WEIGHT_SPECTRUM columns to the tiled stman
    newtab.bindColumn(MS::columnName(MS::DATA),tiledStMan1);
    newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan1f);
    newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM),tiledStMan2);
    newtab.bindColumn(MS::columnName(MS::UVW),tiledStMan3);
  } else {
    newtab.bindColumn(MS::columnName(MS::DATA),aipsStMan);
    newtab.bindColumn(MS::columnName(MS::FLAG),aipsStMan);
    newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM),aipsStMan);
    newtab.bindColumn(MS::columnName(MS::UVW),aipsStMan);
  }
  // avoid lock overheads by locking the table permanently
  TableLock lock(TableLock::PermanentLocking);
  MeasurementSet ms(newtab,lock);

  // create all subtables
  // we make new tables with 0 rows
  Table::TableOption option=Table::New;
  // Set up the subtables for the UVFITS MS
  ms.createDefaultSubtables(option);
 
  // update the references to the subtable keywords
  ms.initRefs();
 
  { // Set the TableInfo
    TableInfo& info(ms.tableInfo());
    info.setType(TableInfo::type(TableInfo::MEASUREMENTSET));
    info.setSubType(String("UVFITS"));
    info.readmeAddLine
      ("This is a measurement set Table holding astronomical observations");
  }

  ms_p=ms;
  msc_p=new MSColumns(ms_p);
}

void MSFitsInput::fillObsTables() {
  const Regex trailing(" *$"); // trailing blanks
  const FitsKeyword* kwp;
  ms_p.observation().addRow();
  String observer;
  observer = (kwp=priGroup_p.kw(FITS::OBSERVER)) ? kwp->asString() : "";
  observer=observer.before(trailing);
  MSObservationColumns msObsCol(ms_p.observation());
  msObsCol.observer().put(0,observer);
  String telescope= (kwp=priGroup_p.kw(FITS::TELESCOP)) ? kwp->asString() : "unknown";
  telescope=telescope.before(trailing);  
  msObsCol.telescopeName().put(0,telescope);
  msObsCol.scheduleType().put(0, "");
  msObsCol.project().put(0, "");

  String date;
  date = (kwp=priGroup_p.kw(FITS::DATE)) ? kwp->asString() : "";
  if (date=="") {
    // try date-obs instead
    date = (kwp=priGroup_p.kw(FITS::DATE_OBS)) ? kwp->asString() : "";
  }
  if (date=="") date = "2000-01-01";
  MVTime timeVal;
  MEpoch::Types epochRef;
  FITSDateUtil::fromFITS(timeVal,epochRef,date,"UTC");
  Vector<Double> times(2);
  times(0)=timeVal.get().getValue();
  times(1)=timeVal.get().getValue(); // change this to last time in input
  msObsCol.timeRange().put(0,times);
  Double time=timeVal.second();
  msObsCol.flagRow().put(0,False);

  // Store all keywords from the first HISTORY keyword onwards in History table
  String history = (kwp=priGroup_p.kw(FITS::HISTORY)) ? kwp->comm(): "";
  history = history.before(trailing);
  MSHistoryColumns msHisCol(ms_p.history());
  Int row=-1;
  while (history!="") {
    ms_p.history().addRow(); row++;
    msHisCol.observationId().put(row,0);
    msHisCol.time().put(row,time);
    msHisCol.priority().put(row,"NORMAL");
    msHisCol.origin().put(row,"MSFitsInput::fillObsTables");
    msHisCol.application().put(row,"ms");
    msHisCol.message().put(row,history);
    history = (kwp=priGroup_p.nextkw()) ? kwp->comm(): "";
    history = history.before(trailing);
  }
}

//
// Extract the data from the PrimaryGroup object and stick it into
// the MeasurementSet 
void MSFitsInput::fillMSMainTable(Int& nField, Int& nSpW)
{
  itsLog << LogOrigin("MSFitsInput", "fillMSMainTable");
  // Get access to the MS columns
  MSColumns& msc(*msc_p);
  const Regex trailing(" *$"); // trailing blanks

  // get the random group parameter names
  Int nParams;
  Int nGroups;
  nParams= priGroup_p.pcount(); 
  nGroups = priGroup_p.gcount(); 
  Vector<String> pType(nParams);
  for (Int i =0; i < nParams; i++) {
    pType(i) = priGroup_p.ptype(i); 
    pType(i) = pType(i).before(trailing);
  }

  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  Int nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
  
  Matrix<Complex> vis(nCorr,nChan);
  Vector<Float> sigma(nCorr), weightSpec(nChan);
  const Int nCat = 3; // three initial categories
  // define the categories
  Vector<String> cat(nCat);
  cat(0)="FLAG_CMD";
  cat(1)="ORIGINAL"; 
  cat(2)="USER"; 
  msc.flagCategory().rwKeywordSet().define("CATEGORY",cat);
  Cube<Bool> flagCat(nCorr,nChan,nCat,False);
  Matrix<Bool> flag = flagCat.xyPlane(0); // references flagCat's storage
  
  // find out the indices for U, V and W, there are several naming schemes
  Int iU,iV,iW;
  iU = getIndexContains(pType,"UU"); 
  iV = getIndexContains(pType,"VV");
  iW = getIndexContains(pType,"WW");
  if (iU < 0 || iV < 0 || iW < 0) {
    throw(AipsError("MSFitsInput: Cannot find UVW information"));
  }
  // get index for baseline
  Int iBsln = getIndex(pType, "BASELINE");
  // get indices for time
  Int iTime0 = getIndex(pType, "DATE",0);
  Int iTime1 = getIndex(pType, "DATE",1);
  // get index for source
  Int iSource = getIndex(pType, "SOURCE");
  // get index for Freq
  Int iFreq = getIndex(pType, "FREQSEL");

  receptorAngle_p.resize(1);
  nAnt_p=0;
  itsLog << LogIO::NORMAL << "Reading and writing " << nGroups
     << " visibility groups"<< LogIO::POST;
  Int row=-1;
  Double startTime, interval;
  startTime=0.0; interval=1;

  ProgressMeter meter(0.0, nGroups*1.0, "UVFITS Filler", "Groups copied", "",
 		      "", True,  nGroups/100);

  Vector<Double> uvw(3); // Move this temporary out of the loop
  Int lastAnt1, lastAnt2, lastArray, lastSpW, lastSourceId;
  lastAnt1=-1; lastAnt2=-1; lastArray=-1, lastSpW=-1; lastSourceId=-1;
  Double lastTime=0;
  Bool lastRowFlag=False;
  Float lastWeight=0.0;
  for (Int group=0; group<nGroups; group++) {
    // Read next group and
    // get time in MJD seconds
    const Double JDofMJD0=2400000.5;
    priGroup_p.read();
    Double time = priGroup_p.parm(iTime0); 
    time -= JDofMJD0;
    if (iTime1>=0) time += priGroup_p.parm(iTime1);
    Float baseline = priGroup_p.parm(iBsln); 
    uvw(0) = priGroup_p.parm(iU);
    uvw(1) = priGroup_p.parm(iV);
    uvw(2) = priGroup_p.parm(iW);
    time  *= C::day; 

    // make a guess at the integration time
    if (row<0) startTime = time;
    if (time > startTime) {
      interval=time-startTime;
      msc.interval().fillColumn(interval);
      msc.exposure().fillColumn(interval);
      startTime = DBL_MAX; // do this only once
    }

    Int array = Int(100.0*(baseline - Int(baseline)+0.001));
    Int ant1 = Int(baseline)/256; 
    nAnt_p = max(nAnt_p,ant1);
    Int ant2 = Int(baseline) - ant1*256; 
    nAnt_p = max(nAnt_p,ant2);
    ant1--; ant2--; // make 0-based
    
    // Convert U,V,W from units of seconds to meters
    uvw *= C::c;

    Int count = 0;

    for (Int ifno=0; ifno<max(1,nIF_p); ifno++) {
      // IFs go to separate rows in the MS
      ms_p.addRow(); 
      row++;
      for (Int chan=0; chan<nChan; chan++) {
 	weightSpec(chan)=0.0; 
 	Int nWt=0;
 	for (Int pol=0; pol<nCorr; pol++) {
 	  Float visReal = priGroup_p(count++);
 	  Float visImag = priGroup_p(count++); 
 	  Float wt = priGroup_p(count++); 
 	  flag(corrIndex_p[pol],chan) = ToBool(wt<=0);
 	  if (wt>0) {nWt++; weightSpec(chan)+=wt;}
 	  vis(corrIndex_p[pol],chan) = Complex(visReal,visImag);
 	}
 	if (nWt>0) weightSpec(chan)/=Float(nWt);
      }
      // fill in values for all the unused columns
      if (row==0) {
 	msc.exposure().put(row,interval);
 	msc.feed1().put(row,0);
 	msc.feed2().put(row,0);
 	msc.flagRow().put(row,False);
 	lastRowFlag=False;
 	msc.interval().put(row,interval);
 	msc.scanNumber().put(row,0);
 	msc.processorId().put(row,-1);
 	msc.observationId().put(row,0);
 	msc.stateId().put(row,-1);
 	Vector<Float> tmp(nCorr); tmp=1.0;
 	msc.sigma().put(row,tmp);
 	msc.weight().put(row,tmp);
 	lastWeight=1.0;
      }
      msc.data().put(row,vis);
      // single channel case: make weight and weightSpectrum identical.
      // multichannel case: weight should not be used.
      if (nChan==1) { 
 	Vector<Float> weight(nCorr); weight=weightSpec(0);
 	if (weight(0)!=lastWeight) {
 	  msc.weight().put(row,weight);
 	  lastWeight=weight(0);
 	}
      }
      msc.weightSpectrum().put(row,weightSpec); 
      msc.flag().put(row,flag);
      msc.flagCategory().put(row,flagCat);
      Bool rowFlag=allEQ(flag,True);
      if (rowFlag!=lastRowFlag) {
 	msc.flagRow().put(row,rowFlag);
 	lastRowFlag=rowFlag;
      }

      if (ant1!=lastAnt1) {
 	msc.antenna1().put(row,ant1);
 	lastAnt1=ant1;
      }
      if (array!=lastArray) {
 	msc.observationId().put(row,array);
 	lastArray=array;
      }
      // Always put antenna2 since it is bound to the
      // aipsStMan and is assumed to change every
      // row
      msc.antenna2().put(row,ant2);
      if (time!=lastTime) {
 	msc.time().put(row,time);
 	msc.timeCentroid().put(row,time);
 	lastTime=time;
      }
      msc.uvw().put(row,uvw);
      
      // determine the spectralWindowId
      Int spW = ifno;
      if (iFreq>=0) {
 	spW = (Int)priGroup_p.parm(iFreq) - 1; // make 0-based
 	if (nIF_p>0) {
 	  spW *=nIF_p; 
 	  spW+=ifno;
 	}
      }
      if (spW!=lastSpW) {
 	msc.dataDescId().put(row,spW);
 	nSpW = max(nSpW, spW+1);
 	lastSpW=spW;
      }
    
      // store the sourceId 
      Int sourceId = 0;
      if (iSource>=0) {
 	// make 0-based
 	sourceId = (Int)priGroup_p.parm(iSource) - 1; 
      }
      if (sourceId!=lastSourceId) {
 	msc.fieldId().put(row,sourceId);
 	nField = max(nField, sourceId+1);
 	lastSourceId=sourceId;
      }
    }
    meter.update((group+1)*1.0);
  }
  // fill the receptorAngle with defaults, just in case there is no AN table
  receptorAngle_p=0;
  // set the Measure References
}

void MSFitsInput::fillAntennaTable(BinaryTable& bt)
{
  itsLog << LogOrigin("MSFitsInput()", "fillAntennaTable");
  const Regex trailing(" *$"); // trailing blanks
  TableRecord btKeywords=bt.getKeywords();
  if (nAnt_p>bt.nrows()) {
    itsLog << "Not all antennas found in antenna table:"
       << " expected " << nAnt_p << ", found " << bt.nrows()
       << LogIO::EXCEPTION;
  }
  Int nAnt=bt.nrows();
  receptorAngle_p.resize(2*nAnt);
  Vector<Double> arrayXYZ(3);
  arrayXYZ=0.0;
  if(!btKeywords.isDefined("ARRAYX")||!btKeywords.isDefined("ARRAYY")||
     !btKeywords.isDefined("ARRAYZ")) {
    throw(AipsError("MSFitsInput: Illegal AN file: no antenna positions"));
  }
   arrayXYZ(0)=bt.getKeywords().asdouble("ARRAYX");
   arrayXYZ(1)=bt.getKeywords().asdouble("ARRAYY");
   arrayXYZ(2)=bt.getKeywords().asdouble("ARRAYZ");
   // itsLog << LogIO::NORMAL << "number of antennas ="<<nAnt<<LogIO::POST;
   // itsLog << LogIO::NORMAL << "array ref pos:"<<arrayXYZ<<LogIO::POST;

   // Since we cannot write these quantities, we cannot rely upon
   // their presence in any UVFITS file that we read:
   Double rdate=0.0;
   String srdate;
   if(btKeywords.isDefined("RDATE")) {
     srdate=btKeywords.asString("RDATE");
   }
   Double gst=0.0;
   if(btKeywords.isDefined("GSTIA0")) {
     gst=btKeywords.asdouble("GSTIA0")*C::degree;
   }
   Double degpdy=0.0;
   if(btKeywords.isDefined("DEGPDY")) {
     degpdy=btKeywords.asdouble("DEGPDY");
   }
   String timsys="TAI";
   if (btKeywords.isDefined("TIMSYS")) {
     timsys=btKeywords.asString("TIMSYS");
     timsys=timsys.before(trailing);
   }
   MVTime timeVal;
   MEpoch::Types epochRef;
   FITSDateUtil::fromFITS(timeVal,epochRef,srdate,timsys);
   // convert to canonical form
   timsys=MEpoch::showType(epochRef);
   rdate=timeVal.second(); // MJD seconds
   String arrnam="Unknown";
   if (btKeywords.isDefined("ARRNAM")) {
     arrnam=btKeywords.asString("ARRNAM");
   }
   // store the time keywords 
   ms_p.antenna().rwKeywordSet().define(String("RDATE"),rdate);
   ms_p.antenna().rwKeywordSet().define(String("GSTIA0"),gst);
   ms_p.antenna().rwKeywordSet().define(String("DEGPDY"),degpdy);
   ms_p.antenna().rwKeywordSet().define(String("TIMSYS"),timsys);
   //save value to set time reference frame elsewhere
   timsys_p=timsys;
   // Fill in some likely values
   Float diameter=25;
   if (array_p=="ATCA") diameter=22;
  
   Table anTab=bt.fullTable("",Table::Scratch);
   MSAntennaColumns& ant(msc_p->antenna());
   ROScalarColumn<String> name(anTab,"ANNAME");
   ROScalarColumn<Int> id(anTab,"NOSTA");
   ROScalarColumn<Int> mountType(anTab,"MNTSTA");
   ROScalarColumn<Float> offset(anTab,"STAXOF");
   ROScalarColumn<Float> polangleA(anTab,"POLAA");
   ROScalarColumn<Float> polangleB(anTab,"POLAB");
   ROArrayColumn<Double> antXYZ(anTab,"STABXYZ");

   // Prepare handling of UVFITS Antenna position coord conventions:
   // VLA requires rotation of local coords:
   Bool doVLARot=(array_p=="VLA");
   // initialize rotation matrix with zero rotation
   Matrix<Double> posRot=Rot3D(0,0.0);  
   if ( doVLARot ) {
     // Form rotation around Z axis by VLA longitude=atan(refy/refx)
     Double vlaLong=atan2(arrayXYZ(1),arrayXYZ(0));
     posRot=Rot3D(2,vlaLong);  // Applied to each ant position below
   }
   // All "VLBI" (==arrayXYZ<1000) requires y-axis reflection: 
   //  (ATCA looks like "VLBI" in UVFITS, but is already correct)
   Bool doVLBIRefl= ((array_p!="ATCA") && allLE(abs(arrayXYZ),1000.0));


   // add antenna info to table
   ant.setPositionRef(MPosition::ITRF);
   Int row=ms_p.antenna().nrow()-1;
   for (Int i=0; i<nAnt; i++) {
     ms_p.antenna().addRow(); row++;
     ant.dishDiameter().put(row,diameter); 
     String mount;
     switch (mountType(i)) {
     case 0: mount="ALT-AZ"; break;
     case 1: mount="EQUATORIAL"; break;
     case 2: mount="X-Y"; break;
     case 3: mount="ORBITING"; break;
     case 4: mount="BIZARRE"; break;
     default: mount="UNKNOWN"; break;
     }
     ant.flagRow().put(row,False);
     ant.mount().put(row,mount);
     ant.name().put(row,String::toString(id(i)));
     Vector<Double> offsets(3); offsets=0.; offsets(0)=offset(i);
     ant.offset().put(row,offsets);
     ant.station().put(row,name(i));
     ant.type().put(row,"GROUND-BASED");

     // Do UVFITS-dependent position corrections:
     // ROArrayColumn antXYZ(i) may need coord transform; do it in corXYZ:
     Vector<Double> corXYZ=antXYZ(i);
     // If nec, rotate coordinates out of local VLA frame to ITRF
     if ( doVLARot ) corXYZ=product(posRot,corXYZ);
     // If nec, reflect y-coord to yield right-handed geocentric:
     if ( doVLBIRefl ) corXYZ(1)=-corXYZ(1);

     ant.position().put(row,arrayXYZ+antXYZ(i));
     // store the angle for use in the feed table
     receptorAngle_p(2*i+0)=polangleA(i)*C::degree;
     receptorAngle_p(2*i+1)=polangleB(i)*C::degree;
   }

   // store these items in non-standard keywords for now
   ant.name().rwKeywordSet().define("ARRAY_NAME",arrnam);
   ant.position().rwKeywordSet().define("ARRAY_POSITION",arrayXYZ);
}

void MSFitsInput::fillSpectralWindowTable(BinaryTable& bt, Int nSpW)
{
  itsLog << LogOrigin("MSFitsInput()", "fillSpectralWindowTable");
  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());
  Int iFreq = getIndex(coordType_p, "FREQ");
  Int nChan = nPixel_p(iFreq);
  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));

  // fill out the polarization info (only single entry allowed in fits input)
  ms_p.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,corrType_p);
  msPol.corrProduct().put(0,corrProduct_p);
  msPol.flagRow().put(0,False);

  Table fqTab=bt.fullTable("",Table::Scratch);
  Int nRow=fqTab.nrow();
  ROScalarColumn<Int> colFrqSel(fqTab,"FRQSEL");
  Matrix<Double> ifFreq(nIF_p,nRow);
  Matrix<Float> chWidth(nIF_p,nRow);
  Matrix<Float> totalBandwidth(nIF_p,nRow);
  // The type of the column changes according to the number of entries
  if (nIF_p==1) {
    ROScalarColumn<Double> colIFFreq(fqTab,"IF FREQ");
    ROScalarColumn<Float> colChWidth(fqTab,"CH WIDTH");
    ROScalarColumn<Float> colTotalBandwidth(fqTab,"TOTAL BANDWIDTH");
    for (Int i=0; i<nRow; i++) {
      ifFreq(0,i)=colIFFreq(i);
      chWidth(0,i)=colChWidth(i);
      totalBandwidth(0,i)=colTotalBandwidth(i);
    }
  } else {
    ROArrayColumn<Double> colIFFreq(fqTab,"IF FREQ");
    ROArrayColumn<Float> colChWidth(fqTab,"CH WIDTH");
    ROArrayColumn<Float> colTotalBandwidth(fqTab,"TOTAL BANDWIDTH");
    colIFFreq.getColumn(ifFreq);
    colChWidth.getColumn(chWidth);
    colTotalBandwidth.getColumn(totalBandwidth);
  }
  for (Int spw=0; spw<nSpW; spw++) {
    ms_p.spectralWindow().addRow();
    ms_p.dataDescription().addRow();
    
    msDD.spectralWindowId().put(spw,spw);
    msDD.polarizationId().put(spw,0);
    msDD.flagRow().put(spw,False);
    Int ifc=0;
    Int freqGroup = 0;
    if (nIF_p>0) {
      ifc=spw%nIF_p;
      freqGroup = spw/nIF_p;
    }
    Int fqRow=spw/max(1,nIF_p);
    if (fqRow != colFrqSel(fqRow)-1) 
      itsLog << LogIO::SEVERE << "Trouble interpreting FQ table, id's may be wrong" << LogIO::POST; 
    msSpW.name().put(spw,"none");
    msSpW.ifConvChain().put(spw,ifc);
    msSpW.numChan().put(spw,nChan);
    Double refChan = refPix_p(iFreq);
    Double refFreq=refVal_p(iFreq)+ifFreq(ifc,fqRow);
    Double chanBandwidth=chWidth(ifc,fqRow);
    Vector<Double> chanFreq(nChan),resolution(nChan);
    for (Int i=0; i < nChan; i++) {
      chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
    }
    resolution=abs(chanBandwidth);
    msSpW.chanFreq().put(spw,chanFreq);
    msSpW.chanWidth().put(spw,resolution);
    msSpW.effectiveBW().put(spw,resolution);
    msSpW.refFrequency().put(spw,refFreq);
    msSpW.resolution().put(spw,resolution);
    msSpW.totalBandwidth().put(spw,totalBandwidth(ifc,fqRow));
    if (chanBandwidth>0) {
      msSpW.netSideband().put(spw,1);
    } else {
      msSpW.netSideband().put(spw,-1);
    }
    msSpW.freqGroup().put(spw,freqGroup);
    msSpW.freqGroupName().put(spw,"none");
    msSpW.flagRow().put(spw,False);
    // set the reference frames for frequency
    msSpW.measFreqRef().put(spw,freqsys_p);
  }
}

void MSFitsInput::fillSpectralWindowTable()
{
  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());
  Int iFreq = getIndex(coordType_p, "FREQ");
  Int nChan = nPixel_p(iFreq);
  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));

  // fill out the polarization info (only single entry allowed in fits input)
  ms_p.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,corrType_p);
  msPol.corrProduct().put(0,corrProduct_p);
  msPol.flagRow().put(0,False);

  Int spw=0;
  ms_p.spectralWindow().addRow();
  ms_p.dataDescription().addRow();

  msDD.spectralWindowId().put(spw,spw);
  msDD.polarizationId().put(spw,0);
  msDD.flagRow().put(spw,False); 

  msSpW.name().put(spw,"none");
  msSpW.ifConvChain().put(spw,0);
  msSpW.numChan().put(spw,nChan);
  Double refChan = refPix_p(iFreq);
  Double refFreq=refVal_p(iFreq);
  Double chanBandwidth=delta_p(iFreq);
  Vector<Double> chanFreq(nChan),resolution(nChan);
  for (Int i=0; i < nChan; i++) {
    chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
  }
  resolution=chanBandwidth;
  msSpW.chanFreq().put(spw,chanFreq);
  msSpW.chanWidth().put(spw,resolution);
  msSpW.effectiveBW().put(spw,resolution);
  msSpW.refFrequency().put(spw,refFreq);
  msSpW.resolution().put(spw,resolution);
  msSpW.totalBandwidth().put(spw,abs(nChan+chanBandwidth));
  if (chanBandwidth>0) {
    msSpW.netSideband().put(spw,1);
  } else {
    msSpW.netSideband().put(spw,-1);
  }
  msSpW.freqGroup().put(spw,0);
  msSpW.freqGroupName().put(spw,"none");
  msSpW.flagRow().put(spw,False);
  // set the reference frames for frequency
  msSpW.measFreqRef().put(spw,freqsys_p);
}

void MSFitsInput::fillFieldTable(BinaryTable& bt, Int nField)
{
  itsLog << LogOrigin("MSFitsInput()", "fillFieldTable");
  MSFieldColumns& msField(msc_p->field());
  Table suTab=bt.fullTable("",Table::Scratch);
  ROScalarColumn<Int> id(suTab,"ID. NO.");
  ROScalarColumn<String> name(suTab,"SOURCE");
  ROScalarColumn<String> code(suTab,"CALCODE");
  // ROScalarColumn<Float> iflux(suTab,"IFLUX"); // etc Q, U, V (Jy)
  ROScalarColumn<Double> ra(suTab,"RAEPO");    //degrees
  ROScalarColumn<Double> dec(suTab,"DECEPO");  //degrees
  ROScalarColumn<Double> epoch(suTab,"EPOCH"); //years
  ROScalarColumn<Double> pmra(suTab,"PMRA");   //deg/day
  ROScalarColumn<Double> pmdec(suTab,"PMDEC"); //deg/day
  if (Int(suTab.nrow())<nField) {
    itsLog << LogIO::NORMAL 
       << "Input Source id's not sequential, adding empty rows in output"
       << LogIO::POST;
  }
  Int outRow=-1;

  // set the DIRECTION MEASURE REFERENCE for appropriate columns
  MDirection::Types epochRef=MDirection::J2000;
  if (nearAbs(epoch(id(0)-1),1950.0,0.01)) {
    epochRef=MDirection::B1950;
  }
  msc_p->setDirectionRef(epochRef);
  
  for (Int inRow=0; inRow<(Int)suTab.nrow(); inRow++) {
    Int fld = id(inRow)-1;
    // add empty rows until the row number in the output matches the source id
    while (fld > outRow) {
      ms_p.field().addRow(); outRow++;
    }
    msField.sourceId().put(fld,-1); // source table not yet filled in
    msField.code().put(fld,code(fld));
    msField.name().put(fld,name(fld));
    Int numPoly = 1;
    if (pmra(fld)==0 && pmdec(fld)==0) {
      numPoly = 0;
    }
    Vector<MDirection> radecMeas(numPoly+1);
    radecMeas(0).set(MVDirection(ra(fld)*C::degree,dec(fld)*C::degree));
    if (numPoly==1) {
      radecMeas(1).set(MVDirection(pmra(fld)*C::degree/C::day,
 				   pmdec(fld)*C::degree/C::day));
    }
    msField.numPoly().put(fld,numPoly);
    msField.delayDirMeasCol().put(fld,radecMeas);
    msField.phaseDirMeasCol().put(fld,radecMeas);
    msField.referenceDirMeasCol().put(fld,radecMeas);

    // Note: this code code attempts to interpret possible FITS usage
    // of the epoch. Here it serves as both the coordinate epoch reference
    // and the 'zero-point' for the proper motion parameters.
    // Normally the Time column in the MSField table would contain
    // the observation time for which the position is accurate (i.e. 
    // zero point for rates). The coordinate epoch would be specified 
    // separately.
    // Need to convert epoch in years to MJD time
    if (nearAbs(epoch(fld),2000.0,0.01)) {
      msField.time().put(fld, MeasData::MJD2000*C::day);
      // assume UTC epoch
    } else if (nearAbs(epoch(fld),1950.0,0.01)) {
      msField.time().put(fld, MeasData::MJDB1950*C::day);
    } else {
      itsLog << LogIO::SEVERE  << " Cannot handle epoch in SU table: " << 
 	epoch(fld) <<LogIO::POST;
    }
    msField.flagRow().put(fld,False);
  }
}

// single source fits case
void MSFitsInput::fillFieldTable(Int nField)
{
  itsLog << LogOrigin("MSFitsInput()", "fillFieldTable");
  // some UVFITS files have the source number set, but have no SU
  // table. We will assume there is only a single source in that case
  // and set all fieldId's back to zero
  if (nField>1) {
    msc_p->fieldId().fillColumn(0);
  }
  // set the DIRECTION MEASURE REFERENCE for appropriate columns
  MDirection::Types epochRef=MDirection::J2000;
  if (nearAbs(epoch_p,1950.0,0.01)) epochRef=MDirection::B1950;
  msc_p->setDirectionRef(epochRef);


  MSFieldColumns& msField(msc_p->field());
  ms_p.field().addRow();
  Int fld=0;
  msField.sourceId().put(fld,-1); // source table not used
  msField.code().put(fld," ");
  msField.name().put(fld,object_p);
  Vector<MDirection> radecMeas(1);
  radecMeas(0).set(MVDirection(refVal_p(getIndex(coordType_p,"RA"))*C::degree,
 			       refVal_p(getIndex(coordType_p,"DEC"))*C::degree));
  msField.numPoly().put(fld,0);
  msField.delayDirMeasCol().put(fld,radecMeas);
  msField.phaseDirMeasCol().put(fld,radecMeas);
  msField.referenceDirMeasCol().put(fld,radecMeas);

  // Need to convert epoch in years to MJD time
  if (nearAbs(epoch_p,2000.0,0.01)) {
    msField.time().put(fld, MeasData::MJD2000*C::day);
    // assume UTC epoch
  } else if (nearAbs(epoch_p,1950.0,0.01)) {
    msField.time().put(fld, MeasData::MJDB1950*C::day);
  } else {
    itsLog << LogIO::SEVERE  << " Cannot handle epoch in Pr Group header: "<< epoch_p <<LogIO::POST;
  }
}

void MSFitsInput::fillFeedTable() {
  MSFeedColumns& msfc(msc_p->feed());

  // find out the POLARIZATION_TYPE
  // In the fits files we handle there can be only a single, uniform type
  // of polarization so the following should work.
  MSPolarizationColumns& msPolC(msc_p->polarization());
  Int numCorr=msPolC.numCorr()(0);
  Vector<String> rec_type(2); rec_type="";
  if (corrType_p(0)>=Stokes::RR && corrType_p(numCorr-1)<=Stokes::LL) {
    rec_type(0)="R"; rec_type(1)="L";
  }
  if (corrType_p(0)>=Stokes::XX && corrType_p(numCorr-1)<=Stokes::YY) {
    rec_type(0)="X"; rec_type(1)="Y";
  }

  Matrix<Complex> polResponse(2,2); 
  polResponse=0.; polResponse(0,0)=polResponse(1,1)=1.;
  Matrix<Double> offset(2,2); offset=0.;
  Vector<Double> position(3); position=0.;

  // fill the feed table
  Int row=-1;
  for (Int ant=0; ant<nAnt_p; ant++) {
    ms_p.feed().addRow(); row++;
    msfc.antennaId().put(row,ant);
    msfc.beamId().put(row,-1);
    msfc.feedId().put(row,0);
    msfc.interval().put(row,0.);
    //    msfc.phasedFeedId().put(row,-1);
    msfc.spectralWindowId().put(row,-1); // all
    msfc.time().put(row,0.);
    msfc.numReceptors().put(row,2);
    msfc.beamOffset().put(row,offset);
    msfc.polarizationType().put(row,rec_type);
    msfc.polResponse().put(row,polResponse);
    msfc.position().put(row,position);
    msfc.receptorAngle().put(row,receptorAngle_p(Slice(2*ant,2)));
  }
}

void MSFitsInput::fixEpochReferences() {
  itsLog << LogOrigin("MSFitsInput()", "fixEpochReferences");
  if (timsys_p=="IAT") timsys_p="TAI";
  if (timsys_p=="UTC" || timsys_p=="TAI") {
    if (timsys_p=="UTC") msc_p->setEpochRef(MEpoch::UTC, False);
    if (timsys_p=="TAI") msc_p->setEpochRef(MEpoch::TAI, False);
  } else {
    if (timsys_p!="")
      itsLog << LogIO::SEVERE << "Unhandled time reference frame: "<<timsys_p<<LogIO::POST;
  }
}
// Local Variables: 
// compile-command: "gmake MSFitsInput"
// End: 
