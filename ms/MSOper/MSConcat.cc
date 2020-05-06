//# MSConcat.cc: A class for concatenating MeasurementSets.
//# Copyright (C) 2000,2002,2003
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

#include <casacore/ms/MSOper/MSConcat.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/ms/MeasurementSets/MSAntenna.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSFeed.h>
#include <casacore/ms/MeasurementSets/MSField.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/tables/Tables/TabVecMath.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/Directory.h>
#include <algorithm>

namespace casacore {

MSConcat::MSConcat(MeasurementSet& ms):
  MSColumns(ms),
  itsMS(ms),
  itsFixedShape(isFixedShape(ms.tableDesc()))
{
  itsDirTol=Quantum<Double>(1.0, "mas");
  itsFreqTol=Quantum<Double>(1.0, "Hz");
  itsWeightScale = 1.;
  itsRespectForFieldName = False;
  doSource_p=False;
  doObsA_p = doObsB_p = False;
  doProcA_p = doProcB_p = False;
}

IPosition MSConcat::isFixedShape(const TableDesc& td) {
  IPosition fixedShape(0);
  Bool isFixed = False;
  const Vector<String> hypercolumnNames=td.hypercolumnNames();
  const uInt nHyperCols = hypercolumnNames.nelements();
  Vector<String> dataColNames,coordColNames,idColNames;
  uInt hc = 0;
  while (isFixed == False && hc < nHyperCols) {
    td.hypercolumnDesc(hypercolumnNames(hc), dataColNames, coordColNames,
		       idColNames);
    const uInt nDataCol = dataColNames.nelements();
    uInt dc = 0;
    while (isFixed == False && dc < nDataCol) {
      const String& dataColName = dataColNames(dc);
      // The order of these if conditions is important as I am trying to get
      // the biggest possible fixed shape.
      if (dataColName == MS::columnName(MS::FLAG_CATEGORY) ||
	  dataColName == MS::columnName(MS::DATA) ||
	  dataColName == MS::columnName(MS::FLAG) ||
	  dataColName == MS::columnName(MS::SIGMA_SPECTRUM) ||
	  dataColName == MS::columnName(MS::WEIGHT_SPECTRUM) ||
	  dataColName == MS::columnName(MS::CORRECTED_WEIGHT_SPECTRUM) ||
	  dataColName == MS::columnName(MS::FLOAT_DATA) ||
	  dataColName == MS::columnName(MS::CORRECTED_DATA) ||
	  dataColName == MS::columnName(MS::MODEL_DATA) ||
	  dataColName == MS::columnName(MS::LAG_DATA) ||
	  dataColName == MS::columnName(MS::SIGMA) ||
	  dataColName == MS::columnName(MS::WEIGHT) ||
	  dataColName == MS::columnName(MS::VIDEO_POINT)) {
	const ColumnDesc& colDesc = td.columnDesc(dataColNames(dc));
	isFixed = colDesc.isFixedShape();
	if (isFixed) fixedShape = colDesc.shape();
      }
      dc++;
    }
    hc++;
    dataColNames.resize(0);
    coordColNames.resize(0);
    idColNames.resize(0);
  }
  return fixedShape;
}

  Bool MSConcat::checkEphIdInField(const MSFieldColumns& otherFldCol) const {
  // test if this MS FIELD table has an ephID column
  if(!itsMS.field().actualTableDesc().isColumn(MSField::columnName(MSField::EPHEMERIS_ID))){
    // if not, test if the other MS uses ephem objects
    Bool usesEphems = False;
    for(rownr_t i=0; i<otherFldCol.nrow(); ++i){
      if(!otherFldCol.ephemPath(i).empty()){
	usesEphems = True;
	break;
      }
    }
    if(usesEphems){ // if yes, the ephID column needs to be added to this MS FIELD table
      return False;
    }
  }
  return True;
}


  void MSConcat::virtualconcat(MeasurementSet& otherMS,
			       const Bool checkShapeAndCateg,
			       const String& obsidAndScanTableName)
{
  LogIO log(LogOrigin("MSConcat", "virtualconcat", WHERE));


  Bool reindexObsidAndScan=!obsidAndScanTableName.empty();

  // check if certain columns are present and set flags accordingly
  Bool doCorrectedData=False, doModelData=False;
  Bool doFloatData=False;

  if (itsMS.tableDesc().isColumn("FLOAT_DATA") &&
      otherMS.tableDesc().isColumn("FLOAT_DATA"))
    doFloatData=True;
  else if (itsMS.tableDesc().isColumn("FLOAT_DATA") &&
	   !otherMS.tableDesc().isColumn("FLOAT_DATA")){
    log << itsMS.tableName()
	<< " has FLOAT_DATA column but not " << otherMS.tableName()
	<< LogIO::EXCEPTION;
    log << "Cannot concatenate these MSs yet...you may split the corrected column of the SD as a work around."
	<< LogIO::EXCEPTION;
  }
  if (itsMS.tableDesc().isColumn("MODEL_DATA") &&
      otherMS.tableDesc().isColumn("MODEL_DATA"))
    doModelData=True;
  else if (itsMS.tableDesc().isColumn("MODEL_DATA") &&
	   !otherMS.tableDesc().isColumn("MODEL_DATA")){
    log << itsMS.tableName()
	<< " has MODEL_DATA column but not " << otherMS.tableName()
	<< LogIO::EXCEPTION;
    log << "You may wish to create this column by loading "
	<< otherMS.tableName()
	<< " in imager or calibrater "
	<< LogIO::EXCEPTION;
  }
  if (itsMS.tableDesc().isColumn("CORRECTED_DATA") &&
      otherMS.tableDesc().isColumn("CORRECTED_DATA"))
    doCorrectedData=True;
  else if (itsMS.tableDesc().isColumn("CORRECTED_DATA") &&
	   !otherMS.tableDesc().isColumn("CORRECTED_DATA"))
    log << itsMS.tableName()
	<<" has CORRECTED_DATA column but not " << otherMS.tableName()
	<< LogIO::EXCEPTION;

  {
    const MSFieldColumns otherMSFCols(otherMS.field());
    if(!checkEphIdInField(otherMSFCols)){
      log << "EPHEMERIS_ID column missing in FIELD table of MS " << itsMS.tableName()
	  << LogIO::EXCEPTION;
    }
  }

  if(checkShapeAndCateg){
    // verify that shape of the two MSs as described in POLARISATION, SPW, and DATA_DESCR
    //   is the same
    if (otherMS.nrow() > 0) {
      if (itsFixedShape.nelements() > 0) {
	const MSPolarizationColumns otherPolCols(otherMS.polarization());
	const MSSpWindowColumns otherSpwCols(otherMS.spectralWindow());
	const MSDataDescColumns otherDDCols(otherMS.dataDescription());
	const uInt nShapes = otherDDCols.nrow();
	for (uInt s = 0; s < nShapes; ++s) {
	  checkShape(getShape(otherDDCols, otherSpwCols, otherPolCols, s));
	}
      }
      const MSMainColumns otherMainCols(otherMS);
      checkCategories(otherMainCols);
    }
  }

  // merge STATE
  Block<uInt> newStateIndices; // INTO TABLE
  Bool doState = False;
  // STATE is a required subtable but can be empty in which case the state id in the main table is -1
  Bool itsStateNull = (itsMS.state().isNull() || (itsMS.state().nrow() == 0));
  Bool otherStateNull = (otherMS.state().isNull() || (otherMS.state().nrow() == 0));

  if(itsStateNull && otherStateNull){
    log << LogIO::NORMAL << "No valid state tables present. Result won't have one either." << LogIO::POST;
  }
  else if(itsStateNull && !otherStateNull){
    log << LogIO::WARN << itsMS.tableName() << " does not have a valid state table," << endl
	<< "  the MS to be appended, however, has one. Result won't have one."
	<< LogIO::POST;
    doState = True; // i.e. the appended MS Main table state id will have to be set to -1
  }
  else if(!itsStateNull && otherStateNull){
    log << LogIO::WARN << itsMS.tableName() << " does have a valid state table," << endl
	<< "  the MS to be appended, however, doesn't. Result won't have one."
	<< LogIO::POST;
    doState = True; // i.e. itsMS Main table state id will have to be set to -1

    RowNumbers delrows(itsMS.state().nrow());
    indgen(delrows);
    itsMS.state().removeRow(RowNumbers(delrows));
  }
  else{ // both state tables are filled
    const rownr_t oldStateRows = itsMS.state().nrow();
    newStateIndices = copyState(otherMS.state());
    const rownr_t addedRows = itsMS.state().nrow() - oldStateRows;
    const rownr_t matchedRows = otherMS.state().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the state subtable" << LogIO::POST;
    doState = True; // state id entries in the main table will have to be modified for otherMS
  }

  //See if there is a SOURCE table and concatenate and reindex it
  {
    rownr_t oldSRows = itsMS.source().nrow();
    copySource(otherMS);
    if(Table::isReadable(itsMS.sourceTableName())){
      rownr_t addedRows =  itsMS.source().nrow() - oldSRows;
      if(addedRows>0){
	log << "Added " << addedRows
	    << " rows to the source subtable" << LogIO::POST;
      }
    }
  }

  // DATA_DESCRIPTION
  rownr_t oldRows = itsMS.dataDescription().nrow();
  rownr_t oldSPWRows = itsMS.spectralWindow().nrow();
  const Block<uInt> newDDIndices = copySpwAndPol(otherMS.spectralWindow(),
						 otherMS.polarization(),
						 otherMS.dataDescription());
  {
    rownr_t addedRows = itsMS.dataDescription().nrow() - oldRows;
    rownr_t matchedRows = otherMS.dataDescription().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the data description subtable" << LogIO::POST;
    addedRows = itsMS.spectralWindow().nrow() - oldSPWRows;
    matchedRows = otherMS.spectralWindow().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the spectral window subtable" << LogIO::POST;
  }

  // correct the spw entries in the SOURCE table and remove redundant rows
  oldRows = itsMS.source().nrow();
  updateSource();
  if(Table::isReadable(itsMS.sourceTableName())){
    rownr_t removedRows =  oldRows - itsMS.source().nrow();
    if(removedRows>0){
      log << "Removed " << removedRows
	  << " redundant rows from the source subtable" << LogIO::POST;
    }
  }

  // merge ANTENNA and FEED
  oldRows = itsMS.antenna().nrow();
  rownr_t oldFeedRows = itsMS.feed().nrow();
  const Block<uInt> newAntIndices = copyAntennaAndFeed(otherMS.antenna(),
						       otherMS.feed());
  Bool antIndexTrivial = True;
  for(uInt ii=0; ii<newAntIndices.size(); ii++){
    //cout << "i, newAntIndices(i) " << ii << " " << newAntIndices[ii] << endl;
    if(newAntIndices[ii]!=ii){
      antIndexTrivial=False;
      break;
    }
  }
  {
    rownr_t addedRows = itsMS.antenna().nrow() - oldRows;
    rownr_t matchedRows = otherMS.antenna().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the antenna subtable" << endl;
    addedRows = itsMS.feed().nrow() - oldFeedRows;
    log << "Added " << addedRows
	<< " rows to the feed subtable" << endl;
  }


  // FIELD
  oldRows = itsMS.field().nrow();
  const Block<uInt> newFldIndices = copyField(otherMS);
  {
    const rownr_t addedRows = itsMS.field().nrow() - oldRows;
    const rownr_t matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the field subtable" << LogIO::POST;
    if(matchedRows>0){ // may have to consolidate SOURCE IDs
      if(updateSource2()){
	log << "Consolidated Source IDs in the source subtable." << LogIO::POST;
      }
    }
  }

  // OBSERVATION
  copyObservation(otherMS.observation(), True);

  // PROCESSOR
  copyProcessor(otherMS.processor(), True);

  // POINTING
  if(!antIndexTrivial){
    copyPointingB(otherMS.pointing(), newAntIndices);
  }

  // SYSCAL
  copySysCal(otherMS.sysCal(), newAntIndices);

  // WEATHER
  copyWeather(otherMS.weather(), newAntIndices);

  /////////////////////////////////////////////////////

  // copying all subtables over to otherMS
  // will need to be done when creating the MMS from the concatenated MSs

  //////////////////////////////////////////////////////

  MSMainColumns mainCols(itsMS);
  MSMainColumns otherMainCols(otherMS);

  const rownr_t otherRows = otherMS.nrow();
  const rownr_t theseRows = itsMS.nrow();

  // create column objects for those columns which potentially need to be modified

  ArrayColumn<Complex> otherData;
  ArrayColumn<Float> otherFloatData;
  ArrayColumn<Complex> otherModelData, otherCorrectedData;

  if(doFloatData){
    otherFloatData.reference(otherMainCols.floatData());
  }
  else{
    otherData.reference(otherMainCols.data());
  }

  if(doCorrectedData){
    otherCorrectedData.reference(otherMainCols.correctedData());
  }
  if(doModelData){
    otherModelData.reference(otherMainCols.modelData());
  }

  ArrayColumn<Double>& otherUvw = otherMainCols.uvw();
  ArrayColumn<Float>& otherWeight = otherMainCols.weight();
  ArrayColumn<Float>& otherWeightSp = otherMainCols.weightSpectrum();
  ArrayColumn<Float>& otherSigma = otherMainCols.sigma();
  ArrayColumn<Float>& otherSigmaSp = otherMainCols.sigmaSpectrum();
  ArrayColumn<Bool>& otherFlag = otherMainCols.flag();
  ArrayColumn<Bool>& otherFlagCat = otherMainCols.flagCategory();

  ScalarColumn<Int>& otherAnt1Col = otherMainCols.antenna1();
  ScalarColumn<Int>& otherAnt2Col = otherMainCols.antenna2();
  ScalarColumn<Int>& otherDDIdCol = otherMainCols.dataDescId();
  ScalarColumn<Int>& otherFieldIdCol = otherMainCols.fieldId();
  ScalarColumn<Int>& otherScanCol = otherMainCols.scanNumber();
  ScalarColumn<Int>& otherStateIdCol = otherMainCols.stateId();
  ScalarColumn<Int>& otherObsIdCol =otherMainCols.observationId();
  ScalarColumn<Int>& otherProcIdCol =otherMainCols.processorId();

  ScalarColumn<Int>& thisScanCol = mainCols.scanNumber();
  ScalarColumn<Int>& thisStateIdCol = mainCols.stateId();
  ScalarColumn<Int>& thisObsIdCol = mainCols.observationId();
  ScalarColumn<Int>& thisProcIdCol = mainCols.processorId();

  Vector<Int> otherAnt1;
  Vector<Int> otherAnt2;

  if(!antIndexTrivial){
    otherAnt1 = otherAnt1Col.getColumn();
    otherAnt2 = otherAnt2Col.getColumn();
  }

  Vector<Int> otherDDId = otherDDIdCol.getColumn();
  Vector<Int> otherFieldId = otherFieldIdCol.getColumn();
  Vector<Int> otherScan = otherScanCol.getColumn();
  Vector<Int> otherStateId(otherMS.nrow(),-1);
  Vector<Int> otherObsIds;
  Vector<Int> otherProcIds;

  if (doState && !otherStateNull){
    otherStateId = otherStateIdCol.getColumn();
  }

  Int defaultScanOffset=0;
  std::map<Int, Int> scanOffsetForOid;
  std::map<Int, Int> encountered;
  vector<Int> distinctObsIdSet;
  vector<Int> minScan;
  vector<Int> maxScan;

  if(reindexObsidAndScan){

    otherObsIds = otherObsIdCol.getColumn();
    Vector<Int> theseObsIds=thisObsIdCol.getColumn();
    otherProcIds = otherProcIdCol.getColumn();
    Vector<Int> theseProcIds=thisProcIdCol.getColumn();
    Vector<Int> theseScans=thisScanCol.getColumn();

    if(doObsA_p){ // the obs ids changed for the first table
      for(rownr_t r = 0; r < theseRows; r++) {
	if(newObsIndexA_p.find(theseObsIds[r]) != newObsIndexA_p.end()){ // apply change
	  theseObsIds[r] = getMapValue(newObsIndexA_p, theseObsIds[r]);
	}
      }
      thisObsIdCol.putColumn(theseObsIds);
    }

    if(doProcA_p){ // the proc ids changed for the first table
      for(uInt r = 0; r < theseRows; r++) {
	if(newProcIndexA_p.find(theseProcIds[r]) != newProcIndexA_p.end()){ // apply change
	  theseProcIds[r] = getMapValue(newProcIndexA_p, theseProcIds[r]);
	}
      }
      thisProcIdCol.putColumn(theseProcIds);
    }

    // SCAN NUMBER
    // find the distinct ObsIds in use in this MS
    // and the maximum scan ID in each of them

    Int maxScanThis=0;

    // read the initial values from a file if it exists
    std::ifstream ifs;
    ifs.open(obsidAndScanTableName.c_str(), ifstream::in);
    if (ifs.good()) {
      log << LogIO::NORMAL << "Reading from " << obsidAndScanTableName << LogIO::POST;
      uInt n;
      Int tobsid, tminscan, tmaxscan;
      ifs >> n;
      for(uInt i=0; i<n; i++){
	if(ifs.good()){
	  ifs >> tobsid; distinctObsIdSet.push_back(tobsid);
	  ifs >> tminscan; minScan.push_back(tminscan);
	  ifs >> tmaxscan; maxScan.push_back(tmaxscan);
	}
	else{
	  log << LogIO::WARN << "Error reading file " << obsidAndScanTableName << LogIO::POST;
	  break;
	}
      }
      if(ifs.good()){
	ifs >> 	maxScanThis;
      }
      else{
	log << LogIO::WARN << "Error reading file " << obsidAndScanTableName << LogIO::POST;
	log << LogIO::WARN << "Will continue with uninitialized obsid and scans information" << LogIO::POST;
	distinctObsIdSet.resize(0);
	minScan.resize(0);
	maxScan.resize(0);
	maxScanThis=0;
      }
      //cout << "distinctObsIdSet " << Vector<Int>(distinctObsIdSet) << endl;
      //cout << "minScan " << Vector<Int>(minScan) << endl;
      //cout << "maxScan " << Vector<Int>(maxScan) << endl;
      //cout << "maxScanThis " << maxScanThis << endl;
    }
    else{
      log << LogIO::NORMAL << "Will create auxiliary file " << obsidAndScanTableName << LogIO::POST;

      // determine the values for distinctObsIdSet, minScan, maxScanThis from scratch
      for(rownr_t r = 0; r < theseRows; r++) {
	Int oid = theseObsIds[r];
	Int scanid = theseScans[r];
	Bool found = False;
	uInt i;
	for(i=0; i<distinctObsIdSet.size(); i++){
	  if(distinctObsIdSet[i]==oid){
	    found = True;
	    break;
	  }
	}
	if(found){
	  if(scanid<minScan[i]){
	    minScan[i] = scanid;
	  }
	  if(scanid>maxScan[i]){
	    maxScan[i] = scanid;
	  }
	}
	else {
	  distinctObsIdSet.push_back(oid);
	  minScan.push_back(scanid);
	  maxScan.push_back(scanid);
	}
	if(scanid>maxScanThis){
	  maxScanThis = scanid;
	}
      }

    }
    ifs.close();


    // set the offset added to scan numbers in each observation
    Int minScanOther = min(otherScan);
    {
      defaultScanOffset = maxScanThis + 1 - minScanOther;
      if(defaultScanOffset<0){
	defaultScanOffset=0;
      }
    }

    for(uInt i=0; i<distinctObsIdSet.size(); i++){
      Int scanOffset;
      if(otherObsIdsWithCounterpart_p.find(distinctObsIdSet[i]) != otherObsIdsWithCounterpart_p.end() && i!=0){
	// This observation is present in both this and the other MS.
	// Need to set the scanOffset based on previous observation
	scanOffset = maxScan[i-1] + 1 - minScanOther;
      }
      else{
	scanOffset = minScan[i] - 1; // assume scan numbers originally start at 1
      }
      if(scanOffset<0){
	log << LogIO::WARN << "Zero or negative scan numbers in MS. May lead to duplicate scan numbers in concatenated MS."
	    << LogIO::POST;
	scanOffset = 0;
      }
      if(scanOffset==0){
	encountered[distinctObsIdSet[i]] = 0; // used later to decide whether to notify user
      }
      scanOffsetForOid[distinctObsIdSet[i]] = scanOffset;
    }


  } // end if reindexObsidAndScan

  if(doState && otherStateNull){ // the state ids for the first table will have to be set to -1
    Vector<Int> tempV(theseRows, -1);
    thisStateIdCol.putColumn(tempV);
  }

  // finished all modifications of MS Main table one

  // now start modifications of the second one

  log << LogIO::NORMAL << "Working on appended Main table ..." << LogIO::POST;


  Bool copyWtSp = (!otherWeightSp.isNull()) && otherWeightSp.isDefined(0);
  Bool copySgSp = (!otherSigmaSp.isNull()) && otherSigmaSp.isDefined(0);
  Bool copyFlagCat = (!otherFlagCat.isNull()) && otherFlagCat.isDefined(0);

  // MAIN

  Bool doWeightScale = (itsWeightScale!=1.) && (itsWeightScale!=0.);
  Float sScale = 1.; // scale for SIGMA
  if (doWeightScale){
    sScale = 1/sqrt(itsWeightScale);
  }

  if(reindexObsidAndScan){
    for (rownr_t r = 0; r < otherRows; r++) {
      Int oid = 0;
      if(doObsB_p && newObsIndexB_p.find(otherObsIds[r]) != newObsIndexB_p.end()){
	// the obs ids have been changed for the table to be appended
	oid = getMapValue (newObsIndexB_p, otherObsIds[r]);
      }
      else{ // this OBS id didn't change
	oid = otherObsIds[r];
      }

      if(oid != otherObsIds[r]){ // obsid actually changed
	if(scanOffsetForOid.find(oid) == scanOffsetForOid.end()){ // offset not set, use default
	  scanOffsetForOid[oid] = defaultScanOffset;
	}
	if(encountered.find(oid)==encountered.end() && scanOffsetForOid.at(oid)!=0){
	  log << LogIO::NORMAL << "Will offset scan numbers by " <<  scanOffsetForOid.at(oid)
	      << " for observations with Obs ID " << oid
	      << " in order to make scan numbers unique." << LogIO::POST;
	  encountered[oid] = 0;
	}
	otherScan[r] = otherScan[r] + scanOffsetForOid.at(oid);
      }

      otherObsIds[r] = oid;

      Int procid = 0;
      if(doProcB_p && newProcIndexB_p.find(otherProcIds[r]) != newProcIndexB_p.end()){
	// the proc ids have been changed for the table to be appended
	procid = getMapValue (newProcIndexB_p, otherProcIds[r]);
      }
      else{ // this PROC id didn't change
	procid = otherProcIds[r];
      }

      otherProcIds[r] = procid;

    }

    // update or create the file with the initial values for the next concat

    Int maxScanOther = 0;
    for(rownr_t r = 0; r < otherRows; r++) {
      Int oid = otherObsIds[r];
      Int scanid = otherScan[r];
      Bool found = False;
      uInt i;
      for(i=0; i<distinctObsIdSet.size(); i++){
	if(distinctObsIdSet[i]==oid){
	  found = True;
	  break;
	}
      }
      if(found){
	if(scanid<minScan[i]){
	  minScan[i] = scanid;
	}
	if(scanid>maxScan[i]){
	  maxScan[i] = scanid;
	}
      }
      else {
	distinctObsIdSet.push_back(oid);
	minScan.push_back(scanid);
	maxScan.push_back(scanid);
      }
      if(scanid>maxScanOther){
	maxScanOther = scanid;
      }
    }

    std::ofstream ofs;
    ofs.open (obsidAndScanTableName.c_str(), ofstream::out);
    if (!ofs) {
      log << LogIO::WARN << "Error opening file " << obsidAndScanTableName
	  << "will continue but the next virtual concat will lack this information:" << LogIO::POST;
      log << "distinctObsIdSet " << Vector<Int>(distinctObsIdSet) << endl;
      log << "minScan " << Vector<Int>(minScan) << endl;
      log << "maxScan " << Vector<Int>(maxScan) << endl;
      log << "maxScanOther " << maxScanOther << LogIO::POST;
    }
    else{
      log << LogIO::NORMAL << "Writing to " << obsidAndScanTableName << LogIO::POST;
      uInt n = distinctObsIdSet.size();
      ofs << n << endl;
      for(uInt i=0; i<n; i++){
	ofs << distinctObsIdSet[i] << endl;
	ofs << minScan[i] << endl;
	ofs << maxScan[i] << endl;
      }
      ofs << maxScanOther << endl; // note: this max is determined after the modification of scanOther
    }
    ofs.close();


  }

  if(doState){
    for (rownr_t r = 0; r < otherRows; r++) {
      if(!(itsStateNull || otherStateNull)){
	otherStateId[r] = newStateIndices[otherStateId[r]];
      }
    }
  }

  const ROMSPolarizationColumns otherPolCols(otherMS.polarization());
  const ROMSDataDescColumns otherDDCols(otherMS.dataDescription());
  Int polId = -1;
  vector<Int> polSwap;

  for (rownr_t r = 0; r < otherRows; r++) {
    // Determine whether we need to swap rows in the visibility matrix
    // if we change the order of the antennas.  This is done by
    // creating a mapping that makes sure the receptor numbers remain
    // correct when the antennas are swapped.
    /////uInt d = otherDDId(r);
    Int p = otherDDCols.polarizationId()(otherDDId(r));
    if (p != polId) {
      const Matrix<Int> &products = otherPolCols.corrProduct()(p);
      polSwap.resize(products.shape()(1));
      for (Int i = 0; i < products.shape()(1); i++) {
	for (Int j = 0; j < products.shape()(1); j++) {
	  if (products(0, i) == products(1, j) &&
	      products(1, i) == products(0, j)) {
	    polSwap[i] = j;
	    break;
	  }
	}
      }
      polId = p;
    }

    Bool doConjugateVis = False;

    if(!antIndexTrivial){
      Int newA1 = newAntIndices[otherAnt1[r]];
      Int newA2 = newAntIndices[otherAnt2[r]];

      if(newA1>newA2){ // swap indices and multiply UVW by -1
	//cout << "   corrected order r: " << r << " " << newA2 << " " << newA1 << endl;
	otherAnt1[r] = newA2;
	otherAnt2[r] = newA1;
	Array<Double> newUvw;
	newUvw.assign(otherUvw(r));
	//cout << "   old UVW " << newUvw;
	newUvw *= -1.;
	//cout << ", new UVW " << newUvw << endl;
	otherUvw.put(r, newUvw);
	doConjugateVis = True;
      }
      else{
	otherAnt1[r] = newA1;
	otherAnt2[r] = newA2;
      }
    }

    if(itsChanReversed[otherDDId[r]]){

      Vector<Int> datShape;
      Matrix<Complex> reversedData;
      Matrix<Float> reversedFloatData;
      Matrix<Complex> swappedData;
      if(doFloatData){
	datShape=otherFloatData.shape(r).asVector();
	reversedFloatData.resize(datShape[0], datShape[1]);
      }
      else{
	datShape=otherData.shape(r).asVector();
	reversedData.resize(datShape[0], datShape[1]);
      }
      Matrix<Complex> reversedCorrData(datShape[0], datShape[1]);
      Matrix<Complex> reversedModData(datShape[0], datShape[1]);
      for (Int k1=0; k1 < datShape[0]; ++k1){
	for(Int k2=0; k2 < datShape[1]; ++k2){
	  if(doFloatData){
	    reversedFloatData(k1,k2)=(Matrix<Float>(otherFloatData(r)))(k1,
									datShape[1]-1-k2);
	  }
	  else{
	    reversedData(k1,k2)=(Matrix<Complex>(otherData(r)))(k1,
								datShape[1]-1-k2);
	  }
	  if(doModelData){
	    reversedModData(k1,k2)=(Matrix<Complex>(otherModelData(r)))(k1,
									datShape[1]-1-k2);
	  }
	  if(doCorrectedData){
	    reversedCorrData(k1,k2)=(Matrix<Complex>(otherCorrectedData(r)))(k1,
									     datShape[1]-1-k2);
	  }

	}
      }
      if(doFloatData){
	otherFloatData.put(r, reversedFloatData);
      }
      else{
	if(doConjugateVis){
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(reversedData)).row(polSwap[p]);
	  }
	  otherData.put(r, conj(swappedData));
	}
	else{
	  otherData.put(r, reversedData);
	}
      }
      if(doCorrectedData){
	if(doConjugateVis){
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(reversedCorrData)).row(polSwap[p]);
	  }
	  otherCorrectedData.put(r, conj(swappedData));
	}
	else{
	  otherCorrectedData.put(r, reversedCorrData);
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(reversedModData)).row(polSwap[p]);
	  }
	  otherModelData.put(r, conj(swappedData));
	}
	else{
	  otherModelData.put(r, reversedModData);
	}
      }
    }
    else{ // no reversal
      Vector<Int> datShape;
      Matrix<Complex> swappedData;
      if(!doFloatData){
	if(doConjugateVis){ // conjugate because order of antennas was reversed
	  datShape=otherData.shape(r).asVector();
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(otherData(r))).row(polSwap[p]);
	  }
	  otherData.put(r, conj(swappedData));
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  datShape=otherModelData.shape(r).asVector();
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(otherModelData(r))).row(polSwap[p]);
	  }
	  otherModelData.put(r, conj(swappedData));
	}
      }
      if(doCorrectedData){
	if(doConjugateVis){
	  datShape=otherCorrectedData.shape(r).asVector();
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(otherCorrectedData(r))).row(polSwap[p]);
	  }
	  otherCorrectedData.put(r, conj(swappedData));
	}
      }
    } // end if itsChanReversed

    otherDDId[r] = newDDIndices[otherDDId[r]];
    otherFieldId[r] = newFldIndices[otherFieldId[r]];

    if(doWeightScale){
      if(doConjugateVis){
	Vector<Int> datShape=otherWeight.shape(r).asVector();
	Vector<Float> swappedWeight(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedWeight(p) = (Vector<Float>(otherWeight(r)))(polSwap[p]);
	}
	otherWeight.put(r, swappedWeight*itsWeightScale);
	if (copyWtSp) {
	  datShape.assign(otherWeightSp.shape(r).asVector());
	  Matrix<Float> swappedWeightSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedWeightSp.row(p) = (Matrix<Float>(otherWeightSp(r))).row(polSwap[p]);
	  }
	  otherWeightSp.put(r, swappedWeightSp*itsWeightScale);
	}
	datShape.assign(otherSigma.shape(r).asVector());
	Vector<Float> swappedSigma(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedSigma(p) = (Vector<Float>(otherSigma(r)))(polSwap[p]);
	}
	otherSigma.put(r, swappedSigma*sScale);
	if (copySgSp) {
	  datShape.assign(otherSigmaSp.shape(r).asVector());
	  Matrix<Float> swappedSigmaSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedSigmaSp.row(p) = (Matrix<Float>(otherSigmaSp(r))).row(polSwap[p]);
	  }
	  otherSigmaSp.put(r, swappedSigmaSp*sScale);
	}
      }
      else{
	otherWeight.put(r, otherWeight(r)*itsWeightScale);
	if (copyWtSp) otherWeightSp.put(r, otherWeightSp(r)*itsWeightScale);
	otherSigma.put(r, otherSigma(r)*sScale);
	if (copySgSp) otherSigmaSp.put(r, otherWeightSp(r)*sScale);
      }
    }
    else{
      if(doConjugateVis){
	Vector<Int> datShape=otherWeight.shape(r).asVector();
	Vector<Float> swappedWeight(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedWeight(p) = (Vector<Float>(otherWeight(r)))(polSwap[p]);
	}
	otherWeight.put(r, swappedWeight);
	if (copyWtSp) {
	  datShape.assign(otherWeightSp.shape(r).asVector());
	  Matrix<Float> swappedWeightSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedWeightSp.row(p) = (Matrix<Float>(otherWeightSp(r))).row(polSwap[p]);
	  }
	  otherWeightSp.put(r, swappedWeightSp);
	}
	datShape.assign(otherSigma.shape(r).asVector());
	Vector<Float> swappedSigma(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedSigma(p) = (Vector<Float>(otherSigma(r)))(polSwap[p]);
	}
	otherSigma.put(r, swappedSigma);
	if (copySgSp) {
	  datShape.assign(otherSigmaSp.shape(r).asVector());
	  Matrix<Float> swappedSigmaSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedSigmaSp.row(p) = (Matrix<Float>(otherSigmaSp(r))).row(polSwap[p]);
	  }
	  otherSigmaSp.put(r, swappedSigmaSp);
	}
      }
    }

    if(doConjugateVis){
      Vector<Int> datShape=otherFlag.shape(r).asVector();
      Matrix<Bool> swappedFlag(datShape[0], datShape[1]);
      for (Int p = 0; p < datShape[0]; p++) {
	swappedFlag.row(p) = (Matrix<Bool>(otherFlag(r))).row(polSwap[p]);
      }
      otherFlag.put(r, swappedFlag);
      if (copyFlagCat) {
	datShape.assign(otherFlagCat.shape(r).asVector());
	Cube<Bool> swappedFlagCat(datShape[0], datShape[1], datShape[2]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedFlagCat.yzPlane(p) = (Cube<Bool>(otherFlagCat(r))).yzPlane(polSwap[p]);
	}
	otherFlagCat.put(r, swappedFlagCat);
      }
    }
  } // end for

  // write the scalar columns

  log << LogIO::NORMAL << "Writing the scalar columns ..." << LogIO::POST;

  if(!antIndexTrivial){
    otherAnt1Col.putColumn(otherAnt1);
    otherAnt2Col.putColumn(otherAnt2);
  }
  otherDDIdCol.putColumn(otherDDId);
  otherFieldIdCol.putColumn(otherFieldId);

  if(doState && !(itsStateNull || otherStateNull)){
    otherStateIdCol.putColumn(otherStateId);
  }

  if(reindexObsidAndScan){
    otherScanCol.putColumn(otherScan);
    otherObsIdCol.putColumn(otherObsIds);
    otherProcIdCol.putColumn(otherProcIds);
  }

  if(doModelData){ //update the MODEL_DATA keywords
    updateModelDataKeywords(otherMS);
  }

}


  //--------------------------------------------------------------------

  void MSConcat::concatenate(const MeasurementSet& otherMS,
			     const uInt handling,
			     const String& destMSName)
{
  LogIO log(LogOrigin("MSConcat", "concatenate", WHERE));

  if(destMSName.empty()){
    log << "Appending " << otherMS.tableName() << " to " << itsMS.tableName() << endl << LogIO::POST;
  }
  else{
    log << "Virtually appending " << otherMS.tableName() << " to " << itsMS.tableName() << endl << LogIO::POST;
  }

  switch(handling){
  case 0: // normal concat
    break;
  case 1:
    log << "*** At user\'s request, MAIN table will not be concatenated!" << LogIO::POST;
    break;
  case 2:
    log << "*** At user\'s request, POINTING table will not be concatenated!" << LogIO::POST;
    break;
  case 3:
    log << "*** At user\'s request, MAIN and POINTING tables will not be concatenated!" << LogIO::POST;
    break;
  default:
    log << "Invalid value for handling switch: " << handling << " (valid range is 0 - 3)"
	<< LogIO::EXCEPTION;
    break;
  }

  // check if certain columns are present and set flags accordingly
  Bool doCorrectedData=False, doModelData=False;
  Bool doFloatData=False;

  if(handling==0 || handling==2){

    if (itsMS.tableDesc().isColumn("FLOAT_DATA") &&
	otherMS.tableDesc().isColumn("FLOAT_DATA"))
      doFloatData=True;
    else if (itsMS.tableDesc().isColumn("FLOAT_DATA") &&
	     !otherMS.tableDesc().isColumn("FLOAT_DATA")){
      log << itsMS.tableName()
	  << " has FLOAT_DATA column but not " << otherMS.tableName()
	  << LogIO::EXCEPTION;
      log << "Cannot concatenate these MSs yet...you may split the corrected column of the SD as a work around."
	  << LogIO::EXCEPTION;
    }
    if (itsMS.tableDesc().isColumn("MODEL_DATA") &&
	otherMS.tableDesc().isColumn("MODEL_DATA"))
      doModelData=True;
    else if (itsMS.tableDesc().isColumn("MODEL_DATA") &&
	     !otherMS.tableDesc().isColumn("MODEL_DATA")){
      log << itsMS.tableName()
	  << " has MODEL_DATA column but not " << otherMS.tableName()
	  << LogIO::EXCEPTION;
      log << "You may wish to create this column by loading "
	  << otherMS.tableName()
	  << " in imager or calibrater "
	  << LogIO::EXCEPTION;
    }
    if (itsMS.tableDesc().isColumn("CORRECTED_DATA") &&
	otherMS.tableDesc().isColumn("CORRECTED_DATA"))
      doCorrectedData=True;
    else if (itsMS.tableDesc().isColumn("CORRECTED_DATA") &&
	     !otherMS.tableDesc().isColumn("CORRECTED_DATA"))
      log << itsMS.tableName()
	  <<" has CORRECTED_DATA column but not " << otherMS.tableName()
	  << LogIO::EXCEPTION;
  }

  {
    const MSFieldColumns otherMSFCols(otherMS.field());
    if(!checkEphIdInField(otherMSFCols)){
      log << "EPHEMERIS_ID column missing in FIELD table of MS " << itsMS.tableName()
	  << LogIO::EXCEPTION;
    }
  }

  // verify that shape of the two MSs as described in POLARISATION, SPW, and DATA_DESCR
  //   is the same
  const MSMainColumns otherMainCols(otherMS);
  if (otherMS.nrow() > 0) {
    if (itsFixedShape.nelements() > 0) {
      const MSPolarizationColumns otherPolCols(otherMS.polarization());
      const MSSpWindowColumns otherSpwCols(otherMS.spectralWindow());
      const MSDataDescColumns otherDDCols(otherMS.dataDescription());
      const rownr_t nShapes = otherDDCols.nrow();
      for (rownr_t s = 0; s < nShapes; s++) {
	checkShape(getShape(otherDDCols, otherSpwCols, otherPolCols, s));
      }
    }
    checkCategories(otherMainCols);
  }

  log << LogIO::DEBUG1 << "ms shapes verified " << endl << LogIO::POST;

  // merge STATE
  Block<uInt> newStateIndices;
  Bool doState = False;
  // STATE is a required subtable but can be empty in which case the state id in the main table is -1
  Bool itsStateNull = (itsMS.state().isNull() || (itsMS.state().nrow() == 0));
  Bool otherStateNull = (otherMS.state().isNull() || (otherMS.state().nrow() == 0));

  if(itsStateNull && otherStateNull){
    log << LogIO::NORMAL << "No valid state tables present. Result won't have one either." << LogIO::POST;
  }
  else if(itsStateNull && !otherStateNull){
    log << LogIO::WARN << itsMS.tableName() << " does not have a valid state table," << endl
	<< "  the MS to be appended, however, has one. Result won't have one."
	<< LogIO::POST;
    doState = True; // i.e. the appended MS Main table state id will have to be set to -1
  }
  else if(!itsStateNull && otherStateNull){
    log << LogIO::WARN << itsMS.tableName() << " does have a valid state table," << endl
	<< "  the MS to be appended, however, doesn't. Result won't have one."
	<< LogIO::POST;
    doState = True; // i.e. itsMS Main table state id will have to be set to -1

    RowNumbers delrows(itsMS.state().nrow());
    indgen(delrows);
    itsMS.state().removeRow(RowNumbers(delrows));
  }
  else{ // both state tables are filled
    const rownr_t oldStateRows = itsMS.state().nrow();
    newStateIndices = copyState(otherMS.state());
    const rownr_t addedRows = itsMS.state().nrow() - oldStateRows;
    const rownr_t matchedRows = otherMS.state().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the state subtable" << LogIO::POST;
    doState = True; // state id entries in the main table will have to be modified for otherMS
  }

  //See if there is a SOURCE table and concatenate and reindex it
  {
    rownr_t oldSRows = itsMS.source().nrow();
    copySource(otherMS);
    if(Table::isReadable(itsMS.sourceTableName())){
      rownr_t addedRows =  itsMS.source().nrow() - oldSRows;
      if(addedRows>0){
	log << "Added " << addedRows
	    << " rows to the source subtable" << LogIO::POST;
      }
    }
  }


  // DATA_DESCRIPTION
  rownr_t oldRows = itsMS.dataDescription().nrow();
  rownr_t oldSPWRows = itsMS.spectralWindow().nrow();
  const Block<uInt> newDDIndices = copySpwAndPol(otherMS.spectralWindow(),
						 otherMS.polarization(),
						 otherMS.dataDescription());

  {
    rownr_t addedRows = itsMS.dataDescription().nrow() - oldRows;
    rownr_t matchedRows = otherMS.dataDescription().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the data description subtable" << LogIO::POST;
    addedRows = itsMS.spectralWindow().nrow() - oldSPWRows;
    matchedRows = otherMS.spectralWindow().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the spectral window subtable" << LogIO::POST;
  }


  // correct the spw entries in the SOURCE table and remove redundant rows
  oldRows = itsMS.source().nrow();
  updateSource();
  if(Table::isReadable(itsMS.sourceTableName())){
    rownr_t removedRows =  oldRows - itsMS.source().nrow();
    if(removedRows>0){
      log << "Removed " << removedRows
	  << " redundant rows from the source subtable" << LogIO::POST;
    }
  }


  // merge ANTENNA and FEED
  oldRows = itsMS.antenna().nrow();
  rownr_t oldFeedRows = itsMS.feed().nrow();
  const Block<uInt> newAntIndices = copyAntennaAndFeed(otherMS.antenna(),
						       otherMS.feed());
  {
    rownr_t addedRows = itsMS.antenna().nrow() - oldRows;
    rownr_t matchedRows = otherMS.antenna().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the antenna subtable" << endl;
    addedRows = itsMS.feed().nrow() - oldFeedRows;
    log << "Added " << addedRows
	<< " rows to the feed subtable" << endl;
  }

  //for(uInt ii=0; ii<newAntIndices.size(); ii++){
  //  cout << "i, newAntIndices(i) " << ii << " " << newAntIndices[ii] << endl;
  //}


  // FIELD
  oldRows = itsMS.field().nrow();
  const Block<uInt> newFldIndices = copyField(otherMS);
  {
    const rownr_t addedRows = itsMS.field().nrow() - oldRows;
    const rownr_t matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows
	<< " rows and matched " << matchedRows
	<< " from the field subtable" << LogIO::POST;
    if(matchedRows>0){ // may have to consolidate SOURCE IDs
      if(updateSource2()){
	log << "Consolidated Source IDs in the source subtable." << LogIO::POST;
      }
    }
  }


  // OBSERVATION
  copyObservation(otherMS.observation(), True);

  // PROCESSOR
  copyProcessor(otherMS.processor(), True);

  // POINTING
  if(handling<2){
    if(!copyPointing(otherMS.pointing(), newAntIndices)){
      log << LogIO::WARN << "Could not merge Pointing subtables " << LogIO::POST ;
    }
  }
  else{ // delete the POINTING table
    log << LogIO::NORMAL << "Deleting all rows in the Pointing subtable ..." << LogIO::POST ;
    RowNumbers delrows(itsMS.pointing().nrow());
    indgen(delrows);
    itsMS.pointing().removeRow(RowNumbers(delrows));
  }

  // SYSCAL
  if(!copySysCal(otherMS.sysCal(), newAntIndices)){
    log << LogIO::WARN << "Could not merge SysCal subtables " << LogIO::POST ;
  }

  // WEATHER
  if(!copyWeather(otherMS.weather(), newAntIndices)){
    log << LogIO::WARN << "Could not merge Weather subtables " << LogIO::POST ;
  }


  //////////////////////////////////////////////////////

  MeasurementSet* destMS = 0;
  MeasurementSet tempMS;

  if(destMSName.empty()){ // no destination MS was given, write to the first MS
    destMS = &itsMS;
  }
  else{ // copy the structure of otherMS to destMSName, then copy the subtables of itsMS to it
    String absNewName = Path(destMSName).absoluteName();
    {
      Table newtab = TableCopy::makeEmptyTable(absNewName, Record(), otherMS, Table::New,
					       Table::AipsrcEndian, True,
					       True); // noRows
      TableCopy::copyInfo (newtab, otherMS);
      TableCopy::copySubTables (newtab, itsMS,
				False); // noRows==False, i.e. subtables are copied
    }
    tempMS = MeasurementSet(destMSName, Table::Update); // open as the output MS for the new Main rows
    destMS = &tempMS;
  }

  // STOP HERE if Main is not to be modified
  if(handling==1 || handling==3){
    return;
  }
  //////////////////////////////////////////////////////


  MSMainColumns destMainCols(*destMS);

  // I need to check that the Measures and units are the same.
  const rownr_t newRows = otherMS.nrow();
  rownr_t curRow = destMS->nrow();

  if (!destMS->canAddRow()) {
    log << LogIO::WARN << "Can't add rows to this ms!  Something is seriously wrong with "
	<< destMS->tableName() << endl << LogIO::POST;
  }

  log << LogIO::DEBUG1 << "trying to add " << newRows << " data rows to the ms, now at: "
      << destMS->nrow() << endl << LogIO::POST;
  destMS->addRow(newRows);
  log << LogIO::DEBUG1 << "added " << newRows << " data rows to the ms, now at: "
      << destMS->nrow() << endl << LogIO::POST;

  // create column objects for those columns which need not be modified
  const ScalarColumn<Double>& otherTime = otherMainCols.time();
  ScalarColumn<Double>& thisTime = destMainCols.time();
  const ScalarColumn<Double>& otherInterval = otherMainCols.interval();
  ScalarColumn<Double>& thisInterval = destMainCols.interval();
  const ScalarColumn<Double>& otherExposure = otherMainCols.exposure();
  ScalarColumn<Double>& thisExposure = destMainCols.exposure();
  const ScalarColumn<Double>& otherTimeCen = otherMainCols.timeCentroid();
  ScalarColumn<Double>& thisTimeCen = destMainCols.timeCentroid();
  const ScalarColumn<Int>& otherArrayId = otherMainCols.arrayId();
  ScalarColumn<Int>& thisArrayId = destMainCols.arrayId();
  const ArrayColumn<Bool>& otherFlag = otherMainCols.flag();
  ArrayColumn<Bool>& thisFlag = destMainCols.flag();
  const ArrayColumn<Bool>& otherFlagCat = otherMainCols.flagCategory();
  ArrayColumn<Bool>& thisFlagCat = destMainCols.flagCategory();
  Bool copyFlagCat = !(thisFlagCat.isNull() || otherFlagCat.isNull());
  copyFlagCat = copyFlagCat && thisFlagCat.isDefined(0)
    && otherFlagCat.isDefined(0);
  const ScalarColumn<Bool>& otherFlagRow = otherMainCols.flagRow();
  ScalarColumn<Bool>& thisFlagRow = destMainCols.flagRow();
  const ScalarColumn<Int>& otherFeed1 = otherMainCols.feed1();
  ScalarColumn<Int>& thisFeed1 = destMainCols.feed1();
  const ScalarColumn<Int>& otherFeed2 = otherMainCols.feed2();
  ScalarColumn<Int>& thisFeed2 = destMainCols.feed2();

  // create column objects for those columns which potentially need to be modified

  ArrayColumn<Complex> otherData;
  ArrayColumn<Complex> thisData;
  ArrayColumn<Float> otherFloatData;
  ArrayColumn<Float> thisFloatData;
  ArrayColumn<Complex> otherModelData, otherCorrectedData;
  ArrayColumn<Complex> thisModelData, thisCorrectedData;

  if(doFloatData){
    thisFloatData.reference(destMainCols.floatData());
    otherFloatData.reference(otherMainCols.floatData());
  }
  else{
    thisData.reference(destMainCols.data());
    otherData.reference(otherMainCols.data());
  }

  if(doCorrectedData){
    thisCorrectedData.reference(destMainCols.correctedData());
    otherCorrectedData.reference(otherMainCols.correctedData());
  }
  if(doModelData){
    thisModelData.reference(destMainCols.modelData());
    otherModelData.reference(otherMainCols.modelData());
  }

  const ScalarColumn<Int>& otherAnt1 = otherMainCols.antenna1();
  ScalarColumn<Int> thisAnt1 = destMainCols.antenna1();
  const ScalarColumn<Int>& otherAnt2 = otherMainCols.antenna2();
  ScalarColumn<Int> thisAnt2 = destMainCols.antenna2();
  const ScalarColumn<Int>& otherDDId = otherMainCols.dataDescId();
  ScalarColumn<Int> thisDDId = destMainCols.dataDescId();
  const ScalarColumn<Int>& otherFieldId = otherMainCols.fieldId();
  ScalarColumn<Int> thisFieldId = destMainCols.fieldId();
  const ArrayColumn<Double>& otherUvw = otherMainCols.uvw();
  ArrayColumn<Double> thisUvw = destMainCols.uvw();
  const ArrayColumn<Float>& otherWeight = otherMainCols.weight();
  ArrayColumn<Float> thisWeight = destMainCols.weight();
  const ArrayColumn<Float>& otherWeightSp = otherMainCols.weightSpectrum();
  ArrayColumn<Float> thisWeightSp = destMainCols.weightSpectrum();
  const ArrayColumn<Float>& otherSigma = otherMainCols.sigma();
  ArrayColumn<Float> thisSigma = destMainCols.sigma();
  const ArrayColumn<Float>& otherSigmaSp = otherMainCols.sigmaSpectrum();
  ArrayColumn<Float> thisSigmaSp = destMainCols.sigmaSpectrum();

  const ScalarColumn<Int>& otherScan = otherMainCols.scanNumber();
  const ScalarColumn<Int>& otherStateId = otherMainCols.stateId();
  const ScalarColumn<Int>& otherObsId=otherMainCols.observationId();
  const ScalarColumn<Int>& otherProcId=otherMainCols.processorId();

  ScalarColumn<Int> thisScan;
  ScalarColumn<Int> thisStateId;
  ScalarColumn<Int> thisObsId;
  ScalarColumn<Int> thisProcId;

  thisScan.reference(scanNumber());
  thisStateId.reference(stateId());
  thisObsId.reference(observationId());
  thisProcId.reference(processorId());

  Vector<Int> obsIds=otherObsId.getColumn();

  if(doObsA_p){ // the obs ids changed for the first table
    Vector<Int> oldObsIds=thisObsId.getColumn();
    for(rownr_t r = 0; r < curRow; r++) {
      if(newObsIndexA_p.find(oldObsIds[r]) != newObsIndexA_p.end()){ // apply change
	thisObsId.put(r, getMapValue (newObsIndexA_p, oldObsIds[r]));
      }
    }
  }

  Vector<Int> procIds=otherProcId.getColumn();

  if(doProcA_p){ // the proc ids changed for the first table
    Vector<Int> oldProcIds=thisProcId.getColumn();
    for(uInt r = 0; r < curRow; r++) {
      if(newProcIndexA_p.find(oldProcIds[r]) != newProcIndexA_p.end()){ // apply change
	thisProcId.put(r, getMapValue (newProcIndexA_p, oldProcIds[r]));
      }
    }
  }

  if(doState && otherStateNull){ // the state ids for the first table will have to be set to -1
    for(rownr_t r = 0; r < curRow; r++) {
      thisStateId.put(r, -1);
    }
  }

  // SCAN NUMBER
  // find the distinct ObsIds in use in this MS
  // and the maximum scan and minimum scan ID in each of them
  std::map<Int, Int> scanOffsetForOid;
  std::map<Int, Int> encountered;
  vector<Int> distinctObsIdSet;
  vector<Int> minScan;
  vector<Int> maxScan;
  Int maxScanThis=0;
  for(rownr_t r = 0; r < curRow; r++) {
    Int oid = thisObsId(r);
    Int scanid = thisScan(r);
    Bool found = False;
    uInt i;
    for(i=0; i<distinctObsIdSet.size(); i++){
      if(distinctObsIdSet[i]==oid){
	found = True;
	break;
      }
    }
    if(found){
      if(scanid<minScan[i]){
	minScan[i] = scanid;
      }
      if(scanid>maxScan[i]){
	maxScan[i] = scanid;
      }
    }
    else {
      distinctObsIdSet.push_back(oid);
      minScan.push_back(scanid);
      maxScan.push_back(scanid);
    }
    if(scanid>maxScanThis){
      maxScanThis = scanid;
    }
  }
  // set the offset added to scan numbers in each observation

  Int defaultScanOffset=0;
  Int minScanOther = 0;
  {
    TableVector<Int> ScanTabVectOther(otherScan);
    minScanOther = min(ScanTabVectOther);
    defaultScanOffset = maxScanThis + 1 - minScanOther;
    if(defaultScanOffset<0){
      defaultScanOffset=0;
    }
  }

  for(uInt i=0; i<distinctObsIdSet.size(); i++){
    Int scanOffset;
    if(otherObsIdsWithCounterpart_p.find(distinctObsIdSet[i])!= otherObsIdsWithCounterpart_p.end() && i!=0){
      // This observation is present in both this and the other MS.
      // Need to set the scanOffset based on previous observation
      scanOffset = maxScan[i-1] + 1 - minScanOther;
    }
    else{
      scanOffset = minScan[i] - 1; // assume scan numbers originally start at 1
    }
    if(scanOffset<0){
      log << LogIO::WARN << "Zero or negative scan numbers in MS. May lead to duplicate scan numbers in concatenated MS."
	  << LogIO::POST;
      scanOffset = 0;
    }
    if(scanOffset==0){
      encountered[distinctObsIdSet[i]] = 0; // used later to decide whether to notify user
    }
    scanOffsetForOid[distinctObsIdSet[i]] = scanOffset;
  }


  // finished all modifications of the MS Main table going to the first part

  // now start modifications of the second (appended) part

  thisScan.reference(destMainCols.scanNumber());
  thisStateId.reference(destMainCols.stateId());
  thisObsId.reference(destMainCols.observationId());
  thisProcId.reference(destMainCols.processorId());

  Bool copyWtSp = !(thisWeightSp.isNull() || otherWeightSp.isNull());
  copyWtSp = copyWtSp && thisWeightSp.isDefined(0)
    && otherWeightSp.isDefined(0);
  Bool copySgSp = !(thisSigmaSp.isNull() || otherSigmaSp.isNull());
  copySgSp = copySgSp && thisSigmaSp.isDefined(0)
    && otherSigmaSp.isDefined(0);

  // MAIN

  Bool notYetFeedWarned = True;
  Bool doWeightScale = (itsWeightScale!=1. && itsWeightScale>0.);
  Float sScale = 1.; // scale for SIGMA
  if (doWeightScale){
    sScale = 1/sqrt(itsWeightScale);
  }

  const ROMSPolarizationColumns otherPolCols(otherMS.polarization());
  const ROMSDataDescColumns otherDDCols(otherMS.dataDescription());
  Int polId = -1;
  vector<Int> polSwap;

  for (rownr_t r = 0; r < newRows; r++, curRow++) {
    // Determine whether we need to swap rows in the visibility matrix
    // if we change the order of the antennas.  This is done by
    // creating a mapping that makes sure the receptor numbers remain
    // correct when the antennas are swapped.
    /////uInt d = otherDDId(r);
    Int p = otherDDCols.polarizationId()(otherDDId(r));
    if (p != polId) {
      const Matrix<Int> &products = otherPolCols.corrProduct()(p);
      polSwap.resize(products.shape()(1));
      for (Int i = 0; i < products.shape()(1); i++) {
        for (Int j = 0; j < products.shape()(1); j++) {
          if (products(0, i) == products(1, j) &&
              products(1, i) == products(0, j)) {
            polSwap[i] = j;
            break;
          }
        }
      }
      polId = p;
    }

    Int newA1 = newAntIndices[otherAnt1(r)];
    Int newA2 = newAntIndices[otherAnt2(r)];
    Bool doConjugateVis = False;
    if(newA1>newA2){ // swap indices and multiply UVW by -1
      //cout << "   corrected order r: " << r << " " << newA2 << " " << newA1 << endl;
      thisAnt1.put(curRow, newA2);
      thisAnt2.put(curRow, newA1);
      Array<Double> newUvw;
      newUvw.assign(otherUvw(r));
      //cout << "   old UVW " << newUvw;
      newUvw *= -1.;
      //cout << ", new UVW " << newUvw << endl;
      thisUvw.put(curRow, newUvw);
      doConjugateVis = True;
    }
    else{
      thisAnt1.put(curRow, newA1);
      thisAnt2.put(curRow, newA2);
      thisUvw.put(curRow, otherUvw, r);
    }

    thisDDId.put(curRow, newDDIndices[otherDDId(r)]);
    thisFieldId.put(curRow, newFldIndices[otherFieldId(r)]);

    Int oid = 0;
    if(doObsB_p && newObsIndexB_p.find(obsIds[r]) != newObsIndexB_p.end()){
      // the obs ids have been changed for the table to be appended
      oid = getMapValue(newObsIndexB_p, obsIds[r]);
    }
    else { // this OBS id didn't change
      oid = obsIds[r];
    }
    thisObsId.put(curRow, oid);

    if(oid != obsIds[r]){ // obsid actually changed
      if(scanOffsetForOid.find(oid) == scanOffsetForOid.end()){ // offset not set, use default
	scanOffsetForOid[oid] = defaultScanOffset;
      }
      if(encountered.find(oid)==encountered.end() && scanOffsetForOid.at(oid)!=0){
	log << LogIO::NORMAL << "Will offset scan numbers by " <<  scanOffsetForOid.at(oid)
	    << " for observations with Obs ID " << oid
	    << " in order to make scan numbers unique." << LogIO::POST;
	encountered[oid] = 0;
      }
      thisScan.put(curRow, otherScan(r) + scanOffsetForOid.at(oid));
    }
    else{
      thisScan.put(curRow, otherScan(r));
    }

    Int procid = 0;
    if(doProcB_p && newProcIndexB_p.find(procIds[r]) != newProcIndexB_p.end()){
      // the proc ids have been changed for the table to be appended
      procid = getMapValue(newProcIndexB_p, procIds[r]);
    }
    else { // this PROC id didn't change
      procid = procIds[r];
    }
    thisProcId.put(curRow, procid);


    if(doState){
      if(itsStateNull || otherStateNull){
	thisStateId.put(curRow, -1);
      }
      else{
	thisStateId.put(curRow, newStateIndices[otherStateId(r)]);
      }
    }
    else{
      thisStateId.put(curRow, otherStateId, r);
    }

    if(itsChanReversed[otherDDId(r)]){

      Vector<Int> datShape;
      Matrix<Complex> reversedData;
      Matrix<Float> reversedFloatData;
      Matrix<Complex> swappedData;
      if(doFloatData){
	datShape=otherFloatData.shape(r).asVector();
	reversedFloatData.resize(datShape[0], datShape[1]);
      }
      else{
	datShape=otherData.shape(r).asVector();
	reversedData.resize(datShape[0], datShape[1]);
      }
      Matrix<Complex> reversedCorrData(datShape[0], datShape[1]);
      Matrix<Complex> reversedModData(datShape[0], datShape[1]);
      for (Int k1=0; k1 < datShape[0]; ++k1){
	for(Int k2=0; k2 < datShape[1]; ++k2){
	  if(doFloatData){
	    reversedFloatData(k1,k2)=(Matrix<Float>(otherFloatData(r)))(k1,
									datShape[1]-1-k2);
	  }
	  else{
	    reversedData(k1,k2)=(Matrix<Complex>(otherData(r)))(k1,
								datShape[1]-1-k2);
	  }
	  if(doModelData){
	    reversedModData(k1,k2)=(Matrix<Complex>(otherModelData(r)))(k1,
									datShape[1]-1-k2);
	  }
	  if(doCorrectedData){
	    reversedCorrData(k1,k2)=(Matrix<Complex>(otherCorrectedData(r)))(k1,
									     datShape[1]-1-k2);
	  }
	}
      }
      if(doFloatData){
	thisFloatData.put(curRow, reversedFloatData);
      }
      else{
	if(doConjugateVis){
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(reversedData)).row(polSwap[p]);
	  }
	  thisData.put(curRow, conj(swappedData));
	}
	else{
	  thisData.put(curRow, reversedData);
	}
      }
      if(doCorrectedData){
	if(doConjugateVis){
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(reversedCorrData)).row(polSwap[p]);
	  }
	  thisCorrectedData.put(curRow, conj(swappedData));
	}
	else{
	  thisCorrectedData.put(curRow, reversedCorrData);
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(reversedModData)).row(polSwap[p]);
	  }
	  thisModelData.put(curRow, conj(swappedData));
	}
	else{
	  thisModelData.put(curRow, reversedModData);
	}
      }
    }
    else{ // no reversal
      Vector<Int> datShape;
      Matrix<Complex> swappedData;
      if(doFloatData){
	thisFloatData.put(curRow, otherFloatData, r);
      }
      else{
	if(doConjugateVis){ // conjugate because order of antennas was reversed
	  datShape=otherData.shape(r).asVector();
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(otherData(r))).row(polSwap[p]);
	  }
	  thisData.put(curRow, conj(swappedData));
	}
	else{
	  thisData.put(curRow, otherData, r);
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  datShape=otherModelData.shape(r).asVector();
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(otherModelData(r))).row(polSwap[p]);
	  }
	  thisModelData.put(curRow, conj(swappedData));
	}
	else{
	  thisModelData.put(curRow, otherModelData, r);
	}
      }
      if(doCorrectedData){
	if(doConjugateVis){
	  datShape=otherCorrectedData.shape(r).asVector();
	  swappedData.resize(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedData.row(p) = (Matrix<Complex>(otherCorrectedData(r))).row(polSwap[p]);
	  }
	  thisCorrectedData.put(curRow, conj(swappedData));
	}
	else{
	  thisCorrectedData.put(curRow, otherCorrectedData, r);
	}
      }
    } // end if itsChanReversed

    if(doWeightScale){
      if(doConjugateVis){
	Vector<Int> datShape=otherWeight.shape(r).asVector();
	Vector<Float> swappedWeight(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedWeight(p) = (Vector<Float>(otherWeight(r)))(polSwap[p]);
	}
	thisWeight.put(curRow, swappedWeight*itsWeightScale);
	if (copyWtSp) {
	  datShape.assign(otherWeightSp.shape(r).asVector());
	  Matrix<Float> swappedWeightSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedWeightSp.row(p) = (Matrix<Float>(otherWeightSp(r))).row(polSwap[p]);
	  }
	  thisWeightSp.put(curRow, swappedWeightSp*itsWeightScale);
	}
	datShape.assign(otherSigma.shape(r).asVector());
	Vector<Float> swappedSigma(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedSigma(p) = (Vector<Float>(otherSigma(r)))(polSwap[p]);
	}
	thisSigma.put(curRow, swappedSigma*sScale);
	if (copySgSp) {
	  datShape.assign(otherSigmaSp.shape(r).asVector());
	  Matrix<Float> swappedSigmaSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedSigmaSp.row(p) = (Matrix<Float>(otherSigmaSp(r))).row(polSwap[p]);
	  }
	  thisSigmaSp.put(curRow, swappedSigmaSp*sScale);
	}
      }
      else {
	thisWeight.put(curRow, otherWeight(r)*itsWeightScale);
	if (copyWtSp)
	  thisWeightSp.put(curRow, otherWeightSp(r)*itsWeightScale);
	thisSigma.put(curRow, otherSigma(r)*sScale);
	if (copySgSp)
	  thisSigmaSp.put(curRow, otherSigmaSp(r)*sScale);
      }
    }
    else{
      if (doConjugateVis){
	Vector<Int> datShape=otherWeight.shape(r).asVector();
	Vector<Float> swappedWeight(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedWeight(p) = (Vector<Float>(otherWeight(r)))(polSwap[p]);
	}
	thisWeight.put(curRow, swappedWeight);
	if (copyWtSp) {
	  datShape.assign(otherWeightSp.shape(r).asVector());
	  Matrix<Float> swappedWeightSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedWeightSp.row(p) = (Matrix<Float>(otherWeightSp(r))).row(polSwap[p]);
	  }
	  thisWeightSp.put(curRow, swappedWeightSp);
	}
	datShape.assign(otherSigma.shape(r).asVector());
	Vector<Float> swappedSigma(datShape[0]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedSigma(p) = (Vector<Float>(otherSigma(r)))(polSwap[p]);
	}
	thisSigma.put(curRow, swappedSigma);
	if (copySgSp) {
	  datShape.assign(otherSigmaSp.shape(r).asVector());
	  Matrix<Float> swappedSigmaSp(datShape[0], datShape[1]);
	  for (Int p = 0; p < datShape[0]; p++) {
	    swappedSigmaSp.row(p) = (Matrix<Float>(otherSigmaSp(r))).row(polSwap[p]);
	  }
	  thisSigmaSp.put(curRow, swappedSigmaSp);
	}
      }
      else{
	thisWeight.put(curRow, otherWeight, r);
	if (copyWtSp) thisWeightSp.put(curRow, otherWeightSp, r);
	thisSigma.put(curRow, otherSigma, r);
	if (copySgSp) thisSigmaSp.put(curRow, otherSigmaSp, r);
      }
    }

    if(notYetFeedWarned && (otherFeed1(r)>0 || otherFeed2(r)>0)){
      log << LogIO::WARN << "MS to be appended contains antennas with multiple feeds. Feed ID reindexing is not implemented.\n"
	  << LogIO::POST;
      notYetFeedWarned = False;
    }

    if(doConjugateVis){
      thisFeed1.put(curRow, otherFeed2, r);
      thisFeed2.put(curRow, otherFeed1, r);
    }
    else{
      thisFeed1.put(curRow, otherFeed1, r);
      thisFeed2.put(curRow, otherFeed2, r);
    }

    thisTime.put(curRow, otherTime, r);
    thisInterval.put(curRow, otherInterval, r);
    thisExposure.put(curRow, otherExposure, r);
    thisTimeCen.put(curRow, otherTimeCen, r);
    thisArrayId.put(curRow, otherArrayId, r);
    if(doConjugateVis){
      Vector<Int> datShape=otherFlag.shape(r).asVector();
      Matrix<Bool> swappedFlag(datShape[0], datShape[1]);
      for (Int p = 0; p < datShape[0]; p++) {
	swappedFlag.row(p) = (Matrix<Bool>(otherFlag(r))).row(polSwap[p]);
      }
      thisFlag.put(curRow, swappedFlag);
      if (copyFlagCat) {
	datShape.assign(otherFlagCat.shape(r).asVector());
	Cube<Bool> swappedFlagCat(datShape[0], datShape[1], datShape[2]);
	for (Int p = 0; p < datShape[0]; p++) {
	  swappedFlagCat.yzPlane(p) = (Cube<Bool>(otherFlagCat(r))).yzPlane(polSwap[p]);
	}
	thisFlagCat.put(curRow, swappedFlagCat);
      }
    }
    else{
      thisFlag.put(curRow, otherFlag, r);
      if (copyFlagCat) thisFlagCat.put(curRow, otherFlagCat, r);
    }
    thisFlagRow.put(curRow, otherFlagRow, r);

  } // end for

  if(doModelData){ //update the MODEL_DATA keywords
    updateModelDataKeywords(*destMS);
  }

}



//-----------------------------------------------------------------------

void MSConcat::setTolerance(Quantum<Double>& freqTol, Quantum<Double>& dirTol){
  itsFreqTol=freqTol;
  itsDirTol=dirTol;
}

void MSConcat::setWeightScale(const Float weightScale){
  if(weightScale<0){
    throw(AipsError(String("MSConcat::setWeightScale: weight scale must be >= 0.")));
  }
  itsWeightScale=weightScale;
}

void MSConcat::setRespectForFieldName(const Bool respectFieldName){
  itsRespectForFieldName = respectFieldName;
}

void MSConcat::checkShape(const IPosition& otherShape) const
{
  const uInt nAxes = min(itsFixedShape.nelements(), otherShape.nelements());
  DebugAssert(nAxes > 0 && nAxes < 4, AipsError);
  if (nAxes > 1 && itsFixedShape(1) != otherShape(1)) {
    throw(AipsError(String("MSConcat::checkShapes\n") +
		    String("cannot concatenate this measurement set as ") +
		    String("it has a different number of channels\n") +
		    String("and this cannot be changed")));
  }
  if (itsFixedShape(0) != otherShape(0)) {
    throw(AipsError(String("MSConcat::checkShapes\n") +
		    String("cannot concatenate this measurement set as ") +
		    String("it has a different number of correlations\n") +
		    String("and this cannot be changed")));
  }
}

IPosition MSConcat::getShape(const MSDataDescColumns& ddCols,
			     const MSSpWindowColumns& spwCols,
			     const MSPolarizationColumns& polCols,
			     uInt whichShape) {
  DebugAssert(whichShape < ddCols.nrow(), AipsError);
  const Int polId = ddCols.polarizationId()(whichShape);
  DebugAssert(polId >= 0 && polId < static_cast<Int>(polCols.nrow()),
	      AipsError);
  const Int spwId = ddCols.spectralWindowId()(whichShape);
  DebugAssert(spwId >= 0 && spwId < static_cast<Int>(spwCols.nrow()),
	      AipsError);
  const Int nCorr = polCols.numCorr()(polId);
  DebugAssert(nCorr > 0, AipsError);
  const Int nChan = spwCols.numChan()(spwId);
  DebugAssert(nChan > 0, AipsError);
  return IPosition(2, nCorr, nChan);
}

void MSConcat::checkCategories(const MSMainColumns& otherCols) const {
   LogIO os(LogOrigin("MSConcat", "checkCategories"));
  const Vector<String> cat = flagCategories();
  const Vector<String> otherCat = otherCols.flagCategories();
  const uInt nCat = cat.nelements();
  if (nCat != otherCat.nelements()) {
    os << LogIO::WARN
       <<"Flag category column shape does not match in these two MSs.\n"
       <<"This may not be important as Flag category is being deprecated. Will try to continue ..."
       << LogIO::POST;
    return;
  }
  for (uInt c = 0; c < nCat; c++) {
    if (cat(c) != otherCat(c)) {
      os << LogIO::WARN
	 <<"Flag category column shape does not match in these two MSs.\n"
	 <<"This may not be important as Flag category is being deprecated. Will try to continue ..."
	 << LogIO::POST;
      return;
    }
  }
}


Bool MSConcat::copyPointing(const MSPointing& otherPoint,const
			    Block<uInt>& newAntIndices ){

  LogIO os(LogOrigin("MSConcat", "copyPointing"));

  Bool itsPointingNull = (itsMS.pointing().isNull() || (itsMS.pointing().nrow() == 0));
  Bool otherPointingNull = (otherPoint.isNull() || (otherPoint.nrow() == 0));

  if(itsPointingNull &&  otherPointingNull){ // neither of the two MSs do have valid pointing tables
    os << LogIO::NORMAL << "No valid pointing tables present. Result won't have one either." << LogIO::POST;
    return True;
  }
  else if(itsPointingNull && !otherPointingNull){
    os << LogIO::WARN << itsMS.tableName() << " does not have a valid pointing table," << endl
       << "  the MS to be appended, however, has one. Result won't have one."
       << LogIO::POST;
    return False;
  }
  else if(!itsPointingNull && otherPointingNull){
    os << LogIO::WARN << "MS to be appended does not have a valid pointing table, "
       << itsMS.tableName() << ", however, has one. Result won't have one." << LogIO::POST;

    RowNumbers delrows(itsMS.pointing().nrow());
    indgen(delrows);
    itsMS.pointing().removeRow(RowNumbers(delrows));

    return False;

  }

  MSPointing& point=itsMS.pointing();
  Int actualRow=point.nrow()-1;
  Int origNRow= actualRow+1;
  Int rowToBeAdded=otherPoint.nrow();
  TableRow pointRow(point);
  const ROTableRow otherPointRow(otherPoint);
  for (Int k=0; k <  rowToBeAdded; ++k){
    ++actualRow;
    point.addRow();
    pointRow.put(actualRow, otherPointRow.get(k, True));

  }

  //Now reassigning antennas to the new indices of the ANTENNA table

  if(rowToBeAdded > 0){
    MSPointingColumns pointCol(point);
    // check antenna IDs
    Vector<Int> antennaIDs=pointCol.antennaId().getColumn();
    Bool idsOK = True;
    Int maxID = static_cast<Int>(newAntIndices.nelements()) - 1;
    for (Int k=origNRow; k <  (origNRow+rowToBeAdded); ++k){
      if(antennaIDs[k] < 0 || antennaIDs[k] > maxID){
	idsOK = False;
	break;
      }
    }
    if(!idsOK){
      os << LogIO::WARN
	 << "Found invalid antenna ids in the POINTING table; the POINTING table will be emptied as it is inconsistent"
	 << LogIO::POST;
      RowNumbers rowtodel(point.nrow());
      indgen(rowtodel);
      point.removeRow(RowNumbers(rowtodel));
      return False;
    }

    for (Int k=origNRow; k <  (origNRow+rowToBeAdded); ++k){
      pointCol.antennaId().put(k, newAntIndices[antennaIDs[k]]);
    }
  }
    return True;

}

Bool MSConcat::copyPointingB(MSPointing& otherPoint,const
			    Block<uInt>& newAntIndices ){

  // prepare otherPoint such that it can be virtually concatenated later
  // (don't write the itsMS)

  LogIO os(LogOrigin("MSConcat", "copyPointing"));

  Bool itsPointingNull = (itsMS.pointing().isNull() || (itsMS.pointing().nrow() == 0));
  Bool otherPointingNull = (otherPoint.isNull() || (otherPoint.nrow() == 0));

  if(itsPointingNull &&  otherPointingNull){ // neither of the two MSs do have valid pointing tables
    os << LogIO::NORMAL << "No valid pointing tables present. Result won't have one either." << LogIO::POST;
    return True;
  }
  else if(itsPointingNull && !otherPointingNull){
    os << LogIO::WARN << itsMS.tableName() << " does not have a valid pointing table," << endl
       << "  the MS to be appended, however, has one. Result won't have one."
       << LogIO::POST;

    RowNumbers delrows(otherPoint.nrow());
    indgen(delrows);
    otherPoint.removeRow(RowNumbers(delrows));

    return False;
  }
//   else if(!itsPointingNull && otherPointingNull){
//     os << LogIO::NORMAL << "MS to be appended does not have a valid pointing table, "
//        << itsMS.tableName() << ", however, has one. Result won't have one." << LogIO::POST;

//     RowNumbers delrows(itsMS.pointing().nrow());
//     indgen(delrows);
//     itsMS.pointing().removeRow(delrows);

//     return False;

//   }

  Int rowToBeAdded=otherPoint.nrow();
  //reassigning antennas to the new indices of the ANTENNA table

  if(rowToBeAdded > 0){
    MSPointingColumns pointCol(otherPoint);
    // check antenna IDs
    Vector<Int> antennaIDs=pointCol.antennaId().getColumn();
    Bool idsOK = True;
    Int maxID = static_cast<Int>(newAntIndices.nelements()) - 1;
    for (Int k=0; k < rowToBeAdded; k++){
      if(antennaIDs[k] < 0 || antennaIDs[k] > maxID){
	idsOK = False;
	break;
      }
      else{
	antennaIDs[k] = newAntIndices[antennaIDs[k]];
      }
    }
    if(!idsOK){
      os << LogIO::WARN
	 << "Found invalid antenna ids in the POINTING table; the POINTING table will be emptied as it is inconsistent"
	 << LogIO::POST;

      RowNumbers delrows(itsMS.pointing().nrow());
      indgen(delrows);
      itsMS.pointing().removeRow(RowNumbers(delrows));

      RowNumbers rowtodel(otherPoint.nrow());
      indgen(rowtodel);
      otherPoint.removeRow(RowNumbers(rowtodel));

      return False;
    }

    pointCol.antennaId().putColumn(antennaIDs);

  }
    return True;

}


Bool MSConcat::copySysCal(const MSSysCal& otherSysCal,
			  const Block<uInt>& newAntIndices){

  LogIO os(LogOrigin("MSConcat", "copySysCal"));

  Bool itsSysCalNull = (itsMS.sysCal().isNull() || (itsMS.sysCal().nrow() == 0));
  Bool otherSysCalNull = (otherSysCal.isNull() || (otherSysCal.nrow() == 0));

  if(itsSysCalNull && otherSysCalNull){ // neither of the two MSs do have valid syscal tables
    os << LogIO::NORMAL << "No valid syscal tables present. Result won't have one either." << LogIO::POST;
    return True;
  }
  else if(itsSysCalNull && !otherSysCalNull){
    os << LogIO::WARN << itsMS.tableName() << " does not have a valid syscal table," << endl
       << "  the MS to be appended, however, has one. Result won't have one."
       << LogIO::POST;
    return False;
  }

  MSSysCal& sysCal=itsMS.sysCal();
  Int actualRow=sysCal.nrow()-1;
  Int origNRow=actualRow+1;
  Int rowToBeAdded=otherSysCal.nrow();
  TableRow sysCalRow(sysCal);
  const ROTableRow otherSysCalRow(otherSysCal);
  for (Int k=0; k < rowToBeAdded; ++k){
    ++actualRow;
    sysCal.addRow();
    sysCalRow.put(actualRow, otherSysCalRow.get(k, True));
  }

  //Now reassigning antennas to the new indices of the ANTENNA table

  if(rowToBeAdded > 0){
    MSSysCalColumns sysCalCol(sysCal);
    // check antenna IDs
    Vector<Int> antennaIDs=sysCalCol.antennaId().getColumn();
    Bool idsOK = True;
    Int maxID = static_cast<Int>(newAntIndices.nelements()) - 1;
    for (Int k=origNRow; k < (origNRow+rowToBeAdded); ++k){
      if(antennaIDs[k] < 0 || antennaIDs[k] > maxID){
	idsOK = False;
	break;
      }
    }
    if(!idsOK){
      os << LogIO::WARN
	 << "Found invalid antenna ids in the SYSCAL table; the SYSCAL table will be emptied as it is inconsistent"
	 << LogIO::POST;
      RowNumbers rowtodel(sysCal.nrow());
      indgen(rowtodel);
      sysCal.removeRow(RowNumbers(rowtodel));
      return False;
    }

    for (Int k=origNRow; k < (origNRow+rowToBeAdded); ++k){
      sysCalCol.antennaId().put(k, newAntIndices[antennaIDs[k]]);
    }
  }

  return True;
}

Bool MSConcat::copyWeather(const MSWeather& otherWeather,
			   const Block<uInt>& newAntIndices){

  LogIO os(LogOrigin("MSConcat", "copyWeather"));

  Bool itsWeatherNull = (itsMS.weather().isNull() || (itsMS.weather().nrow() == 0));
  Bool otherWeatherNull = (otherWeather.isNull() || (otherWeather.nrow() == 0));

  if(itsWeatherNull && otherWeatherNull){ // neither of the two MSs do have valid weather tables
    os << LogIO::NORMAL << "No valid weather tables present. Result won't have one either." << LogIO::POST;
    return True;
  }
  else if(itsWeatherNull && !otherWeatherNull){
    os << LogIO::WARN << itsMS.tableName() << " does not have a valid weather table," << endl
       << "  the MS to be appended, however, has one. Result won't have one."
       << LogIO::POST;
    return False;
  }

  MSWeather& weather=itsMS.weather();
  Int actualRow=weather.nrow()-1;
  Int origNRow=actualRow+1;
  Int rowToBeAdded=otherWeather.nrow();
  TableRow weatherRow(weather);
  const ROTableRow otherWeatherRow(otherWeather);
  for (Int k=0; k < rowToBeAdded; ++k){
    ++actualRow;
    weather.addRow();
    weatherRow.put(actualRow, otherWeatherRow.get(k, True));
  }

  //Reassign antennas to the new indices of the ANTENNA table

  if(rowToBeAdded > 0){
    MSWeatherColumns weatherCol(weather);
    // check antenna IDs
    Vector<Int> antennaIDs=weatherCol.antennaId().getColumn();
    Bool idsOK = True;
    Int maxID = static_cast<Int>(newAntIndices.nelements()) - 1;
    for (Int k=origNRow; k < (origNRow+rowToBeAdded); ++k){
      if(antennaIDs[k] < -1 || antennaIDs[k] > maxID){
	os << LogIO::WARN
	   << "Found invalid antenna id " << antennaIDs[k] << " in the WEATHER table; the WEATHER table will be emptied as it is inconsistent"
	   << LogIO::POST;
	idsOK = False;
	break;
      }
      //// the following could be commented in if a warning about undefined antenna ids was deemed useful
      // else if(antennaIDs[k] == -1){
      // 	os << LogIO::WARN
      // 	   << "Found undefined antenna ids (value -1) in the WEATHER table; these will not be reindexed."
      // 	   << LogIO::POST;
      // 	break;
      // }
    }
    if(!idsOK){
      RowNumbers rowstodel(weather.nrow());
      indgen(rowstodel);
      weather.removeRow(RowNumbers(rowstodel));
      return False;
    }

    for (Int k=origNRow; k < (origNRow+rowToBeAdded); ++k){
      if(antennaIDs[k]>-1){
	weatherCol.antennaId().put(k, newAntIndices[antennaIDs[k]]);
      }
    }
  }

  return True;
}


Int MSConcat::copyObservation(const MSObservation& otherObs,
			      const Bool remRedunObsId){
  LogIO os(LogOrigin("MSConcat", "copyObservation"));

  MSObservation& obs=itsMS.observation();
  TableRow obsRow(obs);
  const ROTableRow otherObsRow(otherObs);
  newObsIndexA_p.clear();
  newObsIndexB_p.clear();
  std::map<Int, Int> tempObsIndex;
  std::map<Int, Int> tempObsIndexReverse;
  std::map<Int, Int> tempObsIndex2;
  doObsA_p = False;
  doObsB_p = True;

  Int originalNrow = obs.nrow(); // remember the original number of rows

  // copy the new obs rows over and note new ids in map
  Int actualRow=obs.nrow()-1;
  for (rownr_t k=0; k < otherObs.nrow() ; ++k){
    obs.addRow();
    ++actualRow;
    obsRow.put(actualRow, otherObsRow.get(k, True));
    tempObsIndex[k] = actualRow;
    tempObsIndexReverse[actualRow] = k;
  }
  if(remRedunObsId){ // remove redundant rows
    MSObservationColumns& obsCol = observation();
    Vector<Bool> rowToBeRemoved(obs.nrow(), False);
    vector<rownr_t> rowsToBeRemoved;
    for(rownr_t j=0; j<obs.nrow(); j++){ // loop over OBS table rows
      for (rownr_t k=j+1; k<obs.nrow(); k++){ // loop over remaining OBS table rows
	if(obsRowsEquivalent(obsCol, j, k)){ // rows equivalent?
	  // make entry in map for (k,j) and mark k for deletion
	  tempObsIndex2[k] = j;
	  if(tempObsIndexReverse.find(k) != tempObsIndexReverse.end()){ // remember that the observation was already in the obs table
	    otherObsIdsWithCounterpart_p[j] = k;
	  }
	  rowToBeRemoved(k) = True;
	  rowsToBeRemoved.push_back(k);
	}
      }
    }// end for j

    // create final maps
    // map for first table
    for(Int i=0; i<originalNrow; i++){ // loop over rows of old first table
      if(tempObsIndex2.find(i) != tempObsIndex2.end()){ // ID changed because of removal
        newObsIndexA_p[i] = tempObsIndex2.at(i);
	  doObsA_p = True;
      }
    }
    // map for second table
    for(rownr_t i=0; i<otherObs.nrow(); i++){ // loop over rows of second table
      if(tempObsIndex.find(i) != tempObsIndex.end()){ // ID changed because of addition to table
	if(tempObsIndex2.find(tempObsIndex.at(i)) != tempObsIndex2.end()){ // ID also changed because of removal
	  newObsIndexB_p[i] = tempObsIndex2.at(tempObsIndex.at(i));
	}
	else { // ID only changed because of addition to the table
	  newObsIndexB_p[i] = tempObsIndex.at(i);
	}
      }
    }
    if(rowsToBeRemoved.size()>0){ // actually remove the rows
      Vector<rownr_t> rowsTBR(rowsToBeRemoved);
      obs.removeRow(rowsTBR);
    }
    os << "Added " << obs.nrow()- originalNrow << " rows and matched "
       << rowsToBeRemoved.size() << " rows in the observation subtable." << LogIO::POST;

  }
  else {
    // create map for second table only
    for(rownr_t i=0; i<otherObs.nrow(); i++){ // loop over rows of second table
      if(tempObsIndex.find(i) != tempObsIndex.end()){ // ID changed because of addition to table
        newObsIndexB_p[i] = tempObsIndex.at(i);
      }
    }
    os << "Added " << obs.nrow()- originalNrow << " rows in the observation subtable." << LogIO::POST;
  } // end if(remRedunObsId)

  return obs.nrow();
}


Int MSConcat::copyProcessor(const MSProcessor& otherProc,
			      const Bool remRedunProcId){
  LogIO os(LogOrigin("MSConcat", "copyProcessor"));

  MSProcessor& proc=itsMS.processor();
  TableRow procRow(proc);
  const ROTableRow otherProcRow(otherProc);
  newProcIndexA_p.clear();
  newProcIndexB_p.clear();
  std::map<Int, Int> tempProcIndex;
  std::map<Int, Int> tempProcIndex2;
  doProcA_p = False;
  doProcB_p = True;

  Int originalNrow = proc.nrow(); // remember the original number of rows

  // copy the new proc rows over and note new ids in map
  Int actualRow=proc.nrow()-1;
  for (uInt k=0; k < otherProc.nrow() ; ++k){
    proc.addRow();
    ++actualRow;
    procRow.put(actualRow, otherProcRow.get(k, True));
    tempProcIndex[k] = actualRow;
  }
  if(remRedunProcId){ // remove redundant rows
    MSProcessorColumns& procCol = processor();
    Vector<Bool> rowToBeRemoved(proc.nrow(), False);
    vector<rownr_t> rowsToBeRemoved;
    for(uInt j=0; j<proc.nrow(); j++){ // loop over PROC table rows
      for (uInt k=j+1; k<proc.nrow(); k++){ // loop over remaining PROC table rows
	if(procRowsEquivalent(procCol, j, k)){ // rows equivalent?
	  // make entry in map for (k,j) and mark k for deletion
	  tempProcIndex2[k] = j;
	  rowToBeRemoved(k) = True;
	  rowsToBeRemoved.push_back(k);
	}
      }
    }// end for j

    if(rowsToBeRemoved.size()<otherProc.nrow()){ // there were new rows, need to determined their index, too
      uInt removedSoFar=0;
      for(uInt j=0; j<proc.nrow(); j++){ // loop over PROC table rows
	if(!rowToBeRemoved(j)){
	  if(removedSoFar>0){
	    // make entry in map for (j,j-removedSoFar), i.e. move j up by removeSoFar
	    tempProcIndex2[j] = j-removedSoFar;
	  }
	}
	else {
	  removedSoFar++;
	}
      }
    }

    // create final maps
    // map for first table
    for(Int i=0; i<originalNrow; i++){ // loop over rows of old first table
      if(tempProcIndex2.find(i) != tempProcIndex2.end()){ // ID changed because of removal
        newProcIndexA_p[i] = tempProcIndex2.at(i);
	  doProcA_p = True;
      }
    }
    // map for second table
    for(uInt i=0; i<otherProc.nrow(); i++){ // loop over rows of second table
      if(tempProcIndex.find(i) != tempProcIndex.end()){ // ID changed because of addition to table
	if(tempProcIndex2.find(tempProcIndex.at(i)) != tempProcIndex2.end()){ // ID also changed because of removal
	  newProcIndexB_p[i] = tempProcIndex2.at(tempProcIndex.at(i));
	}
	else { // ID only changed because of addition to the table
	  newProcIndexB_p[i] = tempProcIndex.at(i);
	}
      }
    }
    if(rowsToBeRemoved.size()>0){ // actually remove the rows
      Vector<rownr_t> rowsTBR(rowsToBeRemoved);
      proc.removeRow(rowsTBR);
    }
    os << "Added " << proc.nrow()- originalNrow << " rows and matched "
       << rowsToBeRemoved.size() << " rows in the processor subtable." << LogIO::POST;

  }
  else {
    // create map for second table only
    for(uInt i=0; i<otherProc.nrow(); i++){ // loop over rows of second table
      if(tempProcIndex.find(i) != tempProcIndex.end()){ // ID changed because of addition to table
        newProcIndexB_p[i] = tempProcIndex.at(i);
      }
    }
    os << "Added " << proc.nrow()- originalNrow << " rows in the processor subtable." << LogIO::POST;
  } // end if(remRedunProcId)

  return proc.nrow();
}



Block<uInt> MSConcat::copyAntennaAndFeed(const MSAntenna& otherAnt,
					 const MSFeed& otherFeed) {
  // uses newSPWIndex_p; to be called after copySpwAndPol

  LogIO os(LogOrigin("MSConcat", "copyAntennaAndFeed"));

  const uInt nAntIds = otherAnt.nrow();
  Block<uInt> antMap(nAntIds);

  const MSAntennaColumns otherAntCols(otherAnt);
  MSAntennaColumns& antCols = antenna();
  MSAntenna& ant = itsMS.antenna();
  const Quantum<Double> tol(1, "m");
  const ROTableRow otherAntRow(otherAnt);
  TableRow antRow(ant);
  TableRecord antRecord;
  //RecordFieldId nameAnt(MSAntenna::columnName(MSAntenna::NAME));

  MSFeedColumns& feedCols = feed();
  const MSFeedColumns otherFeedCols(otherFeed);

  const String& antIndxName = MSFeed::columnName(MSFeed::ANTENNA_ID);
  const String& spwIndxName = MSFeed::columnName(MSFeed::SPECTRAL_WINDOW_ID);
  MSFeed& feed = itsMS.feed();
  const ROTableRow otherFeedRow(otherFeed);
  TableRow feedRow(feed);
  TableRecord feedRecord, feedRecord2;
  ColumnsIndex feedIndex(otherFeed, Vector<String>(1, antIndxName));
  ColumnsIndex itsFeedIndex(feed, Vector<String>(1, antIndxName));

  RecordFieldPtr<Int> antInd(feedIndex.accessKey(), antIndxName);
  RecordFieldPtr<Int> itsAntInd(itsFeedIndex.accessKey(), antIndxName);

  RecordFieldId antField(antIndxName);
  RecordFieldId spwField(spwIndxName);


  if(!feedCols.focusLengthQuant().isNull() && otherFeedCols.focusLengthQuant().isNull()){
    os << LogIO::WARN << "MS appended to has optional column FOCUS_LENGTH in FEED table, but MS to be appended does not.\n"
       << "Potential new rows in FEED will have FOCUS_LENGTH zero." << LogIO::POST;
  }
  else if(feedCols.focusLengthQuant().isNull() && !otherFeedCols.focusLengthQuant().isNull()){
    os << LogIO::WARN << "MS appended to does not have optional column FOCUS_LENGTH in FEED table, but MS to be appended does.\n"
       << "Output FEED table will not have a FOCUS_LENGTH column." << LogIO::POST;
  }

  for (uInt a = 0; a < nAntIds; a++) {
    const Int newAntId = antCols.matchAntennaAndStation(otherAntCols.name()(a),
							otherAntCols.station()(a),
							otherAntCols.positionMeas()(a), tol);

    Bool addNewEntry = True;

    if (newAntId >= 0) {

      // Check that the FEED table contains all the entries for
      // this antenna and that they are the same.

      *antInd = a;
      *itsAntInd = newAntId;
      const Vector<rownr_t> feedsToCompare = feedIndex.getRowNumbers();
      const Vector<rownr_t> itsFeedsToCompare = itsFeedIndex.getRowNumbers();
      const uInt nFeedsToCompare = feedsToCompare.nelements();
      uInt matchingFeeds = 0;
      RowNumbers ignoreRows;
      Unit s("s");
      Unit m("m");

      if(itsFeedsToCompare.nelements() == nFeedsToCompare){
	//cout << "Antenna " << a << " same number of feeds: "<< nFeedsToCompare << endl;
	for(uInt f=0; f<nFeedsToCompare; f++){
	  uInt k = feedsToCompare(f);
	  Quantum<Double> newTimeQ;
	  Quantum<Double> newIntervalQ;

	  Int newSPWId = otherFeedCols.spectralWindowId()(k);
	  if(doSPW_p){ // the SPW table was rearranged
	    //cout << "modifiying spwid from " << newSPWId << " to " << newSPWIndex_p.at(newSPWId) << endl;
	    newSPWId = getMapValue (newSPWIndex_p, newSPWId);
	  }
	  Quantum<Double> fLengthQ;
	  if(!otherFeedCols.focusLengthQuant().isNull()){
	    fLengthQ = otherFeedCols.focusLengthQuant()(k);
	  }
	  const Int matchingFeedRow = feedCols.matchFeed(newTimeQ,
							 newIntervalQ,
							 a,
							 otherFeedCols.feedId()(k),
							 newSPWId,
							 otherFeedCols.timeQuant()(k),
							 otherFeedCols.intervalQuant()(k),
							 otherFeedCols.numReceptors()(k),
							 otherFeedCols.beamOffsetQuant()(k),
							 otherFeedCols.polarizationType()(k),
							 otherFeedCols.polResponse()(k),
							 otherFeedCols.positionQuant()(k),
							 otherFeedCols.receptorAngleQuant()(k),
							 ignoreRows,
							 fLengthQ
							 );
	  if(matchingFeedRow>=0){
	    //cout << "Antenna " << a << " found matching feed " << matchingFeedRow << endl;
	    if(newTimeQ.getValue(s)!=0.){ // need to adjust time information

//  	      cout << "this " << feedCols.timeQuant()(matchingFeedRow).getValue(s) << " "
//  	          <<  feedCols.intervalQuant()(matchingFeedRow).getValue(s) << endl;
//  	      cout << " other " << otherFeedCols.timeQuant()(k).getValue(s) << " "
//  	          << otherFeedCols.intervalQuant()(k).getValue(s)   << endl;
//  	      cout << " new " << newTimeQ.getValue(s) << " " << newIntervalQ.getValue(s) << endl;

	      // modify matchingFeedRow
	      feedCols.timeQuant().put(matchingFeedRow, newTimeQ);
	      feedCols.intervalQuant().put(matchingFeedRow, newIntervalQ);
	    }
	    matchingFeeds++;
	    ignoreRows.resize(matchingFeeds, True);
	    ignoreRows(matchingFeeds-1) = matchingFeedRow;
	  }
	}
      }

      antMap[a] = newAntId;
      addNewEntry = False;

      if(matchingFeeds != nFeedsToCompare){
//  	cout << "Antenna " << a << " did not find all needed feeds "
//  	     << matchingFeeds << "/" << nFeedsToCompare << endl;
	const Vector<rownr_t> feedsToCopy = feedIndex.getRowNumbers();
	const uInt nFeedsToCopy = feedsToCopy.nelements();
	rownr_t destRow = feed.nrow();
	uInt rCount = 0;
	for (uInt f = 0; f < nFeedsToCopy; f++) {
	  Bool present=False;
	  for(uInt g=0; g<matchingFeeds; g++){
	    if(feedsToCopy(f)==ignoreRows(g)){
	      present=True;
	      break;
	    }
	  }
	  if(!present){
	    feed.addRow(1);
	    feedRecord = otherFeedRow.get(feedsToCopy(f));
	    feedRecord.define(antField, static_cast<Int>(antMap[a]));
	    if(doSPW_p){ // the SPW table was rearranged
	      Int newSPWId = otherFeedCols.spectralWindowId()(feedsToCopy(f));
//  	      cout << "When writing new feed row: modifiying spwid from " << newSPWId
//  		   << " to " << newSPWIndex_p.at(newSPWId) << endl;
	      feedRecord.define(spwField, getMapValue(newSPWIndex_p, newSPWId));
	    }
	    feedRow.putMatchingFields(destRow, feedRecord);
	    rCount++;
	    destRow++;
	  }
	}
	//	cout << "Added " << rCount << " rows to the Feed table." << endl;
      }
//       else{
// 	cout << "Antenna " << a << " found all matching feeds: " << matchingFeeds << endl;
//       }

    }


    if(addNewEntry){ // need to add a new entry in the ANTENNA subtable

      antMap[a] = ant.nrow();
      ant.addRow();
      antRecord = otherAntRow.get(a);

      // determine if the antenna was just moved
      Int movedAntId=-1;
      if( (movedAntId=antCols.matchAntenna(otherAntCols.name()(a),
					   otherAntCols.positionMeas()(a), Quantum<Double>(100, "AU")))
	  >= 0){
	os << "*** Antenna " << antCols.name()(movedAntId) << " (station " <<  antCols.station()(movedAntId)
	   << ", ID " << movedAntId << ") has changed its position between MSs."  << endl
 	   << "    Moved antenna is on station " << otherAntCols.station()(a)
	   << " and will have ID " << antMap[a] << "." << LogIO::POST;
// 	String newName = otherAntCols.name()(a)+"m";
// 	Int secondMovedAntId = -1;
// 	Int count = 1;
// 	while((secondMovedAntId=antCols.matchAntenna(newName,
// 						     otherAntCols.positionMeas()(a), Quantum<Double>(100, "AU")))
// 	      >= 0){ // append numbers starting at 2 until there is no match
// 	  newName = newName+String::toString(++count);
// 	  movedAntId = secondMovedAntId;
// 	}
// 	os << "Antenna " << antCols.name()(movedAntId)  << " (ID " << movedAntId << ") has changed its position between MSs."
// 	   << " Moved antenna will be named " << newName << " (ID " << antMap[a] << ")" << LogIO::POST;
// 	antRecord.define(nameAnt, newName); // append an "m" to the name to make it unique
      }

      antRow.putMatchingFields(antMap[a], antRecord);

      // Copy all the feeds associated with the antenna into the feed
      // table. I'm assuming that they are not already there.
      *antInd = a;
      const Vector<rownr_t> feedsToCopy = feedIndex.getRowNumbers();
      const uInt nFeedsToCopy = feedsToCopy.nelements();
      rownr_t destRow = feed.nrow();
      feed.addRow(nFeedsToCopy);
      //cout << "antenna " << antMap[a] << ": copying " <<  nFeedsToCopy << " feeds." << endl;
      for (uInt f = 0; f < nFeedsToCopy; f++, destRow++) {
	feedRecord = otherFeedRow.get(feedsToCopy(f));
	feedRecord.define(antField, static_cast<Int>(antMap[a]));
	Int newSPWId = otherFeedCols.spectralWindowId()(feedsToCopy(f));
	if(doSPW_p){ // the SPW table was rearranged
	  //cout << "modifiying spwid from " << newSPWId << " to " << newSPWIndex_p.at(newSPWId) << endl;
	  newSPWId = getMapValue(newSPWIndex_p, newSPWId);
	  feedRecord.define(spwField, newSPWId);
	}
	feedRow.putMatchingFields(destRow, feedRecord);
      }
    }
  }
  return antMap;
}

Block<uInt> MSConcat::copyState(const MSState& otherState) {
  const uInt nStateIds = otherState.nrow();
  Block<uInt> stateMap(nStateIds);

  const MSStateColumns otherStateCols(otherState);
  MSStateColumns& stateCols = state();
  MSState& stateT = itsMS.state();
  const ROTableRow otherStateRow(otherState);
  TableRow stateRow(stateT);
  const Quantum<Double> tol(1, "K");

  for (uInt s = 0; s < nStateIds; s++) {
    const Int newStateId = stateCols.matchState(otherStateCols.calQuant()(s),
						otherStateCols.loadQuant()(s),
						otherStateCols.obsMode()(s),
						otherStateCols.ref()(s),
						otherStateCols.sig()(s),
						otherStateCols.subScan()(s),
						tol);
    if (newStateId >= 0) {
      stateMap[s] = newStateId;
    } else { // need to add a new entry in the STATE subtable
      stateMap[s] = stateT.nrow();
      stateT.addRow();
      stateRow.putMatchingFields(stateMap[s], otherStateRow.get(s));
    }
  }
  return stateMap;
}

Block<uInt>  MSConcat::copyField(const MeasurementSet& otherms) {
  const MSField otherFld = otherms.field();
  const rownr_t nFlds = otherFld.nrow();
  Block<uInt> fldMap(nFlds);
  const Quantum<Double> tolerance=itsDirTol;
  const MSFieldColumns otherFieldCols(otherFld);
  MSFieldColumns& fieldCols = field();

  const MDirection::Types dirType = MDirection::castType(
    fieldCols.referenceDirMeasCol().getMeasRef().getType());
  const MDirection::Types otherDirType = MDirection::castType(
    otherFieldCols.referenceDirMeasCol().getMeasRef().getType());

  MDirection::Convert dirCtr;
  if (dirType != otherDirType) { // setup a converter
    dirCtr = MDirection::Convert(otherDirType, dirType);
  }
  MDirection refDir, delayDir, phaseDir;
  MSField& fld = itsMS.field();
  const ROTableRow otherFldRow(otherFld);
  RecordFieldId sourceIdId(MSSource::columnName(MSSource::SOURCE_ID));

  // find max ephemeris id
  Int maxThisEphId = -2; // meaning there is no EPHEMERIS_ID column in the field table
  Vector<Double> validityRange;

  if(itsMS.field().actualTableDesc().isColumn(MSField::columnName(MSField::EPHEMERIS_ID))){
    for(rownr_t i=0; i<fieldCols.nrow(); i++){
      if(fieldCols.ephemerisId()(i)>maxThisEphId){
	maxThisEphId = fieldCols.ephemerisId()(i);
      }
    }
  }
  if(maxThisEphId>-1){ // this MS has at least one field using an ephemeris.
                       // maxThisEphId==-1 would mean there is an EPHEMERIS_ID column but there are no entries
    // find first and last obs time of other MS
    RowNumbers sortedI(otherms.nrow());
    MSMainColumns msmc(otherms);
    Vector<Double> mainTimesV = msmc.time().getColumn();
    GenSortIndirect<Double,rownr_t>::sort(sortedI,mainTimesV);
    validityRange.resize(2);
    validityRange(0) = mainTimesV(sortedI(0));
    validityRange(1) = mainTimesV(sortedI(otherms.nrow()-1));
  }


  TableRow fldRow(fld);
  for (uInt f = 0; f < nFlds; f++) {

    String ephPath = otherFieldCols.ephemPath(f);
    Double otherOrigTime = otherFieldCols.time()(f);

    try{
      delayDir = otherFieldCols.delayDirMeas(f);
      phaseDir = otherFieldCols.phaseDirMeas(f);
      refDir = otherFieldCols.referenceDirMeas(f);
    }
    catch(AipsError& x){
      if(!ephPath.empty()){
	LogIO os(LogOrigin("MSConcat", "copyField"));
	os << LogIO::SEVERE << "Field " << f << " (" << otherFieldCols.name()(f) << ", to be appended)"
	   << " is using an ephemeris with incorrect time origin setup: the time origin (" << otherOrigTime
	   << " s) in the FIELD table is outside the validity range of the ephemeris." << LogIO::POST;
      }
      throw(x);
    }

    if (dirType != otherDirType) {
      delayDir = dirCtr(delayDir.getValue());
      phaseDir = dirCtr(phaseDir.getValue());
      refDir = dirCtr(refDir.getValue());
    }

    const Int newFld = fieldCols.matchDirection(refDir, delayDir, phaseDir, tolerance,
						-1, // don't specify a tryrow
						otherOrigTime); // compare at the start time of the other field
    // cout << "other field, newFld " << f << ", " << newFld << endl;

    Bool canUseThisEntry = (newFld>=0);
    if(canUseThisEntry){
      String thisEphPath = fieldCols.ephemPath(newFld);
      if(!thisEphPath.empty()){ // this field uses an ephemeris
	if(ephPath.empty()){ // the other field does not
	  canUseThisEntry = False;
	}
	else{ // both use an ephemeris
	  // is the time coverage of this ephem sufficient to be also used for the other field?
	  stringstream ss;
	  for(uInt i=0; i<2; i++){
	    try{
	      MDirection tMDir = fieldCols.phaseDirMeas(newFld, validityRange(i));
	    }
	    catch(AipsError& x){
	      canUseThisEntry = False;
	      ss << validityRange(i) << ", ";
	    }
	  }
	  if(!canUseThisEntry){
	    LogIO os(LogOrigin("MSConcat", "copyField"));
	    os << LogIO::NORMAL << "Ephemeris " << thisEphPath << endl
	       << " from field " << newFld << " (" << fieldCols.name()(newFld) << ") "
	       << " cannot be used for data from field " << f << " (" << otherFieldCols.name()(f) << ", to be appended)"
	       << " because it does not cover time(s) " << ss.str() << endl
	       << " creating separate FIELD table entry." << LogIO::POST;
	  }
	}
      }
      else{ // this field does not use an ephemeris
	if(!ephPath.empty()){ // the other field does
	  canUseThisEntry = False;
	}
      }
    }

    if ( canUseThisEntry
	 && (!itsRespectForFieldName
	     || (itsRespectForFieldName && fieldCols.name()(newFld) == otherFieldCols.name()(f))
	     )
	) {
	fldMap[f] = newFld;
    }
    else { // need to add a new entry in the FIELD subtable
      fldMap[f] = fld.nrow();
      fld.addRow();
      fldRow.putMatchingFields(fldMap[f], otherFldRow.get(f));

      if (dirType != otherDirType) {
 	DebugAssert(fieldCols.numPoly()(fldMap[f]) == 0, AipsError);
 	Vector<MDirection> vdir(1, refDir);
 	fieldCols.referenceDirMeasCol().put(fldMap[f], vdir);
 	vdir(0) = delayDir;
 	fieldCols.delayDirMeasCol().put(fldMap[f], vdir);
 	vdir(0) = phaseDir;
 	fieldCols.phaseDirMeasCol().put(fldMap[f], vdir);
      }

      if(!ephPath.empty() && otherFieldCols.ephemerisId()(f)>-1){ // f has a non-trivial ephemeris id
	maxThisEphId++;
	String ephName = Path(ephPath).baseName();
	if(!fld.addEphemeris(maxThisEphId, ephPath,
			     ephName.substr(ephName.find("_")+1, ephName.size()-4-ephName.find("_")-1)) // extract comment from name
	   ){
	  LogIO os(LogOrigin("MSConcat", "copyField"));
	  os << LogIO::SEVERE << "Error transferring ephemeris " << ephPath << " to concatvis." << LogIO::POST;
	}
	fieldCols.ephemerisId().put(fldMap[f], maxThisEphId);
      }
      else if(maxThisEphId>-2){ // this MS has an ephemeris id column
	// for the case the appended MS has no ephemeris column, need to set the default explicitly
	fieldCols.ephemerisId().put(fldMap[f], -1);
      }

      //source table has been concatenated; use new index reference
      if(doSource_p){
	Int oldIndex=fieldCols.sourceId()(fldMap[f]);
	if(newSourceIndex_p.find(oldIndex) != newSourceIndex_p.end()){
	  fieldCols.sourceId().put(fldMap[f], newSourceIndex_p.at(oldIndex));
	}
      }
      if(doSource2_p){
	Int oldIndex=fieldCols.sourceId()(fldMap[f]);
	if(newSourceIndex2_p.find(oldIndex) != newSourceIndex2_p.end()){
	  fieldCols.sourceId().put(fldMap[f], newSourceIndex2_p.at(oldIndex));
	}
      }
    }
  }
  return fldMap;
}

Bool MSConcat::copySource(const MeasurementSet& otherms){
  doSource_p=False;
  if(Table::isReadable(itsMS.sourceTableName())){
    MSSource& newSource=itsMS.source();
    MSSourceColumns& sourceCol=source();
    Int maxSrcId=0;
    if(!Table::isReadable(otherms.sourceTableName())){
      return False;
    }
    const MSSource& otherSource=otherms.source();
    if(otherSource.nrow()==0){
      return False;
    }
    if(newSource.nrow()==0){
      maxSrcId = -1;
      //cout << "Initial source table is empty." << endl;
    }
    else{
      maxSrcId=max(sourceCol.sourceId().getColumn());
    }

    TableRecord sourceRecord;
    newSourceIndex_p.clear();
    Int numrows=otherSource.nrow();
    Int destRow=newSource.nrow();
    MSSourceColumns otherSourceCol(otherms.source());
    Vector<Int> otherId=otherSourceCol.sourceId().getColumn();
    newSource.addRow(numrows);
    const ROTableRow otherSourceRow(otherSource);
    TableRow sourceRow(newSource);
    RecordFieldId sourceIdId(MSSource::columnName(MSSource::SOURCE_ID));
    RecordFieldId spwIdId(MSSource::columnName(MSSource::SPECTRAL_WINDOW_ID));
    // the spw ids
    Vector<Int> otherSpectralWindowId=otherSourceCol.spectralWindowId().getColumn();

    for (Int k =0 ; k < numrows ; ++k){
      sourceRecord = otherSourceRow.get(k);
      //define a new source id
      newSourceIndex_p[k] = maxSrcId+1+otherId(k);
      sourceRecord.define(sourceIdId, maxSrcId+1+otherId(k));

      //define a new temporary spw id by subtracting 10000
      // later to be replaced in updateSource
      if(otherSpectralWindowId(k)>=0){
	sourceRecord.define(spwIdId, otherSpectralWindowId(k)-10000);
      }

      sourceRow.putMatchingFields(destRow, sourceRecord);

      ++destRow;
    }

    doSource_p=True;

    solSystObjects_p.clear();

    const MSFieldColumns otherFieldCols(otherms.field());
    const MSFieldColumns fieldCols(itsMS.field());
    for(rownr_t i=0; i<itsMS.field().nrow(); i++){
      MDirection::Types refType = MDirection::castType(fieldCols.phaseDirMeas(i).getRef().getType());
      if(refType>=MDirection::MERCURY && refType<MDirection::N_Planets){ // we have a solar system object
	solSystObjects_p[fieldCols.sourceId()(i)] = (Int) refType;
      }
      if(!fieldCols.ephemPath(i).empty()){ // this is an ephemeris object
        solSystObjects_p[fieldCols.sourceId()(i)] = -2; // mark as -2
      }
    }
    for(rownr_t i=0; i<otherms.field().nrow(); i++){
      MDirection::Types refType = MDirection::castType(otherFieldCols.phaseDirMeas(i).getRef().getType());
      if(refType>=MDirection::MERCURY && refType<MDirection::N_Planets){ // we have a solar system object
	solSystObjects_p[otherFieldCols.sourceId()(i)+maxSrcId+1] = (Int) refType;
      }
      if(!otherFieldCols.ephemPath(i).empty()){ // this is an ephemeris object
	solSystObjects_p[otherFieldCols.sourceId()(i)+maxSrcId+1] = -2; // mark as -2
      }
    }

  }

  return doSource_p;
}

Bool MSConcat::updateSource(){ // to be called after copySource and copySpwAndPol
                               //   but before copyField!

  doSource2_p = False;

  if(Table::isReadable(itsMS.sourceTableName())){

    MSSource& newSource=itsMS.source();
    MSSourceColumns& sourceCol=source();

    // the number of rows in the source table
    Int numrows_this=newSource.nrow();

    if(numrows_this > 0){  // the source table is not empty

      TableRecord sourceRecord;

      // maps for recording the changes in source id
      std::map<Int, Int> tempSourceIndex;
      std::map<Int, Int> tempSourceIndex2;
      std::map<Int, Int> tempSourceIndex3;
      tempSourceIndex.clear();
      tempSourceIndex2.clear();
      tempSourceIndex3.clear();
      newSourceIndex2_p.clear();

      // the source columns
      Vector<Int> thisId=sourceCol.sourceId().getColumn();
      Vector<Int> thisSPWId=sourceCol.spectralWindowId().getColumn();

      // containers for the rows from the two input tables
      TableRow sourceRow(newSource);

      // convert the string containing the column name into a record field ID
      RecordFieldId sourceIdId(MSSource::columnName(MSSource::SOURCE_ID));
      RecordFieldId sourceSPWId(MSSource::columnName(MSSource::SPECTRAL_WINDOW_ID));

      // loop over the rows of the merged source table
      for (Int j =0 ; j < numrows_this ; ++j){
	if(thisSPWId(j)<-1){ // came from the second input table
	  sourceRecord = sourceRow.get(j);
	  if(doSPW_p || newSPWIndex_p.find(thisSPWId(j)+10000) != newSPWIndex_p.end()){ // the SPW table was rearranged
	    sourceCol.spectralWindowId().put(j, getMapValue(newSPWIndex_p, thisSPWId(j)+10000) );
	    //sourceRecord.define(sourceSPWId, newSPWIndex_p.at(thisSPWId(j)+10000) );
	  }
	  else { // the SPW table did not have to be rearranged, just revert changes to SPW from copySource
	    sourceCol.spectralWindowId().put(j,  thisSPWId(j)+10000 );
	    //sourceRecord.define(sourceSPWId, thisSPWId(j)+10000 );
	  }
	  //sourceRow.putMatchingFields(j, sourceRecord);
	} // end for j
      }

      // Check if there are redundant rows and remove them creating map for copyField
      // loop over the columns of the merged source table
      Vector<Bool> rowToBeRemoved(numrows_this, False);
      vector<rownr_t> rowsToBeRemoved;
      Vector<Int> thisSPWIdB=sourceCol.spectralWindowId().getColumn();

      for (Int j=0 ; j < numrows_this ; ++j){
	if(rowToBeRemoved(j)){
	  continue;
	}
	// check if row j has an equivalent row somewhere else in the table
	Int reftypej = getMapValue (solSystObjects_p, thisId(j));
	for (Int k=j+1 ; k < numrows_this ; ++k){
	  if (!rowToBeRemoved(k)){
	    if(thisSPWIdB(j)==thisSPWIdB(k)){ // the SPW id is the same
	      Int reftypek = getMapValue (solSystObjects_p, thisId(k));
 	      Bool sameSolSystObjects = ((reftypek==reftypej) && (reftypek>-1)) // object with solar syst ref frame
 		|| ((reftypek==reftypej) && (reftypek==-2)); // ephemeris object
	      if( sourceRowsEquivalent(sourceCol, j, k, sameSolSystObjects) ){ // and all columns are the same (not testing source, spw id, time, and interval)
		//cout << "Found SOURCE rows " << j << " and " << k << " to be identical." << endl;

		// set the time and interval to a superset of the two
		Double blowk = sourceCol.time()(k) - sourceCol.interval()(k)/2.;
		Double bhighk = sourceCol.time()(k) + sourceCol.interval()(k)/2.;
		Double blowj = sourceCol.time()(j) - sourceCol.interval()(j)/2.;
		Double bhighj = sourceCol.time()(j) + sourceCol.interval()(j)/2.;
		Double newInterval = max(bhighk,bhighj)-min(blowk,blowj);
		Double newTime = (max(bhighk,bhighj)+min(blowk,blowj))/2.;

		//cout << "new time = " << newTime << ", new interval = " << newInterval << endl;

		sourceCol.interval().put(j, newTime);
		sourceCol.interval().put(k, newTime);
		sourceCol.interval().put(j, newInterval);
		sourceCol.interval().put(k, newInterval);

		// make entry in map for (k, j) and delete k
		tempSourceIndex[thisId(k)] = thisId(j);
		rowToBeRemoved(k) = True;
		rowsToBeRemoved.push_back(k);
	      }
	    }
	  }
	}
      } // end for j

      Int newNumrows_this = numrows_this; // copy of number of rows
      Vector<Int> newThisId(thisId);      // copy of vector of IDs

      if(rowsToBeRemoved.size()>0){ // actually remove the rows
	Vector<rownr_t> rowsTBR(rowsToBeRemoved);
	newSource.removeRow(rowsTBR);
//	cout << "Removed " << rowsToBeRemoved.size() << " redundant rows from SOURCE table." << endl;
	newNumrows_this=newSource.nrow(); // update number of rows
 	sourceCol.sourceId().getColumn(newThisId, True); // update vector if IDs
      }

      // renumber consecutively
      Int nnrow = 0;
      for (Int j=0 ; j < newNumrows_this ; ++j){
	if(newThisId(j) > nnrow){
	  nnrow++;
	  //sourceRecord = sourceRow.get(j);
	  //sourceRecord.define(sourceIdId, nnrow );
	  //sourceRow.putMatchingFields(j, sourceRecord);
	  tempSourceIndex2[newThisId(j)] = nnrow;
	  sourceCol.sourceId().put(j, nnrow);
	}
      }

      // give equivalent rows the same source id
      Bool rowsRenamed(False);
      Int nDistinctSources = newNumrows_this;
      Vector<Int> thisSourceId=sourceCol.sourceId().getColumn();
      for (Int j=0 ; j < newNumrows_this ; ++j){
	// check if row j has an equivalent row somewhere down in the table
	Int reftypej = getMapValue (solSystObjects_p, thisId(j));
	for (Int k=j+1 ; k < newNumrows_this ; ++k){
	  if(thisSourceId(j)!=thisSourceId(k)){
	    Int reftypek = getMapValue (solSystObjects_p, thisId(k));
 	    Bool sameSolSystObjects = ((reftypek==reftypej) && (reftypek>-1)) // object with solar syst ref frame
 	      || ((reftypek==reftypej) && (reftypek==-2)); // ephemeris object;
	    if( sourceRowsEquivalent(sourceCol, j, k, sameSolSystObjects)){
	                                          // all columns are the same except source id (not testing spw id),
	                                          // spw id must be different, otherwise row would have been deleted above
	      //cout << "Found SOURCE rows " << j << " and " << k << " to be identical except for the SPW ID and source id. "
	      //	 << newThisId(k) << " mapped to " << newThisId(j) << endl;
	      // give same source id
	      // make entry in map for (k, j) and rename k
	      tempSourceIndex3[newThisId(k)] = newThisId(j);
	      //sourceRecord = sourceRow.get(k);
	      //sourceRecord.define(sourceIdId, newThisId(j) );
	      //sourceRow.putMatchingFields(k, sourceRecord);
	      thisSourceId(k) = newThisId(j);
	      rowsRenamed = True;
	      nDistinctSources--;
	    }
	  }
	}
      } // end for j
      if(rowsRenamed){
	sourceCol.sourceId().putColumn(thisSourceId);
      }

//      cout << "Ndistinct = " << nDistinctSources << endl;

      if(rowsRenamed){ 	// reduce ID values to minimal range
 	sourceCol.sourceId().getColumn(newThisId, True); // update vector if IDs
	Int counter = 0;
	for (Int j=0 ; j < newNumrows_this ; ++j){
	  if(newThisId(j) >= nDistinctSources){
	    sourceRecord = sourceRow.get(j);
	    tempSourceIndex3[newThisId(j)] = nDistinctSources-counter-1;
	    sourceRecord.define(sourceIdId, nDistinctSources-counter-1);
	    sourceRow.putMatchingFields(j, sourceRecord);
	    counter++;
// 	    cout << "Found SOURCE row " << j << " to have a source id " << newThisId(j)
//               << " larger than the number of distinct sources: " << nDistinctSources << ". "
// 		 << newThisId(j) << " mapped to " << nDistinctSources-counter-1 << endl;
	  }
	}
      }

      if(rowsToBeRemoved.size()>0 || rowsRenamed){
	// create map for copyField
	for (Int j=0 ; j < numrows_this ; ++j){ // loop over old indices
	  if(tempSourceIndex.find(j) != tempSourceIndex.end()){ // ID changed because of redundancy
	    if(tempSourceIndex2.find(tempSourceIndex.at(j)) != tempSourceIndex2.end()){ // ID changed also because of renumbering
	      if( tempSourceIndex3.find(tempSourceIndex2.at(tempSourceIndex.at(j))) != tempSourceIndex3.end() ){ // ID also changed because of renaming
		newSourceIndex2_p[j] = tempSourceIndex3.at(tempSourceIndex2.at(tempSourceIndex.at(j))); // abc
	      }
	      else { // ID changed because of redundancy and renumberning
                newSourceIndex2_p[j] = tempSourceIndex2.at(tempSourceIndex.at(j)); // ab
	      }
	    }
	    else{
	      if( tempSourceIndex3.find(tempSourceIndex.at(j)) != tempSourceIndex3.end() ){ // ID  changed because of redundancy and renaming
		newSourceIndex2_p[j] = tempSourceIndex3.at(tempSourceIndex.at(j)); // ac
	      }
	      else { // ID only changed because of redundancy
		newSourceIndex2_p[j] = tempSourceIndex.at(j); // a
	      }
	    }
	  }
	  else if(tempSourceIndex2.find(j) != tempSourceIndex2.end()){
	    if( tempSourceIndex3.find(tempSourceIndex2.at(j)) != tempSourceIndex3.end() ){ // ID  changed because of renumbering and renaming
	      newSourceIndex2_p[j] = tempSourceIndex3.at(tempSourceIndex2.at(j)); // bc
	    }
	    else { // ID only changed because of renumbering
	      newSourceIndex2_p[j] = tempSourceIndex2.at(j); // b
	    }
	  }
	  else if(tempSourceIndex3.find(j) != tempSourceIndex3.end()){ // ID only changed because of renaming
            newSourceIndex2_p[j] = tempSourceIndex3.at(j); // c
	    }
	}
	doSource2_p=True;
      }

    } // end if(numrows_this > 0)
  }
  return doSource2_p;
}

Bool MSConcat::updateSource2(){ // to be called after copyField

  // Go over the SOURCE table in the light of FIELD table merging
  // and correct SOURCE IDs if necessary.

  Bool rval = False; // were changes made?

  if(Table::isReadable(itsMS.sourceTableName())){

    MSSource& newSource=itsMS.source();
    MSSourceColumns& sourceCol=source();
    MSFieldColumns& fieldCol=field();
    vector<rownr_t> rowsToBeRemoved;

    // the number of rows in the source table
    Int numrows_this=newSource.nrow();

    if(numrows_this > 0){  // the source table is not empty

      Vector<Int> thisId=sourceCol.sourceId().getColumn();
      Vector<String> thisName=sourceCol.name().getColumn();
      Vector<Int> thisSPWId=sourceCol.spectralWindowId().getColumn();
      Vector<Int> thisFSId=fieldCol.sourceId().getColumn();

      // loop over the rows of the merged source table
      for (Int j=0 ; j < numrows_this ; ++j){
	if(std::find(thisFSId.begin(), thisFSId.end(), thisId[j]) == thisFSId.end()){ // source id doesn't exist in field table
	  //cout << "source id " <<   thisId[j] << " doesn't exist in field table" << endl;
	  // find equivalent source with different SPW and take that ID
	  Int foundRow = -1;
	  for(Int k=0; k < numrows_this ; ++k){
	    if(thisSPWId[k]!=thisSPWId[j] && thisId[k]!=thisId[j]
	       && sourceRowsEquivalent(sourceCol, k, j, False, True) ){ // do check direction but not transition and rest (they are potentially different for each SPW)
	      foundRow = k;
	      break;
	    }
	  }
	  if(foundRow>=0){
	    sourceCol.sourceId().put(j, thisId[foundRow]);
	    thisId[j] = thisId[foundRow];
	  }
	  else{ // no adequate source id found
	    //cout << "Selecting row " << j << " for removal from SOURCE table." << endl;
	    rowsToBeRemoved.push_back(j);
	  }
	  rval = True;
	}
      }

      if(rowsToBeRemoved.size()>0){ // actually remove the rows
	Vector<rownr_t> rowsTBR(rowsToBeRemoved);
	newSource.removeRow(rowsTBR);
	//cout << "Removed " << rowsToBeRemoved.size() << " stray rows from SOURCE table." << endl;
      }

    }
  }
  return rval;
}


Bool MSConcat::sourceRowsEquivalent(const MSSourceColumns& sourceCol, const rownr_t& rowi, const rownr_t& rowj,
				    const Bool dontTestDirection, const Bool dontTestTransAndRest){
  // check if the two SOURCE table rows are identical IGNORING SOURCE_ID, SPW_ID, time, and interval

  Bool areEquivalent(False);

  // test the non-optional columns first
  if(areEQ(sourceCol.calibrationGroup(), rowi, rowj) &&
     areEQ(sourceCol.code(), rowi, rowj) &&
     areEQ(sourceCol.name(), rowi, rowj) &&
     areEQ(sourceCol.numLines(), rowi, rowj) &&
     // do NOT test SPW ID!
     // areEQ(sourceCol.spectralWindowId(), rowi, rowj) &&
     (areEQ(sourceCol.direction(), rowi, rowj) || dontTestDirection) &&
     areEQ(sourceCol.properMotion(), rowi, rowj)
     ){

    //    cout << "All non-optionals equal" << endl;

    // test the optional columns next
    areEquivalent = True;
    if(!(sourceCol.position().isNull()) && !dontTestDirection){
      try {
	areEquivalent = areEQ(sourceCol.position(), rowi, rowj);
      }
      catch (AipsError& x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal position" << endl;
    }
    if(!(sourceCol.pulsarId().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.pulsarId(), rowi, rowj);
      }
      catch (AipsError& x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal pulsarId" << endl;
    }
    if(!dontTestTransAndRest && !(sourceCol.restFrequency().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.restFrequency(), rowi, rowj);
      }
      catch (AipsError& x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal restFrequency" << endl;
    }
    if(!(sourceCol.sysvel().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.sysvel(), rowi, rowj);
      }
      catch (AipsError& x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal sysvel" << endl;
    }
    if(!dontTestTransAndRest && !(sourceCol.transition().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.transition(), rowi, rowj);
      }
      catch (AipsError& x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal transition" << endl;
    }
  }
  return areEquivalent;
}

Bool MSConcat::obsRowsEquivalent(const MSObservationColumns& obsCol, const rownr_t& rowi, const rownr_t& rowj){
  // check if the two OBSERVATION table rows are identical ignoring LOG and SCHEDULE

  Bool areEquivalent(False);

  if(areEQ(obsCol.flagRow(), rowi, rowj) &&
     areEQ(obsCol.observer(), rowi, rowj) &&
     areEQ(obsCol.project(), rowi, rowj) &&
     areEQ(obsCol.releaseDate(), rowi, rowj) &&
     areEQ(obsCol.telescopeName(), rowi, rowj) &&
     areEQ(obsCol.timeRange(), rowi, rowj)
     ){
    areEquivalent = True;
  }
  return areEquivalent;
}

Bool MSConcat::procRowsEquivalent(const MSProcessorColumns& procCol, const uInt& rowi, const uInt& rowj){
  // check if the two PROCESSOR table rows are identical

  Bool areEquivalent(False);

  if(areEQ(procCol.flagRow(), rowi, rowj) &&
     areEQ(procCol.modeId(), rowi, rowj) &&
     areEQ(procCol.type(), rowi, rowj) &&
     areEQ(procCol.typeId(), rowi, rowj) &&
     areEQ(procCol.subType(), rowi, rowj)
     ){
    areEquivalent = True;

    // passId is optional
    if(!procCol.passId().isNull() && !areEQ(procCol.passId(), rowi, rowj)){
      areEquivalent = False;
    }
  }
  return areEquivalent;
}


Block<uInt> MSConcat::copySpwAndPol(const MSSpectralWindow& otherSpw,
				    const MSPolarization& otherPol,
				    const MSDataDescription& otherDD) {

  LogIO os(LogOrigin("MSConcat", "copySpwAndPol"));

  const uInt nDDs = otherDD.nrow();
  Block<uInt> ddMap(nDDs);

  const MSSpWindowColumns otherSpwCols(otherSpw);
  MSSpectralWindow& spw = itsMS.spectralWindow();
  MSSpWindowColumns& spwCols = spectralWindow();
  const ROTableRow otherSpwRow(otherSpw);
  TableRow spwRow(spw);
  const MSPolarizationColumns otherPolCols(otherPol);
  MSPolarization& pol = itsMS.polarization();
  MSPolarizationColumns& polCols = polarization();
  const ROTableRow otherPolRow(otherPol);
  TableRow polRow(pol);

  const MSDataDescColumns otherDDCols(otherDD);
  MSDataDescColumns& ddCols = dataDescription();

  const Quantum<Double> freqTol=itsFreqTol;
  const String& spwIdxName = MSDataDescription::columnName(MSDataDescription::SPECTRAL_WINDOW_ID);
  const String& polIdxName = MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID);
  Vector<String> ddIndexCols(2);
  ddIndexCols(0) = spwIdxName;
  ddIndexCols(1) = polIdxName;
  ColumnsIndex ddIndex(itsMS.dataDescription(), ddIndexCols);
  RecordFieldPtr<Int> newSpwPtr(ddIndex.accessKey(), spwIdxName);
  RecordFieldPtr<Int> newPolPtr(ddIndex.accessKey(), polIdxName);
  Vector<Int> corrInt;
  Vector<Stokes::StokesTypes> corrPol;
  itsChanReversed.resize(nDDs);
  itsChanReversed.set(False);
  newSPWIndex_p.clear();
  doSPW_p = False;

  Vector<Bool> foundInDD(otherSpw.nrow(), False);

  // loop over the rows of the other data description table
  for (uInt d = 0; d < nDDs; d++) {
    //cout << "other DD " << d << endl;
    Bool matchedSPW = False;
    DebugAssert(otherDDCols.spectralWindowId()(d) >= 0 &&
		otherDDCols.spectralWindowId()(d) < static_cast<Int>(otherSpw.nrow()),
		AipsError);
    const Int otherSpwId = otherDDCols.spectralWindowId()(d);
    DebugAssert(otherSpwCols.numChan()(otherSpwId) > 0, AipsError);

    foundInDD(otherSpwId) = True;

    Vector<Double> otherFreqs = otherSpwCols.chanFreq()(otherSpwId);

    if(otherSpwCols.totalBandwidthQuant()(otherSpwId).getValue(Unit("Hz"))<=0.){
      os << LogIO::WARN << "Negative or zero total bandwidth in SPW "
	 << otherSpwId << " of MS to be appended." << LogIO::POST;
    }

    *newSpwPtr = spwCols.matchSpw(otherSpwCols.refFrequencyMeas()(otherSpwId),
				  static_cast<uInt>(otherSpwCols.numChan()(otherSpwId)),
				  otherSpwCols.totalBandwidthQuant()(otherSpwId),
				  otherSpwCols.ifConvChain()(otherSpwId), freqTol,
				  otherFreqs, itsChanReversed[d]);

    if (*newSpwPtr < 0) {
      // cout << "no counterpart found for other spw " << otherSpwId << endl;
      // need to add a new entry in the SPECTRAL_WINDOW subtable
      *newSpwPtr= spw.nrow();
      spw.addRow();
      spwRow.putMatchingFields(*newSpwPtr, otherSpwRow.get(otherSpwId));
      // fill map to be used by updateSource()
      newSPWIndex_p[otherSpwId] = *newSpwPtr;
      // There cannot be an entry in the DATA_DESCRIPTION Table
      doSPW_p = True;
    }
    else{
      // cout << "counterpart found for other spw " << otherSpwId
      //     << " found in this spw " << *newSpwPtr << endl;
      matchedSPW = True;
      if(*newSpwPtr != otherSpwId){
	newSPWIndex_p[otherSpwId] = *newSpwPtr;
      }
    }

    DebugAssert(otherDDCols.polarizationId()(d) >= 0 &&
		otherDDCols.polarizationId()(d) <
		static_cast<Int>(otherPol.nrow()), AipsError);
    const uInt otherPolId = static_cast<uInt>(otherDDCols.polarizationId()(d));

    otherPolCols.corrType().get(otherPolId, corrInt, True);
    const uInt nCorr = corrInt.nelements();
    corrPol.resize(nCorr);
    for (uInt p = 0; p < nCorr; p++) {
      corrPol(p) = Stokes::type(corrInt(p));
    }
    Bool matchedDD=False;
    uInt numActPol =0;
    while ( numActPol < polCols.nrow() ){
      *newPolPtr = polCols.match(corrPol, numActPol);
      if (*newPolPtr < 0) {
	// cout << "need to add a new entry in the POLARIZATION subtable" << endl;
	*newPolPtr= pol.nrow();
	pol.addRow();
	polRow.putMatchingFields(*newPolPtr, otherPolRow.get(otherPolId));
	break; // break out of the while loop
      }
      else{ // we have a Pol match
	if(matchedSPW){
	  // We need to check if there exists an entry in the DATA_DESCRIPTION
	  // table with the required spectral window and polarization index.
	  ddMap[d] = ddIndex.getRowNumber(matchedDD); // sets matchedDD to True if a matching DD table entry is found
	}
	//cout << "Found matching pol. Fould matching DD? " << matchedDD << " d ddMap[d] " << d << " " << ddMap[d] << endl;
      }
      ++numActPol;
    }


    if (!matchedDD) {
      // Add an entry to the data description sub-table
      ddMap[d] = ddCols.nrow();
      itsMS.dataDescription().addRow(1);
      ddCols.polarizationId().put(ddMap[d], *newPolPtr);
      ddCols.spectralWindowId().put(ddMap[d], *newSpwPtr);
    }
  }

  // Finally, see if there are additional SPWs in the SPW table which are
  //  not used in the DD table
  for(uInt otherSpwId=0; otherSpwId<otherSpw.nrow(); otherSpwId++){

    if(!foundInDD(otherSpwId)){ // not already processed above

      DebugAssert(otherSpwCols.numChan()(otherSpwId) > 0, AipsError);
      Vector<Double> otherFreqs = otherSpwCols.chanFreq()(otherSpwId);

      if(otherSpwCols.totalBandwidthQuant()(otherSpwId).getValue(Unit("Hz"))<=0.){
	os << LogIO::WARN << "Negative or zero total bandwidth in SPW "
	   << otherSpwId << " of MS to be appended." << LogIO::POST;
      }
      Bool chanReversed = False;

      Int newSpwId = spwCols.matchSpw(otherSpwCols.refFrequencyMeas()(otherSpwId),
				      static_cast<uInt>(otherSpwCols.numChan()(otherSpwId)),
				      otherSpwCols.totalBandwidthQuant()(otherSpwId),
				      otherSpwCols.ifConvChain()(otherSpwId), freqTol,
				      otherFreqs, chanReversed);

      if (newSpwId < 0) {
	// cout << "Second iteration: no counterpart found for other spw " << otherSpwId << endl;
	// need to add a new entry in the SPECTRAL_WINDOW subtable
	newSpwId = spw.nrow();
	spw.addRow();
	spwRow.putMatchingFields(newSpwId, otherSpwRow.get(otherSpwId));
	// fill map to be used by updateSource()
	newSPWIndex_p[otherSpwId] = newSpwId;
	doSPW_p = True;
      }
      // else{
      // cout << "Second iteration: counterpart found for other spw " << otherSpwId
      //     << " found in this spw " << newSpwId << endl;
      //
      // }

    } // endif

  }

  return ddMap;
}

void MSConcat::updateModelDataKeywords(MeasurementSet& theMS){
  Int nSpw=theMS.spectralWindow().nrow();
  MSSpWindowColumns msSpW(theMS.spectralWindow());
  Matrix<Int> selection(2,nSpw);
  // fill in default selection
  selection.row(0)=0; //start
  selection.row(1)=msSpW.numChan().getColumn();
  TableColumn col(theMS,"MODEL_DATA");
  if (col.keywordSet().isDefined("CHANNEL_SELECTION"))
    col.rwKeywordSet().removeField("CHANNEL_SELECTION");
  col.rwKeywordSet().define("CHANNEL_SELECTION",selection);
}
// Local Variables:
// compile-command: "gmake MSConcat"
// End:

} //#End casa namespace
