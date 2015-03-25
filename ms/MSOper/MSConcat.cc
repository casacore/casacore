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

namespace casacore {

MSConcat::MSConcat(MeasurementSet& ms):
  MSColumns(ms),
  itsMS(ms),
  itsFixedShape(isFixedShape(ms.tableDesc())), 
  newSourceIndex_p(-1), newSourceIndex2_p(-1), newSPWIndex_p(-1),
  newObsIndexA_p(-1), newObsIndexB_p(-1), otherObsIdsWithCounterpart_p(-1),
  solSystObjects_p(-1)
{
  itsDirTol=Quantum<Double>(1.0, "mas");
  itsFreqTol=Quantum<Double>(1.0, "Hz");
  itsWeightScale = 1.;
  itsRespectForFieldName = False;
  doSource_p=False;
  doObsA_p = doObsB_p = False;
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

  Bool MSConcat::checkEphIdInField(const ROMSFieldColumns& otherFldCol) const {
  // test if this MS FIELD table has an ephID column
  if(!itsMS.field().actualTableDesc().isColumn(MSField::columnName(MSField::EPHEMERIS_ID))){
    // if not, test if the other MS uses ephem objects
    Bool usesEphems = False;
    for(uInt i=0; i<otherFldCol.nrow(); i++){
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
    const ROMSFieldColumns otherMSFCols(otherMS.field());
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
	const ROMSPolarizationColumns otherPolCols(otherMS.polarization());
	const ROMSSpWindowColumns otherSpwCols(otherMS.spectralWindow());
	const ROMSDataDescColumns otherDDCols(otherMS.dataDescription());
	const uInt nShapes = otherDDCols.nrow();
	for (uInt s = 0; s < nShapes; s++) {
	  checkShape(getShape(otherDDCols, otherSpwCols, otherPolCols, s));
	}
      }
      const ROMSMainColumns otherMainCols(otherMS);
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

    Vector<uInt> delrows(itsMS.state().nrow());
    indgen(delrows);
    itsMS.state().removeRow(delrows); 
  }
  else{ // both state tables are filled
    const uInt oldStateRows = itsMS.state().nrow();
    newStateIndices = copyState(otherMS.state());
    const uInt addedRows = itsMS.state().nrow() - oldStateRows;
    const uInt matchedRows = otherMS.state().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the state subtable" << LogIO::POST;
    doState = True; // state id entries in the main table will have to be modified for otherMS
  }

  //See if there is a SOURCE table and concatenate and reindex it
  {
    uInt oldSRows = itsMS.source().nrow();
    copySource(otherMS); 
    if(Table::isReadable(itsMS.sourceTableName())){
      uInt addedRows =  itsMS.source().nrow() - oldSRows;
      if(addedRows>0){
	log << "Added " << addedRows 
	    << " rows to the source subtable" << LogIO::POST;
      }
    }
  }

  // DATA_DESCRIPTION
  uInt oldRows = itsMS.dataDescription().nrow();
  uInt oldSPWRows = itsMS.spectralWindow().nrow();
  const Block<uInt> newDDIndices = copySpwAndPol(otherMS.spectralWindow(),
						 otherMS.polarization(),
						 otherMS.dataDescription());
  {
    uInt addedRows = itsMS.dataDescription().nrow() - oldRows;
    uInt matchedRows = otherMS.dataDescription().nrow() - addedRows;
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
    uInt removedRows =  oldRows - itsMS.source().nrow();
    if(removedRows>0){
      log << "Removed " << removedRows 
	  << " redundant rows from the source subtable" << LogIO::POST;
    }
  }

  // merge ANTENNA and FEED
  oldRows = itsMS.antenna().nrow();
  uInt oldFeedRows = itsMS.feed().nrow();
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
    uInt addedRows = itsMS.antenna().nrow() - oldRows;
    uInt matchedRows = otherMS.antenna().nrow() - addedRows;
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
    const uInt addedRows = itsMS.field().nrow() - oldRows;
    const uInt matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the field subtable" << LogIO::POST;
  }

  // OBSERVATION
  copyObservation(otherMS.observation(), True);

  // POINTING
  if(!antIndexTrivial){
    copyPointingB(otherMS.pointing(), newAntIndices);
  }
  
  /////////////////////////////////////////////////////

  // copying all subtables over to otherMS
  // will need to be done when creating the MMS from the concatenated MSs

  //////////////////////////////////////////////////////

  MSMainColumns mainCols(itsMS);
  MSMainColumns otherMainCols(otherMS);

  const uInt otherRows = otherMS.nrow();
  const uInt theseRows = itsMS.nrow();

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

  ScalarColumn<Int>& otherAnt1Col = otherMainCols.antenna1();
  ScalarColumn<Int>& otherAnt2Col = otherMainCols.antenna2();
  ScalarColumn<Int>& otherDDIdCol = otherMainCols.dataDescId();
  ScalarColumn<Int>& otherFieldIdCol = otherMainCols.fieldId();
  ScalarColumn<Int>& otherScanCol = otherMainCols.scanNumber();
  ScalarColumn<Int>& otherStateIdCol = otherMainCols.stateId();
  ScalarColumn<Int>& otherObsIdCol =otherMainCols.observationId();

  ScalarColumn<Int>& thisScanCol = mainCols.scanNumber();
  ScalarColumn<Int>& thisStateIdCol = mainCols.stateId();
  ScalarColumn<Int>& thisObsIdCol = mainCols.observationId();

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

  if (doState && !otherStateNull){
    otherStateId = otherStateIdCol.getColumn();
  }

  Int defaultScanOffset=0;
  SimpleOrderedMap <Int, Int> scanOffsetForOid(-1);
  SimpleOrderedMap <Int, Int> encountered(-1);
  vector<Int> distinctObsIdSet;
  vector<Int> minScan;
  vector<Int> maxScan;

  if(reindexObsidAndScan){

    otherObsIds = otherObsIdCol.getColumn();
    Vector<Int> theseObsIds=thisObsIdCol.getColumn();
    Vector<Int> theseScans=thisScanCol.getColumn();
    
    if(doObsA_p){ // the obs ids changed for the first table
      for(uInt r = 0; r < theseRows; r++) {
	if(newObsIndexA_p.isDefined(theseObsIds[r])){ // apply change 
	  theseObsIds[r] = newObsIndexA_p(theseObsIds[r]);
	}
      }
      thisObsIdCol.putColumn(theseObsIds);
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
      for(uInt r = 0; r < theseRows; r++) {
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
      if(otherObsIdsWithCounterpart_p.isDefined(distinctObsIdSet[i]) && i!=0){
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
	encountered.define(distinctObsIdSet[i],0); // used later to decide whether to notify user
      }
      scanOffsetForOid.define(distinctObsIdSet[i], scanOffset); 
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
  
  // MAIN
  
  Bool doWeightScale = (itsWeightScale!=1.);
  
  if(reindexObsidAndScan){
    for (uInt r = 0; r < otherRows; r++) {
      Int oid = 0;
      if(doObsB_p && newObsIndexB_p.isDefined(otherObsIds[r])){ 
	// the obs ids have been changed for the table to be appended
	oid = newObsIndexB_p(otherObsIds[r]);
      }
      else{ // this OBS id didn't change
	oid = otherObsIds[r];
      }
      
      if(oid != otherObsIds[r]){ // obsid actually changed
	if(!scanOffsetForOid.isDefined(oid)){ // offset not set, use default
	  scanOffsetForOid.define(oid, defaultScanOffset);
	}
	if(!encountered.isDefined(oid) && scanOffsetForOid(oid)!=0){
	  log << LogIO::NORMAL << "Will offset scan numbers by " <<  scanOffsetForOid(oid)
	      << " for observations with Obs ID " << oid
	      << " in order to make scan numbers unique." << LogIO::POST;
	  encountered.define(oid,0);
	}
	otherScan[r] = otherScan[r] + scanOffsetForOid(oid);
      }

      otherObsIds[r] = oid;

    }

    // update or create the file with the initial values for the next concat

    Int maxScanOther = 0;
    for(uInt r = 0; r < otherRows; r++) {
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
    for (uInt r = 0; r < otherRows; r++) {
      if(!(itsStateNull || otherStateNull)){
	otherStateId[r] = newStateIndices[otherStateId[r]];
      }
    }  
  }

  for (uInt r = 0; r < otherRows; r++) {
    
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
	  otherData.put(r, conj(reversedData));	  
	}
	else{
	  otherData.put(r, reversedData);
	}
      }
      if(doCorrectedData){
	if(doConjugateVis){
	  otherCorrectedData.put(r, conj(reversedCorrData));
	}
	else{
	  otherCorrectedData.put(r, reversedCorrData);
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  otherModelData.put(r, conj(reversedModData));
	}
	else{
	  otherModelData.put(r, reversedModData);
	}
      }
    }
    else{ // no reversal
      if(!doFloatData){
	if(doConjugateVis){ // conjugate because order of antennas was reversed
	  otherData.put(r, conj(otherData(r)));
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  otherModelData.put(r, conj(otherModelData(r)));
	}
      } 
      if(doCorrectedData){
	if(doConjugateVis){
	  otherCorrectedData.put(r, conj(otherCorrectedData(r)));
	}
      }
    } // end if itsChanReversed

    otherDDId[r] = newDDIndices[otherDDId[r]];
    otherFieldId[r] = newFldIndices[otherFieldId[r]];
    
    if(doWeightScale){
      otherWeight.put(r, otherWeight(r)*itsWeightScale);
      if (copyWtSp) otherWeightSp.put(r, otherWeightSp(r)*itsWeightScale);
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
    const ROMSFieldColumns otherMSFCols(otherMS.field());
    if(!checkEphIdInField(otherMSFCols)){
      log << "EPHEMERIS_ID column missing in FIELD table of MS " << itsMS.tableName()
	  << LogIO::EXCEPTION;
    }
  }

  // verify that shape of the two MSs as described in POLARISATION, SPW, and DATA_DESCR
  //   is the same
  const ROMSMainColumns otherMainCols(otherMS);
  if (otherMS.nrow() > 0) {
    if (itsFixedShape.nelements() > 0) {
      const ROMSPolarizationColumns otherPolCols(otherMS.polarization());
      const ROMSSpWindowColumns otherSpwCols(otherMS.spectralWindow());
      const ROMSDataDescColumns otherDDCols(otherMS.dataDescription());
      const uInt nShapes = otherDDCols.nrow();
      for (uInt s = 0; s < nShapes; s++) {
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

    Vector<uInt> delrows(itsMS.state().nrow());
    indgen(delrows);
    itsMS.state().removeRow(delrows); 
  }
  else{ // both state tables are filled
    const uInt oldStateRows = itsMS.state().nrow();
    newStateIndices = copyState(otherMS.state());
    const uInt addedRows = itsMS.state().nrow() - oldStateRows;
    const uInt matchedRows = otherMS.state().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the state subtable" << LogIO::POST;
    doState = True; // state id entries in the main table will have to be modified for otherMS
  }

  //See if there is a SOURCE table and concatenate and reindex it
  {
    uInt oldSRows = itsMS.source().nrow();
    copySource(otherMS); 
    if(Table::isReadable(itsMS.sourceTableName())){
      uInt addedRows =  itsMS.source().nrow() - oldSRows;
      if(addedRows>0){
	log << "Added " << addedRows 
	    << " rows to the source subtable" << LogIO::POST;
      }
    }
  }


  // DATA_DESCRIPTION
  uInt oldRows = itsMS.dataDescription().nrow();
  uInt oldSPWRows = itsMS.spectralWindow().nrow();
  const Block<uInt> newDDIndices = copySpwAndPol(otherMS.spectralWindow(),
						 otherMS.polarization(),
						 otherMS.dataDescription());

  {
    uInt addedRows = itsMS.dataDescription().nrow() - oldRows;
    uInt matchedRows = otherMS.dataDescription().nrow() - addedRows;
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
    uInt removedRows =  oldRows - itsMS.source().nrow();
    if(removedRows>0){
      log << "Removed " << removedRows 
	  << " redundant rows from the source subtable" << LogIO::POST;
    }
  }


  // merge ANTENNA and FEED
  oldRows = itsMS.antenna().nrow();
  uInt oldFeedRows = itsMS.feed().nrow();
  const Block<uInt> newAntIndices = copyAntennaAndFeed(otherMS.antenna(), 
						       otherMS.feed());
  {
    uInt addedRows = itsMS.antenna().nrow() - oldRows;
    uInt matchedRows = otherMS.antenna().nrow() - addedRows;
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
    const uInt addedRows = itsMS.field().nrow() - oldRows;
    const uInt matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the field subtable" << LogIO::POST;
  }


  // OBSERVATION
  copyObservation(otherMS.observation(), True);

  // POINTING
  if(handling<2){
    if(!copyPointing(otherMS.pointing(), newAntIndices)){
      log << LogIO::WARN << "Could not merge Pointing subtables " << LogIO::POST ;
    }
  }
  else{ // delete the POINTING table
    log << LogIO::NORMAL << "Deleting all rows in the Pointing subtable ..." << LogIO::POST ;
    Vector<uInt> delrows(itsMS.pointing().nrow());
    indgen(delrows);
    itsMS.pointing().removeRow(delrows); 
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
  const uInt newRows = otherMS.nrow();
  uInt curRow = destMS->nrow();
  
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
  const ROScalarColumn<Double>& otherTime = otherMainCols.time();
  ScalarColumn<Double>& thisTime = destMainCols.time();
  const ROScalarColumn<Double>& otherInterval = otherMainCols.interval();
  ScalarColumn<Double>& thisInterval = destMainCols.interval();
  const ROScalarColumn<Double>& otherExposure = otherMainCols.exposure();
  ScalarColumn<Double>& thisExposure = destMainCols.exposure();
  const ROScalarColumn<Double>& otherTimeCen = otherMainCols.timeCentroid();
  ScalarColumn<Double>& thisTimeCen = destMainCols.timeCentroid();
  const ROScalarColumn<Int>& otherArrayId = otherMainCols.arrayId();
  ScalarColumn<Int>& thisArrayId = destMainCols.arrayId();
  const ROArrayColumn<Float>& otherSigma = otherMainCols.sigma();
  ArrayColumn<Float>& thisSigma = destMainCols.sigma();
  const ROArrayColumn<Bool>& otherFlag = otherMainCols.flag();
  ArrayColumn<Bool>& thisFlag = destMainCols.flag();
  const ROArrayColumn<Bool>& otherFlagCat = otherMainCols.flagCategory();
  ArrayColumn<Bool>& thisFlagCat = destMainCols.flagCategory();
  Bool copyFlagCat = !(thisFlagCat.isNull() || otherFlagCat.isNull());
  copyFlagCat = copyFlagCat && thisFlagCat.isDefined(0) 
    && otherFlagCat.isDefined(0);
  const ROScalarColumn<Bool>& otherFlagRow = otherMainCols.flagRow();
  ScalarColumn<Bool>& thisFlagRow = destMainCols.flagRow();
  const ROScalarColumn<Int>& otherFeed1 = otherMainCols.feed1();
  ScalarColumn<Int>& thisFeed1 = destMainCols.feed1();
  const ROScalarColumn<Int>& otherFeed2 = otherMainCols.feed2();
  ScalarColumn<Int>& thisFeed2 = destMainCols.feed2();
  
  // create column objects for those columns which potentially need to be modified
  
  ROArrayColumn<Complex> otherData;
  ArrayColumn<Complex> thisData;
  ROArrayColumn<Float> otherFloatData;
  ArrayColumn<Float> thisFloatData;
  ROArrayColumn<Complex> otherModelData, otherCorrectedData;
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
  
  const ROScalarColumn<Int>& otherAnt1 = otherMainCols.antenna1();
  ScalarColumn<Int> thisAnt1 = destMainCols.antenna1();
  const ROScalarColumn<Int>& otherAnt2 = otherMainCols.antenna2();
  ScalarColumn<Int> thisAnt2 = destMainCols.antenna2();
  const ROScalarColumn<Int>& otherDDId = otherMainCols.dataDescId();
  ScalarColumn<Int> thisDDId = destMainCols.dataDescId();
  const ROScalarColumn<Int>& otherFieldId = otherMainCols.fieldId();
  ScalarColumn<Int> thisFieldId = destMainCols.fieldId();
  const ROArrayColumn<Double>& otherUvw = otherMainCols.uvw();
  ArrayColumn<Double> thisUvw = destMainCols.uvw();
  const ROArrayColumn<Float>& otherWeight = otherMainCols.weight();
  ArrayColumn<Float> thisWeight = destMainCols.weight();
  const ROArrayColumn<Float>& otherWeightSp = otherMainCols.weightSpectrum();
  ArrayColumn<Float> thisWeightSp = destMainCols.weightSpectrum();

  const ROScalarColumn<Int>& otherScan = otherMainCols.scanNumber();
  const ROScalarColumn<Int>& otherStateId = otherMainCols.stateId();
  const ROScalarColumn<Int>& otherObsId=otherMainCols.observationId();

  ScalarColumn<Int> thisScan;
  ScalarColumn<Int> thisStateId;
  ScalarColumn<Int> thisObsId;
  
  thisScan.reference(scanNumber());
  thisStateId.reference(stateId());
  thisObsId.reference(observationId());
  
  Vector<Int> obsIds=otherObsId.getColumn();
  
  if(doObsA_p){ // the obs ids changed for the first table
    Vector<Int> oldObsIds=thisObsId.getColumn();
    for(uInt r = 0; r < curRow; r++) {
      if(newObsIndexA_p.isDefined(oldObsIds[r])){ // apply change 
	thisObsId.put(r, newObsIndexA_p(oldObsIds[r]));
      }
    }
  }  
  
  if(doState && otherStateNull){ // the state ids for the first table will have to be set to -1
    for(uInt r = 0; r < curRow; r++) {
      thisStateId.put(r, -1);
    }
  }  
  
  // SCAN NUMBER
  // find the distinct ObsIds in use in this MS
  // and the maximum scan and minimum scan ID in each of them
  SimpleOrderedMap <Int, Int> scanOffsetForOid(-1);
  SimpleOrderedMap <Int, Int> encountered(-1);
  vector<Int> distinctObsIdSet;
  vector<Int> minScan;
  vector<Int> maxScan;
  Int maxScanThis=0;
  for(uInt r = 0; r < curRow; r++) {
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
    ROTableVector<Int> ScanTabVectOther(otherScan);
    minScanOther = min(ScanTabVectOther);
    defaultScanOffset = maxScanThis + 1 - minScanOther;
    if(defaultScanOffset<0){
      defaultScanOffset=0;
    }
  }

  for(uInt i=0; i<distinctObsIdSet.size(); i++){
    Int scanOffset;
    if(otherObsIdsWithCounterpart_p.isDefined(distinctObsIdSet[i]) && i!=0){
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
      encountered.define(distinctObsIdSet[i],0); // used later to decide whether to notify user
    }
    scanOffsetForOid.define(distinctObsIdSet[i], scanOffset); 
  }
  
  
  // finished all modifications of the MS Main table going to the first part 
  
  // now start modifications of the second (appended) part 
  
  thisScan.reference(destMainCols.scanNumber());
  thisStateId.reference(destMainCols.stateId());
  thisObsId.reference(destMainCols.observationId());
  
  Bool copyWtSp = !(thisWeightSp.isNull() || otherWeightSp.isNull()); 
  copyWtSp = copyWtSp && thisWeightSp.isDefined(0) 
    && otherWeightSp.isDefined(0);
  
  // MAIN
  
  Bool doWeightScale = (itsWeightScale!=1. && itsWeightScale>0.);
  Float sScale = 1.; // scale for SIGMA
  if (doWeightScale){
    sScale = 1/sqrt(itsWeightScale);
  }

  for (uInt r = 0; r < newRows; r++, curRow++) {
    
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
    if(doObsB_p && newObsIndexB_p.isDefined(obsIds[r])){ 
      // the obs ids have been changed for the table to be appended
      oid = newObsIndexB_p(obsIds[r]); 
    }
    else { // this OBS id didn't change 
      oid = obsIds[r];
    }
    thisObsId.put(curRow, oid);
    
    if(oid != obsIds[r]){ // obsid actually changed
      if(!scanOffsetForOid.isDefined(oid)){ // offset not set, use default
	scanOffsetForOid.define(oid, defaultScanOffset);
      }
      if(!encountered.isDefined(oid) && scanOffsetForOid(oid)!=0){
	log << LogIO::NORMAL << "Will offset scan numbers by " <<  scanOffsetForOid(oid)
	    << " for observations with Obs ID " << oid
	    << " in order to make scan numbers unique." << LogIO::POST;
	encountered.define(oid,0);
      }
      thisScan.put(curRow, otherScan(r) + scanOffsetForOid(oid));
    }
    else{
      thisScan.put(curRow, otherScan(r));
    }
    
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
	  thisData.put(curRow, conj(reversedData));	  
	}
	else{
	  thisData.put(curRow, reversedData);
	}
      }
      if(doCorrectedData){
	if(doConjugateVis){
	  thisCorrectedData.put(curRow, conj(reversedCorrData));
	}
	else{
	  thisCorrectedData.put(curRow, reversedCorrData);
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  thisModelData.put(curRow, conj(reversedModData));
	}
	else{
	  thisModelData.put(curRow, reversedModData);
	}
      }
    }
    else{ // no reversal
      if(doFloatData){
	thisFloatData.put(curRow, otherFloatData, r);
      }
      else{
	if(doConjugateVis){ // conjugate because order of antennas was reversed
	  thisData.put(curRow, conj(otherData(r)));
	}
	else{
	  thisData.put(curRow, otherData, r);
	}
      }
      if(doModelData){
	if(doConjugateVis){
	  thisModelData.put(curRow, conj(otherModelData(r)));
	}
	else{
	  thisModelData.put(curRow, otherModelData, r);
	}
      } 
      if(doCorrectedData){
	if(doConjugateVis){
	  thisCorrectedData.put(curRow, conj(otherCorrectedData(r)));
	}
	else{
	  thisCorrectedData.put(curRow, otherCorrectedData, r);
	}
      }
    } // end if itsChanReversed
    
    if(doWeightScale){
      thisWeight.put(curRow, otherWeight(r)*itsWeightScale);
      if (copyWtSp) thisWeightSp.put(curRow, otherWeightSp(r)*itsWeightScale);
      thisSigma.put(curRow, otherSigma(r) * sScale);
    }
    else{
      thisWeight.put(curRow, otherWeight, r);
      if (copyWtSp) thisWeightSp.put(curRow, otherWeightSp, r);
      thisSigma.put(curRow, otherSigma, r);
    }
    
    thisFeed1.put(curRow, otherFeed2, r);
    thisFeed2.put(curRow, otherFeed1, r);
    thisTime.put(curRow, otherTime, r);
    thisInterval.put(curRow, otherInterval, r);
    thisExposure.put(curRow, otherExposure, r);
    thisTimeCen.put(curRow, otherTimeCen, r);
    thisArrayId.put(curRow, otherArrayId, r);
    thisFlag.put(curRow, otherFlag, r);
    if (copyFlagCat) thisFlagCat.put(curRow, otherFlagCat, r);
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

IPosition MSConcat::getShape(const ROMSDataDescColumns& ddCols, 
			     const ROMSSpWindowColumns& spwCols, 
			     const ROMSPolarizationColumns& polCols, 
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

void MSConcat::checkCategories(const ROMSMainColumns& otherCols) const {
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
             
    Vector<uInt> delrows(itsMS.pointing().nrow());
    indgen(delrows);
    itsMS.pointing().removeRow(delrows); 

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
      Vector<uInt> rowtodel(point.nrow());
      indgen(rowtodel);
      point.removeRow(rowtodel);
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

    Vector<uInt> delrows(otherPoint.nrow());
    indgen(delrows);
    otherPoint.removeRow(delrows); 

    return False;
  }
//   else if(!itsPointingNull && otherPointingNull){
//     os << LogIO::NORMAL << "MS to be appended does not have a valid pointing table, "
//        << itsMS.tableName() << ", however, has one. Result won't have one." << LogIO::POST;
             
//     Vector<uInt> delrows(itsMS.pointing().nrow());
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

      Vector<uInt> delrows(itsMS.pointing().nrow());
      indgen(delrows);
      itsMS.pointing().removeRow(delrows); 

      Vector<uInt> rowtodel(otherPoint.nrow());
      indgen(rowtodel);
      otherPoint.removeRow(rowtodel);
      
      return False;
    } 
	
    pointCol.antennaId().putColumn(antennaIDs);

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
  SimpleOrderedMap <Int, Int> tempObsIndex(-1);
  SimpleOrderedMap <Int, Int> tempObsIndexReverse(-1);
  SimpleOrderedMap <Int, Int> tempObsIndex2(-1);
  doObsA_p = False; 
  doObsB_p = True;

  Int originalNrow = obs.nrow(); // remember the original number of rows

  // copy the new obs rows over and note new ids in map
  Int actualRow=obs.nrow()-1;
  for (uInt k=0; k < otherObs.nrow() ; ++k){ 
    obs.addRow();
    ++actualRow;
    obsRow.put(actualRow, otherObsRow.get(k, True));
    tempObsIndex.define(k, actualRow);
    tempObsIndexReverse.define(actualRow, k);
  }
  if(remRedunObsId){ // remove redundant rows
    MSObservationColumns& obsCol = observation();
    Vector<Bool> rowToBeRemoved(obs.nrow(), False);
    vector<uInt> rowsToBeRemoved;
    for(uInt j=0; j<obs.nrow(); j++){ // loop over OBS table rows
      for (uInt k=j+1; k<obs.nrow(); k++){ // loop over remaining OBS table rows
	if(obsRowsEquivalent(obsCol, j, k)){ // rows equivalent?
	  // make entry in map for (k,j) and mark k for deletion
	  tempObsIndex2.define(k, j);
	  if(tempObsIndexReverse.isDefined(k)){ // remember that the observation was already in the obs table
	    otherObsIdsWithCounterpart_p.define(j, k);
	  }
	  rowToBeRemoved(k) = True;
	  rowsToBeRemoved.push_back(k);
	}
      }	     
    }// end for j

    // create final maps
    // map for first table
    for(Int i=0; i<originalNrow; i++){ // loop over rows of old first table
      if(tempObsIndex2.isDefined(i)){ // ID changed because of removal
	  newObsIndexA_p.define(i,tempObsIndex2(i));
	  doObsA_p = True;
      }
    }
    // map for second table
    for(uInt i=0; i<otherObs.nrow(); i++){ // loop over rows of second table
      if(tempObsIndex.isDefined(i)){ // ID changed because of addition to table
	if(tempObsIndex2.isDefined(tempObsIndex(i))){ // ID also changed because of removal 
	  newObsIndexB_p.define(i,tempObsIndex2(tempObsIndex(i)));
	}
	else { // ID only changed because of addition to the table
	  newObsIndexB_p.define(i,tempObsIndex(i));
	}
      }
    }
    if(rowsToBeRemoved.size()>0){ // actually remove the rows
      Vector<uInt> rowsTBR(rowsToBeRemoved);
      obs.removeRow(rowsTBR);
    }    
    os << "Added " << obs.nrow()- originalNrow << " rows and matched "
       << rowsToBeRemoved.size() << " rows in the observation subtable." << LogIO::POST;

  }
  else {
    // create map for second table only
    for(uInt i=0; i<otherObs.nrow(); i++){ // loop over rows of second table
      if(tempObsIndex.isDefined(i)){ // ID changed because of addition to table
	  newObsIndexB_p.define(i,tempObsIndex(i));
      }
    }
    os << "Added " << obs.nrow()- originalNrow << " rows in the observation subtable." << LogIO::POST;
  } // end if(remRedunObsId)

  return obs.nrow();
}


Block<uInt> MSConcat::copyAntennaAndFeed(const MSAntenna& otherAnt,
					 const MSFeed& otherFeed) {
  // uses newSPWIndex_p; to be called after copySpwAndPol

  LogIO os(LogOrigin("MSConcat", "copyAntennaAndFeed"));

  const uInt nAntIds = otherAnt.nrow();
  Block<uInt> antMap(nAntIds);

  const ROMSAntennaColumns otherAntCols(otherAnt);
  MSAntennaColumns& antCols = antenna();
  MSAntenna& ant = itsMS.antenna();
  const Quantum<Double> tol(1, "m");
  const ROTableRow otherAntRow(otherAnt);
  TableRow antRow(ant);
  TableRecord antRecord;
  //RecordFieldId nameAnt(MSAntenna::columnName(MSAntenna::NAME));

  MSFeedColumns& feedCols = feed();
  const ROMSFeedColumns otherFeedCols(otherFeed);

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
      const Vector<uInt> feedsToCompare = feedIndex.getRowNumbers();
      const Vector<uInt> itsFeedsToCompare = itsFeedIndex.getRowNumbers();
      const uInt nFeedsToCompare = feedsToCompare.nelements();
      uInt matchingFeeds = 0;
      Vector<uInt> ignoreRows;
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
	    //cout << "modifiying spwid from " << newSPWId << " to " << newSPWIndex_p(newSPWId) << endl;
	    newSPWId = newSPWIndex_p(newSPWId);
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
	const Vector<uInt> feedsToCopy = feedIndex.getRowNumbers();
	const uInt nFeedsToCopy = feedsToCopy.nelements();
	uInt destRow = feed.nrow();
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
//  		   << " to " << newSPWIndex_p(newSPWId) << endl;
	      feedRecord.define(spwField, newSPWIndex_p(newSPWId));
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
      const Vector<uInt> feedsToCopy = feedIndex.getRowNumbers();
      const uInt nFeedsToCopy = feedsToCopy.nelements();
      uInt destRow = feed.nrow();
      feed.addRow(nFeedsToCopy);
      for (uInt f = 0; f < nFeedsToCopy; f++, destRow++) {
	feedRecord = otherFeedRow.get(feedsToCopy(f));
	feedRecord.define(antField, static_cast<Int>(antMap[a]));
	feedRow.putMatchingFields(destRow, feedRecord);
      }
    }
  }
  return antMap;
}

Block<uInt> MSConcat::copyState(const MSState& otherState) {
  const uInt nStateIds = otherState.nrow();
  Block<uInt> stateMap(nStateIds);

  const ROMSStateColumns otherStateCols(otherState);
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
  const uInt nFlds = otherFld.nrow();
  Block<uInt> fldMap(nFlds);
  const Quantum<Double> tolerance=itsDirTol;
  const ROMSFieldColumns otherFieldCols(otherFld);
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
    for(uInt i=0; i<fieldCols.nrow(); i++){
      if(fieldCols.ephemerisId()(i)>maxThisEphId){
	maxThisEphId = fieldCols.ephemerisId()(i);
      }
    }
  }
  if(maxThisEphId>-1){ // this MS has at least one field using an ephemeris.
                       // maxThisEphId==-1 would mean there is an EPHEMERIS_ID column but there are no entries
    // find first and last obs time of other MS
    Vector<uInt> sortedI(otherms.nrow());
    ROMSMainColumns msmc(otherms);
    Vector<Double> mainTimesV = msmc.time().getColumn();
    GenSortIndirect<Double>::sort(sortedI,mainTimesV);
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
    catch(AipsError x){
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
	    catch(AipsError x){
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
	if(newSourceIndex_p.isDefined(oldIndex)){
	  fieldCols.sourceId().put(fldMap[f], newSourceIndex_p(oldIndex));
	}
      } 
      if(doSource2_p){
	Int oldIndex=fieldCols.sourceId()(fldMap[f]);
	if(newSourceIndex2_p.isDefined(oldIndex)){
	  fieldCols.sourceId().put(fldMap[f], newSourceIndex2_p(oldIndex));
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
    ROMSSourceColumns otherSourceCol(otherms.source());
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
      newSourceIndex_p.define(otherId(k), maxSrcId+1+otherId(k)); 
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

    const ROMSFieldColumns otherFieldCols(otherms.field());
    const ROMSFieldColumns fieldCols(itsMS.field());
    for(uInt i=0; i<itsMS.field().nrow(); i++){
      MDirection::Types refType = MDirection::castType(fieldCols.phaseDirMeas(i).getRef().getType());
      if(refType>=MDirection::MERCURY && refType<MDirection::N_Planets){ // we have a solar system object
	solSystObjects_p.define(fieldCols.sourceId()(i), (Int) refType);
      }
      if(!fieldCols.ephemPath(i).empty()){ // this is an ephemeris object
        solSystObjects_p.define(fieldCols.sourceId()(i), -2); // mark as -2
      }	
    }
    for(uInt i=0; i<otherms.field().nrow(); i++){
      MDirection::Types refType = MDirection::castType(otherFieldCols.phaseDirMeas(i).getRef().getType());
      if(refType>=MDirection::MERCURY && refType<MDirection::N_Planets){ // we have a solar system object
	solSystObjects_p.define(otherFieldCols.sourceId()(i)+maxSrcId+1, (Int) refType);
      }
      if(!fieldCols.ephemPath(i).empty()){ // this is an ephemeris object
	solSystObjects_p.define(otherFieldCols.sourceId()(i)+maxSrcId+1, -2); // mark as -2
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
      SimpleOrderedMap <Int, Int> tempSourceIndex(-1);
      SimpleOrderedMap <Int, Int> tempSourceIndex2(-1);
      SimpleOrderedMap <Int, Int> tempSourceIndex3(-1);
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
      
      // loop over the columns of the merged source table 
      for (Int j =0 ; j < numrows_this ; ++j){
	if(thisSPWId(j)<-1){ // came from the second input table
	  sourceRecord = sourceRow.get(j);
	  if(doSPW_p || newSPWIndex_p.isDefined(thisSPWId(j)+10000)){ // the SPW table was rearranged
	    sourceCol.spectralWindowId().put(j, newSPWIndex_p(thisSPWId(j)+10000) );
	    //sourceRecord.define(sourceSPWId, newSPWIndex_p(thisSPWId(j)+10000) );
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
      vector<uInt> rowsToBeRemoved;
      Vector<Int> thisSPWIdB=sourceCol.spectralWindowId().getColumn();

      for (Int j=0 ; j < numrows_this ; ++j){
	if(rowToBeRemoved(j)){
	  continue;
	}
	// check if row j has an equivalent row somewhere else in the table
	Int reftypej = solSystObjects_p(thisId(j));
	for (Int k=j+1 ; k < numrows_this ; ++k){
	  if (!rowToBeRemoved(k)){
	    if(thisSPWIdB(j)==thisSPWIdB(k)){ // the SPW id is the same
	      Int reftypek = solSystObjects_p(thisId(k));
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
		tempSourceIndex.define(thisId(k), thisId(j));
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
	Vector<uInt> rowsTBR(rowsToBeRemoved);
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
	  tempSourceIndex2.define(newThisId(j), nnrow);
	  sourceCol.sourceId().put(j, nnrow);
	}
      }
	
      // give equivalent rows the same source id 
      Bool rowsRenamed(False);
      Int nDistinctSources = newNumrows_this;
      Vector<Int> thisSourceId=sourceCol.sourceId().getColumn();
      for (Int j=0 ; j < newNumrows_this ; ++j){
	// check if row j has an equivalent row somewhere down in the table
	Int reftypej = solSystObjects_p(thisId(j));
	for (Int k=j+1 ; k < newNumrows_this ; ++k){
	  if(thisSourceId(j)!=thisSourceId(k)){
	    Int reftypek = solSystObjects_p(thisId(k));
 	    Bool sameSolSystObjects = ((reftypek==reftypej) && (reftypek>-1)) // object with solar syst ref frame
 	      || ((reftypek==reftypej) && (reftypek==-2)); // ephemeris object;
	    if( sourceRowsEquivalent(sourceCol, j, k, sameSolSystObjects)){ 
	                                          // all columns are the same except source id (not testing spw id),
	                                          // spw id must be different, otherwise row would have been deleted above
	      //cout << "Found SOURCE rows " << j << " and " << k << " to be identical except for the SPW ID and source id. "
	      //	 << newThisId(k) << " mapped to " << newThisId(j) << endl;
	      // give same source id
	      // make entry in map for (k, j) and rename k
	      tempSourceIndex3.define(newThisId(k), newThisId(j));
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
	    tempSourceIndex3.define(newThisId(j), nDistinctSources-counter-1 );
	    sourceRecord.define(sourceIdId, nDistinctSources-counter-1 );
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
	  if(tempSourceIndex.isDefined(j)){ // ID changed because of redundancy
	    if(tempSourceIndex2.isDefined(tempSourceIndex(j))){ // ID changed also because of renumbering
	      if( tempSourceIndex3.isDefined(tempSourceIndex2(tempSourceIndex(j))) ){ // ID also changed because of renaming
		newSourceIndex2_p.define(j, tempSourceIndex3(tempSourceIndex2(tempSourceIndex(j))) ); // abc
	      }
	      else { // ID changed because of redundancy and renumberning
		  newSourceIndex2_p.define(j, tempSourceIndex2(tempSourceIndex(j))); // ab
	      }
	    }
	    else{ 
	      if( tempSourceIndex3.isDefined(tempSourceIndex(j)) ){ // ID  changed because of redundancy and renaming
		newSourceIndex2_p.define(j, tempSourceIndex3(tempSourceIndex(j))); // ac		
	      }
	      else { // ID only changed because of redundancy
		newSourceIndex2_p.define(j, tempSourceIndex(j)); // a
	      }
	    }
	  }
	  else if(tempSourceIndex2.isDefined(j)){ 
	    if( tempSourceIndex3.isDefined(tempSourceIndex2(j)) ){ // ID  changed because of renumbering and renaming
	      newSourceIndex2_p.define(j, tempSourceIndex3(tempSourceIndex2(j))); // bc
	    }
	    else { // ID only changed because of renumbering
	      newSourceIndex2_p.define(j, tempSourceIndex2(j)); // b
	    }
	  }
	  else if(tempSourceIndex3.isDefined(j)){ // ID only changed because of renaming
	      newSourceIndex2_p.define(j, tempSourceIndex3(j)); // c
	    }
	}
	doSource2_p=True;
      }
   
    } // end if(numrows_this > 0) 
  }
  return doSource2_p;
}


Bool MSConcat::sourceRowsEquivalent(const MSSourceColumns& sourceCol, const uInt& rowi, const uInt& rowj,
				    const Bool dontTestDirection){
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
      catch (AipsError x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal position" << endl;
    }
    if(!(sourceCol.pulsarId().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.pulsarId(), rowi, rowj);
      }
      catch (AipsError x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal pulsarId" << endl;
    }
    if(!(sourceCol.restFrequency().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.restFrequency(), rowi, rowj);
      }
      catch (AipsError x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal restFrequency" << endl;
    }
    if(!(sourceCol.sysvel().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.sysvel(), rowi, rowj);
      }
      catch (AipsError x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal sysvel" << endl;
    }
    if(!(sourceCol.transition().isNull())){
      try {
	areEquivalent = areEQ(sourceCol.transition(), rowi, rowj);
      }
      catch (AipsError x) {
	// row has invalid data
	areEquivalent = True;
      }
      //      if(!areEquivalent) cout << "not equal transition" << endl;
    }
  }
  return areEquivalent;
}

Bool MSConcat::obsRowsEquivalent(const MSObservationColumns& obsCol, const uInt& rowi, const uInt& rowj){
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


Block<uInt> MSConcat::copySpwAndPol(const MSSpectralWindow& otherSpw,
				    const MSPolarization& otherPol,
				    const MSDataDescription& otherDD) {

  LogIO os(LogOrigin("MSConcat", "copySpwAndPol"));

  const uInt nDDs = otherDD.nrow();
  Block<uInt> ddMap(nDDs);
  
  const ROMSSpWindowColumns otherSpwCols(otherSpw);
  MSSpectralWindow& spw = itsMS.spectralWindow();
  MSSpWindowColumns& spwCols = spectralWindow();
  const ROTableRow otherSpwRow(otherSpw);
  TableRow spwRow(spw);
  const ROMSPolarizationColumns otherPolCols(otherPol);
  MSPolarization& pol = itsMS.polarization();
  MSPolarizationColumns& polCols = polarization();
  const ROTableRow otherPolRow(otherPol);
  TableRow polRow(pol);

  const ROMSDataDescColumns otherDDCols(otherDD);
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
      newSPWIndex_p.define(otherSpwId, *newSpwPtr); 
      // There cannot be an entry in the DATA_DESCRIPTION Table
      doSPW_p = True;      
    }
    else{
      // cout << "counterpart found for other spw " << otherSpwId 
      //     << " found in this spw " << *newSpwPtr << endl;
      matchedSPW = True;
      if(*newSpwPtr != otherSpwId){
	newSPWIndex_p.define(otherSpwId, *newSpwPtr);
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
	newSPWIndex_p.define(otherSpwId, newSpwId); 
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
