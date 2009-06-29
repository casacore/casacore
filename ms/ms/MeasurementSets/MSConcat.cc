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

#include <ms/MeasurementSets/MSConcat.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Block.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordField.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicMath/Math.h>
#include <ms/MeasurementSets/MSAntenna.h>
#include <ms/MeasurementSets/MSAntennaColumns.h>
#include <ms/MeasurementSets/MSDataDescColumns.h>
#include <ms/MeasurementSets/MSFeed.h>
#include <ms/MeasurementSets/MSField.h>
#include <ms/MeasurementSets/MSFieldColumns.h>
#include <ms/MeasurementSets/MSMainColumns.h>
#include <ms/MeasurementSets/MSPolColumns.h>
#include <ms/MeasurementSets/MSSpWindowColumns.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/TableMeasures/ScalarMeasColumn.h>
#include <measures/TableMeasures/ScalarQuantColumn.h>
#include <tables/Tables/ColumnsIndex.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableRow.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

namespace casa {

MSConcat::MSConcat(MeasurementSet& ms):
  MSColumns(ms),
  itsMS(ms),
  itsFixedShape(isFixedShape(ms.tableDesc())), 
  newSourceIndex_p(-1), newSourceIndex2_p(-1), newSPWIndex_p(-1),
  newObsIndexA_p(-1), newObsIndexB_p(-1)
{
  itsDirTol=Quantum<Double>(1.0, "mas");
  itsFreqTol=Quantum<Double>(1.0, "Hz");
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
	  dataColName == MS::columnName(MS::FLOAT_DATA) ||
	  dataColName == MS::columnName(MS::CORRECTED_DATA) || 
	  dataColName == MS::columnName(MS::MODEL_DATA) || 
	  dataColName == MS::columnName(MS::LAG_DATA) ||
	  dataColName == MS::columnName(MS::SIGMA) || 
	  dataColName == MS::columnName(MS::WEIGHT) || 
	  dataColName == MS::columnName(MS::IMAGING_WEIGHT) || 
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

  void MSConcat::concatenate(const MeasurementSet& otherMS)
{
  LogIO log(LogOrigin("MSConcat", "concatenate"));

  log << "Appending " << otherMS.tableName() 
      << " to " << itsMS.tableName() << LogIO::POST;


  // check if certain columns are present and set flags accordingly
  Bool doCorrectedData=False, doImagingWeight=False, doModelData=False;
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
	<< " in imager or calibrater "  	<< LogIO::EXCEPTION;
  }
  if (itsMS.tableDesc().isColumn("CORRECTED_DATA") && 
      otherMS.tableDesc().isColumn("CORRECTED_DATA"))
    doCorrectedData=True;
  else if (itsMS.tableDesc().isColumn("CORRECTED_DATA") && 
	   !otherMS.tableDesc().isColumn("CORRECTED_DATA"))
    log << itsMS.tableName() 
	<<" has CORRECTED_DATA column but not " << otherMS.tableName()
	<< LogIO::EXCEPTION;
  if (itsMS.tableDesc().isColumn("IMAGING_WEIGHT") && 
      otherMS.tableDesc().isColumn("IMAGING_WEIGHT"))
    doImagingWeight=True;
  else if (itsMS.tableDesc().isColumn("IMAGING_WEIGHT") && 
	   !otherMS.tableDesc().isColumn("IMAGING_WEIGHT"))
    log << itsMS.tableName() 
	<< " has IMAGING_WEIGHT column but not " << otherMS.tableName() 
	<< LogIO::EXCEPTION;

  
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


  // merge ANTENNA and FEED
  uInt oldRows = itsMS.antenna().nrow();
  const Block<uInt> newAntIndices = 
    copyAntennaAndFeed(otherMS.antenna(), otherMS.feed());
  {
    const uInt addedRows = itsMS.antenna().nrow() - oldRows;
    const uInt matchedRows = otherMS.antenna().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the antenna subtable" << endl;
  }

  //See if there is a SOURCE table and concatenate and reindex it
  copySource(otherMS);

  // DATA_DESCRIPTION
  oldRows = itsMS.dataDescription().nrow();
  const Block<uInt> newDDIndices = copySpwAndPol(otherMS.spectralWindow(),
						 otherMS.polarization(),
						 otherMS.dataDescription());
  {
    const uInt addedRows = itsMS.dataDescription().nrow() - oldRows;
    const uInt matchedRows = otherMS.dataDescription().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the data description subtable" << endl;
  }

  // correct the spw entries in the SOURCE table and remove redundant rows
  oldRows = itsMS.source().nrow();
  updateSource();
  {
    uInt removedRows =  oldRows - itsMS.source().nrow();
    if(removedRows>0){
      log << "Removed " << removedRows 
	  << " redundant rows from the source subtable" << endl;
    }
  }

  // FIELD
  oldRows = itsMS.field().nrow();
  const Block<uInt> newFldIndices = copyField(otherMS.field());
  {
    const uInt addedRows = itsMS.field().nrow() - oldRows;
    const uInt matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the field subtable" << endl;
  }


  // I need to check that the Measures and units are the same.
  const uInt newRows = otherMS.nrow();
  uInt curRow = itsMS.nrow();
  itsMS.addRow(newRows);

  ROArrayColumn<Complex> otherModelData, otherCorrectedData;
  ROArrayColumn<Float> otherImagingWeight;
  ArrayColumn<Complex> thisModelData, thisCorrectedData;
  ArrayColumn<Float> thisImagingWeight;
  
  if(doCorrectedData){
    thisCorrectedData.reference(correctedData());
    otherCorrectedData.reference(otherMainCols.correctedData());
  }
  if(doModelData){
    thisModelData.reference(modelData());
    otherModelData.reference(otherMainCols.modelData());
  }
  if(doImagingWeight){
    thisImagingWeight.reference(imagingWeight());
    otherImagingWeight.reference(otherMainCols.imagingWeight());
  }
  const ROScalarColumn<Double>& otherTime = otherMainCols.time();
  ScalarColumn<Double>& thisTime = time();
  const ROScalarColumn<Int>& otherAnt1 = otherMainCols.antenna1();
  ScalarColumn<Int>& thisAnt1 = antenna1();
  const ROScalarColumn<Int>& otherAnt2 = otherMainCols.antenna2();
  ScalarColumn<Int>& thisAnt2 = antenna2();
  const ROScalarColumn<Int>& otherFeed1 = otherMainCols.feed1();
  ScalarColumn<Int>& thisFeed1 = feed1();
  const ROScalarColumn<Int>& otherFeed2 = otherMainCols.feed2();
  ScalarColumn<Int>& thisFeed2 = feed2();
  const ROScalarColumn<Int>& otherDDId = otherMainCols.dataDescId();
  ScalarColumn<Int>& thisDDId = dataDescId();
  const ROScalarColumn<Int>& otherFieldId = otherMainCols.fieldId();
  ScalarColumn<Int>& thisFieldId = fieldId();
  const ROScalarColumn<Double>& otherInterval = otherMainCols.interval();
  ScalarColumn<Double>& thisInterval = interval();
  const ROScalarColumn<Double>& otherExposure = otherMainCols.exposure();
  ScalarColumn<Double>& thisExposure = exposure();
  const ROScalarColumn<Double>& otherTimeCen = otherMainCols.timeCentroid();
  ScalarColumn<Double>& thisTimeCen = timeCentroid();
  const ROScalarColumn<Int>& otherScan = otherMainCols.scanNumber();
  ScalarColumn<Int>& thisScan = scanNumber();
  const ROScalarColumn<Int>& otherArrayId = otherMainCols.arrayId();
  ScalarColumn<Int>& thisArrayId = arrayId();
  const ROScalarColumn<Int>& otherStateId = otherMainCols.stateId();
  ScalarColumn<Int>& thisStateId = stateId();
  const ROArrayColumn<Double>& otherUvw = otherMainCols.uvw();
  ArrayColumn<Double>& thisUvw = uvw();

  ROArrayColumn<Complex> otherData;
  ArrayColumn<Complex> thisData;
  ROArrayColumn<Float> otherFloatData;
  ArrayColumn<Float> thisFloatData;
  if(doFloatData){
    thisFloatData.reference(floatData());
    otherFloatData.reference(otherMainCols.floatData());
  }
  else{
    thisData.reference(data());
    otherData.reference(otherMainCols.data());
  }

  const ROArrayColumn<Float>& otherSigma = otherMainCols.sigma();
  ArrayColumn<Float>& thisSigma = sigma();
  const ROArrayColumn<Float>& otherWeight = otherMainCols.weight();
  ArrayColumn<Float>& thisWeight = weight();
  const ROArrayColumn<Bool>& otherFlag = otherMainCols.flag();
  ArrayColumn<Bool>& thisFlag = flag();
  const ROArrayColumn<Bool>& otherFlagCat = otherMainCols.flagCategory();
  ArrayColumn<Bool>& thisFlagCat = flagCategory();
  Bool copyFlagCat = !(thisFlagCat.isNull() || otherFlagCat.isNull());
  copyFlagCat = copyFlagCat && thisFlagCat.isDefined(0) 
    && otherFlagCat.isDefined(0);
  const ROScalarColumn<Bool>& otherFlagRow = otherMainCols.flagRow();
  ScalarColumn<Bool>& thisFlagRow = flagRow();
  const ROScalarColumn<Int>& otherObsId=otherMainCols.observationId();
  Vector<Int> obsIds=otherObsId.getColumn();

  // OBSERVATION
  copyObservation(otherMS.observation());

  // POINTING
  copyPointing(otherMS.pointing(), newAntIndices);

  ScalarColumn<Int>& thisObsId = observationId();
  const ROArrayColumn<Float>& otherWeightSp = otherMainCols.weightSpectrum();
  ArrayColumn<Float>& thisWeightSp = weightSpectrum();
  Bool copyWtSp = !(thisWeightSp.isNull() || otherWeightSp.isNull()); 
  copyWtSp = copyWtSp && thisWeightSp.isDefined(0) 
    && otherWeightSp.isDefined(0);

  if(doObsA_p){ // the obs ids changed for the first table
    Vector<Int> oldObsIds=thisObsId.getColumn();
    for(uInt r = 0; r < curRow; r++) {
      if(newObsIndexA_p.isDefined(oldObsIds[r])){ // apply change 
	thisObsId.put(r, newObsIndexA_p(oldObsIds[r]));
      }
    }
  }  
      
  for (uInt r = 0; r < newRows; r++, curRow++) {
    thisTime.put(curRow, otherTime, r);
    thisAnt1.put(curRow, newAntIndices[otherAnt1(r)]);
    thisAnt2.put(curRow, newAntIndices[otherAnt2(r)]);
    thisFeed1.put(curRow, otherFeed1, r);
    thisFeed2.put(curRow, otherFeed2, r);
    thisDDId.put(curRow, newDDIndices[otherDDId(r)]);
    thisFieldId.put(curRow, newFldIndices[otherFieldId(r)]);
    thisInterval.put(curRow, otherInterval, r);
    thisExposure.put(curRow, otherExposure, r);
    thisTimeCen.put(curRow, otherTimeCen, r);
    thisScan.put(curRow, otherScan, r);
    thisArrayId.put(curRow, otherArrayId, r);

    if(doObsB_p && newObsIndexB_p.isDefined(obsIds[r])){ 
      // the obs ids have been changed for the table to be appended
      thisObsId.put(curRow, newObsIndexB_p(obsIds[r]));
    }
    else { // this OBS id didn't change
      thisObsId.put(curRow, obsIds[r]);
    }

    thisStateId.put(curRow, otherStateId, r);
    thisUvw.put(curRow, otherUvw, r);
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
	thisData.put(curRow, reversedData);
      }
      if(doCorrectedData){
	thisCorrectedData.put(curRow, reversedCorrData);
      }
      if(doModelData){
	thisModelData.put(curRow, reversedModData);
      }
    }
    else{
      if(doFloatData){
	thisFloatData.put(curRow, otherFloatData, r);
      }
      else{
	thisData.put(curRow, otherData, r);
      }
      if(doModelData)
	thisModelData.put(curRow, otherModelData, r);
      if(doCorrectedData)
	thisCorrectedData.put(curRow, otherCorrectedData, r);
    }
    if(doImagingWeight)
      thisImagingWeight.put(curRow, otherImagingWeight, r);
    thisSigma.put(curRow, otherSigma, r);
    thisWeight.put(curRow, otherWeight, r);
    thisFlag.put(curRow, otherFlag, r);
    if (copyFlagCat) thisFlagCat.put(curRow, otherFlagCat, r);
    thisFlagRow.put(curRow, otherFlagRow, r);
    if (copyWtSp) thisWeightSp.put(curRow, otherWeightSp, r);
  } 

  if(doModelData){
    //update the MODEL_DATA keywords
    updateModelDataKeywords();
  }

}

void MSConcat::setTolerance(Quantum<Double>& freqTol, Quantum<Double>& dirTol){

  itsFreqTol=freqTol;
  itsDirTol=dirTol;
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
       <<"Flag category columns do match in these two ms's\n" 
       <<"This is not important as Flag category is being deprecated"
       << LogIO::POST;
    return;
    //throw(AipsError(String("MSConcat::checkCategories\n") + 
    //		    String("cannot concatenate this measurement set as ") +
    //		    String("it has a different number of flag categories")));
  }
  for (uInt c = 0; c < nCat; c++) {
    if (cat(c) != otherCat(c)) {
      os << LogIO::WARN 
	 <<"Flag category columns do match in these two ms's\n" 
	 <<"This is not important as Flag category is being deprecated"
	 << LogIO::POST;
      return;
      //throw(AipsError(String("MSConcat::checkCategories\n") + 
      //		      String("cannot concatenate this measurement set as ") +
      //		      String("it has different flag categories")));
    }
  }
}


Bool MSConcat::copyPointing(const MSPointing& otherPoint,const 
			    Block<uInt>& newAntIndices ){

  LogIO os(LogOrigin("MSConcat", "concatenate"));

  if(itsMS.pointing().isNull()|| (itsMS.pointing().nrow() == 0)){
    //We do not have a valid pointing table so we don't care
    return False;
  }
  if(otherPoint.isNull() || (otherPoint.nrow() == 0)){

    os << LogIO::WARN 
       << "No valid pointing table in ms that is being concatenated" 
       << LogIO::POST;
    os << LogIO::WARN 
       << "It may be a problem for e.g mosaicing,\n so all pointing information is being deleted" 
       << LogIO::POST;

             
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
    Vector<Int> antennaIDs=pointCol.antennaId().getColumn();
    if(( min(antennaIDs) <0) || ( uInt(max(antennaIDs)) > newAntIndices.nelements())){
      
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


Int MSConcat::copyObservation(const MSObservation& otherObs, 
			      const Bool remRedunObsId){
  LogIO os(LogOrigin("MSConcat", "copyObservation"));

  MSObservation& obs=itsMS.observation();
  TableRow obsRow(obs);
  const ROTableRow otherObsRow(otherObs);
  newObsIndexA_p.clear();
  newObsIndexB_p.clear();
  SimpleOrderedMap <Int, Int> tempObsIndex(-1);
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
  }
  if(remRedunObsId){ // remove redundant rows
    MSObservationColumns& obsCol = observation();
    Vector<Bool> rowToBeRemoved(obs.nrow(), False);
    vector<uint> rowsToBeRemoved;
    for(uInt j=0; j<obs.nrow(); j++){ // loop over OBS table rows
      for (uInt k=j+1; k<obs.nrow(); k++){ // loop over remaining OBS table rows
	if(obsRowsEquivalent(obsCol, j, k)){ // rows equivalent?
	  // make entry in map for (k,j) and mark k for deletion
	  tempObsIndex2.define(k, j);
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
       << rowsToBeRemoved.size() << " rows in the observation subtable." << endl;

  }
  else {
    // create map for second table only
    for(uInt i=0; i<otherObs.nrow(); i++){ // loop over rows of second table
      if(tempObsIndex.isDefined(i)){ // ID changed because of addition to table
	  newObsIndexB_p.define(i,tempObsIndex(i));
      }
    }
    os << "Added " << obs.nrow()- originalNrow << " rows in the observation subtable." << endl;
  } // end if(remRedunObsId)

  return obs.nrow();
}


Block<uInt> MSConcat::copyAntennaAndFeed(const MSAntenna& otherAnt,
					 const MSFeed& otherFeed) {
  const uInt nAntIds = otherAnt.nrow();
  Block<uInt> antMap(nAntIds);

  const ROMSAntennaColumns otherAntCols(otherAnt);
  MSAntennaColumns& antCols = antenna();
  MSAntenna& ant = itsMS.antenna();
  const Quantum<Double> tol(1, "m");
  const ROTableRow otherAntRow(otherAnt);
  TableRow antRow(ant);

  const String& antIndxName = MSFeed::columnName(MSFeed::ANTENNA_ID);
  MSFeed& feed = itsMS.feed();
  const ROTableRow otherFeedRow(otherFeed);
  TableRow feedRow(feed);
  TableRecord feedRecord;
  ColumnsIndex feedIndex(otherFeed, Vector<String>(1, antIndxName));
  RecordFieldPtr<Int> antInd(feedIndex.accessKey(), antIndxName);
  RecordFieldId antField(antIndxName);
  
  for (uInt a = 0; a < nAntIds; a++) {
    const Int newAntId = antCols.matchAntenna(otherAntCols.name()(a), 
					      otherAntCols.positionMeas()(a), tol);
    if (newAntId >= 0 && 
	antCols.station()(newAntId)==otherAntCols.station()(a) ) { // require that also the STATION matches
      antMap[a] = newAntId;
      // Should really check that the FEED table contains all the entries for
      // this antenna and that they are the same. I'll just assume this for
      // now.
    } else { // need to add a new entry in the ANTENNA subtable
      antMap[a] = ant.nrow();
      ant.addRow();
      antRow.putMatchingFields(antMap[a], otherAntRow.get(a));
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

Block<uInt>  MSConcat::copyField(const MSField& otherFld) {
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

  TableRow fldRow(fld);
  for (uInt f = 0; f < nFlds; f++) {
    delayDir = otherFieldCols.delayDirMeas(f);
     phaseDir = otherFieldCols.phaseDirMeas(f);
     refDir = otherFieldCols.referenceDirMeas(f);
     if (dirType != otherDirType) {
       delayDir = dirCtr(delayDir.getValue());
       phaseDir = dirCtr(phaseDir.getValue());
       refDir = dirCtr(refDir.getValue());
     }

     const Int newFld = 
       fieldCols.matchDirection(refDir, delayDir, phaseDir, tolerance);
     if (newFld >= 0) {
       fldMap[f] = newFld;
     } else { // need to add a new entry in the FIELD subtable
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
    if(newSource.nrow() > 0){ 
      if(!Table::isReadable(otherms.sourceTableName())){
	return False;
      }
      const MSSource& otherSource=otherms.source();
      if(otherSource.nrow()==0){
	return False;
      }
      maxSrcId=max(sourceCol.sourceId().getColumn());
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
	  if(doSPW_p){ // the SPW table was rearranged
	    sourceRecord.define(sourceSPWId, newSPWIndex_p(thisSPWId(j)+10000) );
	  }
	  else { // the SPW table did not have to be rearranged, just revert changes to SPW from copySource
	    sourceRecord.define(sourceSPWId, thisSPWId(j)+10000 );
	  }
	  sourceRow.putMatchingFields(j, sourceRecord);
	} // end for j
      }

      // Check if there are redundant rows and remove them creating map for copyField
      // loop over the columns of the merged source table 
      Vector<Bool> rowToBeRemoved(numrows_this, False);
      vector<uint> rowsToBeRemoved;
      for (Int j=0 ; j < numrows_this ; ++j){
	// check if row j has an equivalent row somewhere else in the table
	for (Int k=0 ; k < numrows_this ; ++k){
	  if (k!=j && !rowToBeRemoved(j) && !rowToBeRemoved(k)){
	    if( sourceRowsEquivalent(sourceCol, j, k) ){ // all columns are the same (not testing source and spw id)
	      if(areEQ(sourceCol.spectralWindowId(),j, k)){ // also the SPW id is the same
//		cout << "Found SOURCE rows " << j << " and " << k << " to be identical." << endl;
		// delete one of the rows
		if(j<k){ // make entry in map for (k, j) and delete k
		  tempSourceIndex.define(thisId(k), thisId(j));
		  rowToBeRemoved(k) = True;
		  rowsToBeRemoved.push_back(k);
		}
		else{ // make entry in map for (j, k) and delete j
		  tempSourceIndex.define(thisId(j), thisId(k));
		  rowToBeRemoved(j) = True;
		  rowsToBeRemoved.push_back(j);
		}
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
      Bool rowsRenumbered(False);
      Int nnrow = 0;
      for (Int j=0 ; j < newNumrows_this ; ++j){
	if(newThisId(j) > nnrow){ 
	  nnrow++;
	  sourceRecord = sourceRow.get(j);
	  tempSourceIndex2.define(newThisId(j), nnrow);
	  sourceRecord.define(sourceIdId, nnrow );
	  sourceRow.putMatchingFields(j, sourceRecord);
	  rowsRenumbered = True;
	}
      }
	
      // give equivalent rows the same source id 
      Bool rowsRenamed(False);
      Int nDistinctSources = newNumrows_this;
      for (Int j=0 ; j < newNumrows_this ; ++j){
	// check if row j has an equivalent row somewhere down in the table
	for (Int k=j+1 ; k < newNumrows_this ; ++k){
	  if( sourceRowsEquivalent(sourceCol, j, k) && 
	      !areEQ(sourceCol.sourceId(),j, k)){ // all columns are the same except source id (not testing spw id),
	                                          // spw id must be different, otherwise row would have been deleted above
// 	    cout << "Found SOURCE rows " << j << " and " << k << " to be identical except for the SPW ID and source id. "
// 		 << newThisId(k) << " mapped to " << newThisId(j) << endl;
	    // give same source id
	    // make entry in map for (k, j) and rename k
	    tempSourceIndex3.define(newThisId(k), newThisId(j));
	    sourceRecord = sourceRow.get(k);
	    sourceRecord.define(sourceIdId, newThisId(j) );
	    sourceRow.putMatchingFields(k, sourceRecord);
	    rowsRenamed = True;
	    nDistinctSources--;
	  } 
	}
      } // end for j

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


Bool MSConcat::sourceRowsEquivalent(const MSSourceColumns& sourceCol, const uInt& rowi, const uInt& rowj){
  // check if the two SOURCE table rows are identical IGNORING SOURCE_ID and SPW_ID

  Bool areEquivalent(False);

  // test the non-optional columns first
  if(areEQ(sourceCol.calibrationGroup(), rowi, rowj) &&
     areEQ(sourceCol.code(), rowi, rowj) &&
     areEQ(sourceCol.name(), rowi, rowj) &&
     areEQ(sourceCol.numLines(), rowi, rowj) &&
     // do NOT test SPW ID!
     // areEQ(sourceCol.spectralWindowId(), rowi, rowj) &&
     areEQ(sourceCol.direction(), rowi, rowj) &&
     areEQ(sourceCol.interval(), rowi, rowj) &&
     areEQ(sourceCol.properMotion(), rowi, rowj) &&
     areEQ(sourceCol.time(), rowi, rowj) 
     ){
    
    //    cout << "All non-optionals equal" << endl;
    
    // test the optional columns next
    areEquivalent = True;
    if(!(sourceCol.position().isNull())){
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
  // loop over the rows of the other data description table
  for (uInt d = 0; d < nDDs; d++) {
    Bool matchedDD = True;
    DebugAssert(otherDDCols.spectralWindowId()(d) >= 0 &&
		otherDDCols.spectralWindowId()(d) < static_cast<Int>(otherSpw.nrow()), 
		AipsError);
    const uInt otherSpwId = static_cast<uInt>(otherDDCols.spectralWindowId()(d));
    DebugAssert(otherSpwCols.numChan()(otherSpwId) > 0, AipsError);    

    Vector<Double> otherFreqs = otherSpwCols.chanFreq()(otherSpwId);
    *newSpwPtr = spwCols.matchSpw(otherSpwCols.refFrequencyMeas()(otherSpwId),
				  static_cast<uInt>(otherSpwCols.numChan()(otherSpwId)),
				  otherSpwCols.totalBandwidthQuant()(otherSpwId),
				  otherSpwCols.ifConvChain()(otherSpwId), freqTol, 
				  otherFreqs, itsChanReversed[d]);
    
    if (*newSpwPtr < 0) {
      // need to add a new entry in the SPECTRAL_WINDOW subtable
      *newSpwPtr= spw.nrow();
      spw.addRow();
      spwRow.putMatchingFields(*newSpwPtr, otherSpwRow.get(otherSpwId));
      // fill map to be used by updateSource()
      newSPWIndex_p.define(otherSpwId, *newSpwPtr); 
      // There cannot be an entry in the DATA_DESCRIPTION Table
      matchedDD = False;
      doSPW_p = True;      
    }
    
    DebugAssert(otherDDCols.polarizationId()(d) >= 0 &&
		otherDDCols.polarizationId()(d) < 
		static_cast<Int>(otherPol.nrow()), AipsError);
    const uInt otherPolId = 
      static_cast<uInt>(otherDDCols.polarizationId()(d));

    otherPolCols.corrType().get(otherPolId, corrInt, True);
    const uInt nCorr = corrInt.nelements();
    corrPol.resize(nCorr);
    for (uInt p = 0; p < nCorr; p++) {
      corrPol(p) = Stokes::type(corrInt(p));
    }
    Bool matchedBoth=False;
    uInt numActPol =0;
    while ( !matchedBoth && (numActPol < polCols.nrow()) ){
      *newPolPtr = polCols.match(corrPol, numActPol);
      if (*newPolPtr < 0) {
	// need to add a new entry in the POLARIZATION subtable
	*newPolPtr= pol.nrow();
	pol.addRow();
	polRow.putMatchingFields(*newPolPtr, otherPolRow.get(otherPolId));
	// Again there cannot be an entry in the DATA_DESCRIPTION Table
	matchedDD = False;
	matchedBoth = True; // just to break out of while loop
      }
      else{
	// We need to check if there exists an entry in the DATA_DESCRIPTION
	// table with the required spectral window and polarization index.
	//if we had a match on spw
	if(matchedDD)
	  ddMap[d] = ddIndex.getRowNumber(matchedBoth);
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
  return ddMap;
}

void MSConcat::updateModelDataKeywords(){
  Int nSpw=itsMS.spectralWindow().nrow();
  MSSpWindowColumns msSpW(itsMS.spectralWindow());
  Matrix<Int> selection(2,nSpw);
  // fill in default selection
  selection.row(0)=0; //start
  selection.row(1)=msSpW.numChan().getColumn(); 
  TableColumn col(itsMS,"MODEL_DATA");
  if (col.keywordSet().isDefined("CHANNEL_SELECTION"))
    col.rwKeywordSet().removeField("CHANNEL_SELECTION");
  col.rwKeywordSet().define("CHANNEL_SELECTION",selection);
}
// Local Variables: 
// compile-command: "gmake MSConcat"
// End: 

} //#End casa namespace
