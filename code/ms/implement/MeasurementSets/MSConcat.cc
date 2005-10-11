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
  itsFixedShape(isFixedShape(ms.tableDesc()))
{


  itsDirTol=Quantum<Double>(1.0, "mas");
  itsFreqTol=Quantum<Double>(1.0, "Hz");
  
  // if (ms.tableInfo().subType() != "UVFITS") {
  // throw(AipsError("MSConcat::MSConcat(..) - Measurement set was not created"
  //		    " from a UVFITS file."));
  //}
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
  //  if (otherMS.tableInfo().subType() != "UVFITS") {
  //    log << "Measurement set was not created from a UVFITS file."
  //	<< LogIO::EXCEPTION;
  //}
  log << "Appending " << otherMS.tableName() 
      << " to " << itsMS.tableName() << endl;

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
  oldRows = itsMS.field().nrow();
  const Block<uInt> newFldIndices = copyField(otherMS.field());
  {
    const uInt addedRows = itsMS.field().nrow() - oldRows;
    const uInt matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the field subtable" << endl;
  }
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
  //  const ROArrayColumn<Complex>& otherData = otherMainCols.data();
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
  // ArrayColumn<Complex>& thisData = data();
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
  Int numObsId=copyObservation(otherMS.observation(), obsIds);
  copyPointing(otherMS.pointing(), newAntIndices);

  // This needs to be fixed when I relaxe the restriction that the input MS
  // must have been created using the uvfits filler.
  const Int curObsId =  observationId()(curRow-1) + 1;
  ScalarColumn<Int>& thisObsId = observationId();
  const ROArrayColumn<Float>& otherWeightSp = otherMainCols.weightSpectrum();
  ArrayColumn<Float>& thisWeightSp = weightSpectrum();
  Bool copyWtSp = !(thisWeightSp.isNull() || otherWeightSp.isNull()); 
  copyWtSp = copyWtSp && thisWeightSp.isDefined(0) 
    && otherWeightSp.isDefined(0);

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
    thisObsId.put(curRow, obsIds[r]);
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
  const Vector<String> cat = flagCategories();
  const Vector<String> otherCat = otherCols.flagCategories();
  const uInt nCat = cat.nelements();
  if (nCat != otherCat.nelements()) {
    throw(AipsError(String("MSConcat::checkCategories\n") + 
		    String("cannot concatenate this measurement set as ") +
		    String("it has a different number of flag categories")));
  }
  for (uInt c = 0; c < nCat; c++) {
    if (cat(c) != otherCat(c)) {
      throw(AipsError(String("MSConcat::checkCategories\n") + 
		      String("cannot concatenate this measurement set as ") +
		      String("it has different flag categories")));
    }
  }
}


Bool MSConcat::copyPointing(const MSPointing& otherPoint,const 
			    Block<uInt>& newAntIndices ){

  LogIO os(LogOrigin("MSConcat", "concatenate"));

  if(itsMS.pointing().isNull()){
    //We do not have a valid pointing table so we don't care
    return False;
  }
  if(otherPoint.isNull()){

    os << LogIO::WARN 
       << "No valid pointing table in ms that is being concatenated" 
       << LogIO::POST;
    os << LogIO::WARN 
       << "It may be a problem for e.g mosaicing, if that is the case: " 
       << LogIO::POST;
    os << LogIO::WARN 
       << "please delete the rows in the POINTING table of the resulting ms " 
       << LogIO::POST;
    os << LogIO::WARN 
       << "imager then will use the FIELD table to get the pointing info " 
       << LogIO::POST;
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
  MSPointingColumns pointCol(point);
  Vector<Int> antennaIDs=pointCol.antennaId().getColumn();
  for (Int k=origNRow; k <  (origNRow+rowToBeAdded); ++k){
    pointCol.antennaId().put(k, newAntIndices[antennaIDs[k]]);
  }
  return True;

}


Int MSConcat::copyObservation(const MSObservation& otherObs, 
			      Vector<Int>& otherObsId){

  Int obsId=-1;
  MSObservation& obs=itsMS.observation();
  TableRow obsRow(obs);
  Int actualRow=obs.nrow()-1;
  const ROTableRow otherObsRow(otherObs);
  for (uInt k=0; k < otherObsId.nelements() ; ++k){ 
    if(obsId != otherObsId[k]){
      obsId=otherObsId[k];
      obs.addRow();
      ++actualRow;
      obsRow.put(actualRow, otherObsRow.get(obsId, True));
      
    }

    otherObsId[k]=actualRow;
  }
  return itsMS.observation().nrow();

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
    const Int newAntId = 
      antCols.matchAntenna(otherAntCols.positionMeas()(a), tol);
    if (newAntId >= 0) {
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
    }
  }
  return fldMap;
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
  // Get a guess at the tolerance
  /*  Double tolerance;
  {
    ROArrayColumn<Double> frequencies(spw,
		    MSSpectralWindow::columnName(MSSpectralWindow::CHAN_FREQ));
    Vector<Double> frequ=frequencies(0);
    tolerance=max(frequ)/1.0e6;
  }
  
  const Quantum<Double> freqTol(tolerance, "Hz");
  */
  const Quantum<Double> freqTol=itsFreqTol;
  const String& spwIdxName = 
    MSDataDescription::columnName(MSDataDescription::SPECTRAL_WINDOW_ID);
  const String& polIdxName = 
    MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID);
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
  for (uInt d = 0; d < nDDs; d++) {
    Bool matchedDD = True;
    DebugAssert(otherDDCols.spectralWindowId()(d) >= 0 &&
		otherDDCols.spectralWindowId()(d) < 
		static_cast<Int>(otherSpw.nrow()), AipsError);
    const uInt otherSpwId = 
      static_cast<uInt>(otherDDCols.spectralWindowId()(d));
    DebugAssert(otherSpwCols.numChan()(otherSpwId) > 0, AipsError);    
    Vector<Double> otherFreqs = otherSpwCols.chanFreq()(otherSpwId);
    *newSpwPtr = 
      spwCols.matchSpw(otherSpwCols.refFrequencyMeas()(otherSpwId),
		       static_cast<uInt>(otherSpwCols.numChan()(otherSpwId)),
		       otherSpwCols.totalBandwidthQuant()(otherSpwId),
		       otherSpwCols.ifConvChain()(otherSpwId), freqTol, 
		       otherFreqs, itsChanReversed[d]);
    
    if (*newSpwPtr < 0) {
      // need to add a new entry in the SPECTRAL_WINDOW subtable
      *newSpwPtr= spw.nrow();
      spw.addRow();
      spwRow.putMatchingFields(*newSpwPtr, otherSpwRow.get(otherSpwId));
      // There cannot be an entry in the DATA_DESCRIPTION Table
      matchedDD = False;
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
