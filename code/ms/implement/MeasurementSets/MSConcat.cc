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

#include <trial/MeasurementSets/MSConcat.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Block.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordField.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Math.h>
#include <aips/MeasurementSets/MSAntenna.h>
#include <aips/MeasurementSets/MSAntennaColumns.h>
#include <aips/MeasurementSets/MSDataDescColumns.h>
#include <aips/MeasurementSets/MSFeed.h>
#include <aips/MeasurementSets/MSField.h>
#include <aips/MeasurementSets/MSFieldColumns.h>
#include <aips/MeasurementSets/MSMainColumns.h>
#include <aips/MeasurementSets/MSPolColumns.h>
#include <aips/MeasurementSets/MSSpWindowColumns.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ColumnsIndex.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRow.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/iostream.h>

MSConcat::MSConcat(MeasurementSet& ms):
  MSColumns(ms),
  itsMS(ms),
  itsFixedShape(isFixedShape(ms.tableDesc()))
{
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

Bool MSConcat::checkSM(const MeasurementSet& ms1) const {
	// At the moment no check of subtable storage managers is done.  This may
	// be need to be added at a future date.  The subtable storage manager
	// can be accesses in a manner similar to accessing the main table storage
	// manager -> Record dminfo = ms.spectralWindow().dataManagerInfo();
	// will access the storage manager for the spectral window subtable.

	// Check to see if all of the columns in the MAIN table which depend
	// on the number of channels are using the TiledShapeStMan -> otherwise
	// return false.
	
	// The logic on this check is bad and incurs a lot of needless overhead.
	// This should be rewritten more robustly at some point.
	
	Record dminfo = ms1.dataManagerInfo();
	TableDesc td = ms1.tableDesc();
	String SMType, colname;

	if (td.isColumn("DATA")) {
		//cout << "Checking DATA column" << endl;
		for (uInt i=0; i<dminfo.nfields(); i++) {
			String type = dminfo.subRecord(i).asString("TYPE");
			Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
			uInt n = 0;
			while (n < cols.nelements()) {
				if (cols(n) == "DATA") {
					SMType = dminfo.subRecord(i).asString("TYPE");
					break;
				}
				n++;
			}
		}
		if (SMType != "TiledShapeStMan") return False;
	}
			
	if (td.isColumn("FLOAT_DATA")) {
		//cout << "Checking FLOAT_DATA column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "FLOAT_DATA") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("VIDEO_POINT")) {
		//cout << "Checking VIDEO_POINT column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "VIDEO_POINT") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("LAG_DATA")) {
		//cout << "Checking LAG_DATA column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "LAG_DATA") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("SIGMA")) {
		//cout << "Checking SIGMA column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "SIGMA") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("SIGMA_SPECTRUM")) {
		//cout << "Checking SIGMA_SPECTRUM column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "SIGMA_SPECTRUM") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("WEIGHT")) {
		//cout << "Checking WEIGHT column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "WEIGHT") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("WEIGHT_SPECTRUM")) {
		//cout << "Checking WEIGHT_SPECTRUM column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "WEIGHT_SPECTRUM") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("FLAG")) {
		//cout << "Checking FLAG column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "FLAG") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}

	if (td.isColumn("FLAG_CATEGORY")) {
		//cout << "Checking FLAG_CATEGORY column" << endl;
        for (uInt i=0; i<dminfo.nfields(); i++) {
            String type = dminfo.subRecord(i).asString("TYPE");
            Vector<String> cols = dminfo.subRecord(i).asArrayString ("COLUMNS");
            uInt n = 0;
            while (n < cols.nelements()) {
                if (cols(n) == "FLAG_CATEGORY") {
                    SMType = dminfo.subRecord(i).asString("TYPE");
                    break;
                }
                n++;
            }
        }
        if (SMType != "TiledShapeStMan") return False;
	}
	
	// Sanity check to make sure a TSM was found
	if (SMType != "TiledShapeStMan") return False;
	else return True;
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
  const ROArrayColumn<Complex>& otherData = otherMainCols.data();
  ArrayColumn<Complex>& thisData = data();
  const ROArrayColumn<Float>& otherSigma = otherMainCols.sigma();
  ArrayColumn<Float>& thisSigma = sigma();
  const ROArrayColumn<Float>& otherWeight = otherMainCols.weight();
  ArrayColumn<Float>& thisWeight = weight();
  const ROArrayColumn<Bool>& otherFlag = otherMainCols.flag();
  ArrayColumn<Bool>& thisFlag = flag();
  const ROArrayColumn<Bool>& otherFlagCat = otherMainCols.flagCategory();
  ArrayColumn<Bool>& thisFlagCat = flagCategory();
  const ROScalarColumn<Bool>& otherFlagRow = otherMainCols.flagRow();
  ScalarColumn<Bool>& thisFlagRow = flagRow();
  // This needs to be fixed when I relaxe the restriction that the input MS
  // must have been created using the uvfits filler.
  const Int curObsId =  observationId()(curRow-1) + 1;
  ScalarColumn<Int>& thisObsId = observationId();
  const ROArrayColumn<Float>& otherWeightSp = otherMainCols.weightSpectrum();
  ArrayColumn<Float>& thisWeightSp = weightSpectrum();
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
    thisObsId.put(curRow, curObsId);
    thisStateId.put(curRow, otherStateId, r);
    thisUvw.put(curRow, otherUvw, r);
    if(itsChanReversed[otherDDId(r)]){
      Vector<Int> datShape=otherData.shape(r).asVector();
      Matrix<Complex> reversedData(datShape[0], datShape[1]);
      for (Int k1=0; k1 < datShape[0]; ++k1){
	for(Int k2=0; k2 < datShape[1]; ++k2){
	  reversedData(k1,k2)=(Matrix<Complex>(otherData(r)))(k1,
							    datShape[1]-1-k2); 
	}
      } 
      thisData.put(curRow, reversedData);
    }
    else{
      thisData.put(curRow, otherData, r);
    }
    thisSigma.put(curRow, otherSigma, r);
    thisWeight.put(curRow, otherWeight, r);
    thisFlag.put(curRow, otherFlag, r);
    thisFlagCat.put(curRow, otherFlagCat, r);
    thisFlagRow.put(curRow, otherFlagRow, r);
    thisWeightSp.put(curRow, otherWeightSp, r);
  } 
}

void MSConcat::setTolerance(Quantum<Double>& freqTol, Quantum<Double>& dirTol){

  itsFreqTol=freqTol;
  itsDirTol=dirTol;
}

void MSConcat::checkShape(const IPosition& otherShape) const 
{
  LogIO os;
  Bool usesTSM = checkSM (itsMS);

  const uInt nAxes = min(itsFixedShape.nelements(), otherShape.nelements());
  DebugAssert(nAxes > 0 && nAxes < 4, AipsError);

  // First check and see if ms1 is using the TSM.  If so, we don't care
  // about the shape since the resulting concatenated MS will use TSM
  // by default.  If ms1 is NOT using the TiledShapeStMan, then we need to
  // make sure that ms1 and ms2 have the same shape.
  if (!usesTSM) {
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
  else {
	os << LogIO::WARN << "MS1 is using TiledShape Storage Manager."
	   "Ignoring shape." << LogIO::POST;
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
    *newPolPtr = polCols.match(corrPol);
    if (*newPolPtr < 0) {
      // need to add a new entry in the POLARIZATION subtable
      *newPolPtr= pol.nrow();
      pol.addRow();
      polRow.putMatchingFields(*newPolPtr, otherPolRow.get(otherPolId));
      // Again there cannot be an entry in the DATA_DESCRIPTION Table
      matchedDD = False;
    }

    if (matchedDD) {
      // We need to check if there exists an entry in the DATA_DESCRIPTION
      // table with the required spectral window and polarization index.
      ddMap[d] = ddIndex.getRowNumber(matchedDD);
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
// Local Variables: 
// compile-command: "gmake MSConcat"
// End: 
