//# MSConcat.cc: A class for concatenating MeasurementSets.
//# Copyright (C) 2000
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
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/MeasurementSets/MSDataDescColumns.h>
#include <aips/MeasurementSets/MSSpWindowColumns.h>
#include <aips/MeasurementSets/MSPolColumns.h>
#include <aips/MeasurementSets/MSFeed.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRow.h>
#include <aips/Tables/ColumnsIndex.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordField.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasConvert.h>

MSConcat::MSConcat(MeasurementSet& ms):
  MSColumns(ms),
  itsMS(ms),
  itsFixedShape(isFixedShape(ms.tableDesc()))
{
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
      << " to " << itsMS.tableName() << endl;
  ROMSColumns otherCols(otherMS);
  if (otherMS.nrow() > 0) {
    if (itsFixedShape.nelements() > 0) {
      const uInt nShapes = itsMS.dataDescription().nrow();
      for (uInt s = 0; s < nShapes; s++) {
	checkShape(getShape(otherCols, s));
      }
    }
    checkCategories(otherCols);
  }
  uInt oldRows = itsMS.antenna().nrow();
  const Block<uInt> newAntIndices = 
    copyAntennaAndFeed(otherMS.antenna(), otherMS.feed());
  {
    uInt addedRows = itsMS.antenna().nrow() - oldRows;
    uInt matchedRows = otherMS.antenna().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the antenna subtable" << endl;
  }
  oldRows = itsMS.field().nrow();
  const Block<uInt> newFldIndices = copyField(otherMS.field());
  {
    uInt addedRows = itsMS.field().nrow() - oldRows;
    uInt matchedRows = otherMS.field().nrow() - addedRows;
    log << "Added " << addedRows 
	<< " rows and matched " << matchedRows 
	<< " from the field subtable" << endl;
  }
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

IPosition MSConcat::getShape(const ROMSColumns& msCols, uInt whichShape)
{
  const ROMSDataDescColumns& ddCol = msCols.dataDescription();
  DebugAssert(whichShape < ddCol.nrow(), AipsError);
  const Int polId = ddCol.polarizationId()(whichShape);
  DebugAssert(polId >= 0 && 
	      polId < static_cast<Int>(msCols.polarization().nrow()),AipsError);
  const Int spwId = ddCol.spectralWindowId()(whichShape);
  DebugAssert(spwId >= 0 && 
	      spwId < static_cast<Int>(msCols.spectralWindow().nrow()),
	      AipsError);
  const Int nCorr = msCols.polarization().numCorr()(polId);
  DebugAssert(nCorr > 0, AipsError);
  const Int nChan = msCols.spectralWindow().numChan()(spwId);
  DebugAssert(nChan > 0, AipsError);
  return IPosition(2, nCorr, nChan);
}

void MSConcat::checkCategories(const ROMSColumns& otherCols) const {
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
  const Quantum<Double> tolerance(.1, "deg");
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
// Local Variables: 
// compile-command: "gmake MSConcat"
// End: 
