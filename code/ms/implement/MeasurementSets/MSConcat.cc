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
#include <aips/MeasurementSets/NewMSDataDescColumns.h>
#include <aips/MeasurementSets/NewMSSpWindowColumns.h>
#include <aips/MeasurementSets/NewMSPolColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRow.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

MSConcat::MSConcat(NewMeasurementSet& ms):
  NewMSColumns(ms),
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
      if (dataColName == NewMS::columnName(NewMS::FLAG_CATEGORY) || 
	  dataColName == NewMS::columnName(NewMS::DATA) ||
	  dataColName == NewMS::columnName(NewMS::FLAG) || 
	  dataColName == NewMS::columnName(NewMS::SIGMA_SPECTRUM) ||
	  dataColName == NewMS::columnName(NewMS::WEIGHT_SPECTRUM) ||
	  dataColName == NewMS::columnName(NewMS::FLOAT_DATA) ||
	  dataColName == NewMS::columnName(NewMS::CORRECTED_DATA) || 
	  dataColName == NewMS::columnName(NewMS::MODEL_DATA) || 
	  dataColName == NewMS::columnName(NewMS::LAG_DATA) ||
	  dataColName == NewMS::columnName(NewMS::SIGMA) || 
	  dataColName == NewMS::columnName(NewMS::WEIGHT) || 
	  dataColName == NewMS::columnName(NewMS::IMAGING_WEIGHT) || 
	  dataColName == NewMS::columnName(NewMS::VIDEO_POINT)) {
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


void MSConcat::concatenate(const NewMeasurementSet& otherMS)
{
  RONewMSColumns otherCols(otherMS);
  if (otherMS.nrow() > 0) {
    if (itsFixedShape.nelements() > 0) {
      const uInt nShapes = itsMS.dataDescription().nrow();
      for (uInt s = 0; s < nShapes; s++) {
	checkShape(getShape(otherCols, s));
      }
    }
    checkCategories(otherCols);
  }
  const Block<uInt> newAntIndices = 
    copyAntenna(otherCols.antenna(), otherMS.antenna());
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

IPosition MSConcat::getShape(const RONewMSColumns& msCols, uInt whichShape)
{
  const RONewMSDataDescColumns& ddCol = msCols.dataDescription();
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

void MSConcat::checkCategories(const RONewMSColumns& otherCols) const {
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

Block<uInt> MSConcat::copyAntenna(const RONewMSAntennaColumns& otherCol,
				  const NewMSAntenna& otherTable) {
  Block<uInt> antMap(otherCol.nrow());
  NewMSAntennaColumns& antCols = antenna();
  const Quantum<Double> tol(1, "m");
  const ROTableRow otherRow(otherTable);
  TableRow antRow(itsMS.antenna());
  for (uInt a = 0; a < otherTable.nrow(); a++) {
    const Int newAnt = 
      antCols.matchAntenna(otherCol.name()(a), otherCol.positionMeas()(a), tol);
    if (newAnt < 0) {
      antMap[a] = itsMS.antenna().nrow();
      itsMS.antenna().addRow();
      antRow.putMatchingFields(antMap[a], otherRow.get(a));
    } else {
      antMap[a] = newAnt;
    }
  }
  return antMap;
}

// Local Variables: 
// compile-command: "gmake MSConcat"
// End: 
