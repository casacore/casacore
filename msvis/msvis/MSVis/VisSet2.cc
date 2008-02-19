//# VisSet2.cc: VisSet.cc is split in 2 parts.
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2005
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
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

#include <msvis/MSVis/VisSet.h>
#include <msvis/MSVis/VisBuffer.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Exceptions/Error.h>
#include <casa/Containers/Record.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/TiledDataStMan.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <tables/Tables/TiledColumnStMan.h>
#include <tables/Tables/StandardStMan.h>
#include <tables/Tables/TiledDataStManAccessor.h>
#include <tables/Tables/TableIter.h>
#include <tables/Tables/CompressComplex.h>
#include <tables/Tables/CompressFloat.h>
#include <casa/Arrays/Slice.h>
#include <casa/Arrays/Slicer.h>
#include <casa/Utilities/GenSort.h>
#include <casa/iostream.h>

#include <casa/Logging/LogMessage.h>
#include <casa/Logging/LogSink.h>


namespace casa { //# NAMESPACE CASA - BEGIN

void VisSet::addCalSet(MeasurementSet& ms, Bool compress) {
  // Add a calibration set (comprising a set of CORRECTED_DATA, 
  // MODEL_DATA and IMAGING_WEIGHT columns) to the MeasurementSet.
  
  // Define a column accessor to the observed data
  ROTableColumn* data;
  if (ms.tableDesc().isColumn(MS::columnName(MS::FLOAT_DATA))) {
    data = new ROArrayColumn<Float> (ms, MS::columnName(MS::FLOAT_DATA));
  } else {
    data = new ROArrayColumn<Complex> (ms, MS::columnName(MS::DATA));
  };

  // Check if the data column is tiled and, if so, the
  // smallest tile shape used.
  TableDesc td = ms.actualTableDesc();
  const ColumnDesc& cdesc = td[data->columnDesc().name()];
  String dataManType = cdesc.dataManagerType();
  String dataManGroup = cdesc.dataManagerGroup();
  IPosition dataTileShape;
  Bool tiled = (dataManType.contains("Tiled"));
  Bool simpleTiling = False;

  if (tiled) {
    ROTiledStManAccessor tsm(ms, dataManGroup);
    uInt nHyper = tsm.nhypercubes();
    // Find smallest tile shape
    Int lowestProduct = 0;
    Int lowestId = 0;
    Bool firstFound = False;
    for (uInt id=0; id < nHyper; id++) {
      Int product = tsm.getTileShape(id).product();
      if (product > 0 && (!firstFound || product < lowestProduct)) {
	lowestProduct = product;
	lowestId = id;
	if (!firstFound) firstFound = True;
      };
    };
    dataTileShape = tsm.getTileShape(lowestId);
    simpleTiling = (dataTileShape.nelements() == 3);
  };

  if (!tiled || !simpleTiling) {
    // Untiled, or tiled at a higher than expected dimensionality
    // Use a canonical tile shape of 128 kB size
    cout << "VisSet: untiled or not simple tiling" << endl;

    Int maxNchan = max (numberChan());
    Int tileSize = maxNchan/10 + 1;
    Int nCorr = data->shape(0)(0);
    dataTileShape = IPosition(3, nCorr, tileSize, 16384/nCorr/tileSize);
  };
  
  // Add the MODEL_DATA column
  TableDesc tdModel, tdModelComp, tdModelScale;
  CompressComplex* ccModel=NULL;
  String colModel=MS::columnName(MS::MODEL_DATA);

  tdModel.addColumn(ArrayColumnDesc<Complex>(colModel,"model data", 2));
  IPosition modelTileShape = dataTileShape;
  if (compress) {
    tdModelComp.addColumn(ArrayColumnDesc<Int>(colModel+"_COMPRESSED",
					       "model data compressed",2));
    tdModelScale.addColumn(ScalarColumnDesc<Float>(colModel+"_SCALE"));
    tdModelScale.addColumn(ScalarColumnDesc<Float>(colModel+"_OFFSET"));
    ccModel = new CompressComplex(colModel, colModel+"_COMPRESSED",
				  colModel+"_SCALE", colModel+"_OFFSET", True);

    StandardStMan modelScaleStMan("ModelScaleOffset");
    ms.addColumn(tdModelScale, modelScaleStMan);

    TiledShapeStMan modelCompStMan("", modelTileShape);
    ms.addColumn(tdModelComp, modelCompStMan);
    ms.addColumn(tdModel, *ccModel);

  } else {
    TiledShapeStMan modelStMan("", modelTileShape);
    ms.addColumn(tdModel, modelStMan);
  };

  // Add the CORRECTED_DATA column
  TableDesc tdCorr, tdCorrComp, tdCorrScale;
  CompressComplex* ccCorr=NULL;
  String colCorr=MS::columnName(MS::CORRECTED_DATA);

  tdCorr.addColumn(ArrayColumnDesc<Complex>(colCorr,"corrected data", 2));
  IPosition corrTileShape = dataTileShape;
  if (compress) {
    tdCorrComp.addColumn(ArrayColumnDesc<Int>(colCorr+"_COMPRESSED",
					      "corrected data compressed",2));
    tdCorrScale.addColumn(ScalarColumnDesc<Float>(colCorr+"_SCALE"));
    tdCorrScale.addColumn(ScalarColumnDesc<Float>(colCorr+"_OFFSET"));
    ccCorr = new CompressComplex(colCorr, colCorr+"_COMPRESSED",
				 colCorr+"_SCALE", colCorr+"_OFFSET", True);

    StandardStMan corrScaleStMan("CorrScaleOffset");
    ms.addColumn(tdCorrScale, corrScaleStMan);

    TiledShapeStMan corrCompStMan("", corrTileShape);
    ms.addColumn(tdCorrComp, corrCompStMan);
    ms.addColumn(tdCorr, *ccCorr);

  } else {
    TiledShapeStMan corrStMan("", corrTileShape);
    ms.addColumn(tdCorr, corrStMan);
  };

  // Add the IMAGING_WEIGHT column
  TableDesc tdImWgt, tdImWgtComp, tdImWgtScale;
  CompressFloat* ccImWgt=NULL;
  String colImWgt=MS::columnName(MS::IMAGING_WEIGHT);

  tdImWgt.addColumn(ArrayColumnDesc<Float>(colImWgt,"imaging weight", 1));
  IPosition imwgtTileShape = dataTileShape.getLast(2);
  if (compress) {
    tdImWgtComp.addColumn(ArrayColumnDesc<Short>(colImWgt+"_COMPRESSED",
						 "imaging weight compressed",
						 1));
    tdImWgtScale.addColumn(ScalarColumnDesc<Float>(colImWgt+"_SCALE"));
    tdImWgtScale.addColumn(ScalarColumnDesc<Float>(colImWgt+"_OFFSET"));
    ccImWgt = new CompressFloat(colImWgt, colImWgt+"_COMPRESSED",
				colImWgt+"_SCALE", colImWgt+"_OFFSET", True);

    StandardStMan imwgtScaleStMan("ImWgtScaleOffset");
    ms.addColumn(tdImWgtScale, imwgtScaleStMan);

    TiledShapeStMan imwgtCompStMan("", imwgtTileShape);
    ms.addColumn(tdImWgtComp, imwgtCompStMan);
    ms.addColumn(tdImWgt, *ccImWgt);

  } else {
    TiledShapeStMan imwgtStMan("", imwgtTileShape);
    ms.addColumn(tdImWgt, imwgtStMan);
  };

  if (ccModel) delete ccModel;
  if (ccCorr) delete ccCorr;
  if (ccImWgt) delete ccImWgt;

  // Set the shapes for each row
  ArrayColumn<Complex> modelData(ms, "MODEL_DATA");
  ArrayColumn<Complex> correctedData(ms, "CORRECTED_DATA");
  ArrayColumn<Float> imagingWeight(ms, "IMAGING_WEIGHT");
  for (uInt row=0; row < ms.nrow(); row++) {
    IPosition rowShape=data->shape(row);
    modelData.setShape(row,rowShape);
    correctedData.setShape(row,rowShape);
    imagingWeight.setShape(row,rowShape.getLast(1));
  };
  delete data;
}


} //# NAMESPACE CASA - END

