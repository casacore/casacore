//# VisSet.cc: Implementation of VisSet
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

VisSet::VisSet(MeasurementSet& ms,const Block<Int>& columns, 
	       const Matrix<Int>& chanSelection, Double timeInterval,
	       Bool compress)
:ms_p(ms)
{
    LogSink logSink;
    LogMessage message(LogOrigin("VisSet","VisSet"));


    // sort out the channel selection
    Int nSpw=ms_p.spectralWindow().nrow();
    MSSpWindowColumns msSpW(ms_p.spectralWindow());
    selection_p.resize(2,nSpw);
    // fill in default selection
    selection_p.row(0)=0; //start
    selection_p.row(1)=msSpW.numChan().getColumn(); 
    for (uInt i=0; i<chanSelection.ncolumn(); i++) {
      Int spw=chanSelection(2,i);
      if (spw>=0 && spw<nSpw && chanSelection(0,i)>=0 && 
	  chanSelection(0,i)+chanSelection(1,i)<=selection_p(1,spw)) {
	// looks like a valid selection, implement it
	selection_p(0,spw)=chanSelection(0,i);
	selection_p(1,spw)=chanSelection(1,i);
      }
    }

    Bool init=True;
    if (ms.tableDesc().isColumn("MODEL_DATA")) {
      TableColumn col(ms,"MODEL_DATA");
      if (col.keywordSet().isDefined("CHANNEL_SELECTION")) {
	Matrix<Int> storedSelection;
	col.keywordSet().get("CHANNEL_SELECTION",storedSelection);
	if (selection_p.shape()==storedSelection.shape() && 
	    allEQ(selection_p,storedSelection)) {
	  init=False;
	} 
      }
    }

    // Add scratch columns
    if (init) {
      message.message("Adding MODEL_DATA, CORRECTED_DATA and IMAGING_WEIGHT columns");
      logSink.post(message);

      removeCalSet(ms);
      addCalSet(ms, compress);
      
      ArrayColumn<Complex> mcd(ms,"MODEL_DATA");
      mcd.rwKeywordSet().define("CHANNEL_SELECTION",selection_p);

      // Force re-sort (in VisIter ctor below) by deleting current sort info 
      if (ms.keywordSet().isDefined("SORT_COLUMNS")) 
	ms.rwKeywordSet().removeField("SORT_COLUMNS");
      if (ms.keywordSet().isDefined("SORTED_TABLE")) 
	  ms.rwKeywordSet().removeField("SORTED_TABLE");
    }


    iter_p=new VisIter(ms_p,columns,timeInterval);
    for (uInt spw=0; spw<selection_p.ncolumn(); spw++) {
      iter_p->selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }

    // Initialize MODEL_DATA and CORRECTED_DATA
    if (init) initCalSet(0);

}



VisSet::VisSet(MeasurementSet& ms, const Matrix<Int>& chanSelection, 
	       Double timeInterval)
:ms_p(ms)
{
    LogSink logSink;
    LogMessage message(LogOrigin("VisSet","VisSet"));


    // sort out the channel selection
    Int nSpw=ms_p.spectralWindow().nrow();
    MSSpWindowColumns msSpW(ms_p.spectralWindow());
    selection_p.resize(2,nSpw);
    // fill in default selection
    selection_p.row(0)=0; //start
    selection_p.row(1)=msSpW.numChan().getColumn(); 
    for (uInt i=0; i<chanSelection.ncolumn(); i++) {
      Int spw=chanSelection(2,i);
      if (spw>=0 && spw<nSpw && chanSelection(0,i)>=0 && 
	  chanSelection(0,i)+chanSelection(1,i)<=selection_p(1,spw)) {
	// looks like a valid selection, implement it
	selection_p(0,spw)=chanSelection(0,i);
	selection_p(1,spw)=chanSelection(1,i);
      }
    }

    

    Block<Int> columns(0);

    iter_p=new VisIter(ms_p,columns,timeInterval);
    for (uInt spw=0; spw<selection_p.ncolumn(); spw++) {
      iter_p->selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }


}

VisSet::VisSet(const VisSet& vs,const Block<Int>& columns, 
	       Double timeInterval)
{
    ms_p=vs.ms_p;
    selection_p.resize(vs.selection_p.shape());
    selection_p=vs.selection_p;

    iter_p=new VisIter(ms_p,columns,timeInterval);
    for (uInt spw=0; spw<selection_p.ncolumn(); spw++) {
      iter_p->selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }
}

VisSet& VisSet::operator=(const VisSet& other) 
{
    if (this == &other) return *this;
    ms_p=other.ms_p;
    selection_p.resize(other.selection_p.shape());
    selection_p=other.selection_p;
    *iter_p=*(other.iter_p);
    return *this;
}

VisSet::~VisSet() {
  ms_p.flush();
  delete iter_p; iter_p=0;
};


void VisSet::initCalSet(Int calSet)
{

  LogSink logSink;
  LogMessage message(LogOrigin("VisSet","VisSet"));

  message.message("Initializing MODEL_DATA (to unity) and CORRECTED_DATA (to DATA)");
  logSink.post(message);

  Vector<Int> lastCorrType;
  Vector<Bool> zero;
  for (iter_p->originChunks(); iter_p->moreChunks(); iter_p->nextChunk()) {
    // figure out which correlations to set to 1. and 0. for the model.
    Vector<Int> corrType; iter_p->corrType(corrType);
    uInt nCorr = corrType.nelements();
    if (nCorr!=lastCorrType.nelements() ||
	!allEQ(corrType,lastCorrType)) {
      lastCorrType.resize(nCorr); lastCorrType=corrType;
      zero.resize(nCorr);
      for (uInt i=0; i<nCorr; i++) {
	zero[i]=(corrType[i]==Stokes::RL || corrType[i]==Stokes::LR ||
		 corrType[i]==Stokes::XY || corrType[i]==Stokes::YX);
      }
    }
    for (iter_p->origin(); iter_p->more(); (*iter_p)++) {
      Cube<Complex> data;
      iter_p->setVis(iter_p->visibility(data,VisibilityIterator::Observed),
		     VisibilityIterator::Corrected);
      data=Complex(1.0,0.0);
      for (uInt i=0; i<nCorr; i++) {
	if (zero[i]) data(Slice(i),Slice(),Slice())=Complex(0.0,0.0);
      }
      iter_p->setVis(data,VisibilityIterator::Model);
    };
  };
  flush();
  iter_p->originChunks();
}

void VisSet::flush() {
  ms_p.flush();
};

VisIter& VisSet::iter() { return *iter_p; }

// Set or reset the channel selection on all iterators
void VisSet::selectChannel(Int nGroup,Int start, Int width, Int increment, 
			   Int spectralWindow)
{
  iter_p->selectChannel(nGroup,start,width,increment,spectralWindow); 
  iter_p->origin();

  selection_p(0,spectralWindow) = start;
  selection_p(1,spectralWindow) = width;

}
Int VisSet::numberAnt() const 
{
  return ((MeasurementSet&)ms_p).antenna().nrow(); // for single (sub)array only..
}
Int VisSet::numberSpw() const 
{
  return ((MeasurementSet&)ms_p).spectralWindow().nrow(); 
}
Vector<Int> VisSet::startChan() const
{
  return selection_p.row(0);
}
Vector<Int> VisSet::numberChan() const 
{
  return selection_p.row(1); 
}
Int VisSet::numberCoh() const 
{
  return ms_p.nrow();
}

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
  String dataManType = data->columnDesc().dataManagerType();
  String dataManGroup = data->columnDesc().dataManagerGroup();
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
  Vector<String> coordColNames(0), idColNames(0);
  TableDesc tdModel, tdModelComp, tdModelScale;
  CompressComplex* ccModel=NULL;
  String colModel=MS::columnName(MS::MODEL_DATA);

  tdModel.addColumn(ArrayColumnDesc<Complex>(colModel,"model data", 2));
  IPosition modelTileShape = dataTileShape;
  if (compress) {
    tdModelComp.addColumn(ArrayColumnDesc<Int>(colModel+"_COMPRESSED",
					       "model data compressed",2));
    tdModelComp.defineHypercolumn("TiledShape-ModelComp",3,
				  stringToVector(colModel+"_COMPRESSED"),
				  coordColNames,idColNames);
    tdModelScale.addColumn(ScalarColumnDesc<Float>(colModel+"_SCALE"));
    tdModelScale.addColumn(ScalarColumnDesc<Float>(colModel+"_OFFSET"));
    ccModel = new CompressComplex(colModel, colModel+"_COMPRESSED",
				  colModel+"_SCALE", colModel+"_OFFSET", True);

    StandardStMan modelScaleStMan("ModelScaleOffset");
    ms.addColumn(tdModelScale, modelScaleStMan);

    TiledShapeStMan modelCompStMan("TiledShape-ModelComp", modelTileShape);
    ms.addColumn(tdModelComp, modelCompStMan);
    ms.addColumn(tdModel, *ccModel);

  } else {
    tdModel.defineHypercolumn("TiledShape-Model",3,
			      stringToVector(colModel), coordColNames,
			      idColNames);
    TiledShapeStMan modelStMan("TiledShape-Model", modelTileShape);
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
    tdCorrComp.defineHypercolumn("TiledShape-CorrComp",3,
				 stringToVector(colCorr+"_COMPRESSED"),
				 coordColNames,idColNames);
    tdCorrScale.addColumn(ScalarColumnDesc<Float>(colCorr+"_SCALE"));
    tdCorrScale.addColumn(ScalarColumnDesc<Float>(colCorr+"_OFFSET"));
    ccCorr = new CompressComplex(colCorr, colCorr+"_COMPRESSED",
				 colCorr+"_SCALE", colCorr+"_OFFSET", True);

    StandardStMan corrScaleStMan("CorrScaleOffset");
    ms.addColumn(tdCorrScale, corrScaleStMan);

    TiledShapeStMan corrCompStMan("TiledShape-CorrComp", corrTileShape);
    ms.addColumn(tdCorrComp, corrCompStMan);
    ms.addColumn(tdCorr, *ccCorr);

  } else {
    tdCorr.defineHypercolumn("TiledShape-Corr",3,
			     stringToVector(colCorr), coordColNames,
			     idColNames);
    TiledShapeStMan corrStMan("TiledShape-Corr", corrTileShape);
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
    tdImWgtComp.defineHypercolumn("TiledShape-ImWgtComp",2,
				  stringToVector(colImWgt+"_COMPRESSED"),
				  coordColNames,idColNames);
    tdImWgtScale.addColumn(ScalarColumnDesc<Float>(colImWgt+"_SCALE"));
    tdImWgtScale.addColumn(ScalarColumnDesc<Float>(colImWgt+"_OFFSET"));
    ccImWgt = new CompressFloat(colImWgt, colImWgt+"_COMPRESSED",
				colImWgt+"_SCALE", colImWgt+"_OFFSET", True);

    StandardStMan imwgtScaleStMan("ImWgtScaleOffset");
    ms.addColumn(tdImWgtScale, imwgtScaleStMan);

    TiledShapeStMan imwgtCompStMan("TiledShape-ImWgtComp", imwgtTileShape);
    ms.addColumn(tdImWgtComp, imwgtCompStMan);
    ms.addColumn(tdImWgt, *ccImWgt);

  } else {
    tdImWgt.defineHypercolumn("TiledShape-ImWgt",2,
			      stringToVector(colImWgt), coordColNames,
			      idColNames);
    TiledShapeStMan imwgtStMan("TiledShape-ImWgt", imwgtTileShape);
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

void VisSet::removeCalSet(MeasurementSet& ms) {
  // Remove an existing calibration set (comprising a set of CORRECTED_DATA, 
  // MODEL_DATA and IMAGING_WEIGHT columns) from the MeasurementSet.

  Vector<String> colNames(3);
  colNames(0)=MS::columnName(MS::MODEL_DATA);
  colNames(1)=MS::columnName(MS::CORRECTED_DATA);
  colNames(2)=MS::columnName(MS::IMAGING_WEIGHT);

  for (uInt j=0; j<colNames.nelements(); j++) {
    if (ms.tableDesc().isColumn(colNames(j))) {
      ms.removeColumn(colNames(j));
    };
    if (ms.tableDesc().isColumn(colNames(j)+"_COMPRESSED")) {
      ms.removeColumn(colNames(j)+"_COMPRESSED");
    };
    if (ms.tableDesc().isColumn(colNames(j)+"_SCALE")) {
      ms.removeColumn(colNames(j)+"_SCALE");
    };
    if (ms.tableDesc().isColumn(colNames(j)+"_OFFSET")) {
      ms.removeColumn(colNames(j)+"_OFFSET");
    };
  };
}


} //# NAMESPACE CASA - END

