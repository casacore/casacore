//# VisSet.cc: Implementation of VisSet
//# Copyright (C) 1996,1997,1998
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

#include <trial/MeasurementEquations/VisSet.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Containers/Record.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledDataStManAccessor.h>
#include <aips/Tables/TableIter.h>
#include <aips/Lattices/Slice.h>
#include <aips/Lattices/Slicer.h>
#include <iostream.h>

#include <aips/Logging/LogMessage.h>
#include <aips/Logging/LogSink.h>


VisSet::VisSet(MeasurementSet& ms,const Block<Int>& columns, 
	       const Matrix<Int>& chanSelection, Double timeInterval)
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
	    allEQ(selection_p.ac(),storedSelection.ac())) {
	  init=False;
	} 
      }
    }
    if (init) {
      message.message("Initializing MODEL_DATA, CORRECTED_DATA and IMAGING_WEIGHT columns");

      logSink.post(message);
      removeColumns(ms);
      addColumns(ms);
      ArrayColumn<Complex> mcd(ms,"MODEL_DATA");
      mcd.rwKeywordSet().define("CHANNEL_SELECTION",selection_p);
    }

    iter_p=VisIter(ms_p,columns,timeInterval);
    for (uInt spw=0; spw<selection_p.ncolumn(); spw++) {
      iter_p.selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }
}

VisSet::VisSet(const VisSet& vs,const Block<Int>& columns, 
	       Double timeInterval)
{
    ms_p=vs.ms_p;
    selection_p.resize(vs.selection_p.shape());
    selection_p=vs.selection_p;

    iter_p=VisIter(ms_p,columns,timeInterval);
    for (uInt spw=0; spw<selection_p.ncolumn(); spw++) {
      iter_p.selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }
}

VisSet& VisSet::operator=(const VisSet& other) 
{
    ms_p=other.ms_p;
    selection_p.resize(other.selection_p.shape());
    selection_p=other.selection_p;
    iter_p=other.iter_p;
    return *this;
}

VisSet::~VisSet() {
  ms_p.flush();
};

void VisSet::flush() {
  ms_p.flush();
};

VisIter& VisSet::iter() { return iter_p; }

// Set or reset the channel selection on all iterators
void VisSet::selectChannel(Int nGroup,Int start, Int width, Int increment, 
			   Int spectralWindow)
{
  iter_p.selectChannel(nGroup,start,width,increment,spectralWindow); 
  iter_p.origin();
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

void VisSet::removeColumns(Table& tab) 
{
  if (tab.tableDesc().isColumn("MODEL_DATA"))
    tab.removeColumn("MODEL_DATA");
  if (tab.tableDesc().isColumn("CORRECTED_DATA")) 
    tab.removeColumn("CORRECTED_DATA");
  if (tab.tableDesc().isColumn("IMAGING_WEIGHT")) 
    tab.removeColumn("IMAGING_WEIGHT");
}

// add the model and corrected data columns and the imaging weight
void VisSet::addColumns(Table& tab) 
{		    

  // Look for the DATA column among the hypercolumns
  // to find the corresponding id column (if any)
  TableDesc td(tab.tableDesc());
  Vector<String> hypercolumnNames=td.hypercolumnNames();
  Bool found=False;
  String dataTileId="";
  if (hypercolumnNames.nelements()>0) {
    for (uInt i=0; i<hypercolumnNames.nelements(); i++) {
      Vector<String> dataColNames,coordColNames,idColNames;
      td.hypercolumnDesc(hypercolumnNames(i),
			 dataColNames,coordColNames,
			 idColNames);
      for (uInt j=0; j<dataColNames.nelements(); j++) {
	if (dataColNames(j)==MS::columnName(MS::DATA)) {
	  found=ToBool(idColNames.nelements()>0);
	  if (found) dataTileId=idColNames(0);
	}
      }
    }
  }

  Vector<String> coordColNames(0), idColNames(1);
  TableDesc td1;
  IPosition shape,shapeWt;
  if (!found) {
    ArrayColumn<Complex> data(tab,MS::columnName(MS::DATA));
    Int numCorr=data.shape(0)(0);
    Int numChan=selection_p(1,0);
    shape=IPosition(2,numCorr,numChan);
    shapeWt=IPosition(1,numChan);
  }
  if (found) {
    idColNames(0)="MODEL_TILE_ID"; 
    td1.addColumn(ArrayColumnDesc<Complex>("MODEL_DATA","model data",2));
    td1.addColumn(ScalarColumnDesc<Int>("MODEL_TILE_ID","tile index"));
  } else {
    idColNames.resize(0);
    td1.addColumn(ArrayColumnDesc<Complex>("MODEL_DATA","model data",shape,
					   ColumnDesc::Direct));
  }    
  td1.defineHypercolumn("TiledData-model",3,
			stringToVector("MODEL_DATA"),coordColNames,
			idColNames);
  TableDesc td2;
  if (found) {
    idColNames(0)="CORRECTED_TILE_ID"; 
    td2.addColumn(ArrayColumnDesc<Complex>("CORRECTED_DATA","corrected data",2));
    td2.addColumn(ScalarColumnDesc<Int>("CORRECTED_TILE_ID","tile index"));
  } else {
    td2.addColumn(ArrayColumnDesc<Complex>("CORRECTED_DATA","corrected data",
					   shape,ColumnDesc::Direct));
  }
  td2.defineHypercolumn("TiledData-corrected",3,
			stringToVector("CORRECTED_DATA"),coordColNames,
			idColNames);
  TableDesc td3;
  if (found) {
    idColNames(0)="IMAGING_WT_TILE_ID"; 
    td3.addColumn(ArrayColumnDesc<Float>("IMAGING_WEIGHT","imaging weight",1));
    td3.addColumn(ScalarColumnDesc<Int>("IMAGING_WT_TILE_ID","tile index"));
  } else {
    td3.addColumn(ArrayColumnDesc<Float>("IMAGING_WEIGHT","imaging weight",
					 shapeWt,ColumnDesc::Direct));
  }
  td3.defineHypercolumn("TiledImagingWeight",2,
		       stringToVector("IMAGING_WEIGHT"),
		       coordColNames,
		       idColNames);

  Bool tiledData=False;

  // If there's no id, assume the data is fixed shape throughout
  if (found) {
    // data shape may change
    TiledDataStMan tiledStMan1("TiledData-model");
    tab.addColumn(td1,tiledStMan1);
    TiledDataStMan tiledStMan2("TiledData-corrected");
    tab.addColumn(td2,tiledStMan2);
    TiledDataStMan tiledStMan3("TiledImagingWeight");
    tab.addColumn(td3,tiledStMan3);
    tiledData=True;
    TiledDataStManAccessor modelDataAccessor(tab,"TiledData-model");
    TiledDataStManAccessor corrDataAccessor(tab,"TiledData-corrected");
    TiledDataStManAccessor imWtAccessor(tab,"TiledImagingWeight");
    TableIterator obsIter(tab,dataTileId);
    for (;!obsIter.pastEnd(); obsIter.next()) {
      ScalarColumn<Int> spwId(obsIter.table(),
			      MS::columnName(MS::SPECTRAL_WINDOW_ID));
      ScalarColumn<Int> tileId(obsIter.table(),dataTileId);
      ArrayColumn<Complex> od(obsIter.table(),MS::columnName(MS::DATA));
      Int spw=spwId(0);
      Int numCorr=od.shape(0)(0);
      Int numChan=selection_p(1,spw);
      // add new hyperCube
      Record values1; values1.define("MODEL_TILE_ID",tileId(0));
      Record values2; values2.define("CORRECTED_TILE_ID",tileId(0));
      Record values3; values3.define("IMAGING_WT_TILE_ID",tileId(0));
      //      Int tileSize=(numChan+numChan/10)/(numChan/10+1);
      // set the tileSize in the channel direction so that we
      // read about 10% for single channel access; we also waste less than
      // 10% at the end of the spectrum. Set the total size to 132k for data.
      Int tileSize=numChan/10+1;
      IPosition cubeShape(3,numCorr,numChan,obsIter.table().nrow());
      IPosition tileShape(3,numCorr,tileSize,16384/numCorr/tileSize);
      IPosition cubeShapeWt(2,numChan,obsIter.table().nrow());
      IPosition tileShapeWt(2,tileSize,16384/tileSize);
      modelDataAccessor.addHypercube(cubeShape,tileShape,values1);
      corrDataAccessor.addHypercube(cubeShape,tileShape,values2);
      imWtAccessor.addHypercube(cubeShapeWt,tileShapeWt,values3);
    }
  } else {
    // fixed data shape
    ArrayColumn<Complex> data(tab,MS::columnName(MS::DATA));
    Int numCorr=data.shape(0)(0);
    Int numChan=selection_p(1,0);
    Int tileSize=numChan/10+1;
    IPosition tileShape(3,numCorr,tileSize,16384/numCorr/tileSize);
    TiledColumnStMan tiledStMan1("TiledData-model",tileShape);
    tab.addColumn(td1,tiledStMan1);
    TiledColumnStMan tiledStMan2("TiledData-corrected",tileShape);
    tab.addColumn(td2,tiledStMan2);
    IPosition tileShapeWt(2,tileSize,16384/tileSize);
    TiledColumnStMan tiledStMan3("TiledImagingWeight",tileShapeWt);
    tab.addColumn(td3,tiledStMan3);
  }
}
