//# VisSet.cc: Implementation of VisSet
//# Copyright (C) 1996
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
#include <aips/Tables/ForwardCol.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledDataStManAccessor.h>
#include <aips/Lattices/Slice.h>
#include <aips/Lattices/Slicer.h>
#include <iostream.h>

#include <aips/Logging/LogMessage.h>
#include <aips/Logging/LogSink.h>


VisSet::VisSet(const MeasurementSet& ms,const Block<Int>& columns, 
	       const Matrix<Int>& chanSelection, Double timeInterval)
:obsCoh_p(ms)
{
    LogSink logSink;
    LogMessage message(LogOrigin("VisSet","VisSet"));

    obsCoh_p.tableInfo().setSubType("observed");

    Block<String> refColumns(1);
    refColumns[0]=MS::columnName(MS::DATA);

    // sort out the channel selection
    Int nSpw=obsCoh_p.spectralWindow().nrow();
    MSSpWindowColumns msSpW(obsCoh_p.spectralWindow());
    selection_p.resize(2,nSpw);
    // fill in default selection
    selection_p.row(0)=0; //start
    selection_p.row(1)=msSpW.numChan().getColumn(); 
    for (Int i=0; i<chanSelection.ncolumn(); i++) {
      Int spw=chanSelection(2,i);
      if (spw>=0 && spw<nSpw && chanSelection(0,i)>=0 && 
	  chanSelection(0,i)+chanSelection(1,i)<=selection_p(1,spw)) {
	// looks like a valid selection, implement it
	selection_p(0,spw)=chanSelection(0,i);
	selection_p(1,spw)=chanSelection(1,i);
      }
    }

    Bool initModel=True;
    if(Table::isReadable(ms.tableName()+"-model")) {
      Table mms=Table(ms.tableName()+"-model",Table::Update);
      mms.tableInfo().setSubType("model");
      TableColumn col(mms,MS::columnName(MS::DATA));
      if (col.keywordSet().isDefined("CHANNEL_SELECTION")) {
	Matrix<Int> storedSelection;
	col.keywordSet().get("CHANNEL_SELECTION",storedSelection);
	if (selection_p.shape()==storedSelection.shape() && 
	    allEQ(selection_p.ac(),storedSelection.ac())) {
	  initModel=False;
	  modelCoh_p=mms;
	}
      }
    }
    if (initModel) {
      Vector<String> extraWritableColumns;
      modelCoh_p=referenceCopy(obsCoh_p,"-model",extraWritableColumns,"zero");
      modelCoh_p.tableInfo().setSubType("model");
      ArrayColumn<Complex> mcd(modelCoh_p,MS::columnName(MS::DATA));
      mcd.keywordSet().define("CHANNEL_SELECTION",selection_p);
    };

    Bool initCorr=True;
    if(Table::isReadable(ms.tableName()+"-corrected")) {
      Table cms=Table(ms.tableName()+"-corrected",Table::Update);
      TableColumn col(cms,MS::columnName(MS::DATA));
      if (col.keywordSet().isDefined("CHANNEL_SELECTION")) {
	Matrix<Int> storedSelection;
	col.keywordSet().get("CHANNEL_SELECTION",storedSelection);
	if (selection_p.shape()==storedSelection.shape() && 
	    allEQ(selection_p.ac(),storedSelection.ac())) {
	  initCorr=False;
	  corrCoh_p=cms;
	}
      }
    }
    if (initCorr) {
      initCorr=True;
      // make a new weight column for corrected MS, so we can use it
      // for imaging weights without writing to the original MS
      Vector<String> extraWritableColumns(1);
      extraWritableColumns(0)=MS::columnName(MS::WEIGHT);
      corrCoh_p=referenceCopy(obsCoh_p,"-corrected", extraWritableColumns,"copy");
      corrCoh_p.tableInfo().setSubType("corrected");
      ArrayColumn<Complex> ccd(corrCoh_p,MS::columnName(MS::DATA));
      ccd.keywordSet().define("CHANNEL_SELECTION",selection_p);

      // Copy the weights from observed to corrected
      ScalarColumn<Float> oWt(obsCoh_p,MS::columnName(MS::WEIGHT));
      ScalarColumn<Float> cWt(corrCoh_p,MS::columnName(MS::WEIGHT));
      cWt.putColumn(oWt.getColumn());
    };

    obsIter_p=VisIter(obsCoh_p,columns,timeInterval);
    for (Int spw=0; spw<selection_p.ncolumn(); spw++) {
      obsIter_p.selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }
    corrIter_p=VisIter(corrCoh_p,columns,timeInterval);
    modelIter_p=VisIter(modelCoh_p,columns,timeInterval);
}

VisSet::VisSet(const VisSet& vs,const Block<Int>& columns, 
	       Double timeInterval)
{
    obsCoh_p=vs.obsCoh_p;
    corrCoh_p=vs.corrCoh_p;
    modelCoh_p=vs.modelCoh_p;
    selection_p.resize(vs.selection_p.shape());
    selection_p=vs.selection_p;

    obsIter_p=VisIter(obsCoh_p,columns,timeInterval);
    for (Int spw=0; spw<selection_p.ncolumn(); spw++) {
      obsIter_p.selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
    }
    corrIter_p=VisIter(corrCoh_p,columns,timeInterval);
    modelIter_p=VisIter(modelCoh_p,columns,timeInterval);
}

VisSet& VisSet::operator=(const VisSet& other) 
{
    obsCoh_p=other.obsCoh_p;
    corrCoh_p=other.corrCoh_p;
    modelCoh_p=other.modelCoh_p;
    selection_p.resize(other.selection_p.shape());
    selection_p=other.selection_p;
    obsIter_p=other.obsIter_p;
    corrIter_p=other.corrIter_p;
    modelIter_p=other.modelIter_p;
    return *this;
}

VisSet::~VisSet() {
  obsCoh_p.flush();
  corrCoh_p.flush();
  modelCoh_p.flush();
};

void VisSet::flush() {
  obsCoh_p.flush();
  corrCoh_p.flush();
  modelCoh_p.flush();
};

VisIter& VisSet::observedCoherence() { return obsIter_p; }
VisIter& VisSet::correctedCoherence() { return corrIter_p; }
VisIter& VisSet::modelCoherence() { return modelIter_p; }

// Set or reset the channel selection on all iterators
void VisSet::selectChannel(Int nGroup,Int start, Int width, Int increment, 
			   Int spectralWindow)
{
  obsIter_p.selectChannel(nGroup,start,width,increment,spectralWindow); 
  obsIter_p.origin();
  corrIter_p.selectChannel(nGroup,start,width,increment,spectralWindow); 
  corrIter_p.origin();
  modelIter_p.selectChannel(nGroup,start,width,increment,spectralWindow); 
  modelIter_p.origin();
}
Int VisSet::numberAnt() const 
{
  return ((MeasurementSet&)obsCoh_p).antenna().nrow(); // for single (sub)array only..
}
Int VisSet::numberSpw() const 
{
  return ((MeasurementSet&)obsCoh_p).spectralWindow().nrow(); 
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
  return obsCoh_p.nrow();
}

// make a reference copy of the MS table, with the exception of
// the DATA column and the 'extraWritableColumns'. 
// We want a new writable data column with the same tile index column (if any).
// Zero fills the new data column if fill=="zero", copies the data from
// the input if fill=="copy".
Table VisSet::referenceCopy(const Table& tab, const String& extension, 
			    const Vector<String>& extraWritableColumns,
			    const String& fill)
{		    

  // Look for the DATA column among the hypercolumns
  // to find the corresponding id column (if any)
  String tableName=tab.tableName()+extension;
  TableDesc td(tab.tableDesc());
  Vector<String> hypercolumnNames=td.hypercolumnNames();
  Bool found=False;
  String dataTileId="";
  if (hypercolumnNames.nelements()>0) {
    for (Int i=0; i<hypercolumnNames.nelements(); i++) {
      Vector<String> dataColNames,coordColNames,idColNames;
      td.hypercolumnDesc(hypercolumnNames(i),
			 dataColNames,coordColNames,
			 idColNames);
      for (Int j=0; j<dataColNames.nelements(); j++) {
	if (dataColNames(j)==MS::columnName(MS::DATA)) {
	  found=True;
	  if (idColNames.nelements()>0) dataTileId=idColNames(0);
	}
      }
    }
  }

  Vector<String> coordColNames(0), idColNames(1);
  if (dataTileId!="") idColNames(0)=dataTileId; 
  else idColNames.resize(0);
  td.defineHypercolumn("TiledData-"+extension,3,
		       stringToVector(MS::columnName(MS::DATA)),
		       coordColNames,
		       idColNames);

  SetupNewTable newTab(tableName, td, Table::New);
  ForwardColumnEngine fwdEngine(tab);
  StManAipsIO aipsStMan;
  newTab.bindAll(fwdEngine);

  Bool tiledData=False;

  // If there's no id, assume the data is fixed shape throughout
  if (idColNames.nelements()==1) {
    // data shape may change
    TiledDataStMan tiledStMan("TiledData-"+extension);
    newTab.bindColumn(MS::columnName(MS::DATA),tiledStMan);
    newTab.bindColumn(dataTileId,tiledStMan);
    tiledData=True;
  } else {
    // fixed data shape
    ArrayColumn<Complex> data(tab,MS::columnName(MS::DATA));
    Int numCorr=data.shape(0)(0);
    Int numChan=selection_p(1,0);
    // choose a tile size in the channel direction that is <=10
    Int tileSize=(numChan+numChan/10)/(numChan/10+1);
    TiledColumnStMan tiledStMan("TiledData-"+extension,
				IPosition(3,numCorr,tileSize,
				4096/numCorr/tileSize));
    newTab.bindColumn(MS::columnName(MS::DATA),tiledStMan);
  }
  for (Int i=0; i<extraWritableColumns.nelements(); i++) {
    newTab.bindColumn(extraWritableColumns(i),aipsStMan);
  }

  Table refTab(newTab,tab.nrow());

  refTab.keywordSet()=tab.keywordSet();

  TiledDataStManAccessor* dataAccessor(0);
  if (tiledData) 
    dataAccessor=new TiledDataStManAccessor(refTab,"TiledData-"+extension);

  // fill the DATA column
  // we need to zero fill the data column with arrays of the correct
  // shape to be able to do putSlice later for individual visibilities
  
  if (tiledData) {
    TableIterator obsIter(tab,dataTileId);
    for (;!obsIter.pastEnd(); obsIter.next()) {
      ScalarColumn<Int> spwId(obsIter.table(),
			      MS::columnName(MS::SPECTRAL_WINDOW_ID));
      ScalarColumn<Int> tileId(obsIter.table(),dataTileId);
      ArrayColumn<Complex> od(obsIter.table(),MS::columnName(MS::DATA));
      Vector<uInt> rows=obsIter.table().rowNumbers();
      Int spw=spwId(0);
      Int numCorr=od.shape(0)(0);
      Int numChan=selection_p(1,spw);
      // add new hyperCube
      Record values; values.define(dataTileId,tileId(0));
      Int tileSize=(numChan+numChan/10)/(numChan/10+1);
      dataAccessor->addHypercube(IPosition(3,numCorr,numChan,0),
				 IPosition(3,numCorr,tileSize,
					   4096/numCorr/tileSize),
				 values);
    }
  }
  
  ScalarColumn<Int> spwId(obsCoh_p,
			  MS::columnName(MS::SPECTRAL_WINDOW_ID));
  ArrayColumn<Complex> od(obsCoh_p,MS::columnName(MS::DATA));
  ArrayColumn<Complex> rd(refTab,MS::columnName(MS::DATA));
  ScalarColumn<Int> tileId;
  if (tiledData) tileId.attach(obsCoh_p,dataTileId);
  Matrix<Complex> vis;
  Slicer slicer;
  for (Int row=0; row<obsCoh_p.nrow(); row++) {
    Int spw=spwId(row);
    Int numCorr=od.shape(row)(0);
    Int numChan=selection_p(1,spw);
    vis.resize(numCorr,numChan);
    vis=Complex(0.,0.);
    if (tiledData) { 
      Record values;
      values.define(dataTileId,tileId(row));
      dataAccessor->extendHypercube(1,values);
    }
    if(fill=="zero") {
	rd.put(row,vis);
    }
    if(fill=="copy") {
      slicer=Slicer(Slice(),Slice(selection_p(0,spw),numChan));
      od.getSlice(row,slicer,vis);
      rd.put(row,vis);
    }
  }
  if (dataAccessor) delete dataAccessor;
  return refTab;
}
