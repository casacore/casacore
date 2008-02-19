//# VisSet.cc: Implementation of VisSet
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

  VisSet::VisSet(MeasurementSet& ms,const Block<Int>& columns, 
		 const Matrix<Int>& chanSelection, Double timeInterval,
		 Bool compress)
    :ms_p(ms)
  {
    LogSink logSink;
    LogMessage message(LogOrigin("VisSet","VisSet"));
    
    blockOfMS_p= new Block<MeasurementSet> ();
    blockOfMS_p->resize(1);
    (*blockOfMS_p)[0]=ms_p;
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
  
  VisSet::VisSet(Block<MeasurementSet>& mss,const Block<Int>& columns, 
		 const Block< Matrix<Int> >& chanSelection, 
		 Double timeInterval,
		 Bool compress)
  {
    
    
    blockOfMS_p = &mss;
    Int numMS=mss.nelements();
    ms_p=mss[numMS-1];
    
  

    
    Block<Vector<Int> > blockNGroup(numMS);
    Block<Vector<Int> > blockStart(numMS);
    Block<Vector<Int> > blockWidth(numMS);
    Block<Vector<Int> > blockIncr(numMS);
    Block<Vector<Int> > blockSpw(numMS);
    
    for (Int k=0; k < numMS; ++k){
      // sort out the channel selection
      Int nSpw=mss[k].spectralWindow().nrow();
      blockNGroup[k]=Vector<Int> (nSpw,1);
      blockIncr[k]=Vector<Int> (nSpw,1);
      // At this stage select all spw
      blockSpw[k].resize(nSpw);
      indgen(blockSpw[k]);
      if(chanSelection[k].nelements()!=0){
	blockStart[k]=chanSelection[k].row(0);
	blockWidth[k]=chanSelection[k].row(1);
      }
      MSSpWindowColumns msSpW(mss[k].spectralWindow());
      selection_p.resize(2,nSpw);
      //Drat...need to figure this one out....
      // fill in default selection
      selection_p.row(0)=0; //start
      selection_p.row(1)=msSpW.numChan().getColumn(); 
      blockStart[k].resize(selection_p.row(0).nelements());
      blockStart[k]=selection_p.row(0);
      blockWidth[k].resize(selection_p.row(1).nelements());
      blockWidth[k]=selection_p.row(1);
      for (uInt i=0; i<chanSelection[k].ncolumn(); i++) {
	Int spw=chanSelection[k](2,i);
	if (spw>=0 && spw<nSpw && chanSelection[k](0,i)>=0 && 
	    chanSelection[k](0,i)+chanSelection[k](1,i)<=selection_p(1,spw)) {
	  // looks like a valid selection, implement it
	  selection_p(0,spw)=chanSelection[k](0,i);
	  selection_p(1,spw)=chanSelection[k](1,i);
	  blockStart[k][spw]=chanSelection[k](0,i);
	  blockWidth[k][spw]=chanSelection[k](1,i);
	}
      }
    }
    iter_p=new VisIter(mss,columns,timeInterval);
    
    iter_p->selectChannel(blockNGroup, blockStart, blockWidth, blockIncr,
			  blockSpw);


    for (Int k=0; k < numMS ; ++k){
      addScratchCols(mss[k], compress);
    }
    
    
    
  }





VisSet::VisSet(MeasurementSet& ms, const Matrix<Int>& chanSelection, 
	       Double timeInterval)
:ms_p(ms)
{
    LogSink logSink;
    LogMessage message(LogOrigin("VisSet","VisSet"));

    blockOfMS_p= new Block<MeasurementSet> ();
    blockOfMS_p->resize(1);
    (*blockOfMS_p)[0]=ms_p;

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
    blockOfMS_p=new Block<MeasurementSet>();
    blockOfMS_p->resize(1);
    (*blockOfMS_p)[0]=ms_p;
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

    blockOfMS_p= new Block<MeasurementSet> ();
    blockOfMS_p->resize(other.blockOfMS_p->nelements());
    for (uInt k=0; k < blockOfMS_p->nelements() ; ++k)
      (*blockOfMS_p)[k]= (*(other.blockOfMS_p))[k];

    selection_p.resize(other.selection_p.shape());
    selection_p=other.selection_p;
    *iter_p=*(other.iter_p);
    return *this;
}

VisSet::~VisSet() {
  ms_p.flush();
  delete iter_p; iter_p=0;
};

void VisSet::resetVisIter(const Block<Int>& columns, Double timeInterval) 
{

  // Delete existing VisIter:
  if (iter_p) delete iter_p;

  // Make new VisIter with existing ms_p, and new sort/interval
  iter_p=new VisIter(ms_p,columns,timeInterval);

  // Inform new VisIter of channel selection
  for (uInt spw=0; spw<selection_p.ncolumn(); spw++) {
    iter_p->selectChannel(1,selection_p(0,spw),selection_p(1,spw),0,spw);
  }

}

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
  if(iter_p->newMS()){
    ms_p=(*blockOfMS_p)[iter_p->msId()];
  }
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

Int VisSet::numberAnt()  
{  
  if(iter_p->newMS()){
    ms_p=(*blockOfMS_p)[iter_p->msId()];
  }
  
  return ((MeasurementSet&)ms_p).antenna().nrow(); // for single (sub)array only..
}
Int VisSet::numberSpw()  
{

  if(iter_p->newMS()){
    ms_p=(*blockOfMS_p)[iter_p->msId()];
  }
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

  Int numcoh=0;
  for (uInt k=0; k < blockOfMS_p->nelements(); ++k){
    numcoh+=(*blockOfMS_p)[k].nrow();
  }
  return numcoh;
}

void VisSet::addScratchCols(MeasurementSet& ms, Bool compress){

  LogSink logSink;
  LogMessage message(LogOrigin("VisSet","VisSet"));

  //function to add scratchy column
  Bool init=True;

  Int nSpw=ms.spectralWindow().nrow();
  MSSpWindowColumns msSpW(ms.spectralWindow());
  selection_p.resize(2,nSpw);
  // fill in default selection
  selection_p.row(0)=0; //start
  selection_p.row(1)=msSpW.numChan().getColumn(); 
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

    // Force re-sort if it was sorted 
    if (ms.keywordSet().isDefined("SORT_COLUMNS")) 
      ms.rwKeywordSet().removeField("SORT_COLUMNS");
    if (ms.keywordSet().isDefined("SORTED_TABLE")) 
      ms.rwKeywordSet().removeField("SORTED_TABLE");
  }

  
  // Initialize MODEL_DATA and CORRECTED_DATA
  if (init) initCalSet(0);



}

String VisSet::msName(){

  String a=ms_p.antenna().tableName();
  return a.before("/ANTENNA");
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

