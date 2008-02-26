//# SubMS.cc 
//# Copyright (C) 1996-2007
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
#include <msvis/MSVis/SubMS.h>
#include <ms/MeasurementSets/MSSelection.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/RefRows.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Slice.h>
#include <casa/Logging/LogIO.h>
#include <casa/OS/File.h>
#include <casa/OS/HostInfo.h>
#include <casa/Containers/Record.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/GenSort.h>
#include <casa/System/AppInfo.h>
#include <msvis/MSVis/VisSet.h>
#include <msvis/MSVis/VisBuffer.h>
#include <msvis/MSVis/VisibilityIterator.h>

#include <tables/Tables/IncrementalStMan.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/StandardStMan.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableInfo.h>
#include <tables/Tables/TableLock.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/TableCopy.h>
#include <tables/Tables/TiledColumnStMan.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <tables/Tables/TiledDataStMan.h>
#include <tables/Tables/TiledStManAccessor.h>
#include <ms/MeasurementSets/MSTileLayout.h>



#include <casa/sstream.h>

namespace casa {
  
Bool SubMS::fillFieldTable() {


  MSFieldColumns& msField(msc_p->field());
 
  //MSField fieldtable= mssel_p.field();
  ROMSFieldColumns & fieldIn= mscIn_p->field(); 
  
  String dirref;
  // Need to define the direction measures right
  fieldIn.delayDir().keywordSet().asRecord("MEASINFO").
                 get("Ref", dirref);
  //  MDirection::getType(dir1, dirref);
  msField.delayDir().rwKeywordSet().asrwRecord("MEASINFO").
                 define("Ref", dirref);
  fieldIn.phaseDir().keywordSet().asRecord("MEASINFO").
    get("Ref", dirref);
  msField.phaseDir().rwKeywordSet().asrwRecord("MEASINFO").
                 define("Ref", dirref);
  fieldIn.referenceDir().keywordSet().asRecord("MEASINFO").
                 get("Ref", dirref);
  msField.referenceDir().rwKeywordSet().asrwRecord("MEASINFO").
                 define("Ref", dirref);


 ROScalarColumn<String> code(fieldIn.code());
 ROArrayColumn<Double> delayDir(fieldIn.delayDir());
 ROScalarColumn<Bool> flagRow(fieldIn.flagRow());
 ROScalarColumn<String> name(fieldIn.name());
 ROScalarColumn<Int> numPoly(fieldIn.numPoly());
 ROArrayColumn<Double> phaseDir(fieldIn.phaseDir());
 ROArrayColumn<Double> refDir(fieldIn.referenceDir());
 ROScalarColumn<Int> sourceId(fieldIn.sourceId());
 ROScalarColumn<Double> time(fieldIn.time());
 

 fieldRelabel_p.resize(mscIn_p->field().nrow());
 fieldRelabel_p.set(-1);


 for(uInt k=0; k < fieldid_p.nelements(); ++k){
   fieldRelabel_p[fieldid_p[k]]=k;
   msOut_p.field().addRow();
   
   msField.code().put(k,code(fieldid_p[k]));
   msField.delayDir().put(k, delayDir(fieldid_p[k]));
   msField.flagRow().put(k, flagRow(fieldid_p[k]));
   msField.name().put(k, name(fieldid_p[k]));
   msField.numPoly().put(k, numPoly(fieldid_p[k]));
   msField.phaseDir().put(k, phaseDir(fieldid_p[k]));
   msField.referenceDir().put(k, refDir(fieldid_p[k]));
   msField.sourceId().put(k, sourceId(fieldid_p[k]));
   


 }

 return True;

}


Bool SubMS::fillMainTable(const String& whichCol){

  LogIO os(LogOrigin("SubMS", "fillMainTable()"));

  msOut_p.addRow(mssel_p.nrow(), True);
  
  if(!antennaSel_p){
    msc_p->antenna1().putColumn(mscIn_p->antenna1());
    msc_p->antenna2().putColumn(mscIn_p->antenna2());
    msc_p->feed1().putColumn(mscIn_p->feed1());
    msc_p->feed2().putColumn(mscIn_p->feed2());
  }
  else{
    Vector<Int> ant1=mscIn_p->antenna1().getColumn();
    Vector<Int> ant2=mscIn_p->antenna2().getColumn();
    Vector<Int> feed1=mscIn_p->feed1().getColumn();
    Vector<Int> feed2=mscIn_p->feed2().getColumn();

    for (uInt k=0; k < ant1.nelements(); ++k){
      ant1[k]=antNewIndex_p[ant1[k]];
      ant2[k]=antNewIndex_p[ant2[k]];
      feed1[k]=feedNewIndex_p[feed1[k]];
      feed2[k]=feedNewIndex_p[feed2[k]];
    }
    msc_p->antenna1().putColumn(ant1);
    msc_p->antenna2().putColumn(ant2);
    msc_p->feed1().putColumn(feed1);
    msc_p->feed2().putColumn(feed2);
  }
  msc_p->arrayId().putColumn(mscIn_p->arrayId());
  msc_p->exposure().putColumn(mscIn_p->exposure());
  //  msc_p->flag().putColumn(mscIn_p->flag());
  // if(!(mscIn_p->flagCategory().isNull()))
  //  if(mscIn_p->flagCategory().isDefined(0))
  //    msc_p->flagCategory().putColumn(mscIn_p->flagCategory());
  msc_p->flagRow().putColumn(mscIn_p->flagRow());
  msc_p->interval().putColumn(mscIn_p->interval());
  msc_p->observationId().putColumn(mscIn_p->observationId());
  msc_p->processorId().putColumn(mscIn_p->processorId());
  msc_p->scanNumber().putColumn(mscIn_p->scanNumber());
  msc_p->stateId().putColumn(mscIn_p->stateId());
  msc_p->time().putColumn(mscIn_p->time());
  msc_p->timeCentroid().putColumn(mscIn_p->timeCentroid());
  msc_p->uvw().putColumn(mscIn_p->uvw());
  msc_p->weight().putColumn(mscIn_p->weight());
  msc_p->sigma().putColumn(mscIn_p->sigma());


  Bool doSpWeight=!(mscIn_p->weightSpectrum().isNull());
  if(doSpWeight)
    doSpWeight= doSpWeight && mscIn_p->weightSpectrum().isDefined(0);

  {
    //relabel data_desc_id  and field_id
    Vector<Int> datDesc = mscIn_p->dataDescId().getColumn();
    Vector<Int> fieldId = mscIn_p->fieldId().getColumn();
    for (uInt k = 0; k < datDesc.nelements(); ++k){
      
      datDesc[k]=spwRelabel_p[oldDDSpwMatch_p[datDesc[k]]];
      fieldId[k]=fieldRelabel_p[fieldId[k]];
      
      
    }
    
    msc_p->dataDescId().putColumn(datDesc);
    msc_p->fieldId().putColumn(fieldId);
  }


  //Deal with data


  String col=whichCol;
  col.upcase();
  String columnName;



  if (col=="OBSERVED" || col==MS::columnName(MS::DATA)) {
    columnName = MS::columnName(MS::DATA);
    os << "splitting out DATA column" << LogIO::POST;
  } else if(col=="MODEL" || col=="MODEL_DATA") {
    if(ms_p.tableDesc().isColumn("MODEL_DATA")) {
      columnName = "MODEL_DATA";
      os << "splitting out MODEL_DATA column" << LogIO::POST;
    } else {
      columnName = MS::columnName(MS::DATA);
      os << LogIO::SEVERE << "MODEL_DATA does not exist, writing DATA"
	 << LogIO::POST;
    }
  } else if(col=="CORRECTED" || col=="CORRECTED_DATA") {
    if(ms_p.tableDesc().isColumn("CORRECTED_DATA")) {
      columnName="CORRECTED_DATA";
      os << "splitting CORRECTED_DATA column" << LogIO::POST;
    } else {
      columnName=MS::columnName(MS::DATA);
      os << LogIO::NORMAL << "CORRECTED_DATA does not exist, writing DATA"
	 << LogIO::POST;
    }
  } else {
    columnName=MS::columnName(MS::DATA);
    os << LogIO::WARN << "Unrecognized column "<<col<<", writing DATA"
       << LogIO::POST;
  }




  if(!doChanAver_p){
    ROArrayColumn<Complex> data;
    if(columnName== MS::columnName(MS::DATA))
      data.reference(mscIn_p->data());
    else if(columnName == "MODEL_DATA" )
      data.reference(mscIn_p->modelData());
    else
    data.reference(mscIn_p->correctedData());
    msc_p->data().putColumn(data);
    msc_p->flag().putColumn(mscIn_p->flag());
  }
  else{


    

    if(sameShape_p){
      //Checking to make sure we have in memory capability else 
      // use visbuffer
      
      Double datavol= mssel_p.nrow()*nchan_p[0]*npol_p[0]*sizeof(Complex);
      Double memAvail= Double (HostInfo::memoryTotal())*(1024);
      //Factoring in 30% for flags and other stuff
      if ((datavol*1.3) >  memAvail)
	sameShape_p = False;


    }

    if(sameShape_p){
      writeSimilarSpwShape(columnName);
    }
    else{
      writeDiffSpwShape(columnName);
    }

  }

  return True;


}

Bool SubMS::fillAverMainTable(const String& whichCol){

  LogIO os(LogOrigin("SubMS", "fillAverMainTable()"));

  Double timeBin=timeBin_p;
  Vector<Int> ant1(0);
  Vector<Int> ant2(0);
  Int numBaselines=numOfBaselines(ant1, ant2, False);
  Int numTimeBins=numOfTimeBins(timeBin);

  if(numTimeBins < 1){
    os << LogIO::SEVERE << "Number of time bins is less than 1...Time averaging bin size may be  too large"
       << LogIO::POST;
  }

  msOut_p.addRow(numBaselines*numTimeBins, True);

  // fill time and timecentroid and antennas
  if(!fillAverAntTime(ant1, ant2, timeBin, numTimeBins))
    return False;


  //Fill array id with first value of input ms for now.
  msc_p->arrayId().fillColumn(mscIn_p->arrayId()(0));
  msc_p->exposure().fillColumn(timeBin);

  msc_p->interval().fillColumn(timeBin);
  msc_p->observationId().fillColumn(mscIn_p->observationId()(0));
  msc_p->exposure().fillColumn(timeBin);
  msc_p->processorId().fillColumn(mscIn_p->processorId()(0));
  msc_p->stateId().fillColumn(mscIn_p->stateId()(0));
  
  //things to be taken care in averData... (1) flagRow, (2) ScanNumber 
  //(3) uvw (4) weight (5) sigma
  if(!fillTimeAverData(ant1, ant2, timeBin, numBaselines, whichCol))
    return False;

  return True;

}



Bool SubMS::copyAntenna(){
  Table oldAnt(mssel_p.antennaTableName(), Table::Old);
  Table& newAnt = msOut_p.antenna();

  if(!antennaSel_p){
    
   
    TableCopy::copyRows(newAnt, oldAnt);
    return True;
  }
  else{
    //Now we try to re-index the antenna list;
    Vector<Int> ant1 = mscIn_p->antenna1().getColumn();
    Int nAnt1=GenSort<Int>::sort(ant1,Sort::Ascending,
				   Sort::NoDuplicates);
    ant1.resize(nAnt1, True);
    Vector<Int> ant2 = mscIn_p->antenna2().getColumn();
    Int nAnt2=GenSort<Int>::sort(ant2,Sort::Ascending,
				 Sort::NoDuplicates);
    ant2.resize(nAnt2, True);
    ant1.resize(nAnt2+nAnt1, True);
    ant1(Slice(nAnt1,nAnt2))=ant2;
    nAnt1=GenSort<Int>::sort(ant1,Sort::Ascending,
			      Sort::NoDuplicates);
    ant1.resize(nAnt1, True);
    antNewIndex_p.resize(oldAnt.nrow());
    antNewIndex_p.set(-1); //So if you see -1 in the main table or feed fix it
    for (Int k=0; k < nAnt1; ++k){
      antNewIndex_p[ant1[k]]=k;
      TableCopy::copyRows(newAnt, oldAnt, k, ant1[k], 1);
    }

    return True;
  }
  return False;

}

Bool SubMS::copyFeed(){

  Table oldFeed(mssel_p.feedTableName(), Table::Old);
  Table& newFeed = msOut_p.feed();
  if(!antennaSel_p){
    TableCopy::copyRows(newFeed, oldFeed);
    return True;
  }
  else{
    Vector<Bool> feedRowSel(oldFeed.nrow());
    feedRowSel.set(False);
    Vector<Int> antIds=ROScalarColumn<Int> (oldFeed, "ANTENNA_ID").getColumn();
    Vector<Int> feedIds=ROScalarColumn<Int> (oldFeed, "FEED_ID").getColumn();
    feedNewIndex_p.resize(max(feedIds)+1);
    feedNewIndex_p.set(-1);
    uInt feedSelected=0;
    for (uInt k=0; k < antIds.nelements(); ++k){
      if(antNewIndex_p[antIds[k]] > -1){
	feedRowSel[k]=True;
	feedNewIndex_p[feedIds[k]]=feedSelected;
	TableCopy::copyRows(newFeed, oldFeed, feedSelected, k, 1);
	++feedSelected;
      }
    }
    ScalarColumn<Int> antCol(newFeed, "ANTENNA_ID");
    ScalarColumn<Int> feedCol(newFeed, "FEED_ID");
    Vector<Int> newAntIds=antCol.getColumn();
    Vector<Int> newFeedIds=feedCol.getColumn();
    for (uInt k=0; k< feedSelected; ++k){
      newAntIds[k]=antNewIndex_p[newAntIds[k]];
      newFeedIds[k]=feedNewIndex_p[newFeedIds[k]];
    } 
    antCol.putColumn(newAntIds);
    feedCol.putColumn(newFeedIds);

    return True;

  }

  return True;

}


Bool SubMS::copySource(){
  //Source is an optinal table..so it may not exist
  if(Table::isReadable(mssel_p.sourceTableName())){
    Table oldSource(mssel_p.sourceTableName(), Table::Old);
    Table& newSource=msOut_p.source();

    if(newSource.actualTableDesc().ncolumn() != 
       oldSource.actualTableDesc().ncolumn()){      
      Vector<String> oldColumnNames=oldSource.actualTableDesc().columnNames();
      Vector<String> optionalCols(6);
      optionalCols[0]="TRANSITION";
      optionalCols[1]="REST_FREQUENCY";
      optionalCols[2]="SYSVEL";
      optionalCols[3]="SOURCE_MODEL";
      optionalCols[4]="PULSAR_ID";
      optionalCols[5]="POSITION";
      for (uInt k=0; k< oldSource.actualTableDesc().ncolumn(); ++k){
	for (uInt j=0; j < optionalCols.nelements(); ++j){
	  if(oldColumnNames[k].contains(optionalCols[j])){
	    TableDesc tabDesc;
	    MSSource::addColumnToDesc(tabDesc, MSSource::columnType(optionalCols[j]));
	    newSource.addColumn(tabDesc[0]);
	  }
	}
      }
    }
    TableCopy::copyRows(newSource, oldSource);
    return True;
  }
  
 
  return False;

}

Bool SubMS::copyObservation(){

  Table oldObs(mssel_p.observationTableName(), Table::Old);
  Table& newObs=msOut_p.observation();
  TableCopy::copyRows(newObs, oldObs);


  return True;

}

Bool SubMS::copyPointing(){
  //Pointing is allowed to not exist
  Bool pointExists=Table::isReadable(mssel_p.pointingTableName());
  if(pointExists){
    Table oldPoint(mssel_p.pointingTableName(), Table::Old);
    if(oldPoint.nrow() > 0){
      Table& newPoint=msOut_p.pointing();
      TableCopy::copyRows(newPoint, oldPoint);
    }
  }
  return True;

}



Bool SubMS::writeDiffSpwShape(String& columnName){

  Bool doSpWeight=!(mscIn_p->weightSpectrum().isNull());
  if(doSpWeight)
    doSpWeight= doSpWeight && mscIn_p->weightSpectrum().isDefined(0);


  Int rowsdone=0;
  Int rowsnow=0;
  Block<Int> sort(0);
  Matrix<Int> noselection;
  
  VisSet *vs= new VisSet(mssel_p, noselection);
  ROVisIter& vi(vs->iter());
  VisBuffer vb(vi);
  Vector<Int> spwindex(max(spw_p)+1);
  spwindex.set(-1);
  for (uInt k=0; k < spw_p.nelements() ; ++k){
    spwindex[spw_p[k]]=k;
  }
  
  for (vi.originChunks();vi.moreChunks();vi.nextChunk()) {
    for (vi.origin(); vi.more(); vi++) {
      rowsnow=vb.nRow();
      RefRows rowstoadd(rowsdone, rowsdone+rowsnow-1);
      Int spw=spwindex[vb.spectralWindow()];
      Cube<Complex> vis;
      if(columnName== MS::columnName(MS::DATA))
	  vis.reference(vb.visCube());
      else if(columnName == "MODEL_DATA" )
	vis.reference(vb.modelVisCube());
      else
	vis.reference(vb.correctedVisCube());
      
      Cube<Bool> inFlag;
      Cube<Float> inSpWeight;
      inFlag.reference(vb.flagCube());
      Matrix<Bool> chanFlag;
      chanFlag.reference(vb.flag());
      Cube<Complex> averdata(npol_p[spw], nchan_p[spw], rowsnow);
      averdata.set(Complex(0,0));
      Cube<Float> spWeight;
      if (doSpWeight){
	spWeight.resize(npol_p[spw], nchan_p[spw], rowsnow);
      }
      Cube<Bool> locflag(npol_p[spw], nchan_p[spw], rowsnow);
      Bool idelete;
      const Bool* iflag=inFlag.getStorage(idelete);
      const Complex* idata=vis.getStorage(idelete);
      //      const Float* iweight=inSpWeight.getStorage(idelete);
      Complex* odata=averdata.getStorage(idelete);
      Bool* oflag=locflag.getStorage(idelete);
      // We have to revisit this once visBuffer provides the spectral Weights

      
      for(Int k=0; k < rowsnow; ++k){
	for(Int j=0; j < nchan_p[spw]; ++j){
	  Vector<Int>counter(npol_p[spw]);
	  counter.set(0);
	  for (Int pol=0; pol < npol_p[spw]; ++pol){
	    Int outoffset=k*nchan_p[spw]*npol_p[spw]+j*npol_p[spw]+pol;
	    if(!averageChannel_p){
	      averdata.xyPlane(k).column(j)=
		vis.xyPlane(k).column(chanStart_p[spw]+ j*chanStep_p[spw]); 
	      locflag.xyPlane(k).column(j)=
		inFlag.xyPlane(k).column(chanStart_p[spw]+ j*chanStep_p[spw]); 
	    }
	    else{  
	      for (Int m=0; m < chanStep_p[spw]; ++m){
		Int inoffset=k*inNumChan_p[spw]*npol_p[spw]+
		  (j*chanStep_p[spw]+m)*npol_p[spw]+ pol;
		
		if(!iflag[inoffset]){
		  odata[outoffset] += idata[inoffset];
		  ++counter[pol];
		}
		
	      }
		
	      
	    }
	    
	    if(averageChannel_p){
	      if(counter[pol] >0){
		odata[outoffset] = odata[outoffset]/counter[pol];
		oflag[outoffset]=False;
		
	      }
	      else{
		odata[outoffset]=0;
		oflag[outoffset]=True;
	      }
	    }
	  }
	  
	}
	


      }
      
      rowsdone+=rowsnow;
      msc_p->data().putColumnCells(rowstoadd, averdata);
      msc_p->flag().putColumnCells(rowstoadd, locflag);
      
    }
  }
  
  return True;
  
  
}

Bool SubMS::writeSimilarSpwShape(String& columnName){


  Int nrow=mssel_p.nrow();
  ROArrayColumn<Complex> data;
  ROArrayColumn<Float> wgtSpec;
  ROArrayColumn<Bool> flag(mscIn_p->flag());
  if(columnName== MS::columnName(MS::DATA))
    data.reference(mscIn_p->data());
  else if(columnName == "MODEL_DATA" )
    data.reference(mscIn_p->modelData());
  else
    data.reference(mscIn_p->correctedData());



  Bool deleteIptr,  deleteIWptr;
  Bool deleteIFptr;
  Matrix<Complex> indatatmp(npol_p[0], inNumChan_p[0]);
  const Complex *iptr = indatatmp.getStorage(deleteIptr);
  Matrix<Bool> inflagtmp(npol_p[0], inNumChan_p[0]);
  const Bool *iflg = inflagtmp.getStorage(deleteIFptr);
  Vector<Complex> outdatatmp(npol_p[0]);
  //    const Complex *optr = outdatatmp.getStorage(deleteOptr);
  Matrix<Float> inwgtspectmp(npol_p[0], inNumChan_p[0]);
  const Float *inwptr = inwgtspectmp.getStorage(deleteIWptr);
  Vector<Float> outwgtspectmp(npol_p[0]);
    //   const Float *owptr = outwgtspectmp.getStorage(deleteOWptr);
    

  Bool doSpWeight=!(mscIn_p->weightSpectrum().isNull());
  if(doSpWeight)
    doSpWeight= doSpWeight && mscIn_p->weightSpectrum().isDefined(0);


  Cube<Complex> outdata(npol_p[0], nchan_p[0], nrow);
  Cube<Bool> outflag(npol_p[0], nchan_p[0], nrow);
  Cube<Float> outspweight ;
  if(doSpWeight){ 
    outspweight.resize(npol_p[0], nchan_p[0], nrow);
    wgtSpec.reference(mscIn_p->weightSpectrum());
  }
    
  for (Int row=0; row < nrow; ++row){

    data.get(row, indatatmp);
    flag.get(row, inflagtmp);
    if(doSpWeight)  wgtSpec.get(row, inwgtspectmp);
    Int ck=0;
    Int chancounter=0;
    Vector<Int> avcounter(npol_p[0]);
    outdatatmp.set(0); outwgtspectmp.set(0);
    avcounter.set(0);
      
    for (Int k=chanStart_p[0]; k< (nchan_p[0]*chanStep_p[0]+chanStart_p[0]);
	 ++k) {
      
      if(chancounter == chanStep_p[0]){
	outdatatmp.set(0); outwgtspectmp.set(0);
	chancounter=0;
	avcounter.set(0);
      }
      ++chancounter;
      for (Int j=0; j< npol_p[0]; ++j){
	Int offset= j + k*npol_p[0];
	if(!iflg[offset]){
	  if(doSpWeight){
	    outdatatmp[j] += iptr[offset]*inwptr[offset];
	    outwgtspectmp[j] += inwptr[offset];
	  }
	  else{
	    outdatatmp[j] += iptr[offset];	   
	      
	  }
	  
	  ++avcounter[j];
	}


	if(chancounter==chanStep_p[0]){
	  if(avcounter[j] !=0){
	    if(doSpWeight){
	      outdata(j,ck,row)=outdatatmp[j]/outwgtspectmp[j];	 
	      outspweight(j,ck,row)=outwgtspectmp[j];
	    }
	    else{
	      outdata(j,ck,row)=outdatatmp[j]/avcounter[j];	    
	    }
	    outflag(j,ck,row)=False;
	  } 
	  else{
	    
	    outdata(j,ck,row)=0;
	    outflag(j,ck,row)=True;
	    if(doSpWeight)outspweight(j,ck,row)=0;
	  }
	  
	}

	  
      }
	

      if(chancounter==chanStep_p[0]){
	++ck;
	
 
      }


    }

    
  }

  msc_p->data().putColumn(outdata);
  msc_p->flag().putColumn(outflag);
  if(doSpWeight)msc_p->weightSpectrum().putColumn(outspweight);


  return True;

}


  Int SubMS::numOfBaselines(Vector<Int>& ant1, Vector<Int>& ant2, 
		    Bool includeAutoCorr){
    Int numRows=mssel_p.nrow();
    Vector<Int> selAnt1(numRows);
    Vector<Int> selAnt2(numRows);
    selAnt1=mscIn_p->antenna1().getColumn();
    selAnt2=mscIn_p->antenna2().getColumn();
    Int numAnt1=GenSort<Int>::sort(selAnt1,Sort::Ascending,
				   Sort::NoDuplicates);
    Int numAnt2=GenSort<Int>::sort(selAnt2,Sort::Ascending,
				   Sort::NoDuplicates);
 


    ant1.resize();
    selAnt1.resize(numAnt1, True);
    ant1=selAnt1;
    ant2.resize();
    selAnt2.resize(numAnt2, True);
    ant2=selAnt2;
    Int numBasl=0;
    Bool hasAuto=False;
    if(numAnt1==numAnt2){
      if(allEQ(ant1, ant2)){
	hasAuto=True;
      }
      
    }

    //For now we are splitting data without autocorrelation
      
    if(Int(Float(numAnt2)/2.0)*2 !=  numAnt2){
      if(!hasAuto){
	numBasl=(numAnt2+1)/2*numAnt1;
      }
      else
	numBasl=(numAnt2-1)/2*numAnt1;
    }
    else if(Int(Float(numAnt1)/2.0)*2 !=  numAnt1){
      if(!hasAuto){
        numBasl=(numAnt1+1)/2*numAnt2;
      }
      else
        numBasl=(numAnt1-1)/2*numAnt2;
    }
    else{
      if(!hasAuto)
	numBasl=(numAnt1+1)*numAnt2/2;
      else
	numBasl=(numAnt1)*numAnt2/2;
    }

    return numBasl;
   
  }

  Int SubMS::numOfTimeBins(const Double& timeBin){
    Int numBin=0;

    if (timeBin > 0.0){
      Int numrows=mssel_p.nrow();
      Vector<Double> timeRows=mscIn_p->time().getColumn();
      Vector<uInt> tOI; //timeOrderIndex
      GenSortIndirect<Double>::sort(tOI, timeRows);
      timeBinIndex_p.resize(numrows);
      newTimeVal_p.resize(numrows);
      numBin=1;
      timeBinIndex_p[tOI[0]]=0;
      newTimeVal_p[0]=timeRows[tOI[0]]+timeBin/2.0;
      for (uInt k =1 ; k < uInt(numrows); ++k){
	if(timeRows[tOI[k]] > (newTimeVal_p[numBin-1]+0.5*timeBin)){
	  if(timeRows[tOI[k]] > newTimeVal_p[numBin-1]+timeBin){
	    newTimeVal_p[numBin]=timeRows[tOI[k]]+0.5*timeBin;
	  }
	  else{
	    newTimeVal_p[numBin]=newTimeVal_p[numBin-1]+timeBin;
	  }
	  ++numBin;
	}
	timeBinIndex_p[tOI[k]]=numBin-1;
      }
      newTimeVal_p.resize(numBin, True);
      return numBin;
    }
    
    return -1;
  }



  Bool SubMS::fillAverAntTime(Vector<Int>& ant1, Vector<Int>& ant2, 
			      const Double& timeBin, 
			      const Int& numOfTimeBins){

    uInt nrows=msOut_p.nrow();
    Vector<Int> antenna1(nrows);
    Vector<Int> antenna2(nrows);


    Vector<Double> rowTime(nrows);

    Vector<Int> ant2Indexer(max(ant2)+1);
    ant2Indexer=-1;
    for (Int j=0; j< Int(ant2.nelements()); ++j){
      ant2Indexer[ant2[j]]=j;
    }
    //Will need to do the weighted averaging of time in the future.
    uInt k=0;
    //Double timeStart=mscIn_p->time()(0)+0.5*timeBin;
    for (Int t=0; t < numOfTimeBins; ++t){
      for (uInt ant1Index=0; ant1Index < ant1.nelements(); ++ant1Index){
	// be careful as selection may have ant1 which is bigger than max
	// ant2
	if(ant1[ant1Index] < max(ant2)){
	  uInt startAnt2Ind=ant2.nelements();
	  
	  Int somcounter=1;
	  //startAnt2Ind=ant2Indexer[ant1[ant1Index]+somcounter];
	  
	  while((somcounter+ant1[ant1Index] < Int(ant2Indexer.nelements())) &&  
		(ant2Indexer[ant1[ant1Index]+somcounter]==-1)){
	    somcounter+=1;
	  }
	  if( ant1[ant1Index]+somcounter < Int(ant2Indexer.nelements())){
	    startAnt2Ind=ant2Indexer[ant1[ant1Index]+somcounter];  
	  }
	  if(k== nrows){
	      throw(AipsError("Something not expected by the programmer happened; Please file a bug report"));
	  }
	  for (uInt ant2Index=startAnt2Ind; ant2Index < ant2.nelements(); 
	       ++ant2Index){ 
	    if(!antennaSel_p){
	      antenna1[k]=ant1[ant1Index];
	      antenna2[k]=ant2[ant2Index];
	    }
	    else{
	     antenna1[k]=antNewIndex_p[ant1[ant1Index]];
	     antenna2[k]=antNewIndex_p[ant2[ant2Index]]; 
	    }
	    rowTime[k]=newTimeVal_p[t];
	    ++k;
	    

	  }
	}
      }
    }

    msc_p->antenna1().putColumn(antenna1);
    msc_p->antenna2().putColumn(antenna2);
    // Feed Ids are not being handled properly...
    // Will work for arrays with one feed setting for all antennas
    // but will need to be fixed for multi feed setting in one array/ms
    //Multi-feed antennas and/or multi-feed setting will be messed up
    msc_p->feed1().fillColumn(mscIn_p->feed1()(0));
    msc_p->feed2().fillColumn(mscIn_p->feed2()(0)); 
    msc_p->time().putColumn(rowTime);
    msc_p->timeCentroid().putColumn(rowTime);

    return True;

  }

  Bool SubMS::fillTimeAverData(Vector<Int>& ant1, Vector<Int>& ant2, 
			       const Double& timeBin, const Int& numbas,
			   const String& columnName){


    LogIO os(LogOrigin("SubMS", "fillAverMainTable()"));




    //Need to deal with uvw too
    Vector<Int> ant1Index(max(ant1)+1);
    ant1Index=-1;
    Vector<Int> ant2Index(max(ant2)+1);
    ant2Index=-1;
    Int numAnt2=ant2.nelements();
    for (Int j=0; j< Int(ant1.nelements()); ++j){
      ant1Index[ant1[j]]=j;
    }
    for (Int j=0; j< Int(ant2.nelements()); ++j){
      ant2Index[ant2[j]]=j;
    }
    Int inNrow=mssel_p.nrow();
    Int outNrow=msOut_p.nrow();
    ROArrayColumn<Complex> data;
    Vector<Int> antenna1=mscIn_p->antenna1().getColumn();
    Vector<Int> antenna2=mscIn_p->antenna2().getColumn();
    Vector<Double> time= mscIn_p->time().getColumn();
    Vector<Double> outTime= msc_p->time().getColumn();
    Vector<Double> nearestTime(outNrow);
    nearestTime.set(1.0e9);
    ROArrayColumn<Float> wgtSpec;
    
    ROArrayColumn<Bool> flag(mscIn_p->flag());
    ROScalarColumn<Bool> rowFlag(mscIn_p->flagRow());
    ROScalarColumn<Int> scanNum(mscIn_p->scanNumber());
    ROScalarColumn<Int> dataDescIn(mscIn_p->dataDescId());
    ROArrayColumn<Double> inUVW(mscIn_p->uvw());
 


    if(columnName== MS::columnName(MS::DATA))
      data.reference(mscIn_p->data());
    else if(columnName == "MODEL_DATA" )
      data.reference(mscIn_p->modelData());
    else
      data.reference(mscIn_p->correctedData());
 
    os << LogIO::NORMAL << "Writing time averaged data of column " 
       << columnName << " in " << newTimeVal_p.nelements() << " time slots"
       << LogIO::POST;

    Bool doSpWeight=!(mscIn_p->weightSpectrum().isNull());
    if(doSpWeight)
      doSpWeight= doSpWeight && mscIn_p->weightSpectrum().isDefined(0);


    Cube<Complex> outData(npol_p[0], nchan_p[0], outNrow);
    outData.set(0.0);
    Matrix<Float> outRowWeight(npol_p[0], outNrow);
    outRowWeight.set(0.0);
    Cube<Bool> outFlag(npol_p[0], nchan_p[0], outNrow);
    outFlag.set(True);
    Vector<Bool> outRowFlag(outNrow);
    outRowFlag.set(True);
    Vector<Int> outScanNum(outNrow);
    Vector<Int> dataDesc(outNrow);
    dataDesc.set(-1);
    outScanNum.set(0);
    Cube<Float> outSpWeight ;
    Matrix<Double> outUVW(3,outNrow);
    outUVW.set(0.0);
    if(doSpWeight){ 
      outSpWeight.resize(npol_p[0], nchan_p[0], outNrow);
      outSpWeight.set(0.0);
      wgtSpec.reference(mscIn_p->weightSpectrum());
    }
    ROArrayColumn<Float> inRowWeight(mscIn_p->weight());

    Int timeChunk=0;
    Int baselineNum=0;

    if(!doChanAver_p){
      for (Int k = 0; k < inNrow; ++k){
	timeChunk=timeBinIndex_p[k];
	Int a1=ant1Index[antenna1[k]];
	Int a2=ant2Index[antenna2[k]];
	baselineNum=a1*numAnt2-(a1*(a1+3))/2 + a2 ;
        Int row=timeChunk*numbas+baselineNum;
	if(dataDesc(row)==-1){
	  dataDesc(row)=dataDescIn(k);
	}
	// Will need to take care of multi datadescription in one time bin.
	if(!rowFlag(k) && dataDesc(row)==dataDescIn(k)){
	  Double timeDiff=abs(time[k]-outTime[row]);
	  if(nearestTime[row] > timeDiff){
	    nearestTime[row]=timeDiff;
	    outUVW.column(row)=inUVW(k);
	    outScanNum[row]=scanNum(k);
	  }
	  outRowWeight.column(row) = outRowWeight.column(row) + inRowWeight(k);
	  outFlag.xyPlane(row) = outFlag.xyPlane(row)* flag(k);
	  outData.xyPlane(row) = outData.xyPlane(row) + data(k)*max(inRowWeight(k));


	  outRowFlag(row) = False;
	}
	
      }

      Matrix<Float> outSigma(npol_p[0], outNrow);
      outSigma.set(0.0);
      //Drat le Rat...the ms tool hate to have dataDesc -1
      for (Int k=0; k < outNrow; ++k){
	if(outRowFlag(k))
	  dataDesc[k]=0;
	if(product(outRowWeight.column(k)) > 0.0){
	  outData.xyPlane(k) = outData.xyPlane(k)/max(outRowWeight.column(k));
	  for (Int j = 0; j< npol_p[0]; ++j){
	    outSigma(j,k)=1/sqrt(outRowWeight(j,k));
	  }
	}	
      }
      msc_p->data().putColumn(outData);
      msc_p->flag().putColumn(outFlag);
      msc_p->flagRow().putColumn(outRowFlag);
      msc_p->uvw().putColumn(outUVW);
      msc_p->scanNumber().putColumn(outScanNum);
      msc_p->dataDescId().putColumn(dataDesc);
      // Free some memory
      outData.resize();
      outFlag.resize();
      outUVW.resize();
      msc_p->weight().putColumn(outRowWeight);
      msc_p->sigma().putColumn(outSigma);
    }
    

    return True;


  }


  void SubMS::checkSpwShape(){

    sameShape_p=True;
    if(inNumChan_p.nelements() > 1){
      for (uInt k=1; k < inNumChan_p.nelements(); ++k){
	sameShape_p= sameShape_p && (inNumChan_p[k] == inNumChan_p[k-1]);      
      }
    }
    
    if(nchan_p.nelements() > 1){
      for (uInt k=1; k < nchan_p.nelements(); ++k){
	sameShape_p= sameShape_p && (nchan_p[k] == nchan_p[k-1]);      
      }
    }


  }



} //#End casa namespace
