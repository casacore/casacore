//# SubMS.cc 
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
#include <trial/MeasurementSets/SubMS.h>
#include <trial/MeasurementSets/MSSelection.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/RefRows.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Logging.h>
#include <aips/Logging/LogIO.h>
#include <aips/OS/File.h>
#include <aips/Containers/Record.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <trial/MeasurementEquations/VisSet.h>
#include <trial/MeasurementEquations/VisBuffer.h>
#include <trial/MeasurementEquations/VisibilityIterator.h>

#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableCopy.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledStManAccessor.h>
#include <trial/MeasurementSets/MSTileLayout.h>



#include <aips/sstream.h>



SubMS::SubMS(String& theMS){
 
  ms_p=MeasurementSet(theMS, Table::Update);
  mssel_p=ms_p;
  doChanAver_p=False;
}

SubMS::SubMS(MeasurementSet& ms){


  ms_p=ms;
  mssel_p=ms_p;
  doChanAver_p=False;
}

SubMS::~SubMS(){

  msOut_p.flush();

}


void SubMS::selectSpw(Vector<Int> spw, Vector<Int> nchan, Vector<Int> start, 
		 Vector<Int> step, Bool averchan) {


  spw_p.resize();
  spw_p=spw;

  //Check size of start, step, nchan


  nchan_p.resize();
  nchan_p=nchan;
  chanStart_p.resize();
  chanStart_p=start;
  chanStep_p.resize();
  chanStep_p=step;
  averageChannel_p=averchan;



}

void SubMS::selectSource(Vector<Int> fieldid){

  fieldid_p.resize();
  fieldid_p=fieldid;

}


Bool SubMS::makeSubMS(String& msname, String& colname){


  makeSelection();
  mscIn_p=new MSColumns(mssel_p);
  msOut_p=setupMS(msname, nchan_p[0], npol_p[0],  
		  mscIn_p->observation().telescopeName()(0));
  msc_p=new MSColumns(msOut_p);
  // fill or update
  fillDDTables();
  fillFieldTable();
  copyAntenna();
  copyFeed();
  copyObservation();
  fillMainTable(colname);
  


  return True;

}


Bool SubMS::makeSelection(){



  if (!ms_p.keywordSet().isDefined("SORTED_TABLE")) {
    Block<int> sort(4);
    sort[0] = MS::FIELD_ID;
    sort[1] = MS::ARRAY_ID;
    sort[2] = MS::DATA_DESC_ID;
    sort[3] = MS::TIME;
    Matrix<Int> noselection;
    VisSet vs(ms_p,sort,noselection);
  }
  Table sorted=ms_p.keywordSet().asTable("SORTED_TABLE");

  MSSelection thisSelection;
  if(fieldid_p.nelements() > 0)
    thisSelection.setFieldIds(fieldid_p);
  if(spw_p.nelements() > 0)
    thisSelection.setSpwIds(spw_p);

  const TableExprNode exprNode=thisSelection.toTableExprNode(sorted);

  // check that sorted table exists (it should), if not, make it now.

  {
    MSPolarization poltable= ms_p.polarization();
    ROArrayColumn<Int> pols(poltable, 
			    MSPolarization::columnName(MSPolarization::CORR_TYPE));
    
    npol_p.resize(spw_p.shape()); //assuming a simple data_desc_id
    for (uInt k=0; k < npol_p.nelements(); ++k){  
      npol_p[k]=pols(spw_p[k]).nelements();
    }
  }
  // Now remake the selected ms
  mssel_p = MeasurementSet(sorted(exprNode));
  
  mssel_p.rename(ms_p.tableName()+"/SELECTED_TABLE", Table::Scratch);


  return True;

}

MeasurementSet& SubMS::setupMS(String MSFileName, Int nchan, Int nCorr, String telescop, Int obsType ){

  

  // Make the MS table
  TableDesc td = MS::requiredTableDesc();
  
  // Even though we know the data is going to be the same shape throughout I'll
  // still create a column that has a variable shape as this will permit MS's
  // with other shapes to be appended.
  MS::addColumnToDesc(td, MS::DATA, 2);

  // add this optional column because random group fits has a
  // weight per visibility
  MS::addColumnToDesc(td, MS::WEIGHT_SPECTRUM, 2);

    td.defineHypercolumn("TiledData",3,
 			 stringToVector(MS::columnName(MS::DATA)));
    td.defineHypercolumn("TiledFlag",3,
 			 stringToVector(MS::columnName(MS::FLAG)));
    td.defineHypercolumn("TiledFlagCategory",4,
 			 stringToVector(MS::columnName(MS::FLAG_CATEGORY)));
    td.defineHypercolumn("TiledWgtSpectrum",3,
 			 stringToVector(MS::columnName(MS::WEIGHT_SPECTRUM)));
    td.defineHypercolumn("TiledUVW",2,
 			 stringToVector(MS::columnName(MS::UVW)));
    td.defineHypercolumn("TiledWgt",2,
			 stringToVector(MS::columnName(MS::WEIGHT)));
    td.defineHypercolumn("TiledSigma", 2,
			 stringToVector(MS::columnName(MS::SIGMA)));
  
  SetupNewTable newtab(MSFileName, td, Table::New);
  
  // Set the default Storage Manager to be the Incr one
  IncrementalStMan incrStMan ("ISMData");
  newtab.bindAll(incrStMan, True);

  // Bind ANTENNA1, ANTENNA2 and DATA_DESC_ID to the standardStMan 
  // as they may change sufficiently frequently to make the
  // incremental storage manager inefficient for these columns.

  StandardStMan aipsStMan(32768);
  newtab.bindColumn(MS::columnName(MS::ANTENNA1), aipsStMan);
  newtab.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan);
  newtab.bindColumn(MS::columnName(MS::DATA_DESC_ID), aipsStMan);

    // Choose an appropriate tileshape
    IPosition dataShape(2,nCorr,nchan);
    IPosition tileShape = MSTileLayout::tileShape(dataShape,obsType, telescop);
    //    itsLog << LogOrigin("MSFitsInput", "setupMeasurementSet");
    //itsLog << LogIO::NORMAL << "Using tile shape "<<tileShape <<" for "<<
    //  array_p<<" with obstype="<< obsType<<LogIO::POST;
 
    TiledShapeStMan tiledStMan1("TiledData",tileShape);
    TiledShapeStMan tiledStMan1f("TiledFlag",tileShape);
    TiledShapeStMan tiledStMan1fc("TiledFlagCategory",
				  IPosition(4,tileShape(0),tileShape(1),1,
 					   tileShape(2)));
    TiledShapeStMan tiledStMan2("TiledWgtSpectrum",tileShape);
    TiledColumnStMan tiledStMan3("TiledUVW",IPosition(2,3,1024));
    TiledShapeStMan tiledStMan4("TiledWgt", 
				IPosition(2,tileShape(0),tileShape(2)));
    TiledShapeStMan tiledStMan5("TiledSigma", 
				IPosition(2,tileShape(0),tileShape(2)));
    
    // Bind the DATA, FLAG & WEIGHT_SPECTRUM columns to the tiled stman
    newtab.bindColumn(MS::columnName(MS::DATA),tiledStMan1);
    newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan1f);
    newtab.bindColumn(MS::columnName(MS::FLAG_CATEGORY),tiledStMan1fc);
    newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM),tiledStMan2);
    newtab.bindColumn(MS::columnName(MS::UVW),tiledStMan3);
    newtab.bindColumn(MS::columnName(MS::WEIGHT),tiledStMan4);
    newtab.bindColumn(MS::columnName(MS::SIGMA),tiledStMan5);

  // avoid lock overheads by locking the table permanently
    TableLock lock(TableLock::PermanentLocking);
    MeasurementSet *ms = new MeasurementSet (newtab,lock);

  // Set up the subtables for the UVFITS MS
  // we make new tables with 0 rows
    Table::TableOption option=Table::New;
    ms->createDefaultSubtables(option); 
  // add the optional Source sub table to allow for 
  // specification of the rest frequency
  TableDesc sourceTD=MSSource::requiredTableDesc();
  MSSource::addColumnToDesc(sourceTD, MSSource::REST_FREQUENCY);
  MSSource::addColumnToDesc(sourceTD, MSSource::SYSVEL);
  MSSource::addColumnToDesc(sourceTD, MSSource::TRANSITION);
  SetupNewTable sourceSetup(ms->sourceTableName(),sourceTD,option);
  ms->rwKeywordSet().defineTable(MS::keywordName(MS::SOURCE),
                                Table(sourceSetup,0));
  // update the references to the subtable keywords
  ms->initRefs();
 
  { // Set the TableInfo
    TableInfo& info(ms->tableInfo());
    info.setType(TableInfo::type(TableInfo::MEASUREMENTSET));
    info.setSubType(String("UVFITS"));
    info.readmeAddLine
      ("This is a measurement set Table holding astronomical observations");
  }


  return *ms;
}


Bool SubMS::fillDDTables(){

  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());

  //POLARIZATION table 
  MSPolarization poltable= mssel_p.polarization();
  ROArrayColumn<Int> corrType(poltable, 
			  MSPolarization::columnName(MSPolarization::CORR_TYPE));
  ROArrayColumn<Int> corrProd(poltable, MSPolarization::columnName(MSPolarization::CORR_PRODUCT));
  ROScalarColumn<Bool> polFlagRow(poltable, MSPolarization::columnName(MSPolarization::FLAG_ROW));
  
  //SPECTRAL_WINDOW table
  MSSpectralWindow spwtable=mssel_p.spectralWindow();
  spwRelabel_p.resize(mscIn_p->spectralWindow().nrow());
  spwRelabel_p.set(-1);

  ROArrayColumn<Double> chanFreq(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::CHAN_FREQ));
  ROArrayColumn<Double> chanWidth(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::CHAN_WIDTH));
  ROArrayColumn<Double> effBW(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::EFFECTIVE_BW));
  ROScalarColumn<Bool> spwFlagRow(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::FLAG_ROW));
  ROScalarColumn<Int> freqGroup(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::FREQ_GROUP));
  ROScalarColumn<String> freqGroupName(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::FREQ_GROUP_NAME));
  ROScalarColumn<Int> ifConvChain(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::IF_CONV_CHAIN));
  ROScalarColumn<Int> measFreqRef(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::MEAS_FREQ_REF));
  ROScalarColumn<String> spwName(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::NAME));
  ROScalarColumn<Int> netSideband(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::NET_SIDEBAND)); 
  ROScalarColumn<Int> numChan(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::NUM_CHAN));
  ROScalarColumn<Double> refFreq(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::REF_FREQUENCY));
  ROArrayColumn<Double> spwResol(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::RESOLUTION));
  ROScalarColumn<Double> totBW(spwtable, MSSpectralWindow::columnName(MSSpectralWindow::TOTAL_BANDWIDTH));
  for(uInt k=0; k < spw_p.nelements(); ++k){
    msOut_p.polarization().addRow();
    msOut_p.spectralWindow().addRow();
    msOut_p.dataDescription().addRow();
    msPol.numCorr().put(k,npol_p[k]);
    msPol.corrType().put(k,corrType(spw_p[k]));
    msPol.corrProduct().put(k,corrProd(spw_p[k]));
    msPol.flagRow().put(k,polFlagRow(spw_p[k]));
    spwRelabel_p[spw_p[k]]=k;
    if(nchan_p[k] != numChan(spw_p[k])){
      doChanAver_p=True; 
      Vector<Double> chanFreqOut(nchan_p[k]);
      Vector<Double> chanFreqIn= chanFreq(spw_p[k]);
      Vector<Double> spwResolOut(nchan_p[k]);
      Vector<Double> spwResolIn= spwResol(spw_p[k]);
      for(Int j=0; j < nchan_p[k]; ++j){
	if(averageChannel_p){
	  chanFreqOut[j]=(chanFreqIn[chanStart_p[k]+j*chanStep_p[k]]+
	    chanFreqIn[chanStart_p[k]+(j+1)*chanStep_p[k]-1])/2;
	  spwResolOut[j]= spwResolIn[chanStart_p[k]+ 
				    j*chanStep_p[k]]*chanStep_p[k];
	}
	else{
	  chanFreqOut[j]=chanFreqIn[chanStart_p[k]+j*chanStep_p[k]];
	  spwResolOut[j]=spwResolIn[chanStart_p[k]+ 
				   j*chanStep_p[k]];
	}
      }
      Double totalBW=chanFreqOut[nchan_p[k]-1]-chanFreqOut[0]+spwResolOut[0];
      msSpW.chanFreq().put(k, chanFreqOut);
      msSpW.resolution().put(k, spwResolOut);
      msSpW.numChan().put(k, nchan_p[k]);
      msSpW.chanWidth().put(k, spwResolOut);
      msSpW.effectiveBW().put(k, spwResolOut);
      msSpW.refFrequency().put(k,chanFreqOut[0]);
      msSpW.totalBandwidth().put(k, totalBW);

  
    }
    else{
      msSpW.chanFreq().put(k, chanFreq(spw_p[k]));
      msSpW.resolution().put(k, spwResol(spw_p[k]));
      msSpW.numChan().put(k, numChan(spw_p[k]));    
      msSpW.chanWidth().put(k, chanWidth(spw_p[k]));
      msSpW.effectiveBW().put(k, effBW(spw_p[k]));
      msSpW.refFrequency().put(k, refFreq(spw_p[k]));
      msSpW.totalBandwidth().put(k, totBW(spw_p[k]));
    }

    msSpW.flagRow().put(k,spwFlagRow(spw_p[k]));
    msSpW.freqGroup().put(k, freqGroup(spw_p[k]));
    msSpW.freqGroupName().put(k, freqGroupName(spw_p[k]));
    msSpW.ifConvChain().put(k, ifConvChain(spw_p[k]));
    msSpW.measFreqRef().put(k, measFreqRef(spw_p[k]));
    msSpW.name().put(k, spwName(spw_p[k]));
    msSpW.netSideband().put(k, netSideband(spw_p[k]));


    msDD.flagRow().put(k, False);
    msDD.polarizationId().put(k,k);
    msDD.spectralWindowId().put(k,k);

  }

 
    
  return True;

}


Bool SubMS::fillFieldTable() {


MSFieldColumns& msField(msc_p->field());

MSField fieldtable= mssel_p.field();

 ROScalarColumn<String> code(fieldtable, MSField::columnName(MSField::CODE));
 ROArrayColumn<Double> delayDir(fieldtable, MSField::columnName(MSField::DELAY_DIR));
 ROScalarColumn<Bool> flagRow(fieldtable, MSField::columnName(MSField::FLAG_ROW));
 ROScalarColumn<String> name(fieldtable, MSField::columnName(MSField::NAME));
 ROScalarColumn<Int> numPoly(fieldtable, MSField::columnName(MSField::NUM_POLY));
 ROArrayColumn<Double> phaseDir(fieldtable, MSField::columnName(MSField::PHASE_DIR));
 ROArrayColumn<Double> refDir(fieldtable, MSField::columnName(MSField::REFERENCE_DIR));
 ROScalarColumn<Int> sourceId(fieldtable, MSField::columnName(MSField::SOURCE_ID));
 ROScalarColumn<Double> time(fieldtable, MSField::columnName(MSField::TIME));
 

 fieldRelabel_p.resize(mscIn_p->field().nrow());
 fieldRelabel_p.set(-1);


 for(uInt k=0; k < fieldid_p.nelements(); ++k){
   fieldRelabel_p[fieldid_p[k]]=k;
   msOut_p.field().addRow();
   
   msField.code().put(k,code(fieldid_p[k]));
   msField.delayDir().put(k,delayDir(fieldid_p[k]));
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

  LogIO os(LogOrigin("SubMS", "fillMainTable()", WHERE));

  msOut_p.addRow(mssel_p.nrow(), True);
  
  msc_p->antenna1().putColumn(mscIn_p->antenna1());
  msc_p->antenna2().putColumn(mscIn_p->antenna2());
  msc_p->arrayId().putColumn(mscIn_p->arrayId());
  msc_p->exposure().putColumn(mscIn_p->exposure());
  msc_p->feed1().putColumn(mscIn_p->feed1());
  msc_p->feed2().putColumn(mscIn_p->feed2());
  //  msc_p->flag().putColumn(mscIn_p->flag());
  msc_p->flagCategory().putColumn(mscIn_p->flagCategory());
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



  {
    //relabel data_desc_id  and field_id
    Vector<Int> datDesc = mscIn_p->dataDescId().getColumn();
    Vector<Int> fieldId = mscIn_p->fieldId().getColumn();
    for (uInt k = 0; k < datDesc.nelements(); ++k){
      
      datDesc[k]=spwRelabel_p[datDesc[k]];
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
    Int rowsdone=0;
    Int rowsnow=0;
    Block<Int> sort(4);
    sort[0] = MS::FIELD_ID;
    sort[1] = MS::ARRAY_ID;
    sort[2] = MS::DATA_DESC_ID;
    sort[3] = MS::TIME;
    Matrix<Int> noselection;
    VisSet *vs= new VisSet(mssel_p, sort, noselection);
    ROVisIter& vi(vs->iter());
    VisBuffer vb(vi);

    for (vi.originChunks();vi.moreChunks();vi.nextChunk()) {
      for (vi.origin(); vi.more(); vi++) {
	rowsnow=vb.nRow();
	RefRows rowstoadd(rowsdone, rowsdone+rowsnow-1);
	Int spw=vb.spectralWindow();
	Cube<Complex> vis;
	if(columnName== MS::columnName(MS::DATA))
	  vis.reference(vb.visCube());
	else if(columnName == "MODEL_DATA" )
	  vis.reference(vb.modelVisCube());
	else
	  vis.reference(vb.correctedVisCube());

	Cube<Bool> inFlag=vb.flagCube();
	Cube<Complex> averdata(npol_p[spw], nchan_p[spw], rowsnow);
	Cube<Bool> locflag(npol_p[spw], nchan_p[spw], rowsnow);
	for(Int k=0; k < rowsnow; ++k){
	  for(Int j=0; j < nchan_p[spw]; ++j){
	    if(!averageChannel_p){
	      averdata.xyPlane(k).column(j)=
		vis.xyPlane(k).column(chanStart_p[spw]+ j*chanStep_p[spw]); 
	      locflag.xyPlane(k).column(j)=
		inFlag.xyPlane(k).column(chanStart_p[spw]+ j*chanStep_p[spw]); 
	    }else{
	      Vector<Bool> tryFlag(npol_p[spw]);
	      Vector<Complex> avervis(npol_p[spw]);
	      avervis=0;
	      tryFlag=False;
	      for (Int m=0; m < chanStep_p[spw]; ++m){
		avervis=avervis+vis.xyPlane(k).column(chanStart_p[spw]+ 
						      j*chanStep_p[spw]+m);
		for (Int pol=0; pol < npol_p[spw]; ++pol){
		  tryFlag[pol]=tryFlag[pol] | 
		    inFlag.xyPlane(k).column(chanStart_p[spw]+ 
					     j*chanStep_p[spw]+m)[pol];
		  
		}

	      }
	      averdata.xyPlane(k).column(j)=avervis/chanStep_p[spw];
	      locflag.xyPlane(k).column(j)=tryFlag;
	      
	    }
	  }
	}
	rowsdone+=rowsnow;
	msc_p->data().putColumnCells(rowstoadd, averdata);
	msc_p->flag().putColumnCells(rowstoadd, locflag);

      }
    }

  }

  return True;


}


Bool SubMS::copyAntenna(){

  Table oldAnt(mssel_p.antennaTableName(), Table::Old);
  Table newAnt(msOut_p.antennaTableName(), Table::New);
  TableCopy::copyRows(newAnt, oldAnt);


  return True;

}

Bool SubMS::copyFeed(){

  Table oldFeed(mssel_p.feedTableName(), Table::Old);
  Table newFeed(msOut_p.feedTableName(), Table::New);
  TableCopy::copyRows(newFeed, oldFeed);


  return True;

}

Bool SubMS::copyObservation(){

  Table oldObs(mssel_p.observationTableName(), Table::Old);
  Table newObs(msOut_p.observationTableName(), Table::New);
  TableCopy::copyRows(newObs, oldObs);


  return True;

}
