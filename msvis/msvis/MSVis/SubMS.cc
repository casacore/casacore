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
  
  SubMS::SubMS(String& theMS){
    
    ms_p=MeasurementSet(theMS, Table::Update);
    mssel_p=ms_p;
    doChanAver_p=False;
    antennaSel_p=False;
    sameShape_p=True;
    timeBin_p=-1.0;
    scanString_p="";
    uvrangeString_p="";
    taqlString_p="";
  }
  
  SubMS::SubMS(MeasurementSet& ms){
    
    
    ms_p=ms;
    mssel_p=ms_p;
    doChanAver_p=False;
    antennaSel_p=False;
    sameShape_p=True;
    timeBin_p=-1.0;
    scanString_p="";
    uvrangeString_p="";
    taqlString_p="";

  }

  SubMS::~SubMS(){
    if(!msOut_p.isNull())
      msOut_p.flush();
    msOut_p=MeasurementSet();
  }


  void SubMS::selectSpw(Vector<Int> spw, Vector<Int> nchan, Vector<Int> start, 
			 Vector<Int> step, const Bool averchan) {

    
    spw_p.resize();
    spw_p=spw;
    
    //check for default
    if(spw_p.nelements() == 1 && spw_p[0] < 0){
      spw_p.resize(ms_p.spectralWindow().nrow());
      for (uInt k =0 ; k < spw_p.nelements() ; ++k){
	spw_p[k]=k;
      
      }
      //no may be we have to redo the chan selection

      if (nchan.nelements() != spw_p.nelements()){
	nchan.resize(spw_p.nelements(), True);
	for(uInt k=1; k < spw_p.nelements(); ++k){
	  nchan[k]=nchan[0];
	}
      }
      if (start.nelements() != spw_p.nelements()){
	start.resize(spw_p.nelements(), True);
	for(uInt k=1; k < spw_p.nelements(); ++k){
	  start[k]=start[0];
	}
      }
      if (step.nelements() != spw_p.nelements()){
	step.resize(spw_p.nelements(), True);
	for(uInt k=1; k < spw_p.nelements(); ++k){
	  step[k]=step[0];
	}
      }
    }
 
    
    nchan_p.resize();
    nchan_p=nchan;
    chanStart_p.resize();
    chanStart_p=start;
    chanStep_p.resize();
    chanStep_p=step;
    averageChannel_p=averchan;
    // check for defaults
    if(nchan_p[0]<=0 || (nchan_p.nelements() != spw_p.nelements())){
      nchan_p.resize(spw_p.nelements());
      ROMSSpWindowColumns mySpwTab(ms_p.spectralWindow());
      for (uInt k =0; k < spw_p.nelements(); ++k){
	if(nchan[0]<=0)
	  nchan_p[k]=mySpwTab.numChan()(spw_p[k]);
	else
	  nchan_p[k]=nchan[0];
      }
      chanStart_p.resize(spw_p.nelements());
      chanStep_p.resize(spw_p.nelements());
      if(chanStart_p.nelements() == start.nelements()){
	chanStart_p=start;
      }
      else{
	chanStart_p.set(start[0]);
      }
      if(chanStep_p.nelements() == step.nelements()){
	chanStep_p=step;
      }
      else{
	chanStep_p.set(step[0]);
      }
      

    }


  }

  void SubMS::setmsselect(const String& spw, const String& field, 
			  const String& baseline, 
			  const String& scan, const String& uvrange, 
			  const String& taql, const Vector<Int>& nchan, 
			  const Vector<Int>& start, const Vector<Int>& step,
			  const Bool averchan){
    Vector<Int> inchan(1,-1);
    Vector<Int> istart(1,0);
    Vector<Int> istep(1,1);
    Record selrec=ms_p.msseltoindex(spw, field);
    Vector<Int>fldids=selrec.asArrayInt("field");
    Vector<Int>spwids=selrec.asArrayInt("spw");
    if(fldids.nelements() < 1){
      fldids=Vector<Int>(1,-1);
    }
    selectSource(fldids);
    if(spwids.nelements() < 1){
      spwids=Vector<Int>(1,-1);
    }
    //use nchan f defined else use caret-column syntax of  msselection 
    if((nchan.nelements()>0) && nchan[0] > 0){
      inchan.resize(); inchan=nchan;
      istep.resize(); istep=step;
      istart.resize(); istart=start;
    }
    else{
      Matrix<Int> chansel=selrec.asArrayInt("channel");
      if(chansel.nelements() !=0){
	inchan.resize(chansel.nrow());
	istep.resize(chansel.nrow());
	istart.resize(chansel.nrow());
	// if the vector step is used ..for averaging ..let's use it
	Bool stepused=False;
	if( (step.nelements() >= 1) && (max(step) > 1))
	    stepused=True;
	for (uInt k =0 ; k < chansel.nrow(); ++k){
	  if(stepused){
	    if(step.nelements()==1)
	      istep[k]=step[0];
	    else if(step.nelements()==istep.nelements())
	      istep[k]=step[k];
	    else //confused at this stage
	      istep[k]=1;
	  }
	  else{
	    istep[k]=chansel.row(k)(3);
	    if(istep[k] < 1)
	      istep[k]=1;
	  }
	  istart[k]=chansel.row(k)(1);
	  inchan[k]=(chansel.row(k)(2)-istart[k]+1)/istep[k];
	  if(inchan[k]<1)
	    inchan[k]=1;	  
	}
      } 
    }
    selectSpw(spwids, inchan, istart, istep, averchan);
  
    if(baseline != ""){
      Vector<Int> antid(0);
      Vector<String> antstr(1,baseline);
      selectAntenna(antid, antstr);
    }
    scanString_p=scan;
    uvrangeString_p=uvrange;
    taqlString_p=taql;

  }

  void SubMS::selectSource(Vector<Int> fieldid){
    
    
    fieldid_p.resize();
    fieldid_p=fieldid;
    if(fieldid.nelements()==1 && fieldid(0)<0){
      fieldid_p.resize(ms_p.field().nrow());
      for (uInt k =0 ; k < fieldid_p.nelements() ; ++k){
	fieldid_p[k]=k;
      
      }
    }

  }
  
 
  void SubMS::selectAntenna(Vector<Int>& antennaids, Vector<String>& antennaSel){
    if((antennaids.nelements()==1) && (antennaids[0]=-1) && antennaSel[0]==""){
      antennaSel_p=False;
      return;
    }

    antennaSel_p=True;
    if((antennaids.nelements()==1) && (antennaids[0]=-1))
      antennaId_p.resize();
    else
      antennaId_p=antennaids;
    antennaSelStr_p=antennaSel;

  }
 
  void SubMS::selectTime(Double timeBin, String timerng){

    timeBin_p=timeBin;
    timeRange_p=timerng;
  }


  Bool SubMS::makeSubMS(String& msname, String& colname){
    
    LogIO os(LogOrigin("SubMS", "makeSubMS()"));
    
    if(max(fieldid_p) >= Int(ms_p.field().nrow())){
      os << LogIO::SEVERE 
	 << "Field selection contains elements that do not exist in "
	 << "this MS"
	 << LogIO::POST;
      ms_p=MeasurementSet();
      return False;
      
      
    }
    if(max(spw_p) >= Int(ms_p.spectralWindow().nrow())){
      os << LogIO::SEVERE 
	 << "SpectralWindow selection contains elements that do not exist in "
	 << "this MS"
	 << LogIO::POST;
      ms_p=MeasurementSet();
      return False;
      
      
    }
    
    if(!makeSelection()){
      os << LogIO::SEVERE 
	 << "Failed on selection: combination of spw and/or field and/or time chosen may be"
	 << " invalid" 
	 << LogIO::POST;
      ms_p=MeasurementSet();
      return False;
    }
    mscIn_p=new MSColumns(mssel_p);
    MeasurementSet* outpointer=setupMS(msname, nchan_p[0], npol_p[0],  
				       mscIn_p->observation().telescopeName()(0));

    msOut_p= *outpointer;
    msc_p=new MSColumns(msOut_p);

    if(!fillAllTables(colname)){
      delete outpointer;
      ms_p=MeasurementSet();
      return False;

    }


    //  msOut_p.relinquishAutoLocks (True);
    //  msOut_p.unlock();
    //Detaching the selected part
    ms_p=MeasurementSet();
    delete outpointer;
    return True;
    
  }


  MeasurementSet* SubMS::makeScratchSubMS(String& colname, Bool forceInMemory){
    
    LogIO os(LogOrigin("SubMS", "makeSubMS()"));
    
    if(max(fieldid_p) >= Int(ms_p.field().nrow())){
      os << LogIO::SEVERE 
	 << "Field selection contains elements that do not exist in "
	 << "this MS"
	 << LogIO::POST;
      ms_p=MeasurementSet();
      return 0;
      
      
    }
    if(max(spw_p) >= Int(ms_p.spectralWindow().nrow())){
      os << LogIO::SEVERE 
	 << "SpectralWindow selection contains elements that do not exist in "
	 << "this MS"
	 << LogIO::POST;
      ms_p=MeasurementSet();
      return 0;
      
      
    }
    
    if(!makeSelection()){
      os << LogIO::SEVERE 
	 << "Failed on selection: combination of spw and/or field and/or time chosen may be"
	 << " invalid" 
	 << LogIO::POST;
      ms_p=MeasurementSet();
      return 0;
    }
    mscIn_p=new MSColumns(mssel_p);
    Double sizeInMB= 1.5*mssel_p.nrow()*nchan_p[0]*npol_p[0]*sizeof(Complex)/1024.0/1024.0;
    String msname=AppInfo::workFileName(uInt(sizeInMB), "TempSubMS");

    MeasurementSet* outpointer=setupMS(msname, nchan_p[0], npol_p[0],  
				       mscIn_p->observation().telescopeName()(0));

    outpointer->markForDelete();
    //Hmmmmmm....memory...... 
    if(sizeInMB <  (Double)(HostInfo::memoryTotal())/(2048.0) 
       || forceInMemory){
      MeasurementSet* a = outpointer;
      outpointer= new MeasurementSet(a->copyToMemoryTable("TmpMemoryMS"));
      outpointer->initRefs();
      delete a;
    }

    msOut_p= *outpointer;
    msc_p=new MSColumns(msOut_p);

    if(!fillAllTables(colname)){
      delete outpointer;
      outpointer=0;
      ms_p=MeasurementSet();
      return 0;

    }

    //Detaching the selected part
    ms_p=MeasurementSet();
    return outpointer;
    
  }



  Bool SubMS::fillAllTables(const String& colname){

    LogIO os(LogOrigin("SubMS", "makeSubMS()"));

    // fill or update
    if(!fillDDTables()){
      return False;
      
    }
    fillFieldTable();
    copySource();
    copyAntenna();
    copyFeed();    // Feed table writing has to be after antenna 
    copyObservation();
    copyPointing();

    
    //check the spw shapes
    checkSpwShape();

    if(timeBin_p <= 0.0){
      fillMainTable(colname);
    }
    else{
      if(!sameShape_p){
	os << LogIO::WARN 
	   << "Time averaging of varying spw shapes is not handled yet"
	   << LogIO::POST;
	os << LogIO::WARN
	   << "Work around: split-average different shape spw seperately and then concatenate " << LogIO::POST;
	return False;
      }      
      else{
	fillAverMainTable(colname);

      }
    }

    return True;

  }
  
  
  Bool SubMS::makeSelection(){
    
    LogIO os(LogOrigin("SubMS", "makeSelection()"));
    
    //VisSet/MSIter will check if the SORTED exists
    //and resort if necessary
    {
      Matrix<Int> noselection;
      VisSet vs(ms_p,noselection);
    }
    const MeasurementSet sorted=ms_p.keywordSet().asTable("SORTED_TABLE");
    
    MSSelection thisSelection;
    if(fieldid_p.nelements() > 0)
      thisSelection.setFieldExpr(MSSelection::indexExprStr(fieldid_p));
    if(spw_p.nelements() > 0)
      thisSelection.setSpwExpr(MSSelection::indexExprStr(spw_p));
    if(antennaSel_p){
      if(antennaId_p.nelements() >0){
	thisSelection.setAntennaExpr( MSSelection::indexExprStr( antennaId_p ));
      }
      if(antennaSelStr_p[0] != ""){
        thisSelection.setAntennaExpr(MSSelection::nameExprStr( antennaSelStr_p));


      }
      
    }
    if(timeRange_p != ""){
      thisSelection.setTimeExpr(timeRange_p);
    }

    thisSelection.setUvDistExpr(uvrangeString_p);
    thisSelection.setScanExpr(scanString_p);
    thisSelection.setTaQLExpr(taqlString_p);

    TableExprNode exprNode=thisSelection.toTableExprNode(&sorted);
    
    {
      
      MSDataDescription ddtable=ms_p.dataDescription();
      ROScalarColumn<Int> polId(ddtable, 
				MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID));
      MSPolarization poltable= ms_p.polarization();
      ROArrayColumn<Int> pols(poltable, 
			      MSPolarization::columnName(MSPolarization::CORR_TYPE));
      
      npol_p.resize(spw_p.shape()); 
      for (uInt k=0; k < npol_p.nelements(); ++k){  
	npol_p[k]=pols(polId(spw_p[k])).nelements();
      }
    }

    // Now remake the selected ms
    if(!(exprNode.isNull())){
      mssel_p =  MeasurementSet(sorted(exprNode));
    }
    else{
      // Null take all the ms ...setdata() blank means that
      mssel_p = MeasurementSet(sorted);
    }
    //mssel_p.rename(ms_p.tableName()+"/SELECTED_TABLE", Table::Scratch);
    if(mssel_p.nrow()==0){
      return False;
    }
    if(mssel_p.nrow() < ms_p.nrow()){
      os << LogIO::NORMAL
	 << mssel_p.nrow() << " rows are going to be considered out of " 
	 << ms_p.nrow() << " rows due to the selection criteria " 
	 << LogIO::POST;
    }
    return True;
    
  }

  MeasurementSet* SubMS::setupMS(String MSFileName, Int nchan, Int nCorr, 
				 String telescop, Int obsType ){

  

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
    TableLock lock(TableLock::AutoLocking);
    MeasurementSet *ms = new MeasurementSet (newtab,lock);

    // Set up the subtables for the UVFITS MS
    // we make new tables with 0 rows
    Table::TableOption option=Table::New;
    ms->createDefaultSubtables(option); 
    // add the optional Source sub table to allow for 
    // specification of the rest frequency
    TableDesc sourceTD=MSSource::requiredTableDesc();
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
    
    
    return ms;
  }


  Bool SubMS::fillDDTables(){
    
    LogIO os(LogOrigin("SubMS", "fillDDTables()"));
    
    MSSpWindowColumns& msSpW(msc_p->spectralWindow());
    MSDataDescColumns& msDD(msc_p->dataDescription());
    MSPolarizationColumns& msPol(msc_p->polarization());
    


  //DD table
    MSDataDescription ddtable= mssel_p.dataDescription();
    ROScalarColumn<Int> polId(ddtable, 
			      MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID));
    
    //Fill in matching spw to datadesc in old ms 
  {
    ROMSDataDescColumns msOldDD(ddtable);
    oldDDSpwMatch_p=msOldDD.spectralWindowId().getColumn();
  }
  //POLARIZATION table 
  
  
  MSPolarization poltable= mssel_p.polarization();
  ROScalarColumn<Int> numCorr (poltable, 
			       MSPolarization::columnName(MSPolarization::NUM_CORR));
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
  inNumChan_p.resize(spw_p.nelements()); 

  Vector<Int> ddPolId=polId.getColumn();
  Bool dum;
  Sort sort( ddPolId.getStorage(dum),sizeof(Int) );
  sort.sortKey((uInt)0,TpInt);
  Vector<uInt> index,uniq;
  sort.sort(index,ddPolId.nelements());
  uInt nPol = sort.unique(uniq,index);
  Vector<Int> selectedPolId(nPol);
  for(uInt k=0; k < nPol; ++k){
    selectedPolId[k]=ddPolId[index[uniq[k]]];
  }
  
  Vector<Int> newPolId(spw_p.nelements());
  for(uInt k=0; k < spw_p.nelements(); ++k){
    for (uInt j=0; j < nPol; ++j){ 
      if(selectedPolId[j]==ddPolId[k])
	newPolId[k]=j;
    }
  }
  
  for(uInt k=0; k < newPolId.nelements(); ++k){
    msOut_p.polarization().addRow();
    msPol.numCorr().put(k,numCorr(polId(spw_p[k])));
    msPol.corrType().put(k,corrType(polId(spw_p[k])));
    msPol.corrProduct().put(k,corrProd(polId(spw_p[k])));
    msPol.flagRow().put(k,polFlagRow(polId(spw_p[k])));
  }
  for(uInt k=0; k < spw_p.nelements(); ++k){
    inNumChan_p[k]=numChan(spw_p[k]);
    msOut_p.spectralWindow().addRow();
    msOut_p.dataDescription().addRow();
    spwRelabel_p[spw_p[k]]=k;
    if(nchan_p[k] != numChan(spw_p[k])){
      Int totchan=nchan_p[k]*chanStep_p[k]+chanStart_p[k];
      if(totchan >  numChan(spw_p[k])){
	os << LogIO::SEVERE
	   << " Channel settings wrong; exceeding number of channels in spw "
	   << spw_p[k]+1 << LogIO::POST;
	return False;
      }
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
    msDD.polarizationId().put(k,newPolId[k]);
    msDD.spectralWindowId().put(k,k);
    
    
  }
  
 
  
  return True;
  
  }
  

} //#End casa namespace
