//# MS1ToMS2Converter.cc: MS1 to MS2 converter
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

#include <casacore/ms/MSOper/MS1ToMS2Converter.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSTableImpl.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/TableMeasDesc.h>
#include <casacore/measures/TableMeasures/TableMeasValueDesc.h>
#include <casacore/measures/TableMeasures/TableQuantumDesc.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

MS1ToMS2Converter::MS1ToMS2Converter(const String& ms2,
				     const String& ms1,
				     Bool inPlace)
: ms1_p    (ms1),
  ms2_p    (ms2),
  inPlace_p(inPlace)
{
  LogOrigin OR("MS1ToMS2Converter", "MS1ToMS2Converter()", WHERE);
  os_p = LogIO(OR);
  if (inPlace_p) {
    ms2_p = ms1_p;
  }
}

MS1ToMS2Converter::~MS1ToMS2Converter() 
{}

void MS1ToMS2Converter::removeColumn(Table& t, const String& col)
{
  if (t.canRemoveColumn(col)) {
    //    t.removeColumn(col);
    t.renameColumn("_OBSOLETE_"+col,col);
  } else {
    t.renameColumn("_OBSOLETE_"+col,col);
  }
}

Bool MS1ToMS2Converter::convert()
{
  // Check that table needs to be converted, if so (deep)copy it if needed.
  {
    Table t(ms1_p);
    if (t.keywordSet().isDefined("MS_VERSION") &&
        t.keywordSet().asFloat("MS_VERSION")>=2.0) {
      throw(AipsError("Input MS already in MS2 format"));
    }
    if (!inPlace_p) t.copy(ms2_p,Table::NewNoReplace);
  }

  // Fix the main table, rename columns, add columns, remove columns
  Table t;
  if (inPlace_p) {
    t = Table(ms1_p, Table::Update);
  } else {
    t = Table(ms2_p,Table::Update);
  }
  t.rwKeywordSet().define("MS_VERSION", Float(2.0));

  t.renameColumn("DATA_DESC_ID","SPECTRAL_WINDOW_ID");

  t.renameColumn("PROCESSOR_ID","CORRELATOR_ID");

  if (t.keywordSet().isDefined("FLAG_HISTORY")) {
    t.renameColumn("FLAG_CATEGORY","FLAG_HISTORY");
    TableColumn flagc(t, "FLAG_CATEGORY");
    if (!flagc.keywordSet().isDefined("CATEGORY"))
      flagc.rwKeywordSet().define("CATEGORY", Vector<String>());
  }

  removeColumn(t,"PULSAR_ID");
  
  t.rwKeywordSet().removeField("ARRAY");

  if (t.keywordSet().isDefined("SORTED_TABLE"))
    t.rwKeywordSet().removeField("SORTED_TABLE");

  if (t.keywordSet().isDefined("SORT_COLUMNS"))
    t.rwKeywordSet().removeField("SORT_COLUMNS");
  
  TableDesc td;
  IncrementalStMan ism;
  MeasurementSet::addColumnToDesc(td,MS::PHASE_ID);
  MeasurementSet::addColumnToDesc(td,MS::STATE_ID);
  MeasurementSet::addColumnToDesc(td,MS::TIME_CENTROID);
  if (!(t.keywordSet().isDefined("FLAG_CATEGORY")))
    MeasurementSet::addColumnToDesc(td,MS::FLAG_CATEGORY);
  t.addColumn(td,ism);

  TableColumn flagc(t, "FLAG_CATEGORY");
  if (!flagc.keywordSet().isDefined("CATEGORY")) {
    flagc.rwKeywordSet().define("CATEGORY", Vector<String>());
  }


  ScalarColumn<Int> stateId(t,MS::columnName(MS::STATE_ID));
  stateId.fillColumn(0);
  ScalarColumn<Double> time(t,MS::columnName(MS::TIME));
  ScalarColumn<Double> timeCentroid(t,MS::columnName(MS::TIME_CENTROID));
  timeCentroid.putColumn(time.getColumn());

  
  ArrayColumn<Double> uvw(t,MS::columnName(MS::UVW));
  TableDesc uvwtd;
  MeasurementSet::addColumnToDesc(uvwtd,MS::UVW);
  uvw.rwKeywordSet().assign(uvwtd[0].keywordSet());
  
  ScalarColumn<Double> exp(t,MS::columnName(MS::EXPOSURE));
  TableDesc exptd;
  MeasurementSet::addColumnToDesc(exptd,MS::EXPOSURE);
  exp.rwKeywordSet().assign(exptd[0].keywordSet());

  ScalarColumn<Double> inter(t,MS::columnName(MS::INTERVAL));
  TableDesc intertd;
  MeasurementSet::addColumnToDesc(intertd,MS::INTERVAL);
  inter.rwKeywordSet().assign(intertd[0].keywordSet());

  TableDesc timetd;
  MeasurementSet::addColumnToDesc(timetd,MS::TIME);
  time.rwKeywordSet().assign(timetd[0].keywordSet());

  Int maxAnt;

  // ANTENNA
  {
    Table anTab(ms2_p+"/ANTENNA",Table::Update);
    // Find out if we need to renumber antennas
    ScalarColumn<Int> antIdCol(anTab, "ANTENNA_ID");
    Vector<Int> ant=antIdCol.getColumn();
    Int nRow=ant.nelements();
    maxAnt=max(ant)+1;
    Vector<Int> antMap(maxAnt);
    Bool renumber=False;
    for (Int i=0; i<nRow; i++) {
      if (i!=ant(i)) renumber=True;
      antMap(ant(i))=i;
    }

    if (renumber) {
      // cout <<"Renumbering antennas in main table and all subtables"<<endl;
      // Main
      {
        Vector<Int> newAnt1(t.nrow());
        Vector<Int> newAnt2(t.nrow());
        ScalarColumn<Int> ant1Col(t,"ANTENNA1");
        ScalarColumn<Int> ant2Col(t,"ANTENNA2");
        for (uInt i=0; i<t.nrow(); i++) {
          newAnt1(i)=antMap(ant1Col(i));
          newAnt2(i)=antMap(ant2Col(i));
        }
        ant1Col.putColumn(newAnt1);
        ant2Col.putColumn(newAnt2);
      }
      // Feed
      {
        Table feedTab(ms2_p+"/FEED",Table::Update);
        ScalarColumn<Int> ant(feedTab,"ANTENNA_ID");
        for (uInt i=0; i<feedTab.nrow(); i++) {
          ant.put(i,antMap(ant(i)));
        }
        removeColumn(feedTab,"ARRAY_ID");
      }
      // Syscal
      if (Table::isReadable(ms2_p+"/SYSCAL")) {
        Table syscalTab(ms2_p+"/SYSCAL",Table::Update);
        ScalarColumn<Int> ant(syscalTab,"ANTENNA_ID");
        for (uInt i=0; i<syscalTab.nrow(); i++) {
          ant.put(i,antMap(ant(i)));
        }
        removeColumn(syscalTab,"ARRAY_ID");
      }
      // Weather
      if (Table::isReadable(ms2_p+"/WEATHER")) {
        Table wTab(ms2_p+"/WEATHER",Table::Update);
        ScalarColumn<Int> ant(wTab,"ANTENNA_ID");
        for (uInt i=0; i<wTab.nrow(); i++) {
          ant.put(i,antMap(ant(i)));
        }
        removeColumn(wTab,"ARRAY_ID");
      }
    }

    // remove columns
    removeColumn(anTab,"ARRAY_ID");
    removeColumn(anTab,"ANTENNA_ID");

    TableDesc td;
    MSAntenna::addColumnToDesc(td,MSAntenna::TYPE);
    MSAntenna::addColumnToDesc(td,MSAntenna::FLAG_ROW);
    anTab.addColumn(td[0]);
    anTab.addColumn(td[1]);
    ScalarColumn<String> type(anTab,"TYPE");
    type.fillColumn("GROUND-BASED");
    ScalarColumn<Bool> flagRow(anTab,"FLAG_ROW");
    flagRow.fillColumn(False);


    ArrayColumn<Double> pos(anTab,"POSITION");
    TableDesc postd;
    MSAntenna::addColumnToDesc(postd,MSAntenna::POSITION);
    pos.rwKeywordSet().assign(postd[0].keywordSet());

    ArrayColumn<Double> offs(anTab,"OFFSET");
    TableDesc offstd;
    MSAntenna::addColumnToDesc(offstd,MSAntenna::OFFSET);
    offs.rwKeywordSet().assign(offstd[0].keywordSet());
  
    ScalarColumn<Double> dish(anTab,"DISH_DIAMETER");
    TableDesc dishtd;
    MSAntenna::addColumnToDesc(dishtd,MSAntenna::DISH_DIAMETER);
    dish.rwKeywordSet().assign(dishtd[0].keywordSet());

    for (uInt j = MSAntenna::NUMBER_REQUIRED_COLUMNS + 1;
	 j < MSAntenna::NUMBER_PREDEFINED_COLUMNS; j = j + 1) {
      MSAntenna::PredefinedColumns i = (MSAntenna::PredefinedColumns) j;
      if (anTab.tableDesc().isColumn(MSAntenna::columnName(i))) {
	TableColumn tbc(anTab, MSAntenna::columnName(i));
	TableDesc td;
	MSAntenna::addColumnToDesc(td, i);
	tbc.rwKeywordSet().assign(td[0].keywordSet());
      }
    }
  }

  // DATA_DESCRIPTION
  {
    // create and fill table and write it out 
    {
      Int nRow = t.keywordSet().asTable("SPECTRAL_WINDOW").nrow();
      SetupNewTable ddSetup(ms2_p+"/DATA_DESCRIPTION",
			    MSDataDescription::requiredTableDesc(),
			    Table::New);
      Table ddt(ddSetup,nRow);
      ScalarColumn<Int> spw(ddt,"SPECTRAL_WINDOW_ID");
      ScalarColumn<Int> pol(ddt,"POLARIZATION_ID");
      ScalarColumn<Bool> flagRow(ddt,"FLAG_ROW");
      Vector<Int> seq(nRow);
      for (Int i=0;i<nRow;i++) seq(i)=i;
      spw.putColumn(seq);
      pol.putColumn(seq);
      flagRow.fillColumn(False);
    }
    Table ddt(ms2_p+"/DATA_DESCRIPTION");
    t.rwKeywordSet().
      defineTable(MS::keywordName(MS::DATA_DESCRIPTION),ddt);
  }
  
  // FEED
  {  
    Table feedTab(ms2_p+"/FEED",Table::Update);
    ScalarColumn<Double> time(feedTab,"TIME");
    TableDesc timetd;
    MSFeed::addColumnToDesc(timetd,MSFeed::TIME);
    time.rwKeywordSet().assign(timetd[0].keywordSet());
    
    ArrayColumn<Double> pos(feedTab,"POSITION");
    TableDesc postd;
    MSFeed::addColumnToDesc(postd,MSFeed::POSITION);
    pos.rwKeywordSet().assign(postd[0].keywordSet());

    ArrayColumn<Double> beam(feedTab,"BEAM_OFFSET");
    TableDesc beamtd;
    MSFeed::addColumnToDesc(beamtd,MSFeed::BEAM_OFFSET);
    beam.rwKeywordSet().assign(beamtd[0].keywordSet());

    ArrayColumn<Double> recep(feedTab,"RECEPTOR_ANGLE");
    TableDesc receptd;
    MSFeed::addColumnToDesc(receptd,MSFeed::RECEPTOR_ANGLE);
    recep.rwKeywordSet().assign(receptd[0].keywordSet());

    ScalarColumn<Double> inter(feedTab,"INTERVAL");
    TableDesc intertd;
    MSFeed::addColumnToDesc(intertd,MSFeed::INTERVAL);
    inter.rwKeywordSet().assign(intertd[0].keywordSet());
    
  }

  // FIELD
  {
    Table fldTab(ms2_p+"/FIELD",Table::Update);
    removeColumn(fldTab,"FIELD_ID");

    Matrix<Double> dd,ddr,pd,pdr,rd,rdr,pntd,pntdr;
    uInt pdtp, rdtp, ddtp;
    {
      ROArrayColumn<Double> delDir(fldTab,"DELAY_DIR");
      ROArrayColumn<Double> delDirRate(fldTab,"DELAY_DIR_RATE");
      ROArrayColumn<Double> phaseDir(fldTab,"PHASE_DIR");
      ROArrayColumn<Double> phaseDirRate(fldTab,"PHASE_DIR_RATE");
      ROArrayColumn<Double> pointingDir(fldTab,"POINTING_DIR");
      ROArrayColumn<Double> pointingDirRate(fldTab,"POINTING_DIR_RATE");
      ROArrayColumn<Double> refDir(fldTab,"REFERENCE_DIR");
      ROArrayColumn<Double> refDirRate(fldTab,"REFERENCE_DIR_RATE");
      dd=delDir.getColumn();
      ddr=delDirRate.getColumn();
      pd=phaseDir.getColumn();
      pdr=phaseDirRate.getColumn();
      pntd=pointingDir.getColumn();
      pntdr=pointingDirRate.getColumn();
      rd=refDir.getColumn();
      rdr=refDirRate.getColumn();

      MDirection::Types tp;
      MDirection::getType(tp, phaseDir.keywordSet().asString("MEASURE_REFERENCE")); 
      pdtp = tp;
      MDirection::getType(tp, refDir.keywordSet().asString("MEASURE_REFERENCE")); 
      rdtp = tp;
      MDirection::getType(tp, delDir.keywordSet().asString("MEASURE_REFERENCE")); 
      ddtp = tp;
    }
    IPosition shape(2,2,2);
    Int numPol = 1;
    if (allEQ(pntdr,0.0) && allEQ(ddr,0.0) && 
	allEQ(pdr,0.0) && allEQ(rdr,0.0)) {
      // all rates are zero, use only one term
      shape(1)=1;
      numPol=0;
    }
    
    removeColumn(fldTab,"DELAY_DIR");
    removeColumn(fldTab,"DELAY_DIR_RATE");
    removeColumn(fldTab,"PHASE_DIR");
    removeColumn(fldTab,"PHASE_DIR_RATE");
    removeColumn(fldTab,"REFERENCE_DIR");
    removeColumn(fldTab,"REFERENCE_DIR_RATE");
    removeColumn(fldTab,"POINTING_DIR");
    removeColumn(fldTab,"POINTING_DIR_RATE");
    TableDesc td;
    MSField::addColumnToDesc(td, MSField::DELAY_DIR,2);
    MSField::addColumnToDesc(td, MSField::PHASE_DIR,2);
    MSField::addColumnToDesc(td, MSField::REFERENCE_DIR,2);
    MSField::addColumnToDesc(td, MSField::NUM_POLY);
    MSField::addColumnToDesc(td, MSField::FLAG_ROW);
    fldTab.addColumn(td[0]);
    fldTab.addColumn(td[1]);
    fldTab.addColumn(td[2]);
    fldTab.addColumn(td[3]);
    fldTab.addColumn(td[4]);

    ArrayMeasColumn<MDirection> delAmc(fldTab, "DELAY_DIR");
    ArrayMeasColumn<MDirection> phaseAmc(fldTab, "PHASE_DIR");
    ArrayMeasColumn<MDirection> refAmc(fldTab, "REFERENCE_DIR");
    delAmc.setDescRefCode(ddtp, False);
    phaseAmc.setDescRefCode(pdtp, False);
    refAmc.setDescRefCode(rdtp, False);

    ArrayColumn<Double> delDir(fldTab,"DELAY_DIR");
    ArrayColumn<Double> phaseDir(fldTab,"PHASE_DIR");
    ArrayColumn<Double> refDir(fldTab,"REFERENCE_DIR");
    ScalarColumn<Bool> flagRow(fldTab,"FLAG_ROW");
    flagRow.fillColumn(False);
    ScalarColumn<Int> numPoly(fldTab,"NUM_POLY");
    numPoly.fillColumn(numPol);

    Int nRow=fldTab.nrow();
    Matrix<Double> zero(shape);
    zero = 0.0;
    for (Int i=0; i<nRow; i++) {
      Matrix<Double> ddir(shape),pdir(shape),rdir(shape),pntdir(shape);
      ddir(0,0)=dd(0,i); ddir(1,0)=dd(1,i);
      pdir(0,0)=pd(0,i); pdir(1,0)=pd(1,i);
      pntdir(0,0)=pntd(0,i); pntdir(1,0)=pntd(1,i);
      rdir(0,0)=rd(0,i); rdir(1,0)=rd(1,i);
      if (numPol==1) {
        ddir(0,1)=ddr(0,i); ddir(1,1)=ddr(1,i);
        pdir(0,1)=pdr(0,i); pdir(1,1)=pdr(1,i);
        pntdir(0,1)=pntdr(0,i); pntdir(1,1)=pntdr(1,i);
        rdir(0,1)=rdr(0,i); rdir(1,1)=rdr(1,i);
      }
      delDir.put(i,ddir);
      if (!allEQ(pdir, zero))
	phaseDir.put(i,pdir);
      else phaseDir.put(i,pntdir);
      refDir.put(i,rdir);
    }

    ScalarColumn<Double> time(fldTab,"TIME");
    TableDesc timetd;
    MSField::addColumnToDesc(timetd,MSField::TIME);
    time.rwKeywordSet().assign(timetd[0].keywordSet());

  }

  // FLAG_CMD
  {
  Int nRow = 0;
  SetupNewTable flagCmdSetup(ms2_p+"/FLAG_CMD",
                             MSFlagCmd::requiredTableDesc(),
                             Table::New);
  Table flagCmdt(flagCmdSetup,nRow);

  t.rwKeywordSet().defineTable(MS::keywordName(MS::FLAG_CMD),
                               flagCmdt);
  }
  


  // HISTORY
  {
    if (t.keywordSet().isDefined("OBS_LOG")) {
      // Table hisTab=t.rwKeywordSet().asTable("OBS_LOG");
      Table hisTab(ms2_p+"/OBS_LOG",Table::Update);
      

      TableDesc td;
      MSHistory::addColumnToDesc(td,MSHistory::PRIORITY);
      MSHistory::addColumnToDesc(td,MSHistory::ORIGIN);
      MSHistory::addColumnToDesc(td,MSHistory::OBJECT_ID);
      MSHistory::addColumnToDesc(td,MSHistory::APPLICATION);
      MSHistory::addColumnToDesc(td,MSHistory::CLI_COMMAND);
      MSHistory::addColumnToDesc(td,MSHistory::APP_PARAMS);
      
      hisTab.addColumn(td[0]);
      hisTab.addColumn(td[1]);
      hisTab.addColumn(td[2]);
      hisTab.addColumn(td[3]);
      hisTab.addColumn(td[4]);
      hisTab.addColumn(td[5]);

      ScalarColumn<Double> time(hisTab,"TIME");
      TableDesc timetd;
      MSHistory::addColumnToDesc(timetd,MSHistory::TIME);
      time.rwKeywordSet().assign(timetd[0].keywordSet());

      t.rwKeywordSet().removeField("OBS_LOG");
      hisTab.rename(ms2_p+"/HISTORY",Table::New);
      t.rwKeywordSet().defineTable("HISTORY", hisTab);
    }
  }
  

  // OBSERVATION
  {
  Table obsTab(ms2_p+"/OBSERVATION",Table::Update);
  removeColumn(obsTab, "CORR_SCHEDULE");
  TableDesc td;
  MSObservation::addColumnToDesc(td, MSObservation::TELESCOPE_NAME);
  MSObservation::addColumnToDesc(td, MSObservation::TIME_RANGE);
  MSObservation::addColumnToDesc(td, MSObservation::SCHEDULE);
  MSObservation::addColumnToDesc(td, MSObservation::SCHEDULE_TYPE);
  MSObservation::addColumnToDesc(td, MSObservation::LOG);
  MSObservation::addColumnToDesc(td, MSObservation::RELEASE_DATE);
  MSObservation::addColumnToDesc(td, MSObservation::FLAG_ROW);
  
  obsTab.addColumn(td[0]);
  obsTab.addColumn(td[1]);
  obsTab.addColumn(td[2]);
  obsTab.addColumn(td[3]);
  obsTab.addColumn(td[4]);
  obsTab.addColumn(td[5]);
  obsTab.addColumn(td[6]);

  Table arrTab(ms2_p+"/ARRAY",Table::Old);
  ROScalarColumn<String> arrName(arrTab,"NAME");
  ScalarColumn<String> telName(obsTab,"TELESCOPE_NAME");
  ArrayColumn<Double> timeRange(obsTab, "TIME_RANGE");
  ScalarColumn<Bool> flagRow(obsTab,"FLAG_ROW");
  flagRow.fillColumn(False);

  ROScalarColumn<Double> time(t, "TIME");
  ROScalarColumn<Double> interval(t, "INTERVAL");
  ROScalarColumn<Int> observationid(t, "OBSERVATION_ID");
  ROScalarColumn<Int> arrayid(t, "ARRAY_ID");
  Vector<Double> tim = time.getColumn();
  Vector<Double> inter = interval.getColumn();
  Vector<Int> obsid = observationid.getColumn();
  Vector<Int> arrid = arrayid.getColumn();
  Vector<String> arrnm = arrName.getColumn();

  Int nObs = obsTab.nrow();
  Int startInd;
  Int endInd;
  Int minPos, maxPos;
  Vector<Double> vt(2);
  
  for (Int obs=0; obs<nObs; obs++) {
    // fill time range
    startInd = 0;
    endInd = t.nrow()-1;
    for (uInt i=0; i<t.nrow(); i++) {
      if (obsid(i) == obs) { startInd = i; break; }
    }
    for (uInt i=startInd; i<t.nrow(); i++) {
      if (obsid(i) > obs) { endInd = i-1; break; }
    }
    vt(0) = tim(startInd);
    vt(1) = tim(endInd);

    vt(0) = min(tim(Slice(startInd,endInd-startInd+1, 1)));
    vt(1) = max(tim(Slice(startInd,endInd-startInd+1, 1)));

    // Sort just in case the time column is not sorted
    vt(0) = tim(startInd);
    vt(1) = tim(startInd);
    minPos = startInd;
    maxPos = startInd;
    for (Int i=startInd; i<=endInd; i++) {
      if (tim(i) < vt(0)) {
	vt(0) = tim(i);
	minPos = i;
      }
      if (tim(i) >= vt(1)) {
	vt(1) = tim(i);
	maxPos = i;
      }
    }

    vt(0) = vt(0) - inter(minPos)/2;
    vt(1) = vt(1) + inter(maxPos)/2;
    timeRange.put(obs, vt);

    // telescope name
    telName.put(obs, arrnm(arrid(startInd)));
  }
  }

  // POINTING
  {
  Int nRow =0;
  SetupNewTable pointingSetup(ms2_p+"/POINTING",
                              MSPointing::requiredTableDesc(),
                              Table::New);
  Table pointTab(pointingSetup,nRow);
  //  TableRecord tbrec = t.rwKeywordSet();
  t.rwKeywordSet().defineTable(MS::keywordName(MS::POINTING),
                               pointTab);
  ROScalarColumn<Double> time(t, MS::columnName(MS::TIME));
  ROScalarColumn<Double> interval(t, MS::columnName(MS::INTERVAL));
  ScalarColumn<Int> fieldId(t, MS::columnName(MS::FIELD_ID));
  Vector<Double> tim = time.getColumn();
  Vector<Double> inter = interval.getColumn();
  Vector<Int> fi = fieldId.getColumn();  
  

  Table fldTab(ms2_p+"/FIELD", Table::Update);
  Cube<Double> pd;
  {
    ROArrayColumn<Double> phaseDir(fldTab, "PHASE_DIR");
    pd = phaseDir.getColumn();
  }

  //  Table pointTab(ms2_p+"/POINTING", Table::Update);
  ScalarColumn<Double> t2(pointTab, MSPointing::columnName(MSPointing::TIME));
  ScalarColumn<Double> i2(pointTab, MSPointing::columnName(MSPointing::INTERVAL));
  ArrayColumn<Double> phaseDir2(pointTab,
		      MSPointing::columnName(MSPointing::DIRECTION));
  ScalarColumn<Int> a2(pointTab, MSPointing::columnName(MSPointing::ANTENNA_ID));
  ScalarColumn<Int> numPoly2(pointTab, MSPointing::columnName(MSPointing::NUM_POLY));
 
  nRow = t.nrow();
  Int fld = -1;
  Int pnt = 0;

  ROScalarColumn<Int> numPoly(fldTab,"NUM_POLY");
  IPosition shape(2,2,numPoly(0)+1);
  Matrix<Double> pdir(shape);

  for (Int i=0; i<nRow; i++) {
    if (fi(i) != fld) {
      fld = fi(i);
      pdir = pd.xyPlane(fld);
      
      pointTab.addRow(maxAnt);
      for (Int j=0; j<maxAnt; j++) {
        t2.put(maxAnt*pnt+j, tim(i));
        i2.put(maxAnt*pnt+j, inter(i));
	a2.put(maxAnt*pnt+j, j+1);
        phaseDir2.put(maxAnt*pnt+j, pdir);
      }
      pnt++;
    }
  }

  nRow = pointTab.nrow();

  for  (Int i=0; i<nRow; i++)
    numPoly2.put(i, numPoly(0));

  uInt ctp;
  ROArrayColumn<Double> obspDir(fldTab,"_OBSOLETE_POINTING_DIR");
  MDirection::Types tp;
  MDirection::getType(tp, obspDir.keywordSet().asString("MEASURE_REFERENCE")); 
  ctp = tp;
  ArrayMeasColumn<MDirection> dirAmc(pointTab, "DIRECTION");
  dirAmc.setDescRefCode(ctp, False);
  ArrayMeasColumn<MDirection> tgAmc(pointTab, "TARGET");
  tgAmc.setDescRefCode(ctp, False);


  for (uInt j = MSPointing::NUMBER_REQUIRED_COLUMNS + 1;
       j < MSPointing::NUMBER_PREDEFINED_COLUMNS; j = j + 1) {
    MSPointing::PredefinedColumns i = (MSPointing::PredefinedColumns) j;
    if (pointTab.tableDesc().isColumn(MSPointing::columnName(i))) {
      TableColumn tbc(pointTab, MSPointing::columnName(i));
      TableDesc td;
      MSPointing::addColumnToDesc(td, i);
      tbc.rwKeywordSet().assign(td[0].keywordSet());
    }
  }

  }


  // POLARIZATION
  // SPECTRAL_WINDOW
  {
  Table spwTab(ms2_p+"/SPECTRAL_WINDOW",Table::Update);
  Int nRow = spwTab.nrow();
  SetupNewTable polarizationSetup(ms2_p+"/POLARIZATION",
				  MSPolarization::requiredTableDesc(),
                                  Table::New);
  Table polTab(polarizationSetup,nRow);
  t.rwKeywordSet().defineTable(MS::keywordName(MS::POLARIZATION),
			       polTab);

  
  //  Table polTab(ms2_p+"/POLARIZATION",Table::Update);

  TableDesc td;
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::NAME);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::FREQ_GROUP);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::FREQ_GROUP_NAME);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::NET_SIDEBAND);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::CHAN_WIDTH);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::EFFECTIVE_BW);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::MEAS_FREQ_REF);
  MSSpectralWindow::addColumnToDesc(td,MSSpectralWindow::FLAG_ROW);
  for (Int i=0; i<8; i++) spwTab.addColumn(td[i]);

  ScalarColumn<Int> snumCorr(spwTab, "NUM_CORR");
  ROArrayColumn<Int> scorrType(spwTab, "CORR_TYPE");
  ROArrayColumn<Int> scorrProduct(spwTab, "CORR_PRODUCT");

  ScalarColumn<Int> pnumCorr(polTab, "NUM_CORR");
  ArrayColumn<Int> pcorrType(polTab, "CORR_TYPE");
  ArrayColumn<Int> pcorrProduct(polTab, "CORR_PRODUCT");

  pnumCorr.putColumn(snumCorr.getColumn());

  for (Int i=0; i<nRow; i++) {
    pcorrType.put(i, scorrType(i)); 
    pcorrProduct.put(i, scorrProduct(i));
  }

  ScalarColumn<Int> freqGrp(spwTab,"FREQ_GROUP");
  freqGrp.fillColumn(0);
  ScalarColumn<Int> netSideb(spwTab,"NET_SIDEBAND");
  netSideb.fillColumn(1);

  ArrayColumn<Double> resol(spwTab, "RESOLUTION");
  ArrayColumn<Double> chanWdth(spwTab, "CHAN_WIDTH");
  ArrayColumn<Double> effBw(spwTab, "EFFECTIVE_BW");
  for (Int i=0; i<nRow; i++) {
    chanWdth.put(i, resol(i));
    effBw.put(i, resol(i));
  }
  ScalarColumn<Bool> flagRow(spwTab,"FLAG_ROW");
  flagRow.fillColumn(False);

  ScalarColumn<Double> reffreq(spwTab,"REF_FREQUENCY");
  MFrequency::Types tp;
  MFrequency::getType(tp, reffreq.keywordSet().asString("MEASURE_REFERENCE"));
  Int meas_freq_ref = tp;

  ScalarColumn<Int> measCol(spwTab,"MEAS_FREQ_REF");
  measCol.fillColumn(meas_freq_ref);

  TableDesc reffreqtd;
  MSSpectralWindow::addColumnToDesc(reffreqtd,MSSpectralWindow::REF_FREQUENCY);
  reffreq.rwKeywordSet().assign(reffreqtd[0].keywordSet());

  
  ArrayColumn<Double> chanfreq(spwTab,"CHAN_FREQ");
  TableDesc chanfreqtd;
  MSSpectralWindow::addColumnToDesc(chanfreqtd,MSSpectralWindow::CHAN_FREQ);
  chanfreq.rwKeywordSet().assign(chanfreqtd[0].keywordSet());

 

  ArrayColumn<Double> chanwidth(spwTab,"CHAN_WIDTH");
  TableDesc chanwidthtd;
  MSSpectralWindow::addColumnToDesc(chanwidthtd,MSSpectralWindow::CHAN_WIDTH);
  chanwidth.rwKeywordSet().assign(chanwidthtd[0].keywordSet());

  ArrayColumn<Double> bw(spwTab,"EFFECTIVE_BW");
  TableDesc bwtd;
  MSSpectralWindow::addColumnToDesc(bwtd,MSSpectralWindow::EFFECTIVE_BW);
  bw.rwKeywordSet().assign(bwtd[0].keywordSet());

  ArrayColumn<Double> res(spwTab,"RESOLUTION");
  TableDesc restd;
  MSSpectralWindow::addColumnToDesc(restd,MSSpectralWindow::RESOLUTION);
  res.rwKeywordSet().assign(restd[0].keywordSet());

  ScalarColumn<Double> tbw(spwTab,"TOTAL_BANDWIDTH");
  TableDesc tbwtd;
  MSSpectralWindow::addColumnToDesc(tbwtd,MSSpectralWindow::TOTAL_BANDWIDTH);
  tbw.rwKeywordSet().assign(tbwtd[0].keywordSet());


  for (uInt j = MSSpectralWindow::NUMBER_REQUIRED_COLUMNS + 1;
       j < MSSpectralWindow::NUMBER_PREDEFINED_COLUMNS; j = j + 1) {
    MSSpectralWindow::PredefinedColumns i = (MSSpectralWindow::PredefinedColumns) j;
    if (spwTab.tableDesc().isColumn(MSSpectralWindow::columnName(i))) {
      TableColumn tbc(spwTab, MSSpectralWindow::columnName(i));
      TableDesc td;
      MSSpectralWindow::addColumnToDesc(td, i);
      tbc.rwKeywordSet().assign(td[0].keywordSet());
    }
  }



  }
  
  // PROCESSOR
  {
  Int nRow = 0;
  SetupNewTable processorSetup(ms2_p+"/PROCESSOR",
			       MSProcessor::requiredTableDesc(),
                               Table::New);
  Table processorSetupt(processorSetup,nRow);
  t.rwKeywordSet().defineTable(MS::keywordName(MS::PROCESSOR),
			       processorSetupt);

  }

  // SOURCE
  {
  Table sourceTab(ms2_p+"/SOURCE",Table::Update);
  TableDesc td;
  MSSource::addColumnToDesc(td, MSSource::NUM_LINES);
  sourceTab.addColumn(td[0]);

  ArrayColumn<Double> pos(sourceTab,"POSITION");
  TableDesc postd;
  MSSource::addColumnToDesc(postd,MSSource::POSITION);
  pos.rwKeywordSet().assign(postd[0].keywordSet());

  ArrayColumn<Double> direc(sourceTab,"DIRECTION");
  TableDesc directd;
  MSSource::addColumnToDesc(directd,MSSource::DIRECTION);
  direc.rwKeywordSet().assign(directd[0].keywordSet());

  ArrayColumn<Double> prop(sourceTab,"PROPER_MOTION");
  TableDesc proptd;
  MSSource::addColumnToDesc(proptd,MSSource::PROPER_MOTION);
  prop.rwKeywordSet().assign(proptd[0].keywordSet());


  ScalarColumn<Double> inter(sourceTab,"INTERVAL");
  TableDesc intertd;
  MSSource::addColumnToDesc(intertd,MSSource::INTERVAL);
  inter.rwKeywordSet().assign(intertd[0].keywordSet());

  ScalarColumn<Double> time(sourceTab, "TIME");
  TableDesc timetd;
  MSSource::addColumnToDesc(timetd,MSSource::TIME);
  time.rwKeywordSet().assign(timetd[0].keywordSet());

  for (uInt j = MSSource::NUMBER_REQUIRED_COLUMNS + 1;
       j < MSSource::NUMBER_PREDEFINED_COLUMNS; j = j + 1) {
    MSSource::PredefinedColumns i = (MSSource::PredefinedColumns) j;
    if (sourceTab.tableDesc().isColumn(MSSource::columnName(i))) {
      TableColumn tbc(sourceTab, MSSource::columnName(i));
      TableDesc td;
      MSSource::addColumnToDesc(td, i);
      tbc.rwKeywordSet().assign(td[0].keywordSet());
    }
  }

  if (sourceTab.tableDesc().isColumn("SYSVEL_OLD")) {
    cout << "Array column SYSVEL_OLD seems to exist" << endl;
  } else {
    sourceTab.renameColumn("SYSVEL_OLD", "SYSVEL");
    sourceTab.addColumn(ArrayColumnDesc<Double>("SYSVEL", 1));
    // Construct a measure for this column in a temporary TableDesc.
    // Copy that keywordset to get the measure in the SYSVEL column.
    TableDesc td;
    td.addColumn(ArrayColumnDesc<Double>("SYSVELX", 1));
    TableMeasValueDesc mvval(td, "SYSVELX");
    TableMeasDesc<MRadialVelocity> mval(mvval);
    mval.write(td);
    ROScalarColumn<Double> vold(sourceTab, "SYSVEL_OLD");
    ArrayColumn<Double> sysvel(sourceTab, "SYSVEL");
    sysvel.rwKeywordSet() = td.columnDesc("SYSVELX").keywordSet();
    // Set data to the old SYSVEL.
    Vector<Double> vec(1);
    for (uInt i=0; i<sourceTab.nrow(); i++) {
      vec(0) = vold(i);
      sysvel.put(i, vec);
    }
  }

  }

  // STATE
  {
  Int nRow = 0;
  
  SetupNewTable stateSetup(ms2_p+"/STATE",
			   MSState::requiredTableDesc(),
			   Table::New);
  Table stateSetupt(stateSetup,nRow);

  t.rwKeywordSet().defineTable(MS::keywordName(MS::STATE),  
			       stateSetupt);

  }

  // SYSCAL
  {
  Table syscalTab(ms2_p+"/SYSCAL",Table::Update);
  if (syscalTab.canRemoveColumn("ARRAY_ID"))
    removeColumn(syscalTab,"ARRAY_ID");
  removeColumn(syscalTab,"NUM_RECEPTORS");

  ScalarColumn<Double> inter(syscalTab,"INTERVAL");
  TableDesc intertd;
  MSSysCal::addColumnToDesc(intertd,MSSysCal::INTERVAL);
  inter.rwKeywordSet().assign(intertd[0].keywordSet());

  ScalarColumn<Double> time(syscalTab, "TIME");
  TableDesc timetd;
  MSSysCal::addColumnToDesc(timetd,MSSysCal::TIME);
  time.rwKeywordSet().assign(timetd[0].keywordSet());

  for (uInt j = MSSysCal::NUMBER_REQUIRED_COLUMNS + 1;
       j < MSSysCal::NUMBER_PREDEFINED_COLUMNS; j = j + 1) {
    MSSysCal::PredefinedColumns i = (MSSysCal::PredefinedColumns) j;
    if (syscalTab.tableDesc().isColumn(MSSysCal::columnName(i))) {
      TableColumn tbc(syscalTab, MSSysCal::columnName(i));
      TableDesc td;
      MSSysCal::addColumnToDesc(td, i);
      tbc.rwKeywordSet().assign(td[0].keywordSet());
    }
  }

  }
  

  // WEATHER
  {
  Table weatherTab(ms2_p+"/WEATHER",Table::Update);
  if (weatherTab.canRemoveColumn("ARRAY_ID"))
    removeColumn(weatherTab,"ARRAY_ID");

  ScalarColumn<Double> inter(weatherTab,"INTERVAL");
  TableDesc intertd;
  MSWeather::addColumnToDesc(intertd,MSWeather::INTERVAL);
  inter.rwKeywordSet().assign(intertd[0].keywordSet());

  ScalarColumn<Double> time(weatherTab, "TIME");
  TableDesc timetd;
  MSWeather::addColumnToDesc(timetd,MSWeather::TIME);
  time.rwKeywordSet().assign(timetd[0].keywordSet());

  for (uInt j = MSWeather::NUMBER_REQUIRED_COLUMNS + 1;
       j < MSWeather::NUMBER_PREDEFINED_COLUMNS; j = j + 1) {
    MSWeather::PredefinedColumns i = (MSWeather::PredefinedColumns) j;
    if (weatherTab.tableDesc().isColumn(MSWeather::columnName(i))) {
      TableColumn tbc(weatherTab, MSWeather::columnName(i));
      TableDesc td;
      MSWeather::addColumnToDesc(td, i);
      tbc.rwKeywordSet().assign(td[0].keywordSet());
    }
  }

  }

  // get correct shape for array weight
  if (t.tableDesc().isColumn("WEIGHT_OLD")) {
    cout << "Array column WEIGHT_OLD seems to exist" << endl;
  } else {
    t.renameColumn ("WEIGHT_OLD", "WEIGHT");
    t.addColumn (ArrayColumnDesc<Float>("WEIGHT", 1));

    ROArrayColumn<Float> sigma (t, "SIGMA");
    ROScalarColumn<Float> wold (t, "WEIGHT_OLD");
    ArrayColumn<Float> weight (t, "WEIGHT");
    for (uInt i=0; i<t.nrow(); i++) {
      Array<Float> arr(sigma.shape(i));
      arr = wold(i);
      weight.put (i, arr);
    }
  }


  os_p << LogIO::NORMAL << "Conversion done" << LogIO::POST;
  return True;
}

} //# NAMESPACE CASACORE - END

