//# MSSelector.cc: selection and iteration of an MS
//# Copyright (C) 1997,1998,1999,2000
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
//#
//# $Id$

#include <trial/MeasurementSets/MSSelector.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/Cube.h>
#include <aips/Containers/Record.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableIter.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/GenSort.h>
#include <trial/MeasurementSets/MSIter.h>
#include <trial/MeasurementSets/MSRange.h>
#include <trial/MeasurementSets/MSSelUtil.h>

static LogIO os;

MSSelector::MSSelector():msIter_p(0),initSel_p(False),dataDescId_p(-1),
lastDataDescId_p(-2),
useSlicer_p(False),haveSlicer_p(False),wantedOne_p(-1),convert_p(False)
{ }

MSSelector::MSSelector(MeasurementSet& ms):ms_p(ms),
selms_p(ms),savems_p(ms),msIter_p(0),initSel_p(False),dataDescId_p(-1),
lastDataDescId_p(-2),
useSlicer_p(False),haveSlicer_p(False),wantedOne_p(-1),convert_p(False)
{ }

MSSelector::MSSelector(const MSSelector& other):msIter_p(0)
{ operator=(other);}

MSSelector& MSSelector::operator=(const MSSelector& other)
{
  if (this==&other) return *this;
  ms_p=other.ms_p;
  selms_p=other.selms_p;
  savems_p=other.savems_p;
  lastDataDescId_p=other.lastDataDescId_p;
  if (msIter_p) delete msIter_p;
  msIter_p = 0;
  if (other.msIter_p) msIter_p=new MSIter(*other.msIter_p);
  initSel_p=other.initSel_p;
  dataDescId_p=other.dataDescId_p;
  useSlicer_p=other.useSlicer_p;
  haveSlicer_p=other.haveSlicer_p;
  slicer_p=other.slicer_p;
  wantedOne_p=other.wantedOne_p;
  convert_p=other.convert_p;
  return *this;
}

MSSelector::~MSSelector() 
{
  if (msIter_p) delete msIter_p;
  msIter_p=0;
}

void MSSelector::setMS(MeasurementSet& ms)
{
  ms_p=ms;
  selms_p=ms;
  savems_p=ms;
  if (msIter_p) delete msIter_p;
  msIter_p=0;
  initSel_p=False;
  dataDescId_p=-1;
  useSlicer_p=False;
  haveSlicer_p=False;
  wantedOne_p=-1;
  convert_p=False;
}

Bool MSSelector::initSelection(Int dataDescId, Bool reset)
{
  // first check if we want to throw all selections away
  if (reset) {
    selms_p=ms_p;
    initSel_p=False;
    dataDescId_p=-1;
    useSlicer_p=False;
    haveSlicer_p=False;
    wantedOne_p=-1;
    convert_p=False;
    return True;
  }

  // check if we can reuse the saved selection
  if (initSel_p && dataDescId==lastDataDescId_p) {
    selms_p=savems_p;
    //os<< LogIO::NORMAL << "Re-using previous selection"<<LogIO::POST;
    return True;
  } else {
    // undo all previous selections
    selms_p=ms_p;
    ifrSelection_p.resize(0);
    rowIndex_p.resize(0,0);
  }
  // selection on data description is optional
  Bool constantShape=True;
  if (ms_p.dataDescription().nrow()>1) {
    if (dataDescId>=0) {
      selms_p=selms_p(selms_p.col(MS::columnName(MS::DATA_DESC_ID)) 
		      == dataDescId);
      dataDescId_p=dataDescId;
    } else {
      // check if the data shape is the same for all data
      // if not: select first data desc id only
      ROScalarColumn<Int> dd(selms_p,MS::columnName(MS::DATA_DESC_ID));
      ROMSDataDescColumns ddc(selms_p.dataDescription());
      Vector<Int> ddId=dd.getColumn();
      Int ndd=GenSort<Int>::sort(ddId, Sort::Ascending, 
				 Sort::HeapSort | Sort::NoDuplicates);
      ROMSSpWindowColumns spwc(ms_p.spectralWindow());
      ROMSPolarizationColumns polc(ms_p.polarization());
      for (Int i=1; i<ndd; i++) {
	if (spwc.numChan()(ddc.spectralWindowId()(i)) != 
	    spwc.numChan()(ddc.spectralWindowId()(i-1)) ||
	    polc.numCorr()(ddc.spectralWindowId()(i)) != 
	    polc.numCorr()(ddc.spectralWindowId()(i-1))) {
	  constantShape = False;
	  break;
	}
      }
      dataDescId_p=-1;
      if (!constantShape) {
	selms_p=selms_p(selms_p.col(MS::columnName(MS::DATA_DESC_ID))
			== ddId(0));
	os<< LogIO::WARN << "Data shape varies, selected first data desc id"
	  " only"<< LogIO::POST;
	dataDescId_p=0;
      }
    }
  } else {
    if (dataDescId>0) {
      os << LogIO::NORMAL << "DataDescId selection ignored, "
	"there is only one" << LogIO::POST;
    }
    dataDescId_p=0;
  }
  lastDataDescId_p=dataDescId_p;

  // if selection is empty, reject it
  initSel_p = ToBool(selms_p.nrow()>0);
  if (!initSel_p) {
    os<< LogIO::WARN << "Selected Table has zero rows"<<LogIO::POST;
  } else {
    // save the current selection
    savems_p=selms_p;
    // Set channel selection to entire spectrum
    ROScalarColumn<Int> ddId(selms_p,MS::columnName(MS::DATA_DESC_ID));
    ROMSDataDescColumns ddc(selms_p.dataDescription());
    ROMSSpWindowColumns spwc(selms_p.spectralWindow());
    selectChannel(spwc.numChan()(ddc.spectralWindowId()(ddId(0))),0,1,1);

    // Set polarization selection/conversion to all present
    ROMSPolarizationColumns polc(selms_p.polarization());
    Vector<Int> pols=polc.corrType()(ddc.polarizationId()(ddId(0)));
    Vector<String> polSel(pols.nelements());
    for (uInt i=0; i<pols.nelements(); i++) 
      polSel(i)=Stokes::name(Stokes::type(pols(i)));
    selectPolarization(polSel);
    os<< LogIO::NORMAL << "Selection initialized ok"<< LogIO::POST;
  }
  return constantShape;
}

Bool MSSelector::selectChannel(Int nChan, Int start, Int width, Int incr)
{
  if (!initSel_p) {
    os << LogIO::WARN <<"Initializing selection with dd=0"
       << LogIO::POST;
    initSelection();
  }
  if (!initSel_p) return False;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  Bool ok = ToBool(nChan>0 && start>=0 && width>0 && incr>0);
  if (!ok) {
    os << LogIO::SEVERE << "Illegal channel selection"<<LogIO::POST;
    return False;
  }
  ROMSColumns msc(selms_p);
  Int dd=msc.dataDescId()(0);
  Int numChan=msc.spectralWindow().numChan()
    (msc.dataDescription().spectralWindowId()(dd));
  Int end=start+(nChan-1)*incr+(width-1);
  ok=ToBool(ok && end < numChan);
  //#  if (incr < 0) ok=ToBool(ok && start+(nChan-1)*incr-(width-1) >= 0);
  if (!ok) {
    os << LogIO::SEVERE << "Illegal channel selection"<<LogIO::POST;
    return False;
  }
  haveSlicer_p=False;
  useSlicer_p=ToBool(start>0 || end<(numChan-1) || incr>1 ||wantedOne_p>=0);
  chanSel_p.resize(4);
  chanSel_p(0)=nChan; 
  chanSel_p(1)=start; 
  chanSel_p(2)=width; 
  chanSel_p(3)=incr;
  Matrix<Double> chanFreq,bandwidth;
  Int nSpW;
  if (dataDescId_p==-1) {
    chanFreq=msc.spectralWindow().chanFreq().getColumn();
    bandwidth=msc.spectralWindow().resolution().getColumn();
    nSpW=chanFreq.ncolumn();
  } else {
    Int spw = msc.dataDescription().spectralWindowId()(dataDescId_p);
    chanFreq.resize(numChan,1); bandwidth.resize(numChan,1);
    chanFreq.column(0)=msc.spectralWindow().chanFreq()(spw);
    bandwidth.column(0)=msc.spectralWindow().resolution()(spw);
    nSpW=1;
  }
  for (Int i=0; i<nSpW; i++) {
    chanFreq_p.resize(nChan,nSpW); bandwidth_p.resize(nChan,nSpW);
    for (Int j=0; j<nChan; j++) {
      Int n=1;
      chanFreq_p(j,i)=chanFreq(start+incr*j,i);
      for (Int k=1; k<width; k++) {
	chanFreq_p(j,i)+=chanFreq(start+incr*j+k,i);
	n++;
      }
      if (n>1) {
	chanFreq_p(j,i)/=n;
      }
      bandwidth_p(j,i)=bandwidth(start+incr*j,i);
      if (n>1) {
	// Not correct if there are gaps between channels
	bandwidth_p(j,i)=(bandwidth(start,i)+bandwidth(start+incr*j+width-1,i))/2
	  + abs(chanFreq(start+incr*j+width-1,i)-chanFreq(start+incr*j,i));
      }
    }
  }
  os << LogIO::NORMAL<< "Channel selection set to #chan="<<nChan<<
    ", start="<<start+1<<", width="<<width<<", incr="<<incr<<LogIO::POST;
  return True;
}

Bool MSSelector::selectPolarization(const Vector<String>& wantedPol)
{
  // this selection/conversion assumes that parallactic angle rotation
  // is taken care of elsewhere (i.e., results may only be correct for
  // CORRECTED_DATA and MODEL_DATA conversions, not for the observed DATA)

  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  // first convert strings to enums
  Int n=wantedPol.nelements();
  Vector<Int> wanted(n);
  for (Int i=0; i<n; i++) wanted(i)=Stokes::type(wantedPol(i));

  // now find out the input polarizations
  ROMSColumns msc(selms_p);
  Int polId=msc.dataDescription().polarizationId()(0);
  Int numCorr=msc.polarization().numCorr()(polId);
  Vector<Int> inputPol=msc.polarization().corrType()(polId);

  // shortcut two cases: we want 1 or all existing pols in the output
  convert_p=False;
  wantedOne_p=-1;
  if (n==1) {
    Bool found=False;
    for (Int i=0; i<numCorr; i++) {
      if (wanted(0)==inputPol(i)) {
	found=True;
	wantedOne_p=i;
	useSlicer_p=True;
	break;
      }
    }
    if (!found) convert_p=True;
  } else if (n==numCorr) {
    for (Int i=0; i<numCorr; i++) {
      if (wanted(i)!=inputPol(i)) {
	convert_p=True;
	break;
      }
    }
  } else {
    convert_p=True;
  }

  if (convert_p) {
    // check validity
    for (Int i=0; i<n; i++) {
      if (wanted(i)==Stokes::Undefined) {
	os << LogIO::SEVERE << "Unrecognized polarization: "<<wantedPol(i)	  << LogIO::POST;
	return False;
      }
    }
    stokesConverter_p.setConversion(wanted,inputPol);
  }
  if (chanSel_p.nelements()==4) {
    Int end=chanSel_p(1)+(chanSel_p(0)-1)*chanSel_p(3)+chanSel_p(2)-1;
    // use slicer if: start > 0, end< nChan-1, inc>1 && width<inc, one pol only
    useSlicer_p=
      ToBool(chanSel_p(1)>0 || 
	     end<(chanSel_p(0)-1) || 
	     (chanSel_p(3)>1 && chanSel_p(2)<chanSel_p(3)) 
	     || wantedOne_p>=0);
  }
  if (useSlicer_p) haveSlicer_p=False;
  polSelection_p.resize(wantedPol.nelements()); polSelection_p=wantedPol;
  os << LogIO::NORMAL<< "selected polarizations: "<< wantedPol 
    << LogIO::POST;
  return True;
}

Bool MSSelector::select(const GlishRecord& items, Bool oneBased)
{
  if (!initSel_p) {
    os << LogIO::WARN <<"Initializing selection with dd=0"
       << LogIO::POST;
    initSelection();
  }
  if (!initSel_p) return False;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  Int n=items.nelements();
  for (Int i=0; i<n; i++) {
    String column=items.name(i);
    MSS::Field fld=MSS::field(column);
    column.upcase();
    switch (fld) {
    case MSS::ANTENNA1:
    case MSS::ANTENNA2:
    case MSS::ARRAY_ID:
    case MSS::DATA_DESC_ID:
    case MSS::FEED1:
    case MSS::FEED2:
    case MSS::FIELD_ID:
    case MSS::SCAN_NUMBER:
      {
	Vector<Int> id;
	if (GlishArray(items.get(i)).get(id) && id.nelements()>0) {
	  if (oneBased) id-=1;
	  if (id.nelements()==1) {
	    selms_p=selms_p(selms_p.col(column) == id(0));
	  } else {
	    selms_p=selms_p(selms_p.col(column).in(id));
	  }
	} else {
	  os<< LogIO::WARN << "Illegal value for item "<<downcase(column)<<
	    LogIO::POST;
	}
      }
      break;
    case MSS::IFR_NUMBER:
      {
	Vector<Int> ifrNum;
	if (GlishArray(items.get(i)).get(ifrNum) && ifrNum.nelements()>0) {
	  // check input values for validity, squeeze out illegal values
	  Int nAnt=selms_p.antenna().nrow();
	  if (oneBased) ifrNum-=1001;
          for (uInt k=0; k<ifrNum.nelements(); k++) {
            if (ifrNum(k)/1000<0 || ifrNum(k)/1000>= nAnt ||
                ifrNum(k)%1000<0 || ifrNum(k)%1000>=nAnt) {
              for (uInt j=k; j<ifrNum.nelements()-1; j++) {
                ifrNum(j)=ifrNum(j+1);
              }
              ifrNum.resize(ifrNum.nelements()-1,True);
            }
	  }
	  if (ifrNum.nelements()==1) {
	    selms_p=selms_p((selms_p.col(MS::columnName(MS::ANTENNA1))*1000+
			     selms_p.col(MS::columnName(MS::ANTENNA2))) ==
			    ifrNum(0));
	  } else {
	    selms_p=selms_p((selms_p.col(MS::columnName(MS::ANTENNA1))*1000+
			     selms_p.col(MS::columnName(MS::ANTENNA2)))
			    .in(ifrNum));
	  }
	  ifrSelection_p.reference(ifrNum);
	} else {
	  os<< LogIO::WARN << "Illegal value for item "<<downcase(column)<<
	    LogIO::POST;
	}
      }
      break;
    case MSS::ROWS:
      {
	Vector<Int> rows;
	if (GlishArray(items.get(i)).get(rows) && rows.nelements()>0) {
	  Int n=rows.nelements();
	  if (oneBased) rows-=1;
	  Vector<uInt> uRows(n);
	  convertArray(uRows,rows);
	  // Select rows from the base table.
	  // This is consistent with the rownumbers returned by range.
	  selms_p=ms_p(uRows);
	}
      }
      break;
    case MSS::TIME:
      {
	Vector<Double> range;
	if (GlishArray(items.get(i)).get(range) && range.nelements()==2) {
	  TableExprNodeSetElem elem(True,range(0),range(1),True);
	  TableExprNodeSet set;
	  set.add(elem);
	  selms_p=selms_p(selms_p.col(column).in(set));
	} else {
	  os << LogIO::WARN << "Illegal value for time range: "<<
	    "two element numeric vector required"<<LogIO::POST;
	}
      }
      break;
    case MSS::TIMES:
      {
	column=MS::columnName(MS::TIME);
	Vector<Double> time;
	if (GlishArray(items.get(i)).get(time) && time.nelements()>0) {
	  if (time.nelements()==1) {
	    selms_p=selms_p(selms_p.col(column) == time(0));
	  } else {
	    selms_p=selms_p(selms_p.col(column).in(time));
	  }
	}
      }
      break;
    case MSS::U:
    case MSS::V:
    case MSS::W:
      {
	Int uvwIndex=fld-MSS::U;
	Vector<Double> range;
	if (GlishArray(items.get(i)).get(range) && range.nelements()==2) {
	  column=MS::columnName(MS::UVW);
	  TableExprNodeSet interval;
	  interval.add (TableExprNodeSetElem (True, range(0), 
					      range(1), True));
	  IPosition idx(1,uvwIndex);
	  selms_p=selms_p(selms_p.col(column)(idx).in(interval)); 
	} else {
	  os << LogIO::WARN << "Illegal value for u, v, w range: "<<
	    "two element numeric vector required"<<LogIO::POST;
	}
      }
      break;
    case MSS::UVDIST:
      {
	Vector<Double> range;
	if (GlishArray(items.get(i)).get(range) && range.nelements()==2) {
	  range*=range; // square
	  ROArrayColumn<Double> uvwcol(selms_p,MS::columnName(MS::UVW));
	  Int nrow=selms_p.nrow();
	  if (nrow>0) {
	    Matrix<Double> uvw=uvwcol.getColumn();
	    Block<Bool> rowsel(nrow);
	    for (Int k=0; k<nrow; k++) {
	      Double uvdist=square(uvw(0,k))+square(uvw(1,k));
	      //attempt to cope with roundoff error in square
	      rowsel[k]=
		ToBool(( uvdist >= range(0) || near(uvdist,range(0)) ) && 
		       ( uvdist <= range(1) || near(uvdist,range(1)) ) );
	    }
	    selms_p=selms_p(rowsel);
	  }
	} else {
	  os << LogIO::WARN << "Illegal value for uvdist range: "<<
	    "two element numeric vector required"<<LogIO::POST;
	}
      }
      break;
    case MSS::UNDEFINED:
    default:
      os << LogIO::WARN << "Unrecognized field in input ignored: "<<
	downcase(column)<<LogIO::POST;
      break;
    }
  }
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is now empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  return True;
}

static void averageId(Vector<Int>& vec)
{
  // "average" a vector of integer id's - replace by length 1 vector with
  // common value if all elements are the same, or -1 otherwise.
  // Used below to "average" things like antenna id.
  if (vec.nelements()>1) {
    Int aver=vec(0);
    if (!allEQ(vec,aver)) aver=-1;
    vec.resize(1); 
    vec(0)=aver;
  }
}

static void averageDouble(Vector<Double>& vec) 
{
  // average a vector of doubles
  Int n=vec.nelements();
  if (n>1) {
    Double aver=vec(0);
    for (Int i=1; i<n; i++) aver+=vec(i);
    aver/=n;
    vec.resize(1);
    vec(0)=aver;
  }
}

GlishRecord MSSelector::getData(const Vector<String>& items, Bool ifrAxis,
				Int inc, Bool average, Bool oneBased)
{
  GlishRecord out;
  if (!initSel_p) {
    os << LogIO::WARN <<"Initializing selection with dd=0"
       << LogIO::POST;
    initSelection();
  }
  if (!initSel_p) return out;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return out;
  }
  if (ifrAxis && dataDescId_p<0) {
    os << LogIO::WARN << " Need to select one dataDesc only"
      " when ifrAxis==True" << LogIO::POST;
    return out;
  }

  Bool wantAmp, wantPhase, wantReal, wantImag, wantData,
    wantCAmp, wantCPhase, wantCReal, wantCImag, wantCData,
    wantMAmp, wantMPhase, wantMReal, wantMImag, wantMData, wantFlag,
    wantFlagSum, 
    wantRAmp, wantRPhase, wantRReal, wantRImag, wantRData,
    wantORAmp, wantORPhase, wantORReal, wantORImag, wantORData,
    wantCorrGain, wantObsCorrGain, wantWeight;
  wantAmp=wantPhase=wantReal=wantImag=wantData=
    wantCAmp=wantCPhase=wantCReal=wantCImag=wantCData=
    wantMAmp=wantMPhase=wantMReal=wantMImag=wantMData=wantFlag=
    wantFlagSum=
    wantRAmp=wantRPhase=wantRReal=wantRImag=wantRData=
    wantORAmp=wantORPhase=wantORReal=wantORImag=wantORData=
    wantCorrGain=wantObsCorrGain=wantWeight=False;

  Matrix<Double> uvw;

  Int nItems=items.nelements(),nRows=selms_p.nrow();
  Table tab;
  if (inc>1 && inc<=nRows) {
    Vector<uInt> rows(nRows/inc);
    indgen(rows,uInt(0),uInt(inc));
    tab=selms_p(rows);
  } else {
    tab=selms_p;
  }
  ROMSColumns msc(tab);
  Int nIfr = ifrSelection_p.nelements();
  if (ifrAxis && nIfr==0) {
    // set default
    //    ifrSelection_p=ifrNumbers(msc.antenna1(),msc.antenna2());
    MSRange msRange(tab);
    GlishArray(msRange.range(MSS::IFR_NUMBER).get(0)).get(ifrSelection_p);
    nIfr = ifrSelection_p.nelements();
  }
  Int nTime = 0;
  Int nRow = 0;
  Vector<Int> ifrSlot;
  Vector<Int> timeSlot;
  Vector<Int> timeSlotRow;
  Bool doIfrAxis = ToBool(ifrAxis && nIfr>0);
  if (doIfrAxis) {
    // figure out which rows go with which timeslot
    Vector<Int> ifr=msc.antenna1().getColumn();
    ifr*=1000;
    ifr+=msc.antenna2().getColumn();
    nRow=ifr.nelements();
    ifrSlot.resize(nRow);
    timeSlot.resize(nRow);
    Vector<Double> time=msc.time().getColumn();
    timeSlot(0)=0;
    for (Int i=1; i<nRow; i++) {
      timeSlot(i)=timeSlot(i-1);
      if (time(i)>time(i-1)) {
	timeSlot(i)++;
      }
    }
    nTime = timeSlot(nRow-1)+1;
    timeSlotRow.resize(nTime);
    timeSlotRow(0)=0;
    for (Int i=1; i<nRow; i++) {
      if (timeSlot(i)!=timeSlot(i-1)) timeSlotRow(timeSlot(i))=i;
    }
    for (Int i=0; i<nRow; i++) {
      for (Int j=0; j<nIfr; j++) {
	if (ifr(i)==ifrSelection_p(j)) {
	  ifrSlot(i)=j;
	  break;
	}
      }
    }
    rowIndex_p.resize(nIfr,nTime); rowIndex_p.set(-1);
    for (Int i=0; i<nRow; i++) rowIndex_p(ifrSlot(i),timeSlot(i))=i;
  } else {
    rowIndex_p.resize(0,0);
  }

  // now get out the data
  for (Int it=0; it<nItems; it++) {
    String item=downcase(items(it));
    MSS::Field fld=MSS::field(item);
    switch (fld) {
    case MSS::AMPLITUDE:
      wantAmp=True;
      break;
    case MSS::CORRECTED_AMPLITUDE:
      wantCAmp=True;
      break;
    case MSS::MODEL_AMPLITUDE:
      wantMAmp=True;
      break;
    case MSS::RESIDUAL_AMPLITUDE:
      wantRAmp=True;
      break;
    case MSS::OBS_RESIDUAL_AMPLITUDE:
      wantORAmp=True;
      break;
    case MSS::ANTENNA1:
      if (doIfrAxis) {
	Vector<Int> ant1(nIfr);
	ant1=ifrSelection_p;
	ant1/=1000;
	if (oneBased) ant1+=1;
	out.add(item,ant1);
      }
      else {
	Vector<Int> ant=msc.antenna1().getColumn();
	if (average) averageId(ant);
	if (oneBased) ant+=1;
	out.add(item,ant);
      }
      break;
    case MSS::ANTENNA2:
      if (doIfrAxis) {
	Vector<Int> ant2(nIfr);
	ant2=ifrSelection_p;
	ant2-=1000*(ifrSelection_p/1000);
	if (oneBased) ant2+=1;
	out.add(item,ant2);
      }
      else {
	Vector<Int> ant= msc.antenna2().getColumn();
	if (average) averageId(ant);
	if (oneBased) ant+=1;
	out.add(item,ant);
      }
      break;
    case MSS::AXIS_INFO:
      {
	GlishRecord axis_info;
	// add info for the axes of the data array
	// 1. corr info (polarizations)
	axis_info.add("corr_axis",polSelection_p);
	// 2. freq info
	GlishRecord freq_axis;
	freq_axis.add("chan_freq",chanFreq_p);
	freq_axis.add("resolution",bandwidth_p);
	axis_info.add("freq_axis",freq_axis);
	if (doIfrAxis) {
	  // 3. ifr info
	  GlishRecord ifr_axis;
	  if (oneBased) {
	    Array<Int> ifr; 
	    ifr=ifrSelection_p;
	    ifr+=1001;
	    ifr_axis.add("ifr_number",ifr);
	  } else {
	    ifr_axis.add("ifr_number",ifrSelection_p);
	  }
	  Vector<String> antName=msc.antenna().name().getColumn();
	  Vector<String> ifrName(nIfr);
	  Vector<String> sName(nIfr);
	  // get common prefix in antenna names by comparing first and 
	  // last antenna in table
	  String prefix = 
	    common_prefix(antName(0),antName(antName.nelements()-1));
	  Matrix<Double> antPos=msc.antenna().position().getColumn();
	  Vector<Double> baseline(nIfr);
	  // PROBLEM: antName elements have string size extending beyond \0
	  // string catenation doesn't work correctly!
	  for (Int k=0; k<nIfr; k++) {
	    Int ant1 = ifrSelection_p(k)/1000;
	    Int ant2 = ifrSelection_p(k)%1000;
	    ifrName(k)=antName(ant1).before('\0');
	    ifrName(k)+="-";
	    ifrName(k)+=antName(ant2).before('\0');
	    sName(k)=String(antName(ant1).after(prefix)).before('\0');
	    sName(k)+="-";
	    sName(k)+=String(antName(ant2).after(prefix)).before('\0');
	    baseline(k)=sqrt(square(antPos(0,ant2)-antPos(0,ant1))+
			     square(antPos(1,ant2)-antPos(1,ant1))+
			     square(antPos(2,ant2)-antPos(2,ant1)));
	  }
	  ifr_axis.add("ifr_name",ifrName);
	  ifr_axis.add("ifr_shortname",sName);
	  ifr_axis.add("baseline",baseline);
	  axis_info.add("ifr_axis",ifr_axis);
	  // 4. time info
	  GlishRecord time_axis;
	  msd_p.setAntennas(msc.antenna());
	  Vector<Double> time=msc.time().getColumn();
	  Vector<Int> fieldId=msc.fieldId().getColumn();
	  Int lastFieldId=-1;
	  Double lastTime=-1;
	  Double startOfDay=( nTime>0 ? 
			      C::day*int(time(timeSlotRow(0))/C::day) : 0);
	  Int nT = (average ? 1 : nTime);
	  Vector<Double> times(nT),ut(nT),ha(nT),last(nT);
	  MEpoch ep=msc.timeMeas()(0);
	  Bool doUT=False, doHA=False, doLAST=False;
	  for (Int k=0; k<nItems; k++) {
	    Int enumval=MSS::field(downcase(items(k)));
	    if (enumval == MSS::UT) doUT=True;
	    if (enumval == MSS::LAST) doLAST=True;
	    if (enumval == MSS::HA) doHA=True;
	  }
	  // Note: HA conversion assumes there is a single fieldId for
	  // each time slot.
	  if (average) {
	    times(0)=0;
	    for (Int k=0; k<nTime; k++) {
	      times(0)+=time(timeSlotRow(k));
	    }
	    if (nTime>0) times(0)/=nTime;
	  } else {
	    for (Int k=0; k<nTime; k++) {
	      times(k)=time(timeSlotRow(k));
	    }
	  }
	  for (Int k=0; k<nT; k++) {
	    if (doUT) {
	      ut(k)=times(k)-startOfDay;
	    }
	    if (doLAST || doHA) {
	      Int curFieldId=fieldId(timeSlotRow(k));
	      if (curFieldId!=lastFieldId) {
		if (msc.field().numPoly()(curFieldId)==0)
		  msd_p.setFieldCenter(msc.field().phaseDirMeas(curFieldId));
		lastFieldId=curFieldId;
	      }
	      if (msc.field().numPoly()(curFieldId)>0)
		msd_p.setFieldCenter(msc.field().
				     phaseDirMeas(curFieldId,times(k)));
	      ep.set(MVEpoch(times(k)/C::day));
	      msd_p.setEpoch(ep);
	      lastTime=times(k);
	      if (doHA) ha(k)=msd_p.hourAngle()/C::_2pi*C::day;
	      if (doLAST) last(k)=msd_p.last().getValue().get();
	    }
	  }
	  time_axis.add("MJDseconds",times);
	  if (doUT) time_axis.add("UT",ut);
	  if (doHA) time_axis.add("HA",ha);
	  if (doLAST) time_axis.add("LAST",last);
	  axis_info.add("time_axis",time_axis);
	}
	out.add("axis_info",axis_info);
      }
      break;
    case MSS::CORRELATOR_GAIN:
      wantCorrGain=True;
    case MSS::OBS_CORRELATOR_GAIN:
      wantObsCorrGain=True;
    case MSS::DATA:
      wantData=True;
      break;
    case MSS::CORRECTED_DATA:
      wantCData=True;
      break;
    case MSS::MODEL_DATA:
      wantMData=True;
      break;
    case MSS::RESIDUAL_DATA:
      wantRData=True;
      break;
    case MSS::OBS_RESIDUAL_DATA:
      wantORData=True;
      break;
    case MSS::FEED1:
      // problem: doIfrAxis won't work with multiple feeds
      if (doIfrAxis) {
	os << LogIO::WARN << "Note - multiple feeds not supported for"
	  " ifrAxis==True" << LogIO::POST;
      }
      {
	Vector<Int> feed=msc.feed1().getColumn();
	if (average) averageId(feed);
	if (oneBased) feed+=1;
	out.add(item,feed);
      }
      break;
    case MSS::FEED2:
      if (doIfrAxis) {
	os << LogIO::WARN << "Note - multiple feeds not supported for"
	  " ifrAxis==True" << LogIO::POST;
      }
      {
	Vector<Int> feed=msc.feed2().getColumn();
	if (average) averageId(feed);
	if (oneBased) feed+=1;
	out.add(item,feed);
      }
      break;
    case MSS::FIELD_ID:
      if (doIfrAxis) {
	Vector<Int> fldId=msc.fieldId().getColumn();
	Vector<Int> fldTime(nTime);
	for (Int k=0; k<nTime; k++) {
	  fldTime(k)=fldId(timeSlotRow(k));
	}
	if (average) averageId(fldTime);
	if (oneBased) fldTime+=1;
	out.add(item,fldTime);
      } else {
	Vector<Int> f=msc.fieldId().getColumn();
	if (average) averageId(f);
	if (oneBased) f+=1;
	out.add(item,f);
      }
      break;
    case MSS::FLAG:
      wantFlag=True;
      break;
    case MSS::FLAG_ROW:
      if (doIfrAxis) {
	Matrix<Bool> itFlag(nIfr,nTime);
	itFlag.set(True); // flag unfilled slots
	Vector<Bool> flagRow=msc.flagRow().getColumn();
	for (Int k=0; k<nRow; k++) {
	  itFlag(ifrSlot(k),timeSlot(k))=flagRow(k);
	}
	out.add(item,itFlag);
      } else {
	out.add(item,msc.flagRow().getColumn());
      }
      break;
    case MSS::FLAG_SUM:
      wantFlagSum=True;
      break;
    case MSS::IFR_NUMBER:
      {
	if (doIfrAxis) {
	  if (oneBased) {
	    Array<Int> ifr;
	    ifr=ifrSelection_p;
	    ifr+=1001;
	    out.add(item,ifr);
	  } else {
	    out.add(item,ifrSelection_p);
	  }
	} else {
	 Vector<Int> ant1=msc.antenna1().getColumn();
	  Array<Int> ant2=msc.antenna2().getColumn();
	  if (oneBased) {
	    ant1+=1; ant2+=1;
	  }
	  ant1*=1000;
	  ant1+=ant2;
	  if (average) averageId(ant1);
	  out.add(item,ant1);
	}
      }
      break;
    case MSS::IMAGINARY:
      wantImag=True;
      break;
    case MSS::CORRECTED_IMAGINARY:
      wantCImag=True;
    case MSS::MODEL_IMAGINARY:
      wantMImag=True;
      break;
    case MSS::RESIDUAL_IMAGINARY:
      wantRImag=True;
      break;
    case MSS::OBS_RESIDUAL_IMAGINARY:
      wantORImag=True;
      break;
    case MSS::IMAGING_WEIGHT:
      {
	if (!msc.imagingWeight().isNull()) {
	  // averaging doesn't make sense here
	  // but we should apply channel selection
	  Array<Float> weight;
	  if (useSlicer_p) {
	    Slice mySlice(chanSel_p(1),chanSel_p(0),chanSel_p(3));
	    Slicer slicer(mySlice);
	    weight=msc.imagingWeight().getColumn(slicer);
	  }
	  else weight=msc.imagingWeight().getColumn(); 
	  if (doIfrAxis) {
	    Array<Float> weight3d; weight3d.set(0);
	    IPosition start(2,0,0),end(2,chanSel_p(0),0);
	    IPosition start2(3,0,0,0),end2(3,chanSel_p(0),0,0);
	    for (Int k=0; k<nRow; k++) {
	      start(1)=end(1)=k;
	      start2(1)=end2(1)=ifrSlot(k);
	      start2(2)=end2(2)=timeSlot(k);
	      weight3d(start2,end2).nonDegenerate()=
		weight(start,end).nonDegenerate();
	    }
	    out.add(item,weight3d);
	  } else {
	    out.add(item,weight);
	  }
	} else {
	  os << LogIO::WARN << "IMAGING_WEIGHT column doesn't exist"<< LogIO::POST;
	}
      }
      break;
    case MSS::PHASE:
      wantPhase=True;
      break;
    case MSS::CORRECTED_PHASE:
      wantCPhase=True;
      break;
    case MSS::MODEL_PHASE:
      wantMPhase=True;
      break;
    case MSS::RESIDUAL_PHASE:
      wantRPhase=True;
      break;
    case MSS::OBS_RESIDUAL_PHASE:
      wantORPhase=True;
      break;
    case MSS::REAL:
      wantReal=True;
      break;
    case MSS::CORRECTED_REAL:
      wantCReal=True;
      break;
    case MSS::MODEL_REAL:
      wantMReal=True;
      break;
    case MSS::RESIDUAL_REAL:
      wantRReal=True;
      break;
    case MSS::OBS_RESIDUAL_REAL:
      wantORReal=True;
      break;
    case MSS::SCAN_NUMBER:
      if (doIfrAxis) {
	Vector<Int> scanNo=msc.scanNumber().getColumn();
	Vector<Int> scanNoTime(nTime);
	for (Int k=0; k<nTime; k++) {
	  scanNoTime(k)=scanNo(timeSlotRow(k));
	}
	if (average) averageId(scanNoTime);
	if (oneBased) scanNoTime+=1;
	out.add(item,scanNoTime);
      } else {
	Vector<Int> scan=msc.scanNumber().getColumn();
	if (average) averageId(scan);
	if (oneBased) scan+=1;
	out.add(item,scan);
      }
      break;
    case MSS::DATA_DESC_ID:
      if (doIfrAxis) {
	Vector<Int> ddNo=msc.dataDescId().getColumn();
	Vector<Int> ddTime(nTime);
	for (Int k=0; k<nTime; k++) {
	  ddTime(k)=ddNo(timeSlotRow(k));
	}
	if (average) averageId(ddTime);
	if (oneBased) ddTime+=1;
	out.add(item,ddTime);
      } else {
	Vector<Int> dd=msc.dataDescId().getColumn();
	if (average) averageId(dd);
	if (oneBased) dd+=1;
	out.add(item,dd);
      }
      break;
    case MSS::TIME:
      {
	Vector<Double> time=msc.time().getColumn();
	if (doIfrAxis) {
	  Vector<Double> times(nTime);
	  for (Int k=0; k<nTime; k++) {
	    times(k)=time(timeSlotRow(k));
	  }
	  if (average) averageDouble(times);
	  out.add(item,times);
	} else {
	  if (average) averageDouble(time);
	  out.add(item,time);
	}
      }
      break;
    case MSS::UVW:
      if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
      if (doIfrAxis) {
	Cube<Double> uvw2(uvw.shape()(0),nIfr,nTime); uvw2.set(0);
	for (Int k=0; k<nRow; k++) {
	  Int ifr=ifrSlot(k);
	  Int time=timeSlot(k);
	  uvw2(0,ifr,time)=uvw(0,k);
	  uvw2(1,ifr,time)=uvw(1,k);
	  uvw2(2,ifr,time)=uvw(2,k);
	}
	out.add(item,uvw2);
      } else {
	out.add(item,uvw);
      }
      break;
    case MSS::U:
    case MSS::V:
    case MSS::W:
      {
	Int index=fld-MSS::U;
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	if (doIfrAxis) {
	  Matrix<Double> uvw2(nIfr,nTime); uvw2.set(0);
	  for (Int k=0; k<nRow; k++) {
	    uvw2(ifrSlot(k),timeSlot(k))=uvw(index,k);
	  }
	  out.add(item,uvw2);
	} else {
	  out.add(item,uvw.row(index));
	}
      }
      break;
    case MSS::UVDIST:
      {
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	Vector<Double> u2(uvw.row(0)),v2(uvw.row(1));
	u2*=u2;
	v2*=v2;
	u2+=v2;
	// take square root - could use u2.apply(sqrt) but this can cause
	// link conflicts with fortran
	Bool deleteIt;
	Double* pu2 = u2.getStorage(deleteIt);
	for (Int i=0; i<nRows; i++) pu2[i]=sqrt(pu2[i]);
	if (doIfrAxis) {
	  Matrix<Double> uvd(nIfr,nTime); uvd.set(0);
	  for (Int k=0; k<nRow; k++) uvd(ifrSlot(k),timeSlot(k))=pu2[k];
	  u2.putStorage(pu2,deleteIt);
	  out.add(item,uvd);
	} else {
	  u2.putStorage(pu2,deleteIt);
	  out.add(item,u2);
	}
      }
      break;
    case MSS::WEIGHT:
      wantWeight=True;
      break;
    case MSS::HA:
    case MSS::LAST:
    case MSS::UT:
      // do nothing, only used within AXIS_INFO
      break;
    case MSS::UNDEFINED:
    default:
      os << LogIO::WARN << "Unrecognized field in input ignored: "<<
	item<<LogIO::POST;
      break;
    }
  }
  uvw.resize(0,0); // reclaim storage before we get the big arrays
  // save the flags and weights if we are averaging data over time/row
  Array<Bool> flags,dataflags;
  Array<Float> weights;
  if (wantWeight || average) {
    Vector<Float> wt(msc.weight().getColumn());
    if (doIfrAxis) {
      Matrix<Float> wts(nIfr,nTime); wts.set(0);
      {for (Int i=0; i<nRow; i++) wts(ifrSlot(i),timeSlot(i))=wt(i); }
      if (wantWeight) {
	if (average) {
	  // averaging weights doesn't make sense, 
	  // return sum of weights instead
	  Vector<Float> sumwt(nIfr); sumwt=0;
	  for (Int i=0; i<nIfr; i++) {
	    for (Int j=0; j<nTime; j++) sumwt(i)+=wts(i,j);
	  }
	  out.add("weight",sumwt);
	} else {
	  out.add("weight",wts);
	}
      }
      weights.reference(wts);
    } else {
      if (wantWeight) {
	if (average) {
	  // return sum of weights
	  Float sumwt=0;
	  for (Int i=0; i<nRow; i++) sumwt+=wt(i);
	  out.add("weight",sumwt);
	} else {
	  out.add("weight",wt);
	}
      }
      weights.reference(wt);
    }
  }
  if (wantFlag || wantFlagSum ||average) {
    Array<Bool> flag;
    getAveragedFlag(flag,msc.flag());
    uInt nPol=flag.shape()(0), nChan=flag.shape()(1), nRow=flag.shape()(2);
    if (doIfrAxis) {
      MSSelUtil2<Bool>::reorderData(flag,ifrSlot,nIfr,timeSlot,nTime,True);
    }
    if (average) flags=flag;
    if (wantFlag && !average) {
      out.add("flag",flag);
    }
    if (wantFlagSum) {
      if (doIfrAxis) {
	Cube<Int> flagSum(nPol,nChan,nIfr);
	flagSum=0;
	IPosition indx(4);
	indx(0)=0; 
	for (uInt j=0; j<nPol; j++, indx(0)++) {
	  indx(1)=0; 
	  for (uInt k=0; k<nChan; k++, indx(1)++) {
	    indx(2)=0; 
	    for (Int l=0; l<nIfr; l++, indx(2)++) {
	      Int count=0;
	      for (indx(3)=0; indx(3)<nTime; indx(3)++) {
		if (flag(indx)) count++;
	      }
	      flagSum(j,k,l)=count;
	    }
	  }
	}
	out.add("flag_sum",flagSum);
      } else {
	Matrix<Int> flagSum(nPol,nChan);
	flagSum=0;
	Cube<Bool> flag2(flag);
	for (uInt j=0; j<nPol; j++) {
	  for (uInt k=0; k<nChan; k++) {
	    Int count=0;
	    for (uInt l=0; l<nRow; l++) {
	      if (flag2(j,k,l)) count++;
	    }
	    flagSum(j,k)=count;
	  }
	}
	out.add("flag_sum",flagSum);
      }
    }
  }
  Bool wantOR = ToBool(wantORAmp || wantORPhase || wantORReal || wantORImag ||
		       wantORData);
  Bool wantR = ToBool(wantRAmp || wantRPhase || wantRReal || wantRImag ||
		      wantRData );
  Array<Complex> observed_data;
  if (wantAmp || wantPhase || wantReal || wantImag || wantData
      || wantOR || wantObsCorrGain) {
    // get the data
    Array<Complex> data;
    if (convert_p) {
      os << LogIO::WARN << "Polarization conversion of uncalibrated"
	 << " data may give incorrect results"<< LogIO::POST;
    }
    getAveragedData(data,msc.data());
    if (doIfrAxis) MSSelUtil2<Complex>::
      reorderData(data,ifrSlot,nIfr,timeSlot,nTime,Complex());
    if (wantOR || wantObsCorrGain) {
      if (average) observed_data=data;
      else observed_data.reference(data);
    }
    if (average) timeAverage(dataflags,data,flags,weights);
    if (wantAmp) out.add("amplitude",amplitude(data));
    if (wantPhase) out.add("phase",phase(data));
    if (wantReal) out.add("real",real(data));
    if (wantImag) out.add("imaginary",imag(data));
    if (wantData) out.add("data",data);
  }
  Array<Complex> corrected_data;
  if  (wantCAmp || wantCPhase || wantCReal || wantCImag || wantCData
       || wantR || wantCorrGain) {
    if (!msc.correctedData().isNull()) {
      // get the data
      Array<Complex> data;
      getAveragedData(data,msc.correctedData());
      if (doIfrAxis) MSSelUtil2<Complex>::
	reorderData(data,ifrSlot,nIfr,timeSlot,nTime,Complex());
      if (wantR || wantCorrGain) {
	if (average) corrected_data=data;
	else corrected_data.reference(data);
      }
      if (average) timeAverage(dataflags,data,flags,weights);
      if (wantCAmp) out.add("corrected_amplitude",amplitude(data));
      if (wantCPhase) out.add("corrected_phase",phase(data));
      if (wantCReal) out.add("corrected_real",real(data));
      if (wantCImag) out.add("corrected_imaginary",imag(data));
      if (wantCData) out.add("corrected_data",data);
    } else {
      os << LogIO::WARN << "CORRECTED_DATA column doesn't exist"<<LogIO::POST;
    }
  }
  
  Array<Complex> model_data;
  if (wantMAmp || wantMPhase || wantMReal || wantMImag || wantMData ||
      wantR || wantOR || wantCorrGain || wantObsCorrGain) {
    if (!msc.modelData().isNull()) {
      // get the data
      Array<Complex> data;
      getAveragedData(data,msc.modelData());
      if (doIfrAxis) MSSelUtil2<Complex>::
	reorderData(data,ifrSlot,nIfr,timeSlot,nTime,Complex());
      if (wantR || wantOR || wantCorrGain || wantObsCorrGain) {
	if (average) {
	  model_data=data;
	} else {
	  model_data.reference(data);
	}
      }
      if (average) timeAverage(dataflags,data,flags,weights);
      if (wantMAmp) out.add("model_amplitude",amplitude(data));
      if (wantMPhase) out.add("model_phase",phase(data));
      if (wantMReal) out.add("model_real",real(data));
      if (wantMImag) out.add("model_imaginary",imag(data));
      if (wantMData) out.add("model_data",data);
      if (wantR) {
	Array<Complex> res_data; 
	res_data=corrected_data; res_data-=model_data;
	if (average) timeAverage(dataflags,res_data,flags,weights);
	if (wantRAmp) out.add("residual_amplitude",amplitude(res_data));
	if (wantRPhase) out.add("residual_phase",phase(res_data));
	if (wantRReal) out.add("residual_real",real(res_data));
	if (wantRImag) out.add("residual_imaginary",imag(res_data));
	if (wantRData) out.add("residual_data",res_data);
      }
      if (wantOR) {
	Array<Complex> res_data; 
	res_data=observed_data; res_data-=model_data;
	if (average) timeAverage(dataflags,res_data,flags,weights);
	if (wantORAmp) out.add("obs_residual_amplitude",
			       amplitude(res_data));
	if (wantORPhase) out.add("obs_residual_phase",phase(res_data));
	if (wantORReal) out.add("obs_residual_real",real(res_data));
	if (wantORImag) out.add("obs_residual_imaginary",imag(res_data));
	if (wantORData) out.add("obs_residual_data",res_data);
      }
      if (wantCorrGain || wantObsCorrGain) {
	Array<Complex> gain;
	LogicalArray mask(model_data!=Complex(0.));
	if (wantCorrGain) {
	  gain=corrected_data; 
	  gain/=model_data(mask);
	  gain(!mask)=1.0;
	  if (average) timeAverage(dataflags,gain,flags,weights);
	  out.add("correlator_gain",gain);
	}
	if (wantObsCorrGain) {
	  gain=observed_data;
	  gain/=model_data(mask);
	  gain(!mask)=1.0;
	  if (average) timeAverage(dataflags,gain,flags,weights);
	  out.add("obs_correlator_gain",gain);
	}
      }
    } else {
      os << LogIO::WARN << "MODEL_DATA column doesn't exist"<<LogIO::POST;
    }
  }
  // only have averaged flags if some data item was requested as well
  if (average && wantFlag){
    out.add("flag",dataflags);
    if (dataflags.nelements()==0) {
      os << LogIO::WARN <<"Flags not calculated because no DATA derived "
	"item was specified" << LogIO::POST;
    }
  }
  return out;
}

Bool MSSelector::putData(const GlishRecord& items)
{
  if (!initSel_p) {
    os << LogIO::WARN <<"Initializing selection with dd=0"
       << LogIO::POST;
    initSelection();
  }
  if (!initSel_p) return False;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  if (!selms_p.isWritable()) {
    os << LogIO::SEVERE << "MeasurementSet is not writable"<< LogIO::POST;
    return False;
  }

  MSColumns msc(selms_p);
  if (useSlicer_p) {
    if (!haveSlicer_p) {
      if (wantedOne_p>=0) makeSlicer(wantedOne_p,1);
      else makeSlicer(0,msc.data().shape(0)(0));
    }
  }
  Int n=items.nelements();
  for (Int i=0; i<n; i++) {
    String item=downcase(items.name(i));
    MSS::Field fld=MSS::field(item);
    switch (fld) {
    case MSS::DATA:
      {
	// averaging not supported
	if (chanSel_p(2)>1) {
	  os << LogIO::SEVERE << "Averaging not supported when writing data"
	     << LogIO::POST;
	  break;
	}
	if (convert_p) {
	  os << LogIO::SEVERE <<"Polarization conversion not supported "
	     << "when writing data" << LogIO::POST;
	  return False;
	}	Array<Complex> data;
	if (GlishArray(items.get(i)).get(data)) {
	  if (data.ndim()==4) {
	    if (data.shape()(2)==Int(rowIndex_p.nrow()) && 
		data.shape()(3)==Int(rowIndex_p.ncolumn())) {
	      MSSelUtil2<Complex>::reorderData(data, rowIndex_p, 
					       selms_p.nrow());
	    } else {
	      os << LogIO::SEVERE<<"Data shape inconsistent with "
		"current selection"<< LogIO::POST;
	      break;
	    }
	  }
	  if (data.ndim()==3) {
	    if (useSlicer_p) msc.data().putColumn(slicer_p, data);
	    else msc.data().putColumn(data);
	  }
	}
      }
      break;
    case MSS::CORRECTED_DATA:
      {
	// averaging not supported
	if (chanSel_p(2)>1) {
	  os << LogIO::SEVERE << "Averaging not supported when writing data"
	     << LogIO::POST;
	  break;
	}
	if (convert_p) {
	  os << LogIO::SEVERE <<"Polarization conversion not supported "
	     << "when writing data" << LogIO::POST;
	  return False;
	}
	Array<Complex> data;
	if (GlishArray(items.get(i)).get(data) &&
	    !msc.correctedData().isNull()) {
	  if (data.ndim()==4) {
	    if (data.shape()(2)==Int(rowIndex_p.nrow()) && 
		data.shape()(3)==Int(rowIndex_p.ncolumn())) {
	      MSSelUtil2<Complex>::reorderData(data, rowIndex_p, 
					       selms_p.nrow());
	    } else {
	      os << LogIO::SEVERE<<"Data shape inconsistent with "
		"current selection"<< LogIO::POST;
	      break;
	    }
	  }
	  if (data.ndim()==3) {
	    if (useSlicer_p) msc.correctedData().putColumn(slicer_p, data);
	    else msc.correctedData().putColumn(data);
	  }
	}
      }
      break;
    case MSS::MODEL_DATA:
      {
	// averaging not supported
	if (chanSel_p(2)>1) {
	  os << LogIO::SEVERE << "Averaging not supported when writing data"
	     << LogIO::POST;
	  break;
	}
	if (convert_p) {
	  os << LogIO::SEVERE <<"Polarization conversion not supported "
	     << "when writing data" << LogIO::POST;
	  return False;
	}
	Array<Complex> data;
	if (GlishArray(items.get(i)).get(data) && !msc.modelData().isNull()) {
	  if (data.ndim()==4) {
	    if (data.shape()(2)==Int(rowIndex_p.nrow()) && 
		data.shape()(3)==Int(rowIndex_p.ncolumn())) {
	      MSSelUtil2<Complex>::reorderData(data, rowIndex_p, 
					       selms_p.nrow());
	    } else {
	      os << LogIO::SEVERE<<"Data shape inconsistent with "
		"current selection"<< LogIO::POST;
	      break;
	    }
	  }
	  if (data.ndim()==3) {
	    if (useSlicer_p) msc.modelData().putColumn(slicer_p, data);
	    else msc.modelData().putColumn(data);
	  }
	}
      }
      break;
    case MSS::FLAG:
      {
	Array<Bool> flag;
	if (GlishArray(items.get(i)).get(flag)) {
	  if (flag.ndim()==4) {
	    if (flag.shape()(2)==Int(rowIndex_p.nrow()) && 
		flag.shape()(3)==Int(rowIndex_p.ncolumn())) {
	      MSSelUtil2<Bool>::reorderData(flag, rowIndex_p, selms_p.nrow());
	    } else {
	      os << LogIO::SEVERE<<"Flag shape inconsistent with "
		"current selection"<< LogIO::POST;
	      break;
	    }
	  }
	  if (flag.ndim()==3) {
	    putAveragedFlag(flag, msc.flag());
	  }
	}
      }
      break;
    case MSS::FLAG_ROW:
      {
	Array<Bool> flagRow;
	if (GlishArray(items.get(i)).get(flagRow)) {
	  if (flagRow.ndim()==2) {
	    reorderFlagRow(flagRow);
	  }
	  if (flagRow.ndim()==1) {
	    msc.flagRow().putColumn(flagRow);
	  }
	}
      }
      break;
    case MSS::IMAGING_WEIGHT:
      {
	// averaging not supported
	if (chanSel_p(2)>1) {
	  os << LogIO::SEVERE << "Averaging not supported when writing data"
	     << LogIO::POST;
	  break;
	}
	Array<Float> weight;
	if (GlishArray(items.get(i)).get(weight) && weight.ndim()==2 &&
	    !msc.imagingWeight().isNull()) {
	  if (useSlicer_p) {
	    Slice mySlice(chanSel_p(1),chanSel_p(0),chanSel_p(3));
	    Slicer slicer(mySlice);
	    msc.imagingWeight().putColumn(slicer, weight);
	  }
	  else msc.imagingWeight().putColumn(weight); 
	}
      }
      break;
    case MSS::WEIGHT:
      {
	Array<Float> weight;
	if (GlishArray(items.get(i)).get(weight)) {
	  if (weight.ndim()==2) {
	    reorderWeight(weight);
	  }
	  if (weight.ndim()==1) {
	    msc.weight().putColumn(weight);
	  }
	}
      }
      break;
    case MSS::UNDEFINED:
    default:
      os << LogIO::WARN << "Unrecognized field in input ignored: "<<
	item<<LogIO::POST;
      break;
    }
  }
  return True;
}

Bool MSSelector::iterInit(const Vector<String>& columns,
			  Double interval, Int maxRows)
{
  if (!initSel_p) {
    os << LogIO::WARN <<"Initializing selection with dd=0"
       << LogIO::POST;
    initSelection();
  }
  if (!initSel_p) return False;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }  
  Int n=columns.nelements();
  Block<Int> col(n);
  for (Int i=0; i<n; i++) {
    col[i]=MS::columnType(columns(i));
    if (col[i]==MS::UNDEFINED_COLUMN) {
      os << LogIO::SEVERE << "Iteration initialization failed: unrecognized"
	" column name: "<<columns(i)<<LogIO::POST;
      return False;
    }
  }
  if (msIter_p) delete msIter_p;
  msIter_p=new MSIter(selms_p,col,interval);
  maxRow_p = maxRows;
  return True;
}

Bool MSSelector::iterNext()
{
  Bool more=False;
  if (msIter_p) {
    Int nIterRow=msIter_p->table().nrow();
    if (startRow_p==0 || startRow_p> nIterRow) {
      (*msIter_p)++;
      more=msIter_p->more();
      if (more) nIterRow=msIter_p->table().nrow();
      startRow_p = 0;
    }
    if (startRow_p>0 || (more && maxRow_p>0 && nIterRow>maxRow_p)) {
      Int nRow=min(maxRow_p,nIterRow-startRow_p);
      selRows_p.resize(nRow);
      indgen(selRows_p,uInt(startRow_p),uInt(1));
      startRow_p+=maxRow_p;
      selms_p=msIter_p->table()(selRows_p);
      more=True;
    } else {
      if (more) selms_p=msIter_p->table();
      else selms_p=msIter_p->ms(); // put back the original selection at the end
    }
  }
  return more;
}

Bool MSSelector::iterOrigin()
{
  Bool ok=False;
  if (msIter_p) {
    startRow_p=0;
    msIter_p->origin();
    Int nIterRow=msIter_p->table().nrow();
    if (maxRow_p==0 || nIterRow<=maxRow_p) {
      selms_p=msIter_p->table();
    } else {
      selRows_p.resize(maxRow_p);
      indgen(selRows_p,uInt(0),uInt(1));
      selms_p=msIter_p->table()(selRows_p);
      startRow_p=maxRow_p;
    }
    ok=True;
  }
  return ok;
}
  
Bool MSSelector::iterEnd()
{
  if (!msIter_p) return False;
  selms_p=msIter_p->ms();
  return True;
}

void MSSelector::getAveragedData(Array<Complex>& avData, 
				 const ROArrayColumn<Complex>& col)
{
  Array<Complex> data;
  if (useSlicer_p) {
    if (!haveSlicer_p) {
      if (wantedOne_p>=0) makeSlicer(wantedOne_p,1);
      else makeSlicer(0,col.shape(0)(0));
    }
    data=col.getColumn(slicer_p);
  } else {
    data=col.getColumn();
  }
  Int nPol=data.shape()(0);
  Int nChan=chanSel_p(0);
  Int nRow=data.shape()(2);
  avData.resize(IPosition(3,nPol,nChan,nRow));
  if (chanSel_p(2)==1) {
    // no averaging, just copy the data across
    avData=data;
  } else {
    // Average channel by channel
    for (Int i=0; i<nChan; i++) {
      // if width>1, the slice doesn't have an increment, so we take big steps
      Int chn=i*chanSel_p(3);
      Array<Complex> ref(avData(IPosition(3,0,i,0),IPosition(3,nPol-1,i,nRow-1)));
      ref=data(IPosition(3,0,chn,0),IPosition(3,nPol-1,chn,nRow-1));
      // average over channels
      // TODO: take flagging into account
      for (Int j=1; j<chanSel_p(2); j++) {
	ref+=data(IPosition(3,0,chn+j,0),IPosition(3,nPol-1,chn+j,nRow-1));
      }
      // This is horrible..
      ref/=Complex(chanSel_p(2));
    }
  }
  // do the polarization conversion
  if (convert_p) {
    Array<Complex> out;
    stokesConverter_p.convert(out,avData);
    avData.reference(out);
  }
}

//# TODO: change the flagging to AND instead of OR and do the right
// thing when averaging the data

void MSSelector::getAveragedFlag(Array<Bool>& avFlag, 
				 const ROArrayColumn<Bool>& col)
{
  Array<Bool> flag;
  if (useSlicer_p) {
    if (!haveSlicer_p) {
      if (wantedOne_p>=0) makeSlicer(wantedOne_p,1);
      else makeSlicer(0,col.shape(0)(0));
    }
    flag=col.getColumn(slicer_p);
  } else {
    flag=col.getColumn();
  }
  Int nPol=flag.shape()(0);
  Int nChan=chanSel_p(0);
  Int nRow=flag.shape()(2);
  avFlag.resize(IPosition(3,nPol,nChan,nRow));
  if (chanSel_p(2)==1) {
    // no averaging, just copy flags
    avFlag=flag;
  } else {
    for (Int i=0; i<nChan; i++) {
      Int chn=i*chanSel_p(3);
      // average over channels
      for (Int j=0; j<nPol;j++) {
	for (Int k=0; k<nRow; k++) {
	  avFlag(IPosition(3,j,i,k))=
	    ToBool(anyEQ(flag(IPosition(3,j,chn,k),
			      IPosition(3,j,chn+chanSel_p(2)-1,k)),True));
	}
      }
    }
  }
  if (convert_p) {
    Array<Bool> out;
    stokesConverter_p.convert(out,avFlag);
    avFlag.reference(out);
  }
}

void MSSelector::putAveragedFlag(const Array<Bool>& avFlag, 
				 ArrayColumn<Bool>& col)
{
  Array<Bool> polFlag=avFlag;
  if (convert_p) {
    Array<Bool> out;
    stokesConverter_p.invert(out,polFlag);
    polFlag.reference(out);
  }
  Array<Bool> flag;
  if (chanSel_p(2)>1) {
    // we need to undo the averaging and distribute the flags
    IPosition shape=polFlag.shape();
    shape(1)=(chanSel_p(0)-1)*chanSel_p(3)+chanSel_p(2);
    if (chanSel_p(3)>chanSel_p(2)) {
      // there are gaps - need to read data before writing it back
      if (useSlicer_p) {
	flag=col.getColumn(slicer_p);
      } else {
	flag=col.getColumn();
      }
    } else {
      flag.resize(shape);
    }
    Int nChan=chanSel_p(0), st=chanSel_p(1), w=chanSel_p(2), inc=chanSel_p(3),
      nRow=shape(2);
    IPosition st1(3,0,st,0),st2(3,0,0,0),end1(3,shape(0)-1,st,nRow-1),
      end2(3,shape(0)-1,0,nRow-1);
    for (Int i=0; i<nChan; i++) {
      st2(1)=end2(1)=i;
      for (Int j=0; j<w; j++) {
	st1(1)=end1(1)=st+i*inc+j;
	flag(st1,end1)=polFlag(st2,end2);
      }
    }
  } else {
    flag.reference(polFlag);
  }
  if (useSlicer_p) col.putColumn(slicer_p, flag);
  else col.putColumn(flag);
}

void MSSelector::makeSlicer(Int start, Int nCorr)
{
  if (chanSel_p(2)==1) {
    // width is one, we can use a stride
    slicer_p=Slicer(Slice(start,nCorr),
		    Slice(chanSel_p(1),chanSel_p(0),chanSel_p(3)));
  } else {
    slicer_p=Slicer(Slice(start,nCorr),
		    Slice(chanSel_p(1),
			  1+(chanSel_p(0)-1)*chanSel_p(3)+(chanSel_p(2)-1)));
  }
  haveSlicer_p=True;
}

// reorder from 2d to 1d (removing ifr axis)
void MSSelector::reorderFlagRow(Array<Bool>& flagRow)
{
  Int nIfr=flagRow.shape()(0), nTime=flagRow.shape()(1);
  Int nRow=selms_p.nrow();
  Bool deleteFlag, deleteRow;
  const Bool* pFlag=flagRow.getStorage(deleteFlag);
  const Int* pRow=rowIndex_p.getStorage(deleteRow);
  Vector<Bool> rowFlag(nRow);
  Int offset=0;
  for (Int i=0; i<nTime; i++, offset+=nIfr) {
    for (Int j=0; j<nIfr; j++) {
      Int k=pRow[offset+j];
      if (k>0) {
	rowFlag(k)=pFlag[offset+j];
      }
    }
  }
  flagRow.freeStorage(pFlag,deleteFlag);
  rowIndex_p.freeStorage(pRow,deleteRow);
  flagRow.reference(rowFlag);
}

// reorder from 2d to 1d (removing ifr axis)
void MSSelector::reorderWeight(Array<Float>& weight)
{
  Int nIfr=weight.shape()(0), nTime=weight.shape()(1);
  Int nRow=selms_p.nrow();
  Bool deleteWeight, deleteRow;
  const Float* pWeight=weight.getStorage(deleteWeight);
  const Int* pRow=rowIndex_p.getStorage(deleteRow);
  Vector<Float> rowWeight(nRow);
  Int offset=0;
  for (Int i=0; i<nTime; i++, offset+=nIfr) {
    for (Int j=0; j<nIfr; j++) {
      Int k=pRow[offset+j];
      if (k>0) {
	rowWeight(k)=pWeight[offset+j];
      }
    }
  }
  weight.freeStorage(pWeight,deleteWeight);
  rowIndex_p.freeStorage(pRow,deleteRow);
  weight.reference(rowWeight);
}

// average data (with flags & weights applied) over it's last axis (time or
// row), return in data (overwritten), dataFlag gives new flags.
void MSSelector::timeAverage(Array<Bool>& dataFlag, Array<Complex>& data, 
			     const Array<Bool>& flag, 
			     const Array<Float>& weight)
{
  Bool delData,delFlag,delWeight;
  const Complex* pdata=data.getStorage(delData);
  const Bool* pflag=flag.getStorage(delFlag);
  const Float* pweight=weight.getStorage(delWeight);
  Int nPol=data.shape()(0),nChan=data.shape()(1);
  Int nIfr=1, nTime=data.shape()(2);
  Array<Complex> out;
  if (data.ndim()==4) {
    nIfr=nTime;
    nTime=data.shape()(3);
    out.resize(IPosition(3,nPol,nChan,nIfr));
  } else {
    out.resize(IPosition(2,nPol,nChan));
  }
  Array<Float> wt(IPosition(3,nPol,nChan,nIfr));
  dataFlag.resize(IPosition(3,nPol,nChan,nIfr));
  dataFlag.set(True);
  Bool delDataflag, delWt, delOut;
  Float* pwt=wt.getStorage(delWt);
  Complex* pout=out.getStorage(delOut);
  Bool* pdflags=dataFlag.getStorage(delDataflag);
  out=0;
  wt=0;
  Int offset=0,off1=0,offw=0;
  for (Int l=0; l<nTime; l++) {
    off1=0;
    for (Int k=0; k<nIfr; k++) {
      for (Int j=0; j<nChan; j++) {
	for (Int i=0; i<nPol; i++) {
	  //	  if (!flag(i,j,k,l)) {
	  if (!pflag[offset]) {
	    //	    out(i,j,k)+=weight(k,l)*data(i,j,k,l);
	    pdflags[off1]=False;
	    pout[off1]+=pweight[offw]*pdata[offset];
	    //	    wt(i,j,k)+=weight(k,l);
	    pwt[off1]+=pweight[offw];
	  }
	  off1++; offset++;
	}
      }
      offw++;
    }
  }
  for (Int k=0; k<nIfr*nChan*nPol; k++) {
    if (pwt[k]>0) pout[k]/=pwt[k];
  }
  data.freeStorage(pdata,delData);
  flag.freeStorage(pflag,delFlag);
  weight.freeStorage(pweight,delWeight);
  dataFlag.putStorage(pdflags,delDataflag);
  wt.putStorage(pwt,delWt);
  out.putStorage(pout,delOut);
  data.reference(out);
}
