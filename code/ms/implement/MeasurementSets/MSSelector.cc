//# MSSelector.cc: selection and iteration of an MS
//# Copyright (C) 1997,1998,1999,2000,2001
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
#include <aips/Arrays/Slice.h>
#include <aips/Containers/Record.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/RefRows.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableIter.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableParse.h>
#include <aips/Utilities/GenSort.h>
#include <trial/MeasurementSets/MSIter.h>
#include <trial/MeasurementSets/MSRange.h>
#include <trial/MeasurementSets/MSSelUtil.h>


MSSelector::MSSelector():msIter_p(0),initSel_p(False),dataDescId_p(0),
lastDataDescId_p(1,-1),
useSlicer_p(False),haveSlicer_p(False),wantedOne_p(-1),convert_p(False)
{ }

MSSelector::MSSelector(MeasurementSet& ms):ms_p(ms),
selms_p(ms),savems_p(ms),msIter_p(0),initSel_p(False),dataDescId_p(0),
lastDataDescId_p(1,-1),
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

Bool MSSelector::initSelection(const Vector<Int>& dataDescId, Bool reset)
{
  LogIO os;
  // first check if we want to throw all selections away & return the pristine
  // MeasurementSet
  if (reset) {
    selms_p=ms_p;
    initSel_p=False;
    dataDescId_p.resize(0);
    lastDataDescId_p.resize(0);
    useSlicer_p=False;
    haveSlicer_p=False;
    wantedOne_p=-1;
    convert_p=False;
    return True;
  }

  // check if we can reuse the saved selection
  if (initSel_p && dataDescId.nelements()==lastDataDescId_p.nelements() &&
      allEQ(dataDescId,lastDataDescId_p)) {
    selms_p=savems_p;
    return True;
  } else {
    // undo all previous selections
    selms_p=ms_p;
    ifrSelection_p.resize(0);
    rowIndex_p.resize(0,0);
  }
  // selection on data description is optional
  Bool constantShape=True;
  if (ms_p.dataDescription().nrow()<=1) {
    if (dataDescId.nelements()>1) {
      os << LogIO::NORMAL << "data desc id selection ignored, "
	"there is only one" << LogIO::POST;
    }
  } else {
    if (dataDescId.nelements()>0 && dataDescId(0)!=-1) {
      selms_p=selms_p(selms_p.col(MS::columnName(MS::DATA_DESC_ID)) 
		      .in(dataDescId));
    }
  }
  // check if the data shape is the same for all data
  // if not: select first data desc id only
  ROScalarColumn<Int> dd(selms_p,MS::columnName(MS::DATA_DESC_ID));
  ROMSDataDescColumns ddc(selms_p.dataDescription());
  Vector<Int> ddId=dd.getColumn();
  Int ndd=GenSort<Int>::sort(ddId, Sort::Ascending, 
			     Sort::HeapSort | Sort::NoDuplicates);
  ddId.resize(ndd,True);
  dataDescId_p.resize(ndd); dataDescId_p=ddId;
  ROMSSpWindowColumns spwc(ms_p.spectralWindow());
  ROMSPolarizationColumns polc(ms_p.polarization());
  spwId_p.resize(ndd);
  polId_p.resize(ndd);
  for (Int i=0; i<ndd; i++) {
    spwId_p(i)=ddc.spectralWindowId()(ddId(i));
    polId_p(i)=ddc.polarizationId()(ddId(i));
  }

  for (Int i=1; i<ndd; i++) {
    if (spwc.numChan()(spwId_p(i)) != spwc.numChan()(spwId_p(i-1)) ||
	polc.numCorr()(polId_p(i)) != polc.numCorr()(polId_p(i-1))) {
      constantShape = False;
      break;
    }
  }
  if (!constantShape) {
    selms_p=selms_p(selms_p.col(MS::columnName(MS::DATA_DESC_ID))
		    == ddId(0));
    os<< LogIO::WARN << "Data shape varies, selected first data desc id"
      " only"<< LogIO::POST;
    dataDescId_p.resize(1);dataDescId_p(0)=ddId(0);
    spwId_p.resize(1,True);
    polId_p.resize(1,True);
  }
  lastDataDescId_p.resize(0); lastDataDescId_p=dataDescId_p;

  // if selection is empty, reject it
  initSel_p = (selms_p.nrow()>0);
  if (!initSel_p) {
    os<< LogIO::WARN << "Selected Table has zero rows"<<LogIO::POST;
  } else {
    // save the current selection
    savems_p=selms_p;
    // Set channel selection to entire spectrum
    ROMSSpWindowColumns spwc(selms_p.spectralWindow());
    selectChannel(spwc.numChan()(spwId_p(0)),0,1,1);

    // Set polarization selection/conversion to all present
    ROMSPolarizationColumns polc(selms_p.polarization());
    Vector<Int> pols=polc.corrType()(polId_p(0));
    Vector<String> polSel(pols.nelements());
    for (uInt i=0; i<pols.nelements(); i++) {
      polSel(i)=Stokes::name(Stokes::type(pols(i)));
    }
    selectPolarization(polSel);
    os<< LogIO::NORMAL << "Selection initialized ok"<< LogIO::POST;
  }
  return constantShape;
}

Bool MSSelector::initSelection(Bool reset) {
  Vector<Int> ddIds;
  return initSelection(ddIds,reset);
}

Bool MSSelector::selectChannel(Int nChan, Int start, Int width, Int incr)
{
  LogIO os;
  if (!checkSelection()) return False;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  Bool ok = (nChan>0 && start>=0 && width>0 && incr>0);
  if (!ok) {
    os << LogIO::SEVERE << "Illegal channel selection"<<LogIO::POST;
    return False;
  }
  ROMSColumns msc(selms_p);
  Int numChan=msc.spectralWindow().numChan()(spwId_p(0));
  Int end=start+(nChan-1)*incr+(width-1);
  ok=(ok && end < numChan);
  //#  if (incr < 0) ok=(ok && start+(nChan-1)*incr-(width-1) >= 0);
  if (!ok) {
    os << LogIO::SEVERE << "Illegal channel selection"<<LogIO::POST;
    return False;
  }
  haveSlicer_p=False;
  useSlicer_p=(start>0 || end<(numChan-1) || incr>1 ||wantedOne_p>=0);
  chanSel_p.resize(4);
  chanSel_p(0)=nChan; 
  chanSel_p(1)=start; 
  chanSel_p(2)=width; 
  chanSel_p(3)=incr;
  Int nSpW=spwId_p.nelements();
  Matrix<Double> chanFreq = 
    msc.spectralWindow().chanFreq().getColumnCells(RefRows(spwId_p));
  Matrix<Double> bandwidth = 
    msc.spectralWindow().resolution().getColumnCells(RefRows(spwId_p));
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
  os << LogIO::NORMAL<< "Channel selection: #chan="<<nChan<<
    ", start="<<start+1<<", width="<<width<<", incr="<<incr<<LogIO::POST;
  return True;
}

Bool MSSelector::selectPolarization(const Vector<String>& wantedPol)
{
  LogIO os;
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

  // now find out the input polarizations, assuming all selected data is
  // the same
  ROMSPolarizationColumns mspol(selms_p.polarization());
  Int numCorr=mspol.numCorr()(polId_p(0));
  Vector<Int> inputPol=mspol.corrType()(polId_p(0));

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
  // check if wanted is just a subset or permutation of inputPol
  subSet_p=True;
  for (Int j=0; j<n; j++) {
    Bool found=False;
    for (Int i=0; i<numCorr; i++) {
      if (wanted(j)==inputPol(i)) found=True;
    }
    if (!found) { 
      subSet_p=False; 
      break;
    }
  }

  if (convert_p) {
    // check validity
    for (Int i=0; i<n; i++) {
      if (wanted(i)==Stokes::Undefined) {
	os << LogIO::SEVERE << "Unrecognized polarization: "<<wantedPol(i)	  << LogIO::POST;
	return False;
      }
    }
    stokesConverter_p.setConversion(wanted,inputPol,True);
  }
  if (chanSel_p.nelements()==4) {
    Int end=chanSel_p(1)+(chanSel_p(0)-1)*chanSel_p(3)+chanSel_p(2)-1;
    // use slicer if: start > 0, end< nChan-1, inc>1 && width<inc, one pol only
    useSlicer_p=
      (chanSel_p(1)>0 || 
	     end<(chanSel_p(0)-1) || 
	     (chanSel_p(3)>1 && chanSel_p(2)<chanSel_p(3)) 
	     || wantedOne_p>=0);
  }
  if (useSlicer_p) haveSlicer_p=False;
  polSelection_p.resize(wantedPol.nelements()); polSelection_p=wantedPol;
  os << LogIO::NORMAL<< "Polarization selection: "<< wantedPol 
    << LogIO::POST;
  return True;
}

Bool MSSelector::select(const GlishRecord& items, Bool oneBased)
{
  LogIO os;
  if (!checkSelection()) return False;
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
		(( uvdist >= range(0) || near(uvdist,range(0)) ) && 
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

Bool MSSelector::select(const String& msSelect)
{
  LogIO os;
  if (!checkSelection()) return False;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return False;
  }
  // check that a selection was given
  Int len = msSelect.length();
  Int nspace = msSelect.freq(' ');
  if (msSelect.empty() || nspace==len) return False;
  String parseString="select from $1 where " + msSelect;
  selms_p=tableCommand(parseString,selms_p);
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

static void averageId(Matrix<Int>& id)
{
  // "average" a Matrix of integer id's - replace by length 1 vector with
  // common value if all elements are the same, or -1 otherwise.
  // Used below to "average" things like antenna id.
  if (id.nelements()>1) {
    Matrix<Int> aver(id.nrow(),1); aver.column(0)=id.column(0);
    for (uInt i=0; i<id.nrow(); i++) {
      if (!allEQ(id.row(i),aver(i,0))) aver(i,0)=-1;
    }
    id.reference(aver);
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

static void averageDouble(Matrix<Double>& mat) 
{
  // average a matrix of doubles in the 2nd dimension
  Int n=mat.ncolumn();
  if (n>1) {
    Matrix<Double> aver(mat.nrow(),1); 
    aver.column(0)=mat.column(0);
    for (Int i=1; i<n; i++) aver+=mat.column(i);
    aver/=Double(n);
    mat.reference(aver);
  }
}

GlishRecord MSSelector::getData(const Vector<String>& items, Bool ifrAxis,
				Int ifrAxisGap,
				Int inc, Bool average, Bool oneBased)
{
  LogIO os;
  GlishRecord out;
  if (!checkSelection()) return out;
  if (selms_p.nrow()==0) {
    os << LogIO::WARN << " Selected Table is empty - use selectinit"
       << LogIO::POST;
    return out;
  }

  Matrix<Bool> want(nFuncType,nDataType,False);
  Bool wantFlag, wantFlagSum, wantWeight, wantSigma;
  wantFlag=wantFlagSum=wantWeight=wantSigma=False;

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
  Int nSlot = 0;
  Int nRow = tab.nrow();
  if (ifrAxis && nIfr==0) {
    // set default
    //    ifrSelection_p=ifrNumbers(msc.antenna1(),msc.antenna2());
    MSRange msRange(tab);
    GlishArray(msRange.range(MSS::IFR_NUMBER).get(0)).get(ifrSelection_p);
    nIfr = ifrSelection_p.nelements();
  }

  Vector<Int> ifrIndex; // the index no into the ifrSelection Vector
  Vector<Int> slot;     // for each ifr, the row ordered occurrence of it
  Bool doIfrAxis = (ifrAxis && nIfr>0);
  if (doIfrAxis) {
    if (ifrAxisGap>=0) {
      // add a small gap before each antenna1 change
      Int gapCount=0;
      for (Int i=1; i<nIfr; i++) {
	if (ifrSelection_p(i)/1000>ifrSelection_p(i-1)/1000) gapCount++;
      }
      ifrAxis_p.resize(nIfr+gapCount*ifrAxisGap);
      ifrAxis_p(0)=ifrSelection_p(0);
      for (Int i=1,j=1; i<nIfr; i++) {
	if (ifrSelection_p(i)/1000>ifrSelection_p(i-1)/1000) {
	  for (Int k=0; k<ifrAxisGap; k++) ifrAxis_p(j++)=-1;
	  ifrAxis_p(j++)=ifrSelection_p(i);
	} else {
	  ifrAxis_p(j++)=ifrSelection_p(i);
	}
      }
      nIfr+=gapCount*ifrAxisGap;
    }
    // figure out which rows go with which slot
    Vector<Int> ifr=msc.antenna1().getColumn();
    ifr*=1000;
    ifr+=msc.antenna2().getColumn();
    nRow=ifr.nelements();
    ifrIndex.resize(nRow);
    slot.resize(nRow);
    Vector<Int> slotNo(nIfr,0);
    for (Int i=0; i<nRow; i++) {
      for (Int j=0; j<nIfr; j++) {
	if (ifr(i)==ifrAxis_p(j)) {
	  ifrIndex(i)=j;
	  slot(i)=slotNo(j)++;
	  break;
	}
      }
    }
    nSlot = max(slotNo);
    if (aips_debug) {
      for (Int i=0; i<nIfr; i++) {
	if (ifrAxis_p(i)>=0 && slotNo(i)!=nSlot) {
	  os<<LogIO::DEBUGGING<<"Not all ifrs are present for all slots: "<<
	    " time and index coordinates will not be aligned across ifrs"<<
	    LogIO::POST;
	  break;
	}
      }
    }
    rowIndex_p.resize(nIfr,nSlot); rowIndex_p.set(-1);
    for (Int i=0; i<nRow; i++) rowIndex_p(ifrIndex(i),slot(i))=i;

    // align the slots in time
    // Note that this doesn't cope with all cases, e.g., if the number
    // of slots is < number of times. Ok if at least one antenna is there
    // for the whole block
    Vector<Double> time=msc.time().getColumn();
    Vector<Int> dd=msc.dataDescId().getColumn();
    Matrix<Double> times(nIfr,nSlot,0);
    Matrix<Int> dds(nIfr,nSlot,-1);
    for (Int k=0; k<nRow; k++) {
      times(ifrIndex(k),slot(k))=time(k);
      dds(ifrIndex(k),slot(k))=dd(k);
    }

    for (Int sl=nSlot-1; sl>=0; sl--) {
      // find latest time in this slot
      IPosition minPos(1),maxPos(1);
      Double minVal,maxVal;
      minMax(minVal,maxVal,minPos,maxPos,times.column(sl));
      //      Double ltime=max(times.column(sl));
      Double ltime=maxVal;
      Int ddAtMax=dds(maxPos(0),sl);
      for (Int i=0; i<nIfr; i++) {
	if (ifrAxis_p(i)>=0) { // ifr exists
	  if (times(i,sl)==0) { // found a hole
	    Int sl2=sl;
	    while (sl2>0 && times(i,sl2)==0) sl2--;
	    if (sl2>=0 && times(i,sl2)==ltime &&
		dds(i,sl2)==ddAtMax) {
	      // move from sl2 to sl to align in time
	      Int row=rowIndex_p(i,sl2);
	      rowIndex_p(i,sl)=row;
	      rowIndex_p(i,sl2)=-1;
	      slot(row)=sl;
	      times(i,sl)=ltime;
	      times(i,sl2)=0;
	    }
	  }
	}
      }
    }
  } else {
    rowIndex_p.resize(0,0);
  }

  // now get out the data
  for (Int it=0; it<nItems; it++) {
    String item=downcase(items(it));
    MSS::Field fld=MSS::field(item);
    switch (fld) {
    case MSS::AMPLITUDE:
    case MSS::CORRECTED_AMPLITUDE:
    case MSS::MODEL_AMPLITUDE:
    case MSS::RATIO_AMPLITUDE:
    case MSS::RESIDUAL_AMPLITUDE:
    case MSS::OBS_RESIDUAL_AMPLITUDE:
      want(Amp,fld-MSS::AMPLITUDE)=True;
      break;
    case MSS::ANTENNA1:
      if (doIfrAxis) {
	Vector<Int> ant1(nIfr);
	ant1=ifrAxis_p;
	ant1/=1000;
	for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ant1(i)=-1;
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
	ant2=ifrAxis_p;
	ant2-=1000*(ifrAxis_p/1000);
	for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ant2(i)=-1;
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
	    Vector<Int> ifr; 
	    ifr=ifrAxis_p;
	    ifr+=1001;
	    for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ifr(i)=0;
	    ifr_axis.add("ifr_number",ifr);
	  } else {
	    ifr_axis.add("ifr_number",ifrSelection_p);
	  }
	  Vector<String> antName=msc.antenna().name().getColumn();
	  Vector<String> ifrName(nIfr,"");
	  Vector<String> sName(nIfr,"");
	  // get common prefix in antenna names by comparing first and 
	  // last antenna in table
	  String prefix = 
	    common_prefix(antName(0),antName(antName.nelements()-1));
	  Matrix<Double> antPos=msc.antenna().position().getColumn();
	  Vector<Double> baseline(nIfr,-1.0);
	  // PROBLEM: antName elements have string size extending beyond \0
	  // string catenation doesn't work correctly!
	  for (Int k=0; k<nIfr; k++) {
	    if (ifrAxis_p(k)>=0) {
	      Int ant1 = ifrAxis_p(k)/1000;
	      Int ant2 = ifrAxis_p(k)%1000;
	      ifrName(k)=antName(ant1).before('\0');
	      ifrName(k)+="-";
	      ifrName(k)+=antName(ant2).before('\0');
	      if (prefix.length()>1) {
		sName(k)=String(antName(ant1).after(prefix)).before('\0');
		sName(k)+="-";
		sName(k)+=String(antName(ant2).after(prefix)).before('\0');
	      } else {
		sName(k)=ifrName(k);
	      }
	      baseline(k)=sqrt(square(antPos(0,ant2)-antPos(0,ant1))+
			       square(antPos(1,ant2)-antPos(1,ant1))+
			       square(antPos(2,ant2)-antPos(2,ant1)));
	    }
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
	  Double startOfDay=( nSlot>0 ? 
			      C::day*int(time(0)/C::day) : 0);
	  Int nT = (average ? 1 : nSlot);
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
	    for (Int k=0; k<nSlot; k++) {
	      times(0)+=time(rowIndex_p(0,k));
	    }
	    if (nSlot>0) times(0)/=nSlot;
	  } else {
	    for (Int k=0; k<nSlot; k++) {
	      times(k)=time(rowIndex_p(0,k));
	    }
	  }
	  for (Int k=0; k<nT; k++) {
	    if (doUT) {
	      ut(k)=times(k)-startOfDay;
	    }
	    if (doLAST || doHA) {
	      Int curFieldId=fieldId(rowIndex_p(0,k));
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
    case MSS::DATA:
    case MSS::CORRECTED_DATA:
    case MSS::MODEL_DATA:
    case MSS::RATIO_DATA:
    case MSS::RESIDUAL_DATA:
    case MSS::OBS_RESIDUAL_DATA:
      want(Data,fld-MSS::DATA)=True;
      break;
    case MSS::DATA_DESC_ID:
    case MSS::FIELD_ID:
    case MSS::SCAN_NUMBER:
      {
	Vector<Int> col = (fld == MSS::DATA_DESC_ID ? 
			   msc.dataDescId().getColumn() :
			   (fld == MSS::FIELD_ID ?
			    msc.fieldId().getColumn() :
			    msc.scanNumber().getColumn()));
	if (doIfrAxis) {
	  Matrix<Int> id(nIfr,nSlot); id.set(-1);
	  for (Int k=0; k<nSlot; k++) {
	    for (Int i=0; i<nIfr; i++) {
	      Int j = rowIndex_p(i,k);
	      id(i,k)=(j<0 ? -1 : col(j));
	    }
	  }
	  if (average) averageId(id);
	  if (oneBased) id+=1;
	  out.add(item,id);
	} else {
	  if (average) averageId(col);
	  if (oneBased) col+=1;
	  out.add(item,col);
	}
      }
      break;
    case MSS::FEED1:
    case MSS::FEED2:
      {
	Vector<Int> feed = (fld == MSS::FEED1 ? 
			    msc.feed1().getColumn() :
			    msc.feed2().getColumn());
	if (doIfrAxis) {
	  Matrix<Int> id(nIfr,nSlot); id.set(-1);
	  for (Int k=0; k<nSlot; k++) {
	    for (Int i=0; i<nIfr; i++) {
	      Int j=rowIndex_p(i,k);
	      id(i,k)=(j<0 ? -1 : feed(j));
	    }
	  }
	  if (average) averageId(id);
	  if (oneBased) id+=1;
	  out.add(item,id);
	} else {
	  if (average) averageId(feed);
	  if (oneBased) feed+=1;
	  out.add(item,feed);
	}
      }
      break;
    case MSS::FLAG:
      wantFlag=True;
      break;
    case MSS::FLAG_ROW:
      if (doIfrAxis) {
	Matrix<Bool> itFlag(nIfr,nSlot);
	itFlag.set(True); // flag unfilled slots
	Vector<Bool> flagRow=msc.flagRow().getColumn();
	for (Int k=0; k<nRow; k++) {
	  itFlag(ifrIndex(k),slot(k))=flagRow(k);
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
	    Vector<Int> ifr;
	    ifr=ifrAxis_p;
	    ifr+=1001;
	    for (Int i=0; i<nIfr; i++) if (ifrAxis_p(i)<0) ifr(i)=0;
	    out.add(item,ifr);
	  } else {
	    out.add(item,ifrAxis_p);
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
    case MSS::CORRECTED_IMAGINARY:
    case MSS::MODEL_IMAGINARY:
    case MSS::RATIO_IMAGINARY:
    case MSS::RESIDUAL_IMAGINARY:
    case MSS::OBS_RESIDUAL_IMAGINARY:
      want(Imag,fld-MSS::IMAGINARY)=True;
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
	      start2(1)=end2(1)=ifrIndex(k);
	      start2(2)=end2(2)=slot(k);
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
    case MSS::FLOAT_DATA:
      want(Data,ObsFloat)=True;
      break;
    case MSS::PHASE:
    case MSS::CORRECTED_PHASE:
    case MSS::MODEL_PHASE:
    case MSS::RATIO_PHASE:
    case MSS::RESIDUAL_PHASE:
    case MSS::OBS_RESIDUAL_PHASE:
      want(Phase,fld-MSS::PHASE)=True;
      break;
    case MSS::REAL:
    case MSS::CORRECTED_REAL:
    case MSS::MODEL_REAL:
    case MSS::RATIO_REAL:
    case MSS::RESIDUAL_REAL:
    case MSS::OBS_RESIDUAL_REAL:
      want(Real,fld-MSS::REAL)=True;
      break;
    case MSS::SIGMA: 
      {
	Matrix<Float> sig = getWeight(msc.sigma(),True);
	Int nCorr=sig.shape()(0);
	if (doIfrAxis) {
	  IPosition wtsidx(3,nCorr,nIfr,nSlot);
	  Cube<Float> wts(wtsidx); wts.set(0);
	  for (Int i=0; i<nRow; i++) {
	    for (Int j=0; j<nCorr; j++) {
	      wts(j, ifrIndex(i),slot(i))=sig(j,i);
	    }
	  }
	  if (average) {
	    // averaging sigma's doesn't make sense,
	    // return sqrt of sum of squares instead
	    IPosition sumwtidx(2,nCorr,nIfr);
	    Matrix<Float> sumsig(sumwtidx); sumsig=0;
	    for (Int i=0; i<nIfr; i++) {
	      for (int j=0; j<nCorr; j++) {
		for (Int k=0; k<nSlot; k++) {sumsig(j,i)+=square(wts(j,i,k));}
		sumsig(j,i)=sqrt(sumsig(j,i));
	      }
	    }
	    out.add("sigma",sumsig);
	  } else {
	    out.add("sigma",wts);
	  }
	} else {
	  if (average) {
	    // return sqrt of sum of squares 
	    Vector<Float> sumsig(nCorr); sumsig=0.0;
	    for (Int j=0; j<nCorr; j++) {
	      for (Int i=0; i<nRow; i++) {sumsig(j)+=square(sig(j,i));}
	      sumsig(j)=sqrt(sumsig(j));
	    }
	    out.add("sigma",sumsig);
	  } else {
	    out.add("sigma",sig);
	  }
	}
      }
      break;
    case MSS::TIME:
      {
	Vector<Double> time=msc.time().getColumn();
	if (doIfrAxis) {
	  Matrix<Double> times(nIfr,nSlot);
	  for (Int k=0; k<nRow; k++) {
	    times(ifrIndex(k),slot(k))=time(k);
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
	Cube<Double> uvw2(uvw.shape()(0),nIfr,nSlot); uvw2.set(0);
	for (Int k=0; k<nRow; k++) {
	  Int ifr=ifrIndex(k);
	  Int time=slot(k);
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
	  Matrix<Double> uvw2(nIfr,nSlot); uvw2.set(0);
	  for (Int k=0; k<nRow; k++) {
	    uvw2(ifrIndex(k),slot(k))=uvw(index,k);
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
	  Matrix<Double> uvd(nIfr,nSlot); uvd.set(0);
	  for (Int k=0; k<nRow; k++) uvd(ifrIndex(k),slot(k))=pu2[k];
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
    Matrix<Float> wt = getWeight(msc.weight());
    Int nCorr=wt.shape()(0);
    if (doIfrAxis) {
      IPosition wtsidx(3,nCorr,nIfr,nSlot);
      Cube<Float> wts(wtsidx); wts.set(0);
      for (Int i=0; i<nRow; i++) {
        for (Int j=0; j<nCorr; j++) {
          wts(j, ifrIndex(i),slot(i))=wt(j,i);
        }
      }
      if (wantWeight) {
        if (average) {
          // averaging weights doesn't make sense,
          // return sum of weights instead
          IPosition sumwtidx(2,nCorr,nIfr);
          Matrix<Float> sumwt(sumwtidx); sumwt=0;
          for (Int i=0; i<nIfr; i++) {
            for (int j=0; j<nCorr; j++) {
              for (Int k=0; k<nSlot; k++) {
                sumwt(j,i)+=wts(j,i,k);
              }
            }
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
          Vector<Float> sumwt(nCorr); sumwt=0.0;
          for (Int i=0; i<nRow; i++) {
            for (Int j=0; j<nCorr; j++) {
              sumwt(j)+=wt(j,i);
            }
          }
          out.add("weight",sumwt);
        } else {
          out.add("weight",wt);
        }
      }
      weights.reference(wt);
    }                                                  
  }


  Array<Bool> flag;
  if (wantFlag || wantFlagSum ||average) {
    Array<Bool> avFlag;
    flag=getAveragedFlag(avFlag,msc.flag());
    uInt nPol=avFlag.shape()(0), nChan=avFlag.shape()(1), nRow=avFlag.shape()(2);
    if (doIfrAxis) {
      MSSelUtil2<Bool>::reorderData(avFlag,ifrIndex,nIfr,slot,nSlot,True);
    }
    if (average) flags=avFlag;
    if (wantFlag && !average) {
      out.add("flag",avFlag);
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
	      for (indx(3)=0; indx(3)<nSlot; indx(3)++) {
		if (avFlag(indx)) count++;
	      }
	      flagSum(j,k,l)=count;
	    }
	  }
	}
	out.add("flag_sum",flagSum);
      } else {
	Matrix<Int> flagSum(nPol,nChan);
	flagSum=0;
	Cube<Bool> flag2(avFlag);
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

  if (want(Data,ObsFloat)) {
    // get the data
    Array<Float> fdata;
    if (!msc.floatData().isNull()) {
      getAveragedData(fdata,flag,msc.floatData());
      if (doIfrAxis) MSSelUtil2<Float>::
	reorderData(fdata,ifrIndex,nIfr,slot,nSlot,Float());
      if (average) MSSelUtil2<Float>::
	timeAverage(dataflags,fdata,flags,weights);
      out.add("float_data",fdata);
    } else {
      os << LogIO::WARN << "FLOAT_DATA column doesn't exist"<< LogIO::POST;
    }
  }


  Array<Complex> observed_data,corrected_data,model_data;
  Bool keepObs = anyEQ(want.column(ObsResidual),True);
  Bool keepMod = anyEQ(want.column(Residual),True)||
    anyEQ(want.column(Ratio),True)|| keepObs;;
  Bool keepCor = anyEQ(want.column(Residual),True)||
    anyEQ(want.column(Ratio),True);

  for (Int dataType=Observed; dataType<=ObsResidual; dataType++) {
    if (anyEQ(want.column(dataType),True)||
	(dataType==Observed && keepObs) ||
	(dataType==Model && keepMod) ||
	(dataType==Corrected && keepCor)) {
      if (convert_p && !subSet_p && dataType==Observed) {
	os << LogIO::WARN << "Polarization conversion of uncalibrated"
	   << " data may give incorrect results"<< LogIO::POST;
      }
      // get the data if this is a data column
      ROArrayColumn<Complex> colData;
      Array<Complex> data;
      if (dataType<=Model) {
	colData.reference( dataType == Observed ? msc.data() :
			   (dataType == Corrected ? msc.correctedData() :
			    msc.modelData()));
	if (colData.isNull()) {
	    os << LogIO::WARN <<"Requested column doesn't exist"<<LogIO::POST;
	} else {
	  getAveragedData(data,flag,colData);
	  if (doIfrAxis) MSSelUtil2<Complex>::
	    reorderData(data,ifrIndex,nIfr,slot,nSlot,Complex());
	}
      }
      String name;
      switch (dataType) {
      case Observed: 
	if (keepObs) observed_data.reference(data);
	name="";
	break;
      case Model:
	if (keepMod) model_data.reference(data);
	name="model_";
	break;
      case Corrected:
	if (keepCor) corrected_data.reference(data);
	name="corrected_";
	break;  
      case Ratio:
	{
	  LogicalArray mask(model_data!=Complex(0.));
	  data = corrected_data;
	  data /= model_data(mask);
	  data(!mask)=1.0;
	  name="ratio_";
	}
	break;
      case Residual:
	data = corrected_data;
	data -= model_data;
	name="residual_";
	break;
      case ObsResidual:
	data = observed_data;
	data -= model_data;
	name="obs_residual_";
	break;
      default:;
      }
      if (average) 
	MSSelUtil2<Complex>::timeAverage(dataflags,data,flags,weights);
      if (want(Amp,dataType)) out.add(name+"amplitude",amplitude(data));
      if (want(Phase,dataType)) out.add(name+"phase",phase(data));
      if (want(Real,dataType)) out.add(name+"real",real(data));
      if (want(Imag,dataType)) out.add(name+"imaginary",imag(data));
      if (want(Data,dataType)) out.add(name+"data",data);
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
  LogIO os;
  if (!checkSelection()) return False;
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
    case MSS::CORRECTED_DATA:
    case MSS::MODEL_DATA:
      {
	ArrayColumn<Complex>& col = (fld==MSS::DATA ? msc.data() : 
				     (fld==MSS::CORRECTED_DATA ?
				      msc.correctedData() :
				      msc.modelData()));
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
	if (GlishArray(items.get(i)).get(data) && ! col.isNull()) {
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
	    if (useSlicer_p) col.putColumn(slicer_p, data);
	    else col.putColumn(data);
	  }
	}
      }
      break;
    case MSS::FLOAT_DATA:
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
	Array<Float> data;
	if (GlishArray(items.get(i)).get(data)) {
	  if (data.ndim()==4) {
	    if (data.shape()(2)==Int(rowIndex_p.nrow()) && 
		data.shape()(3)==Int(rowIndex_p.ncolumn())) {
	      MSSelUtil2<Float>::reorderData(data, rowIndex_p, 
					     selms_p.nrow());
	    } else {
	      os << LogIO::SEVERE<<"Data shape inconsistent with "
		"current selection"<< LogIO::POST;
	      break;
	    }
	  }
	  if (data.ndim()==3) {
	    if (useSlicer_p) msc.floatData().putColumn(slicer_p, data);
	    else msc.floatData().putColumn(data);
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
    case MSS::SIGMA:
    case MSS::WEIGHT:
      {
	Array<Float> weight;
	if (GlishArray(items.get(i)).get(weight)) {
	  if (weight.ndim()==3) {
	    reorderWeight(weight);
	  }
	  if (weight.ndim()==2) {
	    if (fld == MSS::SIGMA) msc.sigma().putColumn(weight);
	    if (fld == MSS::WEIGHT) msc.weight().putColumn(weight);
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
			  Double interval, Int maxRows,
			  Bool addDefaultSortColumns)
{
  LogIO os;
  if (!checkSelection()) return False;
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
  msIter_p=new MSIter(selms_p,col,interval,addDefaultSortColumns);
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
void MSSelector::getAveragedData(Array<Complex>& avData, const Array<Bool>& flag,
				 const ROArrayColumn<Complex>& col) const
{
  getAveragedData(avData,flag,col,Slicer(Slice()));
}


void MSSelector::getAveragedData(Array<Complex>& avData, const Array<Bool>& flag,
				 const ROArrayColumn<Complex>& col,
				 const Slicer & rowSlicer) const
{
  Array<Complex> data;
  if (useSlicer_p) {
    if (!haveSlicer_p) {
      if (wantedOne_p>=0) makeSlicer(wantedOne_p,1);
      else makeSlicer(0,col.shape(0)(0));
    }
    data=col.getColumnRange(rowSlicer,slicer_p);
  } else {
    data=col.getColumnRange(rowSlicer);
  }
  Int nPol=data.shape()(0);
  Vector<Int> chanSel(chanSel_p);
  if (chanSel.nelements()==0) {
    // not yet initialized, set to default
    chanSel.resize(4);
    chanSel(0)=data.shape()(1); chanSel(1)=0; chanSel(2)=1; chanSel(3)=1;
  }
  Int nChan=chanSel(0);
  Int nRow=data.shape()(2);
  avData.resize(IPosition(3,nPol,nChan,nRow));
  if (chanSel(2)==1) {
    // no averaging, just copy the data across
    avData=data;
  } else {
    // Average channel by channel
    for (Int i=0; i<nChan; i++) {
      // if width>1, the slice doesn't have an increment, so we take big steps
      Int chn=i*chanSel(3);
      Array<Complex> ref(avData(IPosition(3,0,i,0),IPosition(3,nPol-1,i,nRow-1)));
      ref=data(IPosition(3,0,chn,0),IPosition(3,nPol-1,chn,nRow-1));
      // average over channels
      // TODO: take flagging into account
      for (Int j=1; j<chanSel(2); j++) {
	ref+=data(IPosition(3,0,chn+j,0),IPosition(3,nPol-1,chn+j,nRow-1));
      }
      // This is horrible..
      ref/=Complex(chanSel(2));
    }
  }
  // do the polarization conversion
  if (convert_p) {
    Array<Complex> out;
    stokesConverter_p.convert(out,avData);
    avData.reference(out);
  }
}

void MSSelector::getAveragedData(Array<Float>& avData, const Array<Bool>& flag,
				 const ROArrayColumn<Float>& col) const
{
  getAveragedData(avData,flag,col,Slicer(Slice()));
}

void MSSelector::getAveragedData(Array<Float>& avData, const Array<Bool>& flag,
				 const ROArrayColumn<Float>& col,
				 const Slicer& rowSlicer) const
{
  Array<Float> data;
  if (useSlicer_p) {
    if (!haveSlicer_p) {
      if (wantedOne_p>=0) makeSlicer(wantedOne_p,1);
      else makeSlicer(0,col.shape(0)(0));
    }
    data=col.getColumnRange(rowSlicer,slicer_p);
  } else {
    data=col.getColumnRange(rowSlicer);
  }
  Int nPol=data.shape()(0);
  Vector<Int> chanSel(chanSel_p);
  if (chanSel.nelements()==0) {
    // not yet initialized, set to default
    chanSel.resize(4);
    chanSel(0)=data.shape()(1); chanSel(1)=0; chanSel(2)=1; chanSel(3)=1;
  }
  Int nChan=chanSel(0);
  Int nRow=data.shape()(2);
  avData.resize(IPosition(3,nPol,nChan,nRow));
  if (chanSel(2)==1) {
    // no averaging, just copy the data across
    avData=data;
  } else {
    // Average channel by channel
    Array<Bool> mask(!flag);
    Array<Float> wt(flag.shape(),0.0); wt(mask)=1.0;
    Array<Float> avWt(avData.shape(),0.0);
    for (Int i=0; i<nChan; i++) {
      // if width>1, the slice doesn't have an increment, so we take big steps
      Int chn=i*chanSel(3);
      IPosition is(3,0,i,0),ie(3,nPol-1,i,nRow-1),
	cs(3,0,chn,0),ce(3,nPol-1,chn,nRow-1);
      Array<Float> ref(avData(is,ie));
      Array<Float> wtref(avWt(is,ie));
      // average over channels
      for (Int j=0; j<chanSel(2); j++,cs(1)++,ce(1)++) {
	MaskedArray<Float> mdata(data(cs,ce),mask(cs,ce));
	ref+=mdata;
	wtref+=wt(cs,ce);
      }
      ref(wtref>Float(0.0))/=wtref(wtref>Float(0.0));
    }
  }
  // do the polarization conversion
  if (convert_p) {
    //    Array<Float> out;
    //  stokesConverter_p.convert(out,avData);
    //    avData.reference(out);
    LogIO os;
    os << LogIO::WARN << "Polarization conversion for FLOAT_DATA "
      "not implemented" << LogIO::POST;
  }
}

Array<Bool> MSSelector::getAveragedFlag(Array<Bool>& avFlag, 
					const ROArrayColumn<Bool>& col) const
{
  return getAveragedFlag(avFlag,col,Slicer(Slice()));
}

Array<Bool> MSSelector::getAveragedFlag(Array<Bool>& avFlag, 
					const ROArrayColumn<Bool>& col,
					const Slicer& rowSlicer) const
{
  Array<Bool> flag;
  if (useSlicer_p) {
    if (!haveSlicer_p) {
      if (wantedOne_p>=0) makeSlicer(wantedOne_p,1);
      else makeSlicer(0,col.shape(0)(0));
    }
    flag=col.getColumnRange(rowSlicer,slicer_p);
  } else {
    flag=col.getColumnRange(rowSlicer);
  }
  Int nPol=flag.shape()(0);
  Vector<Int> chanSel(chanSel_p);
  if (chanSel.nelements()==0) {
    // not yet initialized, set to default
    chanSel.resize(4);
    chanSel(0)=flag.shape()(1); chanSel(1)=0; chanSel(2)=1; chanSel(3)=1;
  }
  Int nChan=chanSel(0);
  Int nRow=flag.shape()(2);
  avFlag.resize(IPosition(3,nPol,nChan,nRow));
  if (chanSel(2)==1) {
    // no averaging, just copy flags
    avFlag=flag;
  } else {
    avFlag=True;
    for (Int i=0; i<nChan; i++) {
      Int chn=i*chanSel(3);
      IPosition is(3,0,i,0),ie(3,nPol-1,i,nRow-1),
	cs(3,0,chn,0),ce(3,nPol-1,chn,nRow-1);
      Array<Bool> ref(avFlag(is,ie));
      // average over channels
      for (Int j=0; j<chanSel(2); j++,cs(1)++,ce(1)++) {
	ref*=flag(cs,ce);
      }
    }
  }
  if (convert_p) {
    Array<Bool> out;
    stokesConverter_p.convert(out,avFlag);
    avFlag.reference(out);
  }
  return flag; // return the raw flags for use in data averaging
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

Array<Float> MSSelector::getWeight(const ROArrayColumn<Float>& wtCol,
				   Bool sigma) const
{
  Array<Float> wt;
  if (wantedOne_p>=0) { 
    wt = wtCol.getColumn(Slicer(Slice(wantedOne_p,1)));
  } else {
    wt = wtCol.getColumn();
  }
  // apply the stokes conversion/selection to the weights
  if (convert_p) {
    Matrix<Float> outwt;
    stokesConverter_p.convert(outwt,wt,sigma);
    wt.reference(outwt);
  }
  return wt;
}

void MSSelector::makeSlicer(Int start, Int nCorr) const
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
  Int nIfr=flagRow.shape()(0), nSlot=flagRow.shape()(1);
  Int nRow=selms_p.nrow();
  Bool deleteFlag, deleteRow;
  const Bool* pFlag=flagRow.getStorage(deleteFlag);
  const Int* pRow=rowIndex_p.getStorage(deleteRow);
  Vector<Bool> rowFlag(nRow);
  Int offset=0;
  for (Int i=0; i<nSlot; i++, offset+=nIfr) {
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

// reorder from 3d to 2d (removing ifr axis)
void MSSelector::reorderWeight(Array<Float>& weight)
{
  Int nCorr=weight.shape()(0), nIfr=weight.shape()(1), nSlot=weight.shape()(2);
  Int nRow=selms_p.nrow();
  Bool deleteWeight, deleteRow, deleteRowWeight;
  const Float* pWeight=weight.getStorage(deleteWeight);
  const Int* pRow=rowIndex_p.getStorage(deleteRow);
  Matrix<Float> rowWeight(nCorr, nRow);
  Float* pRowWeight=rowWeight.getStorage(deleteRowWeight);
  Int offset=0;
  for (Int i=0; i<nSlot; i++) {
    for (Int j=0; j<nIfr; j++, offset++) {
      Int k=pRow[offset];
      if (k>0) {
	Int wOffset = nCorr*offset;
	Int rwOffset = nCorr*k;
	for (Int c=0; c<nCorr; c++) {
	  pRowWeight[rwOffset++] = pWeight[wOffset++];
	}
      }
    }
  }
  weight.freeStorage(pWeight,deleteWeight);
  rowIndex_p.freeStorage(pRow,deleteRow);
  rowWeight.putStorage(pRowWeight,deleteRowWeight);
  weight.reference(rowWeight);
}

Bool MSSelector::checkSelection() {
  if (!initSel_p) {
    LogIO os;
    os << LogIO::NORMAL <<"Initializing with default selection"
       << LogIO::POST;
    initSelection();
  }
  return initSel_p;
}

