///# MSRange.cc: selection and iteration of an MS
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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

#include <casacore/ms/MeasurementSets/MSRange.h>
#include <casacore/ms/MSSel/MSSelector.h>

#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSRange::MSRange():blockSize_p(10),ddId_p(0),constantShape_p(false),sel_p(0)
{}

MSRange::MSRange(const MeasurementSet& ms)
:ms_p(ms),blockSize_p(10),constantShape_p(false),sel_p(0)
{}

MSRange::MSRange(const MSSelector& msSel)
  :ms_p(msSel.selectedTable()),blockSize_p(10),constantShape_p(false),
   sel_p(&msSel)
{ddId_p=msSel.dataDescId();}

MSRange::MSRange(const MSRange& other)
{ operator=(other); }


MSRange& MSRange::operator=(const MSRange& other)
{
  if (this==&other) return *this;
  ms_p=other.ms_p;
  blockSize_p=other.blockSize_p;
  ddId_p.resize(0); ddId_p=other.ddId_p;
  spwId_p.resize(0); spwId_p=other.spwId_p;
  polId_p.resize(0); polId_p=other.polId_p;
  constantShape_p=other.constantShape_p;
  sel_p=other.sel_p;
  return *this;
}

bool MSRange::checkShapes()
{
  int32_t n=ddId_p.nelements();
  // check already done
  if (n>0 && spwId_p.nelements()>0) return constantShape_p; 
  constantShape_p=true;
  if (n==0) {
    ScalarColumn<int32_t> dd(ms_p,MS::columnName(MS::DATA_DESC_ID));
    Vector<int32_t> ddId=scalarRange(dd);
    ddId_p=ddId;
  }
  int32_t n2=ddId_p.nelements();
  MSDataDescColumns ddc(ms_p.dataDescription());
  spwId_p.resize(n2);
  polId_p.resize(n2);
  for (int32_t i=0; i<n2; i++) {
    spwId_p(i)=ddc.spectralWindowId()(ddId_p(i));
    polId_p(i)=ddc.polarizationId()(ddId_p(i));
  }
  if (n>0) return constantShape_p; // no need to check, done by MSSelector

  // check if the shape is the same for all spectral windows that occur
  // in the main table
  MSSpWindowColumns spwc(ms_p.spectralWindow());
  MSPolarizationColumns polc(ms_p.polarization());
  for (int32_t i=1; i<n2; i++) {
    if (spwc.numChan()(spwId_p(i)) != spwc.numChan()(spwId_p(i-1)) ||
	polc.numCorr()(polId_p(i)) != polc.numCorr()(polId_p(i-1))) {
      constantShape_p = false;
      break;
    }
  }
  return constantShape_p;
}

Record MSRange::range(const Vector<String>& items, 
			   bool useFlags,
			   bool oneBased)
{
  LogIO os;
  int32_t n=items.nelements();
  Vector<int32_t> keys(n);
  // translate strings to enums
  int32_t k=0;
  String keyword;
  for (int32_t i=0; i<n; i++) {
    keyword=downcase(items(i));
    keys(k)=MSS::field(keyword);
    if (keys(k)!=MSS::UNDEFINED) {
      k++;
    } else {
      os<< LogIO::WARN << "Unrecognized field in input ignored: "<<
	keyword<< LogIO::POST;
    }
  }
  keys.resize(k,true); // squeeze out the UNDEFINEDs
  return range(keys,useFlags,oneBased);
}

Record MSRange::range(const Vector<int32_t>& keys, 
			   bool useFlags,
			   bool oneBased)
{
  LogIO os;
  const int32_t option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;

  Record out(RecordInterface::Variable);
  if (ms_p.nrow()==0) {
    os<< LogIO::WARN << "Table is empty - nothing to do"<<LogIO::POST;
    return out;
  }
  MSColumns msc(ms_p);
  Matrix<bool> want(nFuncType,nDataType,false);
  // use HeapSort as it's performance is guaranteed, quicksort is often
  // extremely slow (O(n*n)) for inputs with many successive duplicates
  Matrix<double> uvw;
  bool shapeChangesWarning=false;
  int32_t n=keys.nelements();
  String keyword;
  for (int32_t i=0; i<n; i++) {
    // get the enum and the keyword value
    MSS::Field fld=MSS::Field(keys(i));
    keyword=MSS::keyword(fld);
    switch (fld) {
    case MSS::AMPLITUDE:
    case MSS::CORRECTED_AMPLITUDE:
    case MSS::MODEL_AMPLITUDE:
    case MSS::RATIO_AMPLITUDE:
    case MSS::RESIDUAL_AMPLITUDE:
    case MSS::OBS_RESIDUAL_AMPLITUDE:
      want(Amp,fld-MSS::AMPLITUDE)=true;
      break;
    case MSS::ANTENNA1:
      scalarRange(out,keyword,msc.antenna1(),oneBased);
      break;
    case MSS::ANTENNA2:
      scalarRange(out,keyword,msc.antenna2(),oneBased);
      break;
    case MSS::ANTENNAS:
      out.define(keyword,msc.antenna().name().getColumn());
      break;
    case MSS::ARRAY_ID:
      scalarRange(out,keyword,msc.arrayId(),oneBased);
      break;
    case MSS::CHAN_FREQ:
      {
	if (checkShapes()) {
	  out.define(keyword,
                     msc.spectralWindow().chanFreq().getColumnCells(RowNumbers(spwId_p)));
	} else {
	  shapeChangesWarning=true;
	}
      }
      break;
    case MSS::CORR_NAMES:
    case MSS::CORR_TYPES:
      {
	if (checkShapes()) {
	  Matrix<int32_t> corrTypes=
	    msc.polarization().corrType().getColumnCells(RowNumbers(polId_p));
	  if (fld==MSS::CORR_NAMES) {
	    Matrix<String> names(corrTypes.shape());
	    for (rownr_t k=0; k<names.nrow(); k++) {
	      for (uint32_t j=0; j<names.ncolumn(); j++) {
		names(k,j)=Stokes::name(Stokes::type(corrTypes(k,j)));
	      }
	    }
	    out.define(keyword,names);
	  } else {
	    out.define(keyword,corrTypes);
	  }
	} else {
	  shapeChangesWarning=true;
	}
      }
      break;
    case MSS::DATA:
    case MSS::CORRECTED_DATA:
    case MSS::MODEL_DATA:
    case MSS::RATIO_DATA:
    case MSS::RESIDUAL_DATA:
    case MSS::OBS_RESIDUAL_DATA:
      want(Data,fld-MSS::DATA)=true;
      break;
    case MSS::DATA_DESC_ID:
      scalarRange(out,keyword,msc.dataDescId(),oneBased);
      break;
    case MSS::FEED1:
      scalarRange(out,keyword,msc.feed1(),oneBased);
      break;
    case MSS::FEED2:
      scalarRange(out,keyword,msc.feed2(),oneBased);
      break;
    case MSS::FIELD_ID:
      scalarRange(out,keyword,msc.fieldId(),oneBased);
      break;
    case MSS::FIELDS:
      out.define(keyword,msc.field().name().getColumn());
      break;
    case MSS::FLOAT_DATA:
      want(Data,ObsFloat)=true;
      break;
    case MSS::IFR_NUMBER:
      {
	Vector<int32_t> ifr=ifrNumbers(msc.antenna1(),msc.antenna2());
	if (oneBased) ifr+=1001;
	out.define(keyword,ifr);
      }
      break;
    case MSS::IMAGINARY:
    case MSS::CORRECTED_IMAGINARY:
    case MSS::MODEL_IMAGINARY:
    case MSS::RATIO_IMAGINARY:
    case MSS::RESIDUAL_IMAGINARY:
    case MSS::OBS_RESIDUAL_IMAGINARY:
      want(Imag,fld-MSS::IMAGINARY)=true;
      break;
    case MSS::NUM_CORR:
      {
	checkShapes();
	out.define(keyword,msc.polarization().numCorr().getColumnCells(RowNumbers(polId_p)));
      }
      break;
    case MSS::NUM_CHAN:
      {
	checkShapes();
	out.define(keyword,msc.spectralWindow().numChan().getColumnCells(RowNumbers(spwId_p)));
      }
      break;
    case MSS::PHASE:
    case MSS::CORRECTED_PHASE:
    case MSS::MODEL_PHASE:
    case MSS::RATIO_PHASE:
    case MSS::RESIDUAL_PHASE:
    case MSS::OBS_RESIDUAL_PHASE:
      want(Phase,fld-MSS::PHASE)=true;
      break;
    case MSS::PHASE_DIR:
      {
	Record phasedir(RecordInterface::Variable);
	// return 0th order position only
	rownr_t nField = ms_p.field().nrow();
	Matrix<double> phaseDir(2,nField);
	Vector<double> dir(2);
	for (rownr_t i=0; i<nField; i++) {
	  dir=msc.field().phaseDirMeas(i).getAngle().getValue();
	  phaseDir(0,i)=dir(0); phaseDir(1,i)=dir(1);
	}
	phasedir.define("direction",phaseDir);
	phasedir.define("epoch",MDirection::showType
		     (msc.field().phaseDirMeasCol().getMeasRef().getType()));
	out.defineRecord(keyword,phasedir);
      }
      break;
    case MSS::REAL:
    case MSS::CORRECTED_REAL:
    case MSS::MODEL_REAL:
    case MSS::RATIO_REAL:
    case MSS::RESIDUAL_REAL:
    case MSS::OBS_RESIDUAL_REAL:
      want(Real,fld-MSS::REAL)=true;
      break;
    case MSS::REF_FREQUENCY:
      {
	checkShapes();
	out.define(keyword,msc.spectralWindow().refFrequency().
                   getColumnCells(RowNumbers(spwId_p)));
      }
      break;
    case MSS::ROWS:
      {
	// Glish doesn't like uint32_t (like me), so convert
	int64_t n=ms_p.nrow();
	Vector<rownr_t> rowNumbers=ms_p.rowNumbers();
	Vector<int64_t> rows(n);
	convertArray(rows,rowNumbers);
	if (oneBased) rows+=int64_t(1);
	out.define(keyword,rows);
      }
      break;
    case MSS::SCAN_NUMBER:
      scalarRange(out,keyword,msc.scanNumber(),oneBased);
      break;
    case MSS::SIGMA:
      if (checkShapes()) {
	Vector<float> range(2); 
	Array<float> sig;
	if (sel_p) sig=sel_p->getWeight(msc.sigma(),true);
	else sig=msc.sigma().getColumn();
	::casacore::minMax(range(0),range(1),sig);
	out.define(keyword,range);
      } else {
	shapeChangesWarning = true;
      }
      break;
    case MSS::TIME:
      {
	Vector<double> time(2);
	::casacore::minMax(time(0),time(1),msc.time().getColumn());
	out.define(keyword,time);
      }
      break;
    case MSS::TIMES:
      {
	Vector<double> times=msc.time().getColumn();
	int64_t n=GenSort<double>::sort (times, order, option);
	out.define(keyword,times(Slice(0,n)));
      }
      break;
    case MSS::U:
    case MSS::V:
    case MSS::W:
      {
	int32_t index=fld-MSS::U;
	Vector<double> range(2);
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	::casacore::minMax(range(0),range(1),uvw.row(index));
	out.define(keyword,range);
      }	
      break;
    case MSS::UVDIST:
      {
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	Array<double> u2,v2;
	u2=uvw.row(0);
	v2=uvw.row(1);
	u2*=u2;
        v2*=v2;
        u2+=v2;
	Vector<double> uvrange(2);
	::casacore::minMax(uvrange(0),uvrange(1),u2);
	uvrange(0)=sqrt(uvrange(0)); uvrange(1)=sqrt(uvrange(1));
	out.define(keyword,uvrange);
      }
      break;
    case MSS::WEIGHT:
      if (checkShapes()) {
	Vector<float> range(2); 
	Array<float> wt;
	if (sel_p) wt=sel_p->getWeight(msc.weight());
	else wt=msc.weight().getColumn();
	::casacore::minMax(range(0),range(1),wt);
	out.define(keyword,range);
      } else {
	shapeChangesWarning = true;
      }
      break;
    case MSS::UNDEFINED:
    default:
      { 
      }
    } 
  }
  // throw away the uvw data (if any)
  uvw.resize(0,0);
  for (int32_t dataType=Observed; dataType<nDataType; dataType++) {
    bool needCol2=false;
    if (anyEQ(want.column(dataType),true)) {
      ArrayColumn<Complex> colData1, colData2;
      if (dataType==Observed || dataType==ObsResidual) {
	colData1.reference(msc.data());
      } else if (dataType==Corrected || dataType==Ratio || dataType==Residual){
	colData1.reference(msc.correctedData());
      } else if (dataType==Model) {
	colData1.reference(msc.modelData());
      }
      if (dataType>=Ratio && dataType<=ObsResidual) {
	colData2.reference(msc.modelData());
	needCol2=true;
      }
      if (dataType!=ObsFloat) {
	if (!colData1.isNull()&&(!needCol2 || !colData2.isNull())) {
	  if (checkShapes()) {
	    if (anyEQ(want(Slice(Amp,4),dataType),true)) {
	      Matrix<float> minmax(2,4);
	      Vector<bool> funcSel(4);
	      for (int32_t funcType=Amp; funcType<=Imag; funcType++) {
		funcSel[funcType]=want(funcType,dataType);
	      }
	      minMax(minmax,funcSel,colData1,colData2,msc.flag(),
		     dataType,useFlags);
	      String name;
	      switch (dataType) {
	      case Corrected: name="corrected_"; break;
	      case Model: name="model_";break;
	      case Ratio: name="ratio_"; break;
	      case Residual: name="residual_"; break;
	      case ObsResidual: name="obs_residual_"; break;
	      default:;
	      }
	      Vector<String> funcName(4);
	      funcName(Amp)="amplitude";
	      funcName(Phase)="phase";
	      funcName(Real)="real";
	      funcName(Imag)="imaginary";
	      for (int32_t funcType=Amp; funcType<=Imag; funcType++) {
		if (want(funcType,dataType)) {
		  out.define(name+funcName(funcType),minmax.column(funcType));
		}
	      }
	    }
	    if (want(Data,dataType)) {
	      os << LogIO::WARN << "range not available for complex DATA"
		 <<LogIO::POST;
	    }
	  } else {
	    shapeChangesWarning=true;
	  }
	} else {
	  if (colData1.isNull()) {
	    os << LogIO::WARN <<"Requested data column doesn't exist"<<LogIO::POST;
	  }
	  if (needCol2 && colData2.isNull()) {
	    os << LogIO::WARN <<"Requested data column doesn't exist"<<LogIO::POST;
	  }
	}
      } else {
	if (!msc.floatData().isNull()) {
	  if (checkShapes()) {
	    Vector<float> amp(2);
	    minMax(amp(0),amp(1),msc.floatData(),msc.flag(),useFlags);
	    out.define("float_data",amp);
	  }
	} else {
	  os << LogIO::WARN << "FLOAT_DATA column doesn't exist"<<LogIO::POST;
	}
      }
    }
  }

  if (shapeChangesWarning) {
    os << LogIO::WARN << "Not all requested items were returned because "
       <<"the input contains "<< endl 
       <<"multiple data descriptions with varying data shape"<<LogIO::POST;
  }
  return out;
}

Record MSRange::range(MSS::Field item, bool useFlags)
{
  Vector<int32_t> key(1);
  key(0)=item;
  return range(key,useFlags);
}

void MSRange::setBlockSize(int32_t blockSize)
{
  if (blockSize>0) blockSize_p=blockSize;
}

void MSRange::scalarRange(Record& out, const String& item, 
		 const ScalarColumn<int32_t>& id, bool oneBased) 
{
  Vector<int32_t> ids=scalarRange(id);
  if (oneBased) ids+=1;
  out.define(item,ids);
}

Vector<int32_t> MSRange::scalarRange(const ScalarColumn<int32_t>& id)
{
  const int32_t option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;
  Vector<int32_t> idvec=id.getColumn();
  int64_t n=GenSort<int32_t>::sort (idvec, order, option);
  Vector<int32_t> ids=idvec(Slice(0,n));
  return ids;
}

void MSRange::minMax(float& mini, float& maxi, 
		     const ArrayColumn<float>& data,
		     const ArrayColumn<bool>& flag,
		     bool useFlags)
{
  IPosition shp=data.shape(0);
  rownr_t nrow=data.nrow();
  rownr_t numrow=rownr_t(blockSize_p*1.0e6/(sizeof(float)*shp(0)*shp(1)));
  for (rownr_t start=0; start<nrow; start+=numrow) {
    rownr_t n=min(numrow,nrow-start);
    float minf, maxf;
    Slicer rowSlicer(Slice(start,n));
    if (sel_p) {
      Array<bool> avFlag;
      Array<bool> flags = sel_p->getAveragedFlag(avFlag,flag,rowSlicer);
      Array<float> avData;
      sel_p->getAveragedData(avData,flags,data,rowSlicer);
      if (useFlags) {
	::casacore::minMax(minf,maxf,avData(!avFlag));
      } else {
	::casacore::minMax(minf,maxf,avData);
      }	
    } else {
      Array<float> tData=data.getColumnRange(rowSlicer);
      if (useFlags) {
	Array<bool> tFlag=flag.getColumnRange(rowSlicer);
	::casacore::minMax(minf,maxf,tData(!tFlag));
      } else {
	::casacore::minMax(minf,maxf,tData);
      }
    }
    if (start==0) {
      mini=minf; maxi=maxf;
    } else {
      mini=min(mini,minf);
      maxi=max(maxi,maxf);
    }
  }
}

void MSRange::minMax(Matrix<float>& minmax, 
		     const Vector<bool>& funcSel,
		     const ArrayColumn<Complex>& data1,
		     const ArrayColumn<Complex>& data2,
		     const ArrayColumn<bool>& flag,
		     int32_t dataType,
		     bool useFlags)
{
  IPosition shp=data1.shape(0);
  rownr_t nrow=data1.nrow();
  rownr_t numrow=rownr_t(blockSize_p*1.0e6/(sizeof(Complex)*shp(0)*shp(1)));
  for (rownr_t start=0; start<nrow; start+=numrow) {
    rownr_t n=min(numrow,nrow-start);
    Vector<float> minf(4), maxf(4);
    Slicer rowSlicer(Slice(start,n));
    Array<Complex> avData;
    if (sel_p) {
      Array<bool> avFlag;
      Array<bool> flags=sel_p->getAveragedFlag(avFlag,flag,rowSlicer);
      Array<Complex> tData;
      sel_p->getAveragedData(tData,flags,data1,rowSlicer);
      if (dataType>=Ratio && dataType<=ObsResidual) {
	Array<Complex> tData2;
	sel_p->getAveragedData(tData2,flags,data2,rowSlicer);
	if (dataType==Ratio) {
	  LogicalArray mask(tData2!=Complex(0.));
	  tData/=tData2(mask);
	  tData(!mask)=1.0;
	} else {
	  tData-=tData2;
	}
      }
      if (useFlags) avData=tData(!avFlag).getCompressedArray();
      else avData.reference(tData);
    } else {
      Array<Complex> tData=data1.getColumnRange(rowSlicer);
      if (dataType>=Ratio && dataType<=ObsResidual) {
	Array<Complex> tData2=data2.getColumnRange(rowSlicer);
	if (dataType==Ratio) {
	  LogicalArray mask(tData2!=Complex(0.));
	  tData/=tData2(mask);
	  tData(!mask)=1.0;
	} else {
	  tData-=tData2;
	}
      }
      if (useFlags) {
	Array<bool> avFlag=flag.getColumnRange(rowSlicer);
	avData=tData(!avFlag).getCompressedArray();
      } else {
	avData.reference(tData);
      }
    }
    // If any unflagged data, get min/max
    if (avData.nelements() > 0) {

      if (funcSel[0]) ::casacore::minMax(minf[0],maxf[0],amplitude(avData));
      if (funcSel[1]) ::casacore::minMax(minf[1],maxf[1],phase(avData));
      if (funcSel[2]) ::casacore::minMax(minf[2],maxf[2],real(avData));
      if (funcSel[3]) ::casacore::minMax(minf[3],maxf[3],imag(avData));
      if (start==0) {
	minmax.row(0)=minf; minmax.row(1)=maxf;
      } else {
	minmax.row(0)=::casacore::min(static_cast<Array<float> >(minmax.row(0)),
			    static_cast<Array<float> >(minf));
	minmax.row(1)=::casacore::max(static_cast<Array<float> >(minmax.row(1))
			    ,static_cast<Array<float> >(maxf));
      }
    }

  }
}

Vector<int32_t> MSRange::ifrNumbers(const ScalarColumn<int32_t>& ant1,
				const ScalarColumn<int32_t>& ant2)
{
  const int32_t option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;
  Vector<int32_t> a1=ant1.getColumn();
  Array<int32_t> a2=ant2.getColumn();
  DebugAssert(max(a1)<1000 && max(a2)<1000,AipsError);
  a1*=1000; a1+=a2;
  int64_t n=GenSort<int32_t>::sort (a1, order, option);
  return a1(Slice(0,n));
}

} //# NAMESPACE CASACORE - END

