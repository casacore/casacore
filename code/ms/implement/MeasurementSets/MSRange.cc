//# MSRange.cc: selection and iteration of an MS
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

#include <trial/MeasurementSets/MSRange.h>
#include <trial/MeasurementSets/MSSelector.h>

#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Slice.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Logging/LogIO.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MDirection.h>
#include <aips/Tables/RefRows.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Assert.h>


MSRange::MSRange():blockSize_p(10),ddId_p(0),constantShape_p(False),sel_p(0)
{}

MSRange::MSRange(const MeasurementSet& ms)
:ms_p(ms),blockSize_p(10),constantShape_p(False),sel_p(0)
{}

MSRange::MSRange(const MSSelector& msSel)
  :ms_p(msSel.selectedTable()),blockSize_p(10),constantShape_p(False),
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

Bool MSRange::checkShapes()
{
  Int n=ddId_p.nelements();
  // check already done
  if (n>0 && spwId_p.nelements()>0) return constantShape_p; 
  constantShape_p=True;
  if (n==0) {
    ROScalarColumn<Int> dd(ms_p,MS::columnName(MS::DATA_DESC_ID));
    Vector<Int> ddId=scalarRange(dd);
    ddId_p=ddId;
  }
  Int n2=ddId_p.nelements();
  ROMSDataDescColumns ddc(ms_p.dataDescription());
  spwId_p.resize(n2);
  polId_p.resize(n2);
  for (Int i=0; i<n2; i++) {
    spwId_p(i)=ddc.spectralWindowId()(ddId_p(i));
    polId_p(i)=ddc.polarizationId()(ddId_p(i));
  }
  if (n>0) return constantShape_p; // no need to check, done by MSSelector

  // check if the shape is the same for all spectral windows that occur
  // in the main table
  ROMSSpWindowColumns spwc(ms_p.spectralWindow());
  ROMSPolarizationColumns polc(ms_p.polarization());
  for (Int i=1; i<n2; i++) {
    if (spwc.numChan()(spwId_p(i)) != spwc.numChan()(spwId_p(i-1)) ||
	polc.numCorr()(polId_p(i)) != polc.numCorr()(polId_p(i-1))) {
      constantShape_p = False;
      break;
    }
  }
  return constantShape_p;
}

GlishRecord MSRange::range(const Vector<String>& items, 
			   Bool useFlags,
			   Bool oneBased)
{
  LogIO os;
  Int n=items.nelements();
  Vector<Int> keys(n);
  // translate strings to enums
  Int k=0;
  String keyword;
  for (Int i=0; i<n; i++) {
    keyword=downcase(items(i));
    keys(k)=MSS::field(keyword);
    if (keys(k)!=MSS::UNDEFINED) {
      k++;
    } else {
      os<< LogIO::WARN << "Unrecognized field in input ignored: "<<
	keyword<< LogIO::POST;
    }
  }
  keys.resize(k,True); // squeeze out the UNDEFINEDs
  return range(keys,useFlags,oneBased);
}

GlishRecord MSRange::range(const Vector<Int>& keys, 
			   Bool useFlags,
			   Bool oneBased)
{
  LogIO os;
  const Int option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;

  GlishRecord out;
  if (ms_p.nrow()==0) {
    os<< LogIO::WARN << "Table is empty - nothing to do"<<LogIO::POST;
    return out;
  }
  ROMSColumns msc(ms_p);
  Bool wantAmp, wantPhase, wantReal, wantImag, wantData, wantFloat,
    wantCAmp, wantCPhase, wantCReal, wantCImag, wantCData,
    wantMAmp, wantMPhase, wantMReal, wantMImag, wantMData;
  wantAmp=wantPhase=wantReal=wantImag=wantData=wantFloat=
    wantCAmp=wantCPhase=wantCReal=wantCImag=wantCData=
    wantMAmp=wantMPhase=wantMReal=wantMImag=wantMData=False;
  // use HeapSort as it's performance is guaranteed, quicksort is often
  // extremely slow (O(n*n)) for inputs with many successive duplicates
  Matrix<Double> uvw;
  Bool shapeChangesWarning=False;
  Int n=keys.nelements();
  String keyword;
  for (Int i=0; i<n; i++) {
    // get the enum and the keyword value
    MSS::Field fld=MSS::Field(keys(i));
    keyword=MSS::keyword(fld);
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
    case MSS::ANTENNA1:
      scalarRange(out,keyword,msc.antenna1(),oneBased);
      break;
    case MSS::ANTENNA2:
      scalarRange(out,keyword,msc.antenna2(),oneBased);
      break;
    case MSS::ANTENNAS:
      out.add(keyword,msc.antenna().name().getColumn());
      break;
    case MSS::ARRAY_ID:
      scalarRange(out,keyword,msc.arrayId(),oneBased);
      break;
    case MSS::CHAN_FREQ:
      {
	if (checkShapes()) {
	  out.add(keyword,
		  msc.spectralWindow().chanFreq().getColumnCells(spwId_p));
	} else {
	  shapeChangesWarning=True;
	}
      }
      break;
    case MSS::CORR_NAMES:
    case MSS::CORR_TYPES:
      {
	if (checkShapes()) {
	  Matrix<Int> corrTypes=
	    msc.polarization().corrType().getColumnCells(polId_p);
	  if (fld==MSS::CORR_NAMES) {
	    Matrix<String> names(corrTypes.shape());
	    for (uInt k=0; k<names.nrow(); k++) {
	      for (uInt j=0; j<names.ncolumn(); j++) {
		names(k,j)=Stokes::name(Stokes::type(corrTypes(k,j)));
	      }
	    }
	    out.add(keyword,names);
	  } else {
	    out.add(keyword,corrTypes);
	  }
	} else {
	  shapeChangesWarning=True;
	}
      }
      break;
    case MSS::DATA:
      wantData=True;
      break;
    case MSS::CORRECTED_DATA:
      wantCData=True;
      break;
    case MSS::MODEL_DATA:
      wantMData=True;
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
      out.add(keyword,msc.field().name().getColumn());
      break;
    case MSS::FLOAT_DATA:
      wantFloat=True;
      break;
    case MSS::IFR_NUMBER:
      {
	Vector<Int> ifr=ifrNumbers(msc.antenna1(),msc.antenna2());
	if (oneBased) ifr+=1001;
	out.add(keyword,ifr);
      }
      break;
    case MSS::IMAGINARY:
      wantImag=True;
      break;
    case MSS::CORRECTED_IMAGINARY:
      wantCImag=True;
      break;
    case MSS::MODEL_IMAGINARY:
      wantMImag=True;
      break;
    case MSS::IMAGING_WEIGHT:
      {
	if (checkShapes()) {
	  if (!msc.imagingWeight().isNull()) {
	    Vector<Float> range(2);
	    ::minMax(range(0),range(1),msc.imagingWeight().getColumn());
	    out.add(keyword,range);
	  } else {
	    os << LogIO::WARN << "IMAGING_WEIGHT column doesn't exist"<< 
	      LogIO::POST;
	  }
	} else {
	  shapeChangesWarning=True;
	}
      }
      break;
    case MSS::NUM_CORR:
      {
	checkShapes();
	out.add(keyword,msc.polarization().numCorr().getColumnCells(polId_p));
      }
      break;
    case MSS::NUM_CHAN:
      {
	checkShapes();
	out.add(keyword,msc.spectralWindow().numChan().getColumnCells(spwId_p));
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
    case MSS::PHASE_DIR:
      {
	GlishRecord phasedir;
	// return 0th order position only
	Int nField = ms_p.field().nrow();
	Matrix<Double> phaseDir(2,nField);
	Vector<Double> dir(2);
	for (Int i=0; i<nField; i++) {
	  dir=msc.field().phaseDirMeas(i).getAngle().getValue();
	  phaseDir(0,i)=dir(0); phaseDir(1,i)=dir(1);
	}
	phasedir.add("direction",phaseDir);
	phasedir.add("epoch",MDirection::showType
		     (msc.field().phaseDirMeasCol().getMeasRef().getType()));
	out.add(keyword,phasedir);
      }
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
    case MSS::REF_FREQUENCY:
      {
	checkShapes();
	out.add(keyword,msc.spectralWindow().refFrequency().
		getColumnCells(spwId_p));
      }
      break;
    case MSS::ROWS:
      {
	// Glish doesn't like uInt (like me), so convert
	Int n=ms_p.nrow();
	Vector<uInt> rowNumbers=ms_p.rowNumbers();
	Vector<Int> rows(n);
	convertArray(rows,rowNumbers);
	if (oneBased) rows+=1;
	out.add(keyword,rows);
      }
      break;
    case MSS::SCAN_NUMBER:
      scalarRange(out,keyword,msc.scanNumber(),oneBased);
      break;
    case MSS::SIGMA:
      if (checkShapes()) {
	Vector<Float> range(2); 
	Array<Float> sig;
	if (sel_p) sig=sel_p->getWeight(msc.sigma(),True);
	else sig=msc.sigma().getColumn();
	::minMax(range(0),range(1),sig);
	out.add(keyword,range);
      } else {
	shapeChangesWarning = True;
      }
      break;
    case MSS::TIME:
      {
	Vector<Double> time(2);
	::minMax(time(0),time(1),msc.time().getColumn());
	out.add(keyword,time);
      }
      break;
    case MSS::TIMES:
      {
	Vector<Double> times=msc.time().getColumn();
	Int n=GenSort<Double>::sort (times, order, option);
	out.add(keyword,times(Slice(0,n)));
      }
      break;
    case MSS::U:
    case MSS::V:
    case MSS::W:
      {
	Int index=fld-MSS::U;
	Vector<Double> range(2);
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	::minMax(range(0),range(1),uvw.row(index));
	out.add(keyword,range);
      }	
      break;
    case MSS::UVDIST:
      {
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	Array<Double> u2,v2;
	u2=uvw.row(0);
	v2=uvw.row(1);
	u2*=u2;
        v2*=v2;
        u2+=v2;
	Vector<Double> uvrange(2);
	::minMax(uvrange(0),uvrange(1),u2);
	uvrange(0)=sqrt(uvrange(0)); uvrange(1)=sqrt(uvrange(1));
	out.add(keyword,uvrange);
      }
      break;
    case MSS::WEIGHT:
      if (checkShapes()) {
	Vector<Float> range(2); 
	Array<Float> wt;
	if (sel_p) wt=sel_p->getWeight(msc.weight());
	else wt=msc.weight().getColumn();
	::minMax(range(0),range(1),wt);
	out.add(keyword,range);
      } else {
	shapeChangesWarning = True;
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
  if (wantAmp || wantPhase || wantReal || wantImag || wantData) {
    if (!msc.data().isNull()) {
      if (checkShapes()) {
	// this now gets the data in smaller chunks (8MB) with getColumnRange
	// but it no longer caches the data read if more than one item is
	// requested. Maybe we need to make a 4-fold minMax that can do
	// 1-4 items at once if needed.
	if (wantAmp) {
	  Vector<Float> amp(2);
	  minMax(amp(0),amp(1),amplitude,msc.data(),msc.flag(),useFlags);
	  out.add("amplitude",amp);
	}
	if (wantPhase) {
	  Vector<Float> phas(2);
	  minMax(phas(0),phas(1),phase,msc.data(),msc.flag(),useFlags);
	  out.add("phase",phas);
	}
	if (wantReal) {
	  Vector<Float> re(2);
	  minMax(re(0),re(1),real,msc.data(),msc.flag(),useFlags);
	  out.add("real",re);
	}
	if (wantImag) {
	  Vector<Float> im(2);
	  minMax(im(0),im(1),imag,msc.data(),msc.flag(),useFlags);
	  out.add("imaginary",im);
	}
	if (wantData) {
	  os << LogIO::WARN << "range not available for complex DATA"
	     <<LogIO::POST;
	}
      } else {
	shapeChangesWarning=True;
      }
    } else {
      os << LogIO::WARN << "DATA column doesn't exist"<<LogIO::POST;
    }
  }

  if (wantFloat) {
    if (!msc.floatData().isNull()) {
      if (checkShapes()) {
	Vector<Float> amp(2);
	minMax(amp(0),amp(1),msc.floatData(),msc.flag(),useFlags);
	out.add("float_data",amp);
      }
    }
  }

  if (wantCAmp || wantCPhase || wantCReal || wantCImag || wantCData) {
    if (!msc.correctedData().isNull()) {
      if (checkShapes()) {
	// get the data
	if (wantCAmp) {
	  Vector<Float> amp(2);
	  minMax(amp(0),amp(1),amplitude,msc.correctedData(),msc.flag(),useFlags);
	  out.add("corrected_amplitude",amp);
	}
	if (wantCPhase) {
	  Vector<Float> phas(2);
	  minMax(phas(0),phas(1),phase,msc.correctedData(),msc.flag(),useFlags);
	  out.add("corrected_phase",phas);
	}
	if (wantCReal) {
	  Vector<Float> re(2);
	  minMax(re(0),re(1),real,msc.correctedData(),msc.flag(),useFlags);
	  out.add("corrected_real",re);
	}
	if (wantCImag) {
	  Vector<Float> im(2);
	  minMax(im(0),im(1),imag,msc.correctedData(),msc.flag(),useFlags);
	  out.add("corrected_imaginary",im);
	}
	if (wantCData) {
	  os << LogIO::WARN << "range not available for complex CORRECTED_DATA"
	     <<LogIO::POST;
	}
      } else {
	shapeChangesWarning=True;
      }
    } else {
	os << LogIO::WARN << "CORRECTED_DATA column doesn't exist"<<LogIO::POST;
    }
  }
  if (wantMAmp || wantMPhase || wantMReal || wantMImag || wantMData) {
    if (!msc.modelData().isNull()) {
      if (checkShapes()) {
	// get the data
	if (wantMAmp) {
	  Vector<Float> amp(2);
	  minMax(amp(0),amp(1),amplitude,msc.modelData(),msc.flag(),useFlags);
	  out.add("model_amplitude",amp);
	}
	if (wantMPhase) {
	  Vector<Float> phas(2);
	  minMax(phas(0),phas(1),phase,msc.modelData(),msc.flag(),useFlags);
	  out.add("model_phase",phas);
	}
	if (wantMReal) {
	  Vector<Float> re(2);
	  minMax(re(0),re(1),real,msc.modelData(),msc.flag(),useFlags);
	  out.add("model_real",re);
	}
	if (wantMImag) {
	  Vector<Float> im(2);
	  minMax(im(0),im(1),imag,msc.modelData(),msc.flag(),useFlags);
	  out.add("model_imaginary",im);
	}
	if (wantMData) {
	  os << LogIO::WARN << "range not available for complex MODEL_DATA"
	     <<LogIO::POST;
	}
      } else { 
	shapeChangesWarning=True;
      }
    } else {
      os << LogIO::WARN << "MODEL_DATA column doesn't exist"<<LogIO::POST;
    }
  }
  if (shapeChangesWarning) {
    os << LogIO::WARN << "Not all requested items were returned because "
       <<"the input contains "<< endl 
       <<"multiple data descriptions with varying data shape"<<LogIO::POST;
  }
  return out;
}

GlishRecord MSRange::range(MSS::Field item, Bool useFlags)
{
  Vector<Int> key(1);
  key(0)=item;
  return range(key,useFlags);
}

void MSRange::setBlockSize(Int blockSize)
{
  if (blockSize>0) blockSize_p=blockSize;
}

void MSRange::scalarRange(GlishRecord& out, const String& item, 
		 const ROScalarColumn<Int>& id, Bool oneBased) 
{
  Vector<Int> ids=scalarRange(id);
  if (oneBased) ids+=1;
  out.add(item,ids);
}

Vector<Int> MSRange::scalarRange(const ROScalarColumn<Int>& id)
{
  const Int option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;
  Vector<Int> idvec=id.getColumn();
  Int n=GenSort<Int>::sort (idvec, order, option);
  Vector<Int> ids=idvec(Slice(0,n));
  return ids;
}

void MSRange::minMax(Float& mini, Float& maxi, 
		     const ROArrayColumn<Float>& data,
		     const ROArrayColumn<Bool>& flag,
		     Bool useFlags)
{
  IPosition shp=data.shape(0);
  Int nrow=data.nrow();
  Int numrow=Int(blockSize_p*1.0e6/(sizeof(Float)*shp(0)*shp(1)));
  for (Int start=0; start<nrow; start+=numrow) {
    Int n=min(numrow,nrow-start);
    Float minf, maxf;
    Slicer rowSlicer(Slice(start,n));
    if (sel_p) {
      Array<Float> avData;
      sel_p->getAveragedData(avData,data,rowSlicer);
      if (useFlags) {
	Array<Bool> avFlag;
	sel_p->getAveragedFlag(avFlag,flag,rowSlicer);
	::minMax(minf,maxf,avData(!avFlag));
      } else {
	::minMax(minf,maxf,avData);
      }	
    } else {
      Array<Float> tData=data.getColumnRange(rowSlicer);
      if (useFlags) {
	Array<Bool> tFlag=flag.getColumnRange(rowSlicer);
	::minMax(minf,maxf,tData(!tFlag));
      } else {
	::minMax(minf,maxf,tData);
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

void MSRange::minMax(Float& mini, Float& maxi, 
		     Array<Float> (*func)(const Array<Complex>&),
		     const ROArrayColumn<Complex>& data,
		     const ROArrayColumn<Bool>& flag,
		     Bool useFlags)
{
  IPosition shp=data.shape(0);
  Int nrow=data.nrow();
  Int numrow=Int(blockSize_p*1.0e6/(sizeof(Complex)*shp(0)*shp(1)));
  for (Int start=0; start<nrow; start+=numrow) {
    Int n=min(numrow,nrow-start);
    Float minf, maxf;
    Slicer rowSlicer(Slice(start,n));
    if (sel_p) {
      Array<Complex> avData;
      sel_p->getAveragedData(avData,data,rowSlicer);
      if (useFlags) {
	Array<Bool> avFlag;
	sel_p->getAveragedFlag(avFlag,flag,rowSlicer);
      ::minMax(minf,maxf,func(avData(!avFlag).getCompressedArray()));
      } else {
	::minMax(minf,maxf,func(avData));
      }
    } else {
      Array<Complex> tData=data.getColumnRange(rowSlicer);
      if (useFlags) {
	Array<Bool> tFlag=flag.getColumnRange(rowSlicer);
	::minMax(minf,maxf,func(tData(!tFlag).getCompressedArray()));
      } else {
	::minMax(minf,maxf,func(tData));
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

Vector<Int> MSRange::ifrNumbers(const ROScalarColumn<Int>& ant1,
				const ROScalarColumn<Int>& ant2)
{
  const Int option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;
  Vector<Int> a1=ant1.getColumn();
  Array<Int> a2=ant2.getColumn();
  DebugAssert(max(a1)<1000 && max(a2)<1000,AipsError);
  a1*=1000; a1+=a2;
  Int n=GenSort<Int>::sort (a1, order, option);
  return a1(Slice(0,n));
}
