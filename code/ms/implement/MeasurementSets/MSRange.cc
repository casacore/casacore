//# NewMSRange.cc: selection and iteration of an MS
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

#include <trial/MeasurementSets/NewMSRange.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Logging/LogIO.h>
#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MDirection.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Assert.h>

static LogIO os;

NewMSRange::NewMSRange():blockSize_p(10),ddId_p(UNCHECKED),
checked_p(False)
{}

NewMSRange::NewMSRange(const NewMeasurementSet& ms, Int dataDescriptionId)
:ms_p(ms),blockSize_p(10),ddId_p(dataDescriptionId),
checked_p(False)
{
}

NewMSRange::NewMSRange(const NewMSRange& other)
{ operator=(other); }


NewMSRange& NewMSRange::operator=(const NewMSRange& other)
{
  if (this==&other) return *this;
  ms_p=other.ms_p;
  blockSize_p=other.blockSize_p;
  ddId_p=other.ddId_p;
  checked_p=other.checked_p;
  return *this;
}

void NewMSRange::setMS(const NewMeasurementSet& ms, Int dataDescriptionId)
{
  ms_p=ms;
  ddId_p=dataDescriptionId;
  checked_p=False;
}

Bool NewMSRange::checkSelection()
{
  if (!checked_p) {
    if (ddId_p<0) {
      // check dd
      ROScalarColumn<Int> dd(ms_p,NewMS::columnName(NewMS::DATA_DESC_ID));
      Vector<Int> ddId=scalarRange(dd);
      Int ndd=ddId.nelements();
      if (ndd==1) {
	ddId_p=dd(0);
      }
      if (ddId_p<0) {
	// check if the shape is the same for all spectral windows that occur
	// in the main table
	Bool constantShape=True;
	RONewMSDataDescColumns ddc(ms_p.dataDescription());
	RONewMSSpWindowColumns spwc(ms_p.spectralWindow());
	RONewMSPolarizationColumns polc(ms_p.polarization());
	for (Int i=1; i<ndd; i++) {
	  if (spwc.numChan()(ddc.spectralWindowId()(i)) != 
	      spwc.numChan()(ddc.spectralWindowId()(i-1)) ||
	      polc.numCorr()(ddc.spectralWindowId()(i)) != 
	      polc.numCorr()(ddc.spectralWindowId()(i-1))) {
	    constantShape = False;
	    break;
	  }
	}
	if (constantShape) ddId_p=ALL; 
	else ddId_p=UNSELECTED; // unselected, not all items available
      }
    }
    checked_p=True;
  }
  return ToBool(ddId_p>=ALL);
}

GlishRecord NewMSRange::range(const Vector<String>& items, Bool oneBased)
{
  Int n=items.nelements();
  Vector<Int> keys(n);
  // translate strings to enums
  Int k=0;
  String keyword;
  for (Int i=0; i<n; i++) {
    keyword=downcase(items(i));
    keys(k)=NewMSS::field(keyword);
    if (keys(k)!=NewMSS::UNDEFINED) {
      k++;
    } else {
      os<< LogIO::WARN << "Unrecognized field in input ignored: "<<
	keyword<< LogIO::POST;
    }
  }
  keys.resize(k,True); // squeeze out the UNDEFINEDs
  return range(keys,oneBased);
}

GlishRecord NewMSRange::range(const Vector<Int>& keys, Bool oneBased)
{
  // TODO: apply channel selection? polconversion?

  const Int option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;

  GlishRecord out;
  if (ms_p.nrow()==0) {
    os<< LogIO::WARN << "Table is empty - use setMS"<<LogIO::POST;
    return out;
  }
  RONewMSColumns msc(ms_p);
  Bool wantAmp, wantPhase, wantReal, wantImag, wantData,
    wantCAmp, wantCPhase, wantCReal, wantCImag, wantCData,
    wantMAmp, wantMPhase, wantMReal, wantMImag, wantMData;
  wantAmp=wantPhase=wantReal=wantImag=wantData=
    wantCAmp=wantCPhase=wantCReal=wantCImag=wantCData=
    wantMAmp=wantMPhase=wantMReal=wantMImag=wantMData=False;
  // use HeapSort as it's performance is guaranteed, quicksort is often
  // extremely slow (O(n*n)) for inputs with many successive duplicates
  Matrix<Double> uvw;
  Bool unselectedWarning=False;
  Int n=keys.nelements();
  String keyword;
  for (Int i=0; i<n; i++) {
    // get the enum and the keyword value
    NewMSS::Field fld=NewMSS::Field(keys(i));
    keyword=NewMSS::keyword(fld);
    switch (fld) {
    case NewMSS::AMPLITUDE:
      wantAmp=True;
      break;
    case NewMSS::CORRECTED_AMPLITUDE:
      wantCAmp=True;
      break;
    case NewMSS::MODEL_AMPLITUDE:
      wantMAmp=True;
      break;
    case NewMSS::ANTENNA1:
      scalarRange(out,keyword,msc.antenna1(),oneBased);
      break;
    case NewMSS::ANTENNA2:
      scalarRange(out,keyword,msc.antenna2(),oneBased);
      break;
    case NewMSS::ANTENNAS:
      out.add(keyword,msc.antenna().name().getColumn());
      break;
    case NewMSS::ARRAY_ID:
      scalarRange(out,keyword,msc.arrayId(),oneBased);
      break;
    case NewMSS::CHAN_FREQ:
      {
	Array<Double> chanFreq;
	Bool selected=checkSelection();
	if (ddId_p == ALL) {
	  chanFreq=msc.spectralWindow().chanFreq().getColumn();
	  out.add(keyword,chanFreq);
	} else if (selected) {
	  if (Int(ms_p.dataDescription().nrow()) > ddId_p);
	  chanFreq=msc.spectralWindow().chanFreq()(msc.dataDescription().
						   spectralWindowId()(ddId_p));
	  out.add(keyword,chanFreq);
	} else {
	  unselectedWarning=True;
	}
      }
      break;
    case NewMSS::CORR_NAMES:
    case NewMSS::CORR_TYPES:
      {
	Bool selected=checkSelection();
	if (ddId_p == ALL) {
	  Matrix<Int> corrTypes=
	    msc.polarization().corrType().getColumn();
	  if (fld==NewMSS::CORR_NAMES) {
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
	} else if (selected) {
	  if (ddId_p<Int(ms_p.dataDescription().nrow())) {
	    Vector<Int> corrTypes=
	      msc.polarization().corrType()
	      (msc.dataDescription().polarizationId()(ddId_p));
	    if (fld==NewMSS::CORR_NAMES) {
	      Vector<String> names(corrTypes.nelements());
	      for (uInt k=0; k<names.nelements(); k++) {
		names(k)=Stokes::name(Stokes::type(corrTypes(k)));
	      }
	      out.add(keyword,names);
	    } else {
	      out.add(keyword,corrTypes);
	    }
	  }
	} else {
	  unselectedWarning=True;
	}
      }
      break;
    case NewMSS::DATA:
      wantData=True;
      break;
    case NewMSS::CORRECTED_DATA:
      wantCData=True;
      break;
    case NewMSS::MODEL_DATA:
      wantMData=True;
      break;
    case NewMSS::DATA_DESC_ID:
      scalarRange(out,keyword,msc.dataDescId(),oneBased);
      break;
    case NewMSS::FEED1:
      scalarRange(out,keyword,msc.feed1(),oneBased);
      break;
    case NewMSS::FEED2:
      scalarRange(out,keyword,msc.feed2(),oneBased);
      break;
    case NewMSS::FIELD_ID:
      scalarRange(out,keyword,msc.fieldId(),oneBased);
      break;
    case NewMSS::FIELDS:
      out.add(keyword,msc.field().name().getColumn());
      break;
    case NewMSS::IFR_NUMBER:
      {
	Vector<Int> ifr=ifrNumbers(msc.antenna1(),msc.antenna2());
	if (oneBased) ifr+=1001;
	out.add(keyword,ifr);
      }
      break;
    case NewMSS::IMAGINARY:
      wantImag=True;
      break;
    case NewMSS::CORRECTED_IMAGINARY:
      wantCImag=True;
      break;
    case NewMSS::MODEL_IMAGINARY:
      wantMImag=True;
      break;
    case NewMSS::IMAGING_WEIGHT:
      {
	Bool selected=checkSelection();
	if (selected) {
	  if (!msc.imagingWeight().isNull()) {
	    Vector<Float> range(2);
	    ::minMax(range(0),range(1),msc.imagingWeight().getColumn());
	    out.add(keyword,range);
	  } else {
	    os << LogIO::WARN << "IMAGING_WEIGHT column doesn't exist"<< 
	      LogIO::POST;
	  }
	} else {
	  unselectedWarning=True;
	}
      }
      break;
    case NewMSS::NUM_CORR:
      {
	Bool selected=checkSelection();
	if (selected) {
	  Int polId=msc.dataDescription().polarizationId()
	    (msc.dataDescId()(0));
	  out.add(keyword,msc.polarization().numCorr()(polId));
	} else {
	  unselectedWarning=True;
	}
      }
      break;
    case NewMSS::NUM_CHAN:
      {
	Bool selected=checkSelection();
	if (selected) {
	  Int spwId=msc.dataDescription().spectralWindowId()
	    (msc.dataDescId()(0));
	  out.add(keyword,msc.spectralWindow().numChan()(spwId));
	} else {
	  unselectedWarning=True;
	}
      }
      break;
    case NewMSS::PHASE:
      wantPhase=True;
      break;
    case NewMSS::CORRECTED_PHASE:
      wantCPhase=True;
      break;
    case NewMSS::MODEL_PHASE:
      wantMPhase=True;
      break;
    case NewMSS::PHASE_DIR:
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
    case NewMSS::REAL:
      wantReal=True;
      break;
    case NewMSS::CORRECTED_REAL:
      wantCReal=True;
      break;
    case NewMSS::MODEL_REAL:
      wantMReal=True;
      break;
    case NewMSS::REF_FREQUENCY:
      {
	Bool selected=checkSelection();
	if (ddId_p==ALL) {
	  // return all values
	  out.add(keyword,msc.spectralWindow().refFrequency().getColumn());
	} else if (selected) {
	  if (Int(ms_p.dataDescription().nrow())>ddId_p) {
	    out.add(keyword,
		    msc.spectralWindow().refFrequency()
		    (msc.dataDescription().spectralWindowId()(ddId_p)));
	  }
	} else {
	  unselectedWarning=True;
	}
      }
      break;
    case NewMSS::ROWS:
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
    case NewMSS::SCAN_NUMBER:
      scalarRange(out,keyword,msc.scanNumber(),oneBased);
      break;
    case NewMSS::TIME:
      {
	Vector<Double> time(2);
	::minMax(time(0),time(1),msc.time().getColumn());
	out.add(keyword,time);
      }
      break;
    case NewMSS::TIMES:
      {
	Vector<Double> times=msc.time().getColumn();
	Int n=GenSort<Double>::sort (times, order, option);
	out.add(keyword,times(Slice(0,n)));
      }
      break;
    case NewMSS::U:
    case NewMSS::V:
    case NewMSS::W:
      {
	Int index=fld-NewMSS::U;
	Vector<Double> range(2);
	if (uvw.nelements()==0) uvw=msc.uvw().getColumn();
	::minMax(range(0),range(1),uvw.row(index));
	out.add(keyword,range);
      }	
      break;
    case NewMSS::UVDIST:
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
    case NewMSS::WEIGHT:
      {
	Vector<Float> range(2); 
	::minMax(range(0),range(1),msc.weight().getColumn());
	out.add(keyword,range);
      }
      break;
    case NewMSS::UNDEFINED:
    default:
      { 
      }
    } 
  }
  // throw away the uvw data (if any)
  uvw.resize(0,0);
  if (wantAmp || wantPhase || wantReal || wantImag || wantData) {
    if (checkSelection()) {
      // this now gets the data in smaller chunks (8MB) with getColumnRange
      // but it no longer caches the data read if more than one item is
      // requested. Maybe we need to make a 4-fold minMax that can do
      // 1-4 items at once if needed.
      if (wantAmp) {
	Vector<Float> amp(2);
	minMax(amp(0),amp(1),amplitude,msc.data());
	out.add("amplitude",amp);
      }
      if (wantPhase) {
	Vector<Float> phas(2);
	minMax(phas(0),phas(1),phase,msc.data());
	out.add("phase",phas);
      }
      if (wantReal) {
	Vector<Float> re(2);
	minMax(re(0),re(1),real,msc.data());
	out.add("real",re);
      }
      if (wantImag) {
	Vector<Float> im(2);
	minMax(im(0),im(1),imag,msc.data());
	out.add("imaginary",im);
      }
      if (wantData) {
	os << LogIO::WARN << "range not available for complex DATA"
	   <<LogIO::POST;
      }
    } else {
      unselectedWarning=True;
    }
  }
  if (wantCAmp || wantCPhase || wantCReal || wantCImag || wantCData) {
    if (!msc.correctedData().isNull()) {
      if (checkSelection()) {
	// get the data
	if (wantCAmp) {
	  Vector<Float> amp(2);
	  minMax(amp(0),amp(1),amplitude,msc.correctedData());
	  out.add("correctedamplitude",amp);
	}
	if (wantCPhase) {
	  Vector<Float> phas(2);
	  minMax(phas(0),phas(1),phase,msc.correctedData());
	  out.add("correctedphase",phas);
	}
	if (wantCReal) {
	  Vector<Float> re(2);
	  minMax(re(0),re(1),real,msc.correctedData());
	  out.add("correctedreal",re);
	}
	if (wantCImag) {
	  Vector<Float> im(2);
	  minMax(im(0),im(1),imag,msc.correctedData());
	  out.add("correctedimaginary",im);
	}
	if (wantCData) {
	  os << LogIO::WARN << "range not available for complex CORRECTED_DATA"
	     <<LogIO::POST;
	}
      } else {
	unselectedWarning=True;
      }
    } else {
	os << LogIO::WARN << "CORRECTED_DATA column doesn't exist"<<LogIO::POST;
    }
  }
  if (wantMAmp || wantMPhase || wantMReal || wantMImag || wantMData) {
    if (!msc.modelData().isNull()) {
      if (checkSelection()) {
	// get the data
	if (wantMAmp) {
	  Vector<Float> amp(2);
	  minMax(amp(0),amp(1),amplitude,msc.modelData());
	  out.add("modelamplitude",amp);
	}
	if (wantMPhase) {
	  Vector<Float> phas(2);
	  minMax(phas(0),phas(1),phase,msc.modelData());
	  out.add("modelphase",phas);
	}
	if (wantMReal) {
	  Vector<Float> re(2);
	  minMax(re(0),re(1),real,msc.modelData());
	  out.add("modelreal",re);
	}
	if (wantMImag) {
	  Vector<Float> im(2);
	  minMax(im(0),im(1),imag,msc.modelData());
	  out.add("modelimaginary",im);
	}
	if (wantMData) {
	  os << LogIO::WARN << "range not available for complex MODEL_DATA"
	     <<LogIO::POST;
	}
      } else { 
	unselectedWarning=True;
      }
    } else {
      os << LogIO::WARN << "MODEL_DATA column doesn't exist"<<LogIO::POST;
    }
  }
  if (unselectedWarning) {
    os << LogIO::WARN << "Not all requested items were returned because "
       << "of multiple spectral windows in the input"<<LogIO::POST;
  }
  return out;
}

GlishRecord NewMSRange::range(NewMSS::Field item)
{
  Vector<Int> key(1);
  key(0)=item;
  return range(key);
}

void NewMSRange::setBlockSize(Int blockSize)
{
  if (blockSize>0) blockSize_p=blockSize;
}

void NewMSRange::scalarRange(GlishRecord& out, const String& item, 
		 const ROScalarColumn<Int>& id, Bool oneBased) 
{
  Vector<Int> ids=scalarRange(id);
  if (oneBased) ids+=1;
  out.add(item,ids);
}

Vector<Int> NewMSRange::scalarRange(const ROScalarColumn<Int>& id)
{
  const Int option=Sort::HeapSort | Sort::NoDuplicates;
  const Sort::Order order=Sort::Ascending;
  Vector<Int> idvec=id.getColumn();
  Int n=GenSort<Int>::sort (idvec, order, option);
  Vector<Int> ids=idvec(Slice(0,n));
  return ids;
}

void NewMSRange::minMax(Float& mini, Float& maxi, 
		     Array<Float> (*func)(const Array<Complex>&),
		     const ROArrayColumn<Complex>& data)
{
  IPosition shp=data.shape(0);
  Int nrow=data.nrow();
  Int numrow=Int(blockSize_p*1.0e6/(sizeof(Complex)*shp(0)*shp(1)));
  for (Int start=0; start<nrow; start+=numrow) {
    Int n=min(numrow,nrow-start);
    Float minf, maxf;
    ::minMax(minf,maxf,func(data.getColumnRange(Slicer(Slice(start,n)))));
    if (start==0) {
      mini=minf; maxi=maxf;
    } else {
      mini=min(mini,minf);
      maxi=max(maxi,maxf);
    }
  }
}

Vector<Int> NewMSRange::ifrNumbers(const ROScalarColumn<Int>& ant1,
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
