//# MSFlagger.cc: selection and iteration of an MS
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
//#
//#
//# $Id$

#include <casacore/ms/MSOper/MSFlagger.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TiledDataStManAccessor.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/ms/MSSel/MSSelector.h>
#include <casacore/ms/MSSel/MSSelUtil.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSFlagger::MSFlagger():msSel_p(0)
{}

MSFlagger::MSFlagger(MSSelector& msSel):msSel_p(&msSel)
{}

MSFlagger& MSFlagger::operator=(const MSFlagger& other)
{
  if (this==&other) return *this;
  msSel_p=other.msSel_p;
  buffer_p=other.buffer_p;
  return *this;
}

MSFlagger::~MSFlagger() 
{
  msSel_p=0;
}

void MSFlagger::setMSSelector(MSSelector& msSel)
{
  msSel_p=&msSel;
  buffer_p=Record(RecordInterface::Variable);
}

Bool MSFlagger::fillDataBuffer(const String& item, Bool ifrAxis)
{
  LogIO os;
  if (!check()) return False;
  String itm=downcase(item);
  Int fld=MSS::field(itm);
  switch (fld) {
  case MSS::AMPLITUDE:
  case MSS::CORRECTED_AMPLITUDE:
  case MSS::MODEL_AMPLITUDE:
  case MSS::RESIDUAL_AMPLITUDE:
  case MSS::OBS_RESIDUAL_AMPLITUDE:
  case MSS::DATA:
  case MSS::CORRECTED_DATA:
  case MSS::MODEL_DATA:
  case MSS::RESIDUAL_DATA:
  case MSS::OBS_RESIDUAL_DATA:
  case MSS::IMAGINARY:
  case MSS::CORRECTED_IMAGINARY:
  case MSS::MODEL_IMAGINARY:
  case MSS::RESIDUAL_IMAGINARY:
  case MSS::OBS_RESIDUAL_IMAGINARY:
  case MSS::PHASE:
  case MSS::CORRECTED_PHASE:
  case MSS::MODEL_PHASE:
  case MSS::RESIDUAL_PHASE:
  case MSS::OBS_RESIDUAL_PHASE:
  case MSS::REAL:
  case MSS::CORRECTED_REAL:
  case MSS::MODEL_REAL:
  case MSS::RESIDUAL_REAL:
  case MSS::OBS_RESIDUAL_REAL:
    {
      Vector<String> items(3);
      items(0)=item;
      items(1)="FLAG";
      items(2)="FLAG_ROW";
      buffer_p=msSel_p->getData(items,ifrAxis);
      buffer_p.define("datafield",itm);
    }
    return True;
  default:
    os << LogIO::WARN <<"No DATA derived item specified, buffer unchanged"
       << LogIO::POST;
  }
  return False;
}

Record MSFlagger::diffDataBuffer(const String& direction, Int window,
				      Bool doMedian)
{
  Record retVal(RecordInterface::Variable);
  LogIO os;
  String dir=downcase(direction);
  if (dir!="time" && dir!="channel") {
    os << LogIO::WARN << "Unrecognized direction "<<direction<< 
      ", specify TIME or CHANNEL"<< LogIO::POST;
    return retVal;
  }
  Int win=max(1,window);
  if (win!=window) os <<LogIO::WARN<<"Setting window to "<<win<< LogIO::POST;
  if (doMedian) win=2*(win/2)+1; // make odd to keep it symmetric
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN<<"Buffer is empty, use fillbuffer first"
       << LogIO::POST;
    return retVal;
  }
  String item = buffer_p.asString(RecordFieldId("datafield"));
  Array<Bool> flag = buffer_p.asArrayBool(RecordFieldId("flag")); 
  Array<Bool> flagRow = buffer_p.asArrayBool(RecordFieldId("flag_row")); ;
  Int fld=MSS::field(item);
  Array<Float> diff;
  Int timeAxis=flag.ndim()-1;
  Int chanAxis=1;
  switch (fld) {
  case MSS::DATA:
  case MSS::CORRECTED_DATA:
  case MSS::MODEL_DATA:
  case MSS::RESIDUAL_DATA:
  case MSS::OBS_RESIDUAL_DATA:
    {
      Array<Complex> data = buffer_p.asArrayComplex(RecordFieldId(item));
      if (dir=="time") {
	diff=MSSelUtil<Complex>::diffData(data,flag,flagRow,timeAxis,win,
					  doMedian);
      }
      else {
	diff=MSSelUtil<Complex>::diffData(data,flag,flagRow,chanAxis,win,
					  doMedian);
      }
      // remove data field from record
      Record gr(RecordInterface::Variable);
      gr.define("flag",buffer_p.asArrayBool(RecordFieldId("flag")));
      gr.define("flag_row",buffer_p.asArrayBool(RecordFieldId("flag_row")));
      if (fld==MSS::DATA) item="amplitude";
      if (fld==MSS::CORRECTED_DATA) item="corrected_amplitude";
      if (fld==MSS::MODEL_DATA) item="model_amplitude";
      if (fld==MSS::RESIDUAL_DATA) item="residual_amplitude";
      if (fld==MSS::OBS_RESIDUAL_DATA) item="obs_residual_amplitude";
      gr.define("datafield",item);
      buffer_p=gr;
    }
    break;
  case MSS::AMPLITUDE:
  case MSS::CORRECTED_AMPLITUDE:
  case MSS::MODEL_AMPLITUDE:
  case MSS::RESIDUAL_AMPLITUDE:
  case MSS::OBS_RESIDUAL_AMPLITUDE:
  case MSS::IMAGINARY:
  case MSS::CORRECTED_IMAGINARY:
  case MSS::MODEL_IMAGINARY:
  case MSS::RESIDUAL_IMAGINARY:
  case MSS::OBS_RESIDUAL_IMAGINARY:
  case MSS::PHASE:
  case MSS::CORRECTED_PHASE:
  case MSS::MODEL_PHASE:
  case MSS::RESIDUAL_PHASE:
  case MSS::OBS_RESIDUAL_PHASE:
  case MSS::REAL:
  case MSS::CORRECTED_REAL:
  case MSS::MODEL_REAL:
  case MSS::RESIDUAL_REAL:
  case MSS::OBS_RESIDUAL_REAL:
    {
      Array<Float> data = buffer_p.asArrayFloat(RecordFieldId(item));
      if (dir=="time") {
	diff=MSSelUtil<Float>::diffData(data,flag,flagRow,timeAxis,win,
					doMedian);
      }
      else {
	diff=MSSelUtil<Float>::diffData(data,flag,flagRow,chanAxis,win,
					doMedian);
      }
    }
    break;
  default:
    break;
  }
  buffer_p.define(item,diff);
  applyRowFlags(flag,flagRow); // need to apply row flags to flags for stats
  addStats(buffer_p,flag,flagRow,diff);
  retVal.define("median",buffer_p.asArrayFloat(RecordFieldId("medTF")));
  retVal.define("aad",buffer_p.asArrayFloat(RecordFieldId("adTF")));
  return retVal;
}
  
void MSFlagger::addStats(Record& buf, const Array<Bool>& flag,
			 const Array<Bool> flagRow, const Array<Float>& data)
{
  // axes PFIT (Polarization, Freq, Interferometer, Time)
  // take median along T and F axes (medT, medF) 
  // calculate median of medians along F and T (medTmedF, medFmedT) to
  // find outlying times and channels, estimate medTF as minumum of latter 2.
  // calculate average absolute deviations over T and F medians, and TF planes
  // (adT, adF and adTF).
  Array<Float> medT, medF, medTmedF, medFmedT, medTF, adT, adF, adTF;
  getStats(medTF, adTF, medT, medFmedT, adT,
	   medF, medTmedF, adF, data, flag, flagRow);
  buf.define("medTF",medTF);
  buf.define("adTF",adTF);
  buf.define("medT",medT);
  buf.define("medFmedT",medFmedT);
  buf.define("adT",adT);
  buf.define("medF",medF);
  buf.define("medTmedF",medTmedF);
  buf.define("adF",adF);
}

void MSFlagger::applyRowFlags(Array<Bool>& flag, Array<Bool>& flagRow)
{
  const Int nXY=flag.shape()(0)*flag.shape()(1);
  Bool deleteFlag, deleteFlagRow;
  Bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  Bool* pflag = flag.getStorage(deleteFlag);
  const Int nEl=flagRow.nelements();
  DebugAssert(nEl*nXY==Int(flag.nelements()),AipsError);
  Int offset=0;
  for (Int i=0; i<nEl; i++, offset+=nXY) {
    if (pflagRow[i]) {
      for (Int j=0; j<nXY; j++) pflag[offset+j]=True;
    } else {
      Bool ok=False;
      for (Int j=0; j<nXY && (ok=pflag[offset+j]); j++) {}
      if (ok) pflagRow[i]=True;
    }
  }
  flag.putStorage(pflag,deleteFlag);
  flagRow.putStorage(pflagRow,deleteFlagRow);
}

void MSFlagger::getStats(Array<Float>& medTF, Array<Float>& adTF, 
			  Array<Float>& medT, Array<Float>& medFmedT, 
			  Array<Float>& adT, Array<Float>& medF, 
			  Array<Float>& medTmedF, Array<Float>& adF,
			  const Array<Float>& diff, const Array<Bool>& flag,
			  const Array<Bool>& flagRow)
{
  IPosition shape=diff.shape();
  const Int nCorr=shape(0);
  const Int nChan=shape(1);
  Int nTime=shape(2);
  Int nIfr=1;
  const Int nXY=nCorr*nChan;
  Array<Float> diff2(diff);
  if (diff.ndim()==3) {
    // make 4D reference to diff's storage so diffMedian will return
    // correct shapes
    Array<Float> ref(diff2.reform(IPosition(4,nCorr,nChan,nIfr,nTime)));
    diff2.reference(ref);
  } else {
    nIfr=nTime;
    nTime=shape(3);
  }
  const Int nXYZ=nXY*nIfr;
  Bool deleteFlag, deleteFlagRow, deleteDiff;
  const Bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  const Bool* pflag = flag.getStorage(deleteFlag);
  const Float* pdiff = diff2.getStorage(deleteDiff);
 
  medTF.resize(IPosition(2,nCorr,nIfr));
  adTF.resize(IPosition(2,nCorr,nIfr));
  medT.resize(IPosition(3,nCorr,nChan,nIfr));
  medFmedT.resize(IPosition(2,nCorr,nIfr));
  adT.resize(IPosition(2,nCorr,nIfr));
  medF.resize(IPosition(3,nCorr,nIfr,nTime));
  medTmedF.resize(IPosition(2,nCorr,nIfr));
  adF.resize(IPosition(2,nCorr,nIfr));

  // calculate medians over time
  diffMedian(medT,diff2,3,flag);
  

  // calculate median over channel of median over time
  diffMedian(medFmedT,medT,1, (medT<=0.0f));

  // calculate median over channel
  diffMedian(medF,diff2,1, flag);

  // calculate median over time of median over channel
  diffMedian(medTmedF,medF,2, (medF<=0.0f));

  // make a guess at the overal median (per pol and ifr)
  min(medTF,medTmedF,medFmedT);

  // calculate average absolute deviation of medians over time
  {
    Bool deletemedT;
    const Float* pmedT=medT.getStorage(deletemedT);
    Int offset=0;
    IPosition polifr(2);
    for (Int pol=0; pol<nCorr; pol++) {
      polifr(0)=pol;
      offset=pol;
      for (Int ifr=0; ifr<nIfr; ifr++, offset+=nXY) {
	polifr(1)=ifr;
	Float ad=0, med=medFmedT(polifr);
	Int count=0, offchan=offset;
	for (Int i=0; i<nChan; i++, offchan+=nCorr) {
	  if (pmedT[offchan]>0) {
	    count++;
	    ad+=abs(pmedT[offchan]-med);
	  }
	}
	if (count>1) ad/=count;
	adT(polifr)=ad;
      }
    }
    medT.freeStorage(pmedT,deletemedT);
  }

  // calculate average absolute deviation of medians over channel
  {
    Bool deletemedF;
    const Float* pmedF=medF.getStorage(deletemedF);
    Int offset=0, nXZ=nCorr*nIfr;
    IPosition polifr(2);
    for (Int pol=0; pol<nCorr; pol++) {
      polifr(0)=pol;
      offset=pol;
      for (Int ifr=0; ifr<nIfr; ifr++, offset+=nCorr) {
	polifr(1)=ifr;
	Float ad=0, med=medTmedF(polifr);
	Int count=0, offtime=offset, offrow=ifr;
	for (Int i=0; i<nTime; i++, offtime+=nXZ, offrow+=nIfr) {
	  if (!pflagRow[offrow]) {
	    count++;
	    ad+=abs(pmedF[offtime]-med);
	  }
	}
	if (count>1) ad/=count;
	adF(polifr)=ad;
      }
    }
    medF.freeStorage(pmedF,deletemedF);
  }

  // calculate overall average deviation (per pol and ifr)
  {
    IPosition polifr(2);
    for (Int pol=0; pol<nCorr; pol++) {
      polifr(0)=pol;
      Int offset=pol;
      for (Int ifr=0; ifr<nIfr; ifr++, offset+=nXY) {  
	polifr(1)=ifr;
	Float ad=0, med=medTF(polifr);
	Int count=0, offset2=offset, offrow=ifr;
	for (Int i=0; i<nTime; i++, offset2+=nXYZ, offrow+=nIfr) {
	  if (!pflagRow[offrow]) {
	    for (Int j=0, offset3=offset2; j<nChan; j++,offset3+=nCorr) {
	      if (!pflag[offset3]) {
		count++;
		ad+=abs(pdiff[offset3]-med);
	      }
	    }
	  }
	}
	if (count>1) ad/=count;
	adTF(polifr)=ad;
      }
    }
  }


  flag.freeStorage(pflag,deleteFlag);
  flagRow.freeStorage(pflagRow,deleteFlagRow);
  diff2.freeStorage(pdiff,deleteDiff);
}

void MSFlagger::diffMedian(Array<Float>& out, const Array<Float>& in, 
			    Int axis, const Array<Bool>& flag)
{
  // collapse array "in" (with absolute differences) 
  // along specified axis by taking medians by profile taking into account
  // the flags.
  Int nDim=in.ndim();
  DebugAssert(axis>=0 && axis<nDim && in.ndim()>0, AipsError);
  IPosition inShape=in.shape(), outShape(max(1,Int(in.ndim())-1));
  outShape(0)=1; // cope with 1-d input
  Int nLess=1, nGreater=1, nAxis=inShape(axis);
  for (Int i=0, count=0; i<nDim; i++) {
    if (i!=axis) outShape(count++)=inShape(i);
    if (i<axis) nLess*=inShape(i);
    if (i>axis) nGreater*=inShape(i);
  }
  out.resize(outShape);

  Bool deleteIn, deleteFlag, deleteOut;
  const Float* pin=in.getStorage(deleteIn);
  const Bool* pflag=flag.getStorage(deleteFlag);
  Float* pout=out.getStorage(deleteOut);
  Block<Float> values(nAxis);
  for (Int j=0, offj=0; j<nGreater; j++, offj+=nLess) {
    for (Int k=0, offk=offj*nAxis, offout=offj; k<nLess; 
	 k++, offk++, offout++) {
      Int count=0;
      for (Int l=0, offin=offk; l<nAxis; l++, offin+=nLess) {
	if (!pflag[offin]) values[count++]=pin[offin];
      }
      if (count>0) pout[offout]=median(Vector<Float>(values,count));
      else pout[offout]=0;
    }
  }
  in.freeStorage(pin,deleteIn);
  flag.freeStorage(pflag,deleteFlag);
  out.putStorage(pout,deleteOut);
}

inline String multiple(Int n) { return n!=1 ? "s" : ""; }

Bool MSFlagger::clipDataBuffer(Float pixelLevel, Float timeLevel, 
				Float channelLevel)
{
  LogIO os;
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN << "No data loaded into buffer yet"<<
      ", use fillbuffer first"<< LogIO::POST;
    return False;
  }
  String item = buffer_p.asString(RecordFieldId("datafield"));
  if (item.contains("data")) {
    os << LogIO::WARN << "Can't clip complex data,"<<
       " use diffbuffer first or load a derived quantity"<< LogIO::POST;
    return False;
  }

  // retrieve the data
  Array<Bool> flag = buffer_p.asArrayBool(RecordFieldId("flag"));
  Array<Bool> flagRow = buffer_p.asArrayBool(RecordFieldId("flag_row"));
  Array<Float> diff = buffer_p.asArrayFloat(RecordFieldId(item));

  // retrieve the stats
  Matrix<Float> adT, adF, medTF, adTF, medFmedT, medTmedF;
  Cube<Float> medT, medF;
  if (!buffer_p.isDefined("medTF")) {
    // we haven't got stats yet
    applyRowFlags(flag,flagRow); // need to apply row flags to flags for stats
    addStats(buffer_p,flag,flagRow,diff);
  }
  medTF = buffer_p.asArrayFloat("medTF");
  adTF = buffer_p.asArrayFloat("adTF");
  medT = buffer_p.asArrayFloat("medT");
  medFmedT = buffer_p.asArrayFloat("medFmedT");
  adT = buffer_p.asArrayFloat("adT");
  medF = buffer_p.asArrayFloat("medF");
  medTmedF = buffer_p.asArrayFloat("medTmedF");
  adF = buffer_p.asArrayFloat("adF");
/*
  GlishArray(buffer_p.get("adTF")).get(adTF);
  GlishArray(buffer_p.get("medT")).get(medT);
  GlishArray(buffer_p.get("medFmedT")).get(medFmedT);
  GlishArray(buffer_p.get("adT")).get(adT);
  GlishArray(buffer_p.get("medF")).get(medF);
  GlishArray(buffer_p.get("medTmedF")).get(medTmedF);
  GlishArray(buffer_p.get("adF")).get(adF);
*/
  Bool deleteFlag, deleteFlagRow, deleteDiff;
  Bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  Bool* pflag = flag.getStorage(deleteFlag);
  const Float* pdiff = diff.getStorage(deleteDiff);
  const Int nCorr=flag.shape()(0);
  const Int nChan=flag.shape()(1);
  Int nTime=flag.shape()(2);
  const Int nXY=nCorr*nChan;
  Int nIfr=1;
  if (flag.ndim()==4) {
    nIfr=nTime;
    nTime=flag.shape()(3);
  }
  const Int nXYZ=nXY*nIfr;
   
  // iterate till no more pixels are flagged
  Bool iter=True;
  Matrix<Int> sum(nCorr,nIfr),sumChan(nCorr,nIfr),sumTime(nCorr,nIfr);
  sum=0, sumChan=0, sumTime=0;
  while (iter) {
    iter=False;

    for (Int ifr=0, offset=0; ifr<nIfr; ifr++, offset=ifr*nXY) {
      for (Int pol=0; pol<nCorr; pol++, offset++) {
	
	// keep these values around
	Float mfmt = medFmedT(pol,ifr);
	Float adt  = adT(pol,ifr);
	Float mtmf = medTmedF(pol,ifr);
	Float adf  = adF(pol,ifr);
	Float mtf  = medTF(pol,ifr);
	Float adtf = adTF(pol,ifr);

	Int chanCount=0, timeCount=0, count=0;
	// flag bad channels
	{
	  for (Int i=0, offset2=offset; i<nChan; i++, offset2+=nCorr) {
	    Float mt=medT(pol,i,ifr);
	    if ( (mt>0) && (abs(mt-mfmt) > channelLevel*adt)) {
	      chanCount++;
	      for (Int j=0, offset3=offset2; j<nTime; j++, offset3+=nXYZ) {
		  pflag[offset3]=True;
	      }
	    }
	  }
	}
	// flag bad times
	{
	  Int offrow=ifr;
	  for (Int i=0, offset2=offset; i<nTime; 
	       i++,offset2+=nXYZ,offrow+=nIfr) {
	    if (!pflagRow[offrow]) {
	      Float mf=medF(pol,ifr,i);
	      if (mf>0 && abs(mf-mtmf) > timeLevel*adf) {
		timeCount++;
		for (Int j=0, offset3=offset2; j<nChan; j++, offset3+=nCorr) {
		  pflag[offset3]=True;
		}
	      }
	    }
	  }
	}
	// flag bad pixels
	{
	  Int offrow=ifr;
	  for (Int i=0, offset2=offset; i<nTime;
	       i++, offset2+=nXYZ, offrow+=nIfr) {
	    if (!pflagRow[offrow]) {
	      for (Int j=0, offset3=offset2; j<nChan; j++, offset3+=nCorr) {
		if (!pflag[offset3] && 
		    abs(pdiff[offset3]-mtf) > pixelLevel*adtf) {
		  pflag[offset3]=True;
		  count++;
		}
	      }
	    }
	  }
	}
	iter= (iter || chanCount>0 || timeCount>0 ||count>0);
	sumChan(pol,ifr)+=chanCount;
	sumTime(pol,ifr)+=timeCount;
	sum(pol,ifr)+=count;
      }
    }
    if (iter) {
      if (deleteFlag||deleteFlagRow) {
	cerr << " arrays have to be written back "<<endl;
	flag.putStorage(pflag,deleteFlag);
	flagRow.putStorage(pflagRow,deleteFlagRow);
      }
      applyRowFlags(flag,flagRow); //need to apply row flags to flags for stats
      getStats(medTF,adTF,medT,medFmedT,adT,medF,medTmedF,adF,
	       diff, flag, flagRow);
      if (deleteFlag||deleteFlagRow) {
	cerr << " arrays have to be read back "<<endl;
	pflag=flag.getStorage(deleteFlag);
	pflagRow=flagRow.getStorage(deleteFlagRow);
      }
    }
  }

  for (Int ifr=0; ifr<nIfr; ifr++) {
    for (Int pol=0; pol<nCorr; pol++) {
      if ((sumChan(pol,ifr)>0 || sumTime(pol,ifr)>0 || sum(pol,ifr)>0)) {
	if (nIfr>1) {
	  os << LogIO::NORMAL << "Polarization# = "<< pol+1 << 
	    ", Interferometer# = "<< ifr+1 << LogIO::POST;
	} else if (nCorr>1) {
	  os << LogIO::NORMAL << "Polarization# = "<< pol+1 << LogIO::POST;
	}
      }
      if (sumChan(pol,ifr)>0) {
	os << LogIO::NORMAL << "Flagged "<<sumChan(pol,ifr)<<" channel"<<
	  multiple(sumChan(pol,ifr))<<
	  " with abs(median - " << medTmedF(pol,ifr) << ") > "<<channelLevel
	   << "*" << adF(pol,ifr) <<LogIO::POST;
      }
      if (sumTime(pol,ifr)>0) {
	os << LogIO::NORMAL << "Flagged "<<sumTime(pol,ifr)<<" time"<<
	  multiple(sumTime(pol,ifr))<<
	  " with abs(median - " << medFmedT(pol,ifr) << ") > "<< timeLevel 
	   << "*" <<adT(pol,ifr)<<LogIO::POST;
      }
      if (sum(pol,ifr)>0) {
	os << LogIO::NORMAL << "Flagged "<<sum(pol,ifr)<<" pixel"<<
	  multiple(sum(pol,ifr))<<
	  " with abs(pixval - "<< medTF(pol,ifr) << ") > "<< pixelLevel << 
	  "*" << adTF(pol,ifr)<<LogIO::POST;
      }
    }
  }
  flag.putStorage(pflag,deleteFlag);
  flagRow.putStorage(pflagRow,deleteFlagRow);
  diff.freeStorage(pdiff,deleteDiff);
  buffer_p.define("flag",flag);
  buffer_p.define("flag_row",flagRow);
  buffer_p.define("medTF",medTF);
  buffer_p.define("adTF",adTF);
  buffer_p.define("medT",medT);
  buffer_p.define("medF",medF);
  buffer_p.define("adT",adT);
  buffer_p.define("adF",adF);
  buffer_p.define("medTmedF",medTmedF);
  buffer_p.define("medFmedT",medFmedT);
  return True;
}

Bool MSFlagger::setDataBufferFlags(const Record& flags)
{
  LogIO os;
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN <<
      "Data buffer is empty, use filldatabuffer first"<< LogIO::POST;
    return False;
  }
  buffer_p.define("flag",flags.asArrayBool(RecordFieldId("flag")));
  buffer_p.define("flag_row",flags.asArrayBool(RecordFieldId("flag_row")));
  return True;
}

Bool MSFlagger::writeDataBufferFlags()
{
  LogIO os;
  if (!check()) return False;
  if (!msSel_p->selectedTable().isWritable()) {
    os << LogIO::SEVERE << "MeasurementSet is not writable"<< LogIO::POST;
    return False;
  }
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN <<
      "Data buffer is empty, use filldatabuffer first"<< LogIO::POST;
    return False;
  }
  Record items(RecordInterface::Variable);
  items.define("flag_row",buffer_p.asArrayBool(RecordFieldId("flag_row")));
  items.define("flag",buffer_p.asArrayBool(RecordFieldId("flag")));
  return msSel_p->putData(items);
}

Bool MSFlagger::createFlagHistory(Int nHis)
{
  LogIO os;
  if (!check()) return False;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isWritable()) {
    os << LogIO::WARN << "MS is not writable"<< LogIO::POST;
    return False;
  }
  if (nHis<2 || nHis>16) {
    os << LogIO::WARN << "Invalid argument: 2<=nHis<=16 "<< LogIO::POST;
    return False;
  } 
  if (tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column already exists"<<LogIO::POST;
    return False;
  }
  // Look for the FLAG column among the hypercolumns
  String flagHypercubeId="";
  Bool found=findHypercubeId(flagHypercubeId,MS::columnName(MS::FLAG),tab);
 
  Vector<String> coordColNames(0), idColNames(1);
  TableDesc td1;
  if (!found) {
    // If there's no id, assume the data is fixed shape throughout
    ROArrayColumn<Bool> flagCol(tab,MS::columnName(MS::FLAG));
    Int numCorr=flagCol.shape(0)(0);
    Int numChan=flagCol.shape(0)(1);
    IPosition shape(3,nHis,numCorr,numChan);
    idColNames.resize(0);
    td1.addColumn(ArrayColumnDesc<Bool>("FLAG_CATEGORY","flag history",shape,
					ColumnDesc::Direct));
    td1.defineHypercolumn("TiledFlagHistory",4,
			  stringToVector("FLAG_CATEGORY"),coordColNames,
			  idColNames);
    // fixed data shape
    Int tileSize=numChan/10+1;
    IPosition tileShape(4,1,numCorr,tileSize,16384/numCorr/tileSize);
    TiledColumnStMan tiledStMan1("TiledFlagHistory",tileShape);
    tab.addColumn(td1,tiledStMan1);
    fillFlagHist(nHis,numCorr,numChan,tab);
  } else {
    {
      ROArrayColumn<Bool> flagCol(tab,MS::columnName(MS::FLAG));
      idColNames(0)="FLAG_CATEGORY_HYPERCUBE_ID"; 
      td1.addColumn(ArrayColumnDesc<Bool>("FLAG_CATEGORY","flag history",3));
      td1.addColumn(ScalarColumnDesc<Int>("FLAG_CATEGORY_HYPERCUBE_ID",
					  "hypercube index"));
      td1.defineHypercolumn("TiledFlagHistory",4,
			    stringToVector("FLAG_CATEGORY"),coordColNames,
			    idColNames);
      // data shape may change
      TiledDataStMan tiledStMan1("TiledFlagCategory");
      tab.addColumn(td1,tiledStMan1);
      TiledDataStManAccessor flagCatAccessor(tab,"TiledFlagCategory");

      // get the hypercube ids, sort them, remove the duplicate values
      ROScalarColumn<Int> hypercubeId(tab,flagHypercubeId);
      Vector<Int> ids=hypercubeId.getColumn();
      Int nId=genSort(ids,Sort::Ascending,Sort::QuickSort+Sort::NoDuplicates);
      ids.resize(nId,True); // resize and copy values
      Vector<Bool> cubeAdded(nId,False);
      Record values1; 
      values1.define("FLAG_CATEGORY_HYPERCUBE_ID",hypercubeId(0));
      Int cube;
      for (cube=0; cube<nId; cube++) if (ids(cube)==hypercubeId(0)) break;
      Int nRow=tab.nrow();
      for (Int i=0; i<nRow; i++) {
	// add new hyperCube
	if (i>0 && hypercubeId(i)!=hypercubeId(i-1)) {
	  values1.define("FLAG_CATEGORY_HYPERCUBE_ID",hypercubeId(i));
	  for (cube=0; cube<nId; cube++) if (ids(cube)==hypercubeId(i)) break;
	}
	if (!cubeAdded(cube)) {
	  cubeAdded(cube)=True;
	  Int numCorr=flagCol.shape(i)(0);
	  Int numChan=flagCol.shape(i)(1);
	  Int tileSize=numChan/10+1;
	  IPosition cubeShape(4,nHis,numCorr,numChan,0);
	  IPosition tileShape(4,1,numCorr,tileSize,16384/numCorr/tileSize);
	  flagCatAccessor.addHypercube(cubeShape,tileShape,values1);
	}
	flagCatAccessor.extendHypercube(1,values1);
      }
    }

    TableIterator obsIter(tab,flagHypercubeId);
    for (;!obsIter.pastEnd(); obsIter.next()) {
      ROArrayColumn<Bool> flagCol(obsIter.table(),MS::columnName(MS::FLAG));
      Int numCorr=flagCol.shape(0)(0);
      Int numChan=flagCol.shape(0)(1);
      Table tab=obsIter.table();
      fillFlagHist(nHis,numCorr,numChan,tab);
    }
  }    
  return True;
}

Bool MSFlagger::findHypercubeId(String& hypercubeId, const String& column,
			    const Table& tab)
{
  // to find the corresponding id column (if any)
  TableDesc td(tab.tableDesc());
  Vector<String> hypercolumnNames=td.hypercolumnNames();
  Bool found=False;
  hypercubeId="";
  if (hypercolumnNames.nelements()>0) {
    for (uInt i=0; i<hypercolumnNames.nelements(); i++) {
      Vector<String> colNames,coordColNames,idColNames;
      td.hypercolumnDesc(hypercolumnNames(i),
			 colNames,coordColNames,
			 idColNames);
      for (uInt j=0; j<colNames.nelements(); j++) {
	if (colNames(j)==column) {
	  found=(idColNames.nelements()>0);
	  if (found) hypercubeId=idColNames(0);
	}
      }
    }
  }
  return found;
}

void MSFlagger::fillFlagHist(Int nHis, Int numCorr, Int numChan, Table& tab)
{
  // fill the first two levels of flagging with the flags present 
  // in the MS columns FLAG and FLAG_ROW.
  const Int maxRow=1000000/(numCorr*numChan); // of order 1 MB chunks
  ROArrayColumn<Bool> flagCol(tab,MS::columnName(MS::FLAG));
  ArrayColumn<Bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  Array<Bool> flagHis(IPosition(4,nHis,numCorr,numChan,maxRow));
  // flag level 0
  Cube<Bool> ref0(flagHis(IPosition(4,0,0,0,0),
			  IPosition(4,0,numCorr-1,numChan-1,maxRow-1)).
			  reform(IPosition(3,numCorr,numChan,maxRow)));
  // flag level 1
  Cube<Bool> ref1(flagHis(IPosition(4,1,0,0,0),
			  IPosition(4,1,numCorr-1,numChan-1,maxRow-1)).
			  reform(IPosition(3,numCorr,numChan,maxRow)));
  flagHis.set(False);
  Int nRow=tab.nrow();
  ROScalarColumn<Bool> flagRowCol(tab,MS::columnName(MS::FLAG_ROW));
  Array<Bool> flagCube;
  Vector<Bool> flagRowVec;
  for (Int i=0; i<=(nRow/maxRow); i+=maxRow) {
    Int n=min(maxRow,nRow-maxRow*i);
    if (n<maxRow) {
      flagHis.resize(IPosition(4,nHis,numCorr,numChan,n));
      flagHis.set(False);
      Array<Bool> tmp0(flagHis(IPosition(4,0,0,0,0),
			      IPosition(4,0,numCorr-1,numChan-1,n-1)).
		      reform(IPosition(3,numCorr,numChan,n)));
      ref0.reference(tmp0);
      Array<Bool> tmp1(flagHis(IPosition(4,1,0,0,0),
			       IPosition(4,1,numCorr-1,numChan-1,n-1)).
		       reform(IPosition(3,numCorr,numChan,n)));
      ref1.reference(tmp1);
    }
    Slicer rowSlice(Slice(i*maxRow,n));
    flagRowCol.getColumnRange(rowSlice,flagRowVec,True);
    flagCol.getColumnRange(rowSlice,flagCube,True);
    ref0=flagCube;
    for (Int j=0; j<n; j++) {
      if (flagRowVec(j)) {
	ref0.xyPlane(j).set(True);
      }
    }
    ref1=ref0;
    flagHisCol.putColumnRange(rowSlice,flagHis);
  }
  // Set the FLAG_LEVEL keyword to 1, to indicate we will be 
  // using these flags (level 0 flags are those already present)
  flagHisCol.rwKeywordSet().define("FLAG_LEVEL",1);
}

Bool MSFlagger::saveFlags(Bool newLevel)
{
  LogIO os;
  if (!check()) return False;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column does not exist"<<LogIO::POST;
    return False;
  }
  if (!tab.isWritable()) {
    os << LogIO::WARN << "MS is not writable"<< LogIO::POST;
    return False;
  }
  ArrayColumn<Bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  Int level;
  flagHisCol.keywordSet().get("FLAG_LEVEL",level);
  if (newLevel) {
    if (level+1>=flagHisCol.shape(0)(0)) {
      os << LogIO::WARN << "No space for new flag level ("<<(level+1)+1<<") in "
	 << "FLAG_CATEGORY column, using current level instead"<<LogIO::POST;
    } else {
      level++;
    }
  } 
  
  String hypercubeId;
  Bool found=findHypercubeId(hypercubeId,MS::columnName(MS::FLAG_CATEGORY),tab);
  if (!found) {
    // data has fixed shape
    saveToFlagHist(level,tab);
  } else {
    // data changes shape, iterate
    TableIterator tabIter(tab,hypercubeId);
    for (; !tabIter.pastEnd(); tabIter++) {
      Table tab1=tabIter.table();
      saveToFlagHist(level,tab1);
    }
  }
  if (newLevel) flagHisCol.rwKeywordSet().define("FLAG_LEVEL",level);
  return True;
}

void MSFlagger::saveToFlagHist(Int level, Table& tab)
{
  ROArrayColumn<Bool> flagCol(tab,MS::columnName(MS::FLAG));
  Int numCorr=flagCol.shape(0)(0);
  Int numChan=flagCol.shape(0)(1);
  const Int maxRow=1000000/(numCorr*numChan); // of order 1 MB chunks
  Array<Bool> flagHis(IPosition(4,1,numCorr,numChan,maxRow));
  Cube<Bool> ref(flagHis.reform(IPosition(3,numCorr,numChan,maxRow)));
  Int nRow=tab.nrow();
  Array<Bool> flagCube;
  Vector<Bool> flagRowVec;
  Slicer slicer(Slice(level,1),Slice(0,numCorr),Slice(0,numChan));
  for (Int i=0; i<=(nRow/maxRow); i+=maxRow) {
    Int n=min(maxRow,nRow-maxRow*i);
    if (n<maxRow) {
      flagHis.resize(IPosition(4,1,numCorr,numChan,n));
      Array<Bool> tmp(flagHis.reform(IPosition(3,numCorr,numChan,n)));
      ref.reference(tmp);
    }
    Vector<uInt> rows(n);
    indgen(rows,uInt(i*maxRow));
    Table sel=tab(rows);
    ArrayColumn<Bool> flagHisCol(sel,MS::columnName(MS::FLAG_CATEGORY));
    ROArrayColumn<Bool> flagCol(sel,MS::columnName(MS::FLAG));
    ROScalarColumn<Bool> flagRowCol(sel,MS::columnName(MS::FLAG_ROW));
    flagCol.getColumn(flagCube,True);
    flagRowCol.getColumn(flagRowVec,True);
    ref=flagCube;
    for (Int j=0; j<n; j++) {
      if (flagRowVec(j)) {
	ref.xyPlane(j).set(True);
      }
    }
    flagHisCol.putColumn(slicer,flagHis);
  }
}

Bool MSFlagger::restoreFlags(Int level)
{
  LogIO os;
  if (!check()) return False;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column does not exist"<<LogIO::POST;
    return False;
  }
  if (!tab.isWritable()) {
    os << LogIO::WARN << "MS is not writable"<< LogIO::POST;
    return False;
  }
  ArrayColumn<Bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  Int flagLevel=level;
  if (flagLevel==-1) flagHisCol.keywordSet().get("FLAG_LEVEL",flagLevel);
  if (flagLevel<0 || flagLevel>=flagHisCol.shape(0)(0)) {
    os << LogIO::WARN << "Invalid flag level ("<<flagLevel+1<<")"<<LogIO::POST;
    return False;
  }
  String hypercubeId;
  Bool found=findHypercubeId(hypercubeId,MS::columnName(MS::FLAG_CATEGORY),tab);
  if (!found) {
    // data has fixed shape
    applyFlagHist(flagLevel,tab);
  } else {
    // data changes shape, iterate
    TableIterator tabIter(tab,hypercubeId);
    for (; !tabIter.pastEnd(); tabIter++) {
      Table tab=tabIter.table();
      applyFlagHist(flagLevel,tab);
    }
  }
  if (level!=-1) flagHisCol.rwKeywordSet().define("FLAG_LEVEL",level);
  return True;
}

void MSFlagger::applyFlagHist(Int level, Table& tab)
{
  Int nRow=tab.nrow();
  ROArrayColumn<Bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  IPosition shape=flagHisCol.shape(0); shape(0)=1;
  const Int maxRow=1000000/(shape(1)*shape(2)); // of order 1 MB chunks
  Slicer slicer(Slice(level,1),Slice(0,shape(1)),Slice(0,shape(2)));
  for (Int i=0; i<=nRow/maxRow; i++) {
    Int n=min(maxRow,nRow-i*maxRow);
    Vector<uInt> rows(n);
    indgen(rows,uInt(i*maxRow));
    Table sel=tab(rows);
    ROArrayColumn<Bool> flagHisCol(sel,MS::columnName(MS::FLAG_CATEGORY));
    Cube<Bool> flag(flagHisCol.getColumn(slicer).
      reform(IPosition(3,shape(1),shape(2),n)));
    ArrayColumn<Bool> flagCol(sel,MS::columnName(MS::FLAG));
    ScalarColumn<Bool> flagRowCol(sel,MS::columnName(MS::FLAG_ROW));
    flagCol.putColumn(flag);
    for (Int j=0; j<n; j++) {
      if (allEQ(flag.xyPlane(j),True)) {
	flagRowCol.put(j,True);
      } else {
	flagRowCol.put(j,False);
      }
    }
  }
}

Int MSFlagger::flagLevel()
{
  LogIO os;
  if (!check()) return False;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column does not exist"<<LogIO::POST;
    return -1;
  }
  ROArrayColumn<Bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  Int flagLevel;
  flagHisCol.keywordSet().get("FLAG_LEVEL",flagLevel);
  return flagLevel;
}
  
Bool MSFlagger::check() 
{
  LogIO os;
  if (msSel_p) return True;
  os << LogIO::WARN << "Flagger is uninitialized"<<LogIO::POST;
  return False;
}




} //# NAMESPACE CASACORE - END

