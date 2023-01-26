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

bool MSFlagger::fillDataBuffer(const String& item, bool ifrAxis)
{
  LogIO os;
  if (!check()) return false;
  String itm=downcase(item);
  int32_t fld=MSS::field(itm);
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
    return true;
  default:
    os << LogIO::WARN <<"No DATA derived item specified, buffer unchanged"
       << LogIO::POST;
  }
  return false;
}

Record MSFlagger::diffDataBuffer(const String& direction, int32_t window,
				      bool doMedian)
{
  Record retVal(RecordInterface::Variable);
  LogIO os;
  String dir=downcase(direction);
  if (dir!="time" && dir!="channel") {
    os << LogIO::WARN << "Unrecognized direction "<<direction<< 
      ", specify TIME or CHANNEL"<< LogIO::POST;
    return retVal;
  }
  int32_t win=max(1,window);
  if (win!=window) os <<LogIO::WARN<<"Setting window to "<<win<< LogIO::POST;
  if (doMedian) win=2*(win/2)+1; // make odd to keep it symmetric
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN<<"Buffer is empty, use fillbuffer first"
       << LogIO::POST;
    return retVal;
  }
  String item = buffer_p.asString(RecordFieldId("datafield"));
  Array<bool> flag = buffer_p.asArrayBool(RecordFieldId("flag")); 
  Array<bool> flagRow = buffer_p.asArrayBool(RecordFieldId("flag_row")); ;
  int32_t fld=MSS::field(item);
  Array<float> diff;
  int32_t timeAxis=flag.ndim()-1;
  int32_t chanAxis=1;
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
      Array<float> data = buffer_p.asArrayFloat(RecordFieldId(item));
      if (dir=="time") {
	diff=MSSelUtil<float>::diffData(data,flag,flagRow,timeAxis,win,
					doMedian);
      }
      else {
	diff=MSSelUtil<float>::diffData(data,flag,flagRow,chanAxis,win,
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
  
void MSFlagger::addStats(Record& buf, const Array<bool>& flag,
			 const Array<bool> flagRow, const Array<float>& data)
{
  // axes PFIT (Polarization, Freq, Interferometer, Time)
  // take median along T and F axes (medT, medF) 
  // calculate median of medians along F and T (medTmedF, medFmedT) to
  // find outlying times and channels, estimate medTF as minumum of latter 2.
  // calculate average absolute deviations over T and F medians, and TF planes
  // (adT, adF and adTF).
  Array<float> medT, medF, medTmedF, medFmedT, medTF, adT, adF, adTF;
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

void MSFlagger::applyRowFlags(Array<bool>& flag, Array<bool>& flagRow)
{
  const int32_t nXY=flag.shape()(0)*flag.shape()(1);
  bool deleteFlag, deleteFlagRow;
  bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  bool* pflag = flag.getStorage(deleteFlag);
  const int32_t nEl=flagRow.nelements();
  DebugAssert(nEl*nXY==int32_t(flag.nelements()),AipsError);
  int32_t offset=0;
  for (int32_t i=0; i<nEl; i++, offset+=nXY) {
    if (pflagRow[i]) {
      for (int32_t j=0; j<nXY; j++) pflag[offset+j]=true;
    } else {
      bool ok=false;
      for (int32_t j=0; j<nXY && (ok=pflag[offset+j]); j++) {}
      if (ok) pflagRow[i]=true;
    }
  }
  flag.putStorage(pflag,deleteFlag);
  flagRow.putStorage(pflagRow,deleteFlagRow);
}

void MSFlagger::getStats(Array<float>& medTF, Array<float>& adTF, 
			  Array<float>& medT, Array<float>& medFmedT, 
			  Array<float>& adT, Array<float>& medF, 
			  Array<float>& medTmedF, Array<float>& adF,
			  const Array<float>& diff, const Array<bool>& flag,
			  const Array<bool>& flagRow)
{
  IPosition shape=diff.shape();
  const int32_t nCorr=shape(0);
  const int32_t nChan=shape(1);
  int32_t nTime=shape(2);
  int32_t nIfr=1;
  const int32_t nXY=nCorr*nChan;
  Array<float> diff2(diff);
  if (diff.ndim()==3) {
    // make 4D reference to diff's storage so diffMedian will return
    // correct shapes
    Array<float> ref(diff2.reform(IPosition(4,nCorr,nChan,nIfr,nTime)));
    diff2.reference(ref);
  } else {
    nIfr=nTime;
    nTime=shape(3);
  }
  const int32_t nXYZ=nXY*nIfr;
  bool deleteFlag, deleteFlagRow, deleteDiff;
  const bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  const bool* pflag = flag.getStorage(deleteFlag);
  const float* pdiff = diff2.getStorage(deleteDiff);
 
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
    bool deletemedT;
    const float* pmedT=medT.getStorage(deletemedT);
    int32_t offset=0;
    IPosition polifr(2);
    for (int32_t pol=0; pol<nCorr; pol++) {
      polifr(0)=pol;
      offset=pol;
      for (int32_t ifr=0; ifr<nIfr; ifr++, offset+=nXY) {
	polifr(1)=ifr;
	float ad=0, med=medFmedT(polifr);
	int32_t count=0, offchan=offset;
	for (int32_t i=0; i<nChan; i++, offchan+=nCorr) {
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
    bool deletemedF;
    const float* pmedF=medF.getStorage(deletemedF);
    int32_t offset=0, nXZ=nCorr*nIfr;
    IPosition polifr(2);
    for (int32_t pol=0; pol<nCorr; pol++) {
      polifr(0)=pol;
      offset=pol;
      for (int32_t ifr=0; ifr<nIfr; ifr++, offset+=nCorr) {
	polifr(1)=ifr;
	float ad=0, med=medTmedF(polifr);
	int32_t count=0, offtime=offset, offrow=ifr;
	for (int32_t i=0; i<nTime; i++, offtime+=nXZ, offrow+=nIfr) {
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
    for (int32_t pol=0; pol<nCorr; pol++) {
      polifr(0)=pol;
      int32_t offset=pol;
      for (int32_t ifr=0; ifr<nIfr; ifr++, offset+=nXY) {  
	polifr(1)=ifr;
	float ad=0, med=medTF(polifr);
	int32_t count=0, offset2=offset, offrow=ifr;
	for (int32_t i=0; i<nTime; i++, offset2+=nXYZ, offrow+=nIfr) {
	  if (!pflagRow[offrow]) {
	    for (int32_t j=0, offset3=offset2; j<nChan; j++,offset3+=nCorr) {
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

void MSFlagger::diffMedian(Array<float>& out, const Array<float>& in, 
			    int32_t axis, const Array<bool>& flag)
{
  // collapse array "in" (with absolute differences) 
  // along specified axis by taking medians by profile taking into account
  // the flags.
  int32_t nDim=in.ndim();
  DebugAssert(axis>=0 && axis<nDim && in.ndim()>0, AipsError);
  IPosition inShape=in.shape(), outShape(max(1,int32_t(in.ndim())-1));
  outShape(0)=1; // cope with 1-d input
  int32_t nLess=1, nGreater=1, nAxis=inShape(axis);
  for (int32_t i=0, count=0; i<nDim; i++) {
    if (i!=axis) outShape(count++)=inShape(i);
    if (i<axis) nLess*=inShape(i);
    if (i>axis) nGreater*=inShape(i);
  }
  out.resize(outShape);

  bool deleteIn, deleteFlag, deleteOut;
  const float* pin=in.getStorage(deleteIn);
  const bool* pflag=flag.getStorage(deleteFlag);
  float* pout=out.getStorage(deleteOut);
  Block<float> values(nAxis);
  for (int32_t j=0, offj=0; j<nGreater; j++, offj+=nLess) {
    for (int32_t k=0, offk=offj*nAxis, offout=offj; k<nLess; 
	 k++, offk++, offout++) {
      int32_t count=0;
      for (int32_t l=0, offin=offk; l<nAxis; l++, offin+=nLess) {
	if (!pflag[offin]) values[count++]=pin[offin];
      }
      if (count>0) pout[offout]=median(Vector<float>(values.begin(),values.begin()+count));
      else pout[offout]=0;
    }
  }
  in.freeStorage(pin,deleteIn);
  flag.freeStorage(pflag,deleteFlag);
  out.putStorage(pout,deleteOut);
}

inline String multiple(int32_t n) { return n!=1 ? "s" : ""; }

bool MSFlagger::clipDataBuffer(float pixelLevel, float timeLevel, 
				float channelLevel)
{
  LogIO os;
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN << "No data loaded into buffer yet"<<
      ", use fillbuffer first"<< LogIO::POST;
    return false;
  }
  String item = buffer_p.asString(RecordFieldId("datafield"));
  if (item.contains("data")) {
    os << LogIO::WARN << "Can't clip complex data,"<<
       " use diffbuffer first or load a derived quantity"<< LogIO::POST;
    return false;
  }

  // retrieve the data
  Array<bool> flag = buffer_p.asArrayBool(RecordFieldId("flag"));
  Array<bool> flagRow = buffer_p.asArrayBool(RecordFieldId("flag_row"));
  Array<float> diff = buffer_p.asArrayFloat(RecordFieldId(item));

  // retrieve the stats
  Matrix<float> adT, adF, medTF, adTF, medFmedT, medTmedF;
  Cube<float> medT, medF;
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
  bool deleteFlag, deleteFlagRow, deleteDiff;
  bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  bool* pflag = flag.getStorage(deleteFlag);
  const float* pdiff = diff.getStorage(deleteDiff);
  const int32_t nCorr=flag.shape()(0);
  const int32_t nChan=flag.shape()(1);
  int32_t nTime=flag.shape()(2);
  const int32_t nXY=nCorr*nChan;
  int32_t nIfr=1;
  if (flag.ndim()==4) {
    nIfr=nTime;
    nTime=flag.shape()(3);
  }
  const int32_t nXYZ=nXY*nIfr;
   
  // iterate till no more pixels are flagged
  bool iter=true;
  Matrix<int32_t> sum(nCorr,nIfr),sumChan(nCorr,nIfr),sumTime(nCorr,nIfr);
  sum=0, sumChan=0, sumTime=0;
  while (iter) {
    iter=false;

    for (int32_t ifr=0, offset=0; ifr<nIfr; ifr++, offset=ifr*nXY) {
      for (int32_t pol=0; pol<nCorr; pol++, offset++) {
	
	// keep these values around
	float mfmt = medFmedT(pol,ifr);
	float adt  = adT(pol,ifr);
	float mtmf = medTmedF(pol,ifr);
	float adf  = adF(pol,ifr);
	float mtf  = medTF(pol,ifr);
	float adtf = adTF(pol,ifr);

	int32_t chanCount=0, timeCount=0, count=0;
	// flag bad channels
	{
	  for (int32_t i=0, offset2=offset; i<nChan; i++, offset2+=nCorr) {
	    float mt=medT(pol,i,ifr);
	    if ( (mt>0) && (abs(mt-mfmt) > channelLevel*adt)) {
	      chanCount++;
	      for (int32_t j=0, offset3=offset2; j<nTime; j++, offset3+=nXYZ) {
		  pflag[offset3]=true;
	      }
	    }
	  }
	}
	// flag bad times
	{
	  int32_t offrow=ifr;
	  for (int32_t i=0, offset2=offset; i<nTime; 
	       i++,offset2+=nXYZ,offrow+=nIfr) {
	    if (!pflagRow[offrow]) {
	      float mf=medF(pol,ifr,i);
	      if (mf>0 && abs(mf-mtmf) > timeLevel*adf) {
		timeCount++;
		for (int32_t j=0, offset3=offset2; j<nChan; j++, offset3+=nCorr) {
		  pflag[offset3]=true;
		}
	      }
	    }
	  }
	}
	// flag bad pixels
	{
	  int32_t offrow=ifr;
	  for (int32_t i=0, offset2=offset; i<nTime;
	       i++, offset2+=nXYZ, offrow+=nIfr) {
	    if (!pflagRow[offrow]) {
	      for (int32_t j=0, offset3=offset2; j<nChan; j++, offset3+=nCorr) {
		if (!pflag[offset3] && 
		    abs(pdiff[offset3]-mtf) > pixelLevel*adtf) {
		  pflag[offset3]=true;
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

  for (int32_t ifr=0; ifr<nIfr; ifr++) {
    for (int32_t pol=0; pol<nCorr; pol++) {
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
  return true;
}

bool MSFlagger::setDataBufferFlags(const Record& flags)
{
  LogIO os;
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN <<
      "Data buffer is empty, use filldatabuffer first"<< LogIO::POST;
    return false;
  }
  buffer_p.define("flag",flags.asArrayBool(RecordFieldId("flag")));
  buffer_p.define("flag_row",flags.asArrayBool(RecordFieldId("flag_row")));
  return true;
}

bool MSFlagger::writeDataBufferFlags()
{
  LogIO os;
  if (!check()) return false;
  if (!msSel_p->selectedTable().isWritable()) {
    os << LogIO::SEVERE << "MeasurementSet is not writable"<< LogIO::POST;
    return false;
  }
  if (!buffer_p.isDefined("datafield")) {
    os << LogIO::WARN <<
      "Data buffer is empty, use filldatabuffer first"<< LogIO::POST;
    return false;
  }
  Record items(RecordInterface::Variable);
  items.define("flag_row",buffer_p.asArrayBool(RecordFieldId("flag_row")));
  items.define("flag",buffer_p.asArrayBool(RecordFieldId("flag")));
  return msSel_p->putData(items);
}

bool MSFlagger::createFlagHistory(int32_t nHis)
{
  LogIO os;
  if (!check()) return false;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isWritable()) {
    os << LogIO::WARN << "MS is not writable"<< LogIO::POST;
    return false;
  }
  if (nHis<2 || nHis>16) {
    os << LogIO::WARN << "Invalid argument: 2<=nHis<=16 "<< LogIO::POST;
    return false;
  } 
  if (tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column already exists"<<LogIO::POST;
    return false;
  }
  // Look for the FLAG column among the hypercolumns
  String flagHypercubeId="";
  bool found=findHypercubeId(flagHypercubeId,MS::columnName(MS::FLAG),tab);
 
  Vector<String> coordColNames(0), idColNames(1);
  TableDesc td1;
  if (!found) {
    // If there's no id, assume the data is fixed shape throughout
    ArrayColumn<bool> flagCol(tab,MS::columnName(MS::FLAG));
    int32_t numCorr=flagCol.shape(0)(0);
    int32_t numChan=flagCol.shape(0)(1);
    IPosition shape(3,nHis,numCorr,numChan);
    idColNames.resize(0);
    td1.addColumn(ArrayColumnDesc<bool>("FLAG_CATEGORY","flag history",shape,
					ColumnDesc::Direct));
    td1.defineHypercolumn("TiledFlagHistory",4,
			  stringToVector("FLAG_CATEGORY"),coordColNames,
			  idColNames);
    // fixed data shape
    int32_t tileSize=numChan/10+1;
    IPosition tileShape(4,1,numCorr,tileSize,16384/numCorr/tileSize);
    TiledColumnStMan tiledStMan1("TiledFlagHistory",tileShape);
    tab.addColumn(td1,tiledStMan1);
    fillFlagHist(nHis,numCorr,numChan,tab);
  } else {
    {
      ArrayColumn<bool> flagCol(tab,MS::columnName(MS::FLAG));
      idColNames(0)="FLAG_CATEGORY_HYPERCUBE_ID"; 
      td1.addColumn(ArrayColumnDesc<bool>("FLAG_CATEGORY","flag history",3));
      td1.addColumn(ScalarColumnDesc<int32_t>("FLAG_CATEGORY_HYPERCUBE_ID",
					  "hypercube index"));
      td1.defineHypercolumn("TiledFlagHistory",4,
			    stringToVector("FLAG_CATEGORY"),coordColNames,
			    idColNames);
      // data shape may change
      TiledDataStMan tiledStMan1("TiledFlagCategory");
      tab.addColumn(td1,tiledStMan1);
      TiledDataStManAccessor flagCatAccessor(tab,"TiledFlagCategory");

      // get the hypercube ids, sort them, remove the duplicate values
      ScalarColumn<int32_t> hypercubeId(tab,flagHypercubeId);
      Vector<int32_t> ids=hypercubeId.getColumn();
      int32_t nId=genSort(ids,Sort::Ascending,Sort::QuickSort+Sort::NoDuplicates);
      ids.resize(nId,true); // resize and copy values
      Vector<bool> cubeAdded(nId,false);
      Record values1; 
      values1.define("FLAG_CATEGORY_HYPERCUBE_ID",hypercubeId(0));
      int32_t cube;
      for (cube=0; cube<nId; cube++) if (ids(cube)==hypercubeId(0)) break;
      int64_t nRow=tab.nrow();
      for (int64_t i=0; i<nRow; i++) {
	// add new hyperCube
	if (i>0 && hypercubeId(i)!=hypercubeId(i-1)) {
	  values1.define("FLAG_CATEGORY_HYPERCUBE_ID",hypercubeId(i));
	  for (cube=0; cube<nId; cube++) if (ids(cube)==hypercubeId(i)) break;
	}
	if (!cubeAdded(cube)) {
	  cubeAdded(cube)=true;
	  int32_t numCorr=flagCol.shape(i)(0);
	  int32_t numChan=flagCol.shape(i)(1);
	  int32_t tileSize=numChan/10+1;
	  IPosition cubeShape(4,nHis,numCorr,numChan,0);
	  IPosition tileShape(4,1,numCorr,tileSize,16384/numCorr/tileSize);
	  flagCatAccessor.addHypercube(cubeShape,tileShape,values1);
	}
	flagCatAccessor.extendHypercube(1,values1);
      }
    }

    TableIterator obsIter(tab,flagHypercubeId);
    for (;!obsIter.pastEnd(); obsIter.next()) {
      ArrayColumn<bool> flagCol(obsIter.table(),MS::columnName(MS::FLAG));
      int32_t numCorr=flagCol.shape(0)(0);
      int32_t numChan=flagCol.shape(0)(1);
      Table tab=obsIter.table();
      fillFlagHist(nHis,numCorr,numChan,tab);
    }
  }    
  return true;
}

bool MSFlagger::findHypercubeId(String& hypercubeId, const String& column,
			    const Table& tab)
{
  // to find the corresponding id column (if any)
  TableDesc td(tab.tableDesc());
  Vector<String> hypercolumnNames=td.hypercolumnNames();
  bool found=false;
  hypercubeId="";
  if (hypercolumnNames.nelements()>0) {
    for (uint32_t i=0; i<hypercolumnNames.nelements(); i++) {
      Vector<String> colNames,coordColNames,idColNames;
      td.hypercolumnDesc(hypercolumnNames(i),
			 colNames,coordColNames,
			 idColNames);
      for (uint32_t j=0; j<colNames.nelements(); j++) {
	if (colNames(j)==column) {
	  found=(idColNames.nelements()>0);
	  if (found) hypercubeId=idColNames(0);
	}
      }
    }
  }
  return found;
}

void MSFlagger::fillFlagHist(int32_t nHis, int32_t numCorr, int32_t numChan, Table& tab)
{
  // fill the first two levels of flagging with the flags present 
  // in the MS columns FLAG and FLAG_ROW.
  const rownr_t maxRow=1000000/(numCorr*numChan); // of order 1 MB chunks
  ArrayColumn<bool> flagCol(tab,MS::columnName(MS::FLAG));
  ArrayColumn<bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  Array<bool> flagHis(IPosition(4,nHis,numCorr,numChan,maxRow));
  // flag level 0
  Cube<bool> ref0(flagHis(IPosition(4,0,0,0,0),
			  IPosition(4,0,numCorr-1,numChan-1,maxRow-1)).
			  reform(IPosition(3,numCorr,numChan,maxRow)));
  // flag level 1
  Cube<bool> ref1(flagHis(IPosition(4,1,0,0,0),
			  IPosition(4,1,numCorr-1,numChan-1,maxRow-1)).
			  reform(IPosition(3,numCorr,numChan,maxRow)));
  flagHis.set(false);
  rownr_t nRow=tab.nrow();
  ScalarColumn<bool> flagRowCol(tab,MS::columnName(MS::FLAG_ROW));
  Array<bool> flagCube;
  Vector<bool> flagRowVec;
  for (rownr_t i=0; i<=(nRow/maxRow); i+=maxRow) {
    rownr_t n=min(maxRow,nRow-maxRow*i);
    if (n<maxRow) {
      flagHis.resize(IPosition(4,nHis,numCorr,numChan,n));
      flagHis.set(false);
      Array<bool> tmp0(flagHis(IPosition(4,0,0,0,0),
			      IPosition(4,0,numCorr-1,numChan-1,n-1)).
		      reform(IPosition(3,numCorr,numChan,n)));
      ref0.reference(tmp0);
      Array<bool> tmp1(flagHis(IPosition(4,1,0,0,0),
			       IPosition(4,1,numCorr-1,numChan-1,n-1)).
		       reform(IPosition(3,numCorr,numChan,n)));
      ref1.reference(tmp1);
    }
    Slicer rowSlice(Slice(i*maxRow,n));
    flagRowCol.getColumnRange(rowSlice,flagRowVec,true);
    flagCol.getColumnRange(rowSlice,flagCube,true);
    ref0=flagCube;
    for (rownr_t j=0; j<n; j++) {
      if (flagRowVec(j)) {
	ref0.xyPlane(j).set(true);
      }
    }
    ref1=ref0;
    flagHisCol.putColumnRange(rowSlice,flagHis);
  }
  // Set the FLAG_LEVEL keyword to 1, to indicate we will be 
  // using these flags (level 0 flags are those already present)
  flagHisCol.rwKeywordSet().define("FLAG_LEVEL",1);
}

bool MSFlagger::saveFlags(bool newLevel)
{
  LogIO os;
  if (!check()) return false;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column does not exist"<<LogIO::POST;
    return false;
  }
  if (!tab.isWritable()) {
    os << LogIO::WARN << "MS is not writable"<< LogIO::POST;
    return false;
  }
  ArrayColumn<bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  int32_t level;
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
  bool found=findHypercubeId(hypercubeId,MS::columnName(MS::FLAG_CATEGORY),tab);
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
  return true;
}

void MSFlagger::saveToFlagHist(int32_t level, Table& tab)
{
  ArrayColumn<bool> flagCol(tab,MS::columnName(MS::FLAG));
  int32_t numCorr=flagCol.shape(0)(0);
  int32_t numChan=flagCol.shape(0)(1);
  const rownr_t maxRow=1000000/(numCorr*numChan); // of order 1 MB chunks
  Array<bool> flagHis(IPosition(4,1,numCorr,numChan,maxRow));
  Cube<bool> ref(flagHis.reform(IPosition(3,numCorr,numChan,maxRow)));
  rownr_t nRow=tab.nrow();
  Array<bool> flagCube;
  Vector<bool> flagRowVec;
  Slicer slicer(Slice(level,1),Slice(0,numCorr),Slice(0,numChan));
  for (rownr_t i=0; i<=(nRow/maxRow); i+=maxRow) {
    rownr_t n=min(maxRow,nRow-maxRow*i);
    if (n<maxRow) {
      flagHis.resize(IPosition(4,1,numCorr,numChan,n));
      Array<bool> tmp(flagHis.reform(IPosition(3,numCorr,numChan,n)));
      ref.reference(tmp);
    }
    RowNumbers rows(n);
    indgen(rows, i*maxRow);
    Table sel=tab(rows);
    ArrayColumn<bool> flagHisCol(sel,MS::columnName(MS::FLAG_CATEGORY));
    ArrayColumn<bool> flagCol(sel,MS::columnName(MS::FLAG));
    ScalarColumn<bool> flagRowCol(sel,MS::columnName(MS::FLAG_ROW));
    flagCol.getColumn(flagCube,true);
    flagRowCol.getColumn(flagRowVec,true);
    ref=flagCube;
    for (rownr_t j=0; j<n; j++) {
      if (flagRowVec(j)) {
	ref.xyPlane(j).set(true);
      }
    }
    flagHisCol.putColumn(slicer,flagHis);
  }
}

bool MSFlagger::restoreFlags(int32_t level)
{
  LogIO os;
  if (!check()) return false;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column does not exist"<<LogIO::POST;
    return false;
  }
  if (!tab.isWritable()) {
    os << LogIO::WARN << "MS is not writable"<< LogIO::POST;
    return false;
  }
  ArrayColumn<bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  int32_t flagLevel=level;
  if (flagLevel==-1) flagHisCol.keywordSet().get("FLAG_LEVEL",flagLevel);
  if (flagLevel<0 || flagLevel>=flagHisCol.shape(0)(0)) {
    os << LogIO::WARN << "Invalid flag level ("<<flagLevel+1<<")"<<LogIO::POST;
    return false;
  }
  String hypercubeId;
  bool found=findHypercubeId(hypercubeId,MS::columnName(MS::FLAG_CATEGORY),tab);
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
  return true;
}

void MSFlagger::applyFlagHist(int32_t level, Table& tab)
{
  rownr_t nRow=tab.nrow();
  ArrayColumn<bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  IPosition shape=flagHisCol.shape(0); shape(0)=1;
  const rownr_t maxRow=1000000/(shape(1)*shape(2)); // of order 1 MB chunks
  Slicer slicer(Slice(level,1),Slice(0,shape(1)),Slice(0,shape(2)));
  for (rownr_t i=0; i<=nRow/maxRow; i++) {
    rownr_t n=min(maxRow,nRow-i*maxRow);
    RowNumbers rows(n);
    indgen(rows, i*maxRow);
    Table sel=tab(rows);
    ArrayColumn<bool> flagHisCol(sel,MS::columnName(MS::FLAG_CATEGORY));
    Cube<bool> flag(flagHisCol.getColumn(slicer).
      reform(IPosition(3,shape(1),shape(2),n)));
    ArrayColumn<bool> flagCol(sel,MS::columnName(MS::FLAG));
    ScalarColumn<bool> flagRowCol(sel,MS::columnName(MS::FLAG_ROW));
    flagCol.putColumn(flag);
    for (rownr_t j=0; j<n; j++) {
      if (allEQ(flag.xyPlane(j),true)) {
	flagRowCol.put(j,true);
      } else {
	flagRowCol.put(j,false);
      }
    }
  }
}

int32_t MSFlagger::flagLevel()
{
  LogIO os;
  if (!check()) return false;
  MeasurementSet tab=msSel_p->selectedTable();
  if (!tab.isColumn(MS::FLAG_CATEGORY)) {
    os << LogIO::WARN << "FLAG_CATEGORY column does not exist"<<LogIO::POST;
    return -1;
  }
  ArrayColumn<bool> flagHisCol(tab,MS::columnName(MS::FLAG_CATEGORY));
  int32_t flagLevel;
  flagHisCol.keywordSet().get("FLAG_LEVEL",flagLevel);
  return flagLevel;
}
  
bool MSFlagger::check() 
{
  LogIO os;
  if (msSel_p) return true;
  os << LogIO::WARN << "Flagger is uninitialized"<<LogIO::POST;
  return false;
}




} //# NAMESPACE CASACORE - END

