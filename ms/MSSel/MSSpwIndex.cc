//# MSSpwIndex.cc: implementation of MSSpwIndex.h
//# Copyright (C) 2000,2001,2002
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

#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/ms/MSSel/MSSpwIndex.h>
#include <casacore/ms/MSSel/MSSpwParse.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>
#include <algorithm>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //
  //------------------------------------------------------------------
  //
  MSSpwIndex::MSSpwIndex(const MSSpectralWindow& msSpw):
        msSpwSubTable_p(msSpw) 
  {
    int32_t nrows = msSpwSubTable_p.nrow();
    spwIDs.resize(nrows);
    indgen(spwIDs);
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchRegexOrPattern(const String& pattern,
						     const bool regex)
  {
    int32_t pos=0;
    Regex reg;
    if (regex) reg=pattern;
    else       reg=reg.fromPattern(pattern);
    
    //  cerr << "Pattern = " << pattern << "  Regex = " << reg.regexp() << endl;
    IPosition sh(msSpwSubTable_p.name().getColumn().shape());
    LogicalArray maskArray(sh,false);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	int32_t ret=(msSpwSubTable_p.name().getColumn()(i).matches(reg,pos));
	maskArray(i) = ( (ret>0) );//&&	 !msSpwSubTable_p.flagRow().getColumn()(i));
      }
    
    MaskedArray<int32_t> maskSpwID(spwIDs,maskArray);
    return maskSpwID.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  // Input list modifier.  Examine the input list and treat elements
  // greater than the number of SPWs by matching them as name
  // strings. Elements for which this match fails or which are less
  // than the number of SPWs remain unmodified.
  void MSSpwIndex::matchNameAsIntID(Vector<int32_t>& list)
  {
    int nSpw = msSpwSubTable_p.name().getColumn().nelements();
    for(unsigned int i=0;i<list.nelements();i++)
      {
	if (list[i] >= nSpw)
	  {
	    // Convert to string and attempt a match against the name
	    // column.
	    std::stringstream ss;
	    ss << list[i];
	    Vector<int> id=matchName(ss.str());
	    if (id.nelements() > 0)
	      list[i]=id[0];
	  }
      }
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchName(const String& name)
  {
    LogicalArray maskArray = (msSpwSubTable_p.name().getColumn()==name);
      //      && !msSpwSubTable_p.flagRow().getColumn());
    MaskedArray<int32_t> maskSpwId(spwIDs, maskArray);

    return maskSpwId.getCompressedArray();
  } 
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchId(const Vector<int32_t>& sourceId)
  {
    Vector<int32_t> IDs;
    IDs = set_intersection(sourceId,spwIDs);
    //
    // If IDs has less than sourceId, some sourceIds found no match.
    // So construct a message, and send it off to the SPW parser error
    // handler.
    //
    if (IDs.nelements() != sourceId.nelements())
      {
	vector<int> tt = IDs.tovector();
	ostringstream Mesg, tok;
	Mesg << "Spw Expression: No match found for ";
	for (uint32_t i=0;i<sourceId.nelements();i++)
	  {
	    vector<int>::iterator ndx = find(tt.begin(), tt.end(), sourceId[i]);
	    if (ndx == tt.end()) tok << sourceId[i] << ",";
	  }
	MSSpwParse::thisMSSpwErrorHandler->reportError(tok.str().c_str(), Mesg.str());
      }

    return IDs;
  } 
  //
  //------------------------------------------------------------------
  //
  bool MSSpwIndex::matchFrequencyRange(const double f0, const double f1, 
				       Vector<int32_t>& spw, Vector<int32_t>& start, 
				       Vector<int32_t>& nchan){
    int32_t nspw=msSpwSubTable_p.nrow();
    bool found=false;

    spw.resize();
    start.resize();
    nchan.resize();
    int32_t nmatch=0;
    for (int32_t k=0; k < nspw; ++k){
      bool locfound=false;
      bool dum;

      Vector<double> chanfreq=msSpwSubTable_p.chanFreq()(k);

      Sort sort( chanfreq.getStorage(dum),sizeof(double) );
      sort.sortKey((uint32_t)0,TpDouble);
      int32_t nch=chanfreq.nelements();
      Vector<uint32_t> sortIndx;
      sort.sort(sortIndx, nch);
      Vector<double>chanwidth=msSpwSubTable_p.chanWidth()(k);
      if(f0 > chanfreq(sortIndx[0]) &&  f0 < chanfreq(sortIndx[nch-1])){
	locfound=true;
      }
      if(f1 > chanfreq(sortIndx[0]) &&  f1 < chanfreq(sortIndx[nch-1])){
	locfound=true;
      }
      if(locfound){
        Vector<int32_t> chanIn(chanfreq.nelements());
	chanIn=-1;
	int32_t numMatched=0;

	for (uint32_t kk=0; kk < chanfreq.nelements(); ++kk){

	  if( ((chanfreq[kk]+0.5*fabs(chanwidth[kk])) > f0) && ((chanfreq[kk]-0.5*fabs(chanwidth[kk])) < f1)   ) {
	    chanIn[numMatched]=kk;
	    ++ numMatched;
	  }
        }
        if(numMatched >0){
          ++nmatch;
          spw.resize(nmatch, true);
          spw(nmatch-1)=k;
          start.resize(nmatch, true);
          nchan.resize(nmatch, true);
          found=true;
          chanIn.resize(numMatched, true);
          start(nmatch-1)=min(chanIn);
          nchan(nmatch-1)=max(chanIn)-start(nmatch-1)+1;
        
        }
      }
      //spw is fully inside region between f0 and f1
      else if((f0 < chanfreq(sortIndx[0])) && (f1 > chanfreq(sortIndx[nch-1]))){
	++nmatch;
	spw.resize(nmatch, true);
	spw(nmatch-1)=k;
	start.resize(nmatch, true);
	start(nmatch-1)=0;
	nchan.resize(nmatch, true);
	nchan(nmatch-1)=nch;
	found=true;
      }
    }
    return found;
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchFrequencyRange(const float f0, const float f1,
					      bool approx, const float f3)
  {
    int32_t nSpwRows=msSpwSubTable_p.nrow();
    bool Found;
    int32_t mode;
    Vector<int32_t> IDs;
    float localStep;
    mode=RANGE;
    if ((f1 < 0) || (f0==f1)) mode=EXACT;
    if (approx) mode=APPROX;

    ArrayColumn<double> chanWidth(msSpwSubTable_p.chanWidth());
    ArrayColumn<double> chanFreq(msSpwSubTable_p.chanFreq());
    for(int32_t n=0;n<nSpwRows;n++)
      {
	float totalBandWidth, refFreq;
	
	double maxChanWidth;
	{
	  Vector<double> shouldNotBeRequired;
	  chanWidth.get(n,shouldNotBeRequired,true);
	  maxChanWidth = max(shouldNotBeRequired);
	  if (f3 < 0) localStep=min(shouldNotBeRequired);
	  else localStep = f3;
	}

	Found = false;
	if (approx) totalBandWidth = msSpwSubTable_p.totalBandwidth()(n);
	else totalBandWidth = 0;
	//	refFreq = msSpwSubTable_p.refFrequency()(n);
	Vector<double> chanFreqList;
	chanFreq.get(n,chanFreqList,true);
	int32_t nChan=chanFreqList.nelements();
	refFreq = (chanFreqList(nChan-1)+chanFreqList(0))/2.0;;

	//cout << chanFreqList[0] << " " << chanFreqList[nChan-1] << " " << f0 << " " << f1 << " " << f3 << " " << maxChanWidth << endl;

	switch (mode)
	  {
	  case EXACT:
	    {
	      if (fabs(refFreq - f0) < maxChanWidth) Found = true;
	      break;
	    }
	  case APPROX:
	    {
	      if ((fabs(refFreq-f0) <= totalBandWidth) 
		  //		  && (!msSpwSubTable_p.flagRow()(n))
		  )
		Found = true;
	      break;
	    }
	  case RANGE:
	    {
	      if (f3 == 0)
		{
		  if ((refFreq >= f0) && 
		      (refFreq <= f1)
		      //		  && (!msSpwSubTable_p.flagRow()(n))
		      )
		    Found = true;
		  break;
		}
	      else
		{
		  for(float freq=f0;freq <=f1; freq+=localStep) {
		    if (fabs(freq - refFreq) < maxChanWidth) {Found = true;break;}
                  }
		  break;
		}
	    }
	  default:
	    {
	      throw(MSSelectionSpwError("Internal error: Unknown mode in MSSpwIndex::matchFrequencyRange()"));
	    }
	  }
	if (Found)
	  {
	    //
	    // Darn!  We don't use standard stuff (STL!)
	    //
	    //IDs.push_back(SpwIds(n));
	    IDs.resize(IDs.nelements()+1,true);
	    IDs(IDs.nelements()-1) = n;
	    if (mode==EXACT) break;
	  }
      }
    if (IDs.nelements()==0)
      {
	ostringstream msg;
	String rangeStr(" frequency range ");
	if (f0==f1) rangeStr=" frequency ";
	msg << "No matching SPW found for" << rangeStr << f0;
	if (f0!=f1) msg << "~" << f1;
	msg << " Hz.";
	throw(MSSelectionSpwError(msg.str()));
      }
    return IDs;
  } 
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchLT(const float* phyVal)
  {
    Vector<double> refFreqs= msSpwSubTable_p.refFrequency().getColumn();
    LogicalArray maskArray = (refFreqs < (double)phyVal[0]);
    MaskedArray<int32_t> maskSpwId(spwIDs,maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchGT(const float* phyVal)
  {
    Vector<double> refFreqs= msSpwSubTable_p.refFrequency().getColumn();
    LogicalArray maskArray = (refFreqs > (double)phyVal[0]);
    MaskedArray<int32_t> maskSpwId(spwIDs,maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchGTAndLT(const float* phyValMin, const float *phyValMax)
  {
    Vector<double> refFreqs= msSpwSubTable_p.refFrequency().getColumn();
    LogicalArray maskArray = ((refFreqs > (double)phyValMin[0]) && 
			      (refFreqs < (double)phyValMax[0]));
    MaskedArray<int32_t> maskSpwId(spwIDs,maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchLT(const int32_t n)
  {
    LogicalArray maskArray = 
      //      ((spwIDs <= n));// && (!msSpwSubTable_p.flagRow().getColumn()));
      ((spwIDs < n));// && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<int32_t> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchGT(const int32_t n)
  {
    LogicalArray maskArray = 
      ((spwIDs > n));// && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<int32_t> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::matchGTAndLT(const int32_t n0, const int32_t n1)
  {
    LogicalArray maskArray = 
      ((spwIDs > n0) && (spwIDs < n1));// &&(!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<int32_t> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<float> MSSpwIndex::convertToMKS(const float f0, const float f1, 
					 const String& unit)
  {
    Vector<float> freqs(2);
    String units(unit);   units.downcase();
    float factor=1.0;
 
    if (units[0] == 'k') factor *= 1000;
    else if (units[0] == 'm') factor *= 1e6;
    else if (units[0] == 'g') factor *= 1e9;
    else if (units[0] == 't') factor *= 1e12;
    freqs(0) = f0*factor;
    freqs(1) = f1*factor;

    return freqs;
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::convertToChannelIndex(const Vector<int32_t>& spw, 
						const Vector<float>& freqList,
						int32_t &nFSpec)
  {
    LogIO log_l(LogOrigin("MSSpw Expression parser", "MSSpwIndex::convertToChannelIndex", WHERE));

    Vector<int32_t> localFreqList;
    vector<int32_t> localFoundSpwList;
    Vector<int32_t> numChans =  msSpwSubTable_p.numChan().getColumn();

    int32_t nSpw = spw.nelements(), nFList=freqList.nelements();
    nFSpec = nFList/4;  // 4 integers per channel specification

    ArrayColumn<double> chanWidth(msSpwSubTable_p.chanWidth());
    ArrayColumn<double> chanFreq(msSpwSubTable_p.chanFreq());

    bool someMatchFailed=false;
    ostringstream Mesg;

    if (nFList > 0)
      {
	localFreqList.resize(nSpw*nFSpec*3);
	int32_t pos=0;

	for(int32_t i=0;i<nSpw;i++)
	  for(int32_t j=0;j<nFList;j+=4)
	    {
	      if ((freqList(j+3) == MSSpwIndex::MSSPW_INDEX) ||
		  (freqList(j+3) == MSSpwIndex::MSSPW_INDEXRANGE))
		{
		  int32_t start=(int32_t)freqList(j), stop=(int32_t)freqList(j+1), step=(int32_t)freqList(j+2);
		  if (start == -1) start = 0;
		  if (stop == -1) stop = numChans(spw(i))-1;
		  if (stop == start)
		    {
		      start = stop = start < 0? 0 : start;
		      if (stop >= numChans(spw(i)))
			{
			  Mesg << "Spot-channel " << stop << " out of range for SPW "
			       << spw(i) << " (valid range 0~" << numChans(spw(i))-1 << ")."
			       << " Limiting it to be within the available range.";
			  //			  throw(MSSelectionSpwError(Mesg.str()));
			  log_l << Mesg.str() << LogIO::WARN << LogIO::POST;
			  stop = start = numChans(spw(i))-1;
			  someMatchFailed=true;
			}
		    }
		  else
		    {
		      if (stop >= numChans(spw(i)))
			{
			  //			  ostringstream Mesg;
			  Mesg << "Channel " << stop << " out of range for SPW "
			       << spw(i) << " (valid range 0~" << numChans(spw(i))-1 << ")."
			       << " Limiting it to be within the available range.";
			  //			  throw(MSSelectionSpwError(Mesg.str()));
			  log_l << Mesg.str() << LogIO::WARN << LogIO::POST;
			  someMatchFailed=true;
			}
		      start = max(0, min(start,numChans(spw(i))-1));
		      stop  = min(numChans(spw(i))-1, max(stop,0));
		    }
		  if ((start != -1) && (stop != -1)) localFoundSpwList.push_back(spw(i));

		  step = (((int32_t)step <= 0) ? 1 : step);

		  localFreqList(pos++)=start;
		  localFreqList(pos++)=stop;
		  localFreqList(pos++)=step;
		}
	      else if (freqList(j+3) == MSSpwIndex::MSSPW_UNITHZ)  // If the spec is XXHz
		{
		  float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		  Vector<double> cf,cw;
		  chanFreq.get(spw(i),cf,true);
		  chanWidth.get(spw(i),cw,true);
		  if (abs(cw(0)) == 0)
		    throw(MSSelectionSpwError("Error in the MS SPECTRAL_WINDOW sub-table (channel width==0)."));
		      
		  int32_t cwDir = (int32_t)(cw(0)/abs(cw(0)));
		  //
		  // Do a brain-dead linear search for the channel
		  // number (linear search is *probably* OK - unless
		  // there are channels worth GBytes of RAM!)
		  //

		  // Obfuscated code alert (but it was fun :))!
		  someMatchFailed |= ((start = findChanIndex_p(start, cf, true,  (cwDir>0)))==-1);
		  someMatchFailed |= ((stop  = findChanIndex_p(stop,  cf, false, (cwDir>0)))==-1);

		  // bool found=false;
		  // int32_t n=cf.nelements();
		  // {
		  //   if (start <= cf(0)) start=0;
		  //   else
		  //     {
		  // 	for(int32_t ii=0;ii<n;ii++)
		  // 	  if (cf(ii) >= start) {start=ii;found=true;break;}
			
		  // 	if (!found)
		  // 	  {someMatchFailed=true;start = -1;}
		  //     }
		    
		  //   found=false;
		  //   if (stop >= cf(n-1)) stop = n-1;
		  //   else
		  //     {
		  // 	for(int32_t ii=n-1;ii>=0;ii--)
		  // 	  if (cf(ii) <= stop) {stop=ii;found=true;break;}
			
		  // 	if (!found)
		  // 	  {someMatchFailed=true; stop=-1;}
		  //     }
		  // }
		  double maxCW=max(cw), minCW=min(cw);
		  if (minCW != maxCW)
		    {
		      log_l << "Channel width across the band is not constant.  "
			    << "Using the maximum of the channel width range." 
			    << LogIO::WARN;
		    }
		  step=fabs(freqList(i+2)/maxCW);
		  // 
		  // Enforce start < stop and step > 0.  
		  //
		  // In case it is ever required to support the case
		  // where start > step (e.g., when the frequency in
		  // the database is in descending order) the variable
		  // cwDir carries the direction in which
		  // freq. increases with increase channel index.
		  //
		  step = (((int32_t)step <= 0) ? 1 : step);//*cwDir;
		  if (start > stop)
		    {
		      float tmp=start;
		      start=stop;stop=tmp;
		    }

		  if ((start != -1) && (stop != -1)) localFoundSpwList.push_back(spw(i));
		  localFreqList(pos++)=(int32_t)start;
		  localFreqList(pos++)=(int32_t)stop;
		  localFreqList(pos++)=(int32_t)step;
		}
	      else  // If the spec is XXKm/s
		{
		  //
		  // Now that I (SB) think about this, veloctiy based
		  // selection in MSSelection does not make sense.
		  //
		  //float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		  //
 		  // cerr << "Start = " << start << " Stop = " << stop << " Step = " << step << endl;
 		  // MRadialVelocity vstart(Quantity(start, "km/s"), MRadialVelocity::LSRK);
 		  // MDoppler mdoppler(vstart.getValue().get(), MDoppler::RADIO);
 		  // MSDopplerUtil msdoppler(*ms_p);
 		  // msdoppler.dopplerInfo(restFreq ,spw(i), fieldid);
		  
 		  // cout << MFrequency::fromDoppler(mdoppler, 
 		  // 				  restFreq).getValue().getValue() << endl;
		}
	    }
      }
    else
      {
	int32_t j=0;
	nFSpec=1;
 	localFreqList.resize(nSpw*3);
 	for(int32_t i=0;i<nSpw;i++)
 	  {
 	    localFreqList(j++)=0;
 	    localFreqList(j++)=numChans(spw(i))-1;
 	    localFreqList(j++)=1;
 	  }
      }
    
    
    // if (localFoundSpwList.size() == 0)
    //   log_l << "No match found for SPW and CHAN combination" << LogIO::WARN << LogIO::POST;
    // else 
    if (someMatchFailed) {
      if (localFoundSpwList.size() != 0) {
	// log_l << "Found match for SPW(s) "  
	//       << Vector<int32_t>(localFoundSpwList) 
	//       << " for some sub-expression." 
	//       << LogIO::WARN << LogIO::POST;
	;
      } else {
        ostringstream m;
        log_l << "Found no matching SPW(s) " << spw << LogIO::WARN << LogIO::POST;
        //	  log_l << m.str() << LogIO::WARN << LogIO::POST;
      }
    }
    
    
    return localFreqList;
  }
  //
  //------------------------------------------------------------------
  //
  int32_t MSSpwIndex::findChanIndex_p(const float& freq, const Vector<double>& chanFreqList,
				  const bool& greaterThan,
				  const bool& ascendingOrder)
  {
    int32_t chanIndex=-1, n=chanFreqList.nelements();
    if (ascendingOrder)
      {
	if (greaterThan)
	  {
	    if (freq <= chanFreqList(0)) 
	      chanIndex=0;
	    else
	      for(int32_t ii=0;ii<n;ii++)
		if (chanFreqList(ii) >= freq) {chanIndex=ii;break;}
	  }
	else
	  {
	    if (freq >= chanFreqList(n-1))
	      chanIndex=n-1;
	    else
	      for(int32_t ii=n-1;ii>=0;ii--)
		if (chanFreqList(ii) <= freq) {chanIndex=ii;break;}
	  }
      }
    else
      {
	if (greaterThan)
	  {
	    if (freq <= chanFreqList(n-1)) 
	      chanIndex=n-1;
	    else
	      for(int32_t ii=n-1;ii>=0;ii--)
		if (chanFreqList(ii) >= freq) {chanIndex=ii;break;}
	  }
	else
	  {
	    if (freq >= chanFreqList(0))
	      chanIndex=0;
	    else
	      for(int32_t ii=0;ii<n;ii++)
		if (chanFreqList(ii) <= freq) {chanIndex=ii;break;}
	  }
      }
    return chanIndex;
  }
  //
  //------------------------------------------------------------------
  //
  Vector<int32_t> MSSpwIndex::convertToSpwIndex(const Vector<float>& freqList,
					    int32_t &nFSpec)
  {
    Vector<int32_t> localFreqList;
    int32_t nFList=freqList.nelements();
    nFSpec = nFList/4;  // 4 integers per channel specification
    
    if (nFList > 0)
      {
	//	localFreqList.resize(nFSpec);
	int32_t pos=0;
	
	for(int32_t j=0;j<nFList;j+=4)
	  {
	    if ((freqList(j+3) == MSSpwIndex::MSSPW_INDEX) ||
		(freqList(j+3) == MSSpwIndex::MSSPW_INDEXRANGE))
	      {
		int32_t start=(int32_t)freqList(j), stop=(int32_t)freqList(j+1), step=(int32_t)freqList(j+2);
		//		  if (step==0) step=1;
		step = (step <= 0? 1 : step);

		int32_t n=0;
		for(int32_t ii=start;ii<=stop;ii+=step) n++;
		localFreqList.resize(n+localFreqList.nelements(),true);
		
		for(int32_t ii=start;ii<=stop;ii+=step)
		  localFreqList(pos++)=ii;
		//		  localFreqList(pos++)=stop;
		//		  localFreqList(pos++)=step;
	      }
	    else if (freqList(j+3) == MSSpwIndex::MSSPW_UNITHZ)
	      {
		float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		
		localFreqList = matchFrequencyRange(start, stop, false, step);
		//		  cout << "Freq SPW List  = " << start << " " << stop << " " << step << " " 
		//		       << localFreqList;
	      }
	  }
      }
    else
      {
	throw(MSSelectionSpwError("Internal error. Contact CASA manager.\n"
				  "nFList in MSSpwIndex::convertToSpwIndex() is <= 0."));
      }
    
    return localFreqList;
  }
  //-------------------------------------------------------------------------
  
} //# NAMESPACE CASACORE - END

