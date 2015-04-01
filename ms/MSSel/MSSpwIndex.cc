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
//#
//# $Id$

#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/ms/MSSel/MSSpwIndex.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //
  //------------------------------------------------------------------
  //
  MSSpwIndex::MSSpwIndex(const MSSpectralWindow& msSpw):
        msSpwSubTable_p(msSpw) 
  {
    Int nrows = msSpwSubTable_p.nrow();
    spwIDs.resize(nrows);
    indgen(spwIDs);
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchRegexOrPattern(const String& pattern,
						     const Bool regex)
  {
    Int pos=0;
    Regex reg;
    if (regex) reg=pattern;
    else       reg=reg.fromPattern(pattern);
    
    //  cerr << "Pattern = " << pattern << "  Regex = " << reg.regexp() << endl;
    IPosition sh(msSpwSubTable_p.name().getColumn().shape());
    LogicalArray maskArray(sh,False);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	Int ret=(msSpwSubTable_p.name().getColumn()(i).matches(reg,pos));
	maskArray(i) = ( (ret>0) );//&&	 !msSpwSubTable_p.flagRow().getColumn()(i));
      }
    
    MaskedArray<Int> maskSpwID(spwIDs,maskArray);
    return maskSpwID.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchName(const String& name)
  {
    LogicalArray maskArray = (msSpwSubTable_p.name().getColumn()==name);
      //      && !msSpwSubTable_p.flagRow().getColumn());
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);

    return maskSpwId.getCompressedArray();
  } 
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchId(const Vector<Int>& sourceId)
  {
    Vector<Int> IDs;
    IDs = set_intersection(sourceId,spwIDs);
    return IDs;
  } 
  //
  //------------------------------------------------------------------
  //
  Bool MSSpwIndex::matchFrequencyRange(const Double f0, const Double f1, 
				       Vector<Int>& spw, Vector<Int>& start, 
				       Vector<Int>& nchan){
    Int nspw=msSpwSubTable_p.nrow();
    Bool found=False;
    ///Bool begIn=False;
    ///Bool aftIn=False;
    spw.resize();
    start.resize();
    nchan.resize();
    Int nmatch=0;
    for (Int k=0; k < nspw; ++k){
      Bool locfound=False;
      Bool dum;
      ///Int chanpositive=1;
      Vector<Double> chanfreq=msSpwSubTable_p.chanFreq()(k);
      ///if (chanfreq.nelements() >1){
      ///chanpositive=((chanfreq[1]-chanfreq[0]) > 0.0) ? 1: -1;
      ///}
      Sort sort( chanfreq.getStorage(dum),sizeof(Double) );
      sort.sortKey((uInt)0,TpDouble);
      Int nch=chanfreq.nelements();
      Vector<uInt> sortIndx;
      sort.sort(sortIndx, nch);
      Vector<Double>chanwidth=msSpwSubTable_p.chanWidth()(k);
      ///begIn=False;
      ///aftIn=False;
      if(f0 > chanfreq(sortIndx[0]) &&  f0 < chanfreq(sortIndx[nch-1])){
	///begIn=True;
	locfound=True;
      }
      if(f1 > chanfreq(sortIndx[0]) &&  f1 < chanfreq(sortIndx[nch-1])){
	///aftIn=True;
	locfound=True;
      }
      if(locfound){
	++nmatch;
	spw.resize(nmatch, True);
	spw(nmatch-1)=k;
	start.resize(nmatch, True);
	nchan.resize(nmatch, True);
	found=True;
	Vector<Int> chanIn(chanfreq.nelements());
	chanIn=-1;
	Int numMatched=0;
	//cerr << "f0 " << f0 << " f1 " << f1 << endl;

	for (uInt kk=0; kk < chanfreq.nelements(); ++kk){
	  //cerr << kk << "  " << chanfreq[kk]+0.5*fabs(chanwidth[kk]) << "   " << (chanfreq[kk]-0.5*fabs(chanwidth[kk])) << "   " << chanfreq[kk] << endl;
	  if( ((chanfreq[kk]+0.5*fabs(chanwidth[kk])) > f0) && ((chanfreq[kk]-0.5*fabs(chanwidth[kk])) < f1)){
	    chanIn[numMatched]=kk;
	    ++ numMatched;
	  }

	}
	chanIn.resize(numMatched, True);
	//cerr << "chanIn "<< chanIn  << endl;
	start(nmatch-1)=min(chanIn);
	nchan(nmatch-1)=max(chanIn)-start(nmatch-1)+1;
	//cerr << "chanIn "<< chanIn << " start " << start << " nchan " << nchan << endl;
	/*
	if(begIn){
	  Int counter=0;
	  //Use abs as sometimes chanwidth is negative and sometimes not !
	  while((chanfreq(sortIndx[counter])+0.5*fabs(chanwidth(sortIndx[counter]))) < f0)
	    ++counter;
	  start(nmatch-1)= counter > 0 ? (chanpositive ? counter-1 : counter+1): 0;
	}
	else{
	  start(nmatch-1)=0;
	}
	if(aftIn){
	  Int counter=nch-1;
	  while((chanfreq(sortIndx[counter])-0.5*chanwidth(sortIndx[counter])) > f1)
	    --counter;
	  if(counter <= start(nmatch-1))
	    nchan(nmatch-1)=1;
	  else
	    nchan(nmatch-1)=counter-start(nmatch-1)+1;
	}
	else{
	  nchan(nmatch-1)=nch-start(nmatch-1);

	}
	//if it is reversed sort order reverse start
	if(sortIndx[0] != 0){
	  start[nmatch-1] = nch-start[nmatch-1]-nchan(nmatch-1);
	  if(start[nmatch-1] < 0) start[nmatch-1]=0;
	  if((start[nmatch-1]+nchan[nmatch-1]) >= nch) nchan[nmatch-1]=nch-start[nmatch-1];	  
	}
*/
      }
      //spw is fully inside region between f0 and f1
      else if((f0 < chanfreq(sortIndx[0])) && (f1 > chanfreq(sortIndx[nch-1]))){
	++nmatch;
	spw.resize(nmatch, True);
	spw(nmatch-1)=k;
	start.resize(nmatch, True);
	start(nmatch-1)=0;
	nchan.resize(nmatch, True);
	nchan(nmatch-1)=nch;
	found=True;
      }
    }
    return found;
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchFrequencyRange(const Float f0, const Float f1,
					      Bool approx, const Float f3)
  {
    Int nSpwRows=msSpwSubTable_p.nrow();
    Bool Found;
    Int mode;
    Vector<Int> IDs;
    Float localStep;
    mode=RANGE;
    if ((f1 < 0) || (f0==f1)) mode=EXACT;
    if (approx) mode=APPROX;

    ROArrayColumn<Double> chanWidth(msSpwSubTable_p.chanWidth());
    ROArrayColumn<Double> chanFreq(msSpwSubTable_p.chanFreq());
    for(Int n=0;n<nSpwRows;n++)
      {
	Float totalBandWidth, refFreq;
	
	Double maxChanWidth;
	{
	  Vector<Double> shouldNotBeRequired;
	  chanWidth.get(n,shouldNotBeRequired,True);
	  maxChanWidth = max(shouldNotBeRequired);
	  if (f3 < 0) localStep=min(shouldNotBeRequired);
	  else localStep = f3;
	}

	Found = False;
	if (approx) totalBandWidth = msSpwSubTable_p.totalBandwidth()(n);
	else totalBandWidth = 0;
	//	refFreq = msSpwSubTable_p.refFrequency()(n);
	Vector<Double> chanFreqList;
	chanFreq.get(n,chanFreqList,True);
	Int nChan=chanFreqList.nelements();
	refFreq = (chanFreqList(nChan-1)+chanFreqList(0))/2.0;;

	//cout << chanFreqList[0] << " " << chanFreqList[nChan-1] << " " << f0 << " " << f1 << " " << f3 << " " << maxChanWidth << endl;

	switch (mode)
	  {
	  case EXACT:
	    {
	      if (fabs(refFreq - f0) < maxChanWidth) Found = True;
	      break;
	    }
	  case APPROX:
	    {
	      if ((fabs(refFreq-f0) <= totalBandWidth) 
		  //		  && (!msSpwSubTable_p.flagRow()(n))
		  )
		Found = True;
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
		    Found = True;
		  break;
		}
	      else
		{
		  for(Float freq=f0;freq <=f1; freq+=localStep) {
		    if (fabs(freq - refFreq) < maxChanWidth) {Found = True;break;}
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
	    IDs.resize(IDs.nelements()+1,True);
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
  Vector<Int> MSSpwIndex::matchLT(const Float* phyVal)
  {
    Vector<Double> refFreqs= msSpwSubTable_p.refFrequency().getColumn();
    LogicalArray maskArray = (refFreqs < (Double)phyVal[0]);
    MaskedArray<Int> maskSpwId(spwIDs,maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchGT(const Float* phyVal)
  {
    Vector<Double> refFreqs= msSpwSubTable_p.refFrequency().getColumn();
    LogicalArray maskArray = (refFreqs > (Double)phyVal[0]);
    MaskedArray<Int> maskSpwId(spwIDs,maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchGTAndLT(const Float* phyValMin, const Float *phyValMax)
  {
    Vector<Double> refFreqs= msSpwSubTable_p.refFrequency().getColumn();
    LogicalArray maskArray = ((refFreqs > (Double)phyValMin[0]) && 
			      (refFreqs < (Double)phyValMax[0]));
    MaskedArray<Int> maskSpwId(spwIDs,maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchLT(const Int n)
  {
    LogicalArray maskArray = 
      //      ((spwIDs <= n));// && (!msSpwSubTable_p.flagRow().getColumn()));
      ((spwIDs < n));// && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchGT(const Int n)
  {
    LogicalArray maskArray = 
      ((spwIDs > n));// && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::matchGTAndLT(const Int n0, const Int n1)
  {
    LogicalArray maskArray = 
      ((spwIDs > n0) && (spwIDs < n1));// &&(!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Float> MSSpwIndex::convertToMKS(const Float f0, const Float f1, 
					 const String& unit)
  {
    Vector<Float> freqs(2);
    String units(unit);   units.downcase();
    Float factor=1.0;
 
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
  Vector<Int> MSSpwIndex::convertToChannelIndex(const Vector<Int>& spw, 
						const Vector<Float>& freqList,
						Int &nFSpec)
  {
    LogIO log_l(LogOrigin("MSSpw Expression parser", "MSSpwIndex::convertToChannelIndex", WHERE));

    Vector<Int> localFreqList;
    vector<Int> localFoundSpwList;
    Vector<Int> numChans =  msSpwSubTable_p.numChan().getColumn();

    Int nSpw = spw.nelements(), nFList=freqList.nelements();
    nFSpec = nFList/4;  // 4 integers per channel specification

    ROArrayColumn<Double> chanWidth(msSpwSubTable_p.chanWidth());
    ROArrayColumn<Double> chanFreq(msSpwSubTable_p.chanFreq());

    Bool someMatchFailed=False;
    ostringstream Mesg;

    if (nFList > 0)
      {
	localFreqList.resize(nSpw*nFSpec*3);
	Int pos=0;

	for(Int i=0;i<nSpw;i++)
	  for(Int j=0;j<nFList;j+=4)
	    {
	      if ((freqList(j+3) == MSSpwIndex::MSSPW_INDEX) ||
		  (freqList(j+3) == MSSpwIndex::MSSPW_INDEXRANGE))
		{
		  Int start=(Int)freqList(j), stop=(Int)freqList(j+1), step=(Int)freqList(j+2);
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
			  someMatchFailed=True;
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
			  someMatchFailed=True;
			}
		      start = start < 0 ? 0 : start;
		      stop  = stop >= numChans(spw(i)) ? numChans(spw(i)) - 1 : stop;
		    }
		  if ((start != -1) && (stop != -1)) localFoundSpwList.push_back(spw(i));

		  step = (((Int)step <= 0) ? 1 : step);

		  localFreqList(pos++)=start;
		  localFreqList(pos++)=stop;
		  localFreqList(pos++)=step;
		}
	      else if (freqList(j+3) == MSSpwIndex::MSSPW_UNITHZ)  // If the spec is XXHz
		{
		  Float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		  Vector<Double> cf,cw;
		  chanFreq.get(spw(i),cf,True);
		  chanWidth.get(spw(i),cw,True);
		  if (abs(cw(0) == 0))
		    throw(MSSelectionSpwError("Error in the MS SPECTRAL_WINDOW sub-table (channel width==0)."));
		      
		  Int cwDir = (Int)(cw(0)/abs(cw(0)));
		  //
		  // Do a brain-dead linear search for the channel
		  // number (linear search is *probably* OK - unless
		  // there are channels worth GBytes of RAM!)
		  //

		  // Obfuscated code alert (but it was fun :))!
		  someMatchFailed |= ((start = findChanIndex_p(start, cf, True,  (cwDir>0)))==-1);
		  someMatchFailed |= ((stop  = findChanIndex_p(stop,  cf, False, (cwDir>0)))==-1);

		  // Bool found=False;
		  // Int n=cf.nelements();
		  // {
		  //   if (start <= cf(0)) start=0;
		  //   else
		  //     {
		  // 	for(Int ii=0;ii<n;ii++)
		  // 	  if (cf(ii) >= start) {start=ii;found=True;break;}
			
		  // 	if (!found)
		  // 	  {someMatchFailed=True;start = -1;}
		  //     }
		    
		  //   found=False;
		  //   if (stop >= cf(n-1)) stop = n-1;
		  //   else
		  //     {
		  // 	for(Int ii=n-1;ii>=0;ii--)
		  // 	  if (cf(ii) <= stop) {stop=ii;found=True;break;}
			
		  // 	if (!found)
		  // 	  {someMatchFailed=True; stop=-1;}
		  //     }
		  // }
		  Double maxCW=max(cw), minCW=min(cw);
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
		  step = (((Int)step <= 0) ? 1 : step);//*cwDir;
		  if (start > stop)
		    {
		      Float tmp=start;
		      start=stop;stop=tmp;
		    }

		  if ((start != -1) && (stop != -1)) localFoundSpwList.push_back(spw(i));
		  localFreqList(pos++)=(Int)start;
		  localFreqList(pos++)=(Int)stop;
		  localFreqList(pos++)=(Int)step;
		}
	      else  // If the spec is XXKm/s
		{
		  //
		  // Now that I (SB) think about this, veloctiy based
		  // selection in MSSelection does not make sense.
		  //
		  //Float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
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
	Int j=0;
	nFSpec=1;
 	localFreqList.resize(nSpw*3);
 	for(Int i=0;i<nSpw;i++)
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
	//       << Vector<Int>(localFoundSpwList) 
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
  Int MSSpwIndex::findChanIndex_p(const Float& freq, const Vector<Double>& chanFreqList,
				  const Bool& greaterThan,
				  const Bool& ascendingOrder)
  {
    Int chanIndex=-1, n=chanFreqList.nelements();
    if (ascendingOrder)
      {
	if (greaterThan)
	  {
	    if (freq <= chanFreqList(0)) 
	      chanIndex=0;
	    else
	      for(Int ii=0;ii<n;ii++)
		if (chanFreqList(ii) >= freq) {chanIndex=ii;break;}
	  }
	else
	  {
	    if (freq >= chanFreqList(n-1))
	      chanIndex=n-1;
	    else
	      for(Int ii=n-1;ii>=0;ii--)
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
	      for(Int ii=n-1;ii>=0;ii--)
		if (chanFreqList(ii) >= freq) {chanIndex=ii;break;}
	  }
	else
	  {
	    if (freq >= chanFreqList(0))
	      chanIndex=0;
	    else
	      for(Int ii=0;ii<n;ii++)
		if (chanFreqList(ii) <= freq) {chanIndex=ii;break;}
	  }
      }
    return chanIndex;
  }
  //
  //------------------------------------------------------------------
  //
  Vector<Int> MSSpwIndex::convertToSpwIndex(const Vector<Float>& freqList,
					    Int &nFSpec)
  {
    Vector<Int> localFreqList;
    Int nFList=freqList.nelements();
    nFSpec = nFList/4;  // 4 integers per channel specification
    
    if (nFList > 0)
      {
	//	localFreqList.resize(nFSpec);
	Int pos=0;
	
	for(Int j=0;j<nFList;j+=4)
	  {
	    if ((freqList(j+3) == MSSpwIndex::MSSPW_INDEX) ||
		(freqList(j+3) == MSSpwIndex::MSSPW_INDEXRANGE))
	      {
		Int start=(Int)freqList(j), stop=(Int)freqList(j+1), step=(Int)freqList(j+2);
		//		  if (step==0) step=1;
		step = (step <= 0? 1 : step);

		Int n=0;
		for(Int ii=start;ii<=stop;ii+=step) n++;
		localFreqList.resize(n+localFreqList.nelements(),True);
		
		for(Int ii=start;ii<=stop;ii+=step)
		  localFreqList(pos++)=ii;
		//		  localFreqList(pos++)=stop;
		//		  localFreqList(pos++)=step;
	      }
	    else if (freqList(j+3) == MSSpwIndex::MSSPW_UNITHZ)
	      {
		Float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		
		localFreqList = matchFrequencyRange(start, stop, False, step);
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

