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

#include <measures/Measures/MDoppler.h>
#include <ms/MeasurementSets/MSSpwIndex.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Utilities/Regex.h>
#include <ms/MeasurementSets/MSSelectionTools.h>
#include <casa/BasicSL/String.h>
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN
  
  //-------------------------------------------------------------------------
  
  MSSpwIndex::MSSpwIndex(const MSSpectralWindow& msSpw):
        msSpwSubTable_p(msSpw) 
  {
    Int nrows = msSpwSubTable_p.nrow();
    spwIDs.resize(nrows);
    indgen(spwIDs);
  };

  //-------------------------------------------------------------------------
  
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

  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchName(const String& name)
  {
    LogicalArray maskArray = (msSpwSubTable_p.name().getColumn()==name);
      //      && !msSpwSubTable_p.flagRow().getColumn());
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);

    return maskSpwId.getCompressedArray();
  }; 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchId(const Vector<Int>& sourceId)
  {
    Vector<Int> IDs;
    IDs = set_intersection(sourceId,spwIDs);
    return IDs;
  }; 
  
  //-------------------------------------------------------------------------
  Bool MSSpwIndex::matchFrequencyRange(const Double f0, const Double f1, 
				       Vector<Int>& spw, Vector<Int>& start, 
				       Vector<Int>& nchan){
    Int nspw=msSpwSubTable_p.nrow();
    Bool found=False;
    Bool begIn=False;
    Bool aftIn=False;
    spw.resize();
    start.resize();
    nchan.resize();
    Int nmatch=0;
    for (Int k=0; k < nspw; ++k){
      Bool locfound=False;
      Vector<Double> chanfreq=msSpwSubTable_p.chanFreq()(k);
      Vector<Double>chanwidth=msSpwSubTable_p.chanWidth()(k);
      Int nch=chanfreq.nelements();
      begIn=False;
      aftIn=False;
      if(f0 > chanfreq(0) &&  f0 < chanfreq(nch-1)){
	begIn=True;
	locfound=True;
      }
      if(f1 > chanfreq(0) &&  f1 < chanfreq(nch-1)){
	aftIn=True;
	locfound=True;
      }
      if(locfound){
	++nmatch;
	spw.resize(nmatch, True);
	spw(nmatch-1)=k;
	start.resize(nmatch, True);
	nchan.resize(nmatch, True);
	found=True;
	if(begIn){
	  Int counter=0;
	  while((chanfreq(counter)+0.5*chanwidth(counter)) < f0)
	    ++counter;
	  start(nmatch-1)=counter;
	}
	else{
	  start(nmatch-1)=0;
	}
	if(aftIn){
	  Int counter=nch-1;
	  while((chanfreq(counter)-0.5*chanwidth(counter)) > f1)
	    --counter;
	  if(counter <= start(nmatch-1))
	    nchan(nmatch-1)=1;
	  else
	    nchan(nmatch-1)=counter-start(nmatch-1)+1;
	}
	else{
	  nchan(nmatch-1)=nch-start(nmatch-1);

	}
      }
      //spw is inside region
      else if((f0 < chanfreq(0)) && (f1 > chanfreq(1))){
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


  Vector<Int> MSSpwIndex::matchFrequencyRange(const Float f0, const Float f1,
					      Bool approx, const Float f3)
  {
    Int nSpwRows=msSpwSubTable_p.nrow();
    Bool Found;
    Int mode;
    Vector<Int> IDs;
    mode=RANGE;
    if ((f1 < 0) || (f0==f1)) mode=EXACT;
    if (approx) mode=APPROX;

    ROArrayColumn<Double> chanWidth(msSpwSubTable_p.chanWidth());
    ROArrayColumn<Double> chanFreq(msSpwSubTable_p.chanFreq());
    for(Int n=0;n<nSpwRows;n++)
      {
	Float totalBandWidth, refFreq;

	Vector<Double> cw;
	chanWidth.get(n,cw,True);
	Double maxChanWidth = max(cw);

	Found = False;
	if (approx) totalBandWidth = msSpwSubTable_p.totalBandwidth()(n);
	else totalBandWidth = 0;
	refFreq = msSpwSubTable_p.refFrequency()(n);
	//	cout << refFreq << " " << f0 << " " << maxChanWidth << " " << fabs(refFreq-f0) << endl;
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
		  for(Float freq=f0;freq <=f1; freq+=f3)
		    if (fabs(freq - refFreq) < maxChanWidth) {Found = True;break;}
		  break;
		}
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
      }
    return IDs;
  }; 
  
  //-------------------------------------------------------------------------
  Vector<Int> MSSpwIndex::matchIDLT(const Int n)
  {
    LogicalArray maskArray = 
      //      ((spwIDs <= n));// && (!msSpwSubTable_p.flagRow().getColumn()));
      ((spwIDs < n));// && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  };

  //-------------------------------------------------------------------------
  Vector<Int> MSSpwIndex::matchIDGT(const Int n)
  {
    LogicalArray maskArray = 
      ((spwIDs > n));// && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  };
  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchIDGTAndLT(const Int n0, const Int n1)
  {
    LogicalArray maskArray = 
      ((spwIDs > n0) && (spwIDs < n1));// &&(!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  };
  //-------------------------------------------------------------------------
  Vector<Float> MSSpwIndex::convertToMKS(const Float f0, const Float f1, 
					 const String& unit)
  {
    Vector<Float> freqs(2);
    String units(unit);
    Float factor=1.0;
    units.downcase();
    if (units[0] == 'k') factor *= 1000;
    else if (units[0] == 'm') factor *= 1e6;
    else if (units[0] == 'g') factor *= 1e9;
    else if (units[0] == 't') factor *= 1e12;
    freqs(0) = f0*factor;
    freqs(1) = f1*factor;
    return freqs;
  };
  //-------------------------------------------------------------------------
  Vector<Int> MSSpwIndex::convertToChannelIndex(const Vector<Int>& spw, 
						const Vector<Float>& freqList,
						Int &nFSpec)
  {
    Vector<Int> localFreqList;
    Vector<Int> numChans =  msSpwSubTable_p.numChan().getColumn();

    Int nSpw = spw.nelements(), nFList=freqList.nelements();
    nFSpec = nFList/4;  // 4 integers per channel specification

    ROArrayColumn<Double> chanWidth(msSpwSubTable_p.chanWidth());
    ROArrayColumn<Double> chanFreq(msSpwSubTable_p.chanFreq());

    //    cout << "freqList = " << freqList << endl << "Spw = " << spw << endl;
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
			  ostringstream Mesg;
			  Mesg << "Channel " << stop << " out of range for SPW "
			       << spw(i) << " (valid range 0~" << numChans(spw(i))-1 << ")";
			  throw(MSSelectionSpwError(Mesg.str()));
			}
		    }
		  else
		    {
		      if (start >= numChans(spw(i)))
			{
			  ostringstream Mesg;
			  Mesg << "Channel " << start << " out of range for SPW "
			       << spw(i) << " (valid range 0~" << numChans(spw(i))-1 << ")";
			  throw(MSSelectionSpwError(Mesg.str()));
			}
		      start = start < 0 ? 0 : start;
		      stop  = stop >= numChans(spw(i)) ? numChans(spw(i)) - 1 : stop;
		    }
		  localFreqList(pos++)=start;
		  localFreqList(pos++)=stop;
		  localFreqList(pos++)=step;
		}
	      else if (freqList(j+3) == MSSpwIndex::MSSPW_UNITHZ)
		{
		  Float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		  Vector<Double> cf,cw;
		  chanFreq.get(spw(i),cf,True);
		  chanWidth.get(spw(i),cw,True);

		  //		  cout << i << " " << cf << endl << freqList << endl;
		  //
		  // Do a brain-dead linear search for the channel
		  // number (linear search is *probably* OK - unless
		  // there are channels worth GBytes of RAM!)
		  //
		  Int n=cf.nelements(),ii;
		  ostringstream Mesg;
		  //		  cout << "start,stop = " << start << " " << stop << endl;
		  Mesg << "Range for SPW " << spw(i) << ": ["
		       << cf[0] << "," << cf(n-1) << "] Hz.";

		  Bool found=False;
		  if (start <= cf(0)) start=0;
		  else
		    {
		      for(ii=0;ii<n;ii++)
			if (cf(ii) >= start) {start=ii;found=True;break;}
		      if (!found)
			{
			  ostringstream m;
			  m << "Start channel not found for frequency " 
			    << start << " Hz." << Mesg.str();
			  throw(MSSelectionSpwError(m.str()));
			}
		    }

		  found=False;
		  if (stop >= cf(n-1)) stop = n-1;
		  else
		    {
		      for(ii=n-1;ii>=0;ii--)
			if (cf(ii) <= stop) {stop=ii;found=True;break;}
		      if (!found)
			{
			  ostringstream m;
			  m << "Stop channel not found for frequency " 
			    << stop << " Hz. " <<  Mesg.str();
			  throw(MSSelectionSpwError(m.str()));
			}
		    }

		  Double maxCW=max(cw), minCW=min(cw);
		  if (minCW != maxCW)
		    {
		      LogIO os(LogOrigin("MSSpw Expression parser", "MSSpwIndex::convertToChannelIndex", WHERE));
		      os << "Channel width across the band is not constant.  Using the maximum of the channel width"
			 << "range." << LogIO::WARN;
		    }
		  step=(freqList(i+2)/maxCW);
		  //		  cout << "start,stop = " << start << " " << stop << endl;

		  localFreqList(pos++)=(Int)start;
		  localFreqList(pos++)=(Int)stop;
		  localFreqList(pos++)=(Int)step;
		  //		  cout << "LocalFreqList " << localFreqList << endl;
		}
	      else
		{
		  Float start=freqList(j),stop=freqList(j+1),step=freqList(j+2);
		  //
		  // Now that I think about this, veloctiy based
		  // selection in MSSelection does not make sense.
		  //
// 		  cerr << "Start = " << start << " Stop = " << stop << " Step = " << step << endl;
// 		  MRadialVelocity vstart(Quantity(start, "km/s"), MRadialVelocity::LSRK);
// 		  MDoppler mdoppler(vstart.getValue().get(), MDoppler::RADIO);
// 		  MSDopplerUtil msdoppler(*ms_p);
// 		  msdoppler.dopplerInfo(restFreq ,spw(i), fieldid);

// 		  cout << MFrequency::fromDoppler(mdoppler, 
// 						  restFreq).getValue().getValue() << endl;

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

    return localFreqList;
  }
  //-------------------------------------------------------------------------
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
		  if (step==0) step=1;
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
  
} //# NAMESPACE CASA - END

