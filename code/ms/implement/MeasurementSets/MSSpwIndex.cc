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

#include <ms/MeasurementSets/MSSpwIndex.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Utilities/Regex.h>

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
	maskArray(i) = ( (ret>0) &&
			 !msSpwSubTable_p.flagRow().getColumn()(i));
      }
    
    MaskedArray<Int> maskSpwID(spwIDs,maskArray);
    return maskSpwID.getCompressedArray();
  }

  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchName(const String& name)
  {
    LogicalArray maskArray = (msSpwSubTable_p.name().getColumn()==name &&
			      !msSpwSubTable_p.flagRow().getColumn());
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);

    return maskSpwId.getCompressedArray();
  }; 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchId(const Vector<Int>& sourceId)
  {
    Vector<Int> IDs;
    return IDs;
  }; 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchFrequencyRange(const Float f0, const Float f1,
					      Bool approx)
  {
    Int nSpwRows=msSpwSubTable_p.nrow();
    Bool Found;
    Int mode;
    Vector<Int> IDs;
    mode=RANGE;
    if (f1 < 0) mode=EXACT;
    if (approx) mode=APPROX;

    for(Int n=0;n<nSpwRows;n++)
      {
	Float totalBandWidth, refFreq;
	Found = False;
	if (approx) totalBandWidth = msSpwSubTable_p.totalBandwidth()(n);
	else totalBandWidth = 0;
	refFreq = msSpwSubTable_p.refFrequency()(n);
	switch (mode)
	  {
	  case EXACT:
	    {
	      if (refFreq == f0) Found = True;
	      break;
	    }
	  case APPROX:
	    {
	      if ((refFreq-f0 <= totalBandWidth) &&
		  (!msSpwSubTable_p.flagRow()(n))
		  )
		Found = True;
	      break;
	    }
	  case RANGE:
	    {
	      if ((refFreq >= f0) && 
		  (refFreq <= f1) && 
		  (!msSpwSubTable_p.flagRow()(n))
		  )
		Found = True;
	      break;
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
	  }
      }
    return IDs;
  }; 
  
  //-------------------------------------------------------------------------
  Vector<Int> MSSpwIndex::matchIDLT(const Int n)
  {
    LogicalArray maskArray = 
      ((spwIDs <= n) && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  };

  //-------------------------------------------------------------------------
  Vector<Int> MSSpwIndex::matchIDGT(const Int n)
  {
    LogicalArray maskArray = 
      ((spwIDs >= n) && (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  };
  //-------------------------------------------------------------------------
  
  Vector<Int> MSSpwIndex::matchIDGTAndLT(const Int n0, const Int n1)
  {
    LogicalArray maskArray = 
      ((spwIDs >= n0) && (spwIDs <= n1) &&
       (!msSpwSubTable_p.flagRow().getColumn()));
    MaskedArray<Int> maskSpwId(spwIDs, maskArray);
    return maskSpwId.getCompressedArray();
  };
  //-------------------------------------------------------------------------
  Vector<Float> MSSpwIndex::convertToHz(const Float f0, const Float f1, 
					const String& unit)
  {
    Vector<Float> freqs(2);
    String units(unit);
    Float factor=1.0;
    units.downcase();
    if (units == "kz") factor *= 1000;
    else if (units == "mhz") factor *= 1e6;
    else if (units == "ghz") factor *= 1e9;
    else if (units == "thz") factor *= 1e12;
    freqs(0) = f0*factor;
    freqs(1) = f1*factor;
    return freqs;
  };
  //-------------------------------------------------------------------------
  
} //# NAMESPACE CASA - END

