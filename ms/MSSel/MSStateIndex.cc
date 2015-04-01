//# MSStateIndex.cc: implementation of MSStateIndex.h
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

#include <casacore/ms/MSSel/MSStateIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //-------------------------------------------------------------------------
  
  MSStateIndex::MSStateIndex(const MSState& state)
    : msStateCols_p(state)
  { 
    nrows_p = msStateCols_p.nrow();
    stateIds_p.resize(nrows_p);
    indgen(stateIds_p);
  }
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSStateIndex::matchStateRegexOrPattern(const String& pattern,
						     const Bool regex)
  {
    Vector<Int> IDs;
    IDs = matchStateObsModeRegexOrPattern(pattern, regex);
    // if (IDs.nelements()==0)
    //   IDs = matchStateCodeRegexOrPattern(pattern, regex);
    return IDs;
  }
  //-------------------------------------------------------------------------
  Int MSStateIndex::matchAnyRegex(const Vector<String>& strList, 
				  const Regex& regex, 
				  const Int pos)
  {
    Int ret=0;
    for(uInt i=0;i<strList.nelements();i++)
      if ((ret=strList[i].matches(regex,pos)) > 0) break;
    return ret;
  }
  //-------------------------------------------------------------------------
  Vector<Int> MSStateIndex::matchStateObsModeRegexOrPattern(const String& pattern,
							    const Bool regex)
  {
    // Match a state name to a set of state id's
    // Input:
    //    name             const String&            State name to match
    // Output:
    //    matchStateName   Vector<Int>              Matching state id's
    //
    Int pos=0;
    Regex reg;
    //   String strippedPattern = stripWhite(pattern);
    const String& strippedPattern = pattern;
    try
      {
	if (regex) reg=strippedPattern;
	else       reg=reg.fromPattern(strippedPattern);
      }
    catch (...) // Since I (SB) don't know the type of exception Regex throws, catch them all!
      {
	ostringstream Mesg;
	Mesg << "State Expression: Invalid regular expression \"" << pattern << "\"";
	throw(MSSelectionStateParseError(Mesg.str().c_str()));
      }
    //cerr << "Pattern = " << strippedPattern << "  Regex = " << reg.regexp() << endl;
    IPosition sh(msStateCols_p.obsMode().getColumn().shape());
    LogicalArray maskArray(sh,False);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	String name = msStateCols_p.obsMode().getColumn()(i);
	String sname = stripWhite(name); // Strip leading and trailing blanks
	//	Int ret=(sname.matches(reg,pos));
	Vector<String> substr;
	split(sname,',',substr);
	Int ret=matchAnyRegex(substr,reg,pos);
	maskArray(i) = ((ret>0) && !msStateCols_p.flagRow().getColumn()(i));
      }
    
    MaskedArray<Int> maskStateID(stateIds_p,maskArray);
    return maskStateID.getCompressedArray();
  } 

  Vector<Int> MSStateIndex::maskStateIDs(const Vector<Int>& ids)
  {
    Vector<Int> tmp = set_intersection(stateIds_p,ids); 
    return tmp;
  }
  //-------------------------------------------------------------------------

  Vector<Int> MSStateIndex::matchStateObsMode(const String& name)
  {
    // Match a state name to a set of state id's
    // Input:
    //    name             const String&            State name to match
    // Output:
    //    matchStateName   Vector<Int>              Matching state id's
    //
    IPosition irow(1,nrows_p);
    LogicalArray tmaskArray(irow,False); 

    for(irow(0)=0;irow(0)<nrows_p;irow(0)++)
      if (!msStateCols_p.flagRow().getColumn()[irow(0)])
	{
	  Vector<String> substr;
	  split(msStateCols_p.obsMode().getColumn()[irow(0)],',',substr);
	  for (uInt istr=0; istr<substr.nelements(); istr++)
	    if (substr[istr]==name)
	      {
		tmaskArray(irow)=True;
		break;
	      }
	}
    // LogicalArray maskArray = (msStateCols_p.obsMode().getColumn()==name &&
    // 			      !msStateCols_p.flagRow().getColumn());
    MaskedArray<Int> maskStateId(stateIds_p, tmaskArray);

    return maskStateId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSStateIndex::matchStateObsMode(const Vector<String>& names)
  {
    // Match a set of state names to a set of state id's
    // Input:
    //    names            const Vector<String>&    State names to match
    // Output:
    //    matchStateNames  Vector<Int>              Matching state id's
    //
    Vector<Int> matchedStateIds;
    // Match each state name individually
    for (uInt fld=0; fld<names.nelements(); fld++) {
      // Add to list of state id's
      Vector<Int> currentMatch = matchStateObsMode(names(fld));
      if (currentMatch.nelements() > 0) {
	Vector<Int> temp(matchedStateIds);
	matchedStateIds.resize(matchedStateIds.nelements() +
			       currentMatch.nelements(), True);
	matchedStateIds = concatenateArray(temp, currentMatch);
      }
    }
    return matchedStateIds;
  }
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSStateIndex::matchStateId(const Int& stateId)
  {
    // Match a source id to a set of state id's
    // Input:
    //    sourceId        const Int&               Source id to match
    // Output:
    //    matchSourceId   Vector<Int>              Matching state id's
    //
    LogicalArray maskArray = 
      (stateIds_p==stateId &&
       !msStateCols_p.flagRow().getColumn());
    MaskedArray<Int> maskStateId(stateIds_p, maskArray);
    return maskStateId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSStateIndex::matchStateId(const Vector<Int>& stateIds)
  {
    // Match a set of state id's to a set of state id's
    // Input:
    //    stateIds       const Vector<Int>&       State id's to match
    // Output:
    //    matchStateIds  Vector<Int>              Matching state id's
    //
    Vector<Int> matchedStateIds;
    // Match each state name individually
    for (uInt fld=0; fld<stateIds.nelements(); fld++) {
      // Add to list of state id's
      Vector<Int> currentMatch = matchStateId(stateIds(fld));
      if (currentMatch.nelements() > 0) {
	Vector<Int> temp(matchedStateIds);
	matchedStateIds.resize(matchedStateIds.nelements() +
			       currentMatch.nelements(), True);
	matchedStateIds = concatenateArray(temp, currentMatch);
      }
    }
    return matchedStateIds;
  }
  
  //-------------------------------------------------------------------------
  Vector<Int> MSStateIndex::matchStateIDLT(const Int n)
  {
    LogicalArray maskArray = 
      //      (msStateCols_p.stateId().getColumn() < n &&
      (stateIds_p < n &&
       !msStateCols_p.flagRow().getColumn());
    MaskedArray<Int> maskStateId(stateIds_p, maskArray);
    return maskStateId.getCompressedArray();
  }

  //-------------------------------------------------------------------------
  Vector<Int> MSStateIndex::matchStateIDGT(const Int n)
  {
    LogicalArray maskArray = 
      //      (msStateCols_p.stateId().getColumn() > n &&
      (stateIds_p > n &&
       !msStateCols_p.flagRow().getColumn());
    MaskedArray<Int> maskStateId(stateIds_p, maskArray);
    return maskStateId.getCompressedArray();
  }
  //-------------------------------------------------------------------------
  
  Vector<Int> MSStateIndex::matchStateIDGTAndLT(const Int n0, const Int n1)
  {
    LogicalArray maskArray = 
//       (msStateCols_p.stateId().getColumn() >= n0 &&
//        msStateCols_p.stateId().getColumn() <= n1 &&
      (stateIds_p > n0 &&
       stateIds_p < n1 &&
       !msStateCols_p.flagRow().getColumn());
    MaskedArray<Int> maskStateId(stateIds_p, maskArray);
    return maskStateId.getCompressedArray();
  }
  //-------------------------------------------------------------------------
  
} //# NAMESPACE CASACORE - END

