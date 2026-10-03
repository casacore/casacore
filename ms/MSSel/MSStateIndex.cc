// # MSStateIndex.cc: implementation of MSStateIndex.h
// # Copyright (C) 2000,2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/ms/MSSel/MSStateIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
namespace casacore {  // # NAMESPACE CASACORE - BEGIN

//-------------------------------------------------------------------------

MSStateIndex::MSStateIndex(const MSState& state) : msStateCols_p(state) {
  nrows_p = msStateCols_p.nrow();
  stateIds_p.resize(nrows_p);
  indgen(stateIds_p);
}

//-------------------------------------------------------------------------

Vector<int> MSStateIndex::matchStateRegexOrPattern(const String& pattern, const bool regex) {
  Vector<int> IDs;
  IDs = matchStateObsModeRegexOrPattern(pattern, regex);
  // if (IDs.nelements()==0)
  //   IDs = matchStateCodeRegexOrPattern(pattern, regex);
  return IDs;
}
//-------------------------------------------------------------------------
int MSStateIndex::matchAnyRegex(const Vector<String>& strList, const Regex& regex, const int pos) {
  int ret = 0;
  for (unsigned int i = 0; i < strList.nelements(); i++)
    if ((ret = RegexMatches(strList[i], regex, pos)) > 0) break;
  return ret;
}
//-------------------------------------------------------------------------
Vector<int> MSStateIndex::matchStateObsModeRegexOrPattern(const String& pattern, const bool regex) {
  // Match a state name to a set of state id's
  // Input:
  //    name             const String&            State name to match
  // Output:
  //    matchStateName   Vector<Int>              Matching state id's
  //
  int pos = 0;
  Regex reg;
  //   String strippedPattern = stripWhite(pattern);
  const String& strippedPattern = pattern;
  try {
    if (regex)
      reg = strippedPattern;
    else
      reg = reg.fromPattern(strippedPattern);
  } catch (...)  // Since I (SB) don't know the type of exception Regex throws, catch them all!
  {
    std::ostringstream Mesg;
    Mesg << "State Expression: Invalid regular expression \"" << pattern << "\"";
    throw(MSSelectionStateParseError(Mesg.str().c_str()));
  }
  // cerr << "Pattern = " << strippedPattern << "  Regex = " << reg.regexp() << std::endl;
  IPosition sh(msStateCols_p.obsMode().getColumn().shape());
  LogicalArray maskArray(sh, false);
  IPosition i = sh;
  for (i(0) = 0; i(0) < sh(0); i(0)++) {
    String name = msStateCols_p.obsMode().getColumn()(i);
    String sname = stripWhite(name);  // Strip leading and trailing blanks
    //	Int ret=(sname.matches(reg,pos));
    Vector<String> substr;
    split(sname, ',', substr);
    int ret = matchAnyRegex(substr, reg, pos);
    maskArray(i) = ((ret > 0) && !msStateCols_p.flagRow().getColumn()(i));
  }

  MaskedArray<int> maskStateID(stateIds_p, maskArray);
  return maskStateID.getCompressedArray();
}

Vector<int> MSStateIndex::maskStateIDs(const Vector<int>& ids) {
  Vector<int> tmp = set_intersection(stateIds_p, ids);
  return tmp;
}
//-------------------------------------------------------------------------

Vector<int> MSStateIndex::matchStateObsMode(const String& name) {
  // Match a state name to a set of state id's
  // Input:
  //    name             const String&            State name to match
  // Output:
  //    matchStateName   Vector<Int>              Matching state id's
  //
  IPosition irow(1, nrows_p);
  LogicalArray tmaskArray(irow, false);

  for (irow(0) = 0; irow(0) < nrows_p; irow(0)++)
    if (!msStateCols_p.flagRow().getColumn()[irow(0)]) {
      Vector<String> substr;
      split(msStateCols_p.obsMode().getColumn()[irow(0)], ',', substr);
      for (unsigned int istr = 0; istr < substr.nelements(); istr++)
        if (substr[istr] == name) {
          tmaskArray(irow) = true;
          break;
        }
    }
  // LogicalArray maskArray = (msStateCols_p.obsMode().getColumn()==name &&
  // 			      !msStateCols_p.flagRow().getColumn());
  MaskedArray<int> maskStateId(stateIds_p, tmaskArray);

  return maskStateId.getCompressedArray();
}

//-------------------------------------------------------------------------

Vector<int> MSStateIndex::matchStateObsMode(const Vector<String>& names) {
  // Match a set of state names to a set of state id's
  // Input:
  //    names            const Vector<String>&    State names to match
  // Output:
  //    matchStateNames  Vector<Int>              Matching state id's
  //
  Vector<int> matchedStateIds;
  // Match each state name individually
  for (unsigned int fld = 0; fld < names.nelements(); fld++) {
    // Add to list of state id's
    Vector<int> currentMatch = matchStateObsMode(names(fld));
    if (currentMatch.nelements() > 0) {
      Vector<int> temp(matchedStateIds);
      matchedStateIds.resize(matchedStateIds.nelements() + currentMatch.nelements(), true);
      matchedStateIds = concatenateArray(temp, currentMatch);
    }
  }
  return matchedStateIds;
}

//-------------------------------------------------------------------------

Vector<int> MSStateIndex::matchStateId(const int& stateId) {
  // Match a source id to a set of state id's
  // Input:
  //    sourceId        const Int&               Source id to match
  // Output:
  //    matchSourceId   Vector<Int>              Matching state id's
  //
  LogicalArray maskArray = (stateIds_p == stateId && !msStateCols_p.flagRow().getColumn());
  MaskedArray<int> maskStateId(stateIds_p, maskArray);
  return maskStateId.getCompressedArray();
}

//-------------------------------------------------------------------------

Vector<int> MSStateIndex::matchStateId(const Vector<int>& stateIds) {
  // Match a set of state id's to a set of state id's
  // Input:
  //    stateIds       const Vector<Int>&       State id's to match
  // Output:
  //    matchStateIds  Vector<Int>              Matching state id's
  //
  Vector<int> matchedStateIds;
  // Match each state name individually
  for (unsigned int fld = 0; fld < stateIds.nelements(); fld++) {
    // Add to list of state id's
    Vector<int> currentMatch = matchStateId(stateIds(fld));
    if (currentMatch.nelements() > 0) {
      Vector<int> temp(matchedStateIds);
      matchedStateIds.resize(matchedStateIds.nelements() + currentMatch.nelements(), true);
      matchedStateIds = concatenateArray(temp, currentMatch);
    }
  }
  return matchedStateIds;
}

//-------------------------------------------------------------------------
Vector<int> MSStateIndex::matchStateIDLT(const int n) {
  LogicalArray maskArray =
      //      (msStateCols_p.stateId().getColumn() < n &&
      (stateIds_p < n && !msStateCols_p.flagRow().getColumn());
  MaskedArray<int> maskStateId(stateIds_p, maskArray);
  return maskStateId.getCompressedArray();
}

//-------------------------------------------------------------------------
Vector<int> MSStateIndex::matchStateIDGT(const int n) {
  LogicalArray maskArray =
      //      (msStateCols_p.stateId().getColumn() > n &&
      (stateIds_p > n && !msStateCols_p.flagRow().getColumn());
  MaskedArray<int> maskStateId(stateIds_p, maskArray);
  return maskStateId.getCompressedArray();
}
//-------------------------------------------------------------------------

Vector<int> MSStateIndex::matchStateIDGTAndLT(const int n0, const int n1) {
  LogicalArray maskArray =
      //       (msStateCols_p.stateId().getColumn() >= n0 &&
      //        msStateCols_p.stateId().getColumn() <= n1 &&
      (stateIds_p > n0 && stateIds_p < n1 && !msStateCols_p.flagRow().getColumn());
  MaskedArray<int> maskStateId(stateIds_p, maskArray);
  return maskStateId.getCompressedArray();
}
//-------------------------------------------------------------------------

}  // namespace casacore
