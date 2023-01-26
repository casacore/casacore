//# MSFieldIndex.cc: implementation of MSFieldIndex.h
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

#include <casacore/ms/MSSel/MSFieldIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
//#include <casacore/casa/Logging/LogIO.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //-------------------------------------------------------------------------
  
  MSFieldIndex::MSFieldIndex(const MSField& field)
    : msFieldCols_p(field)
  { 
    // Construct from an MS FIELD subtable
    // Input:
    //    field           const MSField&           Input MSField object
    // Output to private data:
    //    msFieldCols_p   MSFieldColumns         MSField columns accessor
    //    fieldIds_p      Vector<int32_t>              Field id's
    //    nrows_p         int32_t                      Number of rows
    //
    // Generate an array of field id's, used in later queries
    nrows_p = msFieldCols_p.nrow();
    fieldIds_p.resize(nrows_p);
    indgen(fieldIds_p);
  }
  
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldRegexOrPattern(const String& pattern,
						     const bool regex)
  {
    Vector<int32_t> IDs;
    IDs = matchFieldNameRegexOrPattern(pattern, regex);
    if (IDs.nelements()==0)
      IDs = matchFieldCodeRegexOrPattern(pattern, regex);
    return IDs;
  }
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldNameRegexOrPattern(const String& pattern,
							 const bool regex)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<int32_t>              Matching field id's
    //
    int32_t pos=0;
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
	Mesg << "Field Expression: Invalid regular expression \"" << pattern << "\"";
	throw(MSSelectionFieldParseError(Mesg.str().c_str()));
      }
    //    cerr << "Pattern = " << strippedPattern << "  Regex = " << reg.regexp() << endl;
    Vector<String> names=msFieldCols_p.name().getColumn();
    Vector<bool> flagRow=msFieldCols_p.flagRow().getColumn();
    IPosition sh(names.shape());
    LogicalArray maskArray(sh,false);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	String sname = stripWhite(names(i)); // Strip leading and trailing blanks
	int32_t ret=(sname.matches(reg,pos));
	maskArray(i) = ((ret>0) && !flagRow(i));
      }
    
    MaskedArray<int32_t> maskFieldID(fieldIds_p,maskArray);
    return maskFieldID.getCompressedArray();
  } 

  Vector<int32_t> MSFieldIndex::maskFieldIDs(const Vector<int32_t>& ids)
  {
    Vector<int32_t> tmp = set_intersection(fieldIds_p,ids); 
    return tmp;
  }
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldCodeRegexOrPattern(const String& pattern,
							 const bool regex)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<int32_t>              Matching field id's
    //
    int32_t pos=0;
    Regex reg;
    if (regex) reg=pattern;
    else       reg=reg.fromPattern(pattern);
    
    //    cerr << "Pattern = " << pattern << "  Regex = " << reg.regexp() << endl;
    Vector<bool> flagRow=msFieldCols_p.flagRow().getColumn();
    Vector<String> codes=msFieldCols_p.code().getColumn();
    IPosition sh(codes.shape());
    LogicalArray maskArray(sh,false);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	int32_t ret=codes(i).matches(reg,pos);
	maskArray(i) = ( (ret>0) && !flagRow(i));
      }
    
    MaskedArray<int32_t> maskFieldID(fieldIds_p,maskArray);
    return maskFieldID.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  Vector<int32_t> MSFieldIndex::matchFieldNameOrCode(const String& name)
  {
    Vector<int32_t> IDs;
    IDs = matchFieldName(name);
    if (IDs.nelements() == 0) 
      IDs = matchFieldCode(name);
    return IDs;
  }
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldName(const String& name)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<int32_t>              Matching field id's
    //
    Vector<String> strippedNames = msFieldCols_p.name().getColumn();
    IPosition sh=strippedNames.shape();
    for(int32_t i=0;i<sh(0);i++)
      {
	String name=strippedNames(i);
	strippedNames(i) = stripWhite(name);
      }
    
    LogicalArray maskArray = (strippedNames==name &&
			      !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);

    return maskFieldId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldCode(const String& code)
  {
    // Match a field code to a set of field id's
    // Input:
    //    code             const String&            Field code to match
    // Output:
    //    matchFieldCode   Vector<int32_t>              Matching field id's
    //
    Vector<String> strippedCodes = msFieldCols_p.code().getColumn();
    int32_t n=strippedCodes.shape()(0);

    for(int32_t i=0;i<n;i++)
      {
	String name=strippedCodes(i);
	strippedCodes(i) = stripWhite(name);
      }

    LogicalArray maskArray = (strippedCodes==code &&
			      !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchSubFieldName(const String& name)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<int32_t>              Matching field id's
    //
    
    Vector<String> fieldnames = msFieldCols_p.name().getColumn();
    uint32_t len = fieldnames.nelements();
    Vector<bool> matchfieldnames(len, false);
    for(uint32_t j = 0; j < len; j++) {
      if(stripWhite(fieldnames[j]).contains(name))
	matchfieldnames(j) = true;
    }
    LogicalArray maskArray( matchfieldnames && !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  } 
  
  
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldName(const Vector<String>& names)
  {
    // Match a set of field names to a set of field id's
    // Input:
    //    names            const Vector<String>&    Field names to match
    // Output:
    //    matchFieldNames  Vector<int32_t>              Matching field id's
    //
    Vector<int32_t> matchedFieldIds;
    // Match each field name individually
    for (uint32_t fld=0; fld<names.nelements(); fld++) {
      // Add to list of field id's
      Vector<int32_t> currentMatch = matchFieldName(names(fld));
      if (currentMatch.nelements() > 0) {
	Vector<int32_t> temp(matchedFieldIds);
	matchedFieldIds.resize(matchedFieldIds.nelements() +
			       currentMatch.nelements(), true);
	matchedFieldIds = concatenateArray(temp, currentMatch);
      }
    }
    return matchedFieldIds;
  }
  
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchSourceId(const int32_t& sourceId)
  {
    // Match a source id to a set of field id's
    // Input:
    //    sourceId        const int32_t&               Source id to match
    // Output:
    //    matchSourceId   Vector<int32_t>              Matching field id's
    //
    LogicalArray maskArray = 
      (msFieldCols_p.sourceId().getColumn()==sourceId &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchSourceId(const Vector<int32_t>& sourceIds)
  {
    // Match a set of source id's to a set of field id's
    // Input:
    //    sourceIds       const Vector<int32_t>&       Source id's to match
    // Output:
    //    matchSourceIds  Vector<int32_t>              Matching field id's
    //
    Vector<int32_t> matchedFieldIds;
    // Match each field name individually
    for (uint32_t fld=0; fld<sourceIds.nelements(); fld++) {
      // Add to list of field id's
      Vector<int32_t> currentMatch = matchSourceId(sourceIds(fld));
      if (currentMatch.nelements() > 0) {
	Vector<int32_t> temp(matchedFieldIds);
	matchedFieldIds.resize(matchedFieldIds.nelements() +
			       currentMatch.nelements(), true);
	matchedFieldIds = concatenateArray(temp, currentMatch);
      }
    }
    return matchedFieldIds;
  }
  
  //-------------------------------------------------------------------------
  Vector<int32_t> MSFieldIndex::matchFieldIDLT(const int32_t n)
  {
    LogicalArray maskArray = 
      (fieldIds_p < n &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  }

  //-------------------------------------------------------------------------
  Vector<int32_t> MSFieldIndex::matchFieldIDGT(const int32_t n)
  {
    LogicalArray maskArray = 
      (fieldIds_p > n &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  }
  //-------------------------------------------------------------------------
  
  Vector<int32_t> MSFieldIndex::matchFieldIDGTAndLT(const int32_t n0, const int32_t n1)
  {
    LogicalArray maskArray = 
      (fieldIds_p > n0 &&
       fieldIds_p < n1 &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<int32_t> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  }
  //-------------------------------------------------------------------------
  // Input list modifier. Elements in the list greater than the number
  // of fields are converted to string and matched against field
  // names.  If a match is found, the element is replaced with the
  // matched name ID (sub-table row number).  Elements less than the
  // number of fields are left unmodified.
  void MSFieldIndex::matchIdAgainstNames(Vector<int32_t>& list)
  {
    for (unsigned int i=0;i<list.nelements();i++)
      if ((unsigned int)list[i] >= fieldIds_p.nelements())
	{
	  std::stringstream ss;
	  ss << list[i];
	  Vector<int> id=matchFieldName(ss.str());
	  if (id.nelements() > 0)
	    list[i]=id[0];
	}
  }
  //-------------------------------------------------------------------------
  Vector<int32_t> MSFieldIndex::validateIndices(const Vector<int32_t>& ids)
  {
    //
    // If any of the IDs is out of range, produce a warning message (and
    // a tip for more reasonable behaviour), and attempt the
    // integar-as-name parsing (yuck) and produce a warning based on
    // the result.
    //
    Vector<int32_t> modifiedIds(ids);  // Make a writeable copy
    vector<int32_t> outOfRangeIdList, intAsNameIdList;
    for (uint32_t i=0;i<ids.nelements();i++)
      if ((ids[i] < 0) || (ids[i] > (int32_t)fieldIds_p.nelements()-1))
	{
	  ostringstream intAsName;
	  outOfRangeIdList.push_back(ids[i]);
	  //	  throw(MSSelectionFieldParseError(Mesg.str()));
	  //	  logIO << Mesg.str() << LogIO::WARN << LogIO::POST;
	  //
	  // Integar-as-name parsing
	  //
	  intAsName << ids[i];
	  Vector<int32_t> intAsNameID=matchFieldNameOrCode(intAsName.str());
	  if (intAsNameID.nelements() > 0)
	    {
	      modifiedIds[i]=intAsNameID[0];
	      intAsNameIdList.push_back(ids[i]);
	    }
	}
    LogIO logIO;
    if (outOfRangeIdList.size()) 
      {
	ostringstream Mesg;
	Mesg << "Field Expression: Found out-of-range index(s) in the list (";
	for (uint32_t i=0;i<outOfRangeIdList.size(); i++) Mesg << outOfRangeIdList[i] << " " ;
	Mesg << ")" << " [TIP: double-quoted strings forces name matching]";
	logIO << Mesg.str() << LogIO::WARN << LogIO::POST;
      }
    if (intAsNameIdList.size())
      {
	ostringstream Mesg;
	Mesg << "Field Expression: Successfully parsed \"";
	for (uint32_t i=0;i<intAsNameIdList.size(); i++) Mesg << intAsNameIdList[i] << " " ;
	Mesg << "\" as name(s) and failed for the rest (please ensure this is what you intended).";
	logIO << Mesg.str() << LogIO::WARN << LogIO::POST;
      }
    //    throw(MSSelectionFieldWarning(Mesg.str()));
    return modifiedIds;
  }
  
} //# NAMESPACE CASACORE - END

