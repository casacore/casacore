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
//#
//# $Id$

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
    //    msFieldCols_p   ROMSFieldColumns         MSField columns accessor
    //    fieldIds_p      Vector<Int>              Field id's
    //    nrows_p         Int                      Number of rows
    //
    // Generate an array of field id's, used in later queries
    nrows_p = msFieldCols_p.nrow();
    fieldIds_p.resize(nrows_p);
    indgen(fieldIds_p);
  }
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldRegexOrPattern(const String& pattern,
						     const Bool regex)
  {
    Vector<Int> IDs;
    IDs = matchFieldNameRegexOrPattern(pattern, regex);
    if (IDs.nelements()==0)
      IDs = matchFieldCodeRegexOrPattern(pattern, regex);
    return IDs;
  }
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldNameRegexOrPattern(const String& pattern,
							 const Bool regex)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<Int>              Matching field id's
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
	Mesg << "Field Expression: Invalid regular expression \"" << pattern << "\"";
	throw(MSSelectionFieldParseError(Mesg.str().c_str()));
      }
    //    cerr << "Pattern = " << strippedPattern << "  Regex = " << reg.regexp() << endl;
    IPosition sh(msFieldCols_p.name().getColumn().shape());
    LogicalArray maskArray(sh,False);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	String name = msFieldCols_p.name().getColumn()(i);
	String sname = stripWhite(name); // Strip leading and trailing blanks
	//	const String &sname = name; // Strip leading and trailing blanks
	//        cout << "Name = " << name << " SName = " << sname << endl;
	Int ret=(sname.matches(reg,pos));
	maskArray(i) = ((ret>0) && !msFieldCols_p.flagRow().getColumn()(i));
      }
    
    MaskedArray<Int> maskFieldID(fieldIds_p,maskArray);
    return maskFieldID.getCompressedArray();
  } 

  Vector<Int> MSFieldIndex::maskFieldIDs(const Vector<Int>& ids)
  {
    Vector<Int> tmp = set_intersection(fieldIds_p,ids); 
    return tmp;
  }
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldCodeRegexOrPattern(const String& pattern,
							 const Bool regex)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<Int>              Matching field id's
    //
    Int pos=0;
    Regex reg;
    if (regex) reg=pattern;
    else       reg=reg.fromPattern(pattern);
    
    //    cerr << "Pattern = " << pattern << "  Regex = " << reg.regexp() << endl;
    IPosition sh(msFieldCols_p.name().getColumn().shape());
    LogicalArray maskArray(sh,False);
    IPosition i=sh;
    for(i(0)=0;i(0)<sh(0);i(0)++)
      {
	Int ret=(msFieldCols_p.code().getColumn()(i).matches(reg,pos));
	maskArray(i) = ( (ret>0) &&
			 !msFieldCols_p.flagRow().getColumn()(i));
      }
    
    MaskedArray<Int> maskFieldID(fieldIds_p,maskArray);
    return maskFieldID.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  Vector<Int> MSFieldIndex::matchFieldNameOrCode(const String& name)
  {
    Vector<Int> IDs;
    IDs = matchFieldName(name);
    if (IDs.nelements() == 0) 
      IDs = matchFieldCode(name);
    return IDs;
  }
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldName(const String& name)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<Int>              Matching field id's
    //
    Vector<String> strippedNames = msFieldCols_p.name().getColumn();
    IPosition sh=strippedNames.shape();
    for(Int i=0;i<sh(0);i++)
	strippedNames(i) = stripWhite(msFieldCols_p.name().getColumn()(i));

    LogicalArray maskArray = (strippedNames==name &&
			      !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);

    return maskFieldId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldCode(const String& code)
  {
    // Match a field code to a set of field id's
    // Input:
    //    code             const String&            Field code to match
    // Output:
    //    matchFieldCode   Vector<Int>              Matching field id's
    //
    Vector<String> strippedCodes = msFieldCols_p.code().getColumn();
    IPosition sh=strippedCodes.shape();
    for(Int i=0;i<sh(0);i++)
	strippedCodes(i) = stripWhite(msFieldCols_p.code().getColumn()(i));

    LogicalArray maskArray = (strippedCodes==code &&
			      !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchSubFieldName(const String& name)
  {
    // Match a field name to a set of field id's
    // Input:
    //    name             const String&            Field name to match
    // Output:
    //    matchFieldName   Vector<Int>              Matching field id's
    //
    
    Vector<String> fieldnames = msFieldCols_p.name().getColumn();
    uInt len = fieldnames.nelements();
    Vector<Bool> matchfieldnames(len, False);
    for(uInt j = 0; j < len; j++) {
      if(stripWhite(fieldnames[j]).contains(name))
	matchfieldnames(j) = True;
    }
    LogicalArray maskArray( matchfieldnames && !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  } 
  
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldName(const Vector<String>& names)
  {
    // Match a set of field names to a set of field id's
    // Input:
    //    names            const Vector<String>&    Field names to match
    // Output:
    //    matchFieldNames  Vector<Int>              Matching field id's
    //
    Vector<Int> matchedFieldIds;
    // Match each field name individually
    for (uInt fld=0; fld<names.nelements(); fld++) {
      // Add to list of field id's
      Vector<Int> currentMatch = matchFieldName(names(fld));
      if (currentMatch.nelements() > 0) {
	Vector<Int> temp(matchedFieldIds);
	matchedFieldIds.resize(matchedFieldIds.nelements() +
			       currentMatch.nelements(), True);
	matchedFieldIds = concatenateArray(temp, currentMatch);
      }
    }
    return matchedFieldIds;
  }
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchSourceId(const Int& sourceId)
  {
    // Match a source id to a set of field id's
    // Input:
    //    sourceId        const Int&               Source id to match
    // Output:
    //    matchSourceId   Vector<Int>              Matching field id's
    //
    LogicalArray maskArray = 
      (msFieldCols_p.sourceId().getColumn()==sourceId &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  } 
  
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchSourceId(const Vector<Int>& sourceIds)
  {
    // Match a set of source id's to a set of field id's
    // Input:
    //    sourceIds       const Vector<Int>&       Source id's to match
    // Output:
    //    matchSourceIds  Vector<Int>              Matching field id's
    //
    Vector<Int> matchedFieldIds;
    // Match each field name individually
    for (uInt fld=0; fld<sourceIds.nelements(); fld++) {
      // Add to list of field id's
      Vector<Int> currentMatch = matchSourceId(sourceIds(fld));
      if (currentMatch.nelements() > 0) {
	Vector<Int> temp(matchedFieldIds);
	matchedFieldIds.resize(matchedFieldIds.nelements() +
			       currentMatch.nelements(), True);
	matchedFieldIds = concatenateArray(temp, currentMatch);
      }
    }
    return matchedFieldIds;
  }
  
  //-------------------------------------------------------------------------
  Vector<Int> MSFieldIndex::matchFieldIDLT(const Int n)
  {
    LogicalArray maskArray = 
      //      (msFieldCols_p.sourceId().getColumn() < n &&
      (fieldIds_p < n &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  }

  //-------------------------------------------------------------------------
  Vector<Int> MSFieldIndex::matchFieldIDGT(const Int n)
  {
    LogicalArray maskArray = 
      //      (msFieldCols_p.sourceId().getColumn() > n &&
      (fieldIds_p > n &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  }
  //-------------------------------------------------------------------------
  
  Vector<Int> MSFieldIndex::matchFieldIDGTAndLT(const Int n0, const Int n1)
  {
    LogicalArray maskArray = 
//       (msFieldCols_p.sourceId().getColumn() >= n0 &&
//        msFieldCols_p.sourceId().getColumn() <= n1 &&
      (fieldIds_p > n0 &&
       fieldIds_p < n1 &&
       !msFieldCols_p.flagRow().getColumn());
    MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
    return maskFieldId.getCompressedArray();
  }
  //-------------------------------------------------------------------------
  Vector<Int> MSFieldIndex::validateIndices(const Vector<Int>& ids)
  {
    //
    // If any of the IDs is out of range, produce a warning message (and
    // a tip for more reasonable behaviour), and attempt the
    // integar-as-name parsing (yuck) and produce a warning based on
    // the result.
    //
    Vector<Int> modifiedIds(ids);  // Make a writeable copy
    vector<Int> outOfRangeIdList, intAsNameIdList;
    for (uInt i=0;i<ids.nelements();i++)
      if ((ids[i] < 0) || (ids[i] > (Int)fieldIds_p.nelements()-1))
	{
	  ostringstream intAsName;
	  outOfRangeIdList.push_back(ids[i]);
	  //	  throw(MSSelectionFieldParseError(Mesg.str()));
	  //	  logIO << Mesg.str() << LogIO::WARN << LogIO::POST;
	  //
	  // Integar-as-name parsing
	  //
	  intAsName << ids[i];
	  Vector<Int> intAsNameID=matchFieldNameOrCode(intAsName.str());
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
	for (uInt i=0;i<outOfRangeIdList.size(); i++) Mesg << outOfRangeIdList[i] << " " ;
	Mesg << ")" << " [TIP: Double-quoted strings forces name matching]";
	logIO << Mesg.str() << LogIO::WARN << LogIO::POST;
      }
    if (intAsNameIdList.size())
      {
	ostringstream Mesg;
	Mesg << "Field Expression: Successfully parsed \"";
	for (uInt i=0;i<intAsNameIdList.size(); i++) Mesg << intAsNameIdList[i] << " " ;
	Mesg << "\" as name(s) and failed for the rest (please ensure this is what you intended).";
	logIO << Mesg.str() << LogIO::WARN << LogIO::POST;
      }
    //    throw(MSSelectionFieldWarning(Mesg.str()));
    return modifiedIds;
  }
  
} //# NAMESPACE CASACORE - END

