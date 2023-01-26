//# MSPolnParse.cc: Classes to hold results from Poln grammar parseing
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#include <casacore/ms/MSSel/MSPolnParse.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MSSel/MSSpwGram.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //  MSPolnParse* MSPolnParse::thisMSSParser = 0x0; // Global pointer to the parser object
  // TableExprNode* MSPolnParse::node_p = 0x0;
  // Vector<int32_t> MSPolnParse::ddIDList;
  // std::map<int32_t, Vector<int32_t> > MSPolnParse::polList(Vector<int32_t>(0)); 
  //# Constructor
  //------------------------------------------------------------------------------
  //  
  extern const char*           strpMSPolnGram;
  MSPolnParse::MSPolnParse ()
    : MSParse(),
      node_p(),			      //      node_p(0x0), 
      ddIDList_p()
  {
    // if (MSPolnParse::node_p!=0x0) delete MSPolnParse::node_p;
    // MSPolnParse::node_p=0x0;
    // node_p = new TableExprNode();
  }
  //# Constructor with given ms name.
  //------------------------------------------------------------------------------
  //  
  MSPolnParse::MSPolnParse (const MeasurementSet* ms)
    : MSParse(ms, "Pol"),
      node_p(),			      //     node_p(0x0), 
      ddIDList_p()
  {
    ddIDList_p.resize(0);
    // if(MSPolnParse::node_p) delete MSPolnParse::node_p;
    // node_p = new TableExprNode();
  }
  //
  //------------------------------------------------------------------------------
  //  
  const TableExprNode MSPolnParse::selectFromIDList(const Vector<int32_t>& ddIDs)
  {
    TableExprNode condition;
    if (ddIDs.nelements() > 0)
      condition = ms()->col(MS::columnName(MS::DATA_DESC_ID)).in(ddIDs);

    // int32_t n=ddIDs.nelements();
    // const String DATA_DESC_ID = MS::columnName(MS::DATA_DESC_ID);
    // if (n > 0)
    //   {
    // 	for(int32_t i=0; i<n; i++)
    // 	  if (condition.isNull())
    // 	    condition = ((ms()->col(DATA_DESC_ID)==ddIDs[i]));
    // 	  else
    // 	    condition = condition || ((ms()->col(DATA_DESC_ID)==ddIDs[i]));
    //   }

    if (condition.isNull()) 
      throw(MSSelectionPolnError(String("No match for the [SPW:]POLN specifications ")));

    // if(node_p->isNull()) *node_p = condition;
    // else                 *node_p = *node_p || condition;
    
    // return node_p;
    if(node_p.isNull()) node_p = condition;
    else                node_p = node_p || condition;
    
    return node_p;
  }
  //
  //------------------------------------------------------------------------------
  //  
  Vector<int32_t> MSPolnParse::getMapToDDIDs(MSDataDescIndex& msDDNdx, 
					 MSPolarizationIndex& /*msPolNdx*/,
					 const Vector<int32_t>& spwIDs, 
					 Vector<int32_t>& polnIDs,
					 Vector<int32_t>& polnIndices)
  {
    Vector<int32_t> ddIDs;
    Vector<int32_t> thisDDList;
    Vector<int32_t> validPolIDs, validPolIndices;
    if (polnIDs.nelements() == 0)
      {
	ostringstream mesg;
	mesg << "No match for polarization ID(s) ";
	throw(MSSelectionPolnParseError(String(mesg.str())));
      }
    for (uint32_t p=0; p<polnIDs.nelements(); p++)
      {
	thisDDList.resize(0);
	for (uint32_t s=0; s<spwIDs.nelements(); s++)
	  {
	    int32_t n;
	    Vector<int32_t> tmp=msDDNdx.matchSpwIdAndPolznId(spwIDs[s],polnIDs[p]);
	    if (tmp.nelements() > 0)
	      {
		ddIDs.resize((n=ddIDs.nelements())+1,true);
		ddIDs[n]=tmp[0];
		thisDDList.resize((n=thisDDList.nelements())+1,true);
		thisDDList[n]=tmp[0];
	      }
	  }
	if (thisDDList.nelements() > 0) 
	  {
	    uint32_t n;
	    setIDLists(polnIDs[p], 1, thisDDList);
	    validPolIDs.resize((n=validPolIDs.nelements())+1,true);
	    validPolIDs[n]=polnIDs[p];
	    validPolIndices.resize((n=validPolIndices.nelements())+1,true);
	    validPolIndices[n]=polnIndices[p];
	    //	    cout << "Found DDID for PolID " << polnIDs[p] << endl;
	  }
	// else
	//   cout << "Not found DDID for PolID " << polnIDs[p] << endl;
      }
    polnIDs.resize(0); polnIDs=validPolIDs;
    polnIndices.resize(0); polnIndices=validPolIndices;
    return ddIDs;
  }
  //
  //------------------------------------------------------------------------------
  //  
  Vector<int32_t> MSPolnParse::getMapToDDIDsV2(const String& polnExpr, 
					   const Vector<int32_t>& spwIDs, 
					   Vector<int32_t>& polnIDs,
					   Vector<int32_t>& polnIndices)
  {
    Vector<int32_t> ddIDs, polTypes;
    Vector<int32_t> thisDDList;
    Vector<int32_t> validPolIDs;//, validPolIndices;
    MSDataDescIndex msDDNdx(ms()->dataDescription());
    MSPolarizationIndex msPolNdx(ms()->polarization());
    //    cout << "SpwIDs = " << spwIDs << endl;
    polnIDs = getPolnIDsV2(polnExpr, polTypes);
    //    cout << "PolIDs = " << polnIDs << " polTypes = " << polTypes << endl;
    //   if (polnIDs.nelements() == 0)
   if (polTypes.nelements() == 0)
      {
	ostringstream mesg;
	mesg << "No match for polarization ID(s) ";
	throw(MSSelectionPolnParseError(String(mesg.str())));
      }
    for (uint32_t p=0; p<polnIDs.nelements(); p++)
      {
	Vector<int32_t> tt;
	tt = getPolnIndices(polnIDs[p],polTypes);
	//	cout << "Poln indices for " << polnIDs[p] << " = " << tt << endl;
	polnIndices.resize(0);
        polnIndices=tt;
	thisDDList.resize(0);
	for (uint32_t s=0; s<spwIDs.nelements(); s++)
	  {
	    int32_t n;
	    Vector<int32_t> tmp=msDDNdx.matchSpwIdAndPolznId(spwIDs[s],polnIDs[p]);
	    if (tmp.nelements() > 0)
	      {
		ddIDs.resize((n=ddIDs.nelements())+1,true);
		ddIDs[n]=tmp[0];
		thisDDList.resize((n=thisDDList.nelements())+1,true);
		thisDDList[n]=tmp[0];
		setIDLists((int32_t)polnIDs[p],0,polnIndices);
		polMap_p[polnIDs[p]].resize(0);
		polMap_p[polnIDs[p]]=polnIndices;
		//		cout << "DDIDs for SPW = " << spwIDs[s] << " = " << tmp[0] << endl;
	      }
	  }
	if (thisDDList.nelements() > 0) 
	  {
	    uint32_t n;
	    setIDLists(polnIDs[p], 1, thisDDList);
	    validPolIDs.resize((n=validPolIDs.nelements())+1,true);
	    validPolIDs[n]=polnIDs[p];
	    // validPolIndices.resize((n=validPolIndices.nelements())+1,true);
	    // validPolIndices[n]=polnIndices[p];
	  }
	// else
	//   cout << "Not found DDID for PolID " << polnIDs[p] << endl;
      }
    if (ddIDs.nelements() == 0)
      {
	ostringstream mesg;
	mesg << "No match for polarization ID(s) ";
	//	strpMSPolnGram = polnExpr.c_str();
	throw(MSSelectionPolnParseError(String(mesg.str())));
      }
    polnIDs.resize(0); polnIDs=validPolIDs;
    // polnIndices.resize(0); polnIndices=validPolIndices;
    return ddIDs;
  }
  //
  //------------------------------------------------------------------------------
  //  Give a list of pol IDs, return the list of row numbers in the
  //  POLARIZATION sub-table which contains the listed Pol IDs.  Pol
  //  IDs are defined as the enumrations Stokes::StokesTypes -
  //  i.e. "RR", "LL" etc.
  //
  Vector<int32_t> MSPolnParse::matchPolIDsToPolTableRow(const Vector<int32_t>& polIds,
						    std::map<int32_t, Vector<int32_t> >& /*polIndexMap*/,
						    Vector<int32_t>& polIndices,
						    bool addToMap)
  {
    Vector<int32_t> rowList;
    MSPolarization mspol(ms()->polarizationTableName());
    MSPolarizationColumns mspolC(mspol);
    //
    // First extract the corrType column of the Polarization sub-table
    // row-by-row (since this column can be of variable shape!)
    //
    for (uint32_t row=0; row<mspolC.nrow();row++)
      {
	Vector<int32_t> corrType;
	mspolC.corrType().get(row,corrType);
	//
	// Next - look for match between the supplied polId list in
	// the extracted corrType.  User support: Do not assume the
	// order of the supplied pol IDs (human free-will was involved
	// in generating that list!).  Also do a max-match.  E.g. a
	// supplied polID list from "RR LL" should match all of the
	// following corrType lists: "RR LL", "RR LL LR RL", "RR",
	// "LL".
	//
	bool allFound=false;
	uint32_t foundCounter=0;
	//	Vector<int32_t> polIndices(0,-1);
	for(uint32_t i=0; i<polIds.nelements(); i++)
	  {
	    for(uint32_t j=0; j<corrType.nelements(); j++)
	      if (polIds[i] == corrType[j])
		{
		  int32_t m=0;
		  polIndices.resize((m=polIndices.nelements())+1,true);
		  polIndices[m]=j;
		  foundCounter++;
		  break;
		}
	  }

	if ((allFound=(foundCounter == polIds.nelements())))
	  {
	    if (addToMap) setIDLists((int32_t)row,0,polIndices);
	  }
	if (allFound)
	  {
	    uint32_t n;
	    rowList.resize((n=rowList.nelements())+1,true);
	    rowList[n]=row;
	  }
      }

    return rowList;
  }
  //
  //------------------------------------------------------------------------------
  //  
  Vector<int32_t> MSPolnParse::getPolnIndices(const int32_t& polId, const Vector<int32_t>& polnTypes)
  {
    MSPolarization mspol(ms()->polarizationTableName());
    MSPolarizationColumns mspolC(mspol);
    Vector<int32_t> polIndices;

    //    for (uint32_t row=0; row<mspolC.nrow();row++)
      {
	Vector<int32_t> corrType;
	mspolC.corrType().get(polId,corrType);
	for(uint32_t i=0; i<polnTypes.nelements(); i++)
	  for(uint32_t j=0; j<corrType.nelements(); j++)
	    if (polnTypes[i] == corrType[j])
	      {
		int32_t m=0;
		polIndices.resize((m=polIndices.nelements())+1,true);
		polIndices[m]=j;
		break;
	      }
      }
    return polIndices;
  }
  //
  //------------------------------------------------------------------------------
  //  
  Vector<int32_t> MSPolnParse::getPolnIDs(const String& polSpec, Vector<int32_t>& polIndices)
  {
    String sep(",");
    Vector<String> tokens;
    Vector<int32_t> idList, polIDList;
    //
    // Split the given string into ";" separated tokens.  Upcase the
    // string before splitting.
    //
    tokenize(polSpec,sep,tokens,true);
    idList.resize(tokens.nelements());
    for(uint32_t i=0;i<idList.nelements();i++)
      idList[i]=Stokes::type(tokens[i]);

    //
    //  Generate a list of DDIDs which will be used to the actual row
    //  selection.  Also make a map of the poln IDs and list of in-row
    //  indices which will then be used for in-row selection.
    //
    polIDList=matchPolIDsToPolTableRow(idList,polMap_p, polIndices);
    //    cout << "IDList=" << idList << " " << polIDList << " " << polIndices << endl;
    return polIDList;
  }
  //  
//------------------------------------------------------------------------------
  //  
  Vector<int32_t> MSPolnParse::getPolnIDsV2(const String& polSpec, Vector<int32_t>& polTypes)
  {
    String sep(",");
    Vector<String> tokens;
    Vector<int32_t> polIDList, polIndices;
    //
    // Split the given string into ";" separated tokens.  Upcase the
    // string before splitting.
    //
    tokenize(polSpec,sep,tokens,true);
    polTypes.resize(tokens.nelements());
    for(uint32_t i=0;i<polTypes.nelements();i++)
      polTypes[i]=Stokes::type(tokens[i]);
    polIDList=matchPolIDsToPolTableRow(polTypes,polMap_p, polIndices);
    return polIDList;
  }
  //
  //------------------------------------------------------------------------------
  //  
  // The actual parser.  Does three things:
  //   1. Tokenize the [SPW:]POLN string into SPW and POLN tokens.
  //   2. Generate list of indices using SPW and POLN tokens.
  //   3. Generate the {SPW, POLN} --> DDID map
  //
  int32_t MSPolnParse::theParser(const String& command) 
  {
    int32_t ret=0, nSpecList=0;
    Vector<String> polnSpecList;
    String sep(";");

    nSpecList=tokenize(command,sep,polnSpecList);

    for(int32_t i=0;i<nSpecList;i++)
      {
	Vector<String> tokens,tmp;
	Vector<int32_t> spwIDs, spwDDIDs;
	Matrix<int32_t> chanIDs;
	Vector<int32_t> polnIDs;

	String s(":"), spwExpr, polnExpr;
	int32_t nTokens;
	//
	// User suppport: Check if they tried [SPW:CHAN:]POLN kind of
	// specification.  Darn - String::freq(...) does not work!
	//
	tokenize(polnSpecList[i],s,tokens);
	tokenize(tokens[0],s,tmp);
	nTokens = tokens.nelements();

	if (nTokens > 2)
	  throw(MSSelectionPolnParseError(String("Too many ':'s.  [Tip: Channel "
	  					 "specification is not useful "
	  					 "and not allowed.]")));
	//
	// If there were two ":" separate tokens, they were of the form SPW:POLN
	//
	if (nTokens == 2)
	  {
	    spwExpr = tokens[0];
	    polnExpr= tokens[1];
	  }
	//
	// If there was only one token, it was POLN - equivalent of *:POLN
	//
	if (nTokens == 1)
	  {
	    spwExpr="*";
	    polnExpr=tokens[0];
	  }
	//
	// Parse the SPW part.  Pass the token to the SPW parser.
	//
	try
	  {
	    TableExprNode colAsTEN = ms()->col(ms()->columnName(MS::DATA_DESC_ID));
	    spwIDs.resize(0);
	    // if (spwExpr_p != "" &&
	    //     msSpwGramParseCommand(ms, spwExpr_p,spwIDs_p, chanIDs_p) == 0)
	    msSpwGramParseCommand(ms()->spectralWindow(), 
				  ms()->dataDescription(),
				  colAsTEN, spwExpr,
				  spwIDs, chanIDs, spwDDIDs);
	    //	    msSpwGramParseCommand(ms(), spwExpr,spwIDs, chanIDs);
	  }
	catch (MSSelectionSpwError &x)
	  {
	    throw(MSSelectionPolnParseError(x.what()));
	  }
	//
	// Parse the POLN part.
	//
	try
	  {
	    Vector<int32_t> polIndices;
	    Vector<int32_t> tddIDList,tt;
	    // polnIDs=getPolnIDs(polnExpr, polIndices);
	    // MSDataDescIndex msDDNdx(ms()->dataDescription());
	    // MSPolarizationIndex msPolNdx(ms()->polarization());
	    // cout << "PolIDs = " << polnIDs << endl;
	    
	    //	    tddIDList=getMapToDDIDs(msDDNdx, msPolNdx, spwIDs, polnIDs, polIndices);
	    //	    cout << "PolExpr = " << polnExpr << endl;
	    tddIDList=getMapToDDIDsV2(polnExpr, spwIDs, polnIDs, polIndices);
	    //	    cout << "DDIDs = " << tddIDList << endl;
	    //	    cout << "-----------------------------------" << endl;
	    tt=set_union(tddIDList, ddIDList_p);
	    ddIDList_p.resize(0);
	    ddIDList_p = tt;
	  }
	catch (MSSelectionPolnParseError& x)
	  {
	    String mesg("(named " + polnExpr + ")");
	    //	    mesg = mesg + polnExpr + ")";
	    x.addMessage(mesg);
	    throw;
	  }
	selectFromIDList(ddIDList_p);
      }
    {
      //
      // Remove entries which did not map to any DD ID(s)
      //
      for (const auto& x : setupMap_p) {
	if (x.second[1].nelements() == 0) {
          setupMap_p.erase(x.first);
        }
      }
    }
    return ret;
  }
  //
  //------------------------------------------------------------------------------
  //  A convenience method to set the vectors of Poln or DD IDs in the setupMap.
  //
  void MSPolnParse::setIDLists(const int32_t key, const int32_t ndx, Vector<int32_t>& val)
  {
    if (ndx>1)
      throw(MSSelectionError("Internal error in MSPolnParse::setIDLists(): Index > 1"));

    if (setupMap_p[key].nelements() !=2) setupMap_p[key].resize(2, true);
    if (val.nelements() > 0)
      {
	Vector<int32_t> v0=val;
	auto elem = setupMap_p.find(key);
        if (elem != setupMap_p.end())
	  {
	    Vector<int32_t> t0;
	    v0.resize(0);
	    v0 = elem->second[ndx];
	    t0=set_union(val,v0);
	    v0.resize(0);
	    v0 = t0;
	  }

	if (setupMap_p[key][ndx].nelements() > 0) setupMap_p[key][ndx].resize(0);
	setupMap_p[key][ndx]=v0;
      }
  }
  //
  //------------------------------------------------------------------------------
  //
  const TableExprNode MSPolnParse::node() { return node_p; }
} //# NAMESPACE CASACORE - END
