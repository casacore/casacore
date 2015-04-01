//# MSObservationParse.cc: Classes to hold results from scan grammar parser
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
//#
//# $Id$

#include <casacore/ms/MSSel/MSObservationParse.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <limits>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  MSObservationParse* MSObservationParse::thisMSObsParser = 0x0; // Global pointer to the parser object
  TableExprNode MSObservationParse::columnAsTEN_p;
  // TableExprNode* MSObservationParse::node_p = 0x0;
  // Vector<Int> MSObservationParse::idList;
  //  std::vector<Int> MSObservationParse::parsedIDList_p;
  
  //# Constructor
  MSObservationParse::MSObservationParse ()
    : MSParse(), colName(MS::columnName(MS::OBSERVATION_ID)),
      maxObs_p(1000)
  {
    columnAsTEN_p=TableExprNode();
  }
  
  //# Constructor with given ms name.
  MSObservationParse::MSObservationParse (const MeasurementSet* ms, const MSObservation& obsSubTable,
					  const TableExprNode& colAsTEN)
    : MSParse(ms, "Observation"), colName(MS::columnName(MS::OBSERVATION_ID)),
      maxObs_p(1000)
  {
    idList.resize(0);
    parsedIDList_p.resize(0);
    Int nrows = obsSubTable.nrow();
    obsIDList_p.resize(nrows);
    indgen(obsIDList_p);
    columnAsTEN_p=colAsTEN;
    maxObs_p=nrows;
  }

  std::vector<Int>& MSObservationParse::accumulateIDs(const Int id0, const Int id1)
  {
    Vector<Int> theIDs;
    if (id1 < 0) 
      {
  	parsedIDList_p.push_back(id0);theIDs.resize(1);theIDs[0]=id0;
  	// Also accumulate IDs in the global ID list which contains IDs
  	// generated from all expressions (INT, INT DASH INT, and bounds
  	// expressions (>ID, <ID, etc.)).
  	//	appendToIDList(theIDs);
      }
    else
      {
  	// Enumerated list of IDs can be treated as a [ID0, ID1]
  	// (range inclusive of the bounds).
  	//	cerr << "Selecting enumerated range: " << id0 << " " << id1 << endl;
  	selectRangeGEAndLE(id0,id1);
      }
    return parsedIDList_p;
  }

  void MSObservationParse::appendToIDList(const Vector<Int>& v)
  {
    Int currentSize = idList.nelements();
    Int n = v.nelements() + currentSize;
    Int j=0;

    idList.resize(n, True);
    for(Int i=currentSize;i<n;i++) idList[i] = v[j++];
  }

  const TableExprNode* MSObservationParse::selectRangeGTAndLT(const Int& n0,const Int& n1)
  {
    // TableExprNode condition = TableExprNode( (ms()->col(colName) > n0) &&
    // 					     (ms()->col(colName) < n1));
    TableExprNode condition = TableExprNode( (columnAsTEN_p > n0) &&
					     (columnAsTEN_p < n1));
    if ((n0 < 0) || (n1 < 0) || (n1 <= n0))
      {
	ostringstream os;
	os << "ObservationID Expression: Malformed range bounds " << n0 << " (lower bound) and " << n1 << " (upper bound)";
	throw(MSSelectionObservationParseError(os.str()));
      }
    Vector<Int> tmp(n1-n0-1);
    Int j=n0+1;
    for(uInt i=0;i<tmp.nelements();i++) tmp[i]=j++;
    appendToIDList(tmp);

    addCondition(node_p,condition);
    
    return &node_p;
  }
  
  const TableExprNode* MSObservationParse::selectRangeGEAndLE(const Int& n0,const Int& n1)
  {
    // TableExprNode condition = TableExprNode( (ms()->col(colName) >= n0) &&
    // 					     (ms()->col(colName) <= n1));
    TableExprNode condition = TableExprNode( (columnAsTEN_p >= n0) &&
     					     (columnAsTEN_p <= n1));
    if ((n0 < 0) || (n1 < 0) || (n1 <= n0))
      {
	ostringstream os;
	os << "ObservationID Expression: Malformed range bounds " << n0 << " (lower bound) and " << n1 << " (upper bound)";
	throw(MSSelectionObservationParseError(os.str()));
      }
    Vector<Int> tmp(n1-n0+1);
    Int j=n0;
    for(uInt i=0;i<tmp.nelements();i++) tmp[i]=j++;
    appendToIDList(tmp);

    addCondition(node_p,condition);
    
    return &node_p;
  }
  
  const TableExprNode* MSObservationParse::selectObservationIds(const Vector<Int>& scanids)
  {
    if (scanids.size() > 0)
      {
	//	cerr << "Selecting disjoint list: " << scanids << endl;
	//TableExprNode condition = TableExprNode(ms()->col(colName).in(scanids));
	TableExprNode condition = TableExprNode(columnAsTEN_p.in(scanids));
	appendToIDList(scanids);
	addCondition(node_p,condition);
      }
    return &node_p;
  }
  
  const TableExprNode* MSObservationParse::selectObservationIdsGT(const Vector<Int>& scanids)
  {
    //TableExprNode condition = TableExprNode(ms()->col(colName) > scanids[0]);
    TableExprNode condition = TableExprNode(columnAsTEN_p > scanids[0]);
    
    Int n=maxObs_p-scanids[0]+1,j;
    Vector<Int> tmp(n);
    j=scanids[0]+1;
    for(Int i=0;i<n;i++) tmp[i]=j++;
    appendToIDList(tmp);
    addCondition(node_p,condition);
    
    return &node_p;
  }
  
  const TableExprNode* MSObservationParse::selectObservationIdsLT(const Vector<Int>& scanids)
  {
    //TableExprNode condition = TableExprNode(ms()->col(colName) < scanids[0]);
    TableExprNode condition = TableExprNode(columnAsTEN_p < scanids[0]);
    Vector<Int> tmp(scanids[0]);
    for(Int i=0;i<scanids[0];i++) tmp[i] = i;
    appendToIDList(tmp);
    addCondition(node_p,condition);
    
    return &node_p;
  }

  const TableExprNode* MSObservationParse::selectObservationIdsGTEQ(const Vector<Int>& scanids)
  {
    //TableExprNode condition = TableExprNode(ms()->col(colName) >= scanids[0]);
    TableExprNode condition = TableExprNode(columnAsTEN_p >= scanids[0]);
    
    Int n=maxObs_p-scanids[0]+1,j;
    Vector<Int> tmp(n);
    j=scanids[0];
    for(Int i=0;i<n;i++) tmp[i]=j++;
    appendToIDList(tmp);
    addCondition(node_p,condition);
    
    return &node_p;
  }
  
  const TableExprNode* MSObservationParse::selectObservationIdsLTEQ(const Vector<Int>& scanids)
  {
    //TableExprNode condition = TableExprNode(ms()->col(colName) <= scanids[0]);
    TableExprNode condition = TableExprNode(columnAsTEN_p <= scanids[0]);
    Vector<Int> tmp(scanids[0]+1);
    for(Int i=0;i<=scanids[0];i++) tmp[i] = i;
    appendToIDList(tmp);
    addCondition(node_p,condition);
    
    return &node_p;
  }
  
  Vector<Int> MSObservationParse::selectedIDs() 
  {
    return set_intersection(obsIDList_p,idList);
  }

  const TableExprNode MSObservationParse::node()
  {
    return node_p;
  }
  
} //# NAMESPACE CASACORE - END
