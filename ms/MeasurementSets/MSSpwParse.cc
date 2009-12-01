//# MSSpwParse.cc: Classes to hold results from Spw grammar parser
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

#include <ms/MeasurementSets/MSSpwParse.h>
#include <ms/MeasurementSets/MSSpwIndex.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <ms/MeasurementSets/MSSelectionError.h>
#include <casa/BasicSL/String.h>
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN
  
  MSSpwParse* MSSpwParse::thisMSSParser = 0x0; // Global pointer to the parser object
  TableExprNode* MSSpwParse::node_p = 0x0;
  Vector<Int> MSSpwParse::idList;
  Matrix<Int> MSSpwParse::chanList; 
  //# Constructor
  MSSpwParse::MSSpwParse ()
    : MSParse()
  {
    //    cout << "Spw cleanup()" << endl;
    if (MSSpwParse::node_p!=0x0) delete MSSpwParse::node_p;
    MSSpwParse::node_p=0x0;
    node_p = new TableExprNode();
  }
  
  //# Constructor with given ms name.
  MSSpwParse::MSSpwParse (const MeasurementSet* ms)
    : MSParse(ms, "Spw")
  {
    idList.resize(0);
    //    cout << "Spw cleanup(X)" << endl;
    if(MSSpwParse::node_p) delete MSSpwParse::node_p;
    node_p = new TableExprNode();
    //    setMS(ms);
  }
  
  const TableExprNode *MSSpwParse::selectSpwIdsFromIDList(const Vector<Int>& SpwIds)
  {
    ROMSSpWindowColumns msSpwSubTable(ms()->spectralWindow());
    ROMSDataDescColumns msDataDescSubTable(ms()->dataDescription());

    Vector<Int> mapDDID2SpwID, notFoundIDs;
    Int nSpwRows, nDDIDRows;
    Bool Found;
    TableExprNode condition;
    const String DATA_DESC_ID = MS::columnName(MS::DATA_DESC_ID),
      FLAG_COL = MS::columnName(MS::FLAG);
    
    nSpwRows = msSpwSubTable.nrow();
    nDDIDRows = msDataDescSubTable.nrow();
    mapDDID2SpwID.resize(nDDIDRows);

    for(Int i=0;i<nDDIDRows;i++)
      mapDDID2SpwID(i) = msDataDescSubTable.spectralWindowId()(i);

    for(uInt n=0;n<SpwIds.nelements();n++)
      {
	Found = False;
	for(Int i=0;i<nDDIDRows;i++)
	  if ((SpwIds(n) == mapDDID2SpwID(i)) && 
	      (!msDataDescSubTable.flagRow()(i)) && 
	      (!msSpwSubTable.flagRow()(SpwIds(n)))
	      )
	    {
	      if (condition.isNull())
		condition = ((ms()->col(DATA_DESC_ID)==i));
	      else
		condition = condition || ((ms()->col(DATA_DESC_ID)==i));
	      Found = True;
	      idList.resize(idList.nelements()+1,True);
	      idList(idList.nelements()-1) = mapDDID2SpwID(i);
	      break;
	    }
	if (!Found)
	  {
	    //
	    // Darn!  We don't use standard stuff (STL!)
	    //
	    //notFoundIDs.push_back(SpwIds(n));
	    notFoundIDs.resize(notFoundIDs.nelements()+1,True);
	    notFoundIDs(notFoundIDs.nelements()-1) = SpwIds(n);
	  }
      }

    if (condition.isNull()) 
      {
	ostringstream Mesg;
	Mesg << "No Spw ID(s) matched specifications ";
	throw(MSSelectionSpwError(Mesg.str()));
      }
    if(node_p->isNull())
      *node_p = condition;
    else
      *node_p = *node_p || condition;
    
    return node_p;
  }
  //
  //------------------------------------------------------------------
  //
  const TableExprNode *MSSpwParse::selectSpwIdsFromFreqList(const Vector<Float>& freq,
							    const Float factor)
  {
    ROMSSpWindowColumns msSpwSubTable(ms()->spectralWindow());
    ROMSDataDescColumns msDataDescSubTable(ms()->dataDescription());
    Vector<Float> mapFreq2SpwID;
    Vector<Int> mapDDID2SpwID;
    Int nSpwRows, nDDIDRows;
    Bool Found;
    TableExprNode condition;
    const String DATA_DESC_ID = MS::columnName(MS::DATA_DESC_ID);
    
    nSpwRows = msSpwSubTable.nrow();
    nDDIDRows = msDataDescSubTable.nrow();
    mapDDID2SpwID.resize(nDDIDRows);
    mapFreq2SpwID.resize(nSpwRows);

    for(Int i=0;i<nDDIDRows;i++)
      mapDDID2SpwID(i) = msDataDescSubTable.spectralWindowId()(i);
    for(Int i=0;i<nSpwRows;i++)
      mapFreq2SpwID(i) = msSpwSubTable.refFrequency()(i);

    for(uInt n=0;n<freq.nelements();n++)
      {
	Found = False;
	//
	// Given a freq. value, find the equivalent SpwID
	//
	Int spw;
	for(spw=0;spw<nSpwRows;spw++)
	  if ((freq(n) == mapFreq2SpwID(spw)*factor) &&
	      (!msSpwSubTable.flagRow()(spw)))
	    {
	      Found = True;
	      break;
	    }
	//
	// Now, given the equivalent SpwID, find the equivalent DDID
	//
	if (Found)
	  {
	    for(Int ddid=0;ddid<nDDIDRows;ddid++)
	      if (mapDDID2SpwID(ddid) == spw)
		{
		  if (condition.isNull())
		    condition = ((ms()->col(DATA_DESC_ID)==ddid));
		  else
		    condition = condition || ((ms()->col(DATA_DESC_ID)==ddid));
		  break;
		}
	  }
	if (!Found)
	  {
	    ostringstream Mesg;
	    Mesg << "No Spw ID found";
	    throw(MSSelectionSpwError(Mesg.str()));
	  }
      }

    if(node_p->isNull())
      *node_p = condition;
    else
      *node_p = *node_p || condition;
    
    return node_p;
  }

  void MSSpwParse::selectChannelsFromIDList(Vector<Int>& spwIds,
                                            Vector<Int>& chanIDList,
                                            Int nFSpec)
  {
    Int n=chanList.shape()(0),
      nSpw = spwIds.nelements(),
      m=nSpw*nFSpec,loc=n,k=0;

    chanList.resize(n+m,4,True);
    for(Int i=0;i<nSpw;i++)
      {
	for(Int j=0;j<nFSpec;j++)
	  {
	    chanList(loc,0) = spwIds(i);
	    chanList(loc,1) = chanIDList(k++);
	    chanList(loc,2) = chanIDList(k++);
	    chanList(loc,3) = chanIDList(k++);
	    loc++;
	  }
      }
  }

  
  void MSSpwParse::selectChannelsFromDefaultList(Vector<Int>& spwIds,
                                                 Vector<Int>& chanIDList)
  {
    if (spwIds.nelements() != chanIDList.nelements()/3)
      throw(AipsError("MSSpwParse::selectChannelsFromDefaultList(): SPW and default channel lists should be of the same size"));
    
    Int n=chanList.shape()(0),
      nSpw = spwIds.nelements();
    Int m=nSpw,loc=n,j=0;
    chanList.resize(n+m,4,True);
    for(Int i=0;i<nSpw;i++)
      {
	chanList(loc,0) = spwIds(i);
	chanList(loc,1) = chanIDList(j++);
	chanList(loc,2) = chanIDList(j++);
	chanList(loc,3) = chanIDList(j++);
	loc++;
      }
  }

  
  const TableExprNode* MSSpwParse::node()
  {
    return node_p;
  }
} //# NAMESPACE CASA - END
