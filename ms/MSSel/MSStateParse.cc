//# MSStateParse.cc: Classes to hold results from field grammar parser
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

#include <casacore/ms/MSSel/MSStateParse.h>
#include <casacore/ms/MSSel/MSStateIndex.h>
#include <casacore/ms/MSSel/MSSourceIndex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  MSStateParse* MSStateParse::thisMSSIParser = 0x0; // Global pointer to the parser object
  TableExprNode* MSStateParse::node_p = 0x0;
  Vector<Int> MSStateParse::idList;
  MSSelectionErrorHandler* MSStateParse::thisMSSErrorHandler = 0;

  //# Constructor
  MSStateParse::MSStateParse ()
    : MSParse(), colName(MS::columnName(MS::STATE_ID))
  {
    if (MSStateParse::node_p!=0x0) delete MSStateParse::node_p;
    MSStateParse::node_p=0x0;
    node_p = new TableExprNode();
  }
  
  //# Constructor with given ms name.
  MSStateParse::MSStateParse (const MeasurementSet* ms)
    : MSParse(ms, "State"), colName(MS::columnName(MS::STATE_ID))
  {
    if (MSStateParse::node_p!=0x0) delete MSStateParse::node_p;
    MSStateParse::node_p=0x0;
    if(node_p) delete node_p;
    node_p = new TableExprNode();
    idList.resize(0);
    //    setMS(ms);
  }
  
  const TableExprNode *MSStateParse::selectStateIds(const Vector<Int>& stateIds)
  {
    {
      Vector<Int> tmp(set_union(stateIds,idList));
      idList.resize(tmp.nelements());
      idList = tmp;
    }
    TableExprNode condition = (ms()->col(colName).in(stateIds));
    
    if(node_p->isNull()) *node_p = condition;
    else                 *node_p = *node_p || condition;
    
    return node_p;
  }
  
  const TableExprNode* MSStateParse::node()
  {
    return node_p;
  }
} //# NAMESPACE CASACORE - END
