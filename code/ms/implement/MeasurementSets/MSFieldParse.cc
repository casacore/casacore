//# MSFieldParse.cc: Classes to hold results from field grammar parser
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

#include <ms/MeasurementSets/MSFieldParse.h>
#include <ms/MeasurementSets/MSFieldIndex.h>
#include <ms/MeasurementSets/MSSourceIndex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode* MSFieldParse::node_p = 0x0;

//# Constructor
MSFieldParse::MSFieldParse ()
: MSParse(), colName(MS::columnName(MS::FIELD_ID))
{
}

//# Constructor with given ms name.
MSFieldParse::MSFieldParse (const MeasurementSet* ms)
: MSParse(ms, "Field"), colName(MS::columnName(MS::FIELD_ID))
{
    if(node_p) delete node_p;
    node_p = new TableExprNode();
}

const TableExprNode *MSFieldParse::selectFieldIds(const Vector<Int>& fieldIds)
{
    TableExprNode condition = (ms()->col(colName).in(fieldIds));

    if(node_p->isNull())
        *node_p = condition;
    else
        *node_p = *node_p || condition;

    return node_p;
}

const TableExprNode *MSFieldParse::selectFieldOrSource(const String& fieldName)
{

  Vector<Int> SourceIdsFromSN ;
  Vector<Int> SourceIdsFromSC ;
  Vector<Int> SourceIdsFromFN ;
  Vector<Int> SourceIdsFromFC ;

  MSFieldIndex msFI(ms()->field());
  cout << " field table created " << endl;
  String colName = MS::columnName(MS::FIELD_ID);
  cout << " column name " << colName << endl;
  TableExprNode condition = 0;
  
  Bool searchField = False;

  if( !ms()->source().isNull()) {
    MSSourceIndex msSI(ms()->source());
    cout << " source table created " << endl;
    SourceIdsFromSN = msSI.matchSourceName(fieldName);
    SourceIdsFromSC = msSI.matchSourceCode(fieldName);;
    //Source name selection  
    if(SourceIdsFromSN.nelements() > 0) {
      cout << " source name found " << endl;
      condition=(ms()->col(colName).in
		 (msFI.matchSourceId(msSI.matchSourceName(fieldName))));
    } else if (SourceIdsFromSC.nelements() > 0) {
      //Source Code selection  
      cout << " source code found " << endl;
      condition=(ms()->col(colName).in
		 (msFI.matchSourceId(msSI.matchSourceCode(fieldName))));
    } else {
      cout << " No matched Souce name(code), search for field  "<< endl;
      searchField = True;
    }
  } 

  if(ms()->source().isNull() ||searchField) {
    
    SourceIdsFromFN = msFI.matchFieldName(fieldName);
    SourceIdsFromFC = msFI.matchFieldCode(fieldName);
    
    if (SourceIdsFromFN.nelements() > 0) {
      //Field name selection
      cout << " field name found " << endl;
      condition =
	(ms()->col(colName).in(msFI.matchFieldName(fieldName)));
    } else if (SourceIdsFromFC.nelements() > 0) {
      //Field code selection
      cout << " field code found " << endl;
      condition =
	(ms()->col(colName).in(msFI.matchFieldCode(fieldName)));
    } else {
      cout << " No matched field name(code) or Souce name(code) "<< endl;
    }
  }

  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node_p;
}

const TableExprNode* MSFieldParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
