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
#include <ms/MeasurementSets/MSSelectionTools.h>
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  MSFieldParse* MSFieldParse::thisMSFParser = 0x0; // Global pointer to the parser object
  TableExprNode* MSFieldParse::node_p = 0x0;
  TableExprNode MSFieldParse::columnAsTEN_p;
  Vector<Int> MSFieldParse::idList;

  //# Constructor
  MSFieldParse::MSFieldParse ()
    : MSParse(), colName(MS::columnName(MS::FIELD_ID))
  {reset();}
  
  //# Constructor with given ms name.
  MSFieldParse::MSFieldParse (const MeasurementSet* ms)
    : MSParse(ms, "Field"), colName(MS::columnName(MS::FIELD_ID)), 
      msFieldSubTable_p(ms->field())
  {reset();}
  
  MSFieldParse::MSFieldParse (const MSField& msFieldSubTable,const TableExprNode& colAsTEN)
    : MSParse(), colName(MS::columnName(MS::FIELD_ID)), 
      msFieldSubTable_p(msFieldSubTable)
  {
    reset(); 
    columnAsTEN_p=colAsTEN;
  }

  void MSFieldParse::reset()
  {
    if (MSFieldParse::node_p!=0x0) delete MSFieldParse::node_p;
    MSFieldParse::node_p=0x0;
    if(node_p) delete node_p;
    node_p = new TableExprNode();
    idList.resize(0);
    //    setMS(ms);
  }

  const TableExprNode *MSFieldParse::selectFieldIds(const Vector<Int>& fieldIds)
  {
    {
      Vector<Int> tmp(set_union(fieldIds,idList));
      idList.resize(tmp.nelements());
      idList = tmp;
    }
    TableExprNode condition;
    //    TableExprNode condition = (msInterface()->asMS()->col(colName).in(fieldIds));
    //    condition = (msInterface()->col(colName).in(fieldIds));
    //    condition = ms()->col(colName).in(fieldIds);
    condition = columnAsTEN_p.in(fieldIds);
    //condition = ms()->col(colName);

    addCondition(*node_p, condition);
    
    // if(node_p->isNull())
    //   *node_p = condition.in(fieldIds);
    // else
    //   *node_p = *node_p || condition.in(fieldIds);
    
    return node_p;
  }
  
  const TableExprNode* MSFieldParse::node()
  {
    return node_p;
  }

} //# NAMESPACE CASA - END

// ---------------OLD CODE START (Feb. 2012)-----------------------
// #include <ms/MeasurementSets/MSFieldParse.h>
// #include <ms/MeasurementSets/MSFieldIndex.h>
// #include <ms/MeasurementSets/MSSourceIndex.h>
// #include <ms/MeasurementSets/MSSelectionTools.h>
// #include <casa/Logging/LogIO.h>

// namespace casa { //# NAMESPACE CASA - BEGIN
  
//   MSFieldParse* MSFieldParse::thisMSFParser = 0x0; // Global pointer to the parser object
//   TableExprNode* MSFieldParse::node_p = 0x0;
//   Vector<Int> MSFieldParse::idList;

//   //# Constructor
//   MSFieldParse::MSFieldParse ()
//     : MSParse(), colName(MS::columnName(MS::FIELD_ID))  {reset();}
  
//   //# Constructor with given ms name.
//   MSFieldParse::MSFieldParse (const MeasurementSet* ms)
//     : MSParse(ms, "Field"), colName(MS::columnName(MS::FIELD_ID))  {reset();}
  
//   void MSFieldParse::reset()
//   {
//     if (MSFieldParse::node_p!=0x0) delete MSFieldParse::node_p;
//     MSFieldParse::node_p=0x0;
//     if(node_p) delete node_p;
//     node_p = new TableExprNode();
//     idList.resize(0);
//     //    setMS(ms);
//   }
//   const TableExprNode *MSFieldParse::selectFieldIds(const Vector<Int>& fieldIds)
//   {
//     {
//       Vector<Int> tmp(set_union(fieldIds,idList));
//       idList.resize(tmp.nelements());
//       idList = tmp;
//     }
//     //    TableExprNode condition = (ms()->col(colName).in(fieldIds));
//     //    TableExprNode condition = (msInterface()->asMS()->col(colName).in(fieldIds));
//     TableExprNode condition = (msInterface()->col(colName).in(fieldIds));
    
//     if(node_p->isNull())
//       *node_p = condition;
//     else
//       *node_p = *node_p || condition;
    
//     return node_p;
//   }
  
//   const TableExprNode* MSFieldParse::node()
//   {
//     return node_p;
//   }
// ---------------OLD CODE END (Feb.2012)-----------------------
  /*
    const TableExprNode *MSFieldParse::selectFieldOrSource(const String& fieldName)
    {
    LogIO os(LogOrigin("MSFieldParse", "selectFieldOrSource", WHERE));
    Vector<Int> SourceIdsFromSN ;
    Vector<Int> SourceIdsFromSC ;
    Vector<Int> FieldIdsFromFN ;
    Vector<Int> FieldIdsFromFC ;
    
    ROMSFieldColumns msFC(ms()->field());
    MSFieldIndex msFI(ms()->field());
    String colName = MS::columnName(MS::FIELD_ID);
    TableExprNode condition = 0;
    
    Bool searchField = False;
    
    
    if( !ms()->source().isNull()) {
    MSSourceIndex msSI(ms()->source());
    SourceIdsFromSN = msSI.matchSourceName(fieldName);
    SourceIdsFromSC = msSI.matchSourceCode(fieldName);;
    //Source name selection  
    if(SourceIdsFromSN.nelements() > 0) {
    condition=(ms()->col(colName).in
    (msFI.matchSourceId(SourceIdsFromSN)));
    } else if (SourceIdsFromSC.nelements() > 0) {
    //Source Code selection  
    condition=(ms()->col(colName).in
    (msFI.matchSourceId(SourceIdsFromSC)));
    } else {
    os << " No Souce name(code) matched, search for field  " << LogIO::POST;
    searchField = True;
    }
    } 
    
    if(ms()->source().isNull() ||searchField) {
    
    FieldIdsFromFN = msFI.matchFieldName(fieldName);
    FieldIdsFromFC = msFI.matchFieldCode(fieldName);
    
    if (FieldIdsFromFN.nelements() > 0) {
    //Field name selection
    condition =
    (ms()->col(colName).in(FieldIdsFromFN));
    } else if (FieldIdsFromFC.nelements() > 0) {
    //Field code selection
    condition =
    (ms()->col(colName).in(FieldIdsFromFC));
    } else {
    os << " No exactly matched field name(code) found! " << LogIO::POST;
    }
    }
    
    if(fieldName.contains('*')) {
    String subFieldName = fieldName.at(0, fieldName.length()-1);
    FieldIdsFromFN = msFI.matchSubFieldName(subFieldName);
    condition =
    (ms()->col(colName).in(FieldIdsFromFN));
    }
    
    if(node_p->isNull())
    *node_p = condition;
    else
    *node_p = *node_p || condition;
    
    return node_p;
    }
  */
