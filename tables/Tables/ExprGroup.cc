//# ExprGroup.cc: Classes for TaQL's GROUPBY clause
//# Copyright (C) 2013
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
//# $Id: TaQLNode.h 21051 2011-04-20 11:46:29Z gervandiepen $

//# Includes
#include <tables/Tables/ExprGroup.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/TableError.h>
#include <casa/Utilities/Sort.h>
#include <limits>


namespace casa { //# NAMESPACE CASA - BEGIN

  // Aggregation and GROUPBY/HAVING clause are handled as follows:
  // Possibly support ROLLUP (to generate subtotals); cannot be used with median
  //  if supported, give such columns the value 0 ("TOTAL" for string).
  // - an aggregation function is detected in select (GROUPBY not needed)
  // -    detected by TaQLNode??? Probably not, otherwise cannot use MAX or so.
  // - thus ExprFuncNode must support gmin, etc and know it is an aggr function.
  //   maybe have an ExprFuncNode::isAggregate, user defined funcs can be used??
  //   support of user defined aggr funcs makes life more complicated.
  // - if GROUPBY or HAVING is given, select must contain an aggr function
  // - TableParse will know if the SELECT and/or HAVING have an aggregate
  // - HAVING can refer to columns in select, but can also have its own aggr.
  // TableParse has map<GroupKeySet,GroupAggr>, fills a GroupKeySet object
  // from the GROUPBY columns and does:
  //    for (i=0..nrow) {
  //      GroupKeySet key = ...
  //      akey = map.find(key);
  //      if (akey == map.end()
  //        akey = map.insert (key, groupaggr);
  //        akey->second.setRow (row);
  //      }
  //      akey.apply (row);
  //    }
  // Aggr func is allowed in select,having, not in where,join,orderby,groupby.
  // Is it allowed at highest level only or also in expressions?
  bool TableExprGroupKey::operator== (const TableExprGroupKey& that) const
  {
    switch (itsDT) {
    case TableExprNodeRep::NTBool:
      return itsBool == that.itsBool;
    case TableExprNodeRep::NTInt:
      return itsInt64 == that.itsInt64;
    case TableExprNodeRep::NTDouble:
      return itsDouble == that.itsDouble;
    default:
      return itsString == that.itsString;
    }
  }

  bool TableExprGroupKey::operator< (const TableExprGroupKey& that) const
  {
    switch (itsDT) {
    case TableExprNodeRep::NTBool:
      return itsBool < that.itsBool;
    case TableExprNodeRep::NTInt:
      return itsInt64 < that.itsInt64;
    case TableExprNodeRep::NTDouble:
      return itsDouble < that.itsDouble;
    default:
      return itsString < that.itsString;
    }
  }


  TableExprGroupKeySet::TableExprGroupKeySet (const vector<TableExprNode>& nodes)
  {
    itsKeys.reserve (nodes.size());
    for (uInt i=0; i<nodes.size(); ++i) {
      addKey (nodes[i].getNodeRep()->dataType());
    }
  }

  void TableExprGroupKeySet::fill (const vector<TableExprNode>& nodes,
                                   const TableExprId& id)
  {
    DebugAssert (nodes.size() == itsKeys.size(), AipsError);
    for (uInt i=0; i<itsKeys.size(); ++i) {
      switch (itsKeys[i].dataType()) {
      case TableExprNodeRep::NTBool:
        itsKeys[i].set (nodes[i].getBool(id));
        break;
      case TableExprNodeRep::NTInt:
        itsKeys[i].set (nodes[i].getInt(id));
        break;
      case TableExprNodeRep::NTDouble:
        itsKeys[i].set (nodes[i].getDouble(id));
        break;
        ///      case TableExprNodeRep::NTDComplex:
        ///        itsKeys[i].set (nodes[i].getComplex(id));
        ///        break;
      case TableExprNodeRep::NTString:
        itsKeys[i].set (nodes[i].getString(id));
        break;
        ///      case TableExprNodeRep::NTDate:
        ///        itsKeys[i].set (nodes[i].getDate(id));
        ///        break;
      default:
        throw TableInvExpr ("TableExprGroupKeySet: unknown data type");
      }
    }
  }

  bool TableExprGroupKeySet::operator== (const TableExprGroupKeySet& that) const
  {
    AlwaysAssert (itsKeys.size() == that.itsKeys.size(), AipsError);
    for (size_t i=0; i<itsKeys.size(); ++i) {
      if (!(itsKeys[i] == that.itsKeys[i])) return false;
    }
    return true;
  }

  bool TableExprGroupKeySet::operator< (const TableExprGroupKeySet& that) const
  {
    AlwaysAssert (itsKeys.size() == that.itsKeys.size(), AipsError);
    for (size_t i=0; i<itsKeys.size(); ++i) {
      if (itsKeys[i] < that.itsKeys[i]) return true;
      if (that.itsKeys[i] < itsKeys[i]) return false;
    }
    return false;
  }


  TableExprGroupFunc::~TableExprGroupFunc()
  {}
  void TableExprGroupFunc::finish()
  {}
  Bool TableExprGroupFunc::getBool()
  { throw TableInvExpr ("TableExprGroupFunc::getBool not implemented"); }
  Int64 TableExprGroupFunc::getInt()
  { throw TableInvExpr ("TableExprGroupFunc::getInt not implemented"); }
  Double TableExprGroupFunc::getDouble()
  { throw TableInvExpr ("TableExprGroupFunc::getDouble not implemented"); }
  DComplex TableExprGroupFunc::getDComplex()
  { throw TableInvExpr ("TableExprGroupFunc::getDComplex not implemented"); }
  MVTime TableExprGroupFunc::getDate()
  { throw TableInvExpr ("TableExprGroupFunc::getDate not implemented"); }
  String TableExprGroupFunc::getString()
  { throw TableInvExpr ("TableExprGroupFunc::getString not implemented"); }

  TableExprGroupFuncBool::~TableExprGroupFuncBool()
  {}
  Bool TableExprGroupFuncBool::getBool()
    { return itsValue; }

  TableExprGroupFuncInt::~TableExprGroupFuncInt()
  {}
  Int64 TableExprGroupFuncInt::getInt()
    { return itsValue; }
  Double TableExprGroupFuncInt::getDouble()
    { return itsValue; }

  TableExprGroupFuncDouble::~TableExprGroupFuncDouble()
  {}
  Double TableExprGroupFuncDouble::getDouble()
    { return itsValue; }

  TableExprGroupFuncDComplex::~TableExprGroupFuncDComplex()
  {}
  DComplex TableExprGroupFuncDComplex::getDComplex()
    { return itsValue; }

  TableExprGroupFuncString::~TableExprGroupFuncString()
  {}
  String TableExprGroupFuncString::getString()
    { return itsValue; }


  TableExprGroupFuncSet::TableExprGroupFuncSet
  (const vector<TableExprAggrNode*>& aggrNodes)
    : itsId (0)
  {
    itsFuncs.reserve (aggrNodes.size());
    for (uInt i=0; i<aggrNodes.size(); ++i) {
      itsFuncs.push_back (aggrNodes[i]->makeGroupFunc());
    }
  }

  /*
  TableExprGroupFuncSet::TableExprGroupFuncSet
  (const TableExprGroupFuncSet& that)
    : itsId (-1)
  {
    operator= (that);
  }

  TableExprGroupFuncSet& TableExprGroupFuncSet::operator=
  (const TableExprGroupFuncSet& that)
  {
    if (this != &that) {
      itsId = that.itsId;
      itsFuncs.resize (that.itsFuncs.size());
      for (uInt i=0; i<itsFuncs.size(); ++i) {
        itsFuncs[i] = that.itsFuncs[i]->clone();
      }
    }
    return *this;
  }
  */

  void TableExprGroupFuncSet::apply (const vector<TableExprAggrNode*>& nodes,
                                     const TableExprId& id)
  {
    itsId = id;
    for (uInt i=0; i<nodes.size(); ++i) {
      itsFuncs[i]->apply (*nodes[i], id);
    }
  }


} //# NAMESPACE CASA - END
