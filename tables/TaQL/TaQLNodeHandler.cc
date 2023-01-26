//# TaQLNodeHandler.cc: Class to handle the nodes in the raw TaQL parse tree
//# Copyright (C) 2005
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

#include <casacore/tables/TaQL/TaQLNodeHandler.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/TaQL/TableParseSortKey.h>
#include <casacore/tables/TaQL/TableParseUpdate.h>
#include <casacore/tables/TaQL/TableParseUtil.h>
#include <casacore/tables/TaQL/TaQLShow.h>
#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/StringDistance.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TaQLNodeHandler::~TaQLNodeHandler()
  {
    clearStack();
  }

  TaQLNodeResult TaQLNodeHandler::handleTree (const TaQLNode& node,
                                  const std::vector<const Table*>& tempTables)
  {
    clearStack();
    itsTempTables = tempTables;
    return node.visit (*this);
  }
    

  TableParseQuery* TaQLNodeHandler::pushStack
                                    (TableParseQuery::CommandType type)
  {
    TableParseQuery* sel = new TableParseQuery(type);
    itsStack.push_back (sel);
    return sel;
  }
  TableParseQuery* TaQLNodeHandler::topStack() const
  {
    AlwaysAssert (itsStack.size() > 0, AipsError);
    return itsStack[itsStack.size()-1];
  }
  void TaQLNodeHandler::popStack()
  {
    delete topStack();
    itsStack.resize (itsStack.size()-1);
  }
  void TaQLNodeHandler::clearStack()
  {
    for (int32_t i=itsStack.size()-1; i>=0; --i) {
      delete itsStack[i];
    }
    itsStack.resize (0);
  }


  TaQLNodeResult TaQLNodeHandler::visitConstNode (const TaQLConstNodeRep& node)
  {
    TableExprNode expr;
    switch (node.itsType) {
    case TaQLConstNodeRep::CTBool:
      expr = TableExprNode(node.itsBValue);
      break;
    case TaQLConstNodeRep::CTInt:
      expr = TableExprNode(node.itsIValue);
      break;
    case TaQLConstNodeRep::CTReal:
      expr = TableExprNode(node.itsRValue);
      break;
    case TaQLConstNodeRep::CTComplex:
      expr = TableExprNode(node.itsCValue);
      break;
    case TaQLConstNodeRep::CTString:
      expr = TableExprNode(node.itsSValue);
      break;
    case TaQLConstNodeRep::CTTime:
      expr = TableExprNode(node.itsTValue);
      break;
    }
    if (! node.getUnit().empty()) {
      expr = expr.useUnit (node.getUnit());
    }
    return new TaQLNodeHRValue (expr);
  }

  TaQLNodeResult TaQLNodeHandler::visitRegexNode (const TaQLRegexNodeRep& node)
  {
    // Remove delimiters.
    String str = node.itsValue.substr(2, node.itsValue.size()-3);
    if (node.itsValue[0] == 'd') {
      return new TaQLNodeHRValue (TableExprNode(TaqlRegex(
                             StringDistance(str,
                                            node.itsMaxDistance,
                                            true,
                                            node.itsIgnoreBlanks,
                                            node.itsCaseInsensitive))));
    } else if (node.itsValue[0] == 'p') {
      str = Regex::fromPattern (str);
    } else if (node.itsValue[0] == 'm') {
      str = ".*(" + str + ").*";
    }
    if (node.itsCaseInsensitive) {
      str = Regex::makeCaseInsensitive(str);
    }
    return new TaQLNodeHRValue (TableExprNode(TaqlRegex(Regex(str, true))));
  }

  TaQLNodeResult TaQLNodeHandler::visitUnaryNode (const TaQLUnaryNodeRep& node)
  {
    bool notexists = true;
    TaQLNodeResult res = visitNode (node.itsChild);
    TableExprNode expr = getHR(res).getExpr();
    switch (node.itsType) {
    case TaQLUnaryNodeRep::U_MINUS:
      return new TaQLNodeHRValue (-expr);
    case TaQLUnaryNodeRep::U_NOT:
      return new TaQLNodeHRValue (!expr);
    case TaQLUnaryNodeRep::U_EXISTS:
      notexists= false;
      break;
    case TaQLUnaryNodeRep::U_NOTEXISTS:
      break;
    case TaQLUnaryNodeRep::U_BITNOT:
      return new TaQLNodeHRValue (~expr);
    }
    TableExprNode exres(topStack()->doExists (notexists,
                                              node.style().doTiming()));
    popStack();
    return new TaQLNodeHRValue(exres);
  }

  TaQLNodeResult TaQLNodeHandler::visitBinaryNode (const TaQLBinaryNodeRep& node)
  {
    TaQLNodeResult resl = visitNode (node.itsLeft);
    TableExprNode left = getHR(resl).getExpr();
    TaQLNodeResult resr = visitNode (node.itsRight);
    if (node.itsType == TaQLBinaryNodeRep::B_INDEX) {
      const TableExprNodeSet& right = getHR(resr).getExprSet();
      return new TaQLNodeHRValue
        (TableParseQuery::handleSlice(left, right, node.itsRight.style()));
    }
    TableExprNode right = getHR(resr).getExpr();
    switch (node.itsType) {
    case TaQLBinaryNodeRep::B_PLUS:
      return new TaQLNodeHRValue (left + right);
    case TaQLBinaryNodeRep::B_MINUS:
      return new TaQLNodeHRValue (left - right);
    case TaQLBinaryNodeRep::B_TIMES:
      return new TaQLNodeHRValue (left * right);
    case TaQLBinaryNodeRep::B_DIVIDE:
      return new TaQLNodeHRValue (left / right);
    case TaQLBinaryNodeRep::B_DIVIDETRUNC:
      return new TaQLNodeHRValue (floor(left / right));
    case TaQLBinaryNodeRep::B_MODULO:
      return new TaQLNodeHRValue (left % right);
    case TaQLBinaryNodeRep::B_POWER:
      return new TaQLNodeHRValue (pow(left, right));
    case TaQLBinaryNodeRep::B_OR:
      return new TaQLNodeHRValue (left || right);
    case TaQLBinaryNodeRep::B_AND:
      return new TaQLNodeHRValue (left && right);
    case TaQLBinaryNodeRep::B_EQ:
      return new TaQLNodeHRValue (left == right);
    case TaQLBinaryNodeRep::B_NE:
      return new TaQLNodeHRValue (left != right);
    case TaQLBinaryNodeRep::B_GT:
      return new TaQLNodeHRValue (left > right);
    case TaQLBinaryNodeRep::B_GE:
      return new TaQLNodeHRValue (left >= right);
    case TaQLBinaryNodeRep::B_LT:
      return new TaQLNodeHRValue (left < right);
    case TaQLBinaryNodeRep::B_LE:
      return new TaQLNodeHRValue (left <= right);
    case TaQLBinaryNodeRep::B_IN:
      return new TaQLNodeHRValue (left.in (right, node.style()));
    case TaQLBinaryNodeRep::B_INDEX:
      break;
    case TaQLBinaryNodeRep::B_EQREGEX:
      return new TaQLNodeHRValue (left == right);
    case TaQLBinaryNodeRep::B_NEREGEX:
      return new TaQLNodeHRValue (left != right);
    case TaQLBinaryNodeRep::B_BITAND:
      return new TaQLNodeHRValue (left & right);
    case TaQLBinaryNodeRep::B_BITXOR:
      return new TaQLNodeHRValue (left ^ right);
    case TaQLBinaryNodeRep::B_BITOR:
      return new TaQLNodeHRValue (left | right);
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitMultiNode (const TaQLMultiNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);                 // takes care of deletion
    TableExprNodeSet* set = new TableExprNodeSet();
    hrval->setExprSet (set);
    hrval->setExpr (TableExprNode(set));       // takes care of deletion
    for (uint32_t i=0; i<node.itsNodes.size(); ++i) {
      if (! node.itsNodes[i].isValid()) {
        throw TableInvExpr("TaQLNodeHandler::visitMultiNode - "
                           "found a null element");
      }
      TaQLNodeResult vres = visitNode (node.itsNodes[i]);
      const TaQLNodeHRValue& vhr = getHR(vres);
      if (vhr.getElem()) {
        set->add (*(vhr.getElem()), node.itsIsSetOrArray);
      } else {
        set->add (TableExprNodeSetElem(vhr.getExpr()), node.itsIsSetOrArray);
      }
    }
    if (node.itsIsSetOrArray) {
      hrval->setExpr (TableExprNode(set->setOrArray()));
    }
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitFuncNode (const TaQLFuncNodeRep& node)
  {
    TaQLNodeResult result = visitNode (node.itsArgs);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setExpr (topStack()->handleFunc (node.itsName,
                                            getHR(result).getExprSet(),
                                            node.style()));
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitRangeNode (const TaQLRangeNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    TaQLNodeResult start = visitNode (node.itsStart);
    TaQLNodeResult end   = visitNode (node.itsEnd);
    TableExprNodeSetElem* elem;
    if (node.itsAsMidWidth) {
      elem = new TableExprNodeSetElem (getHR(start).getExpr(),
                                       getHR(end).getExpr());
    } else {
      if (start.isValid()) {
        if (end.isValid()) {
          elem = new TableExprNodeSetElem (node.itsLeftClosed,
                                           getHR(start).getExpr(),
                                           getHR(end).getExpr(),
                                           node.itsRightClosed);
        } else {
          elem = new TableExprNodeSetElem (node.itsLeftClosed,
                                           getHR(start).getExpr());
        }
      } else {
        elem = new TableExprNodeSetElem (getHR(end).getExpr(),
                                         node.itsRightClosed);
      }
    }
    hrval->setElem (elem);
    hrval->setExpr (TableExprNode(elem));
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitIndexNode (const TaQLIndexNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    TaQLNodeResult start = visitNode (node.itsStart);
    TaQLNodeResult end   = visitNode (node.itsEnd);
    TaQLNodeResult incr  = visitNode (node.itsIncr);
    const TableExprNode* se = start.isValid() ? &(getHR(start).getExpr()) : 0;
    const TableExprNode* ee =   end.isValid() ? &(getHR(  end).getExpr()) : 0;
    const TableExprNode* ie =  incr.isValid() ? &(getHR( incr).getExpr()) : 0;
    TableExprNodeSetElem* elem = 0;
    // A single boolean node indicates a mask.
    if (se && !ee && !ie && se->dataType() == TpBool) {
      elem = new TableExprNodeSetElem (*se);
    } else {
      elem = new TableExprNodeSetElem (se, ee, ie, node.style().isEndExcl());
    }
    hrval->setElem (elem);
    hrval->setExpr (TableExprNode(elem));  // Takes care of deleting elem
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitKeyColNode (const TaQLKeyColNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setExpr (topStack()->handleKeyCol (node.itsName, true));
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitTableNode (const TaQLTableNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue;
    TaQLNodeResult res(hrval);
    if (node.itsTable.nodeType() == TaQLNode_Const) {
      handleTableName (hrval, node.itsTable);
    } else {
      TaQLNodeResult res = visitNode (node.itsTable);
      hrval->setTable (getHR(res).getTable());
    }
    hrval->setAlias (node.itsAlias);
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitColNode (const TaQLColNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue;
    TaQLNodeResult res(hrval);
    if (node.itsExpr.nodeType() == TaQLNode_KeyCol) {
      // A single column name.
      TaQLKeyColNodeRep* keyNode = (TaQLKeyColNodeRep*)(node.itsExpr.getRep());
      hrval->setString (keyNode->itsName);
    } else if (node.itsExpr.nodeType() == TaQLNode_Regex) {
      // A wildcarded column name has an int value >= 0.
      TaQLRegexNodeRep* regexNode = (TaQLRegexNodeRep*)(node.itsExpr.getRep());
      int32_t val = 0;
      if (regexNode->itsCaseInsensitive) val = val|1;
      if (regexNode->itsNegate)          val = val|2;
      hrval->setInt (val);
      hrval->setString (regexNode->itsValue);
    } else {
      // An expression.
      TaQLNodeResult result = visitNode(node.itsExpr);
      hrval->setExpr (getHR(result).getExpr());
    }
    if (hrval->getExpr().isNull()  &&  ! node.itsNameMask.empty()) {
      throw TableInvExpr("value AS (col,mask) can only be given if value "
                         "is an expression");
    }
    hrval->setAlias (node.itsName);
    hrval->setNameMask (node.itsNameMask);
    hrval->setDtype (node.itsDtype);
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitColumnsNode (const TaQLColumnsNodeRep& node)
  {
    if (node.itsNodes.isValid()) {
      const TaQLMultiNodeRep* columns = node.itsNodes.getMultiRep();
      const std::vector<TaQLNode>& nodes = columns->itsNodes;
      for (uint32_t i=0; i<nodes.size(); ++i) {
        TaQLNodeResult result = visitNode (nodes[i]);
        const TaQLNodeHRValue& res = getHR(result);
        topStack()->handleColumn (res.getInt(),
                                  res.getString(), res.getExpr(),
                                  res.getAlias(), res.getNameMask(),
                                  res.getDtype());
      }
    }
    topStack()->handleColumnFinish (node.itsDistinct);
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitJoinNode (const TaQLJoinNodeRep&)
  {
    throw TableInvExpr("Joins are not implemented yet");
  }

  TaQLNodeResult TaQLNodeHandler::visitGroupNode (const TaQLGroupNodeRep& node)
  {
    const TaQLMultiNodeRep* keys = node.itsNodes.getMultiRep();
    const std::vector<TaQLNode>& nodes = keys->itsNodes;
    std::vector<TableExprNode> outnodes(nodes.size());
    for (uint32_t i=0; i<nodes.size(); ++i) {
      TaQLNodeResult result = visitNode (nodes[i]);
      outnodes[i] = getHR(result).getExpr();
    }
    topStack()->handleGroupby (outnodes,
                               node.itsType==TaQLGroupNodeRep::Rollup);
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitSortKeyNode (const TaQLSortKeyNodeRep&)
  {
    // This function cannot be called, because visitSortNode handles
    // the keys.
    throw AipsError("TaQLNodeHandler::visitSortKeyNode should not be called");
  }

  TaQLNodeResult TaQLNodeHandler::visitSortNode (const TaQLSortNodeRep& node)
  {
    const TaQLMultiNodeRep* keys = node.itsKeys.getMultiRep();
    const std::vector<TaQLNode>& nodes = keys->itsNodes;
    std::vector<TableParseSortKey> outkeys(nodes.size());
    for (uint32_t i=0; i<nodes.size(); ++i) {
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_SortKey, AipsError);
      TaQLSortKeyNodeRep* keyNode = (TaQLSortKeyNodeRep*)(nodes[i].getRep());
      TaQLNodeResult result = visitNode (keyNode->itsChild);
      const TaQLNodeHRValue& res = getHR(result);
      if (keyNode->itsType == TaQLSortKeyNodeRep::None) {
        outkeys[i] = TableParseSortKey (res.getExpr());
      } else {
        Sort::Order sortOrder = Sort::Ascending;
        if (keyNode->itsType == TaQLSortKeyNodeRep::Descending) {
          sortOrder = Sort::Descending;
        }
        outkeys[i] = TableParseSortKey (res.getExpr(), sortOrder);
      }
    }
    Sort::Order defaultSortOrder = Sort::Ascending;
    if (node.itsType == TaQLSortNodeRep::Descending) {
      defaultSortOrder = Sort::Descending;
    }
    topStack()->handleSort (outkeys, node.itsUnique, defaultSortOrder);
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitLimitOffNode (const TaQLLimitOffNodeRep& node)
  {
    if (node.itsLimit.isValid()) {
      TaQLNodeResult result = visitNode (node.itsLimit);
      const TaQLNodeHRValue& res = getHR(result);
      // If start:end:incr is given, the result is a set element.
      // Otherwise the result is an expression (for a single limit value).
      if (res.getElem()) {
        topStack()->handleLimit (*res.getElem());
      } else {
        topStack()->handleLimit (res.getExpr());
      }
    }
    if (node.itsOffset.isValid()) {
      TaQLNodeResult result = visitNode (node.itsOffset);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleOffset (res.getExpr());
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitGivingNode (const TaQLGivingNodeRep& node)
  {
    if (node.itsExprList.isValid()) {
      // Expressions in Giving clause.
      TaQLNodeResult result = visitNode (node.itsExprList);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleGiving (res.getExprSet());
    } else {
      // Table in Giving clause.
      Record type = handleMultiRecFld (node.itsType);
      topStack()->handleGiving (node.itsName, type);
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitUpdExprNode (const TaQLUpdExprNodeRep& node)
  {
    TaQLNodeResult eres = visitNode (node.itsExpr);
    TableExprNode expr = getHR(eres).getExpr();
    if (node.itsIndices1.isValid()) {
      TaQLNodeResult ires1 = visitNode (node.itsIndices1);
      if (node.itsIndices2.isValid()) {
        TaQLNodeResult ires2 = visitNode (node.itsIndices2);
        topStack()->addUpdate (new TableParseUpdate(node.itsName,
                                                    node.itsNameMask,
                                                    getHR(ires1).getExprSet(),
                                                    getHR(ires2).getExprSet(),
                                                    expr,
                                                    node.itsIndices1.style()));
      } else {
        topStack()->addUpdate (new TableParseUpdate(node.itsName,
                                                    node.itsNameMask,
                                                    getHR(ires1).getExprSet(),
                                                    expr,
                                                    node.itsIndices1.style()));
      }
    } else {
      topStack()->addUpdate (new TableParseUpdate(node.itsName,
                                                  node.itsNameMask,
                                                  expr));
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitSelectNode (const TaQLSelectNodeRep& node)
  {
    // Add an entry to the stack.
    bool outer = itsStack.empty();
    TableParseQuery* curSel = pushStack (TableParseQuery::PSELECT);
    // First handle LIMIT/OFFSET, because limit is needed when creating
    // a temp table for a select without a FROM.
    // In its turn limit/offset might use WITH tables, so do them very first.
    handleTables (node.itsWith, false);
    visitNode (node.itsLimitOff);
    if (node.itsTables.isValid()) {
      handleTables (node.itsTables);
    } else {
      // A select without a FROM means that a temp table must be created.
      topStack()->handleTableNoFrom();
    }
    curSel->setDMInfo (handleMultiRecFld (node.itsDMInfo));
    // Handle WHERE before SELECT because WHERE cannot use columns in a
    // table resulting from SELECT, while the other clauses can.
    // The reason is that selection has to be done before projection.
    // Furthermore, handle GIVING first, because projection needs to know
    // the resulting table name.
    visitNode     (node.itsGiving);
    handleJoins   (node.itsJoins);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsGroupby);
    visitNode     (node.itsColumns);
    handleHaving  (node.itsHaving);
    visitNode     (node.itsSort);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    if (! node.getNoExecute()) {
      if (outer) {
        curSel->execute (node.style().doTiming(), false, false, 0,
                         node.style().doTracing(), itsTempTables, itsStack);
        hrval->setTable (curSel->getTable());
        Block<String> block = curSel->getColumnNames();
        hrval->setNames (Vector<String>(block.begin(), block.end()));
        hrval->setString ("select");
      } else {
        if (node.getFromExecute()) {
          hrval->setTable (curSel->doFromQuery(node.style().doTiming()));
        } else {
          hrval->setExpr (curSel->doSubQuery(node.style().doTiming()));
        }
      }
      popStack();
    }
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitUpdateNode (const TaQLUpdateNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PUPDATE);
    // First handle LIMIT/OFFSET, because limit is needed when creating
    // a temp table for a select without a FROM.
    // In its turn limit/offset might use WITH tables, so do them very first.
    handleTables  (node.itsWith, false);
    handleTables  (node.itsTables);
    handleTables  (node.itsFrom);
    handleUpdate  (node.itsUpdate);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsSort);
    visitNode     (node.itsLimitOff);
    curSel->execute (node.style().doTiming(), false, true, 0);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    Block<String> block = curSel->getColumnNames();
    hrval->setNames (Vector<String>(block.begin(), block.end()));
    hrval->setString ("update");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitInsertNode (const TaQLInsertNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PINSERT);
    handleTables  (node.itsWith, false);
    handleTables  (node.itsTables);
    handleInsCol  (node.itsColumns);
    if (node.itsLimit.isValid()) {
      TaQLNodeResult res = visitNode (node.itsLimit);
      curSel->handleLimit (getHR(res).getExpr());
    }
    bool addedSel = false;
    if (node.itsValues.nodeType() == TaQLNode_Multi) {
      // Individual value expressions given.
      handleInsVal (node.itsValues);
      curSel->handleInsert();
    } else {
      // A subquery is given.
      AlwaysAssert (node.itsValues.nodeType() == TaQLNode_Select, AipsError);
      visitNode (node.itsValues);
      curSel->handleInsert (topStack());
      addedSel = true;
    }
    curSel->execute (node.style().doTiming(), false, true, 0);
    if (addedSel) {
      popStack();        // remove insert subquery
    }
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    Block<String> block = curSel->getColumnNames();
    hrval->setNames (Vector<String>(block.begin(), block.end()));
    hrval->setString ("insert");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitDeleteNode (const TaQLDeleteNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PDELETE);
    handleTables  (node.itsWith, false);
    handleTables  (node.itsTables);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsSort);
    visitNode     (node.itsLimitOff);
    curSel->execute (node.style().doTiming(), false, true, 0);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setString ("delete");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitCountNode (const TaQLCountNodeRep& node)
  {
    bool outer = itsStack.empty();
    TableParseQuery* curSel = pushStack (TableParseQuery::PCOUNT);
    handleTables  (node.itsWith, false);
    handleTables  (node.itsTables);
    visitNode     (node.itsColumns);
    handleWhere   (node.itsWhere);
    curSel->handleCount();
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    AlwaysAssert (! node.getNoExecute(), AipsError);
    if (outer) {
      curSel->execute (node.style().doTiming(), false, true, 0);
      hrval->setTable (curSel->getTable());
      Block<String> block = curSel->getColumnNames();
      hrval->setNames (Vector<String>(block.begin(), block.end()));
      hrval->setString ("count");
    } else {
      AlwaysAssert (node.getFromExecute(), AipsError);
      hrval->setTable (curSel->doFromQuery(node.style().doTiming()));
    }
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitCalcNode (const TaQLCalcNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PCALC);
    handleTables (node.itsWith, false);
    handleTables (node.itsTables);
    // If where, orderby, limit and/or offset is given, handle as FROM query.
    if (node.itsWhere.isValid() || node.itsSort.isValid() ||
        node.itsLimitOff.isValid()) {
      handleWhere (node.itsWhere);
      visitNode   (node.itsSort);
      visitNode   (node.itsLimitOff);
      Table tab = curSel->doFromQuery(node.style().doTiming());
      // Replace with the resulting table.
      curSel->replaceTable (tab);
    }
    TaQLNodeResult eres = visitNode (node.itsExpr);
    curSel->handleCalcComm (getHR(eres).getExpr());
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setExpr (curSel->getNode());
    hrval->setString ("calc");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitCreTabNode (const TaQLCreTabNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PCRETAB);
    handleTables (node.itsWith, false);
    visitNode (node.itsGiving);
    topStack()->initDescriptions (TableDesc(), Record());
    handleLikeDrop (node.itsLikeDrop);
    handleColSpecs (node.itsColumns);
    Record dminfo = handleMultiRecFld (node.itsDMInfo);
    if (node.itsLimit.isValid()) {
      TaQLNodeResult res = visitNode (node.itsLimit);
      curSel->handleLimit (getHR(res).getExpr());
    }
    curSel->handleCreTab (dminfo, itsTempTables, itsStack);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    Block<String> block = curSel->getColumnNames();
    hrval->setNames (Vector<String>(block.begin(), block.end()));
    hrval->setString ("cretab");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitColSpecNode (const TaQLColSpecNodeRep& node)
  {
    Record spec = handleMultiRecFld (node.itsSpec);
    topStack()->handleColSpec (node.itsName, node.itsLikeCol, node.itsDtype,
                               spec, node.style().isCOrder());
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitRecFldNode (const TaQLRecFldNodeRep& node)
  {
    std::string error;
    ValueHolder vh;
    if (! node.itsFromName.empty()) {
      vh = topStack()->getRecFld (node.itsFromName);
    } else if (! node.itsValues.isValid()) {
      // Invalid node means an empty vector.
      vh = ValueHolder (1, true);
    } else if (node.itsValues.nodeType() == TaQLNode_Multi  &&
               node.itsValues.getRep() != 0  &&
               ! ((const TaQLMultiNodeRep*)(node.itsValues.getRep()))->itsIsSetOrArray) {
      vh = ValueHolder (handleMultiRecFld (node.itsValues));
    } else {
      handleWhere (node.itsValues);
      TableExprNode expr = topStack()->getNode();
      if (! expr.getRep()->isConstant()) {
        error = "must be constant";
      } else {
        switch (expr.dataType()) {
        case TpBool:
          if (expr.isScalar()) {
            vh = ValueHolder(expr.getBool (0));
          } else {
            vh = ValueHolder(expr.getArrayBool(0));
          }
          break;
        case TpInt64:
          if (expr.isScalar()) {
            vh = ValueHolder(expr.getInt (0));
          } else {
            vh = ValueHolder(expr.getArrayInt(0));
          }
          break;
        case TpDouble:
          if (expr.isScalar()) {
            vh = ValueHolder(expr.getDouble (0));
          } else {
            vh = ValueHolder(expr.getArrayDouble(0));
          }
          break;
        case TpDComplex:
          if (expr.isScalar()) {
            vh = ValueHolder(expr.getDComplex (0));
          } else {
            vh = ValueHolder(expr.getArrayDComplex(0));
          }
          break;
        case TpString:
          if (expr.isScalar()) {
            vh = ValueHolder(expr.getString (0));
          } else {
            vh = ValueHolder(expr.getArrayString(0));
          }
          break;
        default:
          error = "has an unknown data type";
        }
      }
    }
    if (! error.empty()) {
      ostringstream os;
      node.itsValues.show (os);
      throw TableInvExpr("Expression " + os.str() + ' ' + error);
    }
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setString (node.itsName);
    hrval->setDtype (node.itsDtype);
    hrval->setValueHolder (vh);
    return res;
  }

  Record TaQLNodeHandler::handleMultiRecFld (const TaQLNode& node)
  {
    if (! node.isValid()) {
      return Record();
    }
    AlwaysAssert (node.nodeType() == TaQLNode_Multi, AipsError);
    const TaQLMultiNodeRep* mnode = (const TaQLMultiNodeRep*)(node.getRep());
    const std::vector<TaQLNode>& vals = mnode->itsNodes;
    for (uint32_t i=0; i<vals.size(); ++i) {
      AlwaysAssert (vals[i].nodeType() == TaQLNode_RecFld, AipsError);
    }
    Record rec;
    for (uint32_t i=0; i<vals.size(); ++i) {
      TaQLNodeResult result = visitNode(vals[i]);
      const TaQLNodeHRValue& res = getHR(result);
      if (res.getValueHolder().dataType() == TpRecord) {
        rec.defineRecord (res.getString(), res.getValueHolder().asRecord());
      } else {
        TableParseUtil::setRecFld (rec, res.getString(), res.getDtype(),
                                   res.getValueHolder());
      }
    }
    return rec;
  }

  TaQLNodeResult TaQLNodeHandler::visitUnitNode (const TaQLUnitNodeRep& node)
  {
    TaQLNodeResult res = visitNode (node.itsChild);
    TableExprNode expr = getHR(res).getExpr();
    return new TaQLNodeHRValue (expr.useUnit(node.itsUnit));
  }

  TaQLNodeResult TaQLNodeHandler::visitAltTabNode (const TaQLAltTabNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PALTTAB);
    handleTables (node.itsWith, false);
    TaQLMultiNode tmnode(false);
    tmnode.add (node.itsTable);
    handleTables (tmnode);     // must be the first table
    handleTables (node.itsFrom);
    curSel->handleAltTab();
    const TaQLMultiNodeRep& clist = *(node.itsCommands.getMultiRep());
    const std::vector<TaQLNode>& commands = clist.itsNodes;
    for (uint32_t i=0; i<commands.size(); ++i) {
      visitNode (commands[i]);
    }
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setString ("alttab");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitAddColNode (const TaQLAddColNodeRep& node)
  {
    topStack()->initDescriptions (TableDesc(), Record());
    handleColSpecs (node.itsColumns);
    Record dminfo = handleMultiRecFld (node.itsDMInfo);
    topStack()->handleAddCol (dminfo);
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitSetKeyNode (const TaQLSetKeyNodeRep& node)
  {
    // Get the value.
    const TaQLMultiNodeRep& nodelist = *(node.itsKeyVals.getMultiRep());
    const std::vector<TaQLNode>& nodes = nodelist.itsNodes;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      TaQLNodeResult result = visitNode(nodes[i]);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleSetKey (res.getString(), res.getDtype(),
                                res.getValueHolder());
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitRenDropNode (const TaQLRenDropNodeRep& node)
  {
    // Get the column/keyword names.
    const TaQLMultiNodeRep& nodelist = *(node.itsNames.getMultiRep());
    const std::vector<TaQLNode>& nodes = nodelist.itsNodes;
    Vector<String> names(nodes.size());
    for (uint32_t i=0; i<nodes.size(); ++i) {
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_KeyCol, AipsError);
      TaQLKeyColNodeRep* colNode = (TaQLKeyColNodeRep*)(nodes[i].getRep());
      names[i] = colNode->itsName;
    }
    // Get the table to operate on.
    Table tab (topStack()->getTable());
    if (node.itsType == 0) {
      // Rename columns.
      AlwaysAssert (names.size() % 2 == 0, AipsError);
      for (uint32_t i=0; i<names.size(); i+=2) {
        tab.renameColumn (names[i+1], names[i]);
      }
    } else if (node.itsType == 1) {
      // Drop columns.
      tab.removeColumn (names);
    } else if (node.itsType == 2) {
      // Rename keywords.
      AlwaysAssert (names.size() % 2 == 0, AipsError);
      for (uint32_t i=0; i<names.size(); i+=2) {
        topStack()->handleRenameKey (names[i], names[i+1]);
      }
    } else if (node.itsType == 3) {
      // Remove keywords
      for (uint32_t i=0; i<names.size(); ++i) {
        topStack()->handleRemoveKey (names[i]);
      }
    } else {
      throw AipsError("TaQLNodeHandler::vistRenDrop -  unhandled type");
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitAddRowNode (const TaQLAddRowNodeRep& node)
  {
    TaQLNodeResult result = visitNode (node.itsNRow);
    const TaQLNodeHRValue& res = getHR(result);
    topStack()->handleAddRow (res.getExpr());
    return TaQLNodeResult();
  }

  void TaQLNodeHandler::handleTableName (TaQLNodeHRValue* hrval,
                                         const TaQLNode& node)
  {
    AlwaysAssert (node.nodeType() == TaQLNode_Const, AipsError);
    TaQLConstNodeRep* tabnm = (TaQLConstNodeRep*)(node.getRep());
    if (tabnm->itsType == TaQLConstNodeRep::CTInt) {
      // A temptable has type int32_t, but the string can contain a subtable name.
      hrval->setInt (tabnm->itsIValue);
      hrval->setString (tabnm->itsSValue);
    } else {
      hrval->setString (tabnm->getString());
    }
  }

  void TaQLNodeHandler::handleTables (const TaQLMultiNode& node, bool addToFromList)
  {
    if (! node.isValid()) {
      return;
    }
    const std::vector<TaQLNode>& nodes = node.getMultiRep()->itsNodes;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      TaQLNodeResult result = visitNode (nodes[i]);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->tableList().addTable (res.getInt(), res.getString(),
                                        res.getTable(), res.getAlias(),
                                        addToFromList, itsTempTables, itsStack);
    }
  }

  void TaQLNodeHandler::handleJoins (const TaQLMultiNode& node)
  {
    if (! node.isValid()) {
      return;     // no joins
    }
    const std::vector<TaQLNode>& nodes = node.getMultiRep()->itsNodes;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      visitNode (nodes[i]);
    }
  }

  TaQLNodeResult TaQLNodeHandler::visitConcTabNode
  (const TaQLConcTabNodeRep& node)
  {
    const std::vector<TaQLNode>& nodes = node.itsTables.getMultiRep()->itsNodes;
    std::vector<Table> tables;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      TaQLNodeResult result = visitNode (nodes[i]);
      const TaQLNodeHRValue& res = getHR(result);
      const String& name = res.getString();
      Table tab(TableParseUtil::getTable (res.getInt(), name,
                                          res.getTable(),
                                          itsTempTables, itsStack, false));
      if (!tab.isNull()) {
        tables.push_back (tab);
      } else if (name.empty()) {
        throw TableInvExpr("No matching tables found for $" +
                           String::toString(res.getInt()));
      } else {
        Vector<String> nms = Directory::shellExpand(Vector<String>(1, name));
        if (nms.empty()) {
          throw TableInvExpr("No matching tables found for " + name);
        }
        for (uint32_t j=0; j<nms.size(); ++j) {
          tables.push_back (TableParseUtil::getTable (res.getInt(), nms[j],
                                                      res.getTable(),
                                                      itsTempTables, itsStack));
        }
      }
    }
    Block<Table> tabs(tables.size());
    for (uint32_t i=0; i<tables.size(); ++i) {
      tabs[i] = tables[i];
    }
    Block<String> subtables;
    if (node.itsSubTables.isValid()) {
      const std::vector<TaQLNode>& names = node.itsSubTables.getMultiRep()->itsNodes;
      subtables.resize (names.size());
      for (uint32_t i=0; i<names.size(); ++i) {
        TaQLNodeResult result = visitNode (names[i]);
        const TaQLNodeHRValue& res = getHR(result);
        subtables[i] = res.getExpr().getString(0);
      }
    }
    Table conctab(tabs, subtables);
    if (! node.itsTableName.empty()) {
      conctab.rename (node.itsTableName, Table::New);
    }
    TaQLNodeHRValue* hr = new TaQLNodeHRValue();
    hr->setTable (conctab);
    return hr;
  }

  TaQLNodeResult TaQLNodeHandler::visitShowNode
  (const TaQLShowNodeRep& node)
  {
    String info;
    bool doInfo = true;
    Vector<String> parts;
    if (node.itsNames.isValid()) {
      const std::vector<TaQLNode>& nodes = node.itsNames.getMultiRep()->itsNodes;
      parts.resize (nodes.size());
      TaQLNodeResult result = visitNode (nodes[0]);
      const TaQLNodeHRValue& res = getHR(result);
      parts[0] = res.getExpr().getString(0);
      parts[0].downcase();
      if (parts[0] == "table"  &&  nodes.size() > 1) {
        TableParseQuery* curSel = pushStack (TableParseQuery::PSHOW);
        TaQLNodeHRValue res;
        handleTableName (&res, nodes[1]);
        curSel->tableList().addTable (res.getInt(), res.getString(),
                                      res.getTable(), res.getAlias(),
                                      true, itsTempTables, itsStack);
        parts[1] = res.getString();
        for (uint32_t i=2; i<nodes.size(); ++i) {
          TaQLNodeResult result = visitNode (nodes[i]);
          const TaQLNodeHRValue& res = getHR(result);
          parts[i] = res.getExpr().getString(0);
        }
        info = curSel->getTableStructure (parts, node.style());
        doInfo = false;
        popStack();
      } else {
        for (uint32_t i=1; i<nodes.size(); ++i) {
          TaQLNodeResult result = visitNode (nodes[i]);
          const TaQLNodeHRValue& res = getHR(result);
          parts[i] = res.getExpr().getString(0);
        }
      }
    }
    if (doInfo) {
      info = "\n" + TaQLShow::getInfo (parts, node.style());
    }
    TaQLNodeHRValue* hr = new TaQLNodeHRValue();
    hr->setString ("show");
    hr->setExpr (TableExprNode(info));
    return hr;
  }

  TaQLNodeResult TaQLNodeHandler::visitCopyColNode
  (const TaQLCopyColNodeRep& node)
  {
    Record dminfo = handleMultiRecFld (node.itsDMInfo);
    topStack()->initDescriptions (TableDesc(), dminfo);
    const TaQLMultiNodeRep& nodelist = *(node.itsNames.getMultiRep());
    const std::vector<TaQLNode>& nodes = nodelist.itsNodes;
    std::vector<String> names(nodes.size());
    // Get the names of the new and old columns.
    for (uint32_t i=0; i<nodes.size(); ++i) {
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_KeyCol, AipsError);
      TaQLKeyColNodeRep* colNode = (TaQLKeyColNodeRep*)(nodes[i].getRep());
      names[i] = colNode->itsName;
    }
    // Get the table to operate on.
    Table tab (topStack()->getTable());
    // Add entries for each column copy.
    AlwaysAssert (names.size() % 2 == 0, AipsError);
    for (uint32_t i=0; i<names.size(); i+=2) {
      // Take the description for the new column from the old one.
      topStack()->handleColSpec (names[i], names[i+1], String(), Record());
      // Add an update command.
      topStack()->addUpdate (new TableParseUpdate (names[i], String(),
                                                   tab.col(names[i+1])));
    }
    // Execute it all.
    topStack()->handleCopyCol (node.style().doTiming());
    return TaQLNodeResult();
  }
  
  TaQLNodeResult TaQLNodeHandler::visitDropTabNode
  (const TaQLDropTabNodeRep& node)
  {
    TableParseQuery* curSel = pushStack (TableParseQuery::PDROPTAB);
    handleTables (node.itsWith, false);
    handleTables (node.itsTables);
    curSel->handleDropTab (itsTempTables, itsStack);
    return TaQLNodeResult (new TaQLNodeHRValue());
  }
  
  void TaQLNodeHandler::handleWhere (const TaQLNode& node)
  {
    if (node.isValid()) {
      TaQLNodeResult result = visitNode (node);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleWhere (res.getExpr());
    }
  }

  void TaQLNodeHandler::handleHaving (const TaQLNode& node)
  {
    if (node.isValid()) {
      TaQLNodeResult result = visitNode (node);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleHaving (res.getExpr());
    }
  }

  void TaQLNodeHandler::handleUpdate (const TaQLMultiNode& node)
  {
    const TaQLMultiNodeRep& updates = *(node.getMultiRep());
    const std::vector<TaQLNode>& nodes = updates.itsNodes;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_UpdExpr, AipsError);
      visitNode (nodes[i]);
    }
    topStack()->handleUpdate();
  }

  void TaQLNodeHandler::handleInsCol (const TaQLMultiNode& node)
  {
    if (! node.isValid()) {
      return;
    }
    const TaQLMultiNodeRep& cols = *(node.getMultiRep());
    const std::vector<TaQLNode>& nodes = cols.itsNodes;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      // Handle each column name.
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_KeyCol, AipsError);
      TaQLKeyColNodeRep* colNode = (TaQLKeyColNodeRep*)(nodes[i].getRep());
      topStack()->handleColumn (-1, colNode->itsName, TableExprNode(),
                                "", colNode->itsNameMask, "");
    }
  }

  void TaQLNodeHandler::handleInsVal (const TaQLNode& node)
  {
    AlwaysAssert (node.nodeType() == TaQLNode_Multi, AipsError);
    const TaQLMultiNodeRep& avals =
      dynamic_cast<const TaQLMultiNodeRep&>(*node.getRep());
    const std::vector<TaQLNode>& anodes = avals.itsNodes;
    std::vector<TableExprNode> exprs;
    AlwaysAssert (anodes.size() > 0, AipsError);
    uint32_t nval = 0;
    for (uint32_t i=0; i<anodes.size(); ++i) {
      // Handle the first insert expression.
      AlwaysAssert (anodes[i].nodeType() == TaQLNode_Multi, AipsError);
      const TaQLMultiNodeRep& vals =
        dynamic_cast<const TaQLMultiNodeRep&>(*anodes[i].getRep());
      const std::vector<TaQLNode>& nodes = vals.itsNodes;
      if (i == 0) {
        nval = nodes.size();
        exprs.reserve (nval*anodes.size());
      } else {
        if (nodes.size() != nval) {
          throw TableInvExpr("Different nr of values given in INSERT");
        }
      }
      for (uint32_t j=0; j<nodes.size(); ++j) {
        TaQLNodeResult eres = visitNode (nodes[j]);
        TableExprNode expr = getHR(eres).getExpr();
        exprs.push_back (expr);
        if (i == 0) {
          topStack()->addUpdate (new TableParseUpdate("", "", expr));
        }
      }
    }
    topStack()->setInsertExprs (exprs);
  }

  void TaQLNodeHandler::handleLikeDrop (const TaQLMultiNode& node)
  {
    if (! node.isValid()) {
      return;
    }
    const TaQLMultiNodeRep& mrep = *(node.getMultiRep());
    const std::vector<TaQLNode>& nodes = mrep.itsNodes;
    AlwaysAssert (nodes.size() > 0, AipsError);
    // Handle the LIKE table; add it to the FROM list.
    TaQLNodeResult result = visitNode (nodes[0]);
    const TaQLNodeHRValue& res = getHR(result);
    Table tab(topStack()->tableList().addTable (res.getInt(), res.getString(),
                                                res.getTable(), res.getAlias(),
                                                true, itsTempTables, itsStack));
    // An exception should be thrown if the table does not exist, but be defensive.
    AlwaysAssert (! tab.isNull(), AipsError);
    TableDesc desc(tab.tableDesc());
    if (nodes.size() == 2) {
      // Handle the columns to remove.
      AlwaysAssert (nodes[1].nodeType() == TaQLNode_Multi, AipsError);
      const TaQLMultiNodeRep& nodeList = *(const TaQLMultiNodeRep*)(nodes[1].getRep());
      const std::vector<TaQLNode>& colNodes = nodeList.itsNodes;
      for (uint32_t i=0; i<colNodes.size(); ++i) {
        AlwaysAssert (colNodes[i].nodeType() == TaQLNode_KeyCol, AipsError);
        TaQLKeyColNodeRep* colNode = (TaQLKeyColNodeRep*)(colNodes[i].getRep());
        if (desc.isColumn (colNode->itsName)) {
          desc.removeColumn (colNode->itsName);
        } else {
          throw TableInvExpr("Column " + colNode->itsName +
                             " to be dropped does not exist in the LIKE table");
        }
      }
    }
    // Ensure no dropped columns are present in the dminfo.
    Record dminfo (DataManInfo::finalizeMerge (desc, tab.dataManagerInfo()));
    topStack()->initDescriptions (desc, dminfo);
  }
  
  void TaQLNodeHandler::handleColSpecs (const TaQLMultiNode& node)
  {
    if (! node.isValid()) {
      return;
    }
    const TaQLMultiNodeRep& cols = *(node.getMultiRep());
    const std::vector<TaQLNode>& nodes = cols.itsNodes;
    for (uint32_t i=0; i<nodes.size(); ++i) {
      // Handle each column specification.
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_ColSpec, AipsError);
      visitNode (nodes[i]);
    }
  }

} //# NAMESPACE CASACORE - END
