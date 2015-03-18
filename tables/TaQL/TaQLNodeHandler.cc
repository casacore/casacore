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
//#
//# $Id$

#include <casacore/tables/TaQL/TaQLNodeHandler.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/StringDistance.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TaQLNodeHRValue::~TaQLNodeHRValue()
  {
    // Note that itsExprSet and itsElem are counted-referenced in itsExpr,
    // so that takes care of deletion.
    delete itsNames;
  }



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
    

  TableParseSelect* TaQLNodeHandler::pushStack
                                    (TableParseSelect::CommandType type)
  {
    TableParseSelect* sel = new TableParseSelect(type);
    itsStack.push_back (sel);
    return sel;
  }
  TableParseSelect* TaQLNodeHandler::topStack() const
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
    for (Int i=itsStack.size()-1; i>=0; --i) {
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
                                            True,
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
    return new TaQLNodeHRValue (TableExprNode(TaqlRegex(Regex(str))));
  }

  TaQLNodeResult TaQLNodeHandler::visitUnaryNode (const TaQLUnaryNodeRep& node)
  {
    Bool notexists = True;
    TaQLNodeResult res = visitNode (node.itsChild);
    TableExprNode expr = getHR(res).getExpr();
    switch (node.itsType) {
    case TaQLUnaryNodeRep::U_MINUS:
      return new TaQLNodeHRValue (-expr);
    case TaQLUnaryNodeRep::U_NOT:
      return new TaQLNodeHRValue (!expr);
    case TaQLUnaryNodeRep::U_EXISTS:
      notexists= False;
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
	(TableParseSelect::handleSlice(left, right, node.itsRight.style()));
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
    TaQLNodeResult res(hrval);
    TableExprNodeSet* set = new TableExprNodeSet();
    hrval->setExprSet (set);
    hrval->setExpr (TableExprNode(set));       // this take care of deletion
    for (uInt i=0; i<node.itsNodes.size(); ++i) {
      if (! node.itsNodes[i].isValid()) {
	throw TableInvExpr("TaQLNodeHandler::visitMultiNode - "
			   "found a null element");
      }
      TaQLNodeResult vres = visitNode (node.itsNodes[i]);
      const TaQLNodeHRValue& vhr = getHR(vres);
      if (vhr.getElem()) {
	set->add (*(vhr.getElem()));
      } else {
	set->add (TableExprNodeSetElem(vhr.getExpr()));
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
    TableExprNodeSetElem* elem = new TableExprNodeSetElem
                                 (se, ee, ie, node.style().isEndExcl());
    hrval->setElem (elem);
    hrval->setExpr (TableExprNode(elem));  // Takes care of deleting elem
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitKeyColNode (const TaQLKeyColNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setExpr (topStack()->handleKeyCol (node.itsName, True));
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitTableNode (const TaQLTableNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue;
    TaQLNodeResult res(hrval);
    if (node.itsTable.nodeType() == TaQLNode_Const) {
      TaQLConstNodeRep* tabnm = (TaQLConstNodeRep*)(node.itsTable.getRep());
      if (tabnm->itsType == TaQLConstNodeRep::CTInt) {
	hrval->setInt (tabnm->itsIValue);
      } else {
	hrval->setString (tabnm->getString());
      }
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
      Int val = 0;
      if (regexNode->itsCaseInsensitive) val = val|1;
      if (regexNode->itsNegate)          val = val|2;
      hrval->setInt (val);
      hrval->setString (regexNode->itsValue);
    } else {
      // An expression.
      TaQLNodeResult result = visitNode(node.itsExpr);
      hrval->setExpr (getHR(result).getExpr());
    }
    hrval->setAlias (node.itsName);
    hrval->setDtype (node.itsDtype);
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitColumnsNode (const TaQLColumnsNodeRep& node)
  {
    if (node.itsNodes.isValid()) {
      const TaQLMultiNodeRep* columns = node.itsNodes.getMultiRep();
      const std::vector<TaQLNode>& nodes = columns->itsNodes;
      for (uInt i=0; i<nodes.size(); ++i) {
	TaQLNodeResult result = visitNode (nodes[i]);
	const TaQLNodeHRValue& res = getHR(result);
	topStack()->handleColumn (res.getInt(), res.getString(), res.getExpr(),
				  res.getAlias(), res.getDtype());
      }
    }
    topStack()->handleColumnFinish (node.itsDistinct);
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitJoinNode (const TaQLJoinNodeRep&)
  {
    throw TableInvExpr ("join is not supported yet");
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitGroupNode (const TaQLGroupNodeRep& node)
  {
    const TaQLMultiNodeRep* keys = node.itsNodes.getMultiRep();
    const std::vector<TaQLNode>& nodes = keys->itsNodes;
    std::vector<TableExprNode> outnodes(nodes.size());
    for (uInt i=0; i<nodes.size(); ++i) {
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
    throw TableError("TaQLNodeHandler::visitSortKeyNode should not be called");
  }

  TaQLNodeResult TaQLNodeHandler::visitSortNode (const TaQLSortNodeRep& node)
  {
    const TaQLMultiNodeRep* keys = node.itsKeys.getMultiRep();
    const std::vector<TaQLNode>& nodes = keys->itsNodes;
    std::vector<TableParseSort> outkeys(nodes.size());
    for (uInt i=0; i<nodes.size(); ++i) {
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_SortKey, AipsError);
      TaQLSortKeyNodeRep* keyNode = (TaQLSortKeyNodeRep*)(nodes[i].getRep());
      TaQLNodeResult result = visitNode (keyNode->itsChild);
      const TaQLNodeHRValue& res = getHR(result);
      if (keyNode->itsType == TaQLSortKeyNodeRep::None) {
	outkeys[i] = TableParseSort (res.getExpr());
      } else {
	Sort::Order sortOrder = Sort::Ascending;
	if (keyNode->itsType == TaQLSortKeyNodeRep::Descending) {
	  sortOrder = Sort::Descending;
	}
	outkeys[i] = TableParseSort (res.getExpr(), sortOrder);
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
    if (node.itsType < 0) {
      // Expressions in Giving clause.
      TaQLNodeResult result = visitNode (node.itsExprList);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleGiving (res.getExprSet());
    } else {
      // Table in Giving clause.
      topStack()->handleGiving (node.itsName, node.itsType);
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitUpdExprNode (const TaQLUpdExprNodeRep& node)
  {
    TaQLNodeResult eres = visitNode (node.itsExpr);
    TableExprNode expr = getHR(eres).getExpr();
    if (node.itsIndices.isValid()) {
      TaQLNodeResult ires = visitNode (node.itsIndices);
      topStack()->addUpdate (new TableParseUpdate(node.itsName,
						  getHR(ires).getExprSet(),
						  expr,
						  node.itsIndices.style()));
    } else {
      topStack()->addUpdate (new TableParseUpdate(node.itsName, expr));
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitSelectNode (const TaQLSelectNodeRep& node)
  {
    // Handle WHERE before SELECT because WHERE cannot use columns in a
    // table resulting from SELECT, while the other clauses can.
    // The reason is that selection has to be done before projection.
    // Furthermore, handle GIVING first, because projection needs to known
    // the resulting table name.
    Bool outer = itsStack.empty();
    TableParseSelect* curSel = pushStack (TableParseSelect::PSELECT);
    handleTables  (node.itsTables);
    visitNode     (node.itsGiving);
    visitNode     (node.itsJoin);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsGroupby);
    visitNode     (node.itsColumns);
    handleHaving  (node.itsHaving);
    visitNode     (node.itsSort);
    visitNode     (node.itsLimitOff);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    if (! node.getNoExecute()) {
      if (outer) {
	curSel->execute (node.style().doTiming(), False, False, 0,
                         node.style().doTracing());
	hrval->setTable (curSel->getTable());
	hrval->setNames (new Vector<String>(curSel->getColumnNames()));
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
    TableParseSelect* curSel = pushStack (TableParseSelect::PUPDATE);
    handleTables  (node.itsTables);
    handleUpdate  (node.itsUpdate);
    handleTables  (node.itsFrom);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsSort);
    visitNode     (node.itsLimitOff);
    curSel->execute (node.style().doTiming(), False, True, 0);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setNames (new Vector<String>(curSel->getColumnNames()));
    hrval->setString ("update");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitInsertNode (const TaQLInsertNodeRep& node)
  {
    TableParseSelect* curSel = pushStack (TableParseSelect::PINSERT);
    handleTables  (node.itsTables);
    handleInsCol  (node.itsColumns);
    Bool addedSel = False;
    if (node.itsValues.nodeType() == TaQLNode_Multi) {
      // Individual value expressions given.
      handleInsVal (node.itsValues);
      curSel->handleInsert();
    } else {
      // A subquery is given.
      AlwaysAssert (node.itsValues.nodeType() == TaQLNode_Select, AipsError);
      visitNode (node.itsValues);
      curSel->handleInsert (topStack());
      addedSel = True;
    }
    curSel->execute (node.style().doTiming(), False, True, 0);
    if (addedSel) {
      popStack();        // remove insert subquery
    }
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setNames (new Vector<String>(curSel->getColumnNames()));
    hrval->setString ("insert");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitDeleteNode (const TaQLDeleteNodeRep& node)
  {
    TableParseSelect* curSel = pushStack (TableParseSelect::PDELETE);
    handleTables  (node.itsTables);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsSort);
    visitNode     (node.itsLimitOff);
    curSel->execute (node.style().doTiming(), False, True, 0);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setString ("delete");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitCountNode (const TaQLCountNodeRep& node)
  {
    Bool outer = itsStack.empty();
    TableParseSelect* curSel = pushStack (TableParseSelect::PCOUNT);
    handleTables  (node.itsTables);
    visitNode     (node.itsColumns);
    handleWhere   (node.itsWhere);
    curSel->handleCount();
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    AlwaysAssert (! node.getNoExecute(), AipsError);
    if (outer) {
      curSel->execute (node.style().doTiming(), False, True, 0);
      hrval->setTable (curSel->getTable());
      hrval->setNames (new Vector<String>(curSel->getColumnNames()));
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
    TableParseSelect* curSel = pushStack (TableParseSelect::PCALC);
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
    TableParseSelect* curSel = pushStack (TableParseSelect::PCRETAB);
    handleColSpec (node.itsColumns);
    Record datamans = handleRecord (node.itsDataMans.getMultiRep());
    curSel->handleCreTab (node.itsName, datamans);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setNames (new Vector<String>(curSel->getColumnNames()));
    hrval->setString ("cretab");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitColSpecNode (const TaQLColSpecNodeRep& node)
  {
    Record spec = handleRecord (node.itsSpec.getMultiRep());
    topStack()->handleColSpec (node.itsName, node.itsDtype, spec,
			       node.style().isCOrder());
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitRecFldNode (const TaQLRecFldNodeRep&)
  {
    // This function cannot be called, because handleRecord processes
    // the fields.
    throw TableInvExpr ("TaQLNodeHandler::visitRecFldNode "
			"should not be called");
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitUnitNode (const TaQLUnitNodeRep& node)
  {
    TaQLNodeResult res = visitNode (node.itsChild);
    TableExprNode expr = getHR(res).getExpr();
    return new TaQLNodeHRValue (expr.useUnit(node.itsUnit));
  }

  Record TaQLNodeHandler::handleRecord (const TaQLMultiNodeRep* node)
  {
    Record rec;
    if (node) {
      const std::vector<TaQLNode>& fields = node->itsNodes;
      for (uInt i=0; i<fields.size(); ++i) {
	handleRecFld (fields[i], rec);
      }
    }
    return rec;
  }

  void TaQLNodeHandler::handleRecFld (const TaQLNode& node, Record& rec)
  {
    AlwaysAssert (node.nodeType() == TaQLNode_RecFld, AipsError);
    const TaQLRecFldNodeRep& fld = *(const TaQLRecFldNodeRep*)(node.getRep());
    if (fld.itsValues.nodeType() == TaQLNode_Multi) {
      // Value is a record or an array of values.
      const TaQLMultiNodeRep* mnode =
                     (const TaQLMultiNodeRep*)(fld.itsValues.getRep());
      handleMultiRecFld (fld.itsName, mnode, rec);
      return;
    }
    // Value is a single literal.
    AlwaysAssert (fld.itsValues.nodeType() == TaQLNode_Const, AipsError);
    const TaQLConstNodeRep& val =
               *(const TaQLConstNodeRep*)(fld.itsValues.getRep());
    switch (val.itsType) {
    case TaQLConstNodeRep::CTBool:
      rec.define (fld.itsName, val.itsBValue);
      break;
    case TaQLConstNodeRep::CTInt:
      rec.define (fld.itsName, Int(val.itsIValue));
      break;
    case TaQLConstNodeRep::CTReal:
      rec.define (fld.itsName, val.itsRValue);
      break;
    case TaQLConstNodeRep::CTComplex:
      rec.define (fld.itsName, val.itsCValue);
      break;
    case TaQLConstNodeRep::CTString:
      rec.define (fld.itsName, val.itsSValue);
      break;
    default:
      throw TableInvExpr ("TaQLNodeHandler::handleRecFld - "
			  "unknown data type");
    }
  }

  void TaQLNodeHandler::handleMultiRecFld (const String& fldName,
					   const TaQLMultiNodeRep* node,
					   Record& rec)
  {
    // No node means an empty record.
    if (node == 0) {
      rec.defineRecord (fldName, Record());
      return;
    }
    const std::vector<TaQLNode>& vals = node->itsNodes;
    if (vals.size() == 0) {
      // No values means an empty vector (of say integers)
      rec.define (fldName, Vector<Int>());
      return;
    }
    int nodeType = vals[0].nodeType();
    // Assert all values have the same node type.
    for (uInt i=1; i<vals.size(); ++i) {
      AlwaysAssert (vals[i].nodeType() == nodeType, AipsError);
    }
    if (nodeType == TaQLNode_RecFld) {
      rec.defineRecord (fldName, handleRecord(node));
      return;
    }
    // There is a vector of values.
    AlwaysAssert (nodeType == TaQLNode_Const, AipsError);
    // Check if all data types are equal or can be made equal.
    int dtype=TpOther;
    for (uInt i=0; i<vals.size(); ++i) {
      TaQLConstNodeRep* val = (TaQLConstNodeRep*)(vals[i].getRep());
      if (i == 0) {
	dtype = val->itsType;
      } else {
	dtype = checkConstDtype (dtype, val->itsType);
      }
    }
    switch (dtype) {
    case TaQLConstNodeRep::CTBool:
      {
	Vector<Bool> v(vals.size());
	for (uInt i=0; i<vals.size(); ++i) {
	  v[i] = ((TaQLConstNodeRep*)(vals[i].getRep()))->itsBValue;
	}
	rec.define (fldName, v);
	break;
      }
    case TaQLConstNodeRep::CTInt:
      {
	Vector<Int> v(vals.size());
	for (uInt i=0; i<vals.size(); ++i) {
	  v[i] = ((TaQLConstNodeRep*)(vals[i].getRep()))->itsIValue;
	}
	rec.define (fldName, v);
	break;
      }
    case TaQLConstNodeRep::CTReal:
      {
	Vector<Double> v(vals.size());
	for (uInt i=0; i<vals.size(); ++i) {
	  v[i] = ((TaQLConstNodeRep*)(vals[i].getRep()))->itsRValue;
	}
	rec.define (fldName, v);
	break;
      }
    case TaQLConstNodeRep::CTComplex:
      {
	Vector<DComplex> v(vals.size());
	for (uInt i=0; i<vals.size(); ++i) {
	  v[i] = ((TaQLConstNodeRep*)(vals[i].getRep()))->itsCValue;
	}
	rec.define (fldName, v);
	break;
      }
    case TaQLConstNodeRep::CTString:
      {
	Vector<String> v(vals.size());
	for (uInt i=0; i<vals.size(); ++i) {
	  v[i] = ((TaQLConstNodeRep*)(vals[i].getRep()))->itsSValue;
	}
	rec.define (fldName, v);
	break;
      }
    default:
      throw TableInvExpr ("TaQLNodeHandler::handleMultiRecFld - "
			  "unknown data type");
    }
  }

  int TaQLNodeHandler::checkConstDtype (int dt1, int dt2)
  {
    if (dt1 == TaQLConstNodeRep::CTTime) {
      dt1 = TaQLConstNodeRep::CTReal;
    }
    if (dt2 == TaQLConstNodeRep::CTTime) {
      dt2 = TaQLConstNodeRep::CTReal;
    }
    if (dt1 == dt2) {
      return dt1;
    }
    if (dt1 == TaQLConstNodeRep::CTBool
    ||  dt2 == TaQLConstNodeRep::CTBool
    ||  dt1 == TaQLConstNodeRep::CTString
    ||  dt2 == TaQLConstNodeRep::CTString) {
      throw TableInvExpr ("Mixed data data types in record value");
    }
    if (dt1 == TaQLConstNodeRep::CTComplex
    ||  dt2 == TaQLConstNodeRep::CTComplex) {
      return TaQLConstNodeRep::CTComplex;
    }
    if (dt1 == TaQLConstNodeRep::CTReal
    ||  dt2 == TaQLConstNodeRep::CTReal) {
      return TaQLConstNodeRep::CTReal;
    }
    return TaQLConstNodeRep::CTInt;
  }

  void TaQLNodeHandler::handleTables (const TaQLMultiNode& node)
  {
    if (! node.isValid()) {
      return;
    }
    const TaQLMultiNodeRep& tables = *(node.getMultiRep());
    const std::vector<TaQLNode>& nodes = tables.itsNodes;
    for (uInt i=0; i<nodes.size(); ++i) {
      TaQLNodeResult result = visitNode (nodes[i]);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->addTable (res.getInt(), res.getString(), res.getTable(),
			    res.getAlias(), itsTempTables, itsStack);
    }
    //# Possibly let handleColumns also add a table (for selected columns)
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
    for (uInt i=0; i<nodes.size(); ++i) {
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
    for (uInt i=0; i<nodes.size(); ++i) {
      // Handle each column name.
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_KeyCol, AipsError);
      TaQLKeyColNodeRep* colNode = (TaQLKeyColNodeRep*)(nodes[i].getRep());
      topStack()->handleColumn (-1, colNode->itsName, TableExprNode(), "", "");
    }
  }

  void TaQLNodeHandler::handleInsVal (const TaQLNode& node)
  {
    AlwaysAssert (node.nodeType() == TaQLNode_Multi, AipsError);
    const TaQLMultiNodeRep& vals = *(const TaQLMultiNodeRep*)(node.getRep());
    const std::vector<TaQLNode>& nodes = vals.itsNodes;
    for (uInt i=0; i<nodes.size(); ++i) {
      // Handle each insert expression.
      TaQLNodeResult eres = visitNode (nodes[i]);
      TableExprNode expr = getHR(eres).getExpr();
      topStack()->addUpdate (new TableParseUpdate("", expr));
    }
  }

  void TaQLNodeHandler::handleColSpec (const TaQLMultiNode& node)
  {
    if (! node.isValid()) {
      return;
    }
    const TaQLMultiNodeRep& cols = *(node.getMultiRep());
    const std::vector<TaQLNode>& nodes = cols.itsNodes;
    for (uInt i=0; i<nodes.size(); ++i) {
      // Handle each column name.
      AlwaysAssert (nodes[i].nodeType() == TaQLNode_ColSpec, AipsError);
      visitNode (nodes[i]);
    }
  }

} //# NAMESPACE CASACORE - END
