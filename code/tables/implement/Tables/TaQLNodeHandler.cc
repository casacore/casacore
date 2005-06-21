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

#include <tables/Tables/TaQLNodeHandler.h>
#include <tables/Tables/TableError.h>
#include <casa/Utilities/Assert.h>


namespace casa { //# NAMESPACE CASA - BEGIN

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
    switch (node.itsType) {
    case TaQLConstNodeRep::CTBool:
      return new TaQLNodeHRValue (TableExprNode(node.itsBValue));
    case TaQLConstNodeRep::CTInt:
      return new TaQLNodeHRValue (TableExprNode(node.itsIValue));
    case TaQLConstNodeRep::CTReal:
      return new TaQLNodeHRValue (TableExprNode(node.itsRValue));
    case TaQLConstNodeRep::CTComplex:
      return new TaQLNodeHRValue (TableExprNode(node.itsCValue));
    case TaQLConstNodeRep::CTString:
      return new TaQLNodeHRValue (TableExprNode(node.itsSValue));
    case TaQLConstNodeRep::CTTime:
      return new TaQLNodeHRValue (TableExprNode(node.itsTValue));
    }
    return TaQLNodeResult();
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
    }
    TableExprNode exres(topStack()->doExists (notexists));
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
      return new TaQLNodeHRValue (TableParseSelect::handleSlice(left,right));
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
    case TaQLBinaryNodeRep::B_MODULO:
      return new TaQLNodeHRValue (left % right);
    case TaQLBinaryNodeRep::B_POWER:
      return new TaQLNodeHRValue (left ^ right);
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
      return new TaQLNodeHRValue (left.in (right));
    case TaQLBinaryNodeRep::B_INDEX:
      break;
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
					    getHR(result).getExprSet()));
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
    TableExprNodeSetElem* elem = new TableExprNodeSetElem (se, ee, ie);
    hrval->setElem (elem);
    hrval->setExpr (TableExprNode(elem));  // Takes care of deleting elem
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitKeyColNode (const TaQLKeyColNodeRep& node)
  {
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setExpr (topStack()->handleKeyCol (node.itsName));
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
    } else {
      // An expression.
      TaQLNodeResult result = visitNode(node.itsExpr);
      hrval->setExpr (getHR(result).getExpr());
    }
    hrval->setAlias (node.itsName);
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
	topStack()->handleColumn (res.getString(), res.getExpr(),
				  res.getAlias());
      }
    }
    if (node.itsDistinct) {
      topStack()->setDistinctCol();
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitJoinNode (const TaQLJoinNodeRep& node)
  {
    throw TableInvExpr ("join is not supported yet");
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
      topStack()->handleLimit (res.getExpr());
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
      TaQLNodeResult result = visitNode (node.itsExprList);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleGiving (res.getExprSet());
    } else {
      topStack()->handleGiving (node.itsName);
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
						  expr));
    } else {
      topStack()->addUpdate (new TableParseUpdate(node.itsName, expr));
    }
    return TaQLNodeResult();
  }

  TaQLNodeResult TaQLNodeHandler::visitSelectNode (const TaQLSelectNodeRep& node)
  {
    Bool outer = itsStack.empty();
    TableParseSelect* curSel = pushStack (TableParseSelect::PSELECT);
    handleTables  (node.itsTables);
    visitNode     (node.itsColumns);
    visitNode     (node.itsJoin);
    handleWhere   (node.itsWhere);
    visitNode     (node.itsGroupby);
    visitNode     (node.itsHaving);
    visitNode     (node.itsSort);
    visitNode     (node.itsLimitOff);
    visitNode     (node.itsGiving);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    if (! node.itsNoExecute) {
      if (outer) {
	curSel->execute (False);
	hrval->setTable (curSel->getTable());
	hrval->setNames (new Vector<String>(curSel->getColumnNames()));
	hrval->setString ("select");
      } else {
	if (node.itsFromExecute) {
	  hrval->setTable (curSel->doFromQuery());
	} else {
	  hrval->setExpr (curSel->doSubQuery());
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
    curSel->execute (False);
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
    curSel->execute (False);
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
    curSel->execute (False);
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setTable (curSel->getTable());
    hrval->setString ("delete");
    popStack();
    return res;
  }

  TaQLNodeResult TaQLNodeHandler::visitCalcNode (const TaQLCalcNodeRep& node)
  {
    TableParseSelect* curSel = pushStack (TableParseSelect::PCALC);
    handleTables  (node.itsTables);
    TaQLNodeResult eres = visitNode (node.itsExpr);
    curSel->handleCalcComm (getHR(eres).getExpr());
    TaQLNodeHRValue* hrval = new TaQLNodeHRValue();
    TaQLNodeResult res(hrval);
    hrval->setExpr (curSel->getNode());
    hrval->setString ("calc");
    popStack();
    return res;
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
  }

  void TaQLNodeHandler::handleWhere (const TaQLNode& node)
  {
    if (node.isValid()) {
      TaQLNodeResult result = visitNode (node);
      const TaQLNodeHRValue& res = getHR(result);
      topStack()->handleSelect (res.getExpr());
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
      topStack()->handleColumn (colNode->itsName, TableExprNode(), "");
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

} //# NAMESPACE CASA - END
