//# TableParseGroupby.cc: Class handling GROUPBY and aggregate functions
//# Copyright (C) 1994-2022
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

//# Includes
#include <casacore/tables/TaQL/TableParseGroupby.h>
#include <casacore/tables/TaQL/ExprGroupAggrFunc.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/Tables/TableError.h>

using namespace std;


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  void TableParseGroupby::handleGroupby
  (const std::vector<TableExprNode>& nodes, bool rollup)
  {
    itsGroupbyNodes  = nodes;
    itsGroupbyRollup = rollup;
    if (rollup) {
      throw TableInvExpr ("ROLLUP is not supported yet in the GROUPBY");
    }
    for (uint32_t i=0; i<nodes.size(); ++i) {
      checkAggrFuncs (nodes[i]);
      if (! nodes[i].isScalar()) {
        throw TableInvExpr("GROUPBY column/expression must result in a scalar value");
      }
    }
  }

  void TableParseGroupby::handleHaving (const TableExprNode& node)
  {
    itsHavingNode = node;
    if (node.dataType() != TpBool  ||  !node.isScalar()) {
      throw TableInvExpr ("HAVING expression must result in a bool scalar value");
    }
  }

  void TableParseGroupby::findGroupAggr
  (const Block<TableExprNode>& columnNodes, bool isSelect)
  {
    itsGroupAggrUsed = 0;
    // Make sure main (where) node does not have aggregate functions.
    // This has been checked before, but use defensive programming.
    std::vector<TableExprNodeRep*> aggr;
    // Get possible aggregate functions used in column nodes and HAVING.
    // Note that column nodes are also used by UPDATE and INSERT commands.
    for (uint32_t i=0; i<columnNodes.size(); ++i) {
      getAggrNodes (columnNodes[i], aggr);
    }
    uint32_t nselAggr = aggr.size();
    if (! itsHavingNode.isNull()) {
      getAggrNodes (itsHavingNode, aggr);
    }
    // Make sure aggregate functions are not used in a UPDATE command, etc.
    // This cannot happen but use defensive programming.
    if (! isSelect) {
      AlwaysAssert (aggr.empty(), AipsError);
    }
    // Make sure HAVING is only used if SELECT has an aggregate function
    // or if GROUPBY is used.
    if (! itsHavingNode.isNull()) {
      if (nselAggr == 0  &&  itsGroupbyNodes.empty()) {
        throw TableInvExpr ("HAVING can only be used if GROUPBY is used or "
                            "an aggregate function is used in SELECT");
      }
    }
    // Test if any group/aggr is given or if only
    // 'SELECT COUNT(*)' is given without GROUPBY.
    if (! itsGroupbyNodes.empty()) itsGroupAggrUsed += GROUPBY;
    if (! aggr.empty())            itsGroupAggrUsed += AGGR_FUNCS;
    if (nselAggr == 1  &&  aggr.size() == 1) {
      TableExprAggrNode* node = dynamic_cast<TableExprAggrNode*>(aggr[0]);
      // Note that the cast fails for a TableExprAggrNodeArray (as it should).
      if (node  &&  node->funcType() == TableExprFuncNode::countallFUNC) {
        itsGroupAggrUsed += ONLY_COUNTALL;
      }
    }
    itsAggrNodes = aggr;
  }

  uint32_t TableParseGroupby::disableApplySelection()
  {
    // Column nodes used in aggregate functions should not adhere applySelection.
    uint32_t ndis = 0;
    for (uint32_t i=0; i<itsAggrNodes.size(); ++i) {
      std::vector<TableExprNodeRep*> colNodes =
        TableExprNodeUtil::getColumnNodes (itsAggrNodes[i]);
      for (uint32_t j=0; j<colNodes.size(); ++j) {
        colNodes[j]->disableApplySelection();
        ndis++;
      }
    }
    return ndis;
  }

  void TableParseGroupby::getAggrNodes (const TableExprNode& node,
                                        std::vector<TableExprNodeRep*>& aggrNodes) const
  {
    std::vector<TableExprNodeRep*> aggr =
      TableExprNodeUtil::getAggrNodes (node.getRep().get());
    aggrNodes.insert (aggrNodes.end(), aggr.begin(), aggr.end());
  }

  void TableParseGroupby::checkAggrFuncs (const TableExprNode& node)
  {
    if (! node.isNull()) {
      TableExprNodeUtil::checkAggrFuncs (node.getRep().get());
    }
  }

  CountedPtr<TableExprGroupResult> TableParseGroupby::execGroupAggr
  (Vector<rownr_t>& rownrs) const
  {
    // If only 'select count(*)' was given, get the size of the WHERE,
    // thus the size of rownrs_p.
    if ((itsGroupAggrUsed & ONLY_COUNTALL) != 0  &&
        (itsGroupAggrUsed & GROUPBY) == 0) {
      return countAll (rownrs);
    }
    return aggregate (rownrs);
  }

  bool TableParseGroupby::execHaving
  (Vector<rownr_t>& rownrs, const CountedPtr<TableExprGroupResult>& groups)
  {
    if (itsHavingNode.isNull()) {
      return false;
    }
    // Find the rows matching the HAVING expression.
    Vector<rownr_t> resRownrs(rownrs.size());
    rownr_t nr = 0;
    TableExprIdAggr rowid(groups);
    for (rownr_t i=0; i<rownrs.size(); ++i) {
      rowid.setRownr (rownrs[i]);
      if (itsHavingNode.getBool (rowid)) {
        resRownrs[nr++] = rownrs[i];
      }
    }
    // Use the found rows from now on.
    resRownrs.resize (nr, true);
    rownrs.reference (resRownrs);
    return true;
  }

  CountedPtr<TableExprGroupResult> TableParseGroupby::aggregate
  (Vector<rownr_t>& rownrs) const
  {
    // Get the aggregate functions to be evaluated lazily.
    std::vector<TableExprNodeRep*> immediateNodes;
    std::vector<TableExprNodeRep*> lazyNodes;
    for (uint32_t i=0; i<itsAggrNodes.size(); ++i) {
      itsAggrNodes[i]->makeGroupAggrFunc();
      if (itsAggrNodes[i]->isLazyAggregate()) {
        lazyNodes.push_back (itsAggrNodes[i]);
      } else {
        immediateNodes.push_back (itsAggrNodes[i]);
      }
    }
    uint32_t nimmediate = immediateNodes.size();
    // For lazy nodes a vector of TableExprId-s needs to be filled per group.
    // So add a node collecting the ids.
    // Note that this node must be alive after the if, so define outside if.
    TableExprAggrNode expridNode(TableExprFuncNode::gexpridFUNC,
                                 TableExprNodeRep::NTInt,
                                 TableExprNodeRep::VTArray,
                                 TableExprNodeSet(),
                                 std::vector<TENShPtr>(),
                                 Block<int32_t>());
    if (! lazyNodes.empty()) {
      immediateNodes.push_back (&expridNode);
    }
    std::vector<CountedPtr<TableExprGroupFuncSet>> funcSets;
    // Use a faster way for a single groupby key.
    if (itsGroupbyNodes.size() == 1  &&
        itsGroupbyNodes[0].dataType() == TpDouble) {
      funcSets = singleKey<double> (immediateNodes, rownrs);
    } else if (itsGroupbyNodes.size() == 1  &&
               itsGroupbyNodes[0].dataType() == TpInt) {
      funcSets = singleKey<int64_t> (immediateNodes, rownrs);
    } else {
      funcSets = multiKey (immediateNodes, rownrs);
    }
    // Let the function nodes finish their operation.
    // Form the rownr vector from the rows kept in the aggregate objects.
    // Similarly, form the TableExprId vector if there are lazy nodes.
    Vector<rownr_t> resRownrs(funcSets.size());
    std::vector<CountedPtr<std::vector<TableExprId> > > ids;
    ids.reserve (funcSets.size());
    rownr_t n=0;
    for (uint32_t i=0; i<funcSets.size(); ++i) {
      const std::vector<CountedPtr<TableExprGroupFuncBase> >& funcs
        = funcSets[i]->getFuncs();
      for (uint32_t j=0; j<funcs.size(); ++j) {
        funcs[j]->finish();
      }
      resRownrs[n++] = funcSets[i]->getId().rownr();
      if (! lazyNodes.empty()) {
        ids.push_back (funcSets[i]->getFuncs()[nimmediate]->getIds());
      }
    }
    rownrs.reference (resRownrs);
    // Save the aggregation results in a result object.
    CountedPtr<TableExprGroupResult> result
      (new TableExprGroupResult (funcSets, ids));
    return result;
  }

  CountedPtr<TableExprGroupResult> TableParseGroupby::countAll
  (Vector<rownr_t>& rownrs) const
  {
    // This function is a special case because it does not need to
    // step though the table. Only its size is of interest. Furthermore,
    // some other columns can also be listed which will be those of the
    // last row.
    // Make a set containing the count(*) aggregate function object.
    std::vector<CountedPtr<TableExprGroupFuncSet> > funcSets
      (1, new TableExprGroupFuncSet());
    CountedPtr<TableExprGroupFuncBase> funcb = itsAggrNodes[0]->makeGroupAggrFunc();
    TableExprGroupCountAll& func = dynamic_cast<TableExprGroupCountAll&>(*funcb);
    // Note: add turns it into a CountedPtr, so it will be deleted automatically.
    funcSets[0]->add (funcb);
    // The nr of rows is the result of count(*), so simply set it.
    func.setResult (rownrs.size());
    // The resulting table has only 1 group; use the last row with it.
    if (! rownrs.empty()) {
      rownrs.reference (Vector<rownr_t>(1, rownrs[rownrs.size()-1]));
    }
    // Save the aggregation results in a result object.
    return CountedPtr<TableExprGroupResult>(new TableExprGroupResult(funcSets));
  }

  std::vector<CountedPtr<TableExprGroupFuncSet>> TableParseGroupby::multiKey
  (const std::vector<TableExprNodeRep*>& nodes, const Vector<rownr_t>& rownrs) const
  {
    // Group the data according to the (maybe empty) groupby.
    // Step through the table in the normal order which may not be the
    // groupby order.
    // A map<key,int> is used to keep track of the results where the int
    // is the index in a vector of a set of aggregate function objects.
    std::vector<CountedPtr<TableExprGroupFuncSet> > funcSets;
    std::map<TableExprGroupKeySet, int32_t> keyFuncMap;
    // Create the set of groupby key objects.
    TableExprGroupKeySet keySet(itsGroupbyNodes);
    // Loop through all rows.
    // For each row generate the key to get the right entry.
    TableExprId rowid(0);
    for (rownr_t i=0; i<rownrs.size(); ++i) {
      rowid.setRownr (rownrs[i]);
      keySet.fill (itsGroupbyNodes, rowid);
      int32_t groupnr = funcSets.size();
      std::map<TableExprGroupKeySet, int32_t>::iterator iter=keyFuncMap.find (keySet);
      if (iter == keyFuncMap.end()) {
        keyFuncMap[keySet] = groupnr;
        funcSets.push_back (new TableExprGroupFuncSet (nodes));
      } else {
        groupnr = iter->second;
      }
      funcSets[groupnr]->apply (rowid);
    }
    return funcSets;
  }


} //# NAMESPACE CASACORE - END
