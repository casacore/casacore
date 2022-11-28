//# TableParseGroupby.h: Class handling GROUPBY and aggregate functions
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
//#
//# $Id$

#ifndef TABLES_TABLEPARSEGROUPBY_H
#define TABLES_TABLEPARSEGROUPBY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprGroup.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations
  class TableParseQuery;

  
  // <summary>
  // Class handling GROUPBY and aggregate functions
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis>
  // This class is used by TableParseQuery to handle TaQL's GROUPBY and HAVING
  // clauses and to setup and evaluate aggregate functions.
  // It checks that the commands and functions are given in a valid way.
  // <br>Note that some hooks are present for the ROLLUP keyword, but it is not
  // possible to use it yet.
  // </synopsis>

  class TableParseGroupby
  {
  public:
    enum GroupAggrType {
      GROUPBY=1,
      AGGR_FUNCS=2,
      ONLY_COUNTALL=4
    };

    // Keep the groupby expressions.
    // It checks if they are all scalar expressions and do not contain
    // aggregate functions..
    void handleGroupby (const std::vector<TableExprNode>&, Bool rollup);

    // Keep the having expression.
    // It checks if the node results in a bool scalar value.
    void handleHaving (const TableExprNode&);

    // Find if groupby and/or aggregate functions are given.
    // The column nodes can only contain aggregate functions if SELECT is used.
    // Finally it checks that HAVING is only used if a column node contains
    // an aggregate function (it makes no sense otherwise).
    void findGroupAggr (const Block<TableExprNode>& columnNodes,
                        Bool isSelect);

    // Is GROUPBY and/or aggregation used?
    Bool isUsed() const
      { return itsGroupAggrUsed != 0; }

    // Is only aggregation used?
    Bool isOnlyAggr() const
      { return itsGroupAggrUsed != 0  &&  (itsGroupAggrUsed & GROUPBY) == 0; }

    // Get the number of aggregation ndes.
    uInt size() const
      { return itsAggrNodes.size(); }
    
    // Disable applySelection for the column nodes of aggregate functions.
    uInt disableApplySelection();
    
    // An exception is thrown if the node uses an aggregate function.
    static void checkAggrFuncs (const TableExprNode& node);

    // Execute the grouping and aggregation and return the results.
    // The rownrs are adapted to the resulting rownrs consisting of the
    // first row of each group.
    CountedPtr<TableExprGroupResult> execGroupAggr (Vector<rownr_t>& rownrs) const;

    // Execute the HAVING clause (if present).
    // Return False in no HAVING.
    Bool execHaving (Vector<rownr_t>& rownrs,
                     const CountedPtr<TableExprGroupResult>& groups);

  private:
    // Do the grouping and aggregation and return the results.
    // It distinguishes the immediate and lazy aggregate functions.
    // The rownrs are adapted to the resulting rownrs consisting of the
    // first row of each group.
    CountedPtr<TableExprGroupResult> aggregate (Vector<rownr_t>& rownrs) const;

    // Do the grouping and aggregation and return the results.
    // It consists of a single COUNTALL operation.
    // The rownrs are adapted to the resulting rownrs consisting of the
    // first row of each group.
    CountedPtr<TableExprGroupResult> countAll (Vector<rownr_t>& rownrs) const;

    // Create the set of aggregate functions and groupby keys.
    std::vector<CountedPtr<TableExprGroupFuncSet>> multiKey
    (const std::vector<TableExprNodeRep*>&, const Vector<rownr_t>& rownrs) const;

    // Create the set of aggregate functions and groupby keys in case
    // a single groupby key is given.
    // This offers much faster map access then the general multipleKeys.
    template<typename T>
    std::vector<CountedPtr<TableExprGroupFuncSet>> singleKey
    (const std::vector<TableExprNodeRep*>& nodes,
     const Vector<rownr_t>& rownrs) const
    {
      // We have to group the data according to the (possibly empty) groupby.
      // We step through the table in the normal order which may not be the
      // groupby order.
      // A map<key,int> is used to keep track of the results where the int
      // is the index in a vector of a set of aggregate function objects.
      std::vector<CountedPtr<TableExprGroupFuncSet> > funcSets;
      std::map<T, int> keyFuncMap;
      T lastKey = std::numeric_limits<T>::max();
      int groupnr = -1;
      // Loop through all rows.
      // For each row generate the key to get the right entry.
      TableExprId rowid(0);
      T key;
      for (rownr_t i=0; i<rownrs.size(); ++i) {
        rowid.setRownr (rownrs[i]);
        itsGroupbyNodes[0].get (rowid, key);
        if (key != lastKey) {
          typename std::map<T, int>::iterator iter = keyFuncMap.find (key);
          if (iter == keyFuncMap.end()) {
            groupnr = funcSets.size();
            keyFuncMap[key] = groupnr;
            funcSets.push_back (new TableExprGroupFuncSet (nodes));
          } else {
            groupnr = iter->second;
          }
        }
        rowid.setRownr (rownrs[i]);
        funcSets[groupnr]->apply (rowid);
      }
      return funcSets;
    }

    // Get pointers to the aggregate nodes in the node expression.
    void getAggrNodes (const TableExprNode& node,
                       std::vector<TableExprNodeRep*>& aggrNodes) const;

    
    //# Data members.
    // The possible GROUPBY expressions.
    std::vector<TableExprNode> itsGroupbyNodes;
    Bool itsGroupbyRollup;               //# use ROLLUP in GROUPBY?
    // The possible HAVING expression.
    TableExprNode itsHavingNode;
    // Pointers to the aggregate function nodes.
    std::vector<TableExprNodeRep*> itsAggrNodes;
    Int itsGroupAggrUsed;
  };


} //# NAMESPACE CASACORE - END

#endif
