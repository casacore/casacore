//# ExprNodeUtil.cc: Utility functions for TableExprNodeRep objects
//# Copyright (C) 2022
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
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  namespace TableExprNodeUtil {

    void checkAggrFuncs (TableExprNodeRep* node)
    {
      if (! getAggrNodes(node). empty()) {
        throw TableInvExpr("Invalid use of an aggregate function "
                           "(only use in SELECT or HAVING clause)");
      }
    }

    std::vector<TableExprNodeRep*> getAggrNodes (TableExprNodeRep* node)
    {
      std::vector<TableExprNodeRep*> allNodes;
      node->flattenTree (allNodes);
      std::vector<TableExprNodeRep*> aggrNodes;
      for (auto nodeP : allNodes) {
        if (nodeP->isAggregate()) {
          aggrNodes.push_back (nodeP);
        }
      }
      return aggrNodes;
    }

    std::vector<TableExprNodeRep*> getColumnNodes (TableExprNodeRep* node)
    {
      std::vector<TableExprNodeRep*> allNodes;
      node->flattenTree (allNodes);
      std::vector<TableExprNodeRep*> colNodes;
      for (auto nodeP : allNodes) {
        if (nodeP->operType() == TableExprNodeRep::OtColumn) {
          colNodes.push_back (nodeP);
        }
      }
      return colNodes;
    }

    std::vector<Table> getNodeTables (TableExprNodeRep* node,
                                      Bool properMain)
    {
      std::vector<TableExprNodeRep*> allNodes;
      node->flattenTree (allNodes);
      std::vector<Table> tables;
      std::vector<String> aliases;
      for (auto nodeP : allNodes) {
        TableExprInfo tabInfo = nodeP->getTableInfo();
        if (! tabInfo.table().isNull()) {
          // A proper Table object.
          // Handle a join table only if needed.
          // Add the table if its alias is not processed before.
          if (!properMain  ||  !tabInfo.isJoinTable()) {
            auto iter = std::find(aliases.begin(), aliases.end(), tabInfo.alias());
            if (iter == aliases.end()) {
              tables.push_back  (tabInfo.table());
              aliases.push_back (tabInfo.alias());
            } else {
              // Check that the same alias is the same table.
              size_t index = std::distance (aliases.begin(), iter);
              AlwaysAssert (tabInfo.table().isSameTable (tables[index]), AipsError);
            }
          }
        }
      }
      return tables;
    }

    rownr_t getCheckNRow (const std::vector<Table>& tables)
    {
      rownr_t nrow = 0;
      Bool first = True;
      for (const Table& tab : tables) {
        if (first) {
          nrow = tab.nrow();
          first = False;
        } else {
          if (tab.nrow() != nrow) {
            throw TableInvExpr("Table " + tab.tableName() + " has " +
                               String::toString(tab.nrow()) + " rows, " +
                               "but previous tables have " +
                               String::toString(nrow) + " rows");
          }
        }
      }
      return nrow;
    }

    
  }

} //# NAMESPACE CASACORE - END
