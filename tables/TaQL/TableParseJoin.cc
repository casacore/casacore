//# TableParseJoin.cc: Class handling the join clause
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
#include <casacore/tables/TaQL/TableParseJoin.h>
#include <casacore/tables/TaQL/TableParseQuery.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TableParseJoin::TableParseJoin (TableParseQuery* parent)
    : itsParent          (parent),
      itsParentJoinIndex (-1),
      itsLastMainRow     (-1),
      itsLastJoinRow     (0)
  {
    // Get the FROM tables which are all FROM tables before this JOIN clause.
    for (const auto& tabp : itsParent->tableList().fromTables()) {
      itsFromTables.push_back (tabp.table());
    }
  }
  
  Int64 TableParseJoin::findRow (const TableExprId& id) const
  {
    // In the initialization phase of TaQLJoin, the itsJoin pointer
    // is not set. In that case the given row id is already the original
    // rownr in the join table and should be returned as such. 
    if (! itsJoin) {
      return id.rownr();
    }
    if (id.rownr() != itsLastMainRow) {
      itsLastMainRow = id.rownr();
      itsLastJoinRow = itsJoin->findRow(id);
    }
    return itsLastJoinRow;
  }

  void TableParseJoin::addTable (Int tabnr, const String& name,
                                 const Table& ftab,
                                 const String& shorthand,
                                 const std::vector<const Table*>& tempTables,
                                 const std::vector<TableParseQuery*>& stack)
  {
    // First add the table to the FROM tables in the parent which gives
    // the Table object.
    // Link it to the index of this TableParseJoin object in the parent.
    Table tab = itsParent->tableList().addTable (tabnr, name, ftab, shorthand,
                                                 True, tempTables, stack,
                                                 itsParent->joins().size() - 1);
    itsJoinTables.push_back (tab);
  }

  void TableParseJoin::handleCondition (const TableExprNode& expr)
  {
    // Check that no aggregate functions are used.
    if (! TableExprNodeUtil::getAggrNodes(expr.getRep().get()).empty()) {
      throw TableInvExpr("Aggregate functions cannot be used in a JOIN condition");
    }
    // The join condition can contain multiple ANDs.
    // Split the condition recursively into its AND parts and handle them.
    std::vector<TENShPtr> parts;
    splitAnd (expr.getRep(), parts);
    handleConditionParts (parts);
  }

  void TableParseJoin::splitAnd (const TENShPtr& node, std::vector<TENShPtr>& parts)
  {
    // Split recursively on the AND operator.
    if (node->operType() == TableExprNodeRep::OtAND) {
      const TableExprNodeBinary& bnode = dynamic_cast<const TableExprNodeBinary&>(*node);
      splitAnd (bnode.getLeftChild(), parts);
      splitAnd (bnode.getRightChild(), parts);
    } else {
      // No more ANDs, so add the part to the vector.
      parts.push_back (node);
    }
  }

  void TableParseJoin::handleConditionParts (std::vector<TENShPtr>& parts)
  {
    std::vector<Table> mainTables;
    std::vector<Table> joinTables;
    std::vector<TableExprNode> eqParts;
    std::vector<TableExprNode> inParts;
    std::vector<TableExprNode> eqMainParts;
    std::vector<TableExprNode> inMainParts;
    for (size_t i=0; i<parts.size(); ++i) {
      // First check if the condition part is correct.
      TENShPtr& part = parts[i];
      // The possible operators can be == and IN where one side is a
      // variable scalar expression of a table and the other is an expression
      // of the table to be joined.
      if (part->valueType() != TableExprNodeRep::VTScalar) {
        throw TableInvExpr ("JOIN condition must be a scalar expression");
      }
      // Check the comparison operator (must be == or IN).
      if (part->operType() != TableExprNodeRep::OtEQ  &&
          part->operType() != TableExprNodeRep::OtIN) {
        throw TableInvExpr ("JOIN condition must be == or IN");
      }
      // Both sides must be variable expressions.
      const TableExprNodeBinary& bnode = dynamic_cast<const TableExprNodeBinary&>(*part);
      TENShPtr leftChild  = bnode.getLeftChild();
      TENShPtr rightChild = bnode.getRightChild();
      if (leftChild->isConstant()  ||  rightChild->isConstant()) {
        throw TableInvExpr ("Join expressions cannot have constant comparison operands");
      }
      // Get the tables used in left and right expression.
      std::vector<Table> leftTables =
        TableExprNodeUtil::getNodeTables (leftChild.get(), False);
      std::vector<Table> rightTables =
        TableExprNodeUtil::getNodeTables (rightChild.get(), False);
      // One side must be main tables and the other join tables.
      // For IN main tables must be on the left.
      // For ==, swap left and right if no join tables on the right.
      if (part->operType() == TableExprNodeRep::OtEQ) {
        if (findMatchingTables (rightTables, itsJoinTables) == 0) {
          leftTables.swap (rightTables); // Always have join tables on the right
          leftChild  = bnode.getRightChild();
          rightChild = bnode.getLeftChild();
        }
      }
      // Test if all tables used in the condition exist at the right place.
      uInt nmatchLeft  = findMatchingTables (leftTables, itsFromTables);
      uInt nmatchRight = findMatchingTables (rightTables, itsJoinTables);
      if (nmatchLeft == 0) {
        throw TableInvExpr ("When using an IN condition in a join, "
                            "the main tables must be on the left side");
      }
      if (nmatchRight == 0) {
        throw TableInvExpr ("Join tables must be on the right side of a join condition");
      }        
      if (nmatchLeft != leftTables.size()  ||  nmatchRight != rightTables.size()) {
        throw TableInvExpr ("Mix of from and join tables on one side of a join condition");
      }
      // Add the unique main and join ables to the overall vectors.
      addUniqueTables (mainTables, leftTables);
      addUniqueTables (joinTables, rightTables);
      if (part->operType() == TableExprNodeRep::OtEQ) {
        eqParts.push_back (rightChild);
        eqMainParts.push_back (leftChild);
      } else {
        inParts.push_back (rightChild);
        inMainParts.push_back (leftChild);
      }
    }
    // Check if all main and join tables have the same size.
    rownr_t nrow = checkNrow (itsJoinTables);
    // See if one or more main tables are join tables at a previous join.
    // In that case this join is basically a nested join.
    // Note that this TableParseJoin object is already part of the vector,
    // but it is innocent to test it.
    const std::vector<TableParseJoin>& joins = itsParent->joins();
    for (Int i=joins.size()-1; i>=0; --i) {
      uInt nmatch = findMatchingTables (mainTables, joins[i].itsJoinTables);
      if (nmatch > 0) {
        // Keep the index of the parent (not its pointer).
        // Note that a pointer might be invalidated if the vector gets extended.
        itsParentJoinIndex = i;
        break;
      }
    }
    // Append the IN parts to the EQ parts, so the faster EQ lookups are done first.
    eqParts.insert (eqParts.end(), inParts.begin(), inParts.end());
    eqMainParts.insert (eqMainParts.end(), inMainParts.begin(), inMainParts.end());
    // Everything seems to be fine.
    // Now read the join data for each part.
    // Joins can only be done on Int, Double, String and DateTime (handled as Double).
    std::vector<rownr_t> rows(nrow);
    for (size_t i=0; i<nrow; ++i) {
      rows[i] = i;
    }
    itsJoin = TaQLJoin::createRecursive(eqMainParts, eqParts, rows, 0);
    // Clear the cache in the TaQLJoinColumn nodes of the join conditions.
    for (const auto& tnode : eqParts) {
      std::vector<TableExprNodeRep*> nodes;
      tnode.getRep()->flattenTree (nodes);
      for (auto node : nodes) {
        TaQLJoinColumn* colNode = dynamic_cast<TaQLJoinColumn*>(node);
        if (colNode) {
          colNode->clear();
        }
      }
    }
  }

  void TableParseJoin::addUniqueTables (std::vector<Table>& tables,
                                        const std::vector<Table>& other)
  {
    for (const Table& tab : other) {
      Bool add = True;
      for (const Table& t2 : tables) {
        if (tab.isSameTable (t2)) {
          add = False;
          break;
        }
      }
      if (add) {
        tables.insert (tables.end(), tab);
      }
    }
  }
  
  uInt TableParseJoin::findMatchingTables (const std::vector<Table>& exprTables,
                                           const std::vector<Table>& tables) const
  {
    uInt nmatch = 0;
    for (const Table& exprTab : exprTables) {
      for (const Table& tab : tables) {
        if (tab.isSameTable (exprTab)) {
          nmatch++;
          break;
        }
      }
    }
    return nmatch;
  }

  rownr_t TableParseJoin::checkNrow (const std::vector<Table>& tables) const
  {
    rownr_t nrow = 0;
    if (! tables.empty()) {
      nrow = tables[0].nrow();
      for (const Table& tab : tables) {
        if (tab.nrow() != nrow) {
          throw TableInvExpr("Join tables in a join must have the same nr of rows");
        }
      }
    }
    return nrow;
  }


} //# NAMESPACE CASACORE - END
