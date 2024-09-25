//# TableParseJoin.h: Class handling the join clause
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_TABLEPARSEJOIN_H
#define TABLES_TABLEPARSEJOIN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/TaQL/TaQLJoin.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations.
  class TableParse;
  class TableParseQuery;
  class TableExprNode;
  class Table;

  // <summary>
  // Class handling a join clause in a TaQL command
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>

  // <synopsis>
  // TableParseQuery holds a vector of TableParseJoin objects, one for each
  // join clause in a TaQL command.
  // <br>TableParseJoin holds the join condition expression and the from and
  // join tables involved in the expression. The from tables are all tables in
  // the TaQL command prior to this join clause. The join tables are the tables
  // mentioned in the join clause.
  // TableParseJoin also tells if the join is nested in another join by keeping
  // the index of the parent TableParseJoin object in the vector in TableParseQuery.
  //
  // The ON condition expression is split in its AND parts; basically each part
  // involves a column comparison. Each part must contain an = for an exact match
  // or IN comparison for matching an interval.
  // A tree, consisting of TaQLJoinBase objects, is built to execute the condition.
  // It finds the matching row in the join table given a row in the main table.
  // Each level in the tree is an AND part in the condition.
  // </synopsis> 

  class TableParseJoin
  {
  public:
    explicit TableParseJoin (TableParseQuery*);
    
    // Add a join table nr, name, or object to the container.
    void addTable (Int tabnr, const String& name,
                   const Table& table,
                   const String& shorthand,
                   const std::vector<const Table*>& tempTables,
                   const std::vector<TableParseQuery*>& stack);

    // Handle the ON condition of a join.
    void handleCondition (const TableExprNode& expr);

    // Find the row in the join table for the given main table row.
    //# In the initialization phase of TaQLJoin, the itsJoin pointer
    //# is not set. In that case the given row id is already the original
    //# rownr in the join table and should be returned as such. 
    Int64 findRow (const TableExprId& id) const;
    
  private:
    // Split the ON condition recursively into its AND parts.
    void splitAnd (const TENShPtr& node, std::vector<TENShPtr>& parts);

    // Handle all AND parts of the join condition.
    void handleConditionParts (std::vector<TENShPtr>& parts);

    // Tell how many tables in the exprTables vector are the same as those
    // in the tables vector.
    uInt findMatchingTables (const std::vector<Table>& exprTables,
                             const std::vector<Table>& tables) const;

    // Check if all join tables in the vector have the same number of rows.
    rownr_t checkNrow (const std::vector<Table>&) const;

    // Add the tables in the other vector to the tables vector.
    // Only do that for tables not occurring in the tables vector yet.
    void addUniqueTables (std::vector<Table>& tables,
                          const std::vector<Table>& other);

    //# Data members.
    TableParseQuery*   itsParent;
    std::vector<Table> itsFromTables;
    std::vector<Table> itsJoinTables;
    //# Index in TableParseQuery's vector of joins; <0 is no parent join.
    Int                itsParentJoinIndex;
    std::shared_ptr<TaQLJoinBase> itsJoin;
    mutable Int64      itsLastMainRow;   // Last main table row looked for
    mutable Int64      itsLastJoinRow;   // Join table row matching itsLastMainRow
  };

} //# NAMESPACE CASACORE - END

#endif
