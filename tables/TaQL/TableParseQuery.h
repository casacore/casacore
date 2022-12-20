//# TableParseQuery.h: Class getting the parser results and executing a query
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

#ifndef TABLES_TABLEPARSEQUERY_H
#define TABLES_TABLEPARSEQUERY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TableParseTableList.h>
#include <casacore/tables/TaQL/TableParseProject.h>
#include <casacore/tables/TaQL/TableParseUpdate.h>
#include <casacore/tables/TaQL/TableParseSortKey.h>
#include <casacore/tables/TaQL/TableParseGroupby.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprGroup.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Containers/Block.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TableExprNodeSet;
  class TableExprNodeSetElem;
  class TableExprNodeIndex;
  class TableDesc;
  class TableColumn;
  class AipsIO;
  class Record;
  class TableRecord;
  template<class T> class ArrayColumn;


  // <summary>
  // Class getting the parser results and executing a query
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //  <li> TableGram.ll and .yy  (flex and bison grammar)
  //  <li> TaQLNodeHandler
  // </prerequisite>

  // <synopsis>
  // The results of the bison parser TableGram.yy and flex scanner TableGram.ll
  // are stored in a tree of TaQLNode objects. When the parsing of a TaQL command
  // is fully done, the tree is traversed by TaQLNodeHandler which creates and
  // fills a stack of TableParseQuery objects, one object per (nested) query.
  // A nested query is executed once it is fully handled.
  // </synopsis> 

  // <motivation>
  // It is necessary to be able to give a table select command in ASCII.
  // This can be used in a CLI or in the table browser to get a subset
  // of a table or to sort a table.
  // </motivation>

  //# <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  //# </todo>

  class TableParseQuery
  {
  public:
    enum CommandType {
      PSELECT,
      PUPDATE,
      PINSERT,
      PDELETE,
      PCOUNT,
      PCALC,
      PCRETAB,
      PALTTAB,
      PDROPTAB,
      PSHOW
    };

    // Construct.
    TableParseQuery (CommandType type);

    // Destructor.
    ~TableParseQuery();

    // Return the command type.
    CommandType commandType() const
      { return commandType_p; }

    // Return the expression node.
    TableExprNode getNode() const
      { return node_p; }

    // Return the table name list.
    TableParseTableList& tableList()
      { return tableList_p; }

    Table& projectExprTable()
      { return projectExprTable_p; }

      
    // Execute the select command (select/sort/projection/groupby/having/giving).
    // The setInGiving flag tells if a set in the GIVING part is allowed.
    // The mustSelect flag tells if a SELECT command must do something.
    // Usually that is required, but not for a SELECT in an INSERT command.
    // Optionally the maximum nr of rows to be selected can be given.
    // It will be used as the default value for the LIMIT clause.
    // 0 = no maximum.
    void execute (Bool showTimings, Bool setInGiving,
                  Bool mustSelect, rownr_t maxRow, Bool doTracing=False,
                  const std::vector<const Table*>& tempTables = std::vector<const Table*>(),
                  const std::vector<TableParseQuery*>& stack = std::vector<TableParseQuery*>());

    // Execute a query in a FROM clause resulting in a Table.
    Table doFromQuery (Bool showTimings);

    // Execute a subquery and create an appropriate node for the result.
    TableExprNode doSubQuery (Bool showTimings);

    // Test if a subquery has sufficient elements.
    // It uses default LIMIT=1, but that can be overidden in the subquery.
    // The flag tells if NOT EXISTS or EXISTS was given.
    TableExprNode doExists (Bool noexists, Bool showTimings);

    // Show the expression tree.
    void show (ostream& os) const;

    // Create a temporary table if no tables are given in FROM.
    void handleTableNoFrom();

    // Keep the selection expression.
    void handleWhere (const TableExprNode&);

    // Keep the groupby expressions.
    // It checks if they are all scalar expressions.
    void handleGroupby (const std::vector<TableExprNode>&, Bool rollup);

    // Keep the having expression.
    void handleHaving (const TableExprNode&);

    // Keep the expression of a calculate command.
    void handleCalcComm (const TableExprNode&);

    // Handle the DROP TABLE command.
    void handleDropTab (const std::vector<const Table*>& tempTables,
                        const std::vector<TableParseQuery*>& stack);
  
    // Keep the create table command.
    void handleCreTab (const Record& dmInfo,
                       const std::vector<const Table*>& tempTables,
                       const std::vector<TableParseQuery*>& stack);

    // Keep the column specification in a create table command.
    void handleColSpec (const String& columnName, const String& likeColName,
                        const String& dataType,
                        const Record& spec, Bool isCOrder=False);

    // Reopen the table (for update) used in the ALTER TABLE command.
    void handleAltTab();

    // Add columns to the table of ALTER TABLE.
    // The column descriptions have already been added to tableDesc_p.
    void handleAddCol (const Record& dmInfo);

    // Handle copying of columns.
    void handleCopyCol (Bool showTimings);
  
    // Add a keyword or replace a keyword with a value.
    // The keyword can be a table or column keyword (col::key).
    // The data type string can be empty leaving the data type unchanged.
    void handleSetKey (const String& name, const String& dtype,
                       const ValueHolder& value);

    // Rename a table or column keyword.
    void handleRenameKey (const String& oldName, const String& newName);

    // Remove a table or column keyword.
    void handleRemoveKey (const String& name);

    // Keep the update expressions.
    void handleUpdate();

    // Make ready for the insert expression.
    // The first one uses values (added via addUpdate),
    // the second one a subquery.
    // <group>
    void handleInsert();
    void handleInsert (TableParseQuery* sel);
    // </group>

    // Make ready for a COUNT command.
    // It checks if all column expressions are scalar.
    void handleCount();

    // Keep the sort expressions.
    void handleSort (const std::vector<TableParseSortKey>& sortList,
                     Bool noDuplicates, Sort::Order defaultSortOrder);

    // Evaluate and keep limit/offset/stride given as start:end:incr
    void handleLimit (const TableExprNodeSetElem& expr);

    // Evaluate and keep the limit value.
    void handleLimit (const TableExprNode& expr);

    // Evaluate and keep the offset value.
    void handleOffset (const TableExprNode& expr);

    // Evaluate and add the rows.
    void handleAddRow (const TableExprNode& expr);

    // Find the keyword or column name and create a TableExprNode from it.
    // If <src>tryProj=True</src> it is first tried if the column is a coluymn
    // in the projected table (i.e., result from the SELECT part).
    TableExprNode handleKeyCol (const String& name, Bool tryProj);

    // Handle a slice operator.
    static TableExprNode handleSlice (const TableExprNode& array,
                                      const TableExprNodeSet& indices,
                                      const TaQLStyle&);

    // Handle a function.
    TableExprNode handleFunc (const String& name,
                              const TableExprNodeSet& arguments,
                              const TaQLStyle&);

    // Add a column to the list of column names.
    void handleColumn (Int type, const String& name, const TableExprNode& expr,
                       const String& newName, const String& nameMask,
                       const String& newDtype);

    // Finish the addition of columns to the list of column names.
    void handleColumnFinish (Bool distinct);

    // Handle the name and type given in a GIVING clause.
    void handleGiving (const String& name, const Record& type);

    // Handle the set given in a GIVING clause.
    void handleGiving (const TableExprNodeSet&);

    // Initialize the table and data manager descriptions.
    void initDescriptions (const TableDesc&, const Record& dminfo);

    // Add a keyword or replace a keyword with the value of another keyword.
    // The keywords can be table or column keywords (col::key).
    ValueHolder getRecFld (const String& name);

    // Split the given name into optional shorthand, column and fields.
    // Find the keywordset for it and fill in the final keyword name.
    // It is a helper function for handleSetKey, etc.
    // If update=True, rwKeywordSet() is used to ensure the table is updated.
    TableRecord& findKeyword (const String& name, String& keyName,
                              Bool update=True);

    // Add an update object.
    void addUpdate (const CountedPtr<TableParseUpdate>& upd)
      { update_p.push_back (upd); }

    // Set the insert expressions for all rows.
    void setInsertExprs (const std::vector<TableExprNode> exprs)
      { insertExprs_p = exprs; }

    // Replace the first table (used by CALC command).
    void replaceTable (const Table& table);

    // Set the DataManager info for a new table.
    void setDMInfo (const Record& dminfo)
      { tableProject_p.setDMInfo (dminfo); }

    // Get the projected column names.
    const Block<String>& getColumnNames() const
      { return tableProject_p.getColumnNames(); }

    // Get the resulting table.
    const Table& getTable() const
      { return table_p; }

    // Get the structure of fromTables_p[0] using the options given in parts[2:].
    String getTableStructure (const Vector<String>& parts, const TaQLStyle& style);

    // Add a column node to applySelNodes_p.
    void addApplySelNode (const TableExprNode& node)
      { applySelNodes_p.push_back (node); }

    // Create a table using the given parameters.
    // The variables set by handleGiven are used for name and type.
    Table createTable (const TableDesc& td,
                       Int64 nrow, const Record& dmInfo,
                       const std::vector<const Table*>& tempTables,
                       const std::vector<TableParseQuery*>& stack);
  private:
    // Do the update step.
    // Rows 0,1,2,.. in UpdTable are updated from the expression result
    // for the rows in the given rownrs vector.
    void doUpdate (Bool showTimings, const Table& origTable,
                   Table& updTable, const Vector<rownr_t>& rownrs,
                   const CountedPtr<TableExprGroupResult>& groups =
                   CountedPtr<TableExprGroupResult>());

    // Do the insert step and return a selection containing the new rows.
    Table doInsert (Bool showTimings, Table& table);

    // Do the delete step.
    void doDelete (Bool showTimings, Table& table);

    // Do the count step returning a memory table containing the unique
    // column values and the counts of the column values.
    Table doCount (Bool showTimings, const Table&);

    // Do the projection step returning a table containing the projection.
    Table doProject (Bool showTimings, const Table&,
                     const CountedPtr<TableExprGroupResult>& groups =
                     CountedPtr<TableExprGroupResult>());

    // Do the projection containing column expressions.
    // Use the selected or unselected columns depending on <src>useSel</src>.
    Table doProjectExpr (Bool useSel,
                         const CountedPtr<TableExprGroupResult>& groups);

    // Create a subtable (used by createTable).
    Table createSubTable (const String& subtableName,
                          const TableDesc& td, Int64 nrow,
                          const Record& dmInfo,
                          const std::vector<const Table*>& tempTables,
                          const std::vector<TableParseQuery*>& stack);

    // Set the selected rows for the column objects in applySelNodes_p.
    // These nodes refer the original table. They requires different row
    // numbers than the selected groups and projected columns.
    // rownrs_p is changed to use row 0..n.
    // It returns the Table containing the subset of rows in the input Table.
    Table adjustApplySelNodes (const Table&);

    // Do the groupby/aggregate step and return its result.
    CountedPtr<TableExprGroupResult> doGroupby (bool showTimings);

    // Do the HAVING step.
    // It returns False if no HAVING step was given.
    Bool doHaving (Bool showTimings,
                   const CountedPtr<TableExprGroupResult>& groups);

    // Do the sort step.
    void doSort (Bool showTimings);

    // Do the limit/offset step.
    void  doLimOff (Bool showTimings);
    Table doLimOff (Bool showTimings, const Table& table);

    // Do the 'select distinct' step.
    Table doDistinct (Bool showTimings, const Table& table);

    // Finish the table (rename, copy, and/or flush).
    Table doFinish (Bool showTimings, Table& table,
                    const std::vector<const Table*>& tempTables,
                    const std::vector<TableParseQuery*>& stack);

    // Make an array from the contents of a column in a subquery.
    TableExprNode getColSet();

    // Make a set from the results of the subquery.
    TableExprNode makeSubSet() const;

    // Evaluate an int scalar expression.
    Int64 evalIntScaExpr (const TableExprNode& expr) const;

    //# Data mambers.
    //# Command type.
    CommandType commandType_p;
    //# List of TableParsePair objects (from WITH and FROM clause).
    TableParseTableList tableList_p;
    //# Object holding the info of table projection (i.e., column selection).
    TableParseProject   tableProject_p;
    //# Name and type of the resulting table (from GIVING part).
    String resultName_p;
    uInt   resultType_p;    //# 0-unknown 1=memory 2=scratch 3=plain
    Bool   resultCreated_p; //# Has the result table been created?
    StorageOption storageOption_p;
    Table::EndianFormat endianFormat_p;
    Bool overwrite_p;
    //# Resulting set (from GIVING part).
    TableExprNodeSet* resultSet_p;
    //# The WHERE expression tree.
    TableExprNode node_p;
    //# The GROUPBY, aggregate and HAVING info.
    TableParseGroupby groupby_p;
    //# Distinct values in output?
    Bool distinct_p;
    //# The possible limit (= max nr of selected rows) (0 means no limit).
    Int64 limit_p;
    //# The possible last row (0 means no end; can be <0).
    //# limit_p and endrow_p cannot be both !=0.
    Int64 endrow_p;
    //# The possible offset (= nr of selected rows to skip).
    Int64 offset_p;
    //# The possible stride in offset:endrow:stride.
    Int64 stride_p;
    //# The update and insert list.
    std::vector<CountedPtr<TableParseUpdate>> update_p;
    //# The insert expressions (possibly for multiple rows).
    std::vector<TableExprNode> insertExprs_p;
    //# The table selection to be inserted.
    TableParseQuery* insSel_p;
    //# The sort list.
    std::vector<TableParseSortKey> sort_p;
    //# The noDuplicates sort switch.
    Bool  noDupl_p;
    //# The default sort order.
    Sort::Order order_p;
    //# All nodes that need to be adjusted for a selection of rownrs.
    //# It can consist of column nodes and the rowid function node.
    //# Some nodes (in aggregate functions) can later be disabled for adjustment.
    std::vector<TableExprNode> applySelNodes_p;
    //# The resulting table.
    Table table_p;
    //# The table resulting from a projection with expressions.
    Table projectExprTable_p;
    //# The resulting row numbers.
    Vector<rownr_t> rownrs_p;
  };


} //# NAMESPACE CASACORE - END

#endif
