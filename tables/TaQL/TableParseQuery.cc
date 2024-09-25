//# TableParseQuery.cc: Class getting the parser results and executing a query
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/TaQL/TableParseQuery.h>
#include <casacore/tables/TaQL/TableParseFunc.h>
#include <casacore/tables/TaQL/TableParseUtil.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/TaQL/ExprRange.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/ostream.h>
#include <algorithm>


namespace casacore { //# NAMESPACE CASACORE - BEGIN



  TableParseQuery::TableParseQuery (CommandType commandType)
    : commandType_p   (commandType),
      tableProject_p  (tableList_p),
      resultType_p    (0),
      resultCreated_p (False),
      endianFormat_p  (Table::AipsrcEndian),
      overwrite_p     (True),
      resultSet_p     (0),
      distinct_p      (False),
      limit_p         (0),
      endrow_p        (0),
      offset_p        (0),
      stride_p        (1),
      insSel_p        (0),
      noDupl_p        (False),
      order_p         (Sort::Ascending)
  {}

  TableParseQuery::~TableParseQuery()
  {
    // Note that insSel_p is simply a pointer to another object,
    // so no delete should be done.
    delete resultSet_p;
  }

  TableParseJoin& TableParseQuery::addJoin()
  {
    // Add a TableParseJoin object.
    joins_p.emplace (joins_p.end(), this);
    return joins_p.back();
  }

  void TableParseQuery::replaceTable (const Table& table)
  {
    AlwaysAssert (! tableList_p.empty(), AipsError);
    tableList_p.replaceTable (table);
  }

  //# Lookup a field name in the table for which the shorthand is given.
  //# If no shorthand is given, use the first table.
  //# The shorthand and name are separated by a period.
  TableExprNode TableParseQuery::handleKeyCol (const String& name, Bool tryProj)
  {
    return tableProject_p.handleKeyCol (name, tryProj, *this);
  }

  TableExprNode TableParseQuery::handleSlice (const TableExprNode& array,
                                              const TableExprNodeSet& indices,
                                              const TaQLStyle& style)
  {
    // Create a masked array if a single bool element is given.
    if (indices.dataType() == TableExprNodeRep::NTBool) {
      if (! (indices.isSingle()  &&  indices.size() == 1  &&
             indices.hasArrays())) {
        throw TableInvExpr ("Second argument of a masked array must be an array; maybe extra brackets are needed like [1,2][[T,F]]");
      }
      return marray (array, TableExprNode(indices[0]->start()));
    }
    return TableExprNode::newArrayPartNode (array, indices, style);
  }
 
  //# Parse the name of a function.
  TableExprNode TableParseQuery::handleFunc (const String& name,
                                             const TableExprNodeSet& arguments,
                                             const TaQLStyle& style)
  {
    //# No functions have to be ignored.
    Vector<Int> ignoreFuncs;
    // Use a default table if no one available.
    if (tableList_p.empty()) {
      return TableParseFunc::makeFuncNode (this, name, arguments, ignoreFuncs,
                                           TableExprInfo(), style);
    }
    TableExprNode node = TableParseFunc::makeFuncNode (this, name, arguments,
                                                       ignoreFuncs,
                                                       tableList_p.first(), style);
    // A rowid function node needs to be added to applySelNodes_p.
    const TENShPtr& rep = node.getRep();
    if (dynamic_cast<const TableExprNodeRowid*>(rep.get())) {
      addApplySelNode (node);
    }
    return node;
  }

  //# Add a column name to the block of column names.
  //# Only take the part beyond the period.
  //# Extend the block each time. Since there are only a few column names,
  //# this will not be too expensive.
  void TableParseQuery::handleColumn (Int stringType,
                                      const String& name,
                                      const TableExprNode& expr,
                                      const String& newName,
                                      const String& newNameMask,
                                      const String& newDtype)
  {
    tableProject_p.handleColumn (stringType, name, expr,
                                 newName, newNameMask, newDtype, *this);
  }

  //# Finish the additions to the block of column names
  //# by removing the deleted empty names and creating Expr objects as needed.
  void TableParseQuery::handleColumnFinish (Bool distinct)
  {
    distinct_p = distinct;
    projectExprTable_p = tableProject_p.handleColumnFinish
      (distinct, resultSet_p, *this);
  }

  Table TableParseQuery::createTable (const TableDesc& td,
                                      Int64 nrow, const Record& dmInfo,
                                      const std::vector<const Table*>& tempTables,
                                      const std::vector<TableParseQuery*>& stack)
  {
    // If the table name contains ::, a subtable has to be created.
    // Split the name at the last ::.
    Vector<String> parts = stringToVector(resultName_p, std::regex("::"));
    if (parts.size() > 1) {
      return createSubTable (parts[parts.size()-1], td, nrow, dmInfo,
                             tempTables, stack);
    } 
    // Create the table.
    // The types are defined in function handleGiving.
    Table::TableType   ttype = Table::Plain;
    Table::TableOption topt  = Table::New;
    if (!overwrite_p)  topt  = Table::NewNoReplace;
    // Use default Memory if no name or 'memory' has been given.
    if (resultName_p.empty()) {
      ttype = Table::Memory;
    } else if (resultType_p == 1) {
      ttype = Table::Memory;
    } else if (resultType_p == 2) {
      topt  = Table::Scratch;
    }
    SetupNewTable newtab(resultName_p, td, topt, storageOption_p);
    newtab.bindCreate (dmInfo);
    Table tab(newtab, ttype, nrow, False, endianFormat_p);
    resultCreated_p = True;
    return tab;
  }

  Table TableParseQuery::createSubTable (const String& subtableName,
                                         const TableDesc& td, Int64 nrow,
                                         const Record& dmInfo,
                                         const std::vector<const Table*>& tempTables,
                                         const std::vector<TableParseQuery*>& stack)
  {
    Table parent (TableParseUtil::openParentTable(resultName_p, subtableName,
                                                  tempTables, stack));
    return TableUtil::createSubTable (parent, subtableName, td,
                                      overwrite_p ? Table::New : Table::NewNoReplace,
                                      storageOption_p, dmInfo, TableLock(),
                                      nrow, False, endianFormat_p, TSMOption());
  }

  //# Add a column specification.
  void TableParseQuery::handleColSpec (const String& colName,
                                       const String& likeColName,
                                       const String& dtstr,
                                       const Record& spec,
                                       Bool isCOrder)
  {
    tableProject_p.handleColSpec (colName, likeColName, dtstr, spec, isCOrder);
  }

  void TableParseQuery::handleGroupby (const std::vector<TableExprNode>& nodes,
                                       Bool rollup)
  {
    groupby_p.handleGroupby (nodes, rollup);
  }

  void TableParseQuery::handleHaving (const TableExprNode& node)
  {
    groupby_p.handleHaving (node);
  }

  void TableParseQuery::handleDropTab(const std::vector<const Table*>& tempTables,
                                      const std::vector<TableParseQuery*>& stack)
  {
    // Delete all tables. It has already been checked they exist.
    for (TableParsePair& tab : tableList_p.fromTablesNC()) {
      // Split the name on :: to see if a subtable has to be deleted.
      Vector<String> parts = stringToVector(tab.name(), std::regex("::"));
      if (parts.size() > 1) {
        // There is a subtable, so delete the keyword in its parent.
        // First get the size of the parent name.
        const String& subName(parts[parts.size() - 1]);
        size_t sz = tab.name().size() - subName.size() - 2;
        Table parent(TableParseUtil::getTable (tab.tabnr(),
                                               tab.name().substr(0,sz),
                                               Table(), tempTables, stack));
        // Make sure subtable is closed, otherwise cannot be deleted.
        tab.table() = Table();
        TableUtil::deleteSubTable (parent, subName);
      } else {
        tab.table().markForDelete();
      }
    }
  }

  void TableParseQuery::handleCreTab (const Record& dmInfo,
                                      const std::vector<const Table*>& tempTables,
                                      const std::vector<TableParseQuery*>& stack)
  {
    DataManInfo::mergeInfo (tableProject_p.dminfo(), dmInfo);
    DataManInfo::finalizeMerge (tableProject_p.tableDesc(),
                                tableProject_p.dminfo());
    table_p = createTable (tableProject_p.tableDesc(), limit_p,
                           tableProject_p.dminfo(), tempTables, stack);
  }

  void TableParseQuery::handleAltTab()
  {
    // The first table has to be altered.
    AlwaysAssert (! tableList_p.empty(), AipsError);
    table_p = tableList_p.firstTable();
    table_p.reopenRW();
    if (! table_p.isWritable()) {
      throw TableInvExpr ("Table " + table_p.tableName() + " is not writable");
    }
  }

  void TableParseQuery::handleAddCol (const Record& dmInfo)
  {
    tableProject_p.handleAddCol (dmInfo, table_p);
  }

  void TableParseQuery::initDescriptions (const TableDesc& desc,
                                          const Record& dminfo)
  {
    tableProject_p.initDescriptions (desc, dminfo);
  }

  ValueHolder TableParseQuery::getRecFld (const String& name)
  {
    String keyName;
    const TableRecord& keyset = findKeyword (name, keyName, False);
    Int fieldnr = keyset.fieldNumber (keyName);
    if (fieldnr < 0) {
      throw (TableInvExpr ("Keyword " + name + " does not exist"));
    }
    return keyset.asValueHolder (fieldnr);
  }

  TableRecord& TableParseQuery::findKeyword (const String& name,
                                             String& keyName,
                                             Bool update)
  {
    //# Split the name into optional shorthand, column, and keyword.
    String shand, columnName;
    Vector<String> fieldNames;
    TableParseUtil::splitName (shand, columnName, fieldNames,
                               name, True, True, False);
    Table tab = tableList_p.findTable (shand, False).table();
    if (tab.isNull()) {
      throw (TableInvExpr("Shorthand " + shand + " not defined in FROM clause"));
    }
    TableRecord* rec;
    String fullName;
    if (columnName.empty()) {
      if (update) {
        rec = TableExprNode::findLastKeyRec (tab.rwKeywordSet(),
                                             fieldNames, fullName);
      } else {
        rec = TableExprNode::findLastKeyRec (tab.keywordSet(),
                                             fieldNames, fullName);
      }
    } else {
      if (update) {
        TableRecord& colkeys (TableColumn(tab, columnName).rwKeywordSet());
        rec = TableExprNode::findLastKeyRec (colkeys, fieldNames, fullName);
      } else {
        const TableRecord& colkeys (TableColumn(tab, columnName).keywordSet());
        rec = TableExprNode::findLastKeyRec (colkeys, fieldNames, fullName);
      }
    }
    keyName = fieldNames[fieldNames.size() -1 ];
    return *rec;
  }

  void TableParseQuery::handleSetKey (const String& name,
                                      const String& dtype,
                                      const ValueHolder& value)
  {
    String keyName;
    TableRecord& keyset = findKeyword (name, keyName);
    if (value.dataType() == TpString  ||  value.dataType() == TpRecord) {
      keyset.defineFromValueHolder (keyName, value);
    } else {
      TableParseUtil::setRecFld (keyset, keyName, dtype, value);
    }
  }

  void TableParseQuery::handleCopyCol (Bool showTimings)
  {
    // Note that table_p, tableDesc_p and dminfo_p have already been set.
    Timer timer;
    handleAddCol (Record());
    doUpdate (False, Table(), table_p, table_p.rowNumbers());
    if (showTimings) {
      timer.show ("  Copy Column ");
    }
  }

  void TableParseQuery::handleRenameKey (const String& oldName,
                                         const String& newName)
  {
    String keyName;
    TableRecord& keyset = findKeyword (oldName, keyName);
    keyset.renameField (newName, keyName);
  }

  void TableParseQuery::handleRemoveKey (const String& name)
  {
    String keyName;
    TableRecord& keyset = findKeyword (name, keyName);
    keyset.removeField (keyName);
  }

  void TableParseQuery::handleWhere (const TableExprNode& node)
  {
    TableParseGroupby::checkAggrFuncs (node);
    node_p = node;
  }

  void TableParseQuery::handleSort (const std::vector<TableParseSortKey>& sort,
                                    Bool noDuplicates,
                                    Sort::Order order)
  {
    noDupl_p = noDuplicates;
    order_p  = order;
    sort_p   = sort;
  }

  void TableParseQuery::handleCalcComm (const TableExprNode& node)
  {
    TableParseGroupby::checkAggrFuncs (node);
    node_p = node;
  }

  //# Execute a query in the FROM clause and return the resulting table.
  Table TableParseQuery::doFromQuery (Bool showTimings)
  {
    Timer timer;
    // Execute the nested command.
    execute (False, False, True, 0);
    if (showTimings) {
      timer.show ("  From query  ");
    }
    return table_p;
  }

  //# Execute a subquery for an EXISTS operator.
  TableExprNode TableParseQuery::doExists (Bool notexists, Bool showTimings)
  {
    Timer timer;
    // Execute the nested command.
    // Default limit_p is 1.
    execute (False, True, True, 1);
    if (showTimings) {
      timer.show ("  Exists query");
    }
    // Flag notexists tells if NOT EXISTS or EXISTS was given.
    return TableExprNode (notexists == (Int64(table_p.nrow()) < limit_p));
  }

  //# Execute a subquery and create the correct node object for it.
  TableExprNode TableParseQuery::doSubQuery (Bool showTimings)
  {
    Timer timer;
    // Execute the nested command.
    execute (False, True, True, 0);
    TableExprNode result;
    if (resultSet_p != 0) {
      // A set specification was given, so make the set.
      result = makeSubSet();
    } else {
      // A single column was given, so get its data.
      result = TableParseUtil::getColSet (table_p);
    }
    if (showTimings) {
      timer.show ("  Subquery    ");
    }
    return result;
  }


  TableExprNode TableParseQuery::makeSubSet() const
  {
    // Perform some checks on the given set.
    if (resultSet_p->hasArrays()) {
      throw (TableInvExpr ("Set in GIVING clause should contain scalar"
                           " elements"));
    }
    resultSet_p->checkEqualDataTypes();
    // Link to set to make sure that TableExprNode hereafter does not delete
    // the object.
    TableExprNodeSet set(rownrs_p, *resultSet_p);
    return set.setOrArray();
  }

  void TableParseQuery::handleLimit (const TableExprNodeSetElem& expr)
  {
    if (expr.start()) {
      offset_p = evalIntScaExpr (TableExprNode(expr.start()));
    }
    if (expr.increment()) {
      stride_p = evalIntScaExpr (TableExprNode(expr.increment()));
      if (stride_p <= 0) {
        throw TableInvExpr ("in the LIMIT clause stride " +
                            String::toString(stride_p) +
                            " must be positive");
      }
    }
    if (expr.end()) {
      endrow_p = evalIntScaExpr (TableExprNode(expr.end()));
    }
  }

  void TableParseQuery::handleLimit (const TableExprNode& expr)
  {
    limit_p = evalIntScaExpr (expr);
  }

  void TableParseQuery::handleOffset (const TableExprNode& expr)
  {
    offset_p = evalIntScaExpr (expr);
  }

  void TableParseQuery::handleTableNoFrom()
  {
    if (limit_p < 0  ||  offset_p < 0  ||  endrow_p < 0) {
      throw TableInvExpr("LIMIT and OFFSET values cannot be negative if no "
                         "tables are given in the FROM clause");
    }
    // Use 1 row if limit_p nor endrow_p is given.
    Int64 nrow = 1;
    if (limit_p > 0) {
      nrow = limit_p + offset_p;
    } else if (endrow_p > 0) {
      nrow = endrow_p;
    }
    // Add a temp table with no columns and some rows to the FROM list.
    Table tab(Table::Memory);
    tab.addRow(nrow);
    tableList_p.addTable (-1, String(), tab, String(), True,
                          std::vector<const Table*>(),
                          std::vector<TableParseQuery*>());
  }

  void TableParseQuery::handleAddRow (const TableExprNode& expr)
  {
    table_p.addRow (evalIntScaExpr (expr));
  }

  Int64 TableParseQuery::evalIntScaExpr (const TableExprNode& expr) const
  {
    TableParseGroupby::checkAggrFuncs (expr);
    if (! TableExprNodeUtil::getNodeTables (expr.getRep().get(), False).empty()) {
      throw TableInvExpr ("LIMIT or OFFSET expression cannot contain columns");
    }
    // Get the value as a double, because some expressions result in double.
    // Round it to an integer.
    TableExprId rowid(0);
    Double val;
    expr.get (rowid, val);
    if (val >= 0) {
      return static_cast<Int64>(val+0.5);
    }
    return -static_cast<Int64>(-val+0.5);
  }

  void TableParseQuery::handleUpdate()
  {
    // Set the column names, so they can be returned by tableCommand().
    tableProject_p.setColumnNames (update_p);
  }

  void TableParseQuery::handleInsert()
  {
    // If no columns were given, all stored columns in the first table
    // are the target columns.
    if (tableProject_p.getColumnNames().empty()) {
      tableProject_p.setStoredColumns();
    }
    // Check if #columns and values match.
    // Copy the names to the update objects.
    const Block<String>& colNames = tableProject_p.getColumnNames();
    if (update_p.size() != colNames.size()) {
      throw TableInvExpr ("Error in INSERT command; nr of columns (=" +
                          String::toString(colNames.size()) +
                          ") mismatches "
                          "number of VALUES expressions (=" +
                          String::toString(Int(update_p.size())) + ")");
    }
    tableProject_p.setUpdateNames (update_p);
  }

  void TableParseQuery::handleInsert (TableParseQuery* sel)
  {
    insSel_p = sel;
  }

  void TableParseQuery::handleCount()
  {
    tableProject_p.checkCountColumns();
  }

  //# Execute the updates.
  void TableParseQuery::doUpdate (Bool showTimings, const Table& origTable,
                                  Table& updTable, const Vector<rownr_t>& rownrs,
                                  const std::shared_ptr<TableExprGroupResult>& groups)
  {
    Timer timer;
    AlwaysAssert (updTable.nrow() == rownrs.size(), AipsError);
    //# If no rows to be updated, return immediately.
    //# (the code below will fail for empty tables)
    if (rownrs.empty()) {
      return;
    }
    // Reopen the table for write.
    updTable.reopenRW();
    if (! updTable.isWritable()) {
      throw TableInvExpr ("Table " + updTable.tableName() + " is not writable");
    }
    //# First check if the update columns and values are correct.
    uInt nrkey = update_p.size();
    Block<TableColumn> cols(nrkey);
    Block<ArrayColumn<Bool> > maskCols(nrkey);
    for (uInt i=0; i<nrkey; i++) {
      TableParseUpdate& key = *(update_p[i]);
      key.check (origTable, updTable);
      // Correct, so attach the TableColumn objects.
      const String& colNameMask = key.columnNameMask();
      if (! colNameMask.empty()) {
        maskCols[i].attach (updTable, colNameMask);
      }
      cols[i].attach (updTable, key.columnName());
      // If needed, make the expression's unit the same as the column unit.
      key.adaptUnit (TableExprNodeColumn::getColumnUnit (cols[i]));
    }
    // Loop through all rows in the table and update each row.
    TableExprIdAggr rowid(groups);
    for (rownr_t row=0; row<rownrs.size(); ++row) {
      rowid.setRownr (rownrs[row]);
      for (uInt i=0; i<nrkey; i++) {
        update_p[i]->updateColumn (cols[i], maskCols[i], row, rowid);
      }
    }
    if (showTimings) {
      timer.show ("  Update      ");
    }
  }

  //# Execute the inserts.
  Table TableParseQuery::doInsert (Bool showTimings, Table& table)
  {
    Timer timer;
    // Reopen the table for write.
    table.reopenRW();
    if (! table.isWritable()) {
      throw TableInvExpr ("Table " + table.tableName() + " is not writable");
    }
    // Add rows if the inserts are given as expressions.
    // Select rows and use update to put the expressions into the rows.
    if (update_p.size() > 0) {
      uInt  nexpr  = insertExprs_p.size();
      Int64 nrowex = nexpr / update_p.size();
      AlwaysAssert (nrowex*update_p.size() == nexpr, AipsError);
      Int64 nrow   = nrowex;
      if (limit_p > 0) {
        // See if #rows is given explicitly.
        nrow = limit_p;
      } else if (limit_p < 0) {
        nrow = table.nrow() + limit_p;
      }
      Vector<rownr_t> newRownrs(nrow);
      indgen (newRownrs, table.nrow());
      Vector<rownr_t> selRownrs(1, table.nrow() + nrow);
      // Add new rows to TableExprNodeRowid.
      // It works because NodeRowid does not obey disableApplySelection.
      for (std::vector<TableExprNode>::iterator iter=applySelNodes_p.begin();
           iter!=applySelNodes_p.end(); ++iter) {
        iter->disableApplySelection();
        iter->applySelection (selRownrs);
      }
      // Add one row at a time, because an insert expression might use
      // the table itself.
      Int64 inx = 0;
      for (Int64 i=0; i<nrow; ++i) {
        selRownrs[0] = table.nrow();
        table.addRow();
        Table sel = table(selRownrs);
        for (uInt j=0; j<update_p.size(); ++j) {
          update_p[j]->setNode (insertExprs_p[inx*update_p.size() + j]);
        }
        doUpdate (False, Table(), sel, selRownrs);
        inx++;
        if (inx == nrowex) inx = 0;
      }
      return table(newRownrs);
    }
    // Handle the inserts from another selection.
    // Do the selection.
    insSel_p->execute (False, False, False, 0);
    Table sel = insSel_p->getTable();
    if (sel.nrow() == 0) {
      return Table();
    }
    // Get the target columns if not given.
    if (tableProject_p.getColumnNames().empty()) {
      tableProject_p.setStoredColumns();
    }
    // Get the source columns.
    Block<String> sourceNames;
    sourceNames = insSel_p->getColumnNames();
    if (sourceNames.size() == 0) {
      sourceNames = TableParseUtil::getStoredColumns (sel);
    }
    // Check if the number of columns match.
    const Block<String>& colNames = tableProject_p.getColumnNames();
    if (sourceNames.size() != colNames.size()) {
      throw TableInvExpr ("Error in INSERT command; nr of columns (=" +
                          String::toString(colNames.size()) +
                          ") mismatches "
                          "number of columns in selection (=" +
                          String::toString(sourceNames.size()) + ")");
    }
    // Check if the data types match.
    const TableDesc& tdesc1 = table.tableDesc();
    const TableDesc& tdesc2 = sel.tableDesc();
    for (uInt i=0; i<colNames.size(); i++) {
      if (tdesc1[colNames[i]].trueDataType() !=
          tdesc2[sourceNames[i]].trueDataType()) {
        throw TableInvExpr ("Error in INSERT command; data type of columns " +
                            colNames[i] + " and " + sourceNames[i] +
                            " mismatch");
      }
    }
    // Add the required nr of rows to the table and make a selection of it.
    rownr_t rownr = table.nrow();
    table.addRow (sel.nrow());
    Vector<rownr_t> rownrs(sel.nrow());
    indgen (rownrs, rownr);     // fill with rownr, rownr+1, etc.
    Table tab = table(rownrs);
    TableRow rowto (tab, Vector<String>(colNames.begin(), colNames.end()));
    ROTableRow rowfrom (sel, Vector<String>(sourceNames.begin(), sourceNames.end()));
    for (rownr_t i=0; i<sel.nrow(); i++) {
      rowto.put (i, rowfrom.get(i), False);
    }
    if (showTimings) {
      timer.show ("  Insert      ");
    }
    return tab;
  }


  //# Execute the deletes.
  void TableParseQuery::doDelete (Bool showTimings, Table& table)
  {
    //# If the selection is empty, return immediately.
    if (rownrs_p.empty()) {
      return;
    }
    Timer timer;
    // Reopen the table for write.
    table.reopenRW();
    if (! table.isWritable()) {
      throw TableInvExpr ("Table " + table.tableName() + " is not writable");
    }
    // Delete all rows.
    table.removeRow (rownrs_p);
    if (showTimings) {
      timer.show ("  Delete      ");
    }
  }


  //# Execute the counts.
  Table TableParseQuery::doCount (Bool showTimings, const Table& table)
  {
    Timer timer;
    // First do the column projection.
    Table intab = doProject (False, table);
    // Create an empty memory table with the same description as the input table.
    Table tab = TableCopy::makeEmptyMemoryTable ("", intab, True);
    // Add the Int64 _COUNT_ column.
    ScalarColumnDesc<Int64> countDesc ("_COUNT_");
    tab.addColumn (countDesc);
    ScalarColumn<Int64> countCol(tab, "_COUNT_");
    // Iterate for all columns through the input table.
    Vector<String> colNames = intab.tableDesc().columnNames();
    Block<String> bcolNames(colNames.size());
    std::copy (colNames.begin(), colNames.end(), bcolNames.begin());
    TableIterator iter (intab, bcolNames);
    while (!iter.pastEnd()) {
      Table tabfrom = iter.table();
      // Add one row containing the column values.
      rownr_t rownr = tab.nrow();
      tab.addRow();
      Table tabto = tab.project (bcolNames);
      TableCopy::copyRows (tabto, tabfrom, rownr, 0, 1);
      // Put the count.
      countCol.put (rownr, tabfrom.nrow());
      iter++;
    }
    if (showTimings) {
      timer.show ("  Count       ");
    }
    return tab;
  }


  //# Execute the groupby.
  std::shared_ptr<TableExprGroupResult> TableParseQuery::doGroupby
  (Bool showTimings)
  {
    Timer timer;
    std::shared_ptr<TableExprGroupResult> result = groupby_p.execGroupAggr(rownrs_p);
    if (showTimings) {
      timer.show ("  Groupby     ");
    }
    return result;
  }

  Table TableParseQuery::adjustApplySelNodes (const Table& table)
  {
    for (std::vector<TableExprNode>::iterator iter=applySelNodes_p.begin();
         iter!=applySelNodes_p.end(); ++iter) {
      iter->applySelection (rownrs_p);
    }
    // Create the subset.
    Table tab(table(rownrs_p));
    // From now on use row numbers 0..n.
    indgen (rownrs_p);
    return tab;
  }

  Bool TableParseQuery::doHaving (Bool showTimings,
                                  const std::shared_ptr<TableExprGroupResult>& groups)
  {
    Timer timer;
    // Find the rows matching the HAVING expression.
    Bool done = groupby_p.execHaving (rownrs_p, groups);
    if (showTimings) {
      timer.show ("  Having      ");
    }
    return done;
  }

  //# Execute the sort.
  void TableParseQuery::doSort (Bool showTimings)
  {
    //# If no rows, return immediately.
    //# (the code below will fail if empty)
    if (rownrs_p.empty()) {
      return;
    }
    Timer timer;
    // Create and fill a Sort object for all keys.
    // The data are kept in vector Arrays and are automatically deleted at the end.
    std::vector<std::shared_ptr<ArrayBase>> arrays;
    Sort sort;
    for (auto& sortKey : sort_p) {
      arrays.push_back (sortKey.addSortValues (sort, order_p, rownrs_p));
    }
    rownr_t nrrow = rownrs_p.size();
    Vector<rownr_t> newRownrs (nrrow);
    int sortOpt = Sort::HeapSort;
    if (noDupl_p) {
      sortOpt += Sort::NoDuplicates;
    }
    sort.sort (newRownrs, nrrow, sortOpt);
    if (showTimings) {
      timer.show ("  Orderby     ");
    }
    // Convert index to rownr.
    for (rownr_t i=0; i<newRownrs.size(); ++i) {
      newRownrs[i] = rownrs_p[newRownrs[i]];
    }
    rownrs_p.reference (newRownrs);
  }


  void TableParseQuery::doLimOff (Bool showTimings)
  {
    Timer timer;
    Vector<rownr_t> newRownrs;
    // Negative values mean from the end (a la Python indexing).
    Int64 nrow = rownrs_p.size();
    if (offset_p < 0) {
      offset_p += nrow;
      if (offset_p < 0) offset_p = 0;
    }
    // A limit (i.e. nr of rows) or an endrow can be given (not both).
    // Convert a limit to endrow.
    if (limit_p != 0) {
      if (limit_p  < 0) limit_p  += nrow;
      endrow_p = offset_p + limit_p*stride_p;
    } else if (endrow_p != 0) {
      if (endrow_p < 0) endrow_p += nrow;
    } else {
      endrow_p = nrow;
    }
    if (endrow_p > nrow) endrow_p = nrow;
    if (offset_p < endrow_p) {
      Int64 nr = 1 + (endrow_p - offset_p - 1) / stride_p;
      newRownrs.reference (rownrs_p(Slice(offset_p, nr, stride_p)).copy());
    }
    rownrs_p.reference (newRownrs);
    if (showTimings) {
      timer.show ("  Limit/Offset");
    }
  }

  Table TableParseQuery::doLimOff (Bool showTimings, const Table& table)
  {
    Timer timer;
    rownrs_p.resize (table.nrow());
    indgen (rownrs_p);
    doLimOff (False);
    return table(rownrs_p);
    if (showTimings) {
      timer.show ("  Limit/Offset");
    }
  }


  Table TableParseQuery::doProject
  (Bool showTimings, const Table& table,
   const std::shared_ptr<TableExprGroupResult>& groups)
  {
    Timer timer;
    Table tabp;
    // doProjectExpr might have been done for some columns, so clear first to avoid
    // they are calculated twice.
    update_p.clear();
    if (tableProject_p.hasExpressions()) {
      // Expressions used, so make a real table.
      tabp = doProjectExpr (False, groups);
    } else {
      // Only column names used, so make a reference table.
      tabp = table(rownrs_p);
      tabp = tableProject_p.project (tabp);
    }
    if (showTimings) {
      timer.show ("  Projection  ");
    }
    if (distinct_p) {
      tabp = doDistinct (showTimings, tabp);
    }
    return tabp;
  }

  Table TableParseQuery::doProjectExpr
  (Bool useSel, const std::shared_ptr<TableExprGroupResult>& groups)
  {
    if (! rownrs_p.empty()) {
      // Add the rows if not done yet.
      if (projectExprTable_p.nrow() == 0) {
        projectExprTable_p.addRow (rownrs_p.size());
      }
      // Turn the expressions of the selected columns into update objects.
      tableProject_p.makeUpdate (useSel, *this);
      // Fill the columns in the table.
      doUpdate (False, Table(), projectExprTable_p, rownrs_p, groups);
      projectExprTable_p.flush();
    }
    return projectExprTable_p;
  }

  Table TableParseQuery::doFinish (Bool showTimings, Table& table,
                                   const std::vector<const Table*>& tempTables,
                                   const std::vector<TableParseQuery*>& stack)
  {
    Timer timer;
    Table result(table);
    // If the table name contains ::, a subtable has to be created.
    // Split the name at the last ::.
    Vector<String> parts = stringToVector(resultName_p, std::regex("::"));
    Table parent;
    String fullName (resultName_p);
    if (parts.size() > 1) {
      parent = TableParseUtil::openParentTable (resultName_p,
                                                parts[parts.size()-1],
                                                tempTables, stack);
      fullName = parent.tableName() + '/' + parts[parts.size()-1];
    }
    if (resultType_p == 1) {
      if (table.tableType() != Table::Memory) {
        result = table.copyToMemoryTable (fullName);
      }
    } else if (! resultCreated_p) {
      if (resultType_p > 0) {
        table.deepCopy (fullName, tableProject_p.dminfo(), storageOption_p,
                        overwrite_p ? Table::New : Table::NewNoReplace,
                        True, endianFormat_p);
        result = Table(fullName);
      } else {
        // Normal reference table.
        table.rename (fullName,
                      overwrite_p ? Table::New : Table::NewNoReplace);
        table.flush();
      }
    }
    // Create a subtable keyword if needed.
    if (parts.size() > 1) {
      parent.reopenRW();
      parent.rwKeywordSet().defineTable (parts[parts.size()-1], table);
    }
    if (showTimings) {
      timer.show ("  Giving      ");
    }
    return result;
  }

  Table TableParseQuery::doDistinct (Bool showTimings, const Table& table)
  {
    Timer timer;
    Table result;
    // Sort the table uniquely on all columns.
    Table tabs = table.sort (tableProject_p.getColumnNames(), Sort::Ascending,
                             Sort::QuickSort|Sort::NoDuplicates);
    if (tabs.nrow() == table.nrow()) {
      // Everything was already unique.
      result = table;
    } else {
      // Get the rownumbers.
      // Make sure it does not reference an internal array.
      Vector<rownr_t> rownrs(tabs.rowNumbers(table));
      rownrs.unique();
      // Put the rownumbers back in the original order.
      GenSort<rownr_t>::sort (rownrs);
      result = table(rownrs);
      rownrs_p.reference (rownrs);
    }
    if (showTimings) {
      timer.show ("  Distinct    ");
    }
    return result;
  }


  //# Keep the name of the resulting table.
  void TableParseQuery::handleGiving (const String& name, const Record& rec)
  {
    resultName_p = name;
    for (uInt i=0; i<rec.nfields(); ++i) {
      String fldName = rec.name(i);
      fldName.downcase();
      Bool done=False;
      if (rec.dataType(i) == TpBool) {
        done = True;
        if (fldName == "memory") {
          resultType_p = 1;
        } else if (fldName == "scratch") {
          resultType_p = 2;
        } else if (fldName == "plain") {
          resultType_p = 3;
        } else if (fldName == "plain_big") {
          resultType_p   = 3;
          endianFormat_p = Table::BigEndian;
        } else if (fldName == "plain_little") {
          resultType_p   = 3;
          endianFormat_p = Table::LittleEndian;
        } else if (fldName == "plain_local") {
          resultType_p   = 3;
          endianFormat_p = Table::LocalEndian;
        } else if (fldName == "overwrite") {
          overwrite_p = rec.asBool(i);
        } else {
          done = False;
        }
      }
      if (done) {
        if (fldName != "overwrite"  &&  !rec.asBool(i)) {
          throw TableParseError ("Field name " + rec.name(i) +
                                 " should not have a False value");
        }
      } else if (fldName == "type") {
        Bool ok = False;
        if (rec.dataType(i) == TpString) {
          ok = True;
          String str = rec.asString(i);
          str.downcase();
          if (str == "plain") {
            resultType_p = 3;
          } else if (str == "scratch") {
            resultType_p = 2;
          } else if (str == "memory") {
            resultType_p = 1;
          } else {
            ok = False;
          }
        }
        if (!ok) {
          throw TableParseError("type must have a string value "
                                "plain, scratch or memory");
        }
      } else if (fldName == "endian") {
        Bool ok = False;
        if (rec.dataType(i) == TpString) {
          ok = True;
          String str = rec.asString(i);
          str.downcase();
          if (str == "big") {
            endianFormat_p = Table::BigEndian;
          } else if (str == "little") {
            endianFormat_p = Table::LittleEndian;
          } else if (str == "local") {
            endianFormat_p = Table::LocalEndian;
          } else if (str == "aipsrc") {
            endianFormat_p = Table::AipsrcEndian;
          } else {
            ok = False;
          }
        }
        if (!ok) {
          throw TableParseError("endian must have a string value "
                                "big, little, local or aipsrc");
        }
      } else if (fldName == "storage") {
        Bool ok = False;
        if (rec.dataType(i) == TpString) {
          ok = True;
          String str = rec.asString(i);
          str.downcase();
          if (str == "multifile") {
            storageOption_p.setOption (StorageOption::MultiFile);
          } else if (str == "multihdf5") {
            storageOption_p.setOption (StorageOption::MultiHDF5);
          } else if (str == "sepfile") {
            storageOption_p.setOption (StorageOption::SepFile);
          } else if (str == "default") {
            storageOption_p.setOption (StorageOption::Default);
          } else if (str == "aipsrc") {
            storageOption_p.setOption (StorageOption::Aipsrc);
          } else {
            ok = False;
          }
        }
        if (!ok) {
          throw TableParseError("storage must have a string value "
                                "multifile, multihdf5, sepfile, default or aipsrc");
        }
      } else if (fldName == "blocksize") {
        try {
          storageOption_p.setBlockSize (rec.asInt(i));
        } catch (...) {
          throw TableParseError("blocksize must have an integer value");
        }
      } else {
        throw TableParseError(rec.name(i) + " is an invalid table options field");
      }
    }
    if (resultName_p.empty()  &&  resultType_p > 2) {
      throw TableParseError ("output table name can only be omitted if "
                             "AS MEMORY or AS SCRATCH is given");
    }
  }

  //# Keep the resulting set expression.
  void TableParseQuery::handleGiving (const TableExprNodeSet& set)
  {
    // In principle GIVING is handled before SELECT, so below is always false.
    // But who knows what future brings us.
    if (! tableProject_p.getColumnNames().empty()) {
      throw TableInvExpr("Expressions can be given in SELECT or GIVING, "
                         "not both");
    }
    resultSet_p = new TableExprNodeSet (set);
    TableExprNodeUtil::checkAggrFuncs (resultSet_p);
  }

  //# Execute all parts of a TaQL command doing some selection.
  void TableParseQuery::execute (Bool showTimings, Bool setInGiving,
                                 Bool mustSelect, rownr_t maxRow,
                                 Bool doTracing,
                                 const std::vector<const Table*>& tempTables,
                                 const std::vector<TableParseQuery*>& stack)
  {
    //# A selection query consists of:
    //#  - SELECT to do projection
    //#     can only refer to columns in FROM or can be constants
    //#     can contain aggregate functions
    //#  - FROM to tell the tables to use
    //#  - WHERE to select rows from tables
    //#     can only refer to columns in FROM
    //#     cannot contain aggregate functions
    //#  - GROUPBY to group result of WHERE
    //#     can refer to columns in FROM or expressions of FROM
    //#     (in SQL92 only to columns in FROM)
    //#     cannot contain aggregate functions
    //#  - HAVING to select groups
    //#     can refer to column in SELECT or FROM
    //#     HAVING is possible without GROUPBY (-> one group only)
    //#     usually refers to aggregate functions/columns
    //#     if non-aggregate function is used, glast is implied
    //#  - apply combination (UNION, INTERSECTION, DIFFERENCE)
    //#     must have equal SELECT result (with equal column names)
    //#  - ORDERBY to sort result of HAVING
    //#     can refer to columns in SELECT or FROM
    //#     (in SQL92 only to columns in SELECT), thus look in SELECT first
    //#     can contain aggregate functions if aggregation or GROUPBY is used
    //#  - LIMIT to skip latest results of ORDERBY
    //#  - OFFSET to ignore first results of ORDERBY
    //# If GROUPBY/aggr is used, all clauses can contain other columns than
    //# aggregate or GROUPBY columns. The last row in a group is used for them.

    //# Set limit if not given.
    if (limit_p == 0) {
      limit_p = maxRow;
      if (doTracing  &&  limit_p) {
        cerr << "LIMIT not given; set to " << limit_p << endl;
      }
    }
    //# Give an error if no command part has been given.
    if (mustSelect  &&  commandType_p == PSELECT
        &&  node_p.isNull()  &&  sort_p.size() == 0
        &&  tableProject_p.getColumnNames().empty()  &&  resultSet_p == 0
        &&  limit_p == 0  &&  endrow_p == 0  &&  stride_p == 1  &&  offset_p == 0) {
      throw (TableInvExpr
             ("TableParse error: no projection, selection, sorting, "
              "limit, offset, or giving-set given in SELECT command"));
    }
    // Test if a "giving set" is possible.
    if (resultSet_p != 0  &&  !setInGiving) {
      throw TableInvExpr ("A query in a FROM can only have "
                          "'GIVING tablename'");
    }
    //# Set the project expressions to be filled in first stage.
    tableProject_p.makeProjectExprSel();
    // Make sure WHERE expression does not use aggregate functions.
    // It has been checked before, but use defensive programming.
    TableParseGroupby::checkAggrFuncs (node_p);
    //# Get nodes representing aggregate functions.
    //# Test if aggregate, groupby, or having is used.
    groupby_p.findGroupAggr (tableProject_p.getColumnExpr(),
                             commandType_p == PSELECT);
    if (! groupby_p.isUsed()) {
      // Check if tables used in projection have the same size.
      tableProject_p.checkTableProjSizes();
    } else if (doTracing) {
      cerr << "GROUPBY to be done using " << groupby_p.size()
           << " aggregate nodes" << endl;
    }
    // Column nodes used in aggregate functions should not adhere applySelection.
    uInt ndisabled = groupby_p.disableApplySelection();
    if (doTracing) {
      cerr << "  disableApplySelection done in " << ndisabled
           << " column nodes of aggregate nodes" << endl;
    }
    // Select distinct makes no sense if aggregate and no groupby is given.
    if (groupby_p.isOnlyAggr()) {
      distinct_p = False;
    }
    //# The first table in the list is the source table.
    Table table = tableList_p.firstTable();
    //# Set endrow_p if positive limit and positive or no offset.
    if (offset_p >= 0  &&  limit_p > 0) {
      endrow_p = offset_p + limit_p * stride_p;
    }
    //# Determine if we can pre-empt the selection loop.
    //# That is possible if a positive limit and offset are given
    //# without sorting, select distinct, groupby, or aggregation.
    rownr_t nrmax=0;
    if (endrow_p > 0  &&  sort_p.size() == 0  &&  !distinct_p  &&
        !groupby_p.isUsed()) {
      nrmax = endrow_p;
      if (doTracing) {
        cerr << "pre-empt WHERE at " << nrmax << " rows" << endl;
      }
    }
    //# First do the where selection.
    Table resultTable(table);
    if (! node_p.isNull()) {
      //#//        cout << "Showing TableExprRange values ..." << endl;
      //#//        Block<TableExprRange> rang;
      //#//        node_p->ranges(rang);
      //#//        for (Int i=0; i<rang.size(); i++) {
      //#//            cout << rang[i].getColumn().columnDesc().name() << rang[i].start()
      //#//                 << rang[i].end() << endl;
      //#//        }
      Timer timer;
      resultTable = table(node_p, nrmax);
      if (showTimings) {
        timer.show ("  Where       ");
      }
      if (doTracing) {
        cerr << "WHERE resulted in " << resultTable.nrow() << " rows" << endl;
      }
    }
    // Get the row numbers of the result of the possible first step.
    rownrs_p.reference (resultTable.rowNumbers(table));
    // Execute possible groupby/aggregate.
    std::shared_ptr<TableExprGroupResult> groupResult;
    if (groupby_p.isUsed() != 0) {
      groupResult = doGroupby (showTimings);
      // Aggregate results and normal table rows need to have the same rownrs,
      // so set the selected rows in the table column objects.
      resultTable = adjustApplySelNodes(table);
      table = resultTable;
      if (doTracing) {
        cerr << "GROUPBY resulted in " << table.nrow() << " groups" << endl;
        cerr << "  applySelection called for " << applySelNodes_p.size()
             << " nodes" << endl;
      }
    }
    // Do the projection of SELECT columns used in HAVING or ORDERBY.
    // Thereafter the column nodes need to use rownrs 0..n.
    if (tableProject_p.nColumnsPreCalc() > 0) {
      doProjectExpr (True, groupResult);
      resultTable = adjustApplySelNodes(table);
      table = resultTable;
      if (doTracing) {
        cerr << "Pre-projected " << tableProject_p.nColumnsPreCalc()
             << " columns" << endl;
        cerr << "  applySelection called for " << applySelNodes_p.size()
             << " nodes" << endl;
      }
    }
    // Do the possible HAVING step.
    if (doHaving (showTimings, groupResult)) {
      if (doTracing) {
        cerr << "HAVING resulted in " << rownrs_p.size() << " rows" << endl;
      }
    }
    //# Then do the sort.
    if (sort_p.size() > 0) {
      doSort (showTimings);
      if (doTracing) {
        cerr << "ORDERBY resulted in " << rownrs_p.size() << " rows" << endl;
      }
    }
    // If select distinct is given, limit/offset can only be done thereafter
    // because duplicate rows will be removed.
    if (!distinct_p  &&  (offset_p != 0  ||  limit_p != 0  ||
                          endrow_p != 0  || stride_p != 1)) {
      doLimOff (showTimings);
      if (doTracing) {
        cerr << "LIMIT/OFFSET resulted in " << rownrs_p.size() << " rows" << endl;
      }
    }
    // Take the correct rows of the projected table (if not empty).
    resultTable = table(rownrs_p);
    if (projectExprTable_p.nrow() > 0) {
      if (rownrs_p.size() < projectExprTable_p.nrow()  ||  sort_p.size() > 0) {
        projectExprTable_p = projectExprTable_p(rownrs_p);
        // Make deep copy if stored in a table.
        if (resultType_p == 3) {
          projectExprTable_p.rename (resultName_p + "_tmpproject",
                                     Table::New);
          projectExprTable_p.deepCopy
            (resultName_p, tableProject_p.dminfo(), storageOption_p,
             overwrite_p ? Table::New : Table::NewNoReplace,
             True, endianFormat_p);
          projectExprTable_p = Table(resultName_p);
          TableUtil::deleteTable (resultName_p + "_tmpproject");
          // Indicate it does not have to be created anymore.
          resultCreated_p = True;
        }
        resultTable = projectExprTable_p;
      }
    }
    //# Then do the update, delete, insert, or projection and so.
    if (commandType_p == PUPDATE) {
      doUpdate (showTimings, table, resultTable, rownrs_p);
      table.flush();
    } else if (commandType_p == PINSERT) {
      Table tabNewRows = doInsert (showTimings, table);
      table.flush();
      resultTable = tabNewRows;
    } else if (commandType_p == PDELETE) {
      doDelete (showTimings, table);
      table.flush();
    } else if (commandType_p == PCOUNT) {
      resultTable = doCount (showTimings, table);
    } else {
      //# Then do the projection.
      if (tableProject_p.getColumnNames().size() > 0) {
        resultTable = doProject (showTimings, table, groupResult);
        if (doTracing) {
          cerr << "Final projection done of "
               << tableProject_p.getColumnNames().size() -
                  tableProject_p.nColumnsPreCalc()
               << " columns resulting in " << resultTable.nrow()
               << " rows" << endl;
        }
      }
      // If select distinct is given, limit/offset must be done at the end.
      if (distinct_p  &&  (offset_p != 0  ||  limit_p != 0  ||
                           endrow_p != 0  || stride_p != 1)) {
        resultTable = doLimOff (showTimings, resultTable);
        if (doTracing) {
          cerr << "LIMIT/OFFSET resulted in " << resultTable.nrow()
               << " rows" << endl;
        }
      }
      //# Finally rename or copy using the given name (and flush it).
      if (resultType_p != 0  ||  ! resultName_p.empty()) {
        resultTable = doFinish (showTimings, resultTable, tempTables, stack);
        if (doTracing) {
          cerr << "Finished the GIVING command" << endl;
        }
      }
    }
    //# Keep the table for later.
    table_p = resultTable;
  }

  String TableParseQuery::getTableStructure (const Vector<String>& parts,
                                             const TaQLStyle& style)
  {
    Bool showdm = False;
    Bool showcol = True;
    Bool showsub = False;
    Bool sortcol = False;
    Bool tabkey = False;
    Bool colkey = False;
    for (uInt i=2; i<parts.size(); ++i) {
      String opt(parts[i]);
      opt.downcase();
      Bool fop = True;
      if (opt.size() > 2   &&  opt.substr(0,2) == "no") {
        fop = False;
        opt = opt.substr(2);
      }
      if (opt == "dm") {
        showdm = fop;
      } else if (opt == "col") {
        showcol = fop;
      } else if (opt == "sort") {
        sortcol = fop;
      } else if (opt == "key") {
        tabkey = fop;
        colkey = fop;
      } else if (opt == "tabkey") {
        tabkey = fop;
      } else if (opt == "colkey") {
        colkey = fop;
      } else if (opt == "recur") {
        showsub = fop;
      } else {
        throw TableInvExpr (parts[i] + " is an unknown show table option; use: "
                            "dm col sort key tabkey colkey recur");
      }
    }
    std::ostringstream os;
    tableList_p.firstTable().showStructure (os, showdm, showcol, showsub,
                                            sortcol, style.isCOrder());
    tableList_p.firstTable().showKeywords (os, showsub, tabkey, colkey);
    return os.str();
  }


  void TableParseQuery::show (ostream& os) const
  {
    if (! node_p.isNull()) {
      node_p.show (os);
    }
  }


} //# NAMESPACE CASACORE - END
