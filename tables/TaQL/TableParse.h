//# TableParse.h: Classes to hold results from table grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#ifndef TABLES_TABLEPARSE_H
#define TABLES_TABLEPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/TaQLResult.h>
#include <casacore/tables/TaQL/ExprGroup.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Containers/Block.h>
#include <map>
#include <vector>
#include <limits>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableExprNodeSet;
class TableExprNodeSetElem;
class TableExprNodeIndex;
class TableColumn;
class AipsIO;
template<class T> class Vector;


// <summary>
// Class to hold values from table grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTableGram">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <etymology>
// TableParse is the class used to parse a table command.
// </etymology>

// <synopsis> 
// TableParse is used by the parser of table select statements.
// The parser is written in Bison and Flex in files TableGram.y and .l.
// The statements in there use the routines in this file to act
// upon a reduced rule.
// Since multiple tables can be given (with a shorthand), the table
// names are stored in a container. The variable names can be qualified
// by the table name and will be looked up in the appropriate table.
//
// A select command is similar to SQL and can look like:
//    SELECT columns FROM tab1 sh1, tab2 sh2, tab3 WHERE
//           sh1.field == 3*sh1.field2 ... ORDERBY columns GIVING table
// This is described in more detail in TableGram.l.
//
// The class TableParse only contains information about a table
// used in the table command.
//
// Global functions are used to operate on the information.
// The main function is the global function tableCommand.
// It executes the given TaQL command and returns the resulting table.
// This is, in fact, the only function to be used by a user.
// </synopsis> 

// <motivation>
// It is necessary to be able to give a table select command in ASCII.
// This can be used in a CLI or in the table browser to get a subset
// of a table or to sort a table.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableParse
{

public:
  // Default constructor for container class.
  TableParse();

  // Copy constructor (copy semantics).
  TableParse (const TableParse&);

  // Assignment (copy semantics).
  TableParse& operator= (const TableParse&);

  // Associate the table and the shorthand.
  TableParse (const Table& table, const String& shorthand);

  // Test if shorthand matches.
  Bool test (const String& shortHand) const;

  // Get the shorthand.
  const String& shorthand() const;

  // Get table object.
  const Table& table() const;

private:
  String  shorthand_p;
  Table   table_p;
};



// <synopsis>
// Parse and execute the given command.
// It will open (and close) all tables needed.
// It returns the resulting table. 
// The command type (select or update) and the selected or updated
// column names can be returned.
// Zero or more temporary tables can be used in the command
// using the $nnn syntax.
// </synopsis>
// <group name=tableCommand>
TaQLResult tableCommand (const String& command);

TaQLResult tableCommand (const String& command,
			 const Table& tempTable);
TaQLResult tableCommand (const String& command,
			 const std::vector<const Table*>& tempTables);
TaQLResult tableCommand (const String& command,
			 Vector<String>& columnNames);
TaQLResult tableCommand (const String& command,
			 Vector<String>& columnNames,
			 String& commandType);
TaQLResult tableCommand (const String& command,
			 const std::vector<const Table*>& tempTables,
			 Vector<String>& columnNames);
TaQLResult tableCommand (const String& command,
			 const std::vector<const Table*>& tempTables,
			 Vector<String>& columnNames,
			 String& commandType);
// </group>




// <summary>
// Helper class for sort keys in TableParse
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableParse
// </prerequisite>

// <etymology>
// TableParseSort holds a sort expression and order.
// </etymology>

// <synopsis> 
// A table command is parsed.
// An object of this class is used to hold the sort expression
// and sort order.
// </synopsis> 


class TableParseSort
{
public:
    // Construct from a given expression.
    // The order is not given.
    TableParseSort();

    // Construct from a given expression.
    // The order is not given.
    explicit TableParseSort (const TableExprNode&);

    // Construct from a given expression and for the given order.
    TableParseSort (const TableExprNode&, Sort::Order);

    ~TableParseSort();

    // Get the expression node.
    const TableExprNode& node() const;

    // Get the sort order.
    Sort::Order order() const;

    // Is the order given?
    Bool orderGiven() const;

private:
    // Check if the node results in a scalar and does not contain
    // aggregate functions.
    void checkNode() const;

    TableExprNode node_p;
    Sort::Order   order_p;
    Bool          given_p;
};




// <summary>
// Helper class for updates in TableParse
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableParse
// </prerequisite>

// <etymology>
// TableParseUpdate holds a column name, optional indices, and an
// update expression.
// </etymology>

// <synopsis> 
// A table command is parsed.
// An object of this class is used to hold the column name, optional indices,
// and value expression for the UPDATE command.
// </synopsis> 


class TableParseUpdate
{
public:
  TableParseUpdate()
    : indexPtr_p(0) {}

  // Construct from a column name and expression.
  // By default it checks if no aggregate functions are used.
  TableParseUpdate (const String& columnName, const TableExprNode&,
                    Bool checkAggr=True);

  // Construct from a column name, subscripts, and expression.
  // It checks if no aggregate functions are used.
  TableParseUpdate (const String& columnName,
		    const TableExprNodeSet& indices,
		    const TableExprNode&,
		    const TaQLStyle&);

  ~TableParseUpdate();

  // Set the column name.
  void setColumnName (const String& name);

  // Get the column name.
  const String& columnName() const;

  // Get the pointer to the indices.
  TableExprNodeIndex* indexPtr() const;

  // Get the index expression node.
  const TableExprNode& indexNode() const;

  // Get the expression node.
  // <group>
  const TableExprNode& node() const;
  TableExprNode& node();
  // </group>

  // Adapt the possible unit of the expression to the possible unit
  // of the column.
  void adaptUnit (const Unit& columnUnit);

private:
  String              columnName_p;
  TableExprNodeIndex* indexPtr_p;      //# copy of pointer in indexNode_p
  TableExprNode       indexNode_p;
  TableExprNode       node_p;
};




// <summary>
// Select-class for flex/bison scanner/parser for TableParse
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//  <li> TableParse
//  <li> TableGram.l and .y  (flex and bison grammar)
// </prerequisite>

// <synopsis> 
// This class is needed for the the actions in the flex scanner
// and bison parser.
// This stores the information by constructing TableParse objects
// as needed and storing them in a vector.
// </synopsis> 

// <motivation>
// It is necessary to be able to give a table select command in ASCII.
// This can be used in a CLI or in the table browser to get a subset
// of a table or to sort a table.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableParseSelect
{
public:
  enum CommandType {
    PSELECT,
    PUPDATE,
    PINSERT,
    PDELETE,
    PCOUNT,
    PCALC,
    PCRETAB
  };

  enum GroupAggrType {
    GROUPBY=1,
    AGGR_FUNCS=2,
    ONLY_COUNTALL=4
  };

  // Construct.
  TableParseSelect (CommandType type);

  // Destructor.
  ~TableParseSelect();

  // Return the command type.
  CommandType commandType() const
    { return commandType_p; }

  // Return the expression node.
  TableExprNode getNode() const
    { return node_p; }

  // Execute the select command (select/sort/projection/groupby/having/giving).
  // The setInGiving flag tells if a set in the GIVING part is allowed.
  // The mustSelect flag tells if a SELECT command must do something.
  // Usually that is required, but not for a SELECT in an INSERT command.
  // Optionally the maximum nr of rows to be selected can be given.
  // It will be used as the default value for the LIMIT clause.
  // 0 = no maximum.
  void execute (Bool showTimings, Bool setInGiving,
                Bool mustSelect, uInt maxRow, Bool doTracing=False);

  // Execute a query in a from clause resulting in a Table.
  Table doFromQuery (Bool showTimings);

  // Execute a subquery and create an appropriate node for the result.
  TableExprNode doSubQuery (Bool showTimings);

  // Test if a subquery has sufficient elements.
  // It uses default LIMIT=1, but that can be overidden in the subquery.
  // The flag tells if NOT EXISTS or EXISTS was given.
  TableExprNode doExists (Bool noexists, Bool showTimings);

  // Show the expression tree.
  void show (ostream& os) const;

  // Keep the selection expression.
  void handleWhere (const TableExprNode&);

  // Keep the groupby expressions.
  // It checks if they are all scalar expressions.
  void handleGroupby (const vector<TableExprNode>&, Bool rollup);

  // Keep the having expression.
  void handleHaving (const TableExprNode&);

  // Keep the expression of a calculate command.
  void handleCalcComm (const TableExprNode&);

  // Keep the create table command.
  void handleCreTab (const String& tableName, const Record& dmInfo);

  // Keep the column specification in a create table command.
  void handleColSpec (const String& columnName, const String& dataType,
		      const Record& spec, Bool isCOrder=False);

  // Add an update object.
  void addUpdate (TableParseUpdate* upd);

  // Keep the update expressions.
  void handleUpdate();

  // Make ready for the insert expression.
  // The first one uses values (added via addUpdate),
  // the second one a subquery.
  // <group>
  void handleInsert();
  void handleInsert (TableParseSelect* sel);
  // </group>

  // Make ready for a COUNT command.
  // It checks if all column expressions are scalar.
  void handleCount();

  // Keep the sort expressions.
  void handleSort (const std::vector<TableParseSort>& sortList,
		   Bool noDuplicates, Sort::Order defaultSortOrder);

  // Evaluate and keep limit/offset/stride given as start:end:incr
  void handleLimit (const TableExprNodeSetElem& expr);

  // Evaluate and keep the limit value.
  void handleLimit (const TableExprNode& expr);

  // Evaluate and keep the offset value.
  void handleOffset (const TableExprNode& expr);

  // Add a table nr, name, or object to the container.
  void addTable (Int tabnr, const String& name,
		 const Table& table,
		 const String& shorthand,
		 const vector<const Table*> tempTables,
		 const vector<TableParseSelect*>& stack);

  // Replace the first table (used by CALC command).
  void replaceTable (const Table& table);

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

  // Make a function object node for the given function name and arguments.
  // The ignoreFuncs vector contains invalid function codes.
  static TableExprNode makeFuncNode (const String& name,
				     const TableExprNodeSet& arguments,
				     const Vector<int>& ignoreFuncs,
				     const Table& table,
				     const TaQLStyle&);

  // Add a column to the list of column names.
  void handleColumn (Int type, const String& name, const TableExprNode& expr,
		     const String& newName, const String& newDtype);

  // Finish the addition of columns to the list of column names.
  void handleColumnFinish (Bool distinct);

  // Handle the name and type given in a GIVING clause.
  void handleGiving (const String& name, Int type);

  // Handle the set given in a GIVING clause.
  void handleGiving (const TableExprNodeSet&);

  // Get the projected column names.
  const Block<String>& getColumnNames() const;

  // Get the resulting table.
  const Table& getTable() const;

  // An exception is thrown if the node uses an aggregate function.
  static void checkAggrFuncs (const TableExprNode& node);

private:
  // Test if groupby or aggregate functions are given.
  // <br> bit 0:  on = groupby is given
  // <br> bit 1:  on = aggregate functions are given
  // <br> bit 2:  on = only select count(*) aggregate function is given
  Int testGroupAggr (vector<TableExprNodeRep*>& aggr) const;

  // Get the aggregate functions used in SELECT and HAVING.
  vector<TableExprNodeRep*> getAggrNodes() const;

  // Find the function code belonging to a function name.
  // Functions to be ignored can be given (as function type values).
  // If the function name is unknown, NRFUNC is returned.
  static TableExprFuncNode::FunctionType findFunc
                                   (const String& name,
				    uInt narguments,
				    const Vector<Int>& ignoreFuncs);

  // Do the update step.
  // Rows 0,1,2,.. in UpdTable are updated from the expression result
  // for the rows in the given rownrs vector.
  void doUpdate (Bool showTimings, const Table& origTable,
                 Table& updTable, const Vector<uInt>& rownrs,
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

  // Make the (empty) table for the epxression in the SELECT clause.
  void makeProjectExprTable();

  // Fill projectExprSelColumn_p telling the columns to be projected
  // at the first stage.
  void makeProjectExprSel();

  // Set the selected rows for the column objects in applySelNodes_p.
  // These nodes refer the original table. They requires different row
  // numbers than the selected groups and projected columns.
  // rownrs_p is changed to use row 0..n.
  // It returns the Table containing the subset of rows in the input Table.
  Table adjustApplySelNodes (const Table&);

  // Do the groupby/aggregate step and return its result.
  CountedPtr<TableExprGroupResult> doGroupby
  (bool showTimings, vector<TableExprNodeRep*> aggrNodes,
   Int groupAggrUsed);

  // Do the HAVING step.
  void doHaving (Bool showTimings,
                 const CountedPtr<TableExprGroupResult>& groups);

  // Do a groupby/aggregate step that only does a 'select count(*)'.
  CountedPtr<TableExprGroupResult> doOnlyCountAll (TableExprNodeRep* aggrNode);

  // Do a full groupby/aggregate step.
  CountedPtr<TableExprGroupResult> doGroupByAggr
  (const vector<TableExprNodeRep*>& aggrNodes);

  // Do the sort step.
  void doSort (Bool showTimings);

  // Do the limit/offset step.
  void  doLimOff (Bool showTimings);
  Table doLimOff (Bool showTimings, const Table& table);

  // Do the 'select distinct' step.
  Table doDistinct (Bool showTimings, const Table& table);

  // Finish the table (rename, copy, and/or flush).
  Table doFinish (Bool showTimings, Table& table);

  // Update the values in the columns (helpers of doUpdate).
  // <group>
  template<typename TCOL, typename TNODE>
  void updateValue2 (uInt row, const TableExprId& rowid, Bool isScalarCol,
                     const TableExprNode& node, TableColumn& col,
                     const Slicer* slicerPtr,
                     IPosition& blc, IPosition& trc, IPosition& inc);
  template<typename T>
  void updateValue1 (uInt row, const TableExprId& rowid, Bool isScalarCol,
                     const TableExprNode& node, TableColumn& col,
                     const Slicer* slicerPtr,
                     IPosition& blc, IPosition& trc, IPosition& inc);
  // </group>

  // Make a data type from the string.
  // It checks if it is compatible with the given (expression) data type.
  DataType makeDataType (DataType dtype, const String& dtstr,
			 const String& colName);

  // Get the order for this key. Use the default order_p if not
  // explicitly given with the key.
  Sort::Order getOrder (const TableParseSort& key) const;

  // Make an array from the contents of a column in a subquery.
  TableExprNode getColSet();

  // Make a set from the results of the subquery.
  TableExprNode makeSubSet() const;

  // Evaluate an int scalar expression.
  Int64 evalIntScaExpr (const TableExprNode& expr) const;

  // Split a name into its parts (shorthand, column and field names).
  // True is returned when the name contained a keyword part.
  // In that case fieldNames contains the keyword name and the possible
  // subfields. The possible shorthand and the column name are
  // filled in if it is a column keyword.
  // If the name represents a column, fieldNames contains the subfields
  // of the column (for the case where the column contains records).
  // If the name is invalid, an exception is thrown if checkError=True.
  // Otherwise the name is treated as a normal name without keyword.
  Bool splitName (String& shorthand, String& columnName,
		  Vector<String>& fieldNames, const String& name,
		  Bool checkError) const;

  // Find a table for the given shorthand.
  // If no shorthand is given, the first table is returned (if there).
  // If not found, a null Table object is returned.
  Table findTable (const String& shorthand) const;

  // Handle the selection of a wildcarded column name.
  void handleWildColumn (Int stringType, const String& name);

  // Add the description of a column to the table description.
  // ndim < 0 means a scalar column.
  void addColumnDesc (TableDesc& td, DataType dtype,
		      const String& colName, Int options,
		      Int ndim, const IPosition& shape,
		      const String& dmType, const String& dmGroup,
		      const String& comment,
		      const String& unitName);

  // Find the names of all stored columns in a table.
  Block<String> getStoredColumns (const Table& tab) const;

  // Try to find the keyword representing a table in one of the tables
  // in any select block (from inner to outer).
  // If not found, an exception is thrown.
  static Table tableKey (const String& fullName,
                         const String& shorthand, const String& columnName,
			 const Vector<String>& fieldNames,
			 const vector<TableParseSelect*>& stack);

  // Try to find the keyword representing a table in the given table.
  // If the columnName is empty, the keyword is a table keyword.
  // If not found, a null Table object is returned.
  static Table findTableKey (const Table& table, const String& columnName,
			     const Vector<String>& keyNames);

  // Create the set of aggregate functions and groupby keys in case
  // a single groupby key is given.
  // This offers much faster map access then doGroupByAggrMultiple.
  template<typename T>
  vector<CountedPtr<TableExprGroupFuncSet> > doGroupByAggrSingleKey
  (const vector<TableExprNodeRep*>& aggrNodes)
  {
    // We have to group the data according to the (possibly empty) groupby.
    // We step through the table in the normal order which may not be the
    // groupby order.
    // A map<key,int> is used to keep track of the results where the int
    // is the index in a vector of a set of aggregate function objects.
    vector<CountedPtr<TableExprGroupFuncSet> > funcSets;
    std::map<T, int> keyFuncMap;
    T lastKey = std::numeric_limits<Double>::max();
    int groupnr = -1;
    // Loop through all rows.
    // For each row generate the key to get the right entry.
    TableExprId rowid(0);
    T key;
    for (uInt i=0; i<rownrs_p.size(); ++i) {
      rowid.setRownr (rownrs_p[i]);
      groupbyNodes_p[0].get (rowid, key);
      if (key != lastKey) {
        typename std::map<T, int>::iterator iter = keyFuncMap.find (key);
        if (iter == keyFuncMap.end()) {
          groupnr = funcSets.size();
          keyFuncMap[key] = groupnr;
          funcSets.push_back (new TableExprGroupFuncSet (aggrNodes));
        } else {
          groupnr = iter->second;
        }
      }
      rowid.setRownr (rownrs_p[i]);
      funcSets[groupnr]->apply (rowid);
    }
    return funcSets;
  }

  // Create the set of aggregate functions and groupby keys in case
  // multiple keys are given.
  vector<CountedPtr<TableExprGroupFuncSet> > doGroupByAggrMultipleKeys
  (const vector<TableExprNodeRep*>& aggrNodes);

  //# Command type.
  CommandType commandType_p;
  //# Table description for a series of column descriptions.
  TableDesc tableDesc_p;
  //# Vector of TableParse objects.
  //# This is needed for the functions above, otherwise they have no
  //# way to communicate.
  vector<TableParse> fromTables_p;
  //# Block of selected column names (new name in case of select).
  Block<String> columnNames_p;
  //# Block of selected column expressions.
  Block<TableExprNode> columnExpr_p;
  //# The old name for a selected column.
  Block<String> columnOldNames_p;
  //# The new data type for a column.
  Block<String> columnDtypes_p;
  //# Number of real expressions used in selected columns.
  uInt nrSelExprUsed_p;
  //# Distinct values in output?
  Bool distinct_p;
  //# Name and type of the resulting table (from GIVING part).
  String resultName_p;
  Int    resultType_p;
  //# Resulting set (from GIVING part).
  TableExprNodeSet* resultSet_p;
  //# The WHERE expression tree.
  TableExprNode node_p;
  //# The GROUPBY expressions.
  vector<TableExprNode> groupbyNodes_p;
  Bool groupbyRollup_p;   //# use ROLLUP in GROUPBY? 
  //# The HAVING expression.
  TableExprNode havingNode_p;
  //# The possible limit (= max nr of selected rows) (0 means no limit).
  Int64 limit_p;
  //# The possible last row (0 means no end; can be <0).
  //# limit_p and endrow_p cannot be both !=0.
  Int64 endrow_p;
  //# The possible offset (= nr of selected rows to skip).
  Int64 offset_p;
  //# The possible stride in offset:endrow:stride.
  Int64 stride_p;
  //# The update or insert expression list.
  std::vector<TableParseUpdate*> update_p;
  //# The table selection to be inserted.
  TableParseSelect* insSel_p;
  //# The sort list.
  std::vector<TableParseSort> sort_p;
  //# The noDuplicates sort switch.
  Bool  noDupl_p;
  //# The default sort order.
  Sort::Order order_p;
  //# All nodes that need to be adjusted for a selection of rownrs.
  //# It can consist of column nodes and the rowid function node.
  //# Some nodes (in aggregate functions) can later be disabled for adjustment.
  vector<TableExprNode> applySelNodes_p;
  //# The resulting table.
  Table table_p;
  //# The first table used when creating a column object.
  //# All other tables used for them should have the same size.
  Table  firstColTable_p;
  String firstColName_p;
  //# The table resulting from a projection with expressions.
  Table projectExprTable_p;
  //# The projected columns used in the HAVING and ORDERBY clauses.
  Block<uInt>  projectExprSubset_p;
  Block<Bool>  projectExprSelColumn_p;
  //# The resulting row numbers.
  Vector<uInt> rownrs_p;
};



//# Implement the inline functions.
inline Bool TableParse::test (const String& str) const
  { return (shorthand_p == str  ?  True : False); }

inline const String& TableParse::shorthand() const
  { return shorthand_p; }

inline const Table& TableParse::table() const
  { return table_p; }


inline void TableParseUpdate::setColumnName (const String& name)
  { columnName_p = name; }
inline const String& TableParseUpdate::columnName() const
  { return columnName_p; }
inline TableExprNodeIndex* TableParseUpdate::indexPtr() const
  { return indexPtr_p; }
inline const TableExprNode& TableParseUpdate::indexNode() const
  { return indexNode_p; }
inline const TableExprNode& TableParseUpdate::node() const
  { return node_p; }
inline TableExprNode& TableParseUpdate::node()
  { return node_p; }
inline void TableParseUpdate::adaptUnit (const Unit& columnUnit)
  { node_p.adaptUnit (columnUnit); }

inline const TableExprNode& TableParseSort::node() const
  { return node_p; }
inline Bool TableParseSort::orderGiven() const
  { return given_p; }
inline Sort::Order TableParseSort::order() const
  { return order_p; }


inline const Block<String>& TableParseSelect::getColumnNames() const
  { return columnNames_p; }

inline const Table& TableParseSelect::getTable() const
  { return table_p; }

inline void TableParseSelect::addUpdate (TableParseUpdate* upd)
  { update_p.push_back (upd); }

inline Sort::Order TableParseSelect::getOrder (const TableParseSort& key) const
  { return (key.orderGiven()  ?  key.order() : order_p); }


} //# NAMESPACE CASACORE - END

#endif
