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

#if !defined(AIPS_TABLEPARSE_H)
#define AIPS_TABLEPARSE_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/Table.h>
#include <aips/BasicSL/String.h>
#include <aips/Utilities/Sort.h>
#include <aips/Containers/List.h>
#include <aips/Containers/Block.h>
#include <aips/Tables/ExprNode.h>

//# Forward Declarations
class TableExprNodeSet;
class TableExprNodeIndex;
class AipsIO;
template<class T> class Vector;


// <summary>
// Class to hold values from table grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
// names are stored in a list. The variable names can be qualified
// by the table name and will be looked up in the appropriate table.
//
// A select command is similar to SQL and can look like:
//    SELECT columns FROM tab1 sh1, tab2 sh2, tab3 WHERE
//           sh1.field == 3*sh1.field2 ... ORDERBY columns GIVING table
// This is described in more detail in TableGram.l.
//
// The class TableParse only contains information about a table
// used in the table command. Global variables (like a list and a vector)
// are used in TableParse.cc to hold further information.
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
// Dummy AipsIO routines; they are needed for the List container.
// <group>
friend AipsIO& operator<< (AipsIO&, const TableParse&);
friend AipsIO& operator>> (AipsIO&, TableParse&);
// </group>

// Parse and execute the given command.
// It will open (and close) all tables needed.
// It returns the resulting table. 
// The command type (select or update) and the selected or updated
// column names can be returned.
// Zero or more temporary tables can be used in the command
// using the $nnn syntax.
// <group>
friend Table tableCommand (const String& command);

friend Table tableCommand (const String& command, const Table& tempTable);
friend Table tableCommand (const String& command,
			   const PtrBlock<const Table*>& tempTables);
friend Table tableCommand (const String& command,
			   Vector<String>& columnNames);
friend Table tableCommand (const String& command,
			   Vector<String>& columnNames,
			   String& commandType);
friend Table tableCommand (const String& command,
			   const PtrBlock<const Table*>& tempTables,
			   Vector<String>& columnNames);
friend Table tableCommand (const String& command,
			   const PtrBlock<const Table*>& tempTables,
			   Vector<String>& columnNames,
			   String& commandType);
// </group>

public:
    // Default constructor for List container class.
    TableParse ();

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




// <summary>
// Helper class for values in TableParse 
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableParse
// </prerequisite>

// <etymology>
// TableParseVal holds a value found when analyzing a table command.
// </etymology>

// <synopsis> 
// A table command is lexically analyzed via flex.
// An object of this class is used to hold a value (like a name
// or a literal) for later use in the parser code.
// </synopsis> 


class TableParseVal
{
public:
    Int      type;          //# i=Int, f=Double, c=DComplex, s=String t=Table
    String   str;           //# string literal; table name; field name
    Int      ival;          //# integer literal
    Double   dval[2];       //# Double/DComplex literal
    Table    tab;           //# Table (from query in e.g. FROM clause)

    // Make a new TableParseVal object (for the flex actions).
    static TableParseVal* makeValue();
};




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
    // Construct from a column name and expression.
    TableParseUpdate (const String& columnName, const TableExprNode&);

    // Construct from a column name, subscripts, and expression.
    TableParseUpdate (const String& columnName,
		      const TableExprNodeSet& indices,
		      const TableExprNode&);

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
    const TableExprNode& node() const;

private:
    String              columnName_p;
    TableExprNodeIndex* indexPtr_p;
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
// as needed and storing them in a List.
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

    // Construct for the given command type.
    TableParseSelect (Int commandType);

    // Destructor.
    ~TableParseSelect();

    // Execute the select command (select/sort/projection/giving).
    // The setInGiving flag tells if a set in the GIVING part is allowed.
    // The mustSelect flag tells if a SELECT command must do something.
    // Usually that is required, but not for a SELECT in an INSERT command.
    // The commandType (select or update) is filled in.
    // Optionally the maximum nr of rows to be selected can be given.
    // It will be used as the default value for the LIMIT clause.
    // 0 = no maximum.
    void execute (Bool setInGiving, String& commandType,
		  Bool mustSelect=True, uInt maxRow=0);

    // Execute a query in a from clause and create an appropriate value
    // for the result.
    TableParseVal* doFromQuery();

    // Execute a subquery and create an appropriate node for the result.
    TableExprNode doSubQuery();

    // Test if a subquery has sufficient elements.
    // It uses default LIMIT=1, but that can be overidden in the subquery.
    // The flag tells if NOT EXISTS or EXISTS was given.
    TableExprNode doExists (Bool noexists);

    // Show the expression tree.
    void show (ostream& os) const;

    // Set the distinct flag for column projection.
    void setDistinctCol();

    // Keep the selection expression.
    // It takes over the pointer (and clears the input pointer).
    void handleSelect (TableExprNode*& node);

    // Keep the update expressions.
    // It takes over the pointer (and clears the input pointer).
    void handleUpdate (PtrBlock<TableParseUpdate*>*& upd);

    // Keep the insert expressions or table selection.
    // It takes over the pointer (and clears the input pointer).
    // <group>
    void handleInsert (PtrBlock<TableParseUpdate*>*& ins);
    void handleInsert (TableParseSelect*& sel);
    // </group>

    // Keep the sort expressions.
    // It takes over the pointer (and clears the input pointer).
    void handleSort (PtrBlock<TableParseSort*>*& sortList, Bool noDuplicates,
		     Sort::Order defaultSortOrder);

    // Evaluate and keep the limit value.
    void handleLimit (const TableExprNode& expr);

    // Evaluate and keep the offset value.
    void handleOffset (const TableExprNode& expr);

    // Add a table name to the linked list.
    void addTable (const TableParseVal* name, const String& shorthand);

    // Find the keyword or column name and create a TableExprNode from it.
    TableExprNode handleKeyCol (const String& name);

    // Handle a slice operator.
    static TableExprNode handleSlice (const TableExprNode& array,
				      const TableExprNodeSet& indices);

    // Handle a function.
    TableExprNode handleFunc (const String& name,
			      const TableExprNodeSet& arguments);

    // Make a function object node for the given function name and arguments.
    // The ignoreFuncs vector contains invalid function codes.
    static TableExprNode makeFuncNode (const String& name,
				       const TableExprNodeSet& arguments,
				       const Vector<int>& ignoreFuncs,
				       const Table& table);

    // Find the function code belonging to a function name.
    // Functions to be ignored can be given (as function type values).
    static TableExprFuncNode::FunctionType findFunc
                                   (const String& name,
				    uInt narguments,
				    const Vector<Int>& ignoreFuncs);

    // Create a TableExprNode from a literal.
    static TableExprNode handleLiteral (TableParseVal*);

    // Add a column to the list of selected column names.
    //# String has to be non-const due to use of String::before/after.
    void handleSelectColumn (const String& name);

    // Handle the name given in a GIVING clause.
    void handleGiving (const String& name);

    // Handle the set given in a GIVING clause.
    void handleGiving (const TableExprNodeSet&);

    // Get the projected column names.
    const Block<String>& getColumnNames() const;

    // Get the resulting table.
    const Table& getTable() const;

    // Create a new TableParseSelect-object for the given command type
    // and put it on the stack (block)
    static void newSelect (Int commandType);

    // Get and remove the last element from the "select stack".
    // Note that this does not delete the object pointed to. It only
    // clears the pointer and decrements the stack size.
    static TableParseSelect* popSelect();

    // Get current TableParseSelect object.
    static TableParseSelect* currentSelect();

    // Clear the select stack.
    // Normally this won't do anything, but in case of exceptions
    // something may be left.
    static void clearSelect();


private:
    // Do the update step.
    void doUpdate (Table& table);

    // Do the insert step and return a selection containing the new rows.
    Table doInsert (Table& table);

    // Do the delete step.
    void doDelete (Table& table, const Table& sel);

    // Do the sort step.
    Table doSort (const Table& table);

    // Do the limit/offset step.
    Table doLimOff (const Table& table);

    // Do the 'select distinct' step.
    Table doDistinct (const Table& table);

    // Get the order for this key. Use the default order_p if not
    // explicitly given with the key.
    Sort::Order getOrder (const TableParseSort& key) const;

    // Make a set from the results of the subquery.
    TableExprNode makeSubSet() const;

    // Evaluate a double scalar expression.
    Double evalDSExpr (const TableExprNode& expr) const;

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

    // Find the names of all stored columns in a table.
    Block<String> getStoredColumns (const Table& tab) const;

    // Try to find the keyword representing a table in one of the tables
    // in any select block (from inner to outer).
    // If not found, an exception is thrown.
    static Table tableKey (const String& shorthand, const String& columnName,
			   const Vector<String>& fieldNames);

    // Try to find the keyword representing a table in the given table.
    // If the columnName is empty, the keyword is a table keyword.
    // If not found, a null Table object is returned.
    static Table findTableKey (const Table& table, const String& columnName,
			       const Vector<String>& keyNames);

    // Block of TableParseSelect objects (acts like a stack).
    static PtrBlock<TableParseSelect*> blockSelect_p;

    // Pointer to current TableParseSelect object (in PtrBlock above).
    // (Acts like pointer to top of stack).
    static uInt currentSelect_p;

    //# Command type (1=select, 2=update, 3=insert, 4=delete).
    Int commandType_p;
    //# Pointer to a linked list of TableParse objects.
    //# This is needed for the functions above, otherwise they have no
    //# way to communicate.
    List<TableParse>*     parseList_p;
    ListIter<TableParse>* parseIter_p;
    //# Block of selected column names.
    Block<String> columnNames_p;
    //# Distinct values in output?
    Bool distinct_p;
    //# Name of the resulting table (from GIVING part).
    String resultName_p;
    //# Resulting set (from GIVING part).
    TableExprNodeSet* resultSet_p;
    //# The WHERE expression tree.
    TableExprNode* node_p;
    //# The possible limit (= max nr of selected rows) (0 means no limit).
    uInt limit_p;
    //# The possible offset (= nr of selected rows to skip).
    uInt offset_p;
    //# The update or insert expression list.
    PtrBlock<TableParseUpdate*>* update_p;
    //# The table selection to be inserted.
    TableParseSelect* insSel_p;
    //# The sort list.
    PtrBlock<TableParseSort*>* sort_p;
    //# The noDuplicates sort switch.
    Bool  noDupl_p;
    //# The default sort order.
    Sort::Order order_p;
    //# The resulting table.
    Table table_p;
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

inline void TableParseSelect::handleSort (PtrBlock<TableParseSort*>*& sort,
					  Bool noDuplicates,
					  Sort::Order order)
{
    noDupl_p = noDuplicates;
    order_p  = order;
    sort_p   = sort;
    sort     = 0;
}

inline void TableParseSelect::setDistinctCol()
{
    distinct_p = True;
}

inline Sort::Order TableParseSelect::getOrder (const TableParseSort& key) const
    { return (key.orderGiven()  ?  key.order() : order_p); }

inline TableParseSelect* TableParseSelect::currentSelect()
    { return blockSelect_p[currentSelect_p-1]; }



#endif
