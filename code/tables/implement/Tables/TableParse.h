//# TableParse.h: Classes to hold results from table grammar parser
//# Copyright (C) 1994,1995,1997,1998
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
#include <aips/Utilities/String.h>
#include <aips/Utilities/Sort.h>
#include <aips/Containers/List.h>
#include <aips/Containers/Block.h>
#include <aips/Tables/ExprNode.h>

//# Forward Declarations
class TableExprNodeSet;
class AipsIO;
template<class T> class Vector;


// <summary>
// Class to hold values from table grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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
friend Table tableCommand (const String& command);

// Parse and execute the given command.
// It is the same as above, but a temporary table can be used in the command
// using the $nnn syntax.
friend Table tableCommand (const String& command, const Table& tempTable);

// Parse and execute the given command.
// It is the same as above, but multiple temporary tables can be used
// in the command using the $nnn syntax.
friend Table tableCommand (const String& command,
			   const PtrBlock<const Table*>& tempTables);

// Parse and execute the command.
// It will open (and close) all tables needed.
// It returns the resulting table and the selected column names.
friend Table tableCommand (const String& command,
			   Vector<String>& columnNames);

// Parse and execute the command.
// It is the same as above, but multiple temporary tables can be used
// in the command using the $nnn syntax.
friend Table tableCommand (const String& command,
			   const PtrBlock<const Table*>& tempTables,
			   Vector<String>& columnNames);

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
// Helper class for class TableParse
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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
    Int      type;              //# i=Int, f=Double, c=DComplex, s=String
    String   str;               //# string literal; table name; field name
    Int      ival;              //# integer literal
    Double   dval[2];           //# Double/DComplex literal

    // Make a new TableParseVal object (for the flex actions).
    static TableParseVal* makeValue();
};




// <summary>
// Helper class for class TableParse
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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
    // Construct from a given expression and for the given order.
    TableParseSort (const TableExprNode&, Sort::Order);

    ~TableParseSort();

    // Get the expression node.
    const TableExprNode& node() const;

    // Get the sort order.
    Sort::Order order() const;

private:
    TableExprNode node_p;
    Sort::Order   order_p;
};




// <summary>
// Select-class for flex/bison scanner/parser for TableParse
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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

    // Default constructor.
    TableParseSelect();

    // Destructor.
    ~TableParseSelect();

    // Execute the select command (select/sort/projection/giving).
    void execute();

    // Execute a subquery and create an appropriate node for the result.
    TableExprNode doSubQuery();

    // Show the expression tree.
    void show (ostream& os) const;

    // Keep the selection expression.
    // It takes over the pointer (and clear the input pointer).
    void handleSelect (TableExprNode*& node);

    // Sort the resulting table.
    // It takes over the pointer (and clears the input pointer).
    void handleSort (PtrBlock<TableParseSort*>*& sortList, Bool noDuplicates);

    // Add a table name to the linked list.
    void addTable (const TableParseVal* name, const String& shorthand);

    // Find the keyword or column name and create a TableExprNode from it.
    TableExprNode handleKeyCol (const String& name);

    // Handle a slice operator.
    TableExprNode handleSlice (const TableExprNode& array,
			       const TableExprNodeSet& indices);

    // Handle a function.
    TableExprNode handleFunc (const String& name,
			      const TableExprNodeSet& arguments);

    // Create a TableExprNode from a literal.
    TableExprNode handleLiteral (TableParseVal*);

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

    // Create a new TableParseSelect-object and put it on the stack (block)
    static void newSelect();

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
    // Do the sort step.
    Table doSort (const Table& table);

    // Make a set from the results of the subquery.
    TableExprNode makeSubSet() const;

    // Split a name into its parts (shorthand, column and field names).
    // True is returned when the name contained a keyword part.
    // In that case fieldNames contains the keyword name and possible subfields.
    // The column name is filled in when it is a column keyword.
    // If the name represent a column, fieldNames contains the subfields
    // of the column (for the case where the column contains records).
    // An exception is thrown if the name was invalid.
    Bool splitName (String& shorthand, String& columnName,
		    Vector<String>& fieldNames, const String& name) const;

    // Find a table for the given shorthand.
    // If no shorthand is given, the first table is returned (if there).
    // If not found, a null Table object is returned.
    Table findTable (const String& shorthand) const;

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

    //# Pointer to a linked list of TableParse objects.
    //# This is needed for the functions above, otherwise they have no
    //# way to communicate.
    List<TableParse>*     parseList_p;
    ListIter<TableParse>* parseIter_p;
    //# Block of selected column names.
    Block<String> columnNames_p;
    //# Name of the resulting table (from GIVING part).
    String resultName_p;
    //# Resulting set (from GIVING part).
    TableExprNodeSet* resultSet_p;
    //# The WHERE expression tree.
    TableExprNode* node_p;
    //# The sort list.
    PtrBlock<TableParseSort*>* sort_p;
    //# The noDuplicates switch.
    Bool  noDupl_p;
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


inline const TableExprNode& TableParseSort::node() const
    { return node_p; }

inline Sort::Order TableParseSort::order() const
    { return order_p; }


inline const Block<String>& TableParseSelect::getColumnNames() const
    { return columnNames_p; }

inline const Table& TableParseSelect::getTable() const
    { return table_p; }

inline void TableParseSelect::handleSelect (TableExprNode*& node)
{
    node_p = node;
    node = 0;
}

inline void TableParseSelect::handleSort (PtrBlock<TableParseSort*>*& sort,
					  Bool noDuplicates)
{
    noDupl_p = noDuplicates;
    sort_p = sort;
    sort = 0;
}

inline TableParseSelect* TableParseSelect::currentSelect()
    { return blockSelect_p[currentSelect_p-1]; }



#endif
