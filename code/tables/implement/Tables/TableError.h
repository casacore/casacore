//# TableError.h: Table error classes
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#if !defined (AIPS_TABLEERROR_H)
#define AIPS_TABLEERROR_H

//# Includes
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>


//# This header file defines the error classes belonging to the table
//# descriptor class and its associated classes.


// <summary>
// Base error class for storage manager
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// This is the generic StMan exception; catching this one means catching
// all Table* exceptions.
// Note that you have to catch AipsError to catch all possible exceptions.
// </synopsis> 

class TableError : public AipsError {
public:
    // The default constructor generates the message "Table error".
    TableError ();
    // Construct with given message.
    TableError (const String& message);
    ~TableError ();
};


// <summary>
// Internal table error
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Internal table error (should never be thrown).
// If this is thrown, something is terribly wrong.
// </synopsis> 

class TableInternalError : public TableError {
public:
    // Add given message to string "Internal Table error: ".
    TableInternalError (const String& message);
    ~TableInternalError ();
};


// <summary>
// Table error; table (description) already exists
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Table (description) with this name already exists.
// </synopsis> 

class TableDuplFile : public TableError {
public:
    // This constructor generates a message telling that the a table
    // or description with the given name already exists.
    TableDuplFile (const String& name);
    // This constructor generates a message telling that the a table
    // or description with the given name already exists.
    // The given message is appended to it.
    TableDuplFile (const String& name, const String& message);
    ~TableDuplFile ();
};


// <summary>
// Table error; table (description) not found
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Table (description) with this name could not be found.
// </synopsis> 

class TableNoFile : public TableError {
public:
    // This constructor generates a message telling that the a table
    // or description with the given name does not exist.
    TableNoFile (const String& name);
    ~TableNoFile ();
};


// <summary>
// Table error; no name given to table description
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// No name given for the table description.
// Only scratch descriptions can have no name (i.e. a blank name).
// </synopsis> 

class TableDescNoName : public TableError {
public:
    // The default constructor generates the message.
    TableDescNoName ();
    ~TableDescNoName ();
};


// <summary>
// Table error; invalid table (description) option
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Invalid Table(Desc) option given for the table (description).
// </synopsis> 

class TableInvOpt : public TableError {
public:
    // This constructor generates a message that an invalid option
    // has been given. The class name is either Table or TableDesc.
    // The given message will be appended to the total message.
    TableInvOpt (const String& className, const String& message);
    ~TableInvOpt ();
};


// <summary>
// Table error; table type mismatch
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// The given table type (i.e. name of the table description) does
// not match the type as stored in the table file.
// </synopsis> 

class TableInvType : public TableError {
public:
    // This constructor generates a message that the in table type
    // mismatches the table type in the file.
    TableInvType (const String& typeIn, const String& typeFile);
    ~TableInvType ();
};


// <summary>
// Table error; invalid column description
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// The description of a column is invalid.
// The given default manager is unknown
// (i.e. not registered in DataManReg.cc).
// </synopsis> 

class TableInvColumnDesc : public TableError {
public:
    // This constructor generates a message that the column
    // with the given name has an invalid description.
    TableInvColumnDesc (const String& columnName, const String& message);
    ~TableInvColumnDesc ();
};


// <summary>
// Table error; invalid hypercolumn description
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// The description of a hypercolumn is invalid.
// The referenced columns are unknown or invalid.
// The message explains the reason.
// </synopsis> 

class TableInvHyperDesc : public TableError {
public:
    // This constructor generates a message that the hypercolumn
    // with the given name has an invalid description.
    TableInvHyperDesc (const String& hypercolumnName, const String& message);
    ~TableInvHyperDesc ();
};


// <summary>
// Table error; unknown column description
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// To be able to reconstruct the correct column description object
// from a stored table description, each column description type
// must register itself (see ColumnDesc.h and ColumnReg.cc).
// </synopsis> 

class TableUnknownDesc : public TableError {
public:
    // This constructor generates a message that the class with the
    // given name is unknown (not registered).
    TableUnknownDesc (const String& name);
    ~TableUnknownDesc ();
};


// <summary>
// Table error; invalid data type
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Checking of the data type of a column is done at runtime.
// This error results from non-matching data types when constructing
// a ScalarColumn or ArrayColumn or from invalid data type promotions
// when doing a get or put.
// </synopsis> 

class TableInvDT : public TableError {
public:
    // The default constructor generates a generic "invalid data type" message.
    TableInvDT ();
    // Put the name of the offending column in the "invalid data type" message.
    TableInvDT (const String& columName);
    ~TableInvDT ();
};


// <summary>
// Table error; invalid operation
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Invalid operation on a table.
// A request was done that could not be handled by the table system
// (e.g. sorting on a column containing arrays).
// The message tells what is wrong.
// </synopsis> 

// Invalid operation on a table.
class TableInvOper : public TableError {
public:
    // The default constructor generates a generic "invalid operation" message.
    TableInvOper ();
    // Add given message to string "Invalid Table operation: ".
    TableInvOper (const String& message);
    ~TableInvOper ();
};


// <summary>
// Table error; non-conformant array
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// When putting a direct array, the shape of the array must conform
// the shape as defined for the table array.
// When getting an array, the receiving array must be zero-length
// or it must conform the shape of the table array.
// </synopsis> 

class TableArrayConformanceError : public TableError {
public:
    // This constructor appends ": Table array conformance error"
    // to the given message.
    TableArrayConformanceError (const String& message);
    ~TableArrayConformanceError ();
};


// <summary>
// Table error; table length conformance error
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// When putting a column, the length of the vector must match the
// length of the table (i.e. its number of rows).
// When getting a column, the length of the vector must be zero or
// it must match the length of the table.
// </synopsis> 

class TableConformanceError : public TableError {
public:
    // This constructor appends ": Table conformance error (#rows mismatch)"
    // to the given message.
    TableConformanceError (const String& message);
    ~TableConformanceError ();
};


// <summary>
// Table error; invalid sort
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Invalid sort operation on a table.
// A sort can only be done on a scalar column.
// </synopsis> 

class TableInvSort : public TableError {
public:
    // The default constructor generates a generic "invalid sort" message.
    TableInvSort ();
    // This constructor appends the given message to the "invalid sort"
    // message.
    TableInvSort (const String& message);
    ~TableInvSort ();
};


// <summary>
// Table error; invalid logical operation
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Invalid logical table operation.
// When combining tables using a union, difference, etc., the
// tables involved have to stem from the same root. I.e. they
// should all refer to the same underlying table.
// </synopsis> 

class TableInvLogic : public TableError {
public:
    // The default constructor generates the message.
    TableInvLogic ();
    ~TableInvLogic ();
};


// <summary>
// Table error; invalid select expression
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Invalid table select expression.
// A column is not a scalar or belongs to another table than
// the table on which the selection will be done.
// </synopsis> 

class TableInvExpr : public TableError {
public:
    TableInvExpr (const String& message);
    // This constructor generates a message containing the name of
    // the offending column. It appends the given message.
    TableInvExpr (const String& columnName, const String& message);
    ~TableInvExpr ();
};


// <summary>
// Table error; non-conformant table vectors
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// Table vectors are not conformant (have different lengths)
// </synopsis> 

class TableVectorNonConform : public TableError {
public:
    // The default constructor generates the message.
    TableVectorNonConform ();
    ~TableVectorNonConform ();
};


// <summary>
// Table error; invalid table command
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// The parser in TableGram/TableParse found an error in
// the given table command.
// </synopsis> 

class TableParseError : public TableError {
public:
    // This constructor generates a message containing the table command.
    TableParseError (const String& commandString);
    ~TableParseError ();
};


#endif
