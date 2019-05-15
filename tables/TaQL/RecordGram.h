//# RecordGram.h: Grammar for record command lines
//# Copyright (C) 2000
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

#ifndef TABLES_RECORDGRAM_H
#define TABLES_RECORDGRAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/TaQL/TableGram.h>
#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/OS/Mutex.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableExprNode;
class TableExprNodeSet;
class TableExprNodeSetElem;
class Table;

// <summary>
// Global functions for flex/bison scanner/parser for RecordGram
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//  <li> RecordGram.l and .y  (flex and bison grammar)
// </prerequisite>

// <synopsis> 
// Global functions are needed to define the input of the flex scanner
// and to start the bison parser.
// The input is taken from a string.
// </synopsis> 

// <motivation>
// It is necessary to be able to give a record select command in ASCII.
// This can be used in a CLI or in the record browser to get a subset
// of a record or to sort a record.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>

// <group name=RecordGramFunctions>

// Declare the bison parser (is implemented by bison command).
int recordGramParseCommand (const String& command);

// The yyerror function for the parser.
// It throws an exception with the current token.
void RecordGramerror (const char*);

// Give the current position in the string.
// This can be used when parse errors occur.
Int& recordGramPosition();

// Declare the input routine for flex/bison.
int recordGramInput (char* buf, int max_size);

// A function to remove escaped characters.
inline String recordGramRemoveEscapes (const String& in)
    { return tableGramRemoveEscapes (in); }

// A function to remove quotes from a quoted string.
inline String recordGramRemoveQuotes (const String& in)
    { return tableGramRemoveQuotes (in); }

// </group>



// <summary>
// Helper class for values in RecordGram
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis> 
// A record selection command is lexically analyzed via flex.
// An object of this class is used to hold a value (like a name
// or a literal) for later use in the parser code.
// </synopsis> 

class RecordGramVal
{
public:
    Int      type;          //# i=Int, f=Double, c=DComplex, s=String r=Regex
    String   str;           //# string literal; table name; field name; unit
    Bool     bval;          //# bool literal
    Int64    ival;          //# integer literal
    Double   dval[2];       //# Double/DComplex literal
};




// <summary>
// Select-class for flex/bison scanner/parser for RecordGram
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//  <li> RecordGram.l and .y  (flex and bison grammar)
// </prerequisite>

// <synopsis> 
// This class is needed for the the actions in the flex scanner
// and bison parser.
// This stores the information by constructing RecordGram objects
// as needed and storing them in a List.
//
// An expression can be given as a string and parsed by the <src>parse</src>
// function.
// The grammar used is as much as possible the same as that for the
// WHERE clause in TaQL (see Note 199).
// It is possible to set the TaQL style to use by setting
// <src>theirTaQLStyle</src> before calling the parse functions.
// A better way is to define the style with the 'USING STYLE' part of
// the command (similar to TaQL).
// </synopsis> 

// <motivation>
// It is necessary to be able to give a record select command in ASCII.
// It is used by the ACSIS people.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class RecordGram
{
public:
    // Define the types of tokens in the grammar.
    enum Token {Node, Val, Elem, Set};

    // Convert an expression string to an expression tree.
    // The expression will operate on a series of Record objects.
    // The given record is needed to know the type of the fields used in
    // the expression.
    //# The record will be put into the static variable to be used by
    //# the other functions.
    static TableExprNode parse (const RecordInterface& record,
                                const String& expression);

    // Convert an expression string to an expression tree.
    // The expression will operate on the given table.
    //# The record will be put into the static variable to be used by
    //# the other functions.
    static TableExprNode parse (const Table& table,
                                const String& expression);

    // Evaluate an expression to the given type.
    // The expression can contain variables; their names and values must be
    // defined in the record.
    // For double values it is possible to specify the desired unit.
    // If the expression is a scalar value, the expr2Array functions will
    // return an array with length 1.
    // <group>
    static Bool     expr2Bool    (const String& expr, const Record& vars=Record());
    static Int64    expr2Int     (const String& expr, const Record& vars=Record());
    static double   expr2Double  (const String& expr, const Record& vars=Record(),
                                  const String& unit=String());
    static DComplex expr2Complex (const String& expr, const Record& vars=Record());
    static String   expr2String  (const String& expr, const Record& vars=Record());
    static MVTime   expr2Date    (const String& expr, const Record& vars=Record());
    static Array<Bool>     expr2ArrayBool    (const String& expr,
                                              const Record& vars=Record());
    static Array<Int64>    expr2ArrayInt     (const String& expr,
                                              const Record& vars=Record());
    static Array<double>   expr2ArrayDouble  (const String& expr,
                                              const Record& vars=Record(),
                                              const String& unit=String());
    static Array<DComplex> expr2ArrayComplex (const String& expr,
                                              const Record& vars=Record());
    static Array<String>   expr2ArrayString  (const String& expr,
                                              const Record& vars=Record());
    static Array<MVTime>   expr2ArrayDate    (const String& expr,
                                              const Record& vars=Record());
    // </group>

    // Create a TableExprNode from a literal.
    static TableExprNode handleLiteral (RecordGramVal*);

    // Find the field name and create a TableExprNode from it.
    // To be called only by the yy parser (under theirMutex).
    static TableExprNode handleField (const String& name);

    // Handle a function.
    // To be called only by the yy parser (under theirMutex).
    static TableExprNode handleFunc (const String& name,
                                     const TableExprNodeSet& arguments);

    // Handle a regex.
    static TableExprNode handleRegex (const TableExprNode& left,
                                      const String& regex);

    // Set the final node pointer.
    static void setNodePtr (TableExprNode* nodePtr)
        { theirNodePtr = nodePtr; }

    // Define the global TaQLStyle to use.
    // By default it is glish style.
    static TaQLStyle theirTaQLStyle;

    // Add a token to the list of tokens to be deleted
    // for the possible tokens in the RecordGram.yy union.
    // The addToken() functions are to be called only by the yy parser (under theirMutex).
    static void addToken (TableExprNode* ptr);
    static void addToken (RecordGramVal* ptr);
    static void addToken (TableExprNodeSet* ptr);
    static void addToken (TableExprNodeSetElem* ptr);

    // Delete a token and remove from the list.
    // The deleteToken() functions are to be called only by the yy parser (under theirMutex).
    static void deleteToken (TableExprNode* ptr);
    static void deleteToken (RecordGramVal* ptr);
    static void deleteToken (TableExprNodeSet* ptr);
    static void deleteToken (TableExprNodeSetElem* ptr);

private:
    // Delete all tokens not deleted yet.
    static void deleteTokenStorage();

    // Do the conversion of an expression string to an expression tree.
    static TableExprNode doParse (const String& expression);

    // Add a token to the list of tokens to be deleted.
    static void addToken (void* ptr, Token type)
      { theirTokens[ptr] = type; }
    // Remove a token from the list of tokens to be deleted.
    static void removeToken (void* ptr)
      { theirTokens.erase (ptr); }

    static std::map<void*, Token> theirTokens;
    static const RecordInterface* theirRecPtr;
    static const Table*           theirTabPtr;
    static TableExprNode*         theirNodePtr;
    static Mutex                  theirMutex;
};



} //# NAMESPACE CASACORE - END

#endif
