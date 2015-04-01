//# RecordGram.cc: Grammar for record command lines
//# Copyright (C) 2000,2001,2003,2005
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

// RecordGram; grammar for record command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on records.


#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/RecordGram.h>
#include <casacore/tables/TaQL/RecordExpr.h>
#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVAngle.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "RecordGram.ycc"                  // bison output
#include "RecordGram.lcc"                  // flex output


// Define the yywrap function for flex.
int RecordGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*  strpRecordGram = 0;
static Int          posRecordGram = 0;

//# Static pointer to the record when parsing the fields.
//# Static pointer to the node holding the final expression tree.
const RecordInterface* RecordGram::theirRecPtr = 0;
TableExprNode* RecordGram::theirNodePtr = 0;
const Table* RecordGram::theirTabPtr = 0;
TaQLStyle RecordGram::theirTaQLStyle;
Mutex RecordGram::theirMutex;

//# The list of nodes to delete (usually in case of exception).
std::map<void*, RecordGram::Token> RecordGram::theirTokens;
void RecordGram::addToken (TableExprNode* ptr)
  { addToken (ptr, RecordGram::Node); }
void RecordGram::addToken (RecordGramVal* ptr)
  { addToken (ptr, RecordGram::Val); }
void RecordGram::addToken (TableExprNodeSet* ptr)
  { addToken (ptr, RecordGram::Set); }
void RecordGram::addToken (TableExprNodeSetElem* ptr)
  { addToken (ptr, RecordGram::Elem); }
void RecordGram::deleteToken (TableExprNode* ptr)
  { delete ptr; removeToken (ptr); }
void RecordGram::deleteToken (RecordGramVal* ptr)
  { delete ptr; removeToken (ptr); }
void RecordGram::deleteToken (TableExprNodeSet* ptr)
  { delete ptr; removeToken (ptr); }
void RecordGram::deleteToken (TableExprNodeSetElem* ptr)
  { delete ptr; removeToken (ptr); }
void RecordGram::deleteTokenStorage()
{
  for (std::map<void*,RecordGram::Token>::const_iterator
         iter=theirTokens.begin(); iter!=theirTokens.end(); ++iter) {
    switch (iter->second) {
    case RecordGram::Node:
      delete static_cast<TableExprNode*>(iter->first);
      break;
    case RecordGram::Val:
      delete static_cast<RecordGramVal*>(iter->first);
      break;
    case RecordGram::Elem:
      delete static_cast<TableExprNodeSetElem*>(iter->first);
      break;
    case RecordGram::Set:
      delete static_cast<TableExprNodeSet*>(iter->first);
      break;
    }
  }
  theirTokens.clear();
}

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int recordGramParseCommand (const String& command)
{
    RecordGramrestart (RecordGramin);
    yy_start = 1;
    strpRecordGram = command.chars();     // get pointer to command string
    posRecordGram  = 0;                   // initialize string position
    return RecordGramparse();             // parse command string
}

//# Give the string position.
Int& recordGramPosition()
{
    return posRecordGram;
}

//# Get the next input characters for flex.
int recordGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpRecordGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpRecordGram++;
    }
    return nr;
}

void RecordGramerror (const char*)
{
    throw (TableInvExpr ("Parse error at or near '" +
			 String(RecordGramtext) + "'"));
}


TableExprNode RecordGram::parse (const RecordInterface& record,
				 const String& expression)
{
    ScopedMutexLock lock(theirMutex);
    theirRecPtr = &record;
    theirTabPtr = 0;
    return doParse (expression);
}

TableExprNode RecordGram::parse (const Table& table,
				 const String& expression)
{
    ScopedMutexLock lock(theirMutex);
    theirRecPtr = 0;
    theirTabPtr = &table;
    return doParse (expression);
}

TableExprNode RecordGram::doParse (const String& expression)
{
    theirTokens.clear();
    String message;
    String command = expression + '\n';
    Bool error = False;
    TableExprNode result;
    try {
	// Parse and execute the command.
	if (recordGramParseCommand(command) != 0) {
	    throw (TableParseError(expression));   // throw exception if error
	}
        // Make this copy before deleteTokenStorage is done,
        // otherwise it will be deleted.
        result = *theirNodePtr;
    } catch (const AipsError& x) {
	message = x.getMesg();
	error = True;
    }
    // Delete possibly non-deleted tokens (usually in case of exception).
    deleteTokenStorage();
    //# If an exception was thrown; throw it again with the message.
    if (error) {
	throw AipsError(message + '\n' + "Scanned so far: " +
	                 command.before(recordGramPosition()));
    }
    return result;
}

//# Convert a constant to a TableExprNode object.
//# The leading and trailing " is removed from a string.
TableExprNode RecordGram::handleLiteral (RecordGramVal* val)
{
    TableExprNode expr;
    switch (val->type) {
    case 'b':
	expr = TableExprNode (val->bval);
	break;
    case 'i':
	expr = TableExprNode (val->ival);
	break;
    case 'f':
	expr = TableExprNode (val->dval[0]);
	if (! val->str.empty()) {
	    expr = expr.useUnit (val->str);
	}
	break;
    case 'c':
	expr= TableExprNode (DComplex (val->dval[0], val->dval[1]));
	break;
    case 's':
	expr = TableExprNode (val->str);
	break;
    case 'd':
      {
	MUString str (val->str);
	Quantity res;
	if (! MVTime::read (res, str)) {
	    throw (TableInvExpr ("invalid date string " + val->str));
	}
	expr = TableExprNode (MVTime(res));
      }
	break;
    case 't':
      {
	Quantity res;
	//# Skip a possible leading / which acts as an escape character.
	if (val->str.length() > 0  &&  val->str[0] == '/') {
	    val->str = val->str.after(0);
	}
	if (! MVAngle::read (res, val->str)) {
	    throw (TableInvExpr ("invalid time/pos string " + val->str));
	}
	expr = TableExprNode (MVAngle(res).radian());
	expr = expr.useUnit ("rad");
      }
	break;
    default:
	throw (TableInvExpr ("RecordGram: unhandled literal type"));
    }
    return expr;
}

TableExprNode RecordGram::handleField (const String& name)
{
  if (theirTabPtr == 0) {
    return makeRecordExpr (*theirRecPtr, name);
  }
  return theirTabPtr->keyCol (name, Vector<String>());
}

TableExprNode RecordGram::handleFunc (const String& name,
				      const TableExprNodeSet& arguments)
{
  // The ROWNR function can only be used with tables.
  if (theirTabPtr == 0) {
    Vector<Int> ignoreFuncs (1, TableExprFuncNode::rownrFUNC);
    return TableParseSelect::makeFuncNode (name, arguments,
					   ignoreFuncs, Table(),
					   theirTaQLStyle);
  }
  return TableParseSelect::makeFuncNode (name, arguments,
					 Vector<Int>(), *theirTabPtr,
					 theirTaQLStyle);
}

TableExprNode RecordGram::handleRegex (const TableExprNode& left,
                                       const String& regex)
{
  Bool caseInsensitive = False;
  Bool negate          = False;
  Int sz = regex.size();
  if (sz > 0  &&  regex[sz-1] == 'i') {
    caseInsensitive = True;
    --sz;
  }
  AlwaysAssert (sz >= 4  &&  regex[sz-1] != ' ', AipsError);
  Int inx = 0;
  if (regex[0] == '!') {
    negate = True;
    ++inx;
  }
  AlwaysAssert (regex[inx] == '~', AipsError);
  while (regex[++inx] == ' ') {}
  AlwaysAssert (regex.size()-inx >= 3, AipsError);
  // Remove delimiters.
  String str = regex.substr(inx+2, sz-inx-3);
  if (regex[inx] == 'p') {
    str = Regex::fromPattern (str);
  } else if (regex[inx] == 'm') {
    str = ".*(" + str + ").*";
  }
  TableExprNode lnode(left);
  if (caseInsensitive) {
    str = Regex::makeCaseInsensitive (str);
  }
  TableExprNode rnode((Regex(str)));
  if (negate) {
    lnode = (lnode != rnode);
  } else {
    lnode = (lnode == rnode);
  }
  return lnode;
}


} //# NAMESPACE CASACORE - END
