//# TableGram.cc: Grammar for table command lines
//# Copyright (C) 1993,1994,1995,1997,1999,2001,2003
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

// TableGram; grammar for table command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on tables.


#include <casacore/tables/TaQL/TableGram.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/TaQL/TaQLNodeDer.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <stack>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
//# Bison defines WHERE, which is also defined in LogOrigin.h (which
//# is included in auto-template mode).
//# So undefine WHERE first.
#undef WHERE
#include <casacore/casa/stdlib.h>

//# Let clang and gcc ignore some warnings in bison/flex code.
//# Undef YY_NULL because flex can redefine it slightly differently (giving a warning).
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpragmas"
#pragma GCC diagnostic ignored "-Wdeprecated-register"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include "TableGram.ycc"                  // bison output
#ifdef YY_NULL
# undef YY_NULL
#endif
#include "TableGram.lcc"                  // flex output
#pragma GCC diagnostic pop


// Define the yywrap function for flex.
int TableGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*  strpTableGram = 0;
static Int          posTableGram = 0;


// Define a class to delete the yy_buffer in case of an exception.
class TableGramState {
public:
  TableGramState (YY_BUFFER_STATE state)
    : itsState (state)
  {}
  ~TableGramState()
    { clear(); }
  TableGramState (const TableGramState&) = delete;
  TableGramState& operator= (const TableGramState&) = delete;
  void clear()
    { if (itsState) {
        TableGram_delete_buffer(itsState);
        itsState=0;
      }
    }
  YY_BUFFER_STATE state() const
    { return itsState; }
private:
  YY_BUFFER_STATE itsState;      //# this is a pointer to yy_buffer_state
};

  
//# Parse the command.
int tableGramParseCommand (const String& command)
{
    // Save current state for re-entrancy.
    int sav_yy_start = yy_start;
    const char* savStrpTableGram = strpTableGram;
    Int savPosTableGram= posTableGram;
    YY_BUFFER_STATE sav_state = YY_CURRENT_BUFFER;
    // Create a new state buffer for new expression.
    TableGramState next (TableGram_create_buffer (TableGramin, YY_BUF_SIZE));
    TableGram_switch_to_buffer (next.state());
    yy_start = 1;
    strpTableGram = command.chars();     // get pointer to command string
    posTableGram  = 0;                   // initialize string position
    int sts = TableGramparse();          // parse command string
    // The current state has to be deleted before switching back to previous.
    next.clear();
    // Restore previous state.
    yy_start = sav_yy_start;
    strpTableGram = savStrpTableGram;
    posTableGram= savPosTableGram;
    TableGram_switch_to_buffer (sav_state);
    return sts;
}

//# Give the string position.
Int& tableGramPosition()
{
    return posTableGram;
}

//# Get the next input characters for flex.
int tableGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpTableGram != 0) {
        if (nr >= max_size) {
            break;                         // get max. max_size char.
        }
        buf[nr++] = *strpTableGram++;
    }
    return nr;
}

void TableGramerror (const char*)
{
    throw TableGramError (tableGramPosition(), String(TableGramtext));
}

String tableGramRemoveEscapes (const String& in)
{
    String out;
    int leng = in.length();
    for (int i=0; i<leng; i++) {
        if (in[i] == '\\') {
            i++;
        }
        out += in[i];
    }
    return out;
}

String tableGramRemoveEscapesQuotes (const String& in)
{
    String out;
    char quote = 0;
    int leng = in.length();
    for (int i=0; i<leng; i++) {
      if (quote) {
        if (in[i] == quote) {
          quote = 0;
        } else {
          out += in[i];
        }
      } else if (in[i] == '"'  ||  in[i] == '\'') {
        quote = in[i];
      } else {
        if (in[i] == '\\') {
          i++;
        }
        out += in[i];
      }
    }
    return out;
}

String tableGramRemoveQuotes (const String& in)
{
    //# A string is formed as "..."'...''...' etc.
    //# All ... parts will be extracted and concatenated into an output string.
    String out;
    String str = in;
    int leng = str.length();
    int pos = 0;
    while (pos < leng) {
        //# Find next occurrence of leading ' or ""
        int inx = str.index (str[pos], pos+1);
        if (inx < 0) {
            throw TableInvExpr ("ill-formed quoted string: " + str);
        }
        out += str.at (pos+1, inx-pos-1);             // add substring
        pos = inx+1;
    }
    return out;
}

Double tableGramParseTime (const String& in)
{
    Quantity res;
    //# Skip a possible leading / which acts as an escape character.
    String val(in);
    if (val.length() > 0  &&  val[0] == '/') {
        val = val.after(0);
    }
    if (! MVAngle::read (res, val)) {
        throw TableInvExpr ("invalid time/pos string " + val);
    }
    return MVAngle(res).radian();
}

MVTime tableGramParseDateTime (const String& in)
{
    MUString str (in);
    Quantity res;
    if (! MVTime::read (res, str)) {
        throw TableInvExpr ("invalid date string " + in);
    }
    return MVTime(res);
}

} //# NAMESPACE CASACORE - END
