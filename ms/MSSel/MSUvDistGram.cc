//# MSUvDistGram.cc: Grammar for UV distribution expressions
//# Copyright (C) 1998,1999,2001,2003
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

// MSUvDistGram; grammar for UV distribution command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSUvDistGram.h>
#include <casacore/ms/MSSel/MSUvDistParse.h> // routines used by bison actions
#include <casacore/ms/MSSel/MSSelectionError.h>

#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>
#include <limits>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "MSUvDistGram.ycc"                  // bison output
#include "MSUvDistGram.lcc"                  // flex output

// Define the yywrap function for flex.
int MSUvDistGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*           strpMSUvDistGram = 0;
static Int                   posMSUvDistGram = 0;


//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int msUvDistGramParseCommand (const MeasurementSet* ms, const String& command,
			      Matrix<Double>& selectedUVRange, Vector<Bool>& units) 
{
  try 
    {
      Int ret;
      MSUvDistGramrestart (MSUvDistGramin);
      yy_start = 1;
      strpMSUvDistGram = command.chars();     // get pointer to command string
      posMSUvDistGram  = 0;                   // initialize string position
      MSUvDistParse parser(ms);               // setup measurement set
      MSUvDistParse::thisMSUParser = &parser; // The global pointer to the parser
      parser.reset();                         // parse command string
      ret=MSUvDistGramparse();                
      selectedUVRange=parser.selectedUV();
      units = parser.selectedUnits();
      return ret;
    }
    catch (MSSelectionUvDistError &x)
      {
	String newMesgs;
	newMesgs = constructMessage(msUvDistGramPosition(),command);
	x.addMessage(newMesgs);
	throw;
      }
}
int msUvDistGramParseCommand (const MeasurementSet* ms, const String& command) 
{
  try 
    {
      Int ret;
      MSUvDistGramrestart (MSUvDistGramin);
      yy_start = 1;
      strpMSUvDistGram = command.chars();     // get pointer to command string
      posMSUvDistGram  = 0;                   // initialize string position
      MSUvDistParse parser(ms);               // setup measurement set
      MSUvDistParse::thisMSUParser = &parser; // The global pointer to the parser
      ret=MSUvDistGramparse();                // parse command string
      return ret;
    }
    catch (MSSelectionUvDistError &x)
      {
	String newMesgs;
	newMesgs = constructMessage(msUvDistGramPosition(),command);
	x.addMessage(newMesgs);
	throw;
      }
}

//# Give the table expression node
const TableExprNode* msUvDistGramParseNode()
{
    return MSUvDistParse::node();
}
void msUvDistGramParseDeleteNode()
{
    return MSUvDistParse::cleanup();
}

//# Give the string position.
Int& msUvDistGramPosition()
{
    return posMSUvDistGram;
}

//# Get the next input characters for flex.
int msUvDistGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpMSUvDistGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpMSUvDistGram++;
    }
    return nr;
}

void MSUvDistGramerror (const char*)
{
  throw (MSSelectionUvDistParseError("UV Distribution Expression: Parse error at or near '"+
				     String(MSUvDistGramtext) + "'"));
}

// String msUvDistGramRemoveEscapes (const String& in)
// {
//     String out;
//     int leng = in.length();
//     for (int i=0; i<leng; i++) {
// 	if (in[i] == '\\') {
// 	    i++;
// 	}
// 	out += in[i];
//     }
//     return out;
// }

// String msUvDistGramRemoveQuotes (const String& in)
// {
//     //# A string is formed as "..."'...''...' etc.
//     //# All ... parts will be extracted and concatenated into an output string.
//     String out;
//     String str = in;
//     int leng = str.length();
//     int pos = 0;
//     while (pos < leng) {
// 	//# Find next occurrence of leading ' or ""
// 	int inx = str.index (str[pos], pos+1);
// 	if (inx < 0) {
// 	    throw (AipsError ("MSUvDistParse - Ill-formed quoted string: " +
// 			      str));
// 	}
// 	out += str.at (pos+1, inx-pos-1);             // add substring
// 	pos = inx+1;
//     }
//     return out;
// }

} //# NAMESPACE CASACORE - END
