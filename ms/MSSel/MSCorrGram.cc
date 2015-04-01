//# MSCorrGram.cc: Grammar for corr expressions
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

// MSCorrGram; grammar for corr command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSCorrGram.h>
#include <casacore/ms/MSSel/MSCorrParse.h> // routines used by bison actions
#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "MSCorrGram.ycc"                  // bison output
#include "MSCorrGram.lcc"                  // flex output

// Define the yywrap function for flex.
int MSCorrGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*           strpMSCorrGram = 0;
static Int                   posMSCorrGram = 0;


//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int msCorrGramParseCommand (const MeasurementSet* ms, const String& command) 
{
    MSCorrGramrestart (MSCorrGramin);
    yy_start = 1;
    strpMSCorrGram = command.chars();     // get pointer to command string
    posMSCorrGram  = 0;                   // initialize string position
    MSCorrParse parser(ms);               // setup measurement set
    return MSCorrGramparse();             // parse command string
}

//# Give the table expression node
const TableExprNode* msCorrGramParseNode()
{
    return MSCorrParse::node();
}
void msCorrGramParseDeleteNode()
{
    return MSCorrParse::cleanup();
}

//# Give the string position.
Int& msCorrGramPosition()
{
    return posMSCorrGram;
}

//# Get the next input characters for flex.
int msCorrGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpMSCorrGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpMSCorrGram++;
    }
    return nr;
}

void MSCorrGramerror (const char*)
{
    throw (AipsError ("Corr Expression: Parse error at or near '" +
		      String(MSCorrGramtext) + "'"));
}

String msCorrGramRemoveEscapes (const String& in)
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

String msCorrGramRemoveQuotes (const String& in)
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
	    throw (AipsError ("MSCorrParse - Ill-formed quoted string: " +
			      str));
	}
	out += str.at (pos+1, inx-pos-1);             // add substring
	pos = inx+1;
    }
    return out;
}

} //# NAMESPACE CASACORE - END
