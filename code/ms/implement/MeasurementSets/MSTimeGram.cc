//# MSTimeGram.cc: Grammar for time expressions
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

// MSTimeGram; grammar for time command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.
// This is a preliminary version; eventually it has to be incorporated
// in the AIPS++ command language.

#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <ms/MeasurementSets/MSTimeGram.h>
#include <ms/MeasurementSets/MSTimeParse.h> // routines used by bison actions
#include <tables/Tables/TableParse.h>       // routines used by bison actions
#include <tables/Tables/TableError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casa/stdlib.h>
#include <MSTimeGram.ycc>                  // flex output
#include <MSTimeGram.lcc>                  // bison output

// Define the yywrap function for flex.
int MSTimeGramwrap()
{
    return 1;
}

namespace casa { //# NAMESPACE CASA - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const MeasurementSet* msMSTimeGram = 0x0;
static const char*           strpMSTimeGram = 0;
static Int                   posMSTimeGram = 0;


//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int msTimeGramParseCommand (const MeasurementSet& ms, const String& command) 
{
    MSTimeGramrestart (MSTimeGramin);
    yy_start = 1;
    strpMSTimeGram = command.chars();     // get pointer to command string
    posMSTimeGram  = 0;                   // initialize string position
    msMSTimeGram = &ms;                   // get pointer to measurement set
    return MSTimeGramparse();             // parse command string
}

//# Give the measurement set
const MeasurementSet& msTimeGramMS()
{
    return *msMSTimeGram;
}

//# Give the string position.
Int& msTimeGramPosition()
{
    return posMSTimeGram;
}

//# Get the next input characters for flex.
int msTimeGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpMSTimeGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpMSTimeGram++;
    }
    return nr;
}

void MSTimeGramerror (char*)
{
    throw (AipsError ("UV Distribution Expression: Parse error at or near '" +
		      String(MSTimeGramtext) + "'"));
}

String msTimeGramRemoveEscapes (const String& in)
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

String msTimeGramRemoveQuotes (const String& in)
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
	    throw (AipsError ("MSTimeParse - Ill-formed quoted string: " +
			      str));
	}
	out += str.at (pos+1, inx-pos-1);             // add substring
	pos = inx+1;
    }
    return out;
}

} //# NAMESPACE CASA - END
