//# TableGram.h: Grammar for table command lines
//# Copyright (C) 1993,1994,1995,1997,1999
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

#ifndef TABLES_TABLEGRAM_H
#define TABLES_TABLEGRAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/MVTime.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Global functions for flex/bison scanner/parser for TableGram
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTableGram">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//  <li> TableGram.l and .y  (flex and bison grammar)
// </prerequisite>

// <synopsis> 
// Global functions are needed to define the input of the flex scanner
// and to start the bison parser.
// The input is taken from a string.
// </synopsis> 

// <motivation>
// It is necessary to be able to give a table select command in ASCII.
// This can be used in a CLI or in the table browser to get a subset
// of a table or to sort a table.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>

// <group name=TableGramFunctions>

// Declare the bison parser (is implemented by bison command).
int tableGramParseCommand (const String& command);

// The yyerror function for the parser.
// It throws an exception with the current token.
void TableGramerror (const char*);

// Give the current position in the string.
// This can be used when parse errors occur.
Int& tableGramPosition();

// Declare the input routine for flex/bison.
int tableGramInput (char* buf, int max_size);

// A function to remove escaped characters.
String tableGramRemoveEscapes (const String& in);

// A function to remove quotes from a quoted string.
String tableGramRemoveQuotes (const String& in);

// A function to parse a date/time string.
MVTime tableGramParseDateTime (const String& in);

// A function to parse a time/position string.
// The value is returned in radians.
Double tableGramParseTime (const String& in);

// </group>



} //# NAMESPACE CASACORE - END

#endif
