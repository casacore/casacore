//# TableGram.cc: Grammar for table command lines
//# Copyright (C) 1993,1994,1995,1997
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

// TableGram; grammar for table command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on tables.
// This is a preliminary version; eventually it has to be incorporated
// in the AIPS++ command language.

#include <stdio.h> // for printf on linux

#if defined (sparc)
#if !defined(AIPS_SUN_NATIVE) && !defined(__GNUG__) && !defined(__KCC)
extern "C" char *__builtin_alloca(int);
#endif
#include <alloca.h>
#endif
#if defined (_AIX)
#pragma alloca
#include <malloc.h>
#endif

#include <aips/Tables/ExprNode.h>
#include <aips/Tables/TableGram.h>
#include <aips/Tables/TableParse.h>       // routines used by bison actions

#include <TableGram.ycc>                  // flex output
#include <TableGram.lcc>                  // bison output


//# Declare a file global pointer to a char* for the input string.
static const char*  strpTableGram = 0;


// Define the yywrap function for flex.
int TableGramwrap()
{
    return 1;
}

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int tableGramParseCommand (const String& command)
{
    TableGramrestart (TableGramin);
    yy_start = 1;
    strpTableGram = command.chars();    // get pointer to command string
    return TableGramparse();            // parse command string
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


String tableGramRemoveEscapes (const String& in)
{
    String out;
    for (int i=0; i<in.length(); i++) {
	if (in[i] == '\\') {
	    i++;
	}
	out += in[i];
    }
    return out;
}
