//# LatticeGram.cc: Grammar for lattice expressions
//# Copyright (C) 1998
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

// LatticeGram; grammar for lattice command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.
// This is a preliminary version; eventually it has to be incorporated
// in the AIPS++ command language.


#if defined (sparc)
#if !defined(AIPS_SUN_NATIVE) && !defined(__GNUG__) && !defined(AIPS_STDLIB)
extern "C" char *__builtin_alloca(int);
#endif
#include <alloca.h>
#endif
#if defined (_AIX)
#pragma alloca
#include <malloc.h>
#endif

#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/LatticeGram.h>
#include <trial/Lattices/LatticeParse.h>    // routines used by bison actions
#include <aips/Exceptions/Error.h>

#include <LatticeGram.ycc>                  // flex output
#include <LatticeGram.lcc>                  // bison output


//# Declare a file global pointer to a char* for the input string.
static const char*  strpLatticeGram = 0;
static Int          posLatticeGram = 0;


// Define the yywrap function for flex.
int LatticeGramwrap()
{
    return 1;
}

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int latticeGramParseCommand (const String& command)
{
    LatticeGramrestart (LatticeGramin);
    yy_start = 1;
    strpLatticeGram = command.chars();     // get pointer to command string
    posLatticeGram  = 0;                   // initialize string position
    return LatticeGramparse();             // parse command string
}

//# Give the string position.
Int& latticeGramPosition()
{
    return posLatticeGram;
}

//# Get the next input characters for flex.
int latticeGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpLatticeGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpLatticeGram++;
    }
    return nr;
}

void LatticeGramerror (char*)
{
    throw (AipsError ("Lattice Expression: Parse error at or near '" +
		      String(yytext) + "'"));
}

String latticeGramRemoveEscapes (const String& in)
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
