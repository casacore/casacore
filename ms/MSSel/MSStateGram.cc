//# MSStateGram.cc: Grammar for field expressions
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

// MSStateGram; grammar for field command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSStateColumns.h>
#include <casacore/ms/MSSel/MSStateGram.h>
#include <casacore/ms/MSSel/MSStateParse.h>
#include <casacore/ms/MSSel/MSStateIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>

#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "MSStateGram.ycc"                  // bison output
#include "MSStateGram.lcc"                  // flex output

// Define the yywrap function for flex.
int MSStateGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*           strpMSStateGram = 0;
static Int                   posMSStateGram = 0;
  // MSStateGramwrap out of namespace

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int msStateGramParseCommand (const MeasurementSet* ms, const String& command) 
{
  try 
    {
      Int ret;
      MSStateGramrestart (MSStateGramin);
      yy_start = 1;
      strpMSStateGram = command.chars();     // get pointer to command string
      posMSStateGram  = 0;                   // initialize string position
      MSStateParse parser(ms);               // setup measurement set
      MSStateParse::thisMSSIParser = &parser; // The global pointer to the parser
      MSStateParse::thisMSSIParser->reset();
      //      fieldError.reset();
      ret=MSStateGramparse();                // parse command string
      return ret;
    }
  catch (MSSelectionStateError &x)
    {
      String newMesgs;
      newMesgs = constructMessage(msStateGramPosition(), command);
      x.addMessage(newMesgs);
      throw;
    }
}

  int msStateGramParseCommand (const MeasurementSet* ms, const String& command, Vector<Int>& selectedIDs)
			       
{
  try 
    {
      Int ret;
      MSStateGramrestart (MSStateGramin);
      yy_start = 1;
      strpMSStateGram = command.chars();     // get pointer to command string
      posMSStateGram  = 0;                   // initialize string position
      MSStateParse parser(ms);               // setup measurement set
      MSStateParse::thisMSSIParser = &parser; // The global pointer to the parser
      parser.reset();
      ret=MSStateGramparse();                // parse command string
      
      selectedIDs=parser.selectedIDs();
      return ret;
    }
  catch (MSSelectionStateError &x)
    {
      String newMesgs;
      newMesgs = constructMessage(msStateGramPosition(), command);
      x.addMessage(newMesgs);
      throw;
    }
}

//# Give the table expression node
const TableExprNode* msStateGramParseNode()
{
  return MSStateParse::node();
}
void msStateGramParseDeleteNode() {MSStateParse::cleanup();}
//# Give the string position.
Int& msStateGramPosition()
{
    return posMSStateGram;
}

//# Get the next input characters for flex.
int msStateGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpMSStateGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpMSStateGram++;
    }
    return nr;
}

void MSStateGramerror (const char*)
{
    throw (MSSelectionStateParseError ("State Expression: Parse error at or near '" +
		      String(MSStateGramtext) + "'"));
}

} //# NAMESPACE CASACORE - END
