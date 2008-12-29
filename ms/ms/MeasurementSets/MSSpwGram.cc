//# MSSpwGram.cc: Grammar for spectral window expressions
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

// MSSpwGram; grammar for field command lines

// This file includes the output files of bison and flex for parsing
// command lines operating on lattices.  This is a preliminary
// version; eventually it has to be incorporated in the AIPS++ command
// language.

#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <ms/MeasurementSets/MeasurementSet.h>
//#include <ms/MeasurementSets/MSSpwColumns.h>
#include <ms/MeasurementSets/MSSpwGram.h>
#include <ms/MeasurementSets/MSSpwParse.h>
#include <ms/MeasurementSets/MSSpwIndex.h>
#include <ms/MeasurementSets/MSSelectionError.h>

#include <tables/Tables/TableParse.h>       // routines used by bison actions
#include <tables/Tables/TableError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casa/stdlib.h>
#include "MSSpwGram.ycc"                  // flex output
#include "MSSpwGram.lcc"                  // bison output

// Define the yywrap function for flex.
int MSSpwGramwrap()
{
    return 1;
}

namespace casa { //# NAMESPACE CASA - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*           strpMSSpwGram = 0;
static Int                   posMSSpwGram = 0;
  // MSSpwGramwrap out of namespace

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
int msSpwGramParseCommand (const MeasurementSet* ms, const String& command) 
{
  try 
    {
      Int ret;
      MSSpwGramrestart (MSSpwGramin);
      yy_start = 1;
      strpMSSpwGram = command.chars();     // get pointer to command string
      posMSSpwGram  = 0;                   // initialize string position
      MSSpwParse parser(ms);               // setup measurement set
      MSSpwParse::thisMSSParser = &parser; // The global pointer to the parser
      parser.reset();
      ret=MSSpwGramparse();                // parse command string
      return ret;
    }
  catch (MSSelectionSpwError &x)
    {
      String newMesgs;
      newMesgs = constructMessage(msSpwGramPosition(), command);
      x.addMessage(newMesgs);
      throw;
    }
}

int msSpwGramParseCommand (const MeasurementSet* ms, const String& command,Vector<Int>& selectedIDs,
			   Matrix<Int>& selectedChans) 
{
  try 
    {
      Int ret;
      MSSpwGramrestart (MSSpwGramin);
      yy_start = 1;
      strpMSSpwGram = command.chars();     // get pointer to command string
      posMSSpwGram  = 0;                   // initialize string position
      MSSpwParse parser(ms);               // setup measurement set
      MSSpwParse::thisMSSParser = &parser; // The global pointer to the parser
      parser.reset();
      ret=MSSpwGramparse();                // parse command string
      selectedIDs = parser.selectedIDs();
      selectedChans = parser.selectedChanIDs();
      return ret;
    }
  catch (MSSelectionSpwError &x)
    {
      String newMesgs;
      newMesgs = constructMessage(msSpwGramPosition(), command);
      x.addMessage(newMesgs);
      throw;
    }
}

//# Give the table expression node
const TableExprNode* msSpwGramParseNode()
{
  return MSSpwParse::node();
}

const void msSpwGramParseDeleteNode()
{
  MSSpwParse::cleanup();
}

//# Give the string position.
Int& msSpwGramPosition()
{
    return posMSSpwGram;
}

//# Get the next input characters for flex.
int msSpwGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpMSSpwGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpMSSpwGram++;
    }
    return nr;
}

void MSSpwGramerror (char*)
{
  throw (MSSelectionSpwParseError("Spw Expression: Parse error at or near '" +
				  String(MSSpwGramtext) + "'"));
}

// String msSpwGramRemoveEscapes (const String& in)
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

// String msSpwGramRemoveQuotes (const String& in)
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
// 	    throw (AipsError ("MSSpwParse - Ill-formed quoted string: " +
// 			      str));
// 	}
// 	out += str.at (pos+1, inx-pos-1);             // add substring
// 	pos = inx+1;
//     }
//     return out;
// }

} //# NAMESPACE CASA - END
