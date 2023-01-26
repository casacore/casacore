//# MSObservationGram.cc: Grammar for scan expressions
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

// MSObservationGram; grammar for scan command lines

// This file includes the output files of bison and flex for
// parsing command lines.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSObservationGram.h>
#include <casacore/ms/MSSel/MSObservationParse.h> // routines used by bison actions
#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>

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
#include "MSObservationGram.ycc"                  // bison output
#ifdef YY_NULL
# undef YY_NULL
#endif
#include "MSObservationGram.lcc"                  // flex output
#pragma GCC diagnostic pop


// Define the yywrap function for flex.
int MSObservationGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*           strpMSObservationGram = 0;
static int32_t                   posMSObservationGram = 0;

  
  //# Parse the command.
  //# Do a yyrestart(yyin) first to make the flex scanner reentrant.
  TableExprNode baseMSObservationGramParseCommand (MSObservationParse* parser, 
						   const String& command, 
						   Vector<int32_t>& selectedIDs)
  {
    try
      {
	MSObservationGramrestart (MSObservationGramin);
	yy_start = 1;
	strpMSObservationGram = command.chars();       // get pointer to command string
	posMSObservationGram  = 0;                     // initialize string position
	//MSObservationParse parser(ms,obsSubTable);     // setup measurement set
	MSObservationParse::thisMSObsParser = parser; // The global pointer to the parser
	parser->reset();
	//	parser->setMaxObs(maxObsIDs);
	MSObservationGramparse();                      // parse command string
	
	selectedIDs=parser->selectedIDs();
	return parser->node();
      }
    catch (MSSelectionObservationError &x)
      {
	String newMesgs;
	newMesgs = constructMessage(msObservationGramPosition(), command);
	x.addMessage(newMesgs);
	throw;
      }
  }
  TableExprNode msObservationGramParseCommand (const MeasurementSet* ms, const MSObservation& obsSubTable,
					       const TableExprNode& colAsTEN,
					       const String& command, 
					       Vector<int32_t>& selectedIDs)
  {
    TableExprNode ret;
    MSObservationParse *thisParser = new MSObservationParse(ms, obsSubTable, colAsTEN);
    try
      {
	ret = baseMSObservationGramParseCommand(thisParser, command, selectedIDs);
      }
    catch (MSSelectionObservationError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return ret;
  }

  // TableExprNode msObservationGramParseCommand (const MeasurementSet* ms, const MSObservation& obsSubTable,
  // 					       const String& command, 
  // 					       Vector<int32_t>& selectedIDs, int32_t maxObsIDs) 
  // {
  //   try
  //     {
  // 	MSObservationGramrestart (MSObservationGramin);
  // 	yy_start = 1;
  // 	strpMSObservationGram = command.chars();       // get pointer to command string
  // 	posMSObservationGram  = 0;                     // initialize string position
  // 	MSObservationParse parser(ms,obsSubTable);     // setup measurement set
  // 	MSObservationParse::thisMSObsParser = &parser; // The global pointer to the parser
  // 	parser.reset();
  // 	parser.setMaxObs(maxObsIDs);
  // 	MSObservationGramparse();                      // parse command string
	
  // 	selectedIDs=parser.selectedIDs();
  // 	return parser.node();
  //     }
  //   catch (MSSelectionObservationError &x)
  //     {
  // 	String newMesgs;
  // 	newMesgs = constructMessage(msObservationGramPosition(), command);
  // 	x.addMessage(newMesgs);
  // 	throw;
  //     }
  // }
  
  //# Give the table expression node
  // const TableExprNode* msObservationGramParseNode()
  // {
  //   //    return MSObservationParse::node();
  //     return &MSObservationParse::thisMSObsParser->node();
  // }
  void msObservationGramParseDeleteNode()
  {
    //    return MSObservationParse::cleanup();
    return MSObservationParse::thisMSObsParser->cleanup();
  }
  
  //# Give the string position.
  int32_t& msObservationGramPosition()
  {
    return posMSObservationGram;
  }
  
  //# Get the next input characters for flex.
  int msObservationGramInput (char* buf, int max_size)
  {
    int nr=0;
    while (*strpMSObservationGram != 0) {
      if (nr >= max_size) {
	break;                         // get max. max_size char.
      }
      buf[nr++] = *strpMSObservationGram++;
    }
    return nr;
  }
  
  void MSObservationGramerror (const char*)
  {
    throw (MSSelectionObservationError ("Scan Expression: Parse error at or near '" +
					String(MSObservationGramtext) + "'"));
  }
  
  // String msObservationGramRemoveEscapes (const String& in)
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
  
  // String msObservationGramRemoveQuotes (const String& in)
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
  // 	    throw (AipsError ("MSObservationParse - Ill-formed quoted string: " +
  // 			      str));
  // 	}
  // 	out += str.at (pos+1, inx-pos-1);             // add substring
  // 	pos = inx+1;
  //     }
  //     return out;
  // }
  
} //# NAMESPACE CASACORE - END
