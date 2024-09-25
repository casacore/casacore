//# MSFeedGram.cc: Grammar for feed expressions
//# Copyright (C) 2015
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

// MSFeedGram; grammar for feed command lines based on antenna grammar

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSFeedGram.h>
#include <casacore/ms/MSSel/MSFeedParse.h> // routines used by bison actions
#include <casacore/ms/MSSel/MSFeedIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>

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
#include "MSFeedGram.ycc"                  // bison output
#ifdef YY_NULL
# undef YY_NULL
#endif
#include "MSFeedGram.lcc"                  // flex output
#pragma GCC diagnostic pop


// Define the yywrap function for flex.
int MSFeedGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
  static const char*           strpMSFeedGram = 0;
  static Int                   posMSFeedGram = 0;

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
  TableExprNode baseMSFeedGramParseCommand(MSFeedParse* parser, const String& command,
					      Vector<Int>& selectedFeeds1,
					      Vector<Int>& selectedFeeds2,
					      Matrix<Int>& selectedFeedPairs)
  {
    try 
      {
	MSFeedGramrestart (MSFeedGramin);
	yy_start = 1;
	strpMSFeedGram = command.chars();     // get pointer to command string
	posMSFeedGram  = 0;                   // initialize string position
	MSFeedParse::thisMSFParser = parser; // The global pointer to the parser
	MSFeedGramparse();                    // parse command string
	
	selectedFeeds1.reference (parser->selectedFeed1());
	selectedFeeds2.reference (parser->selectedFeed2());
	selectedFeedPairs.reference (parser->selectedFeedPairs());
	return parser->node();
      } 
    catch (MSSelectionFeedError& x) 
      {
	String newMesgs;
	newMesgs = constructMessage(msFeedGramPosition(),command);
	x.addMessage(newMesgs);
	throw;
      }
  }					      


  TableExprNode msFeedGramParseCommand (Table& subTable,
					   TableExprNode& col1TEN,
					   TableExprNode& col2TEN,
                       const String& command, 
                       Vector<Int>& selectedFeeds1,
                       Vector<Int>& selectedFeeds2,
                       Matrix<Int>& selectedFeedPairs) 
  {

    TableExprNode feedTEN;
    MSFeedParse thisParser(subTable, col1TEN, col2TEN);
    try
      {
	feedTEN = baseMSFeedGramParseCommand(&thisParser, command, 
						   selectedFeeds1, selectedFeeds2,
						   selectedFeedPairs);
      }
    catch(MSSelectionFeedError &x)
      {
	    throw;
      }
    
    return feedTEN;
  }

  TableExprNode msFeedGramParseCommand (MSFeedParse* thisParser,
                                           const String& command, 
                                           Vector<Int>& selectedFeeds1,
                                           Vector<Int>& selectedFeeds2,
                                           Matrix<Int>& selectedFeedPairs) 
  {
    TableExprNode feedTEN;
    try
      {
	feedTEN=baseMSFeedGramParseCommand(thisParser, command, 
						 selectedFeeds1, selectedFeeds2,
						 selectedFeedPairs);
      }
    catch(MSSelectionFeedError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return feedTEN;
  }

  TableExprNode msFeedGramParseCommand (const MeasurementSet* ms,
                                           const String& command, 
                                           Vector<Int>& selectedFeeds1,
                                           Vector<Int>& selectedFeeds2,
                                           Matrix<Int>& selectedFeedPairs) 
  {
    TableExprNode feedTEN;
    TableExprNode col1AsTEN = ms->col(ms->columnName(MS::FEED1)),
    col2AsTEN = ms->col(ms->columnName(MS::FEED2));
    MSFeedParse *thisParser = new MSFeedParse(ms->feed(),col1AsTEN, col2AsTEN);
    try
      {
	    feedTEN=baseMSFeedGramParseCommand(thisParser, command, 
						 selectedFeeds1, selectedFeeds2,
						 selectedFeedPairs);
      }
    catch(MSSelectionFeedError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return feedTEN;
  }
  
  //# Give the string position.
  Int& msFeedGramPosition()
  {
    return posMSFeedGram;
  }
  
  //# Get the next input characters for flex.
  int msFeedGramInput (char* buf, int max_size)
  {
    int nr=0;
    while (*strpMSFeedGram != 0) {
      if (nr >= max_size) {
	break;                         // get max. max_size char.
      }
      buf[nr++] = *strpMSFeedGram++;
    }
    return nr;
  }
  
  void MSFeedGramerror (const char*)
  {
    throw (MSSelectionFeedParseError ("Feed Expression: Parse error at or near '" +
					 String(MSFeedGramtext) + "'"));
  }
  
} //# NAMESPACE CASACORE - END
