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
// command lines operating on spectral window selection.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
//#include <casacore/ms/MeasurementSets/MSSpwColumns.h>
#include <casacore/ms/MSSel/MSSpwGram.h>
#include <casacore/ms/MSSel/MSSpwParse.h>
#include <casacore/ms/MSSel/MSSpwIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>

#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "MSSpwGram.ycc"                  // bison output
#include "MSSpwGram.lcc"                  // flex output

// Define the yywrap function for flex.
int MSSpwGramwrap()
{
  return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
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
  
  int baseMSSpwGramParseCommand (MSSpwParse* parser, const String& command,
				 Vector<Int>& selectedIDs, Matrix<Int>&selectedChans,
				 Vector<Int>& selectedDDIDs) 
  {
    try 
      {
	Int ret;
	MSSpwGramrestart (MSSpwGramin);
	yy_start = 1;
	strpMSSpwGram = command.chars();     // get pointer to command string
	posMSSpwGram  = 0;                   // initialize string position
	MSSpwParse::thisMSSParser = parser; // The global pointer to the parser
	parser->reset();
	ret=MSSpwGramparse();                // parse command string
	selectedIDs = parser->selectedIDs();
	selectedChans = parser->selectedChanIDs();
	selectedDDIDs = parser->selectedDDIDs();
	if ((selectedIDs.size() == 0) ||
	    (selectedChans.size() == 0))
	  throw(MSSelectionSpwParseError("No valid SPW & Chan combination found"));
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
  
  int msSpwGramParseCommand (const MSSpectralWindow& spwSubTable, 
			     const MSDataDescription& ddSubTable, 
			     const TableExprNode& colAsTEN,
			     const String& command,
			     Vector<Int>& selectedIDs,
			     Matrix<Int>& selectedChans,
			     Vector<Int>& selectedDDIDs) 
  {
    Int ret;
    //    MSSpwParse *thisParser = new MSSpwParse(ms);
    MSSpwParse *thisParser = new MSSpwParse(spwSubTable, ddSubTable, colAsTEN);
    try
      {
	ret=baseMSSpwGramParseCommand(thisParser, command, selectedIDs, selectedChans, selectedDDIDs);
      }
    catch(MSSelectionSpwError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return ret;
  }

  int msSpwGramParseCommand (const MeasurementSet *ms, 
			     const String& command,Vector<Int>& selectedIDs,
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
	if ((selectedIDs.size() == 0) ||
	    (selectedChans.size() == 0))
	  throw(MSSelectionSpwParseError("No valie SPW & Chan combination found"));
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
  
  void msSpwGramParseDeleteNode()
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
  
  void MSSpwGramerror (const char*)
  {
    throw (MSSelectionSpwParseError("Spw Expression: Parse error at or near '" +
				    String(MSSpwGramtext) + "'"));
  }
  
} //# NAMESPACE CASACORE - END
