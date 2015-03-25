//# MSAntennaGram.cc: Grammar for antenna expressions
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

// MSAntennaGram; grammar for antenna command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSAntennaGram.h>
#include <casacore/ms/MSSel/MSAntennaParse.h> // routines used by bison actions
#include <casacore/ms/MSSel/MSAntennaIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "MSAntennaGram.ycc"                  // bison output
#include "MSAntennaGram.lcc"                  // flex output

// Define the yywrap function for flex.
int MSAntennaGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
  static const char*           strpMSAntennaGram = 0;
  static Int                   posMSAntennaGram = 0;

//# Parse the command.
//# Do a yyrestart(yyin) first to make the flex scanner reentrant.
  TableExprNode baseMSAntennaGramParseCommand(MSAntennaParse* parser, const String& command,
					      Vector<Int>& selectedAnts1,
					      Vector<Int>& selectedAnts2,
					      Matrix<Int>& selectedBaselines)
  {
    try 
      {
	MSAntennaGramrestart (MSAntennaGramin);
	yy_start = 1;
	strpMSAntennaGram = command.chars();     // get pointer to command string
	posMSAntennaGram  = 0;                   // initialize string position
	parser->setComplexity();
	MSAntennaParse::thisMSAParser = parser; // The global pointer to the parser
	MSAntennaGramparse();                    // parse command string
	
	selectedAnts1.reference (parser->selectedAnt1());
	selectedAnts2.reference (parser->selectedAnt2());
	selectedBaselines.reference (parser->selectedBaselines());
	return parser->node();
      } 
    catch (MSSelectionAntennaError& x) 
      {
	String newMesgs;
	newMesgs = constructMessage(msAntennaGramPosition(),command);
	x.addMessage(newMesgs);
	throw;
      }
  }					      

  TableExprNode msAntennaGramParseCommand (MSSelectableTable& msLike,
                                           const String& command, 
                                           Vector<Int>& selectedAnts1,
                                           Vector<Int>& selectedAnts2,
                                           Matrix<Int>& selectedBaselines) 
  {
    TableExprNode col1TEN = msLike.col(msLike.columnName(MS::ANTENNA1)),
      col2TEN = msLike.col(msLike.columnName(MS::ANTENNA2));

    TableExprNode antennaTEN;
    //    MSAntennaParse *thisParser = new MSAntennaParse(msLike.antenna(), col1TEN, col2TEN);
    MSAntennaParse thisParser(msLike.antenna(), col1TEN, col2TEN);
    try
      {
	antennaTEN = baseMSAntennaGramParseCommand(&thisParser, command, 
						   selectedAnts1, selectedAnts2,
						   selectedBaselines);
      }
    catch(MSSelectionAntennaError &x)
      {
	//	delete thisParser;
	throw;
      }
    
    //delete thisParser;
    return antennaTEN;
  }

  TableExprNode msAntennaGramParseCommand (Table& subTable,
					   TableExprNode& col1TEN,
					   TableExprNode& col2TEN,
                                           const String& command, 
                                           Vector<Int>& selectedAnts1,
                                           Vector<Int>& selectedAnts2,
                                           Matrix<Int>& selectedBaselines) 
  {
    // TableExprNode col1TEN = msLike.col(msLike.columnName(MS::ANTENNA1)),
    //   col2TEN = msLike.col(msLike.columnName(MS::ANTENNA2));

    TableExprNode antennaTEN;
    //    MSAntennaParse *thisParser = new MSAntennaParse(msLike.antenna(), col1TEN, col2TEN);
    MSAntennaParse thisParser(subTable, col1TEN, col2TEN);
    try
      {
	antennaTEN = baseMSAntennaGramParseCommand(&thisParser, command, 
						   selectedAnts1, selectedAnts2,
						   selectedBaselines);
      }
    catch(MSSelectionAntennaError &x)
      {
	//	delete thisParser;
	throw;
      }
    
    //delete thisParser;
    return antennaTEN;
  }

  TableExprNode msAntennaGramParseCommand (MSAntennaParse* thisParser,
                                           const String& command, 
                                           Vector<Int>& selectedAnts1,
                                           Vector<Int>& selectedAnts2,
                                           Matrix<Int>& selectedBaselines) 
  {
    TableExprNode antennaTEN;
    try
      {
	antennaTEN=baseMSAntennaGramParseCommand(thisParser, command, 
						 selectedAnts1, selectedAnts2,
						 selectedBaselines);
      }
    catch(MSSelectionAntennaError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return antennaTEN;
  }

  TableExprNode msAntennaGramParseCommand (const MeasurementSet* ms,
                                           const String& command, 
                                           Vector<Int>& selectedAnts1,
                                           Vector<Int>& selectedAnts2,
                                           Matrix<Int>& selectedBaselines) 
  {
    TableExprNode antennaTEN;
    TableExprNode col1AsTEN = ms->col(ms->columnName(MS::ANTENNA1)),
    col2AsTEN = ms->col(ms->columnName(MS::ANTENNA2));
    MSAntennaParse *thisParser = new MSAntennaParse(ms->antenna(),col1AsTEN, col2AsTEN);
    try
      {
	antennaTEN=baseMSAntennaGramParseCommand(thisParser, command, 
						 selectedAnts1, selectedAnts2,
						 selectedBaselines);
      }
    catch(MSSelectionAntennaError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return antennaTEN;



    // try 
    //   {
    // 	MSAntennaGramrestart (MSAntennaGramin);
    // 	yy_start = 1;
    // 	strpMSAntennaGram = command.chars();     // get pointer to command string
    // 	posMSAntennaGram  = 0;                   // initialize string position
    // 	MSAntennaParse parser(ms);               // setup measurement set
    // 	parser.setComplexity();
    // 	MSAntennaParse::thisMSAParser = &parser; // The global pointer to the parser
    // 	MSAntennaGramparse();                    // parse command string
	
    // 	selectedAnts1.reference (parser.selectedAnt1());
    // 	selectedAnts2.reference (parser.selectedAnt2());
    // 	selectedBaselines.reference (parser.selectedBaselines());
    // 	return parser.node();
    //   } 
    // catch (MSSelectionAntennaError& x) 
    //   {
    // 	String newMesgs;
    // 	newMesgs = constructMessage(msAntennaGramPosition(),command);
    // 	x.addMessage(newMesgs);
    // 	throw;
    //   }
  }
  
  //# Give the string position.
  Int& msAntennaGramPosition()
  {
    return posMSAntennaGram;
  }
  
  //# Get the next input characters for flex.
  int msAntennaGramInput (char* buf, int max_size)
  {
    int nr=0;
    while (*strpMSAntennaGram != 0) {
      if (nr >= max_size) {
	break;                         // get max. max_size char.
      }
      buf[nr++] = *strpMSAntennaGram++;
    }
    return nr;
  }
  
  void MSAntennaGramerror (const char*)
  {
    throw (MSSelectionAntennaParseError ("Antenna Expression: Parse error at or near '" +
					 String(MSAntennaGramtext) + "'"));
  }
  
  // String msAntennaGramRemoveEscapes (const String& in)
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
  
  // String msAntennaGramRemoveQuotes (const String& in)
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
  // 	    throw (AipsError ("MSAntennaParse - Ill-formed quoted string: " +
  // 			      str));
  // 	}
  // 	out += str.at (pos+1, inx-pos-1);             // add substring
  // 	pos = inx+1;
  //     }
  //     return out;
  // }
  
} //# NAMESPACE CASACORE - END
