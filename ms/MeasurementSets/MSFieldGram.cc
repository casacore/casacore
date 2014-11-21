//# MSUvDistGram.cc: Grammar for field expressions
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

// MSUvDistGram; grammar for field command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSFieldGram.h>
#include <casacore/ms/MeasurementSets/MSFieldParse.h>
#include <casacore/ms/MeasurementSets/MSFieldIndex.h>
#include <casacore/ms/MeasurementSets/MSSelectionError.h>

#include <casacore/tables/TaQL/TableParse.h>       // routines used by bison actions
#include <casacore/tables/Tables/TableError.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
#include <casacore/casa/stdlib.h>
//# Define register as empty string to avoid warnings in C++11 compilers
//# because keyword register is not supported anymore.
#define register
#include "MSFieldGram.ycc"                  // bison output
#include "MSFieldGram.lcc"                  // flex output

// Define the yywrap function for flex.
int MSFieldGramwrap()
{
  return 1;
}

namespace casa { //# NAMESPACE CASA - BEGIN
  
  //# Declare a file global pointer to a char* for the input string.
  static const char*           strpMSFieldGram = 0;
  static Int                   posMSFieldGram = 0;
  // MSFieldGramwrap out of namespace
  
  //# Parse the command.
  //# Do a yyrestart(yyin) first to make the flex scanner reentrant.
  // TableExprNode msFieldGramParseCommand (const MeasurementSet* ms, const String& command) 
  // {
  //   try 
  //     {
  // 	MSFieldGramrestart (MSFieldGramin);
  // 	yy_start = 1;
  // 	strpMSFieldGram = command.chars();     // get pointer to command string
  // 	posMSFieldGram  = 0;                   // initialize string position
  // 	MSFieldParse parser(ms);               // setup measurement set
  // 	MSFieldParse::thisMSFParser = &parser; // The global pointer to the parser
  // 	MSFieldParse::thisMSFParser->reset();
  // 	//      fieldError.reset();
  // 	MSFieldGramparse();                // parse command string
  // 	return parser.node();
  //     }
  //   catch (MSSelectionFieldError &x)
  //     {
  // 	String newMesgs;
  // 	newMesgs = constructMessage(msFieldGramPosition(), command);
  // 	x.addMessage(newMesgs);
  // 	throw;
  //     }
  // }
  
  // TableExprNode msFieldGramParseCommand (const MeasurementSet* ms, const String& command, Vector<Int>& selectedIDs)
  // {
  //   //    MSFieldParse *thisParser = new MSFieldParse(ms);
  //   TableExprNode dummy,ten;
  //   MSFieldParse *thisParser = new MSFieldParse(ms->field(),dummy);
  //   ten=baseMSFieldGramParseCommand(thisParser, command, selectedIDs);
  //   delete thisParser;
  //   return ten;
  // }

  TableExprNode msFieldGramParseCommand (const MSField& msFieldSubTable, const TableExprNode& ten,
					 const String& command, Vector<Int>& selectedIDs)
  {
    //    MSFieldParse *thisParser = new MSFieldParse(ms);
    TableExprNode fieldTEN;
    MSFieldParse *thisParser = new MSFieldParse(msFieldSubTable,ten);
    try
      {
	fieldTEN=baseMSFieldGramParseCommand(thisParser, command, selectedIDs);
      }
    catch(MSSelectionFieldError &x)
      {
	delete thisParser;
	throw;
      }
    delete thisParser;
    return fieldTEN;
  }

  TableExprNode baseMSFieldGramParseCommand (MSFieldParse* parser, const String& command, Vector<Int>& selectedIDs)
  {
    //    MSFieldParse parser(ms);               // setup measurement set
    try 
      {
	MSFieldGramrestart (MSFieldGramin);
	yy_start = 1;
	strpMSFieldGram = command.chars();     // get pointer to command string
	posMSFieldGram  = 0;                   // initialize string position
	//	MSFieldParse::thisMSFParser = &parser; // The global pointer to the parser
	MSFieldParse::thisMSFParser = parser; // The global pointer to the parser
	parser->reset();
	MSFieldGramparse();                // parse command string
	
	selectedIDs=parser->selectedIDs();
	return *(msFieldGramParseNode());
      }
    catch (MSSelectionFieldError &x)
      {
	String newMesgs;
	newMesgs = constructMessage(msFieldGramPosition(), command);
	x.addMessage(newMesgs);
	throw;
      }
  }
  
  //# Give the table expression node
  const TableExprNode* msFieldGramParseNode()
  {
    return MSFieldParse::node();
  }
  void msFieldGramParseDeleteNode() {MSFieldParse::cleanup();}
  //# Give the string position.
  Int& msFieldGramPosition()
  {
    return posMSFieldGram;
  }
  
  //# Get the next input characters for flex.
  int msFieldGramInput (char* buf, int max_size)
  {
    int nr=0;
    while (*strpMSFieldGram != 0) {
      if (nr >= max_size) {
	break;                         // get max. max_size char.
      }
      buf[nr++] = *strpMSFieldGram++;
    }
    return nr;
  }
  
  void MSFieldGramerror (const char*)
  {
    throw (MSSelectionFieldParseError ("Field Expression: Parse error at or near '" +
				       String(MSFieldGramtext) + "'"));
  }
  
  // String msFieldGramRemoveEscapes (const String& in)
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
  
  // String msFieldGramRemoveQuotes (const String& in)
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
  // 	    throw (AipsError ("MSFieldParse - Ill-formed quoted string: " +
  // 			      str));
  // 	}
  // 	out += str.at (pos+1, inx-pos-1);             // add substring
  // 	pos = inx+1;
  //     }
  //     return out;
  // }
  
} //# NAMESPACE CASA - END
