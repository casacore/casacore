//# MSSpwGram.h: Grammar for ms field sub-expressions
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

#ifndef MS_MSSPWGRAM_H
#define MS_MSSPWGRAM_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //# Forward Declarations
  class MeasurementSet;
  class TableExprNode;
  
  // <summary>
  // Global functions for flex/bison scanner/parser for MSSpwGram
  // </summary>
  
  // <use visibility=local>
  
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>
  
  // <prerequisite>
  //# Classes you should understand before using this one.
  //  <li> MSSpwGram.l and .y  (flex and bison grammar)
  // </prerequisite>
  
  // <synopsis> 
  // Global functions are needed to define the input of the flex scanner
  // and to start the bison parser.
  // The input is taken from a string.
  // </synopsis> 
  
  // <motivation>
  // It is necessary to be able to give an image expression in ASCII.
  // This can be used in glish.
  // </motivation>
  // <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  // </todo>
  
  
  // <group name=MSSpwGramFunctions>
  
  // Declare the bison parser (is implemented by bison command).
  int msSpwGramParseCommand (const MeasurementSet *ms, const String& command);
  int msSpwGramParseCommand (const MeasurementSet *ms, const String& command,
			     Vector<Int>& selectedIDs, Matrix<Int>& selectedChanIDs);
  int msSpwGramParseCommand (const MSSpectralWindow& spwSubTable, const MSDataDescription& ddSubTable, 
			     const TableExprNode& colAsTEN,
			     const String& command,
			     Vector<Int>& selectedIDs,
			     Matrix<Int>& selectedChans,
			     Vector<Int>& selectedDDIDs) ;
  
  // The yyerror function for the parser.
  // It throws an exception with the current token.
  void MSSpwGramerror (const char*);
  
  // Give the table expression node.
  const TableExprNode *msSpwGramParseNode();
  void msSpwGramParseDeleteNode();
  
  // Give the current position in the string.
  // This can be used when parse errors occur.
  Int& msSpwGramPosition();
  
  // Declare the input routine for flex/bison.
  int msSpwGramInput (char* buf, int max_size);
  
  // A function to remove escaped characters.
  //String msSpwGramRemoveEscapes (const String& in);
  
  // A function to remove quotes from a quoted string.
  //String msSpwGramRemoveQuotes (const String& in);
  
  // </group>
  
} //# NAMESPACE CASACORE - END

#endif
