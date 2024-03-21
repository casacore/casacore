//# MSFeedGram.h: Grammar for ms feed sub-expressions
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

#ifndef MS_MSFEEDGRAM_H
#define MS_MSFEEDGRAM_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/ms/MSSel/MSFeedParse.h> // routines used by bison actions

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;
class TableExprNode;

// <summary>
// Global functions for flex/bison scanner/parser for MSFeedGram
// </summary>
  
// <use visibility=local>
  
// <reviewed reviewer="" date="" tests="">
// </reviewed>
  
// <prerequisite>
//# Classes you should understand before using this one.
//  <li> MSFeedGram.l and .y  (flex and bison grammar)
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
  
  
// <group name=MSFeedGramFunctions>
  
// Declare the bison parser (is implemented by bison command).
// It returns a TaQL expression tree.
  TableExprNode msFeedGramParseCommand (MSFeedParse* thisParser,
					   const TableExprNode& col1TEN,
					   const TableExprNode& col2TEN,
                                           const String& command, 
                                           Vector<Int>& selectedFeeds1,
                                           Vector<Int>& selectedFeeds2,
                                           Matrix<Int>& selectedFeedPairs) ;
  TableExprNode msFeedGramParseCommand (Table& subTable,
					   TableExprNode& col1TEN,
					   TableExprNode& col2TEN,
                                           const String& command, 
                                           Vector<Int>& selectedFeeds1,
                                           Vector<Int>& selectedFeeds2,
                                           Matrix<Int>& selectedFeedPairs) ;
  TableExprNode msFeedGramParseCommand (const MeasurementSet *ms,
					   const String& command,
					   Vector<Int>& selectedFeeds1,
					   Vector<Int>& selectedFeeds2,
					   Matrix<Int>& selectedFeedPairs);
  
  TableExprNode baseMSFeedGramParseCommand(MSFeedParse* parser, const String& command,
					      Vector<Int>& selectedFeeds1,
					      Vector<Int>& selectedFeeds2,
					      Matrix<Int>& selectedFeedPairs);
  // The yyerror function for the parser.
  // It throws an exception with the current token.
    void MSFeedGramerror (const char*);
  
  // Give the current position in the string.
  // This can be used when parse errors occur.
  Int& msFeedGramPosition();
  
  // Declare the input routine for flex/bison.
  int msFeedGramInput (char* buf, int max_size);
  
  // A function to remove escaped characters.
  //String msFeedGramRemoveEscapes (const String& in);
  
  // A function to remove quotes from a quoted string.
  //String msFeedGramRemoveQuotes (const String& in);
  
  // </group>
  
} //# NAMESPACE CASACORE - END

#endif
