//# MSPolnGram.h: Grammar for ms field sub-expressions
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

#ifndef MS_MSPOLNGRAM_H
#define MS_MSPOLNGRAM_H


//# Includes
#include <casa/BasicSL/String.h>
#include <ms/MeasurementSets/MSDataDescIndex.h>
#include <ms/MeasurementSets/MSPolIndex.h>
#include <casa/Containers/OrderedMap.h>

namespace casa { //# NAMESPACE CASA - BEGIN
  
  //# Forward Declarations
  class MeasurementSet;
  class TableExprNode;
  
  // <summary>
  // Global functions for flex/bison scanner/parser for MSPolnGram
  // </summary>
  
  // <use visibility=local>
  
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>
  
  // <prerequisite>
  //# Classes you should understand before using this one.
  //  <li> MSPolnGram.l and .y  (flex and bison grammar)
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
  
  
  // <group name=MSPolnGramFunctions>
  
  // Declare the bison parser (is implemented by bison command).
  int msPolnGramParseCommand (const MeasurementSet *ms, const String& command);
  int msPolnGramParseCommand (const MeasurementSet *ms, const String& command,
			      TableExprNode& node,
			      Vector<Int>& selectedDDIDs, 
			      OrderedMap<Int, Vector<Int> >& selectedPolnMap,
			      OrderedMap<Int, Vector<Vector<Int> > >& selectedSetupMap
			      );
  
  // The yyerror function for the parser.
  // It throws an exception with the current token.
  void MSPolnGramerror (char*);
  
  // Give the table expression node.
  const TableExprNode *msPolnGramParseNode();
  void msPolnGramParseDeleteNode();
  
  // Give the current position in the string.
  // This can be used when parse errors occur.
  int& msPolnGramPosition();
  
  // </group>
  
} //# NAMESPACE CASA - END

#endif
