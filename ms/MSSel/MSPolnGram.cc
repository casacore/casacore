//# MSPolnGram.cc: Grammar for polarization selection expressions
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

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSPolnGram.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MeasurementSets/MSPolarization.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>

#include <casacore/tables/TaQL/TableParse.h> 
#include <casacore/tables/Tables/TableError.h>
#include <casacore/ms/MSSel/MSSpwGram.h>
#include <casacore/ms/MSSel/MSPolnParse.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/casa/Containers/MapIO.h>

// Define the yywrap function for flex.
int MSPolnGramwrap()
{
  return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //# Declare a file global pointer to a char* for the input string.
  /* static const char*           strpMSPolnGram = 0; */
  static Int                   posMSPolnGram = 0;
  
  // MSPolnGramwrap out of namespace
  //------------------------------------------------------------------------------
  //  
  //# Parse the command.
  //# Do a yyrestart(yyin) first to make the flex scanner reentrant.

  //
  //------------------------------------------------------------------------------
  //  
  int msPolnGramParseCommand (const MeasurementSet* ms, 
			      const String& command,
			      TableExprNode& node,
			      Vector<Int>& selectedDDIDs,
			      OrderedMap<Int, Vector<Int> >& selectedPolnMap,
			      OrderedMap<Int, Vector<Vector<Int> > >& selectedSetupMap) 
  {
    try 
      {
	Int ret;
	MSPolnParse parser(ms);
	parser.reset();
	// parse command string
	ret=parser.theParser(command);
	//	node=(*(parser.node()));
	node=((parser.node()));
	selectedDDIDs = parser.selectedDDIDs();
	selectedPolnMap = parser.selectedPolnMap();
	selectedSetupMap = parser.selectedSetupMap();
	return ret;
      }
    catch (MSSelectionPolnError &x)
      {
	String newMesgs;
	newMesgs = constructMessage(msPolnGramPosition(), command);
	x.addMessage(newMesgs);
	MSPolnGramerror((char *)(x.getMesg().c_str()));
	throw;
      }
  }
  //
  //------------------------------------------------------------------------------
  //  
  //# Give the table expression node
  const TableExprNode* msPolnGramParseNode()
  {
    //   return MSPolnParse::node();
    return NULL;
  }
  //
  //------------------------------------------------------------------------------
  //  
  void msPolnGramParseDeleteNode()
  {
    //    MSPolnParse::cleanup();
  }
  //
  //------------------------------------------------------------------------------
  //  
  //# Give the string position.
  int& msPolnGramPosition()
  {
    return posMSPolnGram;
  }
  //
  //------------------------------------------------------------------------------
  //  
  void MSPolnGramerror (char* msg)
  {
    throw(MSSelectionPolnParseError(String("Poln. expression error: ")+msg));

//     throw (MSSelectionPolnParseError("Poln Expression: Parse error at or near '" +
// 				     String(MSPolnGramtext) + "'"));
  }
  
} //# NAMESPACE CASACORE - END
