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
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/ms/MSSel/MSDataDescIndex.h>
#include <casacore/ms/MSSel/MSPolIndex.h>
#include <casacore/casa/Containers/OrderedMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //# Forward Declarations
  class MeasurementSet;
  class TableExprNode;
  
  // <summary>
  // Global functions to drive the MSPolnParse class.  These, for
  // Polarization selection, need not be global functions, but are
  // done this way to keep the interface uniform for the various
  // selection expressions.
  // </summary>
  
  // <use visibility=local>
  
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>
  
  // <prerequisite>
  //# Classes you should understand before using this one.
  // </prerequisite>
  
  // <synopsis> 
  // </synopsis> 
  
  // <motivation>
  // </motivation>
  // <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  // </todo>
  
  
  // <group name=MSPolnGramFunctions>
  
  // The top level interface to the parser.
  int msPolnGramParseCommand (const MeasurementSet *ms, const String& command,
			      TableExprNode& node,
			      Vector<Int>& selectedDDIDs, 
			      OrderedMap<Int, Vector<Int> >& selectedPolnMap,
			      OrderedMap<Int, Vector<Vector<Int> > >& selectedSetupMap
			      );
  
  // The error handler.
  // It throws an exception with the current token.
  void MSPolnGramerror (char*);
  
  // Give the table expression node.
  const TableExprNode *msPolnGramParseNode();
  void msPolnGramParseDeleteNode();
  
  // Give the current position in the string.
  // This can be used when parse errors occur.
  int& msPolnGramPosition();
  
  // </group>
  
} //# NAMESPACE CASACORE - END

#endif
