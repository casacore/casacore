//# MSSelection.h: Class to represent a selection on an MS
//# Copyright (C) 1996,1997,1998,1999,2001
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#ifndef MS_MSSELECTION_H
#define MS_MSSELECTION_H

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MRadialVelocity.h>
#include <tables/Tables/ExprNode.h>
#include <ms/MeasurementSets/MeasurementSet.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// MSSelection: Class to represent a selection on an MS
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MeasurementSet">MeasurementSet</linkto> module
// </prerequisite>
//
// <etymology>
// From "MeasurementSet" and "selection".
// </etymology>
//
// <synopsis>
// The MSSelection class represents a selection on a MeasurementSet (MS).
// This class is used in translating MS selections represented as
// selection items in the user interface, and for converting between
// MS selection and pure TaQL selection.
// </etymology>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// This class is used by the MS access classes.
// </motivation>
//
// <todo asof="01/03/01">
// </todo>

class MSSelection
{
 public:
   enum MSExprType {NO_EXPR = 0,
                    ANTENNA_EXPR,
                    CORR_EXPR,
                    FIELD_EXPR,
                    SPW_EXPR,
                    TIME_EXPR,
                    UVDIST_EXPR,
                    MAX_EXPR = UVDIST_EXPR};

   // Default null constructor, and destructor
   MSSelection();
   virtual ~MSSelection();

   // Construct from a record representing a selection item
   // at the CLI or user interface level.
   MSSelection(const Record& selectionItem);

   // Copy constructor
   MSSelection(const MSSelection& other);

   // Assignment operator
   MSSelection& operator=(const MSSelection& other);

   // Helper method for converting index vectors to expression strings
   static String indexExprStr(Vector<Int> index);

   // Expression accessors
   Bool setAntennaExpr(const String& antennaExpr);
   Bool setCorrExpr(const String& corrExpr);
   Bool setFieldExpr(const String& fieldExpr);
   Bool setSpwExpr(const String& spwExpr);
   Bool setTimeExpr(const String& timeExpr);
   Bool setUvDistExpr(const String& uvDistExpr);

   // Clear all subexpression and reset priority
   void clear(void);

   // Convert to TableExprNode format (C++ interface to TaQL)
   TableExprNode toTableExprNode(const MeasurementSet& ms);

 private:
   // Set into the order of the selection expression
   Bool setOrder(MSSelection::MSExprType type);

   // Initialize from a Record representing a selection
   // item from the user interface or CLI
   void fromSelectionItem(const Record& selectionItem);

   // Check if record field exists and is not unset
   Bool definedAndSet(const Record& inpRec, const String& fieldName);

   // Convert an MS select string to TaQL
   //   const String msToTaQL(const String& msSelect) {};

   // Selection expressions
   String antennaExpr_p;
   String corrExpr_p;
   String fieldExpr_p;
   String spwExpr_p;
   String timeExpr_p;
   String uvDistExpr_p;

   // Priority
   Vector<Int> exprOrder_p;
};

} //# NAMESPACE CASA - END

#endif
   

