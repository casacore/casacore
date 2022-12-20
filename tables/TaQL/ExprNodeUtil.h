//# ExprNodeUtil.h: Utility functions for TableExprNodeRep objects
//# Copyright (C) 2022
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

#ifndef TABLES_EXPRNODEUTIL_H
#define TABLES_EXPRNODEUTIL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to handle a Regex or StringDistance.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Regex>Regex</linkto>
//   <li> <linkto class=StringDistance>StringDistance</linkto>
// </prerequisite>

// <synopsis> 
// A StringDistance (Levensthein distance) in TaQL is given in the same way
// as a Regex. This class is needed to have a single object in the parse tree
// objects containing them (in class TableExprNodeConstRegex).
// </synopsis> 

  namespace TableExprNodeUtil
  {
    // Throw an exception if an aggregate function is used in
    // the expression node or its children.
    void checkAggrFuncs (TableExprNodeRep* node);

    // Get the aggregate function nodes used in the node and its children.
    std::vector<TableExprNodeRep*> getAggrNodes (TableExprNodeRep* node);

    // Get the column nodes used in the node and its children.
    std::vector<TableExprNodeRep*> getColumnNodes (TableExprNodeRep* node);

    // Get the (unique) tables used in the node and its children.
    // If <src>properMain</src> only proper main tables (i.e., tables
    // specified in the FROM clause) are returned.
    std::vector<Table> getNodeTables (TableExprNodeRep* node,
                                      Bool properMain);

    // Get the nr of rows in the tables used.
    // An exception is thrown if the tables differ in the nr of rows.
    rownr_t getCheckNRow (const std::vector<Table>&);
}
  

} //# NAMESPACE CASACORE - END

#endif
