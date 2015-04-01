//# TaQLResult.h: Class to hold the result of a TaQL command
//# Copyright (C) 2004
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

#ifndef TABLES_TAQLRESULT_H
#define TABLES_TAQLRESULT_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/TaQL/ExprNode.h>

namespace casacore {

// <summary>
// Class to hold the result of a TaQL command.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <synopsis> 
// The result of a TaQL command can be a Table or a TableExprNode.
// This class holds the actual result.
// </synopsis> 

// <motivation>
// It is possible to give a TaQL command resulting in a TableExprNode
// to make it possible to make expressions of columns.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TaQLResult
{
public:
  // Construct from a Table.
  TaQLResult (const Table& = Table());

  // Construct from a TableExprNode.
  explicit TaQLResult (const TableExprNode&);

  // Is the result a Table?
  Bool isTable() const
    { return itsNode.isNull(); }

  // Return the result as a Table.
  // It throws an exception if it is not a Table.
  Table table() const;

  // Make it possible to convert automatically to a Table
  //# (for backwards compatibility).
  operator Table() const
    { return table(); }

  // Return the result as a TableExprNode.
  // It throws an exception if it is not a TableExprNode.
  TableExprNode node() const;

private:
  Table itsTable;
  TableExprNode itsNode;
};

}
#endif
