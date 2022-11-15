//# TableParseSortKey.h: Class to manage a key in a TaQL SORT command
//# Copyright (C) 1994-2022
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

#ifndef TABLES_TABLEPARSESORTKEY_H
#define TABLES_TABLEPARSESORTKEY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Utilities/Sort.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Helper class for sort keys in TableParse
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis> 
  // An object of this class is used to hold the sort expression
  // and sort order of a single sort key.
  // </synopsis> 


  class TableParseSortKey
  {
  public:
    // Construct from a given expression.
    // The order is not given.
    TableParseSortKey();

    // Construct from a given expression.
    // The order is not given.
    explicit TableParseSortKey (const TableExprNode&);

    // Construct from a given expression and order.
    TableParseSortKey (const TableExprNode&, Sort::Order);

    // Get the expression node.
    const TableExprNode& node() const
      { return node_p; }

    // Get the sort order.
    Sort::Order order() const
      { return order_p; }

    // Is the order given?
    Bool orderGiven() const
      { return given_p; }

    // Add the values of the sort key to the Sort object by reading the
    // values of the expression into an array.
    // The array has to be deletedlater  after the full sort is done.
    std::shared_ptr<ArrayBase> addSortValues (Sort& sort,
                                              Sort::Order mainOrder,
                                              const Vector<rownr_t>&) const;

  private:
    // Check if the node results in a scalar and does not contain
    // aggregate functions.
    void checkNode() const;

    TableExprNode node_p;
    Sort::Order   order_p;
    Bool          given_p;
  };


} //# NAMESPACE CASACORE - END

#endif
