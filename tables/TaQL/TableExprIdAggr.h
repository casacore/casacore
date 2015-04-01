//# TableExprIdAggr.h: The Table Expression Selection id used with aggregation
//# Copyright (C) 2013
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
//#
//# $Id: TableExprId.h 21403 2013-12-03 20:51:02Z gervandiepen $


#ifndef TABLES_TABLEEXPRIDAGGR_H
#define TABLES_TABLEEXPRIDAGGR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TableExprId.h>
#include <casacore/tables/TaQL/ExprGroup.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/stdvector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // The Table Expression Selection id used with aggregation
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tExprGroup">
  // </reviewed>

  // <synopsis>
  // This class provides the user the ability to identify the data objects
  // to test in a TaQL expression using aggregate functions.
  // <br>
  // It adds a TableExprGroupResult object to the TableExprId class which
  // is used by the various classes derived from TableExprGroupFuncBase
  // to get or evaluate the aggregated value.
  // An aggregated value can be determined in two ways:
  // <ul>
  //  <li> An immediate aggregate function has calculated its value in its
  //       <src>apply</src> function.
  //       Each group has a TableExprGroupFuncSet containing the values
  //       of all immediate aggregate functions of that group.
  //       A vector of these objects is part of of TableExprGroupResult
  //       and is used by the TableExprAggrNode(Array) <src>get</src> functions
  //       to return the correct value.
  //  <li> A lazy aggregate function calculates the value as part of its
  //       <src>get</src> function. It uses the vector of TableExprId objects
  //       in TableExprGroupResult to know which rows (or records) belong to
  //       which group. Note that UDF aggregate functions are always lazy.
  // </ul>
  // TableExprIdAggr contains a static function to (statically) cast a
  // TableExprId object to TableExprIdAggr. A magic value is used to check
  // that the cast is valid.
  // <br>No dynamic_cast is used, because it requires TableExprId to contain
  // virtual functions making the object larger. TaQL can hold quite large
  // vectors of TableExprId objects, so such overhead is unacceptably large.
  // </synopsis>

  // <motivation>
  // This class makes it possible to retrieve aggregated values using the
  // standard <src>get</src> functions taking an TableExprId.
  // </motivation>

  class TableExprIdAggr: public TableExprId
  {
  public:
    // Construct it from the aggregation results.
    explicit TableExprIdAggr (const CountedPtr<TableExprGroupResult>& result)
      : itsMagicValue (0xabababab),
        itsResult     (result)
    {}

    // Get out the aggregation result object.
    const TableExprGroupResult& result() const
      { return *itsResult; }

    // Get the magic value (to check if it correct).
    uInt getMagicValue() const
      { return itsMagicValue; }

    // Cast a TableExprId object to TableExprIdAggr. It check if the cast
    // if correct by checking the magic value.
    static const TableExprIdAggr& cast (const TableExprId& id)
    {
      const TableExprIdAggr& idAggr = reinterpret_cast<const TableExprIdAggr&>(id);
      AlwaysAssert (idAggr.getMagicValue() == 0xabababab, AipsError);
      return idAggr;
    }

  private:
    uInt itsMagicValue;
    CountedPtr<TableExprGroupResult> itsResult;
  };


} //# NAMESPACE CASACORE - END

#endif
