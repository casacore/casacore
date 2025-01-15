//# TaQLResult.cc: Class to hold the result of a TaQL command
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/tables/TaQL/TaQLResult.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore {

TaQLResult::TaQLResult (const Table& table)
: itsTable (table)
{}

TaQLResult::TaQLResult (const TableExprNode& node)
  : itsNode  (node)
{}

const Table& TaQLResult::table() const
{
  AlwaysAssert (isTable(), AipsError);
  return itsTable;
}

TableExprNode TaQLResult::node() const
{
  AlwaysAssert (!isTable(), AipsError);
  return itsNode;
}
 
} //#NAMESPACE CASACORE - END
