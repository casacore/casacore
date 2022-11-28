//# TableParseTableList.cc: Lists of tables used in a TaQL query
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
//#
//# $Id$

//# Includes
#include <casacore/tables/TaQL/TableParseUtil.h>
#include <casacore/tables/TaQL/TableParseQuery.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  TableParsePair::TableParsePair (const Table& table, Int tabnr,
                                  const String& name, const String& shorthand,
                                  Int joinIndex)
    : tabnr_p     (tabnr),
      joinIndex_p (joinIndex),
      name_p      (name),
      shorthand_p (shorthand),
      table_p     (table)
  {}

  TableExprInfo TableParsePair::getTableInfo() const
  {
    return TableExprInfo (table_p, shorthand_p, joinIndex_p>=0);
  }

  
  //# Construct a TableParse object and add it to the container.
  Table TableParseTableList::addTable (Int tabnr, const String& name,
                                       const Table& ftab,
                                       const String& shorthand,
                                       Bool addToFromList,
                                       const std::vector<const Table*>& tempTables,
                                       const std::vector<TableParseQuery*>& stack,
                                       Int joinIndex)
  {
    Table table = TableParseUtil::getTable (tabnr, name, ftab,
                                            tempTables, stack);
    // Check that a shorthand is used only once, except for an empty one.
    // Don't take the WITH tables into account, otherwise it will complain
    // about a WITH shorthand used in a FROM.
    if (! shorthand.empty()) {
      TableParsePair tablePair = findTable (shorthand, False);
      if (! tablePair.table().isNull()) {
        throw TableInvExpr("Shorthand '" + shorthand + "' has already been used");
      }
    }
    if (addToFromList) {
      itsFromTables.push_back (TableParsePair(table, tabnr, name, shorthand, joinIndex));
    } else {
      itsWithTables.push_back (TableParsePair(table, tabnr, name, shorthand));
    }
    return table;
  }

  void TableParseTableList::replaceTable (const Table& table)
  {
    AlwaysAssert (!itsFromTables.empty(), AipsError);
    itsFromTables[0].replaceTable (table);
  }

  TableParsePair TableParseTableList::findTable (const String& shorthand, Bool doWith,
                                                 const std::vector<TableParseQuery*>& stack)
  {
    TableParsePair tab;
    for (Int i=stack.size()-1; i>=0; i--) {
      tab = stack[i]->tableList().findTable (shorthand, doWith);
      if (! tab.table().isNull()) {
        break;
      }
    }
    return tab;
  }

  TableParsePair TableParseTableList::findTable (const String& shorthand,
                                                 Bool doWith) const
  {
    //# If no shorthand given, first table is taken (if there).
    for (uInt i=0; i<itsFromTables.size(); i++) {
      if (itsFromTables[i].test (shorthand)) {
        return itsFromTables[i];
      }
    }
    if (doWith) {
      for (uInt i=0; i<itsWithTables.size(); i++) {
        if (itsWithTables[i].test (shorthand)) {
          return itsWithTables[i];
        }
      }
    }
    return TableParsePair();
  }

  
} //# NAMESPACE CASACORE - END
