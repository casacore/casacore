//# TableParse.cc: Functions to parse and execute a TaQL command
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
//# $Id: TableParse.cc 21399 2013-11-12 07:55:35Z gervandiepen $

#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/TaQL/TaQLNodeHandler.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Timer.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


//# Simplified forms of general tableCommand function.
TaQLResult tableCommand (const String& str)
{
  Vector<String> cols;
  return tableCommand (str, cols);
}
TaQLResult tableCommand (const String& str, const Table& tempTable)
{
  std::vector<const Table*> tmp(1);
  tmp[0] = &tempTable;
  return tableCommand (str, tmp);
}
TaQLResult tableCommand (const String& str,
                         const std::vector<const Table*>& tempTables)
{
  Vector<String> cols;
  return tableCommand (str, tempTables, cols);
}
TaQLResult tableCommand (const String& str, Vector<String>& cols)
{
  std::vector<const Table*> tmp;
  return tableCommand (str, tmp, cols);
}

TaQLResult tableCommand (const String& str,
                         Vector<String>& cols,
                         String& commandType)
{
  std::vector<const Table*> tmp;
  return tableCommand (str, tmp, cols, commandType);
}

TaQLResult tableCommand (const String& str,
                         const std::vector<const Table*>& tempTables,
                         Vector<String>& cols)
{
  String commandType;
  return tableCommand (str, tempTables, cols, commandType);
}

//# Do the actual parsing of a command and execute it.
TaQLResult tableCommand (const String& str,
                         const std::vector<const Table*>& tempTables,
                         Vector<String>& cols,
                         String& commandType)
{
  commandType = "error";
  // Do the first parse step. It returns a raw parse tree
  // (or throws an exception).
  Timer timer;
  TaQLNode tree = TaQLNode::parse(str);
  // Now process the raw tree and get the final ParseSelect object.
  try {
    TaQLNodeHandler treeHandler;
    TaQLNodeResult res = treeHandler.handleTree (tree, tempTables);
    const TaQLNodeHRValue& hrval = TaQLNodeHandler::getHR(res);
    commandType = hrval.getString();
    TableExprNode expr = hrval.getExpr();
    if (tree.style().doTiming()) {
      timer.show (" Total time   ");
    }
    if (! expr.isNull()) {
      return TaQLResult(expr);                 // result of CALC command
    }
    //# Copy the possibly selected column names.
    cols.reference (hrval.getNames());
    return TaQLResult(hrval.getTable());
  } catch (std::exception& x) {
    throw TableParseError ("'" + str + "'\n  " + x.what());
  }
}

} //# NAMESPACE CASACORE - END
