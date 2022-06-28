//# TableParseFunc.h: Class handling TaQL functions
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

#ifndef TABLES_TABLEPARSEFUNC_H
#define TABLES_TABLEPARSEFUNC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprFuncNode.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations
  class TableParseQuery;

  
  // <summary>
  // Class containing static functions to handle TaQL functions.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis>
  // This class contains a few functions to recognize TaQL functions.
  // A function name can be preceded by "shorthand.udflib." telling the
  // table to operate on and the UDF shared library containing the function.
  // Note that most functions are table agnostic, but a few (such as rowid())
  // are not. Also User Defined Functions (UDFs) can be table aware.
  // Both shorthand and udflib are optional. If a single part is given, it is
  // first tried as a shorthand.
  //
  // It looks up the function name and creates a TableExprFuncNode object for it.
  // If the function name is not standard, it is tried to create a TableExprUDFNode
  // object. Otherwise an exception is thrown.
  // </synopsis>

  class TableParseFunc
  {
  public:
    // Make a function node.
    // The name is split into shorthand, udflib and function as explained
    // in the synopsis.
    static TableExprNode makeFuncNode (TableParseQuery*,
                                       const String& name,
                                       const TableExprNodeSet& arguments,
                                       const Vector<int>& ignoreFuncs,
                                       const Table& table,
                                       const TaQLStyle&);

    // Try to make a UDF function node for the given function name and arguments.
    // The function name can contain "shorthand." and must contain "udflib.".
    static TableExprNode makeUDFNode (TableParseQuery*,
                                      const String& name,
                                      const TableExprNodeSet& arguments,
                                      const Table& table,
                                      const TaQLStyle&);

    // Find the function code belonging to a function name.
    // Functions to be ignored can be given (as function type values).
    // If the function name is unknown, NRFUNC is returned.
    static TableExprFuncNode::FunctionType findFunc (const String& name,
                                                     uInt narguments,
                                                     const Vector<Int>& ignoreFuncs);
  };


} //# NAMESPACE CASACORE - END

#endif
