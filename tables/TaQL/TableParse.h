//# TableParse.h: Functions to parse and execute a TaQL command
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_TABLEPARSE_H
#define TABLES_TABLEPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TaQLResult.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class Table;
  class String;


  // <synopsis>
  // Parse and execute the given TaQL command.
  // It will open (and close) all tables needed.
  // It returns the resulting table.
  // The command type and the selected or updated
  // column names can be returned.
  // Zero or more temporary tables can be used in the command
  // using the $nnn syntax.
  // </synopsis>
  // <group name=tableCommand>
  TaQLResult tableCommand (const String& command);

  TaQLResult tableCommand (const String& command,
                           const Table& tempTable);
  TaQLResult tableCommand (const String& command,
                           const std::vector<const Table*>& tempTables);
  TaQLResult tableCommand (const String& command,
                           Vector<String>& columnNames);
  TaQLResult tableCommand (const String& command,
                           Vector<String>& columnNames,
                           String& commandType);
  TaQLResult tableCommand (const String& command,
                           const std::vector<const Table*>& tempTables,
                           Vector<String>& columnNames);
  TaQLResult tableCommand (const String& command,
                           const std::vector<const Table*>& tempTables,
                           Vector<String>& columnNames,
                           String& commandType);
  // </group>


} //# NAMESPACE CASACORE - END

#endif
