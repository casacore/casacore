//# TableTrace.h: Class with static functions for tracing column IO
//# Copyright (C) 2014
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
//# $Id: BaseColumn.h 21130 2011-10-18 07:39:05Z gervandiepen $

#ifndef TABLES_COLUMNTRACE_H
#define TABLES_COLUMNTRACE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Regex.h>
#include <ostream>
#include <fstream>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations.
class ColumnDesc;
class RefRows;
class IPosition;


// <summary>
// Class with static functions for tracing column IO
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis> 
// This class contains some static functions to enable table and column tracing.
// It maintains a map of table name to table-id.
// <br>
// The following aipsrc variables variables determine if tracing will be done,
// and if so, which columns and operations will be traced.
// <ul>
//  <li> <src>table.trace.filename</src> gives the name of the file in which
//       the trace will be written. If empty (default), no tracing will be done.
//  <li> <src>table.trace.operation</src> gives the operation to trace.
//       be traced. It can be one or more of:
//       <br>s: creation of RefTable (selection/sort/iter)
//       <br>r: reads
//       <br>w: writes
//       <br>The default is ''. Note that opening and closing a PlainTable
//       are always traced.
//  <li> <src>table.trace.columntype</src> gives the types of columns to trace
//       for read and/or write.
//       It can be one or more of:
//       <br> s: scalar columns
//       <br> a: array columns
//       <br> r: record columns
//       <br>The default is ''.
//  <li> <src>table.trace.column</src> gives names of additional columns to
//       trace for read and/or write.
//       The names are separated by commas without any whitespace.
//       Each name can be a glob-like pattern.
//       <br>The default is ''.
// </ul>
// If both <src>table.trace.columntype</src> and <src>table.trace.column</src>
// have an empty value, all array columns are traced.

class TableTrace
{
public:
  enum ColType {
    SCALAR = 1,
    ARRAY  = 2,
    RECORD = 4
  };
  enum Oper {
    READ  = 1,
    WRITE = 2
  };

  // Does the given column have to be traced for read and/or write?
  // bit 0 set means read tracing; bit 1 write tracing.
  static int traceColumn (const ColumnDesc&);

  // If needed, write a trace message for table open or create.
  // It adds the table to the map and returns the table-id.
  static int traceTable (const String& tableName, char oper);

  // If needed, trace closing a table.
  // It removes the table from the map.
  static void traceClose (const String& tableName);

  // If needed, trace an operation on a table.
  static void traceFile (int tabid, const String& oper);

  // If needed, write a trace message for reftable open, create, or close.
  static void traceRefTable (const String& parentName, char oper);

  // If needed, write a trace message 
  // Write a trace message for a scalar column.
  static void trace (int tabid, const String& columnName, char oper);
  // Write a trace message for a scalar row.
  static void trace (int tabid, const String& columnName, char oper,
                     Int64 row);
  // Write a trace message for ranges of scalar rows.
  static void trace (int tabid, const String& columnName, char oper,
                     const RefRows& rownrs);
  // Write a trace message for an array column.
  static void trace (int tabid, const String& columnName, char oper,
                     const IPosition& shape);
  // Write a trace message for an array row.
  static void trace (int tabid, const String& columnName, char oper,
                     Int64 row, const IPosition& shape);
  // Write a trace message for ranges of array rows.
  static void trace (int tabid, const String& columnName, char oper,
                     const RefRows& rownrs, const IPosition& shape);
  // Write a trace message for an array column slice.
  static void trace (int tabid, const String& columnName, char oper,
                     const IPosition& shape,
                     const IPosition& blc, const IPosition& trc,
                     const IPosition& inc);
  // Write a trace message for an array row slice.
  static void trace (int tabid, const String& columnName, char oper,
                     Int64 row, const IPosition& shape,
                     const IPosition& blc, const IPosition& trc,
                     const IPosition& inc);
  // Write a trace message for ranges of array rows slice.
  static void trace (int tabid, const String& columnName, char oper,
                     const RefRows& rownrs, const IPosition& shape,
                     const IPosition& blc, const IPosition& trc,
                     const IPosition& inc);

private:
  // Initialize the tracing mechanism which should be done only once.
  static void initTracing();
  static void initOper();
  static void initColumn();

  // Find the table name in the vector. -1 is returned if not found.
  static int findTable (const String& name);

  // Write the first part of the trace message.
  static void writeTraceFirst (int tabid, const String& name, char oper);

  // Write the RefRows as vector of rows or slices.
  static void writeRefRows (const RefRows& rownrs);

  // Write the blc, trc, and inc of an array slice.
  static void writeSlice (const IPosition& blc,
                          const IPosition& trc,
                          const IPosition& inc);

  //# Data members
  static std::ofstream       theirTraceFile;
  static int                 theirDoTrace;   //# 0=init -1=no 1=yes 2=reftable
  static int                 theirOper;      //# 1=rtrace 2=wtrace
  static int                 theirColType;   //# 1=scalar 2=array 4=record
  static std::vector<Regex>  theirColumns;
  static std::vector<String> theirTables;
};




} //# NAMESPACE CASACORE - END

#endif
