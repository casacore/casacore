//# TableTrace.cc: Class with static functions for tracing column IO
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
//# $Id: BaseColumn.cc 21130 2011-10-18 07:39:05Z gervandiepen $

#include <casacore/tables/Tables/TableTrace.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/OS/Path.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Define the statics.
  CallOnce0 TableTrace::theirCallOnce;
  Mutex TableTrace::theirMutex;
  std::ofstream TableTrace::theirTraceFile;
  std::ostream* TableTrace::theirStream = 0;
  int TableTrace::theirDoTrace = 0;
  int TableTrace::theirOper = 0;
  int TableTrace::theirColType = 0;
  std::vector<Regex> TableTrace::theirColumns;
  std::vector<String> TableTrace::theirTables;

  int TableTrace::traceTable (const String& tableName, char oper)
  {
    // Open trace file if not done yet.
    theirCallOnce(initTracing);
    int tabid = -1;
    if (theirDoTrace > 0) {
      ScopedMutexLock locker(theirMutex);
      // Table should not be found, but who knows ...
      tabid = findTable (tableName);
      int id = tabid;
      if (tabid < 0) {
        // Find a free table entry. If none, append.
        tabid = findTable (String());
        if (tabid < 0) {
          tabid = theirTables.size();
          theirTables.push_back (tableName);
        } else {
          theirTables[tabid] = tableName;
        }
      }
      writeTraceFirst (tabid, tableName, oper);
      if (id >= 0) {
        *theirStream << "**ERROR** table already in use";
      }
      *theirStream << endl;
    }
    return tabid;
  }

  void TableTrace::traceClose (const String& tableName)
  {
    theirCallOnce(initTracing);
    if (theirDoTrace > 0) {
      ScopedMutexLock locker(theirMutex);
      int tabid = findTable (tableName);
      writeTraceFirst (tabid, tableName, 'c');
      if (tabid < 0) {
        *theirStream << "**ERROR** unknown table";
      } else {
        // Free entry.
        theirTables[tabid] = String();
      }
      *theirStream << endl;
    }
  }

  void TableTrace::traceFile (int tabid, const String& oper)
  {
    theirCallOnce(initTracing);
    if (theirDoTrace > 0) {
      writeTraceFirst (tabid, '*'+oper+'*', 't');
      *theirStream << endl;
    }
  }

  void TableTrace::traceRefTable (const String& parentName, char oper)
  {
    theirCallOnce(initTracing);
    if (theirDoTrace > 1) {
      int tabid = findTable (parentName);
      writeTraceFirst (tabid, "*reftable*", oper);
      *theirStream << endl;
    }
  }

  int TableTrace::traceColumn (const ColumnDesc& cd)
  {
    theirCallOnce(initTracing);
    int traceCol = 0;
    if (theirOper > 0) {
      // First test if all scalar, array, or record columns are traced.
      if ((cd.isScalar()  &&  (theirColType&SCALAR) != 0)  ||
          (cd.isArray()   &&  (theirColType&ARRAY) != 0)  ||
          ((theirColType&RECORD) != 0)) {
        traceCol = theirOper;
      } else {
        // Otherwise see if this column is traced.
        for (size_t i=0; i<theirColumns.size(); ++i) {
          if (cd.name().matches (theirColumns[i])) {
            traceCol = theirOper;
            break;
          }
        }
      }
    }
    return traceCol;
  }

  int TableTrace::findTable (const String& name)
  {
    for (size_t i=0; i<theirTables.size(); ++i) {
      if (theirTables[i] == name) {
        return i;
      }
    }
    return -1;
  }

  void TableTrace::writeTraceFirst (int tabid, const String& name, char oper)
  {
    // Get current time.
    MVTime time((Time()));
    time.print (*theirStream, MVTime::Format(MVTime::TIME, 9));
    *theirStream << ' ' << oper << " t=" << tabid << ' ' << name << ' ';
  }

  void TableTrace::writeRefRows (const RefRows& rownrs)
  {
    const Vector<rownr_t> rows = rownrs.rowVector();
    for (rownr_t i=0; i<rows.size(); ++i) {
      if (i>0) *theirStream << ',';
      *theirStream << rows[i];
      if (rownrs.isSliced()) {
        if (rows[i+1] >= rows[i] + rows[i+2]) {
          *theirStream << ':' << rows[i+1];
          if (rows[i+2] > 1) {
            *theirStream << ':' << rows[i+2];
          }
        }
        i += 2;
      }
    }
  }

  void TableTrace::writeSlice (const IPosition& blc,
                               const IPosition& trc,
                               const IPosition& inc)
  {
    *theirStream << ' ';
    // Use showContainer instead of operator<< to avoid spaces.
    showContainer (*theirStream, blc);
    showContainer (*theirStream, trc);
    showContainer (*theirStream, inc);
  }

  void TableTrace::trace (int tabid, const String& columnName, char oper)
  {
    writeTraceFirst (tabid, columnName, oper);
    *theirStream << '*' << endl;
  }
  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          Int64 row)
  {
    writeTraceFirst (tabid, columnName, oper);
    *theirStream << row << endl;
  }
  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          const RefRows& rownrs)
  {
    writeTraceFirst (tabid, columnName, oper);
    writeRefRows (rownrs);
    *theirStream << endl;
  }

  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          const IPosition& shape)
  {
    writeTraceFirst (tabid, columnName, oper);
    *theirStream << "* ";
    showContainer (*theirStream, shape);
    *theirStream << endl;
  }
  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          Int64 row, const IPosition& shape)
  {
    writeTraceFirst (tabid, columnName, oper);
    *theirStream << row << ' ';
    showContainer (*theirStream, shape);
    *theirStream << endl;
  }
  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          const RefRows& rownrs, const IPosition& shape)
  {
    writeTraceFirst (tabid, columnName, oper);
    writeRefRows (rownrs);
    *theirStream << ' ';
    showContainer (*theirStream, shape);
    *theirStream << endl;
  }

  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          const IPosition& shape,
                          const IPosition& blc, const IPosition& trc,
                          const IPosition& inc)
  {
    writeTraceFirst (tabid, columnName, oper);
    *theirStream << "* ";
    showContainer (*theirStream, shape);
    writeSlice (blc, trc, inc);
    *theirStream << endl;
  }
  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          Int64 row, const IPosition& shape,
                          const IPosition& blc, const IPosition& trc,
                          const IPosition& inc)
  {
    writeTraceFirst (tabid, columnName, oper);
    *theirStream << row << ' ';
    showContainer (*theirStream, shape);
    writeSlice (blc, trc, inc);
    *theirStream << endl;
  }
  void TableTrace::trace (int tabid, const String& columnName, char oper,
                          const RefRows& rownrs, const IPosition& shape,
                          const IPosition& blc, const IPosition& trc,
                          const IPosition& inc)
  {
    writeTraceFirst (tabid, columnName, oper);
    writeRefRows (rownrs);
    *theirStream << ' ';
    showContainer (*theirStream, shape);
    writeSlice (blc, trc, inc);
    *theirStream << endl;
  }

  void TableTrace::initTracing()
  {
    // Set initially to no tracing.
    theirDoTrace = -1;
    // Get the file name.
    String fname;
    AipsrcValue<String>::find (fname, "table.trace.filename", "");
    if (! fname.empty()) {
      if (fname == "stdout") {
        theirStream = &std::cout;
      } else if (fname == "stderr") {
        theirStream = &std::cerr;
      } else {
        String expName = Path(fname).expandedName();
        theirTraceFile.open (fname.c_str());
        if (! theirTraceFile) {
          throw TableError ("Could not open table column trace file " + fname);
        }
        theirStream = &theirTraceFile;
      }
      *theirStream << "# time oper tabid name row(s) shape blc/trc/inc"
                     << endl;
      *theirStream << "# Note: shapes are in Fortran order" << endl << endl;
      theirDoTrace = 1;
      initOper();
      initColumn();
    }
  }

  void TableTrace::initOper()
  {
    // Get the operations to trace.
    String operStr;
    AipsrcValue<String>::find (operStr, "table.trace.operation", "");
    if (! operStr.empty()) {
      operStr.downcase();
      for (uInt i=0; i<operStr.size(); ++i) {
        if (operStr[i] == 's') {
          theirDoTrace |= 2;
        } else if (operStr[i] == 'r') {
          theirOper |= READ;
        } else if (operStr[i] == 'w') {
          theirOper |= WRITE;
        }
      }
    }
  }

  void TableTrace::initColumn()
  {
    // Get the patterns telling which columns to trace.
    String typeStr;
    AipsrcValue<String>::find (typeStr, "table.trace.columntype", "");
    String colStr;
    AipsrcValue<String>::find (colStr, "table.trace.column", "");
    if (! typeStr.empty()) {
      typeStr.downcase();
      for (uInt i=0; i<typeStr.size(); ++i) {
        if (typeStr[i] == 's') {
          theirColType |= SCALAR;
        } else if (typeStr[i] == 'a') {
          theirColType |= ARRAY;
        } else if (typeStr[i] == 'r') {
          theirColType |= RECORD;
        }
      }
    } else if (colStr.empty()) {
      // If nothing specified, default is array columns.
      theirColType = 2;
    }
    Vector<String> cols = stringToVector (colStr, ',');
    theirColumns.reserve (cols.size());
    for (uInt i=0; i<cols.size(); ++i) {
      if (! cols[i].empty()) {
        theirColumns.push_back (Regex(Regex::fromPattern(cols[i])));
      }
    }
  }
  
} // end namespace
