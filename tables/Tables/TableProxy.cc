//# TableProxy.cc: High-level interface to tables
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004
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


#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/ReadAsciiTable.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableCache.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdio.h>                  // needed for sprintf

namespace casacore { //# NAMESPACE CASACORE - BEGIN


TableProxy::TableProxy()
{}

TableProxy::TableProxy (const String& tableName,
			const Record& lockOptions,
			int option)
{
  table_p = TableUtil::openTable (tableName, makeLockOptions(lockOptions),
                                  Table::TableOption(option));
}

TableProxy::TableProxy (const String& tableName,
			const Record& lockOptions,
			const String& endianFormat,
			const String& memType,
			int64_t nrow,
			const Record& tableDesc,
			const Record& dmInfo)
{
  // Interpret the endian option.
  Table::EndianFormat endOpt = makeEndianFormat (endianFormat);
  // Get the type.
  Table::TableType type = Table::Plain;
  if (memType == "memory") {
    type = Table::Memory;
  }
  // Get nr of rows.
  if (nrow < 0) {
    nrow = 0;
  }
  
  TableDesc tabdesc;
  String message;
  if (!makeTableDesc (tableDesc, tabdesc, message)) {
    throw TableError (tableName + " failed: " + message);
  }
  // Try to create the table (scratch if no table name given).
  table_p = TableUtil::createTable (tableName, tabdesc, Table::New,
                                    type, StorageOption(), dmInfo,
                                    makeLockOptions(lockOptions),
                                    nrow, false, endOpt);
}

TableProxy::TableProxy (const Vector<String>& tableNames,
			const Vector<String>& concatenateSubTableNames, 
			const Record& lockOptions,
			int option)
{
  // Open the tables here (and not by Table ctor) to make it
  // possible to use the :: syntax for subtables.
  TableLock lockOpt = makeLockOptions(lockOptions);
  Block<Table> tabs(tableNames.size());
  for (uint32_t i=0; i<tableNames.size(); ++i) {
    tabs[i] = TableUtil::openTable (tableNames[i], lockOpt,
                                    Table::TableOption(option));
  }
  Block<String> subNames(concatenateSubTableNames.size());
  std::copy (concatenateSubTableNames.begin(), concatenateSubTableNames.end(),
	     subNames.begin());
  table_p = Table (tabs, subNames);
}
 
TableProxy::TableProxy (const std::vector<TableProxy>& tables,
			const Vector<String>& concatenateSubTableNames,
			int, int, int)
{
  Block<Table> tabs(tables.size());
  for (uint32_t i=0; i<tables.size(); ++i) {
    tabs[i] = tables[i].table();
  }
  Block<String> subNames(concatenateSubTableNames.size());
  std::copy (concatenateSubTableNames.begin(), concatenateSubTableNames.end(),
	     subNames.begin());
  table_p = Table (tabs, subNames);
}

TableProxy::TableProxy (const String& command,
			const std::vector<TableProxy>& tables)
{
  std::vector<const Table*> tabs(tables.size());
  for (uint32_t i=0; i<tabs.size(); i++) {
    tabs[i] = &(tables[i].table());
  }
  // Try to execute the command.
  TaQLResult taqlResult;
  taqlResult = tableCommand (command, tabs);
  // Command succeeded.
  // Add table if result is a table.
  if (taqlResult.isTable()) {
    table_p = taqlResult.table();
  } else {
    // Result is a calculation. Return the resulting values.
    calcValues (calcResult_p, taqlResult.node());
  }
}

TableProxy::TableProxy (const String& fileName,
			const String& headerName,
			const String& tableName,
			bool autoHeader,
			const IPosition& autoShape,
			const String& separator,
			const String& commentMarker,
			int64_t firstLine,
			int64_t lastLine,
			const Vector<String>& columnNames,
			const Vector<String>& dataTypes)
{
  if (separator.length() != 1) {
    throw AipsError ("tablefromascii : separator must be 1 char");
  }
  char sep = separator[0];
  // Create the table
  String inputFormat;
  if (headerName.empty()) {
    if (columnNames.size() == 0  &&  dataTypes.size() == 0) {
      asciiFormat_p = readAsciiTable(fileName, "", tableName, autoHeader,
				     sep, commentMarker, firstLine, lastLine,
				     IPosition(autoShape));
    } else {
      asciiFormat_p = readAsciiTable(fileName, "", tableName,
				     columnNames, dataTypes,
				     sep, commentMarker, firstLine, lastLine);
    }
  } else {
    asciiFormat_p = readAsciiTable(headerName, fileName, "", tableName, sep,
				   commentMarker, firstLine, lastLine);
  }
  // Open the table.
  table_p = Table(tableName);
}

TableProxy::TableProxy (const TableProxy& that)
: table_p (that.table_p)
{}

TableProxy::~TableProxy()
{}

TableProxy& TableProxy::operator= (const TableProxy& that)
{
  if (this != &that) {
    table_p = that.table_p;
  }
  return *this;
}


String TableProxy::endianFormat() const
{
  // Return the endian format as a string.
  if (table_p.endianFormat() == Table::BigEndian) {
    return "big";
  }
  return "little";
}

void TableProxy::lock (bool mode, int32_t nattempts)
{
  table_p.lock (mode, nattempts);
}

void TableProxy::unlock()
{
  table_p.unlock();
}

bool TableProxy::hasDataChanged()
{
  return table_p.hasDataChanged();
}

bool TableProxy::hasLock (bool mode)
{
  return table_p.hasLock (mode);
}

Record TableProxy::lockOptions()
{
  // Return the lock options as a record.
  const TableLock& lock = table_p.lockOptions();
  Record rec;
  String option;
  switch (lock.option()) {
  case TableLock::PermanentLocking:
    option = "permanent";
    break;
  case TableLock::PermanentLockingWait:
    option = "permanentwait";
    break;
  case TableLock::UserLocking:
    if (lock.readLocking()) {
      option = "user";
    } else {
      option = "usernoread";
    }
    break;
  case TableLock::AutoLocking:
    if (lock.readLocking()) {
      option = "auto";
    } else {
      option = "autonoread";
    }
    break;
  default:
    option = "unknown";
  }
  rec.define ("option", option);
  rec.define ("interval", lock.interval());
  rec.define ("maxwait", int32_t(lock.maxWait()));
  return rec;
}

bool TableProxy::isMultiUsed (bool checkSubTables)
{
  return table_p.isMultiUsed (checkSubTables);
}

String TableProxy::toAscii (const String& asciiFile, 
                            const String& headerFile, 
                            const Vector<String>& columns, 
                            const String& sep,
                            const Vector<int32_t>& precision,
                            bool useBrackets)
{
  // Possible warning message.
  String message;
  // Determine separator
  String theSep(sep);
  if (sep.empty()) {
    theSep = " ";
  }
  // Determine names of columns to write.
  Vector<String> colNames(columns);
  if (columns.empty() || columns(0).empty()) {
    // No columns given, so use all.
    colNames.assign (columnNames());
  }
  int32_t ncols = colNames.size();
  // Analyse the columns.
  vector<bool>   col_is_good(ncols);
  vector<String> col_type(ncols);
  int32_t last_good_col = 0;
  for (int32_t j=0; j<ncols; j++) {
    col_is_good[j] = getColInfo (colNames[j], useBrackets,
                                 col_type[j], message);
    // Remember last good column 
    if (col_is_good[j]) {
      last_good_col = j;
    }
  }
  // Open the output files.
  // Determine if header info is in separate file.
  std::ofstream ofs, ofs2;
  std::ofstream *ofsp;
  ofs.open (asciiFile.c_str(), ofstream::out);
  if (!ofs) {
    throw TableError("TableProxy::toAscii - error opening file '"
                     + asciiFile + "'");
  }
  ofsp = &ofs;     // set initially header output to same file as data
  if (!headerFile.empty() && (headerFile != asciiFile)) {
    ofs2.open (headerFile.c_str(), ofstream::out);
    if (!ofs2) {
      throw TableError("TableProxy::toAscii - error opening file '" +
                       headerFile + "'");
    }
    ofsp = &ofs2;  // redirect header output to separate header file
  }
  // Write the format into the header file.
  //  - column names
  for (int32_t i=0; i<ncols; i++) {
    if (col_is_good[i]) {
      *ofsp << colNames[i];
      if (i<last_good_col) {
        *ofsp << theSep;
      }
    }
  }
  *ofsp << endl;
  //  - data types
  for (int32_t i=0; i<ncols; i++) {
    if (col_is_good[i]) {
      *ofsp << col_type[i];
      if (i<last_good_col) {
        *ofsp << theSep;
      }
    }
  }
  *ofsp << endl;
  // Close headerfile if needed.
  if (ofsp == &ofs2) {
    ofs2.close();
  }
  // Write the data
  for (int64_t i=0; i<nrows(); i++) {
    for (int32_t j=0; j<ncols; j++) {
      int32_t prec = (j < int32_t(precision.size())  ?  precision[j] : 0);
      if (col_is_good[j]) {
        printValueHolder (getCell(colNames[j], i), ofs, theSep,
                          prec, useBrackets);
        if (j < last_good_col) {
          ofs << theSep;
        }
      }
    }
    ofs << endl;
  }

  ofs.close();
  return message;
}

// Get the column info for output.
bool TableProxy::getColInfo (const String& colName, bool useBrackets,
                             String& colType, String& message)
{
  bool good = true;
  ColumnDesc colDesc(table_p.tableDesc().columnDesc (colName));
  // Ignore columns containing Records or variable shaped arrays
  // if not using brackets.
  if (!useBrackets) {
    if (colDesc.dataType() == TpRecord) {
      message += "Column " + colName + " contains Record values.\n";
      good = false;
    } else if (! colDesc.isFixedShape()) {
      message += "Column " + colName +
        " possibly contains variable shaped arrays.\n";
      good = false;
    }
  }
  if (good) {
    ostringstream oss;
    // Implement the type naming convention as in class ReadTableAscii.
    switch (colDesc.dataType()) {
    case TpBool:
      oss << "B";
      break;
    case TpUChar:
    case TpShort:
    case TpUShort:
      oss << "S";
      break;
    case TpInt:
    case TpUInt:
    case TpInt64:
      oss << "I";
      break;
    case TpFloat:
      oss << "R";
      break;
    case TpDouble:
      oss << "D";
      break;
    case TpComplex:
      oss << "X";
      break;
    case TpDComplex:
      oss << "DX";
      break;
    case TpString:
      oss << "A";
      break;
    case TpRecord:
      oss << "REC";
      break;
    default:
      message += "Column " + colName +
        " ignored because it contains values with an unknown type.\n";
      good = false;
      break;
    }
    // Append the type with the array shape. Use [] if brackets are to be used.
    // If variable shape, use the shape of the first row.
    if (colDesc.isArray()) {
      IPosition colShape;
      if (colDesc.isFixedShape()) {
        colShape = colDesc.shape();
      }
      if (useBrackets) {
        oss << "[";
      } else {
        // Show non-fixed shape of first row if no brackets are used.
        if (!colDesc.isFixedShape()  &&  table_p.nrow() > 0) {
          colShape = TableColumn(table_p, colName).shape(0);
        }
      }
      for (uint32_t i=0; i<colShape.size(); ++i) {
        if (i > 0) {
          oss << ",";
        }
        oss << colShape[i];
      }
      if (useBrackets) {
        oss << "]";
      }
    }
    colType = oss.str();
  }
  return good;
}

void TableProxy::printValueHolder (const ValueHolder& vh, ostream& os,
                                   const String& sep, int32_t prec,
                                   bool useBrackets) const
{
  int32_t defPrec = 18;
  switch (vh.dataType()) {
  case TpBool:
    os << vh.asBool();
    break;
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    os << vh.asInt64();
    break;
  case TpFloat:
    defPrec = 9;
    CASACORE_FALLTHROUGH;
  case TpDouble:
    {
      // set precision; set it back at the end.
      if (prec <= 0) prec = defPrec;
      streamsize oldPrec = os.precision(prec);
      os << vh.asDouble();
      os.precision (oldPrec);
    }
    break;
  case TpComplex:
    defPrec = 9;
    CASACORE_FALLTHROUGH;
  case TpDComplex:
    {
      // set precision; set it back at the end.
      if (prec <= 0) prec = defPrec;
      streamsize oldPrec = os.precision(prec);
      os << vh.asDComplex();
      os.precision (oldPrec);
    }
    break;
  case TpString:
    os << '"' << vh.asString() << '"';
    break;
  case TpArrayBool:
    {
      Array<bool> arr = vh.asArrayBool();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<bool>::const_iterator iterend = arr.end();
        for (Array<bool>::const_iterator iter=arr.begin();
             iter!=iterend; ++iter) {
          if (iter != arr.begin()) {
            os << sep;
          }
          os << *iter;
        }
      }
    }
    break;
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayUShort:
  case TpArrayInt:
  case TpArrayUInt:
  case TpArrayInt64:
    {
      Array<int64_t> arr = vh.asArrayInt64();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<int64_t>::const_iterator iterend = arr.end();
        for (Array<int64_t>::const_iterator iter=arr.begin();
             iter!=iterend; ++iter) {
          if (iter != arr.begin()) {
            os << sep;
          }
          os << *iter;
        }
      }
    }
    break;
  case TpArrayFloat:
    defPrec = 9;
    CASACORE_FALLTHROUGH;
  case TpArrayDouble:
    {
      // set precision; set it back at the end.
      if (prec <= 0) prec = defPrec;
      streamsize oldPrec = os.precision(prec);
      Array<double> arr = vh.asArrayDouble();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<double>::const_iterator iterend = arr.end();
        for (Array<double>::const_iterator iter=arr.begin();
             iter!=iterend; ++iter) {
          if (iter != arr.begin()) {
            os << sep;
          }
          os << *iter;
        }
      }
      os.precision (oldPrec);
    }
    break;
  case TpArrayComplex:
    defPrec = 9;
    CASACORE_FALLTHROUGH;
  case TpArrayDComplex:
    {
      // set precision; set it back at the end.
      if (prec <= 0) prec = defPrec;
      streamsize oldPrec = os.precision(prec);
      Array<DComplex> arr = vh.asArrayDComplex();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<DComplex>::const_iterator iterend = arr.end();
        for (Array<DComplex>::const_iterator iter=arr.begin();
             iter!=iterend; ++iter) {
          if (iter != arr.begin()) {
            os << sep;
          }
          os << iter->real() << sep << iter->imag();
        }
      }
      os.precision (oldPrec);
    }
    break;
  case TpArrayString:
    {
      Array<String> arr = vh.asArrayString();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<String>::const_iterator iterend = arr.end();
        for (Array<String>::const_iterator iter=arr.begin();
             iter!=iterend; ++iter) {
          if (iter != arr.begin()) {
            os << sep;
          }
          os << '"' << *iter << '"';
        }
      }
    }
    break;
  case TpRecord:
    os << '{' << vh.asRecord() << '}';
    break;
  default:
    throw AipsError ("ValueHolder::write - unknown data type");
    break;
  }
}

template<typename T>
void TableProxy::printArray (const Array<T>& arr, ostream& os,
                             const String& sep) const
{
  if (arr.empty()) {
    cout << "[]";
  } else {
    const IPosition& shp = arr.shape();
    uint32_t ndim = shp.size();
    IPosition pos(shp.size(), 0);
    typename Array<T>::const_iterator iter = arr.begin();
    uint32_t i = ndim;
    while (true) {
      for (uint32_t j=0; j<i; ++j) {
        os << '[';
      }
      printArrayValue (os, *iter, sep);
      ++iter;
      for (i=0; i<ndim; ++i) {
        if (++pos[i] < shp[i]) {
          break;
        }
        os << ']';
        pos[i] = 0;
      }
      if (i == ndim) {
        break;
      }
      os << sep;
    }
  }
}

void TableProxy::rename (const String& newTableName)
{
  table_p.rename (newTableName, Table::New);
}

TableProxy TableProxy::copy (const String& newTableName,
			     bool toMemory,
			     bool deepCopy,
			     bool valueCopy,
			     const String& endianFormat,
			     const Record& dminfo,
			     bool noRows)
{
  Table::EndianFormat endOpt = makeEndianFormat (endianFormat);
  // Always valuecopy if dminfo is not empty or if no rows are copied.
  if (dminfo.nfields() > 0  ||  noRows) {
    valueCopy = true;
  }
  Table outtab;
  if (toMemory) {
    outtab = table_p.copyToMemoryTable (newTableName, noRows);
  } else {
    if (deepCopy || valueCopy) {
      table_p.deepCopy (newTableName, dminfo, Table::New, valueCopy,
			endOpt, noRows);
    } else {
      table_p.copy (newTableName, Table::New);
    }
    outtab = Table(newTableName);
  }
  return TableProxy(outtab);
}

void TableProxy::copyRows (TableProxy& out,
			   int64_t startIn,
			   int64_t startOut,
			   int64_t nrow)
{
  Table tableOut = out.table();
  if (startOut < 0) {
    startOut = tableOut.nrow();
  }
  nrow = checkRowColumn (table_p, "", startIn, nrow, 1,
			 "TableProxy::copyRows");
  if (startOut > int64_t(tableOut.nrow())) {
    throw TableError ("TableProxy::copyRows: start output row too high");
  }
  TableCopy::copyRows (tableOut, table_p, startOut, startIn, nrow);
}

void TableProxy::deleteTable (bool checkSubTables)
{
  if (table_p.isMultiUsed (false)) {
    throw TableError ("Table " + table_p.tableName() +
		      " cannot be deleted; it is used by another process");
  }
  if (checkSubTables) {
    if (table_p.isMultiUsed (true)) {
      throw TableError ("Table " + table_p.tableName() +
			" cannot be deleted;"
			" one of its subtables is used by another process");
    }
  }
  table_p.markForDelete();
}

TableProxy TableProxy::selectRows (const Vector<int64_t>& rownrs,
				   const String& outName)
{
  // If needed, synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  if (anyLT (rownrs, int64_t(0))  ||  anyGE (rownrs, int64_t(table_p.nrow()))) {
    throw TableError("rownumbers should be >= 1 and <= nrow");
  }
  table_p.unlock();
  // Create a table from the selected rows.
  // Rename it and make it non-scratch if a name is given.
  Vector<rownr_t> rows(rownrs.nelements());
  convertArray (rows, rownrs);
  Table ntable = table_p(rows);
  if (! outName.empty()) {
    ntable.rename (outName, Table::New);
  }
  // Command succeeded.
  return ntable;
}

void TableProxy::stillSameShape (int32_t& same, IPosition& shape,
                                 const IPosition& newShape)
{
  if (same == 0) {
    same = 1;           // not first time anymore
    shape = newShape;
  } else if (! newShape.isEqual(shape)) {
    same = 2;           // varying shape
  }
}

void TableProxy::calcValues (Record& rec, const TableExprNode& expr)
{
  if (expr.isScalar()) {
    Vector<rownr_t> rownrs(expr.nrow());
    indgen (rownrs);
    switch (expr.getColumnDataType()) {
    case TpBool:
      rec.define ("values", expr.getColumnBool (rownrs));
      break;
    case TpUChar:
      rec.define ("values", expr.getColumnuChar (rownrs));
      break;
    case TpShort:
      rec.define ("values", expr.getColumnShort (rownrs));
      break;
    case TpUShort:
    {
      Vector<uint16_t> vs = expr.getColumnuShort (rownrs);
      Vector<int32_t> vi(vs.nelements());
      convertArray (vi, vs);
      rec.define ("values", vi);
      break;
    }
    case TpInt:
      rec.define ("values", expr.getColumnInt (rownrs));
      break;
    case TpUInt:
    {
      Vector<uint32_t> vs = expr.getColumnuInt (rownrs);
      Vector<int64_t> vi(vs.nelements());
      convertArray (vi, vs);
      rec.define ("values", vi);
      break;
    }
    case TpInt64:
      rec.define ("values", expr.getColumnInt64 (rownrs));
      break;
    case TpFloat:
      rec.define ("values", expr.getColumnFloat (rownrs));
      break;
    case TpDouble:
      rec.define ("values", expr.getColumnDouble (rownrs));
      break;
    case TpComplex:
      rec.define ("values", expr.getColumnComplex (rownrs));
      break;
    case TpDComplex:
      rec.define ("values", expr.getColumnDComplex (rownrs));
      break;
    case TpString:
      rec.define ("values", expr.getColumnString (rownrs));
      break;
    default:
      throw AipsError("Unknown calc expression scalar type");
    }
  } else {
    // Array result. Check if shape is always the same.
    int32_t sameShape = 0;
    IPosition resShape;
    Record res;
    switch (expr.dataType()) {
    case TpBool:
      for (rownr_t i=0; i<expr.nrow(); i++) {
	MArray<bool> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr.array());
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpInt64:
      for (rownr_t i=0; i<expr.nrow(); i++) {
	MArray<int64_t> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr.array());
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpDouble:
      for (rownr_t i=0; i<expr.nrow(); i++) {
	MArray<double> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr.array());
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpDComplex:
      for (rownr_t i=0; i<expr.nrow(); i++) {
	MArray<DComplex> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr.array());
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpString:
      for (rownr_t i=0; i<expr.nrow(); i++) {
	MArray<String> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr.array());
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    default:
      throw AipsError("Unknown calc expression array type");
    }
    if (sameShape == 2) {
      // Varying shape, so define as Records.
      rec.defineRecord ("values", res);
      // All the same shape, so define as a single array.
    } else {
      switch (expr.dataType()) {
      case TpBool:
        rec.define ("values", record2Array<bool>(res));
        break;
      case TpInt64:
        rec.define ("values", record2Array<int64_t>(res));
        break;
      case TpDouble:
        rec.define ("values", record2Array<double>(res));
        break;
      case TpDComplex:
        rec.define ("values", record2Array<DComplex>(res));
        break;
      case TpString:
        rec.define ("values", record2Array<String>(res));
        break;
      default:
        throw AipsError("Unknown calc expression array type");
      }
    }
  }
}

Record TableProxy::getDataManagerInfo()
{
  return table_p.dataManagerInfo();
}

Record TableProxy::getProperties (const String& name, bool byColumn)
{
  RODataManAccessor acc (table_p, name, byColumn);
  return acc.getProperties();
}

void TableProxy::setProperties (const String& name, const Record& properties,
                                bool byColumn)
{
  RODataManAccessor acc (table_p, name, byColumn);
  acc.setProperties (properties);
}

Record TableProxy::getTableDescription (bool actual, bool cOrder)
{
  // Get the table description.
  std::unique_ptr<const TableDesc> tableDescPtr;
  if (actual) {
    tableDescPtr.reset(new TableDesc(table_p.actualTableDesc()));
  } else {
    tableDescPtr.reset(new TableDesc(table_p.tableDesc()));
  }
  Record rec = getTableDesc(*tableDescPtr, cOrder);

  return rec;
}

Record TableProxy::getTableDesc(const TableDesc & tabdesc, bool cOrder)
{
    Record rec;

    // Convert columns
    for (uint32_t i=0; i<tabdesc.ncolumn(); i++) {
      const ColumnDesc& columnDescription = tabdesc.columnDesc(i);
      rec.defineRecord (columnDescription.name(),
                recordColumnDesc (columnDescription, cOrder));
    }
    
    // Convert hypercolumns
    rec.defineRecord ("_define_hypercolumn_",
        recordHCDesc (tabdesc));

    // Convert keywords
    rec.defineRecord("_keywords_",
        tabdesc.keywordSet().toRecord());
    // Convert private keywords
    rec.defineRecord("_private_keywords_",
        tabdesc.privateKeywordSet().toRecord());


    return rec;
}


Record TableProxy::getColumnDescription (const String& columnName,
					 bool actual, bool cOrder)
{
  // Get the table description.
  std::unique_ptr<const TableDesc> tableDescPtr;
  if (actual) {
    tableDescPtr.reset(new TableDesc(table_p.actualTableDesc()));
  } else {
    tableDescPtr.reset(new TableDesc(table_p.tableDesc()));
  }
  // Return the column description as a record.
  const ColumnDesc& columnDescription = (*tableDescPtr) [columnName];

  return recordColumnDesc (columnDescription, cOrder);
}

String TableProxy::tableName()
{
  return table_p.tableName();
}

Vector<String> TableProxy::getPartNames (bool recursive)
{
	Block<String> partNames(table_p.getPartNames (recursive));
  return Vector<String>(partNames.begin(), partNames.end());
}

String TableProxy::getAsciiFormat() const
{
  return asciiFormat_p;
}

Record TableProxy::getCalcResult() const
{
  return calcResult_p;
}

String TableProxy::showStructure (bool showDataMan, bool showColumns,
                                  bool showSubTables, bool sortColumns) const
{
  ostringstream ostr;
  table_p.showStructure (ostr, showDataMan, showColumns, showSubTables,
                         sortColumns);
  return ostr.str();
}

int64_t TableProxy::nrows()
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  return table_p.nrow();
}

int32_t TableProxy::ncolumns()
{
  return table_p.tableDesc().ncolumn();
}

Vector<int64_t> TableProxy::shape()
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  Vector<int64_t> result(2);
  result(0) = table_p.tableDesc().ncolumn();
  result(1) = table_p.nrow();
  return result;
}

Vector<int64_t> TableProxy::rowNumbers (TableProxy& other)
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  table_p.unlock();
  Vector<int64_t> result(table_p.nrow());
  if (other.table().isNull()) {
    convertArray (result, table_p.rowNumbers());
  } else {
    convertArray (result, table_p.rowNumbers(other.table()));
  }
  return result;
}

Vector<String> TableProxy::columnNames()
{
  // Put the column names into a vector.
  const TableDesc& tabdesc = table_p.tableDesc();
  Vector<String> result (tabdesc.ncolumn());
  for (uint32_t i=0; i< result.nelements(); i++) {
    result(i) = tabdesc.columnDesc(i).name();
  }
  return result;
}

void TableProxy::setMaximumCacheSize (const String& columnName,
				      int32_t nbytes)
{
  TableColumn col (table_p, columnName);
  col.setMaximumCacheSize (nbytes);
}

bool TableProxy::isScalarColumn (const String& columnName)
{
  const TableDesc& tabdesc = table_p.tableDesc();
  return tabdesc.columnDesc(columnName).isScalar();
}

String TableProxy::columnDataType (const String& columnName)
{
  const TableDesc& tabdesc = table_p.tableDesc();
  DataType type = tabdesc.columnDesc(columnName).dataType();
  return getTypeStr(type);
}

String TableProxy::columnArrayType (const String& columnName)
{
  const TableDesc& tableDesc = table_p.tableDesc();
  const ColumnDesc& coldesc = tableDesc.columnDesc (columnName);
  if (coldesc.isScalar()) {
    throw TableError("column " + columnName + " is a scalar column");
  }
  String result;
  int columnOption = coldesc.options ();
  if (columnOption & ColumnDesc::Direct) {
    result = "Direct";
  } else {
    result = "Indirect";
  }
  if (columnOption & ColumnDesc::FixedShape) {
    result += ", fixed";
  } else {
    result += ", variable";
  }
  result += " sized arrays";
  return result;
}

ValueHolder TableProxy::getCell (const String& columnName,
				 int64_t row)
{
  int64_t nrow = getRowsCheck (columnName, row, 1, 1, "getCell");
  return getValueFromTable (columnName, row, nrow, 1, true);
}

void TableProxy::getCellVH (const String& columnName,
                            int64_t row, const ValueHolder& vh)
{
  int64_t nrow = getRowsCheck (columnName, row, 1, 1, "getCellVH");
  getValueFromTable (columnName, row, nrow, 1, true, vh);
}

ValueHolder TableProxy::getCellSlice (const String& columnName,
				      int64_t row,
				      const Vector<int32_t>& blc,
				      const Vector<int32_t>& trc,
				      const Vector<int32_t>& inc)
{
  return getCellSliceIP (columnName, row, blc, trc, inc);
}

void TableProxy::getCellSliceVH (const String& columnName,
                                 int64_t row,
                                 const Vector<int32_t>& blc,
                                 const Vector<int32_t>& trc,
                                 const Vector<int32_t>& inc,
                                 const ValueHolder& vh)
{
  return getCellSliceVHIP (columnName, row, blc, trc, inc, vh);
}

ValueHolder TableProxy::getCellSliceIP (const String& columnName,
                                        int64_t row,
                                        const IPosition& blc,
                                        const IPosition& trc,
                                        const IPosition& inc)
{
  Slicer slicer;
  int64_t nrow = getRowsSliceCheck (slicer, columnName, row, 1, 1,
                                  blc, trc, inc, "getCellSlice");
  return getValueSliceFromTable (columnName, slicer, row, nrow, 1, true);
}

void TableProxy::getCellSliceVHIP (const String& columnName,
                                   int64_t row,
                                   const IPosition& blc,
                                   const IPosition& trc,
                                   const IPosition& inc,
                                   const ValueHolder& vh)
{
  Slicer slicer;
  int64_t nrow = getRowsSliceCheck (slicer, columnName, row, 1, 1,
                                  blc, trc, inc, "getCellSliceVH");
  getValueSliceFromTable (columnName, slicer, row, nrow, 1, true, vh);
}

ValueHolder TableProxy::getColumn (const String& columnName,
				   int64_t row,
				   int64_t nrow,
				   int64_t incr)
{
  int64_t nrows = getRowsCheck (columnName, row, nrow, incr, "getColumn");
  return getValueFromTable (columnName, row, nrows, incr, false);
}

void TableProxy::getColumnVH (const String& columnName,
                              int64_t row,
                              int64_t nrow,
                              int64_t incr,
                              const ValueHolder& vh)
{
  int64_t nrows = getRowsCheck (columnName, row, nrow, incr, "getColumnVH");
  return getValueFromTable (columnName, row, nrows, incr, false, vh);
}

Record TableProxy::getVarColumn (const String& columnName,
				 int64_t row,
				 int64_t nrow,
				 int64_t incr)
{
  int64_t nrows = getRowsCheck (columnName, row, nrow, incr, "getVarColumn");
  TableColumn tabcol (table_p, columnName);
  Record rec;
  char namebuf[22];
  for (int64_t i=0; i<nrows; i++) {
    // Add the result to the record with field name formed from 1-based rownr.
    sprintf (namebuf, "r%lli", row+1);
    if (tabcol.isDefined(row)) {
      getValueFromTable(columnName, row, 1, 1, false).toRecord (rec, namebuf);
    } else {
      ////      rec.add (namebuf, GlishValue::getUnset());
      rec.define (namebuf, false);
    }
    row += incr;
  }
  return rec;
}

ValueHolder TableProxy::getColumnSlice (const String& columnName,
					int64_t row,
					int64_t nrow,
					int64_t incr,
					const Vector<int32_t>& blc,
					const Vector<int32_t>& trc,
					const Vector<int32_t>& inc)
{
  return getColumnSliceIP (columnName, blc, trc, inc, row, nrow, incr);
}

ValueHolder TableProxy::getColumnSliceIP (const String& columnName,
					  const IPosition& blc,
					  const IPosition& trc,
					  const IPosition& inc,
					  int64_t row,
					  int64_t nrow,
					  int64_t incr)
{
  Slicer slicer;
  int64_t nrows = getRowsSliceCheck (slicer, columnName, row, nrow, incr,
                                   blc, trc, inc, "getColumnSlice");
  return getValueSliceFromTable (columnName, slicer, row, nrows, incr, false);
}

void TableProxy::getColumnSliceVH (const String& columnName,
                                   int64_t row,
                                   int64_t nrow,
                                   int64_t incr,
                                   const Vector<int32_t>& blc,
                                   const Vector<int32_t>& trc,
                                   const Vector<int32_t>& inc,
                                   const ValueHolder& vh)
{
  getColumnSliceVHIP (columnName, blc, trc, inc, row, nrow, incr, vh);
}

void TableProxy::getColumnSliceVHIP (const String& columnName,
                                     const IPosition& blc,
                                     const IPosition& trc,
                                     const IPosition& inc,
                                     int64_t row,
                                     int64_t nrow,
                                     int64_t incr,
                                     const ValueHolder& vh)
{
  Slicer slicer;
  int64_t nrows = getRowsSliceCheck (slicer, columnName, row, nrow, incr,
                                   blc, trc, inc, "getColumnSliceVH");
  getValueSliceFromTable (columnName, slicer, row, nrows, incr, false, vh);
}

void TableProxy::putColumn (const String& columnName,
			    int64_t row,
			    int64_t nrow,
			    int64_t incr,
			    const ValueHolder& value)
{
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  nrow = checkRowColumn (table_p, columnName, row, nrow, incr,
			 "TableProxy::putColumn");
  putValueInTable (columnName, row, nrow, incr, false, value);
}

void TableProxy::putVarColumn (const String& columnName,
			       int64_t row,
			       int64_t nrow,
			       int64_t incr,
			       const Record& values)
{
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  nrow = checkRowColumn (table_p, columnName, row, nrow, incr,
			 "TableProxy::putVarColumn");
  if (int32_t(values.nfields()) != nrow) {
    throw TableError("TableProxy::putVarColumn: "
		     "#rows mismatches #elem in value");
  }
  for (int64_t i=0; i<nrow; i++) {
    putValueInTable (columnName, row, 1, 1, false,
		     ValueHolder::fromRecord(values, i));
    row += incr;
  }
}

void TableProxy::putColumnSlice (const String& columnName,
				 int64_t row,
				 int64_t nrow,
				 int64_t incr,
				 const Vector<int32_t>& blc,
				 const Vector<int32_t>& trc,
				 const Vector<int32_t>& inc,
				 const ValueHolder& value)
{
  putColumnSliceIP (columnName, value, blc, trc, inc, row, nrow, incr);
}

void TableProxy::putColumnSliceIP (const String& columnName,
				   const ValueHolder& value,
				   const IPosition& blc,
				   const IPosition& trc,
				   const IPosition& inc,
				   int64_t row,
				   int64_t nrow,
				   int64_t incr)
{
  IPosition cblc, ctrc;
  cblc = blc;
  ctrc = trc;
  setDefaultForSlicer (cblc);
  setDefaultForSlicer (ctrc);
  Slicer slicer;
  if (inc.nelements() > 0) {
    slicer = Slicer (cblc, ctrc, inc, Slicer::endIsLast);
  }else{
    slicer = Slicer (cblc, ctrc, Slicer::endIsLast);
  }
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  nrow = checkRowColumn (table_p, columnName, row, nrow, incr,
			 "TableProxy::putColumn");
  putValueSliceInTable (columnName, slicer, row, nrow, incr, false, value);
}

void TableProxy::putCell (const String& columnName,
			  const Vector<int64_t>& rownrs,
			  const ValueHolder& value)
{
  // Synchronize table to get up-to-date #rows.
  syncTable (table_p);
  for (rownr_t i=0; i<rownrs.nelements(); i++) {
    // Check that the row number is within the table bounds.
    int64_t row = rownrs(i);
    int64_t nrow = checkRowColumn (table_p, columnName, row, 1, 1,
			       "TableProxy::putColumn");
    putValueInTable (columnName, row, nrow, 1, true, value);
  }
}

void TableProxy::putCellSlice (const String& columnName,
			       int64_t row,
			       const Vector<int32_t>& blc,
			       const Vector<int32_t>& trc,
			       const Vector<int32_t>& inc,
			       const ValueHolder& value)
{
  putCellSliceIP (columnName, row, value, blc, trc, inc);
}

void TableProxy::putCellSliceIP (const String& columnName,
				 int64_t row,
				 const ValueHolder& value,
				 const IPosition& blc,
				 const IPosition& trc,
				 const IPosition& inc)
{
  IPosition cblc, ctrc;
  cblc = blc;
  ctrc = trc;
  setDefaultForSlicer (cblc);
  setDefaultForSlicer (ctrc);
  Slicer slicer;
  if (inc.nelements() > 0) {
    slicer = Slicer (cblc, ctrc, inc, Slicer::endIsLast);
  }else{
    slicer = Slicer (cblc, ctrc, Slicer::endIsLast);
  }
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  int64_t nrow = checkRowColumn (table_p, columnName, row, 1, 1,
			     "TableProxy::putColumn");
  putValueSliceInTable (columnName, slicer, row, nrow, 1, true, value);
}

Vector<String> TableProxy::getColumnShapeString (const String& columnName,
						 int64_t rownr,
						 int64_t nrow,
						 int64_t incr,
						 bool cOrder)
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  // Check that the row number is within the table bounds.
  // However, accept a row number equal to nrow when no rows are needed.
  int64_t tabnrow = table_p.nrow();
  if (rownr < 0  ||  rownr > tabnrow  ||  (rownr==tabnrow && nrow>0)) {
    throw TableError("TableProxy::getColumnShapeString: no such row");
  }
  if (incr <= 0) {
    throw TableError("TableProxy::getColumnShapeString: rowincr<=0");
  }
  if (! table_p.tableDesc().isColumn(columnName)) {
    throw TableError("TableProxy::getColumnShapeString: column " +
		     columnName + " does not exist");
  }
  int64_t maxnrow = (tabnrow - rownr + incr - 1) / incr;
  if (nrow < 0  ||  nrow > maxnrow) {
    nrow = maxnrow;
  }
  Vector<String> result;
  TableColumn col (table_p, columnName);
  IPosition shape = col.shapeColumn();
  if (shape.nelements() > 0) {
    //# This is a fixed shape, so return immediately.
    ostringstream os;
    os << fillAxes (shape, cOrder);
    result.resize(1);
    result(0) = os.str();
  } else {
    result.resize (nrow);
    int64_t lastRow(nrow+rownr);
    for (int64_t i=0; i<nrow && rownr<lastRow; i++) {
      ostringstream os;
      os << fillAxes (col.shape (rownr), cOrder);
      result(i) = os.str();
      rownr += incr;
    }
  }
  table_p.unlock();
  return result;
}

bool TableProxy::cellContentsDefined (const String& columnName,
				      int64_t rownr)
{
  TableColumn tabColumn (table_p, columnName);
  return tabColumn.isDefined (rownr);
}

ValueHolder TableProxy::getKeyword (const String& columnName,
				    const String& keywordName,
				    int32_t keywordIndex)
{
  const TableRecord* keySet;
  if (columnName.empty()) {
    keySet = &(table_p.keywordSet());
  }else{
    TableColumn tabColumn (table_p, columnName);
    keySet = &(tabColumn.keywordSet());
  }
  RecordFieldId fieldid(0);
  if (keywordName.empty()) {
    fieldid = RecordFieldId(keywordIndex);
  } else {
    findKeyId (fieldid, keySet, keywordName, columnName);
  }
  return keySet->asValueHolder(fieldid);
}

Record TableProxy::getKeywordSet (const String& columnName)
{
  const TableRecord* keySet;
  if (columnName.empty()) {
    keySet = &(table_p.keywordSet());
  }else{
    TableColumn tabColumn (table_p, columnName);
    keySet = &(tabColumn.keywordSet());
  }
  return keySet->toRecord();
}

void TableProxy::putKeyword (const String& columnName,
			     const String& keywordName,
			     int32_t keywordIndex,
			     bool makeSubRecord,
			     const ValueHolder& value)
{
  TableRecord* keySet;
  if (columnName.empty()) {
    keySet = &(table_p.rwKeywordSet());
  } else {
    TableColumn tabColumn (table_p, columnName);
    keySet = &(tabColumn.rwKeywordSet());
  }
  RecordFieldId fieldid(0);
  if (keywordName.empty()) {
    fieldid = RecordFieldId(keywordIndex-1);
  } else {
    findKeyId (fieldid, keySet, keywordName, columnName,
	       false, true, makeSubRecord);
  }
  keySet->defineFromValueHolder (fieldid, value);
}

void TableProxy::putKeywordSet (const String& columnName,
				const Record& valueSet)
{
  TableRecord* keySet;
  if (columnName.empty()) {
    keySet = &(table_p.rwKeywordSet());
  } else {
    TableColumn tabColumn (table_p, columnName);
    keySet = &(tabColumn.rwKeywordSet());
  }
  keySet->fromRecord (valueSet);
}

void TableProxy::removeKeyword (const String& columnName,
				const String& keywordName,
				int32_t keywordIndex)
{
  TableRecord* keySet;
  if (columnName.empty()) {
    keySet = &(table_p.rwKeywordSet());
  }else{
    TableColumn tabColumn (table_p, columnName);
    keySet = &(tabColumn.rwKeywordSet());
  }
  RecordFieldId fieldid(0);
  if (keywordName.empty()) {
    fieldid = RecordFieldId(keywordIndex);
  } else {
    findKeyId (fieldid, keySet, keywordName, columnName,
	       true, true, false);
  }
  keySet->removeField (fieldid);
}

Vector<String> TableProxy::getFieldNames (const String& columnName,
					  const String& keywordName,
					  int32_t keywordIndex)
{
  const TableRecord* keySet;
  if (columnName.empty()) {
    keySet = &(table_p.keywordSet());
  }else{
    TableColumn tabColumn (table_p, columnName);
    keySet = &(tabColumn.keywordSet());
  }
  RecordFieldId fieldid(0);
  if (keywordIndex < 0) {
    if (! keywordName.empty()) {
      findKeyId (fieldid, keySet, keywordName, columnName);
    }
  }
  const RecordDesc* desc;
  if (keywordName.empty()) {
    desc = &(keySet->description());
  } else {
    if (keySet->dataType (fieldid) != TpRecord) {
      throw TableError("Keyword does not contain a subrecord");
    }
    desc = &(keySet->subRecord(fieldid).description());
  }
  Vector<String> result(desc->nfields());
  for (uint32_t i=0; i<result.nelements(); i++) {
    result(i) = desc->name(i);
  }
  return result;
}

void TableProxy::flush (bool recursive)
{
  table_p.flush (false, recursive);
}

void TableProxy::close()
{
  if (! table_p.isNull()) {
    flush(true);
    unlock();
    table_p = Table();
  }
}

void TableProxy::reopenRW()
{
  table_p.reopenRW();
}

void TableProxy::resync()
{
  table_p.resync();
}

Record TableProxy::tableInfo()
{
  const TableInfo& info = table_p.tableInfo();
  Record rec;
  rec.define ("type", info.type());
  rec.define ("subType", info.subType());
  rec.define ("readme", info.readme());
  return rec;
}

void TableProxy::putTableInfo (const Record& value)
{
  if (! table_p.isWritable()) {
    throw TableError("Table " + table_p.tableName() + " is not writable");
  }
  TableInfo& info = table_p.tableInfo();
  // Loop through all fields in the value and check if they are valid.
  for (uint32_t i=0; i<value.nfields(); i++) {
    String str (value.asString(i));
    if (value.name(i) == "type") {
      info.setType (str);
    } else if (value.name(i) == "subType") {
      info.setSubType (str);
    } else if (value.name(i) == "readme") {
      info.readmeClear();
      if (! str.empty()) {
	info.readmeAddLine (str);
      }
    } else {
      throw TableError("Name of field " + value.name(i) +
		       " must be type, subType or readme");
    }
  }
  // Flush the info to be sure it is on disk.
  table_p.flushTableInfo();
}

void TableProxy::addReadmeLine (const String& line)
{
  if (! table_p.isWritable()) {
    throw TableError("Table " + table_p.tableName() + " is not writable");
  }
  TableInfo& info = table_p.tableInfo();
  info.readmeAddLine (line);
  // Flush the info to be sure it is on disk.
  table_p.flushTableInfo();
}

bool TableProxy::isReadable() const
{
  return true;
}

bool TableProxy::isWritable() const
{
  return table_p.isWritable();
}

void TableProxy::addColumns (const Record& tableDesc,
			     const Record& dminfo,
                             bool addToParent)
{
  TableDesc tabdesc;
  String message;
  if (! makeTableDesc (tableDesc, tabdesc, message)) {
    throw TableError("addColumns failed: " + message);
  }
  if (dminfo.nfields() > 0) {
    table_p.addColumn (tabdesc, dminfo, addToParent);
  } else {
    for (uint32_t i=0; i<tabdesc.ncolumn(); i++) {
      table_p.addColumn (tabdesc[i], addToParent);
    }
  }
}

void TableProxy::renameColumn (const String& nameOld,
			       const String& nameNew)
{
  table_p.renameColumn (nameNew, nameOld);
}

void TableProxy::removeColumns (const Vector<String>& columnNames)
{
  table_p.removeColumn (columnNames);
}

void TableProxy::addRow (int64_t nrow)
{
  table_p.addRow (nrow);
}

void TableProxy::removeRow (const Vector<int64_t>& rownrs)
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  Vector<rownr_t> rows(rownrs.nelements());
  convertArray (rows, rownrs);
  table_p.removeRow (rows);
}


bool TableProxy::makeHC (const Record& gdesc, TableDesc& tabdesc,
			 String& message)
{
  for (uint32_t i=0; i<gdesc.nfields(); i++) {
    String name = gdesc.name(i);
    const Record& cold = gdesc.asRecord((i));
    if (! cold.isDefined("HCndim")) {
      message = "No HCndim for hypercolumn " + name;
      return false;
    }
    int32_t ndim = cold.asInt("HCndim");
    Vector<String> dataNames;
    Vector<String> coordNames;
    Vector<String> idNames;
    if (! cold.isDefined("HCdatanames")) {
      message = "No HCdatanames for hypercolumn " + name;
      return false;
    }
    dataNames = cold.asArrayString("HCdatanames");
    if (cold.isDefined("HCcoordnames")) {
      coordNames = cold.asArrayString("HCcoordnames");
    }
    if (cold.isDefined("HCidnames")) {
      idNames = cold.asArrayString("HCidnames");
    }
    tabdesc.defineHypercolumn (name, ndim, dataNames, coordNames, idNames);
  }
  return true;
}

bool TableProxy::makeTableDesc (const Record& gdesc, TableDesc& tabdesc,
				String& message)
{
    for(uint32_t nrdone=0, nrcols=0; nrdone < gdesc.nfields(); ++nrdone) {
        String name = gdesc.name(nrdone);
        const Record& cold (gdesc.asRecord(nrdone));

        // Avoid special records for now
        if(name == "_define_hypercolumn_") {
            // Ignore, for now, handled later
            continue;
        } else if(name == "_define_dminfo_") {
            // Ignore, this is obsolete
            continue;
        } else if(name == "_keywords_") {
            // Unpack keywords into TableDesc
            tabdesc.rwKeywordSet().fromRecord(cold);
            continue;
        } else if(name == "_private_keywords_") {
            // Ignore, private keywords are not
            // publicly accessable on TableDesc
            continue;
        } else if(!cold.isDefined("valueType")) {
            // Assume it is a column and complain as
            // no value type exists to describe it
            message = "No value type for column " + name;
            return false;
        }

        String valtype = cold.asString("valueType");
        valtype.downcase();

        int option = 0;
        if (cold.isDefined("option")) {
            option = cold.asInt("option");
        }

        int maxlen = 0;
        if (cold.isDefined("maxlen")) {
            maxlen = cold.asInt("maxlen");
        }

        String comment, dmtype, dmgrp;
        if (cold.isDefined("comment")) {
            comment = cold.asString ("comment");
        }

        if (cold.isDefined("dataManagerType")) {
            dmtype = cold.asString("dataManagerType");
        }

        if (cold.isDefined("dataManagerGroup")) {
            dmgrp = cold.asString("dataManagerGroup");
        }

        bool isArray = cold.isDefined("ndim");
        int32_t ndim;
        Vector<int64_t> shape;

        if (isArray) {
            ndim = cold.asInt("ndim");
            if (cold.isDefined("shape")) {
                shape = cold.toArrayInt64 ("shape");
            }
            bool cOrder = false;
            if (cold.isDefined("_c_order")) {
                cOrder = cold.asBool ("_c_order");
            }
            if (! addArrayColumnDesc (tabdesc, valtype, name, comment,
                                      dmtype, dmgrp, option,
                                      ndim, shape, cOrder, message)) {
                return false;
            }
        } else {
            if (valtype == "boolean"  ||  valtype == "bool") {
                tabdesc.addColumn (ScalarColumnDesc<bool>
                                   (name, comment, dmtype, dmgrp, option));
            } else if (valtype == "byte"  ||  valtype == "uchar") {
                tabdesc.addColumn (ScalarColumnDesc<unsigned char>
                                   (name, comment, dmtype, dmgrp, 0, option));
            } else if (valtype == "short") {
                tabdesc.addColumn (ScalarColumnDesc<int16_t>
                                   (name, comment, dmtype, dmgrp, 0, option));
            } else if (valtype == "ushort") {
                tabdesc.addColumn (ScalarColumnDesc<uint16_t>
                                   (name, comment, dmtype, dmgrp, 0, option));
            } else if (valtype == "integer"  ||  valtype == "int") {
                tabdesc.addColumn (ScalarColumnDesc<int32_t>
                                   (name, comment, dmtype, dmgrp, 0, option));
            } else if (valtype == "uint") {
                tabdesc.addColumn (ScalarColumnDesc<uint32_t>
                                   (name, comment, dmtype, dmgrp, 0, option));
            } else if (valtype == "int64") {
                tabdesc.addColumn (ScalarColumnDesc<int64_t>
                                   (name, comment, dmtype, dmgrp, 0, option));
            } else if (valtype == "float") {
                tabdesc.addColumn (ScalarColumnDesc<float>
                                   (name, comment, dmtype, dmgrp, option));
            } else if (valtype == "double") {
                tabdesc.addColumn (ScalarColumnDesc<double>
                                   (name, comment, dmtype, dmgrp, option));
            } else if (valtype == "complex") {
                tabdesc.addColumn (ScalarColumnDesc<Complex>
                                   (name, comment, dmtype, dmgrp, option));
            } else if (valtype == "dcomplex") {
                tabdesc.addColumn (ScalarColumnDesc<DComplex>
                                   (name, comment, dmtype, dmgrp, option));
            } else if (valtype == "string") {
                tabdesc.addColumn (ScalarColumnDesc<String>
                                   (name, comment, dmtype, dmgrp, option));
            } else if (valtype == "record") {
                tabdesc.addColumn (ScalarRecordColumnDesc
                                   (name, comment, dmtype, dmgrp));
            }else{
                message = "Unknown data type " + valtype +
                        " for scalar column " + name;
                return false;
            }
        }
        // Set maximum string length.
        if (maxlen > 0) {
            tabdesc.rwColumnDesc(nrcols).setMaxLength (maxlen);
        }
        // Define the keywords if needed.
        if (cold.isDefined ("keywords")) {
            TableRecord& keySet (tabdesc.rwColumnDesc(nrcols).rwKeywordSet());
            keySet.fromRecord (cold.asRecord("keywords"));
        }

        ++nrcols;
    }

    if (gdesc.isDefined ("_define_hypercolumn_"))  {
        if (! makeHC (gdesc.asRecord("_define_hypercolumn_"), tabdesc, message))  {
            return false;
        }
    }

    return true;
}

bool TableProxy::addArrayColumnDesc (TableDesc& tabdesc,
				     const String& valtype,
				     const String& name, const String& comment,
				     const String& dmtype, const String& dmgrp,
				     int option,
				     int32_t ndim, const Vector<int64_t>& shape,
				     bool cOrder,
				     String& message)
{
  if (ndim <= 0  &&  shape.nelements() > 0) {
    message = "arrayColumnDesc: shape should not be given when ndim <= 0";
    return false;
  }
  if (ndim > 0  &&  shape.nelements() != 0
  &&  uint32_t(ndim) != shape.nelements()) {
    message = "arrayColumnDesc: ndim and shape mismatch";
    return false;
  }
  IPosition shp;
  if (shape.nelements() > 0) {
    if (anyLE (shape, int64_t(0))) {
      message = "arrayColumnDesc: shape < 0";
      return false;
    }
    shp = fillAxes (IPosition(shape), cOrder);
    option |= ColumnDesc::FixedShape;
  }
  if (valtype == "boolean"  ||  valtype == "bool") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<bool>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<bool>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "byte"  ||  valtype == "uchar") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<unsigned char>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<unsigned char>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "short") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<int16_t>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<int16_t>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "ushort") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<uint16_t>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<uint16_t>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "integer"  ||  valtype == "int") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<int32_t>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<int32_t>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "uint") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<uint32_t>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<uint32_t>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "int64") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<int64_t>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<int64_t>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "float") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<float>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<float>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "double") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<double>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<double>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "complex") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<Complex>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<Complex>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "dcomplex") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<DComplex>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<DComplex>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "string") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<String>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<String>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  }else{
    message = "Unknown data type " + valtype +
              " for array column " + name;
    return false;
  }
  return true;
}

String TableProxy::getTypeStr (DataType dtype)
{
  switch (dtype) {
  case TpBool:
    return "boolean";
  case TpUChar:
    return "uchar";
  case TpShort:
    return "short";
  case TpUShort:
    return "ushort";
  case TpInt:
    return "int";
  case TpUInt:
    return "uint";
  case TpInt64:
    return "int64";
  case TpFloat:
    return "float";
  case TpDouble:
    return "double";
  case TpComplex:
    return "complex";
  case TpDComplex:
    return "dcomplex";
  case TpString:
    return "string";
  case TpRecord:
    return "record";
  default:
    break;
  }
  return "int";
}

Record TableProxy::recordColumnDesc (const ColumnDesc& cold, bool cOrder)
{
  Record cdesc;
  cdesc.define ("valueType", getTypeStr(cold.dataType()));
  cdesc.define ("dataManagerType", cold.dataManagerType());
  cdesc.define ("dataManagerGroup", cold.dataManagerGroup());
  cdesc.define ("option", int32_t(cold.options()));
  cdesc.define ("maxlen", int32_t(cold.maxLength()));
  cdesc.define ("comment", cold.comment());
  if (cold.isArray()) {
    cdesc.define ("ndim", int32_t(cold.ndim()));
    IPosition shape = fillAxes (cold.shape(), cOrder);
    if (shape.nelements() > 0) {
      Vector<int64_t> vec(shape.nelements());
      for (uint32_t i=0; i<shape.nelements(); i++) {
	vec(i) = shape(i);
      }
      cdesc.define ("shape", vec);
    }
    if (cOrder) {
      cdesc.define ("_c_order", cOrder);
    }
  }

  // Column keywords
  const TableRecord & keys = cold.keywordSet();
  cdesc.defineRecord("keywords", keys.toRecord());

  return cdesc;
}

Record TableProxy::recordHCDesc (const TableDesc& tableDesc)
{
  Record rec;
  Vector<String> hcNames = tableDesc.hypercolumnNames();
  for (uint32_t i=0; i<hcNames.nelements(); i++) {
    Vector<String> dataNames;
    Vector<String> coordNames;
    Vector<String> idNames;
    int32_t ndim = tableDesc.hypercolumnDesc (hcNames(i), dataNames,
					  coordNames, idNames);
    Record hrec;
    hrec.define ("HCndim", ndim);
    hrec.define ("HCdatanames", dataNames);
    hrec.define ("HCcoordnames", coordNames);
    hrec.define ("HCidnames", idNames);
    rec.defineRecord (hcNames(i), hrec);
  }
  return rec;
}

int64_t TableProxy::getRowsCheck (const String& columnName,
                                int64_t row, int64_t nrow, int64_t incr,
                                const String& caller)
{
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  return checkRowColumn (table_p, columnName, row, nrow, incr, caller);
}

int64_t TableProxy::getRowsSliceCheck (Slicer& slicer,
                                     const String& columnName,
                                     int64_t row, int64_t nrow, int64_t incr,
                                     const IPosition& blc,
                                     const IPosition& trc,
                                     const IPosition& inc,
                                     const String& caller)
{
  IPosition cblc, ctrc;
  cblc = blc;
  ctrc = trc;
  setDefaultForSlicer (cblc);
  setDefaultForSlicer (ctrc);
  if (inc.nelements() > 0) {
    slicer = Slicer (cblc, ctrc, inc, Slicer::endIsLast);
  }else{
    slicer = Slicer (cblc, ctrc, Slicer::endIsLast);
  }
  return getRowsCheck (columnName, row, nrow, incr, caller);
}

int64_t TableProxy::checkRowColumn (Table& table,
				const String& colName,
				int64_t rownr, int64_t nrow, int64_t incr,
				const String& caller)
{
  // Check that the row number is within the table bounds.
  // However, accept a row number equal to nrow when no rows are needed.
  int64_t tabnrow = table.nrow();
  if (rownr < 0  ||  rownr > tabnrow  ||  (rownr==tabnrow && nrow>0)) {
    throw TableError ("TableProxy::" + caller + ": no such row");
  } else if (incr <= 0) {
    throw TableError (String(caller) + ": rowincr<=0");
  } else {
    if (!colName.empty()  &&  !table.tableDesc().isColumn(colName)) {
      throw TableError ("TableProxy::" + caller + ": column " + colName +
                        " does not exist");
    }
  }
  int64_t maxnrow = (tabnrow - rownr + incr - 1) / incr;
  if (nrow < 0  ||  nrow > maxnrow) {
    nrow = maxnrow;
  }
  return nrow;
}


ValueHolder TableProxy::makeEmptyArray (DataType dtype)
{
  IPosition shape(1,0);
  switch (dtype) {
  case TpBool:
    return ValueHolder(Array<bool>(shape));
  case TpUChar:
    return ValueHolder(Array<unsigned char>(shape));
  case TpShort:
    return ValueHolder(Array<int16_t>(shape));
  case TpUShort:
    return ValueHolder(Array<uint16_t>(shape));
  case TpInt:
    return ValueHolder(Array<int32_t>(shape));
  case TpUInt:
    return ValueHolder(Array<uint32_t>(shape));
  case TpInt64:
    return ValueHolder(Array<int64_t>(shape));
  case TpFloat:
    return ValueHolder(Array<float>(shape));
  case TpDouble:
    return ValueHolder(Array<double>(shape));
  case TpComplex:
    return ValueHolder(Array<Complex>(shape));
  case TpDComplex:
    return ValueHolder(Array<DComplex>(shape));
  case TpString:
    return ValueHolder(Array<String>(shape));
  default:
    throw TableError ("TableProxy::getCell/Column: Unknown scalar type");
  }
}

ValueHolder TableProxy::getValueFromTable (const String& colName,
					   int64_t rownr, int64_t nrow,
					   int64_t incr,
					   bool isCell)
{
  // Exit immediately if no rows have to be done.
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  bool isScalar = cdesc.isScalar();
  DataType dtype = cdesc.dataType();
  if (nrow == 0) {
    return makeEmptyArray (dtype);
  }
  if (isScalar) {
    switch (dtype) {
    case TpBool: 
      {
	ScalarColumn<bool> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUChar:
      {
	ScalarColumn<unsigned char> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpShort:
      {
	ScalarColumn<int16_t> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUShort:
      {
	ScalarColumn<uint16_t> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpInt:
      {
	ScalarColumn<int32_t> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{ 
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUInt:
      {
	ScalarColumn<uint32_t> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpInt64:
      {
	ScalarColumn<int64_t> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{ 
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpFloat:
      {
	ScalarColumn<float> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpDouble:
      {
	ScalarColumn<double> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpComplex:
      {
	ScalarColumn<Complex> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpDComplex:
      {
	ScalarColumn<DComplex> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpString:
      {
	ScalarColumn<String> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpRecord:
      {
	ScalarColumn<TableRecord> ac(table_p,colName); 
	if (isCell) {
	  // Transform a TableRecord into a Record.
	  return ValueHolder (ac(rownr).toRecord());
	} else {
	  throw TableError ("TableProxy::getColumn not possible for a column"
	                    " containing records");
	}
      }
      break;
    default:
      throw TableError ("TableProxy::getCell/Column: Unknown scalar type");
    }
  } else {
    switch (dtype) {
    case TpBool:
      {
	ArrayColumn<bool> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUChar:
      {
	ArrayColumn<unsigned char> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpShort:
      {
	ArrayColumn<int16_t> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUShort:
      {
	ArrayColumn<uint16_t> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpInt:
      {
	ArrayColumn<int32_t> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUInt:
      {
	ArrayColumn<uint32_t> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpInt64:
      {
	ArrayColumn<int64_t> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpFloat:
      {
	ArrayColumn<float> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpDouble:
      {
	ArrayColumn<double> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpComplex:
      {
	ArrayColumn<Complex> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpDComplex:
      {
	ArrayColumn<DComplex> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpString:
      {
	ArrayColumn<String> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    default:
      break;
    }
  }
  throw TableError ("TableProxy::getCell/Column: Unknown array type");
}

void TableProxy::getValueFromTable (const String& colName,
                                    int64_t rownr, int64_t nrow,
                                    int64_t incr,
                                    bool isCell,
                                    const ValueHolder& vh)
{
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  bool isScalar = cdesc.isScalar();
  DataType dtype = cdesc.dataType();
  if (isScalar && isCell) {
    throw TableError("A scalar value cannot be read into a python variable");
  }
  if (nrow == 0) {
    return;
  }
  switch (vh.dataType()) {
  case TpArrayBool:
    {
      Array<bool> arr(vh.asArrayBool());
      if (dtype != TpBool) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayBool();
      } else if (isScalar) {
        // Read directly into the array (thus into Python ndarray object).
        ScalarColumn<bool> ac(table_p, colName);
        Vector<bool> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<bool> ac(table_p, colName);
        if (isCell) {
          ac.get (rownr, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), arr);
        }
      }
    }
    break;
  case TpArrayInt:
    {
      Array<int32_t> arr(vh.asArrayInt());
      if (dtype != TpInt) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayInt();
      } else if (isScalar) {
        ScalarColumn<int32_t> ac(table_p, colName);
        Vector<int32_t> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<int32_t> ac(table_p, colName);
        if (isCell) {
          ac.get (rownr, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), arr);
        }
      }
    }
    break;
  case TpArrayFloat:
    {
      Array<float> arr(vh.asArrayFloat());
      if (dtype != TpFloat) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayFloat();
      } else if (isScalar) {
        ScalarColumn<float> ac(table_p, colName);
        Vector<float> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<float> ac(table_p, colName);
        if (isCell) {
          ac.get (rownr, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), arr);
        }
      }
    }
    break;
  case TpArrayDouble:
    {
      Array<double> arr(vh.asArrayDouble());
      if (dtype != TpDouble) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayDouble();
      } else if (isScalar) {
        ScalarColumn<double> ac(table_p, colName);
        Vector<double> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<double> ac(table_p, colName);
        if (isCell) {
          ac.get (rownr, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), arr);
        }
      }
    }
    break;
  case TpArrayComplex:
    {
      Array<Complex> arr(vh.asArrayComplex());
      if (dtype != TpComplex) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayComplex();
      } else if (isScalar) {
        ScalarColumn<Complex> ac(table_p, colName);
        Vector<Complex> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<Complex> ac(table_p, colName);
        if (isCell) {
          ac.get (rownr, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), arr);
        }
      }
    }
    break;
  case TpArrayDComplex:
    {
      Array<DComplex> arr(vh.asArrayDComplex());
      if (dtype != TpDComplex) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayDComplex();
      } else if (isScalar) {
        ScalarColumn<DComplex> ac(table_p, colName);
        Vector<DComplex> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<DComplex> ac(table_p, colName);
        if (isCell) {
          ac.get (rownr, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), arr);
        }
      }
    }
    break;
  default:
    throw TableError ("TableProxy::getCell/Column: Unknown data type " +
                      ValType::getTypeStr(vh.dataType()));
  }
}

ValueHolder TableProxy::getValueSliceFromTable (const String& colName,
						const Slicer& slicer,
						int64_t rownr, int64_t nrow, int64_t incr,
						bool isCell)
{
  // Check that the column is an array.
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  if (! cdesc.isArray()) {
    throw TableError ("TableProxy::getColumnSlice: column " +
		      String(colName) + " is not an array column");
  }
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return makeEmptyArray (cdesc.dataType());
  }
  switch(cdesc.dataType()) {
  case TpBool:
    {
      ArrayColumn<bool> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpUChar:
    {
      ArrayColumn<unsigned char> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpShort:
    {
      ArrayColumn<int16_t> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpUShort:
    {
      ArrayColumn<uint16_t> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpInt:
    {
      ArrayColumn<int32_t> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpUInt:
    {
      ArrayColumn<uint32_t> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpInt64:
    {
      ArrayColumn<int64_t> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
  case TpFloat:
    {
      ArrayColumn<float> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpDouble:
    {
      ArrayColumn<double> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpComplex:
    {
      ArrayColumn<Complex> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpDComplex:
    {
      ArrayColumn<DComplex> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpString:
    {
      ArrayColumn<String> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  default:
    break;
  }
  throw TableError ("TableProxy::getColumnSlice: Unknown array type");
}

void TableProxy::getValueSliceFromTable (const String& colName,
                                         const Slicer& slicer,
                                         int64_t rownr, int64_t nrow, int64_t incr,
                                         bool isCell,
                                         const ValueHolder& vh)
{
  // Check that the column is an array.
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  if (! cdesc.isArray()) {
    throw TableError ("TableProxy::getColumnSlice: column " +
		      String(colName) + " is not an array column");
  }
  if (nrow == 0) {
    return;
  }
  DataType dtype = cdesc.dataType();
  switch (vh.dataType()) {
  case TpArrayBool:
    {
      Array<bool> arr(vh.asArrayBool());
      if (dtype != TpBool) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayBool();
      } else {
        ArrayColumn<bool> ac(table_p, colName);
        if (isCell) {
          ac.getSlice (rownr, slicer, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), slicer, arr);
        }
      }
    }
    break;
  case TpArrayInt:
    {
      Array<int32_t> arr(vh.asArrayInt());
      if (dtype != TpInt) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayInt();
      } else {
        ArrayColumn<int32_t> ac(table_p, colName);
        if (isCell) {
          ac.getSlice (rownr, slicer, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), slicer, arr);
        }
      }
    }
    break;
  case TpArrayFloat:
    {
      Array<float> arr(vh.asArrayFloat());
      if (dtype != TpFloat) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayFloat();
      } else {
        ArrayColumn<float> ac(table_p, colName);
        if (isCell) {
          ac.getSlice (rownr, slicer, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), slicer, arr);
        }
      }
    }
    break;
  case TpArrayDouble:
    {
      Array<double> arr(vh.asArrayDouble());
      if (dtype != TpDouble) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayDouble();
      } else {
        ArrayColumn<double> ac(table_p, colName);
        if (isCell) {
          ac.getSlice (rownr, slicer, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), slicer, arr);
        }
      }
    }
    break;
  case TpArrayComplex:
    {
      Array<Complex> arr(vh.asArrayComplex());
      if (dtype != TpComplex) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayComplex();
      } else {
        ArrayColumn<Complex> ac(table_p, colName);
        if (isCell) {
          ac.getSlice (rownr, slicer, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), slicer, arr);
        }
      }
    }
    break;
  case TpArrayDComplex:
    {
      Array<DComplex> arr(vh.asArrayDComplex());
      if (dtype != TpDComplex) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayDComplex();
      } else {
        ArrayColumn<DComplex> ac(table_p, colName);
        if (isCell) {
          ac.getSlice (rownr, slicer, arr);
        } else {
          ac.getColumnRange (Slice(rownr, nrow, incr), slicer, arr);
        }
      }
    }
    break;
  default:
    throw TableError ("TableProxy::getCell/Column: Unknown data type " +
                      ValType::getTypeStr(vh.dataType()));
  }
}


void TableProxy::putValueInTable (const String& colName,
				  int64_t rownr, int64_t nrow, int64_t incr,
				  bool isCell, const ValueHolder& value)
{
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return;
  }
  bool isScalar = table_p.tableDesc().columnDesc(colName).isScalar();
  DataType type = table_p.tableDesc().columnDesc(colName).dataType();
  if (isScalar) {
    switch (type) {
    case TpBool: 
      {
	ScalarColumn<bool> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asBool());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayBool());
	}
      }
      break;
    case TpUChar:
      {
	ScalarColumn<unsigned char> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asuChar());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuChar());
	}
      }
      break;
    case TpShort:
      {
	ScalarColumn<int16_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asShort());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayShort());
	}
      }
      break;
    case TpUShort:
      {
	ScalarColumn<uint16_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asuShort());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuShort());
	}
      }
      break;
    case TpInt:
      {
	ScalarColumn<int32_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asInt());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayInt());
	}
      }
      break;
    case TpUInt:
      {
	ScalarColumn<uint32_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asuInt());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuInt());
	}
      }
      break;
    case TpInt64:
      {
	ScalarColumn<int64_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asInt64());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayInt64());
	}
      }
      break;
    case TpFloat:
      {
	ScalarColumn<float> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asFloat());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayFloat());
	}
      }
      break;
    case TpDouble:
      {
	ScalarColumn<double> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asDouble());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayDouble());
	}
      }
      break;
    case TpComplex:
      {
	ScalarColumn<Complex> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asComplex());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayComplex());
	}
      }
      break;
    case TpDComplex:
      {
	ScalarColumn<DComplex> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asDComplex());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayDComplex());
	}
      }
      break;
    case TpString:
      {
	ScalarColumn<String> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asString());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayString());
	}
      }
      break;
    case TpRecord:
      {
	ScalarColumn<TableRecord> col(table_p, colName);
	if (isCell) {
	  // Transform a Record into a TableRecord.
	  TableRecord rec;
	  rec.fromRecord (value.asRecord());
	  col.put (rownr, rec);
	} else {
	  throw TableError ("TableProxy::putColumn not possible for a column"
			    " containing records");
	}
      }
      break;
    default:
      throw TableError ("TableProxy::put: unknown scalar data type");
    }
  }else{
    switch (type) {
    case TpBool:
      {
	ArrayColumn<bool> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayBool());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayBool());
	}
      }
      break;
    case TpUChar:
      {
	ArrayColumn<unsigned char> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayuChar());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuChar());
	}
      }
      break;
    case TpShort:
      {
	ArrayColumn<int16_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayShort());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayShort());
	}
      }
      break;
    case TpUShort:
      {
	ArrayColumn<uint16_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayuShort());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuShort());
	}
      }
      break;
    case TpInt:
      {
	ArrayColumn<int32_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayInt());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayInt());
	}
      }
      break;
    case TpUInt:
      {
	ArrayColumn<uint32_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayuInt());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuInt());
	}
      }
      break;
    case TpInt64:
      {
	ArrayColumn<int64_t> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayInt64());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayInt64());
	}
      }
      break;
    case TpFloat:
      {
	ArrayColumn<float> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayFloat());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayFloat());
	}
      }
      break;
    case TpDouble:
      {
	ArrayColumn<double> col(table_p, colName);
	  if (isCell) {
	  col.put (rownr, value.asArrayDouble());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayDouble());
	}
      }
      break;
    case TpComplex:
      {
	ArrayColumn<Complex> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayComplex());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayComplex());
	}
      }
      break;
    case TpDComplex:
      {
	ArrayColumn<DComplex> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayDComplex());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayDComplex());
	}
      }
      break;
    case TpString:
      {
	ArrayColumn<String> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayString());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayString());
	}
      }
      break;
    default:
      throw TableError("TableProxy::put: unknown array data type");
    }
  }
}

void TableProxy::putValueSliceInTable (const String& colName,
				       const Slicer& slicer,
				       int64_t rownr, int64_t nrow, int64_t incr,
				       bool isCell, const ValueHolder& value)
{
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return;
  }
  switch (table_p.tableDesc().columnDesc(colName).dataType()) {
  case TpBool:
    {
      ArrayColumn<bool> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayBool());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayBool());
      }
    }
    break;
  case TpUChar:
    {
      ArrayColumn<unsigned char> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayuChar());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayuChar());
      }
    }
    break;
    case TpShort:
    {
      ArrayColumn<int16_t> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayShort());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayShort());
      }
    }
    break;
  case TpUShort:
    {
      ArrayColumn<uint16_t> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayuShort());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayuShort());
      }
    }
    break;
  case TpInt:
    {
      ArrayColumn<int32_t> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayInt());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayInt());
      }
    }
    break;
  case TpUInt:
    {
      ArrayColumn<uint32_t> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayuInt());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayuInt());
      }
    }
    break;
  case TpInt64:
    {
      ArrayColumn<int64_t> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayInt64());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayInt64());
      }
    }
    break;
  case TpFloat:
    {
      ArrayColumn<float> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayFloat());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayFloat());
      }
    }
    break;
  case TpDouble:
    {
      ArrayColumn<double> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayDouble());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayDouble());
      }
    }
    break;
  case TpComplex:
    {
      ArrayColumn<Complex> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayComplex());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayComplex());
      }
    }
    break;
  case TpDComplex:
    {
      ArrayColumn<DComplex> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayDComplex());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayDComplex());
      }
    }
    break;
  case TpString:
    {
      ArrayColumn<String> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayString());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayString());
      }
    }
    break;
  default:
    throw TableError ("TableProxy::putColumnSlice: unknown array data type");
  }
}

void TableProxy::findKeyId (RecordFieldId& fieldid,
			    const TableRecord*& keySet,
			    const String& keyname,
			    const String& column)
{
  TableRecord* ksPtr = const_cast<TableRecord*>(keySet);
  findKeyId (fieldid, ksPtr, keyname, column, true, false, false);
  keySet = ksPtr;
}

void TableProxy::findKeyId (RecordFieldId& fieldid,
			    TableRecord*& keySet,
			    const String& keyname,
			    const String& column,
			    bool mustExist,
			    bool change, bool makeSubRecord)
{
  if (keyname.empty()) {
    throw TableError ("Empty keyword name given");
  }
  // A keyword name can consist of multiple names for a keyword hierarchy.
  Vector<String> keys = stringToVector (keyname, '.');
  String usedName;
  for (uint32_t i=0; i<keys.nelements(); i++) {
    if (keys(i).empty()) {
      throw TableError ("Part of keyword name " + keyname + " is empty");
    }
    usedName += keys(i);
    if (! keySet->isDefined (keys(i))) {
      if (mustExist) {
	if (column.empty()) {
	  throw TableError ("Table keyword " + usedName + " does not exist");
	} else {
	  throw TableError ("Keyword " + usedName + " in column " + column +
			    " does not exist");
	}
      } else {
	if (makeSubRecord  &&  i+1 < keys.nelements()) {
	  keySet->defineRecord (keys(i), TableRecord());
	}
      }
    }
    fieldid = RecordFieldId(keys(i));
    if (i+1 < keys.nelements()) {
      if (keySet->dataType (fieldid) != TpRecord) {
	if (column.empty()) {
	  throw TableError ("Table keyword " + usedName +
			    " does not have a record-value");
	} else {
	  throw TableError ("Keyword " + usedName + " in column " +
			    column + " does not have a record-value");
	}
      }
      if (change) {
	keySet = &(keySet->rwSubRecord (fieldid));
      } else {
	keySet = (TableRecord*)(&(keySet->subRecord (fieldid)));
      }
    }
    usedName += '.';
  }
}


void TableProxy::syncTable (Table& table)
{
  if (table.lockOptions().readLocking()) {
    table.lock (FileLocker::Read);
  }
}

void TableProxy::setDefaultForSlicer (IPosition& vec) const
{
  for (uint32_t i=0; i<vec.nelements(); i++) {
    if (vec(i) < 0) {
      vec(i) = Slicer::MimicSource;
    }
  }
}

TableLock TableProxy::makeLockOptions (const Record& options)
{
  if (options.nfields() == 0) {
    return TableLock();
  }
  if (! options.isDefined("option")) {
    throw TableError ("lockOptions must contain field 'option'");
  }
  String str = options.asString ("option");
  str.downcase();
  TableLock::LockOption opt = TableLock::AutoLocking;
  if (str == "default") {
    opt = TableLock::DefaultLocking;
  } else if (str == "auto") {
    opt = TableLock::AutoLocking;
  } else if (str == "autonoread") {
    opt = TableLock::AutoNoReadLocking;
  } else if (str == "user") {
    opt = TableLock::UserLocking;
  } else if (str == "usernoread") {
    opt = TableLock::UserNoReadLocking;
  } else if (str == "permanent") {
    opt = TableLock::PermanentLocking;
  } else if (str == "permanentwait") {
    opt = TableLock::PermanentLockingWait;
  } else {
    throw TableError ("'" + str + "' is an unknown lock option; valid are "
		      "default,auto,autonoread,user,usernoread,permanent,"
		      "permanentwait");
  }
  if (options.nfields() == 1) {
    return TableLock(opt);
  }
  double interval = 5;
  int maxWait = 0;
  if (options.isDefined ("interval")) {
    interval = options.asDouble ("interval");
  }
  if (options.isDefined ("maxwait")) {
    maxWait = options.asInt ("maxwait");
  }
  return TableLock (opt, interval, maxWait);
}

Table::EndianFormat TableProxy::makeEndianFormat (const String& endianFormat)
{
  // Interpret the endian option.
  Table::EndianFormat endOpt = Table::AipsrcEndian;
  if (! endianFormat.empty()) {
    String str(endianFormat);
    str.downcase();
    if (str == "aipsrc") {
      endOpt = Table::AipsrcEndian;
    } else if (str == "local") {
      endOpt = Table::LocalEndian;
    } else if (str == "big") {
      endOpt = Table::BigEndian;
    } else if (str == "little") {
      endOpt = Table::LittleEndian;
    } else {
      throw TableError ("endian format '" + endianFormat + "' is incorrect; "
			" must be aipsrc, local, big, or little");
    }
  }
  return endOpt;
}

IPosition TableProxy::fillAxes (const IPosition& ipos, bool cOrder)
{
  IPosition s(ipos);
  int32_t nd = s.size();
  if (cOrder  &&  nd > 1) {
    for (int32_t i=0; i<nd; i++) {
      s[i] = ipos[nd-i-1];
    }
  }
  return s;
}

} //# NAMESPACE CASACORE - END
