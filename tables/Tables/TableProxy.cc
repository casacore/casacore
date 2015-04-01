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
//#
//# $Id$


#include <casacore/tables/Tables/TableProxy.h>
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
#include <casacore/casa/Arrays/ArrayIO.h>
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
  table_p = Table::openTable (tableName, makeLockOptions(lockOptions),
                              Table::TableOption(option));
}

TableProxy::TableProxy (const String& tableName,
			const Record& lockOptions,
			const String& endianFormat,
			const String& memType,
			Int nrow,
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
  SetupNewTable newtab(tableName, tabdesc,
                       tableName.empty()  ?  Table::Scratch : Table::New);
  // Apply a possible dminfo object.
  newtab.bindCreate (dmInfo);
  table_p = Table (newtab, type, makeLockOptions(lockOptions),
		   nrow, False, endOpt);
}

TableProxy::TableProxy (const Vector<String>& tableNames,
			const Vector<String>& concatenateSubTableNames, 
			const Record& lockOptions,
			int option)
{
  Block<String> names(tableNames.size());
  std::copy (tableNames.begin(), tableNames.end(), names.begin());
  Block<String> subNames(concatenateSubTableNames.size());
  std::copy (concatenateSubTableNames.begin(), concatenateSubTableNames.end(),
	     subNames.begin());
  table_p = Table (names, subNames, makeLockOptions(lockOptions),
		   Table::TableOption(option));
}
 
TableProxy::TableProxy (const std::vector<TableProxy>& tables,
			const Vector<String>& concatenateSubTableNames,
			int, int, int)
{
  Block<Table> tabs(tables.size());
  for (uInt i=0; i<tables.size(); ++i) {
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
  for (uInt i=0; i<tabs.size(); i++) {
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
			Bool autoHeader,
			const IPosition& autoShape,
			const String& separator,
			const String& commentMarker,
			Int firstLine,
			Int lastLine,
			const Vector<String>& columnNames,
			const Vector<String>& dataTypes)
{
  if (separator.length() != 1) {
    throw AipsError ("tablefromascii : separator must be 1 char");
  }
  Char sep = separator[0];
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

void TableProxy::lock (Bool mode, Int nattempts)
{
  table_p.lock (mode, nattempts);
}

void TableProxy::unlock()
{
  table_p.unlock();
}

Bool TableProxy::hasDataChanged()
{
  return table_p.hasDataChanged();
}

Bool TableProxy::hasLock (Bool mode)
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
  rec.define ("maxwait", Int(lock.maxWait()));
  return rec;
}

Bool TableProxy::isMultiUsed (Bool checkSubTables)
{
  return table_p.isMultiUsed (checkSubTables);
}

String TableProxy::toAscii (const String& asciiFile, 
                            const String& headerFile, 
                            const Vector<String>& columns, 
                            const String& sep,
                            const Vector<Int>& precision,
                            Bool useBrackets)
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
  Int ncols = colNames.size();
  // Analyse the columns.
  vector<Bool>   col_is_good(ncols);
  vector<String> col_type(ncols);
  Int last_good_col = 0;
  for (Int j=0; j<ncols; j++) {
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
  for (Int i=0; i<ncols; i++) {
    if (col_is_good[i]) {
      *ofsp << colNames[i];
      if (i<last_good_col) {
        *ofsp << theSep;
      }
    }
  }
  *ofsp << endl;
  //  - data types
  for (Int i=0; i<ncols; i++) {
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
  for (Int i=0; i<nrows(); i++) {
    for (Int j=0; j<ncols; j++) {
      Int prec = (j < Int(precision.size())  ?  precision[j] : 0);
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
Bool TableProxy::getColInfo (const String& colName, Bool useBrackets,
                             String& colType, String& message)
{
  Bool good = True;
  ColumnDesc colDesc(table_p.tableDesc().columnDesc (colName));
  // Ignore columns containing Records or variable shaped arrays
  // if not using brackets.
  if (!useBrackets) {
    if (colDesc.dataType() == TpRecord) {
      message += "Column " + colName + " contains Record values.\n";
      good = False;
    } else if (! colDesc.isFixedShape()) {
      message += "Column " + colName +
        " possibly contains variable shaped arrays.\n";
      good = False;
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
      good = False;
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
      for (uInt i=0; i<colShape.size(); ++i) {
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
                                   const String& sep, Int prec,
                                   bool useBrackets) const
{
  Int defPrec = 18;
  switch (vh.dataType()) {
  case TpBool:
    os << vh.asBool();
    break;
  case TpUChar:
  case TpShort:
  case TpInt:
  case TpInt64:
    os << vh.asInt64();
    break;
  case TpFloat:
    defPrec = 9;
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
      Array<Bool> arr = vh.asArrayBool();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<Bool>::const_iterator iterend = arr.end();
        for (Array<Bool>::const_iterator iter=arr.begin();
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
  case TpArrayInt:
  case TpArrayInt64:
    {
      Array<Int64> arr = vh.asArrayInt64();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<Int64>::const_iterator iterend = arr.end();
        for (Array<Int64>::const_iterator iter=arr.begin();
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
  case TpArrayDouble:
    {
      // set precision; set it back at the end.
      if (prec <= 0) prec = defPrec;
      streamsize oldPrec = os.precision(prec);
      Array<Double> arr = vh.asArrayDouble();
      if (useBrackets) {
        printArray (arr, os, sep);
      } else {
        Array<Double>::const_iterator iterend = arr.end();
        for (Array<Double>::const_iterator iter=arr.begin();
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
    uInt ndim = shp.size();
    IPosition pos(shp.size(), 0);
    typename Array<T>::const_iterator iter = arr.begin();
    uInt i = ndim;
    while (True) {
      for (uInt j=0; j<i; ++j) {
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
			     Bool toMemory,
			     Bool deepCopy,
			     Bool valueCopy,
			     const String& endianFormat,
			     const Record& dminfo,
			     Bool noRows)
{
  Table::EndianFormat endOpt = makeEndianFormat (endianFormat);
  // Always valuecopy if dminfo is not empty or if no rows are copied.
  if (dminfo.nfields() > 0  ||  noRows) {
    valueCopy = True;
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
			   Int startIn,
			   Int startOut,
			   Int nrow)
{
  Table tableOut = out.table();
  if (startOut < 0) {
    startOut = tableOut.nrow();
  }
  nrow = checkRowColumn (table_p, "", startIn, nrow, 1,
			 "TableProxy::copyRows");
  if (startOut > Int(tableOut.nrow())) {
    throw TableError ("TableProxy::copyRows: start output row too high");
  }
  TableCopy::copyRows (tableOut, table_p, startOut, startIn, nrow);
}

void TableProxy::deleteTable (Bool checkSubTables)
{
  if (table_p.isMultiUsed (False)) {
    throw TableError ("Table " + table_p.tableName() +
		      " cannot be deleted; it is used by another process");
  }
  if (checkSubTables) {
    if (table_p.isMultiUsed (True)) {
      throw TableError ("Table " + table_p.tableName() +
			" cannot be deleted;"
			" one of its subtables is used by another process");
    }
  }
  table_p.markForDelete();
}

TableProxy TableProxy::selectRows (const Vector<Int>& rownrs,
				   const String& outName)
{
  // If needed, synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  if (anyLT (rownrs, 0) || anyGE (rownrs, Int(table_p.nrow()))) {
    throw TableError("rownumbers should be >= 1 and <= nrow");
  }
  table_p.unlock();
  // Create a table from the selected rows.
  // Rename it and make it non-scratch if a name is given.
  Vector<uInt> rows(rownrs.nelements());
  convertArray (rows, rownrs);
  Table ntable = table_p(rows);
  if (! outName.empty()) {
    ntable.rename (outName, Table::New);
  }
  // Command succeeded.
  return ntable;
}

void TableProxy::stillSameShape (Int& same, IPosition& shape,
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
    Vector<uInt> rownrs(expr.nrow());
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
      Vector<uShort> vs = expr.getColumnuShort (rownrs);
      Vector<Int> vi(vs.nelements());
      convertArray (vi, vs);
      rec.define ("values", vi);
      break;
    }
    case TpInt:
      rec.define ("values", expr.getColumnInt (rownrs));
      break;
    case TpUInt:
    {
      Vector<uInt> vs = expr.getColumnuInt (rownrs);
      Vector<Int> vi(vs.nelements());
      convertArray (vi, vs);
      rec.define ("values", vi);
      break;
    }
    ///    case TpInt64:
    ///      rec.define ("values", expr.getColumnInt (rownrs));
    ///      break;
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
    Int sameShape = 0;
    IPosition resShape;
    Record res;
    switch (expr.dataType()) {
    case TpBool:
      for (uInt i=0; i<expr.nrow(); i++) {
	Array<Bool> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr);
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpInt64:
      for (uInt i=0; i<expr.nrow(); i++) {
	Array<Int64> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr);
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpDouble:
      for (uInt i=0; i<expr.nrow(); i++) {
	Array<Double> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr);
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpDComplex:
      for (uInt i=0; i<expr.nrow(); i++) {
	Array<DComplex> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr);
        stillSameShape (sameShape, resShape, arr.shape());
      }
      break;
    case TpString:
      for (uInt i=0; i<expr.nrow(); i++) {
	Array<String> arr;
	expr.get (i, arr);
	res.define (String::toString(i), arr);
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
        rec.define ("values", record2Array<Bool>(res));
        break;
      case TpInt64:
        rec.define ("values", record2Array<Int64>(res));
        break;
      case TpDouble:
        rec.define ("values", record2Array<Double>(res));
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

Record TableProxy::getProperties (const String& name, Bool byColumn)
{
  RODataManAccessor acc (table_p, name, byColumn);
  return acc.getProperties();
}

void TableProxy::setProperties (const String& name, Bool byColumn,
                                const Record& properties)
{
  RODataManAccessor acc (table_p, name, byColumn);
  acc.setProperties (properties);
}

Record TableProxy::getTableDescription (Bool actual, Bool cOrder)
{
  // Get the table description.
  TableDesc* tableDescPtr;
  if (actual) {
    tableDescPtr = new TableDesc(table_p.actualTableDesc());
  } else {
    tableDescPtr = new TableDesc(table_p.tableDesc());
  }
  // Return the table description as a record.
  Record rec;
  for (uInt i=0; i<tableDescPtr->ncolumn(); i++) {
    const ColumnDesc& columnDescription = tableDescPtr->columnDesc(i);
    rec.defineRecord (columnDescription.name(), 
		      recordColumnDesc (columnDescription, cOrder));
  }
  rec.defineRecord ("_define_hypercolumn_", recordHCDesc (*tableDescPtr));
  delete tableDescPtr;
  return rec;
}

Record TableProxy::getColumnDescription (const String& columnName,
					 Bool actual, Bool cOrder)
{
  // Get the table description.
  TableDesc* tableDescPtr;
  if (actual) {
    tableDescPtr = new TableDesc(table_p.actualTableDesc());
  } else {
    tableDescPtr = new TableDesc(table_p.tableDesc());
  }
  // Return the column description as a record.
  const ColumnDesc& columnDescription = (*tableDescPtr) [columnName];
  return recordColumnDesc (columnDescription, cOrder);
}

String TableProxy::tableName()
{
  return table_p.tableName();
}

Vector<String> TableProxy::getPartNames (Bool recursive)
{
  return Vector<String>(table_p.getPartNames (recursive));
}

String TableProxy::getAsciiFormat() const
{
  return asciiFormat_p;
}

Record TableProxy::getCalcResult() const
{
  return calcResult_p;
}

String TableProxy::showStructure (Bool showDataMan, Bool showColumns,
                                  Bool showSubTables, Bool sortColumns) const
{
  ostringstream ostr;
  table_p.showStructure (ostr, showDataMan, showColumns, showSubTables,
                         sortColumns);
  return ostr.str();
}

Int TableProxy::nrows()
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  return table_p.nrow();
}

Int TableProxy::ncolumns()
{
  return table_p.tableDesc().ncolumn();
}

Vector<Int> TableProxy::shape()
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  Vector<Int> result(2);
  result(0) = table_p.tableDesc().ncolumn();
  result(1) = table_p.nrow();
  return result;
}

Vector<Int> TableProxy::rowNumbers (TableProxy& other)
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  table_p.unlock();
  Vector<Int> result(table_p.nrow());
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
  for (uInt i=0; i< result.nelements(); i++) {
    result(i) = tabdesc.columnDesc(i).name();
  }
  return result;
}

void TableProxy::setMaximumCacheSize (const String& columnName,
				      Int nbytes)
{
  TableColumn col (table_p, columnName);
  col.setMaximumCacheSize (nbytes);
}

Bool TableProxy::isScalarColumn (const String& columnName)
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
				 Int row)
{
  Int64 nrow = getRowsCheck (columnName, row, 1, 1, "getCell");
  return getValueFromTable (columnName, row, nrow, 1, True);
}

void TableProxy::getCellVH (const String& columnName,
                            Int row, const ValueHolder& vh)
{
  Int64 nrow = getRowsCheck (columnName, row, 1, 1, "getCellVH");
  getValueFromTable (columnName, row, nrow, 1, True, vh);
}

ValueHolder TableProxy::getCellSlice (const String& columnName,
				      Int row,
				      const Vector<Int>& blc,
				      const Vector<Int>& trc,
				      const Vector<Int>& inc)
{
  return getCellSliceIP (columnName, row, blc, trc, inc);
}

void TableProxy::getCellSliceVH (const String& columnName,
                                 Int row,
                                 const Vector<Int>& blc,
                                 const Vector<Int>& trc,
                                 const Vector<Int>& inc,
                                 const ValueHolder& vh)
{
  return getCellSliceVHIP (columnName, row, blc, trc, inc, vh);
}

ValueHolder TableProxy::getCellSliceIP (const String& columnName,
                                        Int row,
                                        const IPosition& blc,
                                        const IPosition& trc,
                                        const IPosition& inc)
{
  Slicer slicer;
  Int64 nrow = getRowsSliceCheck (slicer, columnName, row, 1, 1,
                                  blc, trc, inc, "getCellSlice");
  return getValueSliceFromTable (columnName, slicer, row, nrow, 1, True);
}

void TableProxy::getCellSliceVHIP (const String& columnName,
                                   Int row,
                                   const IPosition& blc,
                                   const IPosition& trc,
                                   const IPosition& inc,
                                   const ValueHolder& vh)
{
  Slicer slicer;
  Int64 nrow = getRowsSliceCheck (slicer, columnName, row, 1, 1,
                                  blc, trc, inc, "getCellSliceVH");
  getValueSliceFromTable (columnName, slicer, row, nrow, 1, True, vh);
}

ValueHolder TableProxy::getColumn (const String& columnName,
				   Int row,
				   Int nrow,
				   Int incr)
{
  Int64 nrows = getRowsCheck (columnName, row, nrow, incr, "getColumn");
  return getValueFromTable (columnName, row, nrows, incr, False);
}

void TableProxy::getColumnVH (const String& columnName,
                              Int row,
                              Int nrow,
                              Int incr,
                              const ValueHolder& vh)
{
  Int64 nrows = getRowsCheck (columnName, row, nrow, incr, "getColumnVH");
  return getValueFromTable (columnName, row, nrows, incr, False, vh);
}

Record TableProxy::getVarColumn (const String& columnName,
				 Int row,
				 Int nrow,
				 Int incr)
{
  Int64 nrows = getRowsCheck (columnName, row, nrow, incr, "getVarColumn");
  TableColumn tabcol (table_p, columnName);
  Record rec;
  char namebuf[16];
  for (Int64 i=0; i<nrows; i++) {
    // Add the result to the record with field name formed from 1-based rownr.
    sprintf (namebuf, "r%i", row+1);
    if (tabcol.isDefined(row)) {
      getValueFromTable(columnName, row, 1, 1, False).toRecord (rec, namebuf);
    } else {
      ////      rec.add (namebuf, GlishValue::getUnset());
      rec.define (namebuf, False);
    }
    row += incr;
  }
  return rec;
}

ValueHolder TableProxy::getColumnSlice (const String& columnName,
					Int row,
					Int nrow,
					Int incr,
					const Vector<Int>& blc,
					const Vector<Int>& trc,
					const Vector<Int>& inc)
{
  return getColumnSliceIP (columnName, blc, trc, inc, row, nrow, incr);
}

ValueHolder TableProxy::getColumnSliceIP (const String& columnName,
					  const IPosition& blc,
					  const IPosition& trc,
					  const IPosition& inc,
					  Int row,
					  Int nrow,
					  Int incr)
{
  Slicer slicer;
  Int64 nrows = getRowsSliceCheck (slicer, columnName, row, nrow, incr,
                                   blc, trc, inc, "getColumnSlice");
  return getValueSliceFromTable (columnName, slicer, row, nrows, incr, False);
}

void TableProxy::getColumnSliceVH (const String& columnName,
                                   Int row,
                                   Int nrow,
                                   Int incr,
                                   const Vector<Int>& blc,
                                   const Vector<Int>& trc,
                                   const Vector<Int>& inc,
                                   const ValueHolder& vh)
{
  getColumnSliceVHIP (columnName, blc, trc, inc, row, nrow, incr, vh);
}

void TableProxy::getColumnSliceVHIP (const String& columnName,
                                     const IPosition& blc,
                                     const IPosition& trc,
                                     const IPosition& inc,
                                     Int row,
                                     Int nrow,
                                     Int incr,
                                     const ValueHolder& vh)
{
  Slicer slicer;
  Int64 nrows = getRowsSliceCheck (slicer, columnName, row, nrow, incr,
                                   blc, trc, inc, "getColumnSliceVH");
  getValueSliceFromTable (columnName, slicer, row, nrows, incr, False, vh);
}

void TableProxy::putColumn (const String& columnName,
			    Int row,
			    Int nrow,
			    Int incr,
			    const ValueHolder& value)
{
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  nrow = checkRowColumn (table_p, columnName, row, nrow, incr,
			 "TableProxy::putColumn");
  putValueInTable (columnName, row, nrow, incr, False, value);
}

void TableProxy::putVarColumn (const String& columnName,
			       Int row,
			       Int nrow,
			       Int incr,
			       const Record& values)
{
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  nrow = checkRowColumn (table_p, columnName, row, nrow, incr,
			 "TableProxy::putVarColumn");
  if (Int(values.nfields()) != nrow) {
    throw TableError("TableProxy::putVarColumn: "
		     "#rows mismatches #elem in value");
  }
  for (Int i=0; i<nrow; i++) {
    putValueInTable (columnName, row, 1, 1, False,
		     ValueHolder::fromRecord(values, i));
    row += incr;
  }
}

void TableProxy::putColumnSlice (const String& columnName,
				 Int row,
				 Int nrow,
				 Int incr,
				 const Vector<Int>& blc,
				 const Vector<Int>& trc,
				 const Vector<Int>& inc,
				 const ValueHolder& value)
{
  putColumnSliceIP (columnName, value, blc, trc, inc, row, nrow, incr);
}

void TableProxy::putColumnSliceIP (const String& columnName,
				   const ValueHolder& value,
				   const IPosition& blc,
				   const IPosition& trc,
				   const IPosition& inc,
				   Int row,
				   Int nrow,
				   Int incr)
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
  putValueSliceInTable (columnName, slicer, row, nrow, incr, False, value);
}

void TableProxy::putCell (const String& columnName,
			  const Vector<Int>& rownrs,
			  const ValueHolder& value)
{
  // Synchronize table to get up-to-date #rows.
  syncTable (table_p);
  for (uInt i=0; i<rownrs.nelements(); i++) {
    // Check that the row number is within the table bounds.
    Int row = rownrs(i);
    Int nrow = checkRowColumn (table_p, columnName, row, 1, 1,
			       "TableProxy::putColumn");
    putValueInTable (columnName, row, nrow, 1, True, value);
  }
}

void TableProxy::putCellSlice (const String& columnName,
			       Int row,
			       const Vector<Int>& blc,
			       const Vector<Int>& trc,
			       const Vector<Int>& inc,
			       const ValueHolder& value)
{
  putCellSliceIP (columnName, row, value, blc, trc, inc);
}

void TableProxy::putCellSliceIP (const String& columnName,
				 Int row,
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
  Int nrow = checkRowColumn (table_p, columnName, row, 1, 1,
			     "TableProxy::putColumn");
  putValueSliceInTable (columnName, slicer, row, nrow, 1, True, value);
}

Vector<String> TableProxy::getColumnShapeString (const String& columnName,
						 Int rownr,
						 Int nrow,
						 Int incr,
						 Bool cOrder)
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  // Check that the row number is within the table bounds.
  // However, accept a row number equal to nrow when no rows are needed.
  Int tabnrow = table_p.nrow();
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
  Int maxnrow = (tabnrow - rownr + incr - 1) / incr;
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
    Int lastRow(nrow+rownr);
    for (Int i=0; i<nrow && rownr<lastRow; i++) {
      ostringstream os;
      os << fillAxes (col.shape (rownr), cOrder);
      result(i) = os.str();
      rownr += incr;
    }
  }
  table_p.unlock();
  return result;
}

Bool TableProxy::cellContentsDefined (const String& columnName,
				      Int rownr)
{
  TableColumn tabColumn (table_p, columnName);
  return tabColumn.isDefined (rownr);
}

ValueHolder TableProxy::getKeyword (const String& columnName,
				    const String& keywordName,
				    Int keywordIndex)
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
  return getKeyValue (*keySet, fieldid);
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
  return getKeyValues (*keySet);
}

void TableProxy::putKeyword (const String& columnName,
			     const String& keywordName,
			     Int keywordIndex,
			     Bool makeSubRecord,
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
	       False, True, makeSubRecord);
  }
  putKeyValue (*keySet, fieldid, value);
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
  putKeyValues (*keySet, valueSet);
}

void TableProxy::removeKeyword (const String& columnName,
				const String& keywordName,
				Int keywordIndex)
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
	       True, True, False);
  }
  keySet->removeField (fieldid);
}

Vector<String> TableProxy::getFieldNames (const String& columnName,
					  const String& keywordName,
					  Int keywordIndex)
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
  for (uInt i=0; i<result.nelements(); i++) {
    result(i) = desc->name(i);
  }
  return result;
}

void TableProxy::flush (Bool recursive)
{
  table_p.flush (False, recursive);
}

void TableProxy::close()
{
  if (! table_p.isNull()) {
    flush(True);
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
  for (uInt i=0; i<value.nfields(); i++) {
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

Bool TableProxy::isReadable() const
{
  return True;
}

Bool TableProxy::isWritable() const
{
  return table_p.isWritable();
}

void TableProxy::addColumns (const Record& tableDesc,
			     const Record& dminfo,
                             Bool addToParent)
{
  TableDesc tabdesc;
  String message;
  if (! makeTableDesc (tableDesc, tabdesc, message)) {
    throw TableError("addColumns failed: " + message);
  }
  if (dminfo.nfields() > 0) {
    table_p.addColumn (tabdesc, dminfo, addToParent);
  } else {
    for (uInt i=0; i<tabdesc.ncolumn(); i++) {
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

void TableProxy::addRow (Int nrow)
{
  table_p.addRow (nrow);
}

void TableProxy::removeRow (const Vector<Int>& rownrs)
{
  // If needed synchronize table to get up-to-date number of rows.
  syncTable (table_p);
  Vector<uInt> rows(rownrs.nelements());
  convertArray (rows, rownrs);
  table_p.removeRow (rows);
}


Bool TableProxy::makeHC (const Record& gdesc, TableDesc& tabdesc,
			 String& message)
{
  for (uInt i=0; i<gdesc.nfields(); i++) {
    String name = gdesc.name(i);
    const Record& cold = gdesc.asRecord((i));
    if (! cold.isDefined("HCndim")) {
      message = "No HCndim for hypercolumn " + name;
      return False;
    }
    Int ndim = cold.asInt("HCndim");
    Vector<String> dataNames;
    Vector<String> coordNames;
    Vector<String> idNames;
    if (! cold.isDefined("HCdatanames")) {
      message = "No HCdatanames for hypercolumn " + name;
      return False;
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
  return True;
}

Bool TableProxy::makeTableDesc (const Record& gdesc, TableDesc& tabdesc,
				String& message)
{
  uInt nrdone = 0;
  while (nrdone < gdesc.nfields()) {
    String name = gdesc.name(nrdone);
    const Record& cold (gdesc.asRecord(nrdone));
    // _define_hypercolumn must be done at the end.
    // _define_dminfo_ is obsolete and ignored.
    if (name != "_define_hypercolumn_"  &&  name != "_define_dminfo_") {
      if (! cold.isDefined("valueType")) {
	message = "No value type for column " + name;
	return False;
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
      Bool isArray = cold.isDefined("ndim");
      Int ndim;
      Vector<Int> shape;
      if (isArray) {
	ndim = cold.asInt("ndim");
	if (cold.isDefined("shape")) {
	  shape = cold.asArrayInt ("shape");
	}
	Bool cOrder = False;
	if (cold.isDefined("_c_order")) {
	  cOrder = cold.asBool ("_c_order");
	}
	if (! addArrayColumnDesc (tabdesc, valtype, name, comment,
				  dmtype, dmgrp, option,
				  ndim, shape, cOrder, message)) {
	  return False;
	}
      }else{
	if (valtype == "boolean"  ||  valtype == "bool") {
	  tabdesc.addColumn (ScalarColumnDesc<Bool>
			     (name, comment, dmtype, dmgrp, option));
	} else if (valtype == "byte"  ||  valtype == "uchar") {
	  tabdesc.addColumn (ScalarColumnDesc<uChar>
			     (name, comment, dmtype, dmgrp, 0, option));
	} else if (valtype == "short") {
	  tabdesc.addColumn (ScalarColumnDesc<Short>
			     (name, comment, dmtype, dmgrp, 0, option));
	} else if (valtype == "ushort") {
	  tabdesc.addColumn (ScalarColumnDesc<uShort>
			     (name, comment, dmtype, dmgrp, 0, option));
	} else if (valtype == "integer"  ||  valtype == "int") {
	  tabdesc.addColumn (ScalarColumnDesc<Int>
			     (name, comment, dmtype, dmgrp, 0, option));
	} else if (valtype == "uint") {
	  tabdesc.addColumn (ScalarColumnDesc<uInt>
			     (name, comment, dmtype, dmgrp, 0, option));
	} else if (valtype == "float") {
	  tabdesc.addColumn (ScalarColumnDesc<Float>
			     (name, comment, dmtype, dmgrp, option));
	} else if (valtype == "double") {
	  tabdesc.addColumn (ScalarColumnDesc<Double>
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
	  return False;
	}
      }
      // Set maximum string length.
      if (maxlen > 0) {
	tabdesc.rwColumnDesc(nrdone).setMaxLength (maxlen);
      }
      // Define the keywords if needed.
      if (cold.isDefined ("keywords")) {
        putKeyValues (tabdesc.rwColumnDesc(nrdone).rwKeywordSet(),
                      cold.asRecord("keywords"));
      }
    }
    nrdone++;
  }
  if (gdesc.isDefined ("_define_hypercolumn_")) {
    if (! makeHC (gdesc.asRecord("_define_hypercolumn_"), tabdesc, message)) {
      return False;
    }
  }
  return True;
}

Bool TableProxy::addArrayColumnDesc (TableDesc& tabdesc,
				     const String& valtype,
				     const String& name, const String& comment,
				     const String& dmtype, const String& dmgrp,
				     int option,
				     Int ndim, const Vector<Int>& shape,
				     Bool cOrder,
				     String& message)
{
  if (ndim <= 0  &&  shape.nelements() > 0) {
    message = "arrayColumnDesc: shape should not be given when ndim <= 0";
    return False;
  }
  if (ndim > 0  &&  shape.nelements() != 0
  &&  uInt(ndim) != shape.nelements()) {
    message = "arrayColumnDesc: ndim and shape mismatch";
    return False;
  }
  IPosition shp;
  if (shape.nelements() > 0) {
    if (anyLE (shape, 0)) {
      message = "arrayColumnDesc: shape < 0";
      return False;
    }
    shp = fillAxes (IPosition(shape), cOrder);
    option |= ColumnDesc::FixedShape;
  }
  if (valtype == "boolean"  ||  valtype == "bool") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<Bool>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<Bool>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "byte"  ||  valtype == "uchar") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<uChar>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<uChar>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "short") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<Short>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<Short>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "ushort") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<uShort>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<uShort>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "integer"  ||  valtype == "int") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<Int>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<Int>
			 (name, comment, dmtype, dmgrp, ndim, option));
    }
  } else if (valtype == "uint") {
    if (shp.nelements() > 0) {
      tabdesc.addColumn (ArrayColumnDesc<uInt>
			 (name, comment, dmtype, dmgrp, shp, option));
    }else{
      tabdesc.addColumn (ArrayColumnDesc<uInt>
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
    return False;
  }
  return True;
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
  case TpUInt:
    return "uint";
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

Record TableProxy::recordColumnDesc (const ColumnDesc& cold, Bool cOrder)
{
  Record cdesc;
  cdesc.define ("valueType", getTypeStr(cold.dataType()));
  cdesc.define ("dataManagerType", cold.dataManagerType());
  cdesc.define ("dataManagerGroup", cold.dataManagerGroup());
  cdesc.define ("option", Int(cold.options()));
  cdesc.define ("maxlen", Int(cold.maxLength()));
  cdesc.define ("comment", cold.comment());
  if (cold.isArray()) {
    cdesc.define ("ndim", Int(cold.ndim()));
    IPosition shape = fillAxes (cold.shape(), cOrder);
    if (shape.nelements() > 0) {
      Vector<Int> vec(shape.nelements());
      for (uInt i=0; i<shape.nelements(); i++) {
	vec(i) = shape(i);
      }
      cdesc.define ("shape", vec);
    }
    if (cOrder) {
      cdesc.define ("_c_order", cOrder);
    }
  }
  return cdesc;
}

Record TableProxy::recordHCDesc (const TableDesc& tableDesc)
{
  Record rec;
  Vector<String> hcNames = tableDesc.hypercolumnNames();
  for (uInt i=0; i<hcNames.nelements(); i++) {
    Vector<String> dataNames;
    Vector<String> coordNames;
    Vector<String> idNames;
    Int ndim = tableDesc.hypercolumnDesc (hcNames(i), dataNames,
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

Int64 TableProxy::getRowsCheck (const String& columnName,
                                Int64 row, Int64 nrow, Int64 incr,
                                const String& caller)
{
  // Synchronize table to get up-to-date #rows.
  // Check that the row number is within the table bounds.
  syncTable (table_p);
  return checkRowColumn (table_p, columnName, row, nrow, incr, caller);
}

Int64 TableProxy::getRowsSliceCheck (Slicer& slicer,
                                     const String& columnName,
                                     Int64 row, Int64 nrow, Int64 incr,
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

Int TableProxy::checkRowColumn (Table& table,
				const String& colName,
				Int64 rownr, Int64 nrow, Int64 incr,
				const String& caller)
{
  // Check that the row number is within the table bounds.
  // However, accept a row number equal to nrow when no rows are needed.
  Int tabnrow = table.nrow();
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
  Int maxnrow = (tabnrow - rownr + incr - 1) / incr;
  if (nrow < 0  ||  nrow > maxnrow) {
    nrow = maxnrow;
  }
  return nrow;
}


ValueHolder TableProxy::getValueFromTable (const String& colName,
					   Int rownr, Int nrow,
					   Int incr,
					   Bool isCell)
{
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return ValueHolder();
  }
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  Bool isScalar = cdesc.isScalar();
  DataType dtype = cdesc.dataType();
  if (isScalar) {
    switch (dtype) {
    case TpBool: 
      {
	ScalarColumn<Bool> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUChar:
      {
	ScalarColumn<uChar> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpShort:
      {
	ScalarColumn<Short> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUShort:
      {
	ScalarColumn<uShort> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpInt:
      {
	ScalarColumn<Int> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{ 
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUInt:
      {
	ScalarColumn<uInt> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpFloat:
      {
	ScalarColumn<Float> ac(table_p,colName); 
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpDouble:
      {
	ScalarColumn<Double> ac(table_p,colName); 
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
	  return ValueHolder (getKeyValues(ac(rownr)));
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
	ArrayColumn<Bool> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUChar:
      {
	ArrayColumn<uChar> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpShort:
      {
	ArrayColumn<Short> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUShort:
      {
	ArrayColumn<uShort> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpInt:
      {
	ArrayColumn<Int> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpUInt:
      {
	ArrayColumn<uInt> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpFloat:
      {
	ArrayColumn<Float> ac(table_p,colName);
	if (isCell) {
	  return ValueHolder (ac(rownr));
	}else{
	  return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr)));
	}
      }
      break;
    case TpDouble:
      {
	ArrayColumn<Double> ac(table_p,colName);
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
                                    Int rownr, Int nrow,
                                    Int incr,
                                    Bool isCell,
                                    const ValueHolder& vh)
{
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  Bool isScalar = cdesc.isScalar();
  DataType dtype = cdesc.dataType();
  if (isScalar && isCell) {
    throw TableError("A scalar value cannot be read into a python variable");
  }
  switch (vh.dataType()) {
  case TpArrayBool:
    {
      Array<Bool> arr(vh.asArrayBool());
      if (dtype != TpBool) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayBool();
      } else if (isScalar) {
        // Read directly into the array (thus into Python ndarray object).
        ScalarColumn<Bool> ac(table_p, colName);
        Vector<Bool> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<Bool> ac(table_p, colName);
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
      Array<Int> arr(vh.asArrayInt());
      if (dtype != TpInt) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayInt();
      } else if (isScalar) {
        ScalarColumn<Int> ac(table_p, colName);
        Vector<Int> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<Int> ac(table_p, colName);
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
      Array<Float> arr(vh.asArrayFloat());
      if (dtype != TpFloat) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayFloat();
      } else if (isScalar) {
        ScalarColumn<Float> ac(table_p, colName);
        Vector<Float> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<Float> ac(table_p, colName);
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
      Array<Double> arr(vh.asArrayDouble());
      if (dtype != TpDouble) {
        // Data has to be converted, so cannot be read directly into the array.
        arr = getValueFromTable (colName, rownr, nrow, incr, isCell).
          asArrayDouble();
      } else if (isScalar) {
        ScalarColumn<Double> ac(table_p, colName);
        Vector<Double> vec(arr);
        ac.getColumnRange(Slice(rownr, nrow, incr), vec);
      } else {
        ArrayColumn<Double> ac(table_p, colName);
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
						Int rownr, Int nrow, Int incr,
						Bool isCell)
{
  // Check that the column is an array.
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  if (! cdesc.isArray()) {
    throw TableError ("TableProxy::getColumnSlice: column " +
		      String(colName) + " is not an array column");
  }
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return ValueHolder();
  }
  switch(cdesc.dataType()) {
  case TpBool:
    {
      ArrayColumn<Bool> ac(table_p,colName);
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
      ArrayColumn<uChar> ac(table_p,colName);
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
      ArrayColumn<Short> ac(table_p,colName);
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
      ArrayColumn<uShort> ac(table_p,colName);
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
      ArrayColumn<Int> ac(table_p,colName);
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
      ArrayColumn<uInt> ac(table_p,colName);
      if (isCell) {
	return ValueHolder (ac.getSlice(rownr, slicer));
      }else{
	return ValueHolder (ac.getColumnRange(Slice(rownr, nrow, incr),
					      slicer));
      }
    }
    break;
  case TpFloat:
    {
      ArrayColumn<Float> ac(table_p,colName);
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
      ArrayColumn<Double> ac(table_p,colName);
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
                                         Int rownr, Int nrow, Int incr,
                                         Bool isCell,
                                         const ValueHolder& vh)
{
  // Check that the column is an array.
  const ColumnDesc& cdesc = table_p.tableDesc().columnDesc(colName);
  if (! cdesc.isArray()) {
    throw TableError ("TableProxy::getColumnSlice: column " +
		      String(colName) + " is not an array column");
  }
  DataType dtype = cdesc.dataType();
  switch (vh.dataType()) {
  case TpArrayBool:
    {
      Array<Bool> arr(vh.asArrayBool());
      if (dtype != TpBool) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayBool();
      } else {
        ArrayColumn<Bool> ac(table_p, colName);
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
      Array<Int> arr(vh.asArrayInt());
      if (dtype != TpInt) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayInt();
      } else {
        ArrayColumn<Int> ac(table_p, colName);
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
      Array<Float> arr(vh.asArrayFloat());
      if (dtype != TpFloat) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayFloat();
      } else {
        ArrayColumn<Float> ac(table_p, colName);
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
      Array<Double> arr(vh.asArrayDouble());
      if (dtype != TpDouble) {
        // Data has to be converted; cannot be read directly into the array.
        arr = getValueSliceFromTable (colName, slicer, rownr, nrow,
                                      incr, isCell).asArrayDouble();
      } else {
        ArrayColumn<Double> ac(table_p, colName);
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
				  Int rownr, Int nrow, Int incr,
				  Bool isCell, const ValueHolder& value)
{
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return;
  }
  Bool isScalar = table_p.tableDesc().columnDesc(colName).isScalar();
  DataType type = table_p.tableDesc().columnDesc(colName).dataType();
  if (isScalar) {
    switch (type) {
    case TpBool: 
      {
	ScalarColumn<Bool> col(table_p, colName);
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
	ScalarColumn<uChar> col(table_p, colName);
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
	ScalarColumn<Short> col(table_p, colName);
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
	ScalarColumn<uShort> col(table_p, colName);
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
	ScalarColumn<Int> col(table_p, colName);
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
	ScalarColumn<uInt> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asuInt());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuInt());
	}
      }
      break;
    case TpFloat:
      {
	ScalarColumn<Float> col(table_p, colName);
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
	ScalarColumn<Double> col(table_p, colName);
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
	  putKeyValues (rec, value.asRecord());
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
	ArrayColumn<Bool> col(table_p, colName);
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
	ArrayColumn<uChar> col(table_p, colName);
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
	ArrayColumn<Short> col(table_p, colName);
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
	ArrayColumn<uShort> col(table_p, colName);
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
	ArrayColumn<Int> col(table_p, colName);
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
	ArrayColumn<uInt> col(table_p, colName);
	if (isCell) {
	  col.put (rownr, value.asArrayuInt());
	}else{
	  col.putColumnRange (Slice(rownr, nrow, incr),
			      value.asArrayuInt());
	}
      }
      break;
    case TpFloat:
      {
	ArrayColumn<Float> col(table_p, colName);
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
	ArrayColumn<Double> col(table_p, colName);
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
				       Int rownr, Int nrow, Int incr,
				       Bool isCell, const ValueHolder& value)
{
  // Exit immediately if no rows have to be done.
  if (nrow == 0) {
    return;
  }
  switch (table_p.tableDesc().columnDesc(colName).dataType()) {
  case TpBool:
    {
      ArrayColumn<Bool> col(table_p, colName);
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
      ArrayColumn<uChar> col(table_p, colName);
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
      ArrayColumn<Short> col(table_p, colName);
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
      ArrayColumn<uShort> col(table_p, colName);
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
      ArrayColumn<Int> col(table_p, colName);
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
      ArrayColumn<uInt> col(table_p, colName);
      if (isCell) {
	col.putSlice (rownr, slicer, value.asArrayuInt());
      }else{
	col.putColumnRange (Slice(rownr, nrow, incr), slicer,
			    value.asArrayuInt());
      }
    }
    break;
  case TpFloat:
    {
      ArrayColumn<Float> col(table_p, colName);
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
      ArrayColumn<Double> col(table_p, colName);
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
  findKeyId (fieldid, ksPtr, keyname, column, True, False, False);
  keySet = ksPtr;
}

void TableProxy::findKeyId (RecordFieldId& fieldid,
			    TableRecord*& keySet,
			    const String& keyname,
			    const String& column,
			    Bool mustExist,
			    Bool change, Bool makeSubRecord)
{
  if (keyname.empty()) {
    throw TableError ("Empty keyword name given");
  }
  // A keyword name can consist of multiple names for a keyword hierarchy.
  Vector<String> keys = stringToVector (keyname, '.');
  String usedName;
  for (uInt i=0; i<keys.nelements(); i++) {
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

Record TableProxy::getKeyValues (const TableRecord& keySet)
{
  Record rec;
  uInt nr = keySet.nfields();
  for (uInt i=0; i<nr; i++) {
    getKeyValue(keySet, i).toRecord (rec, keySet.name(i));
  }
  return rec;
}

ValueHolder TableProxy::getKeyValue (const TableRecord& keySet, 
				     const RecordFieldId& fieldId)
{
  switch (keySet.dataType(fieldId)) {
  case TpBool:
    return ValueHolder (keySet.asBool(fieldId));
  case TpUChar:
    return ValueHolder (keySet.asuChar(fieldId));
  case TpShort:
    return ValueHolder (keySet.asShort(fieldId));
  case TpInt:
    return ValueHolder (keySet.asInt(fieldId));
  case TpUInt:
    return ValueHolder (keySet.asuInt(fieldId));
  case TpInt64:
    return ValueHolder (keySet.asInt64(fieldId));
  case TpFloat:
    return ValueHolder (keySet.asFloat(fieldId));
  case TpDouble:
    return ValueHolder (keySet.asDouble(fieldId));
  case TpComplex:
    return ValueHolder (keySet.asComplex(fieldId));
  case TpDComplex:
    return ValueHolder (keySet.asDComplex(fieldId));
  case TpString:
    return ValueHolder (keySet.asString(fieldId));
  case TpArrayBool:
    return ValueHolder (keySet.asArrayBool(fieldId));
  case TpArrayUChar:
    return ValueHolder (keySet.asArrayuChar(fieldId));
  case TpArrayShort:
    return ValueHolder (keySet.asArrayShort(fieldId));
  case TpArrayInt:
    return ValueHolder (keySet.asArrayInt(fieldId));
  case TpArrayUInt:
    return ValueHolder (keySet.asArrayuInt(fieldId));
  case TpArrayInt64:
    return ValueHolder (keySet.asArrayInt64(fieldId));
  case TpArrayFloat:
    return ValueHolder (keySet.asArrayFloat(fieldId));
  case TpArrayDouble:
    return ValueHolder (keySet.asArrayDouble(fieldId));
  case TpArrayComplex:
    return ValueHolder (keySet.asArrayComplex(fieldId));
  case TpArrayDComplex:
    return ValueHolder (keySet.asArrayDComplex(fieldId));
  case TpArrayString:
    return ValueHolder (keySet.asArrayString(fieldId));
  case TpTable:
    return ValueHolder ("Table: "+keySet.tableAttributes(fieldId).name());
  case TpRecord:
    return ValueHolder (getKeyValues(keySet.subRecord(fieldId)));
  default:
    throw (AipsError ("TableProxy::getKeyword: unknown data type"));
  }
}

void TableProxy::putKeyValues (TableRecord& keySet, const Record& valueSet)
{
  for (uInt i=0; i<valueSet.nfields(); i++) {
    putKeyValue (keySet, valueSet.name(i),
		 ValueHolder::fromRecord(valueSet, i));
  }
}

void TableProxy::putKeyValue (TableRecord& keySet, 
			      const RecordFieldId& fieldId,
			      const ValueHolder& value)
{
  switch (value.dataType()) {
  case TpBool:
    keySet.define (fieldId, value.asBool());
    break;
  case TpArrayBool:
    keySet.define (fieldId, value.asArrayBool());
    break;
  case TpUChar:
    keySet.define (fieldId, value.asuChar());
    break;
  case TpArrayUChar:
    keySet.define (fieldId, value.asArrayuChar());
    break;
  case TpShort:
    keySet.define (fieldId, value.asShort());
    break;
  case TpArrayShort:
    keySet.define (fieldId, value.asArrayShort());
    break;
  case TpInt:
    keySet.define (fieldId, value.asInt());
    break;
  case TpArrayInt:
    keySet.define (fieldId, value.asArrayInt());
    break;
  case TpUInt:
    keySet.define (fieldId, value.asuInt());
    break;
  case TpArrayUInt:
    keySet.define (fieldId, value.asArrayuInt());
    break;
  case TpInt64:
    keySet.define (fieldId, value.asInt64());
    break;
  case TpArrayInt64:
    keySet.define (fieldId, value.asArrayInt64());
    break;
  case TpFloat:
    keySet.define (fieldId, value.asFloat());
    break;
  case TpArrayFloat:
    keySet.define (fieldId, value.asArrayFloat());
    break;
  case TpDouble:
    keySet.define (fieldId, value.asDouble());
    break;
  case TpArrayDouble:
    keySet.define (fieldId, value.asArrayDouble());
    break;
  case TpComplex:
    keySet.define (fieldId, value.asComplex());
    break;
  case TpArrayComplex:
    keySet.define (fieldId, value.asArrayComplex());
    break;
  case TpDComplex:
    keySet.define (fieldId, value.asDComplex());
    break;
  case TpArrayDComplex:
    keySet.define (fieldId, value.asArrayDComplex());
    break;
  case TpString:
    {
      String val = value.asString();
      if (val.index("Table: ") == 0  &&  Table::isReadable (val.from(7))) {
	Table tab(val.from(7));
	keySet.defineTable (fieldId, tab);
      } else {
	keySet.define (fieldId, val);
      }
    }
    break;
  case TpArrayString:
    keySet.define (fieldId, value.asArrayString());
    break;
  case TpRecord:
    {
      TableRecord trec;
      putKeyValues (trec, value.asRecord());
      keySet.defineRecord (fieldId, trec);
    }
    break;
  default:
    throw (AipsError ("TableProxy::putKeyValue - "
		      "cannot handle given keyword type"));
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
  for (uInt i=0; i<vec.nelements(); i++) {
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

IPosition TableProxy::fillAxes (const IPosition& ipos, Bool cOrder)
{
  IPosition s(ipos);
  Int nd = s.size();
  if (cOrder  &&  nd > 1) {
    for (Int i=0; i<nd; i++) {
      s[i] = ipos[nd-i-1];
    }
  }
  return s;
}

} //# NAMESPACE CASACORE - END
