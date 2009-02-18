//# TableProxy.h: High-level interface to tables
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2005
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

#ifndef TABLES_TABLEPROXY_H
#define TABLES_TABLEPROXY_H


//# Includes
#include <casa/aips.h>
#include <tables/Tables/Table.h>
#include <casa/Containers/Record.h>
#include <casa/Arrays/Vector.h>
#include <vector>

#include <casa/namespace.h>

//# Forward Declarations
namespace casa { //# NAMESPACE CASA - BEGIN
  class ValueHolder;
  class RecordFieldId;
  class Table;
  class TableLock;
  class ColumnDesc;
  class TableExprNode;
  template<class T> class Vector;
  class Slicer;


// <summary>
// High-level interface to tables
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/09/15" tests="tgtable.g" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> class Table
//   <li> class GlishTableHolder
//   <li> class GlishTableIteratorHolder
//   <li> class GlishTableRowHolder
//   <li> program gtable
//   <li> script gtable.g
// </prerequisite>

// <etymology>
// TableProxy is a proxy for access to tables from any script.
// </etymology>

// <synopsis> 
// TableProxy gives access to most of the functionality in the Table System.
// It is primarily meant to be used in classes that wrap access to it
// from scripting languages (like Glish and Python).
// However, it can also be used directly from other C++ code.
//
// It has functions to open, create, read, write, and query tables.
// Accompying proxy classes give access to other functionality. They are:
// <ul>
//  <li> <linkto class=TableIterProxy>TableIterProxy</linkto> for iteration
//       through a table using class
//       <linkto class=TableIterator>TableIterator</linkto>.
//  <li> <linkto class=TableRowProxy>TableRowProxy</linkto> for access to
//       table rows using class <linkto class=TableRow>TableRow</linkto>.
//  <li> <linkto class=TableIndexProxy>TableIterProxy</linkto> for faster
//       indexed access to using classes
//       <linkto class=ColumnsIndex>ColumnsIndex</linkto> and
//       <linkto class=ColumnsIndexArray>ColumnsIndexArray</linkto>.
// </ul>
//
// TableProxy does not have the TableRecord type in its interface, because
// such a type cannot be handled by e.g. Glish or Python. Instead it
// converts TableRecords to/from Records. If a TableRecord contains a field
// with a Table object, it is represented in the Record as a string
// with the value "Table: NAME" where NAME is the table name.
// </synopsis>

// <motivation>
// TableProxy is the Tasking-independent high-level table interface.
// Different front-ends (e.g. GlishTableProxy) can be put on top of it.
// </motivation>

class TableProxy
{
public:
  // Default constructor initializes to not open.
  // This constructor is only needed for containers.
  TableProxy();

  // Create the object from an existing table (used by some methods).
  TableProxy (const Table& table)
    : table_p (table) {}

  // Open the table with a given name.
  TableProxy (const String& tableName,
	      const Record& lockOptions,
	      int option);

  // Create a table with given name and description, etc.
  TableProxy (const String& tableName,
	      const Record& lockOptions,
	      const String& endianFormat,
	      const String& memType,
	      int nrow,
  	      const Record& tableDesc,
  	      const Record& dmInfo);

  // Create a table object to concatenate a number of similar tables.
  // The keyword set of the first table is take as the keyword set of the
  // entire concatenation. However, it can be specified which subtables
  // have to be concatenated as well which means that for each subtable name
  // the subtable in the keywordsets are concatenated.
  // <note>For Boost-Python the constructors must have different nr of arguments.
  // Hence some dummy arguments are added.
  //</note>
  // <group>
  TableProxy (const Vector<String>& tableNames,
	      const Vector<String>& concatenateSubTableNames, 
	      const Record& lockOptions,
	      int option);
  TableProxy (const std::vector<TableProxy>& tables,
	      const Vector<String>& concatenateSubTableNames,
	      int dummy1=0, int dummy2=0, int dummy3=0);
  // </group>

  // Create a table object from a table command (as defined in TableGram).
  // <br>If a CALC command was given, the resulting values are stored in
  // the a record and a null TableProxy object is returned.
  // The result can be obtained using getCalcResult.
  // <note>
  // If the command string contains no GIVING part, the resulting
  // table is temporary and its name is blank.
  // </note>
  TableProxy (const String& command,
	      const std::vector<TableProxy>& tables);

  // Create a table from an Ascii file.
  // It fills a string containing the names and types
  // of the columns (in the form COL1=R, COL2=D, ...).
  // The string can be obtained using getAsciiFormat.
  TableProxy (const String& fileName,
	      const String& headerName,
	      const String& tableName,
	      Bool autoHeader,
	      const IPosition& autoShape,
	      const String& separator,
	      const String& commentMarker,
	      Int firstLine,
	      Int lastLine,
	      const Vector<String>& columnNames = Vector<String>(),
	      const Vector<String>& dataTypes = Vector<String>());

  // Copy constructor.
  TableProxy (const TableProxy&);

  // Close the table.
  ~TableProxy();

  // Assignment.
  TableProxy& operator= (const TableProxy&);

  // Select the given rows from the table and create a new (reference) table.
  // If outName is not empty, the new table is made persistent with that name.
  TableProxy selectRows (const Vector<Int>& rownrs,
			 const String& outName);

  // Reopen the table for read/write.
  void reopenRW();

  // Resync the table.
  void resync();

  // Flush the table and optionally all its subtables.
  void flush (Bool recursive);

  // Flush and close the table and all its subtables.
  void close();

  // Get the endian format of the table.
  // It fills the result with value "big" or "little".
  String endianFormat() const;

  // Acquire a (read or write) lock on the table.
  void lock (Bool mode, Int nattempts);

  // Release a lock on the table.
  void unlock();

  // Determine if data in the table has changed.
  Bool hasDataChanged();

  // Determine if the process has a read or write lock on the table.
  Bool hasLock (Bool mode);

  // Get the lock options of the table.
  // It fills the record with the fields option, interval and maxwait.
  Record lockOptions();

  // Determine if the table (and optionally its subtables) are in use
  // in another process.
  Bool isMultiUsed (Bool checkSubTables);

  // Write the table to an ASCII file
  // (approximately the inverse of the from-ASCII-contructor).
  // If <src>headerFile</src> is empty or equal to <src>asciiFile</src>, the
  // headers are written in the same file as the data, otherwise in a separate
  // file.
  // If no columns are given (or if the first column name is empty), all
  // table columns are written. However, columns containing records or
  // variable shaped arrays are skipped because they cannot be written.
  // <br>Argument <src>sep</src> is used as separator between the values.
  // If it is empty, a blank is used.
  // <br>For each column the precision can be given. It is only used for
  // columns containing floating point numbers. A value <=0 means using the
  // default which is 7 for single and 16 for double precision.
  // <br>If <src>useBrackets=True</src>, arrays are enclosed in [] (for each
  // dimension), so variable shaped arrays can be read back unambiguously.
  // The type in the header will be something like D[].
  // If False, arrays are written linearly where a shape [4,64] is given in
  // the header like D4,64. In that case arrays should have constant shape.
  String toAscii (const String& asciiFile, 
                  const String& headerFile, 
                  const Vector<String>& columns, 
                  const String& sep,
                  const Vector<Int>& precision,
                  Bool useBrackets);

  // Rename the table
  void rename (const String& newTableName);

  // Copy the table (possibly a deep copy).
  // If noRows=True, an empty table is created.
  TableProxy copy (const String& newTableName,
		   Bool toMemoryTable,
		   Bool deepCopy,
		   Bool valueCopy,
		   const String& endianFormat,
		   const Record& dminfo,
		   Bool noRows);

  // Copy rows from one table to another.
  // If startOut<0, it is set to the end of the output table.
  void copyRows (TableProxy& out,
		 Int startIn,
		 Int startOut,
		 Int nrow);

  // Close and delete the table.
  void deleteTable (Bool checkSubTables);

  // Get the table info of the table.
  Record tableInfo();

  // Put the table info of the table.
  void putTableInfo (const Record& value);

  // Add a line to the TableInfo readme.
  void addReadmeLine (const String& line);

  // Test if a table is readable.
  Bool isReadable() const;

  // Test if a table is writable.
  Bool isWritable() const;

  // Set the maximum cache size for the given column in the
  // table with the given id.
  void setMaximumCacheSize (const String& columnName,
			    Int nbytes);

  // Add one or more columns to the table.
  void addColumns (const Record& tableDesc,
		   const Record& dminfo);

  // Rename a column in the table.
  void renameColumn (const String& nameOld,
		     const String& nameNew);

  // Remove one or more columns from the table.
  void removeColumns (const Vector<String>& columnNames);

  // Add rows to the table.
  void addRow (Int nrow);

  // Remove rows from the table.
  void removeRow (const Vector<Int>& rownrs);

  // Get some or all values from a column in the table.
  // row is the starting row number (0-relative).
  // nrow=-1 means until the end of the table.
  // incr is the step in row number.
  // <group>
  ValueHolder getColumn (const String& columnName,
			 Int row,
			 Int nrow,
			 Int incr);
  Record getVarColumn (const String& columnName,
		       Int row,
		       Int nrow,
		       Int incr);
  // </group>

  // Get some or all value slices from a column in the table.
  // If the inc vector is empty, it defaults to all 1.
  // <group>
  ValueHolder getColumnSlice (const String& columnName,
			      Int row,
			      Int nrow,
			      Int incr,
			      const Vector<Int>& blc,
			      const Vector<Int>& trc,
			      const Vector<Int>& inc);
  ValueHolder getColumnSliceIP (const String& columnName,
				const IPosition& blc,
				const IPosition& trc,
				const IPosition& inc,
				Int row,
				Int nrow,
				Int incr);
  // </group>

  // Put some or all values into a column in the table.
  // row is the starting row number (0-relative).
  // nrow=-1 means until the end of the table.
  // incr is the step in row number.
  // <group>
  void putColumn (const String& columnName,
		  Int row,
		  Int nrow,
		  Int incr,
		  const ValueHolder&);
  void putVarColumn (const String& columnName,
		     Int row,
		     Int nrow,
		     Int incr,
		     const Record& values);
  // </group>

  // Put some or all value slices into a column in the table.
  // <group>
  void putColumnSlice (const String& columnName,
		       Int row,
		       Int nrow,
		       Int incr,
		       const Vector<Int>& blc,
		       const Vector<Int>& trc,
		       const Vector<Int>& inc,
		       const ValueHolder&);
  void putColumnSliceIP (const String& columnName,
			 const ValueHolder&,
			 const IPosition& blc,
			 const IPosition& trc,
			 const IPosition& inc,
			 Int row,
			 Int nrow,
			 Int incr);
  // </group>

  // Tests if the contents of a cell are defined.
  // Only a column with variable shaped arrays can have an empty cell.
  Bool cellContentsDefined (const String& columnName,
			    Int rownr);

  // Get a value from a column in the table.
  ValueHolder getCell (const String& columnName,
		       Int row);

  // Get a value slice from a column in the table.
  // If the inc vector is empty, it defaults to all 1.
  // <group>
  ValueHolder getCellSlice (const String& columnName,
			    Int row,
			    const Vector<Int>& blc,
			    const Vector<Int>& trc,
			    const Vector<Int>& inc);
  ValueHolder getCellSliceIP (const String& columnName,
			      Int row,
			      const IPosition& blc,
			      const IPosition& trc,
			      const IPosition& inc);
  // </group>

  // Put a value into a column in the table.
  void putCell (const String& columnName,
		const Vector<Int>& rownrs,
		const ValueHolder&);

  // Put a value slice into a column in the table.
  // If the inc vector is empty, it defaults to all 1.
  // <group>
  void putCellSlice (const String& columnName,
		     Int row,
		     const Vector<Int>& blc,
		     const Vector<Int>& trc,
		     const Vector<Int>& inc,
		     const ValueHolder&);
  void putCellSliceIP (const String& columnName,
		       Int row,
		       const ValueHolder&,
		       const IPosition& blc,
		       const IPosition& trc,
		       const IPosition& inc);
  // </group>

  // Get the shape of one or more cells in a column as a vector of Strings
  // containing the shapes as [a,b,c].
  // If the shape is fixed, a single String is returned.
  Vector<String> getColumnShapeString (const String& columnName,
				       Int rownr,
				       Int nrow,
				       Int incr,
				       Bool cOrder = False);

  // Get a table or column keyword value in the table.
  // If the columnName is empty, a given keyword is a table keyword.
  // The keyword can be given as a name or a 0-based index.
  ValueHolder getKeyword (const String& columnName,
			  const String& keywordName,
			  Int keywordIndex);

  // Get the table or column keyword values in the table.
  // If the columnName is empty, the table keyword values are returned.
  Record getKeywordSet (const String& columnName);

  // Define a table or column keyword in the table.
  // If the column name is empty, a table keyword is defined.
  // The keyword can be given as a name or a 0-based number.
  // The value should be a record containing the value of the keyword.
  // The value can be any type (including a record).
  void putKeyword (const String& columnName,
		   const String& keywordName,
		   Int keywordIndex,
		   Bool makeSubRecord,
		   const ValueHolder&);

  // Define multiple table or column keywords in the table.
  // If the column name is empty, a table keywords are defined.
  // The value should be a record containing the values of the keywords.
  // The values can be any type (including a record).
  // The field names are the keyword names.
  void putKeywordSet (const String& columnName,
		      const Record& valueSet);

  // Remove a table or column keyword from the table.
  // If the column name is empty, a table keyword is removed.
  void removeKeyword (const String& columnName,
		      const String& keywordName,
		      Int keywordIndex);

  // Get the names of all field in a record in the table.
  // If the column name is empty, the table keywords are used.
  // If the keyword name is empty, the names of all keywords are returned.
  // Otherwise the names of all fields in the keyword value are returned.
  // In that case the value has to be a record.
  Vector<String> getFieldNames (const String& columnName,
				const String& keywordName,
				Int keywordIndex);

  // Get table name.
  String tableName();

  // Get #columns of the table.
  Int ncolumns();

  // Get #rows of the table.
  Int nrows();

  // Get the shape (#columns, #rows) of the table.
  Vector<Int> shape();

  // Get the row numbers of the table.
  Vector<Int> rowNumbers (TableProxy& other);

  // Get all column names in the table.
  Vector<String> columnNames();

  // Return in result if the column contains scalars.
  Bool isScalarColumn (const String& columnName);

  // Return the data type of the column as:
  //  Bool, UChar, Short, UShort, Int, UInt, 
  //  Float, Double, Complex, DComplex, String, Table, or unknown.
  String columnDataType (const String& columnName);

  // Return the type of array in the column as:
  //    Direct
  //    Undefined
  //    FixedShape
  //    Direct,Undefined
  //    Direct,FixedShape
  //    Undefined,FixedShape
  //    Direct,Undefined,FixedShape
  // or Error -- unexpected column type
  String columnArrayType (const String& columnName);

  // Get the data manager info of the table with the given id.
  Record getDataManagerInfo();

  // Get the table description of the table with the given id.
  // It returns a record containing the description.
  Record getTableDescription (Bool actual,         //# use actual description?
			      Bool cOrder=False);

  // Get the column description of a column in the table with the given id.
  // It returns a record containing the description.
  Record getColumnDescription (const String& columnName,
			       Bool actual,        //# use actual description?
			       Bool cOrder=False);

  // Get ascii format string.
  String getAsciiFormat() const;

  // Get result of possible CALC statement.
  Record getCalcResult() const;

  // Return the table object.
  // <group>
  Table& table()
    { return table_p; }
  const Table& table() const
    { return table_p; }
  // </group>

  // Get or put the values of all keywords.
  // Thus convert from TableRecord to/from Record.
  // Keywords containing a table are converted to a string containing
  // the table name preceeded by 'Table: '.
  // <group>
  static Record getKeyValues (const TableRecord& keySet);
  static void putKeyValues (TableRecord& keySet, const Record& valueSet);
  // </group>


private:
  // Get the lock options from the fields in the record.
  // If the record or lockoption is invalid, an exception is thrown.
  static TableLock makeLockOptions (const Record& options);

  // Turn the string into the endian format option.
  // An exception is thrown if the string is invalid.
  static Table::EndianFormat makeEndianFormat (const String& endianFormat);

  // Make hypercolumn definitions for the given hypercolumns.
  static Bool makeHC (const Record& gdesc, TableDesc& tabdesc,
		      String& message);

  // Get the column info for toAscii.
  Bool getColInfo (const String& colName, Bool useBrackets,
                   String& type, String& message);

  // Print the data in a table cell for toAscii.
  // <group>
  void printValueHolder (const ValueHolder& vh, ostream& os,
                         const String& sep, Int prec, Bool useBrackets) const;
  template<typename T>
  void printArray (const Array<T>& arr, ostream& os,
                   const String& sep, const String& quote) const;
  // </group>

  // Check if the column name and row numbers are valid.
  // Return the recalculated nrow so that it does not exceed #rows.
  Int checkRowColumn (Table& table,
		      const String& colName,
		      Int rownr, Int nrow, Int incr,
		      const Char* caller);

  // Get values from the column.
  // Nrow<0 means till the end of the column.
  ValueHolder getValueFromTable (const String& colName, 
				 Int rownr, Int nrow, Int incr,
				 Bool isCell);

  // Get value slices from the column.
  // Nrow<0 means till the end of the column.
  ValueHolder getValueSliceFromTable(const String& colName, 
				     const Slicer& slicer,
				     Int rownr, Int nrow, Int incr,
				     Bool isCell);

  // Put values into the column.
  // Nrow<0 means till the end of the column.
  void putValueInTable (const String& colName,
			Int rownr, Int nrow, Int incr,
			Bool isCell, const ValueHolder&);

  // Put value slices into the column.
  // Nrow<0 means till the end of the column.
  void putValueSliceInTable (const String& colName,
			     const Slicer& slicer,
			     Int rownr, Int nrow, Int incr,
			     Bool isCell, const ValueHolder&);

  // Split the keyname into its separate parts (separator is .).
  // Check if each part exists and is a subrecord (except last part).
  // When putting, subrecords are created if undefined and if
  // makeSubRecord is set.
  // On return it fills in the fieldid with the latest keyword part.
  // KeySet is set to the last subrecord.
  // <group>
  void findKeyId (RecordFieldId& fieldid,
		  const TableRecord*& keySet,
		  const String& keyname,
		  const String& column);
  void findKeyId (RecordFieldId& fieldid,
		  TableRecord*& keySet,
		  const String& keyname,
		  const String& column,
		  Bool mustExist, Bool change, Bool makeSubRecord);
  // </group>

  // Get the value of a keyword.
  static ValueHolder getKeyValue (const TableRecord& keySet,
				  const RecordFieldId& fieldId);

  // Put the value of a keyword.
  static void putKeyValue (TableRecord& keySet,
			   const RecordFieldId& fieldId,
			   const ValueHolder& value);

  // Make a real table description from a table description in a record.
  // An exception is thrown if the record table description is invalid.
  // A record table description is a Record object as returned by
  // getDesc.
  static Bool makeTableDesc (const Record& gdesc, TableDesc& tabdesc,
			     String& message);

  // Add an array column description to the table description.
  // It is used by the function makeDesc.
  static Bool addArrayColumnDesc (TableDesc& tableDesc,
				  const String& valueType,
				  const String& columnName,
				  const String& comment,
				  const String& dataManagerType,
				  const String& dataManagerGroup,
				  int options,
				  Int ndim, const Vector<Int>& shape,
				  Bool cOrder,
				  String& message);

  // Make a record containing the column description.
  static Record recordColumnDesc (const ColumnDesc&, Bool cOrder);

  // Make a record containing the description of all hypercolumns.
  static Record recordHCDesc (const TableDesc& tableDesc);

  // Replace the user-given default value (<0) by the default value
  // used by Slicer (i.e. by Slicer::MimicSource).
  void setDefaultForSlicer (IPosition& vec) const;

  // Calculate the values of a CALC expression and store them in field
  // 'values' in rec.
  static void calcValues (Record& rec, const TableExprNode& expr);

  // Synchronize table if readlocking is in effect.
  // In this way the number of rows is up-to-date.
  void syncTable (Table& table);

  // Get the type string as used externally (in e.g. glish).
  static String getTypeStr (DataType);

  // Optionally reverse the axes.
  static IPosition fillAxes (const IPosition&, Bool cOrder);


  //# The data members.
  Table  table_p;
  String asciiFormat_p;
  Record calcResult_p;
};

} //# NAMESPACE CASA - END

#endif
