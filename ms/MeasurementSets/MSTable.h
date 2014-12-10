//# MSTable.h: A Table to hold astronomical data (a set of Measurements)
//# Copyright (C) 1996,1997,2000,2001,2002
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
//#
//# $Id$

#ifndef MS_MSTABLE_H
#define MS_MSTABLE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/ColumnDesc.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations, more could be if they weren't part of the
//# static classes 
class TableRecord;
template <class T> class Block;

// <summary> 
// A Table intended to hold astronomical data
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">

// <prerequisite>
//   <li> <linkto module="Tables:description">Tables</linkto> module
// </prerequisite>
//
// <etymology>
// The MSTable is the base class for all MeasurementSet Tables, hence the
// name.
// </etymology>
//
// <synopsis> 
// A MSTable is a Table.  Most operations on a MSTable are
// Table operations. See the <linkto module="Tables:description">Tables</linkto> 
// module for a list of those operations.  The member functions provided by this
// class are primarily convenience functions to help users follow the 
// agreed upon column and keyword naming conventions.  They are useful when
// creating a Table following the MSTable conventions from
// scratch as well as when creating the column objects to access those
// columns. All actual MeasurementSet Tables will be derived from this class.
//
// The standard way of accessing
// table columns is through Strings.  Mistakes in typing the column
// name will not be caught at compile time (and may not be caught at
// run time).  We have therefore decided to use an enumeration
// to specify columns so that many mistakes will be caught at compile
// time.  This requires functions to map to and from this enumeration
// to the strings that are ultimately used. 
//
// Upon destruction, the table is checked to see that all
// required columns and keywords are still present.
// If not an exception is thrown.  Nevertheless,
// the table will be flushed to disk if it is writable -
// preserving its state.
//
// </synopsis> 
//
// <example>
// For examples of use, see the MeasurementSet class.
// </example>
//
// <motivation>
// The Table module is more than adequate as a container of data.  
// However, in order for applications to be useful with data from 
// different sources, some conventions need to be adopted in the use 
// of Tables to store data.  The MSTable provides the framework for
// these conventions and conversion functions. The actual definitions
// of columns and keywords are found in the derived classes and their
// "enum" base class (e.g. MSAntenna and MSAntennaEnums).
// </motivation>
//
// <todo asof="1996/2/22">
// <li> referenceCopy() should be more flexible with the storage managers used 
//      for the columns which are not merely references.
// <li> When ForwardColumnEngine is fixed so that it can deal with
//      tables already in the cache, modify the test program.  It may also
//      be necessary to modify referenceCopy().
// </todo>

template <class ColEnum, class KeyEnum> class MSTable : public Table 
{
public:
    // ColEnum convenience functions
    // <group name=columns>
    // check to see if a column exists
    Bool isColumn(ColEnum which) const;

    // check to see if a column is writable
    // <group>
    Bool isColumnWritable(ColEnum which) const;
    Bool isColumnWritable (const String& columnName) const 
    { return Table::isColumnWritable(columnName); }
    Bool isColumnWritable (uInt columnIndex) const
    { return Table::isColumnWritable(columnIndex); }
    // </group>

    // Information about scalar vs array of a column
    // <group>
    Bool isScalar(ColEnum which) const;
    Bool isArray(ColEnum which) const;  
    // </group>

    // Return the UNIT keyword value associated with the specified column
    // <group>
    const String& unit(const String& which) const;
    const String& unit(ColEnum which) const
    { return unit(columnName(which)); }
    // </group>

    // Convert a ColEnum to the actual column name.
    static const String& columnName(ColEnum which);
    // Convert a name to a ColEnum
    static ColEnum columnType(const String &name);
    //  return the data type for a given ColEnum
    static DataType columnDataType(ColEnum which);
    // return the standard comment for a given ColEnum
    static const String& columnStandardComment(ColEnum which);
    // return the UNIT string for a given ColEnum
    static const String& columnUnit(ColEnum which);
    // return the MEASURE_TYPE string for a given ColEnum
    static const String& columnMeasureType(ColEnum which);

    // add a column to a TableDesc
    // An exception is thrown for an invalid data type.  This indicates a 
    // programming error in this class when this occurs.
    // For Array columns you can optionally define the dimension here.
    // For Measure columns you can define a variable reference column.
    // <thrown>
    //   <li> AipsError
    // </thrown>
    static void addColumnToDesc(TableDesc & tabDesc, ColEnum which,
				Int ndim=-1,const String& refCol="");
    // add a column to a TableDesc, defining the shape and setting
    // the ColumnDesc option (Fixed, Undefined, Direct) 
    // For Measure columns you can define a variable reference column.
    static void addColumnToDesc(TableDesc & tabDesc, ColEnum which,
			  const IPosition& shape, ColumnDesc::Option option,
				const String& refCol="");

    // </group>

    // KeyEnum convenience functions
    // <group name=keywords>
    static const String& keywordName(KeyEnum which);
    static KeyEnum keywordType(const String &name);
    static DataType keywordDataType(KeyEnum which);
    static const String& keywordStandardComment(KeyEnum which);

    // check to see if a keyword exists
    Bool isKeyword(KeyEnum which) const;

    // add a keyword to a TableDesc
    // An exception is thrown for an invalid data type.  This indicates a 
    // missing data type in the code..
    // <thrown>
    //   <li> AipsError
    // </thrown>
    static void addKeyToDesc(TableDesc & tabDesc, KeyEnum key);

    // </group>

    // tableDesc convenience functions
    // <group>
 
    // check that a TableDesc is valid 
    static Bool validate(const TableDesc& tabDesc);
 
    // check that the keyword set is valid 
    static Bool validate(const TableRecord& tabKeySet);
 
    // validate self (make sure that this MS is valid)
    Bool validate() const 
      { return this->isNull() ? False : validate(this->tableDesc());}
 
    // return the required table description
    static const TableDesc& requiredTableDesc();

    // Add the compress option for the given column to the TableDesc.
    // It can only be used for a Float or a Complex column.
    // For complex columns the type determines which CompressComplex
    // engine is used. "SD" means that CompressComplexSD is used; otherwise
    // CompressComplex is used.
    static void addColumnCompression (TableDesc& td, ColEnum which,
				      Bool autoScale = True,
				      const String& type = String());

    // </group>

protected:
    // These constructors mirror the Table ones
    // <group name=tableLikeConstructors>
    // Default constructor for use by derived classes
    MSTable ();
    MSTable (const String &tableName, TableOption option);
    MSTable (const String &tableName, const TableLock& lockOptions,
	     TableOption option);
    MSTable (const String &tableName, const String &tableDescName,
	     TableOption option);
    MSTable (const String &tableName, const String &tableDescName,
	     const TableLock& lockOptions, TableOption option);
    MSTable (SetupNewTable &newTab, uInt nrrow,
	     Bool initialize);
    MSTable (SetupNewTable &newTab, const TableLock& lockOptions, uInt nrrow,
	     Bool initialize);
    MSTable (const Table &table);
    MSTable (const MSTable<ColEnum,KeyEnum> &other);
    // </group>
    ~MSTable();

    //  Assignment operator, reference semantics
    MSTable& operator=(const MSTable<ColEnum,KeyEnum>&);
 
    // These are the static ordered maps which contain the above info
    // ColEnum -> name
    static SimpleOrderedMap<Int, String> columnMap_p;
    // ColEnum -> DataType
    static SimpleOrderedMap<Int, Int> colDTypeMap_p;
    // ColEnum -> comment string
    static SimpleOrderedMap<Int, String> colCommentMap_p;
    // ColEnum -> UNIT string
    static SimpleOrderedMap<Int, String> colUnitMap_p;
    // ColEnum -> MEASURE_TYPE string
    static SimpleOrderedMap<Int, String> colMeasureTypeMap_p;
 

    // KeyEnum -> name
    static SimpleOrderedMap<Int, String> keywordMap_p;
    // KeyEnum -> DataType
    static SimpleOrderedMap<Int, Int> keyDTypeMap_p;
    // KeyEnum -> comment string
    static SimpleOrderedMap<Int, String> keyCommentMap_p;

    // The required TableDesc
    //# following fails in static initialization (segm. fault).
    //    static TableDesc requiredTD_p;
    static CountedPtr<TableDesc> requiredTD_p;
 
    // Define an entry in the column maps
    static void colMapDef(ColEnum col,
			  const String& colName,
			  DataType colType,
			  const String& colComment,
			  const String& colUnit="",
			  const String& colMeasureType="");

    // Define an entry in the keyword maps
    static void keyMapDef(KeyEnum key,
			  const String& keyName,
			  DataType keyType,
			  const String& keyComment);

    // Return a table that references all columns in this table except for
    // those given in writableColumns, those are empty and writable.
    Table referenceCopy(const String& newTableName, 
			const Block<String>& writableColumns) const;
};


} //# NAMESPACE CASACORE - END

//# #ifndef CASACORE_NO_AUTO_TEMPLATES
//# #include <casacore/ms/MeasurementSets/MSTable.tcc>
//# #endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
