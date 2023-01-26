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

#ifndef MS_MSTABLE_H
#define MS_MSTABLE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations, more could be if they weren't part of the
//# static classes 
class TableRecord;
template <class T> class Block;


// <summary> 
// A struct holding the maps used in MSTable.
// </summary>
struct MSTableMaps
{
  // ColEnum -> name
  std::map<int32_t, String> columnMap_p;
  // ColEnum -> DataType
  std::map<int32_t, int32_t> colDTypeMap_p;
  // ColEnum -> comment string
  std::map<int32_t, String> colCommentMap_p;
  // ColEnum -> UNIT string
  std::map<int32_t, String> colUnitMap_p;
  // ColEnum -> MEASURE_TYPE string
  std::map<int32_t, String> colMeasureTypeMap_p;
  // KeyEnum -> name
  std::map<int32_t, String> keywordMap_p;
  // KeyEnum -> DataType
  std::map<int32_t, int32_t> keyDTypeMap_p;
  // KeyEnum -> comment string
  std::map<int32_t, String> keyCommentMap_p;
  // The required TableDesc
  TableDesc requiredTD_p;

  // Convert a name to a ColEnum or KeyEnum.
  int32_t columnType (const String& name) const
    { return mapType (columnMap_p, name); }
  int32_t keywordType (const String& name) const
    { return mapType (keywordMap_p, name); }
  int32_t mapType (const std::map<int32_t,String>&, const String& name) const;
};


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
// If not an exception is thrown. (Not a good idea!) Nevertheless,
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

template <class MSEnum> class MSTable : public Table 
{
public:
    // Define the column and keyword enuym types.
    typedef typename MSEnum::PredefinedColumns ColEnum;
    typedef typename MSEnum::PredefinedKeywords KeyEnum;
  
    // ColEnum convenience functions
    // <group name=columns>
    // check to see if a column exists
    bool isColumn(ColEnum which) const;

    // check to see if a column is writable
    // <group>
    bool isColumnWritable(ColEnum which) const;
    bool isColumnWritable (const String& columnName) const 
    { return Table::isColumnWritable(columnName); }
    bool isColumnWritable (uint32_t columnIndex) const
    { return Table::isColumnWritable(columnIndex); }
    // </group>

    // Information about scalar vs array of a column
    // <group>
    bool isScalar(ColEnum which) const;
    bool isArray(ColEnum which) const;  
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
				int32_t ndim=-1,const String& refCol="");
    // add a column to a TableDesc, defining the shape and setting
    // the ColumnDesc option (Fixed, Undefined, Direct) 
    // For Measure columns you can define a variable reference column.
    static void addColumnToDesc(TableDesc & tabDesc, ColEnum which,
                                const IPosition& shape, ColumnDesc::Option option,
				const String& refCol="");
    static void addColumnToDesc(MSTableMaps&, ColEnum which,
				int32_t ndim=-1,const String& refCol="");
    // add a column to a TableDesc, defining the shape and setting
    // the ColumnDesc option (Fixed, Undefined, Direct) 
    // For Measure columns you can define a variable reference column.
    static void addColumnToDesc(MSTableMaps&, ColEnum which,
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
    bool isKeyword(KeyEnum which) const;

    // add a keyword to a TableDesc
    // An exception is thrown for an invalid data type.  This indicates a 
    // missing data type in the code..
    // <thrown>
    //   <li> AipsError
    // </thrown>
    static void addKeyToDesc(TableDesc&, KeyEnum key);
    static void addKeyToDesc(MSTableMaps&, KeyEnum key);

    // </group>

    // tableDesc convenience functions
    // <group>
 
    // check that a TableDesc is valid 
    static bool validate(const TableDesc& tabDesc);
 
    // check that the keyword set is valid 
    static bool validate(const TableRecord& tabKeySet);
 
    // validate self (make sure that this MS is valid)
    bool validate() const 
      { return this->isNull() ? false : validate(this->tableDesc());}
 
    // return the required table description
    static const TableDesc& requiredTableDesc();

    // Add the compress option for the given column to the TableDesc.
    // It can only be used for a float or a Complex column.
    // For complex columns the type determines which CompressComplex
    // engine is used. "SD" means that CompressComplexSD is used; otherwise
    // CompressComplex is used.
    static void addColumnCompression (TableDesc& td, ColEnum which,
				      bool autoScale = true,
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
    MSTable (SetupNewTable &newTab, rownr_t nrrow,
	     bool initialize);
    MSTable (SetupNewTable &newTab, const TableLock& lockOptions, rownr_t nrrow,
	     bool initialize);
#ifdef HAVE_MPI
    MSTable (MPI_Comm comm, SetupNewTable &newTab, rownr_t nrrow,
	     bool initialize);
    MSTable (MPI_Comm comm, SetupNewTable &newTab, const TableLock& lockOptions,
             rownr_t nrrow, bool initialize);
#endif // HAVE_MPI
    MSTable (const Table &table);
    MSTable (const MSTable<MSEnum> &other);
    // </group>
    ~MSTable();

    //  Assignment operator, reference semantics
    MSTable& operator=(const MSTable<MSEnum>&);

    // Get the static struct containing all column and keyword mappings.
    static MSTableMaps& getMaps();

    // Define an entry in the column maps
    static void colMapDef(MSTableMaps& maps,
                          ColEnum col,
			  const String& colName,
			  DataType colType,
			  const String& colComment,
			  const String& colUnit="",
			  const String& colMeasureType="");

    // Define an entry in the keyword maps
    static void keyMapDef(MSTableMaps& maps,
                          KeyEnum key,
			  const String& keyName,
			  DataType keyType,
			  const String& keyComment);

    // Return a table that references all columns in this table except for
    // those given in writableColumns, those are empty and writable.
    Table referenceCopy(const String& newTableName, 
			const Block<String>& writableColumns) const;

  // Convert a ColEnum to the actual column name.
  inline static
  const String& columnName(const MSTableMaps& maps,
                           ColEnum which)
    { return maps.columnMap_p.at(which); }

  inline static
  DataType columnDataType(const MSTableMaps& maps,
                          ColEnum which)
    { return DataType(maps.colDTypeMap_p.at(which)); }

  inline static
  const String& columnStandardComment(const MSTableMaps& maps,
                                      ColEnum which)
    { return maps.colCommentMap_p.at(which); }
  
  inline static
  const String& columnUnit(const MSTableMaps& maps,
                           ColEnum which)
    { return maps.colUnitMap_p.at(which); }

  inline static
  const String& columnMeasureType(const MSTableMaps& maps,
                                  ColEnum which)
    { return maps.colMeasureTypeMap_p.at(which); }

  inline static
  const String& keywordName(const MSTableMaps& maps,
                            KeyEnum which)
    { return maps.keywordMap_p.at(which); }

  inline static
  DataType keywordDataType(const MSTableMaps& maps,
                           KeyEnum which)
    { return DataType(maps.keyDTypeMap_p.at(which)); }

  inline static
  const String& keywordStandardComment(const MSTableMaps& maps,
                                       KeyEnum which)
    { return maps.keyCommentMap_p.at(which); }

};


} //# NAMESPACE CASACORE - END


//# Do not instantiate the templates automatically, because it means that the static
//# in function getMaps() might be instantiated in many compilation units.
//# The templates are instantiated in MSTable.cc.
//#ifndef CASACORE_NO_AUTO_TEMPLATES
//#include <casacore/ms/MeasurementSets/MSTable.tcc>
//#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
