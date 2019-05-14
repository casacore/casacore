//# DataManager.cc: Storage manager for tables
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2016
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


//# Includes
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/DataMan/MemoryStMan.h>
#include <casacore/tables/DataMan/CompressFloat.h>
#include <casacore/tables/DataMan/CompressComplex.h>
#include <casacore/tables/DataMan/MappedArrayEngine.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/tables/DataMan/BitFlagsEngine.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/casa/Arrays/ArrayBase.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/DynLib.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/stdio.h>                     // for sprintf

#ifdef HAVE_MPI
#ifdef HAVE_ADIOS2
#include <casacore/tables/DataMan/Adios2StMan.h>
#endif
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

DataManager::DataManager()
: nrcol_p       (0),
  seqnr_p       (0),
  asBigEndian_p (False),
  tsmOption_p   (TSMOption::Buffer, 0, 0),
  multiFile_p   (0),
  clone_p       (0)
{
    table_p = new Table;
}

DataManager::~DataManager()
    { delete table_p; }


String DataManager::dataManagerName() const
    { return String(); }

void DataManager::dataManagerInfo (Record& info) const
{
    info.define ("SEQNR", seqnr_p);
    info.defineRecord ("SPEC", dataManagerSpec());
}

Record DataManager::dataManagerSpec() const
    { return Record(); }

Record DataManager::getProperties() const
    { return Record(); }

void DataManager::setProperties (const Record&)
    {}

Bool DataManager::isStorageManager() const
    { return True; }


uInt DataManager::open1 (uInt nrrow, AipsIO& ios)
{
    open (nrrow, ios);
    return nrrow;
}

uInt DataManager::resync1 (uInt nrrow)
{
    resync (nrrow);
    return nrrow;
}

void DataManager::reopenRW()
{}

void DataManager::setMaximumCacheSize (uInt)
{}

void DataManager::showCacheStatistics (ostream&) const
{}

void DataManager::setTsmOption (const TSMOption& tsmOption)
{
  AlwaysAssert (multiFile_p==0, AipsError);
  tsmOption_p = tsmOption;
}

void DataManager::setMultiFile (MultiFileBase* mfile)
{
  multiFile_p = mfile;
  // Only caching can be used with a MultiFile.
  if (multiFile_p) {
    tsmOption_p = TSMOption(TSMOption::Cache, 0, tsmOption_p.maxCacheSizeMB());
  }
}

//# Create a column object for a scalar.
//# Check its data type.
DataManagerColumn* DataManager::createScalarColumn (const String& columnName,
						    int dataType,
						    const String& dataTypeId)
{
    DataManagerColumn* colPtr = makeScalarColumn (columnName, dataType,
						  dataTypeId);
    colPtr->setColumnName (columnName);
    checkDataType (colPtr, columnName, dataType, dataTypeId);
    colPtr->setIsFixedShape (True);
    nrcol_p++;
    return colPtr;
}
//# Creating a column object for an indirect array.
//# Check its data type.
DataManagerColumn* DataManager::createIndArrColumn (const String& columnName,
						    int dataType,
						    const String& dataTypeId)
{
    DataManagerColumn* colPtr = makeIndArrColumn (columnName, dataType,
						  dataTypeId);
    colPtr->setColumnName (columnName);
    checkDataType (colPtr, columnName, dataType, dataTypeId);
    nrcol_p++;
    return colPtr;
}
//# Creating a column object for a direct array.
//# Check its data type.
DataManagerColumn* DataManager::createDirArrColumn (const String& columnName,
						    int dataType,
						    const String& dataTypeId)
{
    DataManagerColumn* colPtr = makeDirArrColumn (columnName, dataType,
						  dataTypeId);
    colPtr->setColumnName (columnName);
    checkDataType (colPtr, columnName, dataType, dataTypeId);
    nrcol_p++;
    return colPtr;
}

void DataManager::checkDataType (const DataManagerColumn* colPtr,
				 const String& columnName,
				 int dataType, const String& dataTypeId) const
{
    if (dataType != colPtr->dataType()) {
	throw (DataManInvDT ("Column " + columnName +
                             " has data type " +
                             String::toString(colPtr->dataTypeId()) +
                             "; expected " + String::toString(dataTypeId)));
    }
    if (dataType == TpOther) {
	if (dataTypeId != colPtr->dataTypeId()) {
	    throw (DataManInvDT ("Column " + columnName
				 + " has data type ID " + colPtr->dataTypeId()
				 + "; expected " + dataTypeId));
        }
    }
}

void DataManager::throwDataTypeOther (const String& columnName,
				      int dataType) const
{
    if (dataType == TpOther) {
	throw (DataManInvOper ("Data manager " + dataManagerType() +
			       " does not support data type TpOther"
			       " (in column " + columnName + ")"));
    }
}


Bool DataManager::hasMultiFileSupport() const
    { return False; }

Bool DataManager::canReallocateColumns() const
    { return False; }
DataManagerColumn* DataManager::reallocateColumn (DataManagerColumn* column)
    { return column; }



//# Compose the keyword name from the given name appended with the
//# sequence number to make the name unique.
String DataManager::keywordName (const String& keyword) const
{
    char strc[8];
    sprintf (strc, "_%i", seqnr_p);
    return keyword + strc;
}

//# Compose the file name from the table name followed by the
//# sequence number to make the name unique.
String DataManager::fileName() const
{
    char strc[8];
    sprintf (strc, ".f%i", seqnr_p);
    return table_p->tableName() + "/table" + strc;
}

ByteIO::OpenOption DataManager::fileOption() const
    { return PlainTable::toAipsIOFoption (table_p->tableOption()); }

Bool DataManager::isRegular() const
    { return True; }

void DataManager::linkToTable (Table& tab)
{
    *table_p = tab;
}

//# Default prepare does nothing.
void DataManager::prepare()
{}


Bool DataManager::canAddRow() const
    { return False; }

Bool DataManager::canRemoveRow() const
    { return False; }

Bool DataManager::canAddColumn() const
    { return False; }

Bool DataManager::canRemoveColumn() const
    { return False; }

Bool DataManager::canRenameColumn() const
    { return True; }

void DataManager::addRow (uInt)
    { throw DataManInvOper ("DataManager::addRow not allowed for "
                            "data manager type " + dataManagerType()); }

void DataManager::removeRow (uInt)
    { throw DataManInvOper ("DataManager::removeRow not allowed for "
                            "data manager type " + dataManagerType()); }

void DataManager::addColumn (DataManagerColumn*)
    { throw DataManInvOper ("DataManager::addColumn not allowed for "
                            "data manager type " + dataManagerType()); }

void DataManager::removeColumn (DataManagerColumn*)
    { throw DataManInvOper ("DataManager::removeColumn not allowed for "
                            "data manager type " + dataManagerType()); }



//# Initialize the static map of "constructors".
// Use a recursive mutex, because loading from a shared library can cause
// a nested lock.
std::map<String,DataManagerCtor>
        DataManager::theirRegisterMap(initRegisterMap());
Mutex DataManager::theirMutex(Mutex::Recursive);

//# Register a mapping.
void DataManager::registerCtor (const String& type, DataManagerCtor func)
{
    ScopedMutexLock lock(theirMutex);
    theirRegisterMap.insert (std::make_pair(type, func));
}

//# Test if the data manager is registered.
Bool DataManager::isRegistered (const String& type)
{
    ScopedMutexLock lock(theirMutex);
    return theirRegisterMap.find(type) != theirRegisterMap.end();
}

//# Get a data manager constructor.
//# Return default function if the data manager is undefined
//# after having tried to load it from a shared library.
DataManagerCtor DataManager::getCtor (const String& type)
{
    ScopedMutexLock lock(theirMutex);
    std::map<String,DataManagerCtor>::const_iterator iter = theirRegisterMap.find (type);
    if (iter != theirRegisterMap.end()) {
        return iter->second;
    }
    // Try to load the data manager from a dynamic library with that name
    // (in lowercase without possible template extension).
    // A < denotes a template name which is discarded.
    // A dot can be used to have a specific library name (so multiple
    // data managers can use the same library).
    String libname(type);
    libname.downcase();
    string::size_type pos = libname.find_first_of (".<");
    if (pos != string::npos) {
        libname = libname.substr (0, pos);
    }
    // Try to load and initialize the dynamic library.
    DynLib dl(libname, string("libcasa_"), CASACORE_STRINGIFY(SOVERSION),
              "register_"+libname, False);
    if (dl.getHandle()) {
        // See if registered now.
        iter = theirRegisterMap.find (type);
        if (iter != theirRegisterMap.end()) {
            return iter->second;
        }
    }
    return unknownDataManager;
}

//# The default "ctor" function for unknown data manager type names.
DataManager* DataManager::unknownDataManager (const String& type,
					      const Record&)
{
    throw DataManUnknownCtor ("Data Manager class " + type +
                              " is not registered\n"
                              "  Check (DY)LD_LIBRARY_PATH matches the"
                              " libraries used during the build of "
                              + type);
    return 0;
}




DataManagerColumn::~DataManagerColumn()
{}

void DataManagerColumn::setMaxLength (uInt)
{}

void DataManagerColumn::setShapeColumn (const IPosition&)
{
    throw DataManInvOper ("setShapeColumn only allowed for FixedShape arrays"
                          " in column " + columnName());
}

void DataManagerColumn::setShape (uInt, const IPosition&)
{
    throw DataManInvOper("setShape only allowed for non-FixedShape arrays"
                         " in column " + columnName());
}

void DataManagerColumn::setShapeTiled (uInt rownr, const IPosition& shape,
				       const IPosition&)
{
    setShape (rownr, shape);
}

// By default the shape is defined (for scalars).
Bool DataManagerColumn::isShapeDefined (uInt)
{
    return True;
}

// The default implementation of ndim is to use the shape.
uInt DataManagerColumn::ndim (uInt rownr)
{
    return shape(rownr).nelements();
}

// The shape of the array in the given row.
IPosition DataManagerColumn::shape (uInt)
{
    return IPosition(0);
}

// The tile shape of the array in the given row.
IPosition DataManagerColumn::tileShape (uInt)
{
    return IPosition(0);
}

Bool DataManagerColumn::canChangeShape() const
{
    return False;
}

Bool DataManagerColumn::canAccessScalarColumn (Bool& reask) const
{
    reask = False;
    return False;
}
Bool DataManagerColumn::canAccessArrayColumn (Bool& reask) const
{
    reask = False;
    return False;
}
Bool DataManagerColumn::canAccessScalarColumnCells (Bool& reask) const
{
    reask = False;
    return False;
}
Bool DataManagerColumn::canAccessArrayColumnCells (Bool& reask) const
{
    reask = False;
    return False;
}
Bool DataManagerColumn::canAccessSlice (Bool& reask) const
{
    reask = False;
    return False;
}
Bool DataManagerColumn::canAccessColumnSlice (Bool& reask) const
{
    reask = False;
    return False;
}


String DataManagerColumn::dataTypeId() const
    { return String(); }

Bool DataManagerColumn::isWritable() const
    { return True; }

void DataManagerColumn::throwGet() const
    { throw (DataManInvOper ("DataManagerColumn::get not allowed in column "
                             + columnName())); }
void DataManagerColumn::throwPut() const
    { throw (DataManInvOper ("DataManagerColumn::put not allowed in column "
                             + columnName())); }


#define DATAMANAGER_GETPUT(T,NM) \
void DataManagerColumn::aips_name2(get,NM) (uInt, T*) \
    { throwGet(); } \
void DataManagerColumn::aips_name2(put,NM) (uInt, const T*) \
    { throwPut(); }

DATAMANAGER_GETPUT(Bool,BoolV)
DATAMANAGER_GETPUT(uChar,uCharV)
DATAMANAGER_GETPUT(Short,ShortV)
DATAMANAGER_GETPUT(uShort,uShortV)
DATAMANAGER_GETPUT(Int,IntV)
DATAMANAGER_GETPUT(uInt,uIntV)
DATAMANAGER_GETPUT(Int64,Int64V)
DATAMANAGER_GETPUT(float,floatV)
DATAMANAGER_GETPUT(double,doubleV)
DATAMANAGER_GETPUT(Complex,ComplexV)
DATAMANAGER_GETPUT(DComplex,DComplexV)
DATAMANAGER_GETPUT(String,StringV)


void DataManagerColumn::getOtherV (uInt, void*)
{
  throw (DataManInvOper ("DataManagerColumn::getOtherV not allowed"
                         " in column " + columnName()));
}
void DataManagerColumn::putOtherV (uInt, const void*)
{
  throw (DataManInvOper ("DataManagerColumn::putOtherV not allowed"
                         " in column " + columnName()));
}

void DataManagerColumn::getScalarColumnV (void*)
{
  throw (DataManInvOper("DataManagerColumn::getScalarColumn not allowed"
                        " in column " + columnName()));
}
void DataManagerColumn::putScalarColumnV (const void*)
{
  throw (DataManInvOper("DataManagerColumn::putScalarColumn not allowed"
                        " in column " + columnName()));
}
void DataManagerColumn::getScalarColumnCellsV (const RefRows& rows, void* dataPtr)
{
  getScalarColumnCellsBase (rows, *static_cast<ArrayBase*>(dataPtr));
}
void DataManagerColumn::putScalarColumnCellsV (const RefRows& rows, const void* dataPtr)
{
  putScalarColumnCellsBase (rows, *static_cast<const ArrayBase*>(dataPtr));
}
uInt DataManagerColumn::getBlockV (uInt, uInt, void*)
{
  throw (DataManInvOper("DataManagerColumn::getBlock not allowed"
                        " in column " + columnName()));
  return 0;
}
void DataManagerColumn::putBlockV (uInt, uInt, const void*)
{
  throw (DataManInvOper("DataManagerColumn::putBlock not allowed"
                        " in column " + columnName()));
}
void DataManagerColumn::getArrayV (uInt, void*)
{
  throw (DataManInvOper("DataManagerColumn::getArray not allowed"
                        " in column " + columnName()));
}
void DataManagerColumn::putArrayV (uInt, const void*)
{
  throw (DataManInvOper("DataManagerColumn::putArray not allowed"
                        " in column " + columnName()));
}
void DataManagerColumn::getArrayColumnV (void* dataPtr)
{
  getArrayColumnBase (*static_cast<ArrayBase*>(dataPtr));
}
void DataManagerColumn::putArrayColumnV (const void* dataPtr)
{
  putArrayColumnBase (*static_cast<const ArrayBase*>(dataPtr));
}
void DataManagerColumn::getArrayColumnCellsV (const RefRows& rows, void* dataPtr)
{
  getArrayColumnCellsBase (rows, *static_cast<ArrayBase*>(dataPtr));
}
void DataManagerColumn::putArrayColumnCellsV (const RefRows& rows, const void* dataPtr)
{
  putArrayColumnCellsBase (rows, *static_cast<const ArrayBase*>(dataPtr));
}
void DataManagerColumn::getSliceV (uInt rownr, const Slicer& slicer, void* dataPtr)
{
  getSliceBase (rownr, slicer, *static_cast<ArrayBase*>(dataPtr));
}
void DataManagerColumn::putSliceV (uInt rownr, const Slicer& slicer, const void* dataPtr)
{
  putSliceBase (rownr, slicer, *static_cast<const ArrayBase*>(dataPtr));
}
void DataManagerColumn::getColumnSliceV (const Slicer& slicer, void* dataPtr)
{
  getColumnSliceBase (slicer, *static_cast<ArrayBase*>(dataPtr));
}
void DataManagerColumn::putColumnSliceV (const Slicer& slicer, const void* dataPtr)
{
  putColumnSliceBase (slicer, *static_cast<const ArrayBase*>(dataPtr));
}
void DataManagerColumn::getColumnSliceCellsV (const RefRows& rows,
					      const Slicer& slicer, void* dataPtr)
{
  getColumnSliceCellsBase (rows, slicer, *static_cast<ArrayBase*>(dataPtr));
}
void DataManagerColumn::putColumnSliceCellsV (const RefRows& rows,
					      const Slicer& slicer, const void* dataPtr)
{
  putColumnSliceCellsBase (rows, slicer, *static_cast<const ArrayBase*>(dataPtr));
}


#define DATAMANAGERCOLUMN_GETCELLS(T,TV)         \
{                                                \
  Vector<T>& vec = static_cast<Vector<T>&>(arr); \
  RefRowsSliceIter iter(rownrs);                 \
  uInt i=0;                                      \
  while (! iter.pastEnd()) {                     \
    uInt rownr = iter.sliceStart();              \
    uInt end   = iter.sliceEnd();                \
    uInt incr  = iter.sliceIncr();               \
    while (rownr <= end) {                       \
      aips_name2(get,TV) (rownr, &(vec[i]));     \
      i++;                                       \
      rownr += incr;                             \
    }                                            \
    iter++;                                      \
  }                                              \
}
#define DATAMANAGERCOLUMN_PUTCELLS(T,TV)         \
{                                                \
  const Vector<T>& vec = static_cast<const Vector<T>&>(arr); \
  RefRowsSliceIter iter(rownrs);                 \
  uInt i=0;                                      \
  while (! iter.pastEnd()) {                     \
    uInt rownr = iter.sliceStart();              \
    uInt end   = iter.sliceEnd();                \
    uInt incr  = iter.sliceIncr();               \
    while (rownr <= end) {                       \
      aips_name2(put,TV) (rownr, &(vec[i]));     \
      i++;                                       \
      rownr += incr;                             \
    }                                            \
    iter++;                                      \
  }                                              \
}

void DataManagerColumn::getScalarColumnCellsBase (const RefRows& rownrs,
                                                  ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_GETCELLS(Bool,BoolV)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_GETCELLS(uChar,uCharV)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_GETCELLS(Short,ShortV)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_GETCELLS(uShort,uShortV)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_GETCELLS(Int,IntV)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_GETCELLS(uInt,uIntV)
    break;
  case TpInt64:
    DATAMANAGERCOLUMN_GETCELLS(Int64,Int64V)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_GETCELLS(float,floatV)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_GETCELLS(double,doubleV)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_GETCELLS(Complex,ComplexV)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_GETCELLS(DComplex,DComplexV)
    break;
  case TpString:
    DATAMANAGERCOLUMN_GETCELLS(String,StringV)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::getScalarColumnCellsV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::putScalarColumnCellsBase (const RefRows& rownrs,
                                                  const ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_PUTCELLS(Bool,BoolV)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_PUTCELLS(uChar,uCharV)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_PUTCELLS(Short,ShortV)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_PUTCELLS(uShort,uShortV)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_PUTCELLS(Int,IntV)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_PUTCELLS(uInt,uIntV)
    break; 
  case TpInt64:
    DATAMANAGERCOLUMN_PUTCELLS(Int64,Int64V)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_PUTCELLS(float,floatV)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_PUTCELLS(double,doubleV)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_PUTCELLS(Complex,ComplexV)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_PUTCELLS(DComplex,DComplexV)
    break;
  case TpString:
    DATAMANAGERCOLUMN_PUTCELLS(String,StringV)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::putScalarColumnCellsV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::getArrayColumnBase (ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    getArrayV (row, &(iter->getArray()));
    iter->next();
  }
}
void DataManagerColumn::putArrayColumnBase (const ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    putArrayV (row, &(iter->getArray()));
    iter->next();
  }
}
void DataManagerColumn::getArrayColumnCellsBase (const RefRows& rows, ArrayBase& arr)
{
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      getArrayV (row, &(iter->getArray()));
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::putArrayColumnCellsBase (const RefRows& rows,
                                                 const ArrayBase& arr)
{
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      putArrayV (row, &(iter->getArray()));
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::getSliceArr (uInt row, const Slicer& section,
                                     CountedPtr<ArrayBase>& fullArr,
                                     ArrayBase& arr)
{
  IPosition shp = shape(row);
  if (shp.isEqual (arr.shape())) {
    getArrayV (row, &arr);
  } else {
    if (! shp.isEqual (fullArr->shape())) {
      fullArr->resize (shp);
    }
    getArrayV (row, fullArr.get());
    arr.assignBase (*(fullArr->getSection (section)));
  }
}
void DataManagerColumn::putSliceArr (uInt row, const Slicer& section,
                                     CountedPtr<ArrayBase>& fullArr,
                                     const ArrayBase& arr)
{
  IPosition shp = shape(row);
  if (shp.isEqual (arr.shape())) {
    putArrayV (row, &arr);
  } else {
    if (! shp.isEqual (fullArr->shape())) {
      fullArr->resize (shp);
    }
    getArrayV (row, fullArr.get());
    (fullArr->getSection(section))->assignBase (arr);
    putArrayV (row, fullArr.get());
  }
}
void DataManagerColumn::getSliceBase (uInt row, const Slicer& section,
                                      ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  getSliceArr (row, section, fullArr, arr);
}
void DataManagerColumn::putSliceBase (uInt row, const Slicer& section,
                                      const ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  putSliceArr (row, section, fullArr, arr);
}
void DataManagerColumn::getColumnSliceBase (const Slicer& section, ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    getSliceArr (row, section, fullArr, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::putColumnSliceBase (const Slicer& section,
                                            const ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    putSliceArr (row, section, fullArr, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::getColumnSliceCellsBase (const RefRows& rows,
                                                 const Slicer& section,
                                                 ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      getSliceArr (row, section, fullArr, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::putColumnSliceCellsBase (const RefRows& rows,
                                                 const Slicer& section,
                                                 const ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      putSliceArr (row, section, fullArr, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}


//# Register all mappings of the names of classes derived from
//# DataManager to a static function calling the default constructor.
//# The class name is the name as returned by the function dataManagerType.
// No locking since private and only called by ctor of static member init.
std::map<String,DataManagerCtor> DataManager::initRegisterMap()
{
  std::map<String,DataManagerCtor> regMap;

  theirRegisterMap.insert (std::make_pair("StManAipsIO",      StManAipsIO::makeObject));
  theirRegisterMap.insert (std::make_pair("StandardStMan",    StandardStMan::makeObject));
  theirRegisterMap.insert (std::make_pair("IncrementalStMan", IncrementalStMan::makeObject));
  theirRegisterMap.insert (std::make_pair("TiledDataStMan",   TiledDataStMan::makeObject));
  theirRegisterMap.insert (std::make_pair("TiledCellStMan",   TiledCellStMan::makeObject));
  theirRegisterMap.insert (std::make_pair("TiledColumnStMan", TiledColumnStMan::makeObject));
  theirRegisterMap.insert (std::make_pair("TiledShapeStMan",  TiledShapeStMan::makeObject));
  theirRegisterMap.insert (std::make_pair("MemoryStMan",      MemoryStMan::makeObject));
#ifdef HAVE_MPI
#ifdef HAVE_ADIOS2
  theirRegisterMap.insert (std::make_pair("Adios2StMan",      Adios2StMan::makeObject));
#endif
#endif
  theirRegisterMap.insert (std::make_pair(CompressFloat::className(),
                                          CompressFloat::makeObject));
  theirRegisterMap.insert (std::make_pair(CompressComplex::className(),
                                          CompressComplex::makeObject));
  theirRegisterMap.insert (std::make_pair(CompressComplexSD::className(),
                                          CompressComplexSD::makeObject));
  theirRegisterMap.insert (std::make_pair(MappedArrayEngine<Complex,DComplex>::className(),
                                          MappedArrayEngine<Complex,DComplex>::makeObject));
  theirRegisterMap.insert (std::make_pair(ForwardColumnEngine::className(),
                                          ForwardColumnEngine::makeObject));
  theirRegisterMap.insert (std::make_pair(VirtualTaQLColumn::className(),
                                          VirtualTaQLColumn::makeObject));
  theirRegisterMap.insert (std::make_pair(BitFlagsEngine<uChar>::className(),
                                          BitFlagsEngine<uChar>::makeObject));
  theirRegisterMap.insert (std::make_pair(BitFlagsEngine<Short>::className(),
                                          BitFlagsEngine<Short>::makeObject));
  theirRegisterMap.insert (std::make_pair(BitFlagsEngine<Int>::className(),
                                          BitFlagsEngine<Int>::makeObject));

  return regMap;
}

} //# NAMESPACE CASACORE - END

