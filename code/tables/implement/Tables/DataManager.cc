//# DataManager.cc: Storage manager for tables
//# Copyright (C) 1994,1995,1996,1997,1998
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
#include <aips/Tables/DataManager.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledCellStMan.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/ForwardCol.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/PlainTable.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/String.h>
#include <aips/Tables/DataManError.h>
#include <stdio.h>                     // for sprintf


DataManager::DataManager()
: nrcol_p (0),
  seqnr_p (0)
{
    table_p = new Table;
    if (table_p == 0) {
	throw (AllocError ("DataManager::DataManager", 1));
    }
}

DataManager::~DataManager()
    { delete table_p; }


String DataManager::dataManagerName() const
    { return String(); }

Bool DataManager::isStorageManager() const
    { return True; }


void DataManager::reopenRW()
{}

void DataManager::setMaximumCacheSize (uInt)
{}

//# Create a column object for a scalar.
//# Check its data type.
DataManagerColumn* DataManager::createScalarColumn (const String& columnName,
						    int dataType,
						    const String& dataTypeId)
{
    DataManagerColumn* colPtr = makeScalarColumn (columnName, dataType,
						  dataTypeId);
    checkDataType (colPtr, columnName, dataType, dataTypeId);
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
    checkDataType (colPtr, columnName, dataType, dataTypeId);
    nrcol_p++;
    return colPtr;
}

void DataManager::checkDataType (const DataManagerColumn* colPtr,
				 const String& columnName,
				 int dataType, const String& dataTypeId) const
{
    if (dataType != colPtr->dataType()) {
	throw (DataManInvDT ("Column " + columnName));
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

void DataManager::addRow (uInt)
    { throw (DataManInvOper ("DataManager::addRow not allowed")); }

void DataManager::removeRow (uInt)
    { throw (DataManInvOper ("DataManager::removeRow not allowed")); }

void DataManager::addColumn (DataManagerColumn*)
    { throw (DataManInvOper ("DataManager::addColumn not allowed")); }

void DataManager::removeColumn (DataManagerColumn*)
    { throw (DataManInvOper ("DataManager::removeColumn not allowed")); }


//# Initialize the static map of "constructors".
SimpleOrderedMap<String,DataManagerCtor>
        DataManager::registerMap (DataManager::unknownDataManager);

//# Register a mapping.
void DataManager::registerCtor (const String& type, DataManagerCtor func)
    { registerMap.define (type, func); }

//# Test if the data manager is registered.
Bool DataManager::isRegistered (const String& type)
{
    if (registerMap.isDefined(type)) {
	return True;
    }
    return False;
}

//# Get a data manager constructor.
//# Return default function if the data manager is undefined.
DataManagerCtor DataManager::getCtor (const String& type)
{
    DataManagerCtor* fp = registerMap.isDefined (type);
    if (fp) {
	return *fp;
    }
    return unknownDataManager;
}

//# The default "ctor" function for unknown data manager type names.
DataManager* DataManager::unknownDataManager (const String& type)
{
    throw (DataManUnknownCtor ("Data Manager class " +type+ 
			       " is not registered"));
    return 0;
}




DataManagerColumn::~DataManagerColumn()
{}

void DataManagerColumn::setMaxLength (uInt)
{}

void DataManagerColumn::setShapeColumn (const IPosition&)
{
    throw (DataManInvOper
	             ("setShapeColumn only allowed for FixedShape arrays"));
}

void DataManagerColumn::setShape (uInt, const IPosition&)
{
    throw (DataManInvOper("setShape only allowed for non-FixedShape arrays"));
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
    { throw (DataManInvOper ("DataManagerColumn::get not allowed")); }
void DataManagerColumn::throwPut() const
    { throw (DataManInvOper ("DataManagerColumn::put not allowed")); }


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
DATAMANAGER_GETPUT(float,floatV)
DATAMANAGER_GETPUT(double,doubleV)
DATAMANAGER_GETPUT(Complex,ComplexV)
DATAMANAGER_GETPUT(DComplex,DComplexV)
DATAMANAGER_GETPUT(String,StringV)


void DataManagerColumn::getOtherV (uInt, void*)
  { throw (DataManInvOper ("DataManagerColumn::getOtherV not allowed")); }
void DataManagerColumn::putOtherV (uInt, const void*)
  { throw (DataManInvOper ("DataManagerColumn::putOtherV not allowed")); }

void DataManagerColumn::getScalarColumnV (void*)
  { throw (DataManInvOper("DataManagerColumn::getScalarColumn not allowed")); }
void DataManagerColumn::putScalarColumnV (const void*)
  { throw (DataManInvOper("DataManagerColumn::putScalarColumn not allowed")); }
void DataManagerColumn::getScalarColumnCellsV (const RefRows&, void*)
  { throw (DataManInvOper("DataManagerColumn::getScalarColumnCells not allowed")); }
void DataManagerColumn::putScalarColumnCellsV (const RefRows&, const void*)
  { throw (DataManInvOper("DataManagerColumn::putScalarColumnCells not allowed")); }
uInt DataManagerColumn::getBlockV (uInt, uInt, void*)
{
    throw (DataManInvOper("DataManagerColumn::getBlock not allowed"));
    return 0;
}
void DataManagerColumn::putBlockV (uInt, uInt, const void*)
  { throw (DataManInvOper("DataManagerColumn::putBlock not allowed")); }
void DataManagerColumn::getArrayV (uInt, void*)
  { throw (DataManInvOper("DataManagerColumn::getArray not allowed")); }
void DataManagerColumn::putArrayV (uInt, const void*)
  { throw (DataManInvOper("DataManagerColumn::putArray not allowed")); }
void DataManagerColumn::getArrayColumnV (void*)
  { throw (DataManInvOper("DataManagerColumn::getArrayColumn not allowed")); }
void DataManagerColumn::putArrayColumnV (const void*)
  { throw (DataManInvOper("DataManagerColumn::putArrayColumn not allowed")); }
void DataManagerColumn::getArrayColumnCellsV (const RefRows&, void*)
  { throw (DataManInvOper("DataManagerColumn::getArrayColumnCells not allowed")); }
void DataManagerColumn::putArrayColumnCellsV (const RefRows&, const void*)
  { throw (DataManInvOper("DataManagerColumn::putArrayColumnCells not allowed")); }
void DataManagerColumn::getSliceV (uInt, const Slicer&, void*)
  { throw (DataManInvOper("DataManagerColumn::getSlice not allowed")); }
void DataManagerColumn::putSliceV (uInt, const Slicer&, const void*)
  { throw (DataManInvOper("DataManagerColumn::putSlice not allowed")); }
void DataManagerColumn::getColumnSliceV (const Slicer&, void*)
  { throw (DataManInvOper("DataManagerColumn::getColumnSlice not allowed")); }
void DataManagerColumn::putColumnSliceV (const Slicer&, const void*)
  { throw (DataManInvOper("DataManagerColumn::putColumnSlice not allowed")); }
void DataManagerColumn::getColumnSliceCellsV (const RefRows&,
					      const Slicer&, void*)
  { throw (DataManInvOper("DataManagerColumn::getColumnSliceCells not allowed")); }
void DataManagerColumn::putColumnSliceCellsV (const RefRows&,
					      const Slicer&, const void*)
  { throw (DataManInvOper("DataManagerColumn::putColumnSliceCells not allowed")); }





//# Register all mappings of the names of classes derived from
//# DataManager to a static function calling the default constructor.
//# The class name is the name as returned by the function dataManagerType.
void DataManager::registerAllCtor ()
{
    registerCtor ("StManAipsIO", StManAipsIO::makeObject);
    registerCtor ("IncrementalStMan", IncrementalStMan::makeObject);
    registerCtor ("TiledDataStMan", TiledDataStMan::makeObject);
    registerCtor ("TiledCellStMan", TiledCellStMan::makeObject);
    registerCtor ("TiledColumnStMan", TiledColumnStMan::makeObject);
    ForwardColumnEngine::registerClass();
}
