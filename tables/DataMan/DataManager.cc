//# DataManager.cc: Abstract base class for a data manager
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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
#include <casacore/tables/DataMan/DataManagerColumn.h>
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
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/DynLib.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/stdio.h>                     // for sprintf


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


rownr_t DataManager::open1 (rownr_t nrrow, AipsIO& ios)
{
    open (nrrow, ios);
    return nrrow;
}

rownr_t DataManager::resync1 (rownr_t nrrow)
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

void DataManager::addRow (rownr_t)
    { throw DataManInvOper ("DataManager::addRow not allowed for "
                            "data manager type " + dataManagerType()); }

void DataManager::removeRow (rownr_t)
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
SimpleOrderedMap<String,DataManagerCtor>
        DataManager::theirRegisterMap (DataManager::unknownDataManager);
MutexedInit DataManager::theirMutexedInit(doRegisterMainCtor, 0,
                                          Mutex::Recursive);


//# Register a mapping.
void DataManager::registerCtor (const String& type, DataManagerCtor func)
{
    ScopedMutexLock lock(theirMutexedInit.mutex());
    unlockedRegisterCtor (type, func);
}

//# Test if the data manager is registered.
Bool DataManager::isRegistered (const String& type)
{
    ScopedMutexLock lock(theirMutexedInit.mutex());
    if (theirRegisterMap.isDefined(type)) {
	return True;
    }
    return False;
}

//# Get a data manager constructor.
//# Return default function if the data manager is undefined.
DataManagerCtor DataManager::getCtor (const String& type)
{
    ScopedMutexLock lock(theirMutexedInit.mutex());
    DataManagerCtor* fp = theirRegisterMap.isDefined (type);
    if (fp) {
        return *fp;
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
        fp = theirRegisterMap.isDefined (type);
        if (fp) {
            return *fp;
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





//# Register all mappings of the names of classes derived from
//# DataManager to a static function calling the default constructor.
//# The class name is the name as returned by the function dataManagerType.
void DataManager::doRegisterMainCtor (void*)
{
  unlockedRegisterCtor ("StManAipsIO", StManAipsIO::makeObject);
  unlockedRegisterCtor ("StandardStMan", StandardStMan::makeObject);
  unlockedRegisterCtor ("IncrementalStMan", IncrementalStMan::makeObject);
  unlockedRegisterCtor ("TiledDataStMan", TiledDataStMan::makeObject);
  unlockedRegisterCtor ("TiledCellStMan", TiledCellStMan::makeObject);
  unlockedRegisterCtor ("TiledColumnStMan", TiledColumnStMan::makeObject);
  unlockedRegisterCtor ("TiledShapeStMan", TiledShapeStMan::makeObject);
  unlockedRegisterCtor ("MemoryStMan", MemoryStMan::makeObject);
  unlockedRegisterCtor (CompressFloat::className(),
                        CompressFloat::makeObject);
  unlockedRegisterCtor (CompressComplex::className(),
                        CompressComplex::makeObject);
  unlockedRegisterCtor (CompressComplexSD::className(),
                        CompressComplexSD::makeObject);
  unlockedRegisterCtor (MappedArrayEngine<Complex,DComplex>::className(),
                        MappedArrayEngine<Complex,DComplex>::makeObject);
  unlockedRegisterCtor (ForwardColumnEngine::className(),
                        ForwardColumnEngine::makeObject);
  unlockedRegisterCtor (VirtualTaQLColumn::className(),
                        VirtualTaQLColumn::makeObject);
  unlockedRegisterCtor (BitFlagsEngine<uChar>::className(),
                        BitFlagsEngine<uChar>::makeObject);
  unlockedRegisterCtor (BitFlagsEngine<Short>::className(),
                        BitFlagsEngine<Short>::makeObject);
  unlockedRegisterCtor (BitFlagsEngine<Int>::className(),
                        BitFlagsEngine<Int>::makeObject);
}

} //# NAMESPACE CASACORE - END

