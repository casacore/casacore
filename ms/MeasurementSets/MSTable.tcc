//# MSTable.cc:  the class that hold measurements from telescopes
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
//# $Id$

#ifndef MS_MSTABLE_TCC
#define MS_MSTABLE_TCC

#include <casacore/ms/MeasurementSets/MSTable.h>
#include <casacore/ms/MeasurementSets/MSTableImpl.h>
#include <casacore/tables/Tables/TableRecord.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


template <class MSEnum> 
MSTable<MSEnum>::MSTable()
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(const String &tableName, 
                         Table::TableOption option) 
    : Table(tableName, option)
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(const String &tableName, 
                         const TableLock& lockOptions,
                         Table::TableOption option) 
    : Table(tableName, lockOptions, option)
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(const String& tableName, 
                         const String &tableDescName,
                         Table::TableOption option)
    : Table(tableName, tableDescName, option)
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(const String& tableName, 
                         const String &tableDescName,
                         const TableLock& lockOptions,
                         Table::TableOption option)
    : Table(tableName, tableDescName, lockOptions, option)
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(SetupNewTable &newTab, uInt nrrow,
                         Bool initialize)
    : Table(MSTableImpl::setupCompression(newTab),
	    nrrow, initialize)
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(SetupNewTable &newTab,
                         const TableLock& lockOptions,
                         uInt nrrow,  Bool initialize)
    : Table(MSTableImpl::setupCompression(newTab),
	    lockOptions, nrrow, initialize)
{}

#ifdef HAVE_MPI
template <class MSEnum>
MSTable<MSEnum>::MSTable(MPI_Comm comm,
				  SetupNewTable &newTab, uInt nrrow,
				  Bool initialize)
    : Table(comm, MSTableImpl::setupCompression(newTab),
	    nrrow, initialize)
{}

template <class MSEnum>
MSTable<MSEnum>::MSTable(MPI_Comm comm,
				  SetupNewTable &newTab,
				  const TableLock& lockOptions,
				  uInt nrrow,  Bool initialize)
    : Table(comm, MSTableImpl::setupCompression(newTab),
	    lockOptions, nrrow, initialize)
{}
#endif // HAVE_MPI

template <class MSEnum> 
MSTable<MSEnum>::MSTable(const Table &table)
    : Table(table)
{}

template <class MSEnum> 
MSTable<MSEnum>::MSTable(const MSTable<MSEnum> &other)
    : Table(other)
{}

template <class MSEnum> 
MSTable<MSEnum>::~MSTable()
{}

template <class MSEnum> 
MSTable<MSEnum>& MSTable<MSEnum>::
operator=(const MSTable<MSEnum>& other)
{
    if (this != &other) Table::operator=(other);
    return *this;
}


template <class MSEnum> 
MSTableMaps& MSTable<MSEnum>::getMaps()
{
  // Create the static and fill it.
  // Note that C++11 guarantees that it is called once and is thread-safe.
  static MSTableMaps maps(MSTableImpl::initMaps(static_cast<MSEnum*>(0)));
  return maps;
}

template <class MSEnum> 
const String& MSTable<MSEnum>::columnName(MSTable<MSEnum>::ColEnum which)
{ 
  return getMaps().columnMap_p.at(which); 
}

template <class MSEnum> 
typename MSTable<MSEnum>::ColEnum MSTable<MSEnum>::columnType(const String &name)
{ return ColEnum(getMaps().columnType(name)); }

template <class MSEnum> 
DataType MSTable<MSEnum>::columnDataType(MSTable<MSEnum>::ColEnum which)
{ return DataType(getMaps().colDTypeMap_p.at(which)); }

template <class MSEnum> 
const String& MSTable<MSEnum>::columnStandardComment(MSTable<MSEnum>::ColEnum which)
{ return getMaps().colCommentMap_p.at(which); }
template <class MSEnum> 

const String& MSTable<MSEnum>::columnUnit(MSTable<MSEnum>::ColEnum which)
{ return getMaps().colUnitMap_p.at(which); }

template <class MSEnum> 
const String& MSTable<MSEnum>::columnMeasureType(MSTable<MSEnum>::ColEnum which)
{ return getMaps().colMeasureTypeMap_p.at(which); }

template <class MSEnum> 
const String& MSTable<MSEnum>::keywordName(MSTable<MSEnum>::KeyEnum which)
{ return getMaps().keywordMap_p.at(which); }

template <class MSEnum> 
typename MSTable<MSEnum>::KeyEnum MSTable<MSEnum>::keywordType(const String &name)
{ return KeyEnum(getMaps().keywordType(name)); }

template <class MSEnum> 
DataType MSTable<MSEnum>::keywordDataType(MSTable<MSEnum>::KeyEnum which)
{ return DataType(getMaps().keyDTypeMap_p.at(which)); }

template <class MSEnum> 
const String& MSTable<MSEnum>::keywordStandardComment(MSTable<MSEnum>::KeyEnum which)
{ return getMaps().keyCommentMap_p.at(which); }

template <class MSEnum> 
Bool MSTable<MSEnum>::isColumn(MSTable<MSEnum>::ColEnum which) const
{ return tableDesc().isColumn(columnName(which)); }

template <class MSEnum> 
Bool MSTable<MSEnum>::isColumnWritable(MSTable<MSEnum>::ColEnum which) const
{ return Table::isColumnWritable(columnName(which)); }

template <class MSEnum> 
Bool MSTable<MSEnum>::isKeyword(MSTable<MSEnum>::KeyEnum which) const
{ return keywordSet().isDefined(keywordName(which)); }

template <class MSEnum> 
Bool MSTable<MSEnum>::isScalar(MSTable<MSEnum>::ColEnum which) const
{ return tableDesc().columnDesc(columnName(which)).isScalar(); }

template <class MSEnum> 
Bool MSTable<MSEnum>::isArray(MSTable<MSEnum>::ColEnum which) const
{ return tableDesc().columnDesc(columnName(which)).isArray(); }

template <class MSEnum> 
const String& MSTable<MSEnum>::unit(const String& which) const
{ return tableDesc().columnDesc(which).keywordSet().
    asArrayString("QuantumUnits")(IPosition(1,0)); }

template <class MSEnum> 
void MSTable<MSEnum>::addColumnToDesc(TableDesc &td, MSTable<MSEnum>::ColEnum which,
                                      Int ndim, const String& refCol)
{
    MSTableImpl::addColumnToDesc(td,columnName(which),columnDataType(which),
				 columnStandardComment(which),
				 columnUnit(which),
				 columnMeasureType(which),
				 ndim,IPosition(),0,refCol);
}

template <class MSEnum> 
void MSTable<MSEnum>::addColumnToDesc(TableDesc &td, MSTable<MSEnum>::ColEnum which,
                                      const IPosition& shape,
                                      ColumnDesc::Option option,
                                      const String& refCol)
{
    MSTableImpl::addColumnToDesc(td,columnName(which),columnDataType(which),
				 columnStandardComment(which),
				 columnUnit(which),
				 columnMeasureType(which),
				 -1,shape,option,refCol);
}

template <class MSEnum> 
void MSTable<MSEnum>::addColumnToDesc(MSTableMaps& maps, MSTable<MSEnum>::ColEnum which,
                                      Int ndim, const String& refCol)
{
  MSTableImpl::addColumnToDesc(maps.requiredTD_p,
                               columnName(maps, which),
                               columnDataType(maps, which),
                               columnStandardComment(maps, which),
                               columnUnit(maps, which),
                               columnMeasureType(maps, which),
                               ndim, IPosition(), 0, refCol);
}

template <class MSEnum> 
void MSTable<MSEnum>::addColumnToDesc(MSTableMaps& maps, MSTable<MSEnum>::ColEnum which,
                                      const IPosition& shape,
                                      ColumnDesc::Option option,
                                      const String& refCol)
{
  MSTableImpl::addColumnToDesc(maps.requiredTD_p,
                               columnName(maps, which),
                               columnDataType(maps, which),
                               columnStandardComment(maps, which),
                               columnUnit(maps, which),
                               columnMeasureType(maps, which),
                               -1, shape, option, refCol);
}

template <class MSEnum> 
void MSTable<MSEnum>::addKeyToDesc(TableDesc& td, MSTable<MSEnum>::KeyEnum key)
{
    MSTableImpl::addKeyToDesc(td,keywordName(key),keywordDataType(key),
			      keywordStandardComment(key));
}

template <class MSEnum> 
void MSTable<MSEnum>::addKeyToDesc(MSTableMaps& maps, MSTable<MSEnum>::KeyEnum key)
{
  MSTableImpl::addKeyToDesc(maps.requiredTD_p,
                            keywordName(maps, key),
                            keywordDataType(maps, key),
                            keywordStandardComment(maps, key));
}

template <class MSEnum> 
void MSTable<MSEnum>::addColumnCompression (TableDesc& td,
                                            MSTable<MSEnum>::ColEnum which,
                                            Bool autoScale,
                                            const String& type)
{
    MSTableImpl::addColumnCompression (td, columnName(which), autoScale, type);
}

template <class MSEnum> 
void MSTable<MSEnum>::colMapDef(MSTableMaps& maps,
                                MSTable<MSEnum>::ColEnum col,
                                const String& colName,
                                DataType colType,
                                const String& colComment,
                                const String& colUnit,
                                const String& colMeasureType)
{
    MSTableImpl::colMapDef(maps.columnMap_p, maps.colDTypeMap_p, maps.colCommentMap_p,
			   maps.colUnitMap_p, maps.colMeasureTypeMap_p,
                           col,colName,colType,colComment,colUnit,colMeasureType);
}

template <class MSEnum> 
void MSTable<MSEnum>::keyMapDef(MSTableMaps& maps,
                                MSTable<MSEnum>::KeyEnum key,
                                const String& keyName,
                                DataType keyType,
                                const String& keyComment)
{
    MSTableImpl::keyMapDef(maps.keywordMap_p, maps.keyDTypeMap_p, maps.keyCommentMap_p,
			   key,keyName,keyType,keyComment);
}

template <class MSEnum> 
Bool MSTable<MSEnum>::validate(const TableDesc& tabDesc)
{
  return MSTableImpl::validate(tabDesc, getMaps().requiredTD_p);
}
 
template <class MSEnum> 
Bool MSTable<MSEnum>::validate(const TableRecord& tabKeySet)
{
  return MSTableImpl::validate(tabKeySet, getMaps().requiredTD_p);
}

template <class MSEnum> 
const TableDesc& MSTable<MSEnum>::requiredTableDesc()
{
  return getMaps().requiredTD_p;
}

template <class MSEnum> 
Table MSTable<MSEnum>::referenceCopy(const String& newTableName, 
			        const Block<String>& writableColumns) const
{
    return MSTableImpl::referenceCopy(*this, newTableName, writableColumns);
}


} //# NAMESPACE CASACORE - END


#endif
