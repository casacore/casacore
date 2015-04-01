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

//# These statics cannot be compiled with egcs 1.0.3a.
#if !defined(__GNUG__) || (defined(__GNUG__) && (__GNUG__ == 2) && (__GNUC_MINOR__ >= 91)) || defined(AIPS_GCC3) || defined(AIPS_GCC4)
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::columnMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, Int> MSTable<ColEnum,KeyEnum>::colDTypeMap_p(TpOther);
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::colCommentMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::colUnitMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::colMeasureTypeMap_p("");

template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::keywordMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, Int> MSTable<ColEnum,KeyEnum>::keyDTypeMap_p(TpOther);
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::keyCommentMap_p("");
template <class ColEnum, class KeyEnum> 
CountedPtr<TableDesc> MSTable<ColEnum,KeyEnum>::requiredTD_p;
#endif


template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable() {}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(const String &tableName, 
				  Table::TableOption option) 
    : Table(tableName, option)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(const String &tableName, 
				  const TableLock& lockOptions,
				  Table::TableOption option) 
    : Table(tableName, lockOptions, option)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(const String& tableName, 
				  const String &tableDescName,
				  Table::TableOption option)
    : Table(tableName, tableDescName, option)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(const String& tableName, 
				  const String &tableDescName,
				  const TableLock& lockOptions,
				  Table::TableOption option)
    : Table(tableName, tableDescName, lockOptions, option)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(SetupNewTable &newTab, uInt nrrow,
				  Bool initialize)
    : Table(MSTableImpl::setupCompression(newTab),
	    nrrow, initialize)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(SetupNewTable &newTab,
				  const TableLock& lockOptions,
				  uInt nrrow,  Bool initialize)
    : Table(MSTableImpl::setupCompression(newTab),
	    lockOptions, nrrow, initialize)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(const Table &table)
    : Table(table)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::MSTable(const MSTable<ColEnum,KeyEnum> &other)
    : Table(other)
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>::~MSTable()
{}

template <class ColEnum, class KeyEnum> 
MSTable<ColEnum,KeyEnum>& MSTable<ColEnum,KeyEnum>::
operator=(const MSTable<ColEnum,KeyEnum>& other)
{
    if (this != &other) Table::operator=(other);
    return *this;
}


template <class ColEnum, class KeyEnum> 
const String& MSTable<ColEnum,KeyEnum>::columnName(ColEnum which)
{ 
    MSTableImpl::init(); return columnMap_p(which); 
}

template <class ColEnum, class KeyEnum> 
ColEnum MSTable<ColEnum,KeyEnum>::columnType(const String &name)
{ MSTableImpl::init(); return ColEnum(MSTableImpl::mapType(columnMap_p,name)); }

template <class ColEnum, class KeyEnum> 
DataType MSTable<ColEnum,KeyEnum>::columnDataType(ColEnum which)
{ MSTableImpl::init(); return DataType(colDTypeMap_p(which)); }

template <class ColEnum, class KeyEnum> 
const String& MSTable<ColEnum,KeyEnum>::columnStandardComment(ColEnum which)
{ MSTableImpl::init(); return colCommentMap_p(which); }
template <class ColEnum, class KeyEnum> 

const String& MSTable<ColEnum,KeyEnum>::columnUnit(ColEnum which)
{ MSTableImpl::init(); return colUnitMap_p(which); }

template <class ColEnum, class KeyEnum> 
const String& MSTable<ColEnum,KeyEnum>::columnMeasureType(ColEnum which)
{ MSTableImpl::init(); return colMeasureTypeMap_p(which); }

template <class ColEnum, class KeyEnum> 
const String& MSTable<ColEnum,KeyEnum>::keywordName(KeyEnum which)
{ MSTableImpl::init(); return keywordMap_p(which); }

template <class ColEnum, class KeyEnum> 
KeyEnum MSTable<ColEnum,KeyEnum>::keywordType(const String &name)
{ MSTableImpl::init(); return KeyEnum(MSTableImpl::mapType(keywordMap_p,name)); }

template <class ColEnum, class KeyEnum> 
DataType MSTable<ColEnum,KeyEnum>::keywordDataType(KeyEnum which)
{ MSTableImpl::init(); return DataType(keyDTypeMap_p(which)); }

template <class ColEnum, class KeyEnum> 
const String& MSTable<ColEnum,KeyEnum>::keywordStandardComment(KeyEnum which)
{ MSTableImpl::init(); return keyCommentMap_p(which); }

template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::isColumn(ColEnum which) const
{ return tableDesc().isColumn(columnName(which)); }

template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::isColumnWritable(ColEnum which) const
{ return Table::isColumnWritable(columnName(which)); }

template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::isKeyword(KeyEnum which) const
{ return keywordSet().isDefined(keywordName(which)); }

template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::isScalar(ColEnum which) const
{ return tableDesc().columnDesc(columnName(which)).isScalar(); }

template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::isArray(ColEnum which) const
{ return tableDesc().columnDesc(columnName(which)).isArray(); }

template <class ColEnum, class KeyEnum> 
const String& MSTable<ColEnum,KeyEnum>::unit(const String& which) const
{ return tableDesc().columnDesc(which).keywordSet().
    asArrayString("QuantumUnits")(IPosition(1,0)); }

template <class ColEnum, class KeyEnum> 
void MSTable<ColEnum,KeyEnum>::addColumnToDesc(TableDesc &td, ColEnum which,
					       Int ndim, const String& refCol)
{
    MSTableImpl::addColumnToDesc(td,columnName(which),columnDataType(which),
				 columnStandardComment(which),
				 columnUnit(which),
				 columnMeasureType(which),
				 ndim,IPosition(),0,refCol);
}

template <class ColEnum, class KeyEnum> 
void MSTable<ColEnum,KeyEnum>::addColumnToDesc(TableDesc &td, ColEnum which,
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

template <class ColEnum, class KeyEnum> 
void MSTable<ColEnum,KeyEnum>::addKeyToDesc(TableDesc& td, KeyEnum key)
{
    MSTableImpl::addKeyToDesc(td,keywordName(key),keywordDataType(key),
			      keywordStandardComment(key));
}

template <class ColEnum, class KeyEnum> 
void MSTable<ColEnum,KeyEnum>::addColumnCompression (TableDesc& td,
						     ColEnum which,
						     Bool autoScale,
						     const String& type)
{
    MSTableImpl::addColumnCompression (td, columnName(which), autoScale, type);
}

template <class ColEnum, class KeyEnum> 
void MSTable<ColEnum,KeyEnum>::colMapDef(ColEnum col,
					 const String& colName,
					 DataType colType,
					 const String& colComment,
					 const String& colUnit,
					 const String& colMeasureType)
{
    MSTableImpl::colMapDef(columnMap_p,colDTypeMap_p,colCommentMap_p,
			   colUnitMap_p,colMeasureTypeMap_p,col,colName,
			   colType,colComment,colUnit,colMeasureType);
}

template <class ColEnum, class KeyEnum> 
void MSTable<ColEnum,KeyEnum>::keyMapDef(KeyEnum key,
					 const String& keyName,
					 DataType keyType,
					 const String& keyComment)
{
    MSTableImpl::keyMapDef(keywordMap_p,keyDTypeMap_p,keyCommentMap_p,
			   key,keyName,keyType,keyComment);
}

template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::validate(const TableDesc& tabDesc)
{
    MSTableImpl::init(); return MSTableImpl::validate(tabDesc,*requiredTD_p);
}
 
template <class ColEnum, class KeyEnum> 
Bool MSTable<ColEnum,KeyEnum>::validate(const TableRecord& tabKeySet)
{
    MSTableImpl::init(); return MSTableImpl::validate(tabKeySet,*requiredTD_p);
}

template <class ColEnum, class KeyEnum> 
const TableDesc& MSTable<ColEnum,KeyEnum>::requiredTableDesc()
{
    MSTableImpl::init(); return *requiredTD_p;
}

template <class ColEnum, class KeyEnum> 
Table MSTable<ColEnum,KeyEnum>::referenceCopy(const String& newTableName, 
			        const Block<String>& writableColumns) const
{
    return MSTableImpl::referenceCopy(*this, newTableName, writableColumns);
}


} //# NAMESPACE CASACORE - END


#endif
