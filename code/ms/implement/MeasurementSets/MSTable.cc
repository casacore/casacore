//# NewMSTable.cc:  the class that hold measurements from telescopes
//# Copyright (C) 1996,1997,2000
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

#include <aips/MeasurementSets/NewMSTable.h>
#include <aips/MeasurementSets/NewMSTableImpl.h>
#include <aips/Tables/TableRecord.h>

template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::columnMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, Int> NewMSTable<ColEnum,KeyEnum>::colDTypeMap_p(TpOther);
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::colCommentMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::colUnitMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::colMeasureTypeMap_p("");

template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::keywordMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, Int> NewMSTable<ColEnum,KeyEnum>::keyDTypeMap_p(TpOther);
template <class ColEnum, class KeyEnum> 
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::keyCommentMap_p("");
template <class ColEnum, class KeyEnum> 
SimpleCountedConstPtr<TableDesc> NewMSTable<ColEnum,KeyEnum>::requiredTD_p;

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable() {}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(const String &tableName, 
				  Table::TableOption option) 
    : Table(tableName, option)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(const String &tableName, 
				  const TableLock& lockOptions,
				  Table::TableOption option) 
    : Table(tableName, lockOptions, option)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(const String& tableName, 
				  const String &tableDescName,
				  Table::TableOption option)
    : Table(tableName, tableDescName, option)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(const String& tableName, 
				  const String &tableDescName,
				  const TableLock& lockOptions,
				  Table::TableOption option)
    : Table(tableName, tableDescName, lockOptions, option)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(SetupNewTable &newTab, uInt nrrow,
				  Bool initialize)
    : Table(newTab, nrrow, initialize)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(SetupNewTable &newTab,
				  const TableLock& lockOptions,
				  uInt nrrow,  Bool initialize)
    : Table(newTab, lockOptions, nrrow, initialize)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(const Table &table)
    : Table(table)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::NewMSTable(const NewMSTable<ColEnum,KeyEnum> &other)
    : Table(other)
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>::~NewMSTable()
{}

template <class ColEnum, class KeyEnum> 
NewMSTable<ColEnum,KeyEnum>& NewMSTable<ColEnum,KeyEnum>::
operator=(const NewMSTable<ColEnum,KeyEnum>& other)
{
    if (this != &other) Table::operator=(other);
    return *this;
}


template <class ColEnum, class KeyEnum> 
const String& NewMSTable<ColEnum,KeyEnum>::columnName(ColEnum which)
{ 
    NewMSTableImpl::init(); return columnMap_p(which); 
}

template <class ColEnum, class KeyEnum> 
ColEnum NewMSTable<ColEnum,KeyEnum>::columnType(const String &name)
{ NewMSTableImpl::init(); return ColEnum(NewMSTableImpl::mapType(columnMap_p,name)); }

template <class ColEnum, class KeyEnum> 
DataType NewMSTable<ColEnum,KeyEnum>::columnDataType(ColEnum which)
{ NewMSTableImpl::init(); return DataType(colDTypeMap_p(which)); }

template <class ColEnum, class KeyEnum> 
const String& NewMSTable<ColEnum,KeyEnum>::columnStandardComment(ColEnum which)
{ NewMSTableImpl::init(); return colCommentMap_p(which); }
template <class ColEnum, class KeyEnum> 

const String& NewMSTable<ColEnum,KeyEnum>::columnUnit(ColEnum which)
{ NewMSTableImpl::init(); return colUnitMap_p(which); }

template <class ColEnum, class KeyEnum> 
const String& NewMSTable<ColEnum,KeyEnum>::columnMeasureType(ColEnum which)
{ NewMSTableImpl::init(); return colMeasureTypeMap_p(which); }

template <class ColEnum, class KeyEnum> 
const String& NewMSTable<ColEnum,KeyEnum>::keywordName(KeyEnum which)
{ NewMSTableImpl::init(); return keywordMap_p(which); }

template <class ColEnum, class KeyEnum> 
KeyEnum NewMSTable<ColEnum,KeyEnum>::keywordType(const String &name)
{ NewMSTableImpl::init(); return KeyEnum(NewMSTableImpl::mapType(keywordMap_p,name)); }

template <class ColEnum, class KeyEnum> 
DataType NewMSTable<ColEnum,KeyEnum>::keywordDataType(KeyEnum which)
{ NewMSTableImpl::init(); return DataType(keyDTypeMap_p(which)); }

template <class ColEnum, class KeyEnum> 
const String& NewMSTable<ColEnum,KeyEnum>::keywordStandardComment(KeyEnum which)
{ NewMSTableImpl::init(); return keyCommentMap_p(which); }

template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::isColumn(ColEnum which) const
{ return tableDesc().isColumn(columnName(which)); }

template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::isColumnWritable(ColEnum which) const
{ return Table::isColumnWritable(columnName(which)); }

template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::isKeyword(KeyEnum which) const
{ return keywordSet().isDefined(keywordName(which)); }

template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::isScalar(ColEnum which) const
{ return tableDesc().columnDesc(columnName(which)).isScalar(); }

template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::isArray(ColEnum which) const
{ return tableDesc().columnDesc(columnName(which)).isArray(); }

template <class ColEnum, class KeyEnum> 
const String& NewMSTable<ColEnum,KeyEnum>::unit(const String& which) const
{ return tableDesc().columnDesc(which).keywordSet().
    asArrayString("QuantumUnits")(IPosition(1,0)); }

template <class ColEnum, class KeyEnum> 
void NewMSTable<ColEnum,KeyEnum>::addColumnToDesc(TableDesc &td, ColEnum which,
					       Int ndim, const String& refCol)
{
    NewMSTableImpl::addColumnToDesc(td,columnName(which),columnDataType(which),
				 columnStandardComment(which),
				 columnUnit(which),
				 columnMeasureType(which),
				 ndim,IPosition(),0,refCol);
}

template <class ColEnum, class KeyEnum> 
void NewMSTable<ColEnum,KeyEnum>::addColumnToDesc(TableDesc &td, ColEnum which,
					       const IPosition& shape,
					       ColumnDesc::Option option,
					       const String& refCol)
{
    NewMSTableImpl::addColumnToDesc(td,columnName(which),columnDataType(which),
				 columnStandardComment(which),
				 columnUnit(which),
				 columnMeasureType(which),
				 -1,shape,option,refCol);
}

template <class ColEnum, class KeyEnum> 
void NewMSTable<ColEnum,KeyEnum>::addKeyToDesc(TableDesc& td, KeyEnum key)
{
    NewMSTableImpl::addKeyToDesc(td,keywordName(key),keywordDataType(key),
			      keywordStandardComment(key));
}

template <class ColEnum, class KeyEnum> 
void NewMSTable<ColEnum,KeyEnum>::colMapDef(ColEnum col,
					 const String& colName,
					 DataType colType,
					 const String& colComment,
					 const String& colUnit,
					 const String& colMeasureType)
{
    NewMSTableImpl::colMapDef(columnMap_p,colDTypeMap_p,colCommentMap_p,
			   colUnitMap_p,colMeasureTypeMap_p,col,colName,
			   colType,colComment,colUnit,colMeasureType);
}

template <class ColEnum, class KeyEnum> 
void NewMSTable<ColEnum,KeyEnum>::keyMapDef(KeyEnum key,
					 const String& keyName,
					 DataType keyType,
					 const String& keyComment)
{
    NewMSTableImpl::keyMapDef(keywordMap_p,keyDTypeMap_p,keyCommentMap_p,
			   key,keyName,keyType,keyComment);
}

template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::validate(const TableDesc& tabDesc)
{
    NewMSTableImpl::init(); return NewMSTableImpl::validate(tabDesc,*requiredTD_p);
}
 
template <class ColEnum, class KeyEnum> 
Bool NewMSTable<ColEnum,KeyEnum>::validate(const TableRecord& tabKeySet)
{
    NewMSTableImpl::init(); return NewMSTableImpl::validate(tabKeySet,*requiredTD_p);
}

template <class ColEnum, class KeyEnum> 
const TableDesc& NewMSTable<ColEnum,KeyEnum>::requiredTableDesc()
{
    NewMSTableImpl::init(); return *requiredTD_p;
}

template <class ColEnum, class KeyEnum> 
Table NewMSTable<ColEnum,KeyEnum>::referenceCopy(const String& newTableName, 
			        const Block<String>& writableColumns) const
{
    return NewMSTableImpl::referenceCopy(*this, newTableName, writableColumns);
}

