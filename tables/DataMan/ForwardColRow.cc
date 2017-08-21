//# ForwardColRow.cc: Virtual Column Engine forwarding to another row/column
//# Copyright (C) 1995,1996,1997,2001
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
#include <casacore/tables/DataMan/ForwardColRow.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/BaseColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Record.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ForwardColumnIndexedRowEngine::ForwardColumnIndexedRowEngine
                                           (const String& dataManagerName,
					    const Record& spec)
: ForwardColumnEngine (dataManagerName, spec),
  lastRow_p           (-1)
{
  setSuffix ("_Row");
  if (spec.isDefined("COLUMNNAME")) {
    spec.get ("COLUMNNAME", rowColumnName_p);
  }
}

ForwardColumnIndexedRowEngine::ForwardColumnIndexedRowEngine
                                               (const Table& referencedTable,
						const String& rowColumnName,
						const String& dataManagerName)
: ForwardColumnEngine (referencedTable, dataManagerName),
  rowColumnName_p     (rowColumnName),
  lastRow_p           (-1)
{
  setSuffix ("_Row");
}

ForwardColumnIndexedRowEngine::ForwardColumnIndexedRowEngine
                                               (const Table& referencedTable,
						const String& rowColumnName)
: ForwardColumnEngine (referencedTable, ""),
  rowColumnName_p     (rowColumnName),
  lastRow_p           (-1)
{
  setSuffix ("_Row");
}

ForwardColumnIndexedRowEngine::~ForwardColumnIndexedRowEngine()
{}

// Clone the engine object.
DataManager* ForwardColumnIndexedRowEngine::clone() const
{
    DataManager* dmPtr = new ForwardColumnIndexedRowEngine (refTable(),
							    rowColumnName_p,
							    dataManagerName());
    return dmPtr;
}


DataManagerColumn* ForwardColumnIndexedRowEngine::makeScalarColumn
                                                     (const String& name,
						      int dataType,
						      const String& dataTypeId)
{
    ForwardColumnIndexedRow* colp = new ForwardColumnIndexedRow
	                                          (this, name, dataType,
						   dataTypeId, refTable());
    addForwardColumn (colp);
    return colp;
}

DataManagerColumn* ForwardColumnIndexedRowEngine::makeIndArrColumn
                                                     (const String& name,
						      int dataType,
						      const String& dataTypeId)
{
    return makeScalarColumn (name, dataType, dataTypeId);
}


void ForwardColumnIndexedRowEngine::create (rownr_t)
{
    // The table is new.
    baseCreate();
    // Define a keyword in the table telling the column containing the
    // row numbers in the original table.
    table().rwKeywordSet().define (keywordName ("_ForwardColumn_RowName"),
				   rowColumnName_p);
}

void ForwardColumnIndexedRowEngine::prepare()
{
    basePrepare();
    // Create the ScalarColumn object for the rownr column.
    // Get its column name from the special table keyword defined in create.
    const String& rowName = table().keywordSet().asString
	            (keywordName ("_ForwardColumn_RowName"));
    rowColumn_p.attach (table(), rowName);
}

void ForwardColumnIndexedRowEngine::reopenRW()
{}


DataManager* ForwardColumnIndexedRowEngine::makeObject
                                          (const String& dataManagerName,
					   const Record& spec)
{
    DataManager* dmPtr = new ForwardColumnIndexedRowEngine (dataManagerName,
							    spec);
    return dmPtr;
}
void ForwardColumnIndexedRowEngine::registerClass()
{
    DataManager::registerCtor (className(), makeObject);
}
String ForwardColumnIndexedRowEngine::dataManagerType() const
{
    return className();
}
String ForwardColumnIndexedRowEngine::className()
{
    return "ForwardColumnIndexedRowEngine";
}

Record ForwardColumnIndexedRowEngine::dataManagerSpec() const
{
  Record spec = ForwardColumnEngine::dataManagerSpec();
  spec.define ("COLUMNNAME", rowColumnName_p);
  return spec;
}





ForwardColumnIndexedRow::ForwardColumnIndexedRow
                                   (ForwardColumnIndexedRowEngine* enginePtr,
				    const String& name,
				    int dataType,
				    const String& dataTypeId,
				    const Table& refTable)
: ForwardColumn (enginePtr, name, dataType, dataTypeId, refTable),
  enginePtr_p   (enginePtr)
{}

ForwardColumnIndexedRow::~ForwardColumnIndexedRow()
{}


void ForwardColumnIndexedRow::prepare (const Table& thisTable)
{
    basePrepare (thisTable, False);
}


void ForwardColumnIndexedRow::setShape (rownr_t, const IPosition&)
{
    throw (DataManInvOper
           ("setShape not supported by data manager ForwardColumnIndexedRow"));
}

uInt ForwardColumnIndexedRow::ndim (rownr_t rownr)
    { return colPtr()->ndim (convertRownr(rownr)); }

IPosition ForwardColumnIndexedRow::shape(rownr_t rownr)
    { return colPtr()->shape (convertRownr(rownr)); }

Bool ForwardColumnIndexedRow::isShapeDefined (rownr_t rownr)
    { return colPtr()->isDefined (convertRownr(rownr)); }

Bool ForwardColumnIndexedRow::canChangeShape() const
{
    return False;       // put is not supported
}

void ForwardColumnIndexedRow::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
    { colPtr()->getArray (convertRownr(rownr), dataPtr); }

void ForwardColumnIndexedRow::getSliceV (rownr_t rownr, const Slicer& ns,
					 ArrayBase& dataPtr)
    { colPtr()->getSlice (convertRownr(rownr), ns, dataPtr); }

void ForwardColumnIndexedRow::putArrayV (rownr_t, const ArrayBase&)
{
    throw (DataManInvOper
           ("putArray not supported by data manager ForwardColumnIndexedRow"));
}

void ForwardColumnIndexedRow::putSliceV (rownr_t, const Slicer&, const ArrayBase&)
{
    throw (DataManInvOper
           ("putSlice not supported by data manager ForwardColumnIndexedRow"));
}


#define FORWARDCOLUMNINDEXEDROW_GETPUT(T,NM) \
void ForwardColumnIndexedRow::aips_name2(get,NM) (rownr_t rownr, T* dataPtr) \
    { colPtr()->get (convertRownr(rownr), dataPtr); } \
void ForwardColumnIndexedRow::aips_name2(put,NM) (rownr_t, const T*) \
{ \
    throw (DataManInvOper \
           ("put not supported by data manager ForwardColumnIndexedRow")); \
}

FORWARDCOLUMNINDEXEDROW_GETPUT(Bool,Bool)
FORWARDCOLUMNINDEXEDROW_GETPUT(uChar,uChar)
FORWARDCOLUMNINDEXEDROW_GETPUT(Short,Short)
FORWARDCOLUMNINDEXEDROW_GETPUT(uShort,uShort)
FORWARDCOLUMNINDEXEDROW_GETPUT(Int,Int)
FORWARDCOLUMNINDEXEDROW_GETPUT(uInt,uInt)
FORWARDCOLUMNINDEXEDROW_GETPUT(Int64,Int64)
FORWARDCOLUMNINDEXEDROW_GETPUT(float,float)
FORWARDCOLUMNINDEXEDROW_GETPUT(double,double)
FORWARDCOLUMNINDEXEDROW_GETPUT(Complex,Complex)
FORWARDCOLUMNINDEXEDROW_GETPUT(DComplex,DComplex)
FORWARDCOLUMNINDEXEDROW_GETPUT(String,String)
FORWARDCOLUMNINDEXEDROW_GETPUT(void,Other)

} //# NAMESPACE CASACORE - END

