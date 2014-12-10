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


void ForwardColumnIndexedRowEngine::create (uInt)
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


void ForwardColumnIndexedRow::setShape (uInt, const IPosition&)
{
    throw (DataManInvOper
	         ("setShape not supported by ForwardColumnIndexedRowEngine"));
}

uInt ForwardColumnIndexedRow::ndim (uInt rownr)
    { return colPtr()->ndim (convertRownr(rownr)); }

IPosition ForwardColumnIndexedRow::shape(uInt rownr)
    { return colPtr()->shape (convertRownr(rownr)); }

Bool ForwardColumnIndexedRow::isShapeDefined (uInt rownr)
    { return colPtr()->isDefined (convertRownr(rownr)); }

Bool ForwardColumnIndexedRow::canChangeShape() const
{
    return False;       // put is not supported
}
Bool ForwardColumnIndexedRow::canAccessScalarColumn (Bool& reask) const
{
    reask = False;
    return False;
}
Bool ForwardColumnIndexedRow::canAccessArrayColumn (Bool& reask) const
{
    reask = False;
    return False;
}
Bool ForwardColumnIndexedRow::canAccessColumnSlice (Bool& reask) const
{
    reask = False;
    return False;
}

void ForwardColumnIndexedRow::getArrayV (uInt rownr, void* dataPtr)
    { colPtr()->get (convertRownr(rownr), dataPtr); }

void ForwardColumnIndexedRow::getSliceV (uInt rownr, const Slicer& ns,
					 void* dataPtr)
    { colPtr()->getSlice (convertRownr(rownr), ns, dataPtr); }

void ForwardColumnIndexedRow::putArrayV (uInt, const void*)
{
    throw (DataManInvOper
	    ("put not supported by ForwardColumnIndexedRowEngine"));
}

void ForwardColumnIndexedRow::putSliceV (uInt, const Slicer&, const void*)
{
    throw (DataManInvOper
	    ("put not supported by ForwardColumnIndexedRowEngine"));
}


#define FORWARDCOLUMNINDEXEDROW_GETPUT(T,NM) \
void ForwardColumnIndexedRow::aips_name2(get,NM) (uInt rownr, T* dataPtr) \
    { colPtr()->get (convertRownr(rownr), dataPtr); } \
void ForwardColumnIndexedRow::aips_name2(put,NM) (uInt, const T*) \
{ \
    throw (DataManInvOper \
	    ("put not supported by ForwardColumnIndexedRowEngine")); \
}

FORWARDCOLUMNINDEXEDROW_GETPUT(Bool,BoolV)
FORWARDCOLUMNINDEXEDROW_GETPUT(uChar,uCharV)
FORWARDCOLUMNINDEXEDROW_GETPUT(Short,ShortV)
FORWARDCOLUMNINDEXEDROW_GETPUT(uShort,uShortV)
FORWARDCOLUMNINDEXEDROW_GETPUT(Int,IntV)
FORWARDCOLUMNINDEXEDROW_GETPUT(uInt,uIntV)
FORWARDCOLUMNINDEXEDROW_GETPUT(float,floatV)
FORWARDCOLUMNINDEXEDROW_GETPUT(double,doubleV)
FORWARDCOLUMNINDEXEDROW_GETPUT(Complex,ComplexV)
FORWARDCOLUMNINDEXEDROW_GETPUT(DComplex,DComplexV)
FORWARDCOLUMNINDEXEDROW_GETPUT(String,StringV)
FORWARDCOLUMNINDEXEDROW_GETPUT(void,OtherV)

} //# NAMESPACE CASACORE - END

