//# ForwardCol.cc: Virtual Column Engine forwarding to another column
//# Copyright (C) 1995,1996,1997,2000,2001
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
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/BaseColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/OS/Path.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ForwardColumnEngine::ForwardColumnEngine (const String& dataManagerName,
					  const Record& spec)
: refColumns_p (0),
  dataManName_p(dataManagerName)
{
  if (spec.isDefined ("FORWARDTABLE")) {
    refTable_p = Table(spec.asString ("FORWARDTABLE"));
  }
}

ForwardColumnEngine::ForwardColumnEngine (const Table& referencedTable,
					  const String& dataManagerName)
: refColumns_p (0),
  refTable_p   (referencedTable),
  dataManName_p(dataManagerName)
{}

ForwardColumnEngine::ForwardColumnEngine (const Table& referencedTable)
: refColumns_p (0),
  refTable_p   (referencedTable)
{}

ForwardColumnEngine::~ForwardColumnEngine()
{
    for (uInt i=0; i<refColumns_p.nelements(); i++) {
	delete refColumns_p[i];
    }
}

// Make new table using a ForwardColumn engine.
SetupNewTable ForwardColumn::setupNewTable (const Table& table,
					    const String& tableName,
					    Table::TableOption option)
{
    SetupNewTable newtab (tableName, table.tableDesc(), option);
    ForwardColumnEngine engine (table);
    newtab.bindAll (engine);
    return newtab;
}

// Clone the engine object.
DataManager* ForwardColumnEngine::clone() const
{
    DataManager* dmPtr = new ForwardColumnEngine (refTable_p, dataManName_p);
    return dmPtr;
}


// Define the RefTable_p if not defined yet.
void ForwardColumnEngine::setRefTable (const Table& refTable)
{
    if (refTable_p.isNull()) {
	refTable_p = refTable;
    }
}


Bool ForwardColumnEngine::canAddRow() const
{ return True; }
Bool ForwardColumnEngine::canRemoveRow() const
{ return True; }
void ForwardColumnEngine::addRow (uInt)
{}
void ForwardColumnEngine::removeRow (uInt)
{}


Bool ForwardColumnEngine::canAddColumn() const
    { return True; }
Bool ForwardColumnEngine::canRemoveColumn() const
    { return True; }

// Note that the column has already been added by makeXXColumn.
// This function is merely for initializing the added column.
void ForwardColumnEngine::addColumn (DataManagerColumn* colp)
{
    for (uInt i=0; i<ncolumn(); i++) {
	if (colp == refColumns_p[i]) {
	    refColumns_p[i]->fillTableName (table(), refTable_p);
	    refColumns_p[i]->prepare (table());
	    return;
	}
    }
    throw (DataManInternalError ("ForwardColumnEngine::addColumn"));
}

void ForwardColumnEngine::removeColumn (DataManagerColumn* colp)
{
    for (uInt i=0; i<ncolumn(); i++) {
	if (refColumns_p[i] == colp) {
	    delete refColumns_p[i];
	    decrementNcolumn();
	    for (; i<ncolumn(); i++) {
		refColumns_p[i] = refColumns_p[i+1];
	    }
            refColumns_p[i] = 0;
	    return;
	}
    }
    throw (DataManInternalError ("ForwardColumnEngine::removeColumn"));
}


void ForwardColumnEngine::addForwardColumn (ForwardColumn* colp)
{
    uInt nr = refColumns_p.nelements();
    refColumns_p.resize(nr + 1);
    refColumns_p[nr] = colp;
}

DataManagerColumn* ForwardColumnEngine::makeScalarColumn (const String& name,
						      int dataType,
						      const String& dataTypeId)
{
    ForwardColumn* colp = new ForwardColumn (this, name, dataType,
					     dataTypeId, refTable_p);
    addForwardColumn (colp);
    return colp;
}

DataManagerColumn* ForwardColumnEngine::makeIndArrColumn (const String& name,
						      int dataType,
						      const String& dataTypeId)
{
    return makeScalarColumn (name, dataType, dataTypeId);
}


void ForwardColumnEngine::create (uInt)
{
    baseCreate();
}

void ForwardColumnEngine::baseCreate()
{
    // The table is new.
    // Define a keyword telling the data manager name.
    table().rwKeywordSet().define
	                 (keywordName ("_ForwardColumn_Name"), dataManName_p);
    // Define a keyword in all columns telling the original table.
    for (uInt i=0; i<refColumns_p.nelements(); i++) {
	refColumns_p[i]->fillTableName (table(), refTable_p);
    }
}

void ForwardColumnEngine::prepare()
{
    basePrepare();
}

void ForwardColumnEngine::basePrepare()
{
    // Get the data manager name (if defined).
    const TableRecord& keySet = table().keywordSet();
    String keyword = keywordName ("_ForwardColumn_Name");
    if (keySet.isDefined (keyword)) {
	dataManName_p = keySet.asString (keyword);
    }
    // Attach all forwarding columns.
    for (uInt i=0; i<refColumns_p.nelements(); i++) {
	refColumns_p[i]->prepare (table());
    }
}

void ForwardColumnEngine::reopenRW()
{
    for (uInt i=0; i<refColumns_p.nelements(); i++) {
	refColumns_p[i]->setRW();
    }
}

DataManager* ForwardColumnEngine::makeObject (const String& dataManagerName,
					      const Record& spec)
{
    DataManager* dmPtr = new ForwardColumnEngine (dataManagerName, spec);
    return dmPtr;
}
void ForwardColumnEngine::registerClass()
{
    DataManager::registerCtor (className(), makeObject);
}
String ForwardColumnEngine::dataManagerName() const
{
    return dataManName_p;
}
String ForwardColumnEngine::dataManagerType() const
{
    return className();
}
String ForwardColumnEngine::className()
{
    return "ForwardColumnEngine";
}

Record ForwardColumnEngine::dataManagerSpec() const
{
  Record spec;
  spec.define ("FORWARDTABLE", refTable_p.tableName());
  return spec;
}


ForwardColumn::ForwardColumn (ForwardColumnEngine* enginePtr,
			      const String& name,
			      int dataType,
			      const String& dataTypeId,
			      const Table& refTable)
: enginePtr_p (enginePtr),
  colName_p   (name),
  dataType_p  (dataType),
  dataTypeId_p(dataTypeId),
  colPtr_p    (0)
{
    if (!refTable.isNull()) {
	refCol_p.attach (refTable, name);
    }
}

ForwardColumn::~ForwardColumn()
{}


int ForwardColumn::dataType() const
    { return dataType_p; }
String ForwardColumn::dataTypeId() const
    { return dataTypeId_p; }

void ForwardColumn::fillTableName (const Table& thisTable,
				   const Table& refTable)
{
    // Set the column (temporarily) to writable, so a TableColumn
    // object can be created for adding the keyword.
    // Prepare will set the writable switch correctly.
    writable_p = True;
    // When the table (in which this virtual column is used) is new,
    // the name of the outermost non-forwarding table will be stored
    // as a column keyword.
    // It is possible that there is a chain of forwarding engines and
    // in this way forwarding is kept to a minimum.
    TableColumn thisCol (thisTable, colName_p);
    // Get the name of the original table.
    // If the referenced table column is a ForwardColumnEngine itself, it
    // will contain the special keyword containing the name of the
    // original table. Otherwise the referenced table is the original
    // table itself.
    String name;
    if (refCol_p.keywordSet().isDefined ("_ForwardColumn_TableName")) {
	name = refCol_p.keywordSet().asString("_ForwardColumn_TableName");
    }else{
        name = refTable.tableName();
    }
    // Make relative to parent table.
    name = Path::stripDirectory (name, thisTable.tableName());
    // Define the keyword containing the original table name.
    thisCol.rwKeywordSet().define ("_ForwardColumn_TableName" +
				   enginePtr_p->suffix(), name);
}

void ForwardColumn::prepare (const Table& thisTable)
{
    basePrepare (thisTable, True);
}

void ForwardColumn::basePrepare (const Table& thisTable, Bool writable)
{
    TableColumn thisCol (thisTable, colName_p);
    // Open the original table as stored in the special keyword.
    // Open it for read/write if this table is writable and if the
    // original table file is writable.
    String name = thisCol.keywordSet().asString
	                 ("_ForwardColumn_TableName" + enginePtr_p->suffix());
    name = Path::addDirectory (name, thisTable.tableName());
    writable_p = writable;
    if (writable_p) {
	if (!(thisTable.isWritable()  &&  Table::isWritable(name))) {
	    writable_p = False;
	}
    }
    if (writable_p) {
	origTable_p = Table (name, Table::Update);
    }else{
	origTable_p = Table (name);
    }
    TableColumn origCol (origTable_p, colName_p);
    // Check if the column descriptions match.
    // If so, get the pointer to the original column.
    if (origCol.columnDesc() != thisCol.columnDesc()) {
	throw (DataManInvOper ("ForwardColumn::prepare: ColumnDesc of "
			       + colName_p + " mismatches original"));
    }
    colPtr_p = origCol.baseColPtr();
    if (writable_p) {
	writable_p = origTable_p.isColumnWritable (colName_p);
    }
    // Set the RefTable in the engine (in case not set yet).
    enginePtr_p->setRefTable (origTable_p);
}


Bool ForwardColumn::isWritable() const
    { return writable_p; }


void ForwardColumn::setRW()
{
    // Set the column to writable if the underlying table is writable
    // and the referenced column in it is writable.
    // First test if the table is already writable, which may often be
    // the case because the same table is used by multiple columns.
    if (! origTable_p.isWritable()) {
	if (Table::isWritable (origTable_p.tableName())) {
	    origTable_p.reopenRW();
	}
    }
    if (origTable_p.isColumnWritable (colName_p)) {
	writable_p = True;
    }
}


void ForwardColumn::setShapeColumn (const IPosition& shape)
{
    if (!refCol_p.isNull()) {
	if (shape != refCol_p.shapeColumn()) {
	    throw (DataManInvOper
		   ("ForwardColumn::setShapeColumn: shape of column "
		    + colName_p	+ " mismatches forwarded column"));
	}
    }
}
void ForwardColumn::setShape (uInt rownr, const IPosition& shape)
    { colPtr_p->setShape (rownr, shape); }

uInt ForwardColumn::ndim (uInt rownr)
    { return colPtr_p->ndim (rownr); }

IPosition ForwardColumn::shape(uInt rownr)
    { return colPtr_p->shape (rownr); }

Bool ForwardColumn::isShapeDefined (uInt rownr)
    { return colPtr_p->isDefined (rownr); }

Bool ForwardColumn::canChangeShape() const
{
    return (colPtr_p == 0  ?  False : colPtr_p->canChangeShape());
}
Bool ForwardColumn::canAccessScalarColumn (Bool& reask) const
{
    return colPtr_p->canAccessScalarColumn (reask);
}
Bool ForwardColumn::canAccessArrayColumn (Bool& reask) const
{
    return colPtr_p->canAccessArrayColumn (reask);
}
Bool ForwardColumn::canAccessSlice (Bool& reask) const
{
    return colPtr_p->canAccessSlice (reask);
}
Bool ForwardColumn::canAccessColumnSlice (Bool& reask) const
{
    return colPtr_p->canAccessColumnSlice (reask);
}

void ForwardColumn::getArrayV (uInt rownr, void* dataPtr)
    { colPtr_p->get (rownr, dataPtr); }

void ForwardColumn::getSliceV (uInt rownr, const Slicer& ns, void* dataPtr)
    { colPtr_p->getSlice (rownr, ns, dataPtr); }

void ForwardColumn::getScalarColumnV (void* dataPtr)
    { colPtr_p->getScalarColumn (dataPtr); }

void ForwardColumn::getArrayColumnV (void* dataPtr)
    { colPtr_p->getArrayColumn (dataPtr); }

void ForwardColumn::getScalarColumnCellsV (const RefRows& rownrs, void* dataPtr)
    { colPtr_p->getScalarColumnCells (rownrs, dataPtr); }

void ForwardColumn::getArrayColumnCellsV (const RefRows& rownrs, void* dataPtr)
    { colPtr_p->getArrayColumnCells (rownrs, dataPtr); }

void ForwardColumn::getColumnSliceV (const Slicer& ns, void* dataPtr)
    { colPtr_p->getColumnSlice (ns, dataPtr); }

void ForwardColumn::getColumnSliceCellsV (const RefRows& rownrs,
                                          const Slicer& ns, void* dataPtr)
    { colPtr_p->getColumnSliceCells (rownrs, ns, dataPtr); }

void ForwardColumn::putArrayV (uInt rownr, const void* dataPtr)
    { colPtr_p->put (rownr, dataPtr); }

void ForwardColumn::putSliceV (uInt rownr, const Slicer& ns,
			       const void* dataPtr)
    { colPtr_p->putSlice (rownr, ns, dataPtr); }

void ForwardColumn::putScalarColumnV (const void* dataPtr)
    { colPtr_p->putScalarColumn (dataPtr); }

void ForwardColumn::putArrayColumnV (const void* dataPtr)
    { colPtr_p->putArrayColumn (dataPtr); }

void ForwardColumn::putScalarColumnCellsV (const RefRows& rownrs,
                                           const void* dataPtr)
    { colPtr_p->putScalarColumnCells (rownrs, dataPtr); }

void ForwardColumn::putArrayColumnCellsV (const RefRows& rownrs,
                                          const void* dataPtr)
    { colPtr_p->putArrayColumnCells (rownrs, dataPtr); }

void ForwardColumn::putColumnSliceV (const Slicer& ns, const void* dataPtr)
    { colPtr_p->putColumnSlice (ns, dataPtr); }

void ForwardColumn::putColumnSliceCellsV (const RefRows& rownrs,
                                          const Slicer& ns, const void* dataPtr)
    { colPtr_p->putColumnSliceCells (rownrs, ns, dataPtr); }



#define FORWARDCOLUMN_GETPUT(T,NM) \
void ForwardColumn::aips_name2(get,NM) (uInt rownr, T* dataPtr) \
    { colPtr_p->get (rownr, dataPtr); } \
void ForwardColumn::aips_name2(put,NM) (uInt rownr, const T* dataPtr) \
    { colPtr_p->put (rownr, dataPtr); }

FORWARDCOLUMN_GETPUT(Bool,BoolV)
FORWARDCOLUMN_GETPUT(uChar,uCharV)
FORWARDCOLUMN_GETPUT(Short,ShortV)
FORWARDCOLUMN_GETPUT(uShort,uShortV)
FORWARDCOLUMN_GETPUT(Int,IntV)
FORWARDCOLUMN_GETPUT(uInt,uIntV)
FORWARDCOLUMN_GETPUT(float,floatV)
FORWARDCOLUMN_GETPUT(double,doubleV)
FORWARDCOLUMN_GETPUT(Complex,ComplexV)
FORWARDCOLUMN_GETPUT(DComplex,DComplexV)
FORWARDCOLUMN_GETPUT(String,StringV)
FORWARDCOLUMN_GETPUT(void,OtherV)

} //# NAMESPACE CASACORE - END

