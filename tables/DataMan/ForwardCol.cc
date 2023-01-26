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
    for (uint32_t i=0; i<refColumns_p.nelements(); i++) {
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


bool ForwardColumnEngine::canAddRow() const
{ return true; }
bool ForwardColumnEngine::canRemoveRow() const
{ return true; }
void ForwardColumnEngine::addRow64 (rownr_t)
{}
void ForwardColumnEngine::removeRow64 (rownr_t)
{}


bool ForwardColumnEngine::canAddColumn() const
    { return true; }
bool ForwardColumnEngine::canRemoveColumn() const
    { return true; }

// Note that the column has already been added by makeXXColumn.
// This function is merely for initializing the added column.
void ForwardColumnEngine::addColumn (DataManagerColumn* colp)
{
    for (uint32_t i=0; i<ncolumn(); i++) {
	if (colp == refColumns_p[i]) {
	    refColumns_p[i]->fillTableName (table(), refTable_p);
	    refColumns_p[i]->prepare (table());
	    return;
	}
    }
    throw DataManInternalError ("ForwardColumnEngine::addColumn on column "
                                + colp->columnName());
}

void ForwardColumnEngine::removeColumn (DataManagerColumn* colp)
{
    for (uint32_t i=0; i<ncolumn(); i++) {
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
    throw DataManInternalError ("ForwardColumnEngine::removeColumn on column "
                                + colp->columnName());
}


void ForwardColumnEngine::addForwardColumn (ForwardColumn* colp)
{
    uint32_t nr = refColumns_p.nelements();
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


void ForwardColumnEngine::create64 (rownr_t)
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
    for (uint32_t i=0; i<refColumns_p.nelements(); i++) {
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
    for (uint32_t i=0; i<refColumns_p.nelements(); i++) {
	refColumns_p[i]->prepare (table());
    }
}

void ForwardColumnEngine::reopenRW()
{
    for (uint32_t i=0; i<refColumns_p.nelements(); i++) {
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
    writable_p = true;
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
    basePrepare (thisTable, true);
}

void ForwardColumn::basePrepare (const Table& thisTable, bool writable)
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
	    writable_p = false;
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


bool ForwardColumn::isWritable() const
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
	writable_p = true;
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
void ForwardColumn::setShape (rownr_t rownr, const IPosition& shape)
    { colPtr_p->setShape (rownr, shape); }

uint32_t ForwardColumn::ndim (rownr_t rownr)
    { return colPtr_p->ndim (rownr); }

IPosition ForwardColumn::shape(rownr_t rownr)
    { return colPtr_p->shape (rownr); }

bool ForwardColumn::isShapeDefined (rownr_t rownr)
    { return colPtr_p->isDefined (rownr); }

bool ForwardColumn::canChangeShape() const
{
    return (colPtr_p == 0  ?  false : colPtr_p->canChangeShape());
}

void ForwardColumn::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
    { colPtr_p->getArray (rownr, dataPtr); }

void ForwardColumn::getSliceV (rownr_t rownr, const Slicer& ns, ArrayBase& dataPtr)
    { colPtr_p->getSlice (rownr, ns, dataPtr); }

void ForwardColumn::getScalarColumnV (ArrayBase& dataPtr)
    { colPtr_p->getScalarColumn (dataPtr); }

void ForwardColumn::getArrayColumnV (ArrayBase& dataPtr)
    { colPtr_p->getArrayColumn (dataPtr); }

void ForwardColumn::getScalarColumnCellsV (const RefRows& rownrs, ArrayBase& dataPtr)
    { colPtr_p->getScalarColumnCells (rownrs, dataPtr); }

void ForwardColumn::getArrayColumnCellsV (const RefRows& rownrs, ArrayBase& dataPtr)
    { colPtr_p->getArrayColumnCells (rownrs, dataPtr); }

void ForwardColumn::getColumnSliceV (const Slicer& ns, ArrayBase& dataPtr)
    { colPtr_p->getColumnSlice (ns, dataPtr); }

void ForwardColumn::getColumnSliceCellsV (const RefRows& rownrs,
                                          const Slicer& ns, ArrayBase& dataPtr)
    { colPtr_p->getColumnSliceCells (rownrs, ns, dataPtr); }

void ForwardColumn::putArrayV (rownr_t rownr, const ArrayBase& dataPtr)
    { colPtr_p->putArray (rownr, dataPtr); }

void ForwardColumn::putSliceV (rownr_t rownr, const Slicer& ns,
			       const ArrayBase& dataPtr)
    { colPtr_p->putSlice (rownr, ns, dataPtr); }

void ForwardColumn::putScalarColumnV (const ArrayBase& dataPtr)
    { colPtr_p->putScalarColumn (dataPtr); }

void ForwardColumn::putArrayColumnV (const ArrayBase& dataPtr)
    { colPtr_p->putArrayColumn (dataPtr); }

void ForwardColumn::putScalarColumnCellsV (const RefRows& rownrs,
                                           const ArrayBase& dataPtr)
    { colPtr_p->putScalarColumnCells (rownrs, dataPtr); }

void ForwardColumn::putArrayColumnCellsV (const RefRows& rownrs,
                                          const ArrayBase& dataPtr)
    { colPtr_p->putArrayColumnCells (rownrs, dataPtr); }

void ForwardColumn::putColumnSliceV (const Slicer& ns, const ArrayBase& dataPtr)
    { colPtr_p->putColumnSlice (ns, dataPtr); }

void ForwardColumn::putColumnSliceCellsV (const RefRows& rownrs,
                                          const Slicer& ns, const ArrayBase& dataPtr)
    { colPtr_p->putColumnSliceCells (rownrs, ns, dataPtr); }



#define FORWARDCOLUMN_GETPUT(T,NM) \
void ForwardColumn::aips_name2(get,NM) (rownr_t rownr, T* dataPtr) \
    { colPtr_p->get (rownr, dataPtr); } \
void ForwardColumn::aips_name2(put,NM) (rownr_t rownr, const T* dataPtr) \
    { colPtr_p->put (rownr, dataPtr); }

FORWARDCOLUMN_GETPUT(bool,Bool)
FORWARDCOLUMN_GETPUT(unsigned char,uChar)
FORWARDCOLUMN_GETPUT(int16_t,Short)
FORWARDCOLUMN_GETPUT(uint16_t,uShort)
FORWARDCOLUMN_GETPUT(int32_t,Int)
FORWARDCOLUMN_GETPUT(uint32_t,uInt)
FORWARDCOLUMN_GETPUT(int64_t,Int64)
FORWARDCOLUMN_GETPUT(float,float)
FORWARDCOLUMN_GETPUT(double,double)
FORWARDCOLUMN_GETPUT(Complex,Complex)
FORWARDCOLUMN_GETPUT(DComplex,DComplex)
FORWARDCOLUMN_GETPUT(String,String)
FORWARDCOLUMN_GETPUT(void,Other)

} //# NAMESPACE CASACORE - END

