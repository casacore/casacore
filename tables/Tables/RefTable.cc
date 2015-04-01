//# RefTable.cc: Class for a table as a view of another table
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

#include <casacore/tables/Tables/RefTable.h>
#include <casacore/tables/Tables/RefColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/TableTrace.h>
#include <casacore/casa/Containers/SimOrdMapIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RefTable::RefTable (AipsIO& ios, const String& name, uInt nrrow, int opt,
		    const TableLock& lockOptions, const TSMOption& tsmOption)
: BaseTable    (name, opt, nrrow),
  rowStorage_p (0),              // initially empty vector of rownrs
  nameMap_p    (""),
  colMap_p     (static_cast<RefColumn*>(0)),
  changed_p    (False)
{
    //# Read the file in.
    // Set initially to no write in destructor.
    // At the end it is reset. In this way nothing is written if
    // an exception is thrown during initialization.
    noWrite_p = True;
    getRef (ios, opt, lockOptions, tsmOption);
    noWrite_p = False;
    TableTrace::traceRefTable (baseTabPtr_p->tableName(), 'o');
}


RefTable::RefTable (BaseTable* btp, Bool order, uInt nrall)
: BaseTable    ("", Table::Scratch, nrall),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (order),
  rowStorage_p (nrall),       // allocate vector of rownrs
  nameMap_p    (""),
  colMap_p     (static_cast<RefColumn*>(0)),
  changed_p    (True)
{
    rows_p = getStorage (rowStorage_p);
    //# Copy the table description and create the columns.
    tdescPtr_p = new TableDesc (btp->tableDesc(), TableDesc::Scratch);
    setup (btp, Vector<String>());
    //# Get root table (will be parent if btp is an reference table).
    //# Link to root table (ie. increase its reference count).
    baseTabPtr_p->link();
    TableTrace::traceRefTable (baseTabPtr_p->tableName(), 's');
}

RefTable::RefTable (BaseTable* btp, const Vector<uInt>& rownrs)
: BaseTable    ("", Table::Scratch, rownrs.nelements()),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (True),
  rowStorage_p (0),
  nameMap_p    (""),
  colMap_p     (static_cast<RefColumn*>(0)),
  changed_p    (True)
{
    //# Copy the table description and create the columns.
    tdescPtr_p = new TableDesc (btp->tableDesc(), TableDesc::Scratch);
    setup (btp, Vector<String>());
    rowStorage_p = rownrs;
    rows_p = getStorage (rowStorage_p);
    //# Check if the row numbers do not exceed #rows.
    uInt nmax = btp->nrow();
    for (uInt i=0; i<nrrow_p; i++) {
	if (rows_p[i] >= nmax) {
	    throw (indexError<Int> ((Int)rows_p[i], "RefTable Row vector"));
	}
    }
    //# Adjust rownrs in case input table is a reference table.
    //# Link to the root table.
    rowOrd_p = btp->adjustRownrs (nrrow_p, rowStorage_p, True);
    baseTabPtr_p->link();
    TableTrace::traceRefTable (baseTabPtr_p->tableName(), 's');
}

RefTable::RefTable (BaseTable* btp, const Vector<Bool>& mask)
: BaseTable    ("", Table::Scratch, 0),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (btp->rowOrder()),
  rowStorage_p (0),              // initially empty vector of rownrs
  nameMap_p    (""),
  colMap_p     (static_cast<RefColumn*>(0)),
  changed_p    (True)
{
    //# Copy the table description and create the columns.
    tdescPtr_p = new TableDesc (btp->tableDesc(), TableDesc::Scratch);
    setup (btp, Vector<String>());
    //# Store the rownr if the mask is set.
    uInt nr = min (mask.nelements(), btp->nrow());
    for (uInt i=0; i<nr; i++) {
	if (mask(i)) {
	    addRownr (i);
	}
    }
    //# Adjust rownrs in case input table is a reference table.
    //# Link to the root table.
    rowOrd_p = btp->adjustRownrs (nrrow_p, rowStorage_p, True);
    baseTabPtr_p->link();
    TableTrace::traceRefTable (baseTabPtr_p->tableName(), 's');
}

RefTable::RefTable (BaseTable* btp, const Vector<String>& columnNames)
: BaseTable    ("", Table::Scratch, btp->nrow()),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (btp->rowOrder()),
  rowStorage_p (0),
  nameMap_p    (""),
  colMap_p     (static_cast<RefColumn*>(0)),
  changed_p    (True)
{
    //# Create table description by copying the selected columns.
    //# Create the columns.
    const TableDesc& td = btp->tableDesc();
    //# Copy the keywords from the root tabledesc.
    tdescPtr_p = new TableDesc (td, "", "", TableDesc::Scratch, False);
    for (uInt i=0; i<columnNames.nelements(); i++) {
	tdescPtr_p->addColumn (td.columnDesc (columnNames(i)));
    }
    setup (btp, columnNames);
    //# Get the row numbers from the input table.
    //# Copy them to this table.
    rowStorage_p = btp->rowNumbers();
    rows_p = getStorage (rowStorage_p);
    //# Link to the root table.
    baseTabPtr_p->link();
    TableTrace::traceRefTable (baseTabPtr_p->tableName(), 'p');
}


RefTable::~RefTable()
{
    //# When needed, write the table files if not marked for delete
    if (!isMarkedForDelete()) {
	if (openedForWrite()  &&  !shouldNotWrite()) {
	    writeRefTable (True);
	}
    }
    TableTrace::traceRefTable (baseTabPtr_p->tableName(), 'c');
    //# Delete all RefColumn objects.
    for (uInt i=0; i<colMap_p.ndefined(); i++) {
	delete colMap_p.getVal(i);
    }
    //# Unlink from root.
    BaseTable::unlink (baseTabPtr_p);
}


void RefTable::getPartNames (Block<String>& names, Bool recursive) const
{
  if (recursive) {
    baseTabPtr_p->getPartNames (names, recursive);
  } else {
    uInt inx = names.size();
    names.resize (inx + 1);
    names[inx] = baseTabPtr_p->tableName();
  }
}

uInt* RefTable::getStorage (Vector<uInt>& rownrs)
{
    Bool deleteIt;
    uInt* p = rownrs.getStorage (deleteIt);
    AlwaysAssert (deleteIt == False, AipsError);
    return p;
}

void RefTable::reopenRW()
{
    baseTabPtr_p->reopenRW();
    option_p = Table::Update;
}

Bool RefTable::asBigEndian() const
{
    return baseTabPtr_p->asBigEndian();
}

const StorageOption& RefTable::storageOption() const
{
    return baseTabPtr_p->storageOption();
}

Bool RefTable::isMultiUsed (Bool) const
{
    return False;
}

const TableLock& RefTable::lockOptions() const
{
    return baseTabPtr_p->lockOptions();
}
void RefTable::mergeLock (const TableLock& lockOptions)
{
    baseTabPtr_p->mergeLock (lockOptions);
}
Bool RefTable::hasLock (FileLocker::LockType type) const
{
    return baseTabPtr_p->hasLock (type);
}
Bool RefTable::lock (FileLocker::LockType type, uInt nattempts)
{
    return baseTabPtr_p->lock (type, nattempts);
}
void RefTable::unlock()
{
    baseTabPtr_p->unlock();
}

void RefTable::flush (Bool fsync, Bool recursive)
{
    if (!isMarkedForDelete()) {
        if (openedForWrite()) {
	    writeRefTable (fsync);
	}
    }
    // Flush the underlying table.
    baseTabPtr_p->flush (fsync, recursive);
}

void RefTable::resync()
{
    baseTabPtr_p->resync();
}

uInt RefTable::getModifyCounter() const
{
    return baseTabPtr_p->getModifyCounter();
}


//# Adjust the input rownrs to the actual rownrs in the root table.
Bool RefTable::adjustRownrs (uInt nr, Vector<uInt>& rowStorage,
			     Bool determineOrder) const
{
    uInt* rownrs = getStorage (rowStorage);
    Bool rowOrder = True;
    for (uInt i=0; i<nr; i++) {
	rownrs[i] = rows_p[rownrs[i]];
    }
    if (determineOrder) {
	for (uInt i=1; i<nr; i++) {
	    if (rownrs[i] <= rownrs[i-1]) {
		rowOrder = False;
		break;
	    }
	}
    }
    return rowOrder;
}


//# Write a reference table into a file.
void RefTable::writeRefTable (Bool)
{
    //# Write name and type of root and write object data.
    //# Do this only when something has changed.
    if (changed_p) {
        TableTrace::traceRefTable (baseTabPtr_p->tableName(), 'w');
	AipsIO ios;
	writeStart (ios, True);
	ios << "RefTable";
	ios.putstart ("RefTable", 2);
	// Make the name of the base table relative to this table.
	ios << Path::stripDirectory (baseTabPtr_p->tableName(),
				     tableName());
	ios << nameMap_p;
	// Write the column names in order of appearance.
	Vector<String> names(tdescPtr_p->ncolumn());
	for (uInt i=0; i<names.nelements(); i++) {
	    names(i) = tdescPtr_p->columnDesc(i).name();
	}
	ios << names;
	ios << baseTabPtr_p->nrow();
	ios << rowOrd_p;
        ios << nrrow_p;
        // Do not write more than 2**20 rownrs at once (CAS-7020).
        uInt done = 0;
        while (done < nrrow_p) {
          uInt todo = std::min(nrrow_p-done, 1048576u);
          ios.put (todo, rows_p+done, False);
          done += todo;
        }
	ios.putend();
	writeEnd (ios);
	changed_p = False;
    }
    //# Write the TableInfo.
    flushTableInfo();
}

//# Read a reference table from a file and read the associated table.
void RefTable::getRef (AipsIO& ios, int opt, const TableLock& lockOptions,
                       const TSMOption& tsmOption)
{
    //# Open the file, read name and type of root and read object data.
    String rootName;
    uInt rootNrow, nrrow;
    Int version = ios.getstart ("RefTable");
    ios >> rootName;
    rootName = Path::addDirectory (rootName, tableName());
    ios >> nameMap_p;
    Vector<String> names;
    if (version > 1) {
        ios >> names;
    }
    ios >> rootNrow;
    ios >> rowOrd_p;
    ios >> nrrow;
    DebugAssert (nrrow == nrrow_p, AipsError);
    //# Resize the block of rownrs and read them in.
    rowStorage_p.resize (nrrow);
    rows_p = getStorage (rowStorage_p);
    // Do not read more than 2**20 rows at once (CAS-7020).
    uInt done = 0;
    while (done < nrrow) {
      uInt todo = std::min(nrrow_p-done, 1048576u);
      ios.get (todo, rows_p+done);
      done += todo;
    }
    ios.getend();
    //# Now read in the root table referenced to.
    //# Check if #rows has not decreased, which is about the only thing
    //# we can do to make sure the referenced rows are still the same.
    Table tab;
    if (opt == Table::Old) {
        tab = Table(rootName, lockOptions, Table::Old, tsmOption);
    }else{
        tab = Table(rootName, lockOptions, Table::Update, tsmOption);
    }
    baseTabPtr_p = tab.baseTablePtr();
    if (rootNrow > baseTabPtr_p->nrow()) {
	throw (TableInvOper
	           ("RefTable::getRef, #rows in referenced table decreased"));
    }
    //# Build up the table description from the name map and the
    //# description of the root table.
    const TableDesc& rootDesc = baseTabPtr_p->tableDesc();
    //# Copy the keywords from the root tabledesc.
    tdescPtr_p = new TableDesc (rootDesc, "", "", TableDesc::Scratch, False);
    makeDesc (*tdescPtr_p, rootDesc, nameMap_p, names);
    //# Create the refColumns.
    makeRefCol();
    //# Read the TableInfo object.
    getTableInfo();
    //# Great, everything is done.
    //# Now link to the root table.
    baseTabPtr_p->link();
}


//# Read description and #rows.
void RefTable::getLayout (TableDesc& desc, AipsIO& ios)
{
    String rootName;
    SimpleOrderedMap<String,String> nameMap("");
    Int version = ios.getstart ("RefTable");
    ios >> rootName;
    ios >> nameMap;
    Vector<String> names;
    if (version > 1) {
        ios >> names;
    }
    // Get description of the parent table.
    TableDesc pdesc;
    Table::getLayout (pdesc, rootName);
    makeDesc (desc, pdesc, nameMap, names);
}

void RefTable::makeDesc (TableDesc& desc, const TableDesc& rootDesc,
			 SimpleOrderedMap<String,String>& nameMap,
			 Vector<String>& names)
{
    //# The names block contains the column names in order of appearance.
    //# For older versions it can be empty. If so, fill it with the
    //# names from the map.
    uInt i;
    if (names.nelements() == 0) {
        names.resize (nameMap.ndefined());
	for (i=0; i<names.nelements(); i++) {
	    names(i) = nameMap.getKey(i);
	}
    }
    //# Build up the table description.
    //# It is possible that columns have disappeared from the root table.
    //# Remember these columns, so they are removed later from the map.
    //# The nameMap maps column names in this table to the names in the
    //# root table, so a rename is needed if names are different.
    SimpleOrderedMap<String,void*> unknownCol (static_cast<RefColumn*>(0));
    for (i=0; i<names.nelements(); i++) {
        const String& name = names(i);
	const String* mapValPtr = nameMap.isDefined (name);
	AlwaysAssert (mapValPtr != 0, AipsError);
	if (rootDesc.isColumn (*mapValPtr)) {
	    desc.addColumn (rootDesc.columnDesc (*mapValPtr));
	    if (name != *mapValPtr) {
		desc.renameColumn (name, *mapValPtr);
	    }
	}else{
	    unknownCol.define (name, static_cast<void*>(0));
	}
    }
    //# Remove the unknown ones.
    for (i=0; i<unknownCol.ndefined(); i++) {
	nameMap.remove (unknownCol.getKey(i));
    }
}

//# Build the name map from the description.
//# Old and new name are (initially) equal.
//# Make RefColumn objects and initialize TableInfo.
void RefTable::setup (BaseTable* btp, const Vector<String>& columnNames)
{
    RefTable* rtp = dynamic_cast<RefTable*>(btp);
    if (rtp != 0) {
        // The table is already a RefTable, so copy its nameMap.
        if (columnNames.nelements() == 0) {
	    nameMap_p = rtp->nameMap_p;
	} else {
	  // Some columns are selected, so copy those only.
	  // Make the map const, so operator() throws an exception
	  // if the key does not exist.
	  const SimpleOrderedMap<String,String>& nm = rtp->nameMap_p;
	    for (uInt i=0; i<columnNames.nelements(); i++) {
	        nameMap_p.define (columnNames[i], nm(columnNames[i]));
	    }
	}
    } else {
        // Otherwise create it from the TableDesc.
        for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	    nameMap_p.define (tdescPtr_p->columnDesc(i).name(),
			      tdescPtr_p->columnDesc(i).name());
	}
    }
    makeRefCol();
    //# The initial table info is a copy of the original.
    tableInfo() = btp->tableInfo();
}

//# Create a RefColumn object for all columns in the description.
//# Insert it with the name in the column map.
void RefTable::makeRefCol()
{
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	const ColumnDesc& cd = tdescPtr_p->columnDesc(i);
	colMap_p.define (cd.name(), cd.makeRefColumn
	     (this, baseTabPtr_p->getColumn(nameMap_p(cd.name()))));
    }
}

//# Add column to this object for an addColumn.
void RefTable::addRefCol (const ColumnDesc& columnDesc)
{
    ColumnDesc& cd = tdescPtr_p->addColumn (columnDesc);
    nameMap_p.define (cd.name(), cd.name());
    // Use cd (and not columnDesc) because underneath a pointer to its
    // BaseColumnDesc which is disastrous for the temporary columnDesc.
    colMap_p.define (cd.name(), cd.makeRefColumn
                     (this, baseTabPtr_p->getColumn(nameMap_p(cd.name()))));
    changed_p = True;
}
void RefTable::addRefCol (const TableDesc& tdesc)
{
    for (uInt i=0; i<tdesc.ncolumn(); i++) {
        addRefCol (tdesc[i]);
    }
}


//# Add a row number of the root table.
void RefTable::addRownr (uInt rnr)
{
    uInt nrow = rowStorage_p.nelements();
    if (nrrow_p >= nrow) {
        nrow = max ( nrow + 1024, uInt(1.2f * nrow));
	rowStorage_p.resize (nrow, True);
	rows_p = getStorage (rowStorage_p);
    }
    rows_p[nrrow_p++] = rnr;
    changed_p = True;
}

//# Set exact number of rows.
void RefTable::setNrrow (uInt nrrow)
{
    if (nrrow > nrrow_p) {
	throw (TableError ("RefTable::setNrrow: exceeds current nrrow"));
    }
    rows_p = getStorage (rowStorage_p);
    nrrow_p = nrrow;
    changed_p = True;
}


//# Test if the parent table is writable.
Bool RefTable::isWritable() const
{
    return baseTabPtr_p->isWritable();
}

void RefTable::copyRefTable (const String& newName, int tableOption)
{
    prepareCopyRename (newName, tableOption);
    // Save state, write, and restore state.
    Bool changed = changed_p;
    Int option   = option_p;
    String name  = name_p;
    changed_p = True;
    option_p  = tableOption;
    name_p    = newName;
    writeRefTable (False);
    changed_p = changed;
    option_p  = option;
    name_p    = name;
    madeDir_p = False;
}

void RefTable::copy (const String& newName, int tableOption) const
{
    // If a memory table, make a deep copy.
    if (tableType() == Table::Memory) {
        deepCopy (newName, Record(), tableOption, True, Table::AipsrcEndian,
                  False);
        // If not persistent, make the copy by writing the table.
    } else if (!madeDir_p) {
        const_cast<RefTable*>(this)->copyRefTable (newName, tableOption);
    } else {
        BaseTable::copy (newName, tableOption);
    }
}

void RefTable::deepCopy (const String& newName,
			 const Record& dataManagerInfo,
			 int tableOption, Bool, int endianFormat,
			 Bool noRows) const
{
    trueDeepCopy (newName, dataManagerInfo,
                  tableOption, endianFormat, noRows);
}

int RefTable::tableType() const
{
    return baseTabPtr_p->tableType();
}

TableDesc RefTable::actualTableDesc() const
{
    // Get the table description of reftable.
    const TableDesc& refDesc = tableDesc();
    // Get actual table desc of parent.
    // Create new tabledesc and copy keywords from parent.
    TableDesc rootDesc = baseTabPtr_p->actualTableDesc();
    TableDesc actualDesc(rootDesc, "", "", TableDesc::Scratch, False);
    // Copy the relevant columns and rename (because reftable
    // can have renamed columns).
    for (uInt i=0; i<refDesc.ncolumn(); i++) {
	const String& newName = refDesc.columnDesc(i).name();
	const String& oldName = nameMap_p(newName);
	ColumnDesc cdesc = rootDesc.columnDesc (oldName);
	cdesc.setName (newName);
	actualDesc.addColumn (cdesc);
    }
    // Invert the map to get map of old to new name
    // and use it to adjust the possible hypercolumn definitions.
    SimpleOrderedMap<String,String> map("", nameMap_p.ndefined());
    for (uInt i=0; i<nameMap_p.ndefined(); i++) {
        map.define (nameMap_p.getVal(i), nameMap_p.getKey(i));
    }
    actualDesc.adjustHypercolumns (map);
    return actualDesc;
}

Record RefTable::dataManagerInfo() const
{
    // Get the info of the parent table.
    // We only have to have this info for the columns in this table.
    Record dmi = baseTabPtr_p->dataManagerInfo();
    // Invert the map to get map of old to new name.
    SimpleOrderedMap<String,String> map("", nameMap_p.ndefined());
    for (uInt i=0; i<nameMap_p.ndefined(); i++) {
        map.define (nameMap_p.getVal(i), nameMap_p.getKey(i));
    }
    // Now remove all columns not part of it.
    // Use the new name.
    // Remove data managers without columns left.
    // Iterate in reverse order because of the remove we do.
    for (uInt i=dmi.nfields(); i>0;) {
        i--;
        Record& rec = dmi.rwSubRecord(i);
	Vector<String> vec (rec.asArrayString ("COLUMNS"));
	Vector<String> newVec(vec.nelements());
	uInt nc=0;
	for (uInt j=0; j<vec.nelements(); j++) {
	    const String* val = map.isDefined (vec(j));
	    if (val != 0) {
	        newVec(nc++) = *val;
	    }
	}
	// Remove field if no columns are left.
	// Otherwise store new names in subrecord.
	if (nc == 0) {
	    dmi.removeField(i);
	} else {
	    rec.define ("COLUMNS", newVec(Slice(0,nc)));
	}
    }
    return dmi;
}

void RefTable::showStructureExtra (ostream& os) const
{
  os << "out of " << baseTabPtr_p->tableName() << " (" 
     << baseTabPtr_p->nrow() << " rows, "
     << baseTabPtr_p->tableDesc().ncolumn() << " columns)" << endl;
}

//# Get the keyword set.
TableRecord& RefTable::keywordSet()
    { return baseTabPtr_p->keywordSet(); }

//# Get the keyword set.
TableRecord& RefTable::rwKeywordSet()
    { return baseTabPtr_p->rwKeywordSet(); }

BaseColumn* RefTable::getColumn (const String& columnName) const
{
    tdescPtr_p->columnDesc(columnName);             // check if column exists
    return colMap_p(columnName);
}
//# We cannot simply return colMap_p.getVal(columnIndex), because the order of
//# the columns in the description is important. So first get the column
//# name and use that as key.
BaseColumn* RefTable::getColumn (uInt columnIndex) const
{ 
    const String& name = tdescPtr_p->columnDesc(columnIndex).name();
    return colMap_p(name);
}
    

Vector<uInt>* RefTable::rowStorage()
    { return &rowStorage_p; }

//# Convert a vector of row numbers to row numbers in this table.
Vector<uInt> RefTable::rootRownr (const Vector<uInt>& rownrs) const
{
    uInt nrow = rownrs.nelements();
    Vector<uInt> rnr(nrow);
    for (uInt i=0; i<nrow; i++) {
	rnr(i) = rows_p[rownrs(i)];
    }
    return rnr;
}
	

BaseTable* RefTable::root()
    { return baseTabPtr_p; }
Bool RefTable::rowOrder() const
    { return rowOrd_p; }

Vector<uInt> RefTable::rowNumbers () const
{
    if (nrrow_p == rowStorage_p.nelements()) {
	return rowStorage_p;
    }
    Vector<uInt> vec (rowStorage_p);
    return vec(Slice(0, nrrow_p));
}


Bool RefTable::checkAddColumn (const String& name, Bool addToParent)
{
  if (! isWritable()) {
    throw TableInvOper ("Table::addColumn; table is not writable");
  }
  if (tdescPtr_p->isColumn(name)) {
    throw TableInvOper ("Table::addColumn; column " + name + " already exists");
  }
  if (baseTabPtr_p->tableDesc().isColumn(name)) {
    return False;
  }
  if (!addToParent) {
    throw TableInvOper ("RefTable::addColumn; column " + name +
                        " does not exist in parent table, but must not be added"
                        " (addToParent=False)");
  }
  return True;
}

void RefTable::addColumn (const ColumnDesc& columnDesc, Bool addToParent)
{
  if (checkAddColumn (columnDesc.name(), addToParent)) {
    baseTabPtr_p->addColumn (columnDesc, addToParent);
  }
  addRefCol (columnDesc);
}
void RefTable::addColumn (const ColumnDesc& columnDesc,
                          const String& dataManager, Bool byName,
                          Bool addToParent)
{
  if (checkAddColumn (columnDesc.name(), addToParent)) {
    baseTabPtr_p->addColumn (columnDesc, dataManager, byName, addToParent); 
  }
  addRefCol (columnDesc);
}
void RefTable::addColumn (const ColumnDesc& columnDesc,
                          const DataManager& dataManager,
                          Bool addToParent)
{
  if (checkAddColumn (columnDesc.name(), addToParent)) {
    baseTabPtr_p->addColumn (columnDesc,dataManager, addToParent);
  }
  addRefCol (columnDesc);
}
void RefTable::addColumn (const TableDesc& tableDesc,
                          const DataManager& dataManager,
                          Bool addToParent)
{
  // First check if all columns exist and can be added or not.
  // Collect all columns to be added to the parent.
  TableDesc addTabDesc;
  for (uInt i=0; i<tableDesc.ncolumn(); ++i) {
    if (checkAddColumn (tableDesc[i].name(), addToParent)) {
      addTabDesc.addColumn (tableDesc[i]);
    }
  }
  // Add to the parent if needed.
  if (addTabDesc.ncolumn() > 0) {
    baseTabPtr_p->addColumn (addTabDesc, dataManager, addToParent);
  }
  addRefCol (tableDesc);
}

//# Rows and columns can be removed and renamed.
Bool RefTable::canRemoveRow() const
    { return True; }
Bool RefTable::canRemoveColumn (const Vector<String>& columnNames) const
{
    return checkRemoveColumn (columnNames, False);
}
Bool RefTable::canRenameColumn (const String& columnName) const
    { return tdescPtr_p->isColumn (columnName); }

void RefTable::removeRow (uInt rownr)
{
    if (rownr >= nrrow_p) {
	throw (TableInvOper ("removeRow: rownr out of bounds"));
    }
    if (rownr < nrrow_p - 1) {
	objmove (rows_p+rownr, rows_p+rownr+1, nrrow_p-rownr-1);
    }
    nrrow_p--;
    changed_p = True;
}


void RefTable::removeColumn (const Vector<String>& columnNames)
{
    checkRemoveColumn (columnNames, True);
    for (uInt i=0; i<columnNames.nelements(); i++) {
        const String& name = columnNames(i);
        tdescPtr_p->removeColumn (name);
	nameMap_p.remove (name);
	delete colMap_p(name);
	colMap_p.remove (name);
    }
    changed_p = True;
}
 
void RefTable::renameColumn (const String& newName, const String& oldName)
{
    tdescPtr_p->renameColumn (newName, oldName);
    colMap_p.rename (newName, oldName);
    nameMap_p.rename (newName, oldName);
    changed_p = True;
}

void RefTable::renameHypercolumn (const String& newName, const String& oldName)
{
    tdescPtr_p->renameHypercolumn (newName, oldName);
    changed_p = True;
}


DataManager* RefTable::findDataManager (const String& name, Bool byColumn) const
{
    String origName(name);
    if (byColumn) {
        // A column can be renamed, so use the original name.
        origName = nameMap_p(name);
    }
    return baseTabPtr_p->findDataManager (origName, byColumn);
}


// And 2 index arrays, which are both in ascending order.
void RefTable::refAnd (uInt nr1, const uInt* inx1,
		       uInt nr2, const uInt* inx2)
{
    uInt allrow = (nr1 < nr2  ?  nr1 : nr2);  // max #output rows
    rowStorage_p.resize (allrow);             // allocate output storage
    rows_p = getStorage (rowStorage_p);
    uInt i1, i2, row1, row2;
    i1 = i2 = 0;
    while (True) {
	if (i1 >= nr1) {
	    row1 = 0xffffffff;                // end of inx1
	}else{
	    row1 = inx1[i1];                  // next element in inx1
	}
	if (i2 >= nr2) {
	    row2 = 0xffffffff;                // end of inx2
	}else{
	    row2 = inx2[i2];                  // next element in inx2
	}
	if (row1 == row2) {
	    if (row1 == 0xffffffff) break;    // end of both inx
	    rows_p[nrrow_p++] = row1;
	    i1++;
	    i2++;
	}else{
	    if (row1 < row2) {
		i1++;                         // next inx1
	    }else{
		i2++;                         // next inx2
	    }
	}
    }
    changed_p = True;
}

// Or 2 index arrays, which are both in ascending order.
void RefTable::refOr (uInt nr1, const uInt* inx1,
		      uInt nr2, const uInt* inx2)
{
    uInt allrow = nr1 + nr2;                  // max #output rows
    rowStorage_p.resize (allrow);             // allocate output storage
    rows_p = getStorage (rowStorage_p);
    uInt i1, i2, row1, row2;
    i1 = i2 = 0;
    while (True) {
	if (i1 >= nr1) {
	    row1 = 0xffffffff;                // end of inx1
	}else{
	    row1 = inx1[i1];                  // next element in inx1
	}
	if (i2 >= nr2) {
	    row2 = 0xffffffff;                // end of inx2
	}else{
	    row2 = inx2[i2];                  // next element in inx2
	}
	if (row1 == row2) {
	    if (row1 == 0xffffffff) break;    // end of both inx
	    rows_p[nrrow_p++] = row1;
	    i1++;
	    i2++;
	}else{
	    if (row1 < row2) {
		rows_p[nrrow_p++] = row1;
		i1++;                         // next inx1
	    }else{
		rows_p[nrrow_p++] = row2;
		i2++;                         // next inx2
	    }
	}
    }
    changed_p = True;
}

// Subtract 2 index arrays, which are both in ascending order.
void RefTable::refSub (uInt nr1, const uInt* inx1,
		       uInt nr2, const uInt* inx2)
{
    uInt allrow = nr1;                        // max #output rows
    rowStorage_p.resize (allrow);             // allocate output storage
    rows_p = getStorage (rowStorage_p);
    uInt i1, i2, row1, row2;
    i1 = i2 = 0;
    while (True) {
	if (i1 >= nr1) {
	    row1 = 0xffffffff;                // end of inx1
	}else{
	    row1 = inx1[i1];                  // next element in inx1
	}
	if (i2 >= nr2) {
	    row2 = 0xffffffff;                // end of inx2
	}else{
	    row2 = inx2[i2];                  // next element in inx2
	}
	if (row1 == row2) {
	    if (row1 == 0xffffffff) break;    // end of both inx
	    i1++;
	    i2++;
	}else{
	    if (row1 < row2) {
		rows_p[nrrow_p++] = row1;
		i1++;                         // next inx1
	    }else{
		i2++;                         // next inx2
	    }
	}
    }
    changed_p = True;
}

// Xor 2 index arrays, which are both in ascending order.
void RefTable::refXor (uInt nr1, const uInt* inx1,
		       uInt nr2, const uInt* inx2)
{
    uInt allrow = nr1 + nr2;                  // max #output rows
    rowStorage_p.resize (allrow);             // allocate output storage
    rows_p = getStorage (rowStorage_p);
    uInt i1, i2, row1, row2;
    i1 = i2 = 0;
    while (True) {
	if (i1 >= nr1) {
	    row1 = 0xffffffff;                // end of inx1
	}else{
	    row1 = inx1[i1];                  // next element in inx1
	}
	if (i2 >= nr2) {
	    row2 = 0xffffffff;                // end of inx2
	}else{
	    row2 = inx2[i2];                  // next element in inx2
	}
	if (row1 == row2) {
	    if (row1 == 0xffffffff) break;    // end of both inx
	    i1++;
	    i2++;
	}else{
	    if (row1 < row2) {
		rows_p[nrrow_p++] = row1;
		i1++;                         // next inx1
	    }else{
		rows_p[nrrow_p++] = row2;
		i2++;                         // next inx2
	    }
	}
    }
    changed_p = True;
}

// Negate a table.
void RefTable::refNot (uInt nr, const uInt* inx, uInt nrtot)
{
    // All rows not in the original table must be "selected".
    // The original table has NRTOT rows.
    // So loop through the inx-array and store all rownrs not in the array.
    uInt allrow = nrtot - nr;                 // #output rows
    rowStorage_p.resize (allrow);             // allocate output storage
    rows_p = getStorage (rowStorage_p);
    uInt start = 0;
    uInt i, j;
    for (i=0; i<nr; i++) {                    // loop through inx-array
	for (j=start; j<inx[i]; j++) {
	    rows_p[nrrow_p++] = j;            // not in inx-array
	}
	start = inx[i] + 1;
    }
    for (j=start; j<nrtot; j++) {             // handle last interval
	rows_p[nrrow_p++] = j;
    }
    changed_p = True;
}

} //# NAMESPACE CASACORE - END
