//# RefTable.cc: Class for a table as a view of another table
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

#include <aips/Tables/RefTable.h>
#include <aips/Tables/RefColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableLock.h>
#include <aips/Containers/SimOrdMapIO.h>
#include <aips/Lattices/Slice.h>
#include <aips/Utilities/Copy.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/Path.h>
#include <aips/Tables/TableError.h>


RefTable::RefTable (AipsIO& ios, const String& name, uInt nrrow, int opt,
		    const TableLock& lockOptions)
: BaseTable    (name, opt, nrrow),
  rowStorage_p (0),              // initially empty vector of rownrs
  nameMap_p    (""),
  colMap_p     ((RefColumn*)0),
  changed_p    (False)
{
    //# Read the file in.
    // Set initially to no write in destructor.
    // At the end it is reset. In this way nothing is written if
    // an exception is thrown during initialization.
    noWrite_p = True;
    getRef (ios, opt, lockOptions);
    noWrite_p = False;
}


RefTable::RefTable (BaseTable* btp, Bool order, uInt nrall)
: BaseTable    ("", Table::Scratch, nrall),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (order),
  rowStorage_p (nrall),       // allocate vector of rownrs
  nameMap_p    (""),
  colMap_p     ((RefColumn*)0),
  changed_p    (True)
{
    rows_p = getStorage (rowStorage_p);
    //# Copy the table description and create the columns.
    tdescPtr_p = new TableDesc (btp->tableDesc(), "", "", TableDesc::Scratch);
    if (tdescPtr_p == 0) {
	throw (AllocError ("RefTable::RefTable", 1));
    }
    setup (btp);
    //# Get root table (will be parent if btp is an reference table).
    //# Link to root table (ie. increase its reference count).
    baseTabPtr_p->link();
}

RefTable::RefTable (BaseTable* btp, const Vector<uInt>& rownrs)
: BaseTable    ("", Table::Scratch, rownrs.nelements()),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (True),
  rowStorage_p (0),
  nameMap_p    (""),
  colMap_p     ((RefColumn*)0),
  changed_p    (True)
{
    //# Copy the table description and create the columns.
    tdescPtr_p = new TableDesc (btp->tableDesc(), "", "", TableDesc::Scratch);
    if (tdescPtr_p == 0) {
	throw (AllocError ("RefTable::RefTable", 1));
    }
    setup (btp);
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
}

RefTable::RefTable (BaseTable* btp, const Vector<Bool>& mask)
: BaseTable    ("", Table::Scratch, 0),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (btp->rowOrder()),
  rowStorage_p (0),              // initially empty vector of rownrs
  nameMap_p    (""),
  colMap_p     ((RefColumn*)0),
  changed_p    (True)
{
    //# Copy the table description and create the columns.
    tdescPtr_p = new TableDesc (btp->tableDesc(), "", "", TableDesc::Scratch);
    if (tdescPtr_p == 0) {
	throw (AllocError ("RefTable::RefTable", 1));
    }
    setup (btp);
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
}

RefTable::RefTable (BaseTable* btp, const Vector<String>& columnNames)
: BaseTable    ("", Table::Scratch, btp->nrow()),
  baseTabPtr_p (btp->root()),
  rowOrd_p     (btp->rowOrder()),
  rowStorage_p (0),
  nameMap_p    (""),
  colMap_p     ((RefColumn*)0),
  changed_p    (True)
{
    //# Create table description by copying the selected columns.
    //# Create the columns.
    const TableDesc& td = btp->tableDesc();
    tdescPtr_p = new TableDesc ("", "", TableDesc::Scratch);
    if (tdescPtr_p == 0) {
	throw (AllocError ("RefTable::RefTable columnNames", 1));
    }
    for (uInt i=0; i<columnNames.nelements(); i++) {
	tdescPtr_p->addColumn (td.columnDesc (columnNames(i)));
    }
    setup (btp);
    //# Get the row numbers from the input table.
    //# Copy them to this table.
    rowStorage_p = btp->rowNumbers();
    rows_p = getStorage (rowStorage_p);
    //# Link to the root table.
    baseTabPtr_p->link();
}


RefTable::~RefTable()
{
    //# When needed, write the table files if not marked for delete
    if (!isMarkedForDelete()) {
	if (openedForWrite()  &&  !shouldNotWrite()) {
	    writeRefTable (True);
	}
    }
    //# Delete all RefColumn objects.
    for (uInt i=0; i<colMap_p.ndefined(); i++) {
	delete colMap_p.getVal(i);
    }
    //# Unlink from root.
    BaseTable::unlink (baseTabPtr_p);
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

Bool RefTable::isMultiUsed() const
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

void RefTable::flush (Bool sync)
{
    if (openedForWrite()) {
	writeRefTable (sync);
    }
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
void RefTable::writeRefTable (Bool sync)
{
    //# Write name and type of root and write object data.
    //# Do this only when something has changed.
    if (changed_p) {
	AipsIO ios;
	writeStart (ios);
	ios << "RefTable";
	ios.putstart ("RefTable", 1);
	// Make the name of the base table relative to this table.
	ios << Path::stripDirectory (baseTabPtr_p->tableName(),
				     tableName());
	ios << nameMap_p;
	ios << baseTabPtr_p->nrow();
	ios << rowOrd_p;
	ios.put (nrrow_p, rows_p);
	ios.putend();
	writeEnd (ios);
	changed_p = False;
    }
    //# Write the TableInfo.
    flushTableInfo();
    //# Write the data in the referred table.
    baseTabPtr_p->flush (sync);
}

//# Read a reference table from a file and read the associated table.
void RefTable::getRef (AipsIO& ios, int opt, const TableLock& lockOptions)
{
    //# Open the file, read name and type of root and read object data.
    String rootName;
    uInt rootNrow, nrrow;
    ios.getstart ("RefTable");
    ios >> rootName;
    rootName = Path::addDirectory (rootName, tableName());
    ios >> nameMap_p;
    ios >> rootNrow;
    ios >> rowOrd_p;
    ios >> nrrow;
    DebugAssert (nrrow == nrrow_p, AipsError);
    //# Resize the block of rownrs and read them in.
    rowStorage_p.resize (nrrow);
    rows_p = getStorage (rowStorage_p);
    ios.get (nrrow, rows_p);
    ios.getend();
    //# Now read in the root table referenced to.
    //# Check if #rows has not decreased, which is about the only thing
    //# we can do to make sure the referenced rows are still the same.
    Table tab;
    if (opt == Table::Old) {
	tab = Table(rootName, lockOptions, Table::Old);
    }else{
	tab = Table(rootName, lockOptions, Table::Update);
    }
    baseTabPtr_p = tab.baseTablePtr();
    if (rootNrow > baseTabPtr_p->nrow()) {
	throw (TableInvOper
	           ("RefTable::getRef, #rows in referenced table decreased"));
    }
    //# Build up the table description from the name map and the
    // description of the root table.
    const TableDesc& rootDesc = baseTabPtr_p->tableDesc();
    tdescPtr_p = new TableDesc ("", "", TableDesc::Scratch);
    if (tdescPtr_p == 0) {
	throw (AllocError ("RefTable::getRef", 1));
    }
    makeDesc (*tdescPtr_p, rootDesc, nameMap_p);
    //# Create the refColumns.
    makeRefCol();
    //# Great, everything is done.
    //# Now link to the root table.
    baseTabPtr_p->link();
}


//# Read description and #rows.
void RefTable::getLayout (TableDesc& desc, AipsIO& ios)
{
    String rootName;
    SimpleOrderedMap<String,String> nameMap("");
    ios.getstart ("RefTable");
    ios >> rootName;
    ios >> nameMap;
    // Get description of the parent table.
    TableDesc pdesc;
    Table::getLayout (pdesc, rootName);
    makeDesc (desc, pdesc, nameMap);
}

void RefTable::makeDesc (TableDesc& desc, const TableDesc& rootDesc,
			 SimpleOrderedMap<String,String>& nameMap)
{
    //# Build up the table description.
    //# It is possible that columns have disappeared from the root table.
    //# Remember these columns, so they be removed later from the map.
    //# The nameMap maps column names in this table to the names in the
    //# root table, so a rename is needed if names are different.
    uInt i;
    SimpleOrderedMap<String,void*> unknownCol ((RefColumn*)0);
    for (i=0; i<nameMap.ndefined(); i++) {
	if (rootDesc.isColumn (nameMap.getVal(i))) {
	    desc.addColumn (rootDesc.columnDesc (nameMap.getVal(i)));
	    if (nameMap.getKey(i) != nameMap.getVal(i)) {
		//# Renames are currently not supported anymore.
		throw (TableInternalError ("renamed column in RefTable"));
//#//		desc.renameColumn (nameMap.getKey(i), nameMap.getVal(i));
	    }
	}else{
	    unknownCol.define (nameMap.getKey(i), (void*)0);
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
void RefTable::setup (BaseTable* btp)
{
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	nameMap_p.define (tdescPtr_p->columnDesc(i).name(),
			  tdescPtr_p->columnDesc(i).name());
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
			          (this, baseTabPtr_p->getColumn(cd.name())));
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
    { return baseTabPtr_p->isWritable(); }

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
BaseColumn* RefTable::getColumn (uInt columnIndex) const
    { return colMap_p.getVal (columnIndex); }
    

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


//# Rows and columns can be removed and renamed.
Bool RefTable::canRemoveRow() const
    { return True; }
Bool RefTable::canRemoveColumn (const String&) const
    { return True; }
Bool RefTable::canRenameColumn() const
    { return True; }

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


void RefTable::removeColumn (const String&)
{ throw (TableInvOper ("RefTable::removeColumn not implemented yet")); }
 
void RefTable::renameColumn (const String&, const String&)
{ throw (TableInvOper ("RefTable::renameColumn not implemented yet")); }


DataManager* RefTable::findDataManager (const String& dataManagerName) const
{
    return baseTabPtr_p->findDataManager (dataManagerName);
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
