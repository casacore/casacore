//# Table.cc: Main interface class to table data
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

#include <aips/Tables/Table.h>
#include <aips/Tables/PlainTable.h>
#include <aips/Tables/RefTable.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/TableError.h>
#include <aips/Tables/StManColumn.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/IO/AipsIO.h>
#include <aips/OS/File.h>
#include <aips/OS/Directory.h>
#include <aips/OS/DirectoryIterator.h>


const Table::ScratchCallback* Table::scratchCallback_p = 0;

const Table::ScratchCallback* Table::setScratchCallback
                                    (const Table::ScratchCallback* fptr)
{
    const Table::ScratchCallback* cur = scratchCallback_p;
    scratchCallback_p = fptr;
    return cur;
}


Table::Table()
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{}

Table::Table (const String& name, TableOption option)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
    { open (name, "", option, TableLock(TableLock::AutoLocking)); }

Table::Table (const String& name, const TableLock& lockOptions,
	      TableOption option)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
    { open (name, "", option, lockOptions); }
    
Table::Table (const String& name, const String& type, TableOption option)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
    { open (name, type, option, TableLock(TableLock::AutoLocking)); }
    
Table::Table (const String& name, const String& type,
	      const TableLock& lockOptions, TableOption option)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
    { open (name, type, option, lockOptions); }

Table::Table (SetupNewTable& newtab, uInt nrrow, Bool initialize)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (newtab, nrrow, initialize,
				   TableLock(TableLock::AutoLocking));
    baseTabPtr_p->link();
}
Table::Table (SetupNewTable& newtab, TableLock::LockOption lockOption,
	      uInt nrrow, Bool initialize)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (newtab, nrrow, initialize,
				   TableLock(lockOption));
    baseTabPtr_p->link();
}
Table::Table (SetupNewTable& newtab, const TableLock& lockOptions,
	      uInt nrrow, Bool initialize)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (newtab, nrrow, initialize, lockOptions);
    baseTabPtr_p->link();
}

Table::Table (BaseTable* btp, Bool countIt)
: baseTabPtr_p     (btp),
  isCounted_p      (countIt),
  lastModCounter_p (0)
{
    if (isCounted_p  &&  baseTabPtr_p != 0) {
	baseTabPtr_p->link();
    }
}

Table::Table (const Table& that)
: baseTabPtr_p     (that.baseTabPtr_p),
  isCounted_p      (that.isCounted_p),
  lastModCounter_p (that.lastModCounter_p)
{
    if (isCounted_p  &&  baseTabPtr_p != 0) {
	baseTabPtr_p->link();
    }
}


Table::~Table()
{
    if (isCounted_p  &&  baseTabPtr_p != 0) {
	BaseTable::unlink (baseTabPtr_p);
    }
}

//# Do not write in case of an exception.
void Table::cleanup()
{
    if (baseTabPtr_p != 0) {
	baseTabPtr_p->doNotWrite();
    }
    this->Table::~Table();
}

Table& Table::operator= (const Table& that)
{
    if (isCounted_p  &&  baseTabPtr_p != 0) {
	BaseTable::unlink (baseTabPtr_p);
    }
    baseTabPtr_p     = that.baseTabPtr_p;
    isCounted_p      = that.isCounted_p;
    lastModCounter_p = that.lastModCounter_p;
    if (isCounted_p  &&  baseTabPtr_p != 0) {
	baseTabPtr_p->link();
    }
    return *this;
}


Bool Table::canDeleteTable (const String& tableName)
{
    String message;
    return canDeleteTable (message, tableName);
}
Bool Table::canDeleteTable (String& message, const String& tableName)
{
    if (! isWritable (tableName)) {
	message = "table is not writable";
	return False;
    }
    if (isOpened (tableName)) {
	message = "table is still open in this process";
	return False;
    }
    Table table(tableName);
    if (table.isMultiUsed()) {
	message = "table is still open in another process";
	return False;
    }
    return True;
}


void Table::deleteTable (const String& tableName)
{
    String message;
    if (! canDeleteTable (message, tableName)) {
	throw (TableError ("Table " + tableName + " cannot be deleted: " +
			   message));
    }
    Table table(tableName, Table::Delete);
}


Vector<String> Table::nonWritableFiles (const String& tableName)
{
    if (! isReadable (tableName)) {
	throw (TableError ("Table::nonWritableFiles: Table " + tableName +
			   " does not exist"));
    }
    uInt n=0;
    Vector<String> names;
    DirectoryIterator iter(tableName);
    while (! iter.pastEnd()) {
	if (! iter.file().isWritable()) {
	    names.resize (n+1, True);
	    names(n++) = iter.name();
	}
	iter++;
    }
    return names;
}


Bool Table::isNativeDataType (DataType dtype)
{
    return StManColumn::isNativeDataType (dtype);
}


//# The logic is similar to that in open.
uInt Table::getLayout (TableDesc& desc, const String& tableName)
{
    uInt nrow, format;
    String tp;
    AipsIO ios (Table::fileName(tableName));
    ios.getstart ("Table");
    ios >> nrow;
    ios >> format;
    ios >> tp;
    if (tp == "PlainTable") {
	PlainTable::getLayout (desc, ios);
    }else{
	if (tp == "RefTable") {
	    RefTable::getLayout (desc, ios);
	}else{
	    throw (TableInternalError
		              ("Table::getLayout: unknown table kind " + tp));
	}
    }
    ios.close();
    return nrow;
}


//# Open the table file and read it in if necessary.
void Table::open (const String& name, const String& type, int tableOption,
		  const TableLock& lockOptions)
{
    //# Option Delete is effectively the same as Old followed by a
    //# markForDelete.
    Bool deleteOpt = False;
    if (tableOption == Table::Delete) {
	tableOption = Table::Old;
	deleteOpt = True;
    }
    //# Look if the table is already in the cache.
    //# If so, link to it.
    BaseTable* btp = lookCache (name, tableOption, lockOptions);
    if (btp != 0) {
	baseTabPtr_p = btp;
    }else{
	//# Check if the table exists.
	if (! Table::isReadable (name)) {
	    throw (TableNoFile (name));
	}
	//# Determine the file option for the table.
	//# Only existing tables can be opened.
	//# This is guaranteed by the calling functions.
	ByteIO::OpenOption fopt = PlainTable::toAipsIOFoption (tableOption);
	//# Open the file.
	AipsIO ios (Table::fileName(name), fopt);
	//# Determine the kind of table by reading the type.
	String tp;
	uInt version = ios.getstart ("Table");
	uInt nrrow, format;
	ios >> nrrow;
	ios >> format;
	ios >> tp;
	if (tp == "PlainTable") {
	    baseTabPtr_p = new PlainTable (ios, version, name, type, nrrow,
					   tableOption, lockOptions);
	} else if (tp == "RefTable") {
	    baseTabPtr_p = new RefTable (ios, name, nrrow, tableOption,
					 lockOptions);
	}else{
	    throw (TableInternalError
		   ("Table::open: unknown table kind " + tp));
	}
	if (baseTabPtr_p == 0) {
	    throw (AllocError("Table::open",1));
	}
	ios.getend();
    }
    baseTabPtr_p->link();
    if (deleteOpt) {
	markForDelete();
    }
}

BaseTable* Table::lookCache (const String& name, int tableOption,
			     const TableLock& lockOptions)
{
    //# Exit if table is not in cache yet.
    PlainTable* btp = PlainTable::tableCache(name);
    if (btp == 0) {
	return btp;
    }
    //# Check if option matches. It does if equal.
    //# Otherwise it does if option in cached table is "more".
    //# Note that class PlainTable already throws an exception if
    //# a new table is created with the same name as an open table.
    int cachedTableOption = btp->tableOption();
    if ((tableOption == cachedTableOption)
    ||  ((cachedTableOption == Table::New
      ||  cachedTableOption == Table::NewNoReplace
      ||  cachedTableOption == Table::Update)
     &&  (tableOption == Table::Update
      ||  tableOption == Table::Old))) {
	btp->mergeLock (lockOptions);
	return btp;
    }
    if (cachedTableOption == Table::Old  &&  tableOption == Table::Update) {
	btp->mergeLock (lockOptions);
	btp->reopenRW();
	return btp;
    }
    throw (TableInvOper ("Table " + name +
			 " cannot be opened/created (already in cache)"));
    return 0;
}


void Table::throwIfNull() const
{
    if (isNull()) {
	throw (TableInvOper ("Table is null"));
    }
}


Bool Table::isOpened (const String& tableName)
{
    return ToBool (PlainTable::tableCache (tableName) != 0);
}


// Check if the table data has changed.
Bool Table::hasDataChanged()
{
    // If the table is not read locked try to get one (without waiting).
    // If not succeeding, another process is writing, thus data is changing.
    // Otherwise unlock immediately.
    if (! hasLock (False)) {
	if (! lock (False, 1)) {
	    return True;
	}
	unlock();
    }
    // Get the modify counter. If different, data have changed.
    uInt counter = baseTabPtr_p->getModifyCounter();
    if (counter != lastModCounter_p) {
	lastModCounter_p = counter;
	return True;
    }
    return False;
}

uInt Table::nAutoLocks()
{
    uInt n=0;
    const TableCache& cache = PlainTable::tableCache;
    uInt ntab = cache.ntable();
    for (uInt i=0; i<ntab; i++) {
	const PlainTable& table = *(cache(i));
	if (table.lockOptions().option() == TableLock::AutoLocking) {
	    //# Having a read lock is enough.
	    if (table.hasLock (False)) {
		n++;
	    }
	}
    }
    return n;
}

void Table::relinquishAutoLocks (Bool all)
{
    TableCache& cache = PlainTable::tableCache;
    uInt ntab = cache.ntable();
    for (uInt i=0; i<ntab; i++) {
	PlainTable& table = *(cache(i));
	if (table.lockOptions().option() == TableLock::AutoLocking) {
	    //# Having a read lock is enough.
	    if (table.hasLock (False)) {
		if (all) {
		    table.unlock();
		}else{
		    table.autoReleaseLock();
		}
	    }
	}
    }
}



Vector<uInt> Table::rowNumbers () const
    { return baseTabPtr_p->rowNumbers(); }

//# Sort on a single column.
//# This is converted to a sort on a vector of column names.
Table Table::sort (const String& name, int order, int option) const
{
    //# Turn the name argument into a block.
    return sort (Block<String>(1, name), order, option);
}

//# Sort on multiple columns, where a global order is given.
//# This is converted to a sort with mixed orders.
Table Table::sort (const Block<String>& names,
		   int order, int option) const
{
    //# Expand the order argument into a block.
    return sort (names, Block<Int>(names.nelements(), order), option);
}

//# Sort on multiple columns and orders.
Table Table::sort (const Block<String>& names,
		   const Block<Int>& orders, int option) const
{
    //# Insert a block with zero compare function pointers.
    return sort (names,
	      PtrBlock<ObjCompareFunc*>(names.nelements(), (ObjCompareFunc*)0),
	      orders, option);
}

//# Sort on multiple columns and orders with given functions.
Table Table::sort (const Block<String>& names,
		   const PtrBlock<ObjCompareFunc*>& cmpFuncs,
		   const Block<Int>& orders, int option) const
    { return Table(baseTabPtr_p->sort (names, cmpFuncs, orders, option)); }


//# Create an expression node to handle a keyword.
//# The code to handle this is in TableExprNode, because there the
//# differentation between data types is being made.
TableExprNode Table::key (const String& name) const
    { return TableExprNode::newKeyConst (keywordSet(), name); }

//# Create an expression node for a column.
TableExprNode Table::col (const String& name) const
    { return TableExprNode::newColumnNode (*this, baseTabPtr_p, name); }

//# Create an expression node for either a keyword or column.
TableExprNode Table::keyCol (const String& name) const
{
    if (tableDesc().isColumn (name)) {
	return col (name);
    }else{
	return key (name);
    }
}

TableExprNode Table::nodeRownr(uInt origin) const
{
    return TableExprNode::newRownrNode (baseTabPtr_p, origin);
}

TableExprNode Table::nodeRandom () const
{
    return TableExprNode::newRandomNode (baseTabPtr_p);
}


//# Select rows based on an expression.
Table Table::operator() (const TableExprNode& expr) const
    { return Table (baseTabPtr_p->select (expr)); }
//# Select rows based on row numbers.
Table Table::operator() (const Vector<uInt>& rownrs) const
    { return Table (baseTabPtr_p->select (rownrs)); }
//# Select rows based on a mask.
Table Table::operator() (const Block<Bool>& mask) const
    { return Table (baseTabPtr_p->select (mask)); }

//# Select columns.
Table Table::project (const Block<String>& names) const
    { return Table (baseTabPtr_p->project (names)); }

//# Combine tables.
Table Table::operator& (const Table& that) const
    { return Table(baseTabPtr_p->tabAnd (that.baseTabPtr_p)); }
Table Table::operator| (const Table& that) const
    { return Table(baseTabPtr_p->tabOr  (that.baseTabPtr_p)); }
Table Table::operator- (const Table& that) const
    { return Table(baseTabPtr_p->tabSub (that.baseTabPtr_p)); }
Table Table::operator^ (const Table& that) const
    { return Table(baseTabPtr_p->tabXor (that.baseTabPtr_p)); }
Table Table::operator! () const
    { return Table(baseTabPtr_p->tabNot()); }


//# Test if table exists and is readable.
Bool Table::isReadable (const String& tableName)
{
    //# Test if the table file exists.
    File file (Table::fileName(tableName));
    if (!file.exists()) {
	return False;
    }
    //# Open the table file and get its type.
    //# An exception may be thrown, but chances are very low.
    AipsIO ios (Table::fileName(tableName));
    Bool valid = True;
    try {
	if (ios.getNextType() != "Table") {
	    valid = False;
	}
    } catch (AipsError x) {
	valid = False;
    } end_try;
    return valid;
}
//# Test if table exists and is writable.
Bool Table::isWritable (const String& tableName)
{
    if (! isReadable (tableName)) {
	return False;
    }
    File file (Table::fileName(tableName));
    return file.isWritable();
}


//# Make the table file name.
String Table::fileName (const String& tableName)
{
    return tableName + "/table.dat";
}


//# Write a table to AipsIO (for TypedKeywords<Table>).
AipsIO& operator<< (AipsIO& ios, const Table& tab)
{
    ios << tab.tableName();
    return ios;
}

//# Read a table from AipsIO (for TypedKeywords<Table>).
//#// There are 2 things to be done:
//#// 1. Now the table is opened as Update if it is writable.
//#//    It's better to do that if the Table is opened as Update.
//#// 2. Only read in the table when needed (i.e. when a get for
//#//    the table is done).
AipsIO& operator>> (AipsIO& ios, Table& tab)
{
    tab.getTableKeyword (ios, True);
    return ios;
}

void Table::getTableKeyword (AipsIO& ios, Bool openWritable)
{
    String name;
    ios >> name;
    TableOption opt = Table::Old;
    if (openWritable  &&  Table::isWritable (name)) {
	opt = Table::Update;
    }
    Table table(name, opt);
    operator= (table);
}

//# Write a table to ostream (for TypedKeywords<Table>).
ostream& operator<< (ostream& ios, const Table& tab)
{
    ios << "Table ";
    ios << tab.tableName();
    ios << "  (";
    ios << tab.tableDesc().ncolumn();
    ios << " columns, ",
    ios << tab.nrow();
    ios << " rows)";
    ios << endl;
    return ios;
}
