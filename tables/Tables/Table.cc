//# Table.cc: Main interface class to table data
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

#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/MemoryTable.h>
#include <casacore/tables/Tables/RefTable.h>
#include <casacore/tables/Tables/ConcatTable.h>
#include <casacore/tables/Tables/NullTable.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/DirectoryIterator.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Table::ScratchCallback* Table::scratchCallback_p = 0;

Table::ScratchCallback* Table::setScratchCallback
                                    (Table::ScratchCallback* fptr)
{
    Table::ScratchCallback* cur = scratchCallback_p;
    scratchCallback_p = fptr;
    return cur;
}


Table::Table()
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new NullTable();
    baseTabPtr_p->link();
}

  Table::Table (const String& name, TableOption option, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
  open (name, "", option, TableLock(), tsmOpt);
}

Table::Table (const String& name, const TableLock& lockOptions,
	      TableOption option, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
  open (name, "", option, lockOptions, tsmOpt);
}

  Table::Table (const String& name, const String& type, TableOption option,
                const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
  open (name, type, option, TableLock(), tsmOpt);
}

Table::Table (const String& name, const String& type,
	      const TableLock& lockOptions, TableOption option,
              const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
  open (name, type, option, lockOptions, tsmOpt);
}

  Table::Table (Table::TableType type, Table::EndianFormat endianFormat,
                const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    SetupNewTable newtab("", TableDesc(), Table::Scratch);
    if (type == Table::Memory) {
        baseTabPtr_p = new MemoryTable (newtab, 0, False);
    } else {
        baseTabPtr_p = new PlainTable (newtab, 0, False,
				       TableLock(), endianFormat, tsmOpt);
    }
    baseTabPtr_p->link();
}

Table::Table (SetupNewTable& newtab, rownr_t nrrow, Bool initialize,
	      Table::EndianFormat endianFormat, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (newtab, nrrow, initialize,
				   TableLock(), endianFormat, tsmOpt);
    baseTabPtr_p->link();
}
Table::Table (SetupNewTable& newtab, Table::TableType type,
	      rownr_t nrrow, Bool initialize,
	      Table::EndianFormat endianFormat, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    if (type == Table::Memory) {
        baseTabPtr_p = new MemoryTable (newtab, nrrow, initialize);
    } else {
        baseTabPtr_p = new PlainTable (newtab, nrrow, initialize,
				       TableLock(), endianFormat, tsmOpt);
    }
    baseTabPtr_p->link();
}
Table::Table (SetupNewTable& newtab, Table::TableType type,
	      const TableLock& lockOptions,
	      rownr_t nrrow, Bool initialize,
	      Table::EndianFormat endianFormat, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    if (type == Table::Memory) {
        baseTabPtr_p = new MemoryTable (newtab, nrrow, initialize);
    } else {
        baseTabPtr_p = new PlainTable (newtab, nrrow, initialize,
				       lockOptions, endianFormat, tsmOpt);
    }
    baseTabPtr_p->link();
}
Table::Table (SetupNewTable& newtab, TableLock::LockOption lockOption,
	      rownr_t nrrow, Bool initialize, Table::EndianFormat endianFormat,
              const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (newtab, nrrow, initialize,
				   TableLock(lockOption),
				   endianFormat, tsmOpt);
    baseTabPtr_p->link();
}
Table::Table (SetupNewTable& newtab, const TableLock& lockOptions,
	      rownr_t nrrow, Bool initialize, Table::EndianFormat endianFormat,
              const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (newtab, nrrow, initialize, lockOptions,
				   endianFormat, tsmOpt);
    baseTabPtr_p->link();
}

#ifdef HAVE_MPI

Table::Table (MPI_Comm mpiComm, Table::TableType type, Table::EndianFormat endianFormat,
                const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    SetupNewTable newtab("", TableDesc(), Table::Scratch);
    if (type == Table::Memory) {
        baseTabPtr_p = new MemoryTable (newtab, 0, False);
    } else {
        baseTabPtr_p = new PlainTable (mpiComm, newtab, 0, False,
				       TableLock(), endianFormat, tsmOpt);
    }
    baseTabPtr_p->link();
}

Table::Table (MPI_Comm mpiComm, SetupNewTable& newtab, rownr_t nrrow, Bool initialize,
	      Table::EndianFormat endianFormat, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (mpiComm, newtab, nrrow, initialize,
				   TableLock(), endianFormat, tsmOpt);
    baseTabPtr_p->link();
}

Table::Table (MPI_Comm mpiComm, SetupNewTable& newtab, Table::TableType type,
	      rownr_t nrrow, Bool initialize,
	      Table::EndianFormat endianFormat, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    if (type == Table::Memory) {
        baseTabPtr_p = new MemoryTable (newtab, nrrow, initialize);
    } else {
        baseTabPtr_p = new PlainTable (mpiComm, newtab, nrrow, initialize,
				       TableLock(), endianFormat, tsmOpt);
    }
    baseTabPtr_p->link();
}

Table::Table (MPI_Comm mpiComm, SetupNewTable& newtab, Table::TableType type,
	      const TableLock& lockOptions,
	      rownr_t nrrow, Bool initialize,
	      Table::EndianFormat endianFormat, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    if (type == Table::Memory) {
        baseTabPtr_p = new MemoryTable (newtab, nrrow, initialize);
    } else {
        baseTabPtr_p = new PlainTable (mpiComm, newtab, nrrow, initialize,
				       lockOptions, endianFormat, tsmOpt);
    }
    baseTabPtr_p->link();
}

Table::Table (MPI_Comm mpiComm, SetupNewTable& newtab, TableLock::LockOption lockOption,
	      rownr_t nrrow, Bool initialize, Table::EndianFormat endianFormat,
              const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (mpiComm, newtab, nrrow, initialize,
				   TableLock(lockOption),
				   endianFormat, tsmOpt);
    baseTabPtr_p->link();
}

Table::Table (MPI_Comm mpiComm, SetupNewTable& newtab, const TableLock& lockOptions,
	      rownr_t nrrow, Bool initialize, Table::EndianFormat endianFormat,
              const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new PlainTable (mpiComm, newtab, nrrow, initialize, lockOptions,
				   endianFormat, tsmOpt);
    baseTabPtr_p->link();
}

#endif

Table::Table (const Block<Table>& tables,
	      const Block<String>& subTables,
              const String& subDirName)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new ConcatTable (tables, subTables, subDirName);
    baseTabPtr_p->link();
}

Table::Table (const Block<String>& tableNames,
	      const Block<String>& subTables,
	      TableOption option, const TSMOption& tsmOpt,
              const String& subDirName)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
    baseTabPtr_p = new ConcatTable (tableNames, subTables, subDirName,
				    option, TableLock(), tsmOpt);
    baseTabPtr_p->link();
}

Table::Table (const Block<String>& tableNames,
	      const Block<String>& subTables,
	      const TableLock& lockOptions,
	      TableOption option, const TSMOption& tsmOpt)
: baseTabPtr_p     (0),
  isCounted_p      (True),
  lastModCounter_p (0)
{
  baseTabPtr_p = new ConcatTable (tableNames, subTables, String(),
				    option, lockOptions, tsmOpt);
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
#ifdef CASACORE_UNLOCK_TABLE_ON_DESTRUCT
        unlock();
#endif
	BaseTable::unlink (baseTabPtr_p);
    }
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

Block<String> Table::getPartNames (Bool recursive) const
{
    Block<String> names;
    baseTabPtr_p->getPartNames (names, recursive);
    return names;
}

void Table::closeSubTables() const
{
  return keywordSet().closeTables();
}

Vector<String> Table::nonWritableFiles (const String& tableName)
{
    String tabName = Path(tableName).absoluteName();
    if (! isReadable (tabName)) {
	throw (TableError ("Table::nonWritableFiles: Table " + tabName +
			   " does not exist"));
    }
    uInt n=0;
    Vector<String> names;
    DirectoryIterator iter(tabName);
    while (! iter.pastEnd()) {
	if (! iter.file().isWritable()) {
	    names.resize (n+1, True);
	    names(n++) = iter.name();
	}
	iter++;
    }
    return names;
}


Table::EndianFormat Table::endianFormat() const
{
  return baseTabPtr_p->asBigEndian() ?  Table::BigEndian : Table::LittleEndian;
}

Bool Table::isNativeDataType (DataType dtype)
{
    return StManColumnBase::isNativeDataType (dtype);
}



void Table::copy (const String& newName, TableOption option,
		  Bool noRows) const
{
    if (noRows) {
        baseTabPtr_p->deepCopy (newName, Record(), StorageOption(),
                                option, False, AipsrcEndian, noRows);
    } else {
        baseTabPtr_p->copy (newName, option);
    }
}

void Table::deepCopy (const String& newName,
		      TableOption option,
		      Bool valueCopy,
		      EndianFormat endianFormat,
		      Bool noRows) const
{
    baseTabPtr_p->deepCopy (newName, Record(), StorageOption(),
                            option, valueCopy, endianFormat, noRows);
}

Table Table::copyToMemoryTable (const String& newName, Bool noRows) const
{
  Table newtab = TableCopy::makeEmptyMemoryTable (newName, *this, noRows);
  if (!noRows) {
    TableCopy::copyRows (newtab, *this);
  }
  TableCopy::copyInfo (newtab, *this);
  TableCopy::copySubTables (newtab, *this, noRows);
  return newtab;
}


//# Open the table file and read it in if necessary.
void Table::open (const String& name, const String& type, int tableOption,
		  const TableLock& lockOptions, const TSMOption& tsmOpt)
{
    //# Option Delete is effectively the same as Old followed by a
    //# markForDelete.
    Bool deleteOpt = False;
    if (tableOption == Table::Delete) {
	tableOption = Table::Old;
	deleteOpt = True;
    }
    // Make name absolute in case a chdir is done in e.g. Python.
    String absName = Path(name).absoluteName();
    //# Look if the table is already in the cache.
    //# If so, link to it.
    BaseTable* btp = lookCache (absName, tableOption, lockOptions);
    if (btp != 0) {
	baseTabPtr_p = btp;
    }else{
        //# Check if the table directory exists.
        File dir(absName);
        if (!dir.exists()) {
            throw TableNoFile(absName);
        }
        if (!dir.isDirectory()) {
            throw TableNoDir(absName);
        }
        //# Check if the table.dat file exists.
        String desc = Table::fileName(absName);
        File file (desc);
        if (!file.exists()) {
            throw TableNoDatFile(desc);
        }
        //# Read the file type and verify that it is a table
        AipsIO ios (desc);
        String t = ios.getNextType();
        if (t != "Table") {
            throw TableInvType(absName, "Table", t);
        }
	//# Check if the table exists.
	if (! Table::isReadable (absName)) {
	    throw (TableNoFile (absName));
	}
	// Create the BaseTable object and add a PlainTable to the cache.
	baseTabPtr_p = makeBaseTable (absName, type, tableOption,
				      lockOptions, tsmOpt, True, 0);
    }
    baseTabPtr_p->link();
    if (deleteOpt) {
	markForDelete();
    }
}

// NOTE: When changing this function because of new Table versions, also change
// TableUtil::getLayout !!!!!
BaseTable* Table::makeBaseTable (const String& name, const String& type,
				 int tableOption, const TableLock& lockOptions,
				 const TSMOption& tsmOpt,
                                 Bool addToCache, uInt locknr)
{
    BaseTable* baseTabPtr = 0;
    //# Determine the file option for the table.
    //# Only existing tables can be opened.
    //# This is guaranteed by the calling functions.
    ByteIO::OpenOption fopt = PlainTable::toAipsIOFoption (tableOption);
    //# Open the file.
    AipsIO ios (Table::fileName(name), fopt);
    //# Determine the kind of table by reading the type.
    String tp;
    uInt version = ios.getstart ("Table");
    if (version > 3) {
      throw TableError ("Table version " + String::toString(version) +
                        " not supported by this version of Casacore");
    }
    uInt format;
    rownr_t nrrow;
    if (version > 2) {
      ios >> nrrow;
    } else {
      uInt n;
      ios >> n;
      nrrow = n;
    }
    ios >> format;
    ios >> tp;
    if (tp == "PlainTable") {
	baseTabPtr = new PlainTable (ios, version, name, type, nrrow,
				     tableOption, lockOptions, tsmOpt,
                                     addToCache, locknr);
    } else if (tp == "RefTable") {
	baseTabPtr = new RefTable (ios, name, nrrow, tableOption,
                                   lockOptions, tsmOpt);
    } else if (tp == "ConcatTable") {
	baseTabPtr = new ConcatTable (ios, name, nrrow, tableOption,
				      lockOptions, tsmOpt);
    } else {
	throw (TableInternalError
	       ("Table::open: unknown table kind " + tp));
    }
    return baseTabPtr;
}

BaseTable* Table::lookCache (const String& name, int tableOption,
			     const TableLock& lockOptions)
{
    return PlainTable::tableCache().lookCache (name, tableOption,
                                               lockOptions);
}


void Table::throwIfNull() const
{
    if (isNull()) {
	throw (TableInvOper ("Table is null"));
    }
}


Bool Table::isOpened (const String& tableName)
{
    return (PlainTable::tableCache()(Path(tableName).absoluteName()) != 0);
}


// Check if the table data has changed.
Bool Table::hasDataChanged()
{
    // If the table is not read locked try to get one (without waiting).
    // If not succeeding, another process is writing, thus data is changing.
    // Otherwise unlock immediately.
    if (! hasLock (FileLocker::Read)) {
	if (! lock (FileLocker::Read, 1)) {
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
  return PlainTable::tableCache().nAutoLocks();
}

void Table::relinquishAutoLocks (Bool all)
{
  PlainTable::tableCache().relinquishAutoLocks (all);
}

Vector<String> Table::getLockedTables (FileLocker::LockType lockType,
                                       int lockOption)
{
  return PlainTable::tableCache().getLockedTables (lockType, lockOption);
}


TableRecord& Table::rwKeywordSet()
{
    if (! isWritable()) {
	throw (TableError ("Table::rwKeywordSet cannot be used: table "
			   + tableName() + " is not writable"));
    }
    return baseTabPtr_p->rwKeywordSet();
}

Bool Table::canRemoveColumn (const String& columnName) const
{
    return baseTabPtr_p->canRemoveColumn (Vector<String>(1, columnName));
}
void Table::removeColumn (const String& columnName)
{
    baseTabPtr_p->removeColumn (Vector<String>(1, columnName));
}

RowNumbers Table::rowNumbers () const
    { return baseTabPtr_p->rowNumbers(); }

RowNumbers Table::rowNumbers (const Table& that, Bool tryFast) const
{
    Vector<rownr_t> thisRows(rowNumbers());
    const rownr_t highValue = std::numeric_limits<rownr_t>::max();
    // If that is the root table of this, we can simply use rowNumbers().
    // The same is true if empty.
    if (that.baseTabPtr_p == baseTabPtr_p->root()  ||  nrow() == 0) {
      return thisRows;
    }
    // Get the rowNumbers of that.
    Vector<rownr_t> thatRows(that.rowNumbers());
    // Try if a fast conversion can be done.
    // That is the case if this is not a superset of that and if orders match.
    if (tryFast) {
      Vector<rownr_t> outRows;
      if (fastRowNumbers (thisRows, thatRows, outRows)) {
        return outRows;
      }
    }
    // Alas, we have to do it the hard way.
    // Transform the rowNumbers of that to a vector
    // mapping rownr in root to rownr in that.
    rownr_t nrthat = thatRows.nelements();
    rownr_t maxv = nrthat;
    Vector<rownr_t> rownrs(thatRows);
    // That mapping only needs to be done if that is not a root table.
    // Non-used rownrs are initialized with a very high value.
    if (! that.isRootTable()) {
        maxv = max(thatRows);
        Vector<rownr_t> tmp(maxv+1, highValue);
	rownrs.reference (tmp);
    }
    Bool deleteIt;
    rownr_t* rownrsData = rownrs.getStorage (deleteIt);
    // Now make the mapping.
    // thatRows is not needed anymore, so resize at the end to reclaim memory.
    if (! that.isRootTable()) {
        Bool deleteThat;
        const rownr_t* thatRowData = thatRows.getStorage (deleteThat);
	for (rownr_t i=0; i<nrthat; i++) {
	    rownrsData[thatRowData[i]] = i;
	}
	thatRows.freeStorage (thatRowData, deleteThat);
	thatRows.resize();
    }
    // Use the first mapping to map the rownrs in this to rownrs in that.
    // First get the rownrs of this in root to achieve it.
    // Use a very high value if the rownr is too high.
    thisRows.unique();
    Bool deleteThis;
    rownr_t* thisRowData = thisRows.getStorage (deleteThis);
    rownr_t nrthis = thisRows.nelements();
    for (rownr_t i=0; i<nrthis; i++) {
        if (thisRowData[i] > maxv) {
	    thisRowData[i] = highValue;
	} else {
	    thisRowData[i] = rownrsData[thisRowData[i]];
	}
    }
    thisRows.putStorage (thisRowData, deleteThis);
    // rownrsData is not used, so don't need to be put.
    // freeStorage requires const pointer though.
    const rownr_t *dummy(rownrsData);
    rownrs.freeStorage (dummy, deleteIt);
    return thisRows;
}

Bool Table::fastRowNumbers (const Vector<rownr_t>& v1, const Vector<rownr_t>& v2,
                            Vector<rownr_t>& rows) const
{
  // v1 cannot be a superset of v2.
  if (v1.size() > v2.size()) {
    return False;
  }
  rows.resize (v1.size());
  if (v1.empty()) {
    return True;
  }
  Bool d1,d2,d3;
  const rownr_t* r1 = v1.getStorage (d1);
  const rownr_t* r2 = v2.getStorage (d2);
  rownr_t* routc = rows.getStorage (d3);
  rownr_t* rout = routc;
  rownr_t i1=0;
  rownr_t i2=0;
  Bool ok = True;
  while (ok) {
    if (r1[i1] == r2[i2]) {
      *rout++ = i2;
      if (++i1 >= v1.size()) {
        break;
      }
    }
    if (++i2 >= v2.size()) {
      ok = False;
    }
  }
  v1.freeStorage (r1, d1);
  v2.freeStorage (r2, d2);
  rows.putStorage (routc, d3);
  return ok;
}

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
    //# Insert a block with null compare objects.
    return sort (names,
                 Block<CountedPtr<BaseCompare> >(names.nelements()),
                 orders, option);
}

//# Sort on multiple columns and orders with given functions.
Table Table::sort (const Block<String>& names,
		   const Block<CountedPtr<BaseCompare> >& cmpObjs,
		   const Block<Int>& orders, int option) const
    { return Table(baseTabPtr_p->sort (names, cmpObjs, orders, option)); }


//# Create an expression node to handle a keyword.
//# The code to handle this is in TableExprNode, because there the
//# differentation between data types is being made.
TableExprNode Table::key (const String& keywordName) const
{
    Vector<String> names(1);
    names(0) = keywordName;
    return TableExprNode::newKeyConst (keywordSet(), names);
}
TableExprNode Table::key (const Vector<String>& fieldNames) const
{
    return TableExprNode::newKeyConst (keywordSet(), fieldNames);
}
TableExprNode Table::col (const String& columnName) const
{
    Vector<String> fieldNames;
    return TableExprNode::newColumnNode (*this, columnName, fieldNames);
}
TableExprNode Table::col (const String& columnName,
			  const Vector<String>& fieldNames) const
{
    return TableExprNode::newColumnNode (*this, columnName, fieldNames);
}

//# Create an expression node for either a keyword or column.
TableExprNode Table::keyCol (const String& name,
			     const Vector<String>& fieldNames) const
{
    if (tableDesc().isColumn (name)) {
	return col (name, fieldNames);
    }else{
	uInt nr = fieldNames.nelements();
	Vector<String> names (nr + 1);
	names (Slice(1,nr)) = fieldNames;
	names(0) = name;
	return key (names);
    }
}

TableExprNode Table::nodeRownr(rownr_t origin) const
{
    return TableExprNode::newRownrNode (*this, origin);
}

TableExprNode Table::nodeRandom () const
{
    return TableExprNode::newRandomNode (*this);
}


//# Select rows based on an expression.
Table Table::operator() (const TableExprNode& expr,
                         rownr_t maxRow, rownr_t offset) const
    { return Table (baseTabPtr_p->select (expr, maxRow, offset)); }
//# Select rows based on row numbers.
Table Table::operator() (const RowNumbers& rownrs) const
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
Bool Table::isReadable (const String& tableName, Bool throwIf)
{
    String tabName = Path(tableName).absoluteName();
    // First see if it is in the table cache. By doing so a new table
    // does not need to exist on disk yet.
    if (PlainTable::tableCache()(tabName)) {
      return True;
    }
    //# Check if the table directory exists.
    File dir(tabName);
    if (!dir.exists()) {
        if (throwIf) {
            throw TableNoFile(tabName);
        }
        return False;
    }
    if (!dir.isDirectory()) {
        if (throwIf) {
            throw TableNoDir(tabName);
        }
        return False;
    }
    //# Test if the table.dat file exists.
    String datFile = Table::fileName(tabName);
    File file (datFile);
    if (!file.exists()) {
        if (throwIf) {
            throw TableNoDatFile(tabName);
        }
        return False;
    }
    //# Open the table file and get its type.
    //# An exception might be thrown, but chances are very low.
    AipsIO ios (Table::fileName(tabName));
    Bool valid = True;
    try {
	if (ios.getNextType() != "Table") {
            if (throwIf) {
                throw TableInvType(tabName, "Table", tabName);
            }
	    valid = False;
	}
    } catch (std::exception& x) {
        if (throwIf) {
            throw;
        }
	valid = False;
    }
    return valid;
}
//# Test if table exists and is writable.
Bool Table::isWritable (const String& tableName, Bool throwIf)
{
    String tabName = Path(tableName).absoluteName();
    if (! isReadable (tabName, throwIf)) {
	return False;
    }
    File file (Table::fileName(tabName));
    Bool wb = file.isWritable();
    if (throwIf  &&  !wb) {
        throw TableError("Table " + tableName + " is not writable");
    }
    return wb;
}


TableDesc Table::actualTableDesc() const
{
    return baseTabPtr_p->actualTableDesc();
}

Record Table::dataManagerInfo() const
{
    return baseTabPtr_p->dataManagerInfo();
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
    ios << tab.tableDesc().ncolumn() << " columns, ";
    ios << uInt(tab.nrow()) << " rows)";
    ios << endl;
    return ios;
}

void Table::showKeywords (ostream& ios, Bool showSubTables,
                          Bool showTabKey, Bool showColKey,
                          Int maxVal) const
{
  if (showTabKey || showColKey) {
    // Show table and/or column keywords.
    ios << endl
        << "Keywords of main table " << endl
        << "----------------------" << endl;
    showKeywordSets (ios, showTabKey, showColKey, maxVal);
    if (showSubTables) {
      // Also show them in the subtables.
      TableRecord keyset (keywordSet());
      for (uInt i=0; i<keyset.nfields(); ++i) {
        if (keyset.dataType(i) == TpTable) {
          Table tab(keyset.asTable(i));
          // Do not show if the subtable references the parent table.
          if (! isSameRoot (tab)) {
            ios << "Keywords of subtable " << keyset.name(i) << endl
                << "--------------------" << endl;
            tab.showKeywordSets (ios, showTabKey, showColKey, maxVal);
          }
        }
      }
    }
  }
}

void Table::showKeywordSets (ostream& ios,
                             Bool showTabKey, Bool showColKey,
                             Int maxVal) const
{
  Bool shown = False;
  if (showTabKey) {
    if (keywordSet().size() > 0) {
      ios << "  Table Keywords" << endl;
      keywordSet().print (ios, maxVal, "    ");
      ios << endl;
      shown = True;
    }
  }
  if (showColKey) {
    Vector<String> colNames (tableDesc().columnNames());
    for (uInt i=0; i<colNames.size(); ++i) {
      TableRecord keys (TableColumn(*this, colNames[i]).keywordSet());
      if (keys.size() > 0) {
        ios << "  Column " << colNames[i] << endl;
        keys.print (ios, maxVal, "    ");
        ios << endl;
        shown = True;
      }
    }
  }
  if (!shown) {
    ios << endl;
  }
}


// Deprecated functions; now in TableUtil.h.
Table Table::openTable (const String& tableName,
                            TableOption tabOpt,
                            const TSMOption& tsmOpt)
  { return TableUtil::openTable (tableName, tabOpt, tsmOpt); }
Table Table::openTable (const String& tableName,
                        const TableLock& lockOptions,
                        TableOption tabOpt,
                        const TSMOption& tsmOpt)
  { return TableUtil::openTable (tableName, lockOptions, tabOpt, tsmOpt); }
Bool Table::canDeleteTable (const String& tableName,
                            Bool checkSubTables)
  { return TableUtil::canDeleteTable (tableName, checkSubTables); }
Bool Table::canDeleteTable (String& message, const String& tableName,
                            Bool checkSubTables)
  { return TableUtil::canDeleteTable (message, tableName, checkSubTables); }
void Table::deleteTable (const String& tableName,
                         Bool checkSubTables)
  { TableUtil::deleteTable (tableName, checkSubTables); }
rownr_t Table::getLayout (TableDesc& desc, const String& tableName)
  { return TableUtil::getLayout (desc, tableName); }
TableInfo Table::tableInfo (const String& tableName)
  { return TableUtil::tableInfo (tableName); }

} //# NAMESPACE CASACORE - END
