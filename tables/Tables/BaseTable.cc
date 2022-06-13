//# BaseTable.cc: Abstract base class for tables
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

#include <thread>
#include <utility>

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/RefTable.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/BaseColumn.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/BaseTabIter.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/BasicSL/STLMath.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// The constructor of the derived class should call unmarkForDelete
// when the construction ended succesfully.
BaseTable::BaseTable (const String& name, int option, rownr_t nrrow)
{
    BaseTableCommon(name, option, nrrow);
}

#ifdef HAVE_MPI
BaseTable::BaseTable (MPI_Comm mpiComm, const String& name, int option, rownr_t nrrow)
    :itsMpiComm  (mpiComm)
{
    BaseTableCommon(name, option, nrrow);
}
#endif

void BaseTable::BaseTableCommon (const String& name, int option, rownr_t nrrow)
{
    nrrow_p = nrrow;
    nrrowToAdd_p = 0;
    name_p = name;
    option_p = option;
    noWrite_p = False;
    delete_p = False;
    madeDir_p = True;
    itsTraceId = -1;

    if (name_p.empty()) {
        name_p = File::newUniqueName ("", "tab").originalName();
    }
    // Make name absolute in case a chdir is done in e.g. Python.
    name_p = makeAbsoluteName (name_p);
    if (option_p == Table::Scratch) {
        option_p = Table::New;
    }
    // Mark initially a new table for delete.
    // When the construction ends successfully, it can be unmarked.
    if (option_p == Table::New  ||  option_p == Table::NewNoReplace) {
        markForDelete (False, "");
        madeDir_p = False;
    }
}

BaseTable::~BaseTable()
{
    //# Delete the table files (if there) if marked for delete.
    if (isMarkedForDelete()) {
	if (madeDir_p) {
	    // The table may be a subtable already deleted when
	    // the parent was deleted. So test if it still exists.
	    File file(name_p);
	    if (file.exists()) {
		Directory directory(file);
		directory.removeRecursive();
	    }
	    //# Do callback indicating that table has been deleted.
	    scratchCallback (False, name_p);
	}
    }
}

Bool BaseTable::isNull() const
{
  return False;
}

std::shared_ptr<BaseTable> BaseTable::getSharedPtr() const
{
  AlwaysAssert (!thisPtr_p.expired(), AipsError);
  return thisPtr_p.lock();
}


void BaseTable::scratchCallback (Bool isScratch, const String& oldName) const
{
    if (Table::scratchCallback_p != 0) {
	if (isScratch) {
	    if (oldName == name_p) {
		Table::scratchCallback_p (name_p, isScratch, "");
	    }else{
		Table::scratchCallback_p (name_p, isScratch, oldName);
	    }
	}else{
	    if (oldName.empty()) {
		Table::scratchCallback_p (name_p, isScratch, "");
	    }else{
		Table::scratchCallback_p (oldName, isScratch, "");
	    }
	}
    }
}

#ifdef HAVE_MPI
template <typename Func, typename ... Args>
void safe_mpi_invoke(Func &&mpi_func, const char *errmsg, Args && ... args)
{
    if (mpi_func(std::forward<Args>(args)...)) {
        throw std::runtime_error(errmsg);
    }
}

static bool is_mpi_initialized()
{
    int mpi_initialized;
    safe_mpi_invoke(MPI_Initialized, "Error when checking for initialized MPI", &mpi_initialized);
    return mpi_initialized != 0;
}

static bool is_mpi_finalized()
{
    int mpi_finalized;
    safe_mpi_invoke(MPI_Finalized, "Error when checking for finalized MPI", &mpi_finalized);
    return mpi_finalized != 0;
}

static std::once_flag ensure_mpi_flag;

static void ensure_mpi()
{
    std::call_once(ensure_mpi_flag, [&]{
        if (is_mpi_initialized() || is_mpi_finalized()) {
            return;
        }
#ifdef USE_THREADS
       int provided;
       safe_mpi_invoke(MPI_Init_thread, "Error when initializing MPI", nullptr, nullptr, MPI_THREAD_MULTIPLE, &provided);
       // Checking if provided == MPI_THREAD_MULTIPLE is not necessarily
       // the best option. Down here we have no idea how users are intending
       // to use these objects, and therefore throwing an exception is not
       // correct. For instance, with OpenMPI @ Ubuntu 16.04.05 lots
       // of unit tests fail.
#else
       safe_mpi_invoke(MPI_Init, "Error when initializing MPI", nullptr, nullptr);
#endif // USE_THREADS
    });
}

static bool is_rank_0(MPI_Comm comm)
{
    ensure_mpi();
    if (is_mpi_finalized()) {
        return false;
    }
    int rank;
    safe_mpi_invoke(MPI_Comm_rank, "Error when getting MPI rank", comm, &rank);
    return rank == 0;
}
#endif // HAVE_MPI

Bool BaseTable::makeTableDir()
{
#ifdef HAVE_MPI
    if (!is_rank_0(itsMpiComm)) {
        return false;
    }
#endif
    //# Exit if the table directory has already been created.
    if (madeDir_p) {
	return False;
    }
    //# Check option.
    if (!openedForWrite()) {
#ifndef HAVE_MPI
	throw (TableInvOpt ("BaseTable::makeTableDir",
			    "must be Table::New, NewNoReplace or Update"));
#endif
    }
    //# Check if the table directory name already exists
    //# and is a directory indeed.
    File file(name_p);
    if (file.exists()) {
	if (!file.isDirectory()) {
	    throw (TableDuplFile(name_p,
				 " (and is not a true table directory)"));
	}
	Directory tdir(file);
	if (! tdir.isEmpty()) {
	    //# Check if file table.dat exist in it.
	    File mfile (Table::fileName(name_p));
	    if (! mfile.exists()) {
	        throw (TableDuplFile(name_p,
				     " (and is not a true table directory)"));
	    }
	    if (option_p == Table::NewNoReplace) {
	        throw (TableDuplFile(name_p));   // table file already exists
	    }
	    //# Remove the directory and possible files in it.
	    //# In this way overwriting an existing table does not leave
	    //# old files.
	    //# Keep the directory, so existing properties (like placement on
	    //# Lustre file system is kept.
	    Directory dir(name_p);
	    dir.removeRecursive (True);
	}
    } else {
	//# Create the table directory.
	Directory dir(name_p);
	dir.create();
    }
    //# Create table.dat.
    //# First do a scratch callback that a table is getting created.
    //# If the file creation fails, the user sees it as a scratch
    //# table, so it can be deleted.
    scratchCallback (True, "");
    RegularFile dfile (Table::fileName(name_p));
    dfile.create();
    madeDir_p = True;
    return True;
}

Bool BaseTable::openedForWrite() const
{
#ifdef HAVE_MPI
    if (!is_rank_0(itsMpiComm)) {
        return false;
    }
#endif
    AlwaysAssert (!isNull(), AipsError);
    return (option_p==Table::Old || option_p==Table::Delete  ?  False : True);
}


int BaseTable::tableType() const
{
  return Table::Plain;
}

void BaseTable::getPartNames (Block<String>& names, Bool) const
{
  uInt inx = names.size();
  names.resize (inx + 1);
  names[inx] = name_p;
}

TableInfo BaseTable::tableInfo (const String& tableName)
{
    return TableInfo (tableName + "/table.info");
}
void BaseTable::getTableInfo()
{
    AlwaysAssert (!isNull(), AipsError);
    info_p = TableInfo (name_p + "/table.info");
}
void BaseTable::flushTableInfo()
{
    AlwaysAssert (!isNull(), AipsError);
    // Create table directory if needed.
    Bool made = makeTableDir();
    info_p.flush (name_p + "/table.info");
    if (made && !isMarkedForDelete()) {
	scratchCallback (False, name_p);
    }
}


void BaseTable::writeStart (AipsIO& ios, Bool bigEndian)
{
    // Check option.
    if (!openedForWrite()) {
        throw (TableInvOpt ("BaseTable::writeStart",
                            "must be Table::New, NewNoReplace or Update"));
    }
    // Create table directory when needed.
    Bool made = makeTableDir();
    // Create the file. It is a temporary file that will be later renamed
    // to the final name. This is so because, in case other process
    // tries to create a Table object with this table, there is a small
    // window in which the table.dat is truncated to lenght 0 (ByteIO::New) by
    // this process while the constructor of Table in the other process
    // will fail because table.dat is empty. Note that the constructor reads
    // reads table.dat before doing any locking
    ios.open (Table::fileName(name_p)+"_tmp", ByteIO::New);
    // Start the object as Table, so class Table can read it back.
    // Version 2 (of PlainTable) does not have its own TableRecord anymore.
    // Use old version if nr of rows fit in an Int, otherwise use new version.
    if (nrrow_p > rownr_t(std::numeric_limits<Int>::max())) {
        ios.putstart ("Table", 3);
        ios << nrrow_p;
    } else {
        ios.putstart ("Table", 2);
        ios << uInt(nrrow_p);
    }
    // Write endianity as a uInt, because older tables contain a uInt 0 here.
    uInt endian = 0;
    if (!bigEndian) {
      endian = 1;
    }
    ios << endian;              // 0=bigendian; 1=littleendian
    if (made && !isMarkedForDelete()) {
        scratchCallback (False, name_p);
    }
}

//# End writing a table file.
void BaseTable::writeEnd (AipsIO& ios)
{
    ios.putend ();
    // Ensure that the buffers have been written before renaming
    ios.close();
    // Now rename the file to the final name (table.dat). Not that the 
    // rename is function (used by RegularFile::move) is atomic 
    // (https://pubs.opengroup.org/onlinepubs/9699919799/functions/rename.html)
    RegularFile(Table::fileName(name_p)+"_tmp").move (Table::fileName(name_p), true);
}


void BaseTable::setTableChanged()
{}


void BaseTable::markForDelete (Bool callback, const String& oldName)
{
    AlwaysAssert (!isNull(), AipsError);
    Bool prev = delete_p;
    delete_p = True;
    //# Do callback if changed from non-scratch to scratch or if name changed.
    if (callback) {
	if (!prev) {
	    scratchCallback (True, "");
	} else if (!oldName.empty()  &&  oldName != name_p) {
	    scratchCallback (True, oldName);
	}
    }
}
void BaseTable::unmarkForDelete (Bool callback, const String& oldName)
{
    AlwaysAssert (!isNull(), AipsError);
    Bool prev = delete_p;
    delete_p = False;
    //# Do callback if changed from scratch to non-scratch.
    if (callback && prev) {
	scratchCallback (False, oldName);
    }
}


//# Prepare for copying or renaming a table.
void BaseTable::prepareCopyRename (const String& newName,
				   int tableOption) const
{
    // Options Delete and Old are wrong.
    if (tableOption == Table::Old  ||  tableOption == Table::Delete) {
	throw (TableInvOpt ("BaseTable::rename",
		      "must be Table::New, NewNoReplace, Scratch or Update"));
    }
    // Do not do anything if the new name is the same as the old name.
    if (newName == name_p) {
	return;
    }
    // Copy and rename is not allowed if the target table is open.
    Path path(newName);
    PlainTable* ptr = PlainTable::tableCache()(path.absoluteName());
    if (ptr) {
        throw (TableInvOper ("Cannot copy/rename; target table " + newName +
			     " is still open (is in the table cache)"));
    }
    // Test if the table already exists.
    // Throw an exception if a file (but not a table) exists.
    File fileNew(newName);
    if (fileNew.exists()) {
	if (!fileNew.isDirectory()) {
	    throw (TableDuplFile(newName,
				 " (and is not a true table directory)"));
	}
	// The table should not exist for NewNoReplace.
	if (tableOption == Table::NewNoReplace) {
	    throw (TableDuplFile(newName));
	}
	Directory directory(fileNew);
	directory.removeRecursive();
    }else{
	// The table must exist for Update.
	if (tableOption == Table::Update) {
            throw (TableNoFile(newName));
	}
    }
}

//# Rename a table.
void BaseTable::rename (const String& newName, int tableOption)
{
    AlwaysAssert (!isNull(), AipsError);
    // Make the name absolute.
    String absNewName = makeAbsoluteName (newName);
    // The table can be renamed if:
    // - it is not created yet
    // - it exists and its file is writable
    if (madeDir_p) {
	File file(name_p);
	if (!file.isWritable()) {
	    throw (TableInvOper ("Table file " + name_p +
				 " is readonly and cannot be renamed"));
	}
    }
    String oldName = name_p;
    // Do not rename physically when the new name is the same as the old name.
    if (absNewName != oldName) {
	prepareCopyRename (absNewName, tableOption);
	//# Do the actual renaming when the table exists.
	//# It is possible that the files do not exist yet if rename
	//# is used very early.
	if (madeDir_p) {
	    Directory fileOld(oldName);
	    fileOld.move (absNewName);
	}
	//# Rename the names of the subtables in the keywords.
	renameSubTables (absNewName, oldName);
	//# Okay, the table file has been renamed.
	//# Now rename in the cache (if there) and internally.
        PlainTable::tableCache().rename (absNewName, oldName);
	name_p = absNewName;
    }
    //# (Un)mark for delete when necessary.
    if (tableOption == Table::Scratch) {
	markForDelete (True, oldName);
    }else{
	unmarkForDelete (True, oldName);
    }
}

void BaseTable::renameSubTables (const String&, const String&)
{}

void BaseTable::deepCopy (const String& newName,
                          const Record& dataManagerInfo,
                          const StorageOption& stopt,
			  int tableOption,
			  Bool valueCopy,
			  int endianFormat,
			  Bool noRows) const
{
    if (valueCopy  ||  dataManagerInfo.nfields() > 0  ||  noRows) {
      trueDeepCopy (newName, dataManagerInfo, stopt, tableOption,
		      endianFormat, noRows);
    } else {
        copy (newName, tableOption);
    }
}

void BaseTable::trueDeepCopy (const String& newName,
			      const Record& dataManagerInfo,
                              const StorageOption& stopt,
			      int tableOption,
			      int endianFormat,
			      Bool noRows) const
{
    AlwaysAssert (!isNull(), AipsError);
    // Make the name absolute.
    String absNewName = makeAbsoluteName (newName);
    // Throw exception if new name is same as old one.
    if (absNewName == name_p) {
        throw TableError
	       ("Table::deepCopy: new name equal to old name " + name_p);
    }
    //# Flush the data and subtables.
    //# (cast is necessary to bypass non-constness).
    BaseTable* ncThis = const_cast<BaseTable*>(this);
    ncThis->flush (True, True);
    //# Prepare the copy (do some extra checks).
    prepareCopyRename (absNewName, tableOption);
    // Create the new table and copy everything.
    Table oldtab(ncThis);
    Table newtab = TableCopy::makeEmptyTable
                        (absNewName, dataManagerInfo, oldtab, Table::New,
			 Table::EndianFormat(endianFormat), True, noRows,
                         stopt);
    if (!noRows) {
      TableCopy::copyRows (newtab, oldtab);
    }
    TableCopy::copyInfo (newtab, oldtab);
    TableCopy::copySubTables (newtab, oldtab, noRows);
}

void BaseTable::copy (const String& newName, int tableOption) const
{
    AlwaysAssert (!isNull(), AipsError);
    // Make the name absolute.
    String absNewName = makeAbsoluteName (newName);
    // Do not copy when the new name is the same as the old name.
    if (absNewName != name_p) {
	//# Throw an exception when directories do not exist yet.
	if (!madeDir_p) {
	    throw (TableError
		   ("BaseTable::copy: no input table files exist for " +
                    name_p));
	}
	//# Flush the data and subtables.
	//# (cast is necessary to bypass non-constness).
	((BaseTable*)this)->flush (True, True);
	//# Copy the files (thus recursively the entire directory).
	//# Set user write permission after the copy.
	prepareCopyRename (absNewName, tableOption);
	Directory fileOld(name_p);
	fileOld.copy (absNewName);
        //# Renaming of subtables is not needed, because their names in
        //# the table directory (the ones copiued) are all relative.
    }
}


//# A column is writable if the table and column are writable.
Bool BaseTable::isColumnWritable (const String& columnName) const
{
    AlwaysAssert (!isNull(), AipsError);
    if (! isWritable()) {
	return False;                 // table is not writable
    }
    return getColumn(columnName)->isWritable();
}
Bool BaseTable::isColumnWritable (uInt columnIndex) const
{
    AlwaysAssert (!isNull(), AipsError);
    if (! isWritable()) {
	return False;                 // table is not writable
    }
    return getColumn(columnIndex)->isWritable();
}

Bool BaseTable::isColumnStored (const String& columnName) const
{
    AlwaysAssert (!isNull(), AipsError);
    return getColumn(columnName)->isStored();
}
Bool BaseTable::isColumnStored (uInt columnIndex) const
{
    AlwaysAssert (!isNull(), AipsError);
    return getColumn(columnIndex)->isStored();
}

//# By default adding, etc. of rows and columns is not possible.
Bool BaseTable::canAddRow() const
    { return False; }
Bool BaseTable::canRemoveRow() const
    { return False; }

void BaseTable::addRow (rownr_t, Bool)
    { throw (TableInvOper ("Table: cannot add a row to table " + name_p)); }

void BaseTable::removeRow (rownr_t)
    { throw (TableInvOper ("Table: cannot remove a row from table " + name_p)); }

void BaseTable::removeRow (const Vector<rownr_t>& rownrs)
{
    //# Copy the rownrs and sort them.
    //# Loop through them from end to start. In that way we are sure
    //# that the deletion of a row does not affect later rows.
    Vector<rownr_t> rownrsCopy;
    rownrsCopy = rownrs;
    genSort (rownrsCopy);
    for (Int64 i=rownrsCopy.nelements()-1; i>=0; i--) {
	removeRow (rownrsCopy(i));
    }
}

void BaseTable::addColumn (const ColumnDesc&, Bool)
    { throw (TableInvOper ("Table: cannot add a column to table " + name_p)); }
void BaseTable::addColumn (const ColumnDesc&, const String&, Bool, Bool)
    { throw (TableInvOper ("Table: cannot add a column to table " + name_p)); }
void BaseTable::addColumn (const ColumnDesc&, const DataManager&, Bool)
    { throw (TableInvOper ("Table: cannot add a column to table " + name_p)); }
void BaseTable::addColumn (const TableDesc&, const DataManager&, Bool)
    { throw (TableInvOper ("Table: cannot add a column to table " + name_p)); }

void BaseTable::addColumns (const TableDesc& desc, const Record& dmInfo,
                            Bool addToParent)
{
  // Create the correct data manager using the record.
  // The record can be the dminfo description itself or contain a
  // single subrecord with the dminfo.
  Record rec(dmInfo);
  if (dmInfo.nfields() == 1  &&  dmInfo.dataType(0) == TpRecord) {
    rec = dmInfo.subRecord(0);
  }
  if (rec.isDefined("TYPE")  &&  rec.isDefined("NAME")) {
    String dmType = rec.asString ("TYPE");
    String dmGroup = rec.asString ("NAME");
    Record sp;
    if (rec.isDefined("SPEC")) {
      sp = rec.subRecord ("SPEC");
    }
    DataManager* dataMan = DataManager::getCtor(dmType) (dmGroup, sp);
    addColumn (desc, *dataMan, addToParent);
    delete dataMan;
  } else {
    throw TableError ("Invalid dmInfo record given in Table::addColumn"
                      " for table " + name_p);
  }
}

//# Get a vector of row numbers.
Vector<rownr_t> BaseTable::rowNumbers() const
{
    AlwaysAssert (!isNull(), AipsError);
    Vector<rownr_t> vec(nrow());
    indgen (vec, (rownr_t)0);                  // store 0,1,... in it
    return vec;
}

//# By default root table is table itself.
BaseTable* BaseTable::root()
    { return this; }

//# By default table is in row order.
Bool BaseTable::rowOrder() const
    { return True; }

//# By the default the table cannot return the storage of rownrs.
Vector<rownr_t>& BaseTable::rowStorage()
{
    throw (TableInvOper ("rowStorage() not possible; table " + name_p +
                         " is no RefTable"));
}


//# Sort a table.
std::shared_ptr<BaseTable> BaseTable::sort
(const Block<String>& names,
 const Block<CountedPtr<BaseCompare> >& cmpObj,
 const Block<Int>& order, int option,
 std::shared_ptr<Vector<rownr_t>> sortIterBoundaries,
 std::shared_ptr<Vector<size_t>> sortIterKeyIdxChange)

{
    AlwaysAssert (!isNull(), AipsError);
    //# Check if the vectors have equal length.
    uInt nrkey = names.nelements();
    if (nrkey != order.nelements()) {
        throw (TableInvSort
               ("Length of column sort names and order vectors mismatch"
                " for table " + name_p));
    }
    //# Get the Column pointers.
    //# Check if a sort key is indeed a column of scalars.
    PtrBlock<BaseColumn*> sortCol(nrkey);
    for (uInt i=0; i<nrkey; i++) {
        sortCol[i] = getColumn (names[i]);         // get BaseColumn object
        if (!sortCol[i]->columnDesc().isScalar()) {
            throw (TableInvSort ("Sort column " + names[i] + " in table " +
                                 name_p + " is not a scalar"));
        }
    }
    // Return the result as a table.
    return doSort (sortCol, cmpObj, order, option,
                   sortIterBoundaries, sortIterKeyIdxChange);
}

//# Do the actual sort.
std::shared_ptr<BaseTable> BaseTable::doSort
(PtrBlock<BaseColumn*>& sortCol,
 const Block<CountedPtr<BaseCompare> >& cmpObj,
 const Block<Int>& order, int option,
 std::shared_ptr<Vector<rownr_t>> sortIterBoundaries,
 std::shared_ptr<Vector<size_t>> sortIterKeyIdxChange)
{
    uInt nrkey = sortCol.nelements();
    //# Create a sort object.
    //# Pass all keys (and their data) to it.
    Sort sortobj;
    Block<CountedPtr<ArrayBase> > data(nrkey);        // to remember data blocks
    Block<CountedPtr<BaseCompare> > cmp(cmpObj);
    for (uInt i=0; i<nrkey; i++) {
        sortCol[i]->makeSortKey (sortobj, cmp[i], order[i], data[i]);
    }
    //# Create a reference table.
    //# This table will NOT be in row order.
    rownr_t nrrow = nrow();
    std::shared_ptr<BaseTable> resultBaseTab = makeRefTable (False, nrrow);
    RefTable* resultTable = resultBaseTab->asRefTable();
    //# Now sort the table storing the row-numbers in the RefTable.
    //# Adjust rownrs in case source table is already a RefTable.
    //# Then delete possible allocated data blocks.
    Vector<rownr_t>& rows = resultTable->rowStorage();
    //# Note that nrrow can change in case Sort::NoDuplicates was given.
    nrrow = sortobj.sort (rows, nrrow, option);
    if(sortIterBoundaries && sortIterKeyIdxChange) {
        sortobj.unique(*sortIterBoundaries, *sortIterKeyIdxChange, rows);
    }
    adjustRownrs (nrrow, rows, False);
    resultTable->setNrrow (nrrow);
    return resultBaseTab;
}

std::shared_ptr<BaseTable> BaseTable::makeRefTable (Bool rowOrder,
                                                    rownr_t initialNrrow)
{
  std::shared_ptr<BaseTable> rtp (new RefTable(this, rowOrder, initialNrrow));
  rtp->setWeakPtr (rtp);
  return rtp;
}

RefTable* BaseTable::asRefTable()
{
  RefTable* rtp = dynamic_cast<RefTable*>(this);
  AlwaysAssert (rtp, AipsError);
  return rtp;
}

//# No rownrs have to be adjusted and they are by default in ascending order.
Bool BaseTable::adjustRownrs (rownr_t, Vector<rownr_t>&, Bool) const
    { return True; }

std::shared_ptr<BaseTable> BaseTable::select (rownr_t maxRow, rownr_t offset)
{
    if (offset > nrow()) {
        offset = nrow();
    }
    if (maxRow == 0  ||  maxRow > nrow()  ) {
        maxRow = nrow() - offset;
    }
    if (offset == 0  &&  maxRow == nrow()) {
      return getSharedPtr();    // pointer to itself
    }
    Vector<rownr_t> rownrs(maxRow);
    indgen(rownrs, rownr_t(offset));
    return select(rownrs);
}

// Do the row selection.
std::shared_ptr<BaseTable> BaseTable::select (const TableExprNode& node,
                                              rownr_t maxRow, rownr_t offset)
{
    // Check we don't deal with a null table.
    AlwaysAssert (!isNull(), AipsError);
    // If it is a null expression, return maxrows.
    if (node.isNull()) {
      return select (maxRow, offset);
    }
    //# First check if the node is a Bool.
    if (node.dataType() != TpBool  ||  !node.isScalar()) {
	throw (TableInvExpr ("select expression result on table " + name_p +
                             " is not Bool scalar"));
    }
    // Accept a const bool expression.
    if (node.getNodeRep()->isConstant()) {
        if (node.getBool(0)) {
            // Select maxRow rows.
            return select (maxRow, offset);
        }
        // Select no rows.
        return select(Vector<rownr_t>());
    }
    // Now check if this table has been used for all columns.
    // Accept that the expression has no table, which can be the case for
    // UDFs in derivedmscal (since they have no function arguments).
    if (!node.table().isNull()  &&  node.table().nrow() != this->nrow()) {
      throw (TableInvExpr ("select expression for table " +
                           node.table().tableName() +
                           " is used on a differently sized table " + name_p));
    }
    //# Create a reference table, which will be in row order.
    //# Loop through all rows and add to reference table if true.
    //# Add the rownr of the root table (one may search a reference table).
    //# Adjust the row numbers to reflect row numbers in the root table.
    std::shared_ptr<BaseTable> resultBaseTab = makeRefTable (True, 0);
    RefTable* resultTable = resultBaseTab->asRefTable();
    Bool val;
    rownr_t nrrow = nrow();
    TableExprId id;
    for (rownr_t i=0; i<nrrow; i++) {
      id.setRownr (i);
      node.get (id, val);
      if (val) {
        if (offset == 0) {
          resultTable->addRownr (i);                  // add row
          // Stop if max #rows reached (note that maxRow==0 means no limit).
          if (resultTable->nrow() == maxRow) {
            break;
          }
        } else {
          // Skip first offset matching rows.
          offset--;
        }
      }
    }
    adjustRownrs (resultTable->nrow(), resultTable->rowStorage(), False);
    return resultBaseTab;
}

std::shared_ptr<BaseTable> BaseTable::select (const Vector<rownr_t>& rownrs)
{
    AlwaysAssert (!isNull(), AipsError);
    RefTable* rtp = new RefTable(this, rownrs);
    return std::shared_ptr<BaseTable> (rtp);
}

std::shared_ptr<BaseTable> BaseTable::select (const Block<Bool>& mask)
{
    AlwaysAssert (!isNull(), AipsError);
    RefTable* rtp = new RefTable(this, Vector<Bool>(mask.begin(), mask.end()));
    return std::shared_ptr<BaseTable> (rtp);
}

std::shared_ptr<BaseTable> BaseTable::project (const Block<String>& names)
{
    AlwaysAssert (!isNull(), AipsError);
    RefTable* rtp = new RefTable(this, Vector<String>(names.begin(), names.end()));
    return std::shared_ptr<BaseTable> (rtp);
}


//# And (intersect) 2 tables and return a new table.
std::shared_ptr<BaseTable> BaseTable::tabAnd (BaseTable* that)
{
    AlwaysAssert (!isNull(), AipsError);
    //# Check if both table have the same root.
    logicCheck (that);
    //# Anding a table with the (possibly sorted) root table gives the table.
    if (this->nrow() == this->root()->nrow()) {
      return that->getSharedPtr();                    // this is root
    }
    if (that->nrow() == that->root()->nrow()) {
      return this->getSharedPtr();                    // that is root
    }
    //# There is no root table involved, so we have to deal with RefTables.
    //# Get both rownr arrays which are sorted if not in row order.
    Vector<rownr_t> r1 = this->logicRows();
    Vector<rownr_t> r2 = that->logicRows();
    // Create RefTable which will be in row order.
    std::shared_ptr<BaseTable> baseTabPtr = makeRefTable (True, 0);
    RefTable* rtp = baseTabPtr->asRefTable();
    // Store rownrs in new RefTable.
    rtp->refAnd (r1.size(), r1.data(), r2.size(), r2.data());
    return baseTabPtr;
}

//# Or (union) 2 tables and return a new table.
std::shared_ptr<BaseTable> BaseTable::tabOr (BaseTable* that)
{
    AlwaysAssert (!isNull(), AipsError);
    //# Check if both tables have the same root.
    logicCheck (that);
    //# Oring a table with the (possibly sorted) root table gives the root.
    if (this->nrow() == this->root()->nrow()
    ||  that->nrow() == that->root()->nrow()) {
        return root()->getSharedPtr();
    }
    //# There is no root table involved, so we have to deal with RefTables.
    //# Get both rownr arrays which are sorted if not in row order.
    Vector<rownr_t> r1 = this->logicRows();
    Vector<rownr_t> r2 = that->logicRows();
    // Create RefTable which will be in row order.
    std::shared_ptr<BaseTable> baseTabPtr = makeRefTable (True, 0);
    RefTable* rtp = baseTabPtr->asRefTable();
    // Store rownrs in new RefTable.
    rtp->refOr (r1.size(), r1.data(), r2.size(), r2.data());
    return baseTabPtr;
}

//# Subtract (difference) 2 tables and return a new table.
std::shared_ptr<BaseTable> BaseTable::tabSub (BaseTable* that)
{
    AlwaysAssert (!isNull(), AipsError);
    //# Check if both table have the same root.
    logicCheck (that);
    //# Subtracting the root table from a table results in an empty table.
    if (that->nrow() == that->root()->nrow()) {
	return makeRefTable (True, 0);
    }
    //# Subtracting a table from the root is negating the table.
    if (this->nrow() == this->root()->nrow()) {
	return that->tabNot();
    }
    //# There is no root table involved, so we have to deal with RefTables.
    //# Get both rownr arrays which are sorted if not in row order.
    Vector<rownr_t> r1 = this->logicRows();
    Vector<rownr_t> r2 = that->logicRows();
    // Create RefTable which will be in row order.
    std::shared_ptr<BaseTable> baseTabPtr = makeRefTable (True, 0);
    RefTable* rtp = baseTabPtr->asRefTable();
    // Store rownrs in new RefTable.
    rtp->refSub (r1.size(), r1.data(), r2.size(), r2.data());
    return baseTabPtr;
}

//# Xor 2 tables and return a new table.
std::shared_ptr<BaseTable> BaseTable::tabXor (BaseTable* that)
{
    AlwaysAssert (!isNull(), AipsError);
    //# Check if both table have the same root.
    logicCheck (that);
    //# Xoring a table with the (possibly sorted) root table is negating.
    if (this->nrow() == this->root()->nrow()) {
	return that->tabNot();
    }
    if (that->nrow() == that->root()->nrow()) {
	return tabNot();
    }
    //# There is no root table involved, so we have to deal with RefTables.
    //# Get both rownr arrays which are sorted if not in row order.
    Vector<rownr_t> r1 = this->logicRows();
    Vector<rownr_t> r2 = that->logicRows();
    // Create RefTable which will be in row order.
    std::shared_ptr<BaseTable> baseTabPtr = makeRefTable (True, 0);
    RefTable* rtp = baseTabPtr->asRefTable();
    // Store rownrs in new RefTable.
    rtp->refXor (r1.size(), r1.data(), r2.size(), r2.data());
    return baseTabPtr;
}

//# Negate a table (i.e. take all rows from the root not in table).
std::shared_ptr<BaseTable> BaseTable::tabNot()
{
    AlwaysAssert (!isNull(), AipsError);
    //# Negating the (possibly sorted) root results in an empty table,
    if (nrow() == root()->nrow()) {
	return makeRefTable (True, 0);
    }
    //# There is no root table involved, so we have to deal with RefTables.
    //# Get rownr array which is sorted if not in row order.
    Vector<rownr_t> r1 = this->logicRows();
    // Create RefTable which will be in row order.
    std::shared_ptr<BaseTable> baseTabPtr = makeRefTable (True, 0);
    RefTable* rtp = baseTabPtr->asRefTable();
    // Store rownrs in new RefTable.
    rtp->refNot (r1.size(), r1.data(), root()->nrow());
    return baseTabPtr;
}

//# Check if both tables have the same root.
void BaseTable::logicCheck (BaseTable* that)
{
    if (root() != that->root()) {
	throw TableInvLogic();
    }
}

//# Get the rownrs from the reference table.
//# Note that rowStorage() throws an exception if it is not a RefTable.
//# Sort them if not in row order.
Vector<rownr_t> BaseTable::logicRows()
{
    AlwaysAssert (!isNull(), AipsError);
    Vector<rownr_t> rows (rowStorage());
    if (rowOrder()) {
      return rows;
    }
    //# rows are not in order, so sort them.
    //# They have to be copied, because the original should not be changed.
    Vector<rownr_t> rowscp (rows.copy());
    GenSort<rownr_t>::sort (rowscp);
    return rowscp;
}


BaseTableIterator* BaseTable::makeIterator
(const Block<String>& names,
 const Block<CountedPtr<BaseCompare> >& cmpObj,
 const Block<Int>& order, int option,
 bool cacheIterationBoundaries)
{
    AlwaysAssert (!isNull(), AipsError);
    if (names.nelements() != order.nelements()
    ||  names.nelements() != cmpObj.nelements()) {
	throw (TableInvOper ("TableIterator: Unequal block lengths"));
    }
    BaseTableIterator* bti = new BaseTableIterator (getSharedPtr(), names,
                                                   cmpObj, order, option,
                                                   cacheIterationBoundaries);
    return bti;
}


const TableDesc& BaseTable::makeEmptyTableDesc() const
{
    if (tdescPtr_p.null()) {
        const_cast<BaseTable*>(this)->tdescPtr_p = new TableDesc();
    }
    return *tdescPtr_p;
}


Bool BaseTable::checkRemoveColumn (const Vector<String>& columnNames,
				   Bool throwException) const
{
    for (uInt i=0; i<columnNames.nelements(); i++) {
        // Check if the column exists.
        if (! tdescPtr_p->isColumn (columnNames(i))) {
	    if (throwException) {
	        throw TableInvOper ("Table::removeColumn - column " +
				    columnNames(i) + " does not exist"
                                    " in table " + name_p);
	    }
	    return False;
	}
        // Check if the column is specified only once.
	for (uInt j=i+1; j<columnNames.nelements(); j++) {
	    if (columnNames(i) == columnNames(j)) {
	        if (throwException) {
		  throw TableInvOper ("Table::removeColumn - column " +
				    columnNames(i) + " is multiply specified");
		}
	    return False;
	    }
	}
    }
    return True;
}

void BaseTable::checkRowNumberThrow (rownr_t rownr) const
{
    throw (TableError ("TableColumn: row number " + String::toString(rownr) +
		       " exceeds #rows " +
		       String::toString(nrrow_p+nrrowToAdd_p)
		       + " in table " + tableName()));
}

void BaseTable::showStructure (ostream& os, Bool showDataMans, Bool showColumns,
                               Bool showSubTables, Bool sortColumns,
                               Bool cOrder)
{
  TableDesc tdesc = actualTableDesc();
  Record dminfo = dataManagerInfo();
  os << endl << "Structure of table " << tableName()
     << endl << "------------------ ";
  os << info_p.type();
  if (! info_p.subType().empty()) {
    os << " (" << info_p.subType() << ')';
  }
  os << endl;
  os << nrow() << " rows, " << tdesc.ncolumn() << " columns in ";
  os << (asBigEndian() ? "big" : "little") << " endian format";
  os << " (using "
     << dminfo.nfields() << " data managers)" <<endl;
  const StorageOption& stopt = storageOption();
  if (stopt.option() == StorageOption::MultiFile) {
    os << "  Stored as MultiFile with blocksize " << stopt.blockSize() << endl;
  } else if (stopt.option() == StorageOption::MultiHDF5) {
    os << "  Stored as MultiHDF5 with blocksize " << stopt.blockSize() << endl;
  }
  showStructureExtra (os);
  uInt maxl = 0;
  for (uInt i=0; i<tdesc.ncolumn(); ++i) {
    if (tdesc[i].name().size() > maxl) {
      maxl = tdesc[i].name().size();
    }
  }
  if (!showDataMans) {
    if (showColumns) {
      os << endl;
      showColumnInfo (os, tdesc, maxl, tdesc.columnNames(), sortColumns,
                      cOrder);
    }
  } else {
    for (uInt i=0; i<dminfo.nfields(); ++i) {
      os << endl << " ";
      const Record& dm = dminfo.subRecord(i);
      Record spec;
      if (dm.isDefined("SPEC")) {
        spec = dm.subRecord("SPEC");
      }
      os << dm.asString("TYPE");
      os << " file=table.f" << dm.asInt("SEQNR") << " ";
      os << " name=" << dm.asString("NAME");
      if (spec.isDefined("BUCKETSIZE")) {
        os << "  bucketsize=" << spec.asInt("BUCKETSIZE");
      }
      os << endl;
      if (spec.isDefined("HYPERCUBES")) {
        os << "    hypercubes:" << endl;
        const Record& hcubes = spec.subRecord("HYPERCUBES");
        for (uInt k=0; k<hcubes.nfields(); ++k) {
          const Record& hcube = hcubes.subRecord(k);
          os << "      bucketsize=" << hcube.asInt("BucketSize");
          os << " tileshape=";
          showContainer (os, hcube.asArrayInt("TileShape"));
          os << " cellshape=";
          showContainer (os, hcube.asArrayInt("CellShape"));
          os << " cubeshape=";
          showContainer (os, hcube.asArrayInt("CubeShape"));
          os << endl;
        }
      }
      Bool extra = False;
      for (uInt j=0; j<spec.nfields(); j++) {
        const String& name = spec.name(j);
        if (name != "SEQNR" && name != "BUCKETSIZE" && name != "HYPERCUBES") {
          if (!extra) {
            os << "   ";
            extra = True;
          }
          os << ' '<< name << '=' << spec.asValueHolder(j);
        }
      }
      if (extra) {
        os << endl;
      }
      if (showColumns) {
        showColumnInfo (os, tdesc, maxl, dm.asArrayString ("COLUMNS"),
                        sortColumns, cOrder);
      }
    }
  }
  TableRecord keywords = keywordSet();
  Bool hasSub = False;
  for (uInt i=0; i<keywords.nfields(); ++i) {
    if (keywords.dataType(i) == TpTable) {
      if (!hasSub) {
        os << endl << " SubTables:" << endl;
        hasSub = True;
      }
      os << "    " << keywords.asTable(i).tableName() << endl;
    }
  }
  if (hasSub && showSubTables) {
    for (uInt i=0; i<keywords.nfields(); ++i) {
      if (keywords.dataType(i) == TpTable) {
        Table tab = keywords.asTable(i);
        // Do not show if the subtable has the same root as this table.
        // This is needed to avoid endless recursion in case of SORTED_TABLE
        // in a MeasurementSet.
        if (tab.isSameRoot (Table(this))) {
          os << endl << "Subtable " << keywords.name(i)
             << " references the parent table!!" << endl;
        } else {
          tab.showStructure (os, showDataMans, showColumns,
                             showSubTables, sortColumns, cOrder);
        }
      }
    }
  }
}

void BaseTable::showStructureExtra (ostream&) const
{}

void BaseTable::showColumnInfo (ostream& os, const TableDesc& tdesc,
                                uInt maxl, const Array<String>& columnNames,
                                Bool sort, Bool cOrder) const
{
  Vector<String> columns(columnNames);
  if (sort) {
    GenSort<String>::sort (columns);
  }
  for (uInt j=0; j<columns.size(); ++j) {
    const ColumnDesc& cdesc = tdesc[columns[j]];
    TableRecord keywords = cdesc.keywordSet();
    os << "  " << cdesc.name();
    for (uInt k=0; k<=maxl - cdesc.name().size(); ++k) {
      os << ' ';
    }
    os << ValType::getTypeStr(cdesc.dataType());
    if (cdesc.isScalar()) {
      os << " scalar";
    } else if (cdesc.isArray()) {
      if (cdesc.options() & ColumnDesc::FixedShape) {
        os << " shape=";
        if (cOrder) {
          showContainer (os, reversedCasaContainer(cdesc.shape()));
        } else {
          showContainer (os, cdesc.shape());
        }
      } else if (cdesc.ndim() > 0) {
        os << " ndim=" << cdesc.ndim();
      } else {
        os << " array";
      }
    }
    if (keywords.isDefined("UNIT") && !keywords.asString("UNIT").empty()) {
      os << " unit=" << keywords.asString("UNIT");
    } else if (keywords.isDefined("QuantumUnits")) {
      os << " unit=";
      showContainer (os, keywords.asArrayString("QuantumUnits"));
    }
    if (keywords.isDefined("MEASINFO")) {
      const TableRecord& meas = keywords.subRecord("MEASINFO");
      os << " measure=";
      if (meas.isDefined("type")) {
	os << meas.asString("type");
      }	else {
	os << "unknown";
      }
      if (meas.isDefined("Ref")) {
        os << ',' << meas.asString("Ref");
      }
      if (meas.isDefined("VarRefCol")) {
        os << " refcol=" << meas.asString("VarRefCol");
      }
    }
    if (cdesc.options() & ColumnDesc::Direct) {
      os << " directly stored";
    }
    os << endl;
  }
}

String BaseTable::makeAbsoluteName (const String& name) const
{
    // Make sure the name contains a character not equal to . or /.
    Bool ok = False;
    for (uInt i=0; i<name.size(); ++i) {
      if (name[i] != '.'  &&  name[i] != '/') {
        ok = True;
        break;
      }
    }
    if (!ok) {
      throw TableError ("BaseTable::makeAbsoluteName - "
                        "table name '" + name + "' is invalid");
    }
    return Path(name).absoluteName();
}

} //# NAMESPACE CASACORE - END

