//# ColumnsIndexArray.cc: Index to a table
//# Copyright (C) 2001,2002
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
#include <casacore/tables/Tables/ColumnsIndexArray.h>
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableLocker.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ColumnsIndexArray::ColumnsIndexArray (const Table& table,
				      const String& columnName)
: itsLowerKeyPtr (0),
  itsUpperKeyPtr (0)
{
  itsTable = table;
  itsNrrow = itsTable.nrow();
  // Add column to the RecordDesc.
  RecordDesc description;
  addColumnToDesc (description, TableColumn (itsTable, columnName));
  makeObjects (description);
  readData();
}

ColumnsIndexArray::ColumnsIndexArray (const ColumnsIndexArray& that)
: itsLowerKeyPtr (0),
  itsUpperKeyPtr (0)
{
  copy (that);
}

ColumnsIndexArray::~ColumnsIndexArray()
{
  deleteObjects();
}

ColumnsIndexArray& ColumnsIndexArray::operator= (const ColumnsIndexArray& that)
{
  copy (that);
  return *this;
}

void ColumnsIndexArray::copy (const ColumnsIndexArray& that)
{
  if (this != &that) {
    deleteObjects();
    itsTable = that.itsTable;
    itsNrrow = itsTable.nrow();
    makeObjects (that.itsLowerKeyPtr->description());
  }
}

const String& ColumnsIndexArray::columnName() const
{
  const RecordDesc& desc = itsLowerKeyPtr->description();
  return desc.name(0);
}

void ColumnsIndexArray::deleteObjects()
{
  switch (itsDataType) {
  case TpUChar:
    delete (RecordFieldPtr<uChar>*)(itsLowerField);
    delete (RecordFieldPtr<uChar>*)(itsUpperField);
    delete (Vector<uChar>*)(itsDataVector);
    break;
  case TpShort:
    delete (RecordFieldPtr<Short>*)(itsLowerField);
    delete (RecordFieldPtr<Short>*)(itsUpperField);
    delete (Vector<Short>*)(itsDataVector);
    break;
  case TpInt:
    delete (RecordFieldPtr<Int>*)(itsLowerField);
    delete (RecordFieldPtr<Int>*)(itsUpperField);
    delete (Vector<Int>*)(itsDataVector);
    break;
  case TpUInt:
    delete (RecordFieldPtr<uInt>*)(itsLowerField);
    delete (RecordFieldPtr<uInt>*)(itsUpperField);
    delete (Vector<uInt>*)(itsDataVector);
    break;
  case TpInt64:
    delete (RecordFieldPtr<Int64>*)(itsLowerField);
    delete (RecordFieldPtr<Int64>*)(itsUpperField);
    delete (Vector<Int64>*)(itsDataVector);
    break;
  case TpString:
    delete (RecordFieldPtr<String>*)(itsLowerField);
    delete (RecordFieldPtr<String>*)(itsUpperField);
    delete (Vector<String>*)(itsDataVector);
    break;
  default:
    throw (TableError ("ColumnsIndexArray: unsupported data type"));
  }
  itsLowerField = 0;
  itsUpperField = 0;
  itsDataVector = 0;
  itsData       = 0;
  delete itsLowerKeyPtr;
  delete itsUpperKeyPtr;
  itsLowerKeyPtr = 0;
  itsUpperKeyPtr = 0;
}

void ColumnsIndexArray::addColumnToDesc (RecordDesc& description,
					 const TableColumn& column)
{
  const ColumnDesc& columnDesc = column.columnDesc();
  DataType dataType = columnDesc.dataType();
  itsDataType = dataType;
  if (! columnDesc.isArray()) {
    throw (TableError ("ColumnsIndexArray: column " + columnDesc.name() +
		       " should be an array column"));
  }
  description.addField (columnDesc.name(), dataType);
}

	    
void ColumnsIndexArray::makeObjects (const RecordDesc& description)
{
  // Create the Record from the description.
  itsLowerKeyPtr = new Record (description);
  itsUpperKeyPtr = new Record (description);
  // Initialize the column and field block.
  itsDataVector = 0;
  itsData = 0;
  itsLowerField = 0;
  itsUpperField = 0;
  itsChanged = True;
  // Create the correct column object for each field.
  // Also create a RecordFieldPtr object for each Key.
  // This makes a fast data copy possible.
  itsDataType = description.type(0);
  switch (itsDataType) {
  case TpUChar:
    {
      itsLowerField = new RecordFieldPtr<uChar>(*itsLowerKeyPtr, 0);
      itsUpperField = new RecordFieldPtr<uChar>(*itsUpperKeyPtr, 0);
      itsDataVector = new Vector<uChar>;
      break;
    }
  case TpShort:
    {
      itsLowerField = new RecordFieldPtr<Short>(*itsLowerKeyPtr, 0);
      itsUpperField = new RecordFieldPtr<Short>(*itsUpperKeyPtr, 0);
      itsDataVector = new Vector<Short>;
      break;
    }
  case TpInt:
    {
      itsLowerField = new RecordFieldPtr<Int>(*itsLowerKeyPtr, 0);
      itsUpperField = new RecordFieldPtr<Int>(*itsUpperKeyPtr, 0);
      itsDataVector = new Vector<Int>;
      break;
    }
  case TpUInt:
    {
      itsLowerField = new RecordFieldPtr<uInt>(*itsLowerKeyPtr, 0);
      itsUpperField = new RecordFieldPtr<uInt>(*itsUpperKeyPtr, 0);
      itsDataVector = new Vector<uInt>;
      break;
    }
  case TpInt64:
    {
      itsLowerField = new RecordFieldPtr<Int64>(*itsLowerKeyPtr, 0);
      itsUpperField = new RecordFieldPtr<Int64>(*itsUpperKeyPtr, 0);
      itsDataVector = new Vector<Int64>;
      break;
    }
  case TpString:
    {
      itsLowerField = new RecordFieldPtr<String>(*itsLowerKeyPtr, 0);
      itsUpperField = new RecordFieldPtr<String>(*itsUpperKeyPtr, 0);
      itsDataVector = new Vector<String>;
      break;
    }
  default:
    throw (TableError ("ColumnsIndexArray: unsupported data type"));
  }
}

void ColumnsIndexArray::readData()
{
  // Acquire a lock if needed.
  TableLocker locker(itsTable, FileLocker::Read);
  rownr_t nrrow = itsTable.nrow();
  if (nrrow != itsNrrow) {
    itsChanged = True;
    itsNrrow = nrrow;
  }
  if (!itsChanged) {
    return;
  }
  Sort sort;
  Bool deleteIt;
  const RecordDesc& desc = itsLowerKeyPtr->description();
  const String& name = desc.name(0);
  switch (itsDataType) {
  case TpUChar:
    {
      Vector<uChar>* vecptr = (Vector<uChar>*)itsDataVector;
      getArray (*vecptr, name);
      itsData = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData, desc.type(0));
      break;
    }
  case TpShort:
    {
      Vector<Short>* vecptr = (Vector<Short>*)itsDataVector;
      getArray (*vecptr, name);
      itsData = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData, desc.type(0));
      break;
    }
  case TpInt:
    {
      Vector<Int>* vecptr = (Vector<Int>*)itsDataVector;
      getArray (*vecptr, name);
      itsData = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData, desc.type(0));
      break;
    }
  case TpUInt:
    {
      Vector<uInt>* vecptr = (Vector<uInt>*)itsDataVector;
      getArray (*vecptr, name);
      itsData = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData, desc.type(0));
      break;
    }
  case TpInt64:
    {
      Vector<Int64>* vecptr = (Vector<Int64>*)itsDataVector;
      getArray (*vecptr, name);
      itsData = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData, desc.type(0));
      break;
    }
  case TpString:
    {
      Vector<String>* vecptr = (Vector<String>*)itsDataVector;
      getArray (*vecptr, name);
      itsData = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData, desc.type(0));
      break;
    }
  default:
    throw (TableError ("ColumnsIndexArray: unsupported data type"));
  }
  // Sort the data if needed.
  // Otherwise fill the index vector with 0..n.
  sort.sort (itsDataIndex, itsRownrs.nelements());
  // Determine all unique keys (itsUniqueIndex will contain the index of
  // each first unique entry in itsDataIndex).
  sort.unique (itsUniqueIndex, itsDataIndex);
  itsDataInx = itsDataIndex.getStorage (deleteIt);
  itsUniqueInx = itsUniqueIndex.getStorage (deleteIt);
  itsChanged = False;
}

rownr_t ColumnsIndexArray::bsearch (Bool& found, void* fieldPtr) const
{
  found = False;
  Int64 lower = 0;
  Int64 upper = itsUniqueIndex.nelements() - 1;
  Int64 middle = 0;
  while (lower <= upper) {
    middle = (upper + lower) / 2;
    Int cmp = compare (fieldPtr, itsData, itsDataType,
		       itsDataInx[itsUniqueInx[middle]]);
    if (cmp < 0) {
      upper = middle - 1;            // go to left
    } else if (cmp > 0) {
      middle++;
      lower = middle;                // go to right
    } else {
      found = True;
      break;
    }
  }
  return middle;
}

Int ColumnsIndexArray::compare (void* fieldPtr,
				void* dataPtr,
				Int dataType,
				rownr_t index)
{
  switch (dataType) {
  case TpUChar:
    {
      const uChar left = *(*(RecordFieldPtr<uChar>*)(fieldPtr));
      const uChar right = ((const uChar*)(dataPtr))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
  case TpShort:
    {
      const Short left = *(*(RecordFieldPtr<Short>*)(fieldPtr));
      const Short right = ((const Short*)(dataPtr))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
  case TpInt:
    {
      const Int left = *(*(RecordFieldPtr<Int>*)(fieldPtr));
      const Int right = ((const Int*)(dataPtr))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
  case TpUInt:
    {
      const uInt left = *(*(RecordFieldPtr<uInt>*)(fieldPtr));
      const uInt right = ((const uInt*)(dataPtr))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
  case TpInt64:
    {
      const Int64 left = *(*(RecordFieldPtr<Int64>*)(fieldPtr));
      const Int64 right = ((const Int64*)(dataPtr))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
  case TpString:
    {
      const String& left = *(*(RecordFieldPtr<String>*)(fieldPtr));
      const String& right = ((const String*)(dataPtr))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
  default:
    throw (TableError ("ColumnsIndexArray: unsupported data type"));
  }
  return 0;
}
 
rownr_t ColumnsIndexArray::getRowNumber (Bool& found, const Record& key)
{
  ColumnsIndex::copyKeyField (itsLowerField, itsDataType, key);
  return getRowNumber (found);
}

rownr_t ColumnsIndexArray::getRowNumber (Bool& found)
{
  if (!isUnique()) {
    throw (TableError ("ColumnsIndexArray::getRowNumber only possible "
		       "when the index keys are unique"));
  }
  // Read the data (if needed).
  readData();
  rownr_t inx = bsearch (found, itsLowerField);
  if (found) {
    inx = itsRownrs[itsDataInx[inx]];
  }
  return inx;
}

RowNumbers ColumnsIndexArray::getRowNumbers (const Record& key,
                                             Bool unique)
{
  ColumnsIndex::copyKeyField (itsLowerField, itsDataType, key);
  return getRowNumbers (unique);
}

RowNumbers ColumnsIndexArray::getRowNumbers (Bool unique)
{
  // Read the data (if needed).
  readData();
  Bool found;
  rownr_t inx = bsearch (found, itsLowerField);
  RowNumbers rows;
  if (found) {
    fillRowNumbers (rows, inx, inx+1, unique);
  }
  return rows;
}

RowNumbers ColumnsIndexArray::getRowNumbers (const Record& lowerKey,
                                             const Record& upperKey,
                                             Bool lowerInclusive,
                                             Bool upperInclusive,
                                             Bool unique)
{
  ColumnsIndex::copyKeyField (itsLowerField, itsDataType, lowerKey);
  ColumnsIndex::copyKeyField (itsUpperField, itsDataType, upperKey);
  return getRowNumbers (lowerInclusive, upperInclusive, unique);
}

RowNumbers ColumnsIndexArray::getRowNumbers (Bool lowerInclusive,
                                             Bool upperInclusive,
                                             Bool unique)
{
  // Read the data (if needed).
  readData();
  Bool found;
  // Try to find the lower key. If not found, bsearch is giving the
  // index of the next higher key.
  // So increment the start index if found and is not to be included.
  rownr_t start = bsearch (found, itsLowerField);
  if (found  &&  !lowerInclusive) {
    start++;
  }
  // Try to find the upper key.
  // Increment the end index such that it is not inclusive
  // (thus increment if the found end index is to be included).
  rownr_t end = bsearch (found, itsUpperField);
  if (found  &&  upperInclusive) {
    end++;
  }
  RowNumbers rows;
  if (start < end) {
    fillRowNumbers (rows, start, end, unique);
  }
  return rows;
}

void ColumnsIndexArray::fillRowNumbers (Vector<rownr_t>& rows,
					rownr_t start, rownr_t end,
					Bool unique) const
{
  start = itsUniqueInx[start];
  if (end < itsUniqueIndex.nelements()) {
    end = itsUniqueInx[end];
  } else {
    end = itsDataIndex.nelements();
  }
  rownr_t nr = end-start;
  rows.resize (nr);
  Bool deleteIt;
  rownr_t* rowStorage = rows.getStorage (deleteIt);
  for (rownr_t i=0; i<nr; i++) {
    rowStorage[i] = itsRownrs[itsDataInx[start+i]];
  }
  rows.putStorage (rowStorage, deleteIt);
  if (unique) {
    rownr_t nrrow = GenSort<rownr_t>::sort (rows, Sort::Ascending,
                                            Sort::NoDuplicates);
    rows.resize (nrrow, True);
  }
}

void ColumnsIndexArray::setChanged()
{
  itsChanged = True;
}

void ColumnsIndexArray::setChanged (const String& columnName)
{
  const RecordDesc& desc = itsLowerKeyPtr->description();
  if (desc.name(0) == columnName) {
    itsChanged = True;
  }
}


void ColumnsIndexArray::getArray (Vector<uChar>& result, const String& name)
{
  ArrayColumn<uChar> arrCol (itsTable, name);
  rownr_t nrrow = arrCol.nrow();
  if (nrrow > 0) {
    Block<rownr_t> nrel(nrrow, rownr_t(0));
    Array<uChar> arr = arrCol(0);
    rownr_t npts = arr.nelements();
    nrel[0] = npts;
    result.resize (nrrow*npts);
    Bool deleteIt;
    uChar* data = result.getStorage(deleteIt);
    objmove (data, arr.getStorage(deleteIt), npts);
    data += npts;
    for (rownr_t i=1; i<nrrow; i++) {
      if (arrCol.isDefined(i)) {
	Array<uChar> arr = arrCol(i);
	rownr_t n = arr.nelements();
	nrel[i] = n;
	if (npts+n > result.nelements()) {
	  result.resize (npts+n, True);
	}
	data = result.getStorage(deleteIt) + npts;
	objmove (data, arr.getStorage(deleteIt), n);
	npts += n;
      }
    }
    result.resize (npts, True);
    fillRownrs (npts, nrel);
  }
}

void ColumnsIndexArray::getArray (Vector<Short>& result, const String& name)
{
  ArrayColumn<Short> arrCol (itsTable, name);
  rownr_t nrrow = arrCol.nrow();
  if (nrrow > 0) {
    Block<rownr_t> nrel(nrrow, rownr_t(0));
    Array<Short> arr = arrCol(0);
    rownr_t npts = arr.nelements();
    nrel[0] = npts;
    result.resize (nrrow*npts);
    Bool deleteIt;
    Short* data = result.getStorage(deleteIt);
    objmove (data, arr.getStorage(deleteIt), npts);
    data += npts;
    for (rownr_t i=1; i<nrrow; i++) {
      if (arrCol.isDefined(i)) {
	Array<Short> arr = arrCol(i);
	rownr_t n = arr.nelements();
	nrel[i] = n;
	if (npts+n > result.nelements()) {
	  result.resize (npts+n, True);
	}
	data = result.getStorage(deleteIt) + npts;
	objmove (data, arr.getStorage(deleteIt), n);
	npts += n;
      }
    }
    result.resize (npts, True);
    fillRownrs (npts, nrel);
  }
}

void ColumnsIndexArray::getArray (Vector<Int>& result, const String& name)
{
  ArrayColumn<Int> arrCol (itsTable, name);
  rownr_t nrrow = arrCol.nrow();
  if (nrrow > 0) {
    Block<rownr_t> nrel(nrrow, rownr_t(0));
    Array<Int> arr = arrCol(0);
    rownr_t npts = arr.nelements();
    nrel[0] = npts;
    result.resize (nrrow*npts);
    Bool deleteIt;
    Int* data = result.getStorage(deleteIt);
    objmove (data, arr.getStorage(deleteIt), npts);
    data += npts;
    for (rownr_t i=1; i<nrrow; i++) {
      if (arrCol.isDefined(i)) {
	Array<Int> arr = arrCol(i);
	rownr_t n = arr.nelements();
	nrel[i] = n;
	if (npts+n > result.nelements()) {
	  result.resize (npts+n, True);
	}
	data = result.getStorage(deleteIt) + npts;
	objmove (data, arr.getStorage(deleteIt), n);
	npts += n;
      }
    }
    result.resize (npts, True);
    fillRownrs (npts, nrel);
  }
}

void ColumnsIndexArray::getArray (Vector<uInt>& result, const String& name)
{
  ArrayColumn<uInt> arrCol (itsTable, name);
  rownr_t nrrow = arrCol.nrow();
  if (nrrow > 0) {
    Block<rownr_t> nrel(nrrow, rownr_t(0));
    Array<uInt> arr = arrCol(0);
    rownr_t npts = arr.nelements();
    nrel[0] = npts;
    result.resize (nrrow*npts);
    Bool deleteIt;
    uInt* data = result.getStorage(deleteIt);
    objmove (data, arr.getStorage(deleteIt), npts);
    data += npts;
    for (rownr_t i=1; i<nrrow; i++) {
      if (arrCol.isDefined(i)) {
	Array<uInt> arr = arrCol(i);
	rownr_t n = arr.nelements();
	nrel[i] = n;
	if (npts+n > result.nelements()) {
	  result.resize (npts+n, True);
	}
	data = result.getStorage(deleteIt) + npts;
	objmove (data, arr.getStorage(deleteIt), n);
	npts += n;
      }
    }
    result.resize (npts, True);
    fillRownrs (npts, nrel);
  }
}

void ColumnsIndexArray::getArray (Vector<Int64>& result, const String& name)
{
  ArrayColumn<Int64> arrCol (itsTable, name);
  rownr_t nrrow = arrCol.nrow();
  if (nrrow > 0) {
    Block<rownr_t> nrel(nrrow, rownr_t(0));
    Array<Int64> arr = arrCol(0);
    rownr_t npts = arr.nelements();
    nrel[0] = npts;
    result.resize (nrrow*npts);
    Bool deleteIt;
    Int64* data = result.getStorage(deleteIt);
    objmove (data, arr.getStorage(deleteIt), npts);
    data += npts;
    for (rownr_t i=1; i<nrrow; i++) {
      if (arrCol.isDefined(i)) {
	Array<Int64> arr = arrCol(i);
	rownr_t n = arr.nelements();
	nrel[i] = n;
	if (npts+n > result.nelements()) {
	  result.resize (npts+n, True);
	}
	data = result.getStorage(deleteIt) + npts;
	objmove (data, arr.getStorage(deleteIt), n);
	npts += n;
      }
    }
    result.resize (npts, True);
    fillRownrs (npts, nrel);
  }
}

void ColumnsIndexArray::getArray (Vector<String>& result, const String& name)
{
  ArrayColumn<String> arrCol (itsTable, name);
  rownr_t nrrow = arrCol.nrow();
  if (nrrow > 0) {
    Block<rownr_t> nrel(nrrow, rownr_t(0));
    Array<String> arr = arrCol(0);
    rownr_t npts = arr.nelements();
    nrel[0] = npts;
    result.resize (nrrow*npts);
    Bool deleteIt;
    String* data = result.getStorage(deleteIt);
    objmove (data, arr.getStorage(deleteIt), npts);
    data += npts;
    for (rownr_t i=1; i<nrrow; i++) {
      if (arrCol.isDefined(i)) {
	Array<String> arr = arrCol(i);
	rownr_t n = arr.nelements();
	nrel[i] = n;
	if (npts+n > result.nelements()) {
	  result.resize (npts+n, True);
	}
	data = result.getStorage(deleteIt) + npts;
	objmove (data, arr.getStorage(deleteIt), n);
	npts += n;
      }
    }
    result.resize (npts, True);
    fillRownrs (npts, nrel);
  }
}

void ColumnsIndexArray::fillRownrs (rownr_t npts, const Block<rownr_t>& nrel)
{
  itsRownrs.resize (npts);
  rownr_t* data = itsRownrs.storage();
  for (rownr_t i=0; i<nrel.nelements(); i++) {
    rownr_t nr = nrel[i];
    for (rownr_t j=0; j<nr; j++) {
      *data++ = i;
    }
  }
}

} //# NAMESPACE CASACORE - END

