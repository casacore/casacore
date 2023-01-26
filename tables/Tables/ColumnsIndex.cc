//# ColumnsIndex.cc: Index to a table
//# Copyright (C) 1998,1999,2000
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
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableLocker.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ColumnsIndex::ColumnsIndex (const Table& table, const String& columnName,
			    Compare* compareFunction, bool noSort)
: itsLowerKeyPtr (0),
  itsUpperKeyPtr (0)
{
  Vector<String> columnNames(1);
  columnNames(0) = columnName;
  create (table, columnNames, compareFunction, noSort);
}

ColumnsIndex::ColumnsIndex (const Table& table,
			    const Vector<String>& columnNames,
			    Compare* compareFunction, bool noSort)
{
  create (table, columnNames, compareFunction, noSort);
}

ColumnsIndex::ColumnsIndex (const ColumnsIndex& that)
: itsLowerKeyPtr (0),
  itsUpperKeyPtr (0)
{
  copy (that);
}

ColumnsIndex::~ColumnsIndex()
{
  deleteObjects();
}

ColumnsIndex& ColumnsIndex::operator= (const ColumnsIndex& that)
{
  copy (that);
  return *this;
}

void ColumnsIndex::copy (const ColumnsIndex& that)
{
  if (this != &that) {
    deleteObjects();
    itsTable = that.itsTable;
    itsNrrow   = itsTable.nrow();
    itsNoSort  = that.itsNoSort;
    itsCompare = that.itsCompare;
    makeObjects (that.itsLowerKeyPtr->description());
  }
}

Vector<String> ColumnsIndex::columnNames() const
{
  const RecordDesc& desc = itsLowerKeyPtr->description();
  const uint32_t nrfield = desc.nfields();
  Vector<String> names(nrfield);
  for (uint32_t i=0; i<nrfield; i++) {
    names(i) = desc.name(i);
  }
  return names;
}

void ColumnsIndex::deleteObjects()
{
  const uint32_t nrfield = itsDataTypes.nelements();
  for (uint32_t i=0; i<nrfield; i++) {
    switch (itsDataTypes[i]) {
    case TpBool:
      delete (RecordFieldPtr<bool>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<bool>*)(itsUpperFields[i]);
      delete (Vector<bool>*)(itsDataVectors[i]);
      break;
    case TpUChar:
      delete (RecordFieldPtr<unsigned char>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<unsigned char>*)(itsUpperFields[i]);
      delete (Vector<unsigned char>*)(itsDataVectors[i]);
      break;
    case TpShort:
      delete (RecordFieldPtr<int16_t>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<int16_t>*)(itsUpperFields[i]);
      delete (Vector<int16_t>*)(itsDataVectors[i]);
      break;
    case TpInt:
      delete (RecordFieldPtr<int32_t>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<int32_t>*)(itsUpperFields[i]);
      delete (Vector<int32_t>*)(itsDataVectors[i]);
      break;
    case TpUInt:
      delete (RecordFieldPtr<uint32_t>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<uint32_t>*)(itsUpperFields[i]);
      delete (Vector<uint32_t>*)(itsDataVectors[i]);
      break;
    case TpInt64:
      delete (RecordFieldPtr<int64_t>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<int64_t>*)(itsUpperFields[i]);
      delete (Vector<int64_t>*)(itsDataVectors[i]);
      break;
    case TpFloat:
      delete (RecordFieldPtr<float>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<float>*)(itsUpperFields[i]);
      delete (Vector<float>*)(itsDataVectors[i]);
      break;
    case TpDouble:
      delete (RecordFieldPtr<double>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<double>*)(itsUpperFields[i]);
      delete (Vector<double>*)(itsDataVectors[i]);
      break;
    case TpComplex:
      delete (RecordFieldPtr<Complex>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<Complex>*)(itsUpperFields[i]);
      delete (Vector<Complex>*)(itsDataVectors[i]);
      break;
    case TpDComplex:
      delete (RecordFieldPtr<DComplex>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<DComplex>*)(itsUpperFields[i]);
      delete (Vector<DComplex>*)(itsDataVectors[i]);
      break;
    case TpString:
      delete (RecordFieldPtr<String>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<String>*)(itsUpperFields[i]);
      delete (Vector<String>*)(itsDataVectors[i]);
      break;
    default:
      throw (TableError ("ColumnsIndex: unknown data type"));
    }
    itsLowerFields[i] = 0;
    itsUpperFields[i] = 0;
    itsDataVectors[i]  = 0;
    itsData[i]  = 0;
  }
  delete itsLowerKeyPtr;
  delete itsUpperKeyPtr;
  itsLowerKeyPtr = 0;
  itsUpperKeyPtr = 0;
}

void ColumnsIndex::addColumnToDesc (RecordDesc& description,
				    const TableColumn& column)
{
  const ColumnDesc& columnDesc = column.columnDesc();
  DataType dataType = columnDesc.dataType();
  if (! columnDesc.isScalar()) {
    throw (TableError ("ColumnsIndex: column " + columnDesc.name() +
		       " should be a scalar column"));
  }
  description.addField (columnDesc.name(), dataType);
}

void ColumnsIndex::create (const Table& table,
			   const Vector<String>& columnNames,
			   Compare* compareFunction,
			   bool noSort)
{
  itsTable = table;
  itsNrrow = itsTable.nrow();
  itsCompare = (compareFunction == 0  ?  compare : compareFunction);
  itsNoSort = noSort;
  // Loop through all column names.
  // Always add it to the RecordDesc.
  RecordDesc description;
  uint32_t nrfields = columnNames.nelements();
  for (uint32_t i=0; i<nrfields; i++) {
    addColumnToDesc (description,
		     TableColumn (itsTable, columnNames(i)));
  }
  makeObjects (description);
  readData();
}
	    
void ColumnsIndex::makeObjects (const RecordDesc& description)
{
  // Create the Records from the description.
  itsLowerKeyPtr = new Record (description);
  itsUpperKeyPtr = new Record (description);
  // Initialize the column and field block.
  uint32_t nrfield = description.nfields();
  itsDataTypes.resize (nrfield, false, false);
  itsDataVectors.resize (nrfield, false, false);
  itsDataVectors.set (static_cast<void*>(0));
  itsData.resize (nrfield, false, false);
  itsData.set (static_cast<void*>(0));
  itsLowerFields.resize (nrfield, false, false);
  itsLowerFields.set (static_cast<void*>(0));
  itsUpperFields.resize (nrfield, false, false);
  itsUpperFields.set (static_cast<void*>(0));
  itsColumnChanged.resize (nrfield, false, false);
  itsColumnChanged.set (true);
  itsChanged = true;
  // Create the correct column object for each field.
  // Also create a RecordFieldPtr object for each Key.
  // This makes a fast data copy possible.
  for (uint32_t i=0; i<nrfield; i++) {
    itsDataTypes[i] = description.type(i);
    switch (description.type(i)) {
    case TpBool:
    {
      itsLowerFields[i] = new RecordFieldPtr<bool>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<bool>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<bool>;
      break;
    }
    case TpUChar:
    {
      itsLowerFields[i] = new RecordFieldPtr<unsigned char>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<unsigned char>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<unsigned char>;
      break;
    }
    case TpShort:
    {
      itsLowerFields[i] = new RecordFieldPtr<int16_t>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<int16_t>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<int16_t>;
      break;
    }
    case TpInt:
    {
      itsLowerFields[i] = new RecordFieldPtr<int32_t>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<int32_t>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<int32_t>;
      break;
    }
    case TpUInt:
    {
      itsLowerFields[i] = new RecordFieldPtr<uint32_t>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<uint32_t>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<uint32_t>;
      break;
    }
    case TpInt64:
    {
      itsLowerFields[i] = new RecordFieldPtr<int64_t>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<int64_t>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<int64_t>;
      break;
    }
    case TpFloat:
    {
      itsLowerFields[i] = new RecordFieldPtr<float>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<float>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<float>;
      break;
    }
    case TpDouble:
    {
      itsLowerFields[i] = new RecordFieldPtr<double>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<double>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<double>;
      break;
    }
    case TpComplex:
    {
      itsLowerFields[i] = new RecordFieldPtr<Complex>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<Complex>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<Complex>;
      break;
    }
    case TpDComplex:
    {
      itsLowerFields[i] = new RecordFieldPtr<DComplex>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<DComplex>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<DComplex>;
      break;
    }
    case TpString:
    {
      itsLowerFields[i] = new RecordFieldPtr<String>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<String>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<String>;
      break;
    }
    default:
      throw (TableError ("ColumnsIndex: unknown data type"));
    }
  }
}

void ColumnsIndex::readData()
{
  // Acquire a lock if needed.
  TableLocker locker(itsTable, FileLocker::Read);
  rownr_t nrrow = itsTable.nrow();
  if (nrrow != itsNrrow) {
    itsColumnChanged.set (true);
    itsChanged = true;
    itsNrrow = nrrow;
  }
  if (!itsChanged) {
    return;
  }
  Sort sort;
  bool deleteIt;
  const RecordDesc& desc = itsLowerKeyPtr->description();
  uint32_t nrfield = itsDataTypes.nelements();
  for (uint32_t i=0; i<nrfield; i++) {
    const String& name = desc.name(i);
    switch (itsDataTypes[i]) {
    case TpBool:
    {
      Vector<bool>* vecptr = (Vector<bool>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<bool>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpUChar:
    {
      Vector<unsigned char>* vecptr = (Vector<unsigned char>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<unsigned char>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpShort:
    {
      Vector<int16_t>* vecptr = (Vector<int16_t>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<int16_t>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpInt:
    {
      Vector<int32_t>* vecptr = (Vector<int32_t>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<int32_t>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpUInt:
    {
      Vector<uint32_t>* vecptr = (Vector<uint32_t>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<uint32_t>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpInt64:
    {
      Vector<int64_t>* vecptr = (Vector<int64_t>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<int64_t>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpFloat:
    {
      Vector<float>* vecptr = (Vector<float>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<float>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpDouble:
    {
      Vector<double>* vecptr = (Vector<double>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<double>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpComplex:
    {
      Vector<Complex>* vecptr = (Vector<Complex>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Complex>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpDComplex:
    {
      Vector<DComplex>* vecptr = (Vector<DComplex>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<DComplex>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpString:
    {
      Vector<String>* vecptr = (Vector<String>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<String>(itsTable, name).getColumn (*vecptr, true);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    default:
      throw (TableError ("ColumnsIndex: unknown data type"));
    }
    itsColumnChanged[i] = false;
  }
  // Sort the data if needed.
  // Otherwise fill the index vector with 0..n.
  itsDataIndex.resize (itsNrrow);
  if (!itsNoSort) {
    sort.sort (itsDataIndex, itsNrrow);
  } else {
    indgen (itsDataIndex);
  }
  // Determine all unique keys (itsUniqueIndex will contain the index of
  // each first unique entry in itsDataIndex).
  sort.unique (itsUniqueIndex, itsDataIndex);
  itsDataInx = itsDataIndex.getStorage (deleteIt);
  itsUniqueInx = itsUniqueIndex.getStorage (deleteIt);
  itsChanged = false;
}

rownr_t ColumnsIndex::bsearch (bool& found, const Block<void*>& fieldPtrs) const
{
  found = false;
  int64_t lower = 0;
  int64_t upper = itsUniqueIndex.nelements();
  upper--;
  int64_t middle = 0;
  while (lower <= upper) {
    middle = (upper + lower) / 2;
    int32_t cmp = itsCompare (fieldPtrs, itsData, itsDataTypes,
			  itsDataInx[itsUniqueInx[middle]]);
    if (cmp < 0) {
      upper = middle - 1;            // go to left
    } else if (cmp > 0) {
      middle++;
      lower = middle;                // go to right
    } else {
      found = true;
      break;
    }
  }
  return middle;
}

int32_t ColumnsIndex::compare (const Block<void*>& fieldPtrs,
			   const Block<void*>& dataPtrs,
			   const Block<int32_t>& dataTypes,
			   rownr_t index)
{
  uint32_t nfield = fieldPtrs.nelements();
  for (uint32_t i=0; i<nfield; i++) {
    switch (dataTypes[i]) {
    case TpBool:
    {
      const bool left = *(*(RecordFieldPtr<bool>*)(fieldPtrs[i]));
      const bool right = ((const bool*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpUChar:
    {
      const unsigned char left = *(*(RecordFieldPtr<unsigned char>*)(fieldPtrs[i]));
      const unsigned char right = ((const unsigned char*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpShort:
    {
      const int16_t left = *(*(RecordFieldPtr<int16_t>*)(fieldPtrs[i]));
      const int16_t right = ((const int16_t*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpInt:
    {
      const int32_t left = *(*(RecordFieldPtr<int32_t>*)(fieldPtrs[i]));
      const int32_t right = ((const int32_t*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpUInt:
    {
      const uint32_t left = *(*(RecordFieldPtr<uint32_t>*)(fieldPtrs[i]));
      const uint32_t right = ((const uint32_t*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpInt64:
    {
      const int64_t left = *(*(RecordFieldPtr<int64_t>*)(fieldPtrs[i]));
      const int64_t right = ((const int64_t*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpFloat:
    {
      const float left = *(*(RecordFieldPtr<float>*)(fieldPtrs[i]));
      const float right = ((const float*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpDouble:
    {
      const double left = *(*(RecordFieldPtr<double>*)(fieldPtrs[i]));
      const double right = ((const double*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpComplex:
    {
      const Complex& left = *(*(RecordFieldPtr<Complex>*)(fieldPtrs[i]));
      const Complex& right = ((const Complex*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpDComplex:
    {
      const DComplex& left = *(*(RecordFieldPtr<DComplex>*)(fieldPtrs[i]));
      const DComplex& right = ((const DComplex*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpString:
    {
      const String& left = *(*(RecordFieldPtr<String>*)(fieldPtrs[i]));
      const String& right = ((const String*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    default:
      throw (TableError ("ColumnsIndex: unknown data type"));
    }
  }
  return 0;
}
 
rownr_t ColumnsIndex::getRowNumber (bool& found, const Record& key)
{
  copyKey (itsLowerFields, key);
  return getRowNumber (found);
}

rownr_t ColumnsIndex::getRowNumber (bool& found)
{
  if (!isUnique()) {
    throw (TableError ("ColumnsIndex::getRowNumber only possible "
		       "when the index keys are unique"));
  }
  // Read the data (if needed).
  readData();
  rownr_t inx = bsearch (found, itsLowerFields);
  if (found) {
    inx = itsDataInx[inx];
  }
  return inx;
}

RowNumbers ColumnsIndex::getRowNumbers (const Record& key)
{
  copyKey (itsLowerFields, key);
  return getRowNumbers();
}

RowNumbers ColumnsIndex::getRowNumbers()
{
  // Read the data (if needed).
  readData();
  bool found;
  rownr_t inx = bsearch (found, itsLowerFields);
  RowNumbers rows;
  if (found) {
    fillRowNumbers (rows, inx, inx+1);
  }
  return rows;
}

RowNumbers ColumnsIndex::getRowNumbers (const Record& lowerKey,
                                        const Record& upperKey,
                                        bool lowerInclusive,
                                        bool upperInclusive)
{
  copyKey (itsLowerFields, lowerKey);
  copyKey (itsUpperFields, upperKey);
  return getRowNumbers (lowerInclusive, upperInclusive);
}

RowNumbers ColumnsIndex::getRowNumbers (bool lowerInclusive,
                                        bool upperInclusive)
{
  // Read the data (if needed).
  readData();
  bool found;
  // Try to find the lower key. If not found, bsearch is giving the
  // index of the next higher key.
  // So increment the start index if found and is not to be included.
  rownr_t start = bsearch (found, itsLowerFields);
  if (found  &&  !lowerInclusive) {
    start++;
  }
  // Try to find the upper key.
  // Increment the end index such that it is not inclusive
  // (thus increment if the found end index is to be included).
  rownr_t end = bsearch (found, itsUpperFields);
  if (found  &&  upperInclusive) {
    end++;
  }
  RowNumbers rows;
  if (start < end) {
    fillRowNumbers (rows, start, end);
  }
  return rows;
}

void ColumnsIndex::fillRowNumbers (Vector<rownr_t>& rows,
				   rownr_t start, rownr_t end) const
{
  start = itsUniqueInx[start];
  if (end < itsUniqueIndex.nelements()) {
    end = itsUniqueInx[end];
  } else {
    end = itsDataIndex.nelements();
  }
  rownr_t nr = end-start;
  rows.resize (nr);
  bool deleteIt;
  rownr_t* rowStorage = rows.getStorage (deleteIt);
  objcopy (rowStorage, itsDataInx+start, nr);
  rows.putStorage (rowStorage, deleteIt);
}

void ColumnsIndex::setChanged()
{
  itsColumnChanged.set (true);
  itsChanged = true;
}

void ColumnsIndex::setChanged (const String& columnName)
{
  const RecordDesc& desc = itsLowerKeyPtr->description();
  uint32_t nrfield = itsColumnChanged.nelements();
  for (uint32_t i=0; i<nrfield; i++) {
    if (desc.name(i) == columnName) {
      itsColumnChanged[i] = true;
      itsChanged = true;
      break;
    }
  }
}

void ColumnsIndex::copyKeyField (void* fieldPtr, int dtype, const Record& key)
{
  switch (dtype) {
    case TpBool:
      copyKeyField(*(RecordFieldPtr<bool>*)(fieldPtr), key);
      break;
    case TpUChar:
      copyKeyField(*(RecordFieldPtr<unsigned char>*)(fieldPtr), key);
      break;
    case TpShort:
      copyKeyField(*(RecordFieldPtr<int16_t>*)(fieldPtr), key);
      break;
    case TpInt:
      copyKeyField(*(RecordFieldPtr<int32_t>*)(fieldPtr), key);
      break;
    case TpUInt:
      copyKeyField(*(RecordFieldPtr<uint32_t>*)(fieldPtr), key);
      break;
    case TpInt64:
      copyKeyField(*(RecordFieldPtr<int64_t>*)(fieldPtr), key);
      break;
    case TpFloat:
      copyKeyField(*(RecordFieldPtr<float>*)(fieldPtr), key);
      break;
    case TpDouble:
      copyKeyField(*(RecordFieldPtr<double>*)(fieldPtr), key);
      break;
    case TpComplex:
      copyKeyField(*(RecordFieldPtr<Complex>*)(fieldPtr), key);
      break;
    case TpDComplex:
      copyKeyField(*(RecordFieldPtr<DComplex>*)(fieldPtr), key);
      break;
    case TpString:
      copyKeyField(*(RecordFieldPtr<String>*)(fieldPtr), key);
      break;
    default:
      throw (TableError ("ColumnsIndex: unknown data type"));
    }
}

void ColumnsIndex::copyKey (Block<void*> fields, const Record& key)
{
  for (uint32_t i=0; i<fields.nelements(); i++) {
    copyKeyField (fields[i], itsDataTypes[i], key);
  }
}

} //# NAMESPACE CASACORE - END
