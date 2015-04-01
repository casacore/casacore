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
//#
//# $Id$

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
			    Compare* compareFunction, Bool noSort)
: itsLowerKeyPtr (0),
  itsUpperKeyPtr (0)
{
  Vector<String> columnNames(1);
  columnNames(0) = columnName;
  create (table, columnNames, compareFunction, noSort);
}

ColumnsIndex::ColumnsIndex (const Table& table,
			    const Vector<String>& columnNames,
			    Compare* compareFunction, Bool noSort)
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
  const uInt nrfield = desc.nfields();
  Vector<String> names(nrfield);
  for (uInt i=0; i<nrfield; i++) {
    names(i) = desc.name(i);
  }
  return names;
}

void ColumnsIndex::deleteObjects()
{
  const uInt nrfield = itsDataTypes.nelements();
  for (uInt i=0; i<nrfield; i++) {
    switch (itsDataTypes[i]) {
    case TpBool:
      delete (RecordFieldPtr<Bool>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<Bool>*)(itsUpperFields[i]);
      delete (Vector<Bool>*)(itsDataVectors[i]);
      break;
    case TpUChar:
      delete (RecordFieldPtr<uChar>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<uChar>*)(itsUpperFields[i]);
      delete (Vector<uChar>*)(itsDataVectors[i]);
      break;
    case TpShort:
      delete (RecordFieldPtr<Short>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<Short>*)(itsUpperFields[i]);
      delete (Vector<Short>*)(itsDataVectors[i]);
      break;
    case TpInt:
      delete (RecordFieldPtr<Int>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<Int>*)(itsUpperFields[i]);
      delete (Vector<Int>*)(itsDataVectors[i]);
      break;
    case TpUInt:
      delete (RecordFieldPtr<uInt>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<uInt>*)(itsUpperFields[i]);
      delete (Vector<uInt>*)(itsDataVectors[i]);
      break;
    case TpFloat:
      delete (RecordFieldPtr<Float>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<Float>*)(itsUpperFields[i]);
      delete (Vector<Float>*)(itsDataVectors[i]);
      break;
    case TpDouble:
      delete (RecordFieldPtr<Double>*)(itsLowerFields[i]);
      delete (RecordFieldPtr<Double>*)(itsUpperFields[i]);
      delete (Vector<Double>*)(itsDataVectors[i]);
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
			   Bool noSort)
{
  itsTable = table;
  itsNrrow = itsTable.nrow();
  itsCompare = (compareFunction == 0  ?  compare : compareFunction);
  itsNoSort = noSort;
  // Loop through all column names.
  // Always add it to the RecordDesc.
  RecordDesc description;
  uInt nrfields = columnNames.nelements();
  for (uInt i=0; i<nrfields; i++) {
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
  uInt nrfield = description.nfields();
  itsDataTypes.resize (nrfield, False, False);
  itsDataVectors.resize (nrfield, False, False);
  itsDataVectors.set (static_cast<void*>(0));
  itsData.resize (nrfield, False, False);
  itsData.set (static_cast<void*>(0));
  itsLowerFields.resize (nrfield, False, False);
  itsLowerFields.set (static_cast<void*>(0));
  itsUpperFields.resize (nrfield, False, False);
  itsUpperFields.set (static_cast<void*>(0));
  itsColumnChanged.resize (nrfield, False, False);
  itsColumnChanged.set (True);
  itsChanged = True;
  // Create the correct column object for each field.
  // Also create a RecordFieldPtr object for each Key.
  // This makes a fast data copy possible.
  for (uInt i=0; i<nrfield; i++) {
    itsDataTypes[i] = description.type(i);
    switch (description.type(i)) {
    case TpBool:
    {
      itsLowerFields[i] = new RecordFieldPtr<Bool>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<Bool>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<Bool>;
      break;
    }
    case TpUChar:
    {
      itsLowerFields[i] = new RecordFieldPtr<uChar>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<uChar>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<uChar>;
      break;
    }
    case TpShort:
    {
      itsLowerFields[i] = new RecordFieldPtr<Short>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<Short>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<Short>;
      break;
    }
    case TpInt:
    {
      itsLowerFields[i] = new RecordFieldPtr<Int>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<Int>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<Int>;
      break;
    }
    case TpUInt:
    {
      itsLowerFields[i] = new RecordFieldPtr<uInt>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<uInt>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<uInt>;
      break;
    }
    case TpFloat:
    {
      itsLowerFields[i] = new RecordFieldPtr<Float>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<Float>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<Float>;
      break;
    }
    case TpDouble:
    {
      itsLowerFields[i] = new RecordFieldPtr<Double>(*itsLowerKeyPtr, i);
      itsUpperFields[i] = new RecordFieldPtr<Double>(*itsUpperKeyPtr, i);
      itsDataVectors[i] = new Vector<Double>;
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
  uInt nrrow = itsTable.nrow();
  if (nrrow != itsNrrow) {
    itsColumnChanged.set (True);
    itsChanged = True;
    itsNrrow = nrrow;
  }
  if (!itsChanged) {
    return;
  }
  Sort sort;
  Bool deleteIt;
  const RecordDesc& desc = itsLowerKeyPtr->description();
  uInt nrfield = itsDataTypes.nelements();
  for (uInt i=0; i<nrfield; i++) {
    const String& name = desc.name(i);
    switch (itsDataTypes[i]) {
    case TpBool:
    {
      Vector<Bool>* vecptr = (Vector<Bool>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Bool>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpUChar:
    {
      Vector<uChar>* vecptr = (Vector<uChar>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<uChar>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpShort:
    {
      Vector<Short>* vecptr = (Vector<Short>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Short>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpInt:
    {
      Vector<Int>* vecptr = (Vector<Int>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Int>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpUInt:
    {
      Vector<uInt>* vecptr = (Vector<uInt>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<uInt>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpFloat:
    {
      Vector<Float>* vecptr = (Vector<Float>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Float>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpDouble:
    {
      Vector<Double>* vecptr = (Vector<Double>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Double>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpComplex:
    {
      Vector<Complex>* vecptr = (Vector<Complex>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<Complex>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpDComplex:
    {
      Vector<DComplex>* vecptr = (Vector<DComplex>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<DComplex>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    case TpString:
    {
      Vector<String>* vecptr = (Vector<String>*)itsDataVectors[i];
      if (itsColumnChanged[i]) {
	ScalarColumn<String>(itsTable, name).getColumn (*vecptr, True);
      }
      itsData[i] = vecptr->getStorage (deleteIt);
      sort.sortKey (itsData[i], desc.type(i));
      break;
    }
    default:
      throw (TableError ("ColumnsIndex: unknown data type"));
    }
    itsColumnChanged[i] = False;
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
  itsChanged = False;
}

uInt ColumnsIndex::bsearch (Bool& found, const Block<void*>& fieldPtrs) const
{
  found = False;
  Int lower = 0;
  Int upper = itsUniqueIndex.nelements() - 1;
  Int middle = 0;
  while (lower <= upper) {
    middle = (upper + lower) / 2;
    Int cmp = itsCompare (fieldPtrs, itsData, itsDataTypes,
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

Int ColumnsIndex::compare (const Block<void*>& fieldPtrs,
			   const Block<void*>& dataPtrs,
			   const Block<Int>& dataTypes,
			   Int index)
{
  uInt nfield = fieldPtrs.nelements();
  for (uInt i=0; i<nfield; i++) {
    switch (dataTypes[i]) {
    case TpBool:
    {
      const Bool left = *(*(RecordFieldPtr<Bool>*)(fieldPtrs[i]));
      const Bool right = ((const Bool*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpUChar:
    {
      const uChar left = *(*(RecordFieldPtr<uChar>*)(fieldPtrs[i]));
      const uChar right = ((const uChar*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpShort:
    {
      const Short left = *(*(RecordFieldPtr<Short>*)(fieldPtrs[i]));
      const Short right = ((const Short*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpInt:
    {
      const Int left = *(*(RecordFieldPtr<Int>*)(fieldPtrs[i]));
      const Int right = ((const Int*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpUInt:
    {
      const uInt left = *(*(RecordFieldPtr<uInt>*)(fieldPtrs[i]));
      const uInt right = ((const uInt*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpFloat:
    {
      const Float left = *(*(RecordFieldPtr<Float>*)(fieldPtrs[i]));
      const Float right = ((const Float*)(dataPtrs[i]))[index];
      if (left < right) {
	return -1;
      } else if (left > right) {
	return 1;
      }
      break;
    }
    case TpDouble:
    {
      const Double left = *(*(RecordFieldPtr<Double>*)(fieldPtrs[i]));
      const Double right = ((const Double*)(dataPtrs[i]))[index];
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
 
uInt ColumnsIndex::getRowNumber (Bool& found, const Record& key)
{
  copyKey (itsLowerFields, key);
  return getRowNumber (found);
}

uInt ColumnsIndex::getRowNumber (Bool& found)
{
  if (!isUnique()) {
    throw (TableError ("ColumnsIndex::getRowNumber only possible "
		       "when the index keys are unique"));
  }
  // Read the data (if needed).
  readData();
  uInt inx = bsearch (found, itsLowerFields);
  if (found) {
    inx = itsDataInx[inx];
  }
  return inx;
}

Vector<uInt> ColumnsIndex::getRowNumbers (const Record& key)
{
  copyKey (itsLowerFields, key);
  return getRowNumbers();
}

Vector<uInt> ColumnsIndex::getRowNumbers()
{
  // Read the data (if needed).
  readData();
  Bool found;
  uInt inx = bsearch (found, itsLowerFields);
  Vector<uInt> rows;
  if (found) {
    fillRowNumbers (rows, inx, inx+1);
  }
  return rows;
}

Vector<uInt> ColumnsIndex::getRowNumbers (const Record& lowerKey,
					  const Record& upperKey,
					  Bool lowerInclusive,
					  Bool upperInclusive)
{
  copyKey (itsLowerFields, lowerKey);
  copyKey (itsUpperFields, upperKey);
  return getRowNumbers (lowerInclusive, upperInclusive);
}

Vector<uInt> ColumnsIndex::getRowNumbers (Bool lowerInclusive,
					  Bool upperInclusive)
{
  // Read the data (if needed).
  readData();
  Bool found;
  // Try to find the lower key. If not found, bsearch is giving the
  // index of the next higher key.
  // So increment the start index if found and is not to be included.
  uInt start = bsearch (found, itsLowerFields);
  if (found  &&  !lowerInclusive) {
    start++;
  }
  // Try to find the upper key.
  // Increment the end index such that it is not inclusive
  // (thus increment if the found end index is to be included).
  uInt end = bsearch (found, itsUpperFields);
  if (found  &&  upperInclusive) {
    end++;
  }
  Vector<uInt> rows;
  if (start < end) {
    fillRowNumbers (rows, start, end);
  }
  return rows;
}

void ColumnsIndex::fillRowNumbers (Vector<uInt>& rows,
				   uInt start, uInt end) const
{
  start = itsUniqueInx[start];
  if (end < itsUniqueIndex.nelements()) {
    end = itsUniqueInx[end];
  } else {
    end = itsDataIndex.nelements();
  }
  uInt nr = end-start;
  rows.resize (nr);
  Bool deleteIt;
  uInt* rowStorage = rows.getStorage (deleteIt);
  objcopy (rowStorage, itsDataInx+start, nr);
  rows.putStorage (rowStorage, deleteIt);
}

void ColumnsIndex::setChanged()
{
  itsColumnChanged.set (True);
  itsChanged = True;
}

void ColumnsIndex::setChanged (const String& columnName)
{
  const RecordDesc& desc = itsLowerKeyPtr->description();
  uInt nrfield = itsColumnChanged.nelements();
  for (uInt i=0; i<nrfield; i++) {
    if (desc.name(i) == columnName) {
      itsColumnChanged[i] = True;
      itsChanged = True;
      break;
    }
  }
}

void ColumnsIndex::copyKeyField (void* fieldPtr, int dtype, const Record& key)
{
  switch (dtype) {
    case TpBool:
      copyKeyField(*(RecordFieldPtr<Bool>*)(fieldPtr), key);
      break;
    case TpUChar:
      copyKeyField(*(RecordFieldPtr<uChar>*)(fieldPtr), key);
      break;
    case TpShort:
      copyKeyField(*(RecordFieldPtr<Short>*)(fieldPtr), key);
      break;
    case TpInt:
      copyKeyField(*(RecordFieldPtr<Int>*)(fieldPtr), key);
      break;
    case TpUInt:
      copyKeyField(*(RecordFieldPtr<uInt>*)(fieldPtr), key);
      break;
    case TpFloat:
      copyKeyField(*(RecordFieldPtr<Float>*)(fieldPtr), key);
      break;
    case TpDouble:
      copyKeyField(*(RecordFieldPtr<Double>*)(fieldPtr), key);
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
  for (uInt i=0; i<fields.nelements(); i++) {
    copyKeyField (fields[i], itsDataTypes[i], key);
  }
}

} //# NAMESPACE CASACORE - END
