// # TableRow.cc: Access to a table row
// # Copyright (C) 1996,1997,1998,1999,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

ROTableRow::ROTableRow() : itsRecord(nullptr) { init(); }

ROTableRow::ROTableRow(const Table& table, bool storedColumnsOnly) : itsRecord(nullptr) {
  init();
  create(table, storedColumnsOnly, false);
}

ROTableRow::ROTableRow(const Table& table, const Vector<String>& columnNames, bool exclude)
    : itsRecord(nullptr) {
  init();
  create(table, columnNames, exclude, false);
}

ROTableRow::ROTableRow(const ROTableRow& that) : itsRecord(nullptr) {
  init();
  copy(that);
}

void ROTableRow::init() {
  itsLastRow = -1;
  itsReread = true;
}

ROTableRow::~ROTableRow() { deleteObjects(); }

ROTableRow& ROTableRow::operator=(const ROTableRow& that) {
  copy(that);
  return *this;
}

void ROTableRow::copy(const ROTableRow& that) {
  if (this != &that) {
    deleteObjects();
    itsTable = that.itsTable;
    itsNrused = that.itsNrused;
    itsLastRow = that.itsLastRow;
    itsReread = that.itsReread;
    if (that.itsRecord != nullptr) {
      makeObjects(that.itsRecord->description());
    }
  }
}

Vector<String> ROTableRow::columnNames() const {
  const RecordDesc& desc = itsRecord->description();
  unsigned int nfield = desc.nfields();
  Vector<String> names(nfield);
  for (unsigned int i = 0; i < nfield; i++) {
    names(i) = desc.name(i);
  }
  return names;
}

void ROTableRow::deleteObjects() {
  if (itsRecord == nullptr) {
    return;
  }
  const RecordDesc& description = itsRecord->description();
  for (unsigned int i = 0; i < itsNrused; i++) {
    delete (TableColumn*)(itsTabCols[i]);
    switch (description.type(i)) {
      case TpBool:
        delete (ScalarColumn<bool>*)(itsColumns[i]);
        delete (RecordFieldPtr<bool>*)(itsFields[i]);
        break;
      case TpArrayBool:
        delete (ArrayColumn<bool>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<bool>>*)(itsFields[i]);
        break;
      case TpUChar:
        delete (ScalarColumn<unsigned char>*)(itsColumns[i]);
        delete (RecordFieldPtr<unsigned char>*)(itsFields[i]);
        break;
      case TpArrayUChar:
        delete (ArrayColumn<unsigned char>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<unsigned char>>*)(itsFields[i]);
        break;
      case TpShort:
        delete (ScalarColumn<short>*)(itsColumns[i]);
        delete (RecordFieldPtr<short>*)(itsFields[i]);
        break;
      case TpArrayShort:
        delete (ArrayColumn<short>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<short>>*)(itsFields[i]);
        break;
      case TpInt:
        delete (ScalarColumn<int>*)(itsColumns[i]);
        delete (RecordFieldPtr<int>*)(itsFields[i]);
        break;
      case TpArrayInt:
        delete (ArrayColumn<int>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<int>>*)(itsFields[i]);
        break;
      case TpUInt:
        delete (ScalarColumn<unsigned int>*)(itsColumns[i]);
        delete (RecordFieldPtr<unsigned int>*)(itsFields[i]);
        break;
      case TpArrayUInt:
        delete (ArrayColumn<unsigned int>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<unsigned int>>*)(itsFields[i]);
        break;
      case TpInt64:
        delete (ScalarColumn<int64_t>*)(itsColumns[i]);
        delete (RecordFieldPtr<int64_t>*)(itsFields[i]);
        break;
      case TpArrayInt64:
        delete (ArrayColumn<int64_t>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<int64_t>>*)(itsFields[i]);
        break;
      case TpFloat:
        delete (ScalarColumn<float>*)(itsColumns[i]);
        delete (RecordFieldPtr<float>*)(itsFields[i]);
        break;
      case TpArrayFloat:
        delete (ArrayColumn<float>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<float>>*)(itsFields[i]);
        break;
      case TpDouble:
        delete (ScalarColumn<double>*)(itsColumns[i]);
        delete (RecordFieldPtr<double>*)(itsFields[i]);
        break;
      case TpArrayDouble:
        delete (ArrayColumn<double>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<double>>*)(itsFields[i]);
        break;
      case TpComplex:
        delete (ScalarColumn<Complex>*)(itsColumns[i]);
        delete (RecordFieldPtr<Complex>*)(itsFields[i]);
        break;
      case TpArrayComplex:
        delete (ArrayColumn<Complex>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<Complex>>*)(itsFields[i]);
        break;
      case TpDComplex:
        delete (ScalarColumn<DComplex>*)(itsColumns[i]);
        delete (RecordFieldPtr<DComplex>*)(itsFields[i]);
        break;
      case TpArrayDComplex:
        delete (ArrayColumn<DComplex>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<DComplex>>*)(itsFields[i]);
        break;
      case TpString:
        delete (ScalarColumn<String>*)(itsColumns[i]);
        delete (RecordFieldPtr<String>*)(itsFields[i]);
        break;
      case TpArrayString:
        delete (ArrayColumn<String>*)(itsColumns[i]);
        delete (RecordFieldPtr<Array<String>>*)(itsFields[i]);
        break;
      case TpRecord:
        delete (ScalarColumn<TableRecord>*)(itsColumns[i]);
        delete (RecordFieldPtr<TableRecord>*)(itsFields[i]);
        break;
      default:
        throw(TableError("TableRow: unknown data type"));
    }
    itsTabCols[i] = nullptr;
    itsColumns[i] = nullptr;
    itsFields[i] = nullptr;
  }
  delete itsRecord;
  itsRecord = nullptr;
}

void ROTableRow::addColumnToDesc(RecordDesc& description, const TableColumn& column,
                                 bool skipOther) {
  const ColumnDesc& columnDesc = column.columnDesc();
  DataType dataType = columnDesc.dataType();
  if (!(skipOther && dataType == TpOther)) {
    if (columnDesc.isArray()) {
      IPosition shape = column.shapeColumn();
      if (shape.nelements() == 0) {
        shape = IPosition(1, -1);
      }
      description.addField(columnDesc.name(), dataType, shape);
    } else {
      description.addField(columnDesc.name(), dataType);
    }
    itsNrused++;
  }
}

void ROTableRow::create(const Table& table, bool storedColumnsOnly, bool writable) {
  itsTable = table;
  // Loop through all columns in the table.
  // Add it to the RecordDesc when the column is writable or
  // when we do not need to write and when the column is stored
  // or no stored columns are asked for..
  itsNrused = 0;
  RecordDesc description;
  unsigned int nrcol = itsTable.tableDesc().ncolumn();
  for (unsigned int i = 0; i < nrcol; i++) {
    if ((!storedColumnsOnly || itsTable.isColumnStored(i)) &&
        (!writable || itsTable.isColumnWritable(i))) {
      addColumnToDesc(description, TableColumn(itsTable, i), true);
    }
  }
  makeObjects(description);
}

void ROTableRow::create(const Table& table, const Vector<String>& columnNames, bool exclude,
                        bool writable) {
  itsTable = table;
  // Loop through all column names.
  // Always add it to the RecordDesc.
  itsNrused = 0;
  RecordDesc description;
  if (exclude) {
    makeDescExclude(description, columnNames, writable);
  } else {
    unsigned int nrcol = columnNames.nelements();
    for (unsigned int i = 0; i < nrcol; i++) {
      addColumnToDesc(description, TableColumn(itsTable, columnNames(i)), true);
    }
  }
  makeObjects(description);
}

void ROTableRow::makeDescExclude(RecordDesc& description, const Vector<String>& columnNames,
                                 bool writable) {
  // Loop through all columns in the table.
  // Add it to the RecordDesc when the column is writable or
  // when we do not need to write. Skip the columns to exclude.
  unsigned int nrcol = itsTable.tableDesc().ncolumn();
  unsigned int nrexcl = columnNames.nelements();
  const TableDesc& tableDesc = itsTable.tableDesc();
  for (unsigned int i = 0; i < nrcol; i++) {
    if (!writable || itsTable.isColumnWritable(i)) {
      const String& name = tableDesc[i].name();
      bool found = false;
      for (unsigned int j = 0; j < nrexcl; j++) {
        if (name == columnNames(j)) {
          found = true;
          break;
        }
      }
      if (!found) {
        addColumnToDesc(description, TableColumn(itsTable, i), true);
      }
    }
  }
}

void ROTableRow::makeObjects(const RecordDesc& description) {
  // Create the TableRecord from the description.
  itsRecord = new TableRecord(description);
  // Initialize the column and field block.
  itsTabCols.resize(itsNrused, false, false);
  itsTabCols.set(static_cast<void*>(0));
  itsColumns.resize(itsNrused, false, false);
  itsColumns.set(static_cast<void*>(0));
  itsFields.resize(itsNrused, false, false);
  itsFields.set(static_cast<void*>(0));
  itsDefined.resize(itsNrused, false, false);
  itsDefined.set(true);
  // Create the correct column object for each field.
  // (if not writing, an RO version is sufficient).
  // Also create a RecordFieldPtr object for each column.
  // This makes a fast data copy possible.
  unsigned int nrfield = description.nfields();
  for (unsigned int i = 0; i < nrfield; i++) {
    const String& name = description.name(i);
    TableColumn* tabColPtr = new TableColumn(itsTable, name);
    itsTabCols[i] = tabColPtr;
    switch (description.type(i)) {
      case TpBool:
        itsColumns[i] = new ScalarColumn<bool>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<bool>(*itsRecord, i);
        break;
      case TpUChar:
        itsColumns[i] = new ScalarColumn<unsigned char>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<unsigned char>(*itsRecord, i);
        break;
      case TpShort:
        itsColumns[i] = new ScalarColumn<short>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<short>(*itsRecord, i);
        break;
      case TpInt:
        itsColumns[i] = new ScalarColumn<int>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<int>(*itsRecord, i);
        break;
      case TpUInt:
        itsColumns[i] = new ScalarColumn<unsigned int>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<unsigned int>(*itsRecord, i);
        break;
      case TpInt64:
        itsColumns[i] = new ScalarColumn<int64_t>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<int64_t>(*itsRecord, i);
        break;
      case TpFloat:
        itsColumns[i] = new ScalarColumn<float>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<float>(*itsRecord, i);
        break;
      case TpDouble:
        itsColumns[i] = new ScalarColumn<double>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<double>(*itsRecord, i);
        break;
      case TpComplex:
        itsColumns[i] = new ScalarColumn<Complex>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Complex>(*itsRecord, i);
        break;
      case TpDComplex:
        itsColumns[i] = new ScalarColumn<DComplex>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<DComplex>(*itsRecord, i);
        break;
      case TpString:
        itsColumns[i] = new ScalarColumn<String>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<String>(*itsRecord, i);
        break;
      case TpRecord:
        itsColumns[i] = new ScalarColumn<TableRecord>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<TableRecord>(*itsRecord, i);
        break;
      case TpArrayBool:
        itsColumns[i] = new ArrayColumn<bool>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<bool>>(*itsRecord, i);
        break;
      case TpArrayUChar:
        itsColumns[i] = new ArrayColumn<unsigned char>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<unsigned char>>(*itsRecord, i);
        break;
      case TpArrayShort:
        itsColumns[i] = new ArrayColumn<short>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<short>>(*itsRecord, i);
        break;
      case TpArrayInt:
        itsColumns[i] = new ArrayColumn<int>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<int>>(*itsRecord, i);
        break;
      case TpArrayUInt:
        itsColumns[i] = new ArrayColumn<unsigned int>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<unsigned int>>(*itsRecord, i);
        break;
      case TpArrayInt64:
        itsColumns[i] = new ArrayColumn<int64_t>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<int64_t>>(*itsRecord, i);
        break;
      case TpArrayFloat:
        itsColumns[i] = new ArrayColumn<float>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<float>>(*itsRecord, i);
        break;
      case TpArrayDouble:
        itsColumns[i] = new ArrayColumn<double>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<double>>(*itsRecord, i);
        break;
      case TpArrayComplex:
        itsColumns[i] = new ArrayColumn<Complex>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<Complex>>(*itsRecord, i);
        break;
      case TpArrayDComplex:
        itsColumns[i] = new ArrayColumn<DComplex>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<DComplex>>(*itsRecord, i);
        break;
      case TpArrayString:
        itsColumns[i] = new ArrayColumn<String>(itsTable, name);
        itsFields[i] = new RecordFieldPtr<Array<String>>(*itsRecord, i);
        break;
      default:
        throw(
            TableError("TableRow: cannot handle Table "
                       "and TableRecord yet"));
    }
  }
}

const TableRecord& ROTableRow::get(rownr_t rownr, bool alwaysRead) const {
  // Only read when needed.
  if (int64_t(rownr) == itsLastRow && !itsReread && !alwaysRead) {
    return *itsRecord;
  }
  const RecordDesc& desc = itsRecord->description();
  int ndim = 0;
  unsigned int nrfield = desc.nfields();
  for (unsigned int i = 0; i < nrfield; i++) {
    // # First determine if an array value is defined.
    // # If not, get its default dimensionality.
    bool isDefined = true;
    if (!(*(TableColumn*)(itsTabCols[i])).isDefined(rownr)) {
      isDefined = false;
      ndim = (*(TableColumn*)(itsTabCols[i])).columnDesc().ndim();
      if (ndim < 0) {
        ndim = 0;
      }
    }
    itsDefined[i] = isDefined;
    // # Now get the value; if undefined, create zero-length array
    // # with the correct dimensionality.
    switch (desc.type(i)) {
      case TpBool:
        (*(const ScalarColumn<bool>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<bool>*)itsFields[i]));
        break;
      case TpArrayBool:
        if (isDefined) {
          (*(const ArrayColumn<bool>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<bool>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<bool>>*)(itsFields[i])).define(Array<bool>(IPosition(ndim, 0)));
        }
        break;
      case TpUChar:
        (*(const ScalarColumn<unsigned char>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<unsigned char>*)itsFields[i]));
        break;
      case TpArrayUChar:
        if (isDefined) {
          (*(const ArrayColumn<unsigned char>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<unsigned char>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<unsigned char>>*)(itsFields[i]))
              .define(Array<unsigned char>(IPosition(ndim, 0)));
        }
        break;
      case TpShort:
        (*(const ScalarColumn<short>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<short>*)itsFields[i]));
        break;
      case TpArrayShort:
        if (isDefined) {
          (*(const ArrayColumn<short>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<short>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<short>>*)(itsFields[i])).define(Array<short>(IPosition(ndim, 0)));
        }
        break;
      case TpInt:
        (*(const ScalarColumn<int>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<int>*)itsFields[i]));
        break;
      case TpArrayInt:
        if (isDefined) {
          (*(const ArrayColumn<int>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<int>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<int>>*)(itsFields[i])).define(Array<int>(IPosition(ndim, 0)));
        }
        break;
      case TpUInt:
        (*(const ScalarColumn<unsigned int>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<unsigned int>*)itsFields[i]));
        break;
      case TpArrayUInt:
        if (isDefined) {
          (*(const ArrayColumn<unsigned int>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<unsigned int>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<unsigned int>>*)(itsFields[i]))
              .define(Array<unsigned int>(IPosition(ndim, 0)));
        }
        break;
      case TpInt64:
        (*(const ScalarColumn<int64_t>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<int64_t>*)itsFields[i]));
        break;
      case TpArrayInt64:
        if (isDefined) {
          (*(const ArrayColumn<int64_t>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<int64_t>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<int64_t>>*)(itsFields[i]))
              .define(Array<int64_t>(IPosition(ndim, 0)));
        }
        break;
      case TpFloat:
        (*(const ScalarColumn<float>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<float>*)itsFields[i]));
        break;
      case TpArrayFloat:
        if (isDefined) {
          (*(const ArrayColumn<float>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<float>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<float>>*)(itsFields[i])).define(Array<float>(IPosition(ndim, 0)));
        }
        break;
      case TpDouble:
        (*(const ScalarColumn<double>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<double>*)itsFields[i]));
        break;
      case TpArrayDouble:
        if (isDefined) {
          (*(const ArrayColumn<double>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<double>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<double>>*)(itsFields[i]))
              .define(Array<double>(IPosition(ndim, 0)));
        }
        break;
      case TpComplex:
        (*(const ScalarColumn<Complex>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<Complex>*)itsFields[i]));
        break;
      case TpArrayComplex:
        if (isDefined) {
          (*(const ArrayColumn<Complex>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<Complex>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<Complex>>*)(itsFields[i]))
              .define(Array<Complex>(IPosition(ndim, 0)));
        }
        break;
      case TpDComplex:
        (*(const ScalarColumn<DComplex>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<DComplex>*)itsFields[i]));
        break;
      case TpArrayDComplex:
        if (isDefined) {
          (*(const ArrayColumn<DComplex>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<DComplex>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<DComplex>>*)(itsFields[i]))
              .define(Array<DComplex>(IPosition(ndim, 0)));
        }
        break;
      case TpString:
        (*(const ScalarColumn<String>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<String>*)itsFields[i]));
        break;
      case TpArrayString:
        if (isDefined) {
          (*(const ArrayColumn<String>*)(itsColumns[i]))
              .get(rownr, *(*(RecordFieldPtr<Array<String>>*)itsFields[i]), true);
        } else {
          (*(RecordFieldPtr<Array<String>>*)(itsFields[i]))
              .define(Array<String>(IPosition(ndim, 0)));
        }
        break;
      case TpRecord:
        (*(const ScalarColumn<TableRecord>*)(itsColumns[i]))
            .get(rownr, *(*(RecordFieldPtr<TableRecord>*)itsFields[i]));
        break;
      default:
        throw(TableError("TableRow: unknown data type"));
    }
  }
  itsLastRow = rownr;
  itsReread = false;
  return *itsRecord;
}

// The values (might) have changed, which is not reflected in the
// internal record. Be sure to reread when the same row is asked for.
void ROTableRow::setReread(rownr_t rownr) {
  if (int64_t(rownr) == itsLastRow) {
    itsReread = true;
  }
}

// put into column, convert if necessary
template <typename Type>
void ROTableRow::PutFieldArray(rownr_t rownr, const TableRecord& record, int whichColumn,
                               int whichField) {
  try {
    (*(ArrayColumn<Type>*)(itsColumns[whichColumn]))
        .put(rownr, record.asArrayGeneric<Type>(whichField));
  } catch (const AipsError& e) {
    (*(ArrayColumn<Type>*)(itsColumns[whichColumn]))
        .put(rownr, record.toArrayGeneric<Type>(whichField));
  }
}

void ROTableRow::putField(rownr_t rownr, const TableRecord& record, int whichColumn,
                          int whichField) {
  switch (itsRecord->description().type(whichColumn)) {
    case TpBool:
      (*(ScalarColumn<bool>*)(itsColumns[whichColumn])).put(rownr, record.asBool(whichField));
      break;
    case TpArrayBool:
      PutFieldArray<bool>(rownr, record, whichColumn, whichField);
      break;
    case TpUChar:
      (*(ScalarColumn<unsigned char>*)(itsColumns[whichColumn]))
          .put(rownr, record.asuChar(whichField));
      break;
    case TpArrayUChar:
      PutFieldArray<unsigned char>(rownr, record, whichColumn, whichField);
      break;
    case TpShort:
      (*(ScalarColumn<short>*)(itsColumns[whichColumn])).put(rownr, record.asShort(whichField));
      break;
    case TpArrayShort:
      PutFieldArray<short>(rownr, record, whichColumn, whichField);
      break;
    case TpInt:
      (*(ScalarColumn<int>*)(itsColumns[whichColumn])).put(rownr, record.asInt(whichField));
      break;
    case TpArrayInt:
      PutFieldArray<int>(rownr, record, whichColumn, whichField);
      break;
    case TpUInt:
      (*(ScalarColumn<unsigned int>*)(itsColumns[whichColumn]))
          .put(rownr, record.asuInt(whichField));
      break;
    case TpArrayUInt:
      PutFieldArray<unsigned int>(rownr, record, whichColumn, whichField);
      break;
    case TpInt64:
      (*(ScalarColumn<int64_t>*)(itsColumns[whichColumn])).put(rownr, record.asInt64(whichField));
      break;
    case TpArrayInt64:
      PutFieldArray<int64_t>(rownr, record, whichColumn, whichField);
      break;
    case TpFloat:
      (*(ScalarColumn<float>*)(itsColumns[whichColumn])).put(rownr, record.asfloat(whichField));
      break;
    case TpArrayFloat:
      PutFieldArray<float>(rownr, record, whichColumn, whichField);
      break;
    case TpDouble:
      (*(ScalarColumn<double>*)(itsColumns[whichColumn])).put(rownr, record.asdouble(whichField));
      break;
    case TpArrayDouble:
      PutFieldArray<double>(rownr, record, whichColumn, whichField);
      break;
    case TpComplex:
      (*(ScalarColumn<Complex>*)(itsColumns[whichColumn])).put(rownr, record.asComplex(whichField));
      break;
    case TpArrayComplex:
      PutFieldArray<Complex>(rownr, record, whichColumn, whichField);
      break;
    case TpDComplex:
      (*(ScalarColumn<DComplex>*)(itsColumns[whichColumn]))
          .put(rownr, record.asDComplex(whichField));
      break;
    case TpArrayDComplex:
      PutFieldArray<DComplex>(rownr, record, whichColumn, whichField);
      break;
    case TpString:
      (*(ScalarColumn<String>*)(itsColumns[whichColumn])).put(rownr, record.asString(whichField));
      break;
    case TpArrayString:
      PutFieldArray<String>(rownr, record, whichColumn, whichField);
      break;
    case TpRecord:
      (*(ScalarColumn<TableRecord>*)(itsColumns[whichColumn]))
          .put(rownr, record.subRecord(whichField));
      break;
    default:
      throw(TableError("TableRow: unknown data type"));
  }
}

void ROTableRow::putRecord(rownr_t rownr) {
  const RecordDesc& desc = itsRecord->description();
  unsigned int nrfield = desc.nfields();
  for (unsigned int i = 0; i < nrfield; i++) {
    switch (desc.type(i)) {
      case TpBool:
        (*(ScalarColumn<bool>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<bool>*)itsFields[i]).get());
        break;
      case TpArrayBool:
        (*(ArrayColumn<bool>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<bool>>*)itsFields[i]).get());
        break;
      case TpUChar:
        (*(ScalarColumn<unsigned char>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<unsigned char>*)itsFields[i]).get());
        break;
      case TpArrayUChar:
        (*(ArrayColumn<unsigned char>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<unsigned char>>*)itsFields[i]).get());
        break;
      case TpShort:
        (*(ScalarColumn<short>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<short>*)itsFields[i]).get());
        break;
      case TpArrayShort:
        (*(ArrayColumn<short>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<short>>*)itsFields[i]).get());
        break;
      case TpInt:
        (*(ScalarColumn<int>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<int>*)itsFields[i]).get());
        break;
      case TpArrayInt:
        (*(ArrayColumn<int>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<int>>*)itsFields[i]).get());
        break;
      case TpUInt:
        (*(ScalarColumn<unsigned int>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<unsigned int>*)itsFields[i]).get());
        break;
      case TpArrayUInt:
        (*(ArrayColumn<unsigned int>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<unsigned int>>*)itsFields[i]).get());
        break;
      case TpInt64:
        (*(ScalarColumn<int64_t>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<int64_t>*)itsFields[i]).get());
        break;
      case TpArrayInt64:
        (*(ArrayColumn<int64_t>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<int64_t>>*)itsFields[i]).get());
        break;
      case TpFloat:
        (*(ScalarColumn<float>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<float>*)itsFields[i]).get());
        break;
      case TpArrayFloat:
        (*(ArrayColumn<float>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<float>>*)itsFields[i]).get());
        break;
      case TpDouble:
        (*(ScalarColumn<double>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<double>*)itsFields[i]).get());
        break;
      case TpArrayDouble:
        (*(ArrayColumn<double>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<double>>*)itsFields[i]).get());
        break;
      case TpComplex:
        (*(ScalarColumn<Complex>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Complex>*)itsFields[i]).get());
        break;
      case TpArrayComplex:
        (*(ArrayColumn<Complex>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<Complex>>*)itsFields[i]).get());
        break;
      case TpDComplex:
        (*(ScalarColumn<DComplex>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<DComplex>*)itsFields[i]).get());
        break;
      case TpArrayDComplex:
        (*(ArrayColumn<DComplex>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<DComplex>>*)itsFields[i]).get());
        break;
      case TpString:
        (*(ScalarColumn<String>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<String>*)itsFields[i]).get());
        break;
      case TpArrayString:
        (*(ArrayColumn<String>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<Array<String>>*)itsFields[i]).get());
        break;
      case TpRecord:
        (*(ScalarColumn<TableRecord>*)(itsColumns[i]))
            .put(rownr, (*(RecordFieldPtr<TableRecord>*)itsFields[i]).get());
        break;
      default:
        throw(TableError("TableRow: unknown data type"));
    }
  }
  // The values (might) have changed, which is not reflected in the
  // internal record. Be sure to reread when the same row is asked for.
  setReread(rownr);
}

TableRow::TableRow() : ROTableRow() {}

TableRow::TableRow(const Table& table, bool storedColumnsOnly) : ROTableRow() {
  if (!table.isWritable()) {
    throw(TableError("TableRow cannot be used: table is not writable"));
  }
  create(table, storedColumnsOnly, true);
}

TableRow::TableRow(const Table& table, const Vector<String>& columnNames, bool exclude)
    : ROTableRow() {
  if (!table.isWritable()) {
    throw(TableError("TableRow cannot be used: table is not writable"));
  }
  create(table, columnNames, exclude, true);
}

TableRow::TableRow(const TableRow& that) : ROTableRow() { copy(that); }

TableRow::~TableRow() {}

TableRow& TableRow::operator=(const TableRow& that) {
  copy(that);
  return *this;
}

void TableRow::putMatchingFields(rownr_t rownr, const TableRecord& record) {
  const RecordDesc& thisDesc = itsRecord->description();
  const RecordDesc& thatDesc = record.description();
  unsigned int nrfield = thatDesc.nfields();
  int field;
  for (unsigned int i = 0; i < nrfield; i++) {
    field = thisDesc.fieldNumber(thatDesc.name(i));
    if (field >= 0) {
      putField(rownr, record, field, i);
    }
  }
  // The values (might) have changed, which is not reflected in the
  // internal record. Be sure to reread when the same row is asked for.
  setReread(rownr);
}

void TableRow::put() {
  if (rowNumber() < 0) {
    throw(TableError("TableRow::put(): no row read yet"));
  }
  put(rowNumber());
}

void TableRow::put(rownr_t rownr, const TableRecord& record, bool checkConformance) {
  if (checkConformance) {
    if (!namesConform(record)) {
      throw(TableError("TableRow::put; names not conforming"));
    }
  }
  const RecordDesc& thisDesc = itsRecord->description();
  unsigned int nrfield = thisDesc.nfields();
  for (unsigned int i = 0; i < nrfield; i++) {
    putField(rownr, record, i, i);
  }
  // The values (might) have changed, which is not reflected in the
  // internal record. Be sure to reread when the same row is asked for.
  setReread(rownr);
}

void TableRow::put(rownr_t rownr, const TableRecord& record, const Block<bool>& valuesDefined,
                   bool checkConformance) {
  if (checkConformance) {
    if (!namesConform(record)) {
      throw(TableError("TableRow::put; names not conforming"));
    }
  }
  const RecordDesc& thisDesc = itsRecord->description();
  unsigned int nrfield = thisDesc.nfields();
  AlwaysAssert(valuesDefined.nelements() >= nrfield, AipsError);
  for (unsigned int i = 0; i < nrfield; i++) {
    if (valuesDefined[i]) {
      putField(rownr, record, i, i);
    }
  }
  // The values (might) have changed, which is not reflected in the
  // internal record. Be sure to reread when the same row is asked for.
  setReread(rownr);
}

bool TableRow::namesConform(const TableRecord& that) const {
  if (that.nfields() != itsNrused) {
    return false;
  }
  const RecordDesc& thisDesc = itsRecord->description();
  const RecordDesc& thatDesc = that.description();
  for (unsigned int i = 0; i < itsNrused; i++) {
    if (thisDesc.name(i) != thatDesc.name(i)) {
      return false;
    }
  }
  return true;
}

}  // namespace casacore
