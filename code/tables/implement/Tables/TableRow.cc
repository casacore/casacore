//# TableRow.cc: Access to a table row
//# Copyright (C) 1996,1997,1998
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
#include <aips/aips.h>
#include <aips/Tables/TableRow.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Containers/RecordField.h>
#include <aips/Arrays/Vector.h>
#include <aips/Tables/TableError.h>


ROTableRow::ROTableRow()
: itsRecord (0)
{
    init();
}

ROTableRow::ROTableRow (const Table& table, Bool storedColumnsOnly)
: itsRecord (0)
{
    init();
    create (table, storedColumnsOnly, False);
}

ROTableRow::ROTableRow (const Table& table, const Vector<String>& columnNames,
			Bool exclude)
: itsRecord (0)
{
    init();
    create (table, columnNames, exclude, False);
}

ROTableRow::ROTableRow (const ROTableRow& that)
: itsRecord (0)
{
    init();
    copy (that, False);
}

void ROTableRow::init()
{
    itsLastRow = new Int;
    *itsLastRow = -1;
    itsReread  = new Bool;
    *itsReread = True;
}

ROTableRow::~ROTableRow()
{
    delete itsLastRow;
    delete itsReread;
    deleteObjects();
}

ROTableRow& ROTableRow::operator= (const ROTableRow& that)
{
    copy (that, False);
    return *this;
}

void ROTableRow::copy (const ROTableRow& that, Bool writable)
{
    if (this != &that) {
	deleteObjects();
	itsTable    = that.itsTable;
	itsNrused   = that.itsNrused;
        *itsLastRow = *that.itsLastRow;
        *itsReread  = *that.itsReread;
	if (that.itsRecord != 0) {
	    makeObjects (that.itsRecord->description(), writable);
	}
    }
}

Vector<String> ROTableRow::columnNames() const
{
    const RecordDesc& desc = itsRecord->description();
    uInt nfield = desc.nfields();
    Vector<String> names(nfield);
    for (uInt i=0; i<nfield; i++) {
	names(i) = desc.name(i);
    }
    return names;
}

void ROTableRow::deleteObjects()
{
    if (itsRecord == 0) {
	return;
    }
    const RecordDesc& description = itsRecord->description();
    for (uInt i=0; i<itsNrused; i++) {
	delete (ROTableColumn*)(itsTabCols[i]);
	switch (description.type(i)) {
	case TpBool:
	    delete (ROScalarColumn<Bool>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Bool>*)(itsFields[i]);
	    break;
	case TpArrayBool:
	    delete (ROArrayColumn<Bool>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<Bool> >*)(itsFields[i]);
	    break;
	case TpUChar:
	    delete (ROScalarColumn<uChar>*)(itsColumns[i]);
	    delete (RecordFieldPtr<uChar>*)(itsFields[i]);
	    break;
	case TpArrayUChar:
	    delete (ROArrayColumn<uChar>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<uChar> >*)(itsFields[i]);
	    break;
	case TpShort:
	    delete (ROScalarColumn<Short>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Short>*)(itsFields[i]);
	    break;
	case TpArrayShort:
	    delete (ROArrayColumn<Short>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<Short> >*)(itsFields[i]);
	    break;
	case TpInt:
	    delete (ROScalarColumn<Int>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Int>*)(itsFields[i]);
	    break;
	case TpArrayInt:
	    delete (ROArrayColumn<Int>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<Int> >*)(itsFields[i]);
	    break;
	case TpUInt:
	    delete (ROScalarColumn<uInt>*)(itsColumns[i]);
	    delete (RecordFieldPtr<uInt>*)(itsFields[i]);
	    break;
	case TpArrayUInt:
	    delete (ROArrayColumn<uInt>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<uInt> >*)(itsFields[i]);
	    break;
	case TpFloat:
	    delete (ROScalarColumn<float>*)(itsColumns[i]);
	    delete (RecordFieldPtr<float>*)(itsFields[i]);
	    break;
	case TpArrayFloat:
	    delete (ROArrayColumn<float>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<float> >*)(itsFields[i]);
	    break;
	case TpDouble:
	    delete (ROScalarColumn<double>*)(itsColumns[i]);
	    delete (RecordFieldPtr<double>*)(itsFields[i]);
	    break;
	case TpArrayDouble:
	    delete (ROArrayColumn<double>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<double> >*)(itsFields[i]);
	    break;
	case TpComplex:
	    delete (ROScalarColumn<Complex>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Complex>*)(itsFields[i]);
	    break;
	case TpArrayComplex:
	    delete (ROArrayColumn<Complex>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<Complex> >*)(itsFields[i]);
	    break;
	case TpDComplex:
	    delete (ROScalarColumn<DComplex>*)(itsColumns[i]);
	    delete (RecordFieldPtr<DComplex>*)(itsFields[i]);
	    break;
	case TpArrayDComplex:
	    delete (ROArrayColumn<DComplex>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<DComplex> >*)(itsFields[i]);
	    break;
	case TpString:
	    delete (ROScalarColumn<String>*)(itsColumns[i]);
	    delete (RecordFieldPtr<String>*)(itsFields[i]);
	    break;
	case TpArrayString:
	    delete (ROArrayColumn<String>*)(itsColumns[i]);
	    delete (RecordFieldPtr<Array<String> >*)(itsFields[i]);
	    break;
	case TpRecord:
	    delete (ROScalarColumn<TableRecord>*)(itsColumns[i]);
	    delete (RecordFieldPtr<TableRecord>*)(itsFields[i]);
	    break;
	default:
	    throw (TableError ("TableRow: unknown data type"));
	}
	itsTabCols[i] = 0;
	itsColumns[i] = 0;
	itsFields[i]  = 0;
    }
    delete itsRecord;
    itsRecord = 0;
}

void ROTableRow::addColumnToDesc (RecordDesc& description,
				  const ROTableColumn& column,
				  Bool skipOther)
{
    const ColumnDesc& columnDesc = column.columnDesc();
    DataType dataType = columnDesc.dataType();
    if (! (skipOther  &&  dataType == TpOther)) {
	if (columnDesc.isArray()) {
	    IPosition shape = column.shapeColumn();
	    if (shape.nelements() == 0) {
		shape = IPosition(1,-1);
	    }
	    description.addField (columnDesc.name(), dataType, shape);
	}else{
	    description.addField (columnDesc.name(), dataType);
	}
	itsNrused++;
    }
}

void ROTableRow::create (const Table& table, Bool storedColumnsOnly,
			 Bool writable)
{
    itsTable = table;
    // Loop through all columns in the table.
    // Add it to the RecordDesc when the column is writable or
    // when we do not need to write and when the column is stored
    // or no stored columns are asked for..
    itsNrused = 0;
    RecordDesc description;
    uInt nrcol = itsTable.tableDesc().ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	if ((!storedColumnsOnly  ||  itsTable.isColumnStored (i))
	&&  (!writable  ||  itsTable.isColumnWritable (i))) {
	    addColumnToDesc (description, ROTableColumn (itsTable, i), True);
	}
    }
    makeObjects (description, writable);
}
	    
void ROTableRow::create (const Table& table, const Vector<String>& columnNames,
			 Bool exclude, Bool writable)
{
    itsTable = table;
    // Loop through all column names.
    // Always add it to the RecordDesc.
    itsNrused = 0;
    RecordDesc description;
    if (exclude) {
	makeDescExclude (description, columnNames, writable);
    }else{
	uInt nrcol = columnNames.nelements();
	for (uInt i=0; i<nrcol; i++) {
	    addColumnToDesc (description,
			     ROTableColumn (itsTable, columnNames(i)),
			     True);
	}
    }
    makeObjects (description, writable);
}
	    
void ROTableRow::makeDescExclude (RecordDesc& description,
				  const Vector<String>& columnNames,
				  Bool writable)
{
    // Loop through all columns in the table.
    // Add it to the RecordDesc when the column is writable or
    // when we do not need to write. Skip the columns to exclude.
    uInt nrcol  = itsTable.tableDesc().ncolumn();
    uInt nrexcl = columnNames.nelements();
    const TableDesc& tableDesc = itsTable.tableDesc();
    for (uInt i=0; i<nrcol; i++) {
	if (!writable  ||  itsTable.isColumnWritable (i)) {
	    const String& name = tableDesc[i].name();
	    Bool found = False;
	    for (uInt j=0; j<nrexcl; j++) {
		if (name == columnNames(j)) {
		    found = True;
		    break;
		}
	    }
	    if (!found) {
		addColumnToDesc (description, ROTableColumn (itsTable, i),
				 True);
	    }
	}
    }
}
	    

void ROTableRow::makeObjects (const RecordDesc& description,
			      Bool writable)
{
    // Create the TableRecord from the description.
    itsRecord = new TableRecord (description);
    // Initialize the column and field block.
    itsTabCols.resize (itsNrused, False, False);
    itsTabCols.set ((void*)0);
    itsColumns.resize (itsNrused, False, False);
    itsColumns.set ((void*)0);
    itsFields.resize (itsNrused, False, False);
    itsFields.set ((void*)0);
    // Create the correct column object for each field.
    // (if not writing, an RO version is sufficient).
    // Also create a RecordFieldPtr object for each column.
    // This makes a fast data copy possible.
    uInt nrfield = description.nfields();
    for (uInt i=0; i<nrfield; i++) {
	const String& name = description.name(i);
	ROTableColumn* tabColPtr = new ROTableColumn (itsTable, name);
	itsTabCols[i] = tabColPtr;
	switch (description.type(i)) {
	case TpBool:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<Bool> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<Bool> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Bool>(*itsRecord, i);
	    break;
	case TpUChar:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<uChar> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<uChar> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<uChar>(*itsRecord, i);
	    break;
	case TpShort:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<Short> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<Short> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Short>(*itsRecord, i);
	    break;
	case TpInt:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<Int> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<Int> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Int>(*itsRecord, i);
	    break;
	case TpUInt:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<uInt> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<uInt> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<uInt>(*itsRecord, i);
	    break;
	case TpFloat:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<float> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<float> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<float>(*itsRecord, i);
	    break;
	case TpDouble:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<double> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<double> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<double>(*itsRecord, i);
	    break;
	case TpComplex:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<Complex> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<Complex> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Complex>(*itsRecord, i);
	    break;
	case TpDComplex:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<DComplex> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<DComplex> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<DComplex>(*itsRecord, i);
	    break;
	case TpString:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<String> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<String> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<String>(*itsRecord, i);
	    break;
	case TpRecord:
	    if (writable) {
		itsColumns[i] = new ScalarColumn<TableRecord> (itsTable, name);
	    }else{
		itsColumns[i] = new ROScalarColumn<TableRecord>(itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<TableRecord>(*itsRecord, i);
	    break;
	case TpArrayBool:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<Bool> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<Bool> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<Bool> >(*itsRecord, i);
	    break;
	case TpArrayUChar:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<uChar> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<uChar> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<uChar> >(*itsRecord, i);
	    break;
	case TpArrayShort:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<Short> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<Short> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<Short> >(*itsRecord, i);
	    break;
	case TpArrayInt:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<Int> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<Int> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<Int> >(*itsRecord, i);
	    break;
	case TpArrayUInt:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<uInt> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<uInt> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<uInt> >(*itsRecord, i);
	    break;
	case TpArrayFloat:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<float> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<float> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<float> >(*itsRecord, i);
	    break;
	case TpArrayDouble:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<double> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<double> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<double> >(*itsRecord, i);
	    break;
	case TpArrayComplex:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<Complex> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<Complex> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<Complex> >(*itsRecord, i);
	    break;
	case TpArrayDComplex:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<DComplex> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<DComplex> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<DComplex> >(*itsRecord, i);
	    break;
	case TpArrayString:
	    if (writable) {
		itsColumns[i] = new ArrayColumn<String> (itsTable, name);
	    }else{
		itsColumns[i] = new ROArrayColumn<String> (itsTable, name);
	    }
	    itsFields[i] = new RecordFieldPtr<Array<String> >(*itsRecord, i);
	    break;
	default:
	    throw (TableError ("TableRow: cannot handle Table "
			       "and TableRecord yet"));
	}
    }
}

const TableRecord& ROTableRow::get (uInt rownr, Bool alwaysRead) const
{
    // Only read when needed.
    if (Int(rownr) == *itsLastRow  &&  !itsReread  &&  !alwaysRead) {
	return *itsRecord;
    }
    const RecordDesc& desc = itsRecord->description();
    Int ndim = 0;
    uInt nrfield = desc.nfields();
    for (uInt i=0; i<nrfield; i++) {
	//# First determine if an array value is defined.
	//# If not, get its default dimensionality.
	Bool isDefined = True;
	if (! (*(ROTableColumn*)(itsTabCols[i])).isDefined (rownr)) {
	    isDefined = False;
	    ndim = (*(ROTableColumn*)(itsTabCols[i])).columnDesc().ndim();
	    if (ndim < 0) {
		ndim = 0;
	    }
	}
	//# Now get the value; if undefined, create zero-length array
	//# with the correct dimensionality.
	switch (desc.type(i)) {
	case TpBool:
	    (*(const ROScalarColumn<Bool>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<Bool>*) itsFields[i]));
	    break;
	case TpArrayBool:
	    if (isDefined) {
		(*(const ROArrayColumn<Bool>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<Bool> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<Bool> >*)(itsFields[i])).define (
		                         Array<Bool> (IPosition(ndim, 0)));
	    }
	    break;
	case TpUChar:
	    (*(const ROScalarColumn<uChar>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<uChar>*) itsFields[i]));
	    break;
	case TpArrayUChar:
	    if (isDefined) {
		(*(const ROArrayColumn<uChar>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<uChar> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<uChar> >*)(itsFields[i])).define (
		                         Array<uChar> (IPosition(ndim, 0)));
	    }
	    break;
	case TpShort:
	    (*(const ROScalarColumn<Short>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<Short>*) itsFields[i]));
	    break;
	case TpArrayShort:
	    if (isDefined) {
		(*(const ROArrayColumn<Short>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<Short> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<Short> >*)(itsFields[i])).define (
		                         Array<Short> (IPosition(ndim, 0)));
	    }
	    break;
	case TpInt:
	    (*(const ROScalarColumn<Int>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<Int>*) itsFields[i]));
	    break;
	case TpArrayInt:
	    if (isDefined) {
		(*(const ROArrayColumn<Int>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<Int> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<Int> >*)(itsFields[i])).define (
		                         Array<Int> (IPosition(ndim, 0)));
	    }
	    break;
	case TpUInt:
	    (*(const ROScalarColumn<uInt>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<uInt>*) itsFields[i]));
	    break;
	case TpArrayUInt:
	    if (isDefined) {
		(*(const ROArrayColumn<uInt>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<uInt> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<uInt> >*)(itsFields[i])).define (
		                         Array<uInt> (IPosition(ndim, 0)));
	    }
	    break;
	case TpFloat:
	    (*(const ROScalarColumn<float>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<float>*) itsFields[i]));
	    break;
	case TpArrayFloat:
	    if (isDefined) {
		(*(const ROArrayColumn<float>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<float> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<float> >*)(itsFields[i])).define (
		                         Array<float> (IPosition(ndim, 0)));
	    }
	    break;
	case TpDouble:
	    (*(const ROScalarColumn<double>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<double>*) itsFields[i]));
	    break;
	case TpArrayDouble:
	    if (isDefined) {
		(*(const ROArrayColumn<double>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<double> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<double> >*)(itsFields[i])).define (
		                         Array<double> (IPosition(ndim, 0)));
	    }
	    break;
	case TpComplex:
	    (*(const ROScalarColumn<Complex>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<Complex>*) itsFields[i]));
	    break;
	case TpArrayComplex:
	    if (isDefined) {
		(*(const ROArrayColumn<Complex>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<Complex> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<Complex> >*)(itsFields[i])).define (
		                         Array<Complex> (IPosition(ndim, 0)));
	    }
	    break;
	case TpDComplex:
	    (*(const ROScalarColumn<DComplex>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<DComplex>*) itsFields[i]));
	    break;
	case TpArrayDComplex:
	    if (isDefined) {
		(*(const ROArrayColumn<DComplex>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<DComplex> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<DComplex> >*)(itsFields[i])).define (
		                         Array<DComplex> (IPosition(ndim, 0)));
	    }
	    break;
	case TpString:
	    (*(const ROScalarColumn<String>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<String>*) itsFields[i]));
	    break;
	case TpArrayString:
	    if (isDefined) {
		(*(const ROArrayColumn<String>*)(itsColumns[i])).get (
		       rownr,
		       *(*(RecordFieldPtr<Array<String> >*) itsFields[i]),
		       True);
	    }else{
		(*(RecordFieldPtr<Array<String> >*)(itsFields[i])).define (
		                         Array<String> (IPosition(ndim, 0)));
	    }
	    break;
	case TpRecord:
	    (*(const ROScalarColumn<TableRecord>*)(itsColumns[i])).get (
		       rownr, *(*(RecordFieldPtr<TableRecord>*) itsFields[i]));
	    break;
	default:
	    throw (TableError ("TableRow: unknown data type"));
	}
    }
    *itsLastRow = rownr;
    *itsReread  = False;
    return *itsRecord;
}

// The values (might) have changed, which is not reflected in the
// internal record. Be sure to reread when the same row is asked for.
void ROTableRow::setReread (uInt rownr)
{
    if (Int(rownr) == *itsLastRow) {
	*itsReread = True;
    }
}

void ROTableRow::putField (uInt rownr, const TableRecord& record,
			   Int whichColumn, Int whichField)
{
    switch (itsRecord->description().type(whichColumn)) {
    case TpBool:
	(*(ScalarColumn<Bool>*)(itsColumns[whichColumn])).put
		                (rownr, record.asBool (whichField));
	break;
    case TpArrayBool:
	(*(ArrayColumn<Bool>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayBool (whichField));
	break;
    case TpUChar:
	(*(ScalarColumn<uChar>*)(itsColumns[whichColumn])).put
		                (rownr, record.asuChar (whichField));
	break;
    case TpArrayUChar:
	(*(ArrayColumn<uChar>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayuChar (whichField));
	break;
    case TpShort:
	(*(ScalarColumn<Short>*)(itsColumns[whichColumn])).put
		                (rownr, record.asShort (whichField));
	break;
    case TpArrayShort:
	(*(ArrayColumn<Short>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayShort (whichField));
	break;
    case TpInt:
	(*(ScalarColumn<Int>*)(itsColumns[whichColumn])).put
		                (rownr, record.asInt (whichField));
	break;
    case TpArrayInt:
	(*(ArrayColumn<Int>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayInt (whichField));
	break;
    case TpUInt:
	(*(ScalarColumn<uInt>*)(itsColumns[whichColumn])).put
		                (rownr, record.asuInt (whichField));
	break;
    case TpArrayUInt:
	(*(ArrayColumn<uInt>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayuInt (whichField));
	break;
    case TpFloat:
	(*(ScalarColumn<Float>*)(itsColumns[whichColumn])).put
		                (rownr, record.asfloat (whichField));
	break;
    case TpArrayFloat:
	(*(ArrayColumn<Float>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayfloat (whichField));
	break;
    case TpDouble:
	(*(ScalarColumn<Double>*)(itsColumns[whichColumn])).put
		                (rownr, record.asdouble (whichField));
	break;
    case TpArrayDouble:
	(*(ArrayColumn<Double>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArraydouble (whichField));
	break;
    case TpComplex:
	(*(ScalarColumn<Complex>*)(itsColumns[whichColumn])).put
		                (rownr, record.asComplex (whichField));
	break;
    case TpArrayComplex:
	(*(ArrayColumn<Complex>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayComplex (whichField));
	break;
    case TpDComplex:
	(*(ScalarColumn<DComplex>*)(itsColumns[whichColumn])).put
		                (rownr, record.asDComplex (whichField));
	break;
    case TpArrayDComplex:
	(*(ArrayColumn<DComplex>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayDComplex (whichField));
	break;
    case TpString:
	(*(ScalarColumn<String>*)(itsColumns[whichColumn])).put
		                (rownr, record.asString (whichField));
	break;
    case TpArrayString:
	(*(ArrayColumn<String>*)(itsColumns[whichColumn])).put
	                        (rownr, record.asArrayString (whichField));
	break;
    case TpRecord:
	(*(ScalarColumn<TableRecord>*)(itsColumns[whichColumn])).put
	                        (rownr, record.subRecord (whichField));
	break;
    default:
	throw (TableError ("TableRow: unknown data type"));
    }
}

void ROTableRow::putRecord (uInt rownr)
{
    const RecordDesc& desc = itsRecord->description();
    uInt nrfield = desc.nfields();
    for (uInt i=0; i<nrfield; i++) {
	switch (desc.type(i)) {
	case TpBool:
	    (*(ScalarColumn<Bool>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Bool>*) itsFields[i]).get());
	    break;
	case TpArrayBool:
	    (*(ArrayColumn<Bool>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<Bool> >*) itsFields[i]).get());
	    break;
	case TpUChar:
	    (*(ScalarColumn<uChar>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<uChar>*) itsFields[i]).get());
	    break;
	case TpArrayUChar:
	    (*(ArrayColumn<uChar>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<uChar> >*) itsFields[i]).get());
	    break;
	case TpShort:
	    (*(ScalarColumn<Short>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Short>*) itsFields[i]).get());
	    break;
	case TpArrayShort:
	    (*(ArrayColumn<Short>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<Short> >*) itsFields[i]).get());
	    break;
	case TpInt:
	    (*(ScalarColumn<Int>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Int>*) itsFields[i]).get());
	    break;
	case TpArrayInt:
	    (*(ArrayColumn<Int>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<Int> >*) itsFields[i]).get());
	    break;
	case TpUInt:
	    (*(ScalarColumn<uInt>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<uInt>*) itsFields[i]).get());
	    break;
	case TpArrayUInt:
	    (*(ArrayColumn<uInt>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<uInt> >*) itsFields[i]).get());
	    break;
	case TpFloat:
	    (*(ScalarColumn<float>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<float>*) itsFields[i]).get());
	    break;
	case TpArrayFloat:
	    (*(ArrayColumn<float>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<float> >*) itsFields[i]).get());
	    break;
	case TpDouble:
	    (*(ScalarColumn<double>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<double>*) itsFields[i]).get());
	    break;
	case TpArrayDouble:
	    (*(ArrayColumn<double>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<double> >*) itsFields[i]).get());
	    break;
	case TpComplex:
	    (*(ScalarColumn<Complex>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Complex>*) itsFields[i]).get());
	    break;
	case TpArrayComplex:
	    (*(ArrayColumn<Complex>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<Complex> >*) itsFields[i]).get());
	    break;
	case TpDComplex:
	    (*(ScalarColumn<DComplex>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<DComplex>*) itsFields[i]).get());
	    break;
	case TpArrayDComplex:
	    (*(ArrayColumn<DComplex>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<DComplex> >*) itsFields[i]).get());
	    break;
	case TpString:
	    (*(ScalarColumn<String>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<String>*) itsFields[i]).get());
	    break;
	case TpArrayString:
	    (*(ArrayColumn<String>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<Array<String> >*) itsFields[i]).get());
	    break;
	case TpRecord:
	    (*(ScalarColumn<TableRecord>*)(itsColumns[i])).put (rownr,
	         (*(RecordFieldPtr<TableRecord>*) itsFields[i]).get());
	    break;
	default:
	    throw (TableError ("TableRow: unknown data type"));
	}
    }
    // The values (might) have changed, which is not reflected in the
    // internal record. Be sure to reread when the same row is asked for.
    setReread (rownr);
}



TableRow::TableRow()
: ROTableRow()
{}

TableRow::TableRow (const Table& table, Bool storedColumnsOnly)
: ROTableRow()
{
    if (! table.isWritable()) {
	throw (TableError ("TableRow cannot be used: table is not writable"));
    }
    create (table, storedColumnsOnly, True);
}

TableRow::TableRow (const Table& table, const Vector<String>& columnNames,
		    Bool exclude)
: ROTableRow()
{
    if (! table.isWritable()) {
	throw (TableError ("TableRow cannot be used: table is not writable"));
    }
    create (table, columnNames, exclude, True);
}

TableRow::TableRow (const TableRow& that)
: ROTableRow()
{
    copy (that, True);
}

TableRow::~TableRow()
{}

TableRow& TableRow::operator= (const TableRow& that)
{
    copy (that, True);
    return *this;
}

void TableRow::putMatchingFields (uInt rownr, const TableRecord& record)
{
    const RecordDesc& thisDesc = itsRecord->description();
    const RecordDesc& thatDesc = record.description();
    uInt nrfield = thatDesc.nfields();
    Int field;
    for (uInt i=0; i<nrfield; i++) {
	field = thisDesc.fieldNumber (thatDesc.name(i));
	if (field >= 0) {
	    putField (rownr, record, field, i);
	}
    }
    // The values (might) have changed, which is not reflected in the
    // internal record. Be sure to reread when the same row is asked for.
    setReread (rownr);
}

void TableRow::put()
{
    if (rowNumber() < 0) {
	throw (TableError ("TableRow::put(): no row read yet"));
    }
    put (rowNumber());
}

void TableRow::put (uInt rownr, const TableRecord& record,
		    Bool checkConformance)
{
    if (checkConformance) {
	if (! namesConform (record)) {
	    throw (TableError ("TableRow::put; names not conforming"));
	}
    }
    const RecordDesc& thisDesc = itsRecord->description();
    uInt nrfield = thisDesc.nfields();
    for (uInt i=0; i<nrfield; i++) {
	putField (rownr, record, i, i);
    }
    // The values (might) have changed, which is not reflected in the
    // internal record. Be sure to reread when the same row is asked for.
    setReread (rownr);
}
    
Bool TableRow::namesConform (const TableRecord& that) const
{
    if (that.nfields() != itsNrused) {
	return False;
    }
    const RecordDesc& thisDesc = itsRecord->description();
    const RecordDesc& thatDesc = that.description();
    for (uInt i=0; i<itsNrused; i++) {
	if (thisDesc.name(i) != thatDesc.name(i)) {
	    return False;
	}
    }
    return True;
}


void copyRows (Table& out, const Table& in, uInt startout, uInt startin,
	       uInt nrrow)
{
    uInt i;
    ROTableRow inrow(in, True);
    Vector<String> columns = inrow.columnNames();
    for (i=0; i<columns.nelements(); i++) {
	if (! out.tableDesc().isColumn (columns(i))) {
	    throw (TableError ("copyTable: column " + columns(i) +
			       " does not exist in output table"));
	}
    }
    if (startout + nrrow > out.nrow()) {
	out.addRow (startout + nrrow - out.nrow());
    }
    TableRow outrow(out, columns);
    for (i=0; i<nrrow; i++) {
	inrow.get (startin + i);
	outrow.put (startout + i, inrow.record(), False);
    }
}
