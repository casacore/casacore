//# ArrayColumn.cc: Access to an array table column with arbitrary data type
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/ValTypeId.h>
#include <aips/Tables/TableError.h>


template<class T>
ROArrayColumn<T>::ROArrayColumn()
: ROTableColumn (),
  reaskAccessSlice_p       (new Bool(True)),
  reaskAccessColumn_p      (new Bool(True)),
  reaskAccessColumnSlice_p (new Bool(True)),
  canAccessSlice_p         (new Bool(False)),
  canAccessColumn_p        (new Bool(False)),
  canAccessColumnSlice_p   (new Bool(False)),
  canChangeShape_p         (False)
{}

template<class T>
ROArrayColumn<T>::ROArrayColumn (const Table& tab,
				 const String& columnName)
: ROTableColumn (tab, columnName),
  reaskAccessSlice_p       (new Bool(True)),
  reaskAccessColumn_p      (new Bool(True)),
  reaskAccessColumnSlice_p (new Bool(True)),
  canAccessSlice_p         (new Bool(False)),
  canAccessColumn_p        (new Bool(False)),
  canAccessColumnSlice_p   (new Bool(False))
{
    checkDataType();
    canChangeShape_p = baseColPtr_p->canChangeShape();
}

template<class T>
ROArrayColumn<T>::ROArrayColumn (const ROTableColumn& column)
: ROTableColumn (column),
  reaskAccessSlice_p       (new Bool(True)),
  reaskAccessColumn_p      (new Bool(True)),
  reaskAccessColumnSlice_p (new Bool(True)),
  canAccessSlice_p         (new Bool(False)),
  canAccessColumn_p        (new Bool(False)),
  canAccessColumnSlice_p   (new Bool(False))
{
    checkDataType();
    canChangeShape_p = baseColPtr_p->canChangeShape();
}

template<class T>
ROArrayColumn<T>::ROArrayColumn (const ROArrayColumn<T>& that)
: ROTableColumn (that),
  reaskAccessSlice_p       (new Bool (*that.reaskAccessSlice_p)),
  reaskAccessColumn_p      (new Bool (*that.reaskAccessColumn_p)),
  reaskAccessColumnSlice_p (new Bool (*that.reaskAccessColumnSlice_p)),
  canAccessSlice_p         (new Bool (*that.canAccessSlice_p)),
  canAccessColumn_p        (new Bool (*that.canAccessColumn_p)),
  canAccessColumnSlice_p   (new Bool (*that.canAccessColumnSlice_p)),
  canChangeShape_p         (that.canChangeShape_p)
{}

template<class T>
ROTableColumn* ROArrayColumn<T>::clone() const
{
    return new ROArrayColumn<T> (*this);
}

template<class T>
void ROArrayColumn<T>::reference (const ROArrayColumn<T>& that)
{
    ROTableColumn::reference (that);
    *reaskAccessSlice_p       = *that.reaskAccessSlice_p;
    *reaskAccessColumn_p      = *that.reaskAccessColumn_p;
    *reaskAccessColumnSlice_p = *that.reaskAccessColumnSlice_p;
    *canAccessSlice_p         = *that.canAccessSlice_p;
    *canAccessColumn_p        = *that.canAccessColumn_p;
    *canAccessColumnSlice_p   = *that.canAccessColumnSlice_p;
    canChangeShape_p          = that.canChangeShape_p;
}

template<class T>
ROArrayColumn<T>::~ROArrayColumn()
{
    delete reaskAccessSlice_p;
    delete reaskAccessColumn_p;
    delete reaskAccessColumnSlice_p;
    delete canAccessSlice_p;
    delete canAccessColumn_p;
    delete canAccessColumnSlice_p;
}


template<class T>
void ROArrayColumn<T>::checkDataType() const
{
    //# Check if the data type matches.
    const ColumnDesc& cd = baseColPtr_p->columnDesc();
    DataType dtype = cd.dataType();
    if (dtype != ValType::getType((T*)0)  ||  !cd.isArray()) {
	throw (TableInvDT (" in ROArrayColumn ctor for column " + cd.name()));
    }
    if (dtype == TpOther) {
	if (cd.dataTypeId() != valDataTypeId((T*)0)) {
	    throw (TableInvDT (" in ROArrayColumn ctor for column "
			       + cd.name() + "; using data type id "
			       + valDataTypeId((T*)0)
			       + ", expected " + cd.dataTypeId()));
	}
    }
}


template<class T>
Array<T> ROArrayColumn<T>::operator() (uInt rownr) const
{
    Array<T> arr;
    get (rownr, arr);
    return arr;
}


template<class T>
void ROArrayColumn<T>::get (uInt rownr, Array<T>& arr, Bool resize) const
{
    TABLECOLUMNCHECKROW(rownr); 
    //# Check the array conformance.
    //# Resize the array if empty or if resize flag is set.
    IPosition shp = shape(rownr);
    if (! shp.isEqual (arr.shape())) {
	if (resize  ||  arr.nelements() == 0) {
	    arr.resize (shp);
	}else{
	    throw (TableArrayConformanceError("ArrayColumn::get"));
	}
    }
    baseColPtr_p->get (rownr, &arr);
}

template<class T>
Array<T> ROArrayColumn<T>::getSlice (uInt rownr, const Slicer& ns) const
{
    Array<T> arr;
    getSlice (rownr, ns, arr);
    return arr;
}

template<class T>
void ROArrayColumn<T>::getSlice (uInt rownr, const Slicer& ns,
				 Array<T>& arr, Bool resize) const
{
    TABLECOLUMNCHECKROW(rownr);
    //# Check the array conformance.
    //# Extend the array if empty.
    IPosition arrayShape (shape(rownr));
    IPosition blc,trc,inc;
    IPosition shp = ns.inferShapeFromSource (arrayShape, blc,trc,inc);
    if (! shp.isEqual (arr.shape())) {
	if (resize  ||  arr.nelements() == 0) {
	    arr.resize (shp);
	}else{
	    throw (TableArrayConformanceError("ArrayColumn::getSlice"));
	}
    }
    //# Ask if we can access the slice (if that is not known yet).
    if (*reaskAccessSlice_p) {
	*canAccessSlice_p = baseColPtr_p->canAccessSlice (*reaskAccessSlice_p);
    }
    //# Access the slice if possible.
    //# Otherwise get the entire array and slice it.
    if (*canAccessSlice_p) {
	baseColPtr_p->getSlice (rownr, ns, &arr);
    }else{
	Array<T> array(arrayShape);
	baseColPtr_p->get (rownr, &array);
	arr = array(blc, trc, inc);
    }
}

template<class T>
Array<T> ROArrayColumn<T>::getColumn() const
{
    Array<T> arr;
    getColumn (arr);
    return arr;
}

template<class T>
void ROArrayColumn<T>::getColumn (Array<T>& arr, Bool resize) const
{
    uInt nrrow = nrow();
    //# Take shape of array in first row.
    IPosition shp;
    if (nrrow > 0) {
	shp = shape(0);
    }
    //# Total shape is array shape plus nr of table rows.
    shp.resize (shp.nelements() + 1);
    shp(shp.nelements()-1) = nrrow;
    if (! shp.isEqual (arr.shape())) {
	if (resize  ||  arr.nelements() == 0) {
	    arr.resize (shp);
	}else{
	    throw (TableArrayConformanceError("ArrayColumn::getColumn"));
	}
    }
    //# Ask if we can access the column (if that is not known yet).
    if (*reaskAccessColumn_p) {
	*canAccessColumn_p = baseColPtr_p->canAccessArrayColumn
	                                           (*reaskAccessColumn_p);
    }
    //# Access the column if possible.
    //# Otherwise fill the entire array by looping through all cells.
    if (*canAccessColumn_p) {
	baseColPtr_p->getArrayColumn (&arr);
    }else{
	ArrayIterator<T> iter(arr, arr.ndim()-1);
	for (uInt rownr=0; rownr<nrrow; rownr++) {
	    baseColPtr_p->get (rownr, &(iter.array()));
	    iter.next();
	}
    }
}

template<class T>
Array<T> ROArrayColumn<T>::getColumn (const Slicer& ns) const
{
    Array<T> arr;
    getColumn (ns, arr);
    return arr;
}

template<class T>
void ROArrayColumn<T>::getColumn (const Slicer& ns,
				  Array<T>& arr, Bool resize) const
{
    uInt nrrow = nrow();
    //# Use shape of array in first row.
    IPosition shp, blc,trc,inc;
    if (nrrow > 0) {
	shp = ns.inferShapeFromSource (shape(0), blc,trc,inc);
    }
    //# Total shape is slice shape plus nr of table rows.
    shp.resize (shp.nelements() + 1);
    shp(shp.nelements()-1) = nrrow;
    if (! shp.isEqual (arr.shape())) {
	if (resize  ||  arr.nelements() == 0) {
	    arr.resize (shp);
	}else{
	    throw (TableArrayConformanceError("ArrayColumn::getColumn"));
	}
    }
    //# Ask if we can access the column slice (if that is not known yet).
    if (*reaskAccessColumnSlice_p) {
	*canAccessColumnSlice_p = baseColPtr_p->canAccessColumnSlice
	                                       (*reaskAccessColumnSlice_p);
    }
    //# Access the column slice if possible.
    //# Otherwise fill the entire array by looping through all cells.
    if (*canAccessColumnSlice_p) {
	baseColPtr_p->getColumnSlice (ns, &arr);
    }else{
	ArrayIterator<T> iter(arr, arr.ndim()-1);
	for (uInt rownr=0; rownr<nrrow; rownr++) {
	    getSlice (rownr, ns, iter.array());
	    iter.next();
	}
    }
}


template<class T>
Array<T> ROArrayColumn<T>::getColumnRange (const Slicer& rowRange) const
{
    Array<T> arr;
    getColumnRange (rowRange, arr);
    return arr;
}

template<class T>
void ROArrayColumn<T>::getColumnRange (const Slicer& rowRange,
				       Array<T>& arr, Bool resize) const
{
    uInt nrrow = nrow();
    IPosition shp, blc, trc, inc;
    shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
    //# When the entire column is accessed, use that function.
    if (shp(0) == nrrow) {
	getColumn (arr, resize);
	return;
    }
    //# Take shape of array in first row.
    nrrow = shp(0);
    IPosition arrshp;
    if (nrrow > 0) {
	arrshp = shape(blc(0));
    }
    //# Total shape is array shape plus nr of table rows.
    arrshp.resize (arrshp.nelements() + 1);
    arrshp(arrshp.nelements()-1) = nrrow;
    if (! arrshp.isEqual (arr.shape())) {
	if (resize  ||  arr.nelements() == 0) {
	    arr.resize (arrshp);
	}else{
	    throw (TableArrayConformanceError("ArrayColumn::getColumnRange"));
	}
    }
    //# Fill the entire array by looping through all cells.
    uInt rownr = blc(0);
    uInt incr = inc(0);
    ArrayIterator<T> iter(arr, arr.ndim()-1);
    for (uInt i=0; i<nrrow; i++) {
	baseColPtr_p->get (rownr, &(iter.array()));
	rownr += incr;
	iter.next();
    }
}




template<class T>
ArrayColumn<T>::ArrayColumn()
: ROTableColumn    (),
  ROArrayColumn<T> (),
  TableColumn      ()
{}

template<class T>
ArrayColumn<T>::ArrayColumn (const Table& tab, const String& columnName)
: ROTableColumn    (tab, columnName),
  ROArrayColumn<T> (tab, columnName),
  TableColumn      (tab, columnName)
{}

template<class T>
ArrayColumn<T>::ArrayColumn (const TableColumn& column)
: ROTableColumn    (column),
  ROArrayColumn<T> (column),
  TableColumn      (column)
{}

template<class T>
ArrayColumn<T>::ArrayColumn (const ArrayColumn<T>& that)
: ROTableColumn    (that),
  ROArrayColumn<T> (that),
  TableColumn      (that)
{}

template<class T>
ROTableColumn* ArrayColumn<T>::clone() const
{
    return new ArrayColumn<T> (*this);
}

template<class T>
void ArrayColumn<T>::reference (const ArrayColumn<T>& that)
    { ROArrayColumn<T>::reference (that); }

template<class T>
ArrayColumn<T>::~ArrayColumn()
{}


template<class T>
void ArrayColumn<T>::setShape (uInt rownr, const IPosition& shape)
{
    TABLECOLUMNCHECKROW(rownr); 
    //# Set shape if not defined yet or if changed (if possible).
    //# Throw exception if already defined with a different shape.
    if (canChangeShape_p  ||  !isDefined(rownr)) {
	baseColPtr_p->setShape (rownr, shape);
    }else{
	if (! shape.isEqual (baseColPtr_p->shape (rownr))) {
	    throw (TableInvOper
		   ("ArrayColumn::setShape; shape cannot be changed"));
	}
    }
}
	
template<class T>
void ArrayColumn<T>::setShape (uInt rownr, const IPosition& shape,
			       const IPosition& tileShape)
{
    TABLECOLUMNCHECKROW(rownr); 
    //# Only set shape if not defined yet.
    //# Throw exception if already defined with a different shape.
    if (canChangeShape_p  ||  !isDefined(rownr)) {
	baseColPtr_p->setShape (rownr, shape, tileShape);
    }else{
	if (! shape.isEqual (baseColPtr_p->shape (rownr))) {
	    throw (TableInvOper
		   ("ArrayColumn::setShape; shape cannot be changed"));
	}
    }
}
	
template<class T>
void ArrayColumn<T>::put (uInt rownr, const Array<T>& arr)
{
    TABLECOLUMNCHECKROW(rownr); 
    //# Define the shape if not defined yet.
    //# If defined, check if shape conforms.
    if (!isDefined(rownr)) {
	setShape (rownr, arr.shape());
    }else{
	if (! arr.shape().isEqual (baseColPtr_p->shape (rownr))) {
	    if (!canChangeShape_p) {
		throw (TableArrayConformanceError("ArrayColumn::put"));
	    }
	    setShape (rownr, arr.shape());
	}
    }
    baseColPtr_p->put (rownr, &arr);
}

template<class T>
void ArrayColumn<T>::putSlice (uInt rownr, const Slicer& ns,
			       const Array<T>& arr)
{
    TABLECOLUMNCHECKROW(rownr); 
    //# Check the array conformance.
    IPosition arrayShape (shape(rownr));
    IPosition blc,trc,inc;
    IPosition shp = ns.inferShapeFromSource (arrayShape, blc,trc,inc);
    if (! shp.isEqual (arr.shape())) {
	throw (TableArrayConformanceError("ArrayColumn::putSlice"));
    }
    //# Ask if we can access the slice (if that is not known yet).
    if (*reaskAccessSlice_p) {
	*canAccessSlice_p = baseColPtr_p->canAccessSlice (*reaskAccessSlice_p);
    }
    //# Access the slice if possible.
    //# Otherwise get the entire array, put the slice and put it back.
    if (*canAccessSlice_p) {
	baseColPtr_p->putSlice (rownr, ns, &arr);
    }else{
	Array<T> array(arrayShape);
	baseColPtr_p->get (rownr, &array);
	array(blc, trc, inc) = arr;
	baseColPtr_p->put (rownr, &array);
    }
}

template<class T>
void ArrayColumn<T>::put (uInt thisRownr, const ROTableColumn& that,
			  uInt thatRownr)
{
    TableColumn::put (thisRownr, that, thatRownr);
}

template<class T>
void ArrayColumn<T>::putColumn (const Array<T>& arr)
{
    //# First check if number of rows matches.
    uInt nrrow = nrow();
    IPosition shp  = arr.shape();
    uInt last = shp.nelements() - 1;
    if (shp(last) != nrrow) {
	throw (TableArrayConformanceError("ArrayColumn::putColumn (nrrow)"));
    }
    //# Remove #rows from shape to get the shape of each cell.
    shp.resize (last);
    //# When the array is fixed shape, check if the shape matches.
    if ((columnDesc().options() & ColumnDesc::FixedShape)
	                                     == ColumnDesc::FixedShape) {
	if (! shp.isEqual (shapeColumn())) {
	    throw (TableArrayConformanceError("ArrayColumn::putColumn"));
	}
    }else{
	//# Otherwise set the shape of each cell (as far as needed).
	for (uInt i=0; i<nrrow; i++) {
	    setShape (i, shp);
	}
    }
    //# Ask if we can access the column (if that is not known yet).
    if (*reaskAccessColumn_p) {
	*canAccessColumn_p = baseColPtr_p->canAccessArrayColumn
	                                             (*reaskAccessColumn_p);
    }
    //# Access the column if possible.
    //# Otherwise put the entire array by looping through all cells.
    if (*canAccessColumn_p) {
	baseColPtr_p->putArrayColumn (&arr);
    }else{
	ReadOnlyArrayIterator<T> iter(arr, arr.ndim()-1);
	for (uInt rownr=0; rownr<nrrow; rownr++) {
	    baseColPtr_p->put (rownr, &(iter.array()));
	    iter.next();
	}
    }
}

template<class T>
void ArrayColumn<T>::putColumn (const Slicer& ns, const Array<T>& arr)
{
    uInt nrrow = nrow();
    //# When the array is fixed shape, check if the shape matches.
    if ((columnDesc().options() & ColumnDesc::FixedShape)
	                                     == ColumnDesc::FixedShape) {
	IPosition blc,trc,inc;
	IPosition shp = ns.inferShapeFromSource (shapeColumn(), blc,trc,inc);
	//# Total shape is slice shape plus nr of table rows.
	shp.resize (shp.nelements() + 1);
	shp(shp.nelements()-1) = nrrow;
	if (! shp.isEqual(arr.shape())) {
	    throw (TableArrayConformanceError("ArrayColumn::putColumn"));
	}
    }
    //# Ask if we can access the column slice (if that is not known yet).
    if (*reaskAccessColumnSlice_p) {
	*canAccessColumnSlice_p = baseColPtr_p->canAccessColumnSlice
	                                       (*reaskAccessColumnSlice_p);
    }
    //# Access the column slice if possible.
    //# Otherwise put the entire array by looping through all cells.
    if (*canAccessColumnSlice_p) {
	baseColPtr_p->putColumnSlice (ns, &arr);
    }else{
	ReadOnlyArrayIterator<T> iter(arr, arr.ndim()-1);
	for (uInt rownr=0; rownr<nrrow; rownr++) {
	    putSlice (rownr, ns, iter.array());
	    iter.next();
	}
    }
}


template<class T>
void ArrayColumn<T>::putColumnRange (const Slicer& rowRange,
				     const Array<T>& arr)
{
    uInt nrrow = nrow();
    IPosition shp, blc, trc, inc;
    shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
    //# When the entire column is accessed, use that function.
    if (shp(0) == nrrow) {
	putColumn (arr);
	return;
    }
    //# First check if number of rows matches.
    nrrow = shp(0);
    IPosition arrshp  = arr.shape();
    uInt last = arrshp.nelements() - 1;
    if (arrshp(last) != nrrow) {
	throw (TableArrayConformanceError(
	                              "ArrayColumn::putColumnRange (nrrow)"));
    }
    //# Remove #rows from shape to get the shape of each cell.
    arrshp.resize (last);
    //# When the array is fixed shape, check if the shape matches.
    if ((columnDesc().options() & ColumnDesc::FixedShape)
	                                     == ColumnDesc::FixedShape) {
	if (! arrshp.isEqual (shapeColumn())) {
	    throw (TableArrayConformanceError("ArrayColumn::putColumn"));
	}
    }else{
	//# Otherwise set the shape of each cell (as far as needed).
	uInt rownr = blc(0);
	uInt incr = inc(0);
	for (uInt i=0; i<nrrow; i++) {
	    setShape (rownr, arrshp);
	    rownr += incr;
	}
    }
    //# Put the entire array by looping through all cells.
    uInt rownr = blc(0);
    uInt incr = inc(0);
    ReadOnlyArrayIterator<T> iter(arr, arr.ndim()-1);
    for (uInt i=0; i<nrrow; i++) {
	baseColPtr_p->put (rownr, &(iter.array()));
	rownr += incr;
	iter.next();
    }
}


template<class T>
void ArrayColumn<T>::put (uInt thisRownr, const ROArrayColumn<T>& that,
			  uInt thatRownr)
{
    put (thisRownr, that(thatRownr));
}

//# This is a very simple implementation.
//# However, it does not need to be more fancy, since an array operation
//# is already much more expensive than the virtual function calls
//# involved in each loop iteration.
template<class T>
void ArrayColumn<T>::fillColumn (const Array<T>& value)
{
    uInt nrrow = nrow();
    for (uInt i=0; i<nrrow; i++) {
	put (i, value);
    }
}

template<class T>
void ArrayColumn<T>::putColumn (const ROArrayColumn<T>& that)
{
    //# Check the column lengths.
    uInt nrrow = nrow();
    if (nrrow != that.nrow()) {
	throw (TableConformanceError ("ArrayColumn<T>::putColumn"));
    }
    for (uInt i=0; i<nrrow; i++) {
	put (i, that, i);
    }
}
