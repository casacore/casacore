//# ScalarColumn.cc: Access to a scalar table column with arbitrary data type
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

#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/BaseColumn.h>
#include <aips/Tables/RefRows.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/ValTypeId.h>
#include <aips/Utilities/String.h>
#include <aips/Tables/TableError.h>


template<class T>
ROScalarColumn<T>::ROScalarColumn()
: ROTableColumn(),
  canAccessColumn_p   (new Bool(False)),
  reaskAccessColumn_p (new Bool(True))
{}

template<class T>
ROScalarColumn<T>::ROScalarColumn (const Table& tab,
				   const String& columnName)
: ROTableColumn (tab, columnName),
  canAccessColumn_p   (new Bool(False)),
  reaskAccessColumn_p (new Bool(True))
{
    checkDataType();
}

template<class T>
ROScalarColumn<T>::ROScalarColumn (const ROTableColumn& column)
: ROTableColumn (column),
  canAccessColumn_p   (new Bool(False)),
  reaskAccessColumn_p (new Bool(True))
{
    checkDataType();
}

template<class T>
ROScalarColumn<T>::ROScalarColumn (const ROScalarColumn<T>& that)
: ROTableColumn (that),
  canAccessColumn_p   (new Bool (*that.canAccessColumn_p)),
  reaskAccessColumn_p (new Bool (*that.reaskAccessColumn_p))
{}

template<class T>
ROTableColumn* ROScalarColumn<T>::clone() const
{
    return new ROScalarColumn<T> (*this);
}

template<class T>
void ROScalarColumn<T>::reference (const ROScalarColumn<T>& that)
{
    ROTableColumn::reference (that);
    *canAccessColumn_p   = *that.canAccessColumn_p;
    *reaskAccessColumn_p = *that.reaskAccessColumn_p;
}

template<class T>
ROScalarColumn<T>::~ROScalarColumn()
{
    delete canAccessColumn_p;
    delete reaskAccessColumn_p;
}

template<class T>
void ROScalarColumn<T>::checkDataType() const
{
    //# Check if the data type matches.
    const ColumnDesc& cd = baseColPtr_p->columnDesc();
    DataType dtype = cd.dataType();
    if (dtype != ValType::getType((T*)0)  ||  !cd.isScalar()) {
	throw (TableInvDT (" in ROScalarColumn ctor for column " + cd.name()));
    }
    if (dtype == TpOther) {
	if (cd.dataTypeId() != valDataTypeId((T*)0)) {
	    throw (TableInvDT (" in ROScalarColumn ctor for column "
			       + cd.name() + "; using data type id "
			       + valDataTypeId((T*)0)
			       + ", expected " + cd.dataTypeId()));
	}
    }
}


template<class T>
Vector<T> ROScalarColumn<T>::getColumn() const
{
    Vector<T> vec;
    getColumn (vec);
    return vec;
}

template<class T>
void ROScalarColumn<T>::getColumn (Vector<T>& vec, Bool resize) const
{
    uInt nrrow = nrow();
    //# Resize the vector if empty; otherwise check its length.
    if (vec.nelements() != nrrow) {
	if (resize  ||  vec.nelements() == 0) {
	    vec.resize (nrrow);
	}else{
	    throw (TableConformanceError("ScalarColumn::getColumn"));
	}
    }
    //# Ask if we can access the column (if that is not known yet).
    if (*reaskAccessColumn_p) {
	*canAccessColumn_p = baseColPtr_p->canAccessScalarColumn
	                                             (*reaskAccessColumn_p);
    }
    //# Access the column if possible.
    //# Otherwise fill the entire vector by looping through all cells.
    if (*canAccessColumn_p) {
	baseColPtr_p->getScalarColumn (&vec);
    }else{
	for (uInt rownr=0; rownr<nrrow; rownr++) {
	    baseColPtr_p->get (rownr, &(vec(rownr)));
	}
    }
}


template<class T>
Vector<T> ROScalarColumn<T>::getColumnRange (const Slicer& rowRange) const
{
    Vector<T> vec;
    getColumnRange (rowRange, vec);
    return vec;
}

template<class T>
void ROScalarColumn<T>::getColumnRange (const Slicer& rowRange,
					Vector<T>& vec, Bool resize) const
{
    uInt nrrow = nrow();
    IPosition shp, blc, trc, inc;
    shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
    //# When the entire column is accessed, use that function.
    if (blc(0) == 0  &&  shp(0) == Int(nrrow)  &&  inc(0) == 1) {
	getColumn (vec, resize);
    } else {
	getColumnCells (RefRows(blc(0), trc(0), inc(0)), vec, resize);
    }
}

template<class T>
Vector<T> ROScalarColumn<T>::getColumnCells (const RefRows& rownrs) const
{
    Vector<T> vec;
    getColumnCells (rownrs, vec);
    return vec;
}

template<class T>
void ROScalarColumn<T>::getColumnCells (const RefRows& rownrs,
					Vector<T>& vec, Bool resize) const
{
    //# Resize the vector if needed; otherwise check its length.
    uInt nrrow = rownrs.nrow();
    if (vec.nelements() != nrrow) {
	if (resize  ||  vec.nelements() == 0) {
	    vec.resize (nrrow);
	}else{
	    throw (TableConformanceError("ScalarColumn::getColumnCells"));
	}
    }
    baseColPtr_p->getScalarColumnCells (rownrs, &vec);
}



template<class T>
ScalarColumn<T>::ScalarColumn()
: ROTableColumn     (),
  ROScalarColumn<T> (),
  TableColumn       ()
{}

template<class T>
ScalarColumn<T>::ScalarColumn (const Table& tab, const String& columnName)
: ROTableColumn     (tab, columnName),
  ROScalarColumn<T> (tab, columnName),
  TableColumn       (tab, columnName)
{}

template<class T>
ScalarColumn<T>::ScalarColumn (const TableColumn& column)
: ROTableColumn     (column),
  ROScalarColumn<T> (column),
  TableColumn       (column)
{}

template<class T>
ScalarColumn<T>::ScalarColumn (const ScalarColumn<T>& that)
: ROTableColumn     (that),
  ROScalarColumn<T> (that),
  TableColumn       (that)
{}

template<class T>
ROTableColumn* ScalarColumn<T>::clone() const
{
    return new ScalarColumn<T> (*this);
}

template<class T>
void ScalarColumn<T>::reference (const ScalarColumn<T>& that)
    { ROScalarColumn<T>::reference (that); }

template<class T>
ScalarColumn<T>::~ScalarColumn()
{}

template<class T>
void ScalarColumn<T>::put (uInt thisRownr, const ROScalarColumn<T>& that,
			   uInt thatRownr)
{
    put (thisRownr, that(thatRownr));
}

template<class T>
void ScalarColumn<T>::put (uInt thisRownr, const ROTableColumn& that,
			   uInt thatRownr)
{
    T value;
    that.getScalarValue (thatRownr, &value, columnDesc().dataTypeId());
    put (thisRownr, value);
}

template<class T>
void ScalarColumn<T>::putColumn (const Vector<T>& vec)
{
    uInt nrrow = nrow();
    //# Check the vector length.
    if (vec.nelements() != nrrow) {
	throw (TableConformanceError("ScalarColumn::putColumn(Vector&)"));
    }
    //# Ask if we can access the column (if that is not known yet).
    if (*reaskAccessColumn_p) {
	*canAccessColumn_p = baseColPtr_p->canAccessScalarColumn
	                                             (*reaskAccessColumn_p);
    }
    //# Access the column if possible.
    //# Otherwise put the entire vector by looping through all cells.
    if (*canAccessColumn_p) {
	baseColPtr_p->putScalarColumn (&vec);
    }else{
	for (uInt rownr=0; rownr<nrrow; rownr++) {
	    baseColPtr_p->put (rownr, &(vec(rownr)));
	}
    }
}

template<class T>
void ScalarColumn<T>::putColumnRange (const Slicer& rowRange,
				      const Vector<T>& vec)
{
    uInt nrrow = nrow();
    IPosition shp, blc, trc, inc;
    shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
    //# When the entire column is accessed, use that function.
    if (blc(0) == 0  &&  shp(0) == Int(nrrow)  &&  inc(0) == 1) {
	putColumn (vec);
    } else {
	putColumnCells (RefRows(blc(0), trc(0), inc(0)), vec);
    }
}

template<class T>
void ScalarColumn<T>::putColumnCells (const RefRows& rownrs,
				      const Vector<T>& vec)
{
    //# Check the vector length.
    uInt nrrow = rownrs.nrow();
    if (vec.nelements() != nrrow) {
	throw (TableConformanceError("ScalarColumn::putColumnCells"));
    }
    baseColPtr_p->putScalarColumnCells (rownrs, &vec);
}


//#// This is a very simple implementation.
//#// Ultimately this must be done more directly via the data manager.
template<class T>
void ScalarColumn<T>::fillColumn (const T& value)
{
    uInt nrrow = nrow();
    for (uInt i=0; i<nrrow; i++) {
	put (i, value);
    }
}

template<class T>
void ScalarColumn<T>::putColumn (const ROScalarColumn<T>& that)
{
    //# Check the column lengths.
    uInt nrrow = nrow();
    if (nrrow != that.nrow()) {
	throw (TableConformanceError ("ScalarColumn<T>::putColumn"));
    }
    for (uInt i=0; i<nrrow; i++) {
	put (i, that, i);
    }
}
