//# ArrayColumn.cc: Access to an array table column with arbitrary data type
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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
//# $Id: ArrayColumn.tcc 21562 2015-02-16 07:03:44Z gervandiepen $

#ifndef TABLES_ARRAYCOLUMN_TCC
#define TABLES_ARRAYCOLUMN_TCC

#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ArrayColumnFunc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValTypeId.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ArrayColumn<T>::ArrayColumn()
: ArrayColumnBase ()
{}

template<class T>
ArrayColumn<T>::ArrayColumn (const Table& tab,
                             const String& columnName)
: ArrayColumnBase (tab, columnName)
{
    checkDataType();
}

template<class T>
ArrayColumn<T>::ArrayColumn (const TableColumn& column)
: ArrayColumnBase (column)
{
    checkDataType();
}

template<class T>
ArrayColumn<T>::ArrayColumn (const ArrayColumn<T>& that)
: ArrayColumnBase (that)
{}

template<class T>
TableColumn* ArrayColumn<T>::clone() const
{
    return new ArrayColumn<T> (*this);
}

template<class T>
ArrayColumn<T>& ArrayColumn<T>::operator= (const ArrayColumn<T>& that)
{
  reference (that);
  return (*this);
}

template<class T>
void ArrayColumn<T>::reference (const ArrayColumn<T>& that)
{
    TableColumn::reference (that);
}

template<class T>
ArrayColumn<T>::~ArrayColumn()
{}


template<class T>
void ArrayColumn<T>::checkDataType() const
{
    //# Check if the data type matches.
    const ColumnDesc& cd = baseColPtr_p->columnDesc();
    DataType dtype = cd.dataType();
    if (dtype != ValType::getType(static_cast<T*>(0))  ||  !cd.isArray()) {
	throw (TableInvDT (" in ArrayColumn ctor for column " + cd.name()));
    }
    if (dtype == TpOther) {
	if (cd.dataTypeId() != valDataTypeId(static_cast<T*>(0))) {
	    throw (TableInvDT (" in ArrayColumn ctor for column "
			       + cd.name() + "; using data type id "
			       + valDataTypeId(static_cast<T*>(0))
			       + ", expected " + cd.dataTypeId()));
	}
    }
}

template<class T>
Array<T> ArrayColumn<T>::operator() (uInt rownr) const
{
    Array<T> arr;
    get (rownr, arr);
    return arr;
}

template<class T>
Array<T> ArrayColumn<T>::get (uInt rownr) const
{
    Array<T> arr;
    get (rownr, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::get (uInt rownr, Array<T>& arr, Bool resize) const
{
    acbGet (rownr, arr, resize);
}


template<class T>
Array<T> ArrayColumn<T>::getSlice (uInt rownr,
                                   const Slicer& arraySection) const
{
    Array<T> arr;
    getSlice (rownr, arraySection, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getSlice (uInt rownr, const Slicer& arraySection,
                               Array<T>& arr, Bool resize) const
{
    acbGetSlice (rownr, arraySection, arr, resize);
}


template<class T>
Array<T> ArrayColumn<T>::getSlice
(uInt rownr, const Vector<Vector<Slice> >& arraySlices) const
{
    Array<T> arr;
    getSlice (rownr, arraySlices, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getSlice (uInt rownr,
                               const Vector<Vector<Slice> >& arraySlices,
                               Array<T>& arr, Bool resize) const
{
    acbGetSlice (rownr, arraySlices, arr, resize);
}

template<class T>
void ArrayColumn<T>::getColumnCells (const RefRows& rows,
                                     const ColumnSlicer& columnSlicer,
                                     Array<T>& destination,
                                     Bool resize) const
{
    acbGetColumnCells (rows, columnSlicer, destination, resize);
}


//template<class T>
//void
//ArrayColumn<T>::getColumnCellsSlicers (const Vector<Vector<Slice> > & arraySlices,
//                                       uInt axis,
//                                       Vector<uInt> selections,
//                                       vector<Slicer *> result) const
//{
//    if (axis == arraySlices.size()){
//
//        IPosition start (axis), increment (axis), length (axis);
//
//        for (uInt i = 0; i < axis; i++){
//
//            const Slice & slice = arraySlices [i] [selections [i]];
//            start [i] = slice.start();
//            increment [i] = slice.inc();
//            length [i] = slice.length();
//
//            result.push_back = new Slicer (start, length, increment);
//
//        }
//
//        return;
//    }
//
//    const Vector<Slice> & thisAxis = arraySlices [axis];
//
//    for (uInt i = 0; i < thisAxis.size(); i++){
//
//        selections [axis] = i;
//        getColumnCellsSlicers (arraySlices, axis + 1, selections, result);
//    }
//
//}




template<class T>
Array<T> ArrayColumn<T>::getColumn() const
{
    Array<T> arr;
    getColumn (arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumn (Array<T>& arr, Bool resize) const
{
    acbGetColumn (arr, resize);
}


template<class T>
Array<T> ArrayColumn<T>::getColumn (const Slicer& arraySection) const
{
    Array<T> arr;
    getColumn (arraySection, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumn (const Slicer& arraySection,
                                Array<T>& arr, Bool resize) const
{
    acbGetColumn (arraySection, arr, resize);
}


template<class T>
Array<T> ArrayColumn<T>::getColumn
(const Vector<Vector<Slice> >& arraySlices) const
{
    Array<T> arr;
    getColumn (arraySlices, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumn (const Vector<Vector<Slice> >& arraySlices,
                                Array<T>& arr, Bool resize) const
{
    acbGetColumn (arraySlices, arr, resize);
}


template<class T>
Array<T> ArrayColumn<T>::getColumnRange (const Slicer& rowRange) const
{
    Array<T> arr;
    getColumnRange (rowRange, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumnRange (const Slicer& rowRange,
                                     Array<T>& arr, Bool resize) const
{
    acbGetColumnRange (rowRange, arr, resize);
}

template<class T>
Array<T> ArrayColumn<T>::getColumnCells (const RefRows& rownrs) const
{
    Array<T> arr;
    getColumnCells (rownrs, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumnCells (const RefRows& rownrs,
                                     Array<T>& arr, Bool resize) const
{
    acbGetColumnCells (rownrs, arr, resize);
}


template<class T>
Array<T> ArrayColumn<T>::getColumnRange (const Slicer& rowRange,
                                         const Slicer& arraySection) const
{
    Array<T> arr;
    getColumnRange (rowRange, arraySection, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumnRange (const Slicer& rowRange,
                                     const Slicer& arraySection,
                                     Array<T>& arr, Bool resize) const
{
    acbGetColumnRange (rowRange, arraySection, arr, resize);
}

template<class T>
Array<T> ArrayColumn<T>::getColumnCells (const RefRows& rownrs,
                                         const Slicer& arraySection) const
{
    Array<T> arr;
    getColumnCells (rownrs, arraySection, arr);
    return arr;
}

template<class T>
void ArrayColumn<T>::getColumnCells (const RefRows& rownrs,
                                     const Slicer& arraySection,
                                     Array<T>& arr, Bool resize) const
{
  acbGetColumnCells (rownrs, arraySection, arr, resize);
}


template<class T>
void ArrayColumn<T>::setShape (uInt rownr, const IPosition& shape)
{
  ArrayColumnBase::setShape (rownr, shape);
}
	
template<class T>
void ArrayColumn<T>::setShape (uInt rownr, const IPosition& shape,
			       const IPosition& tileShape)
{
  ArrayColumnBase::setShape (rownr, shape, tileShape);
}
	
template<class T>
void ArrayColumn<T>::put (uInt rownr, const Array<T>& arr)
{
  acbPut (rownr, arr);
}

template<class T>
void ArrayColumn<T>::putSlice (uInt rownr, const Slicer& arraySection,
			       const Array<T>& arr)
{
  acbPutSlice (rownr, arraySection, arr);
}

template<class T>
void ArrayColumn<T>::putSlice (uInt rownr,
                               const Vector<Vector<Slice> >& arraySlices,
			       const Array<T>& arr)
{
  acbPutSlice (rownr, arraySlices, arr);
}

template<class T>
void ArrayColumn<T>::putColumnCells (const RefRows& rows,
                                     const Vector<Vector<Slice> >& arraySlices,
                                     const Array<T>& source)
{
  acbPutColumnCells (rows, arraySlices, source);
}

template <typename T>
void
ArrayColumn<T>::putColumnCells (const RefRows& rows,
                                const ColumnSlicer& columnSlicer,
                                const Array<T>& source)
{
  acbPutColumnCells (rows, columnSlicer, source);
}


template<class T>
void ArrayColumn<T>::put (uInt thisRownr, const TableColumn& that,
			  uInt thatRownr, Bool preserveTileShape)
{
  TableColumn::put (thisRownr, that, thatRownr, preserveTileShape);
}

template<class T>
void ArrayColumn<T>::putColumn (const Array<T>& arr)
{
  acbPutColumn (arr);
}

template<class T>
void ArrayColumn<T>::putColumn (const Slicer& arraySection, const Array<T>& arr)
{
  acbPutColumn (arraySection, arr);
}

template<class T>
void ArrayColumn<T>::putColumn (const Vector<Vector<Slice> >& arraySlices,
                                const Array<T>& arr)
{
  acbPutColumn (arraySlices, arr);
}

template<class T>
void ArrayColumn<T>::putColumnRange (const Slicer& rowRange,
				     const Array<T>& arr)
{
  acbPutColumnRange (rowRange, arr);
}

template<class T>
void ArrayColumn<T>::putColumnCells (const RefRows& rownrs,
				     const Array<T>& arr)
{
  acbPutColumnCells (rownrs, arr);
}

template<class T>
void ArrayColumn<T>::putColumnRange (const Slicer& rowRange,
				     const Slicer& arraySection,
				     const Array<T>& arr)
{
  acbPutColumnRange (rowRange, arraySection, arr);
}

template<class T>
void ArrayColumn<T>::putColumnCells (const RefRows& rownrs,
				     const Slicer& arraySection,
				     const Array<T>& arr)
{
  acbPutColumnCells (rownrs, arraySection, arr);
}


//# This is a very simple implementation.
//# However, it does not need to be more fancy, since an array operation
//# is already much more expensive than the virtual function calls
//# involved in each loop iteration.
template<class T>
void ArrayColumn<T>::fillColumn (const Array<T>& value)
{
  acbFillColumn (value);
}

template<class T>
void ArrayColumn<T>::putColumn (const ArrayColumn<T>& that)
{
  acbPutColumn (that);
}


} //# NAMESPACE CASACORE - END

#endif
