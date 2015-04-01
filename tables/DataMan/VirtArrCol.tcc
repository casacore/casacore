//# VirtArrCol.cc: Base virtual column data manager class
//# Copyright (C) 1994,1995,1996,1999,2000
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

#ifndef TABLES_VIRTARRCOL_TCC
#define TABLES_VIRTARRCOL_TCC

//# Includes
#include <casacore/tables/DataMan/VirtArrCol.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
VirtualArrayColumn<T>::~VirtualArrayColumn()
{}

template<class T>
Bool VirtualArrayColumn<T>::isWritable() const
    { return False; }

template<class T>
int VirtualArrayColumn<T>::dataType() const
    { return ValType::getType (static_cast<T*>(0)); }

template<class T>
String VirtualArrayColumn<T>::dataTypeId() const
{
    return valDataTypeId (static_cast<T*>(0));
}


template<class T>
Bool VirtualArrayColumn<T>::canAccessSlice (Bool& reask) const
{
    reask = False;
    return True;
}
template<class T>
Bool VirtualArrayColumn<T>::canAccessArrayColumn (Bool& reask) const
{
    reask = False;
    return True;
}
template<class T>
Bool VirtualArrayColumn<T>::canAccessColumnSlice (Bool& reask) const
{
    reask = False;
    return True;
}


template<class T>
void VirtualArrayColumn<T>::getArrayV (uInt rownr, void* dataPtr)
    { getArray (rownr, *static_cast<Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::putArrayV (uInt rownr, const void* dataPtr)
    { putArray (rownr, *static_cast<const Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::getSliceV (uInt rownr, const Slicer& slicer,
				       void* dataPtr)
    { getSlice (rownr, slicer, *static_cast<Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::putSliceV (uInt rownr, const Slicer& slicer,
				       const void* dataPtr)
    { putSlice (rownr, slicer, *static_cast<const Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::getArrayColumnV (void* dataPtr)
    { getArrayColumn (*static_cast<Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::putArrayColumnV (const void* dataPtr)
    { putArrayColumn (*static_cast<const Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::getArrayColumnCellsV (const RefRows& rownrs,
						  void* dataPtr)
    { getArrayColumnCells (rownrs, *static_cast<Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::putArrayColumnCellsV (const RefRows& rownrs,
						  const void* dataPtr)
    { putArrayColumnCells (rownrs, *static_cast<const Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::getColumnSliceV (const Slicer& slicer,
					     void* dataPtr)
    { getColumnSlice (slicer, *static_cast<Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::putColumnSliceV (const Slicer& slicer,
					     const void* dataPtr)
    { putColumnSlice (slicer, *static_cast<const Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::getColumnSliceCellsV (const RefRows& rownrs,
						  const Slicer& slicer,
						  void* dataPtr)
    { getColumnSliceCells (rownrs, slicer, *static_cast<Array<T>*>(dataPtr)); }
template<class T>
void VirtualArrayColumn<T>::putColumnSliceCellsV (const RefRows& rownrs,
						  const Slicer& slicer,
						  const void* dataPtr)
    { putColumnSliceCells (rownrs, slicer, *static_cast<const Array<T>*>(dataPtr)); }



//# The default implementations of get/putSlice get the entire array
//# and access the required slice.
template<class T>
void VirtualArrayColumn<T>::getSlice (uInt rownr, const Slicer& slicer,
				      Array<T>& arraySlice)
{
    //# Get the entire array and take the slice required.
    //# Infer the exact slice shape from the array shape.
    IPosition shp = shape(rownr);
    Array<T> arr(shp);
    getArray (rownr, arr);
    IPosition start, end, stride;
    slicer.inferShapeFromSource (shp, start, end, stride);
    arraySlice = arr(start, end, stride);
}
template<class T>
void VirtualArrayColumn<T>::putSlice (uInt rownr, const Slicer& slicer,
				      const Array<T>& arraySlice)
{
    //# Get the entire array, store the slice required and put
    //# the entire array back.
    //# Infer the exact slice shape from the array shape.
    IPosition shp = shape(rownr);
    Array<T> arr(shp);
    getArray (rownr, arr);
    IPosition start, end, stride;
    slicer.inferShapeFromSource (shp, start, end, stride);
    arr(start, end, stride) = arraySlice;
    putArray (rownr, arr);
}

//# The default implementations of get/putArrayColumn handle the
//# array in each individual row.
template<class T>
void VirtualArrayColumn<T>::getArrayColumn (Array<T>& array)
{
    ArrayIterator<T> iter(array, array.ndim()-1);
    uInt rownr = 0;
    while (! iter.pastEnd()) {
	getArray (rownr, iter.array());
	rownr++;
	iter.next();
    }
}
template<class T>
void VirtualArrayColumn<T>::putArrayColumn (const Array<T>& array)
{
    ReadOnlyArrayIterator<T> iter(array, array.ndim()-1);
    uInt rownr = 0;
    while (! iter.pastEnd()) {
	putArray (rownr, iter.array());
	rownr++;
	iter.next();
    }
}

//# The default implementations of get/putColumnSlice take a slice
//# for each individual row.
template<class T>
void VirtualArrayColumn<T>::getColumnSlice (const Slicer& slicer,
					    Array<T>& array)
{
    ArrayIterator<T> iter(array, array.ndim()-1);
    uInt rownr = 0;
    while (! iter.pastEnd()) {
	getSlice (rownr, slicer, iter.array());
	rownr++;
	iter.next();
    }
}
template<class T>
void VirtualArrayColumn<T>::putColumnSlice (const Slicer& slicer,
					    const Array<T>& array)
{
    ReadOnlyArrayIterator<T> iter(array, array.ndim()-1);
    uInt rownr = 0;
    while (! iter.pastEnd()) {
	putSlice (rownr, slicer, iter.array());
	rownr++;
	iter.next();
    }
}

//# The default implementations of the Cells functions throw an exception.
template<class T>
void VirtualArrayColumn<T>::getArrayColumnCells (const RefRows& rownrs,
                                                 Array<T>& value)
{
    ArrayIterator<T> iter(value, value.ndim()-1);
    RefRowsSliceIter rowiter(rownrs);
    while (! rowiter.pastEnd()) {
        uInt rownr = rowiter.sliceStart();
        uInt end = rowiter.sliceEnd();
        uInt incr = rowiter.sliceIncr();
        while (rownr <= end) {
            if (! isFixedShape()) {
                if (! iter.array().shape().isEqual (shape(rownr))) {
                    throw DataManError("getArrayColumnCells shape mismatch");
                }
            }
  	    getArray (rownr, iter.array());
            rownr += incr;
	    iter.next();
	}
        rowiter++;
    }
}
template<class T>
void VirtualArrayColumn<T>::putArrayColumnCells (const RefRows& rownrs,
						 const Array<T>& value)
{
    ReadOnlyArrayIterator<T> iter(value, value.ndim()-1);
    RefRowsSliceIter rowiter(rownrs);
    while (! rowiter.pastEnd()) {
        uInt rownr = rowiter.sliceStart();
        uInt end = rowiter.sliceEnd();
        uInt incr = rowiter.sliceIncr();
        while (rownr <= end) {
  	    putArray (rownr, iter.array());
            rownr += incr;
	    iter.next();
	}
        rowiter++;
    }
}
template<class T>
void VirtualArrayColumn<T>::getColumnSliceCells (const RefRows& rownrs,
						 const Slicer& ns,
						 Array<T>& value)
{
    ArrayIterator<T> iter(value, value.ndim()-1);
    RefRowsSliceIter rowiter(rownrs);
    while (! rowiter.pastEnd()) {
        uInt rownr = rowiter.sliceStart();
        uInt end = rowiter.sliceEnd();
        uInt incr = rowiter.sliceIncr();
        while (rownr <= end) {
	    getSlice (rownr, ns, iter.array());
            rownr += incr;
	    iter.next();
	}
        rowiter++;
    }
}
template<class T>
void VirtualArrayColumn<T>::putColumnSliceCells (const RefRows& rownrs,
						 const Slicer& ns,
						 const Array<T>& value)
{
    ReadOnlyArrayIterator<T> iter(value, value.ndim()-1);
    RefRowsSliceIter rowiter(rownrs);
    while (! rowiter.pastEnd()) {
        uInt rownr = rowiter.sliceStart();
        uInt end = rowiter.sliceEnd();
        uInt incr = rowiter.sliceIncr();
        while (rownr <= end) {
	    putSlice (rownr, ns, iter.array());
            rownr += incr;
	    iter.next();
	}
        rowiter++;
    }
}

//# The default implementation of the put function throws
//# an exception.
template<class T>
void VirtualArrayColumn<T>::putArray (uInt, const Array<T>&)
{ 
    throw (DataManInvOper ("VirtualArrayColumn::putArray not possible"));
}

//# The default implementations of the shape functions throw
//# an exception.
template<class T>
void VirtualArrayColumn<T>::setShapeColumn (const IPosition&)
{ 
    throw (DataManInvOper ("VirtualArrayColumn::setShapeColumn not possible"));
}
template<class T>
void VirtualArrayColumn<T>::setShape (uInt, const IPosition&)
{
    throw (DataManInvOper ("VirtualArrayColumn::setShape not possible"));
}
template<class T>
Bool VirtualArrayColumn<T>::isShapeDefined (uInt)
{
    throw (DataManInvOper ("VirtualArrayColumn::isShapeDefined not possible"));
    return False;
}
template<class T>
uInt VirtualArrayColumn<T>::ndim (uInt)
{
    throw (DataManInvOper ("VirtualArrayColumn::ndim not possible")); 
    return 0;
}
template<class T>
IPosition VirtualArrayColumn<T>::shape (uInt)
{
    throw (DataManInvOper ("VirtualArrayColumn::shape not possible"));
    return IPosition(0);
}

} //# NAMESPACE CASACORE - END


#endif
