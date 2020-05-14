//# VirtArrCol.tcc: Base virtual column data manager class
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
int VirtualArrayColumn<T>::dataType() const
    { return ValType::getType (static_cast<T*>(0)); }

template<class T>
String VirtualArrayColumn<T>::dataTypeId() const
{
    return valDataTypeId (static_cast<T*>(0));
}


template<class T>
void VirtualArrayColumn<T>::getArrayV (rownr_t rownr, ArrayBase& array)
    { getArray (rownr, static_cast<Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::putArrayV (rownr_t rownr, const ArrayBase& array)
    { putArray (rownr, static_cast<const Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::getSliceV (rownr_t rownr, const Slicer& slicer,
				       ArrayBase& array)
    { getSlice (rownr, slicer, static_cast<Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::putSliceV (rownr_t rownr, const Slicer& slicer,
				       const ArrayBase& array)
    { putSlice (rownr, slicer, static_cast<const Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::getArrayColumnV (ArrayBase& array)
    { getArrayColumn (static_cast<Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::putArrayColumnV (const ArrayBase& array)
    { putArrayColumn (static_cast<const Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::getArrayColumnCellsV (const RefRows& rownrs,
						  ArrayBase& array)
    { getArrayColumnCells (rownrs, static_cast<Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::putArrayColumnCellsV (const RefRows& rownrs,
						  const ArrayBase& array)
    { putArrayColumnCells (rownrs, static_cast<const Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::getColumnSliceV (const Slicer& slicer,
					     ArrayBase& array)
    { getColumnSlice (slicer, static_cast<Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::putColumnSliceV (const Slicer& slicer,
					     const ArrayBase& array)
    { putColumnSlice (slicer, static_cast<const Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::getColumnSliceCellsV (const RefRows& rownrs,
						  const Slicer& slicer,
						  ArrayBase& array)
    { getColumnSliceCells (rownrs, slicer, static_cast<Array<T>&>(array)); }
template<class T>
void VirtualArrayColumn<T>::putColumnSliceCellsV (const RefRows& rownrs,
						  const Slicer& slicer,
						  const ArrayBase& array)
    { putColumnSliceCells (rownrs, slicer, static_cast<const Array<T>&>(array)); }



//# The default implementations of get/putSlice get the entire array
//# and access the required slice.
template<class T>
void VirtualArrayColumn<T>::getSlice (rownr_t rownr, const Slicer& slicer,
				      Array<T>& arraySlice)
{
  getSliceBase (rownr, slicer, arraySlice);
}
template<class T>
void VirtualArrayColumn<T>::putSlice (rownr_t rownr, const Slicer& slicer,
				      const Array<T>& arraySlice)
{
  putSliceBase (rownr, slicer, arraySlice);
}

//# The default implementations of get/putArrayColumn handle the
//# array in each individual row.
template<class T>
void VirtualArrayColumn<T>::getArrayColumn (Array<T>& array)
{
  getArrayColumnBase (array);
}
template<class T>
void VirtualArrayColumn<T>::putArrayColumn (const Array<T>& array)
{
  putArrayColumnBase (array);
}

//# The default implementations of get/putColumnSlice take a slice
//# for each individual row.
template<class T>
void VirtualArrayColumn<T>::getColumnSlice (const Slicer& slicer,
					    Array<T>& array)
{
  getColumnSliceBase (slicer, array);
}
template<class T>
void VirtualArrayColumn<T>::putColumnSlice (const Slicer& slicer,
					    const Array<T>& array)
{
  putColumnSliceBase (slicer, array);
}

//# The default implementations of the Cells functions throw an exception.
template<class T>
void VirtualArrayColumn<T>::getArrayColumnCells (const RefRows& rownrs,
                                                 Array<T>& value)
{
  getArrayColumnCellsBase (rownrs, value);
}
template<class T>
void VirtualArrayColumn<T>::putArrayColumnCells (const RefRows& rownrs,
						 const Array<T>& value)
{
  putArrayColumnCellsBase (rownrs, value);
}
template<class T>
void VirtualArrayColumn<T>::getColumnSliceCells (const RefRows& rownrs,
						 const Slicer& ns,
						 Array<T>& value)
{
  getColumnSliceCellsBase (rownrs, ns, value);
}
template<class T>
void VirtualArrayColumn<T>::putColumnSliceCells (const RefRows& rownrs,
						 const Slicer& ns,
						 const Array<T>& value)
{
  putColumnSliceCellsBase (rownrs, ns, value);
}

//# The default implementation of the put function throws
//# an exception.
template<class T>
void VirtualArrayColumn<T>::putArray (rownr_t, const Array<T>&)
{ 
    throw DataManInvOper ("VirtualArrayColumn::putArray not possible"
                          " for column " + columnName());
}

} //# NAMESPACE CASACORE - END


#endif
