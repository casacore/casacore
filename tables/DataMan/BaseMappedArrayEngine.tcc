//# BaseMappedArrayEngine.cc: Abstract virtual column engine for virtual->stored mapping
//# Copyright (C) 1995,1996,2001,2002
//# Associated Universitie Inc. Washington DC, USA.
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

#ifndef TABLES_BASEMAPPEDARRAYENGINE_TCC
#define TABLES_BASEMAPPEDARRAYENGINE_TCC

//# Includes
#include <casacore/tables/DataMan/BaseMappedArrayEngine.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class VirtualType, class StoredType>
BaseMappedArrayEngine<VirtualType, StoredType>::BaseMappedArrayEngine ()
: virtualName_p  (""),
  storedName_p   (""),
  isWritable_p   (True),
  tempWritable_p (False),
  initialNrrow_p (0),
  arrayIsFixed_p (False),
  column_p       (0)
{}

template<class VirtualType, class StoredType>
BaseMappedArrayEngine<VirtualType, StoredType>::BaseMappedArrayEngine
                                        (const String& virtualColumnName,
					 const String& storedColumnName)
: virtualName_p  (virtualColumnName),
  storedName_p   (storedColumnName), 
  isWritable_p   (True),
  tempWritable_p (False),
  initialNrrow_p (0),
  arrayIsFixed_p (False),
  column_p       (0)
{}

template<class VirtualType, class StoredType>
BaseMappedArrayEngine<VirtualType, StoredType>::BaseMappedArrayEngine
                   (const BaseMappedArrayEngine<VirtualType, StoredType>& that)
: VirtualColumnEngine(),
  VirtualArrayColumn<VirtualType>(),
  virtualName_p  (that.virtualName_p),
  storedName_p   (that.storedName_p),
  isWritable_p   (that.isWritable_p),
  tempWritable_p (False),
  initialNrrow_p (0),
  arrayIsFixed_p (False),
  column_p       (0)
{}

template<class VirtualType, class StoredType>
BaseMappedArrayEngine<VirtualType, StoredType>::~BaseMappedArrayEngine()
{
    delete column_p;
}


// The function prepare is called upon initialization of the virtual column.
// The initialization order of the columns is undetermined, which means
// that this function isWritable can be called before the column has been
// initialized.
// For example, suppose column A uses column B and A gets initialized
// before B. Then A will call B's isWritable(), while B has not been
// initialized yet.
// This all means that isWritable must take care of the case
// where the writable_p flag is not set yet.
template<class VirtualType, class StoredType>
Bool BaseMappedArrayEngine<VirtualType, StoredType>::isWritable() const
{
    if (tempWritable_p) {
	return True;
    }
    return isWritable_p  &&  table().isColumnWritable (storedName_p);
}


// Create the column object for the array column in this engine.
// This merely checks if the virtual column name matches.
template<class VirtualType, class StoredType>
DataManagerColumn*
          BaseMappedArrayEngine<VirtualType, StoredType>::makeIndArrColumn
                            (const String& columnName, int, const String&)
{
    //# Check if the column name matches the virtual column name.
    //# The virtual name is only filled in case of creating a new table.
    //# In case the table is read back, makeIndArrColumn is called
    //# before prepare, thus before the virtual name can be read back.
    if (virtualName_p.empty()) {
	virtualName_p = columnName;
    } else if (columnName != virtualName_p) {
	throw (DataManInvOper
	       ("BaseMappedArrayEngine with virtual column " + virtualName_p +
		" bound to column " + columnName + "; should be the same"));
    }
    return this;
}
 
template<class VirtualType, class StoredType>
TableColumn BaseMappedArrayEngine<VirtualType, StoredType>::makeTableColumn
                                                (const String& columnName)
{
    tempWritable_p = True;
    TableColumn thisCol (table(), columnName);
    tempWritable_p = False;
    return thisCol;
}


template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::create (uInt initialNrrow)
{
    //# Define the stored name as a column keyword in the virtual.
    makeTableColumn (virtualName_p).rwKeywordSet().define
	                       ("_BaseMappedArrayEngine_Name", storedName_p);
    initialNrrow_p = initialNrrow;
}

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::prepare()
{
    prepare1();
    prepare2();
}


template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::prepare1()
{
    //# Get the name of the stored column from the keywords in the
    //# virtual column.
    tempWritable_p = True;
    TableColumn thisCol (table(), virtualName_p);
    storedName_p = thisCol.keywordSet().asString
	                                    ("_BaseMappedArrayEngine_Name");
    //# Determine if the stored column is writable.
    //# Allocate an object to get from the stored column.
    //# Allocate one to put if the column is writable.
    column_p = new ArrayColumn<StoredType> (table(), storedName_p);
    tempWritable_p = False;
    //# It is not permitted to have a FixedShape stored and non-FixedShape
    //# virtual column.
    if ((! arrayIsFixed_p)  &&
              ((column_p->columnDesc().options() & ColumnDesc::FixedShape)
	                                          == ColumnDesc::FixedShape)) {
	throw (DataManInvOper ("BaseMappedArrayEngine: virtual column " +
			       virtualName_p + " is FixedShape, but stored " +
			       storedName_p + " is not"));
    }
}

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::prepare2()
{
    //# Add the initial number of rows (thus only done after create).
    //# This will set the shape of the stored arrays when needed.
    if (initialNrrow_p > 0) {
	addRowInit (0, initialNrrow_p);
    }
}

//# Add nrrow rows to the end of the table.
//# Set the shape if virtual is FixedShape and stored is non-FixedShape.
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::addRow (uInt nrrow)
{
  addRowInit (table().nrow(), nrrow);
}
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::addRowInit (uInt startRow,
								uInt nrrow)
{
    if (arrayIsFixed_p  &&
              ((column_p->columnDesc().options() & ColumnDesc::FixedShape)
	                                        != ColumnDesc::FixedShape)) {
	for (uInt i=0; i<nrrow; i++) {
	    column_p->setShape (startRow++, shapeFixed_p);
	}
    }
}

//# This function is called in case the virtual column has FixedShape arrays.
//# If the stored has non-FixedShape arrays this shape will be set for the
//# array in each row of the stored (by function addRow).
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::setShapeColumn
                                                   (const IPosition& shape)
{
    shapeFixed_p   = shape;
    arrayIsFixed_p = True;
}

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::setShape
                                       (uInt rownr, const IPosition& shape)
{
    column_p->setShape (rownr, shape);
}

template<class VirtualType, class StoredType>
Bool BaseMappedArrayEngine<VirtualType, StoredType>::isShapeDefined (uInt rownr)
{
    return column_p->isDefined (rownr);
}

template<class VirtualType, class StoredType>
uInt BaseMappedArrayEngine<VirtualType, StoredType>::ndim (uInt rownr)
{
    return column_p->ndim (rownr);
}

template<class VirtualType, class StoredType>
IPosition BaseMappedArrayEngine<VirtualType, StoredType>::shape (uInt rownr)
{
    return column_p->shape (rownr);
}

template<class VirtualType, class StoredType>
Bool BaseMappedArrayEngine<VirtualType, StoredType>::canChangeShape() const
{
    return (column_p == 0  ?  False : column_p->canChangeShape());
}


template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::getArray
(uInt rownr, Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    column().baseGet (rownr, target);
    mapOnGet (array, target);
  }
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::putArray
(uInt rownr, const Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    mapOnPut (array, target);
    column().basePut (rownr, target);
  }

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::getSlice
(uInt rownr, const Slicer& slicer, Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(rownr, array.shape()));
    column().getSlice (rownr, getStoredSlicer(slicer), target);
    mapOnGet (array, target);
  }
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::putSlice
(uInt rownr, const Slicer& slicer, const Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(rownr, array.shape()));
    mapOnPut (array, target);
    column().putSlice (rownr, getStoredSlicer(slicer), target);
  }

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::getArrayColumn
(Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    column().getColumn (target);
    mapOnGet (array, target);
  }
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::putArrayColumn
(const Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    mapOnPut (array, target);
    column().putColumn (target);
  }

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::getArrayColumnCells
(const RefRows& rownrs, Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    column().getColumnCells (rownrs, target);
    mapOnGet (array, target);
  }
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::putArrayColumnCells
(const RefRows& rownrs, const Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    mapOnPut (array, target);
    column().putColumnCells (rownrs, target);
  }

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::getColumnSlice
(const Slicer& slicer, Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    column().getColumn (getStoredSlicer(slicer), target);
    mapOnGet (array, target);
  }
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::putColumnSlice
(const Slicer& slicer, const Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    mapOnPut (array, target);
    column().putColumn (getStoredSlicer(slicer), target);
  }

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::getColumnSliceCells
(const RefRows& rownrs, const Slicer& slicer, Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    column().getColumnCells (rownrs, getStoredSlicer(slicer), target);
    mapOnGet (array, target);
  }
template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::putColumnSliceCells
(const RefRows& rownrs, const Slicer& slicer, const Array<VirtualType>& array)
  {
    Array<StoredType> target(getStoredShape(0, array.shape()));
    mapOnPut (array, target);
    column().putColumnCells (rownrs, getStoredSlicer(slicer), target);
  }

template<class VirtualType, class StoredType>
IPosition BaseMappedArrayEngine<VirtualType, StoredType>::getStoredShape
(uInt, const IPosition& virtualShape)
{
  return virtualShape;
}

template<class VirtualType, class StoredType>
Slicer BaseMappedArrayEngine<VirtualType, StoredType>::getStoredSlicer
(const Slicer& virtualSlicer) const
{
  return virtualSlicer;
}

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::mapOnGet
(Array<VirtualType>&, const Array<StoredType>&)
{
  throw ("BaseMappedArrayEngine::mapOnGet not implemented");
}

template<class VirtualType, class StoredType>
void BaseMappedArrayEngine<VirtualType, StoredType>::mapOnPut
(const Array<VirtualType>&, Array<StoredType>&)
{
  throw ("BaseMappedArrayEngine::mapOnPut not implemented");
}


} //# NAMESPACE CASACORE - END

#endif
