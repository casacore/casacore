//# ArrColData.cc: Access to a table column containing arrays
//# Copyright (C) 1994,1995,1996,1997,1998,2000,2002
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

#ifndef TABLES_ARRCOLDATA_TCC
#define TABLES_ARRCOLDATA_TCC

#include <casacore/tables/Tables/ArrColData.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableTrace.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/AipsIO.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ArrayColumnData<T>::ArrayColumnData (const ArrayColumnDesc<T>* cd,
				     ColumnSet* csp)
: PlainColumn  (cd, csp),
  arrDescPtr_p (cd),
  shapeColDef_p(False),
  shapeCol_p   ()
{
    if (cd->shape().nelements() > 0) {
	setShapeColumn (cd->shape());
    }
}

template<class T>
ArrayColumnData<T>::~ArrayColumnData()
{}


//# Create the data manager column for this column.
//# The shape of a fixed shape array has to be defined in advance.
template<class T>
void ArrayColumnData<T>::createDataManagerColumn()
{
    //# Create the data manager column (direct or indirect).
    if ((colDescPtr_p->options() & ColumnDesc::Direct)
	                                   == ColumnDesc::Direct) {
	dataColPtr_p = dataManPtr_p->createDirArrColumn
	                      (colDescPtr_p->name(),
			       colDescPtr_p->dataType(),
			       colDescPtr_p->dataTypeId());
    }else{
	dataColPtr_p = dataManPtr_p->createIndArrColumn
	                      (colDescPtr_p->name(),
			       colDescPtr_p->dataType(),
			       colDescPtr_p->dataTypeId());
    }
    //# Check if the shape is defined in case fixed.
    //# Pass it to the created data manager column.
    if ((colDescPtr_p->options() & ColumnDesc::FixedShape)
	                                   == ColumnDesc::FixedShape) {
	if (!shapeColDef_p) {
	    throw (TableInvOper ("ArrayColumnData::createDataManagerColumn; "
				 "shape of FixedShape array in column "
				 + colDescPtr_p->name() + " not defined"));
	}
	dataColPtr_p->setFixedShapeColumn (shapeCol_p);
    }
    //# Set the maximum length of an item.
    dataColPtr_p->setMaxLength (colDescPtr_p->maxLength());
}


//# Initialize the array in the given rows.
//# This removes an array if present.
template<class T>
void ArrayColumnData<T>::initialize (uInt, uInt)
{}

template<class T>
uInt ArrayColumnData<T>::ndimColumn() const
{
    Int ndim = columnDesc().ndim();
    return (ndim > 0  ?  ndim : shapeCol_p.nelements());
}
template<class T>
IPosition ArrayColumnData<T>::shapeColumn() const
    { return shapeCol_p; }

template<class T>
void ArrayColumnData<T>::setShapeColumn (const IPosition& shp)
{
    if (shapeColDef_p) {
	if (shp != shapeCol_p) {
	    throw (TableInvOper
		   ("ArrayColumnData: change in shape of FixedShape array"
                    " of column " + colDescPtr_p->name()));
	}
    }
    if (columnDesc().ndim() > 0) {
	if (Int(shp.nelements()) != columnDesc().ndim()) {
	    throw (TableInvOper
	       ("ArrayColumnData: mismatch in #dim of FixedShape array shape"
                " of column " + colDescPtr_p->name()));
	}
    }
    shapeCol_p    = shp;
    shapeColDef_p = True;
}

template<class T>
Bool ArrayColumnData<T>::isDefined (uInt rownr) const
{
    return dataColPtr_p->isShapeDefined(rownr);
}
template<class T>
uInt ArrayColumnData<T>::ndim (uInt rownr) const
{
    return dataColPtr_p->ndim(rownr);
}
template<class T>
IPosition ArrayColumnData<T>::shape (uInt rownr) const
{
    return dataColPtr_p->shape(rownr);
}


template<class T>
void ArrayColumnData<T>::setShape (uInt rownr, const IPosition& shp)
{
    checkShape (shp);
    checkWriteLock (True);
    dataColPtr_p->setShape (rownr, shp);
    autoReleaseLock();
}
template<class T>
void ArrayColumnData<T>::setShape (uInt rownr, const IPosition& shp,
				   const IPosition& tileShp)
{
    checkShape (shp);
    checkWriteLock (True);
    dataColPtr_p->setShapeTiled (rownr, shp, tileShp);
    autoReleaseLock();
}

template<class T>
Bool ArrayColumnData<T>::canChangeShape() const
{
    return dataColPtr_p->canChangeShape();
}

template<class T>
Bool ArrayColumnData<T>::canAccessSlice (Bool& reask) const
{
    return dataColPtr_p->canAccessSlice (reask);
}
template<class T>
Bool ArrayColumnData<T>::canAccessArrayColumn (Bool& reask) const
{
    return dataColPtr_p->canAccessArrayColumn (reask);
}
template<class T>
Bool ArrayColumnData<T>::canAccessArrayColumnCells (Bool& reask) const
{
    return dataColPtr_p->canAccessArrayColumnCells (reask);
}
template<class T>
Bool ArrayColumnData<T>::canAccessColumnSlice (Bool& reask) const
{
    return dataColPtr_p->canAccessColumnSlice (reask);
}


template<class T>
void ArrayColumnData<T>::get (uInt rownr, void* arrayPtr) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownr,
                         static_cast<const Array<T>*>(arrayPtr)->shape());
    }
    checkReadLock (True);
    dataColPtr_p->getArrayV (rownr, (Array<T>*)arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::getSlice (uInt rownr, const Slicer& ns,
				   void* arrayPtr) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownr,
                         static_cast<const Array<T>*>(arrayPtr)->shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkReadLock (True);
    dataColPtr_p->getSliceV (rownr, ns, (Array<T>*)arrayPtr);
    autoReleaseLock();
}


template<class T>
void ArrayColumnData<T>::put (uInt rownr, const void* arrayPtr)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownr,
                         static_cast<const Array<T>*>(arrayPtr)->shape());
    }
    checkValueLength ((const Array<T>*)arrayPtr);
    checkWriteLock (True);
    dataColPtr_p->putArrayV (rownr, (const Array<T>*)arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::putSlice (uInt rownr, const Slicer& ns,
				   const void* arrayPtr)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownr,
                         static_cast<const Array<T>*>(arrayPtr)->shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkValueLength ((const Array<T>*)arrayPtr);
    checkWriteLock (True);
    dataColPtr_p->putSliceV (rownr, ns, (const Array<T>*)arrayPtr);
    autoReleaseLock();
}



//# Get or put the column by iterating through the array and getting the
//# column array for each row.

template<class T>
void ArrayColumnData<T>::getArrayColumn (void* arrayPtr) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r',
                         static_cast<const Array<T>*>(arrayPtr)->shape());
    }
    checkReadLock (True);
    dataColPtr_p->getArrayColumnV ((Array<T>*)arrayPtr);
    autoReleaseLock();
}


template<class T>
void ArrayColumnData<T>::getArrayColumnCells (const RefRows& rownrs,
					      void *arrayPtr) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownrs,
                         static_cast<const Array<T>*>(arrayPtr)->shape());
    }
    checkReadLock (True);
    dataColPtr_p->getArrayColumnCellsV (rownrs, arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::getColumnSlice (const Slicer& ns,
					 void* arrayPtr) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r',
                         static_cast<const Array<T>*>(arrayPtr)->shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkReadLock (True);
    dataColPtr_p->getColumnSliceV (ns, (Array<T>*)arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::getColumnSliceCells (const RefRows& rownrs,
					      const Slicer& ns,
					      void* arrayPtr) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownrs,
                         static_cast<const Array<T>*>(arrayPtr)->shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkReadLock (True);
    dataColPtr_p->getColumnSliceCellsV (rownrs, ns, arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::putArrayColumn (const void* arrayPtr)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w',
                         static_cast<const Array<T>*>(arrayPtr)->shape());
    }
    checkValueLength ((const Array<T>*)arrayPtr);
    checkWriteLock (True);
    dataColPtr_p->putArrayColumnV ((const Array<T>*)arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::putArrayColumnCells (const RefRows& rownrs,
					      const void* arrayPtr)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownrs,
                         static_cast<const Array<T>*>(arrayPtr)->shape());
    }
    checkValueLength ((const Array<T>*)arrayPtr);
    checkWriteLock (True);
    dataColPtr_p->putArrayColumnCellsV (rownrs, arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::putColumnSlice (const Slicer& ns,
					 const void* arrayPtr)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w',
                         static_cast<const Array<T>*>(arrayPtr)->shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkValueLength ((const Array<T>*)arrayPtr);
    checkWriteLock (True);
    dataColPtr_p->putColumnSliceV (ns, (const Array<T>*)arrayPtr);
    autoReleaseLock();
}

template<class T>
void ArrayColumnData<T>::putColumnSliceCells (const RefRows& rownrs,
					      const Slicer& ns,
					      const void* arrayPtr)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownrs,
                         static_cast<const Array<T>*>(arrayPtr)->shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkValueLength ((const Array<T>*)arrayPtr);
    checkWriteLock (True);
    dataColPtr_p->putColumnSliceCellsV (rownrs, ns, arrayPtr);
    autoReleaseLock();
}


//# Check if the array shape is set correctly.
//# It is only possible for non-FixedShape arrays.
//# Its dimensionality must match the possible #dim in the column description.
template<class T>
void ArrayColumnData<T>::checkShape (const IPosition& shape) const
{
    if ((columnDesc().options() & ColumnDesc::FixedShape)
	                               == ColumnDesc::FixedShape) {
	throw (TableInvOper
	     ("ArrayColumn::setShape only possible for non-FixedShape arrays"
              " of column " + colDescPtr_p->name()));
    }
    if (columnDesc().ndim() > 0) {
	if (Int(shape.nelements()) != columnDesc().ndim()) {
	    throw (TableInvOper
		   ("ArrayColumn::setShape: mismatch in #dim of array"
                    " of column " + colDescPtr_p->name()));
	}
    }
}


//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
template<class T>
void ArrayColumnData<T>::putFileDerived (AipsIO& ios)
{
    ios << (uInt)1;                  // class version 1
    ios << dataManPtr_p->sequenceNr();
    ios << shapeColDef_p;
    if (shapeColDef_p) {
	ios << shapeCol_p;
    }
}

template<class T>
void ArrayColumnData<T>::getFileDerived (AipsIO& ios,
					 const ColumnSet& colset)
{
    uInt version;
    ios >> version;
    uInt seqnr;
    ios >> seqnr;
    ios >> shapeColDef_p;
    if (shapeColDef_p) {
	ios >> shapeCol_p;
    }
    dataManPtr_p = colset.getDataManager (seqnr);
    createDataManagerColumn();
}

} //# NAMESPACE CASACORE - END

#endif
