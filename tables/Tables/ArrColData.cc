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
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayColumnData::ArrayColumnData (const ArrayColumnDescBase* cd,
                                  ColumnSet* csp)
: PlainColumn  (cd, csp),
  arrDescPtr_p (cd),
  shapeColDef_p(False),
  shapeCol_p   ()
{
    if (cd->shape().nelements() > 0) {
	setShapeColumn (cd->shape());
    }
    checkValueLength_p = (columnDesc().dataType() == TpString  &&
                          columnDesc().maxLength() > 0);
}

ArrayColumnData::~ArrayColumnData()
{}


//# Create the data manager column for this column.
//# The shape of a fixed shape array has to be defined in advance.
void ArrayColumnData::createDataManagerColumn()
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
void ArrayColumnData::initialize (rownr_t, rownr_t)
{}

uInt ArrayColumnData::ndimColumn() const
{
    Int ndim = columnDesc().ndim();
    return (ndim > 0  ?  ndim : shapeCol_p.nelements());
}
IPosition ArrayColumnData::shapeColumn() const
    { return shapeCol_p; }

void ArrayColumnData::setShapeColumn (const IPosition& shp)
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

Bool ArrayColumnData::isDefined (rownr_t rownr) const
{
    return dataColPtr_p->isShapeDefined(rownr);
}
uInt ArrayColumnData::ndim (rownr_t rownr) const
{
    return dataColPtr_p->ndim(rownr);
}
IPosition ArrayColumnData::shape (rownr_t rownr) const
{
    return dataColPtr_p->shape(rownr);
}
IPosition ArrayColumnData::tileShape (rownr_t rownr) const
{
    return dataColPtr_p->tileShape(rownr);
}


void ArrayColumnData::setShape (rownr_t rownr, const IPosition& shp)
{
    checkShape (shp);
    checkWriteLock (True);
    dataColPtr_p->setShape (rownr, shp);
    autoReleaseLock();
}
void ArrayColumnData::setShape (rownr_t rownr, const IPosition& shp,
                                const IPosition& tileShp)
{
    checkShape (shp);
    checkWriteLock (True);
    dataColPtr_p->setShapeTiled (rownr, shp, tileShp);
    autoReleaseLock();
}

Bool ArrayColumnData::canChangeShape() const
{
    return dataColPtr_p->canChangeShape();
}


void ArrayColumnData::getArray (rownr_t rownr, ArrayBase& array) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownr,
                         array.shape());
    }
    checkReadLock (True);
    dataColPtr_p->getArrayV (rownr, array);
    autoReleaseLock();
}

void ArrayColumnData::getSlice (rownr_t rownr, const Slicer& ns,
                                ArrayBase& array) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownr,
                         array.shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkReadLock (True);
    dataColPtr_p->getSliceV (rownr, ns, array);
    autoReleaseLock();
}


void ArrayColumnData::putArray (rownr_t rownr, const ArrayBase& array)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownr,
                         array.shape());
    }
    if (checkValueLength_p) {
      checkValueLength (static_cast<const Array<String>*>(&array));
    }
    checkWriteLock (True);
    dataColPtr_p->putArrayV (rownr, array);
    autoReleaseLock();
}

void ArrayColumnData::putSlice (rownr_t rownr, const Slicer& ns,
                                const ArrayBase& array)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownr,
                         array.shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    if (checkValueLength_p) {
      checkValueLength (static_cast<const Array<String>*>(&array));
    }
    checkWriteLock (True);
    dataColPtr_p->putSliceV (rownr, ns, array);
    autoReleaseLock();
}



//# Get or put the column by iterating through the array and getting the
//# column array for each row.

void ArrayColumnData::getArrayColumn (ArrayBase& array) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r',
                         array.shape());
    }
    checkReadLock (True);
    dataColPtr_p->getArrayColumnV (array);
    autoReleaseLock();
}


void ArrayColumnData::getArrayColumnCells (const RefRows& rownrs,
                                           ArrayBase& array) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownrs,
                         array.shape());
    }
    checkReadLock (True);
    dataColPtr_p->getArrayColumnCellsV (rownrs, array);
    autoReleaseLock();
}

void ArrayColumnData::getColumnSlice (const Slicer& ns,
                                      ArrayBase& array) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r',
                         array.shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkReadLock (True);
    dataColPtr_p->getColumnSliceV (ns, array);
    autoReleaseLock();
}

void ArrayColumnData::getColumnSliceCells (const RefRows& rownrs,
                                           const Slicer& ns,
                                           ArrayBase& array) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownrs,
                         array.shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    checkReadLock (True);
    dataColPtr_p->getColumnSliceCellsV (rownrs, ns, array);
    autoReleaseLock();
}

void ArrayColumnData::putArrayColumn (const ArrayBase& array)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w',
                         array.shape());
    }
    if (checkValueLength_p) {
      checkValueLength (static_cast<const Array<String>*>(&array));
    }
    checkWriteLock (True);
    dataColPtr_p->putArrayColumnV (array);
    autoReleaseLock();
}

void ArrayColumnData::putArrayColumnCells (const RefRows& rownrs,
                                           const ArrayBase& array)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownrs,
                         array.shape());
    }
    if (checkValueLength_p) {
      checkValueLength (static_cast<const Array<String>*>(&array));
    }
    checkWriteLock (True);
    dataColPtr_p->putArrayColumnCellsV (rownrs, array);
    autoReleaseLock();
}

void ArrayColumnData::putColumnSlice (const Slicer& ns,
                                      const ArrayBase& array)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w',
                         array.shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    if (checkValueLength_p) {
      checkValueLength (static_cast<const Array<String>*>(&array));
    }
    checkWriteLock (True);
    dataColPtr_p->putColumnSliceV (ns, array);
    autoReleaseLock();
}

void ArrayColumnData::putColumnSliceCells (const RefRows& rownrs,
                                           const Slicer& ns,
                                           const ArrayBase& array)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownrs,
                         array.shape(),
                         ns.start(), ns.end(), ns.stride());
    }
    if (checkValueLength_p) {
      checkValueLength (static_cast<const Array<String>*>(&array));
    }
    checkWriteLock (True);
    dataColPtr_p->putColumnSliceCellsV (rownrs, ns, array);
    autoReleaseLock();
}


//# Check if the array shape is set correctly.
//# It is only possible for non-FixedShape arrays.
//# Its dimensionality must match the possible #dim in the column description.
void ArrayColumnData::checkShape (const IPosition& shape) const
{
    if ((columnDesc().options() & ColumnDesc::FixedShape)
	                               != ColumnDesc::FixedShape) {
      if (columnDesc().ndim() > 0) {
	if (Int(shape.nelements()) != columnDesc().ndim()) {
	    throw (TableInvOper
		   ("ArrayColumn::setShape: mismatch in #dim of array"
                    " of column " + colDescPtr_p->name()));
	}
      }
    }
}


//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void ArrayColumnData::putFileDerived (AipsIO& ios)
{
    ios << (uInt)1;                  // class version 1
    ios << dataManPtr_p->sequenceNr();
    ios << shapeColDef_p;
    if (shapeColDef_p) {
	ios << shapeCol_p;
    }
}

void ArrayColumnData::getFileDerived (AipsIO& ios,
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

