//# ScaColData.cc: Access to a table column containing scalars
//# Copyright (C) 1994,1995,1996,1997,1998,2000,2001
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

#ifndef TABLES_SCACOLDATA_TCC
#define TABLES_SCACOLDATA_TCC

#include <casacore/tables/Tables/ScaColData.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/TableTrace.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/IO/AipsIO.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ScalarColumnData<T>::ScalarColumnData (const ScalarColumnDesc<T>* cd,
				       ColumnSet* csp)
: PlainColumn  (cd, csp),
  scaDescPtr_p (cd),
  undefFlag_p  (false),
  undefVal_p   (cd->defaultValue())
{
    if ((cd->options() & ColumnDesc::Undefined)  ==  ColumnDesc::Undefined) {
	undefFlag_p = true;
    }
}

template<class T>
ScalarColumnData<T>::~ScalarColumnData()
{}


template<class T>
void ScalarColumnData<T>::createDataManagerColumn()
{
    dataColPtr_p = dataManPtr_p->createScalarColumn
	            (colDescPtr_p->name(),
		     colDescPtr_p->dataType(),
		     colDescPtr_p->dataTypeId());
    //# Set the maximum length of an item.
    dataColPtr_p->setMaxLength (colDescPtr_p->maxLength());
}


template<class T>
void ScalarColumnData<T>::initialize (rownr_t startRow, rownr_t endRow)
{
    if (colDescPtr_p->dataType() != TpOther) {
	for (rownr_t i=startRow; i<=endRow; i++) {
	    dataColPtr_p->put (i, &(scaDescPtr_p->defaultValue()));
	}
    }
}	

template<class T>
bool ScalarColumnData<T>::isDefined (rownr_t rownr) const
{
    if (!undefFlag_p) {
	return true;
    }
    T val;
    dataColPtr_p->get (rownr, &val);
    return ( (!(val == undefVal_p)));
}


template<class T>
void ScalarColumnData<T>::get (rownr_t rownr, void* val) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownr);
    }
    checkReadLock (true);
    dataColPtr_p->get (rownr, static_cast<T*>(val));
    autoReleaseLock();
}


template<class T>
void ScalarColumnData<T>::getScalarColumn (ArrayBase& val) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r');
    }
    if (val.ndim() != 1  ||  val.nelements() != nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::getScalarColumn"));
    }
    checkReadLock (true);
    dataColPtr_p->getScalarColumnV (val);
    autoReleaseLock();
}

template<class T>
void ScalarColumnData<T>::getScalarColumnCells (const RefRows& rownrs,
						ArrayBase& val) const
{
    if (rtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'r', rownrs);
    }
    if (val.ndim() != 1  ||  val.nelements() != rownrs.nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::getScalarColumnCells"));
    }
    checkReadLock (true);
    dataColPtr_p->getScalarColumnCellsV (rownrs, val);
    autoReleaseLock();
}


template<class T>
void ScalarColumnData<T>::put (rownr_t rownr, const void* val)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownr);
    }
    checkValueLength (static_cast<const T*>(val));
    checkWriteLock (true);
    dataColPtr_p->put (rownr, static_cast<const T*>(val));
    autoReleaseLock();
}

template<class T>
void ScalarColumnData<T>::putScalarColumn (const ArrayBase& val)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w');
    }
    if (val.ndim() != 1  ||  val.nelements() != nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::putColumn"));
    }
    checkValueLength (static_cast<const Array<T>*>(&val));
    checkWriteLock (true);
    dataColPtr_p->putScalarColumnV (val);
    autoReleaseLock();
}

template<class T>
void ScalarColumnData<T>::putScalarColumnCells (const RefRows& rownrs,
						const ArrayBase& val)
{
    if (wtraceColumn_p) {
      TableTrace::trace (traceId(), columnDesc().name(), 'w', rownrs);
    }
    if (val.ndim() != 1  ||  val.nelements() != rownrs.nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::putColumn"));
    }
    checkValueLength (static_cast<const Array<T>*>(&val));
    checkWriteLock (true);
    dataColPtr_p->putScalarColumnCellsV (rownrs, val);
    autoReleaseLock();
}


template<class T>
void ScalarColumnData<T>::makeSortKey (Sort& sortobj,
				       CountedPtr<BaseCompare>& cmpObj,
				       int32_t order,
				       CountedPtr<ArrayBase>& dataSave)
{
    //# Get the data as a column.
    //# Save the pointer to the vector for deletion by freeSortKey().
    Vector<T>* vecPtr = new Vector<T>(nrow());
    dataSave = vecPtr;
    getScalarColumn (*vecPtr);
    fillSortKey (vecPtr, sortobj, cmpObj, order);
}

template<class T>
void ScalarColumnData<T>::makeRefSortKey (Sort& sortobj,
                                          CountedPtr<BaseCompare>& cmpObj,
					  int32_t order,
					  const Vector<rownr_t>& rownrs,
					  CountedPtr<ArrayBase>& dataSave)
{
    //# Get the data as a column.
    Vector<T>* vecPtr = new Vector<T>(rownrs.size());
    dataSave = vecPtr;
    getScalarColumnCells (rownrs, *vecPtr);
    fillSortKey (vecPtr, sortobj, cmpObj, order);
}

template<class T>
void ScalarColumnData<T>::fillSortKey (const Vector<T>* vecPtr,
				       Sort& sortobj,
                                       CountedPtr<BaseCompare>& cmpObj,
				       int32_t order)
{
    //# Pass the real vector storage as the sort data.
    //# Use the compare function if given, otherwise pass data type.
    //# Throw an exception if no compare function is given for
    //# an unknown data type.
    AlwaysAssert (vecPtr->contiguousStorage(), AipsError);
    if (cmpObj.null()) {
        cmpObj = new ObjCompare<T>();
    }
    sortobj.sortKey (vecPtr->data(), cmpObj, sizeof(T),
		     order == Sort::Descending  ?  Sort::Descending
		                                 : Sort::Ascending);
}

template<class T>
void ScalarColumnData<T>::allocIterBuf (void*& lastVal, void*& curVal,
					CountedPtr<BaseCompare>& cmpObj)
{
    T* valp = new T[2];
    lastVal = valp;
    curVal  = valp + 1;
    if (cmpObj.null()) {
	cmpObj = new ObjCompare<T>;
    }
}

template<class T>
void ScalarColumnData<T>::freeIterBuf (void*& lastVal, void*& curVal)
{
    delete [] static_cast<T*>(lastVal);
    lastVal = 0;
    curVal  = 0;
}


//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
template<class T>
void ScalarColumnData<T>::putFileDerived (AipsIO& ios)
{
    ios << (uint32_t)1;                  // class version 1
    ios << dataManPtr_p->sequenceNr();
}

template<class T>
void ScalarColumnData<T>::getFileDerived (AipsIO& ios,
					  const ColumnSet& colset)
{
    uint32_t version;
    ios >> version;
    uint32_t seqnr;
    ios >> seqnr;
    dataManPtr_p = colset.getDataManager (seqnr);
    createDataManagerColumn();
}

} //# NAMESPACE CASACORE - END

#endif
