//# ScaColData.cc: Access to a table column containing scalars
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

#include <aips/Tables/ScaColData.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ColumnSet.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/RefRows.h>
#include <aips/Arrays/Vector.h>
#include <aips/Tables/DataManager.h>
#include <aips/Utilities/ValType.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/Sort.h>
#include <aips/IO/AipsIO.h>



template<class T>
ScalarColumnData<T>::ScalarColumnData (const ScalarColumnDesc<T>* cd,
				       ColumnSet* csp)
: PlainColumn  (cd, csp),
  scaDescPtr_p (cd),
  undefFlag_p  (False),
  undefVal_p   (cd->defaultValue())
{
    if ((cd->options() & ColumnDesc::Undefined)  ==  ColumnDesc::Undefined) {
	undefFlag_p = True;
    }
}

template<class T>
ScalarColumnData<T>::~ScalarColumnData()
{}


template<class T>
void ScalarColumnData<T>::createDataManagerColumn()
{
    dataColPtr_p = dataManPtr_p->createScalarColumn
	            (originalName_p, colDescPtr_p->dataType(),
		     colDescPtr_p->dataTypeId());
    //# Set the maximum length of an item.
    dataColPtr_p->setMaxLength (colDescPtr_p->maxLength());
}


template<class T>
Bool ScalarColumnData<T>::canAccessScalarColumn (Bool& reask) const
{
    return dataColPtr_p->canAccessScalarColumn (reask);
}
template<class T>
Bool ScalarColumnData<T>::canAccessScalarColumnCells (Bool& reask) const
{
    return dataColPtr_p->canAccessScalarColumnCells (reask);
}


template<class T>
void ScalarColumnData<T>::initialize (uInt startRow, uInt endRow)
{
    if (colDescPtr_p->dataType() != TpOther) {
	for (uInt i=startRow; i<=endRow; i++) {
	    dataColPtr_p->put (i, &(scaDescPtr_p->defaultValue()));
	}
    }
}	

template<class T>
Bool ScalarColumnData<T>::isDefined (uInt rownr) const
{
    if (!undefFlag_p) {
	return True;
    }
    T val;
    dataColPtr_p->get (rownr, &val);
    return (ToBool (!(val == undefVal_p)));
}


template<class T>
void ScalarColumnData<T>::get (uInt rownr, void* val) const
{
    checkLock (FileLocker::Read, True);
    dataColPtr_p->get (rownr, (T*)val);
    autoReleaseLock();
}


template<class T>
void ScalarColumnData<T>::getScalarColumn (void* val) const
{
    Vector<T>* vecPtr = (Vector<T>*)val;
    if (vecPtr->nelements() != nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::getScalarColumn"));
    }
    checkLock (FileLocker::Read, True);
    dataColPtr_p->getScalarColumnV (vecPtr);
    autoReleaseLock();
}

// Remove eventually - needed for g++
typedef Vector<uInt> forgnugpp_scacoldata;

template<class T>
void ScalarColumnData<T>::getScalarColumnCells (const RefRows& rownrs,
						void* val) const
{
    Vector<T>& vec = *(Vector<T>*)val;
    uInt nr = rownrs.nrow();
    if (vec.nelements() != nr) {
	throw (TableArrayConformanceError("ScalarColumnData::getColumnCells"));
    }
    checkLock (FileLocker::Read, True);
    dataColPtr_p->getScalarColumnCellsV (rownrs, &vec);
    autoReleaseLock();
}


template<class T>
void ScalarColumnData<T>::put (uInt rownr, const void* val)
{
    checkValueLength ((const T*)val);
    checkLock (FileLocker::Write, True);
    dataColPtr_p->put (rownr, (const T*)val);
    autoReleaseLock();
}

template<class T>
void ScalarColumnData<T>::putScalarColumn (const void* val)
{
    const Vector<T>* vecPtr = (const Vector<T>*)val;
    if (vecPtr->nelements() != nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::putColumn"));
    }
    checkValueLength (vecPtr);
    checkLock (FileLocker::Write, True);
    dataColPtr_p->putScalarColumnV (vecPtr);
    autoReleaseLock();
}

template<class T>
void ScalarColumnData<T>::putScalarColumnCells (const RefRows& rownrs,
						const void* val)
{
    const Vector<T>& vec = *(const Vector<T>*)val;
    if (vec.nelements() != rownrs.nrow()) {
	throw (TableArrayConformanceError("ScalarColumnData::putColumn"));
    }
    checkValueLength (&vec);
    checkLock (FileLocker::Write, True);
    dataColPtr_p->putScalarColumnCellsV (rownrs, &vec);
    autoReleaseLock();
}


template<class T>
void ScalarColumnData<T>::makeSortKey (Sort& sortobj,
				       ObjCompareFunc* cmpFunc,
				       Int order,
				       const void*& dataSave)
{
    //#// Optimal is to ask the data manager for a pointer to
    //#// the consecutive data. Often this may succeed.
    //# Get the data as a column.
    //# Save the pointer to the vector for deletion by freeSortKey().
    dataSave = 0;
    uInt nrrow = nrow();
    Vector<T>* vecPtr = new Vector<T>(nrrow);
    if (vecPtr == 0) {
	throw (AllocError ("ScalarColumnData::makeSortKey", 1));
    }
    Bool reask;
    if (canAccessScalarColumn (reask)) {
	getScalarColumn (vecPtr);
    }else{
	checkLock (FileLocker::Read, True);
	for (uInt i=0; i<nrrow; i++) {
	    dataColPtr_p->get (i,  &(*vecPtr)(i));
	}
	autoReleaseLock();
    }
    dataSave = vecPtr;
    fillSortKey (vecPtr, sortobj, cmpFunc, order);
}

template<class T>
void ScalarColumnData<T>::makeRefSortKey (Sort& sortobj,
					  ObjCompareFunc* cmpFunc,
					  Int order,
					  const Vector<uInt>& rownrs,
					  const void*& dataSave)
{
    //#// Optimal is to ask the data manager for a pointer to
    //#// the consecutive data. Often this may succeed.
    //# Get the data as a column.
    dataSave = 0;
    uInt nrrow = rownrs.nelements();
    Vector<T>* vecPtr = new Vector<T>(nrrow);
    if (vecPtr == 0) {
	throw (AllocError ("ScalarColumnData::makeRefSortKey", 1));
    }
    Bool reask;
    if (canAccessScalarColumnCells (reask)) {
	getScalarColumnCells (rownrs, vecPtr);
    }else{
	checkLock (FileLocker::Read, True);
	for (uInt i=0; i<nrrow; i++) {
	    dataColPtr_p->get (rownrs(i),  &(*vecPtr)(i));
	}
	autoReleaseLock();
    }
    dataSave = vecPtr;
    fillSortKey (vecPtr, sortobj, cmpFunc, order);
}

template<class T>
void ScalarColumnData<T>::fillSortKey (const Vector<T>* vecPtr,
				       Sort& sortobj,
				       ObjCompareFunc* cmpFunc,
				       Int order)
{
    //# Pass the real vector storage as the sort data.
    //# Use the compare function if given, otherwise pass data type.
    //# Throw an exception if no compare function is given for
    //# an unknown data type.
    Bool deleteIt;
    const T* datap = vecPtr->getStorage (deleteIt);
    if (cmpFunc == 0) {
	cmpFunc = ObjCompare<T>::compare;
    }
    sortobj.sortKey (datap, cmpFunc, sizeof(T),
		     order == Sort::Descending  ?  Sort::Descending
		                                 : Sort::Ascending);
    vecPtr->freeStorage (datap, deleteIt);
}

template<class T>
void ScalarColumnData<T>::freeSortKey (const void*& dataSave)
{
    if (dataSave != 0) {
	delete (Vector<T>*)dataSave;
    }
    dataSave = 0;
}

template<class T>
void ScalarColumnData<T>::allocIterBuf (void*& lastVal, void*& curVal,
					ObjCompareFunc*& cmpFunc)
{
    T* valp = new T[2];
    if (valp == 0) {
	throw (AllocError ("ScalarColumnData::AllocIterBuf", 1));
    }
    lastVal = valp;
    curVal  = valp + 1;
    if (cmpFunc == 0) {
	cmpFunc = ObjCompare<T>::compare;
    }
}

template<class T>
void ScalarColumnData<T>::freeIterBuf (void*& lastVal, void*& curVal)
{
    delete [] lastVal;
    lastVal = 0;
    curVal  = 0;
}


//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
template<class T>
void ScalarColumnData<T>::putFileDerived (AipsIO& ios)
{
    ios << (uInt)1;                  // class version 1
    ios << dataManPtr_p->sequenceNr();
}

template<class T>
void ScalarColumnData<T>::getFileDerived (AipsIO& ios,
					  const ColumnSet& colset)
{
    uInt version;
    ios >> version;
    uInt seqnr;
    ios >> seqnr;
    dataManPtr_p = colset.getDataManager (seqnr);
    createDataManagerColumn();
}
