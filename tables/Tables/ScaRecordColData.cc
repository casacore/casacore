//# ScaRecordColData.cc: Access to a table column containing scalar records
//# Copyright (C) 1998,2000,2001
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

#include <casacore/tables/Tables/ScaRecordColData.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ScalarRecordColumnData::ScalarRecordColumnData
                                   (const ScalarRecordColumnDesc* cd,
				    ColumnSet* csp)
: PlainColumn (cd, csp)
{}

ScalarRecordColumnData::~ScalarRecordColumnData()
{}


void ScalarRecordColumnData::createDataManagerColumn()
{
    // The records are stored as an array.
    // Because their lengths can vary, it has to be indirect.
    dataColPtr_p = dataManPtr_p->createIndArrColumn
                                (colDescPtr_p->name(), TpUChar, "");
}


void ScalarRecordColumnData::initialize (rownr_t, rownr_t)
{}	

Bool ScalarRecordColumnData::isDefined (rownr_t) const
{
    return True;
}


void ScalarRecordColumnData::get (rownr_t rownr, void* val) const
{
    checkReadLock (True);
    getRecord (rownr, *(TableRecord*)val);
    autoReleaseLock();
}

void ScalarRecordColumnData::getScalarColumn (ArrayBase& val) const
{
    Vector<TableRecord>& vec = static_cast<Vector<TableRecord>&>(val);
    rownr_t nr = nrow();
    if (vec.nelements() != nr) {
	throw (TableArrayConformanceError
                                 ("ScalarRecordColumnData::getScalarColumn"));
    }
    checkReadLock (True);
    for (rownr_t i=0; i<nr; i++) {
	getRecord (i, vec(i));
    }
    autoReleaseLock();
}

void ScalarRecordColumnData::getScalarColumnCells (const RefRows& rownrs,
						   ArrayBase& val) const
{
    Vector<TableRecord>& vec = static_cast<Vector<TableRecord>&>(val);
    if (vec.nelements() != rownrs.nrow()) {
	throw (TableArrayConformanceError
                                 ("ScalarRecordColumnData::getColumnCells"));
    }
    checkReadLock (True);
    RefRowsSliceIter iter(rownrs);
    rownr_t i=0;
    while (! iter.pastEnd()) {
	rownr_t rownr = iter.sliceStart();
	rownr_t end = iter.sliceEnd();
	rownr_t incr = iter.sliceIncr();
	while (rownr <= end) {
	    getRecord (rownr, vec(i++));
	    rownr += incr;
	}
	iter++;
    }
    autoReleaseLock();
}


void ScalarRecordColumnData::put (rownr_t rownr, const void* val)
{
    checkWriteLock (True);
    putRecord (rownr, *(const TableRecord*)val);
    autoReleaseLock();
}

void ScalarRecordColumnData::putScalarColumn (const ArrayBase& val)
{
    const Vector<TableRecord>& vec = static_cast<const Vector<TableRecord>&>(val);
    rownr_t nr = nrow();
    if (vec.nelements() != nr) {
	throw (TableArrayConformanceError
                                 ("ScalarRecordColumnData::putScalarColumn"));
    }
    checkWriteLock (True);
    for (rownr_t i=0; i<nr; i++) {
	putRecord (i, vec(i));
    }
    autoReleaseLock();
}

void ScalarRecordColumnData::putScalarColumnCells (const RefRows& rownrs,
						   const ArrayBase& val)
{
    const Vector<TableRecord>& vec = static_cast<const Vector<TableRecord>&>(val);
    if (vec.nelements() != rownrs.nrow()) {
	throw (TableArrayConformanceError
                                 ("ScalarRecordColumnData::putColumnCells"));
    }
    checkWriteLock (True);
    RefRowsSliceIter iter(rownrs);
    rownr_t i=0;
    while (! iter.pastEnd()) {
	rownr_t rownr = iter.sliceStart();
	rownr_t end = iter.sliceEnd();
	rownr_t incr = iter.sliceIncr();
	while (rownr <= end) {
	    putRecord (rownr, vec(i++));
	    rownr += incr;
	}
	iter++;
    }
    autoReleaseLock();
}


void ScalarRecordColumnData::getRecord (rownr_t rownr, TableRecord& rec) const
{
    if (! dataColPtr_p->isShapeDefined (rownr)) {
	rec = TableRecord();
    } else {
	IPosition shape = dataColPtr_p->shape (rownr);
	AlwaysAssert (shape.nelements() == 1, AipsError);
	Array<uChar> data(shape);
	dataColPtr_p->getArrayV (rownr, data);
	Bool deleteIt;
	const uChar* buf = data.getStorage (deleteIt);
	MemoryIO memio (buf, shape(0));
	AipsIO aio(&memio);
	rec.getRecord (aio, TableAttr(dataManager()->table()));
	data.freeStorage (buf, deleteIt);
    }
}

void ScalarRecordColumnData::putRecord (rownr_t rownr, const TableRecord& rec)
{
    MemoryIO memio;
    AipsIO aio(&memio);
    rec.putRecord (aio, TableAttr(dataManager()->table().tableName()));
    IPosition shape (1, Int(memio.length()));
    Vector<uChar> data(shape, (uChar*)(memio.getBuffer()), SHARE);
    dataColPtr_p->setShape (rownr, shape);
    dataColPtr_p->putArrayV (rownr, data);
}


void ScalarRecordColumnData::makeSortKey (Sort&,
					  std::shared_ptr<BaseCompare>&,
					  Int,
					  std::shared_ptr<ArrayBase>&)
{
    throw (TableError ("Sorting on a column containing records "
		       "is not possible"));
}

void ScalarRecordColumnData::makeRefSortKey (Sort&,
                                             std::shared_ptr<BaseCompare>&,
					     Int,
					     const Vector<rownr_t>&,
					     std::shared_ptr<ArrayBase>&)
{
    throw (TableError ("Sorting on a column containing records "
		       "is not possible"));
}

void ScalarRecordColumnData::allocIterBuf (void*&, void*&,
                                           std::shared_ptr<BaseCompare>&)
{
    throw (TableError ("Iterating on a column containing records "
		       "is not possible"));
}

void ScalarRecordColumnData::freeIterBuf (void*& lastVal, void*& curVal)
{
    lastVal = 0;
    curVal  = 0;
}


//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void ScalarRecordColumnData::putFileDerived (AipsIO& ios)
{
    ios << (uInt)1;                  // class version 1
    ios << dataManPtr_p->sequenceNr();
}

void ScalarRecordColumnData::getFileDerived (AipsIO& ios,
					     const ColumnSet& colset)
{
    uInt version;
    ios >> version;
    uInt seqnr;
    ios >> seqnr;
    dataManPtr_p = colset.getDataManager (seqnr);
    createDataManagerColumn();
}

} //# NAMESPACE CASACORE - END
