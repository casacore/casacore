//# BaseTabIter.cc: Base class for table iterator
//# Copyright (C) 1994,1995,1996,1997
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

#include <casacore/tables/Tables/BaseTabIter.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/RefTable.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// BaseTableIterator is the base class for the table iterators.
// It is a letter class of the envelope TableIterator.
//
// BaseTableIterator iterates by sorting the table in the required
// order and then creating a RefTable for each step containing the row
// numbers of the rows for that iteration step.


BaseTableIterator::BaseTableIterator (BaseTable* btp,
				      const Block<String>& keys,
				      const Block<CountedPtr<BaseCompare> >& cmp,
				      const Block<Int>& order,
				      int option)
: lastRow_p (0),
  nrkeys_p  (keys.nelements()),
  lastVal_p (keys.nelements()),
  curVal_p  (keys.nelements()),
  colPtr_p  (keys.nelements()),
  cmpObj_p  (cmp)
{
    // If needed sort the table in order of the iteration keys.
    // The passed in compare functions are for the iteration.
    if (option == TableIterator::NoSort) {
	sortTab_p = btp;
    }else{
	Sort::Option sortopt = Sort::QuickSort;
	if (option == TableIterator::HeapSort) {
	    sortopt = Sort::HeapSort;
	} else if (option == TableIterator::InsSort) {
	    sortopt = Sort::InsSort;
	}
	Block<Int> ord(nrkeys_p, Sort::Ascending);
	for (uInt i=0; i<nrkeys_p; i++) {
	    if (order[i] == TableIterator::Descending) {
		ord[i] = Sort::Descending;
	    }
	}
	sortTab_p = (RefTable*) (btp->sort (keys, cmpObj_p, ord, sortopt));
    }
    sortTab_p->link();
    // Get the pointers to the BaseColumn object.
    // Get a buffer to hold the current and last value per column.
    for (uInt i=0; i<nrkeys_p; i++) {
	colPtr_p[i] = sortTab_p->getColumn (keys[i]);
	colPtr_p[i]->allocIterBuf (lastVal_p[i], curVal_p[i], cmpObj_p[i]);
    }
}


BaseTableIterator* BaseTableIterator::clone() const
{
    BaseTableIterator* newbti = new BaseTableIterator (*this);
    return newbti;
}


BaseTableIterator::BaseTableIterator (const BaseTableIterator& that)
: lastRow_p (0),
  nrkeys_p  (that.nrkeys_p),
  lastVal_p (that.nrkeys_p),
  curVal_p  (that.nrkeys_p),
  colPtr_p  (that.colPtr_p),
  cmpObj_p  (that.cmpObj_p)
{
    // Get the pointers to the BaseColumn object.
    // Get a buffer to hold the current and last value per column.
    for (uInt i=0; i<nrkeys_p; i++) {
	colPtr_p[i]->allocIterBuf (lastVal_p[i], curVal_p[i], cmpObj_p[i]);
    }
    // Link against the table (ie. increase its ref.count).
    sortTab_p = that.sortTab_p;
    sortTab_p->link();
}


BaseTableIterator::~BaseTableIterator()
{
    // Delete the value buffers.
    for (uInt i=0; i<nrkeys_p; i++) {
	colPtr_p[i]->freeIterBuf (lastVal_p[i], curVal_p[i]);
    }
    // Unlink from the table and delete it.
    BaseTable::unlink (sortTab_p);
}


void BaseTableIterator::reset()
{
    lastRow_p = 0;
}


BaseTable* BaseTableIterator::next()
{
    uInt i;
    // Allocate a RefTable to represent the rows in the iteration group.
    RefTable* itp = sortTab_p->makeRefTable (False, 0);
    if (lastRow_p >= sortTab_p->nrow()) {
	return itp;                              // the end of the table
    }
    // Add the last found rownr to this iteration group.
    itp->addRownr (lastRow_p);
    for (i=0; i<nrkeys_p; i++) {
	colPtr_p[i]->get (lastRow_p, lastVal_p[i]);
    }
    Bool match;
    uInt nr = sortTab_p->nrow();
    while (++lastRow_p < nr) {
	match = True;
	for (i=0; i<nrkeys_p; i++) {
	    colPtr_p[i]->get (lastRow_p, curVal_p[i]);
	    if (cmpObj_p[i]->comp (curVal_p[i], lastVal_p[i])  != 0) {
		match = False;
		break;
	    }
	}
	if (!match) {
	    break;
	}
	itp->addRownr (lastRow_p);
    }
    //# Adjust rownrs in case source table is already a RefTable.
    Vector<uInt>& rownrs = *(itp->rowStorage());
    sortTab_p->adjustRownrs (itp->nrow(), rownrs, False);
    return itp;
}

} //# NAMESPACE CASACORE - END

