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
    int option,
    bool cacheIterationBoundaries)
: lastRow_p (0),
  nrkeys_p  (keys.nelements()),
  keyChangeAtLastNext_p(""),
  colPtr_p  (keys.nelements()),
  cmpObj_p  (cmp),
  lastVal_p (keys.nelements()),
  curVal_p  (keys.nelements()),
  sortIterBoundaries_p   (nullptr),
  sortIterKeyIdxChange_p (nullptr),
  aRefTable_p(nullptr)
{
    // If needed sort the table in order of the iteration keys.
    // The passed in compare functions are for the iteration.
    if (option == TableIterator::NoSort) {
        sortTab_p = btp;
    }else{
        Sort::Option sortopt = Sort::QuickSort;
        if (option == TableIterator::HeapSort) {
            sortopt = Sort::HeapSort;
        } else if (option == TableIterator::ParSort) {
            sortopt = Sort::ParSort;
        } else if (option == TableIterator::InsSort) {
            sortopt = Sort::InsSort;
        }
        Block<Int> ord(nrkeys_p, Sort::Ascending);
        for (uInt i=0; i<nrkeys_p; i++) {
            if (order[i] == TableIterator::Descending) {
                ord[i] = Sort::Descending;
            }
        }
        if(cacheIterationBoundaries)
        {
            sortIterBoundaries_p   = std::make_shared<Vector<rownr_t>>();
            sortIterKeyIdxChange_p = std::make_shared<Vector<size_t>>();
        }
        sortTab_p = (RefTable*) (btp->sort (keys, cmpObj_p, ord, sortopt,
                                            sortIterBoundaries_p,
                                            sortIterKeyIdxChange_p ));
    }
    sortTab_p->link();
    // Get the pointers to the BaseColumn object.
    // Get a buffer to hold the current and last value per column.
    for (uInt i=0; i<nrkeys_p; i++) {
        colPtr_p[i] = sortTab_p->getColumn (keys[i]);
        colPtr_p[i]->allocIterBuf (lastVal_p[i], curVal_p[i], cmpObj_p[i]);
    }
    if(cacheIterationBoundaries)
    {
        sortIterBoundariesIt_p   = sortIterBoundaries_p->begin();
        sortIterKeyIdxChangeIt_p = sortIterKeyIdxChange_p->begin();
        aRefTable_p = sortTab_p->makeRefTable (False, 0);
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
  colPtr_p  (that.colPtr_p),
  cmpObj_p  (that.cmpObj_p),
  lastVal_p (that.nrkeys_p),
  curVal_p  (that.nrkeys_p),
  sortIterBoundaries_p   (that.sortIterBoundaries_p),
  sortIterKeyIdxChange_p (that.sortIterKeyIdxChange_p),
  aRefTable_p(nullptr)
{
    // Get the pointers to the BaseColumn object.
    // Get a buffer to hold the current and last value per column.
    for (uInt i=0; i<nrkeys_p; i++) {
        colPtr_p[i]->allocIterBuf (lastVal_p[i], curVal_p[i], cmpObj_p[i]);
    }
    // Link against the table (ie. increase its ref.count).
    sortTab_p = that.sortTab_p;
    sortTab_p->link();
    if(sortIterBoundaries_p)
    {
        sortIterBoundariesIt_p = sortIterBoundaries_p->begin();
    }
    if(sortIterKeyIdxChange_p)
    {
        sortIterKeyIdxChangeIt_p = sortIterKeyIdxChange_p->begin();
    }
    if(sortIterBoundaries_p && sortIterKeyIdxChange_p)
    {
        aRefTable_p = sortTab_p->makeRefTable (False, 0);
    }
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
    if(sortIterBoundaries_p)
    {
        sortIterBoundariesIt_p = sortIterBoundaries_p->begin();
    }
    if(sortIterKeyIdxChange_p)
    {
        sortIterKeyIdxChangeIt_p = sortIterKeyIdxChange_p->begin();
    }
}

BaseTable* BaseTableIterator::next()
{
    // If there are no group boundaries precomputed do an expensive
    // walk to check where a new boundary happens by calling the comparion
    // functions
    if(!sortIterBoundaries_p || !sortIterKeyIdxChange_p)
    {
        return noCachedIterBoundariesNext();
    }

    // Allocate a RefTable to represent the rows in the iteration group.
    aRefTable_p->removeAllRow();
    if (lastRow_p >= sortTab_p->nrow()) {
        return aRefTable_p;                              // the end of the table
    }

    // Go to the next group boundary (the one after this), which will be
    // one past the end of the current group.
    ++sortIterBoundariesIt_p;
    rownr_t startNextGroup;
    if(sortIterBoundariesIt_p == sortIterBoundaries_p->end())
    {
        startNextGroup = sortTab_p->nrow();
    }
    else
    {
        startNextGroup = *sortIterBoundariesIt_p;
    }
    // lastRow_p contains the starting point for this group
    rownr_t startThisGroup = lastRow_p;
    aRefTable_p->addRownrRange (startThisGroup, startNextGroup - 1);
    // Set lastRow_p to the starting point of next group
    lastRow_p = startNextGroup;

    // If we've reached the end of the table, clear the keyCh_p
    if (lastRow_p==sortTab_p->nrow())
    {
        keyChangeAtLastNext_p=String();
    }
    // If not, get the name of the column from the sorting column ID
    else
    {
        keyChangeAtLastNext_p=colPtr_p[*sortIterKeyIdxChangeIt_p]->columnDesc().name();
    }
    ++sortIterKeyIdxChangeIt_p;

    //# Adjust rownrs in case source table is already a RefTable.
    Vector<rownr_t>& rownrs = *(aRefTable_p->rowStorage());
    sortTab_p->adjustRownrs (aRefTable_p->nrow(), rownrs, False);
    return aRefTable_p;
}

BaseTable* BaseTableIterator::noCachedIterBoundariesNext()
{
    // This is an expensive way to find the next group boundary by calling
    // the sorting function for each individual row.

    // Allocate a RefTable to represent the rows in the iteration group.
    RefTable* itp = sortTab_p->makeRefTable (False, 0);
    if (lastRow_p >= sortTab_p->nrow()) {
	return itp;                              // the end of the table
    }
    // Add the last found rownr to this iteration group.
    itp->addRownr (lastRow_p);
    for (uInt i=0; i<nrkeys_p; i++) {
	colPtr_p[i]->get (lastRow_p, lastVal_p[i]);
    }
    Bool match;
    rownr_t nr = sortTab_p->nrow();
    while (++lastRow_p < nr) {
	match = True;
	for (uInt i=0; i<nrkeys_p; i++) {
	    colPtr_p[i]->get (lastRow_p, curVal_p[i]);
	    if (cmpObj_p[i]->comp (curVal_p[i], lastVal_p[i])  != 0) {
		match = False;
		// update so users can see which key changed
		keyChangeAtLastNext_p=colPtr_p[i]->columnDesc().name();   
		break;
	    }
	}
	if (!match) {
	    break;
	}
	itp->addRownr (lastRow_p);
    }

    // If we've reached the end of the table, clear the keyCh_p
    if (lastRow_p==nr)
      keyChangeAtLastNext_p=String();

    //# Adjust rownrs in case source table is already a RefTable.
    Vector<rownr_t>& rownrs = *(itp->rowStorage());
    sortTab_p->adjustRownrs (itp->nrow(), rownrs, False);
    return itp;
}

void
BaseTableIterator::copyState(const BaseTableIterator &other)
{
  lastRow_p = other.lastRow_p;
  keyChangeAtLastNext_p = other.keyChangeAtLastNext_p;

  if(sortIterBoundaries_p)
  {
      sortIterBoundariesIt_p = sortIterBoundaries_p->begin();
      std::advance(sortIterBoundariesIt_p,
                   std::distance(other.sortIterBoundaries_p->begin(),
                                 other.sortIterBoundariesIt_p));
  }
  if(sortIterKeyIdxChange_p)
  {
      sortIterKeyIdxChangeIt_p = sortIterKeyIdxChange_p->begin();
      std::advance(sortIterKeyIdxChangeIt_p,
                   std::distance(other.sortIterKeyIdxChange_p->begin(),
                                 other.sortIterKeyIdxChangeIt_p));
  } 
}

} //# NAMESPACE CASACORE - END

