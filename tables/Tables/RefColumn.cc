//# RefColumn.cc: Abstract base class for a table column
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2001
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

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/RefColumn.h>
#include <casacore/tables/Tables/RefTable.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RefColumn::RefColumn (const BaseColumnDesc* bcdp,
		      RefTable* reftab, BaseColumn* bcp)
: BaseColumn (bcdp),
  refTabPtr_p(reftab),
  colPtr_p   (bcp)
{}

RefColumn::~RefColumn()
{}


Bool RefColumn::isWritable() const
    { return colPtr_p->isWritable(); }

Bool RefColumn::isStored() const
    { return colPtr_p->isStored(); }

TableRecord& RefColumn::rwKeywordSet()
    { return colPtr_p->rwKeywordSet(); }
TableRecord& RefColumn::keywordSet()
    { return colPtr_p->keywordSet(); }


uInt RefColumn::nrow() const
    { return refTabPtr_p->nrow(); }

void RefColumn::initialize (uInt startRow, uInt endRow)
{
    uInt rownr;
    for (uInt i=startRow; i<endRow; i++) {
	rownr = refTabPtr_p->rootRownr(i);
	colPtr_p->initialize (rownr, rownr);
    }
}

void RefColumn::setShape (uInt rownr, const IPosition& shape)
    { colPtr_p->setShape (refTabPtr_p->rootRownr(rownr), shape); }

void RefColumn::setShape (uInt rownr, const IPosition& shape,
			  const IPosition& tileShape)
    { colPtr_p->setShape (refTabPtr_p->rootRownr(rownr), shape, tileShape); }

uInt RefColumn::ndimColumn() const
    { return colPtr_p->ndimColumn(); }

IPosition RefColumn::shapeColumn() const
    { return colPtr_p->shapeColumn(); }

uInt RefColumn::ndim (uInt rownr) const
    { return colPtr_p->ndim (refTabPtr_p->rootRownr(rownr)); }

IPosition RefColumn::shape(uInt rownr) const
    { return colPtr_p->shape (refTabPtr_p->rootRownr(rownr)); }

Bool RefColumn::isDefined (uInt rownr) const
    { return colPtr_p->isDefined (refTabPtr_p->rootRownr(rownr)); }


Bool RefColumn::canAccessScalarColumn (Bool& reask) const
    { return colPtr_p->canAccessScalarColumnCells (reask); }
Bool RefColumn::canAccessScalarColumnCells (Bool& reask) const
    { return colPtr_p->canAccessScalarColumnCells (reask); }
Bool RefColumn::canAccessArrayColumn (Bool& reask) const
    { return colPtr_p->canAccessArrayColumnCells (reask); }
Bool RefColumn::canAccessArrayColumnCells (Bool& reask) const
    { return colPtr_p->canAccessArrayColumnCells (reask); }
Bool RefColumn::canAccessSlice (Bool& reask) const
    { return colPtr_p->canAccessSlice (reask); }
Bool RefColumn::canAccessColumnSlice (Bool& reask) const
{
    Bool reask1;
    Bool acc1 = colPtr_p->canAccessColumnSlice (reask1);
    Bool acc2 = colPtr_p->canAccessArrayColumnCells (reask);
    if (reask1) {
	reask = reask1;
    }
    return  (acc1 && acc2);
}

Bool RefColumn::canChangeShape() const
    { return colPtr_p->canChangeShape(); }


void RefColumn::get (uInt rownr, void* dataPtr) const
    { colPtr_p->get (refTabPtr_p->rootRownr(rownr), dataPtr); }

void RefColumn::getSlice (uInt rownr, const Slicer& ns, void* dataPtr) const
    { colPtr_p->getSlice (refTabPtr_p->rootRownr(rownr), ns, dataPtr); }

void RefColumn::put (uInt rownr, const void* dataPtr)
    { colPtr_p->put (refTabPtr_p->rootRownr(rownr), dataPtr); }

void RefColumn::putSlice (uInt rownr, const Slicer& ns, const void* dataPtr)
    { colPtr_p->putSlice (refTabPtr_p->rootRownr(rownr), ns, dataPtr); }

void RefColumn::getScalarColumn (void* dataPtr) const
{
    colPtr_p->getScalarColumnCells (refTabPtr_p->rowNumbers(), dataPtr);
}
void RefColumn::getArrayColumn (void* dataPtr) const
{
    colPtr_p->getArrayColumnCells (refTabPtr_p->rowNumbers(), dataPtr);
}
void RefColumn::getColumnSlice (const Slicer& ns,
				void* dataPtr) const
{
    colPtr_p->getColumnSliceCells (refTabPtr_p->rowNumbers(), ns, dataPtr); 
}
void RefColumn::getScalarColumnCells (const RefRows& rownrs,
				      void* dataPtr) const
{
    colPtr_p->getScalarColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				    dataPtr);
}
void RefColumn::getArrayColumnCells (const RefRows& rownrs,
				     void* dataPtr) const
{
    colPtr_p->getArrayColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   dataPtr);
}
void RefColumn::getColumnSliceCells (const RefRows& rownrs,
				     const Slicer& ns,
				     void* dataPtr) const
{
    colPtr_p->getColumnSliceCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   ns, dataPtr);
}
void RefColumn::putScalarColumn (const void* dataPtr)
{
    colPtr_p->putScalarColumnCells (refTabPtr_p->rowNumbers(), dataPtr);
}
void RefColumn::putArrayColumn (const void* dataPtr)
{
    colPtr_p->putArrayColumnCells (refTabPtr_p->rowNumbers(), dataPtr);
}
void RefColumn::putColumnSlice (const Slicer& ns,
				const void* dataPtr)
{
    colPtr_p->putColumnSliceCells (refTabPtr_p->rowNumbers(), ns, dataPtr); 
}
void RefColumn::putScalarColumnCells (const RefRows& rownrs,
				      const void* dataPtr)
{
    colPtr_p->putScalarColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				    dataPtr);
}
void RefColumn::putArrayColumnCells (const RefRows& rownrs,
				     const void* dataPtr)
{
    colPtr_p->putArrayColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   dataPtr);
}
void RefColumn::putColumnSliceCells (const RefRows& rownrs,
				     const Slicer& ns,
				     const void* dataPtr)
{
    colPtr_p->putColumnSliceCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   ns, dataPtr);
}


ColumnCache& RefColumn::columnCache()
    { return colCache_p; }

void RefColumn::setMaximumCacheSize (uInt nbytes)
    { colPtr_p->setMaximumCacheSize (nbytes); }


void RefColumn::makeSortKey (Sort& sortobj, CountedPtr<BaseCompare>& cmpObj,
			     Int order, const void*& dataSave)
    { colPtr_p->makeRefSortKey (sortobj, cmpObj, order,
				refTabPtr_p->rowNumbers(), dataSave); }

void RefColumn::freeSortKey (const void*& dataSave)
    { colPtr_p->freeSortKey (dataSave); }

void RefColumn::allocIterBuf (void*& lastVal, void*& curVal,
			      CountedPtr<BaseCompare>& cmpObj)
    { colPtr_p->allocIterBuf (lastVal, curVal, cmpObj); }

void RefColumn::freeIterBuf (void*& lastVal, void*& curVal)
    { colPtr_p->freeIterBuf (lastVal, curVal); }

} //# NAMESPACE CASACORE - END

