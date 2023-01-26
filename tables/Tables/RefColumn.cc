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


bool RefColumn::isWritable() const
    { return colPtr_p->isWritable(); }

bool RefColumn::isStored() const
    { return colPtr_p->isStored(); }

TableRecord& RefColumn::rwKeywordSet()
    { return colPtr_p->rwKeywordSet(); }
TableRecord& RefColumn::keywordSet()
    { return colPtr_p->keywordSet(); }


rownr_t RefColumn::nrow() const
    { return refTabPtr_p->nrow(); }

void RefColumn::initialize (rownr_t startRow, rownr_t endRow)
{
    rownr_t rownr;
    for (rownr_t i=startRow; i<endRow; i++) {
	rownr = refTabPtr_p->rootRownr(i);
	colPtr_p->initialize (rownr, rownr);
    }
}

void RefColumn::setShape (rownr_t rownr, const IPosition& shape)
    { colPtr_p->setShape (refTabPtr_p->rootRownr(rownr), shape); }

void RefColumn::setShape (rownr_t rownr, const IPosition& shape,
			  const IPosition& tileShape)
    { colPtr_p->setShape (refTabPtr_p->rootRownr(rownr), shape, tileShape); }

uint32_t RefColumn::ndimColumn() const
    { return colPtr_p->ndimColumn(); }

IPosition RefColumn::shapeColumn() const
    { return colPtr_p->shapeColumn(); }

uint32_t RefColumn::ndim (rownr_t rownr) const
    { return colPtr_p->ndim (refTabPtr_p->rootRownr(rownr)); }

IPosition RefColumn::shape(rownr_t rownr) const
    { return colPtr_p->shape (refTabPtr_p->rootRownr(rownr)); }

bool RefColumn::isDefined (rownr_t rownr) const
    { return colPtr_p->isDefined (refTabPtr_p->rootRownr(rownr)); }


bool RefColumn::canChangeShape() const
    { return colPtr_p->canChangeShape(); }


void RefColumn::get (rownr_t rownr, void* dataPtr) const
    { colPtr_p->get (refTabPtr_p->rootRownr(rownr), dataPtr); }

void RefColumn::getArray (rownr_t rownr, ArrayBase& data) const
    { colPtr_p->getArray (refTabPtr_p->rootRownr(rownr), data); }

void RefColumn::getSlice (rownr_t rownr, const Slicer& ns, ArrayBase& data) const
    { colPtr_p->getSlice (refTabPtr_p->rootRownr(rownr), ns, data); }

void RefColumn::put (rownr_t rownr, const void* dataPtr)
    { colPtr_p->put (refTabPtr_p->rootRownr(rownr), dataPtr); }

void RefColumn::putArray (rownr_t rownr, const ArrayBase& data)
    { colPtr_p->putArray (refTabPtr_p->rootRownr(rownr), data); }

void RefColumn::putSlice (rownr_t rownr, const Slicer& ns, const ArrayBase& data)
    { colPtr_p->putSlice (refTabPtr_p->rootRownr(rownr), ns, data); }

void RefColumn::getScalarColumn (ArrayBase& data) const
{
    colPtr_p->getScalarColumnCells (refTabPtr_p->rowNumbers(), data);
}
void RefColumn::getArrayColumn (ArrayBase& data) const
{
    colPtr_p->getArrayColumnCells (refTabPtr_p->rowNumbers(), data);
}
void RefColumn::getColumnSlice (const Slicer& ns,
				ArrayBase& data) const
{
    colPtr_p->getColumnSliceCells (refTabPtr_p->rowNumbers(), ns, data); 
}
void RefColumn::getScalarColumnCells (const RefRows& rownrs,
				      ArrayBase& data) const
{
    colPtr_p->getScalarColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				    data);
}
void RefColumn::getArrayColumnCells (const RefRows& rownrs,
				     ArrayBase& data) const
{
    colPtr_p->getArrayColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   data);
}
void RefColumn::getColumnSliceCells (const RefRows& rownrs,
				     const Slicer& ns,
				     ArrayBase& data) const
{
    colPtr_p->getColumnSliceCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   ns, data);
}
void RefColumn::putScalarColumn (const ArrayBase& data)
{
    colPtr_p->putScalarColumnCells (refTabPtr_p->rowNumbers(), data);
}
void RefColumn::putArrayColumn (const ArrayBase& data)
{
    colPtr_p->putArrayColumnCells (refTabPtr_p->rowNumbers(), data);
}
void RefColumn::putColumnSlice (const Slicer& ns,
				const ArrayBase& data)
{
    colPtr_p->putColumnSliceCells (refTabPtr_p->rowNumbers(), ns, data); 
}
void RefColumn::putScalarColumnCells (const RefRows& rownrs,
				      const ArrayBase& data)
{
    colPtr_p->putScalarColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				    data);
}
void RefColumn::putArrayColumnCells (const RefRows& rownrs,
				     const ArrayBase& data)
{
    colPtr_p->putArrayColumnCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   data);
}
void RefColumn::putColumnSliceCells (const RefRows& rownrs,
				     const Slicer& ns,
				     const ArrayBase& data)
{
    colPtr_p->putColumnSliceCells (rownrs.convert(refTabPtr_p->rowNumbers()),
				   ns, data);
}


ColumnCache& RefColumn::columnCache()
    { return colCache_p; }

void RefColumn::setMaximumCacheSize (uint32_t nbytes)
    { colPtr_p->setMaximumCacheSize (nbytes); }


void RefColumn::makeSortKey (Sort& sortobj, CountedPtr<BaseCompare>& cmpObj,
			     int32_t order, CountedPtr<ArrayBase>& dataSave)
    { colPtr_p->makeRefSortKey (sortobj, cmpObj, order,
				refTabPtr_p->rowNumbers(), dataSave); }

void RefColumn::allocIterBuf (void*& lastVal, void*& curVal,
			      CountedPtr<BaseCompare>& cmpObj)
    { colPtr_p->allocIterBuf (lastVal, curVal, cmpObj); }

void RefColumn::freeIterBuf (void*& lastVal, void*& curVal)
    { colPtr_p->freeIterBuf (lastVal, curVal); }

} //# NAMESPACE CASACORE - END

