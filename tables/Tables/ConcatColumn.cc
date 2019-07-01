//# ConcatColumn.cc: A column in a concatenated table
//# Copyright (C) 2008
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

#include <casacore/tables/Tables/ConcatColumn.h>
#include <casacore/tables/Tables/ConcatTable.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/GenSort.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  ConcatColumn::ConcatColumn (const BaseColumnDesc* bcdp,
			      ConcatTable* reftab)
  : BaseColumn  (bcdp),
    refTabPtr_p (reftab),
    refColPtr_p (reftab->getRefColumns (bcdp->name()))
  {
    keywordSet_p = refColPtr_p[0]->keywordSet();
  }

  ConcatColumn::~ConcatColumn()
  {}

  Bool ConcatColumn::isWritable() const
  {
    return refTabPtr_p->isWritable()  &&  refColPtr_p[0]->isWritable();
  }

  Bool ConcatColumn::isStored() const
  {
    return refColPtr_p[0]->isStored();
  }

  TableRecord& ConcatColumn::keywordSet()
  {
    return keywordSet_p;
  }

  TableRecord& ConcatColumn::rwKeywordSet()
  {
    return keywordSet_p;
  }

  rownr_t ConcatColumn::nrow() const
  {
    return refTabPtr_p->nrow();
  }

  void ConcatColumn::initialize (rownr_t startRow, rownr_t endRow)
  {
    uInt tableNr;
    uInt tabRownr;
    for (rownr_t i=startRow; i<endRow; ++i) {
      refTabPtr_p->rows().mapRownr (tableNr, tabRownr, i);
      refColPtr_p[tableNr]->initialize (tabRownr, tabRownr);
    }
  }

  void ConcatColumn::setShape (rownr_t rownr, const IPosition& shape)
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->setShape (tabRownr, shape);
  }

  void ConcatColumn::setShape (rownr_t rownr, const IPosition& shape,
			       const IPosition& tileShape)
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->setShape (tabRownr, shape, tileShape);
  }

  uInt ConcatColumn::ndimColumn() const
  {
    return refColPtr_p[0]->ndimColumn();
  }

  IPosition ConcatColumn::shapeColumn() const
  {
    return refColPtr_p[0]->shapeColumn();
  }

  uInt ConcatColumn::ndim (rownr_t rownr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    return refColPtr_p[tableNr]->ndim (tabRownr);
  }

  IPosition ConcatColumn::shape(rownr_t rownr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    return refColPtr_p[tableNr]->shape (tabRownr);
  }

  IPosition ConcatColumn::tileShape(rownr_t rownr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    return refColPtr_p[tableNr]->tileShape (tabRownr);
  }

  Bool ConcatColumn::isDefined (rownr_t rownr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    return refColPtr_p[tableNr]->isDefined (tabRownr);
  }


  Bool ConcatColumn::canChangeShape() const
  {
    return refColPtr_p[0]->canChangeShape();
  }


  void ConcatColumn::get (rownr_t rownr, void* dataPtr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->get (tabRownr, dataPtr);
    // Set the column cache to the table used.
    ///setColumnCache (tableNr, refColPtr_p[tableNr]->columnCache());
  }

  void ConcatColumn::getArray (rownr_t rownr, ArrayBase& arr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->getArray (tabRownr, arr);
  }

  void ConcatColumn::getSlice (rownr_t rownr, const Slicer& ns,
			       ArrayBase& arr) const
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->getSlice (tabRownr, ns, arr);
  }

  void ConcatColumn::put (rownr_t rownr, const void* dataPtr)
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->put (tabRownr, dataPtr);
    // Set the column cache to the table used.
    ///setColumnCache (tableNr, refColPtr_p[tableNr]->columnCache());
  }

  void ConcatColumn::putArray (rownr_t rownr, const ArrayBase& arr)
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->putArray (tabRownr, arr);
  }

  void ConcatColumn::putSlice (rownr_t rownr, const Slicer& ns,
			       const ArrayBase& arr)
  {
    uInt tableNr;
    uInt tabRownr;
    refTabPtr_p->rows().mapRownr (tableNr, tabRownr, rownr);
    refColPtr_p[tableNr]->putSlice (tabRownr, ns, arr);
  }

  void ConcatColumn::setMaximumCacheSize (uInt nbytes)
  {
    for (uInt i=0; i<refColPtr_p.nelements(); ++i) {
      refColPtr_p[i]->setMaximumCacheSize (nbytes);
    }
  }

  void ConcatColumn::allocIterBuf (void*& lastVal, void*& curVal,
                                   CountedPtr<BaseCompare>& cmpObj)
    { refColPtr_p[0]->allocIterBuf (lastVal, curVal, cmpObj); }

  void ConcatColumn::freeIterBuf (void*& lastVal, void*& curVal)
    { refColPtr_p[0]->freeIterBuf (lastVal, curVal); }


  void ConcatColumn::getArrayColumn (ArrayBase& arr) const
  {
    accessColumn (0, arr, &getColumnPart);
  }

  void ConcatColumn::getColumnSlice (const Slicer& ns,
				     ArrayBase& arr) const
  {
    accessColumn (&ns, arr, &getColumnSlicePart);
  }

  void ConcatColumn::getArrayColumnCells (const RefRows& rownrs,
					  ArrayBase& arr) const
  {
    accessRows (rownrs, 0, arr, &getRowsPart);
  }

  void ConcatColumn::getColumnSliceCells (const RefRows& rownrs,
					  const Slicer& ns,
					  ArrayBase& arr) const
  {
    accessRows (rownrs, &ns, arr, &getRowsSlicePart);
  }

  void ConcatColumn::putArrayColumn (const ArrayBase& arr)
  {
    accessColumn (0, const_cast<ArrayBase&>(arr), &putColumnPart);
  }

  void ConcatColumn::putColumnSlice (const Slicer& ns,
				     const ArrayBase& arr)
  {
    accessColumn (&ns, const_cast<ArrayBase&>(arr), &putColumnSlicePart);
  }

  void ConcatColumn::putArrayColumnCells (const RefRows& rownrs,
					  const ArrayBase& arr)
  {
    accessRows (rownrs, 0, const_cast<ArrayBase&>(arr), &putRowsPart);
  }

  void ConcatColumn::putColumnSliceCells (const RefRows& rownrs,
					  const Slicer& ns,
					  const ArrayBase& arr)
  {
    accessRows (rownrs, &ns, const_cast<ArrayBase&>(arr), &putRowsSlicePart);
  }

  void ConcatColumn::accessColumn (const Slicer* ns,
				   ArrayBase& arr,
				   AccessColumnFunc* accessFunc) const
  {
    IPosition st(arr.ndim(), 0);
    IPosition sz(arr.shape());
    uInt nlast = arr.ndim() - 1;
    for (uInt i=0; i<refColPtr_p.nelements(); ++i) {
      rownr_t nr = refColPtr_p[i]->nrow();
      sz[nlast] = nr;
      CountedPtr<ArrayBase> part (arr.getSection (Slicer(st, sz)));
      accessFunc (refColPtr_p[i], ns, *part);
      st[nlast] += nr;
    }
  }

  void ConcatColumn::accessRows (const RefRows& rownrs,
				 const Slicer* ns,
				 ArrayBase& arr,
				 AccessRowsFunc* accessFunc) const
  {
    // The rows to access.
    Vector<uInt> rows = rownrs.convert();
    // We have one or more slices of rows.
    // Try to access them also in a sliced way, because that is faster.
    // First make resources.
    // The row number mapping.
    const ConcatRows& ccRows = refTabPtr_p->rows();
    // The RefRows vector for the rownrs to be handled in an underlying table.
    // Make it as large as needed to avoid resizes.
    Vector<uInt> tabRowNrs(rows.nelements());
    // The rows are handled by combining them as much as possible in a RefRows
    // slice. This is possible until a different underlying table needs to
    // be accessed.
    // First setup the various loop variables.
    uInt rowAxis = arr.ndim() - 1;   // row axis in array
    IPosition st(arr.ndim(), 0);     // start of array part
    IPosition sz(arr.shape());       // size of array part
    Int lastTabNr = -1;
    uInt tableNr;
    // Step through all concat rownrs.
    for (rownr_t i=0; i<rows.nelements(); ++i) {
      // Map to the table and rownr in it.
      ccRows.mapRownr (tableNr, tabRowNrs[i], rows[i]);
      // An access has to be done if we have another table.
      if (Int(tableNr) != lastTabNr) {
	// Access the cells if not the first time.
	if (lastTabNr >= 0) {
	  rownr_t nrrow = i - st[rowAxis];
	  sz[rowAxis] = nrrow;
	  Vector<uInt> rowPart(tabRowNrs(Slice(st[rowAxis], nrrow))); 
	  CountedPtr<ArrayBase> part (arr.getSection (Slicer(st, sz)));
	  accessFunc (refColPtr_p[lastTabNr], RefRows(rowPart), ns, *part);
	}
        st[rowAxis] = i;
        lastTabNr = tableNr;
      }
    }
    if (lastTabNr >= 0) {
      rownr_t nrrow = rows.nelements() - st[rowAxis];
      sz[rowAxis] = nrrow;
      Vector<uInt> rowPart(tabRowNrs(Slice(st[rowAxis], nrrow))); 
      CountedPtr<ArrayBase> part (arr.getSection (Slicer(st, sz)));
      accessFunc (refColPtr_p[lastTabNr], RefRows(rowPart), ns, *part);
    }
  }

  void ConcatColumn::getColumnPart (BaseColumn* col,
				    const Slicer*, ArrayBase& arr)
  {
    col->getArrayColumn (arr);
  }
  void ConcatColumn::putColumnPart (BaseColumn* col,
				    const Slicer*, ArrayBase& arr)
  {
    col->putArrayColumn (arr);
  }
  void ConcatColumn::getColumnSlicePart (BaseColumn* col,
					 const Slicer* ns, ArrayBase& arr)
  {
    col->getColumnSlice (*ns, arr);
  }
  void ConcatColumn::putColumnSlicePart (BaseColumn* col,
					 const Slicer* ns, ArrayBase& arr)
  {
    col->putColumnSlice (*ns, arr);
  }
  void ConcatColumn::getRowsPart (BaseColumn* col, const RefRows& rows,
				  const Slicer*, ArrayBase& arr)
  {
    col->getArrayColumnCells (rows, arr);
  }
  void ConcatColumn::putRowsPart (BaseColumn* col, const RefRows& rows,
				  const Slicer*, ArrayBase& arr)
  {
    col->putArrayColumnCells (rows, arr);
  }
  void ConcatColumn::getRowsSlicePart (BaseColumn* col, const RefRows& rows,
				       const Slicer* ns, ArrayBase& arr)
  {
    col->getColumnSliceCells (rows, *ns, arr);
  }
  void ConcatColumn::putRowsSlicePart (BaseColumn* col, const RefRows& rows,
				       const Slicer* ns, ArrayBase& arr)
  {
    col->putColumnSliceCells (rows, *ns, arr);
  }

  ColumnCache& ConcatColumn::columnCache()
    { return colCache_p; }

  void ConcatColumn::setColumnCache (uInt tableNr,
				     const ColumnCache& colCache) const
  {
    // Please note that his is not fully safe, because if the cache in the
    // underlying table gets changed, it is not reflected in this cache.
    // There should be some kind of callback or this cache should point
    // to the other one.
    // So for the time being this function is not used.
    const ConcatRows& ccRows = refTabPtr_p->rows();
    colCache_p.set (ccRows.offset(tableNr) + colCache.start(),
		    ccRows.offset(tableNr) + colCache.end(),
		    colCache.dataPtr());
    colCache_p.setIncrement (colCache.incr());
  }

} //# NAMESPACE CASACORE - END
