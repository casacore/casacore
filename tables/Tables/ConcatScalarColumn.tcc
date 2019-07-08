//# ConcatScalarColumn.cc: A typed scalar column in a concatenated table
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

#ifndef TABLES_CONCATSCALARCOLUMN_TCC
#define TABLES_CONCATSCALARCOLUMN_TCC

#include <casacore/tables/Tables/ConcatScalarColumn.h>
#include <casacore/tables/Tables/ConcatTable.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/GenSort.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template<typename T>
  ConcatScalarColumn<T>::ConcatScalarColumn (const BaseColumnDesc* bcdp,
					     ConcatTable* reftab)
    : ConcatColumn (bcdp, reftab)
  {}

  template<typename T>
  ConcatScalarColumn<T>::~ConcatScalarColumn()
  {}

  template<typename T>
  void ConcatScalarColumn<T>::getScalarColumn (ArrayBase& arr) const
  {
    Vector<T>& vec = static_cast<Vector<T>&>(arr);
    rownr_t st = 0;
    for (uInt i=0; i<refColPtr_p.nelements(); ++i) {
      rownr_t nr = refColPtr_p[i]->nrow();
      Vector<T> part = vec(Slice(st, nr));
      refColPtr_p[i]->getScalarColumn (part);
      st += nr;
    }
    // Set the column cache to the first table.
    ///setColumnCache (0, refColPtr_p[0]->columnCache());
  }

  template<typename T>
  void ConcatScalarColumn<T>::getScalarColumnCells (const RefRows& rownrs,
						    ArrayBase& arr) const
  {
    Vector<T>& vec = static_cast<Vector<T>&>(arr);
    // Get the rownrs as a vector and sort it.
    // In this way the data will be read in sequential order.
    Vector<rownr_t> rows = rownrs.convert();
    Vector<rownr_t> inx;
    GenSortIndirect<rownr_t,rownr_t>::sort (inx, rows);
    const ConcatRows& ccRows = refTabPtr_p->rows();
    rownr_t tabRownr;
    uInt    tableNr=0;
    // Map each row to rownr and tablenr.
    // Note this is pretty fast because it is done in row order.
    for (uInt i=0; i<inx.nelements(); ++i) {
      rownr_t row = inx[i];
      ccRows.mapRownr (tableNr, tabRownr, rows[row]);
      refColPtr_p[tableNr]->get (tabRownr, &(vec[row]));
    }
    // Set the column cache to the last table used.
    ///setColumnCache (tableNr, refColPtr_p[tableNr]->columnCache());
  }

  template<typename T>
  void ConcatScalarColumn<T>::putScalarColumn (const ArrayBase& arr)
  {
    Vector<T> vec (static_cast<const Vector<T>&>(arr));
    rownr_t st = 0;
    for (uInt i=0; i<refColPtr_p.nelements(); ++i) {
      rownr_t nr = refColPtr_p[i]->nrow();
      Vector<T> part = vec(Slice(st, nr));
      refColPtr_p[i]->putScalarColumn (part);
      st += nr;
    }
    // Set the column cache to the first table.
    ///setColumnCache (0, refColPtr_p[0]->columnCache());
  }

  template<typename T>
  void ConcatScalarColumn<T>::putScalarColumnCells (const RefRows& rownrs,
						    const ArrayBase& arr)
  {
    const Vector<T>& vec = static_cast<const Vector<T>&>(arr);
    // Get the rownrs as a vector and sort it.
    // In this way the data will be read in sequential order.
    Vector<rownr_t> rows = rownrs.convert();
    Vector<rownr_t> inx;
    GenSortIndirect<rownr_t,rownr_t>::sort (inx, rows);
    const ConcatRows& ccRows = refTabPtr_p->rows();
    rownr_t tabRownr;
    uInt    tableNr=0;
    // Map each row to rownr and tablenr.
    // Note this is pretty fast because it is done in row order.
    for (uInt i=0; i<inx.nelements(); ++i) {
      rownr_t row = inx[i];
      ccRows.mapRownr (tableNr, tabRownr, rows[row]);
      refColPtr_p[tableNr]->put (tabRownr, &(vec[row]));
    }
    // Set the column cache to the last table used.
    ///setColumnCache (tableNr, refColPtr_p[tableNr]->columnCache());
  }


  template<class T>
  void ConcatScalarColumn<T>::makeSortKey (Sort& sortobj,
                                           CountedPtr<BaseCompare>& cmpObj,
					   Int order,
					   CountedPtr<ArrayBase>& dataSave)
  {
    //# Get the data as a column.
    Vector<T>* vecPtr = new Vector<T>(nrow());
    dataSave = vecPtr;
    getScalarColumn (*vecPtr);
    fillSortKey (vecPtr, sortobj, cmpObj, order);
  }

  template<class T>
  void ConcatScalarColumn<T>::makeRefSortKey (Sort& sortobj,
                                              CountedPtr<BaseCompare>& cmpObj,
					      Int order,
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
  void ConcatScalarColumn<T>::fillSortKey (const Vector<T>* vecPtr,
					   Sort& sortobj,
                                           CountedPtr<BaseCompare>& cmpObj,
					   Int order)
  {
    //# Pass the real vector storage as the sort data.
    //# Use the compare object if given, otherwise pass data type.
    //# Throw an exception if no compare function is given for
    //# an unknown data type.
    Bool deleteIt;
    const T* datap = vecPtr->getStorage (deleteIt);
    if (cmpObj.null()) {
      cmpObj = new ObjCompare<T>();
    }
    sortobj.sortKey (datap, cmpObj, sizeof(T),
		     order == Sort::Descending  ?  Sort::Descending
		     : Sort::Ascending);
    vecPtr->freeStorage (datap, deleteIt);
  }

} //# NAMESPACE CASACORE - END

#endif
