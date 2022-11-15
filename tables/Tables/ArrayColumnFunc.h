//# ArrayColumnFunbc.h: Functors to operate on an ArrayColumn
//# Copyright (C) 2009
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

#ifndef TABLES_ARRAYCOLUMNFUNC_H
#define TABLES_ARRAYCOLUMNFUNC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ArrayColumnBase.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary> Abstract baseclass for slices functors </summary>
  // <synopsis>
  // There are several ArrayColumn functions to get or put irregular array
  // slices. ArrayColumnBase::handleSlices is used to perform all common
  // operations using a functor derived from this base class.
  // </synopsis>
  class BaseSlicesFunctor
  {
  public:
    virtual ~BaseSlicesFunctor()
    {}
    virtual void apply (const Slicer& slicer, ArrayBase& arr) = 0;
  };

  // <summary> Functor to get irregular array slices from a cell</summary>
  class GetCellSlices : public BaseSlicesFunctor
  {
  public:
    GetCellSlices (const ArrayColumnBase& col, rownr_t rownr)
      : itsCol(col), itsRow(rownr)
    {}
    virtual void apply (const Slicer& slicer, ArrayBase& arr)
      { itsCol.baseGetSlice (itsRow, slicer, arr); }
  private:
    const ArrayColumnBase& itsCol;
    rownr_t                itsRow;
  };

  // <summary> Functor to get irregular array slices from a column</summary>
  class GetColumnSlices : public BaseSlicesFunctor
  {
  public:
    GetColumnSlices (const ArrayColumnBase& col)
      : itsCol(col)
    {}
    virtual void apply (const Slicer& slicer, ArrayBase& arr)
      { itsCol.acbGetColumn (slicer, arr, False); }
  private:
    const ArrayColumnBase& itsCol;
  };

  // <summary> Functor to put irregular array slices into a cell </summary>
  class PutCellSlices : public BaseSlicesFunctor
  {
  public:
    PutCellSlices (ArrayColumnBase& col, rownr_t rownr)
      : itsCol(col), itsRow(rownr)
    {}
    virtual void apply (const Slicer& slicer, ArrayBase& arr)
      { itsCol.basePutSlice (itsRow, slicer, arr); }
  private:
    ArrayColumnBase& itsCol;
    rownr_t          itsRow;
  };

  // <summary> Functor to get irregular array slices from a column</summary>
  class PutColumnSlices : public BaseSlicesFunctor
  {
  public:
    PutColumnSlices (ArrayColumnBase& col)
      : itsCol(col)
    {}
    virtual void apply (const Slicer& slicer, ArrayBase& arr)
      { itsCol.acbPutColumn (slicer, arr); }
  private:
    ArrayColumnBase& itsCol;
  };

} //# NAMESPACE CASACORE - END

#endif
