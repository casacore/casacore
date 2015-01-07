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
//#
//# $Id$

#ifndef TABLES_ARRAYCOLUMNFUNC_H
#define TABLES_ARRAYCOLUMNFUNC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary> Abstract baseclass for slices functors </summary>
  // <synopsis>
  // There are several ArrayColumn functions to get or put irregular array
  // slices. ArrayColumn::handleSlices is used to perform all common
  // operations using a functor derived from this base class.
  // </synopsis>
  template<typename T>
  class BaseSlicesFunctor
  {
  public:
    virtual ~BaseSlicesFunctor()
    {}
    virtual void apply (const Slicer& slicer, Array<T>& arr) = 0;
  };

  // <summary> Functor to get irregular array slices from a cell</summary>
  template<typename T>
  class GetCellSlices : public BaseSlicesFunctor<T>
  {
  public:
    GetCellSlices (const ArrayColumn<T>& col, uInt rownr)
      : itsCol(col), itsRow(rownr)
    {}
    virtual void apply (const Slicer& slicer, Array<T>& arr)
      { itsCol.getSlice (itsRow, slicer, arr); }
  private:
    const ArrayColumn<T>& itsCol;
    uInt                  itsRow;
  };

  // <summary> Functor to get irregular array slices from a column</summary>
  template<typename T>
  class GetColumnSlices : public BaseSlicesFunctor<T>
  {
  public:
    GetColumnSlices (const ArrayColumn<T>& col)
      : itsCol(col)
    {}
    virtual void apply (const Slicer& slicer, Array<T>& arr)
      { itsCol.getColumn (slicer, arr); }
  private:
    const ArrayColumn<T>& itsCol;
  };

  // <summary> Functor to put irregular array slices into a cell </summary>
  template<typename T>
  class PutCellSlices : public BaseSlicesFunctor<T>
  {
  public:
    PutCellSlices (ArrayColumn<T>& col, uInt rownr)
      : itsCol(col), itsRow(rownr)
    {}
    virtual void apply (const Slicer& slicer, Array<T>& arr)
      { itsCol.putSlice (itsRow, slicer, arr); }
  private:
    ArrayColumn<T>& itsCol;
    uInt            itsRow;
  };

  // <summary> Functor to get irregular array slices from a column</summary>
  template<typename T>
  class PutColumnSlices : public BaseSlicesFunctor<T>
  {
  public:
    PutColumnSlices (ArrayColumn<T>& col)
      : itsCol(col)
    {}
    virtual void apply (const Slicer& slicer, Array<T>& arr)
      { itsCol.putColumn (slicer, arr); }
  private:
    ArrayColumn<T>& itsCol;
  };

} //# NAMESPACE CASACORE - END

#endif
