//# TableCopy.tcc: Class with static functions for copying a table
//# Copyright (C) 2016
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

#ifndef TABLES_TABLECOPY_TCC
#define TABLES_TABLECOPY_TCC

//# Includes
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template<typename T>
  void TableCopy::cloneColumnTyped (const Table& fromTable,
                                    const String& fromColumn,
                                    Table& toTable, const String& newColumn,
                                    const String& dataManagerName)
  {
    // Get existing column description.
    ColumnDesc cd(fromTable.tableDesc()[fromColumn]);
    if (cd.isScalar()) {
      ScalarColumnDesc<T> scd(newColumn, cd.comment(), cd.dataManagerType(),
                              cd.dataManagerGroup(), T(), cd.options());
      cd = ColumnDesc(scd);
    } else {
      ArrayColumnDesc<T> acd(newColumn, cd.comment(), cd.dataManagerType(),
                             cd.dataManagerGroup(),
                             cd.shape(), cd.options(), cd.ndim());
      cd = ColumnDesc(acd);
    }
    doCloneColumn (fromTable, fromColumn, toTable, cd, dataManagerName);
  }

  template<typename T>
  void TableCopy::fillArrayColumn (Table& table, const String& column,
                                   const Array<T>& value)
  {
    ArrayColumn<T> acol(table, column);
    acol.fillColumn (value);
  }

  template<typename T>
  void TableCopy::fillColumnData (Table& table, const String& column,
                                  const T& value)
  {
    TableColumn col(table, column);
    if (col.columnDesc().isScalar()) {
      ScalarColumn<T> scol(col);
      scol.fillColumn (value);
    } else {
      // Fill the array in each row with the value.
      TableCopy::fillColumnData (table, column, value, table, column);
    }
  }

  template<typename T>
  void TableCopy::fillColumnData (Table& table, const String& column,
                                  const T& value,
                                  const Table& fromTable,
                                  const String& fromColumn,
                                  Bool preserveTileShape)
  {
    TableColumn fromCol(fromTable, fromColumn);
    AlwaysAssert (fromCol.columnDesc().isArray(), AipsError);
    Array<T> arr;
    ArrayColumn<T> toCol(table, column);
    for (uInt i=0; i<table.nrow(); ++i) {
      // Only write if the source cell contains an array.
      if (fromCol.isDefined(i)) {
        IPosition shp(fromCol.shape(i));
        if (! shp.isEqual (arr.shape())) {
          if (shp.size() == arr.ndim()) {
            // reformOrResize cannot change the dimensionality.
            arr.reformOrResize (shp);
          } else {
            arr.resize (shp);
          }
          arr = value;
          if (preserveTileShape) {
            toCol.setShape (i, arr.shape(), fromCol.tileShape(i));
          }
        }
        toCol.put (i, arr);
      }
    }
  }

} //# NAMESPACE CASACORE - END

#endif
