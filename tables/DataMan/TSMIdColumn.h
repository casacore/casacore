// # TSMIdColumn.h: An id column in Tiled Storage Manager
// # Copyright (C) 1995,1996,1997,1999
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_TSMIDCOLUMN_H
#define TABLES_TSMIDCOLUMN_H

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Forward Declarations

// <summary>
// An id column in Tiled Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
// # Classes you should understand before using this one.
//   <li> <linkto class=TSMColumn>TSMColumn</linkto>
//   <li> <linkto class=TSMCube>TSMCube</linkto>
//   <li> <linkto class=Record>Record</linkto>
// </prerequisite>

// <etymology>
// TSMIdColumn handles an id column for a Tiled
// Storage Manager.
// </etymology>

// <synopsis>
// TSMIdColumn is used by
// <linkto class=TiledStMan>TiledStMan</linkto>
// to handle the access to
// a table column containing an id value of a tiled hypercube.
// Explicitly putting an id value is not possible. The only way to
// define the value is by specifying it when adding a hypercube
// in <linkto class=TiledDataStMan>TiledDataStMan</linkto>.
// <p>
// The id values are held in a TSMCube object. The row number
// determines which TSMCube object has to be accessed.
// <p>
// The creation of a TSMIdColumn object is done by a TSMColumn object.
// This process is described in more detail in the class
// <linkto class=TSMColumn>TSMColumn</linkto>.
// </synopsis>

// <motivation>
// Handling coordinate columns in the Tiled Storage Manager is
// different from other columns.
// </motivation>

// # <todo asof="$DATE:$">
// # A List of bugs, limitations, extensions or planned refinements.
// # </todo>

class TSMIdColumn : public TSMColumn {
 public:
  // Create an id column from the given column.
  TSMIdColumn(const TSMColumn& column) : TSMColumn(column) {}

  // Frees up the storage.
  virtual ~TSMIdColumn() = default;

  // Forbid copy constructor.
  TSMIdColumn(const TSMIdColumn&) = delete;

  // Forbid assignment.
  TSMIdColumn& operator=(const TSMIdColumn&) = delete;

  // Get a scalar value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn get function).
  // <group>
  void getBool(rownr_t rownr, Bool* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getInt(rownr_t rownr, Int* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getuInt(rownr_t rownr, uInt* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getInt64(rownr_t rownr, Int64* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getfloat(rownr_t rownr, float* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getdouble(rownr_t rownr, double* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getComplex(rownr_t rownr, Complex* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getDComplex(rownr_t rownr, DComplex* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getString(rownr_t rownr, String* dataPtr) override { GetGeneric(rownr, dataPtr); }
  // </group>

  // Put a scalar value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn get function).
  // The value to be put must match the value which has already
  // been inserted by the TiledStMan::addHypercube function.
  // The put function is only there to be fully orthogonal.
  // <group>
  void putBool(rownr_t rownr, const Bool* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putInt(rownr_t rownr, const Int* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putuInt(rownr_t rownr, const uInt* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putInt64(rownr_t rownr, const Int64* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putfloat(rownr_t rownr, const float* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putdouble(rownr_t rownr, const double* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putComplex(rownr_t rownr, const Complex* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putDComplex(rownr_t rownr, const DComplex* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putString(rownr_t rownr, const String* dataPtr) override { PutGeneric(rownr, dataPtr); }
  // </group>

 private:
  template <typename T>
  void GetGeneric(rownr_t rownr, T* dataPtr) {
    TSMCube* hypercube = stmanPtr_p->getHypercube(rownr);
    hypercube->valueRecord().get(columnName(), *dataPtr);
  }

  template <typename T>
  void PutGeneric(rownr_t rownr, const T* dataPtr) {
    T value;
    GetGeneric<T>(rownr, &value);
    if (value != *dataPtr) {
      throw TSMError(
          "TSMIdColumn::put: new value mismatches existing"
          " in id column " +
          columnName());
    }
  }
};

}  // namespace casacore

#endif
