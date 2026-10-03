// # TSMCoordColumn.h: A coordinate column in Tiled Storage Manager
// # Copyright (C) 1995,1996,1999
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

#ifndef TABLES_TSMCOORDCOLUMN_H
#define TABLES_TSMCOORDCOLUMN_H

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TSMCube.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Forward declarations

// <summary>
// A coordinate column in Tiled Storage Manager
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
// TSMCoordColumn handles a coordinate column for a Tiled
// Storage Manager.
// </etymology>

// <synopsis>
// TSMCoordColumn is used by
// <linkto class=TiledStMan>TiledStMan</linkto>
// to handle the access to
// a table column containing coordinates of a tiled hypercube axis.
// There are 2 types of coordinates (as described at
// <linkto class=TableDesc:defineHypercolumn>
// TableDesc::defineHypercolumn</linkto>):
// <ol>
//  <li> As a vector. These are the coordinates of the arrays held
//       in the data cells. They are accessed via the get/putArray
//       functions. Their shapes are dependent on the hypercube shape,
//       so it is checked if they match.
//  <li> As a scalar. These are the coordinates of the extra axes
//       defined in the hypercube. They are accessed via the get/put
//       functions.
// </ol>
// The coordinates are held in a TSMCube object. The row number
// determines which TSMCube object has to be accessed.
// <p>
// The creation of a TSMCoordColumn object is done by a TSMColumn object.
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

class TSMCoordColumn : public TSMColumn {
 public:
  // Create a coordinate column from the given column.
  TSMCoordColumn(const TSMColumn& column, unsigned int axisNr);

  // Frees up the storage.
  ~TSMCoordColumn() override;

  // Forbid copy constructor.
  TSMCoordColumn(const TSMCoordColumn&) = delete;

  // Forbid assignment.
  TSMCoordColumn& operator=(const TSMCoordColumn&) = delete;

  // Set the shape of the coordinate vector in the given row.
  void setShape(rownr_t rownr, const IPosition& shape) override;

  // Is the value shape defined in the given row?
  bool isShapeDefined(rownr_t rownr) override;

  // Get the shape of the item in the given row.
  IPosition shape(rownr_t rownr) override;

  // Get a scalar value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the Scalar/ArrayColumn get function).
  // <group>
  void getInt(rownr_t rownr, int* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getuInt(rownr_t rownr, unsigned int* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getInt64(rownr_t rownr, Int64* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getfloat(rownr_t rownr, float* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getdouble(rownr_t rownr, double* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getComplex(rownr_t rownr, Complex* dataPtr) override { GetGeneric(rownr, dataPtr); }
  void getDComplex(rownr_t rownr, DComplex* dataPtr) override { GetGeneric(rownr, dataPtr); }
  // </group>

  // Put a scalar value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the Scalar/ArrayColumn put function).
  // <group>
  void putInt(rownr_t rownr, const int* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putuInt(rownr_t rownr, const unsigned int* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putInt64(rownr_t rownr, const Int64* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putfloat(rownr_t rownr, const float* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putdouble(rownr_t rownr, const double* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putComplex(rownr_t rownr, const Complex* dataPtr) override { PutGeneric(rownr, dataPtr); }
  void putDComplex(rownr_t rownr, const DComplex* dataPtr) override { PutGeneric(rownr, dataPtr); }
  // </group>

  // Get the array value in the given row.
  // The array pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  void getArrayV(rownr_t rownr, ArrayBase& dataPtr) override;

  // Put the array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  void putArrayV(rownr_t rownr, const ArrayBase& dataPtr) override;

 private:
  template<typename T>
  void GetGeneric(rownr_t rownr, T* dataPtr) {
    IPosition position;
    TSMCube* hypercube = stmanPtr_p->getHypercube(rownr, position);
    RORecordFieldPtr<Array<T>> field(hypercube->valueRecord(), columnName());
    *dataPtr = (*field)(IPosition(1, position(axisNr_p)));
  }
  
  template<typename T>
  void PutGeneric(rownr_t rownr, const T* dataPtr) {
    IPosition position;
    TSMCube* hypercube = stmanPtr_p->getHypercube(rownr, position);
    RecordFieldPtr<Array<T>> field(hypercube->rwValueRecord(), columnName());
    (*field)(IPosition(1, position(axisNr_p))) = *dataPtr;
    stmanPtr_p->setDataChanged();
  }
  
  // The axis number of the coordinate.
  unsigned int axisNr_p;
};

}  // namespace casacore

#endif
