//# MSMIndColumn.cc: Memory storage manager for variable shaped table arrays
//# Copyright (C) 2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/DataMan/MSMIndColumn.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/string.h>                           // for memcpy


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Define a macro which gets the pointer for the given row
//# and casts it to the block.
#define MSMINDCOLUMN_GETDATA(rownr) \
(static_cast<MSMIndColumn::Data*>(getArrayPtr(rownr)))



MSMIndColumn::MSMIndColumn (MSMBase* smptr, int dataType)
: MSMColumn (smptr, dataType, True)
{}

//# Delete all objects created.
MSMIndColumn::~MSMIndColumn()
{
  rownr_t nr = stmanPtr_p->nrow();
  for (rownr_t i=0; i<nr; i++) {
    deleteArray (i);
  }
}

void MSMIndColumn::setShapeColumn (const IPosition& shape)
{
  fixedShape_p = shape;
}

void MSMIndColumn::setShape (rownr_t rownr, const IPosition& shape)
{
  // See if there is already a shape and if it matches.
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr);
  if (ptr != 0) {
    if (ptr->shape().isEqual (shape)) {
      return;
    }
    delete ptr;
  }
  // Create the array.
  ptr = new Data (shape, dataType(), elemSize());
  putArrayPtr (rownr, ptr);
}

//# Get the shape for the array (if any) in the given row.
//# Read shape if not read yet.
MSMIndColumn::Data* MSMIndColumn::getShape (rownr_t rownr)
{
  void* ptr = getArrayPtr(rownr);
  if (ptr == 0) {
    throw (DataManInvOper ("MSM: no array in row " +
			   String::toString(rownr) +
                           " in column " + columnName() +
			   " of " + stmanPtr_p->fileName()));
  }
  return static_cast<Data*>(ptr);
}

Bool MSMIndColumn::isShapeDefined (rownr_t rownr)
  { return (getArrayPtr(rownr) == 0  ?  False : True); }

uInt MSMIndColumn::ndim (rownr_t rownr)
  { return getShape(rownr)->shape().nelements(); }

IPosition MSMIndColumn::shape (rownr_t rownr)
  { return getShape(rownr)->shape(); }

Bool MSMIndColumn::canChangeShape() const
  { return True; }


void MSMIndColumn::getArrayV (rownr_t rownr, ArrayBase& arr)
{
  Data* data = getShape(rownr);   //# also checks if row contains data
  DebugAssert (data->shape().isEqual (arr.shape()), AipsError);
  Bool deleteIt;
  void* arrData = arr.getVStorage (deleteIt);
  if (dtype() == TpString) {
    objcopy (static_cast<String*>(arrData),
             static_cast<const String*>(data->data()),
             arr.size());
  } else {
    memcpy (static_cast<char*>(arrData),
            static_cast<const char*>(data->data()),
            elemSize() * arr.size());
  }
  arr.putVStorage (arrData, deleteIt);
}

void MSMIndColumn::putArrayV (rownr_t rownr, const ArrayBase& arr)
{
  Data* data = getShape(rownr);   //# also checks if row contains data
  DebugAssert (shape(rownr).isEqual (arr.shape()), AipsError);
  Bool deleteIt;
  const void* arrData = arr.getVStorage (deleteIt);
  if (dtype() == TpString) {
    objcopy (static_cast<String*>(data->data()),
             static_cast<const String*>(arrData),
             arr.size());
  } else {
    memcpy (static_cast<char*>(data->data()),
            static_cast<const char*>(arrData),
            elemSize() * arr.size());
  }
  arr.freeVStorage (arrData, deleteIt);
  stmanPtr_p->setHasPut();
}

void MSMIndColumn::getSliceV (rownr_t rownr, const Slicer& ns,
                              ArrayBase& arr)
{
  Data* data = getShape(rownr);   //# also checks if row contains data
  const IPosition& shp = data->shape();
  IPosition blc, trc, inc;
  ns.inferShapeFromSource (shp, blc, trc, inc);
  switch (dtype()) {
  case TpBool:
    arr.assignBase (Array<Bool>(shp, static_cast<Bool*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpUChar:
    arr.assignBase (Array<uChar>(shp, static_cast<uChar*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpShort:
    arr.assignBase (Array<short>(shp, static_cast<short*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpUShort:
    arr.assignBase (Array<uShort>(shp, static_cast<uShort*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpInt:
    arr.assignBase (Array<Int>(shp, static_cast<Int*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpUInt:
    arr.assignBase (Array<uInt>(shp, static_cast<uInt*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpInt64:
    arr.assignBase (Array<Int64>(shp, static_cast<Int64*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpFloat:
    arr.assignBase (Array<float>(shp, static_cast<float*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpDouble:
    arr.assignBase (Array<double>(shp, static_cast<double*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpComplex:
    arr.assignBase (Array<Complex>(shp, static_cast<Complex*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpDComplex:
    arr.assignBase (Array<DComplex>(shp, static_cast<DComplex*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  case TpString:
    arr.assignBase (Array<String>(shp, static_cast<String*>(data->data()), SHARE)
                    (blc, trc, inc), False);
    break;
  default:
    throw DataManInvDT ("MSMIndColumn::getSliceV");
  }
}

void MSMIndColumn::putSliceV (rownr_t rownr, const Slicer& ns,
                              const ArrayBase& arr)
{
  Data* data = MSMINDCOLUMN_GETDATA(rownr);
  const IPosition& shp = data->shape();
  IPosition blc, trc, inc;
  ns.inferShapeFromSource (shp, blc, trc, inc);
  switch (dtype()) {
  case TpBool:
    Array<Bool>(shp, static_cast<Bool*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpUChar:
    Array<uChar>(shp, static_cast<uChar*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpShort:
    Array<Short>(shp, static_cast<Short*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpUShort:
    Array<uShort>(shp, static_cast<uShort*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpInt:
    Array<Int>(shp, static_cast<Int*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpUInt:
    Array<uInt>(shp, static_cast<uInt*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpInt64:
    Array<Int64>(shp, static_cast<Int64*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpFloat:
    Array<float>(shp, static_cast<float*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpDouble:
    Array<double>(shp, static_cast<double*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpComplex:
    Array<Complex>(shp, static_cast<Complex*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpDComplex:
    Array<DComplex>(shp, static_cast<DComplex*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  case TpString:
    Array<String>(shp, static_cast<String*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, False);
    break;
  default:
    throw DataManInvDT ("MSMIndColumn::putSliceV");
  }
  stmanPtr_p->setHasPut();
}
    

void MSMIndColumn::remove (rownr_t rownr)
{
  deleteArray (rownr);
  MSMColumn::remove (rownr);
}


void MSMIndColumn::deleteArray (rownr_t rownr)
{
  // Remove the array for this row (if there).
  delete MSMINDCOLUMN_GETDATA(rownr);
}



MSMIndColumn::Data::Data (const IPosition& shape, int dtype, int elemSize)
: shape_p (shape),
  data_p  (nullptr),
  data_is_string(dtype == TpString)
{
  Int64 nelem = shape.product();
  if (data_is_string) {
    data_p = new String[nelem];
  } else {
    data_p = new char[nelem * elemSize];
  }
}

MSMIndColumn::Data::~Data()
{
  if (data_is_string) {
    delete [] static_cast<String*>(data_p);
  } else {
    delete [] static_cast<char*>(data_p);
  }
}

} //# NAMESPACE CASACORE - END

