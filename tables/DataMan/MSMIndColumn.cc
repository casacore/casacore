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
//#        Internet email: aips2-request@nrao.edu.
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
: MSMColumn (smptr, dataType, true)
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
    ptr->clear(dataType());
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

bool MSMIndColumn::isShapeDefined (rownr_t rownr)
  { return (getArrayPtr(rownr) == 0  ?  false : true); }

uint32_t MSMIndColumn::ndim (rownr_t rownr)
  { return getShape(rownr)->shape().nelements(); }

IPosition MSMIndColumn::shape (rownr_t rownr)
  { return getShape(rownr)->shape(); }

bool MSMIndColumn::canChangeShape() const
  { return true; }


void MSMIndColumn::getArrayV (rownr_t rownr, ArrayBase& arr)
{
  Data* data = getShape(rownr);   //# also checks if row contains data
  DebugAssert (data->shape().isEqual (arr.shape()), AipsError);
  bool deleteIt;
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
  bool deleteIt;
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
    arr.assignBase (Array<bool>(shp, static_cast<bool*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpUChar:
    arr.assignBase (Array<unsigned char>(shp, static_cast<unsigned char*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpShort:
    arr.assignBase (Array<short>(shp, static_cast<short*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpUShort:
    arr.assignBase (Array<uint16_t>(shp, static_cast<uint16_t*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpInt:
    arr.assignBase (Array<int32_t>(shp, static_cast<int32_t*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpUInt:
    arr.assignBase (Array<uint32_t>(shp, static_cast<uint32_t*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpInt64:
    arr.assignBase (Array<int64_t>(shp, static_cast<int64_t*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpFloat:
    arr.assignBase (Array<float>(shp, static_cast<float*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpDouble:
    arr.assignBase (Array<double>(shp, static_cast<double*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpComplex:
    arr.assignBase (Array<Complex>(shp, static_cast<Complex*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpDComplex:
    arr.assignBase (Array<DComplex>(shp, static_cast<DComplex*>(data->data()), SHARE)
                    (blc, trc, inc), false);
    break;
  case TpString:
    arr.assignBase (Array<String>(shp, static_cast<String*>(data->data()), SHARE)
                    (blc, trc, inc), false);
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
    Array<bool>(shp, static_cast<bool*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpUChar:
    Array<unsigned char>(shp, static_cast<unsigned char*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpShort:
    Array<int16_t>(shp, static_cast<int16_t*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpUShort:
    Array<uint16_t>(shp, static_cast<uint16_t*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpInt:
    Array<int32_t>(shp, static_cast<int32_t*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpUInt:
    Array<uint32_t>(shp, static_cast<uint32_t*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpInt64:
    Array<int64_t>(shp, static_cast<int64_t*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpFloat:
    Array<float>(shp, static_cast<float*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpDouble:
    Array<double>(shp, static_cast<double*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpComplex:
    Array<Complex>(shp, static_cast<Complex*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpDComplex:
    Array<DComplex>(shp, static_cast<DComplex*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
    break;
  case TpString:
    Array<String>(shp, static_cast<String*>(data->data()), SHARE)
      (blc, trc, inc).assignBase (arr, false);
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
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr);
  // Remove the array for this row (if there).
  if (ptr != 0) {
    ptr->clear(dataType());
    delete ptr;
  }
}



  MSMIndColumn::Data::Data (const IPosition& shape, int dtype, int elemSize)
: shape_p (shape),
  data_p  (0)
{
  int64_t nelem = shape.product();
  if (dtype == TpString) {
    data_p = new String[nelem];
  } else {
    data_p = new char[nelem * elemSize];
  }
}

MSMIndColumn::Data::~Data() noexcept(false)
{
  if (data_p != 0) {
    throw DataManInternalError("MSMIndColumn::dtor: data array not deleted");
  }
}

void MSMIndColumn::Data::clear (int dtype)
{
  if (dtype == TpString) {
    delete [] static_cast<String*>(data_p);
  } else {
    delete [] static_cast<char*>(data_p);
  }
  data_p = 0;
}

} //# NAMESPACE CASACORE - END

