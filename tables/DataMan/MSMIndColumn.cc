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
//#
//# $Id$

#include <casacore/tables/DataMan/MSMIndColumn.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/DataMan/DataManError.h>


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
  uInt nr = stmanPtr_p->nrow();
  for (uInt i=0; i<nr; i++) {
    deleteArray (i);
  }
}

void MSMIndColumn::setShapeColumn (const IPosition& shape)
{
  fixedShape_p = shape;
}

void MSMIndColumn::setShape (uInt rownr, const IPosition& shape)
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
  // Create the shape and array together.
  // Fiddle a bit to store the shape at the beginning.
  ptr = new Data (shape, dataType());
  putArrayPtr (rownr, ptr);
}

//# Get the shape for the array (if any) in the given row.
//# Read shape if not read yet.
MSMIndColumn::Data* MSMIndColumn::getShape (uInt rownr)
{
  void* ptr = getArrayPtr(rownr);
  if (ptr == 0) {
    throw (DataManInvOper ("MSM: no array in row " +
			   String::toString(rownr) +
			   " of " + stmanPtr_p->fileName()));
  }
  return static_cast<Data*>(ptr);
}

Bool MSMIndColumn::isShapeDefined (uInt rownr)
  { return (getArrayPtr(rownr) == 0  ?  False : True); }

uInt MSMIndColumn::ndim (uInt rownr)
  { return getShape(rownr)->shape().nelements(); }

IPosition MSMIndColumn::shape (uInt rownr)
  { return getShape(rownr)->shape(); }

Bool MSMIndColumn::canChangeShape() const
  { return True; }


Bool MSMIndColumn::canAccessSlice (Bool& reask) const
{
  reask = False;
  return True;
}


void MSMIndColumn::getArrayfloatV (uInt rownr, Array<float>* arr)
{
  float* ptr = static_cast<float*>(MSMINDCOLUMN_GETDATA(rownr)->data());
  Bool deleteIt;
  float* data = arr->getStorage (deleteIt);
  objcopy (data, ptr, arr->nelements());
  arr->putStorage (data, deleteIt);
}

void MSMIndColumn::putArrayfloatV (uInt rownr, const Array<float>* arr)
{
  float* ptr = static_cast<float*>(MSMINDCOLUMN_GETDATA(rownr)->data());
  Bool deleteIt;
  const float* data = arr->getStorage (deleteIt);
  objcopy (ptr, data, arr->nelements());
  arr->freeStorage (data, deleteIt);
}

void MSMIndColumn::getSlicefloatV (uInt rownr, const Slicer& ns,
				   Array<float>* arr)
{
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr);
  const IPosition& shape = ptr->shape();
  Array<float> tabarr (shape, static_cast<float*>(ptr->data()), SHARE);
  IPosition blc, trc, inc;
  ns.inferShapeFromSource (shape, blc, trc, inc);
  *arr = tabarr(blc, trc, inc);
}

void MSMIndColumn::putSlicefloatV (uInt rownr, const Slicer& ns,
				   const Array<float>* arr)
{
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr);
  const IPosition& shape = ptr->shape();
  Array<float> tabarr (shape, static_cast<float*>(ptr->data()), SHARE);
  IPosition blc, trc, inc;
  ns.inferShapeFromSource (shape, blc, trc, inc);
  tabarr(blc, trc, inc) = *arr;
}
    

#define MSMINDCOLUMN_GETPUT(T,NM) \
void MSMIndColumn::aips_name2(getArray,NM) (uInt rownr, Array<T>* arr) \
{ \
  T* ptr = static_cast<T*>(MSMINDCOLUMN_GETDATA(rownr)->data()); \
  Bool deleteIt; \
  T* data = arr->getStorage (deleteIt); \
  objcopy (data, ptr, arr->nelements()); \
  arr->putStorage (data, deleteIt); \
} \
void MSMIndColumn::aips_name2(putArray,NM) (uInt rownr, const Array<T>* arr) \
{ \
  T* ptr = static_cast<T*>(MSMINDCOLUMN_GETDATA(rownr)->data()); \
  Bool deleteIt; \
  const T* data = arr->getStorage (deleteIt); \
  objcopy (ptr, data, arr->nelements()); \
  arr->freeStorage (data, deleteIt); \
} \
void MSMIndColumn::aips_name2(getSlice,NM) \
                             (uInt rownr, const Slicer& ns, Array<T>* arr) \
{ \
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr); \
  const IPosition& shape = ptr->shape(); \
  Array<T> tabarr (shape, static_cast<T*>(ptr->data()), SHARE); \
  IPosition blc, trc, inc; \
  ns.inferShapeFromSource (shape, blc, trc, inc); \
  *arr = tabarr(blc, trc, inc); \
} \
void MSMIndColumn::aips_name2(putSlice,NM) \
                        (uInt rownr, const Slicer& ns, const Array<T>* arr) \
{ \
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr); \
  const IPosition& shape = ptr->shape(); \
  Array<T> tabarr (shape, static_cast<T*>(ptr->data()), SHARE); \
  IPosition blc, trc, inc; \
  ns.inferShapeFromSource (shape, blc, trc, inc); \
  tabarr(blc, trc, inc) = *arr; \
}

MSMINDCOLUMN_GETPUT(Bool,BoolV)
MSMINDCOLUMN_GETPUT(uChar,uCharV)
MSMINDCOLUMN_GETPUT(Short,ShortV)
MSMINDCOLUMN_GETPUT(uShort,uShortV)
MSMINDCOLUMN_GETPUT(Int,IntV)
MSMINDCOLUMN_GETPUT(uInt,uIntV)
//#//MSMINDCOLUMN_GETPUT(float,floatV)
MSMINDCOLUMN_GETPUT(double,doubleV)
MSMINDCOLUMN_GETPUT(Complex,ComplexV)
MSMINDCOLUMN_GETPUT(DComplex,DComplexV)
MSMINDCOLUMN_GETPUT(String,StringV)


void MSMIndColumn::remove (uInt rownr)
{
  deleteArray (rownr);
  MSMColumn::remove (rownr);
}


void MSMIndColumn::deleteArray (uInt rownr)
{
  Data* ptr = MSMINDCOLUMN_GETDATA(rownr);
  // Make sure possible IPosition storage is removed.
  if (ptr != 0) {
    ptr->clear(dataType());
    delete ptr;
  }
}



MSMIndColumn::Data::Data (const IPosition& shape, int dtype)
: shape_p (shape),
  data_p  (0)
{
  Int nelem = shape.product();
  switch (dtype) {
  case TpBool:
    data_p = new Bool[nelem];
    break;
  case TpChar:
    data_p = new Char[nelem];
    break;
  case TpUChar:
    data_p = new uChar[nelem];
    break;
  case TpShort:
    data_p = new Short[nelem];
    break;
  case TpUShort:
    data_p = new uShort[nelem];
    break;
  case TpInt:
    data_p = new Int[nelem];
    break;
  case TpUInt:
    data_p = new uInt[nelem];
    break;
  case TpFloat:
    data_p = new Float[nelem];
    break;
  case TpDouble:
    data_p = new Double[nelem];
    break;
  case TpComplex:
    data_p = new Complex[nelem];
    break;
  case TpDComplex:
    data_p = new DComplex[nelem];
    break;
  case TpString:
    data_p = new String[nelem];
    break;
  default:
    throw (DataManInvDT("MSMIndColumn"));
  }
}

MSMIndColumn::Data::~Data()
{
  if (data_p != 0) {
    throw DataManInternalError("MSMIndColumn::dtor: data array not deleted");
  }
}

void MSMIndColumn::Data::clear (int dtype)
{
  switch (dtype) {
  case TpBool:
    delete [] static_cast<Bool*>(data_p);
    break;
  case TpChar:
    delete [] static_cast<Char*>(data_p);
    break;
  case TpUChar:
    delete [] static_cast<uChar*>(data_p);
    break;
  case TpShort:
    delete [] static_cast<Short*>(data_p);
    break;
  case TpUShort:
    delete [] static_cast<uShort*>(data_p);
    break;
  case TpInt:
    delete [] static_cast<Int*>(data_p);
    break;
  case TpUInt:
    delete [] static_cast<uInt*>(data_p);
    break;
  case TpFloat:
    delete [] static_cast<Float*>(data_p);
    break;
  case TpDouble:
    delete [] static_cast<Double*>(data_p);
    break;
  case TpComplex:
    delete [] static_cast<Complex*>(data_p);
    break;
  case TpDComplex:
    delete [] static_cast<DComplex*>(data_p);
    break;
  case TpString:
    delete [] static_cast<String*>(data_p);
    break;
  default:
    throw (DataManInvDT("MSMIndColumn"));
  }
  data_p = 0;
}

} //# NAMESPACE CASACORE - END

