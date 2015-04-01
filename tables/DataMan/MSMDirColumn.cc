//# MSMDirColumn.cc: Memory storage manager for fixed shape table arrays
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


#include <casacore/tables/DataMan/MSMDirColumn.h>
#include <casacore/tables/DataMan/MSMBase.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSMDirColumn::MSMDirColumn (MSMBase* smptr, int dataType)
: MSMColumn (smptr, dataType, True),
  nrelem_p  (0)
{}

MSMDirColumn::~MSMDirColumn()
{
  uInt nr = stmanPtr_p->nrow();
  for (uInt i=0; i<nr; i++) {
    deleteArray (i);
  }
}


void MSMDirColumn::setShapeColumn (const IPosition& shape)
{
  shape_p  = shape;
  nrelem_p = shape.product();
}


void MSMDirColumn::addRow (uInt nrnew, uInt nrold)
{
  //# Extend data blocks if needed.
  MSMColumn::addRow (nrnew, nrold);
  //# Allocate the fixed shape data arrays.
  void* ptr;
  for (; nrold<nrnew; nrold++) {
    ptr = allocData (nrelem_p, False);
    putArrayPtr (nrold, ptr);
  }
}

void MSMDirColumn::doCreate (uInt nrrow)
{
  addRow (nrrow, 0);
  for (uInt i=0; i<nrrow; i++) {
    initData (getArrayPtr(i), nrelem_p);
  } 
}

uInt MSMDirColumn::ndim (uInt)
  { return shape_p.nelements(); }

IPosition MSMDirColumn::shape (uInt)
  { return shape_p; }


Bool MSMDirColumn::canAccessSlice (Bool& reask) const
{
  reask = False;
  return True;
}
Bool MSMDirColumn::canAccessArrayColumn (Bool& reask) const
{
  reask = False;
  return True;
}

void MSMDirColumn::getArrayfloatV (uInt rownr, Array<float>* arr)
{
  Bool deleteIt;
  float* data = arr->getStorage (deleteIt);
  objcopy (data, (const float*)(getArrayPtr (rownr)), nrelem_p);
  arr->putStorage (data, deleteIt);
}
void MSMDirColumn::putArrayfloatV (uInt rownr, const Array<float>* arr)
{
  Bool deleteIt;
  const float* data = arr->getStorage (deleteIt);
  objcopy ((float*)(getArrayPtr (rownr)), data, nrelem_p);
  arr->freeStorage (data, deleteIt);
}
void MSMDirColumn::getSlicefloatV (uInt rownr, const Slicer& ns,
				   Array<float>* arr)
{
  Array<float> tabarr (shape_p, (float*) (getArrayPtr (rownr)), SHARE);
  IPosition blc, trc, inc;
  ns.inferShapeFromSource (shape_p, blc, trc, inc);
  *arr = tabarr(blc, trc, inc);
}
void MSMDirColumn::putSlicefloatV (uInt rownr, const Slicer& ns,
				   const Array<float>* arr)
{
  Array<float> tabarr (shape_p, (float*) (getArrayPtr (rownr)), SHARE);
  IPosition blc, trc, inc;
  ns.inferShapeFromSource (shape_p, blc, trc, inc);
  tabarr(blc, trc, inc) = *arr;
}

#define MSMARRCOLUMN_GETPUT(T,NM) \
void MSMDirColumn::aips_name2(getArray,NM) (uInt rownr, Array<T>* arr) \
{ \
  Bool deleteIt; \
  T* data = arr->getStorage (deleteIt); \
  objcopy (data, (const T*)(getArrayPtr (rownr)), nrelem_p); \
  arr->putStorage (data, deleteIt); \
} \
void MSMDirColumn::aips_name2(putArray,NM) (uInt rownr, const Array<T>* arr) \
{ \
  Bool deleteIt; \
  const T* data = arr->getStorage (deleteIt); \
  objcopy ((T*)(getArrayPtr (rownr)), data, nrelem_p); \
  arr->freeStorage (data, deleteIt); \
} \
void MSMDirColumn::aips_name2(getSlice,NM) \
                          (uInt rownr, const Slicer& ns, Array<T>* arr) \
{ \
  Array<T> tabarr (shape_p, (T*) (getArrayPtr (rownr)), SHARE); \
  IPosition blc, trc, inc; \
  ns.inferShapeFromSource (shape_p, blc, trc, inc); \
  *arr = tabarr(blc, trc, inc); \
} \
void MSMDirColumn::aips_name2(putSlice,NM) \
                          (uInt rownr, const Slicer& ns, const Array<T>* arr) \
{ \
  Array<T> tabarr (shape_p, (T*) (getArrayPtr (rownr)), SHARE); \
  IPosition blc, trc, inc; \
  ns.inferShapeFromSource (shape_p, blc, trc, inc); \
  tabarr(blc, trc, inc) = *arr; \
}

MSMARRCOLUMN_GETPUT(Bool,BoolV)
MSMARRCOLUMN_GETPUT(uChar,uCharV)
MSMARRCOLUMN_GETPUT(Short,ShortV)
MSMARRCOLUMN_GETPUT(uShort,uShortV)
MSMARRCOLUMN_GETPUT(Int,IntV)
MSMARRCOLUMN_GETPUT(uInt,uIntV)
//#//MSMARRCOLUMN_GETPUT(float,floatV)
MSMARRCOLUMN_GETPUT(double,doubleV)
MSMARRCOLUMN_GETPUT(Complex,ComplexV)
MSMARRCOLUMN_GETPUT(DComplex,DComplexV)
MSMARRCOLUMN_GETPUT(String,StringV)


void MSMDirColumn::getArrayColumnfloatV (Array<float>* arr)
{
  uInt nrmax = arr->shape()(arr->ndim()-1);
  Bool deleteItTarget;
  float* target = arr->getStorage (deleteItTarget);
  uInt nr;
  void* ext;
  uInt extnr = 0;
  while ((nr = nextExt (ext, extnr, nrmax))  >  0) {
    const float** dpa = (const float**)ext;
    for (uInt i=0; i<nr; i++) {
      objcopy (target, *dpa, nrelem_p);
      target += nrelem_p;
      dpa++;
    }
  }
  arr->putStorage (target, deleteItTarget);
}
void MSMDirColumn::putArrayColumnfloatV (const Array<float>* arr)
{
  uInt nrmax = arr->shape()(arr->ndim()-1);
  Bool deleteItTarget;
  const float* target = arr->getStorage (deleteItTarget);
  uInt nr;
  void* ext;
  uInt extnr = 0;
  while ((nr = nextExt (ext, extnr, nrmax))  >  0) {
    float** dpa = (float**)ext;
    for (uInt i=0; i<nr; i++) {
      objcopy (*dpa, target, nrelem_p);
      target += nrelem_p;
      dpa++;
    }
  }
  arr->freeStorage (target, deleteItTarget);
}
#define MSMARRCOLUMN_GETPUTCOLUMN(T,NM) \
void MSMDirColumn::aips_name2(getArrayColumn,NM) (Array<T>* arr) \
{ \
  uInt nrmax = arr->shape()(arr->ndim()-1); \
  Bool deleteItTarget; \
  T* target = arr->getStorage (deleteItTarget); \
  uInt nr; \
  void* ext; \
  uInt extnr = 0; \
  while ((nr = nextExt (ext, extnr, nrmax))  >  0) { \
    const T** dpa = (const T**)ext; \
    for (uInt i=0; i<nr; i++) { \
      objcopy (target, *dpa, nrelem_p); \
      target += nrelem_p; \
      dpa++; \
    } \
  } \
  arr->putStorage (target, deleteItTarget); \
} \
void MSMDirColumn::aips_name2(putArrayColumn,NM) (const Array<T>* arr) \
{ \
  uInt nrmax = arr->shape()(arr->ndim()-1); \
  Bool deleteItTarget; \
  const T* target = arr->getStorage (deleteItTarget); \
  uInt nr; \
  void* ext; \
  uInt extnr = 0; \
  while ((nr = nextExt (ext, extnr, nrmax))  >  0) { \
    T** dpa = (T**)ext; \
    for (uInt i=0; i<nr; i++) { \
      objcopy (*dpa, target, nrelem_p); \
      target += nrelem_p; \
      dpa++; \
    } \
  } \
  arr->freeStorage (target, deleteItTarget); \
}

MSMARRCOLUMN_GETPUTCOLUMN(Bool,BoolV)
MSMARRCOLUMN_GETPUTCOLUMN(uChar,uCharV)
MSMARRCOLUMN_GETPUTCOLUMN(Short,ShortV)
MSMARRCOLUMN_GETPUTCOLUMN(uShort,uShortV)
MSMARRCOLUMN_GETPUTCOLUMN(Int,IntV)
MSMARRCOLUMN_GETPUTCOLUMN(uInt,uIntV)
//#//MSMARRCOLUMN_GETPUTCOLUMN(float,floatV)
MSMARRCOLUMN_GETPUTCOLUMN(double,doubleV)
MSMARRCOLUMN_GETPUTCOLUMN(Complex,ComplexV)
MSMARRCOLUMN_GETPUTCOLUMN(DComplex,DComplexV)
MSMARRCOLUMN_GETPUTCOLUMN(String,StringV)



void MSMDirColumn::remove (uInt rownr)
{
  deleteArray (rownr);
  MSMColumn::remove (rownr);
}


Bool MSMDirColumn::ok() const
{
  return MSMColumn::ok();
}


void MSMDirColumn::deleteArray (uInt rownr)
{
  void* datap = getArrayPtr (rownr);
  deleteData (datap, False);
}

} //# NAMESPACE CASACORE - END

