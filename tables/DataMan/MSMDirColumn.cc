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


#include <casacore/tables/DataMan/MSMDirColumn.h>
#include <casacore/tables/DataMan/MSMBase.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/string.h>                           // for memcpy


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSMDirColumn::MSMDirColumn (MSMBase* smptr, int dataType)
: MSMColumn (smptr, dataType, True),
  nrelem_p  (0)
{}

MSMDirColumn::~MSMDirColumn()
{
  rownr_t nr = stmanPtr_p->nrow();
  for (rownr_t i=0; i<nr; i++) {
    deleteArray (i);
  }
}


void MSMDirColumn::setShapeColumn (const IPosition& shape)
{
  shape_p  = shape;
  nrelem_p = shape.product();
}


void MSMDirColumn::addRow (rownr_t nrnew, rownr_t nrold)
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

void MSMDirColumn::doCreate (rownr_t nrrow)
{
  addRow (nrrow, 0);
  for (rownr_t i=0; i<nrrow; i++) {
    initData (getArrayPtr(i), nrelem_p);
  } 
}

uInt MSMDirColumn::ndim (rownr_t)
  { return shape_p.nelements(); }

IPosition MSMDirColumn::shape (rownr_t)
  { return shape_p; }


void MSMDirColumn::getArrayV (rownr_t rownr, ArrayBase& arr)
{
    DebugAssert (shape_p.isEqual (arr.shape()), AipsError);
    Bool deleteIt;
    void* data = arr.getVStorage (deleteIt);
    if (dtype() == TpString) {
      objcopy (static_cast<String*>(data),
               static_cast<const String*>(getArrayPtr (rownr)),
               nrelem_p);
    } else {
      memcpy (static_cast<char*>(data),
              static_cast<const char*>(getArrayPtr (rownr)),
              elemSize() * nrelem_p);
    }
    arr.putVStorage (data, deleteIt);
}
void MSMDirColumn::putArrayV (rownr_t rownr, const ArrayBase& arr)
{
    DebugAssert (shape_p.isEqual (arr.shape()), AipsError);
    Bool deleteIt;
    const void* data = arr.getVStorage (deleteIt);
    if (dtype() == TpString) {
      objcopy (static_cast<String*>(getArrayPtr (rownr)),
               static_cast<const String*>(data),
               nrelem_p);
    } else {
      memcpy (static_cast<char*>(getArrayPtr (rownr)),
              static_cast<const char*>(data),
              elemSize() * nrelem_p);
    }
    arr.freeVStorage (data, deleteIt);
    stmanPtr_p->setHasPut();
}

void MSMDirColumn::getSliceV (rownr_t rownr, const Slicer& slicer, ArrayBase& arr)
{
  switch (dtype()) {
  case TpBool:
    doGetSlice (rownr, slicer, static_cast<Array<Bool>&>(arr));
    break;
  case TpUChar:
    doGetSlice (rownr, slicer, static_cast<Array<uChar>&>(arr));
    break;
  case TpShort:
    doGetSlice (rownr, slicer, static_cast<Array<Short>&>(arr));
    break;
  case TpUShort:
    doGetSlice (rownr, slicer, static_cast<Array<uShort>&>(arr));
    break;
  case TpInt:
    doGetSlice (rownr, slicer, static_cast<Array<Int>&>(arr));
    break;
  case TpUInt:
    doGetSlice (rownr, slicer, static_cast<Array<uInt>&>(arr));
    break;
  case TpInt64:
    doGetSlice (rownr, slicer, static_cast<Array<Int64>&>(arr));
    break;
  case TpFloat:
    doGetSlice (rownr, slicer, static_cast<Array<Float>&>(arr));
    break;
  case TpDouble:
    doGetSlice (rownr, slicer, static_cast<Array<Double>&>(arr));
    break;
  case TpComplex:
    doGetSlice (rownr, slicer, static_cast<Array<Complex>&>(arr));
    break;
  case TpDComplex:
    doGetSlice (rownr, slicer, static_cast<Array<DComplex>&>(arr));
    break;
  case TpString:
    doGetSlice (rownr, slicer, static_cast<Array<String>&>(arr));
    break;
  default:
    throw (DataManInvDT ("MSMDirColumn::getSlice"));
  }
}

void MSMDirColumn::putSliceV (rownr_t rownr, const Slicer& slicer, const ArrayBase& arr)
{
  switch (dtype()) {
  case TpBool:
    doPutSlice (rownr, slicer, static_cast<const Array<Bool>&>(arr));
    break;
  case TpUChar:
    doPutSlice (rownr, slicer, static_cast<const Array<uChar>&>(arr));
    break;
  case TpShort:
    doPutSlice (rownr, slicer, static_cast<const Array<Short>&>(arr));
    break;
  case TpUShort:
    doPutSlice (rownr, slicer, static_cast<const Array<uShort>&>(arr));
    break;
  case TpInt:
    doPutSlice (rownr, slicer, static_cast<const Array<Int>&>(arr));
    break;
  case TpUInt:
    doPutSlice (rownr, slicer, static_cast<const Array<uInt>&>(arr));
    break;
  case TpInt64:
    doPutSlice (rownr, slicer, static_cast<const Array<Int64>&>(arr));
    break;
  case TpFloat:
    doPutSlice (rownr, slicer, static_cast<const Array<Float>&>(arr));
    break;
  case TpDouble:
    doPutSlice (rownr, slicer, static_cast<const Array<Double>&>(arr));
    break;
  case TpComplex:
    doPutSlice (rownr, slicer, static_cast<const Array<Complex>&>(arr));
    break;
  case TpDComplex:
    doPutSlice (rownr, slicer, static_cast<const Array<DComplex>&>(arr));
    break;
  case TpString:
    doPutSlice (rownr, slicer, static_cast<const Array<String>&>(arr));
    break;
  default:
    throw (DataManInvDT ("MSMDirColumn::getSlice"));
  }
  stmanPtr_p->setHasPut();
}


void MSMDirColumn::remove (rownr_t rownr)
{
  deleteArray (rownr);
  MSMColumn::remove (rownr);
}


void MSMDirColumn::deleteArray (rownr_t rownr)
{
  void* datap = getArrayPtr (rownr);
  deleteData (datap, False);
}

} //# NAMESPACE CASACORE - END

