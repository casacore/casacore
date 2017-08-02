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
//# $Id: MSMDirColumn.cc 20551 2009-03-25 00:11:33Z Malte.Marquarding $


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


void MSMDirColumn::getArrayV (uInt rownr, ArrayBase& arr)
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
void MSMDirColumn::putArrayV (uInt rownr, const ArrayBase& arr)
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


void MSMDirColumn::remove (uInt rownr)
{
  deleteArray (rownr);
  MSMColumn::remove (rownr);
}


void MSMDirColumn::deleteArray (uInt rownr)
{
  void* datap = getArrayPtr (rownr);
  deleteData (datap, False);
}

} //# NAMESPACE CASACORE - END

