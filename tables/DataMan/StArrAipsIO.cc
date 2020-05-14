//# StArrAipsIO.cc: Read/write a table column of arrays array using AipsIO
//# Copyright (C) 1994,1995,1996,1997,1999
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
//# $Id: StArrAipsIO.cc 20551 2009-03-25 00:11:33Z Malte.Marquarding $

#include <casacore/tables/DataMan/StArrAipsIO.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/string.h>                           // for memcpy


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StManColumnArrayAipsIO::StManColumnArrayAipsIO (StManAipsIO* smptr,
						int dataType)
: StManColumnAipsIO (smptr, dataType, True),
  nrelem_p  (0)
{}

StManColumnArrayAipsIO::~StManColumnArrayAipsIO()
{
  uInt nr = stmanPtr_p->nrow();
  for (uInt i=0; i<nr; i++) {
    deleteArray (i);
  }
}

void StManColumnArrayAipsIO::setShapeColumn (const IPosition& shape)
{
  shape_p  = shape;
  nrelem_p = shape.product();
}


void StManColumnArrayAipsIO::addRow (rownr_t nrnew, rownr_t nrold)
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

void StManColumnArrayAipsIO::doCreate (rownr_t nrrow)
{
  addRow (nrrow, 0);
  for (uInt i=0; i<nrrow; i++) {
    initData (getArrayPtr(i), nrelem_p);
  } 
}

uInt StManColumnArrayAipsIO::ndim (rownr_t)
  { return shape_p.nelements(); }

IPosition StManColumnArrayAipsIO::shape (rownr_t)
  { return shape_p; }


void StManColumnArrayAipsIO::getArrayV (rownr_t rownr, ArrayBase& arr)
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
void StManColumnArrayAipsIO::putArrayV (rownr_t rownr, const ArrayBase& arr)
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


void StManColumnArrayAipsIO::remove (rownr_t rownr)
{
  deleteArray (rownr);
  MSMColumn::remove (rownr);
}


void StManColumnArrayAipsIO::deleteArray (rownr_t rownr)
{
    void* datap = getArrayPtr (rownr);
    deleteData (datap, False);
}


//# Write all data into AipsIO.
void StManColumnArrayAipsIO::putFile (rownr_t nrval, AipsIO& ios)
{
    // Version 2 does not write dtype, shape and nelem anymore.
    // They are automatically set on reconstruction of the storage manager.
    ios.putstart ("StManColumnArrayAipsIO", 2);      // class version 2
    StManColumnAipsIO::putFile (nrval, ios);
    ios.putend();
}

//# Read all data from AipsIO.
void StManColumnArrayAipsIO::getFile (rownr_t nrval, AipsIO& ios)
{
    uInt version = ios.getstart ("StManColumnArrayAipsIO");
    if (version == 1) {
	IPosition shape;
	uInt n;
	ios >> n;            // data type
	ios >> shape;
	ios >> n;            // nelem
    }
    StManColumnAipsIO::getFile (nrval, ios);
    ios.getend();
}


#define STMANCOLUMNARRAYAIPSIO_PUTDATA(T) \
    { \
	T** dpa = (T**)dp; \
	while (nrval--) { \
          ios.put (nrelem_p, *dpa, False);   \
	    dpa++; \
	} \
    }

void StManColumnArrayAipsIO::putData (void* dp, uInt nrval, AipsIO& ios)
{
    ios << nrval * nrelem_p;
    switch (dtype()) {
    case TpBool:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(Bool)
	break;
    case TpUChar:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(uChar)
	break;
    case TpShort:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(Short)
	break;
    case TpUShort:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(uShort)
	break;
    case TpInt:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(Int)
	break;
    case TpUInt:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(uInt)
	break;
    case TpInt64:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(Int64)
	break;
    case TpFloat:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(float)
	break;
    case TpDouble:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(double)
	break;
    case TpComplex:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(Complex)
	break;
    case TpDComplex:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(DComplex)
	break;
    case TpString:
	STMANCOLUMNARRAYAIPSIO_PUTDATA(String)
	break;
    default:
      throw DataManInvDT("StArrAipsIO::putData");
    }
}


#define STMANCOLUMNARRAYAIPSIO_GETDATA(T) \
    { \
	uInt nr; \
	T** dparr = (T**)dp + inx; \
        T* dpd; \
	while (nrval--) { \
            dpd = (T*) allocData (nrelem_p, False);    \
	    *dparr++ = dpd; \
            if (version == 1) { \
	        ios >> nr; \
            } \
	    ios.get (nrelem_p, dpd);            \
	} \
    }

void StManColumnArrayAipsIO::getData (void* dp, uInt inx, uInt nrval,
				      AipsIO& ios, uInt version)
{
    uInt nr;
    if (version > 1) {
	ios >> nr;
    }
    switch (dtype()) {
    case TpBool:
	STMANCOLUMNARRAYAIPSIO_GETDATA(Bool)
	break;
    case TpUChar:
	STMANCOLUMNARRAYAIPSIO_GETDATA(uChar)
	break;
    case TpShort:
	STMANCOLUMNARRAYAIPSIO_GETDATA(Short)
	break;
    case TpUShort:
	STMANCOLUMNARRAYAIPSIO_GETDATA(uShort)
	break;
    case TpInt:
	STMANCOLUMNARRAYAIPSIO_GETDATA(Int)
	break;
    case TpUInt:
	STMANCOLUMNARRAYAIPSIO_GETDATA(uInt)
	break;
    case TpInt64:
	STMANCOLUMNARRAYAIPSIO_GETDATA(Int64)
	break;
    case TpFloat:
	STMANCOLUMNARRAYAIPSIO_GETDATA(float)
	break;
    case TpDouble:
	STMANCOLUMNARRAYAIPSIO_GETDATA(double)
	break;
    case TpComplex:
	STMANCOLUMNARRAYAIPSIO_GETDATA(Complex)
	break;
    case TpDComplex:
	STMANCOLUMNARRAYAIPSIO_GETDATA(DComplex)
	break;
    case TpString:
	STMANCOLUMNARRAYAIPSIO_GETDATA(String)
	break;
    default:
      throw DataManInvDT("StArrAipsIO::getData");
    }
}

} //# NAMESPACE CASACORE - END

