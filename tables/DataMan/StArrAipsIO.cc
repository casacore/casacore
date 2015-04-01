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
//# $Id$

#include <casacore/tables/DataMan/StArrAipsIO.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StManColumnArrayAipsIO::StManColumnArrayAipsIO (StManAipsIO* smptr,
						int dataType)
: StManColumnAipsIO (smptr, dataType, True),
  nrelem_p (0)
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
    nrelem_p = shape_p.product();
}


void StManColumnArrayAipsIO::addRow (uInt nrnew, uInt nrold)
{
    //# Extend data blocks if needed.
    StManColumnAipsIO::addRow (nrnew, nrold);
    //# Allocate the data arrays.
    void* ptr;
    for (; nrold<nrnew; nrold++) {
	ptr = allocData (nrelem_p, False);
	putArrayPtr (nrold, ptr);
    }
}

uInt StManColumnArrayAipsIO::ndim (uInt)
    { return shape_p.nelements(); }

IPosition StManColumnArrayAipsIO::shape (uInt)
    { return shape_p; }


Bool StManColumnArrayAipsIO::canAccessSlice (Bool& reask) const
{
    reask = False;
    return True;
}
Bool StManColumnArrayAipsIO::canAccessArrayColumn (Bool& reask) const
{
    reask = False;
    return True;
}

void StManColumnArrayAipsIO::getArrayfloatV (uInt rownr, Array<float>* arr)
{
    Bool deleteIt;
    float* data = arr->getStorage (deleteIt);
    objcopy (data, (const float*)(getArrayPtr (rownr)), nrelem_p);
    arr->putStorage (data, deleteIt);
}
void StManColumnArrayAipsIO::putArrayfloatV (uInt rownr,
					     const Array<float>* arr)
{
    Bool deleteIt;
    const float* data = arr->getStorage (deleteIt);
    objcopy ((float*)(getArrayPtr (rownr)), data, nrelem_p);
    arr->freeStorage (data, deleteIt);
    stmanPtr_p->setHasPut();
}
void StManColumnArrayAipsIO::getSlicefloatV (uInt rownr, const Slicer& ns,
					     Array<float>* arr)
{
    Array<float> tabarr (shape_p, (float*) (getArrayPtr (rownr)), SHARE);
    IPosition blc, trc, inc;
    ns.inferShapeFromSource (shape_p, blc, trc, inc);
    *arr = tabarr(blc, trc, inc);
}
void StManColumnArrayAipsIO::putSlicefloatV (uInt rownr, const Slicer& ns,
					     const Array<float>* arr)
{
    Array<float> tabarr (shape_p, (float*) (getArrayPtr (rownr)), SHARE);
    IPosition blc, trc, inc;
    ns.inferShapeFromSource (shape_p, blc, trc, inc);
    tabarr(blc, trc, inc) = *arr;
    stmanPtr_p->setHasPut();
}

#define STMANCOLUMNARRAYAIPSIO_GETPUT(T,NM) \
void StManColumnArrayAipsIO::aips_name2(getArray,NM) (uInt rownr, \
						      Array<T>* arr) \
{ \
    Bool deleteIt; \
    T* data = arr->getStorage (deleteIt); \
    objcopy (data, (const T*)(getArrayPtr (rownr)), nrelem_p); \
    arr->putStorage (data, deleteIt); \
} \
void StManColumnArrayAipsIO::aips_name2(putArray,NM) (uInt rownr, \
						      const Array<T>* arr) \
{ \
    Bool deleteIt; \
    const T* data = arr->getStorage (deleteIt); \
    objcopy ((T*)(getArrayPtr (rownr)), data, nrelem_p); \
    arr->freeStorage (data, deleteIt); \
    stmanPtr_p->setHasPut(); \
} \
void StManColumnArrayAipsIO::aips_name2(getSlice,NM) \
                          (uInt rownr, const Slicer& ns, Array<T>* arr) \
{ \
    Array<T> tabarr (shape_p, (T*) (getArrayPtr (rownr)), SHARE); \
    IPosition blc, trc, inc; \
    ns.inferShapeFromSource (shape_p, blc, trc, inc); \
    *arr = tabarr(blc, trc, inc); \
} \
void StManColumnArrayAipsIO::aips_name2(putSlice,NM) \
                          (uInt rownr, const Slicer& ns, const Array<T>* arr) \
{ \
    Array<T> tabarr (shape_p, (T*) (getArrayPtr (rownr)), SHARE); \
    IPosition blc, trc, inc; \
    ns.inferShapeFromSource (shape_p, blc, trc, inc); \
    tabarr(blc, trc, inc) = *arr; \
    stmanPtr_p->setHasPut(); \
}

STMANCOLUMNARRAYAIPSIO_GETPUT(Bool,BoolV)
STMANCOLUMNARRAYAIPSIO_GETPUT(uChar,uCharV)
STMANCOLUMNARRAYAIPSIO_GETPUT(Short,ShortV)
STMANCOLUMNARRAYAIPSIO_GETPUT(uShort,uShortV)
STMANCOLUMNARRAYAIPSIO_GETPUT(Int,IntV)
STMANCOLUMNARRAYAIPSIO_GETPUT(uInt,uIntV)
//#//STMANCOLUMNARRAYAIPSIO_GETPUT(float,floatV)
STMANCOLUMNARRAYAIPSIO_GETPUT(double,doubleV)
STMANCOLUMNARRAYAIPSIO_GETPUT(Complex,ComplexV)
STMANCOLUMNARRAYAIPSIO_GETPUT(DComplex,DComplexV)
STMANCOLUMNARRAYAIPSIO_GETPUT(String,StringV)


void StManColumnArrayAipsIO::getArrayColumnfloatV (Array<float>* arr)
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
void StManColumnArrayAipsIO::putArrayColumnfloatV (const Array<float>* arr)
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
    stmanPtr_p->setHasPut();
}
#define STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(T,NM) \
void StManColumnArrayAipsIO::aips_name2(getArrayColumn,NM) \
						      (Array<T>* arr) \
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
void StManColumnArrayAipsIO::aips_name2(putArrayColumn,NM) \
						      (const Array<T>* arr) \
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
    stmanPtr_p->setHasPut(); \
}

STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(Bool,BoolV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(uChar,uCharV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(Short,ShortV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(uShort,uShortV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(Int,IntV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(uInt,uIntV)
//#//STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(float,floatV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(double,doubleV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(Complex,ComplexV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(DComplex,DComplexV)
STMANCOLUMNARRAYAIPSIO_GETPUTCOLUMN(String,StringV)



void StManColumnArrayAipsIO::remove (uInt rownr)
{
    deleteArray (rownr);
    StManColumnAipsIO::remove (rownr);
}


Bool StManColumnArrayAipsIO::ok() const
{
    return StManColumnAipsIO::ok();
}


void StManColumnArrayAipsIO::deleteArray (uInt rownr)
{
    void* datap = getArrayPtr (rownr);
    deleteData (datap, False);
}


//# Write all data into AipsIO.
void StManColumnArrayAipsIO::putFile (uInt nrval, AipsIO& ios)
{
    // Version 2 does not write dtype, shape and nelem anymore.
    // They are automatically set on reconstruction of the storage manager.
    ios.putstart ("StManColumnArrayAipsIO", 2);      // class version 2
    StManColumnAipsIO::putFile (nrval, ios);
    ios.putend();
}

//# Read all data from AipsIO.
void StManColumnArrayAipsIO::getFile (uInt nrval, AipsIO& ios)
{
    uInt version = ios.getstart ("StManColumnArrayAipsIO");
    if (version == 1) {
	IPosition shape;
	uInt n;
	ios >> n;            // data type
	ios >> shape_p;
	ios >> n;            // nelem
    }
    StManColumnAipsIO::getFile (nrval, ios);
    ios.getend();
}


#define STMANCOLUMNARRAYAIPSIO_PUTDATA(T) \
    { \
	T** dpa = (T**)dp; \
	while (nrval--) { \
	    ios.put (nrelem_p, *dpa, False); \
	    dpa++; \
	} \
    }

void StManColumnArrayAipsIO::putData (void* dp, uInt nrval, AipsIO& ios)
{
    ios << nrval * nrelem_p;
    switch (dtype_p) {
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
    }
}


#define STMANCOLUMNARRAYAIPSIO_GETDATA(T) \
    { \
	uInt nr; \
	T** dparr = (T**)dp + inx; \
        T* dpd; \
	while (nrval--) { \
	    dpd = (T*) allocData (nrelem_p, False); \
	    *dparr++ = dpd; \
            if (version == 1) { \
	        ios >> nr; \
            } \
	    ios.get (nrelem_p, dpd); \
	} \
    }

void StManColumnArrayAipsIO::getData (void* dp, uInt inx, uInt nrval,
				      AipsIO& ios, uInt version)
{
    uInt nr;
    if (version > 1) {
	ios >> nr;
    }
    switch (dtype_p) {
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
    }
}

} //# NAMESPACE CASACORE - END

