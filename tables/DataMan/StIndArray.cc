//# StIndArray.cc: Read/write indirect arrays
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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

#include <casacore/tables/DataMan/StIndArray.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StIndArray::StIndArray (Int64 fileOffset)
: fileOffset_p (fileOffset),
  arrOffset_p  (0)
{}

StIndArray::StIndArray (const StIndArray& that)
: fileOffset_p (that.fileOffset_p),
  arrOffset_p  (that.arrOffset_p),
  shape_p      (that.shape_p)
{}

StIndArray::~StIndArray()
{}

StIndArray& StIndArray::operator= (const StIndArray& that)
{
    if (this != &that) {
	fileOffset_p = that.fileOffset_p;
	arrOffset_p  = that.arrOffset_p;
	shape_p.resize (that.shape_p.nelements());
	shape_p      = that.shape_p;
    }
    return *this;
}

void StIndArray::getShape (StManArrayFile& ios)
{
    if (arrOffset_p == 0) {
	arrOffset_p = ios.getShape (fileOffset_p, shape_p);
    }
}

uInt StIndArray::refCount (StManArrayFile& ios)
{
    return ios.getRefCount (fileOffset_p);
}

void StIndArray::incrementRefCount (StManArrayFile& ios)
{
    uInt refCount = ios.getRefCount (fileOffset_p);
    refCount++;
    ios.putRefCount (refCount, fileOffset_p);
#ifdef AIPS_TRACE
    cout << "   incr refcount to " << refCount << " at "<<fileOffset_p<<endl;
#endif
}

void StIndArray::decrementRefCount (StManArrayFile& ios)
{
    uInt refCount = ios.getRefCount (fileOffset_p);
    refCount--;
    ios.putRefCount (refCount, fileOffset_p);
#ifdef AIPS_TRACE
    cout << "   decr refcount to " << refCount << " at "<<fileOffset_p<<endl;
#endif
}

Bool StIndArray::setShape (StManArrayFile& ios, int dataType,
			   const IPosition& shape)
{
    // Return immediately if the shape is defined and is the same.
    if (arrOffset_p != 0  &&  shape_p.isEqual (shape)) {
	return False;
    }
    // Set the shape.
    shape_p.resize (shape.nelements());
    shape_p = shape;
    // Store the shape in the file and allocate storage for the array.
    switch (dataType) {
    case TpBool:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<Bool*>(0));
	break;
    case TpUChar:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<uChar*>(0));
	break;
    case TpShort:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<Short*>(0));
	break;
    case TpUShort:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<uShort*>(0));
	break;
    case TpInt:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<Int*>(0));
	break;
    case TpUInt:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<uInt*>(0));
	break;
    case TpFloat:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<float*>(0));
	break;
    case TpDouble:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<double*>(0));
	break;
    case TpComplex:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<Complex*>(0));
	break;
    case TpDComplex:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<DComplex*>(0));
	break;
    case TpString:
	arrOffset_p = ios.putShape (shape_p, fileOffset_p, static_cast<String*>(0));
	break;
    }
    return True;
}


void StIndArray::copyData (StManArrayFile& ios, int dataType,
			   const StIndArray& other)

{
    // Check if both shape are equal.
    if (! shape_p.isEqual (other.shape_p)) {
	throw (DataManInternalError
	         ("StManIndArray::copyData shapes not conforming"));
    }
    switch (dataType) {
    case TpBool:
	ios.copyArrayBool (fileOffset_p + arrOffset_p,
			   other.fileOffset_p + other.arrOffset_p,
			   shape_p.product());
	break;
    case TpUChar:
	ios.copyArrayuChar (fileOffset_p + arrOffset_p,
			    other.fileOffset_p + other.arrOffset_p,
			    shape_p.product());
	break;
    case TpShort:
	ios.copyArrayShort (fileOffset_p + arrOffset_p,
			    other.fileOffset_p + other.arrOffset_p,
			    shape_p.product());
	break;
    case TpUShort:
	ios.copyArrayuShort (fileOffset_p + arrOffset_p,
			     other.fileOffset_p + other.arrOffset_p,
			     shape_p.product());
	break;
    case TpInt:
	ios.copyArrayInt (fileOffset_p + arrOffset_p,
			  other.fileOffset_p + other.arrOffset_p,
			  shape_p.product());
	break;
    case TpUInt:
	ios.copyArrayuInt (fileOffset_p + arrOffset_p,
			   other.fileOffset_p + other.arrOffset_p,
			   shape_p.product());
	break;
    case TpFloat:
	ios.copyArrayFloat (fileOffset_p + arrOffset_p,
			    other.fileOffset_p + other.arrOffset_p,
			    shape_p.product());
	break;
    case TpDouble:
	ios.copyArrayDouble (fileOffset_p + arrOffset_p,
			     other.fileOffset_p + other.arrOffset_p,
			     shape_p.product());
	break;
    case TpComplex:
	ios.copyArrayComplex (fileOffset_p + arrOffset_p,
			      other.fileOffset_p + other.arrOffset_p,
			      shape_p.product());
	break;
    case TpDComplex:
	ios.copyArrayDComplex (fileOffset_p + arrOffset_p,
			       other.fileOffset_p + other.arrOffset_p,
			       shape_p.product());
	break;
    case TpString:
	ios.copyArrayString (fileOffset_p + arrOffset_p,
			     other.fileOffset_p + other.arrOffset_p,
			     shape_p.product());
	break;
    }
}


void StIndArray::checkShape (const IPosition& userArrayShape,
			     const IPosition& tableArrayShape) const
{
    if (! userArrayShape.isEqual (tableArrayShape)) {
	throw (DataManInternalError
                 ("StManIndArray::get/put shapes not conforming"));
    }
}


void StIndArray::getArrayfloatV (StManArrayFile& ios, Array<float>* arr)
{
    checkShape (arr->shape(), shape_p);
    Bool deleteIt;
    float* value = arr->getStorage (deleteIt);
    ios.get (fileOffset_p + arrOffset_p, 0, shape_p.product(), value);
    arr->putStorage (value, deleteIt);
}
void StIndArray::putArrayfloatV (StManArrayFile& ios, const Array<float>* arr)
{
    checkShape (arr->shape(), shape_p);
    Bool deleteIt;
    const float* value = arr->getStorage (deleteIt);
    ios.put (fileOffset_p + arrOffset_p, 0, shape_p.product(), value);
    arr->freeStorage (value, deleteIt);
}
void StIndArray::getSlicefloatV (StManArrayFile& ios, const Slicer& ns,
				 Array<float>* arr)
{
    Bool deleteIt;
    float* value = arr->getStorage (deleteIt);
    getSliceData (ios, ns, value, arr->shape(),
		  &StIndArray::getVecfloatV);
    arr->putStorage (value, deleteIt);
}
void StIndArray::getVecfloatV (StManArrayFile& ios,
			       Int64 fileOffset,
			       uInt start, uInt leng, uInt inc,
			       uInt valInx, void* value)
{
    float* valp = (float*)value + valInx;
    if (inc == 1) {
	ios.get (fileOffset, start, leng, valp);
    }else{
	while (leng-- > 0) {
	    ios.get (fileOffset, start, 1, valp++);
	    start += inc;
	}
    }
}
void StIndArray::getSliceData (StManArrayFile& ios, const Slicer& ns,
      void* value, const IPosition& userShape,
      void (*getVec) (StManArrayFile&, Int64, uInt, uInt, uInt, uInt, void*))
{
    //# Check if the shape of the slice and user array match.
    uInt ndim = ns.ndim();
    IPosition blc(ndim), trc(ndim), inc(ndim), shape(ndim);
    shape = ns.inferShapeFromSource (shape_p, blc,trc,inc);
    checkShape (userShape, shape);
    //# We'll get a vector at the time; get its length.
    //# Get the offset of the array in the file.
    //# If the array is 1-dim, we can just use the vector get.
    uInt leng = shape(0);
    Int64 fileOffset = fileOffset_p + arrOffset_p;
    if (ndim == 1) {
	getVec (ios, fileOffset, blc(0), leng, inc(0), 0, value);
    }else{
	//# Loop through the slice a vector at a time.
	ArrayPositionIterator iter(shape, 1);
	IPosition pos(ndim);
	uInt i, offset;
	uInt count=0;
	while (! iter.pastEnd()) {
	    //# Get the iterator position in the slice and transform
	    //# that to the file-offset for the corresponding part in
	    //# the table array.
	    pos = iter.pos();
	    offset = 0;
	    for (i=ndim-1; i>0; i--) {
		offset += blc(i) + pos(i) * inc(i);
		offset *= shape_p(i-1);
	    }
	    offset += blc(0);
	    getVec (ios, fileOffset,
		    offset, leng, inc(0), count, value);
	    count += leng;
	    iter.next();
	}
    }
}

void StIndArray::putSlicefloatV (StManArrayFile& ios, const Slicer& ns,
				 const Array<float>* arr)
{
    Bool deleteIt;
    const float* value = arr->getStorage (deleteIt);
    putSliceData (ios, ns, value, arr->shape(),
		  &StIndArray::putVecfloatV);
    arr->freeStorage (value, deleteIt);
}
void StIndArray::putVecfloatV (StManArrayFile& ios,
			       Int64 fileOffset,
			       uInt start, uInt leng, uInt inc,
			       uInt valInx, const void* value)
{
    float* valp = (float*)value + valInx;
    if (inc == 1) {
	ios.put (fileOffset, start, leng, valp);
    }else{
	while (leng-- > 0) {
	    ios.put (fileOffset, start, 1, valp++);
	    start += inc;
	}
    }
}
//# putSliceData works similar to getSliceData.
void StIndArray::putSliceData (StManArrayFile& ios, const Slicer& ns,
	const void* value, const IPosition& userShape,
        void (*putVec) (StManArrayFile&, Int64, uInt, uInt, uInt, uInt,
			const void*))
{
    uInt ndim = ns.ndim();
    IPosition blc(ndim), trc(ndim), inc(ndim), shape(ndim);
    shape = ns.inferShapeFromSource (shape_p, blc,trc,inc);
    checkShape (userShape, shape);
    uInt leng = shape(0);
    Int64 fileOffset = fileOffset_p + arrOffset_p;
    if (ndim == 1) {
	putVec (ios, fileOffset, blc(0), leng, inc(0), 0, value);
    }else{
	ArrayPositionIterator iter(shape, 1);
	IPosition pos(ndim);
	uInt i, offset;
	uInt count=0;
	while (! iter.pastEnd()) {
	    pos = iter.pos();
	    offset = 0;
	    for (i=ndim-1; i>0; i--) {
		offset += blc(i) + pos(i) * inc(i);
		offset *= shape_p(i-1);
	    }
	    offset += blc(0);
	    putVec (ios, fileOffset,
		    offset, leng, inc(0), count, value);
	    count += leng;
	    iter.next();
	}
    }
}
    

#define STINDARRAY_GETPUT(T,NM) \
void StIndArray::aips_name2(getArray,NM) (StManArrayFile& ios, Array<T>* arr) \
{ \
    checkShape (arr->shape(), shape_p); \
    Bool deleteIt; \
    T* value = arr->getStorage (deleteIt); \
    ios.get (fileOffset_p + arrOffset_p, \
	     0, shape_p.product(), value); \
    arr->putStorage (value, deleteIt); \
} \
void StIndArray::aips_name2(putArray,NM) (StManArrayFile& ios, \
					  const Array<T>* arr) \
{ \
    checkShape (arr->shape(), shape_p); \
    Bool deleteIt; \
    const T* value = arr->getStorage (deleteIt); \
    ios.put (fileOffset_p + arrOffset_p, \
	     0, shape_p.product(), value); \
    arr->freeStorage (value, deleteIt); \
} \
void StIndArray::aips_name2(getSlice,NM) (StManArrayFile& ios, \
                                          const Slicer& ns, Array<T>* arr) \
{  \
    Bool deleteIt; \
    T* value = arr->getStorage (deleteIt); \
    getSliceData (ios, ns, value, arr->shape(), \
		  &StIndArray::aips_name2(getVec,NM)); \
    arr->putStorage (value, deleteIt); \
} \
void StIndArray::aips_name2(getVec,NM) \
                  (StManArrayFile& ios, Int64 fileOffset, \
		   uInt start, uInt leng, uInt inc, uInt valInx, void* value) \
{ \
    T* valp = (T*)value + valInx; \
    if (inc == 1) { \
	ios.get (fileOffset, start, leng, valp); \
    }else{ \
	while (leng-- > 0) { \
	    ios.get (fileOffset, start, 1, valp++); \
	    start += inc; \
	} \
    } \
} \
void StIndArray::aips_name2(putSlice,NM) (StManArrayFile& ios, \
                                          const Slicer& ns, \
                                          const Array<T>* arr) \
{ \
    Bool deleteIt; \
    const T* value = arr->getStorage (deleteIt); \
    putSliceData (ios, ns, value, arr->shape(), \
		  &StIndArray::aips_name2(putVec,NM)); \
    arr->freeStorage (value, deleteIt); \
} \
void StIndArray::aips_name2(putVec,NM) \
                  (StManArrayFile& ios, Int64 fileOffset, \
		   uInt start, uInt leng, uInt inc, uInt valInx, \
                   const void* value) \
{ \
    T* valp = (T*)value + valInx; \
    if (inc == 1) { \
	ios.put (fileOffset, start, leng, valp); \
    }else{ \
	while (leng-- > 0) { \
	    ios.put (fileOffset, start, 1, valp++); \
	    start += inc; \
	} \
    } \
}

STINDARRAY_GETPUT(Bool,BoolV)
STINDARRAY_GETPUT(uChar,uCharV)
STINDARRAY_GETPUT(Short,ShortV)
STINDARRAY_GETPUT(uShort,uShortV)
STINDARRAY_GETPUT(Int,IntV)
STINDARRAY_GETPUT(uInt,uIntV)
//#//STINDARRAY_GETPUT(float,floatV)
STINDARRAY_GETPUT(double,doubleV)
STINDARRAY_GETPUT(Complex,ComplexV)
STINDARRAY_GETPUT(DComplex,DComplexV)
STINDARRAY_GETPUT(String,StringV)

} //# NAMESPACE CASACORE - END

