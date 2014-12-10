//# ISMColumn.cc: The Column of the Incremental Storage Manager
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#include <casacore/tables/DataMan/ISMColumn.h>
#include <casacore/tables/DataMan/ISMBase.h>
#include <casacore/tables/DataMan/ISMBucket.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ISMColumn::ISMColumn (ISMBase* parent, int dataType, uInt colnr)
: StManColumn   (dataType),
  stmanPtr_p    (parent),
  fixedLength_p (0),
  colnr_p       (colnr),
  nrelem_p      (1),
  startRow_p    (-1),
  endRow_p      (-1),
  lastValue_p   (0),
  lastRowPut_p  (0)
{
    //# The increment in the column cache is always 0,
    //# because multiple rows refer to the same value.
    columnCache().setIncrement (0);
}

ISMColumn::~ISMColumn()
{
    clear();
}

void ISMColumn::clear()
{
    switch (dataType()) {
    case TpBool:
	delete [] (Bool*)lastValue_p;
	break;
    case TpUChar:
	delete [] (uChar*)lastValue_p;
	break;
    case TpShort:
	delete [] (Short*)lastValue_p;
	break;
    case TpUShort:
	delete [] (uShort*)lastValue_p;
	break;
    case TpInt:
	delete [] (Int*)lastValue_p;
	break;
    case TpUInt:
	delete [] (uInt*)lastValue_p;
	break;
    case TpFloat:
	delete [] (float*)lastValue_p;
	break;
    case TpDouble:
	delete [] (double*)lastValue_p;
	break;
    case TpComplex:
	delete [] (Complex*)lastValue_p;
	break;
    case TpDComplex:
	delete [] (DComplex*)lastValue_p;
	break;
    case TpString:
	delete [] (String*)lastValue_p;
	break;
    }
    lastValue_p = 0;
}

void ISMColumn::setShapeColumn (const IPosition& shape)
{
    nrelem_p = shape.product();
    shape_p  = shape;
}

uInt ISMColumn::ndim (uInt)
{
    return shape_p.nelements();
}
IPosition ISMColumn::shape (uInt)
{
    return shape_p;
}


void ISMColumn::addRow (uInt, uInt)
{
    //# Nothing to do.
}

void ISMColumn::remove (uInt bucketRownr, ISMBucket* bucket, uInt bucketNrrow,
			uInt newNrrow)
{
    uInt inx, stint, endint, offset;
    // Get the index where to remove the value.
    // If the rownr is not the start of the interval, index is one further.
    inx = bucket->getInterval (colnr_p, bucketRownr, bucketNrrow,
			       stint, endint, offset);
#ifdef AIPS_TRACE
    cout << "remove column " << colnr_p << ", row " << bucketRownr << " (nrelem="<<nrelem_p<<", leng="<<fixedLength_p<<")"<< endl;
    cout << "  Bucket start row " << bucketRownr << " (" << bucketNrrow << " rows)" << endl;
    cout << "  Interval " << stint << "-" << endint << " (row="<< bucketRownr<<", offset="<<offset<<")"<<endl;
#endif

    // Get bucket information needed to remove the data.
    Block<uInt>& rowIndex = bucket->rowIndex (colnr_p);
    Block<uInt>& offIndex = bucket->offIndex (colnr_p);
    uInt& nused = bucket->indexUsed (colnr_p);
    // Invalidate the last value read.
    columnCache().invalidate();
    startRow_p = -1;
    endRow_p   = -1;
    // We have to change the bucket, so let the cache set the dirty flag
    // for this bucket.
    stmanPtr_p->setBucketDirty();
    // If the row is single, remove the value by shifting left one value.
    if (stint == endint) {
	handleRemove (bucketRownr, bucket->get (offset));
	bucket->shiftLeft (inx, 1, rowIndex, offIndex, nused, fixedLength_p);
	//# We can also test if previous and next value are equal and
	//# shift left one more. However, that is not implemented.
    } else {
	// If not single, start of this interval does not change.
	// The index has to be incremented if row is at start of interval.
	if (bucketRownr == stint) {
	    inx++;
	}
    }
    // Decrement the row number for all following rows.
    for (uInt i=inx; i<nused; i++) {
	rowIndex[i]--;
    }
    // Decrement lastRowPut if beyond last row now.
    if (lastRowPut_p > newNrrow) {
	lastRowPut_p = newNrrow+1;
    }
}


void ISMColumn::getBoolV (uInt rownr, Bool* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Bool*)lastValue_p;
}
void ISMColumn::getuCharV (uInt rownr, uChar* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(uChar*)lastValue_p;
}
void ISMColumn::getShortV (uInt rownr, Short* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Short*)lastValue_p;
}
void ISMColumn::getuShortV (uInt rownr, uShort* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(uShort*)lastValue_p;
}
void ISMColumn::getIntV (uInt rownr, Int* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Int*)lastValue_p;
}
void ISMColumn::getuIntV (uInt rownr, uInt* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(uInt*)lastValue_p;
}
void ISMColumn::getfloatV (uInt rownr, float* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(float*)lastValue_p;
}
void ISMColumn::getdoubleV (uInt rownr, double* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(double*)lastValue_p;
}
void ISMColumn::getComplexV (uInt rownr, Complex* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Complex*)lastValue_p;
}
void ISMColumn::getDComplexV (uInt rownr, DComplex* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(DComplex*)lastValue_p;
}
void ISMColumn::getStringV (uInt rownr, String* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(String*)lastValue_p;
}

void ISMColumn::getScalarColumnBoolV (Vector<Bool>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getBoolV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(Bool*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnuCharV (Vector<uChar>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getuCharV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(uChar*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnShortV (Vector<Short>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getShortV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(Short*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnuShortV (Vector<uShort>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getuShortV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(uShort*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnIntV (Vector<Int>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getIntV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(Int*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnuIntV (Vector<uInt>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getuIntV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(uInt*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnfloatV (Vector<float>* dataPtr)
{
    //# Note: using getStorage/putStorage is about 3 times faster
    //# if the vector is consecutive, but it is slower if not.
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getfloatV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(float*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumndoubleV (Vector<double>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getdoubleV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(double*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnComplexV (Vector<Complex>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getComplexV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(Complex*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnDComplexV (Vector<DComplex>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getDComplexV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(DComplex*)lastValue_p;
	}
    }
}
void ISMColumn::getScalarColumnStringV (Vector<String>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    uInt rownr = 0;
    while (rownr < nrrow) {
	getStringV (rownr, &((*dataPtr)(rownr)));
	for (rownr++; Int(rownr)<=endRow_p; rownr++) {
	    (*dataPtr)(rownr) = *(String*)lastValue_p;
	}
    }
}

#define ISMCOLUMN_GET(T,NM) \
void ISMColumn::aips_name2(getScalarColumnCells,NM) \
                                             (const RefRows& rownrs, \
					      Vector<T>* values) \
{ \
    Bool delV; \
    T* value = values->getStorage (delV); \
    T* valptr = value; \
    const ColumnCache& cache = columnCache(); \
    if (rownrs.isSliced()) { \
        RefRowsSliceIter iter(rownrs); \
        while (! iter.pastEnd()) { \
            uInt rownr = iter.sliceStart(); \
            uInt end = iter.sliceEnd(); \
            uInt incr = iter.sliceIncr(); \
            while (rownr <= end) { \
                if (rownr < cache.start()  ||  rownr > cache.end()) { \
                    aips_name2(get,NM) (rownr, valptr); \
                    DebugAssert (cache.incr() == 0, AipsError); \
                } \
                const T* cacheValue = (const T*)(cache.dataPtr()); \
                uInt endrow = min (end, cache.end()); \
                while (rownr <= endrow) { \
	            *valptr++ = *cacheValue; \
                    rownr += incr; \
	        } \
     	    } \
	    iter++; \
        } \
    } else { \
        const Vector<uInt>& rowvec = rownrs.rowVector(); \
        uInt nr = rowvec.nelements(); \
        if (nr > 0) { \
            Bool delR; \
            const uInt* rows = rowvec.getStorage (delR); \
            if (rows[0] < cache.start()  ||  rows[0] > cache.end()) { \
                aips_name2(get,NM) (0, &(value[0])); \
            } \
            const T* cacheValue = (const T*)(cache.dataPtr()); \
            uInt strow = cache.start(); \
            uInt endrow = cache.end(); \
            AlwaysAssert (cache.incr() == 0, AipsError); \
            for (uInt i=0; i<nr; i++) { \
	        uInt rownr = rows[i]; \
                if (rownr >= strow  &&  rownr <= endrow) { \
	            value[i] = *cacheValue; \
	        } else { \
	            aips_name2(get,NM) (rownr, &(value[i])); \
                    cacheValue = (const T*)(cache.dataPtr()); \
                    strow = cache.start(); \
                    endrow = cache.end(); \
	        } \
	    } \
            rowvec.freeStorage (rows, delR); \
        } \
    } \
    values->putStorage (value, delV); \
}
ISMCOLUMN_GET(Bool,BoolV)
ISMCOLUMN_GET(uChar,uCharV)
ISMCOLUMN_GET(Short,ShortV)
ISMCOLUMN_GET(uShort,uShortV)
ISMCOLUMN_GET(Int,IntV)
ISMCOLUMN_GET(uInt,uIntV)
ISMCOLUMN_GET(float,floatV)
ISMCOLUMN_GET(double,doubleV)
ISMCOLUMN_GET(Complex,ComplexV)
ISMCOLUMN_GET(DComplex,DComplexV)
ISMCOLUMN_GET(String,StringV)

void ISMColumn::getValue (uInt rownr, void* value, Bool setCache)
{
    // Get the bucket with its row number boundaries.
    uInt bucketStartRow, bucketNrrow;
    ISMBucket* bucket = stmanPtr_p->getBucket (rownr, bucketStartRow,
					       bucketNrrow);
    // Get the interval in the bucket with its rownr boundaries.
    rownr -= bucketStartRow;
    uInt offset, stint, endint;
    bucket->getInterval (colnr_p, rownr, bucketNrrow, stint, endint, offset);
    // Get the value.
    // Set the start and end rownr for which this value is valid.
    readFunc_p (value, bucket->get (offset), nrcopy_p);
    startRow_p = bucketStartRow + stint;
    endRow_p   = bucketStartRow + endint;
    if (setCache) {
	columnCache().set (startRow_p, endRow_p, lastValue_p);
    }
}

void ISMColumn::putBoolV (uInt rownr, const Bool* value)
{
    putValue (rownr, value);
}
void ISMColumn::putuCharV (uInt rownr, const uChar* value)
{
    putValue (rownr, value);
}
void ISMColumn::putShortV (uInt rownr, const Short* value)
{
    putValue (rownr, value);
}
void ISMColumn::putuShortV (uInt rownr, const uShort* value)
{
    putValue (rownr, value);
}
void ISMColumn::putIntV (uInt rownr, const Int* value)
{
    putValue (rownr, value);
}
void ISMColumn::putuIntV (uInt rownr, const uInt* value)
{
    putValue (rownr, value);
}
void ISMColumn::putfloatV (uInt rownr, const float* value)
{
    putValue (rownr, value);
}
void ISMColumn::putdoubleV (uInt rownr, const double* value)
{
    putValue (rownr, value);
}
void ISMColumn::putComplexV (uInt rownr, const Complex* value)
{
    putValue (rownr, value);
}
void ISMColumn::putDComplexV (uInt rownr, const DComplex* value)
{
    putValue (rownr, value);
}
void ISMColumn::putStringV (uInt rownr, const String* value)
{
    putValue (rownr, value);
}

void ISMColumn::putScalarColumnBoolV (const Vector<Bool>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnuCharV (const Vector<uChar>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnShortV (const Vector<Short>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnuShortV (const Vector<uShort>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnIntV (const Vector<Int>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnuIntV (const Vector<uInt>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnfloatV (const Vector<float>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumndoubleV (const Vector<double>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnComplexV (const Vector<Complex>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnDComplexV (const Vector<DComplex>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}
void ISMColumn::putScalarColumnStringV (const Vector<String>* dataPtr)
{
    uInt nrrow = dataPtr->nelements();
    for (uInt i=0; i<nrrow; i++) {
	putValue (i, &((*dataPtr)(i)));
    }
}

void ISMColumn::getArrayBoolV (uInt rownr, Array<Bool>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<Bool> (shape_p, (Bool*)lastValue_p, SHARE);
}

void ISMColumn::putArrayBoolV (uInt rownr, const Array<Bool>* value)
{
    Bool deleteIt;
    const Bool* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayuCharV (uInt rownr, Array<uChar>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<uChar> (shape_p, (uChar*)lastValue_p, SHARE);
}

void ISMColumn::putArrayuCharV (uInt rownr, const Array<uChar>* value)
{
    Bool deleteIt;
    const uChar* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayShortV (uInt rownr, Array<Short>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<Short> (shape_p, (Short*)lastValue_p, SHARE);
}

void ISMColumn::putArrayShortV (uInt rownr, const Array<Short>* value)
{
    Bool deleteIt;
    const Short* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayuShortV (uInt rownr, Array<uShort>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<uShort> (shape_p, (uShort*)lastValue_p, SHARE);
}

void ISMColumn::putArrayuShortV (uInt rownr, const Array<uShort>* value)
{
    Bool deleteIt;
    const uShort* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayIntV (uInt rownr, Array<Int>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<Int> (shape_p, (Int*)lastValue_p, SHARE);
}

void ISMColumn::putArrayIntV (uInt rownr, const Array<Int>* value)
{
    Bool deleteIt;
    const Int* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayuIntV (uInt rownr, Array<uInt>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<uInt> (shape_p, (uInt*)lastValue_p, SHARE);
}

void ISMColumn::putArrayuIntV (uInt rownr, const Array<uInt>* value)
{
    Bool deleteIt;
    const uInt* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayfloatV (uInt rownr, Array<float>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<float> (shape_p, (float*)lastValue_p, SHARE);
}

void ISMColumn::putArrayfloatV (uInt rownr, const Array<float>* value)
{
    Bool deleteIt;
    const float* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArraydoubleV (uInt rownr, Array<double>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<double> (shape_p, (double*)lastValue_p, SHARE);
}

void ISMColumn::putArraydoubleV (uInt rownr, const Array<double>* value)
{
    Bool deleteIt;
    const double* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayComplexV (uInt rownr, Array<Complex>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<Complex> (shape_p, (Complex*)lastValue_p, SHARE);
}

void ISMColumn::putArrayComplexV (uInt rownr, const Array<Complex>* value)
{
    Bool deleteIt;
    const Complex* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayDComplexV (uInt rownr, Array<DComplex>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<DComplex> (shape_p, (DComplex*)lastValue_p, SHARE);
}

void ISMColumn::putArrayDComplexV (uInt rownr, const Array<DComplex>* value)
{
    Bool deleteIt;
    const DComplex* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}
void ISMColumn::getArrayStringV (uInt rownr, Array<String>* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    *value = Array<String> (shape_p, (String*)lastValue_p, SHARE);
}

void ISMColumn::putArrayStringV (uInt rownr, const Array<String>* value)
{
    Bool deleteIt;
    const String* data = value->getStorage (deleteIt);
    putValue (rownr, data);
    value->freeStorage (data, deleteIt);
}


void ISMColumn::putValue (uInt rownr, const void* value)
{
    // Get the bucket and interval to which the row belongs.
    uInt bucketStartRow, bucketNrrow;
    ISMBucket* bucket = stmanPtr_p->getBucket (rownr, bucketStartRow,
					       bucketNrrow);
    uInt bucketRownr = rownr - bucketStartRow;
    uInt inx, stint, endint, offset;
    // Get the index where to add/replace the new value.
    // Note: offset gives the offset of the current value, which is often NOT
    //       the same as offIndex[inx] (usually it is offIndex[inx-1]).
    inx = bucket->getInterval (colnr_p, bucketRownr, bucketNrrow,
			       stint, endint, offset);

#ifdef AIPS_TRACE
    cout << "put column " << colnr_p << ", row " << rownr << " (nrelem="<<nrelem_p<<", leng="<<fixedLength_p<<")"<< endl;
    cout << "  Bucket start row " << bucketStartRow << " (" << bucketNrrow << " rows)" << endl;
    cout << "  Interval " << stint << "-" << endint << " (row="<< bucketRownr<<", offset="<<offset<<")"<<endl;
#endif

    // Get bucket information needed to write the data.
    Block<uInt>& rowIndex = bucket->rowIndex (colnr_p);
    Block<uInt>& offIndex = bucket->offIndex (colnr_p);
    uInt& nused = bucket->indexUsed (colnr_p);
    // Determine if the new row is after the last row ever put for this column.
    Bool afterLastRowPut = False;
    if (rownr >= lastRowPut_p) {

#ifdef AIPS_TRACE
        cout << " after last row " << lastRowPut_p<<endl;
#endif

	afterLastRowPut = True;
	lastRowPut_p    = rownr+1;
    }
    // Invalidate the last value read.
    columnCache().invalidate();
    startRow_p = -1;
    endRow_p   = -1;
    // Exit if new value equals current value.
    readFunc_p (lastValue_p, bucket->get (offset), nrcopy_p);
    if (compareValue (value, lastValue_p)) {

#ifdef AIPS_TRACE
        cout << " equal value" << endl;
#endif

	return;
    }
    // We have to write the value, so let the cache set the dirty flag
    // for this bucket.
    stmanPtr_p->setBucketDirty();
    // Get the temporary buffer from the storage manager.
    uInt lenData;
    char* buffer = stmanPtr_p->tempBuffer();
    // Test if the value equals the previous one if at the start of
    // the interval (or equals next value if at the end of the interval).
    // Take care if row is first (or last) row in bucket.
    // The index of the next value is inx+1 if the row is the starting
    // row of the interval.
    Bool equalPrev = False;
    Bool equalNext = False;
    uInt nextInx = inx;
    if (bucketRownr == stint) {
	nextInx++;
	if (bucketRownr > 0) {
	    readFunc_p (lastValue_p, bucket->get (offIndex[inx-1]),
			nrcopy_p);
	    if (compareValue (value, lastValue_p)) {
		equalPrev = True;
	    }
	}
    }
    if (bucketRownr == endint) {
	if (bucketRownr < bucketNrrow-1) {
	    readFunc_p (lastValue_p, bucket->get (offIndex[nextInx]),
			nrcopy_p);
	    if (compareValue (value, lastValue_p)) {
		equalNext = True;
	    }
	}
    }
    // If the row is higher than the last row ever put for this column,
    // the value is valid for all rows from this row on.
    // If it is the first row in the bucket, replace the value.
    // If the new value equals the previous one, combine them.
    // Otherwise add it to this bucket.
    // In any case put the value in all further buckets.
    if (afterLastRowPut) {
	lenData = writeFunc_p (buffer, value, nrcopy_p);
	if (bucketRownr == 0) {
	    replaceData (bucket, bucketStartRow, bucketNrrow, bucketRownr,
			 offIndex[inx], buffer, lenData);
	}else{
	    if (equalPrev) {

#ifdef AIPS_TRACE
		cout << " equal prev";
#endif

		bucket->shiftLeft (inx, 1, rowIndex, offIndex, nused,
				   fixedLength_p);
	    }else{
		addData (bucket, bucketStartRow, bucketNrrow,
			 bucketRownr, inx, buffer, lenData, True);
	    }
	}
        putFromRow (rownr, buffer, lenData);
	return;
    }
    // If the new value matches previous and next, we can contract the
    // values by removing 2 of them.
    if (equalPrev && equalNext) {

#ifdef AIPS_TRACE
        cout << " equal prev and next"<<endl;
#endif

	bucket->shiftLeft (inx, 2, rowIndex, offIndex, nused, fixedLength_p);
	return;
    }
    // Determine if the value is the only one in the interval.
    // If the row to change is the first of the interval, increment
    // the interval start.
    Bool single = (stint==endint);
//#    if (!single  &&  bucketRownr == stint) {
//#	rowIndex[inx]++;
//#    }
    // If matching the previous, combine with previous interval
    // (which is already done by incrementing the rowIndex above).
    // If it was a single value, contract the intervals.
    if (equalPrev) {

#ifdef AIPS_TRACE
        cout << " equal prev";
#endif

	if (single) {

#ifdef AIPS_TRACE
	    cout << " and single";
#endif

	    bucket->shiftLeft (inx, 1, rowIndex, offIndex, nused,
			       fixedLength_p);
	}else{
	    rowIndex[inx]++;
	}

#ifdef AIPS_TRACE
	cout << endl;
#endif

	return;
    }
    // If equal to next value, act similarly as above.
    if (equalNext) {

#ifdef AIPS_TRACE
        cout << " equal next";
#endif

	if (single) {

#ifdef AIPS_TRACE
	    cout << " and single";
#endif

	    bucket->shiftLeft (inx, 1, rowIndex, offIndex, nused,
			       fixedLength_p);
	}
	rowIndex[inx]--;

#ifdef AIPS_TRACE
	cout << endl;
#endif

	return;
    }
    // We have to add or replace the new data value.
    // If the value is single, simply replace it.
    // This will also update the offset value if needed.
    if (single) {
	lenData = writeFunc_p (buffer, value, nrcopy_p);
	replaceData (bucket, bucketStartRow, bucketNrrow, bucketRownr,
		     offIndex[inx], buffer, lenData);
	return;
    }
    // Add the data item.
    // If the new value is in the middle of the interval, the
    // original value has to be duplicated. Give a derived class
    // the opportunity to handle the copy.
    // Do this before inserting the new value. Otherwise the new
    // value may get at the end of the bucket and fill up the bucket.
    // Thereafter inserting the duplicate results in a split and the new
    // value may get promoted to the new bucket iso. the original.
    if (bucketRownr > stint  &&  bucketRownr < endint) {
	lenData = writeFunc_p (buffer, lastValue_p, nrcopy_p);
	addData (bucket, bucketStartRow, bucketNrrow,
		 bucketRownr+1, inx, buffer, lenData);
	handleCopy (rownr, buffer);
	putValue (rownr, value);
    }else{
	lenData = writeFunc_p (buffer, value, nrcopy_p);
	addData (bucket, bucketStartRow, bucketNrrow,
		 bucketRownr, inx, buffer, lenData);
    }
}

void ISMColumn::putFromRow (uInt rownr, const char* data, uInt lenData)
{
    // Skip the first bucket, because that is the one containing the
    // row just written.
    // Note that the previous write may have resulted in a bucket split.
    // So the previously calculated end of the bucket may not be right
    // anymore. Therefore we start at the given row and skip that bucket.
#ifdef AIPS_TRACE
    cout << "  putFromRow";
#endif

    ISMBucket* bucket;
    uInt bucketNrrow;
    uInt cursor = 0;
    bucket = stmanPtr_p->nextBucket (cursor, rownr, bucketNrrow);
    // Loop through all buckets from the given row on.
    // Replace the starting value in them.
    while ((bucket = stmanPtr_p->nextBucket (cursor, rownr, bucketNrrow))
	                                                           != 0) {
#ifdef AIPS_TRACE
        cout << "," << rownr;
#endif

	// Set the dirty flag for this bucket.
	stmanPtr_p->setBucketDirty();
	replaceData (bucket, rownr, bucketNrrow, 0,
		     bucket->getOffset (colnr_p, 0), data, lenData);
	// The value has been duplicated; give a derived class the
	// opportunity to handle it.
	handleCopy (rownr, data);
    }

#ifdef AIPS_TRACE
        cout << endl;
#endif
}

void ISMColumn::putData (ISMBucket* bucket, uInt bucketStartRow,
			 uInt bucketNrrow, uInt bucketRownr,
			 const char* data, uInt lenData,
			 Bool afterLastRow, Bool canSplit)
{
    // Determine the index.
    uInt inx, start, end, dum3;
    inx = bucket->getInterval (colnr_p, bucketRownr, 0, start, end, dum3);
    if ((afterLastRow  &&  bucketRownr == 0)  ||  start == end) {
	Block<uInt>& offIndex = bucket->offIndex (colnr_p);
	replaceData (bucket, bucketStartRow, bucketNrrow, bucketRownr,
		     offIndex[inx], data, lenData, canSplit);
    }else{
	addData (bucket, bucketStartRow, bucketNrrow, bucketRownr, inx,
		 data, lenData, afterLastRow, canSplit);
    }
}

void ISMColumn::replaceData (ISMBucket* bucket, uInt bucketStartRow,
			     uInt bucketNrrow, uInt bucketRownr, uInt& offset,
			     const char* data, uInt lenData, Bool canSplit)
{
    // Replacing a value means removing the old value.
    // So give the opportunity to handle a removal before the
    // actual replace is done.
    // If the new value fits in the bucket, it can simply be replaced.
    uInt oldLeng = bucket->getLength (fixedLength_p, bucket->get (offset));
    if (bucket->canReplaceData (lenData, oldLeng)) {
	handleRemove (bucketRownr, bucket->get (offset));
	bucket->replaceData (offset, data, lenData, oldLeng);
	return;
    }
    // The bucket is too small, so split it in the middle (if allowed).
    AlwaysAssert (canSplit, AipsError);
    ISMBucket* left;
    ISMBucket* right;
    Block<Bool> duplicated;
    uInt splitRownr = bucket->split (left, right, duplicated,
				     bucketStartRow, bucketNrrow,
				     colnr_p, bucketRownr, lenData - oldLeng);

#ifdef AIPS_TRACE
    cout << "  replace split at rownr "<<splitRownr<<endl;
#endif

    handleSplit (*right, duplicated);
    // Replace the bucket by the left one (its starting row is the same).
    bucket->copy (*left);
    delete left;
    // Replace the data in the correct part.
    if (bucketRownr >= splitRownr) {
	bucket = right;
	bucketRownr -= splitRownr;
    }
    uInt& offs = bucket->getOffset (colnr_p, bucketRownr);
    handleRemove (bucketRownr, bucket->get (offs));
    bucket->replaceData (offs, data, lenData, oldLeng);
    // Add the right bucket to the index.
    stmanPtr_p->addBucket (splitRownr + bucketStartRow, right);
}

Bool ISMColumn::addData (ISMBucket* bucket, uInt bucketStartRow,
			 uInt bucketNrrow, uInt bucketRownr, uInt inx,
			 const char* data, uInt lenData,
			 Bool afterLastRow, Bool canSplit)
{
    // If the value fits in the bucket, it can simply be added.
    if (bucket->canAddData (lenData)) {
	bucket->addData (colnr_p, bucketRownr, inx, data, lenData);
	return False;
    }
    // The bucket is too small, so split it in the middle (if allowed).
    AlwaysAssert (canSplit, AipsError);
    ISMBucket* left;
    ISMBucket* right;
    Block<Bool> duplicated;
    uInt splitRownr = bucket->split (left, right, duplicated,
				     bucketStartRow, bucketNrrow,
				     colnr_p, bucketRownr, lenData);

#ifdef AIPS_TRACE
    cout << "  add split at rownr "<<splitRownr<<endl;
#endif

    handleSplit (*right, duplicated);
    // Replace the bucket by the left one (its starting row is the same).
    bucket->copy (*left);
    delete left;
    // Add the data to the correct part.
    uInt startRow = bucketStartRow;
    uInt nrrow    = splitRownr;
    if (bucketRownr >= splitRownr) {
	bucket = right;
	bucketRownr -= splitRownr;
	startRow    += splitRownr;
	nrrow        = bucketNrrow - splitRownr;
    }
    // The next put cannot split anymore.
    putData (bucket, startRow, nrrow, bucketRownr, data, lenData,
	     afterLastRow, False);
    // Add the right bucket to the index.
    stmanPtr_p->addBucket (splitRownr + bucketStartRow, right);
    return True;
}

#ifdef AIPS_TRACE
void ISMColumn::handleCopy (uInt rownr, const char*)
{
    cout << "   handleCopy for row " << rownr
	 << ", column " << colnr_p << endl;
#else
void ISMColumn::handleCopy (uInt, const char*)
{
#endif
}

#ifdef AIPS_TRACE
void ISMColumn::handleRemove (uInt rownr, const char*)
{
    cout << "   handleRemove for row " << rownr
	 << ", column " << colnr_p << endl;
#else
void ISMColumn::handleRemove (uInt, const char*)
{
#endif
}


void ISMColumn::handleSplit (ISMBucket& bucket, const Block<Bool>& duplicated)
{
    // Loop through all columns.
    // If the split duplicated a value, handle the copied value.
    uInt nrcol = stmanPtr_p->ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	if (duplicated[i]) {
	    uInt offset = bucket.getOffset (i, 0);
	    stmanPtr_p->getColumn(i).handleCopy (0, bucket.get (offset));
	}
    }
}


Bool ISMColumn::compareValue (const void* val1, const void* val2) const
{
    const char* v1 = (const char*)val1;
    const char* v2 = (const char*)val2;
    for (uInt i=0; i<nrelem_p; i++) {
	if (compareFunc_p (v1, v2)  !=  0) {
	    return False;
	}
	v1 += typeSize_p;
	v2 += typeSize_p;
    }
    return True;
}

void ISMColumn::init()
{
    clear();
    DataType dt = (DataType)dataType();
    typeSize_p = ValType::getTypeSize (dt);
    Bool asBigEndian = stmanPtr_p->asBigEndian();
    nrcopy_p = nrelem_p;
    if (dt == TpString) {
	fixedLength_p = 0;
    } else if (dt == TpBool) {
	fixedLength_p = (nrelem_p + 7) / 8;
    }else{
	fixedLength_p = ValType::getCanonicalSize (dt, asBigEndian);
	uInt nrel;
	ValType::getCanonicalFunc (dt, readFunc_p, writeFunc_p, nrel,
				   asBigEndian);
	nrcopy_p *= nrel;
	fixedLength_p *= nrelem_p;
    }
    switch (dt) {
    case TpBool:
	{
	    readFunc_p  = &Conversion::bitToBool;
	    writeFunc_p = &Conversion::boolToBit;
	    compareFunc_p = ObjCompare<Bool>::compare;
	    lastValue_p   = new Bool [nrelem_p];
	    Bool undef = False;
	    objset ((Bool*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpUChar:
	{
	    compareFunc_p = ObjCompare<uChar>::compare;
	    lastValue_p   = new uChar [nrelem_p];
	    uChar undef = 0;
	    objset ((uChar*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpShort:
	{
	    compareFunc_p = ObjCompare<Short>::compare;
	    lastValue_p   = new Short [nrelem_p];
	    Short undef = 0;
	    objset ((Short*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpUShort:
	{
	    compareFunc_p = ObjCompare<uShort>::compare;
	    lastValue_p   = new uShort [nrelem_p];
	    uShort undef = 0;
	    objset ((uShort*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpInt:
	{
	    compareFunc_p = ObjCompare<Int>::compare;
	    lastValue_p   = new Int [nrelem_p];
	    Int undef = 0;
	    objset ((Int*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpUInt:
	{
	    compareFunc_p = ObjCompare<uInt>::compare;
	    lastValue_p   = new uInt [nrelem_p];
	    uInt undef = 0;
	    objset ((uInt*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpFloat:
	{
	    compareFunc_p = ObjCompare<float>::compare;
	    lastValue_p   = new float [nrelem_p];
	    float undef = 0;
	    objset ((float*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpDouble:
	{
	    compareFunc_p = ObjCompare<double>::compare;
	    lastValue_p   = new double [nrelem_p];
	    double undef = 0;
	    objset ((double*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpComplex:
	{
	    compareFunc_p = ObjCompare<Complex>::compare;
	    lastValue_p   = new Complex [nrelem_p];
	    Complex undef;
	    objset ((Complex*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpDComplex:
	{
	    compareFunc_p = ObjCompare<DComplex>::compare;
	    lastValue_p   = new DComplex [nrelem_p];
	    DComplex undef;
	    objset ((DComplex*)lastValue_p, undef, nrelem_p);
	}
	break;
    case TpString:
	{
	    if (asBigEndian) {
		readFunc_p  = readStringBE;
		writeFunc_p = writeStringBE;
	    }else{
		readFunc_p  = readStringLE;
		writeFunc_p = writeStringLE;
	    }
	    compareFunc_p = ObjCompare<String>::compare;
	    lastValue_p   = new String [nrelem_p];
	    String undef;
	    objset ((String*)lastValue_p, undef, nrelem_p);
	}
	break;
    default:
	AlwaysAssert (0, AipsError);
    }
    AlwaysAssert (lastValue_p != 0, AipsError);
}

void ISMColumn::doCreate (ISMBucket* bucket)
{
    init();
    char* buffer = stmanPtr_p->tempBuffer();
    uInt leng = writeFunc_p (buffer, lastValue_p, nrcopy_p);
    bucket->addData (colnr_p, 0, 0, buffer, leng);
}

void ISMColumn::getFile (uInt nrrow)
{
    init();
    lastRowPut_p = nrrow;
}
Bool ISMColumn::flush (uInt, Bool)
{
    return False;
}
void ISMColumn::resync (uInt nrrow)
{
    // Invalidate the last value read.
    columnCache().invalidate();
    startRow_p   = -1;
    endRow_p     = -1;
    lastRowPut_p = nrrow;
}
void ISMColumn::reopenRW()
{}


Conversion::ValueFunction* ISMColumn::getReaduInt (Bool asBigEndian)
{
    if (asBigEndian) {
	return CanonicalConversion::getToLocal (static_cast<uInt*>(0));
    }
    return LECanonicalConversion::getToLocal (static_cast<uInt*>(0));
}
Conversion::ValueFunction* ISMColumn::getWriteuInt (Bool asBigEndian)
{
    if (asBigEndian) {
	return CanonicalConversion::getFromLocal (static_cast<uInt*>(0));
    }
    return LECanonicalConversion::getFromLocal (static_cast<uInt*>(0));
}


size_t ISMColumn::fromString (void* out, const void* in, size_t n,
			    Conversion::ValueFunction* writeLeng)
{
    // The first entry represents the length of the entire object.
    // If there is only one string, it is the length of the string.
    // If there are multiple strings, it is the length of all strings
    // (including the lengths of their lengths).
    // Note that the length of itself is also included.
    char* buf = (char*)out;
    uInt leng = 0;
    size_t strleng;
    if (n > 1) {
	leng = writeLeng (buf, &leng, 1);
    }
    for (size_t i=0; i<n; i++) {
	const String& str = ((const String*)in)[i];
	strleng = str.length();
	leng += writeLeng (buf + leng, &strleng, 1);
	memcpy (buf + leng, str.chars(), strleng);
	leng += strleng;
    }
    writeLeng (buf, &leng, 1);
    return leng;
}
size_t ISMColumn::toString (void* out, const void* in, size_t n,
                            Conversion::ValueFunction* readLeng)
{
    const char* buf = (const char*)in;
    uInt strleng;
    uInt leng = readLeng (&strleng, buf, 1);
    strleng -= leng;
    for (size_t i=0; i<n; i++) {
	if (n > 1) {
	    leng += readLeng (&strleng, buf + leng, 1);
	}
	String& str = static_cast<String*>(out)[i];
	str.resize (strleng);       // resize storage which adds trailing 0
	char* var = &(str[0]);      // get actual string
	memcpy (var, buf + leng, strleng);
#ifdef USE_OLD_STRING
	var[strleng] = '\0';
#endif
	leng += strleng;
    }
    return leng;
}
size_t ISMColumn::writeStringBE (void* out, const void* in, size_t n)
{
    return fromString (out, in, n, CanonicalConversion::fromLocalUInt);
}
size_t ISMColumn::readStringBE (void* out, const void* in, size_t n)
{
    return toString (out, in, n, CanonicalConversion::toLocalUInt);
}
size_t ISMColumn::writeStringLE (void* out, const void* in, size_t n)
{
    return fromString (out, in, n, LECanonicalConversion::fromLocalUInt);
}
size_t ISMColumn::readStringLE (void* out, const void* in, size_t n)
{
    return toString (out, in, n, LECanonicalConversion::toLocalUInt);
}

} //# NAMESPACE CASACORE - END

