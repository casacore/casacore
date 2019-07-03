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
: StManColumnBase(dataType),
  stmanPtr_p     (parent),
  fixedLength_p  (0),
  colnr_p        (colnr),
  nrelem_p       (1),
  startRow_p     (-1),
  endRow_p       (-1),
  lastValue_p    (0),
  lastRowPut_p   (0)
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
    case TpInt64:
	delete [] (Int64*)lastValue_p;
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
    default:
	AlwaysAssert (0, AipsError);
    }
    lastValue_p = 0;
}

void ISMColumn::setShapeColumn (const IPosition& shape)
{
    nrelem_p = shape.product();
    shape_p  = shape;
}

uInt ISMColumn::ndim (rownr_t)
{
    return shape_p.nelements();
}
IPosition ISMColumn::shape (rownr_t)
{
    return shape_p;
}


void ISMColumn::addRow (rownr_t, rownr_t)
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
    Block<rownr_t>& rowIndex = bucket->rowIndex (colnr_p);
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


void ISMColumn::getBool (rownr_t rownr, Bool* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Bool*)lastValue_p;
}
void ISMColumn::getuChar (rownr_t rownr, uChar* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(uChar*)lastValue_p;
}
void ISMColumn::getShort (rownr_t rownr, Short* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Short*)lastValue_p;
}
void ISMColumn::getuShort (rownr_t rownr, uShort* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(uShort*)lastValue_p;
}
void ISMColumn::getInt (rownr_t rownr, Int* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Int*)lastValue_p;
}
void ISMColumn::getuInt (rownr_t rownr, uInt* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(uInt*)lastValue_p;
}
void ISMColumn::getInt64 (rownr_t rownr, Int64* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Int64*)lastValue_p;
}
void ISMColumn::getfloat (rownr_t rownr, float* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(float*)lastValue_p;
}
void ISMColumn::getdouble (rownr_t rownr, double* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(double*)lastValue_p;
}
void ISMColumn::getComplex (rownr_t rownr, Complex* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(Complex*)lastValue_p;
}
void ISMColumn::getDComplex (rownr_t rownr, DComplex* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(DComplex*)lastValue_p;
}
void ISMColumn::getString (rownr_t rownr, String* value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, True);
    }
    *value = *(String*)lastValue_p;
}

void ISMColumn::getScalarColumnV (ArrayBase& dataPtr)
{
  switch (dtype()) {
  case TpBool:
    getScaCol (static_cast<Vector<Bool>&>(dataPtr));
    break;
  case TpUChar:
    getScaCol (static_cast<Vector<uChar>&>(dataPtr));
    break;
  case TpShort:
    getScaCol (static_cast<Vector<Short>&>(dataPtr));
    break;
  case TpUShort:
    getScaCol (static_cast<Vector<uShort>&>(dataPtr));
    break;
  case TpInt:
    getScaCol (static_cast<Vector<Int>&>(dataPtr));
    break;
  case TpUInt:
    getScaCol (static_cast<Vector<uInt>&>(dataPtr));
    break;
  case TpInt64:
    getScaCol (static_cast<Vector<Int64>&>(dataPtr));
    break;
  case TpFloat:
    getScaCol (static_cast<Vector<float>&>(dataPtr));
    break;
  case TpDouble:
    getScaCol (static_cast<Vector<double>&>(dataPtr));
    break;
  case TpComplex:
    getScaCol (static_cast<Vector<Complex>&>(dataPtr));
    break;
  case TpDComplex:
    getScaCol (static_cast<Vector<DComplex>&>(dataPtr));
    break;
  case TpString:
    getScaCol (static_cast<Vector<String>&>(dataPtr));
    break;
  default:
    AlwaysAssert (0, AipsError);
  }
}

void ISMColumn::getScalarColumnCellsV (const RefRows& rows, ArrayBase& dataPtr)
{
  switch (dtype()) {
  case TpBool:
    getScaColCells (rows, static_cast<Vector<Bool>&>(dataPtr));
    break;
  case TpUChar:
    getScaColCells (rows, static_cast<Vector<uChar>&>(dataPtr));
    break;
  case TpShort:
    getScaColCells (rows, static_cast<Vector<Short>&>(dataPtr));
    break;
  case TpUShort:
    getScaColCells (rows, static_cast<Vector<uShort>&>(dataPtr));
    break;
  case TpInt:
    getScaColCells (rows, static_cast<Vector<Int>&>(dataPtr));
    break;
  case TpUInt:
    getScaColCells (rows, static_cast<Vector<uInt>&>(dataPtr));
    break;
  case TpInt64:
    getScaColCells (rows, static_cast<Vector<Int64>&>(dataPtr));
    break;
  case TpFloat:
    getScaColCells (rows, static_cast<Vector<float>&>(dataPtr));
    break;
  case TpDouble:
    getScaColCells (rows, static_cast<Vector<double>&>(dataPtr));
    break;
  case TpComplex:
    getScaColCells (rows, static_cast<Vector<Complex>&>(dataPtr));
    break;
  case TpDComplex:
    getScaColCells (rows, static_cast<Vector<DComplex>&>(dataPtr));
    break;
  case TpString:
    getScaColCells (rows, static_cast<Vector<String>&>(dataPtr));
    break;
  default:
    AlwaysAssert (0, AipsError);
  }
}

#define ISMCOLUMN_GET(T) \
void ISMColumn::getScaCol (Vector<T>& dataPtr) \
{ \
    rownr_t nrrow = dataPtr.nelements(); \
    rownr_t rownr = 0; \
    while (rownr < nrrow) { \
        aips_name2(get,T) (rownr, &(dataPtr(rownr))); \
	for (rownr++; Int64(rownr)<=endRow_p; rownr++) { \
	    dataPtr(rownr) = *(T*)lastValue_p; \
	} \
    } \
} \
void ISMColumn::getScaColCells (const RefRows& rownrs, \
                                Vector<T>& values) \
{ \
    Bool delV; \
    T* value = values.getStorage (delV); \
    T* valptr = value; \
    const ColumnCache& cache = columnCache(); \
    if (rownrs.isSliced()) { \
        RefRowsSliceIter iter(rownrs); \
        while (! iter.pastEnd()) { \
            rownr_t rownr = iter.sliceStart(); \
            rownr_t end = iter.sliceEnd(); \
            rownr_t incr = iter.sliceIncr(); \
            while (rownr <= end) { \
                if (rownr < cache.start()  ||  rownr > cache.end()) { \
                    aips_name2(get,T) (rownr, valptr); \
                    DebugAssert (cache.incr() == 0, AipsError); \
                } \
                const T* cacheValue = (const T*)(cache.dataPtr()); \
                rownr_t endrow = min (end, cache.end()); \
                while (rownr <= endrow) { \
	            *valptr++ = *cacheValue; \
                    rownr += incr; \
	        } \
     	    } \
	    iter++; \
        } \
    } else { \
        const Vector<rownr_t>& rowvec = rownrs.rowVector(); \
        rownr_t nr = rowvec.nelements(); \
        if (nr > 0) { \
            Bool delR; \
            const rownr_t* rows = rowvec.getStorage (delR); \
            if (rows[0] < cache.start()  ||  rows[0] > cache.end()) { \
                aips_name2(get,T) (0, &(value[0])); \
            } \
            const T* cacheValue = (const T*)(cache.dataPtr()); \
            rownr_t strow = cache.start(); \
            rownr_t endrow = cache.end(); \
            AlwaysAssert (cache.incr() == 0, AipsError); \
            for (rownr_t i=0; i<nr; i++) { \
	        rownr_t rownr = rows[i]; \
                if (rownr >= strow  &&  rownr <= endrow) { \
	            value[i] = *cacheValue; \
	        } else { \
	            aips_name2(get,T) (rownr, &(value[i])); \
                    cacheValue = (const T*)(cache.dataPtr()); \
                    strow = cache.start(); \
                    endrow = cache.end(); \
	        } \
	    } \
            rowvec.freeStorage (rows, delR); \
        } \
    } \
    values.putStorage (value, delV); \
}
ISMCOLUMN_GET(Bool)
ISMCOLUMN_GET(uChar)
ISMCOLUMN_GET(Short)
ISMCOLUMN_GET(uShort)
ISMCOLUMN_GET(Int)
ISMCOLUMN_GET(uInt)
ISMCOLUMN_GET(Int64)
ISMCOLUMN_GET(float)
ISMCOLUMN_GET(double)
ISMCOLUMN_GET(Complex)
ISMCOLUMN_GET(DComplex)
ISMCOLUMN_GET(String)

void ISMColumn::getValue (rownr_t rownr, void* value, Bool setCache)
{
    // Get the bucket with its row number boundaries.
    rownr_t bucketStartRow;
    uInt bucketNrrow;
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

void ISMColumn::putBool (rownr_t rownr, const Bool* value)
{
    putValue (rownr, value);
}
void ISMColumn::putuChar (rownr_t rownr, const uChar* value)
{
    putValue (rownr, value);
}
void ISMColumn::putShort (rownr_t rownr, const Short* value)
{
    putValue (rownr, value);
}
void ISMColumn::putuShort (rownr_t rownr, const uShort* value)
{
    putValue (rownr, value);
}
void ISMColumn::putInt (rownr_t rownr, const Int* value)
{
    putValue (rownr, value);
}
void ISMColumn::putuInt (rownr_t rownr, const uInt* value)
{
    putValue (rownr, value);
}
void ISMColumn::putInt64 (rownr_t rownr, const Int64* value)
{
    putValue (rownr, value);
}
void ISMColumn::putfloat (rownr_t rownr, const float* value)
{
    putValue (rownr, value);
}
void ISMColumn::putdouble (rownr_t rownr, const double* value)
{
    putValue (rownr, value);
}
void ISMColumn::putComplex (rownr_t rownr, const Complex* value)
{
    putValue (rownr, value);
}
void ISMColumn::putDComplex (rownr_t rownr, const DComplex* value)
{
    putValue (rownr, value);
}
void ISMColumn::putString (rownr_t rownr, const String* value)
{
    putValue (rownr, value);
}

void ISMColumn::putScalarColumnV (const ArrayBase& dataPtr)
{
  switch (dtype()) {
  case TpBool:
    putScaCol (static_cast<const Vector<Bool>&>(dataPtr));
    break;
  case TpUChar:
    putScaCol (static_cast<const Vector<uChar>&>(dataPtr));
    break;
  case TpShort:
    putScaCol (static_cast<const Vector<Short>&>(dataPtr));
    break;
  case TpUShort:
    putScaCol (static_cast<const Vector<uShort>&>(dataPtr));
    break;
  case TpInt:
    putScaCol (static_cast<const Vector<Int>&>(dataPtr));
    break;
  case TpUInt:
    putScaCol (static_cast<const Vector<uInt>&>(dataPtr));
    break;
  case TpInt64:
    putScaCol (static_cast<const Vector<Int64>&>(dataPtr));
    break;
  case TpFloat:
    putScaCol (static_cast<const Vector<float>&>(dataPtr));
    break;
  case TpDouble:
    putScaCol (static_cast<const Vector<double>&>(dataPtr));
    break;
  case TpComplex:
    putScaCol (static_cast<const Vector<Complex>&>(dataPtr));
    break;
  case TpDComplex:
    putScaCol (static_cast<const Vector<DComplex>&>(dataPtr));
    break;
  case TpString:
    putScaCol (static_cast<const Vector<String>&>(dataPtr));
    break;
  default:
    AlwaysAssert (0, AipsError);
  }
}


void ISMColumn::putScaCol (const Vector<Bool>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<uChar>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<Short>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<uShort>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<Int>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<uInt>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<Int64>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<float>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<double>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<Complex>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<DComplex>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}
void ISMColumn::putScaCol (const Vector<String>& dataPtr)
{
    rownr_t nrrow = dataPtr.nelements();
    for (rownr_t i=0; i<nrrow; i++) {
	putValue (i, &(dataPtr(i)));
    }
}

void ISMColumn::getArrayV (rownr_t rownr, ArrayBase& value)
{
    if (isLastValueInvalid (rownr)) {
	getValue (rownr, lastValue_p, False);
    }
    if (dtype() == TpString) {
      value.assignBase (Array<String> (shape_p, (String*)lastValue_p, SHARE));
    } else {
      Bool deleteIt;
      void* vptr = value.getVStorage(deleteIt);
      memcpy (vptr, lastValue_p, nrelem_p * typeSize_p);
      value.putVStorage (vptr, deleteIt);
    }
}

void ISMColumn::putArrayV (rownr_t rownr, const ArrayBase& value)
{
    Bool deleteIt;
    const void* data = value.getVStorage (deleteIt);
    putValue (rownr, data);
    value.freeVStorage (data, deleteIt);
}


void ISMColumn::putValue (rownr_t rownr, const void* value)
{
    // Get the bucket and interval to which the row belongs.
    rownr_t bucketStartRow;
    uInt bucketNrrow;
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
    Block<rownr_t>& rowIndex = bucket->rowIndex (colnr_p);
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

void ISMColumn::putFromRow (rownr_t rownr, const char* data, uInt lenData)
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
void ISMColumn::handleCopy (rownr_t rownr, const char*)
{
    cout << "   handleCopy for row " << rownr
	 << ", column " << colnr_p << endl;
#else
void ISMColumn::handleCopy (rownr_t, const char*)
{
#endif
}

#ifdef AIPS_TRACE
void ISMColumn::handleRemove (rownr_t rownr, const char*)
{
    cout << "   handleRemove for row " << rownr
	 << ", column " << colnr_p << endl;
#else
void ISMColumn::handleRemove (rownr_t, const char*)
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
    case TpInt64:
	{
	    compareFunc_p = ObjCompare<Int64>::compare;
	    lastValue_p   = new Int64 [nrelem_p];
	    Int64 undef = 0;
	    objset ((Int64*)lastValue_p, undef, nrelem_p);
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

void ISMColumn::getFile (rownr_t nrrow)
{
    init();
    lastRowPut_p = nrrow;
}
Bool ISMColumn::flush (rownr_t, Bool)
{
    return False;
}
void ISMColumn::resync (rownr_t nrrow)
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
Conversion::ValueFunction* ISMColumn::getReadRownr (Bool asBigEndian)
{
    if (asBigEndian) {
	return CanonicalConversion::getToLocal (static_cast<rownr_t*>(0));
    }
    return LECanonicalConversion::getToLocal (static_cast<rownr_t*>(0));
}
Conversion::ValueFunction* ISMColumn::getWriteuInt (Bool asBigEndian)
{
    if (asBigEndian) {
	return CanonicalConversion::getFromLocal (static_cast<uInt*>(0));
    }
    return LECanonicalConversion::getFromLocal (static_cast<uInt*>(0));
}
Conversion::ValueFunction* ISMColumn::getWriteRownr (Bool asBigEndian)
{
    if (asBigEndian) {
	return CanonicalConversion::getFromLocal (static_cast<rownr_t*>(0));
    }
    return LECanonicalConversion::getFromLocal (static_cast<rownr_t*>(0));
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
    uInt strleng;
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

