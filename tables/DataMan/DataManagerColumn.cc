//# DataManagerColumn.cc: Abstract base class for a data manager column
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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


//# Includes
#include <casacore/tables/DataMan/DataManagerColumn.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/DataMan/DataManError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

DataManagerColumn::~DataManagerColumn()
{}

void DataManagerColumn::setMaxLength (uInt)
{}

void DataManagerColumn::setShapeColumn (const IPosition&)
{
    throw DataManInvOper ("setShapeColumn only allowed for FixedShape arrays"
                          " in column " + columnName());
}

void DataManagerColumn::setShape (rownr_t, const IPosition&)
{
    throw DataManInvOper("setShape only allowed for non-FixedShape arrays"
                         " in column " + columnName());
}

void DataManagerColumn::setShapeTiled (rownr_t rownr, const IPosition& shape,
				       const IPosition&)
{
    setShape (rownr, shape);
}

// By default the shape is defined (for scalars).
Bool DataManagerColumn::isShapeDefined (rownr_t)
{
    return True;
}

// The default implementation of ndim is to use the shape.
uInt DataManagerColumn::ndim (rownr_t rownr)
{
    return shape(rownr).nelements();
}

// The shape of the array in the given row.
IPosition DataManagerColumn::shape (rownr_t)
{
    return IPosition(0);
}

// The tile shape of the array in the given row.
IPosition DataManagerColumn::tileShape (rownr_t)
{
    return IPosition(0);
}

Bool DataManagerColumn::canChangeShape() const
{
    return False;
}


String DataManagerColumn::dataTypeId() const
    { return String(); }

Bool DataManagerColumn::isWritable() const
    { return True; }

void DataManagerColumn::throwGet() const
    { throw (DataManInvOper ("DataManagerColumn::get not allowed in column "
                             + columnName())); }
void DataManagerColumn::throwPut() const
    { throw (DataManInvOper ("DataManagerColumn::put not allowed in column "
                             + columnName())); }

void DataManagerColumn::getBool (rownr_t, Bool*)
  { throwGet(); }
void DataManagerColumn::getuChar (rownr_t, uChar*)
  { throwGet(); }
void DataManagerColumn::getShort (rownr_t, Short*)
  { throwGet(); }
void DataManagerColumn::getuShort (rownr_t, uShort*)
  { throwGet(); }
void DataManagerColumn::getInt (rownr_t, Int*)
  { throwGet(); }
void DataManagerColumn::getuInt (rownr_t, uInt*)
  { throwGet(); }
void DataManagerColumn::getInt64 (rownr_t, Int64*)
  { throwGet(); }
void DataManagerColumn::getfloat (rownr_t, float*)
  { throwGet(); }
void DataManagerColumn::getdouble (rownr_t, double*)
  { throwGet(); }
void DataManagerColumn::getComplex (rownr_t, Complex*)
  { throwGet(); }
void DataManagerColumn::getDComplex (rownr_t, DComplex*)
  { throwGet(); }
void DataManagerColumn::getString (rownr_t, String*)
  { throwGet(); }

void DataManagerColumn::putBool (rownr_t, const Bool*)
  { throwPut(); }
void DataManagerColumn::putuChar (rownr_t, const uChar*)
  { throwPut(); }
void DataManagerColumn::putShort (rownr_t, const Short*)
  { throwPut(); }
void DataManagerColumn::putuShort (rownr_t, const uShort*)
  { throwPut(); }
void DataManagerColumn::putInt (rownr_t, const Int*)
  { throwPut(); }
void DataManagerColumn::putuInt (rownr_t, const uInt*)
  { throwPut(); }
void DataManagerColumn::putInt64 (rownr_t, const Int64*)
  { throwPut(); }
void DataManagerColumn::putfloat (rownr_t, const float*)
  { throwPut(); }
void DataManagerColumn::putdouble (rownr_t, const double*)
  { throwPut(); }
void DataManagerColumn::putComplex (rownr_t, const Complex*)
  { throwPut(); }
void DataManagerColumn::putDComplex (rownr_t, const DComplex*)
  { throwPut(); }
void DataManagerColumn::putString (rownr_t, const String*)
  { throwPut(); }

void DataManagerColumn::getOther (rownr_t, void*)
{
  throw (DataManInvOper ("DataManagerColumn::getOther not allowed"
                         " in column " + columnName()));
}
void DataManagerColumn::putOther (rownr_t, const void*)
{
  throw (DataManInvOper ("DataManagerColumn::putOther not allowed"
                         " in column " + columnName()));
}

// Define a macro to get or put a scalar column.
// It gets the value for row i which might fill the ColumnCache.
// If the cache gets filled, use it to get next values in a faster way.
#define DATAMANAGERCOLUMN_GETCOL(T) \
{ \
  Vector<T>& vec = static_cast<Vector<T>&>(arr); \
  rownr_t nr = vec.nelements(); \
  rownr_t rownr = 0; \
  while (rownr < nr) { \
    aips_name2(get,T) (rownr, &vec[rownr]); \
    rownr++; \
    if (rownr <= colCache_p.end()  &&  rownr > colCache_p.start()) { \
      rownr_t last = std::min(nr-1, colCache_p.end()); \
      rownr_t inx = (rownr - colCache_p.start()) * colCache_p.incr();  \
      const T* cptr = static_cast<const T*>(colCache_p.dataPtr()) + inx; \
      for (rownr_t j=rownr; j<=last; ++j) { \
        vec[rownr++] = *cptr; \
        cptr += colCache_p.incr(); \
      } \
    } \
  } \
}
#define DATAMANAGERCOLUMN_PUTCOL(T) \
{ \
  const Vector<T>& vec = static_cast<const Vector<T>&>(arr); \
  rownr_t nr = vec.nelements(); \
  for (rownr_t rownr=0; rownr<nr; ++rownr) { \
    aips_name2(put,T) (rownr, &vec[rownr]); \
  } \
}
#define DATAMANAGERCOLUMN_GETCELLS(T) \
{ \
  Vector<T>& vec = static_cast<Vector<T>&>(arr); \
  if (rownrs.isSliced()) { \
    RefRowsSliceIter iter(rownrs); \
    rownr_t i=0; \
    while (! iter.pastEnd()) { \
      rownr_t rownr = iter.sliceStart(); \
      rownr_t end   = iter.sliceEnd(); \
      rownr_t incr  = iter.sliceIncr(); \
      while (rownr <= end) { \
        if (rownr < colCache_p.start()  ||  rownr > colCache_p.end()) { \
          aips_name2(get,T) (rownr, &(vec[i])); \
          i++; \
          rownr += incr; \
        } else { \
          rownr_t inx = (rownr - colCache_p.start()) * colCache_p.incr(); \
          const T* cptr = static_cast<const T*>(colCache_p.dataPtr()) + inx; \
          rownr_t endrow = std::min (end, colCache_p.end()); \
          while (rownr <= endrow) { \
	    vec[i++] = *cptr; \
            rownr += incr; \
	    cptr += incr * colCache_p.incr(); \
          } \
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
      const T* cptr = static_cast<const T*>(colCache_p.dataPtr()); \
      rownr_t strow  = colCache_p.start(); \
      rownr_t endrow = colCache_p.end(); \
      for (rownr_t i=0; i<nr; ++i) { \
	rownr_t rownr = rows[i]; \
        if (rownr >= strow  &&  rownr <= endrow) { \
	  vec[i] = cptr[(rownr-strow)*colCache_p.incr()];       \
	} else { \
	  aips_name2(get,T) (rownr, &(vec[i])); \
          cptr = static_cast<const T*>(colCache_p.dataPtr()); \
          strow  = colCache_p.start(); \
          endrow = colCache_p.end(); \
        } \
      } \
      rowvec.freeStorage (rows, delR); \
    } \
  } \
}
#define DATAMANAGERCOLUMN_PUTCELLS(T) \
{ \
  const Vector<T>& vec = static_cast<const Vector<T>&>(arr); \
  RefRowsSliceIter iter(rownrs);                 \
  rownr_t i=0;                                      \
  while (! iter.pastEnd()) {                     \
    rownr_t rownr = iter.sliceStart();              \
    rownr_t end   = iter.sliceEnd();                \
    rownr_t incr  = iter.sliceIncr();               \
    while (rownr <= end) {                       \
      aips_name2(put,T) (rownr, &(vec[i]));     \
      i++;                                       \
      rownr += incr;                             \
    }                                            \
    iter++;                                      \
  }                                              \
}

void DataManagerColumn::getScalarColumnV (ArrayBase& arr)
{
  getScalarColumnBase (arr);
}
void DataManagerColumn::putScalarColumnV (const ArrayBase& arr)
{
  putScalarColumnBase (arr);
}
void DataManagerColumn::getScalarColumnCellsV (const RefRows& rows,
                                               ArrayBase& arr)
{
  getScalarColumnCellsBase (rows, arr);
}
void DataManagerColumn::putScalarColumnCellsV (const RefRows& rows,
                                               const ArrayBase& arr)
{
  putScalarColumnCellsBase (rows, arr);
}
void DataManagerColumn::getArrayV (rownr_t, ArrayBase&)
{
  throw DataManError("getArrayV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putArrayV (rownr_t, const ArrayBase&)
{
  throw DataManError("putArrayV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getArrayColumnV (ArrayBase& arr)
{
  getArrayColumnBase (arr);
}
void DataManagerColumn::putArrayColumnV (const ArrayBase& arr)
{
  putArrayColumnBase (arr);
}
void DataManagerColumn::getArrayColumnCellsV (const RefRows& rows,
                                              ArrayBase& arr)
{
  getArrayColumnCellsBase (rows, arr);
}
void DataManagerColumn::putArrayColumnCellsV (const RefRows& rows,
                                              const ArrayBase& arr)
{
  putArrayColumnCellsBase (rows, arr);
}
void DataManagerColumn::getSliceV (rownr_t rownr, const Slicer& slicer, ArrayBase& arr)
{
  getSliceBase (rownr, slicer, arr);
}
void DataManagerColumn::putSliceV (rownr_t rownr, const Slicer& slicer,
                                   const ArrayBase& arr)
{
  putSliceBase (rownr, slicer, arr);
}
void DataManagerColumn::getColumnSliceV (const Slicer& slicer, ArrayBase& arr)
{
  getColumnSliceBase (slicer, arr);
}
void DataManagerColumn::putColumnSliceV (const Slicer& slicer, const ArrayBase& arr)
{
  putColumnSliceBase (slicer, arr);
}
void DataManagerColumn::getColumnSliceCellsV (const RefRows& rows,
                                              const Slicer& slicer, ArrayBase& arr)
{
  getColumnSliceCellsBase (rows, slicer, arr);
}
void DataManagerColumn::putColumnSliceCellsV (const RefRows& rows,
                                              const Slicer& slicer, const ArrayBase& arr)
{
  putColumnSliceCellsBase (rows, slicer, arr);
}

void DataManagerColumn::getScalarColumnBase (ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_GETCOL(Bool)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_GETCOL(uChar)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_GETCOL(Short)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_GETCOL(uShort)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_GETCOL(Int)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_GETCOL(uInt)
    break;
  case TpInt64:
    DATAMANAGERCOLUMN_GETCOL(Int64)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_GETCOL(float)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_GETCOL(double)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_GETCOL(Complex)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_GETCOL(DComplex)
    break;
  case TpString:
    DATAMANAGERCOLUMN_GETCOL(String)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::getScalarColumnV not allowed"
                          " for column " + columnName()));
  }
}

void DataManagerColumn::putScalarColumnBase (const ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_PUTCOL(Bool)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_PUTCOL(uChar)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_PUTCOL(Short)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_PUTCOL(uShort)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_PUTCOL(Int)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_PUTCOL(uInt)
    break;
  case TpInt64:
    DATAMANAGERCOLUMN_PUTCOL(Int64)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_PUTCOL(float)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_PUTCOL(double)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_PUTCOL(Complex)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_PUTCOL(DComplex)
    break;
  case TpString:
    DATAMANAGERCOLUMN_PUTCOL(String)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::putScalarColumnV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::getScalarColumnCellsBase (const RefRows& rownrs,
                                                  ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_GETCELLS(Bool)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_GETCELLS(uChar)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_GETCELLS(Short)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_GETCELLS(uShort)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_GETCELLS(Int)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_GETCELLS(uInt)
    break;
  case TpInt64:
    DATAMANAGERCOLUMN_GETCELLS(Int64)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_GETCELLS(float)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_GETCELLS(double)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_GETCELLS(Complex)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_GETCELLS(DComplex)
    break;
  case TpString:
    DATAMANAGERCOLUMN_GETCELLS(String)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::getScalarColumnCellsV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::putScalarColumnCellsBase (const RefRows& rownrs,
                                                  const ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_PUTCELLS(Bool)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_PUTCELLS(uChar)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_PUTCELLS(Short)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_PUTCELLS(uShort)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_PUTCELLS(Int)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_PUTCELLS(uInt)
    break; 
  case TpInt64:
    DATAMANAGERCOLUMN_PUTCELLS(Int64)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_PUTCELLS(float)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_PUTCELLS(double)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_PUTCELLS(Complex)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_PUTCELLS(DComplex)
    break;
  case TpString:
    DATAMANAGERCOLUMN_PUTCELLS(String)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::putScalarColumnCellsV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::getArrayColumnBase (ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  rownr_t nr = shp[shp.size() - 1];
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (rownr_t row=0; row<nr; ++row) {
    getArrayV (row, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::putArrayColumnBase (const ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  rownr_t nr = shp[shp.size() - 1];
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (rownr_t row=0; row<nr; ++row) {
    putArrayV (row, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::getArrayColumnCellsBase (const RefRows& rows, ArrayBase& arr)
{
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (rownr_t row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      getArrayV (row, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::putArrayColumnCellsBase (const RefRows& rows,
                                                 const ArrayBase& arr)
{
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (rownr_t row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      putArrayV (row, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::getSliceArr (rownr_t row, const Slicer& section,
                                     std::shared_ptr<ArrayBase>& fullArr,
                                     ArrayBase& arr)
{
  IPosition shp = shape(row);
  if (shp.isEqual (arr.shape())) {
    getArrayV (row, arr);
  } else {
    if (! shp.isEqual (fullArr->shape())) {
      fullArr->resize (shp);
    }
    getArrayV (row, *fullArr);
    arr.assignBase (*(fullArr->getSection (section)));
  }
}
void DataManagerColumn::putSliceArr (rownr_t row, const Slicer& section,
                                     std::shared_ptr<ArrayBase>& fullArr,
                                     const ArrayBase& arr)
{
  IPosition shp = shape(row);
  if (shp.isEqual (arr.shape())) {
    putArrayV (row, arr);
  } else {
    if (! shp.isEqual (fullArr->shape())) {
      fullArr->resize (shp);
    }
    getArrayV (row, *fullArr);
    (fullArr->getSection(section))->assignBase (arr);
    putArrayV (row, *fullArr);
  }
}
void DataManagerColumn::getSliceBase (rownr_t row, const Slicer& section,
                                      ArrayBase& arr)
{
  std::shared_ptr<ArrayBase> fullArr(arr.makeArray());
  getSliceArr (row, section, fullArr, arr);
}
void DataManagerColumn::putSliceBase (rownr_t row, const Slicer& section,
                                      const ArrayBase& arr)
{
  std::shared_ptr<ArrayBase> fullArr(arr.makeArray());
  putSliceArr (row, section, fullArr, arr);
}
void DataManagerColumn::getColumnSliceBase (const Slicer& section, ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  rownr_t nr = shp[shp.size() - 1];
  std::shared_ptr<ArrayBase> fullArr(arr.makeArray());
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (rownr_t row=0; row<nr; ++row) {
    getSliceArr (row, section, fullArr, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::putColumnSliceBase (const Slicer& section,
                                            const ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  rownr_t nr = shp[shp.size() - 1];
  std::shared_ptr<ArrayBase> fullArr(arr.makeArray());
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (rownr_t row=0; row<nr; ++row) {
    putSliceArr (row, section, fullArr, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::getColumnSliceCellsBase (const RefRows& rows,
                                                 const Slicer& section,
                                                 ArrayBase& arr)
{
  std::shared_ptr<ArrayBase> fullArr(arr.makeArray());
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (rownr_t row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      getSliceArr (row, section, fullArr, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::putColumnSliceCellsBase (const RefRows& rows,
                                                 const Slicer& section,
                                                 const ArrayBase& arr)
{
  std::shared_ptr<ArrayBase> fullArr(arr.makeArray());
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (rownr_t row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      putSliceArr (row, section, fullArr, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}


} //# NAMESPACE CASACORE - END

