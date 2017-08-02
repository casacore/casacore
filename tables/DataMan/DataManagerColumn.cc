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
//#
//# $Id$


//# Includes
#include <casacore/tables/DataMan/DataManagerColumn.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
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

void DataManagerColumn::setShape (uInt, const IPosition&)
{
    throw DataManInvOper("setShape only allowed for non-FixedShape arrays"
                         " in column " + columnName());
}

void DataManagerColumn::setShapeTiled (uInt rownr, const IPosition& shape,
				       const IPosition&)
{
    setShape (rownr, shape);
}

// By default the shape is defined (for scalars).
Bool DataManagerColumn::isShapeDefined (uInt)
{
    return True;
}

// The default implementation of ndim is to use the shape.
uInt DataManagerColumn::ndim (uInt rownr)
{
    return shape(rownr).nelements();
}

// The shape of the array in the given row.
IPosition DataManagerColumn::shape (uInt)
{
    return IPosition(0);
}

// The tile shape of the array in the given row.
IPosition DataManagerColumn::tileShape (uInt)
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

void DataManagerColumn::getBoolV (uInt, Bool*)
  { throwGet(); }
void DataManagerColumn::getuCharV (uInt, uChar*)
  { throwGet(); }
void DataManagerColumn::getShortV (uInt, Short*)
  { throwGet(); }
void DataManagerColumn::getuShortV (uInt, uShort*)
  { throwGet(); }
void DataManagerColumn::getIntV (uInt, Int*)
  { throwGet(); }
void DataManagerColumn::getuIntV (uInt, uInt*)
  { throwGet(); }
void DataManagerColumn::getInt64V (uInt, Int64*)
  { throwGet(); }
void DataManagerColumn::getfloatV (uInt, float*)
  { throwGet(); }
void DataManagerColumn::getdoubleV (uInt, double*)
  { throwGet(); }
void DataManagerColumn::getComplexV (uInt, Complex*)
  { throwGet(); }
void DataManagerColumn::getDComplexV (uInt, DComplex*)
  { throwGet(); }
void DataManagerColumn::getStringV (uInt, String*)
  { throwGet(); }

void DataManagerColumn::putBoolV (uInt, const Bool*)
  { throwPut(); }
void DataManagerColumn::putuCharV (uInt, const uChar*)
  { throwPut(); }
void DataManagerColumn::putShortV (uInt, const Short*)
  { throwPut(); }
void DataManagerColumn::putuShortV (uInt, const uShort*)
  { throwPut(); }
void DataManagerColumn::putIntV (uInt, const Int*)
  { throwPut(); }
void DataManagerColumn::putuIntV (uInt, const uInt*)
  { throwPut(); }
void DataManagerColumn::putInt64V (uInt, const Int64*)
  { throwPut(); }
void DataManagerColumn::putfloatV (uInt, const float*)
  { throwPut(); }
void DataManagerColumn::putdoubleV (uInt, const double*)
  { throwPut(); }
void DataManagerColumn::putComplexV (uInt, const Complex*)
  { throwPut(); }
void DataManagerColumn::putDComplexV (uInt, const DComplex*)
  { throwPut(); }
void DataManagerColumn::putStringV (uInt, const String*)
  { throwPut(); }

void DataManagerColumn::getOtherV (uInt, void*)
{
  throw (DataManInvOper ("DataManagerColumn::getOtherV not allowed"
                         " in column " + columnName()));
}
void DataManagerColumn::putOtherV (uInt, const void*)
{
  throw (DataManInvOper ("DataManagerColumn::putOtherV not allowed"
                         " in column " + columnName()));
}

// Define a macro to get or put a scalar column.
// It gets the value for row i which might fill the ColumnCache.
// If the cache gets filled, use it to get next values in a faster way.
#define DATAMANAGERCOLUMN_GETCOL(T,NM) \
{ \
  Vector<T>& vec = static_cast<Vector<T>&>(arr); \
  uInt nr = vec.nelements(); \
  uInt rownr = 0; \
  while (rownr < nr) { \
    aips_name2(get,NM) (rownr, &vec[rownr]); \
    rownr++; \
    if (rownr <= colCache_p.end()  &&  rownr > colCache_p.start()) { \
      uInt last = std::min(nr-1, colCache_p.end()); \
      uInt inx = (rownr - colCache_p.start()) * colCache_p.incr();  \
      const T* cptr = static_cast<const T*>(colCache_p.dataPtr()) + inx; \
      for (uInt j=rownr; j<=last; ++j) { \
        vec[rownr++] = *cptr; \
        cptr += colCache_p.incr(); \
      } \
    } \
  } \
}
#define DATAMANAGERCOLUMN_PUTCOL(T,NM) \
{ \
  const Vector<T>& vec = static_cast<const Vector<T>&>(arr); \
  uInt nr = vec.nelements(); \
  for (uInt rownr=0; rownr<nr; ++rownr) { \
    aips_name2(put,NM) (rownr, &vec[rownr]); \
  } \
}
#define DATAMANAGERCOLUMN_GETCELLS(T,NM) \
{ \
  Vector<T>& vec = static_cast<Vector<T>&>(arr); \
  if (rownrs.isSliced()) { \
    RefRowsSliceIter iter(rownrs); \
    uInt i=0; \
    while (! iter.pastEnd()) { \
      uInt rownr = iter.sliceStart(); \
      uInt end   = iter.sliceEnd(); \
      uInt incr  = iter.sliceIncr(); \
      while (rownr <= end) { \
        if (rownr < colCache_p.start()  ||  rownr > colCache_p.end()) { \
          aips_name2(get,NM) (rownr, &(vec[i])); \
          i++; \
          rownr += incr; \
        } else { \
          uInt inx = (rownr - colCache_p.start()) * colCache_p.incr(); \
          const T* cptr = static_cast<const T*>(colCache_p.dataPtr()) + inx; \
          uInt endrow = std::min (end, colCache_p.end()); \
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
    const Vector<uInt>& rowvec = rownrs.rowVector(); \
    uInt nr = rowvec.nelements(); \
    if (nr > 0) { \
      Bool delR; \
      const uInt* rows = rowvec.getStorage (delR); \
      const T* cptr = static_cast<const T*>(colCache_p.dataPtr()); \
      uInt strow  = colCache_p.start(); \
      uInt endrow = colCache_p.end(); \
      for (uInt i=0; i<nr; ++i) { \
	uInt rownr = rows[i]; \
        if (rownr >= strow  &&  rownr <= endrow) { \
	  vec[i] = cptr[(rownr-strow)*colCache_p.incr()];       \
	} else { \
	  aips_name2(get,NM) (rownr, &(vec[i])); \
          cptr = static_cast<const T*>(colCache_p.dataPtr()); \
          strow  = colCache_p.start(); \
          endrow = colCache_p.end(); \
        } \
      } \
      rowvec.freeStorage (rows, delR); \
    } \
  } \
}
#define DATAMANAGERCOLUMN_PUTCELLS(T,NM) \
{ \
  const Vector<T>& vec = static_cast<const Vector<T>&>(arr); \
  RefRowsSliceIter iter(rownrs);                 \
  uInt i=0;                                      \
  while (! iter.pastEnd()) {                     \
    uInt rownr = iter.sliceStart();              \
    uInt end   = iter.sliceEnd();                \
    uInt incr  = iter.sliceIncr();               \
    while (rownr <= end) {                       \
      aips_name2(put,NM) (rownr, &(vec[i]));     \
      i++;                                       \
      rownr += incr;                             \
    }                                            \
  }                                              \
  iter++;                                        \
}

void DataManagerColumn::getScalarColumnV (ArrayBase&)
{
  throw DataManError("getScalarColumnV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putScalarColumnV (const ArrayBase&)
{
  throw DataManError("putScalarColumnV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getScalarColumnCellsV (const RefRows&,
                                               ArrayBase&)
{
  throw DataManError("getScalarColumnCellsV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putScalarColumnCellsV (const RefRows&,
                                               const ArrayBase&)
{
  throw DataManError("getScalarColumnCellsV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getArrayV (uInt, ArrayBase&)
{
  throw DataManError("getArrayV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putArrayV (uInt, const ArrayBase&)
{
  throw DataManError("putArrayV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getArrayColumnV (ArrayBase&)
{
  throw DataManError("getArrayColumnV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putArrayColumnV (const ArrayBase&)
{
  throw DataManError("putArrayColumnV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getArrayColumnCellsV (const RefRows&,
                                              ArrayBase&)
{
  throw DataManError("getArrayColumnCellsV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putArrayColumnCellsV (const RefRows&,
                                              const ArrayBase&)
{
  throw DataManError("putArrayColumnCellsV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getSliceV (uInt, const Slicer&, ArrayBase&)
{
  throw DataManError("getSliceV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putSliceV (uInt, const Slicer&,
                                   const ArrayBase&)
{
  throw DataManError("putSliceV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getColumnSliceV (const Slicer&, ArrayBase&)
{
  throw DataManError("getColumnSliceV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putColumnSliceV (const Slicer&, const ArrayBase&)
{
  throw DataManError("putColumnSliceV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::getColumnSliceCellsV (const RefRows&,
                                              const Slicer&, ArrayBase&)
{
  throw DataManError("getColumnSliceCellsV not implemented"
                     " for column " + columnName());
}
void DataManagerColumn::putColumnSliceCellsV (const RefRows&,
                                              const Slicer&, const ArrayBase&)
{
  throw DataManError("putColumnSliceCellsV not implemented"
                     " for column " + columnName());
}

void DataManagerColumn::dmGetScalarColumnV (ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_GETCOL(Bool,BoolV)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_GETCOL(uChar,uCharV)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_GETCOL(Short,ShortV)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_GETCOL(uShort,uShortV)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_GETCOL(Int,IntV)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_GETCOL(uInt,uIntV)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_GETCOL(float,floatV)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_GETCOL(double,doubleV)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_GETCOL(Complex,ComplexV)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_GETCOL(DComplex,DComplexV)
    break;
  case TpString:
    DATAMANAGERCOLUMN_GETCOL(String,StringV)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::getScalarColumnV not allowed"
                          " for column " + columnName()));
  }
}

void DataManagerColumn::dmPutScalarColumnV (const ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_PUTCOL(Bool,BoolV)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_PUTCOL(uChar,uCharV)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_PUTCOL(Short,ShortV)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_PUTCOL(uShort,uShortV)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_PUTCOL(Int,IntV)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_PUTCOL(uInt,uIntV)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_PUTCOL(float,floatV)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_PUTCOL(double,doubleV)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_PUTCOL(Complex,ComplexV)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_PUTCOL(DComplex,DComplexV)
    break;
  case TpString:
    DATAMANAGERCOLUMN_PUTCOL(String,StringV)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::putScalarColumnV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::dmGetScalarColumnCellsV (const RefRows& rownrs,
                                                 ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_GETCELLS(Bool,BoolV)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_GETCELLS(uChar,uCharV)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_GETCELLS(Short,ShortV)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_GETCELLS(uShort,uShortV)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_GETCELLS(Int,IntV)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_GETCELLS(uInt,uIntV)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_GETCELLS(float,floatV)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_GETCELLS(double,doubleV)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_GETCELLS(Complex,ComplexV)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_GETCELLS(DComplex,DComplexV)
    break;
  case TpString:
    DATAMANAGERCOLUMN_GETCELLS(String,StringV)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::getScalarColumnCellsV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::dmPutScalarColumnCellsV (const RefRows& rownrs,
                                                 const ArrayBase& arr)
{
  switch (dataType()) {
  case TpBool:
    DATAMANAGERCOLUMN_PUTCELLS(Bool,BoolV)
    break;
  case TpUChar:
    DATAMANAGERCOLUMN_PUTCELLS(uChar,uCharV)
    break;
  case TpShort:
    DATAMANAGERCOLUMN_PUTCELLS(Short,ShortV)
    break;
  case TpUShort:
    DATAMANAGERCOLUMN_PUTCELLS(uShort,uShortV)
    break;
  case TpInt:
    DATAMANAGERCOLUMN_PUTCELLS(Int,IntV)
    break;
  case TpUInt:
    DATAMANAGERCOLUMN_PUTCELLS(uInt,uIntV)
    break;
  case TpFloat:
    DATAMANAGERCOLUMN_PUTCELLS(float,floatV)
    break;
  case TpDouble:
    DATAMANAGERCOLUMN_PUTCELLS(double,doubleV)
    break;
  case TpComplex:
    DATAMANAGERCOLUMN_PUTCELLS(Complex,ComplexV)
    break;
  case TpDComplex:
    DATAMANAGERCOLUMN_PUTCELLS(DComplex,DComplexV)
    break;
  case TpString:
    DATAMANAGERCOLUMN_PUTCELLS(String,StringV)
    break;
  default:
    throw (DataManInvOper("DataManagerColumn::putScalarColumnCellsV not allowed"
                          " in column " + columnName()));
  }
}

void DataManagerColumn::dmGetArrayColumnV (ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    getArrayV (row, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::dmPutArrayColumnV (const ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    putArrayV (row, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::dmGetArrayColumnCellsV (const RefRows& rows, ArrayBase& arr)
{
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      getArrayV (row, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::dmPutArrayColumnCellsV (const RefRows& rows,
                                                const ArrayBase& arr)
{
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      putArrayV (row, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::getSliceArr (uInt row, const Slicer& section,
                                     CountedPtr<ArrayBase>& fullArr,
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
void DataManagerColumn::putSliceArr (uInt row, const Slicer& section,
                                     CountedPtr<ArrayBase>& fullArr,
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
void DataManagerColumn::dmGetSliceV (uInt row, const Slicer& section,
                                     ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  getSliceArr (row, section, fullArr, arr);
}
void DataManagerColumn::dmPutSliceV (uInt row, const Slicer& section,
                                     const ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  putSliceArr (row, section, fullArr, arr);
}
void DataManagerColumn::dmGetColumnSliceV (const Slicer& section, ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    getSliceArr (row, section, fullArr, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::dmPutColumnSliceV (const Slicer& section,
                                           const ArrayBase& arr)
{
  const IPosition& shp = arr.shape();
  uInt nr = shp[shp.size() - 1];
  DebugAssert (nr == nrow(), AipsError);
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (shp.size()-1);
  for (uInt row=0; row<nr; ++row) {
    putSliceArr (row, section, fullArr, iter->getArray());
    iter->next();
  }
}
void DataManagerColumn::dmGetColumnSliceCellsV (const RefRows& rows,
                                                const Slicer& section,
                                                ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
         row+=rowsIter.sliceIncr()) {
      DebugAssert (! iter->pastEnd(), AipsError);
      getSliceArr (row, section, fullArr, iter->getArray());
      iter->next();
    }
    rowsIter.next();
  }
  DebugAssert (iter->pastEnd(), AipsError);
}
void DataManagerColumn::dmPutColumnSliceCellsV (const RefRows& rows,
                                                const Slicer& section,
                                                const ArrayBase& arr)
{
  CountedPtr<ArrayBase> fullArr = arr.makeArray();
  CountedPtr<ArrayPositionIterator> iter = arr.makeIterator (arr.ndim()-1);
  RefRowsSliceIter rowsIter(rows);
  while (! rowsIter.pastEnd()) {
    for (uInt row=rowsIter.sliceStart(); row<=rowsIter.sliceEnd();
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

