//# MSMColumn.cc: A column in the MemoryStMan
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

#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define EXTBLSZ 32

MSMColumn::MSMColumn (MSMBase* smptr, int dataType, Bool byPtr)
: StManColumn(dataType),
  stmanPtr_p (smptr),
  dtype_p    (dataType),
  byPtr_p    (byPtr),
  nralloc_p  (0),
  nrext_p    (0),
  data_p     (EXTBLSZ,static_cast<void*>(0)),
  ncum_p     (EXTBLSZ,(uInt)0)
{}

MSMColumn::~MSMColumn()
{
  deleteAll();
}


void MSMColumn::doCreate (uInt nrrow)
{
  addRow (nrrow, 0);
  initData (data_p[1], nrrow);
}

void MSMColumn::addRow (uInt nrnew, uInt)
{
  //# Extend the column sizes if needed.
  if (nrnew > nralloc_p) {
    uInt n = nralloc_p + 4096;
    if (n < nrnew) {
      n = nrnew;
    }
    resize (n);
  }
}

void MSMColumn::resize (uInt nr)
{
  //# Extend internal blocks if needed.
  if (nrext_p+1 >= data_p.nelements()) {
    //#	cout << "resize internal blocks " << nrext_p << endl;
    data_p.resize(nrext_p + 1+EXTBLSZ);
    ncum_p.resize(nrext_p + 1+EXTBLSZ);
  }
  //# Allocate another block of the correct data type.
  data_p[nrext_p+1] = allocData (nr-nralloc_p, byPtr_p);
  //#    cout << "allocated new block " << nr-nralloc_p << endl;
  nrext_p++;
  ncum_p[nrext_p] = nr;
  nralloc_p = nr;
  return;
}


uInt MSMColumn::findExt (uInt index, Bool setCache)
{
  //# Use a binary search to get the block containing the index.
  Int st = 0;
  Int ent= nrext_p;
  Int i  = 0;
  while (st<=ent) {
    i = (st+ent)/2;
    if (index < ncum_p[i]) {
      ent = i-1;
    }else{
      if (index > ncum_p[i]) {
	i++;
	st = i;
      }else{
	ent = -1;             // found
	i++;
      }
    }
  }
  if (i > Int(nrext_p)) {
    throw (indexError<uInt>(index, "MSMColumn::findExt - "
			    "rownr out of range"));
  }
  if (setCache) {
    columnCache().set (ncum_p[i-1], ncum_p[i]-1, data_p[i]);
  }
  return i;
}

uInt MSMColumn::nextExt (void*& ext, uInt& extnr, uInt nrmax) const
{
  if (++extnr > nrext_p) {
    return 0;
  }
  ext = data_p[extnr];
  uInt n = ncum_p[extnr];
  if (n > nrmax) {
    n = nrmax;
  }
  if (n < ncum_p[extnr-1]) {
    return 0;
  }
  return n - ncum_p[extnr-1];
}


#define MSMCOLUMN_GETPUT(T,NM) \
void MSMColumn::aips_name2(get,NM) (uInt rownr, T* value) \
{ \
  uInt extnr = findExt(rownr, True); \
  *value = ((T*)(data_p[extnr])) [rownr-ncum_p[extnr-1]]; \
} \
void MSMColumn::aips_name2(put,NM) (uInt rownr, const T* value) \
{ \
  uInt extnr = findExt(rownr, True); \
  ((T*)(data_p[extnr])) [rownr-ncum_p[extnr-1]] = *value; \
} \
uInt MSMColumn::aips_name2(getBlock,NM) (uInt rownr, uInt nrmax, T* value) \
{ \
  uInt nr; \
  uInt extnr = findExt(rownr, True); \
  nrmax = min (nrmax, nralloc_p-rownr); \
  uInt nrm = nrmax; \
  while (nrmax > 0) { \
    nr = min (nrmax, ncum_p[extnr]-rownr); \
    objcopy (value, ((T*)(data_p[extnr])) +rownr-ncum_p[extnr-1], nr); \
    nrmax -= nr; \
    value += nr; \
    rownr  = ncum_p[extnr]; \
    extnr++; \
  } \
  return nrm; \
} \
void MSMColumn::aips_name2(putBlock,NM) (uInt rownr, uInt nrmax, const T* value) \
{ \
  uInt nr; \
  uInt extnr = findExt(rownr, True); \
  nrmax = min (nrmax, nralloc_p-rownr); \
  while (nrmax > 0) { \
    nr = min (nrmax, ncum_p[extnr]-rownr); \
    objcopy (((T*)(data_p[extnr])) +rownr-ncum_p[extnr-1], value, nr); \
    nrmax -= nr; \
    value += nr; \
    rownr  = ncum_p[extnr]; \
    extnr++; \
  } \
} \
void MSMColumn::aips_name2(getScalarColumnCells,NM) \
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
        } \
	uInt inx = rownr - cache.start(); \
        const T* cacheValue = (const T*)(cache.dataPtr()) + inx; \
        uInt endrow = min (end, cache.end()); \
        while (rownr <= endrow) { \
	  *valptr++ = *cacheValue; \
          rownr += incr; \
	  cacheValue += incr; \
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
        findExt(rows[0], True); \
      } \
      const T* cacheValue = (const T*)(cache.dataPtr()); \
      uInt strow = cache.start(); \
      uInt endrow = cache.end(); \
      for (uInt i=0; i<nr; i++) { \
	uInt rownr = rows[i]; \
        if (rownr >= strow  &&  rownr <= endrow) { \
	  value[i] = cacheValue[rownr-strow]; \
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

MSMCOLUMN_GETPUT(Bool,BoolV)
MSMCOLUMN_GETPUT(uChar,uCharV)
MSMCOLUMN_GETPUT(Short,ShortV)
MSMCOLUMN_GETPUT(uShort,uShortV)
MSMCOLUMN_GETPUT(Int,IntV)
MSMCOLUMN_GETPUT(uInt,uIntV)
MSMCOLUMN_GETPUT(float,floatV)
MSMCOLUMN_GETPUT(double,doubleV)
MSMCOLUMN_GETPUT(Complex,ComplexV)
MSMCOLUMN_GETPUT(DComplex,DComplexV)
MSMCOLUMN_GETPUT(String,StringV)


void MSMColumn::remove (uInt index)
{
  //# Find the extension.
  uInt extnr  = findExt(index, False);
  uInt nrval  = ncum_p[extnr] - ncum_p[extnr-1];
  void* datap = data_p[extnr];
  //# If the extension contains only this element, remove the extension.
  if (nrval == 1) {
    deleteData (datap, byPtr_p);
    for (uInt i=extnr; i<nrext_p; i++) {
      data_p[i] = data_p[i+1];
      ncum_p[i] = ncum_p[i+1];
    }
    ncum_p[nrext_p] = 0;
    nrext_p--;
  }else{
    removeData (datap, index-ncum_p[extnr-1], nrval-1);
  }
  nralloc_p--;
//#    cout << "Remove " << nrext_p << " " << nralloc_p << " " << extnr <<" "<<nrval<< endl;
  for (uInt i=extnr; i<=nrext_p; i++) {
    ncum_p[i]--;
  }
  columnCache().invalidate();
//#    for (i=1; i<=nrext_p; i++) {
//#	cout << " " << ncum_p[i] << " ";
//#    }
//#    cout << endl;
}


Bool MSMColumn::ok() const
{
  //# Internal blocks cannot be empty and must be equal in length.
  //# Their lengths must be >= nr of extensions.
  //# Their first elements must be zero.
  if (data_p.nelements() == 0  ||  data_p.nelements() < nrext_p)
    return False;
  if (data_p.nelements() != ncum_p.nelements())
    return False;
  if (data_p[0] != 0  || ncum_p[0] != 0)
    return False;
  //# If no points, there should be no extensions (and vice versa).
  if ((nralloc_p == 0)  !=  (nrext_p == 0))
    return False;
  //# If no extensions, first length must also be zero.
  if (nrext_p == 0  &&  ncum_p[1] != 0)
    return False;
  //# All extension pointers must be filled in.
  //# The ncum_p array must be increasing.
  for (uInt i=1; i<=nrext_p; i++) {
    if (data_p[i] == 0  ||  ncum_p[i] <= ncum_p[i-1])
      return False;
  }
  return True;
}


void MSMColumn::deleteAll()
{
  for (uInt i=1; i<=nrext_p; i++) {
    deleteData (data_p[i], byPtr_p);
  }
  nralloc_p = 0;
  nrext_p   = 0;
  ncum_p[1] = 0;
}

void MSMColumn::deleteData (void* datap, Bool byPtr)
{
  if (byPtr) {
    delete [] (void**)datap;
  }else{
    switch (dtype_p) {
    case TpBool:
      delete [] (Bool*)datap;
      break;
    case TpUChar:
      delete [] (uChar*)datap;
      break;
    case TpShort:
      delete [] (Short*)datap;
      break;
    case TpUShort:
      delete [] (uShort*)datap;
      break;
    case TpInt:
      delete [] (Int*)datap;
      break;
    case TpUInt:
      delete [] (uInt*)datap;
      break;
    case TpFloat:
      delete [] (float*)datap;
      break;
    case TpDouble:
      delete [] (double*)datap;
      break;
    case TpComplex:
      delete [] (Complex*)datap;
      break;
    case TpDComplex:
      delete [] (DComplex*)datap;
      break;
    case TpString:
      delete [] (String*)datap;
      break;
    default:
      throw DataManInvDT();
    }
  }
  datap = 0;
}


void* MSMColumn::allocData (uInt nrval, Bool byPtr)
{
  void* datap = 0;
  if (byPtr) {
    datap = new void*[nrval];
    if (datap != 0) {
      void** dp = (void**)datap;
      for (uInt i=0; i<nrval; i++)  {
	*dp++ = 0;
      }
    }
  }else{
    switch (dtype_p) {
    case TpBool:
      datap = new Bool[nrval];
      break;
    case TpUChar:
      datap = new uChar[nrval];
      break;
    case TpShort:
      datap = new Short[nrval];
      break;
    case TpUShort:
      datap = new uShort[nrval];
      break;
    case TpInt:
      datap = new Int[nrval];
      break;
    case TpUInt:
      datap = new uInt[nrval];
      break;
    case TpFloat:
      datap = new float[nrval];
      break;
    case TpDouble:
      datap = new double[nrval];
      break;
    case TpComplex:
      datap = new Complex[nrval];
      break;
    case TpDComplex:
      datap = new DComplex[nrval];
      break;
    case TpString:
      datap = new String[nrval];
      break;
    default:
      throw DataManInvDT();
    }
  }
  return datap;
}
	

void MSMColumn::removeData (void* dp, uInt inx, uInt nrvalAfter)
{
  if (inx >= nrvalAfter) {
    return;
  }
  if (byPtr_p) {
    objmove (((void**)dp) + inx,  ((void**)dp) + inx+1 ,nrvalAfter-inx);
    return;
  }
  switch (dtype_p) {
  case TpBool:
    objmove (((Bool*)dp) + inx,   ((Bool*)dp) + inx+1,  nrvalAfter-inx);
    break;
  case TpUChar:
    objmove (((uChar*)dp) + inx,  ((uChar*)dp) + inx+1, nrvalAfter-inx);
    break;
  case TpShort:
    objmove (((Short*)dp) + inx,  ((Short*)dp) + inx+1, nrvalAfter-inx);
    break;
  case TpUShort:
    objmove (((uShort*)dp) + inx, ((uShort*)dp) + inx+1,nrvalAfter-inx);
    break;
  case TpInt:
    objmove (((Int*)dp) + inx,    ((Int*)dp) + inx+1,   nrvalAfter-inx);
    break;
  case TpUInt:
    objmove (((uInt*)dp) + inx,   ((uInt*)dp) + inx+1,  nrvalAfter-inx);
    break;
  case TpFloat:
    objmove (((float*)dp) + inx,  ((float*)dp) + inx+1, nrvalAfter-inx);
    break;
  case TpDouble:
    objmove (((double*)dp) + inx, ((double*)dp) + inx+1,nrvalAfter-inx);
    break;
  case TpComplex:
    objmove (((Complex*)dp)+inx,  ((Complex*)dp)+inx+1, nrvalAfter-inx);
    break;
  case TpDComplex:
    objmove (((DComplex*)dp)+inx, ((DComplex*)dp)+inx+1,nrvalAfter-inx);
    break;
  case TpString:
    objmove (((String*)dp) + inx, ((String*)dp) + inx+1,nrvalAfter-inx);
    break;
  default:
    throw DataManInvDT();
  }
}


void MSMColumn::initData (void* datap, uInt nrval)
{
  // Pointers are already initialized by allocData.
  if (!byPtr_p) {
    switch (dtype_p) {
    case TpBool:
      objset ((Bool*)datap, True, nrval);
      break;
    case TpUChar:
      objset ((uChar*)datap, uChar(0), nrval);
      break;
    case TpShort:
      objset ((Short*)datap, Short(0), nrval);
      break;
    case TpUShort:
      objset ((uShort*)datap, uShort(0), nrval);
      break;
    case TpInt:
      objset ((Int*)datap, Int(0), nrval);
      break;
    case TpUInt:
      objset ((uInt*)datap, uInt(0), nrval);
      break;
    case TpFloat:
      objset ((Float*)datap, Float(0), nrval);
      break;
    case TpDouble:
      objset ((Double*)datap, Double(0), nrval);
      break;
    default:
      break;
    }
  }
}


void* MSMColumn::getArrayPtr (uInt rownr)
{
  uInt extnr = findExt(rownr, False);
  return ((void**)(data_p[extnr])) [rownr-ncum_p[extnr-1]];
}
void MSMColumn::putArrayPtr (uInt rownr, void* ptr)
{
  uInt extnr = findExt(rownr, False);
  ((void**)(data_p[extnr])) [rownr-ncum_p[extnr-1]] = ptr;
}

} //# NAMESPACE CASACORE - END

