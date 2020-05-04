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
//# $Id: MSMColumn.cc 20551 2009-03-25 00:11:33Z Malte.Marquarding $

#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/tables/DataMan/MSMBase.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/string.h>                           // for memcpy


namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define EXTBLSZ 32

MSMColumn::MSMColumn (MSMBase* smptr, int dataType, Bool byPtr)
: StManColumnBase(dataType),
  stmanPtr_p (smptr),
  byPtr_p    (byPtr),
  nralloc_p  (0),
  nrext_p    (0),
  data_p     (EXTBLSZ,static_cast<void*>(0)),
  ncum_p     (EXTBLSZ,(rownr_t)0)
{}

MSMColumn::~MSMColumn()
{
  deleteAll();
}


void MSMColumn::doCreate (rownr_t nrrow)
{
  addRow (nrrow, 0);
  initData (data_p[1], nrrow);
}

void MSMColumn::addRow (rownr_t nrnew, rownr_t)
{
  //# Extend the column sizes if needed.
  if (nrnew > nralloc_p) {
    rownr_t n = nralloc_p + 4096;
    if (n < nrnew) {
      n = nrnew;
    }
    resize (n);
  }
}

void MSMColumn::resize (rownr_t nr)
{
  //# Extend internal blocks if needed.
  if (nrext_p+1 >= data_p.nelements()) {
    //#cout << "resize internal blocks " << nrext_p << endl;
    data_p.resize(nrext_p + 1+EXTBLSZ);
    ncum_p.resize(nrext_p + 1+EXTBLSZ);
  }
  //# Allocate another block of the correct data type.
  data_p[nrext_p+1] = allocData (nr-nralloc_p, byPtr_p);
  //#cout << "allocated new block " << nr-nralloc_p << endl;
  nrext_p++;
  ncum_p[nrext_p] = nr;
  nralloc_p = nr;
  return;
}


uInt MSMColumn::findExt (rownr_t index, Bool setCache)
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
    throw (indexError<rownr_t>(index, "MSMColumn::findExt - "
                               "rownr " + String::toString(index) +
                               " in column " + columnName() +
                               " out of range"));
  }
  if (setCache) {
    columnCache().set (ncum_p[i-1], ncum_p[i]-1, data_p[i]);
  }
  return i;
}


void MSMColumn::getScalarColumnV (ArrayBase& vec)
{
  rownr_t nrow = stmanPtr_p->nrow();
  // Get a pointer to the destination data.
  // It assures the data are contiguous.
  Bool deleteIt;
  void* ptr = vec.getVStorage (deleteIt);
  // About all data types can be simply copied.
  // Only String has to be handled specifically.
  if (dtype() == TpString) {
    String* to = static_cast<String*>(ptr);
    for (uInt i=1; i<=nrext_p; ++i) {
      const String* from = static_cast<String*>(data_p[i]);
      rownr_t nr = min(nrow, ncum_p[i]) - ncum_p[i-1];
      for (rownr_t j=0; j<nr; ++j) {
        *to++ = from[j];
      }
    }
  } else {
    char* to = static_cast<char*>(ptr);
    for (uInt i=1; i<=nrext_p; ++i) {
      const char* from = static_cast<char*>(data_p[i]);
      rownr_t nr = min(nrow, ncum_p[i]) - ncum_p[i-1];
      memcpy (to, from, nr * elemSize());
      to += nr * elemSize();
    }
  }
  // This frees the storage if needed.
  vec.putVStorage (ptr, deleteIt);
}

void MSMColumn::putScalarColumnV (const ArrayBase& vec)
{
  rownr_t nrow = stmanPtr_p->nrow();
  // Get a pointer to the destination data.
  // It assures the data are contiguous.
  Bool deleteIt;
  const void* ptr = vec.getVStorage (deleteIt);
  // About all data types can be simply copied.
  // Only String has to be handled specifically.
  if (dtype() == TpString) {
    const String* from = static_cast<const String*>(ptr);
    for (uInt i=1; i<=nrext_p; ++i) {
      String* to = static_cast<String*>(data_p[i]);
      rownr_t nr = min(nrow, ncum_p[i]) - ncum_p[i-1];
      for (rownr_t j=0; j<nr; ++j) {
        to[j] = *from++;
      }
    }
  } else {
    const char* from = static_cast<const char*>(ptr);
    for (uInt i=1; i<=nrext_p; ++i) {
      char* to = static_cast<char*>(data_p[i]);
      rownr_t nr = min(nrow, ncum_p[i]) - ncum_p[i-1];
      memcpy (to, from, nr * elemSize());
      from += nr * elemSize();
    }
  }
  // This frees the storage if needed.
  vec.freeVStorage (ptr, deleteIt);
}

void MSMColumn::getBool (rownr_t rownr, Bool* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const Bool*>(cache.dataPtr())[inx];
}
void MSMColumn::putBool (rownr_t rownr, const Bool* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<Bool*>(static_cast<const Bool*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getuChar (rownr_t rownr, uChar* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const uChar*>(cache.dataPtr())[inx];
}
void MSMColumn::putuChar (rownr_t rownr, const uChar* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<uChar*>(static_cast<const uChar*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getShort (rownr_t rownr, Short* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const Short*>(cache.dataPtr())[inx];
}
void MSMColumn::putShort (rownr_t rownr, const Short* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<Short*>(static_cast<const Short*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getuShort (rownr_t rownr, uShort* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const uShort*>(cache.dataPtr())[inx];
}
void MSMColumn::putuShort (rownr_t rownr, const uShort* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<uShort*>(static_cast<const uShort*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getInt (rownr_t rownr, Int* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const Int*>(cache.dataPtr())[inx];
}
void MSMColumn::putInt (rownr_t rownr, const Int* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<Int*>(static_cast<const Int*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getuInt (rownr_t rownr, uInt* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const uInt*>(cache.dataPtr())[inx];
}
void MSMColumn::putuInt (rownr_t rownr, const uInt* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<uInt*>(static_cast<const uInt*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getInt64 (rownr_t rownr, Int64* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const Int64*>(cache.dataPtr())[inx];
}
void MSMColumn::putInt64 (rownr_t rownr, const Int64* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<Int64*>(static_cast<const Int64*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getfloat (rownr_t rownr, float* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const float*>(cache.dataPtr())[inx];
}
void MSMColumn::putfloat (rownr_t rownr, const float* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<float*>(static_cast<const float*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getdouble (rownr_t rownr, double* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const double*>(cache.dataPtr())[inx];
}
void MSMColumn::putdouble (rownr_t rownr, const double* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<double*>(static_cast<const double*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getComplex (rownr_t rownr, Complex* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const Complex*>(cache.dataPtr())[inx];
}
void MSMColumn::putComplex (rownr_t rownr, const Complex* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<Complex*>(static_cast<const Complex*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getDComplex (rownr_t rownr, DComplex* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const DComplex*>(cache.dataPtr())[inx];
}
void MSMColumn::putDComplex (rownr_t rownr, const DComplex* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<DComplex*>(static_cast<const DComplex*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}

void MSMColumn::getString (rownr_t rownr, String* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  const ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  *value = static_cast<const String*>(cache.dataPtr())[inx];
}
void MSMColumn::putString (rownr_t rownr, const String* value)
{
  // Note that the ColumnCache references the appropriate data array in data_p.
  ColumnCache& cache = columnCache();
  if (rownr < cache.start()  ||  rownr > cache.end()) {
    findExt (rownr, True);
  }
  rownr_t inx = rownr - cache.start();
  const_cast<String*>(static_cast<const String*>(cache.dataPtr()))[inx] = *value;
  stmanPtr_p->setHasPut();
}


void MSMColumn::remove (rownr_t index)
{
  //# Find the extension.
  uInt extnr  = findExt(index, False);
  rownr_t nrval  = ncum_p[extnr] - ncum_p[extnr-1];
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
  //#cout << "Remove " << nrext_p << " " << nralloc_p << " " << extnr <<" "<<nrval<< endl;
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
    delete [] static_cast<void**>(datap);
  } else if (dtype() == TpString) {
    delete [] static_cast<String*>(datap);
  } else {
    delete [] static_cast<char*>(datap);
  }
  datap = 0;
}


void* MSMColumn::allocData (rownr_t nrval, Bool byPtr)
{
  void* datap = 0;
  if (byPtr) {
    datap = new void*[nrval];
    memset (datap, 0, nrval*sizeof(void*));
  } else if (dtype() == TpString) {
    datap = new String[nrval];
  } else {
    datap = new char[nrval * elemSize()];
  }
  return datap;
}
	

void MSMColumn::removeData (void* dp, rownr_t inx, rownr_t nrvalAfter)
{
  if (inx >= nrvalAfter) {
    return;
  }
  if (byPtr_p) {
    objmove (static_cast<void**>(dp) + inx,
             static_cast<void**>(dp) + inx+1, nrvalAfter-inx);
  } else if (dtype() == TpString) {
    objmove (static_cast<String*>(dp) + inx,
             static_cast<String*>(dp) + inx+1, nrvalAfter-inx);
  } else {
    memmove (static_cast<char*>(dp) + inx*elemSize(),
             static_cast<char*>(dp) + (inx+1)*elemSize(),
             (nrvalAfter-inx)*elemSize());
  }
}


void MSMColumn::initData (void* datap, rownr_t nrval)
{
  // Pointers are already initialized by allocData.
  if (!byPtr_p) {
    if (dtype() == TpBool) {
      objset (static_cast<Bool*>(datap), True, nrval);
    } else if (dtype() != TpString) {
      memset (datap, 0, nrval*elemSize());
    }
  }
}


void* MSMColumn::getArrayPtr (rownr_t rownr)
{
  uInt extnr = findExt(rownr, False);
  return (static_cast<void**>(data_p[extnr])) [rownr-ncum_p[extnr-1]];
}

void MSMColumn::putArrayPtr (rownr_t rownr, void* ptr)
{
  uInt extnr = findExt(rownr, False);
  (static_cast<void**>(data_p[extnr])) [rownr-ncum_p[extnr-1]] = ptr;
  stmanPtr_p->setHasPut();
}

void MSMColumn::putFile (rownr_t, AipsIO&)
{}
void MSMColumn::getFile (rownr_t, AipsIO&)
{}
void MSMColumn::reopenRW()
{}


} //# NAMESPACE CASACORE - END

