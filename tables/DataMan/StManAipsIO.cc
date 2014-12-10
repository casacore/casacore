//# StManAipsIO.cc: Storage manager for tables using AipsIO
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

#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StArrAipsIO.h>
#include <casacore/tables/DataMan/StIndArrAIO.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/DOos.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define EXTBLSZ 32

StManColumnAipsIO::StManColumnAipsIO (StManAipsIO* smptr,
				      int dataType, Bool byPtr)
: StManColumn(dataType),
  stmanPtr_p (smptr),
  dtype_p    (dataType),
  byPtr_p    (byPtr),
  nralloc_p  (0),
  nrext_p    (0),
  data_p     (EXTBLSZ,static_cast<void*>(0)),
  ncum_p     (EXTBLSZ,(uInt)0)
{}

StManColumnAipsIO::~StManColumnAipsIO()
    { deleteAll(); }


void StManColumnAipsIO::doCreate (uInt nrrow)
    { addRow (nrrow, 0); }

void StManColumnAipsIO::reopenRW()
{}

void StManColumnAipsIO::addRow (uInt nrnew, uInt)
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

void StManColumnAipsIO::resize (uInt nr)
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


uInt StManColumnAipsIO::findExt (uInt index, Bool setCache)
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
	throw (indexError<uInt>(index, "StManColumnAipsIO::findExt - "
				"rownr out of range"));
    }
    if (setCache) {
	columnCache().set (ncum_p[i-1], ncum_p[i]-1, data_p[i]);
    }
    return i;
}

uInt StManColumnAipsIO::nextExt (void*& ext, uInt& extnr, uInt nrmax) const
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


#define STMANCOLUMNAIPSIO_GETPUT(T,NM) \
void StManColumnAipsIO::aips_name2(get,NM) (uInt rownr, T* value) \
{ \
    uInt extnr = findExt(rownr, True); \
    *value = ((T*)(data_p[extnr])) [rownr-ncum_p[extnr-1]]; \
} \
void StManColumnAipsIO::aips_name2(put,NM) (uInt rownr, const T* value) \
{ \
    uInt extnr = findExt(rownr, True); \
    ((T*)(data_p[extnr])) [rownr-ncum_p[extnr-1]] = *value; \
    stmanPtr_p->setHasPut(); \
} \
uInt StManColumnAipsIO::aips_name2(getBlock,NM) (uInt rownr, uInt nrmax, T* value) \
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
void StManColumnAipsIO::aips_name2(putBlock,NM) (uInt rownr, uInt nrmax, const T* value) \
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
    stmanPtr_p->setHasPut(); \
} \
void StManColumnAipsIO::aips_name2(getScalarColumnCells,NM) \
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

STMANCOLUMNAIPSIO_GETPUT(Bool,BoolV)
STMANCOLUMNAIPSIO_GETPUT(uChar,uCharV)
STMANCOLUMNAIPSIO_GETPUT(Short,ShortV)
STMANCOLUMNAIPSIO_GETPUT(uShort,uShortV)
STMANCOLUMNAIPSIO_GETPUT(Int,IntV)
STMANCOLUMNAIPSIO_GETPUT(uInt,uIntV)
STMANCOLUMNAIPSIO_GETPUT(float,floatV)
STMANCOLUMNAIPSIO_GETPUT(double,doubleV)
STMANCOLUMNAIPSIO_GETPUT(Complex,ComplexV)
STMANCOLUMNAIPSIO_GETPUT(DComplex,DComplexV)
STMANCOLUMNAIPSIO_GETPUT(String,StringV)


void StManColumnAipsIO::remove (uInt index)
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


Bool StManColumnAipsIO::ok() const
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


void StManColumnAipsIO::deleteAll()
{
    for (uInt i=1; i<=nrext_p; i++) {
	deleteData (data_p[i], byPtr_p);
    }
    nralloc_p = 0;
    nrext_p   = 0;
    ncum_p[1] = 0;
}

void StManColumnAipsIO::deleteData (void* datap, Bool byPtr)
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


void* StManColumnAipsIO::allocData (uInt nrval, Bool byPtr)
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
	

void StManColumnAipsIO::removeData (void* dp, uInt inx, uInt nrvalAfter)
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


//# Write all data into AipsIO.
void StManColumnAipsIO::putFile (uInt nrval, AipsIO& ios)
{
    ios.putstart ("StManColumnAipsIO", 2);     // class version 2
    ios << nrval;
    uInt nr;
    for (uInt i=1; i<=nrext_p; i++) {
	nr = ncum_p[i] - ncum_p[i-1];
	if (nr > nrval) {
	    nr = nrval;
	}
	if (nr > 0) {
	    ios << nr;
	    putData (data_p[i], nr, ios);
	    nrval -= nr;
	}
    }
    ios.putend();
}
	
void StManColumnAipsIO::putData (void* dp, uInt nrval, AipsIO& ios)
{
    switch (dtype_p) {
    case TpBool:
	ios.put (nrval, (Bool*)dp);
	break;
    case TpUChar:
	ios.put (nrval, (uChar*)dp);
	break;
    case TpShort:
	ios.put (nrval, (Short*)dp);
	break;
    case TpUShort:
	ios.put (nrval, (uShort*)dp);
	break;
    case TpInt:
	ios.put (nrval, (Int*)dp);
	break;
    case TpUInt:
	ios.put (nrval, (uInt*)dp);
	break;
    case TpFloat:
	ios.put (nrval, (float*)dp);
	break;
    case TpDouble:
	ios.put (nrval, (double*)dp);
	break;
    case TpComplex:
	ios.put (nrval, (Complex*)dp);
	break;
    case TpDComplex:
	ios.put (nrval, (DComplex*)dp);
	break;
    case TpString:
	ios.put (nrval, (String*)dp);
	break;
    }
}


//# Read all data from AipsIO.
void StManColumnAipsIO::getFile (uInt nrval, AipsIO& ios)
{
    uInt version = ios.getstart ("StManColumnAipsIO");
    uInt nr;
    //# Get and check nr of values.
    ios >> nr;
    if (nr != nrval) {
	throw (DataManInternalError
	                ("StManColumnAipsIO::getFile: mismatch in #values"));
    }
    deleteAll();
    if (nrval > 0) {
	resize (nrval);
	void* datap = data_p[1];
	uInt nrd=0;
	while (nrd < nrval) {
	    ios >> nr;
	    if (nr == 0) {
	        nr = nrval - nrd;
	    }
	    if (nr+nrd > nrval) {
		throw (DataManInternalError ("StManColumnAipsIO::getFile"));
	    }
	    getData (datap, nrd, nr, ios, version);
	    nrd += nr;
	}
    }
    ios.getend();
    columnCache().invalidate();
}


void StManColumnAipsIO::getData (void* datap, uInt inx, uInt nrval,
				 AipsIO& ios, uInt)
{
    uInt nr;
    ios >> nr;
    switch (dtype_p) {
    case TpBool:
	ios.get (nrval, (Bool*)datap + inx);
	break;
    case TpUChar:
	ios.get (nrval, (uChar*)datap + inx);
	break;
    case TpShort:
	ios.get (nrval, (Short*)datap + inx);
	break;
    case TpUShort:
	ios.get (nrval, (uShort*)datap + inx);
	break;
    case TpInt:
	ios.get (nrval, (Int*)datap + inx);
	break;
    case TpUInt:
	ios.get (nrval, (uInt*)datap + inx);
	break;
    case TpFloat:
	ios.get (nrval, (float*)datap + inx);
	break;
    case TpDouble:
	ios.get (nrval, (double*)datap + inx);
	break;
    case TpComplex:
	ios.get (nrval, (Complex*)datap + inx);
	break;
    case TpDComplex:
	ios.get (nrval, (DComplex*)datap + inx);
	break;
    case TpString:
	ios.get (nrval, (String*)datap + inx);
	break;
    }
}


void* StManColumnAipsIO::getArrayPtr (uInt rownr)
{
    uInt extnr = findExt(rownr, False);
    return ((void**)(data_p[extnr])) [rownr-ncum_p[extnr-1]];
}
void StManColumnAipsIO::putArrayPtr (uInt rownr, void* ptr)
{
    uInt extnr = findExt(rownr, False);
    ((void**)(data_p[extnr])) [rownr-ncum_p[extnr-1]] = ptr;
    stmanPtr_p->setHasPut();
}




StManAipsIO::StManAipsIO ()
: DataManager (),
  uniqnr_p    (0),
  nrrow_p     (0),
  colSet_p    (0),
  hasPut_p    (False),
  iosfile_p   (0)
{}

StManAipsIO::StManAipsIO (const String& storageManagerName)
: DataManager (),
  stmanName_p (storageManagerName),
  uniqnr_p    (0),
  nrrow_p     (0),
  colSet_p    (0),
  hasPut_p    (False),
  iosfile_p   (0)
{}

StManAipsIO::StManAipsIO (const String& storageManagerName, const Record&)
: DataManager (),
  stmanName_p (storageManagerName),
  uniqnr_p    (0),
  nrrow_p     (0),
  colSet_p    (0),
  hasPut_p    (False),
  iosfile_p   (0)
{}

StManAipsIO::~StManAipsIO()
{
    for (uInt i=0; i<ncolumn(); i++) {
	delete colSet_p[i];
    }
    delete iosfile_p;
}

DataManager* StManAipsIO::clone() const
{
    StManAipsIO* smp = new StManAipsIO (stmanName_p);
    return smp;
}

DataManager* StManAipsIO::makeObject (const String& storageManagerName,
				      const Record& spec)
{
    StManAipsIO* smp = new StManAipsIO (storageManagerName, spec);
    return smp;
}

String StManAipsIO::dataManagerType() const
    { return "StManAipsIO"; }

String StManAipsIO::dataManagerName() const
    { return stmanName_p; }

//# Does the storage manager allow to add rows? (yes)
Bool StManAipsIO::canAddRow() const
    { return True; }

//# Does the storage manager allow to delete rows? (yes)
Bool StManAipsIO::canRemoveRow() const
    { return True; }

//# Does the storage manager allow to add columns? (yes)
Bool StManAipsIO::canAddColumn() const
    { return True; }

//# Does the storage manager allow to delete columns? (yes)
Bool StManAipsIO::canRemoveColumn() const
    { return True; }


DataManagerColumn* StManAipsIO::makeScalarColumn (const String& columnName,
						  int dataType, const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    StManColumnAipsIO* colp = new StManColumnAipsIO (this, dataType, False);
    colSet_p[ncolumn()] = colp;
    return colp;
}
DataManagerColumn* StManAipsIO::makeDirArrColumn (const String& columnName,
						  int dataType, const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    StManColumnAipsIO* colp = new StManColumnArrayAipsIO (this, dataType);
    colSet_p[ncolumn()] = colp;
    return colp;
}
DataManagerColumn* StManAipsIO::makeIndArrColumn (const String& columnName,
						  int dataType, const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    StManColumnAipsIO* colp = new StManColumnIndArrayAipsIO (this, dataType);
    colSet_p[ncolumn()] = colp;
    return colp;
}


// Note that the column has already been added by makeXXColumn.
// This function is merely for initializing the added column.
void StManAipsIO::addColumn (DataManagerColumn* colp)
{
    for (uInt i=0; i<ncolumn(); i++) {
	if (colp == colSet_p[i]) {
	    colSet_p[i]->doCreate (nrrow_p);
	    setHasPut();
	    return;
	}
    }
    throw (DataManInternalError ("StManAipsIO::addColumn"));
}

void StManAipsIO::removeColumn (DataManagerColumn* colp)
{
    for (uInt i=0; i<ncolumn(); i++) {
	if (colSet_p[i] == colp) {
	    delete colSet_p[i];
	    decrementNcolumn();
	    for (uInt j=i; j<ncolumn(); j++) {
		colSet_p[j] = colSet_p[j+1];
	    }
	    setHasPut();
	    return;
	}
    }
    throw (DataManInternalError ("StManAipsIO::removeColumn: no such column"));
}

void StManAipsIO::addRow (uInt nr)
{
    //# Add the number of rows to each column.
    for (uInt i=0; i<ncolumn(); i++) {
	colSet_p[i]->addRow (nrrow_p+nr, nrrow_p);
    }
    nrrow_p += nr;
    setHasPut();
}


void StManAipsIO::removeRow (uInt rownr)
{
    for (uInt i=0; i<ncolumn(); i++) {
	colSet_p[i]->remove (rownr);
    }
    nrrow_p--;
    setHasPut();
}


Bool StManAipsIO::flush (AipsIO&, Bool)
{
    //# Do not write if nothing has been put.
    if (! hasPut_p) {
	return False;
    }
    uInt i;
    AipsIO ios(fileName(), ByteIO::New);
    ios.putstart ("StManAipsIO", 2);           // version 2
    //# Write the number of rows and columns and the column types.
    //# This is only done to check it when reading back.
    ios << stmanName_p;                        // this is added in version 2
    ios << sequenceNr();
    ios << uniqnr_p;
    ios << nrrow_p;
    ios << ncolumn();
    for (i=0; i<ncolumn(); i++) {
	ios << colSet_p[i]->dataType();
    }
    for (i=0; i<ncolumn(); i++) {
	colSet_p[i]->putFile (nrrow_p, ios);
    }
    ios.putend();
    hasPut_p = False;
    return True;
}

void StManAipsIO::create (uInt nrrow)
{
    nrrow_p = nrrow;
    //# Let the column create something if needed.
    for (uInt i=0; i<ncolumn(); i++) {
	colSet_p[i]->doCreate (nrrow);
    }
    setHasPut();
}

void StManAipsIO::open (uInt tabNrrow, AipsIO&)
{
    resync (tabNrrow);
}
void StManAipsIO::resync (uInt nrrow)
{
    if (iosfile_p != 0) {
        iosfile_p->resync();
    }
    AipsIO ios(fileName());
    uInt version = ios.getstart ("StManAipsIO");
    //# Get and check the number of rows and columns and the column types.
    uInt i, nrc, snr;
    int  dt;
    if (version > 1) {
	ios >> stmanName_p;
    }
    ios >> snr;
    ios >> uniqnr_p;
    ios >> nrrow_p;
    ios >> nrc;
    if (snr != sequenceNr()  ||  nrc != ncolumn()) {
	throw (DataManInternalError
	                 ("StManAipsIO::open: mismatch in seqnr,#col"));
    }
    if (nrrow != nrrow_p) {
#if defined(TABLEREPAIR)
        cerr << "StManAipsIO::open: mismatch in #row (expected " << nrrow
	     << ", found " << nrrow_p << ")" << endl;
	cerr << "Remainder will be added or discarded" << endl;
	setHasPut();
#else
	throw (DataManInternalError
	                 ("StManAipsIO::open: mismatch in #row; expected " +
			  String::toString(nrrow) + ", found " +
			  String::toString(nrrow_p)));
#endif
    }
    for (i=0; i<ncolumn(); i++) {
	ios >> dt;
	if (dt != colSet_p[i]->dataType()) {
	    throw (DataManInternalError
		         ("StManAipsIO::open: mismatch in data type"));
	}
    }
    //# Now read in all the columns.
    for (i=0; i<ncolumn(); i++) {
	colSet_p[i]->getFile (nrrow_p, ios);
	//# The following can only be executed in case of TABLEREPAIR.
	//# Add rows if storage manager has fewer rows than table.
	//# Remove rows if storage manager has more rows than table.
	if (nrrow > nrrow_p) {
	    colSet_p[i]->addRow (nrrow, nrrow_p);
	} else if (nrrow < nrrow_p) {
	    for (uInt r=nrrow; r<nrrow_p; r++) {
	        colSet_p[i]->remove (nrrow);
	    }
	}
    }
    nrrow_p = nrrow;
    ios.getend();
}


StManArrayFile* StManAipsIO::openArrayFile (ByteIO::OpenOption opt)
{
    if (iosfile_p == 0) {
	iosfile_p = new StManArrayFile (fileName() + 'i', opt);
    }
    return iosfile_p;
}

void StManAipsIO::reopenRW()
{
    for (uInt i=0; i<ncolumn(); i++) {
	colSet_p[i]->reopenRW();
    }
}

void StManAipsIO::deleteManager()
{
    delete iosfile_p;
    iosfile_p = 0;
    DOos::remove (fileName() + 'i', False, False);
    DOos::remove (fileName(), False, False);
}

} //# NAMESPACE CASACORE - END

