//# StIndArrAIO.cc: Read/write a table column of arrays array using AipsIO
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/Tables/StIndArrAIO.h>
#include <aips/Tables/StArrayFile.h>
#include <aips/Tables/StIndArray.h>
#include <aips/Utilities/DataType.h>
#include <aips/IO/AipsIO.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/String.h>
#include <aips/Tables/DataManError.h>
#include <stdio.h>                            //# for sprintf


//# Define a macro which gets the pointer for the given row
//# and casts it to the block.
#define STMANINDGETBLOCK(rownr) \
((StIndArray*)(getArrayPtr(rownr)))



StManColumnIndArrayAipsIO::StManColumnIndArrayAipsIO (StManAipsIO* smptr,
						      int dataType)
: StManColumnAipsIO (smptr, dataType, True),
  seqnr_p       (smptr->uniqueNr()),
  shapeIsFixed_p(False),
  iosfile_p     (0)
{}

//# Delete all objects created.
StManColumnIndArrayAipsIO::~StManColumnIndArrayAipsIO()
{
    uInt nr = stmanPtr_p->nrow();
    for (uInt i=0; i<nr; i++) {
	deleteArray (i);
    }
    delete iosfile_p;
}


//# Create the array file (for a new column).
//# Compose the file name from the mother file name extended with
//# the unique column sequence nr.
//# Create the given nr of rows in it.
void StManColumnIndArrayAipsIO::doCreate (uInt nrrow)
{
    //# Create the file.
    openFile (ByteIO::New);
    //# Add the nr of rows.
    addRow (nrrow, 0);
}

void StManColumnIndArrayAipsIO::openFile (ByteIO::OpenOption opt)
{
    //# Open/create the file holding the arrays in the column.
    char strc[8];
    sprintf (strc, "i%i", seqnr_p);
    iosfile_p = new StManArrayFile (stmanPtr_p->fileName() + strc, opt);
    if (iosfile_p == 0) {
	throw (AllocError ("StManColumnIndArrayAipsIO::openFile", 1));
    }
}

void StManColumnIndArrayAipsIO::reopenRW()
{
    iosfile_p->reopenRW();
}


void StManColumnIndArrayAipsIO::setShapeColumn (const IPosition& shape)
{
    fixedShape_p   = shape;
    shapeIsFixed_p = True;
}


void StManColumnIndArrayAipsIO::addRow (uInt nrnew, uInt nrold)
{
    //# Extend data blocks if needed.
    StManColumnAipsIO::addRow (nrnew, nrold);
    //# Allocate the data arrays if fixed shape.
    if (shapeIsFixed_p) {
	for (; nrold<nrnew; nrold++) {
	    setShape (nrold, fixedShape_p);
	}
    }
}


void StManColumnIndArrayAipsIO::setShape (uInt rownr, const IPosition& shape)
{
    StIndArray* ptr = STMANINDGETBLOCK(rownr);
    if (ptr == 0) {
	ptr = new StIndArray (0);
    }
    //# Put the new shape (if changed).
    //# When changed, put the file offset.
    if (ptr->setShape (*iosfile_p, dtype_p, shape)) {
	putArrayPtr (rownr, ptr);
    }
}

//# Get the shape for the array (if any) in the given row.
//# Read shape if not read yet.
StIndArray* StManColumnIndArrayAipsIO::getShape (uInt rownr)
{
    StIndArray* ptr = STMANINDGETBLOCK(rownr);
    if (ptr == 0) {
	throw (DataManInvOper ("StMan: no array in this row"));
    }
    ptr->getShape (*iosfile_p);
    return ptr;
}

Bool StManColumnIndArrayAipsIO::isShapeDefined (uInt rownr)
    { return (STMANINDGETBLOCK(rownr) == 0  ?  False : True); }

uInt StManColumnIndArrayAipsIO::ndim (uInt rownr)
    { return getShape(rownr)->shape().nelements(); }

IPosition StManColumnIndArrayAipsIO::shape (uInt rownr)
    { return getShape(rownr)->shape(); }

Bool StManColumnIndArrayAipsIO::canChangeShape() const
    { return (shapeIsFixed_p  ?  False : True); }


Bool StManColumnIndArrayAipsIO::canAccessSlice (Bool& reask) const
{
    reask = False;
    return True;
}


void StManColumnIndArrayAipsIO::getArrayfloatV (uInt rownr, Array<float>* arr)
{
    getShape(rownr)->getArrayfloatV (*iosfile_p, arr);
}

void StManColumnIndArrayAipsIO::putArrayfloatV (uInt rownr,
						const Array<float>* arr)
{
    getShape(rownr)->putArrayfloatV (*iosfile_p, arr);
    stmanPtr_p->setHasPut();
}

void StManColumnIndArrayAipsIO::getSlicefloatV (uInt rownr, const Slicer& ns,
						Array<float>* arr)
{
    getShape(rownr)->getSlicefloatV (*iosfile_p, ns, arr);
}

void StManColumnIndArrayAipsIO::putSlicefloatV (uInt rownr, const Slicer& ns,
						const Array<float>* arr)
{
    getShape(rownr)->putSlicefloatV (*iosfile_p, ns, arr);
    stmanPtr_p->setHasPut();
}
    

#define STMANCOLUMNINDARRAYAIPSIO_GETPUT(T,NM) \
void StManColumnIndArrayAipsIO::aips_name2(getArray,NM) (uInt rownr, \
							 Array<T>* arr) \
{ \
    getShape(rownr)->aips_name2(getArray,NM) (*iosfile_p, arr); \
} \
void StManColumnIndArrayAipsIO::aips_name2(putArray,NM) (uInt rownr, \
						         const Array<T>* arr) \
{ \
    getShape(rownr)->aips_name2(putArray,NM) (*iosfile_p, arr); \
    stmanPtr_p->setHasPut(); \
} \
void StManColumnIndArrayAipsIO::aips_name2(getSlice,NM) \
                             (uInt rownr, const Slicer& ns, Array<T>* arr) \
{ \
    getShape(rownr)->aips_name2(getSlice,NM) (*iosfile_p, ns, arr); \
} \
void StManColumnIndArrayAipsIO::aips_name2(putSlice,NM) \
                        (uInt rownr, const Slicer& ns, const Array<T>* arr) \
{ \
    getShape(rownr)->aips_name2(putSlice,NM) (*iosfile_p, ns, arr); \
    stmanPtr_p->setHasPut(); \
}

STMANCOLUMNINDARRAYAIPSIO_GETPUT(Bool,BoolV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(uChar,uCharV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(Short,ShortV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(uShort,uShortV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(Int,IntV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(uInt,uIntV)
//#//STMANCOLUMNINDARRAYAIPSIO_GETPUT(float,floatV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(double,doubleV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(Complex,ComplexV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(DComplex,DComplexV)
STMANCOLUMNINDARRAYAIPSIO_GETPUT(String,StringV)


void StManColumnIndArrayAipsIO::remove (uInt rownr)
{
    deleteArray (rownr);
    StManColumnAipsIO::remove (rownr);
}


Bool StManColumnIndArrayAipsIO::ok() const
{
    return StManColumnAipsIO::ok();
}


void StManColumnIndArrayAipsIO::deleteArray (uInt rownr)
{
    delete STMANINDGETBLOCK(rownr);
}


//# Write all data into AipsIO.
void StManColumnIndArrayAipsIO::putFile (uInt nrval, AipsIO& ios)
{
    ios.putstart ("StManColumnIndArrayAipsIO", 1);     // class version 1
    ios << dtype_p;                    // for backward compatibility
    ios << seqnr_p;
    StManColumnAipsIO::putFile (nrval, ios);
    ios.putend();
}

void StManColumnIndArrayAipsIO::putData (void* dp, uInt nrval, AipsIO& ios)
{
    StIndArray** dpa = (StIndArray**)dp;
    while (nrval--) {
	if (*dpa == 0) {
	    ios << (uInt)0;
	}else{
	    ios << (*dpa)->fileOffset();
	}
	dpa++;
    }
}


//# Read all data from AipsIO.
void StManColumnIndArrayAipsIO::getFile (uInt nrval, AipsIO& ios)
{
    int dtype;
    ios.getstart ("StManColumnIndArrayAipsIO");
    ios >> dtype;                          // for backward compatibility
    ios >> seqnr_p;
    openFile (stmanPtr_p->fileOption());   // open the array file
    StManColumnAipsIO::getFile (nrval, ios);
    ios.getend();
}

void StManColumnIndArrayAipsIO::getData (void* dp, uInt inx, uInt nrval,
					 AipsIO& ios, uInt)
{
    uInt offset;
    StIndArray** dpa = (StIndArray**)dp;
    dpa += inx;
    while (nrval--) {
	ios >> offset;
	if (offset == 0) {
	    *dpa = 0;
	}else{
	    *dpa = new StIndArray (offset);
	    if (*dpa == 0) {
		throw (AllocError ("StManColumnIndArrayAipsIO::getData",1));
	    }
	}
	dpa++;
    }
}
