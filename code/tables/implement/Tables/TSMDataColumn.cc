//# TSMDataColumn.cc: Tiled Hypercube Storage Manager for data columns
//# Copyright (C) 1995,1996,1997,1998,1999
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
#include <aips/Tables/TSMDataColumn.h>
#include <aips/Tables/TiledStMan.h>
#include <aips/Tables/TSMCube.h>
#include <aips/Tables/DataManError.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/ValType.h>
#include <aips/Utilities/String.h>
#include <string.h>


TSMDataColumn::TSMDataColumn (const TSMColumn& column)
: TSMColumn (column)
{
    DataType dt = DataType(dataType());
    localPixelSize_p = ValType::getTypeSize (dt);
    convPixelSize_p  = 1;
    if (dt == TpBool) {
	readFunc_p  = Conversion::bitToBool;
	writeFunc_p = Conversion::boolToBit;
	tilePixelSize_p = 0;
    }else{
	if (stmanPtr_p->asCanonical()) {
	    ValType::getCanonicalFunc (dt, readFunc_p, writeFunc_p,
				       convPixelSize_p);
	    tilePixelSize_p = ValType::getCanonicalSize (dt);
	}else{
	    readFunc_p = writeFunc_p = Conversion::valueCopy;
	    tilePixelSize_p = localPixelSize_p;
	    convPixelSize_p = localPixelSize_p;
	}
    }
}

TSMDataColumn::~TSMDataColumn()
{}

uInt TSMDataColumn::dataLength (uInt nrPixels) const
{
    // For Bools a byte can hold 8 pixels.
    if (tilePixelSize_p == 0) {
	return (nrPixels + 7) / 8;
    }
    return tilePixelSize_p * nrPixels;
}


Bool TSMDataColumn::canAccessScalarColumn (Bool& reask) const
{
    reask = True;
    return ToBool (stmanPtr_p->nhypercubes() == 1);
}
Bool TSMDataColumn::canAccessArrayColumn (Bool& reask) const
{
    reask = True;
    return ToBool (stmanPtr_p->nhypercubes() == 1);
}
Bool TSMDataColumn::canAccessSlice (Bool& reask) const
{
    reask = False;
    return True;
}
Bool TSMDataColumn::canAccessColumnSlice (Bool& reask) const
{
    reask = True;
    return ToBool (stmanPtr_p->nhypercubes() == 1);
}


Bool TSMDataColumn::canChangeShape() const
{
    // This storage manager can handle changing array shapes
    // for non-FixedShape columns.
    if (shapeColumn().nelements() != 0) {
	return False;
    }
    return stmanPtr_p->canChangeShape();
}

void TSMDataColumn::setShape (uInt rownr, const IPosition& shape)
{
    setShapeTiled (rownr, shape, stmanPtr_p->defaultTileShape());
}

void TSMDataColumn::setShapeTiled (uInt rownr, const IPosition& shape,
				   const IPosition& tileShape)
{
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    IPosition cubeShape = hypercube->cubeShape();
    if (cubeShape.nelements() == 0) {
	stmanPtr_p->setShape (rownr, hypercube, shape, tileShape);
    }else{
	if (! shape.isEqual(cubeShape)) {
	    if (! canChangeShape()) {
		throw (TSMError
		       ("Shape of data cells in same hypercube differs"));
	    }
	    //# Set the new shape and take care that on the next access
	    //# the cache size is recalculated.
	    stmanPtr_p->setShape (rownr, hypercube, shape, tileShape);
	    hypercube->setLastColAccess (TSMCube::NoAccess);
	}
    }
}

Bool TSMDataColumn::isShapeDefined (uInt rownr)
{
    //# The shape is defined when the shape is fixed or when
    //# a hypercube has been defined for this row.
    if (shapeColumn().nelements() != 0) {
	return True;                             // FixedShape
    }
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    return ToBool (hypercube->cubeShape().nelements() != 0);
}

IPosition TSMDataColumn::shape (uInt rownr)
{
    //# Return the shape when it is fixed.
    if (shapeColumn().nelements() != 0) {
	return shapeColumn();
    }
    //# When no shape is defined, throw exception.
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    IPosition cubeShape = hypercube->cubeShape();
    if (cubeShape.nelements() == 0) {
	throw (DataManInvOper ("StMan: no array in this row"));
    }
    IPosition shape (stmanPtr_p->nrCoordVector());
    for (uInt i=0; i<shape.nelements(); i++) {
	shape(i) = cubeShape(i);
    }
    return shape;
}


void TSMDataColumn::accessCell (uInt rownr, const void* dataPtr, 
				Bool writeFlag)
{
    // Get the hypercube the row is in.
    // It also gives the position of the row in the hypercube.
    IPosition end;
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, end);
    IPosition start (end);
    for (uInt i=0; i<stmanPtr_p->nrCoordVector(); i++) {
	start(i) = 0;
	end(i)--;
    }
    // Size the cache if the user has not done it and if the
    // last access was not to a cell.
    if (hypercube->getLastColAccess() != TSMCube::CellAccess) {
	if (! stmanPtr_p->userSetCache (rownr)) {
	    hypercube->setCacheSize (1 + end - start, IPosition(),
				     IPosition(), IPosition(), False, False);
	    hypercube->setLastColAccess (TSMCube::CellAccess);
	}
    }
    hypercube->accessSection (start, end, (char*)dataPtr, colnr_p,
			      localPixelSize_p, writeFlag);
}

void TSMDataColumn::accessCellSlice (uInt rownr, const Slicer& ns,
				     const void* dataPtr, Bool writeFlag)
{
    IPosition end;
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, end);
    IPosition endcp (end);
    IPosition start (end);
    IPosition stride (end.nelements(), 1);
    IPosition blc, trc, inc;
    IPosition slice = ns.inferShapeFromSource (shape(rownr), blc, trc, inc);
    // Set the correct start and end of the slice in the hypercube.
    // A slice of the data cell is accessed, thus copy the Slicer
    // info for the vector coordinates.
    for (uInt i=0; i<stmanPtr_p->nrCoordVector(); i++) {
	start(i)  = blc(i);
	end(i)    = trc(i);
	stride(i) = inc(i);
    }
    // Size the cache if the user has not done it
    // and if the access type or slice shape differs.
    if (hypercube->getLastColAccess() != TSMCube::SliceAccess
    ||  ! slice.isEqual (hypercube->getLastColSlice())) {
	if (! stmanPtr_p->userSetCache (rownr)) {
	    // The main access path is assumed to be along the full slice
	    // dimensions.
	    uInt naxis = 0;
	    IPosition axisPath (end.nelements());
	    for (uInt i=0; i<stmanPtr_p->nrCoordVector(); i++) {
		if (blc(i) == 0  &&  trc(i) == endcp(i)) {
		    axisPath(naxis++) = i;
		}
	    }
	    axisPath.resize (naxis);
	    hypercube->setCacheSize (1 + end - start, IPosition(),
				     IPosition(), axisPath, False, False);
	    hypercube->setLastColAccess (TSMCube::SliceAccess);
	    hypercube->setLastColSlice (slice);
	}
    }
    hypercube->accessStrided (start, end, stride,
			      (char*)dataPtr, colnr_p,
			      localPixelSize_p, writeFlag);
}

void TSMDataColumn::accessColumn (const void* dataPtr, Bool writeFlag)
{
    // Get the single hypercube and the shape of the hypercube.
    TSMCube* hypercube = stmanPtr_p->singleHypercube();
    IPosition end (hypercube->cubeShape());
    end -= 1;
    IPosition start (end.nelements(), 0);
    // Size the cache if the user has not done it.
    if (! stmanPtr_p->userSetCache (0)) {
	hypercube->setCacheSize (end + 1, IPosition(),
				 IPosition(), IPosition(), False, False);
	hypercube->setLastColAccess (TSMCube::ColumnAccess);
    }
    hypercube->accessSection (start, end, (char*)dataPtr, colnr_p,
			      localPixelSize_p, writeFlag);
}

void TSMDataColumn::accessColumnSlice (const Slicer& ns,
				       const void* dataPtr, Bool writeFlag)
{
    // Get the single hypercube and the shape of the hypercube.
    TSMCube* hypercube = stmanPtr_p->singleHypercube();
    IPosition end (hypercube->cubeShape());
    end -= 1;
    IPosition endcp (end);
    IPosition start (end.nelements(), 0);
    IPosition stride (end.nelements(), 1);
    IPosition blc, trc, inc;
    IPosition slice = ns.inferShapeFromSource (shape(0), blc, trc, inc);
    // Set the correct start and end of the slice in the hypercube.
    // The entire column is accessed, thus all scalar coordinates.
    // A slice of each data cell is accessed, thus copy the Slicer
    // info for the vector coordinates.
    for (uInt i=0; i<stmanPtr_p->nrCoordVector(); i++) {
	start(i)  = blc(i);
	end(i)    = trc(i);
	stride(i) = inc(i);
    }
    // Size the cache if the user has not done it
    // and if the access type or slice shape differs.
    if (hypercube->getLastColAccess() != TSMCube::ColumnSliceAccess
    ||  ! slice.isEqual (hypercube->getLastColSlice())) {
	if (! stmanPtr_p->userSetCache (0)) {
	    // The main access path is assumed to be along the full slice
	    // dimensions.
	    uInt naxis = 0;
	    IPosition axisPath (end.nelements());
	    uInt i;
	    for (i=0; i<stmanPtr_p->nrCoordVector(); i++) {
		if (blc(i) == 0  &&  trc(i) == endcp(i)) {
		    axisPath(naxis++) = i;
		}
	    }
	    // The further access path is along the trailing axes.
	    for (i=stmanPtr_p->nrCoordVector(); i<axisPath.nelements(); i++) {
		axisPath(naxis++) = i;
	    }
	    axisPath.resize (naxis);
	    hypercube->setCacheSize (1 + end - start, IPosition(),
				     IPosition(), axisPath, False, False);
	    hypercube->setLastColAccess (TSMCube::ColumnSliceAccess);
	    hypercube->setLastColSlice (slice);
	}
    }
    hypercube->accessStrided (start, end, stride,
			      (char*)dataPtr, colnr_p,
			      localPixelSize_p, writeFlag);
}


void TSMDataColumn::getfloatV (uInt rownr, float* dataPtr)
{
    accessCell (rownr, dataPtr, False);
}
void TSMDataColumn::putfloatV (uInt rownr, const float* dataPtr)
{
    accessCell (rownr, dataPtr, True);
}


void TSMDataColumn::getArrayfloatV (uInt rownr, Array<float>* dataPtr)
{
    Bool deleteIt;
    float* data = dataPtr->getStorage (deleteIt);
    accessCell (rownr, data, False);
    dataPtr->putStorage (data, deleteIt);
}

void TSMDataColumn::putArrayfloatV (uInt rownr, const Array<float>* dataPtr)
{
    Bool deleteIt;
    const float* data = dataPtr->getStorage (deleteIt);
    accessCell (rownr, data, True);
    dataPtr->freeStorage (data, deleteIt);
}

void TSMDataColumn::getSlicefloatV (uInt rownr, const Slicer& ns,
				    Array<float>* dataPtr)
{
    Bool deleteIt;
    float* data = dataPtr->getStorage (deleteIt);
    accessCellSlice (rownr, ns, data, False);
    dataPtr->putStorage (data, deleteIt);
}
void TSMDataColumn::putSlicefloatV (uInt rownr, const Slicer& ns,
				    const Array<float>* dataPtr)
{
    Bool deleteIt;
    const float* data = dataPtr->getStorage (deleteIt);
    accessCellSlice (rownr, ns, data, True);
    dataPtr->freeStorage (data, deleteIt);
}

void TSMDataColumn::getScalarColumnfloatV (Vector<float>* dataPtr)
{
    TSMDataColumn::getArrayColumnfloatV (dataPtr);
}
void TSMDataColumn::putScalarColumnfloatV (const Vector<float>* dataPtr)
{
    TSMDataColumn::putArrayColumnfloatV (dataPtr);
}

void TSMDataColumn::getArrayColumnfloatV (Array<float>* dataPtr)
{
    Bool deleteIt;
    float* data = dataPtr->getStorage (deleteIt);
    accessColumn (data, False);
    dataPtr->putStorage (data, deleteIt);
}
void TSMDataColumn::putArrayColumnfloatV (const Array<float>* dataPtr)
{
    Bool deleteIt;
    const float* data = dataPtr->getStorage (deleteIt);
    accessColumn (data, True);
    dataPtr->freeStorage (data, deleteIt);
}

void TSMDataColumn::getColumnSlicefloatV (const Slicer& ns,
					  Array<float>* dataPtr)
{
    Bool deleteIt;
    float* data = dataPtr->getStorage (deleteIt);
    accessColumnSlice (ns, data, False);
    dataPtr->putStorage (data, deleteIt);
}
void TSMDataColumn::putColumnSlicefloatV (const Slicer& ns,
					  const Array<float>* dataPtr)
{
    Bool deleteIt;
    const float* data = dataPtr->getStorage (deleteIt);
    accessColumnSlice (ns, data, True);
    dataPtr->freeStorage (data, deleteIt);
}



#define TSMDATACOLUMN_GETPUT(T,NM) \
void TSMDataColumn::aips_name2(get,NM) (uInt rownr, T* dataPtr) \
{ \
    accessCell (rownr, dataPtr, False); \
} \
void TSMDataColumn::aips_name2(put,NM) (uInt rownr, const T* dataPtr) \
{ \
    accessCell (rownr, dataPtr, True); \
} \
void TSMDataColumn::aips_name2(getArray,NM) (uInt rownr, Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    T* data = dataPtr->getStorage (deleteIt); \
    accessCell (rownr, data, False); \
    dataPtr->putStorage (data, deleteIt); \
} \
void TSMDataColumn::aips_name2(putArray,NM) (uInt rownr, const Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    const T* data = dataPtr->getStorage (deleteIt); \
    accessCell (rownr, data, True); \
    dataPtr->freeStorage (data, deleteIt); \
} \
void TSMDataColumn::aips_name2(getSlice,NM) (uInt rownr, const Slicer& ns, \
					     Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    T* data = dataPtr->getStorage (deleteIt); \
    accessCellSlice (rownr, ns, data, False); \
    dataPtr->putStorage (data, deleteIt); \
} \
void TSMDataColumn::aips_name2(putSlice,NM) (uInt rownr, const Slicer& ns, \
					     const Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    const T* data = dataPtr->getStorage (deleteIt); \
    accessCellSlice (rownr, ns, data, True); \
    dataPtr->freeStorage (data, deleteIt); \
} \
void TSMDataColumn::aips_name2(getScalarColumn,NM) (Vector<T>* dataPtr) \
{ \
    TSMDataColumn::aips_name2(getArrayColumn,NM) (dataPtr); \
} \
void TSMDataColumn::aips_name2(putScalarColumn,NM) (const Vector<T>* dataPtr) \
{ \
    TSMDataColumn::aips_name2(putArrayColumn,NM) (dataPtr); \
} \
void TSMDataColumn::aips_name2(getArrayColumn,NM) (Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    T* data = dataPtr->getStorage (deleteIt); \
    accessColumn (data, False); \
    dataPtr->putStorage (data, deleteIt); \
} \
void TSMDataColumn::aips_name2(putArrayColumn,NM) (const Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    const T* data = dataPtr->getStorage (deleteIt); \
    accessColumn (data, True); \
    dataPtr->freeStorage (data, deleteIt ); \
} \
void TSMDataColumn::aips_name2(getColumnSlice,NM) (const Slicer& ns, \
						   Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    T* data = dataPtr->getStorage (deleteIt); \
    accessColumnSlice (ns, data, False); \
    dataPtr->putStorage (data, deleteIt); \
} \
void TSMDataColumn::aips_name2(putColumnSlice,NM) (const Slicer& ns, \
						   const Array<T>* dataPtr) \
{ \
    Bool deleteIt; \
    const T* data = dataPtr->getStorage (deleteIt); \
    accessColumnSlice (ns, data, True); \
    dataPtr->freeStorage (data, deleteIt); \
}

TSMDATACOLUMN_GETPUT(Bool,BoolV)
TSMDATACOLUMN_GETPUT(uChar,uCharV)
TSMDATACOLUMN_GETPUT(Short,ShortV)
TSMDATACOLUMN_GETPUT(uShort,uShortV)
TSMDATACOLUMN_GETPUT(Int,IntV)
TSMDATACOLUMN_GETPUT(uInt,uIntV)
//#TSMDATACOLUMN_GETPUT(float,floatV)
TSMDATACOLUMN_GETPUT(double,doubleV)
TSMDATACOLUMN_GETPUT(Complex,ComplexV)
TSMDATACOLUMN_GETPUT(DComplex,DComplexV)
