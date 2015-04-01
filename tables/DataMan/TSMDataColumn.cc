//# TSMDataColumn.cc: Tiled Hypercube Storage Manager for data columns
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/tables/DataMan/TSMDataColumn.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/string.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TSMDataColumn::TSMDataColumn (const TSMColumn& column)
: TSMColumn (column)
{
    DataType dt = DataType(dataType());
    localPixelSize_p = ValType::getTypeSize (dt);
    convPixelSize_p  = 1;
    if (dt == TpBool) {
	readFunc_p  = &Conversion::bitToBool;
	writeFunc_p = &Conversion::boolToBit;
	tilePixelSize_p = 0;
        mustConvert_p   = True;
    }else{
        Bool asBigEndian = stmanPtr_p->asBigEndian();
	ValType::getCanonicalFunc (dt, readFunc_p, writeFunc_p,
				   convPixelSize_p, asBigEndian);
	tilePixelSize_p = ValType::getCanonicalSize (dt, asBigEndian);
        mustConvert_p = localPixelSize_p > 1  &&
                        asBigEndian != HostInfo::bigEndian();
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
    return stmanPtr_p->canAccessColumn (reask);
}
Bool TSMDataColumn::canAccessArrayColumn (Bool& reask) const
{
    return stmanPtr_p->canAccessColumn (reask);
}
Bool TSMDataColumn::canAccessSlice (Bool& reask) const
{
    reask = False;
    return True;
}
Bool TSMDataColumn::canAccessColumnSlice (Bool& reask) const
{
    return stmanPtr_p->canAccessColumn (reask);
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
    return (hypercube->cubeShape().nelements() != 0);
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
        throw (DataManInvOper ("TSM: no array in row " +
			       String::toString(rownr) +
			       " of column " + columnName() +
			       " in "+ stmanPtr_p->fileName()));
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
				     IPosition(), IPosition(), True, False);
	    hypercube->setLastColAccess (TSMCube::CellAccess);
	}
    }
    hypercube->accessSection (start, end, (char*)dataPtr, colnr_p,
			      localPixelSize_p, tilePixelSize_p, writeFlag);
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
				     IPosition(), axisPath, True, False);
	    hypercube->setLastColAccess (TSMCube::SliceAccess);
	    hypercube->setLastColSlice (slice);
	}
    }
    hypercube->accessStrided (start, end, stride,
			      (char*)dataPtr, colnr_p,
			      localPixelSize_p, tilePixelSize_p, writeFlag);
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
				 IPosition(), IPosition(), True, False);
	hypercube->setLastColAccess (TSMCube::ColumnAccess);
    }
    hypercube->accessSection (start, end, (char*)dataPtr, colnr_p,
			      localPixelSize_p, tilePixelSize_p, writeFlag);
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
				     IPosition(), axisPath, True, False);
	    hypercube->setLastColAccess (TSMCube::ColumnSliceAccess);
	    hypercube->setLastColSlice (slice);
	}
    }
    hypercube->accessStrided (start, end, stride,
			      (char*)dataPtr, colnr_p,
			      localPixelSize_p, tilePixelSize_p, writeFlag);
}


void TSMDataColumn::accessColumnCells (const RefRows& rownrs,
				       const IPosition& arrShape,
				       const void* dataPtr, Bool writeFlag)
{
  char* data = (char*)(dataPtr);
  uInt lastAxis  = arrShape.nelements() - 1;
  IPosition cellShape = arrShape.getFirst (lastAxis);
  uInt chunkSize = arrShape.product() / arrShape(lastAxis) * localPixelSize_p;
  uInt nrinc = 0;
  Int  lastRowPos = 0;
  TSMCube* lastCube = 0;
  IPosition rowpos;
  IPosition start(lastAxis+1);
  IPosition end(lastAxis+1);
  IPosition incr(lastAxis+1);
  // Step through all rownr intervals and all rownrs in each interval.
  RefRowsSliceIter iter(rownrs);
  while (! iter.pastEnd()) {
    uInt rownr = iter.sliceStart();
    uInt erow = iter.sliceEnd();
    uInt irow = iter.sliceIncr();
    while (rownr <= erow) {
      // Get the hypercube and the position of the row in it.
      // A read has to be done if we have another hypercube
      // or if the rownr is not higher.
      TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, rowpos);
      Int hcRowPos = rowpos(lastAxis);
      Bool doIt = False;
      if (hypercube != lastCube  ||  hcRowPos <= lastRowPos) {
	doIt = True;
      } else {
	// The same hypercube. Check if the stride is the same.
	// The first time the stride has to be determined.
	if (nrinc == 0) {
	  incr(lastAxis) = hcRowPos - end(lastAxis);
	} else {
	  if (hcRowPos - end(lastAxis) != incr(lastAxis)) {
	    doIt = True;
	  }
	}
      }
      if (doIt) {
	if (lastCube != 0) {
	  accessFullCells (lastCube, data, writeFlag, start, end, incr);
	  data += (nrinc+1) * chunkSize;
	} else {
	  for (uInt i=0; i<lastAxis; i++) {
	    start(i) = 0;
	    end(i)   = rowpos(i) - 1;
	    incr(i)  = 1;
	  }
	}
	lastCube = hypercube;
	start(lastAxis) = hcRowPos;
	end(lastAxis)   = hcRowPos;
	incr(lastAxis)  = 1;
	nrinc = 0;
	// Each new hypercube might have a different shape, while it is
	// needed that all arrays to be read have the same shape.
	// So check if shape matches for non fixedshape arrays.
	if (! isFixedShape()) {
	  IPosition hcShape = lastCube->cubeShape().getFirst (lastAxis);
	  if (! cellShape.isEqual (hcShape)) {
	    throw DataManError("getArrayColumnCells shape mismatch");
	  }
	}
      } else {
	end(lastAxis) = hcRowPos;
	nrinc++;

      }
      lastRowPos = hcRowPos;
      rownr += irow;
    }
    iter++;
  }
  if (lastCube != 0) {
    accessFullCells (lastCube, data, writeFlag, start, end, incr);
  }
}

void TSMDataColumn::accessColumnSliceCells (const RefRows& rownrs,
					    const Slicer& ns,
					    const IPosition& arrShape,
					    const void* dataPtr,
					    Bool writeFlag)
{
  char* data = (char*)(dataPtr);
  uInt lastAxis  = arrShape.nelements() - 1;
  uInt chunkSize = arrShape.product() / arrShape(lastAxis) * localPixelSize_p;
  uInt nrinc = 0;
  Int  lastRowPos = 0;
  TSMCube* lastCube = 0;
  IPosition rowpos;
  IPosition start(lastAxis+1);
  IPosition end(lastAxis+1);
  IPosition incr(lastAxis+1);
  // Step through all rownr intervals and all rownrs in each interval.
  RefRowsSliceIter iter(rownrs);
  while (! iter.pastEnd()) {
    uInt rownr = iter.sliceStart();
    uInt erow = iter.sliceEnd();
    uInt irow = iter.sliceIncr();
    while (rownr <= erow) {
      // Get the hypercube and the position of the row in it.
      // A read has to be done if we have another hypercube
      // or if the rownr is not higher.
      TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, rowpos);
      Int hcRowPos = rowpos(lastAxis);
      Bool doIt = False;
      if (hypercube != lastCube  ||  hcRowPos <= lastRowPos) {
	doIt = True;
      } else {
	// The same hypercube. Check if the stride is the same.
	// The first time the stride has to be determined.
	if (nrinc == 0) {
	  incr(lastAxis) = hcRowPos - end(lastAxis);
	} else {
	  if (hcRowPos - end(lastAxis) != incr(lastAxis)) {
	    doIt = True;
	  }
	}
      }
      if (doIt) {
	if (lastCube != 0) {
	  accessSlicedCells (lastCube, data, writeFlag, start, end, incr);
	  data += (nrinc+1) * chunkSize;
	} else {
	  IPosition blc, trc, inc;
	  ns.inferShapeFromSource (shape(rownr), blc, trc, inc);
	  for (uInt i=0; i<lastAxis; i++) {
	    start(i) = blc(i);
	    end(i)   = trc(i);
	    incr(i)  = inc(i);
	  }
	}
	lastCube = hypercube;
	start(lastAxis) = hcRowPos;
	end(lastAxis)   = hcRowPos;
	incr(lastAxis)  = 1;
	nrinc = 0;
      } else {
	end(lastAxis) = hcRowPos;
	nrinc++;
      }
      lastRowPos = hcRowPos;
      rownr += irow;
    }
    iter++;
  }
  if (lastCube != 0) {
    accessSlicedCells (lastCube, data, writeFlag, start, end, incr);
  }
}

void TSMDataColumn::accessFullCells (TSMCube* hypercube,
				     char* dataPtr, Bool writeFlag,
				     const IPosition& start,
				     const IPosition& end,
				     const IPosition& incr)
{
  //  cout << "accessFullCells " << start << end << incr << endl;
  // Size the cache if the user has not done it.
  if (! stmanPtr_p->userSetCache (0)) {
    if (hypercube->getLastColAccess() != TSMCube::ColumnAccess) {
      hypercube->setCacheSize (hypercube->cubeShape(), IPosition(),
			       IPosition(), IPosition(), True, False);
      hypercube->setLastColAccess (TSMCube::ColumnAccess);
    }
  }
  hypercube->accessStrided (start, end, incr, dataPtr, colnr_p,
			    localPixelSize_p, tilePixelSize_p, writeFlag);
}

void TSMDataColumn::accessSlicedCells (TSMCube* hypercube,
				       char* dataPtr, Bool writeFlag,
				       const IPosition& start,
				       const IPosition& end,
				       const IPosition& incr)
{
  //  cout << "accessSlicedCells " << start << end << incr << endl;
  // Size the cache if the user has not done it.
  if (! stmanPtr_p->userSetCache (0)) {
    // The main access path is assumed to be along the full slice
    // dimensions.
    uInt naxis = 0;
    IPosition axisPath (end.nelements());
    IPosition sliceShp = hypercube->cubeShape();
    for (uInt i=0; i<stmanPtr_p->nrCoordVector(); i++) {
      if (start(i) == 0  &&  end(i) == sliceShp(i)-1) {
	axisPath(naxis++) = i;
      }
      sliceShp(i) = 1 + end(i) - start(i);
    }
    // Set only if a different slice is accessed.
    if (hypercube->getLastColAccess() != TSMCube::ColumnSliceAccess
    ||  ! sliceShp.isEqual (hypercube->getLastColSlice())) {
      // The further access path is along the trailing axes.
      for (uInt i=stmanPtr_p->nrCoordVector(); i<axisPath.nelements(); i++) {
	axisPath(naxis++) = i;
      }
      axisPath.resize (naxis);
      hypercube->setCacheSize (sliceShp, IPosition(),
			       IPosition(), axisPath, True, False);
      hypercube->setLastColAccess (TSMCube::ColumnSliceAccess);
      hypercube->setLastColSlice (sliceShp);
    }
  }
  hypercube->accessStrided (start, end, incr, dataPtr, colnr_p,
			    localPixelSize_p, tilePixelSize_p, writeFlag);
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

void TSMDataColumn::getScalarColumnCellsfloatV (const RefRows& rownrs,
						Vector<float>* dataPtr)
{
    // Only use optimized accessColumnCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) {
        TSMDataColumn::getArrayColumnCellsfloatV (rownrs, dataPtr);
    } else {
        StManColumn::getScalarColumnCellsfloatV (rownrs, dataPtr);
    }
}

void TSMDataColumn::putScalarColumnCellsfloatV (const RefRows& rownrs,
						const Vector<float>* dataPtr)
{
    // Only use optimized accessColumnCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) {
        TSMDataColumn::putArrayColumnCellsfloatV (rownrs, dataPtr);
    } else {
        StManColumn::putScalarColumnCellsfloatV (rownrs, dataPtr);
    }
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


void TSMDataColumn::getArrayColumnCellsfloatV (const RefRows& rownrs,
					       Array<float>* dataPtr)
{
    // Only use optimized accessColumnCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	float* data = dataPtr->getStorage (deleteIt);
	accessColumnCells (rownrs, dataPtr->shape(), data, False);
	dataPtr->putStorage (data, deleteIt);
    } else {
        StManColumn::getArrayColumnCellsfloatV (rownrs, dataPtr);
    }
}

void TSMDataColumn::putArrayColumnCellsfloatV (const RefRows& rownrs,
					       const Array<float>* dataPtr)
{
    // Only use optimized accessColumnCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	const float* data = dataPtr->getStorage (deleteIt);
	accessColumnCells (rownrs, dataPtr->shape(), data, True);
	dataPtr->freeStorage (data, deleteIt);
    } else {
        StManColumn::putArrayColumnCellsfloatV (rownrs, dataPtr);
    }
}

void TSMDataColumn::getColumnSliceCellsfloatV (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<float>* dataPtr)
{
    // Only use optimized accessColumnSliceCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	float* data = dataPtr->getStorage (deleteIt);
	accessColumnSliceCells (rownrs, ns, dataPtr->shape(), data, False);
	dataPtr->putStorage (data, deleteIt);
    } else {
        StManColumn::getColumnSliceCellsfloatV (rownrs, ns, dataPtr);
    }
}

void TSMDataColumn::putColumnSliceCellsfloatV (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<float>* dataPtr)
{
    // Only use optimized accessColumnSliceCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	const float* data = dataPtr->getStorage (deleteIt);
	accessColumnSliceCells (rownrs, ns, dataPtr->shape(), data, True);
	dataPtr->freeStorage (data, deleteIt);
    } else {
        StManColumn::putColumnSliceCellsfloatV (rownrs, ns, dataPtr);
    }
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
} \
void TSMDataColumn::aips_name2(getScalarColumnCells,NM)(const RefRows& rownrs,\
						        Vector<T>* dataPtr) \
{ \
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) { \
        TSMDataColumn::aips_name2(getArrayColumnCells,NM) (rownrs, dataPtr); \
    } else { \
        StManColumn::aips_name2(getScalarColumnCells,NM) (rownrs, dataPtr); \
    } \
} \
void TSMDataColumn::aips_name2(putScalarColumnCells,NM)(const RefRows& rownrs,\
						    const Vector<T>* dataPtr) \
{ \
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) { \
        TSMDataColumn::aips_name2(putArrayColumnCells,NM) (rownrs, dataPtr); \
    } else { \
        StManColumn::aips_name2(putScalarColumnCells,NM) (rownrs, dataPtr); \
    } \
} \
void TSMDataColumn::aips_name2(getArrayColumnCells,NM)(const RefRows& rownrs, \
					               Array<T>* dataPtr) \
{ \
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) { \
        Bool deleteIt; \
	T* data = dataPtr->getStorage (deleteIt); \
	accessColumnCells (rownrs, dataPtr->shape(), data, False); \
	dataPtr->putStorage (data, deleteIt); \
    } else { \
        StManColumn::aips_name2(getArrayColumnCells,NM) (rownrs, dataPtr); \
    } \
} \
void TSMDataColumn::aips_name2(putArrayColumnCells,NM)(const RefRows& rownrs, \
					             const Array<T>* dataPtr) \
{ \
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) { \
        Bool deleteIt; \
	const T* data = dataPtr->getStorage (deleteIt); \
	accessColumnCells (rownrs, dataPtr->shape(), data, True); \
	dataPtr->freeStorage (data, deleteIt); \
    } else { \
        StManColumn::aips_name2(putArrayColumnCells,NM) (rownrs, dataPtr); \
    } \
} \
void TSMDataColumn::aips_name2(getColumnSliceCells,NM)(const RefRows& rownrs, \
						       const Slicer& ns, \
						       Array<T>* dataPtr) \
{ \
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) { \
        Bool deleteIt; \
	T* data = dataPtr->getStorage (deleteIt); \
	accessColumnSliceCells (rownrs, ns, dataPtr->shape(), data, False); \
	dataPtr->putStorage (data, deleteIt); \
    } else { \
        StManColumn::aips_name2(getColumnSliceCells,NM) (rownrs, ns, dataPtr);\
    } \
} \
void TSMDataColumn::aips_name2(putColumnSliceCells,NM)(const RefRows& rownrs, \
					               const Slicer& ns, \
					             const Array<T>* dataPtr) \
{ \
    if (dataPtr->ndim() == stmanPtr_p->nrCoordVector() + 1) { \
        Bool deleteIt; \
	const T* data = dataPtr->getStorage (deleteIt); \
	accessColumnSliceCells (rownrs, ns, dataPtr->shape(), data, True); \
	dataPtr->freeStorage (data, deleteIt); \
    } else { \
        StManColumn::aips_name2(putColumnSliceCells,NM) (rownrs, ns, dataPtr);\
    } \
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

} //# NAMESPACE CASACORE - END

