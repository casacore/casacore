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

uInt64 TSMDataColumn::dataLength (uInt64 nrPixels) const
{
    // For Bools a byte can hold 8 pixels.
    if (tilePixelSize_p == 0) {
	return (nrPixels + 7) / 8;
    }
    return tilePixelSize_p * nrPixels;
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

void TSMDataColumn::setShape (rownr_t rownr, const IPosition& shape)
{
    setShapeTiled (rownr, shape, stmanPtr_p->defaultTileShape());
}

void TSMDataColumn::setShapeTiled (rownr_t rownr, const IPosition& shape,
				   const IPosition& tileShape)
{
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    IPosition cubeShape = hypercube->cubeShape();
    if (cubeShape.nelements() == 0) {
	stmanPtr_p->setShape (rownr, hypercube, shape, tileShape);
    }else{
        Bool eq = True;
        for (uInt i=0; i<shape.size(); ++i) {
          if (shape[i] != cubeShape[i]) {
            eq = False;
          }
        }
	if (!eq) {
	    if (! canChangeShape()) {
		throw TSMError ("Shape of data cells in same hypercube differs"
                                " for column " + columnName());
	    }
	    //# Set the new shape and take care that on the next access
	    //# the cache size is recalculated.
	    stmanPtr_p->setShape (rownr, hypercube, shape, tileShape);
	    hypercube->setLastColAccess (TSMCube::NoAccess);
	}
    }
}

Bool TSMDataColumn::isShapeDefined (rownr_t rownr)
{
    //# The shape is defined when the shape is fixed or when
    //# a hypercube has been defined for this row.
    if (shapeColumn().nelements() != 0) {
	return True;                             // FixedShape
    }
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    return (hypercube->cubeShape().nelements() != 0);
}

IPosition TSMDataColumn::shape (rownr_t rownr)
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

IPosition TSMDataColumn::tileShape (rownr_t rownr)
{
    //# If no shape is defined, throw exception.
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    return hypercube->tileShape();
}


void TSMDataColumn::accessCell (rownr_t rownr, const void* dataPtr, 
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

void TSMDataColumn::accessCellSlice (rownr_t rownr, const Slicer& ns,
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
	    for (uInt i=0; i<stmanPtr_p->nrCoordVector(); i++) {
		if (blc(i) == 0  &&  trc(i) == endcp(i)) {
		    axisPath(naxis++) = i;
		}
	    }
	    // The further access path is along the trailing axes.
	    for (uInt i=stmanPtr_p->nrCoordVector(); i<axisPath.nelements(); i++) {
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
  uInt64 chunkSize = arrShape.product() / arrShape(lastAxis) * localPixelSize_p;
  uInt64 nrinc = 0;
  Int64  lastRowPos = 0;
  TSMCube* lastCube = 0;
  IPosition rowpos;
  IPosition start(lastAxis+1);
  IPosition end(lastAxis+1);
  IPosition incr(lastAxis+1);
  // Step through all rownr intervals and all rownrs in each interval.
  RefRowsSliceIter iter(rownrs);
  while (! iter.pastEnd()) {
    rownr_t rownr = iter.sliceStart();
    rownr_t erow = iter.sliceEnd();
    rownr_t irow = iter.sliceIncr();
    while (rownr <= erow) {
      // Get the hypercube and the position of the row in it.
      // A read has to be done if we have another hypercube
      // or if the rownr is not higher.
      TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, rowpos);
      Int64 hcRowPos = rowpos(lastAxis);
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
	    throw DataManError("getArrayColumnCells shape mismatch in column "
                               + columnName());
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
  uInt64 chunkSize = arrShape.product() / arrShape(lastAxis) * localPixelSize_p;
  uInt64 nrinc = 0;
  Int64  lastRowPos = 0;
  TSMCube* lastCube = 0;
  IPosition rowpos;
  IPosition start(lastAxis+1);
  IPosition end(lastAxis+1);
  IPosition incr(lastAxis+1);
  // Step through all rownr intervals and all rownrs in each interval.
  RefRowsSliceIter iter(rownrs);
  while (! iter.pastEnd()) {
    rownr_t rownr = iter.sliceStart();
    rownr_t erow = iter.sliceEnd();
    rownr_t irow = iter.sliceIncr();
    while (rownr <= erow) {
      // Get the hypercube and the position of the row in it.
      // A read has to be done if we have another hypercube
      // or if the rownr is not higher.
      TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, rowpos);
      Int64 hcRowPos = rowpos(lastAxis);
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


void TSMDataColumn::getfloat (rownr_t rownr, float* dataPtr)
{
    accessCell (rownr, dataPtr, False);
}
void TSMDataColumn::putfloat (rownr_t rownr, const float* dataPtr)
{
    accessCell (rownr, dataPtr, True);
}


void TSMDataColumn::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
{
    Bool deleteIt;
    void* data = dataPtr.getVStorage (deleteIt);
    accessCell (rownr, data, False);
    dataPtr.putVStorage (data, deleteIt);
}

void TSMDataColumn::putArrayV (rownr_t rownr, const ArrayBase& dataPtr)
{
    Bool deleteIt;
    const void* data = dataPtr.getVStorage (deleteIt);
    accessCell (rownr, data, True);
    dataPtr.freeVStorage (data, deleteIt);
}

void TSMDataColumn::getSliceV (rownr_t rownr, const Slicer& ns,
                               ArrayBase& dataPtr)
{
    Bool deleteIt;
    void* data = dataPtr.getVStorage (deleteIt);
    accessCellSlice (rownr, ns, data, False);
    dataPtr.putVStorage (data, deleteIt);
}
void TSMDataColumn::putSliceV (rownr_t rownr, const Slicer& ns,
                               const ArrayBase& dataPtr)
{
    Bool deleteIt;
    const void* data = dataPtr.getVStorage (deleteIt);
    accessCellSlice (rownr, ns, data, True);
    dataPtr.freeVStorage (data, deleteIt);
}

void TSMDataColumn::getArrayColumnV (ArrayBase& dataPtr)
{
  if (! stmanPtr_p->canAccessColumn()) {
    getArrayColumnBase (dataPtr);
  } else {
    Bool deleteIt;
    void* data = dataPtr.getVStorage (deleteIt);
    accessColumn (data, False);
    dataPtr.putVStorage (data, deleteIt);
  }
}
void TSMDataColumn::putArrayColumnV (const ArrayBase& dataPtr)
{
  if (! stmanPtr_p->canAccessColumn()) {
    putArrayColumnBase (dataPtr);
  } else {
    Bool deleteIt;
    const void* data = dataPtr.getVStorage (deleteIt);
    accessColumn (data, True);
    dataPtr.freeVStorage (data, deleteIt);
  }
}

void TSMDataColumn::getColumnSliceV (const Slicer& ns,
                                     ArrayBase& dataPtr)
{
  if (! stmanPtr_p->canAccessColumn()) {
    getColumnSliceBase (ns, dataPtr);
  } else {
    Bool deleteIt;
    void* data = dataPtr.getVStorage (deleteIt);
    accessColumnSlice (ns, data, False);
    dataPtr.putVStorage (data, deleteIt);
  }
}
void TSMDataColumn::putColumnSliceV (const Slicer& ns,
                                     const ArrayBase& dataPtr)
{
  if (! stmanPtr_p->canAccessColumn()) {
    putColumnSliceBase (ns, dataPtr);
  } else {
    Bool deleteIt;
    const void* data = dataPtr.getVStorage (deleteIt);
    accessColumnSlice (ns, data, True);
    dataPtr.freeVStorage (data, deleteIt);
  }
}


void TSMDataColumn::getArrayColumnCellsV (const RefRows& rownrs,
                                          ArrayBase& dataPtr)
{
    // Only use optimized accessColumnCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr.ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	void* data = dataPtr.getVStorage (deleteIt);
	accessColumnCells (rownrs, dataPtr.shape(), data, False);
	dataPtr.putVStorage (data, deleteIt);
    } else {
        getArrayColumnCellsBase (rownrs, dataPtr);
    }
}

void TSMDataColumn::putArrayColumnCellsV (const RefRows& rownrs,
                                          const ArrayBase& dataPtr)
{
    // Only use optimized accessColumnCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr.ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	const void* data = dataPtr.getVStorage (deleteIt);
	accessColumnCells (rownrs, dataPtr.shape(), data, True);
	dataPtr.freeVStorage (data, deleteIt);
    } else {
        putArrayColumnCellsBase (rownrs, dataPtr);
    }
}

void TSMDataColumn::getColumnSliceCellsV (const RefRows& rownrs,
                                          const Slicer& ns,
                                          ArrayBase& dataPtr)
{
    // Only use optimized accessColumnSliceCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr.ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	void* data = dataPtr.getVStorage (deleteIt);
	accessColumnSliceCells (rownrs, ns, dataPtr.shape(), data, False);
	dataPtr.putVStorage (data, deleteIt);
    } else {
        getColumnSliceCellsBase (rownrs, ns, dataPtr);
    }
}

void TSMDataColumn::putColumnSliceCellsV (const RefRows& rownrs,
                                          const Slicer& ns,
                                          const ArrayBase& dataPtr)
{
    // Only use optimized accessColumnSliceCells for hypercubes where the rows
    // are mapped to a single axis.
    if (dataPtr.ndim() == stmanPtr_p->nrCoordVector() + 1) {
        Bool deleteIt;
	const void* data = dataPtr.getVStorage (deleteIt);
	accessColumnSliceCells (rownrs, ns, dataPtr.shape(), data, True);
	dataPtr.freeVStorage (data, deleteIt);
    } else {
        putColumnSliceCellsBase (rownrs, ns, dataPtr);
    }
}


#define TSMDATACOLUMN_GETPUT(T,NM) \
void TSMDataColumn::aips_name2(get,T) (rownr_t rownr, T* dataPtr) \
{ \
    accessCell (rownr, dataPtr, False); \
} \
void TSMDataColumn::aips_name2(put,T) (rownr_t rownr, const T* dataPtr) \
{ \
    accessCell (rownr, dataPtr, True); \
}

TSMDATACOLUMN_GETPUT(Bool,BoolV)
TSMDATACOLUMN_GETPUT(uChar,uCharV)
TSMDATACOLUMN_GETPUT(Short,ShortV)
TSMDATACOLUMN_GETPUT(uShort,uShortV)
TSMDATACOLUMN_GETPUT(Int,IntV)
TSMDATACOLUMN_GETPUT(uInt,uIntV)
TSMDATACOLUMN_GETPUT(Int64,Int64V)
//#TSMDATACOLUMN_GETPUT(float,floatV)
TSMDATACOLUMN_GETPUT(double,doubleV)
TSMDATACOLUMN_GETPUT(Complex,ComplexV)
TSMDATACOLUMN_GETPUT(DComplex,DComplexV)

} //# NAMESPACE CASACORE - END

