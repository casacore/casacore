//# TiledFileAccess.cc: Tiled access to an array in a file
//# Copyright (C) 2001
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


#include <trial/Tables/TiledFileAccess.h>
#include <trial/Tables/TiledFileHelper.h>
#include <aips/Tables/TableError.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Utilities/ValType.h>
#include <aips/Utilities/Assert.h>


TiledFileAccess::TiledFileAccess (const String& fileName,
				  Int64 fileOffset,
				  const IPosition& shape,
				  const IPosition& tileShape,
				  DataType dataType,
				  uInt maximumCacheSize,
				  Bool writable,
				  Bool canonical)
: itsCube     (0),
  itsTSM      (0),
  itsWritable (writable),
  itsDataType (dataType)
{
  itsLocalPixelSize = ValType::getTypeSize (dataType);
  itsTSM  = new TiledFileHelper (fileName, shape, dataType, maximumCacheSize,
				 writable, canonical);
  itsCube = new TSMCube (itsTSM, itsTSM->file(), shape, tileShape, fileOffset);
}


TiledFileAccess::~TiledFileAccess()
{
  delete itsCube;
  delete itsTSM;
}

Array<Short> TiledFileAccess::getShort (const Slicer& section)
{
  Array<Short> arr;
  get (arr, section);
  return arr;
}
Array<Int> TiledFileAccess::getInt (const Slicer& section)
{
  Array<Int> arr;
  get (arr, section);
  return arr;
}
Array<Float> TiledFileAccess::getFloat (const Slicer& section)
{
  Array<Float> arr;
  get (arr, section);
  return arr;
}
Array<Double> TiledFileAccess::getDouble (const Slicer& section)
{
  Array<Double> arr;
  get (arr, section);
  return arr;
}
Array<Complex> TiledFileAccess::getComplex (const Slicer& section)
{
  Array<Complex> arr;
  get (arr, section);
  return arr;
}
Array<DComplex> TiledFileAccess::getDComplex (const Slicer& section)
{
  Array<DComplex> arr;
  get (arr, section);
  return arr;
}

void TiledFileAccess::get (Array<Short>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  Short* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<Int>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpInt, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  Int* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<Float>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpFloat, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  Float* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<Double>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpDouble, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  Double* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<Complex>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  Complex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<DComplex>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpDComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  DComplex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}


void TiledFileAccess::put (const Array<Short>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const Short* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<Int>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpInt, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const Int* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<Float>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpFloat, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const Float* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<Double>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpDouble, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const Double* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<Complex>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const Complex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<DComplex>& buffer,
			   const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpDComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const DComplex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}


void TiledFileAccess::setMaximumCacheSize (uInt nbytes)
{
  itsTSM->setMaximumCacheSize (nbytes);
}

uInt TiledFileAccess::maximumCacheSize() const
{
  return itsTSM->maximumCacheSize();
}
