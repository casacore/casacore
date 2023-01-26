//# TiledFileAccess.cc: Tiled access to an array in a file
//# Copyright (C) 2001,2002
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


#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/tables/DataMan/TiledFileHelper.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/HostInfo.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledFileAccess::TiledFileAccess (const String& fileName,
				  int64_t fileOffset,
				  const IPosition& shape,
				  const IPosition& tileShape,
				  DataType dataType,
                                  const TSMOption& tsmOpt,
				  bool writable)
: itsCube     (0),
  itsTSM      (0),
  itsWritable (writable),
  itsDataType (dataType)
{
  itsLocalPixelSize = ValType::getTypeSize (dataType);
  itsTSM  = new TiledFileHelper (fileName, shape, dataType, tsmOpt,
				 writable, HostInfo::bigEndian());
  itsCube = itsTSM->makeTSMCube (itsTSM->file(), shape, tileShape,
                                 Record(), fileOffset);
}

TiledFileAccess::TiledFileAccess (const String& fileName,
				  int64_t fileOffset,
				  const IPosition& shape,
				  const IPosition& tileShape,
				  DataType dataType,
                                  const TSMOption& tsmOpt,
				  bool writable,
				  bool bigEndian)
: itsCube     (0),
  itsTSM      (0),
  itsWritable (writable),
  itsDataType (dataType)
{
  itsLocalPixelSize = ValType::getTypeSize (dataType);
  itsTSM  = new TiledFileHelper (fileName, shape, dataType, tsmOpt,
				 writable, bigEndian);
  itsCube = itsTSM->makeTSMCube (itsTSM->file(), shape, tileShape,
                                 Record(), fileOffset);
}

TiledFileAccess::~TiledFileAccess()
{
  delete itsCube;
  delete itsTSM;
}

Array<bool> TiledFileAccess::getBool (const Slicer& section)
{
  Array<bool> arr;
  get (arr, section);
  return arr;
}
Array<unsigned char> TiledFileAccess::getUChar (const Slicer& section)
{
  Array<unsigned char> arr;
  get (arr, section);
  return arr;
}
Array<int16_t> TiledFileAccess::getShort (const Slicer& section)
{
  Array<int16_t> arr;
  get (arr, section);
  return arr;
}
Array<int32_t> TiledFileAccess::getInt (const Slicer& section)
{
  Array<int32_t> arr;
  get (arr, section);
  return arr;
}
Array<float> TiledFileAccess::getFloat (const Slicer& section)
{
  Array<float> arr;
  get (arr, section);
  return arr;
}
Array<double> TiledFileAccess::getDouble (const Slicer& section)
{
  Array<double> arr;
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

void TiledFileAccess::get (Array<bool>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpBool, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  bool* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<unsigned char>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpUChar, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  unsigned char* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<int16_t>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  int16_t* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<int32_t>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpInt, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  int32_t* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<float>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpFloat, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  float* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<double>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpDouble, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  double* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<Complex>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  Complex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}

void TiledFileAccess::get (Array<DComplex>& buffer, const Slicer& section)
{
  AlwaysAssert (itsDataType == TpDComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  bool deleteIt;
  DComplex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, false);
  buffer.putStorage (dataPtr, deleteIt);
}


Array<float> TiledFileAccess::getFloat (const Slicer& section,
					float scale, float offset,
					unsigned char deleteValue, 
					bool examineForDeleteValues)
{
  Array<float> arr;
  get (arr, section, scale, offset, deleteValue, examineForDeleteValues);
  return arr;
}

Array<float> TiledFileAccess::getFloat (const Slicer& section,
					float scale, float offset,
					int16_t deleteValue, 
					bool examineForDeleteValues)
{
  Array<float> arr;
  get (arr, section, scale, offset, deleteValue, examineForDeleteValues);
  return arr;
}

Array<float> TiledFileAccess::getFloat (const Slicer& section,
					float scale, float offset,
					int32_t deleteValue, bool examineForDeleteValues)
{
  Array<float> arr;
  get (arr, section, scale, offset, deleteValue, examineForDeleteValues);
  return arr;
}

void TiledFileAccess::get (Array<float>& buffer, const Slicer& section,
			   float scale, float offset, unsigned char deleteValue,
                           bool examineForDeleteValues)
{
  Array<unsigned char> arr = getUChar (section);
  buffer.resize (arr.shape());
  bool deleteArr, deleteBuf;
  const unsigned char* arrPtr = arr.getStorage (deleteArr);
  float* bufPtr = buffer.getStorage (deleteBuf);
  uint64_t n = arr.nelements();
  if (examineForDeleteValues) {
    for (uint64_t i=0; i<n; i++) {
      if (arrPtr[i] == deleteValue) {
        setNaN (bufPtr[i]);
      } else {
        bufPtr[i] = arrPtr[i] * scale + offset;
      }
    }
  } else {
    for (uint64_t i=0; i<n; i++) {
      bufPtr[i] = arrPtr[i] * scale + offset;
    }
  }
  arr.freeStorage (arrPtr, deleteArr);
  buffer.putStorage (bufPtr, deleteBuf);
}

void TiledFileAccess::get (Array<float>& buffer, const Slicer& section,
			   float scale, float offset, int16_t deleteValue,
                           bool examineForDeleteValues)
{
  Array<int16_t> arr = getShort (section);
  buffer.resize (arr.shape());
  bool deleteArr, deleteBuf;
  const int16_t* arrPtr = arr.getStorage (deleteArr);
  float* bufPtr = buffer.getStorage (deleteBuf);
  uint64_t n = arr.nelements();
  if (examineForDeleteValues) {
    for (uint64_t i=0; i<n; i++) {
      if (arrPtr[i] == deleteValue) {
        setNaN (bufPtr[i]);
      } else {
        bufPtr[i] = arrPtr[i] * scale + offset;
      }
    }
  } else {
    for (uint64_t i=0; i<n; i++) {
      bufPtr[i] = arrPtr[i] * scale + offset;
    }
  }
  arr.freeStorage (arrPtr, deleteArr);
  buffer.putStorage (bufPtr, deleteBuf);
}

void TiledFileAccess::get (Array<float>& buffer, const Slicer& section,
			   float scale, float offset, int32_t deleteValue,
                           bool examineForDeleteValues)
{
  Array<int32_t> arr = getInt (section);
  buffer.resize (arr.shape());
  bool deleteArr, deleteBuf;
  const int32_t* arrPtr = arr.getStorage (deleteArr);
  float* bufPtr = buffer.getStorage (deleteBuf);
  uint64_t n = arr.nelements();
  if (examineForDeleteValues) {
    for (uint64_t i=0; i<n; i++) {
      if (arrPtr[i] == deleteValue) {
        setNaN (bufPtr[i]);
      } else {
        bufPtr[i] = arrPtr[i] * scale + offset;
      }
    }
  } else {
    for (uint64_t i=0; i<n; i++) {
      bufPtr[i] = arrPtr[i] * scale + offset;
    }
  }
  arr.freeStorage (arrPtr, deleteArr);
  buffer.putStorage (bufPtr, deleteBuf);
}


void TiledFileAccess::put (const Array<bool>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpBool, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  bool deleteIt;
  const bool* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<unsigned char>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  bool deleteIt;
  const unsigned char* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<int16_t>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  bool deleteIt;
  const int16_t* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<int32_t>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpInt, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  bool deleteIt;
  const int32_t* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<float>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpFloat, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  bool deleteIt;
  const float* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
  buffer.freeStorage (dataPtr, deleteIt);
}

void TiledFileAccess::put (const Array<double>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  AlwaysAssert (itsDataType == TpDouble, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  bool deleteIt;
  const double* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
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
  bool deleteIt;
  const Complex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
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
  bool deleteIt;
  const DComplex* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, itsLocalPixelSize, true);
  buffer.freeStorage (dataPtr, deleteIt);
}


void TiledFileAccess::setMaximumCacheSize (uint64_t nbytes)
{
  itsTSM->setMaximumCacheSize (nbytes);
}

uint64_t TiledFileAccess::maximumCacheSize() const
{
  return itsTSM->maximumCacheSize();
}

IPosition TiledFileAccess::makeTileShape (const IPosition& arrayShape,
					  uint32_t nrPixelsPerTile)
{
  float nrPixels = nrPixelsPerTile;
  uint32_t ndim = arrayShape.nelements();
  IPosition tileShape (ndim, 1);
  for (uint32_t i=0; i<ndim; i++) {
    uint64_t leng = arrayShape(i);
    if (leng <= nrPixels) {
      tileShape(i) = leng;
      nrPixels /= tileShape(i);
    } else {
      // Take a part of the axis as the tile shape.
      // The part must be exactly divisible, so we may have some work to do.
      uint64_t tileLeng = int32_t(nrPixels + 0.5);
      if (leng % tileLeng  ==  0) {
	tileShape(i) = tileLeng;
      } else {
	// Not exact, so try around this value until we find something.
	uint64_t nr = min (tileLeng, leng - tileLeng + 1);
	for (uint64_t j=1; j<nr; j++) {
	  if (leng % (tileLeng-j) == 0) {
	    tileShape(i) = tileLeng-j;
	    break;
	  }
	  if (leng % (tileLeng+j) == 0) {
	    tileShape(i) = tileLeng+j;
	    break;
	  }
	}
	// It should always find a value; either 1 or leng.
	AlwaysAssert (tileShape(i) > 0, AipsError);
      }
      break;
    }
  }
  return tileShape;
}

} //# NAMESPACE CASACORE - END

