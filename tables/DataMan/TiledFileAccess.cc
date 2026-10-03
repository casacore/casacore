// # TiledFileAccess.cc: Tiled access to an array in a file
// # Copyright (C) 2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/tables/DataMan/TiledFileHelper.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/HostInfo.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

TiledFileAccess::TiledFileAccess(const String& fileName, Int64 fileOffset, const IPosition& shape,
                                 const IPosition& tileShape, DataType dataType,
                                 const TSMOption& tsmOpt, bool writable)
    : itsCube(0), itsTSM(0), itsWritable(writable), itsDataType(dataType) {
  itsLocalPixelSize = ValType::getTypeSize(dataType);
  itsTSM = new TiledFileHelper(fileName, shape, dataType, tsmOpt, writable, HostInfo::bigEndian());
  itsCube = itsTSM->makeTSMCube(itsTSM->file(), shape, tileShape, Record(), fileOffset);
}

TiledFileAccess::TiledFileAccess(const String& fileName, Int64 fileOffset, const IPosition& shape,
                                 const IPosition& tileShape, DataType dataType,
                                 const TSMOption& tsmOpt, bool writable, bool bigEndian)
    : itsCube(0), itsTSM(0), itsWritable(writable), itsDataType(dataType) {
  itsLocalPixelSize = ValType::getTypeSize(dataType);
  itsTSM = new TiledFileHelper(fileName, shape, dataType, tsmOpt, writable, bigEndian);
  itsCube = itsTSM->makeTSMCube(itsTSM->file(), shape, tileShape, Record(), fileOffset);
}

TiledFileAccess::~TiledFileAccess() {
  delete itsCube;
  delete itsTSM;
}

Array<bool> TiledFileAccess::getBool(const Slicer& section) {
  Array<bool> arr;
  get(arr, section);
  return arr;
}
Array<uChar> TiledFileAccess::getUChar(const Slicer& section) {
  Array<uChar> arr;
  get(arr, section);
  return arr;
}
Array<Short> TiledFileAccess::getShort(const Slicer& section) {
  Array<Short> arr;
  get(arr, section);
  return arr;
}
Array<Int> TiledFileAccess::getInt(const Slicer& section) {
  Array<Int> arr;
  get(arr, section);
  return arr;
}
Array<Float> TiledFileAccess::getFloat(const Slicer& section) {
  Array<Float> arr;
  get(arr, section);
  return arr;
}
Array<Double> TiledFileAccess::getDouble(const Slicer& section) {
  Array<Double> arr;
  get(arr, section);
  return arr;
}
Array<Complex> TiledFileAccess::getComplex(const Slicer& section) {
  Array<Complex> arr;
  get(arr, section);
  return arr;
}
Array<DComplex> TiledFileAccess::getDComplex(const Slicer& section) {
  Array<DComplex> arr;
  get(arr, section);
  return arr;
}

void TiledFileAccess::get(Array<bool>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpBool, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  bool* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<uChar>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpUChar, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  uChar* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<Short>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  Short* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<Int>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpInt, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  Int* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<Float>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpFloat, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  Float* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<Double>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpDouble, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  Double* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<Complex>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  Complex* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

void TiledFileAccess::get(Array<DComplex>& buffer, const Slicer& section) {
  AlwaysAssert(itsDataType == TpDComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  buffer.resize(shp);
  bool deleteIt;
  DComplex* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, false);
  buffer.putStorage(dataPtr, deleteIt);
}

Array<Float> TiledFileAccess::getFloat(const Slicer& section, Float scale, Float offset,
                                       uChar deleteValue, bool examineForDeleteValues) {
  Array<Float> arr;
  get(arr, section, scale, offset, deleteValue, examineForDeleteValues);
  return arr;
}

Array<Float> TiledFileAccess::getFloat(const Slicer& section, Float scale, Float offset,
                                       Short deleteValue, bool examineForDeleteValues) {
  Array<Float> arr;
  get(arr, section, scale, offset, deleteValue, examineForDeleteValues);
  return arr;
}

Array<Float> TiledFileAccess::getFloat(const Slicer& section, Float scale, Float offset,
                                       Int deleteValue, bool examineForDeleteValues) {
  Array<Float> arr;
  get(arr, section, scale, offset, deleteValue, examineForDeleteValues);
  return arr;
}

void TiledFileAccess::get(Array<Float>& buffer, const Slicer& section, Float scale, Float offset,
                          uChar deleteValue, bool examineForDeleteValues) {
  Array<uChar> arr = getUChar(section);
  buffer.resize(arr.shape());
  bool deleteArr, deleteBuf;
  const uChar* arrPtr = arr.getStorage(deleteArr);
  Float* bufPtr = buffer.getStorage(deleteBuf);
  uInt64 n = arr.nelements();
  if (examineForDeleteValues) {
    for (uInt64 i = 0; i < n; i++) {
      if (arrPtr[i] == deleteValue) {
        setNaN(bufPtr[i]);
      } else {
        bufPtr[i] = arrPtr[i] * scale + offset;
      }
    }
  } else {
    for (uInt64 i = 0; i < n; i++) {
      bufPtr[i] = arrPtr[i] * scale + offset;
    }
  }
  arr.freeStorage(arrPtr, deleteArr);
  buffer.putStorage(bufPtr, deleteBuf);
}

void TiledFileAccess::get(Array<Float>& buffer, const Slicer& section, Float scale, Float offset,
                          Short deleteValue, bool examineForDeleteValues) {
  Array<Short> arr = getShort(section);
  buffer.resize(arr.shape());
  bool deleteArr, deleteBuf;
  const Short* arrPtr = arr.getStorage(deleteArr);
  Float* bufPtr = buffer.getStorage(deleteBuf);
  uInt64 n = arr.nelements();
  if (examineForDeleteValues) {
    for (uInt64 i = 0; i < n; i++) {
      if (arrPtr[i] == deleteValue) {
        setNaN(bufPtr[i]);
      } else {
        bufPtr[i] = arrPtr[i] * scale + offset;
      }
    }
  } else {
    for (uInt64 i = 0; i < n; i++) {
      bufPtr[i] = arrPtr[i] * scale + offset;
    }
  }
  arr.freeStorage(arrPtr, deleteArr);
  buffer.putStorage(bufPtr, deleteBuf);
}

void TiledFileAccess::get(Array<Float>& buffer, const Slicer& section, Float scale, Float offset,
                          Int deleteValue, bool examineForDeleteValues) {
  Array<Int> arr = getInt(section);
  buffer.resize(arr.shape());
  bool deleteArr, deleteBuf;
  const Int* arrPtr = arr.getStorage(deleteArr);
  Float* bufPtr = buffer.getStorage(deleteBuf);
  uInt64 n = arr.nelements();
  if (examineForDeleteValues) {
    for (uInt64 i = 0; i < n; i++) {
      if (arrPtr[i] == deleteValue) {
        setNaN(bufPtr[i]);
      } else {
        bufPtr[i] = arrPtr[i] * scale + offset;
      }
    }
  } else {
    for (uInt64 i = 0; i < n; i++) {
      bufPtr[i] = arrPtr[i] * scale + offset;
    }
  }
  arr.freeStorage(arrPtr, deleteArr);
  buffer.putStorage(bufPtr, deleteBuf);
}

void TiledFileAccess::put(const Array<bool>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpBool, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const bool* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<uChar>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const uChar* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<Short>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpShort, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const Short* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<Int>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpInt, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const Int* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<Float>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpFloat, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const Float* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<Double>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpDouble, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const Double* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<Complex>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const Complex* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::put(const Array<DComplex>& buffer, const Slicer& section) {
  AlwaysAssert(isWritable(), AipsError);
  AlwaysAssert(itsDataType == TpDComplex, AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource(itsCube->cubeShape(), start, end, stride);
  AlwaysAssert(shp.isEqual(buffer.shape()), AipsError);
  bool deleteIt;
  const DComplex* dataPtr = buffer.getStorage(deleteIt);
  itsCube->accessStrided(start, end, stride, (char*)dataPtr, 0, itsLocalPixelSize,
                         itsLocalPixelSize, true);
  buffer.freeStorage(dataPtr, deleteIt);
}

void TiledFileAccess::setMaximumCacheSize(uInt64 nbytes) { itsTSM->setMaximumCacheSize(nbytes); }

uInt64 TiledFileAccess::maximumCacheSize() const { return itsTSM->maximumCacheSize(); }

IPosition TiledFileAccess::makeTileShape(const IPosition& arrayShape, uInt nrPixelsPerTile) {
  Float nrPixels = nrPixelsPerTile;
  uInt ndim = arrayShape.nelements();
  IPosition tileShape(ndim, 1);
  for (uInt i = 0; i < ndim; i++) {
    uInt64 leng = arrayShape(i);
    if (leng <= nrPixels) {
      tileShape(i) = leng;
      nrPixels /= tileShape(i);
    } else {
      // Take a part of the axis as the tile shape.
      // The part must be exactly divisible, so we may have some work to do.
      uInt64 tileLeng = Int(nrPixels + 0.5);
      if (leng % tileLeng == 0) {
        tileShape(i) = tileLeng;
      } else {
        // Not exact, so try around this value until we find something.
        uInt64 nr = min(tileLeng, leng - tileLeng + 1);
        for (uInt64 j = 1; j < nr; j++) {
          if (leng % (tileLeng - j) == 0) {
            tileShape(i) = tileLeng - j;
            break;
          }
          if (leng % (tileLeng + j) == 0) {
            tileShape(i) = tileLeng + j;
            break;
          }
        }
        // It should always find a value; either 1 or leng.
        AlwaysAssert(tileShape(i) > 0, AipsError);
      }
      break;
    }
  }
  return tileShape;
}

}  // namespace casacore
