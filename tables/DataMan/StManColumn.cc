// # StManColumn.cc: Base storage manager column class
// # Copyright (C) 1994,1995,1996,1998,1999,2002
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

// # Includes
#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/DataMan/DataManError.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

StManColumn::~StManColumn() {}

// Map to backward compatibility functions.
void StManColumn::setShape(rownr_t rownr, const IPosition& shape) { setShape(static_cast<unsigned int>(rownr), shape); }

void StManColumn::setShapeTiled(rownr_t rownr, const IPosition& shape, const IPosition& tileShape) {
  setShapeTiled(static_cast<unsigned int>(rownr), shape, tileShape);
}

bool StManColumn::isShapeDefined(rownr_t rownr) { return isShapeDefined(static_cast<unsigned int>(rownr)); }

unsigned int StManColumn::ndim(rownr_t rownr) { return ndim(static_cast<unsigned int>(rownr)); }

IPosition StManColumn::shape(rownr_t rownr) { return shape(static_cast<unsigned int>(rownr)); }

IPosition StManColumn::tileShape(rownr_t rownr) { return tileShape(static_cast<unsigned int>(rownr)); }

void StManColumn::setShape(unsigned int, const IPosition&) {
  throw DataManInvOper(
      "setShape only allowed for non-FixedShape arrays"
      " in column " +
      columnName());
}

void StManColumn::setShapeTiled(unsigned int rownr, const IPosition& shape, const IPosition&) {
  setShape(rownr, shape);
}

// By default the shape is defined (for scalars).
bool StManColumn::isShapeDefined(unsigned int) { return true; }

// The default implementation of ndim is to use the shape.
unsigned int StManColumn::ndim(unsigned int rownr) { return shape(rownr).nelements(); }

// The shape of the array in the given row.
IPosition StManColumn::shape(unsigned int) { return IPosition(0); }

// The tile shape of the array in the given row.
IPosition StManColumn::tileShape(unsigned int) { return IPosition(0); }

// The following takes care of backward compatibility for external storage managers.
// It maps the get/putXX functions taking rownr_t to the old get/putXXV taking uInt.
// As before the default get/putXXV implementations throw a 'not implemented' exception.
void StManColumn::getBool(rownr_t rownr, bool* dataPtr) { getBoolV(rownr, dataPtr); }

void StManColumn::putBool(rownr_t rownr, const bool* dataPtr) { putBoolV(rownr, dataPtr); }

void StManColumn::getBoolV(unsigned int, bool*) { throwInvalidOp("getBoolV"); }

void StManColumn::putBoolV(unsigned int, const bool*) { throwInvalidOp("putBoolV"); }

void StManColumn::getuChar(rownr_t rownr, unsigned char* dataPtr) { getuCharV(rownr, dataPtr); }

void StManColumn::putuChar(rownr_t rownr, const unsigned char* dataPtr) { putuCharV(rownr, dataPtr); }

void StManColumn::getuCharV(unsigned int, unsigned char*) { throwInvalidOp("getuCharV"); }

void StManColumn::putuCharV(unsigned int, const unsigned char*) { throwInvalidOp("putuCharV"); }

void StManColumn::getShort(rownr_t rownr, short* dataPtr) { getShortV(rownr, dataPtr); }

void StManColumn::putShort(rownr_t rownr, const short* dataPtr) { putShortV(rownr, dataPtr); }

void StManColumn::getShortV(unsigned int, short*) { throwInvalidOp("getShortV"); }

void StManColumn::putShortV(unsigned int, const short*) { throwInvalidOp("putShortV"); }

void StManColumn::getuShort(rownr_t rownr, unsigned short* dataPtr) { getuShortV(rownr, dataPtr); }

void StManColumn::putuShort(rownr_t rownr, const unsigned short* dataPtr) { putuShortV(rownr, dataPtr); }

void StManColumn::getuShortV(unsigned int, unsigned short*) { throwInvalidOp("getuShortV"); }

void StManColumn::putuShortV(unsigned int, const unsigned short*) { throwInvalidOp("putuShortV"); }

void StManColumn::getInt(rownr_t rownr, int* dataPtr) { getIntV(rownr, dataPtr); }

void StManColumn::putInt(rownr_t rownr, const int* dataPtr) { putIntV(rownr, dataPtr); }

void StManColumn::getIntV(unsigned int, int*) { throwInvalidOp("getIntV"); }

void StManColumn::putIntV(unsigned int, const int*) { throwInvalidOp("putIntV"); }

void StManColumn::getuInt(rownr_t rownr, unsigned int* dataPtr) { getuIntV(rownr, dataPtr); }

void StManColumn::putuInt(rownr_t rownr, const unsigned int* dataPtr) { putuIntV(rownr, dataPtr); }

void StManColumn::getuIntV(unsigned int, unsigned int*) { throwInvalidOp("getuIntV"); }

void StManColumn::putuIntV(unsigned int, const unsigned int*) { throwInvalidOp("putuIntV"); }

void StManColumn::getfloat(rownr_t rownr, float* dataPtr) { getFloatV(rownr, dataPtr); }

void StManColumn::putfloat(rownr_t rownr, const float* dataPtr) { putFloatV(rownr, dataPtr); }

void StManColumn::getFloatV(unsigned int, float*) { throwInvalidOp("getFloatV"); }

void StManColumn::putFloatV(unsigned int, const float*) { throwInvalidOp("putFloatV"); }

void StManColumn::getdouble(rownr_t rownr, double* dataPtr) { getDoubleV(rownr, dataPtr); }

void StManColumn::putdouble(rownr_t rownr, const double* dataPtr) { putDoubleV(rownr, dataPtr); }

void StManColumn::getDoubleV(unsigned int, double*) { throwInvalidOp("getDoubleV"); }

void StManColumn::putDoubleV(unsigned int, const double*) { throwInvalidOp("putDoubleV"); }

void StManColumn::getComplex(rownr_t rownr, Complex* dataPtr) { getComplexV(rownr, dataPtr); }

void StManColumn::putComplex(rownr_t rownr, const Complex* dataPtr) { putComplexV(rownr, dataPtr); }

void StManColumn::getComplexV(unsigned int, Complex*) { throwInvalidOp("getComplexV"); }

void StManColumn::putComplexV(unsigned int, const Complex*) { throwInvalidOp("putComplexV"); }

void StManColumn::getDComplex(rownr_t rownr, DComplex* dataPtr) { getDComplexV(rownr, dataPtr); }

void StManColumn::putDComplex(rownr_t rownr, const DComplex* dataPtr) {
  putDComplexV(rownr, dataPtr);
}

void StManColumn::getDComplexV(unsigned int, DComplex*) { throwInvalidOp("getDComplexV"); }

void StManColumn::putDComplexV(unsigned int, const DComplex*) { throwInvalidOp("putDComplexV"); }

void StManColumn::getString(rownr_t rownr, String* dataPtr) { getStringV(rownr, dataPtr); }

void StManColumn::putString(rownr_t rownr, const String* dataPtr) { putStringV(rownr, dataPtr); }

void StManColumn::getStringV(unsigned int, String*) { throwInvalidOp("getStringV"); }

void StManColumn::putStringV(unsigned int, const String*) { throwInvalidOp("putStringV"); }

// # Call the correct getScalarColumnX function depending on the data type.
void StManColumn::getScalarColumnV(ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getScalarColumnBoolV(static_cast<Vector<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getScalarColumnuCharV(static_cast<Vector<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getScalarColumnShortV(static_cast<Vector<short>*>(&dataPtr));
      break;
    case TpUShort:
      getScalarColumnuShortV(static_cast<Vector<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getScalarColumnIntV(static_cast<Vector<int>*>(&dataPtr));
      break;
    case TpUInt:
      getScalarColumnuIntV(static_cast<Vector<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getScalarColumnInt64V(static_cast<Vector<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getScalarColumnfloatV(static_cast<Vector<float>*>(&dataPtr));
      break;
    case TpDouble:
      getScalarColumndoubleV(static_cast<Vector<double>*>(&dataPtr));
      break;
    case TpComplex:
      getScalarColumnComplexV(static_cast<Vector<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getScalarColumnDComplexV(static_cast<Vector<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getScalarColumnStringV(static_cast<Vector<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getScalarColumn"));
  }
}

// # Call the correct putScalarColumnX function depending on the data type.
void StManColumn::putScalarColumnV(const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putScalarColumnBoolV(static_cast<const Vector<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putScalarColumnuCharV(static_cast<const Vector<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putScalarColumnShortV(static_cast<const Vector<short>*>(&dataPtr));
      break;
    case TpUShort:
      putScalarColumnuShortV(static_cast<const Vector<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putScalarColumnIntV(static_cast<const Vector<int>*>(&dataPtr));
      break;
    case TpUInt:
      putScalarColumnuIntV(static_cast<const Vector<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putScalarColumnInt64V(static_cast<const Vector<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putScalarColumnfloatV(static_cast<const Vector<float>*>(&dataPtr));
      break;
    case TpDouble:
      putScalarColumndoubleV(static_cast<const Vector<double>*>(&dataPtr));
      break;
    case TpComplex:
      putScalarColumnComplexV(static_cast<const Vector<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putScalarColumnDComplexV(static_cast<const Vector<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putScalarColumnStringV(static_cast<const Vector<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putScalarColumn"));
  }
}

// # Call the correct getScalarColumnCellsX function depending on the data type.
void StManColumn::getScalarColumnCellsV(const RefRows& rownrs, ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getScalarColumnCellsBoolV(rownrs, static_cast<Vector<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getScalarColumnCellsuCharV(rownrs, static_cast<Vector<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getScalarColumnCellsShortV(rownrs, static_cast<Vector<short>*>(&dataPtr));
      break;
    case TpUShort:
      getScalarColumnCellsuShortV(rownrs, static_cast<Vector<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getScalarColumnCellsIntV(rownrs, static_cast<Vector<int>*>(&dataPtr));
      break;
    case TpUInt:
      getScalarColumnCellsuIntV(rownrs, static_cast<Vector<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getScalarColumnCellsInt64V(rownrs, static_cast<Vector<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getScalarColumnCellsfloatV(rownrs, static_cast<Vector<float>*>(&dataPtr));
      break;
    case TpDouble:
      getScalarColumnCellsdoubleV(rownrs, static_cast<Vector<double>*>(&dataPtr));
      break;
    case TpComplex:
      getScalarColumnCellsComplexV(rownrs, static_cast<Vector<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getScalarColumnCellsDComplexV(rownrs, static_cast<Vector<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getScalarColumnCellsStringV(rownrs, static_cast<Vector<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getScalarColumnCells"));
  }
}

// # Call the correct putScalarColumnCellsX function depending on the data type.
void StManColumn::putScalarColumnCellsV(const RefRows& rownrs, const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putScalarColumnCellsBoolV(rownrs, static_cast<const Vector<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putScalarColumnCellsuCharV(rownrs, static_cast<const Vector<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putScalarColumnCellsShortV(rownrs, static_cast<const Vector<short>*>(&dataPtr));
      break;
    case TpUShort:
      putScalarColumnCellsuShortV(rownrs, static_cast<const Vector<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putScalarColumnCellsIntV(rownrs, static_cast<const Vector<int>*>(&dataPtr));
      break;
    case TpUInt:
      putScalarColumnCellsuIntV(rownrs, static_cast<const Vector<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putScalarColumnCellsInt64V(rownrs, static_cast<const Vector<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putScalarColumnCellsfloatV(rownrs, static_cast<const Vector<float>*>(&dataPtr));
      break;
    case TpDouble:
      putScalarColumnCellsdoubleV(rownrs, static_cast<const Vector<double>*>(&dataPtr));
      break;
    case TpComplex:
      putScalarColumnCellsComplexV(rownrs, static_cast<const Vector<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putScalarColumnCellsDComplexV(rownrs, static_cast<const Vector<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putScalarColumnCellsStringV(rownrs, static_cast<const Vector<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putScalarColumnCells"));
  }
}

// # Call the correct getArrayX function depending on the data type.
void StManColumn::getArrayV(rownr_t rownr, ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getArrayBoolV(rownr, static_cast<Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getArrayuCharV(rownr, static_cast<Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getArrayShortV(rownr, static_cast<Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      getArrayuShortV(rownr, static_cast<Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getArrayIntV(rownr, static_cast<Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      getArrayuIntV(rownr, static_cast<Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getArrayInt64V(rownr, static_cast<Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getArrayfloatV(rownr, static_cast<Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      getArraydoubleV(rownr, static_cast<Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      getArrayComplexV(rownr, static_cast<Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getArrayDComplexV(rownr, static_cast<Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getArrayStringV(rownr, static_cast<Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getArray"));
  }
}

// # Call the correct putArrayX function depending on the data type.
void StManColumn::putArrayV(rownr_t rownr, const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putArrayBoolV(rownr, static_cast<const Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putArrayuCharV(rownr, static_cast<const Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putArrayShortV(rownr, static_cast<const Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      putArrayuShortV(rownr, static_cast<const Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putArrayIntV(rownr, static_cast<const Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      putArrayuIntV(rownr, static_cast<const Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putArrayInt64V(rownr, static_cast<const Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putArrayfloatV(rownr, static_cast<const Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      putArraydoubleV(rownr, static_cast<const Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      putArrayComplexV(rownr, static_cast<const Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putArrayDComplexV(rownr, static_cast<const Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putArrayStringV(rownr, static_cast<const Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putArray"));
  }
}

// # Call the correct getColumnSliceX function depending on the data type.
void StManColumn::getSliceV(rownr_t rownr, const Slicer& ns, ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getSliceBoolV(rownr, ns, static_cast<Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getSliceuCharV(rownr, ns, static_cast<Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getSliceShortV(rownr, ns, static_cast<Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      getSliceuShortV(rownr, ns, static_cast<Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getSliceIntV(rownr, ns, static_cast<Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      getSliceuIntV(rownr, ns, static_cast<Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getSliceInt64V(rownr, ns, static_cast<Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getSlicefloatV(rownr, ns, static_cast<Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      getSlicedoubleV(rownr, ns, static_cast<Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      getSliceComplexV(rownr, ns, static_cast<Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getSliceDComplexV(rownr, ns, static_cast<Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getSliceStringV(rownr, ns, static_cast<Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getSlice"));
  }
}

// # Call the correct putSliceX function depending on the data type.
void StManColumn::putSliceV(rownr_t rownr, const Slicer& ns, const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putSliceBoolV(rownr, ns, static_cast<const Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putSliceuCharV(rownr, ns, static_cast<const Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putSliceShortV(rownr, ns, static_cast<const Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      putSliceuShortV(rownr, ns, static_cast<const Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putSliceIntV(rownr, ns, static_cast<const Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      putSliceuIntV(rownr, ns, static_cast<const Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putSliceInt64V(rownr, ns, static_cast<const Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putSlicefloatV(rownr, ns, static_cast<const Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      putSlicedoubleV(rownr, ns, static_cast<const Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      putSliceComplexV(rownr, ns, static_cast<const Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putSliceDComplexV(rownr, ns, static_cast<const Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putSliceStringV(rownr, ns, static_cast<const Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putSlice"));
  }
}

// # Call the correct getArrayColumnX function depending on the data type.
void StManColumn::getArrayColumnV(ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getArrayColumnBoolV(static_cast<Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getArrayColumnuCharV(static_cast<Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getArrayColumnShortV(static_cast<Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      getArrayColumnuShortV(static_cast<Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getArrayColumnIntV(static_cast<Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      getArrayColumnuIntV(static_cast<Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getArrayColumnInt64V(static_cast<Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getArrayColumnfloatV(static_cast<Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      getArrayColumndoubleV(static_cast<Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      getArrayColumnComplexV(static_cast<Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getArrayColumnDComplexV(static_cast<Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getArrayColumnStringV(static_cast<Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getArrayColumn"));
  }
}

// # Call the correct putArrayColumnX function depending on the data type.
void StManColumn::putArrayColumnV(const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putArrayColumnBoolV(static_cast<const Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putArrayColumnuCharV(static_cast<const Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putArrayColumnShortV(static_cast<const Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      putArrayColumnuShortV(static_cast<const Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putArrayColumnIntV(static_cast<const Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      putArrayColumnuIntV(static_cast<const Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putArrayColumnInt64V(static_cast<const Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putArrayColumnfloatV(static_cast<const Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      putArrayColumndoubleV(static_cast<const Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      putArrayColumnComplexV(static_cast<const Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putArrayColumnDComplexV(static_cast<const Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putArrayColumnStringV(static_cast<const Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putArrayColumn"));
  }
}

// # Call the correct getArrayColumnCellsX function depending on the data type.
void StManColumn::getArrayColumnCellsV(const RefRows& rownrs, ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getArrayColumnCellsBoolV(rownrs, static_cast<Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getArrayColumnCellsuCharV(rownrs, static_cast<Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getArrayColumnCellsShortV(rownrs, static_cast<Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      getArrayColumnCellsuShortV(rownrs, static_cast<Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getArrayColumnCellsIntV(rownrs, static_cast<Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      getArrayColumnCellsuIntV(rownrs, static_cast<Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getArrayColumnCellsInt64V(rownrs, static_cast<Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getArrayColumnCellsfloatV(rownrs, static_cast<Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      getArrayColumnCellsdoubleV(rownrs, static_cast<Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      getArrayColumnCellsComplexV(rownrs, static_cast<Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getArrayColumnCellsDComplexV(rownrs, static_cast<Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getArrayColumnCellsStringV(rownrs, static_cast<Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getArrayColumnCells"));
  }
}

// # Call the correct putArrayColumnCellsX function depending on the data type.
void StManColumn::putArrayColumnCellsV(const RefRows& rownrs, const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putArrayColumnCellsBoolV(rownrs, static_cast<const Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putArrayColumnCellsuCharV(rownrs, static_cast<const Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putArrayColumnCellsShortV(rownrs, static_cast<const Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      putArrayColumnCellsuShortV(rownrs, static_cast<const Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putArrayColumnCellsIntV(rownrs, static_cast<const Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      putArrayColumnCellsuIntV(rownrs, static_cast<const Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putArrayColumnCellsInt64V(rownrs, static_cast<const Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putArrayColumnCellsfloatV(rownrs, static_cast<const Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      putArrayColumnCellsdoubleV(rownrs, static_cast<const Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      putArrayColumnCellsComplexV(rownrs, static_cast<const Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putArrayColumnCellsDComplexV(rownrs, static_cast<const Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putArrayColumnCellsStringV(rownrs, static_cast<const Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putArrayColumnCells"));
  }
}

// # Call the correct getColumnSliceX function depending on the data type.
void StManColumn::getColumnSliceV(const Slicer& ns, ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getColumnSliceBoolV(ns, static_cast<Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getColumnSliceuCharV(ns, static_cast<Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getColumnSliceShortV(ns, static_cast<Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      getColumnSliceuShortV(ns, static_cast<Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getColumnSliceIntV(ns, static_cast<Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      getColumnSliceuIntV(ns, static_cast<Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getColumnSliceInt64V(ns, static_cast<Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getColumnSlicefloatV(ns, static_cast<Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      getColumnSlicedoubleV(ns, static_cast<Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      getColumnSliceComplexV(ns, static_cast<Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getColumnSliceDComplexV(ns, static_cast<Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getColumnSliceStringV(ns, static_cast<Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getColumnSlice"));
  }
}

// # Call the correct putColumnSliceX function depending on the data type.
void StManColumn::putColumnSliceV(const Slicer& ns, const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putColumnSliceBoolV(ns, static_cast<const Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putColumnSliceuCharV(ns, static_cast<const Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putColumnSliceShortV(ns, static_cast<const Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      putColumnSliceuShortV(ns, static_cast<const Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putColumnSliceIntV(ns, static_cast<const Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      putColumnSliceuIntV(ns, static_cast<const Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putColumnSliceInt64V(ns, static_cast<const Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putColumnSlicefloatV(ns, static_cast<const Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      putColumnSlicedoubleV(ns, static_cast<const Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      putColumnSliceComplexV(ns, static_cast<const Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putColumnSliceDComplexV(ns, static_cast<const Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putColumnSliceStringV(ns, static_cast<const Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putColumnSlice"));
  }
}

// # Call the correct getColumnSliceCellsX function depending on the data type.
void StManColumn::getColumnSliceCellsV(const RefRows& rownrs, const Slicer& ns,
                                       ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      getColumnSliceCellsBoolV(rownrs, ns, static_cast<Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      getColumnSliceCellsuCharV(rownrs, ns, static_cast<Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      getColumnSliceCellsShortV(rownrs, ns, static_cast<Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      getColumnSliceCellsuShortV(rownrs, ns, static_cast<Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      getColumnSliceCellsIntV(rownrs, ns, static_cast<Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      getColumnSliceCellsuIntV(rownrs, ns, static_cast<Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      getColumnSliceCellsInt64V(rownrs, ns, static_cast<Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      getColumnSliceCellsfloatV(rownrs, ns, static_cast<Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      getColumnSliceCellsdoubleV(rownrs, ns, static_cast<Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      getColumnSliceCellsComplexV(rownrs, ns, static_cast<Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      getColumnSliceCellsDComplexV(rownrs, ns, static_cast<Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      getColumnSliceCellsStringV(rownrs, ns, static_cast<Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::getColumnSliceCells"));
  }
}

// # Call the correct putColumnSliceCellsX function depending on the data type.
void StManColumn::putColumnSliceCellsV(const RefRows& rownrs, const Slicer& ns,
                                       const ArrayBase& dataPtr) {
  switch (dtype()) {
    case TpBool:
      putColumnSliceCellsBoolV(rownrs, ns, static_cast<const Array<bool>*>(&dataPtr));
      break;
    case TpUChar:
      putColumnSliceCellsuCharV(rownrs, ns, static_cast<const Array<unsigned char>*>(&dataPtr));
      break;
    case TpShort:
      putColumnSliceCellsShortV(rownrs, ns, static_cast<const Array<short>*>(&dataPtr));
      break;
    case TpUShort:
      putColumnSliceCellsuShortV(rownrs, ns, static_cast<const Array<unsigned short>*>(&dataPtr));
      break;
    case TpInt:
      putColumnSliceCellsIntV(rownrs, ns, static_cast<const Array<int>*>(&dataPtr));
      break;
    case TpUInt:
      putColumnSliceCellsuIntV(rownrs, ns, static_cast<const Array<unsigned int>*>(&dataPtr));
      break;
    case TpInt64:
      putColumnSliceCellsInt64V(rownrs, ns, static_cast<const Array<Int64>*>(&dataPtr));
      break;
    case TpFloat:
      putColumnSliceCellsfloatV(rownrs, ns, static_cast<const Array<float>*>(&dataPtr));
      break;
    case TpDouble:
      putColumnSliceCellsdoubleV(rownrs, ns, static_cast<const Array<double>*>(&dataPtr));
      break;
    case TpComplex:
      putColumnSliceCellsComplexV(rownrs, ns, static_cast<const Array<Complex>*>(&dataPtr));
      break;
    case TpDComplex:
      putColumnSliceCellsDComplexV(rownrs, ns, static_cast<const Array<DComplex>*>(&dataPtr));
      break;
    case TpString:
      putColumnSliceCellsStringV(rownrs, ns, static_cast<const Array<String>*>(&dataPtr));
      break;
    default:
      throw(DataManInvDT("StManColumn::putColumnSliceCells"));
  }
}

void StManColumn::throwInvalidOp(const String& op) const {
  throw(DataManInvOper("StManColumn::" + op +
                       " not possible"
                       " for column " +
                       columnName()));
}

// # The default implementations use the DataManagerColumn counterparts.
#define STMANCOLUMN_GETPUT(T, NM)                                                                  \
  void StManColumn::aips_name2(getScalarColumn, NM)(Vector<T> * dataPtr) {                         \
    getScalarColumnBase(*dataPtr);                                                                 \
  }                                                                                                \
  void StManColumn::aips_name2(putScalarColumn, NM)(const Vector<T>* dataPtr) {                    \
    putScalarColumnBase(*dataPtr);                                                                 \
  }                                                                                                \
  void StManColumn::aips_name2(getArray, NM)(unsigned int, Array<T>*) { throwInvalidOp("getArray" #NM); }  \
  void StManColumn::aips_name2(putArray, NM)(unsigned int, const Array<T>*) {                              \
    throwInvalidOp("putArray" #NM);                                                                \
  }                                                                                                \
  void StManColumn::aips_name2(getSlice, NM)(unsigned int rownr, const Slicer& slicer, Array<T>* arr) {    \
    getSliceBase(rownr, slicer, *arr);                                                             \
  }                                                                                                \
  void StManColumn::aips_name2(putSlice, NM)(unsigned int rownr, const Slicer& slicer,                     \
                                             const Array<T>* arr) {                                \
    putSliceBase(rownr, slicer, *arr);                                                             \
  }                                                                                                \
  void StManColumn::aips_name2(getArrayColumn, NM)(Array<T> * arr) { getArrayColumnBase(*arr); }   \
  void StManColumn::aips_name2(putArrayColumn, NM)(const Array<T>* arr) {                          \
    putArrayColumnBase(*arr);                                                                      \
  }                                                                                                \
  void StManColumn::aips_name2(getColumnSlice, NM)(const Slicer& slicer, Array<T>* arr) {          \
    getColumnSliceBase(slicer, *arr);                                                              \
  }                                                                                                \
  void StManColumn::aips_name2(putColumnSlice, NM)(const Slicer& slicer, const Array<T>* arr) {    \
    putColumnSliceBase(slicer, *arr);                                                              \
  }                                                                                                \
  void StManColumn::aips_name2(getScalarColumnCells, NM)(const RefRows& rownrs,                    \
                                                         Vector<T>* values) {                      \
    getScalarColumnCellsBase(rownrs, *values);                                                     \
  }                                                                                                \
  void StManColumn::aips_name2(putScalarColumnCells, NM)(const RefRows& rownrs,                    \
                                                         const Vector<T>* values) {                \
    putScalarColumnCellsBase(rownrs, *values);                                                     \
  }                                                                                                \
  void StManColumn::aips_name2(getArrayColumnCells, NM)(const RefRows& rownrs, Array<T>* values) { \
    getArrayColumnCellsBase(rownrs, *values);                                                      \
  }                                                                                                \
  void StManColumn::aips_name2(putArrayColumnCells, NM)(const RefRows& rownrs,                     \
                                                        const Array<T>* values) {                  \
    putArrayColumnCellsBase(rownrs, *values);                                                      \
  }                                                                                                \
  void StManColumn::aips_name2(getColumnSliceCells, NM)(const RefRows& rownrs, const Slicer& ns,   \
                                                        Array<T>* values) {                        \
    getColumnSliceCellsBase(rownrs, ns, *values);                                                  \
  }                                                                                                \
  void StManColumn::aips_name2(putColumnSliceCells, NM)(const RefRows& rownrs, const Slicer& ns,   \
                                                        const Array<T>* values) {                  \
    putColumnSliceCellsBase(rownrs, ns, *values);                                                  \
  }

STMANCOLUMN_GETPUT(bool, BoolV)
STMANCOLUMN_GETPUT(unsigned char, uCharV)
STMANCOLUMN_GETPUT(short, ShortV)
STMANCOLUMN_GETPUT(unsigned short, uShortV)
STMANCOLUMN_GETPUT(int, IntV)
STMANCOLUMN_GETPUT(unsigned int, uIntV)
STMANCOLUMN_GETPUT(Int64, Int64V)
STMANCOLUMN_GETPUT(float, floatV)
STMANCOLUMN_GETPUT(double, doubleV)
STMANCOLUMN_GETPUT(Complex, ComplexV)
STMANCOLUMN_GETPUT(DComplex, DComplexV)

void StManColumn::getScalarColumnStringV(Vector<String>* dataPtr) { getScalarColumnBase(*dataPtr); }
void StManColumn::putScalarColumnStringV(const Vector<String>* dataPtr) {
  putScalarColumnBase(*dataPtr);
}
void StManColumn::getArrayStringV(unsigned int, Array<String>*) { throwInvalidOp("getArrayStringV"); }
void StManColumn::putArrayStringV(unsigned int, const Array<String>*) { throwInvalidOp("putArrayStringV"); }
void StManColumn::getSliceStringV(unsigned int rownr, const Slicer& slicer, Array<String>* arr) {
  getSliceBase(rownr, slicer, *arr);
}
void StManColumn::putSliceStringV(unsigned int rownr, const Slicer& slicer, const Array<String>* arr) {
  putSliceBase(rownr, slicer, *arr);
}
void StManColumn::getArrayColumnStringV(Array<String>* arr) { getArrayColumnBase(*arr); }
void StManColumn::putArrayColumnStringV(const Array<String>* arr) { putArrayColumnBase(*arr); }
void StManColumn::getColumnSliceStringV(const Slicer& slicer, Array<String>* arr) {
  getColumnSliceBase(slicer, *arr);
}
void StManColumn::putColumnSliceStringV(const Slicer& slicer, const Array<String>* arr) {
  putColumnSliceBase(slicer, *arr);
}
void StManColumn::getScalarColumnCellsStringV(const RefRows& rownrs, Vector<String>* values) {
  getScalarColumnCellsBase(rownrs, *values);
}
void StManColumn::putScalarColumnCellsStringV(const RefRows& rownrs, const Vector<String>* values) {
  putScalarColumnCellsBase(rownrs, *values);
}
void StManColumn::getArrayColumnCellsStringV(const RefRows& rownrs, Array<String>* values) {
  getArrayColumnCellsBase(rownrs, *values);
}
void StManColumn::putArrayColumnCellsStringV(const RefRows& rownrs, const Array<String>* values) {
  putArrayColumnCellsBase(rownrs, *values);
}
void StManColumn::getColumnSliceCellsStringV(const RefRows& rownrs, const Slicer& ns,
                                             Array<String>* values) {
  getColumnSliceCellsBase(rownrs, ns, *values);
}
void StManColumn::putColumnSliceCellsStringV(const RefRows& rownrs, const Slicer& ns,
                                             const Array<String>* values) {
  putColumnSliceCellsBase(rownrs, ns, *values);
}

}  // namespace casacore
