//# StManColumn.cc: Base storage manager column class
//# Copyright (C) 1994,1995,1996,1998,1999,2002
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


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StManColumn::StManColumn (int dataType)
  : dtype_p (static_cast<DataType>(dataType))
{
    elemSize_p = ValType::getTypeSize (dtype_p);
}

StManColumn::~StManColumn()
{}

int StManColumn::dataType() const
    { return dtype_p; }


Bool StManColumn::isNativeDataType (int dtype)
{
    switch (dtype) {
    case TpBool:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpFloat:
    case TpDouble:
    case TpComplex:
    case TpDComplex:
    case TpString:
    case TpArrayBool:
    case TpArrayUChar:
    case TpArrayShort:
    case TpArrayUShort:
    case TpArrayInt:
    case TpArrayUInt:
    case TpArrayFloat:
    case TpArrayDouble:
    case TpArrayComplex:
    case TpArrayDComplex:
    case TpArrayString:
	return True;
    }
    return False;
}


//# Call the correct getScalarColumnX function depending on the data type.
void StManColumn::getScalarColumnV (ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getScalarColumnBoolV (static_cast<Vector<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getScalarColumnuCharV (static_cast<Vector<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getScalarColumnShortV (static_cast<Vector<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getScalarColumnuShortV (static_cast<Vector<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getScalarColumnIntV (static_cast<Vector<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getScalarColumnuIntV (static_cast<Vector<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getScalarColumnfloatV (static_cast<Vector<float>*>(&dataPtr));
	break;
    case TpDouble:
	getScalarColumndoubleV (static_cast<Vector<double>*>(&dataPtr));
	break;
    case TpComplex:
	getScalarColumnComplexV (static_cast<Vector<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getScalarColumnDComplexV (static_cast<Vector<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getScalarColumnStringV (static_cast<Vector<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getScalarColumn"));
    }
}

//# Call the correct putScalarColumnX function depending on the data type.
void StManColumn::putScalarColumnV (const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putScalarColumnBoolV (static_cast<const Vector<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putScalarColumnuCharV (static_cast<const Vector<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putScalarColumnShortV (static_cast<const Vector<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putScalarColumnuShortV (static_cast<const Vector<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putScalarColumnIntV (static_cast<const Vector<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putScalarColumnuIntV (static_cast<const Vector<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putScalarColumnfloatV (static_cast<const Vector<float>*>(&dataPtr));
	break;
    case TpDouble:
	putScalarColumndoubleV (static_cast<const Vector<double>*>(&dataPtr));
	break;
    case TpComplex:
	putScalarColumnComplexV (static_cast<const Vector<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putScalarColumnDComplexV (static_cast<const Vector<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putScalarColumnStringV (static_cast<const Vector<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putScalarColumn"));
    }
}

//# Call the correct getScalarColumnCellsX function depending on the data type.
void StManColumn::getScalarColumnCellsV (const RefRows& rownrs,
					 ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getScalarColumnCellsBoolV (rownrs, static_cast<Vector<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getScalarColumnCellsuCharV (rownrs, static_cast<Vector<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getScalarColumnCellsShortV (rownrs, static_cast<Vector<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getScalarColumnCellsuShortV (rownrs, static_cast<Vector<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getScalarColumnCellsIntV (rownrs, static_cast<Vector<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getScalarColumnCellsuIntV (rownrs, static_cast<Vector<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getScalarColumnCellsfloatV (rownrs, static_cast<Vector<float>*>(&dataPtr));
	break;
    case TpDouble:
	getScalarColumnCellsdoubleV (rownrs, static_cast<Vector<double>*>(&dataPtr));
	break;
    case TpComplex:
	getScalarColumnCellsComplexV (rownrs, static_cast<Vector<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getScalarColumnCellsDComplexV (rownrs, static_cast<Vector<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getScalarColumnCellsStringV (rownrs, static_cast<Vector<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getScalarColumnCells"));
    }
}

//# Call the correct putScalarColumnCellsX function depending on the data type.
void StManColumn::putScalarColumnCellsV (const RefRows& rownrs,
					 const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putScalarColumnCellsBoolV (rownrs, static_cast<const Vector<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putScalarColumnCellsuCharV (rownrs, static_cast<const Vector<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putScalarColumnCellsShortV (rownrs, static_cast<const Vector<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putScalarColumnCellsuShortV (rownrs, static_cast<const Vector<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putScalarColumnCellsIntV (rownrs, static_cast<const Vector<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putScalarColumnCellsuIntV (rownrs, static_cast<const Vector<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putScalarColumnCellsfloatV (rownrs, static_cast<const Vector<float>*>(&dataPtr));
	break;
    case TpDouble:
	putScalarColumnCellsdoubleV (rownrs, static_cast<const Vector<double>*>(&dataPtr));
	break;
    case TpComplex:
	putScalarColumnCellsComplexV (rownrs, static_cast<const Vector<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putScalarColumnCellsDComplexV (rownrs, static_cast<const Vector<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putScalarColumnCellsStringV (rownrs, static_cast<const Vector<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putScalarColumnCells"));
    }
}


//# Call the correct getArrayX function depending on the data type.
void StManColumn::getArrayV (uInt rownr, ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getArrayBoolV (rownr, static_cast<Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getArrayuCharV (rownr, static_cast<Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getArrayShortV (rownr, static_cast<Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getArrayuShortV (rownr, static_cast<Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getArrayIntV (rownr, static_cast<Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getArrayuIntV (rownr, static_cast<Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getArrayfloatV (rownr, static_cast<Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	getArraydoubleV (rownr, static_cast<Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	getArrayComplexV (rownr, static_cast<Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getArrayDComplexV (rownr, static_cast<Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getArrayStringV (rownr, static_cast<Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getArray"));
    }
}

//# Call the correct putArrayX function depending on the data type.
void StManColumn::putArrayV (uInt rownr, const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putArrayBoolV (rownr, static_cast<const Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putArrayuCharV (rownr, static_cast<const Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putArrayShortV (rownr, static_cast<const Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putArrayuShortV (rownr, static_cast<const Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putArrayIntV (rownr, static_cast<const Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putArrayuIntV (rownr, static_cast<const Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putArrayfloatV (rownr, static_cast<const Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	putArraydoubleV (rownr, static_cast<const Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	putArrayComplexV (rownr, static_cast<const Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putArrayDComplexV (rownr, static_cast<const Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putArrayStringV (rownr, static_cast<const Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putArray"));
    }
}

//# Call the correct getColumnSliceX function depending on the data type.
void StManColumn::getSliceV (uInt rownr, const Slicer& ns, ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getSliceBoolV (rownr, ns, static_cast<Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getSliceuCharV (rownr, ns, static_cast<Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getSliceShortV (rownr, ns, static_cast<Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getSliceuShortV (rownr, ns, static_cast<Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getSliceIntV (rownr, ns, static_cast<Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getSliceuIntV (rownr, ns, static_cast<Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getSlicefloatV (rownr, ns, static_cast<Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	getSlicedoubleV (rownr, ns, static_cast<Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	getSliceComplexV (rownr, ns, static_cast<Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getSliceDComplexV (rownr, ns, static_cast<Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getSliceStringV (rownr, ns, static_cast<Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getSlice"));
    }
}

//# Call the correct putSliceX function depending on the data type.
void StManColumn::putSliceV (uInt rownr, const Slicer& ns, const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putSliceBoolV (rownr, ns, static_cast<const Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putSliceuCharV (rownr, ns, static_cast<const Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putSliceShortV (rownr, ns, static_cast<const Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putSliceuShortV (rownr, ns, static_cast<const Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putSliceIntV (rownr, ns, static_cast<const Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putSliceuIntV (rownr, ns, static_cast<const Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putSlicefloatV (rownr, ns, static_cast<const Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	putSlicedoubleV (rownr, ns, static_cast<const Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	putSliceComplexV (rownr, ns, static_cast<const Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putSliceDComplexV (rownr, ns, static_cast<const Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putSliceStringV (rownr, ns, static_cast<const Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putSlice"));
    }
}

//# Call the correct getArrayColumnX function depending on the data type.
void StManColumn::getArrayColumnV (ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getArrayColumnBoolV (static_cast<Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getArrayColumnuCharV (static_cast<Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getArrayColumnShortV (static_cast<Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getArrayColumnuShortV (static_cast<Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getArrayColumnIntV (static_cast<Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getArrayColumnuIntV (static_cast<Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getArrayColumnfloatV (static_cast<Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	getArrayColumndoubleV (static_cast<Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	getArrayColumnComplexV (static_cast<Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getArrayColumnDComplexV (static_cast<Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getArrayColumnStringV (static_cast<Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getArrayColumn"));
    }
}

//# Call the correct putArrayColumnX function depending on the data type.
void StManColumn::putArrayColumnV (const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putArrayColumnBoolV (static_cast<const Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putArrayColumnuCharV (static_cast<const Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putArrayColumnShortV (static_cast<const Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putArrayColumnuShortV (static_cast<const Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putArrayColumnIntV (static_cast<const Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putArrayColumnuIntV (static_cast<const Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putArrayColumnfloatV (static_cast<const Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	putArrayColumndoubleV (static_cast<const Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	putArrayColumnComplexV (static_cast<const Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putArrayColumnDComplexV (static_cast<const Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putArrayColumnStringV (static_cast<const Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putArrayColumn"));
    }
}

//# Call the correct getArrayColumnCellsX function depending on the data type.
void StManColumn::getArrayColumnCellsV (const RefRows& rownrs,
					ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getArrayColumnCellsBoolV (rownrs, static_cast<Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getArrayColumnCellsuCharV (rownrs, static_cast<Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getArrayColumnCellsShortV (rownrs, static_cast<Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getArrayColumnCellsuShortV (rownrs, static_cast<Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getArrayColumnCellsIntV (rownrs, static_cast<Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getArrayColumnCellsuIntV (rownrs, static_cast<Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getArrayColumnCellsfloatV (rownrs, static_cast<Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	getArrayColumnCellsdoubleV (rownrs, static_cast<Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	getArrayColumnCellsComplexV (rownrs, static_cast<Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getArrayColumnCellsDComplexV (rownrs, static_cast<Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getArrayColumnCellsStringV (rownrs, static_cast<Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getArrayColumnCells"));
    }
}

//# Call the correct putArrayColumnCellsX function depending on the data type.
void StManColumn::putArrayColumnCellsV (const RefRows& rownrs,
					const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putArrayColumnCellsBoolV (rownrs, static_cast<const Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putArrayColumnCellsuCharV (rownrs, static_cast<const Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putArrayColumnCellsShortV (rownrs, static_cast<const Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putArrayColumnCellsuShortV (rownrs, static_cast<const Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putArrayColumnCellsIntV (rownrs, static_cast<const Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putArrayColumnCellsuIntV (rownrs, static_cast<const Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putArrayColumnCellsfloatV (rownrs, static_cast<const Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	putArrayColumnCellsdoubleV (rownrs, static_cast<const Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	putArrayColumnCellsComplexV (rownrs, static_cast<const Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putArrayColumnCellsDComplexV (rownrs, static_cast<const Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putArrayColumnCellsStringV (rownrs, static_cast<const Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putArrayColumnCells"));
    }
}

//# Call the correct getColumnSliceX function depending on the data type.
void StManColumn::getColumnSliceV (const Slicer& ns, ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getColumnSliceBoolV (ns, static_cast<Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getColumnSliceuCharV (ns, static_cast<Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getColumnSliceShortV (ns, static_cast<Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getColumnSliceuShortV (ns, static_cast<Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getColumnSliceIntV (ns, static_cast<Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getColumnSliceuIntV (ns, static_cast<Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getColumnSlicefloatV (ns, static_cast<Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	getColumnSlicedoubleV (ns, static_cast<Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	getColumnSliceComplexV (ns, static_cast<Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getColumnSliceDComplexV (ns, static_cast<Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getColumnSliceStringV (ns, static_cast<Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getColumnSlice"));
    }
}

//# Call the correct putColumnSliceX function depending on the data type.
void StManColumn::putColumnSliceV (const Slicer& ns, const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putColumnSliceBoolV (ns, static_cast<const Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putColumnSliceuCharV (ns, static_cast<const Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putColumnSliceShortV (ns, static_cast<const Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putColumnSliceuShortV (ns, static_cast<const Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putColumnSliceIntV (ns, static_cast<const Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putColumnSliceuIntV (ns, static_cast<const Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putColumnSlicefloatV (ns, static_cast<const Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	putColumnSlicedoubleV (ns, static_cast<const Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	putColumnSliceComplexV (ns, static_cast<const Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putColumnSliceDComplexV (ns, static_cast<const Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putColumnSliceStringV (ns, static_cast<const Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putColumnSlice"));
    }
}

//# Call the correct getColumnSliceCellsX function depending on the data type.
void StManColumn::getColumnSliceCellsV (const RefRows& rownrs,
					const Slicer& ns, ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getColumnSliceCellsBoolV (rownrs, ns, static_cast<Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	getColumnSliceCellsuCharV (rownrs, ns, static_cast<Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	getColumnSliceCellsShortV (rownrs, ns, static_cast<Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	getColumnSliceCellsuShortV (rownrs, ns, static_cast<Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	getColumnSliceCellsIntV (rownrs, ns, static_cast<Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	getColumnSliceCellsuIntV (rownrs, ns, static_cast<Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	getColumnSliceCellsfloatV (rownrs, ns, static_cast<Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	getColumnSliceCellsdoubleV (rownrs, ns, static_cast<Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	getColumnSliceCellsComplexV (rownrs, ns, static_cast<Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	getColumnSliceCellsDComplexV (rownrs, ns, static_cast<Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	getColumnSliceCellsStringV (rownrs, ns, static_cast<Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::getColumnSliceCells"));
    }
}

//# Call the correct putColumnSliceCellsX function depending on the data type.
void StManColumn::putColumnSliceCellsV (const RefRows& rownrs,
					const Slicer& ns, const ArrayBase& dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putColumnSliceCellsBoolV (rownrs, ns, static_cast<const Array<Bool>*>(&dataPtr));
	break;
    case TpUChar:
	putColumnSliceCellsuCharV (rownrs, ns, static_cast<const Array<uChar>*>(&dataPtr));
	break;
    case TpShort:
	putColumnSliceCellsShortV (rownrs, ns, static_cast<const Array<Short>*>(&dataPtr));
	break;
    case TpUShort:
	putColumnSliceCellsuShortV (rownrs, ns, static_cast<const Array<uShort>*>(&dataPtr));
	break;
    case TpInt:
	putColumnSliceCellsIntV (rownrs, ns, static_cast<const Array<Int>*>(&dataPtr));
	break;
    case TpUInt:
	putColumnSliceCellsuIntV (rownrs, ns, static_cast<const Array<uInt>*>(&dataPtr));
	break;
    case TpFloat:
	putColumnSliceCellsfloatV (rownrs, ns, static_cast<const Array<float>*>(&dataPtr));
	break;
    case TpDouble:
	putColumnSliceCellsdoubleV (rownrs, ns, static_cast<const Array<double>*>(&dataPtr));
	break;
    case TpComplex:
	putColumnSliceCellsComplexV (rownrs, ns, static_cast<const Array<Complex>*>(&dataPtr));
	break;
    case TpDComplex:
	putColumnSliceCellsDComplexV (rownrs, ns, static_cast<const Array<DComplex>*>(&dataPtr));
	break;
    case TpString:
	putColumnSliceCellsStringV (rownrs, ns, static_cast<const Array<String>*>(&dataPtr));
	break;
    default:
	throw (DataManInvDT ("StManColumn::putColumnSliceCells"));
    }
}


void StManColumn::throwGetArray() const
    { throw (DataManInvOper ("StManColumn::getArray not possible"
                             " for column " + columnName())); }
void StManColumn::throwPutArray() const
    { throw (DataManInvOper ("StManColumn::putArray not possible"
                             " for column " + columnName())); }


//# The default implementations use the DataManagerColumn counterparts.
#define STMANCOLUMN_GETPUT(T,NM) \
void StManColumn::aips_name2(getScalarColumn,NM) (Vector<T>* dataPtr) \
    { dmGetScalarColumnV (*dataPtr); } \
void StManColumn::aips_name2(putScalarColumn,NM) (const Vector<T>* dataPtr) \
    { dmPutScalarColumnV (*dataPtr); } \
void StManColumn::aips_name2(getArray,NM) (uInt, Array<T>*) \
    { throwGetArray(); } \
void StManColumn::aips_name2(putArray,NM) (uInt, const Array<T>*) \
    { throwPutArray(); } \
void StManColumn::aips_name2(getSlice,NM) (uInt rownr, const Slicer& slicer, \
                                           Array<T>* arr) \
    { dmGetSliceV (rownr, slicer, *arr); } \
void StManColumn::aips_name2(putSlice,NM) (uInt rownr, const Slicer& slicer, \
                                           const Array<T>* arr) \
    { dmPutSliceV (rownr, slicer, *arr); } \
void StManColumn::aips_name2(getArrayColumn,NM) (Array<T>* arr) \
    { dmGetArrayColumnV (*arr); } \
void StManColumn::aips_name2(putArrayColumn,NM) (const Array<T>* arr) \
    { dmPutArrayColumnV (*arr); } \
void StManColumn::aips_name2(getColumnSlice,NM) (const Slicer& slicer, \
                                                 Array<T>* arr) \
    { dmGetColumnSliceV (slicer, *arr); } \
void StManColumn::aips_name2(putColumnSlice,NM) (const Slicer& slicer, \
                                                 const Array<T>* arr) \
    { dmPutColumnSliceV (slicer, *arr); } \
void StManColumn::aips_name2(getScalarColumnCells,NM) \
                                             (const RefRows& rownrs, \
					      Vector<T>* values) \
    { dmGetScalarColumnCellsV (rownrs, *values); } \
void StManColumn::aips_name2(putScalarColumnCells,NM) \
                                             (const RefRows& rownrs, \
					      const Vector<T>* values) \
    { dmPutScalarColumnCellsV (rownrs, *values); } \
void StManColumn::aips_name2(getArrayColumnCells,NM) \
                                            (const RefRows& rownrs, \
					     Array<T>* values) \
    { dmGetArrayColumnCellsV (rownrs, *values); } \
void StManColumn::aips_name2(putArrayColumnCells,NM) \
                                            (const RefRows& rownrs, \
					     const Array<T>* values) \
    { dmPutArrayColumnCellsV (rownrs, *values); } \
void StManColumn::aips_name2(getColumnSliceCells,NM) \
                                            (const RefRows& rownrs, \
					     const Slicer& ns, \
					     Array<T>* values) \
    { dmGetColumnSliceCellsV (rownrs, ns, *values); } \
void StManColumn::aips_name2(putColumnSliceCells,NM) \
                                            (const RefRows& rownrs, \
					     const Slicer& ns, \
					     const Array<T>* values) \
    { dmPutColumnSliceCellsV (rownrs, ns, *values); } \

STMANCOLUMN_GETPUT(Bool,BoolV)
STMANCOLUMN_GETPUT(uChar,uCharV)
STMANCOLUMN_GETPUT(Short,ShortV)
STMANCOLUMN_GETPUT(uShort,uShortV)
STMANCOLUMN_GETPUT(Int,IntV)
STMANCOLUMN_GETPUT(uInt,uIntV)
STMANCOLUMN_GETPUT(float,floatV)
STMANCOLUMN_GETPUT(double,doubleV)
STMANCOLUMN_GETPUT(Complex,ComplexV)
STMANCOLUMN_GETPUT(DComplex,DComplexV)
STMANCOLUMN_GETPUT(String,StringV)

/*
    Vector<T> value = *values; \
    const ColumnCache* cachePtr = columnCachePtr(); \
    uInt nr = rownrs.nelements(); \
Timer timer; \
    for (uInt i=0; i<nr; i++) { \
	uInt rownr = rownrs(i); \
	Int off = cachePtr->offset(rownr); \
	if (off >= 0) { \
	    value(i) = ((T*)(cachePtr->dataPtr()))[off]; \
	} else { \
	    aips_name2(get,NM) (rownr, &(value(i))); \
	} \
    } \
timer.show("a"); \
*/

} //# NAMESPACE CASACORE - END
