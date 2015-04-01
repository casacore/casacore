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
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StManColumn::StManColumn (int dataType)
: dtype_p (dataType)
{}

StManColumn::~StManColumn()
{}

int StManColumn::dataType() const
    { return dtype_p; }


//# Accessing an entire column is by default possible for scalars.
Bool StManColumn::canAccessScalarColumn (Bool& reask) const
{
    reask = False;
    return True;
}

//# Accessing column cells is possible for scalars and arrays.
Bool StManColumn::canAccessScalarColumnCells (Bool& reask) const
{
    reask = False;
    return True;
}
Bool StManColumn::canAccessArrayColumnCells (Bool& reask) const
{
    reask = False;
    return True;
}


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
void StManColumn::getScalarColumnV (void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getScalarColumnBoolV ((Vector<Bool>*)dataPtr);
	break;
    case TpUChar:
	getScalarColumnuCharV ((Vector<uChar>*)dataPtr);
	break;
    case TpShort:
	getScalarColumnShortV ((Vector<Short>*)dataPtr);
	break;
    case TpUShort:
	getScalarColumnuShortV ((Vector<uShort>*)dataPtr);
	break;
    case TpInt:
	getScalarColumnIntV ((Vector<Int>*)dataPtr);
	break;
    case TpUInt:
	getScalarColumnuIntV ((Vector<uInt>*)dataPtr);
	break;
    case TpFloat:
	getScalarColumnfloatV ((Vector<float>*)dataPtr);
	break;
    case TpDouble:
	getScalarColumndoubleV ((Vector<double>*)dataPtr);
	break;
    case TpComplex:
	getScalarColumnComplexV ((Vector<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getScalarColumnDComplexV ((Vector<DComplex>*)dataPtr);
	break;
    case TpString:
	getScalarColumnStringV ((Vector<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getScalarColumn"));
    }
}

//# Call the correct putScalarColumnX function depending on the data type.
void StManColumn::putScalarColumnV (const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putScalarColumnBoolV ((const Vector<Bool>*)dataPtr);
	break;
    case TpUChar:
	putScalarColumnuCharV ((const Vector<uChar>*)dataPtr);
	break;
    case TpShort:
	putScalarColumnShortV ((const Vector<Short>*)dataPtr);
	break;
    case TpUShort:
	putScalarColumnuShortV ((const Vector<uShort>*)dataPtr);
	break;
    case TpInt:
	putScalarColumnIntV ((const Vector<Int>*)dataPtr);
	break;
    case TpUInt:
	putScalarColumnuIntV ((const Vector<uInt>*)dataPtr);
	break;
    case TpFloat:
	putScalarColumnfloatV ((const Vector<float>*)dataPtr);
	break;
    case TpDouble:
	putScalarColumndoubleV ((const Vector<double>*)dataPtr);
	break;
    case TpComplex:
	putScalarColumnComplexV ((const Vector<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putScalarColumnDComplexV ((const Vector<DComplex>*)dataPtr);
	break;
    case TpString:
	putScalarColumnStringV ((const Vector<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putScalarColumn"));
    }
}

//# Call the correct getScalarColumnCellsX function depending on the data type.
void StManColumn::getScalarColumnCellsV (const RefRows& rownrs,
					 void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getScalarColumnCellsBoolV (rownrs, (Vector<Bool>*)dataPtr);
	break;
    case TpUChar:
	getScalarColumnCellsuCharV (rownrs, (Vector<uChar>*)dataPtr);
	break;
    case TpShort:
	getScalarColumnCellsShortV (rownrs, (Vector<Short>*)dataPtr);
	break;
    case TpUShort:
	getScalarColumnCellsuShortV (rownrs, (Vector<uShort>*)dataPtr);
	break;
    case TpInt:
	getScalarColumnCellsIntV (rownrs, (Vector<Int>*)dataPtr);
	break;
    case TpUInt:
	getScalarColumnCellsuIntV (rownrs, (Vector<uInt>*)dataPtr);
	break;
    case TpFloat:
	getScalarColumnCellsfloatV (rownrs, (Vector<float>*)dataPtr);
	break;
    case TpDouble:
	getScalarColumnCellsdoubleV (rownrs, (Vector<double>*)dataPtr);
	break;
    case TpComplex:
	getScalarColumnCellsComplexV (rownrs, (Vector<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getScalarColumnCellsDComplexV (rownrs, (Vector<DComplex>*)dataPtr);
	break;
    case TpString:
	getScalarColumnCellsStringV (rownrs, (Vector<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getScalarColumnCells"));
    }
}

//# Call the correct putScalarColumnCellsX function depending on the data type.
void StManColumn::putScalarColumnCellsV (const RefRows& rownrs,
					 const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putScalarColumnCellsBoolV (rownrs, (const Vector<Bool>*)dataPtr);
	break;
    case TpUChar:
	putScalarColumnCellsuCharV (rownrs, (const Vector<uChar>*)dataPtr);
	break;
    case TpShort:
	putScalarColumnCellsShortV (rownrs, (const Vector<Short>*)dataPtr);
	break;
    case TpUShort:
	putScalarColumnCellsuShortV (rownrs, (const Vector<uShort>*)dataPtr);
	break;
    case TpInt:
	putScalarColumnCellsIntV (rownrs, (const Vector<Int>*)dataPtr);
	break;
    case TpUInt:
	putScalarColumnCellsuIntV (rownrs, (const Vector<uInt>*)dataPtr);
	break;
    case TpFloat:
	putScalarColumnCellsfloatV (rownrs, (const Vector<float>*)dataPtr);
	break;
    case TpDouble:
	putScalarColumnCellsdoubleV (rownrs, (const Vector<double>*)dataPtr);
	break;
    case TpComplex:
	putScalarColumnCellsComplexV (rownrs, (const Vector<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putScalarColumnCellsDComplexV (rownrs, (const Vector<DComplex>*)dataPtr);
	break;
    case TpString:
	putScalarColumnCellsStringV (rownrs, (const Vector<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putScalarColumnCells"));
    }
}

//# Call the correct getBlockX function depending on the data type.
uInt StManColumn::getBlockV (uInt rownr, uInt nrmax, void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	return getBlockBoolV (rownr, nrmax, (Bool*)dataPtr);
    case TpUChar:
	return getBlockuCharV (rownr, nrmax, (uChar*)dataPtr);
    case TpShort:
	return getBlockShortV (rownr, nrmax, (Short*)dataPtr);
    case TpUShort:
	return getBlockuShortV (rownr, nrmax, (uShort*)dataPtr);
    case TpInt:
	return getBlockIntV (rownr, nrmax, (Int*)dataPtr);
    case TpUInt:
	return getBlockuIntV (rownr, nrmax, (uInt*)dataPtr);
    case TpFloat:
	return getBlockfloatV (rownr, nrmax, (float*)dataPtr);
    case TpDouble:
	return getBlockdoubleV (rownr, nrmax, (double*)dataPtr);
    case TpComplex:
	return getBlockComplexV (rownr, nrmax, (Complex*)dataPtr);
    case TpDComplex:
	return getBlockDComplexV (rownr, nrmax, (DComplex*)dataPtr);
    case TpString:
	return getBlockStringV (rownr, nrmax, (String*)dataPtr);
    default:
	throw (DataManInvDT ("StManColumn::getBlock"));
    }
    // NOTREACHED - to shut up a warning message
    return 0;
}

//# Call the correct putBlockX function depending on the data type.
void StManColumn::putBlockV (uInt rownr, uInt nrmax, const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putBlockBoolV (rownr, nrmax, (const Bool*)dataPtr);
	break;
    case TpUChar:
	putBlockuCharV (rownr, nrmax, (const uChar*)dataPtr);
	break;
    case TpShort:
	putBlockShortV (rownr, nrmax, (const Short*)dataPtr);
	break;
    case TpUShort:
	putBlockuShortV (rownr, nrmax, (const uShort*)dataPtr);
	break;
    case TpInt:
	putBlockIntV (rownr, nrmax, (const Int*)dataPtr);
	break;
    case TpUInt:
	putBlockuIntV (rownr, nrmax, (const uInt*)dataPtr);
	break;
    case TpFloat:
	putBlockfloatV (rownr, nrmax, (const float*)dataPtr);
	break;
    case TpDouble:
	putBlockdoubleV (rownr, nrmax, (const double*)dataPtr);
	break;
    case TpComplex:
	putBlockComplexV (rownr, nrmax, (const Complex*)dataPtr);
	break;
    case TpDComplex:
	putBlockDComplexV (rownr, nrmax, (const DComplex*)dataPtr);
	break;
    case TpString:
	putBlockStringV (rownr, nrmax, (const String*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putBlock"));
    }
}

//# Call the correct getArrayX function depending on the data type.
void StManColumn::getArrayV (uInt rownr, void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getArrayBoolV (rownr, (Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	getArrayuCharV (rownr, (Array<uChar>*)dataPtr);
	break;
    case TpShort:
	getArrayShortV (rownr, (Array<Short>*)dataPtr);
	break;
    case TpUShort:
	getArrayuShortV (rownr, (Array<uShort>*)dataPtr);
	break;
    case TpInt:
	getArrayIntV (rownr, (Array<Int>*)dataPtr);
	break;
    case TpUInt:
	getArrayuIntV (rownr, (Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	getArrayfloatV (rownr, (Array<float>*)dataPtr);
	break;
    case TpDouble:
	getArraydoubleV (rownr, (Array<double>*)dataPtr);
	break;
    case TpComplex:
	getArrayComplexV (rownr, (Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getArrayDComplexV (rownr, (Array<DComplex>*)dataPtr);
	break;
    case TpString:
	getArrayStringV (rownr, (Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getArray"));
    }
}

//# Call the correct putArrayX function depending on the data type.
void StManColumn::putArrayV (uInt rownr, const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putArrayBoolV (rownr, (const Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	putArrayuCharV (rownr, (const Array<uChar>*)dataPtr);
	break;
    case TpShort:
	putArrayShortV (rownr, (const Array<Short>*)dataPtr);
	break;
    case TpUShort:
	putArrayuShortV (rownr, (const Array<uShort>*)dataPtr);
	break;
    case TpInt:
	putArrayIntV (rownr, (const Array<Int>*)dataPtr);
	break;
    case TpUInt:
	putArrayuIntV (rownr, (const Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	putArrayfloatV (rownr, (const Array<float>*)dataPtr);
	break;
    case TpDouble:
	putArraydoubleV (rownr, (const Array<double>*)dataPtr);
	break;
    case TpComplex:
	putArrayComplexV (rownr, (const Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putArrayDComplexV (rownr, (const Array<DComplex>*)dataPtr);
	break;
    case TpString:
	putArrayStringV (rownr, (const Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putArray"));
    }
}

//# Call the correct getColumnSliceX function depending on the data type.
void StManColumn::getSliceV (uInt rownr, const Slicer& ns, void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getSliceBoolV (rownr, ns, (Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	getSliceuCharV (rownr, ns, (Array<uChar>*)dataPtr);
	break;
    case TpShort:
	getSliceShortV (rownr, ns, (Array<Short>*)dataPtr);
	break;
    case TpUShort:
	getSliceuShortV (rownr, ns, (Array<uShort>*)dataPtr);
	break;
    case TpInt:
	getSliceIntV (rownr, ns, (Array<Int>*)dataPtr);
	break;
    case TpUInt:
	getSliceuIntV (rownr, ns, (Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	getSlicefloatV (rownr, ns, (Array<float>*)dataPtr);
	break;
    case TpDouble:
	getSlicedoubleV (rownr, ns, (Array<double>*)dataPtr);
	break;
    case TpComplex:
	getSliceComplexV (rownr, ns, (Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getSliceDComplexV (rownr, ns, (Array<DComplex>*)dataPtr);
	break;
    case TpString:
	getSliceStringV (rownr, ns, (Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getSlice"));
    }
}

//# Call the correct putSliceX function depending on the data type.
void StManColumn::putSliceV (uInt rownr, const Slicer& ns, const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putSliceBoolV (rownr, ns, (const Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	putSliceuCharV (rownr, ns, (const Array<uChar>*)dataPtr);
	break;
    case TpShort:
	putSliceShortV (rownr, ns, (const Array<Short>*)dataPtr);
	break;
    case TpUShort:
	putSliceuShortV (rownr, ns, (const Array<uShort>*)dataPtr);
	break;
    case TpInt:
	putSliceIntV (rownr, ns, (const Array<Int>*)dataPtr);
	break;
    case TpUInt:
	putSliceuIntV (rownr, ns, (const Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	putSlicefloatV (rownr, ns, (const Array<float>*)dataPtr);
	break;
    case TpDouble:
	putSlicedoubleV (rownr, ns, (const Array<double>*)dataPtr);
	break;
    case TpComplex:
	putSliceComplexV (rownr, ns, (const Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putSliceDComplexV (rownr, ns, (const Array<DComplex>*)dataPtr);
	break;
    case TpString:
	putSliceStringV (rownr, ns, (const Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putSlice"));
    }
}

//# Call the correct getArrayColumnX function depending on the data type.
void StManColumn::getArrayColumnV (void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getArrayColumnBoolV ((Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	getArrayColumnuCharV ((Array<uChar>*)dataPtr);
	break;
    case TpShort:
	getArrayColumnShortV ((Array<Short>*)dataPtr);
	break;
    case TpUShort:
	getArrayColumnuShortV ((Array<uShort>*)dataPtr);
	break;
    case TpInt:
	getArrayColumnIntV ((Array<Int>*)dataPtr);
	break;
    case TpUInt:
	getArrayColumnuIntV ((Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	getArrayColumnfloatV ((Array<float>*)dataPtr);
	break;
    case TpDouble:
	getArrayColumndoubleV ((Array<double>*)dataPtr);
	break;
    case TpComplex:
	getArrayColumnComplexV ((Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getArrayColumnDComplexV ((Array<DComplex>*)dataPtr);
	break;
    case TpString:
	getArrayColumnStringV ((Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getArrayColumn"));
    }
}

//# Call the correct putArrayColumnX function depending on the data type.
void StManColumn::putArrayColumnV (const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putArrayColumnBoolV ((const Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	putArrayColumnuCharV ((const Array<uChar>*)dataPtr);
	break;
    case TpShort:
	putArrayColumnShortV ((const Array<Short>*)dataPtr);
	break;
    case TpUShort:
	putArrayColumnuShortV ((const Array<uShort>*)dataPtr);
	break;
    case TpInt:
	putArrayColumnIntV ((const Array<Int>*)dataPtr);
	break;
    case TpUInt:
	putArrayColumnuIntV ((const Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	putArrayColumnfloatV ((const Array<float>*)dataPtr);
	break;
    case TpDouble:
	putArrayColumndoubleV ((const Array<double>*)dataPtr);
	break;
    case TpComplex:
	putArrayColumnComplexV ((const Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putArrayColumnDComplexV ((const Array<DComplex>*)dataPtr);
	break;
    case TpString:
	putArrayColumnStringV ((const Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putArrayColumn"));
    }
}

//# Call the correct getArrayColumnCellsX function depending on the data type.
void StManColumn::getArrayColumnCellsV (const RefRows& rownrs,
					void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getArrayColumnCellsBoolV (rownrs, (Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	getArrayColumnCellsuCharV (rownrs, (Array<uChar>*)dataPtr);
	break;
    case TpShort:
	getArrayColumnCellsShortV (rownrs, (Array<Short>*)dataPtr);
	break;
    case TpUShort:
	getArrayColumnCellsuShortV (rownrs, (Array<uShort>*)dataPtr);
	break;
    case TpInt:
	getArrayColumnCellsIntV (rownrs, (Array<Int>*)dataPtr);
	break;
    case TpUInt:
	getArrayColumnCellsuIntV (rownrs, (Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	getArrayColumnCellsfloatV (rownrs, (Array<float>*)dataPtr);
	break;
    case TpDouble:
	getArrayColumnCellsdoubleV (rownrs, (Array<double>*)dataPtr);
	break;
    case TpComplex:
	getArrayColumnCellsComplexV (rownrs, (Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getArrayColumnCellsDComplexV (rownrs, (Array<DComplex>*)dataPtr);
	break;
    case TpString:
	getArrayColumnCellsStringV (rownrs, (Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getArrayColumnCells"));
    }
}

//# Call the correct putArrayColumnCellsX function depending on the data type.
void StManColumn::putArrayColumnCellsV (const RefRows& rownrs,
					const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putArrayColumnCellsBoolV (rownrs, (const Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	putArrayColumnCellsuCharV (rownrs, (const Array<uChar>*)dataPtr);
	break;
    case TpShort:
	putArrayColumnCellsShortV (rownrs, (const Array<Short>*)dataPtr);
	break;
    case TpUShort:
	putArrayColumnCellsuShortV (rownrs, (const Array<uShort>*)dataPtr);
	break;
    case TpInt:
	putArrayColumnCellsIntV (rownrs, (const Array<Int>*)dataPtr);
	break;
    case TpUInt:
	putArrayColumnCellsuIntV (rownrs, (const Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	putArrayColumnCellsfloatV (rownrs, (const Array<float>*)dataPtr);
	break;
    case TpDouble:
	putArrayColumnCellsdoubleV (rownrs, (const Array<double>*)dataPtr);
	break;
    case TpComplex:
	putArrayColumnCellsComplexV (rownrs, (const Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putArrayColumnCellsDComplexV (rownrs, (const Array<DComplex>*)dataPtr);
	break;
    case TpString:
	putArrayColumnCellsStringV (rownrs, (const Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putArrayColumnCells"));
    }
}

//# Call the correct getColumnSliceX function depending on the data type.
void StManColumn::getColumnSliceV (const Slicer& ns, void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getColumnSliceBoolV (ns, (Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	getColumnSliceuCharV (ns, (Array<uChar>*)dataPtr);
	break;
    case TpShort:
	getColumnSliceShortV (ns, (Array<Short>*)dataPtr);
	break;
    case TpUShort:
	getColumnSliceuShortV (ns, (Array<uShort>*)dataPtr);
	break;
    case TpInt:
	getColumnSliceIntV (ns, (Array<Int>*)dataPtr);
	break;
    case TpUInt:
	getColumnSliceuIntV (ns, (Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	getColumnSlicefloatV (ns, (Array<float>*)dataPtr);
	break;
    case TpDouble:
	getColumnSlicedoubleV (ns, (Array<double>*)dataPtr);
	break;
    case TpComplex:
	getColumnSliceComplexV (ns, (Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getColumnSliceDComplexV (ns, (Array<DComplex>*)dataPtr);
	break;
    case TpString:
	getColumnSliceStringV (ns, (Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getColumnSlice"));
    }
}

//# Call the correct putColumnSliceX function depending on the data type.
void StManColumn::putColumnSliceV (const Slicer& ns, const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putColumnSliceBoolV (ns, (const Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	putColumnSliceuCharV (ns, (const Array<uChar>*)dataPtr);
	break;
    case TpShort:
	putColumnSliceShortV (ns, (const Array<Short>*)dataPtr);
	break;
    case TpUShort:
	putColumnSliceuShortV (ns, (const Array<uShort>*)dataPtr);
	break;
    case TpInt:
	putColumnSliceIntV (ns, (const Array<Int>*)dataPtr);
	break;
    case TpUInt:
	putColumnSliceuIntV (ns, (const Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	putColumnSlicefloatV (ns, (const Array<float>*)dataPtr);
	break;
    case TpDouble:
	putColumnSlicedoubleV (ns, (const Array<double>*)dataPtr);
	break;
    case TpComplex:
	putColumnSliceComplexV (ns, (const Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putColumnSliceDComplexV (ns, (const Array<DComplex>*)dataPtr);
	break;
    case TpString:
	putColumnSliceStringV (ns, (const Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putColumnSlice"));
    }
}

//# Call the correct getColumnSliceCellsX function depending on the data type.
void StManColumn::getColumnSliceCellsV (const RefRows& rownrs,
					const Slicer& ns, void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	getColumnSliceCellsBoolV (rownrs, ns, (Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	getColumnSliceCellsuCharV (rownrs, ns, (Array<uChar>*)dataPtr);
	break;
    case TpShort:
	getColumnSliceCellsShortV (rownrs, ns, (Array<Short>*)dataPtr);
	break;
    case TpUShort:
	getColumnSliceCellsuShortV (rownrs, ns, (Array<uShort>*)dataPtr);
	break;
    case TpInt:
	getColumnSliceCellsIntV (rownrs, ns, (Array<Int>*)dataPtr);
	break;
    case TpUInt:
	getColumnSliceCellsuIntV (rownrs, ns, (Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	getColumnSliceCellsfloatV (rownrs, ns, (Array<float>*)dataPtr);
	break;
    case TpDouble:
	getColumnSliceCellsdoubleV (rownrs, ns, (Array<double>*)dataPtr);
	break;
    case TpComplex:
	getColumnSliceCellsComplexV (rownrs, ns, (Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	getColumnSliceCellsDComplexV (rownrs, ns, (Array<DComplex>*)dataPtr);
	break;
    case TpString:
	getColumnSliceCellsStringV (rownrs, ns, (Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::getColumnSliceCells"));
    }
}

//# Call the correct putColumnSliceCellsX function depending on the data type.
void StManColumn::putColumnSliceCellsV (const RefRows& rownrs,
					const Slicer& ns, const void* dataPtr)
{
    switch (dtype_p) {
    case TpBool:
	putColumnSliceCellsBoolV (rownrs, ns, (const Array<Bool>*)dataPtr);
	break;
    case TpUChar:
	putColumnSliceCellsuCharV (rownrs, ns, (const Array<uChar>*)dataPtr);
	break;
    case TpShort:
	putColumnSliceCellsShortV (rownrs, ns, (const Array<Short>*)dataPtr);
	break;
    case TpUShort:
	putColumnSliceCellsuShortV (rownrs, ns, (const Array<uShort>*)dataPtr);
	break;
    case TpInt:
	putColumnSliceCellsIntV (rownrs, ns, (const Array<Int>*)dataPtr);
	break;
    case TpUInt:
	putColumnSliceCellsuIntV (rownrs, ns, (const Array<uInt>*)dataPtr);
	break;
    case TpFloat:
	putColumnSliceCellsfloatV (rownrs, ns, (const Array<float>*)dataPtr);
	break;
    case TpDouble:
	putColumnSliceCellsdoubleV (rownrs, ns, (const Array<double>*)dataPtr);
	break;
    case TpComplex:
	putColumnSliceCellsComplexV (rownrs, ns,
				     (const Array<Complex>*)dataPtr);
	break;
    case TpDComplex:
	putColumnSliceCellsDComplexV (rownrs, ns,
				      (const Array<DComplex>*)dataPtr);
	break;
    case TpString:
	putColumnSliceCellsStringV (rownrs, ns, (const Array<String>*)dataPtr);
	break;
    default:
	throw (DataManInvDT ("StManColumn::putColumnSliceCells"));
    }
}


void StManColumn::throwGetArray() const
    { throw (DataManInvOper ("StManColumn::getArray not possible")); }
void StManColumn::throwPutArray() const
    { throw (DataManInvOper ("StManColumn::putArray not possible")); }


//# For scalars the default implementation of get/putScalarColumn handles
//# its data using get/putBlock. For arrays it throws an exception.
//# The default implementation of getBlock gets one value.
//# The default implementation of putBlock puts one value at a time.
#define STMANCOLUMN_GETPUT(T,NM) \
void StManColumn::aips_name2(getScalarColumn,NM) (Vector<T>* dataPtr) \
{ \
    Bool deleteIt; \
    T* data = dataPtr->getStorage (deleteIt); \
    uInt nrdone = 0; \
    uInt nrgot; \
    for (uInt nrtodo=dataPtr->nelements(); nrtodo>0;) { \
        nrgot = aips_name2(getBlock,NM) (nrdone, nrtodo, data); \
        nrtodo -= nrgot; \
        nrdone += nrgot; \
        data += nrgot; \
    } \
    dataPtr->putStorage (data, deleteIt); \
} \
void StManColumn::aips_name2(putScalarColumn,NM) (const Vector<T>* dataPtr) \
{ \
    Bool deleteIt; \
    const T* data = dataPtr->getStorage (deleteIt); \
    aips_name2(putBlock,NM) (0, dataPtr->nelements(), data); \
    dataPtr->freeStorage (data, deleteIt); \
} \
uInt StManColumn::aips_name2(getBlock,NM) \
                                 (uInt rownr, uInt nrmax, T* dataPtr) \
{ \
    if (nrmax > 0) { \
	aips_name2(get,NM) (rownr, dataPtr); \
	return 1; \
    } \
    return 0; \
} \
void StManColumn::aips_name2(putBlock,NM) \
                                 (uInt rownr, uInt nrmax, const T* dataPtr) \
{ \
    while (nrmax > 0) { \
	aips_name2(put,NM) (rownr++, dataPtr++); \
	nrmax--; \
    } \
} \
void StManColumn::aips_name2(getArray,NM) (uInt, Array<T>*) \
    { throwGetArray(); } \
void StManColumn::aips_name2(putArray,NM) (uInt, const Array<T>*) \
    { throwPutArray(); } \
void StManColumn::aips_name2(getSlice,NM) (uInt, const Slicer&, Array<T>*) \
    { throwGetArray(); } \
void StManColumn::aips_name2(putSlice,NM) (uInt, const Slicer&, const Array<T>*) \
    { throwPutArray(); } \
void StManColumn::aips_name2(getArrayColumn,NM) (Array<T>*) \
    { throwGetArray(); } \
void StManColumn::aips_name2(putArrayColumn,NM) (const Array<T>*) \
    { throwPutArray(); } \
void StManColumn::aips_name2(getColumnSlice,NM) (const Slicer&, Array<T>*) \
    { throwGetArray(); } \
void StManColumn::aips_name2(putColumnSlice,NM) (const Slicer&, const Array<T>*) \
    { throwPutArray(); } \
void StManColumn::aips_name2(getScalarColumnCells,NM) \
                                             (const RefRows& rownrs, \
					      Vector<T>* values) \
{ \
    Vector<T>& value = *values; \
    uInt i=0; \
    RefRowsSliceIter rowiter(rownrs); \
    while (! rowiter.pastEnd()) { \
        uInt rownr = rowiter.sliceStart(); \
        uInt end = rowiter.sliceEnd(); \
        uInt incr = rowiter.sliceIncr(); \
        while (rownr <= end) { \
	    aips_name2(get,NM) (rownr, &(value(i++))); \
            rownr += incr; \
        } \
	rowiter++; \
    } \
} \
void StManColumn::aips_name2(putScalarColumnCells,NM) \
                                             (const RefRows& rownrs, \
					      const Vector<T>* values) \
{ \
    const Vector<T>& value = *values; \
    uInt i=0; \
    RefRowsSliceIter rowiter(rownrs); \
    while (! rowiter.pastEnd()) { \
        uInt rownr = rowiter.sliceStart(); \
        uInt end = rowiter.sliceEnd(); \
        uInt incr = rowiter.sliceIncr(); \
        while (rownr <= end) { \
	    aips_name2(put,NM) (rownr, &(value(i++))); \
            rownr += incr; \
        } \
	rowiter++; \
    } \
} \
void StManColumn::aips_name2(getArrayColumnCells,NM) \
                                            (const RefRows& rownrs, \
					     Array<T>* values) \
{ \
    Array<T>& value = *values; \
    ArrayIterator<T> iter(value, value.ndim()-1); \
    RefRowsSliceIter rowiter(rownrs); \
    while (! rowiter.pastEnd()) { \
        uInt rownr = rowiter.sliceStart(); \
        uInt end = rowiter.sliceEnd(); \
        uInt incr = rowiter.sliceIncr(); \
        while (rownr <= end) { \
            if (! isFixedShape()) { \
                if (! iter.array().shape().isEqual (shape(rownr))) { \
                    throw DataManError("getArrayColumnCells shape mismatch"); \
                } \
            } \
  	    aips_name2(getArray,NM) (rownr, &(iter.array())); \
            rownr += incr; \
	    iter.next(); \
	} \
        rowiter++; \
    } \
} \
void StManColumn::aips_name2(putArrayColumnCells,NM) \
                                            (const RefRows& rownrs, \
					     const Array<T>* values) \
{ \
    const Array<T>& value = *values; \
    ReadOnlyArrayIterator<T> iter(value, value.ndim()-1); \
    RefRowsSliceIter rowiter(rownrs); \
    while (! rowiter.pastEnd()) { \
        uInt rownr = rowiter.sliceStart(); \
        uInt end = rowiter.sliceEnd(); \
        uInt incr = rowiter.sliceIncr(); \
        while (rownr <= end) { \
  	    aips_name2(putArray,NM) (rownr, &(iter.array())); \
            rownr += incr; \
	    iter.next(); \
	} \
        rowiter++; \
    } \
} \
void StManColumn::aips_name2(getColumnSliceCells,NM) \
                                            (const RefRows& rownrs, \
					     const Slicer& ns, \
					     Array<T>* values) \
{ \
    Array<T>& value = *values; \
    ArrayIterator<T> iter(value, value.ndim()-1); \
    RefRowsSliceIter rowiter(rownrs); \
    while (! rowiter.pastEnd()) { \
        uInt rownr = rowiter.sliceStart(); \
        uInt end = rowiter.sliceEnd(); \
        uInt incr = rowiter.sliceIncr(); \
        while (rownr <= end) { \
	    aips_name2(getSlice,NM) (rownr, ns, &(iter.array())); \
            rownr += incr; \
	    iter.next(); \
	} \
        rowiter++; \
    } \
} \
void StManColumn::aips_name2(putColumnSliceCells,NM) \
                                            (const RefRows& rownrs, \
					     const Slicer& ns, \
					     const Array<T>* values) \
{ \
    const Array<T>& value = *values; \
    ReadOnlyArrayIterator<T> iter(value, value.ndim()-1); \
    RefRowsSliceIter rowiter(rownrs); \
    while (! rowiter.pastEnd()) { \
        uInt rownr = rowiter.sliceStart(); \
        uInt end = rowiter.sliceEnd(); \
        uInt incr = rowiter.sliceIncr(); \
        while (rownr <= end) { \
	    aips_name2(putSlice,NM) (rownr, ns, &(iter.array())); \
            rownr += incr; \
	    iter.next(); \
	} \
        rowiter++; \
    } \
}

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

