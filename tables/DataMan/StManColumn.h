//# StManColumn.h: Base storage manager column class
//# Copyright (C) 1994,1995,1996,1998,2002
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

#ifndef TABLES_STMANCOLUMN_H
#define TABLES_STMANCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Vector;


// <summary>
// Base table column storage manager class
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManagerColumn
// </prerequisite>

// <etymology>
// StManColumn handles a column for a storage manager.
// </etymology>

// <synopsis> 
// StManColumn is the abstract base class to handle a column in all
// kind of storage managers. It is derived from DataManagerColumn
// and implements several virtual functions for derived storage
// manager column classes (like StManColumnAipsIO).
//
// All get and put functions (except for single scalars) in the abstract
// base class DataManagerColumn have a generic void* data argument.
// This is done to allow arbitrary typed arguments. This can be done
// because the data type of the derived class always matches the
// type of the data argument.
// However, at one time the void* has to be casted to the exact type.
// Storage managers only support the standard data types; therefore
// it is possible to do the cast in a base class. This concentrates
// the burden in one class and allows the derived classes to work
// with correctly typed arguments.
// The price is an extra virtual function call, but that is not a
// problem for (expensive) operations on arrays.
// It is not done for single scalars, because that price may be too high.
// 
// See StManColumnAipsIO for the get/put functions required in a storage
// manager column class handling scalars.
// See StManColumnArrayAipsIO for the get/put functions required in a
// storage manager column class handling arrays. This class also
// contains the shape functions for direct arrays, while
// StManColumnIndArrayAipsIO contains the shape functions for indirec
// arrays.
//
// StManColumn also contains the data type of the column (which it
// gets from the derived classes at construction time) and implements
// the function dataType on behalf of its derived classes.
// </synopsis> 

// <motivation>
// Making life easier for the derived classes.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class StManColumn : public DataManagerColumn
{
public:

    // Default constructor.
    StManColumn (int dataType);

    ~StManColumn();

    // Test if the given data type is supported by storage managers.
    // It is used by the function Table::isNativeDataType.
    static Bool isNativeDataType (int dtype);

    // Return the data type of the column.
    int dataType() const;

    // By default the storage manager can handle access to a scalar column.
    Bool canAccessScalarColumn (Bool& reask) const;

    // All storage managers can handle access to scalar column cells, because
    // this class contains a default implementation of getScalarColumnCellsV.
    Bool canAccessScalarColumnCells (Bool& reask) const;

    // All storage managers can handle access to array column cells, because
    // this class contains a default implementation of getArrayColumnCellsV.
    Bool canAccessArrayColumnCells (Bool& reask) const;

    // Get all scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation calls the appropriate getScalarColumnXXV.
    void getScalarColumnV (void* dataPtr);

    // Put all scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    // The default implementation calls the appropriate putScalarColumnXXV.
    void putScalarColumnV (const void* dataPtr);

    // Get some scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation calls the appropriate getScalarColumnCellsXXV.
    void getScalarColumnCellsV (const RefRows& rownrs,
				void* dataPtr);

    // Put some scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation calls the appropriate putScalarColumnCellsXXV.
    void putScalarColumnCellsV (const RefRows& rownrs,
				const void* dataPtr);

    // Get scalars from the given row on with a maximum of nrmax values.
    // It returns the actual number of values got.
    // This can be used to get an entire column of scalars or to get
    // a part of a column (for a cache for example).
    // The argument dataPtr is in fact a T*, but a void*
    // is needed to be generic.
    // The default implementation calls the appropriate getBlockXXV function.
    uInt getBlockV (uInt rownr, uInt nrmax, void* dataPtr);

    // Put nrmax scalars from the given row on.
    // It returns the actual number of values put.
    // This can be used to put an entire column of scalars or to put
    // a part of a column (for a cache for example).
    // The argument dataPtr is in fact a const T*, but a const void*
    // is needed to be generic.
    // The default implementation calls the appropriate putBlockXXV function.
    void putBlockV (uInt rownr, uInt nrmax, const void* dataPtr);

    // Get the array value in the given row.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation calls the appropriate getArrayXXV.
    void getArrayV (uInt rownr, void* dataPtr);

    // Put the array value into the given row.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation calls the appropriate putArrayXXV.
    void putArrayV (uInt rownr, const void* dataPtr);

    // Get all array values in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getArrayColumnXXV.
    void getArrayColumnV (void* dataPtr);

    // Put all array values in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation calls the appropriate putArrayColumnXXV.
    void putArrayColumnV (const void* dataPtr);

    // Get some array values in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getArrayColumnCellsXXV.
    void getArrayColumnCellsV (const RefRows& rownrs, void* dataPtr);

    // Put some array values in the column.
    // The argument dataPtr is in fact an const Array<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate putArrayColumnCellsXXV.
    void putArrayColumnCellsV (const RefRows& rownrs, const void* dataPtr);

    // Get a section of the array in the given row.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getSlice function).
    // The default implementation calls the appropriate getSliceXXV.
    void getSliceV (uInt rownr, const Slicer& slicer, void* dataPtr);

    // Put into a section of the array in the given row.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    // The default implementation calls the appropriate putSliceXXV.
    void putSliceV (uInt rownr, const Slicer& slicer, const void* dataPtr);

    // Get a section of all arrays in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getColumnSliceXXV.
    void getColumnSliceV (const Slicer& slicer, void* dataPtr);

    // Put into a section of all arrays in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation calls the appropriate putColumnSliceXXV.
    void putColumnSliceV (const Slicer& slicer, const void* dataPtr);

    // Get a section of some arrays in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getColumnSliceCellsXXV.
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, void* dataPtr);

    // Put into a section of some arrays in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation calls the appropriate putColumnSliceCellsXXV.
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const void* dataPtr);


private:
    // The object cannot be copied.
    StManColumn (const StManColumn&);

    // The object cannot be assigned to.
    StManColumn& operator= (const StManColumn&);

    // Throw an "invalid operation" exception for the default
    // implementation of getArray.
    void throwGetArray() const;

    // Throw an "invalid operation" exception for the default
    // implementation of putArray.
    void throwPutArray() const;


protected:
    // Get the scalar values in the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation uses the corresponding getBlockXXXV.
    // <group>
    virtual void getScalarColumnBoolV     (Vector<Bool>* dataPtr);
    virtual void getScalarColumnuCharV    (Vector<uChar>* dataPtr);
    virtual void getScalarColumnShortV    (Vector<Short>* dataPtr);
    virtual void getScalarColumnuShortV   (Vector<uShort>* dataPtr);
    virtual void getScalarColumnIntV      (Vector<Int>* dataPtr);
    virtual void getScalarColumnuIntV     (Vector<uInt>* dataPtr);
    virtual void getScalarColumnfloatV    (Vector<float>* dataPtr);
    virtual void getScalarColumndoubleV   (Vector<double>* dataPtr);
    virtual void getScalarColumnComplexV  (Vector<Complex>* dataPtr);
    virtual void getScalarColumnDComplexV (Vector<DComplex>* dataPtr);
    virtual void getScalarColumnStringV   (Vector<String>* dataPtr);
    // </group>

    // Put the scalar values into the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn putColumn function).
    // The default implementation uses the corresponding putBlockXXXV.
    // <group>
    virtual void putScalarColumnBoolV     (const Vector<Bool>* dataPtr);
    virtual void putScalarColumnuCharV    (const Vector<uChar>* dataPtr);
    virtual void putScalarColumnShortV    (const Vector<Short>* dataPtr);
    virtual void putScalarColumnuShortV   (const Vector<uShort>* dataPtr);
    virtual void putScalarColumnIntV      (const Vector<Int>* dataPtr);
    virtual void putScalarColumnuIntV     (const Vector<uInt>* dataPtr);
    virtual void putScalarColumnfloatV    (const Vector<float>* dataPtr);
    virtual void putScalarColumndoubleV   (const Vector<double>* dataPtr);
    virtual void putScalarColumnComplexV  (const Vector<Complex>* dataPtr);
    virtual void putScalarColumnDComplexV (const Vector<DComplex>* dataPtr);
    virtual void putScalarColumnStringV   (const Vector<String>* dataPtr);
    // </group>

    // Get the scalar values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumnCells function).
    // The default implementation loops through all rows.
    // <group>
    virtual void getScalarColumnCellsBoolV     (const RefRows& rownrs,
						Vector<Bool>* dataPtr);
    virtual void getScalarColumnCellsuCharV    (const RefRows& rownrs,
						Vector<uChar>* dataPtr);
    virtual void getScalarColumnCellsShortV    (const RefRows& rownrs,
						Vector<Short>* dataPtr);
    virtual void getScalarColumnCellsuShortV   (const RefRows& rownrs,
						Vector<uShort>* dataPtr);
    virtual void getScalarColumnCellsIntV      (const RefRows& rownrs,
						Vector<Int>* dataPtr);
    virtual void getScalarColumnCellsuIntV     (const RefRows& rownrs,
						Vector<uInt>* dataPtr);
    virtual void getScalarColumnCellsfloatV    (const RefRows& rownrs,
						Vector<float>* dataPtr);
    virtual void getScalarColumnCellsdoubleV   (const RefRows& rownrs,
						Vector<double>* dataPtr);
    virtual void getScalarColumnCellsComplexV  (const RefRows& rownrs,
						Vector<Complex>* dataPtr);
    virtual void getScalarColumnCellsDComplexV (const RefRows& rownrs,
						Vector<DComplex>* dataPtr);
    virtual void getScalarColumnCellsStringV   (const RefRows& rownrs,
						Vector<String>* dataPtr);
    // </group>

    // Put the scalar values into some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn putColumnCells function).
    // The default implementation loops through all rows.
    // <group>
    virtual void putScalarColumnCellsBoolV     (const RefRows& rownrs,
						const Vector<Bool>* dataPtr);
    virtual void putScalarColumnCellsuCharV    (const RefRows& rownrs,
						const Vector<uChar>* dataPtr);
    virtual void putScalarColumnCellsShortV    (const RefRows& rownrs,
						const Vector<Short>* dataPtr);
    virtual void putScalarColumnCellsuShortV   (const RefRows& rownrs,
						const Vector<uShort>* dataPtr);
    virtual void putScalarColumnCellsIntV      (const RefRows& rownrs,
						const Vector<Int>* dataPtr);
    virtual void putScalarColumnCellsuIntV     (const RefRows& rownrs,
						const Vector<uInt>* dataPtr);
    virtual void putScalarColumnCellsfloatV    (const RefRows& rownrs,
						const Vector<float>* dataPtr);
    virtual void putScalarColumnCellsdoubleV   (const RefRows& rownrs,
						const Vector<double>* dataPtr);
    virtual void putScalarColumnCellsComplexV  (const RefRows& rownrs,
						const Vector<Complex>* dataPtr);
    virtual void putScalarColumnCellsDComplexV (const RefRows& rownrs,
					       const Vector<DComplex>* dataPtr);
    virtual void putScalarColumnCellsStringV   (const RefRows& rownrs,
						const Vector<String>* dataPtr);
    // </group>

    // Get scalars from the given row on with a maximum of nrmax values.
    // This can be used to get an entire column of scalars or to get
    // a part of a column (for a cache for example).
    // The buffer pointed to by dataPtr has to have the length nrmax.
    // (which is guaranteed by the ScalarColumn get function).
    // The default implementation gets one value.
    // <group>
    virtual uInt getBlockBoolV     (uInt rownr, uInt nrmax,
				    Bool* dataPtr);
    virtual uInt getBlockuCharV    (uInt rownr, uInt nrmax,
				    uChar* dataPtr);
    virtual uInt getBlockShortV    (uInt rownr, uInt nrmax,
				    Short* dataPtr);
    virtual uInt getBlockuShortV   (uInt rownr, uInt nrmax,
				    uShort* dataPtr);
    virtual uInt getBlockIntV      (uInt rownr, uInt nrmax,
				    Int* dataPtr);
    virtual uInt getBlockuIntV     (uInt rownr, uInt nrmax,
				    uInt* dataPtr);
    virtual uInt getBlockfloatV    (uInt rownr, uInt nrmax,
				    float* dataPtr);
    virtual uInt getBlockdoubleV   (uInt rownr, uInt nrmax,
				    double* dataPtr);
    virtual uInt getBlockComplexV  (uInt rownr, uInt nrmax,
				    Complex* dataPtr);
    virtual uInt getBlockDComplexV (uInt rownr, uInt nrmax,
				    DComplex* dataPtr);
    virtual uInt getBlockStringV   (uInt rownr, uInt nrmax,
				    String* dataPtr);
    // </group>

    // Put nrmax scalars from the given row on.
    // This can be used to put an entire column of scalars or to put
    // a part of a column (for a cache for example).
    // The buffer pointed to by dataPtr has to have the length nrmax.
    // The default implementation puts one value at the time.
    // <group>
    virtual void putBlockBoolV     (uInt rownr, uInt nrmax,
				    const Bool* dataPtr);
    virtual void putBlockuCharV    (uInt rownr, uInt nrmax,
				    const uChar* dataPtr);
    virtual void putBlockShortV    (uInt rownr, uInt nrmax,
			            const Short* dataPtr);
    virtual void putBlockuShortV   (uInt rownr, uInt nrmax,
				    const uShort* dataPtr);
    virtual void putBlockIntV      (uInt rownr, uInt nrmax,
				    const Int* dataPtr);
    virtual void putBlockuIntV     (uInt rownr, uInt nrmax,
				    const uInt* dataPtr);
    virtual void putBlockfloatV    (uInt rownr, uInt nrmax,
				    const float* dataPtr);
    virtual void putBlockdoubleV   (uInt rownr, uInt nrmax,
				    const double* dataPtr);
    virtual void putBlockComplexV  (uInt rownr, uInt nrmax,
				    const Complex* dataPtr);
    virtual void putBlockDComplexV (uInt rownr, uInt nrmax,
				    const DComplex* dataPtr);
    virtual void putBlockStringV   (uInt rownr, uInt nrmax,
				    const String* dataPtr);
    // </group>

    // Get the array value in the given row.
    // The array pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation loops through all rows.
    // <group>
    virtual void getArrayBoolV     (uInt rownr, Array<Bool>* dataPtr);
    virtual void getArrayuCharV    (uInt rownr, Array<uChar>* dataPtr);
    virtual void getArrayShortV    (uInt rownr, Array<Short>* dataPtr);
    virtual void getArrayuShortV   (uInt rownr, Array<uShort>* dataPtr);
    virtual void getArrayIntV      (uInt rownr, Array<Int>* dataPtr);
    virtual void getArrayuIntV     (uInt rownr, Array<uInt>* dataPtr);
    virtual void getArrayfloatV    (uInt rownr, Array<float>* dataPtr);
    virtual void getArraydoubleV   (uInt rownr, Array<double>* dataPtr);
    virtual void getArrayComplexV  (uInt rownr, Array<Complex>* dataPtr);
    virtual void getArrayDComplexV (uInt rownr, Array<DComplex>* dataPtr);
    virtual void getArrayStringV   (uInt rownr, Array<String>* dataPtr);
    // </group>

    // Put the array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation loops through all rows.
    // <group>
    virtual void putArrayBoolV     (uInt rownr,
				    const Array<Bool>* dataPtr);
    virtual void putArrayuCharV    (uInt rownr,
				    const Array<uChar>* dataPtr);
    virtual void putArrayShortV    (uInt rownr,
				    const Array<Short>* dataPtr);
    virtual void putArrayuShortV   (uInt rownr,
				    const Array<uShort>* dataPtr);
    virtual void putArrayIntV      (uInt rownr,
				    const Array<Int>* dataPtr);
    virtual void putArrayuIntV     (uInt rownr,
				    const Array<uInt>* dataPtr);
    virtual void putArrayfloatV    (uInt rownr,
				    const Array<float>* dataPtr);
    virtual void putArraydoubleV   (uInt rownr,
				    const Array<double>* dataPtr);
    virtual void putArrayComplexV  (uInt rownr,
				    const Array<Complex>* dataPtr);
    virtual void putArrayDComplexV (uInt rownr,
				    const Array<DComplex>* dataPtr);
    virtual void putArrayStringV   (uInt rownr,
				    const Array<String>* dataPtr);
    // </group>

    // Get the array values in the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation uses the corresponding getBlockXXXV.
    // <group>
    virtual void getArrayColumnBoolV     (Array<Bool>* dataPtr);
    virtual void getArrayColumnuCharV    (Array<uChar>* dataPtr);
    virtual void getArrayColumnShortV    (Array<Short>* dataPtr);
    virtual void getArrayColumnuShortV   (Array<uShort>* dataPtr);
    virtual void getArrayColumnIntV      (Array<Int>* dataPtr);
    virtual void getArrayColumnuIntV     (Array<uInt>* dataPtr);
    virtual void getArrayColumnfloatV    (Array<float>* dataPtr);
    virtual void getArrayColumndoubleV   (Array<double>* dataPtr);
    virtual void getArrayColumnComplexV  (Array<Complex>* dataPtr);
    virtual void getArrayColumnDComplexV (Array<DComplex>* dataPtr);
    virtual void getArrayColumnStringV   (Array<String>* dataPtr);
    // </group>

    // Put the array values into the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation uses the corresponding putBlockXXXV.
    // <group>
    virtual void putArrayColumnBoolV     (const Array<Bool>* dataPtr);
    virtual void putArrayColumnuCharV    (const Array<uChar>* dataPtr);
    virtual void putArrayColumnShortV    (const Array<Short>* dataPtr);
    virtual void putArrayColumnuShortV   (const Array<uShort>* dataPtr);
    virtual void putArrayColumnIntV      (const Array<Int>* dataPtr);
    virtual void putArrayColumnuIntV     (const Array<uInt>* dataPtr);
    virtual void putArrayColumnfloatV    (const Array<float>* dataPtr);
    virtual void putArrayColumndoubleV   (const Array<double>* dataPtr);
    virtual void putArrayColumnComplexV  (const Array<Complex>* dataPtr);
    virtual void putArrayColumnDComplexV (const Array<DComplex>* dataPtr);
    virtual void putArrayColumnStringV   (const Array<String>* dataPtr);
    // </group>

    // Get the array values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn getColumnCells function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void getArrayColumnCellsBoolV     (const RefRows& rownrs,
					       Array<Bool>* dataPtr);
    virtual void getArrayColumnCellsuCharV    (const RefRows& rownrs,
					       Array<uChar>* dataPtr);
    virtual void getArrayColumnCellsShortV    (const RefRows& rownrs,
					       Array<Short>* dataPtr);
    virtual void getArrayColumnCellsuShortV   (const RefRows& rownrs,
					       Array<uShort>* dataPtr);
    virtual void getArrayColumnCellsIntV      (const RefRows& rownrs,
					       Array<Int>* dataPtr);
    virtual void getArrayColumnCellsuIntV     (const RefRows& rownrs,
					       Array<uInt>* dataPtr);
    virtual void getArrayColumnCellsfloatV    (const RefRows& rownrs,
					       Array<float>* dataPtr);
    virtual void getArrayColumnCellsdoubleV   (const RefRows& rownrs,
					       Array<double>* dataPtr);
    virtual void getArrayColumnCellsComplexV  (const RefRows& rownrs,
					       Array<Complex>* dataPtr);
    virtual void getArrayColumnCellsDComplexV (const RefRows& rownrs,
					       Array<DComplex>* dataPtr);
    virtual void getArrayColumnCellsStringV   (const RefRows& rownrs,
					       Array<String>* dataPtr);
    // </group>

    // Put the array values into some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn putColumnCells function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void putArrayColumnCellsBoolV     (const RefRows& rownrs,
					       const Array<Bool>* dataPtr);
    virtual void putArrayColumnCellsuCharV    (const RefRows& rownrs,
					       const Array<uChar>* dataPtr);
    virtual void putArrayColumnCellsShortV    (const RefRows& rownrs,
					       const Array<Short>* dataPtr);
    virtual void putArrayColumnCellsuShortV   (const RefRows& rownrs,
					       const Array<uShort>* dataPtr);
    virtual void putArrayColumnCellsIntV      (const RefRows& rownrs,
					       const Array<Int>* dataPtr);
    virtual void putArrayColumnCellsuIntV     (const RefRows& rownrs,
					       const Array<uInt>* dataPtr);
    virtual void putArrayColumnCellsfloatV    (const RefRows& rownrs,
					       const Array<float>* dataPtr);
    virtual void putArrayColumnCellsdoubleV   (const RefRows& rownrs,
					       const Array<double>* dataPtr);
    virtual void putArrayColumnCellsComplexV  (const RefRows& rownrs,
					       const Array<Complex>* dataPtr);
    virtual void putArrayColumnCellsDComplexV (const RefRows& rownrs,
					       const Array<DComplex>* dataPtr);
    virtual void putArrayColumnCellsStringV   (const RefRows& rownrs,
					       const Array<String>* dataPtr);
    // </group>

    // Get the array value in the given row.
    // The array pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void getSliceBoolV     (uInt rownr, const Slicer& ns,
				    Array<Bool>* dataPtr);
    virtual void getSliceuCharV    (uInt rownr, const Slicer& ns,
				    Array<uChar>* dataPtr);
    virtual void getSliceShortV    (uInt rownr, const Slicer& ns,
				    Array<Short>* dataPtr);
    virtual void getSliceuShortV   (uInt rownr, const Slicer& ns,
				    Array<uShort>* dataPtr);
    virtual void getSliceIntV      (uInt rownr, const Slicer& ns,
				    Array<Int>* dataPtr);
    virtual void getSliceuIntV     (uInt rownr, const Slicer& ns,
				    Array<uInt>* dataPtr);
    virtual void getSlicefloatV    (uInt rownr, const Slicer& ns,
				    Array<float>* dataPtr);
    virtual void getSlicedoubleV   (uInt rownr, const Slicer& ns,
				    Array<double>* dataPtr);
    virtual void getSliceComplexV  (uInt rownr, const Slicer& ns,
				    Array<Complex>* dataPtr);
    virtual void getSliceDComplexV (uInt rownr, const Slicer& ns,
				    Array<DComplex>* dataPtr);
    virtual void getSliceStringV   (uInt rownr, const Slicer& ns,
				    Array<String>* dataPtr);
    // </group>

    // Put the array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void putSliceBoolV     (uInt rownr, const Slicer& ns,
				    const Array<Bool>* dataPtr);
    virtual void putSliceuCharV    (uInt rownr, const Slicer& ns,
				    const Array<uChar>* dataPtr);
    virtual void putSliceShortV    (uInt rownr, const Slicer& ns,
				    const Array<Short>* dataPtr);
    virtual void putSliceuShortV   (uInt rownr, const Slicer& ns,
				    const Array<uShort>* dataPtr);
    virtual void putSliceIntV      (uInt rownr, const Slicer& ns,
				    const Array<Int>* dataPtr);
    virtual void putSliceuIntV     (uInt rownr, const Slicer& ns,
				    const Array<uInt>* dataPtr);
    virtual void putSlicefloatV    (uInt rownr, const Slicer& ns,
				    const Array<float>* dataPtr);
    virtual void putSlicedoubleV   (uInt rownr, const Slicer& ns,
				    const Array<double>* dataPtr);
    virtual void putSliceComplexV  (uInt rownr, const Slicer& ns,
				    const Array<Complex>* dataPtr);
    virtual void putSliceDComplexV (uInt rownr, const Slicer& ns,
				    const Array<DComplex>* dataPtr);
    virtual void putSliceStringV   (uInt rownr, const Slicer& ns,
				    const Array<String>* dataPtr);
    // </group>

    // Get the array values in the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation uses the corresponding getBlockXXXV.
    // <group>
    virtual void getColumnSliceBoolV     (const Slicer& ns,
					  Array<Bool>* dataPtr);
    virtual void getColumnSliceuCharV    (const Slicer& ns,
					  Array<uChar>* dataPtr);
    virtual void getColumnSliceShortV    (const Slicer& ns,
					  Array<Short>* dataPtr);
    virtual void getColumnSliceuShortV   (const Slicer& ns,
					  Array<uShort>* dataPtr);
    virtual void getColumnSliceIntV      (const Slicer& ns,
					  Array<Int>* dataPtr);
    virtual void getColumnSliceuIntV     (const Slicer& ns,
					  Array<uInt>* dataPtr);
    virtual void getColumnSlicefloatV    (const Slicer& ns,
					  Array<float>* dataPtr);
    virtual void getColumnSlicedoubleV   (const Slicer& ns,
					  Array<double>* dataPtr);
    virtual void getColumnSliceComplexV  (const Slicer& ns,
					  Array<Complex>* dataPtr);
    virtual void getColumnSliceDComplexV (const Slicer& ns,
					  Array<DComplex>* dataPtr);
    virtual void getColumnSliceStringV   (const Slicer& ns,
					  Array<String>* dataPtr);
    // </group>

    // Put the array values into the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation uses the corresponding putBlockXXXV.
    // <group>
    virtual void putColumnSliceBoolV     (const Slicer& ns,
					  const Array<Bool>* dataPtr);
    virtual void putColumnSliceuCharV    (const Slicer& ns,
					  const Array<uChar>* dataPtr);
    virtual void putColumnSliceShortV    (const Slicer& ns,
					  const Array<Short>* dataPtr);
    virtual void putColumnSliceuShortV   (const Slicer& ns,
					  const Array<uShort>* dataPtr);
    virtual void putColumnSliceIntV      (const Slicer& ns,
					  const Array<Int>* dataPtr);
    virtual void putColumnSliceuIntV     (const Slicer& ns,
					  const Array<uInt>* dataPtr);
    virtual void putColumnSlicefloatV    (const Slicer& ns,
					  const Array<float>* dataPtr);
    virtual void putColumnSlicedoubleV   (const Slicer& ns,
					  const Array<double>* dataPtr);
    virtual void putColumnSliceComplexV  (const Slicer& ns,
					  const Array<Complex>* dataPtr);
    virtual void putColumnSliceDComplexV (const Slicer& ns,
					  const Array<DComplex>* dataPtr);
    virtual void putColumnSliceStringV   (const Slicer& ns,
					  const Array<String>* dataPtr);
    // </group>

    // Get the array values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn getColumnCells function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void getColumnSliceCellsBoolV     (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Bool>* dataPtr);
    virtual void getColumnSliceCellsuCharV    (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<uChar>* dataPtr);
    virtual void getColumnSliceCellsShortV    (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Short>* dataPtr);
    virtual void getColumnSliceCellsuShortV   (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<uShort>* dataPtr);
    virtual void getColumnSliceCellsIntV      (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Int>* dataPtr);
    virtual void getColumnSliceCellsuIntV     (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<uInt>* dataPtr);
    virtual void getColumnSliceCellsfloatV    (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<float>* dataPtr);
    virtual void getColumnSliceCellsdoubleV   (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<double>* dataPtr);
    virtual void getColumnSliceCellsComplexV  (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Complex>* dataPtr);
    virtual void getColumnSliceCellsDComplexV (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<DComplex>* dataPtr);
    virtual void getColumnSliceCellsStringV   (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<String>* dataPtr);
    // </group>

    // Put the array values into some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn putColumnSlice function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void putColumnSliceCellsBoolV     (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Bool>* dataPtr);
    virtual void putColumnSliceCellsuCharV    (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<uChar>* dataPtr);
    virtual void putColumnSliceCellsShortV    (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Short>* dataPtr);
    virtual void putColumnSliceCellsuShortV   (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<uShort>* dataPtr);
    virtual void putColumnSliceCellsIntV      (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Int>* dataPtr);
    virtual void putColumnSliceCellsuIntV     (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<uInt>* dataPtr);
    virtual void putColumnSliceCellsfloatV    (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<float>* dataPtr);
    virtual void putColumnSliceCellsdoubleV   (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<double>* dataPtr);
    virtual void putColumnSliceCellsComplexV  (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Complex>* dataPtr);
    virtual void putColumnSliceCellsDComplexV (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<DComplex>* dataPtr);
    virtual void putColumnSliceCellsStringV   (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<String>* dataPtr);
    // </group>


private:
    // The data type of the column.
    int dtype_p;
};




} //# NAMESPACE CASACORE - END

#endif
