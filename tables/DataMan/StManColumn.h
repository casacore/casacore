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
#include <casacore/tables/DataMan/StManColumnBase.h>

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
// StManColumn is the old storage manager base class which has been replaced
// by StManColumnBase.
// However, the class still exists for backward compatibility for external storage
// managers (such as LofarStMan) that do not derive from StManColumnBase yet.
// It also maps the new get/put functions taking a rownr_t to the old
// functions taking a uInt rownr.
// </synopsis> 

// <motivation>
// Provide backward compatibility for external storage managers.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class StManColumn : public StManColumnBase
{
public:

    // Default constructor.
    StManColumn (int dataType)
      : StManColumnBase (dataType)
    {}

    virtual ~StManColumn();

    // Get the scalar value in the given row.
    // <group>
    virtual void getBool     (rownr_t rownr, Bool* dataPtr);
    virtual void getuChar    (rownr_t rownr, uChar* dataPtr);
    virtual void getShort    (rownr_t rownr, Short* dataPtr);
    virtual void getuShort   (rownr_t rownr, uShort* dataPtr);
    virtual void getInt      (rownr_t rownr, Int* dataPtr);
    virtual void getuInt     (rownr_t rownr, uInt* dataPtr);
    virtual void getfloat    (rownr_t rownr, float* dataPtr);
    virtual void getdouble   (rownr_t rownr, double* dataPtr);
    virtual void getComplex  (rownr_t rownr, Complex* dataPtr);
    virtual void getDComplex (rownr_t rownr, DComplex* dataPtr);
    virtual void getString   (rownr_t rownr, String* dataPtr);
    // </group>

    // Put the scalar value in the given row.
    // <group>
    virtual void putBool     (rownr_t rownr, const Bool* dataPtr);
    virtual void putuChar    (rownr_t rownr, const uChar* dataPtr);
    virtual void putShort    (rownr_t rownr, const Short* dataPtr);
    virtual void putuShort   (rownr_t rownr, const uShort* dataPtr);
    virtual void putInt      (rownr_t rownr, const Int* dataPtr);
    virtual void putuInt     (rownr_t rownr, const uInt* dataPtr);
    virtual void putfloat    (rownr_t rownr, const float* dataPtr);
    virtual void putdouble   (rownr_t rownr, const double* dataPtr);
    virtual void putComplex  (rownr_t rownr, const Complex* dataPtr);
    virtual void putDComplex (rownr_t rownr, const DComplex* dataPtr);
    virtual void putString   (rownr_t rownr, const String* dataPtr);
    // </group>

    // Get all scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>&, but an ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation calls the appropriate getScalarColumnXXV.
    virtual void getScalarColumnV (ArrayBase& dataPtr);

    // Put all scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    // The default implementation calls the appropriate putScalarColumnXXV.
    virtual void putScalarColumnV (const ArrayBase& dataPtr);

    // Get some scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>&, but an ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation calls the appropriate getScalarColumnCellsXXV.
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
                                        ArrayBase& dataPtr);

    // Put some scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation calls the appropriate putScalarColumnCellsXXV.
    virtual void putScalarColumnCellsV (const RefRows& rownrs,
                                        const ArrayBase& dataPtr);

    // Get the array value in the given row.
    // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation calls the appropriate getArrayXXV.
    virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr);

    // Put the array value into the given row.
    // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation calls the appropriate putArrayXXV.
    virtual void putArrayV (rownr_t rownr, const ArrayBase& dataPtr);

    // Get all array values in the column.
    // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getArrayColumnXXV.
    virtual void getArrayColumnV (ArrayBase& dataPtr);

    // Put all array values in the column.
    // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation calls the appropriate putArrayColumnXXV.
    virtual void putArrayColumnV (const ArrayBase& dataPtr);

    // Get some array values in the column.
    // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getArrayColumnCellsXXV.
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
                                       ArrayBase& dataPtr);

    // Put some array values in the column.
    // The argument dataPtr is in fact an const Array<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate putArrayColumnCellsXXV.
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
                                       const ArrayBase& dataPtr);

    // Get a section of the array in the given row.
    // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getSlice function).
    // The default implementation calls the appropriate getSliceXXV.
    virtual void getSliceV (rownr_t rownr, const Slicer& slicer,
                            ArrayBase& dataPtr);

    // Put into a section of the array in the given row.
    // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    // The default implementation calls the appropriate putSliceXXV.
    virtual void putSliceV (rownr_t rownr, const Slicer& slicer,
                            const ArrayBase& dataPtr);

    // Get a section of all arrays in the column.
    // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getColumnSliceXXV.
    virtual void getColumnSliceV (const Slicer& slicer, ArrayBase& dataPtr);

    // Put into a section of all arrays in the column.
    // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation calls the appropriate putColumnSliceXXV.
    virtual void putColumnSliceV (const Slicer& slicer, const ArrayBase& dataPtr);

    // Get a section of some arrays in the column.
    // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation calls the appropriate getColumnSliceCellsXXV.
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, ArrayBase& dataPtr);

    // Put into a section of some arrays in the column.
    // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation calls the appropriate putColumnSliceCellsXXV.
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const ArrayBase& dataPtr);


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
    // Get the scalar value in the given row.
    // <group>
    virtual void getBoolV     (uInt rownr, Bool* dataPtr);
    virtual void getuCharV    (uInt rownr, uChar* dataPtr);
    virtual void getShortV    (uInt rownr, Short* dataPtr);
    virtual void getuShortV   (uInt rownr, uShort* dataPtr);
    virtual void getIntV      (uInt rownr, Int* dataPtr);
    virtual void getuIntV     (uInt rownr, uInt* dataPtr);
    virtual void getfloatV    (uInt rownr, float* dataPtr);
    virtual void getdoubleV   (uInt rownr, double* dataPtr);
    virtual void getComplexV  (uInt rownr, Complex* dataPtr);
    virtual void getDComplexV (uInt rownr, DComplex* dataPtr);
    virtual void getStringV   (uInt rownr, String* dataPtr);
    // </group>

    // Put the scalar value in the given row.
    // <group>
    virtual void putBoolV     (uInt rownr, const Bool* dataPtr);
    virtual void putuCharV    (uInt rownr, const uChar* dataPtr);
    virtual void putShortV    (uInt rownr, const Short* dataPtr);
    virtual void putuShortV   (uInt rownr, const uShort* dataPtr);
    virtual void putIntV      (uInt rownr, const Int* dataPtr);
    virtual void putuIntV     (uInt rownr, const uInt* dataPtr);
    virtual void putfloatV    (uInt rownr, const float* dataPtr);
    virtual void putdoubleV   (uInt rownr, const double* dataPtr);
    virtual void putComplexV  (uInt rownr, const Complex* dataPtr);
    virtual void putDComplexV (uInt rownr, const DComplex* dataPtr);
    virtual void putStringV   (uInt rownr, const String* dataPtr);
    // </group>

    // Get the scalar values in the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementations call 
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
};




} //# NAMESPACE CASACORE - END

#endif
