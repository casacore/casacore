// # StManColumn.h: Base storage manager column class
// # Copyright (C) 1994,1995,1996,1998,2002
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

#ifndef TABLES_STMANCOLUMN_H
#define TABLES_STMANCOLUMN_H

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// <summary>
// Base table column storage manager class
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
// # Classes you should understand before using this one.
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
// # A List of bugs, limitations, extensions or planned refinements.
// </todo>

class StManColumn : public StManColumnBase {
 public:
  // Default constructor.
  StManColumn(int dataType) : StManColumnBase(dataType) {}

  virtual ~StManColumn();

  // The object cannot be copied.
  StManColumn(const StManColumn&) = delete;

  // The object cannot be assigned to.
  StManColumn& operator=(const StManColumn&) = delete;

  // Set the shape of an (variable-shaped) array in the given row.
  // By default it throws a "not possible" exception.
  virtual void setShape(rownr_t rownr, const IPosition& shape);
  virtual void setShape(unsigned int rownr, const IPosition& shape);

  // Set the shape and tile shape of an (variable-shaped) array
  // in the given row.
  // By default it ignores the tile shape (thus only sets the shape).
  virtual void setShapeTiled(rownr_t rownr, const IPosition& shape, const IPosition& tileShape);
  virtual void setShapeTiled(unsigned int rownr, const IPosition& shape,
                             const IPosition& tileShape);

  // Is the value shape defined in the given row?
  // By default it returns true.
  virtual bool isShapeDefined(rownr_t rownr);
  virtual bool isShapeDefined(unsigned int rownr);

  // Get the dimensionality of the item in the given row.
  // By default it returns shape(rownr).nelements().
  virtual unsigned int ndim(rownr_t rownr);
  virtual unsigned int ndim(unsigned int rownr);

  // Get the shape of the item in the given row.
  // By default it returns a zero-length IPosition (for a scalar value).
  virtual IPosition shape(rownr_t rownr);
  virtual IPosition shape(unsigned int rownr);

  // Get the tile shape of the item in the given row.
  // By default it returns a zero-length IPosition.
  virtual IPosition tileShape(rownr_t rownr);
  virtual IPosition tileShape(unsigned int rownr);

  // Get the scalar value in the given row.
  // <group>
  virtual void getBool(rownr_t rownr, bool* dataPtr);
  virtual void getuChar(rownr_t rownr, unsigned char* dataPtr);
  virtual void getShort(rownr_t rownr, short* dataPtr);
  virtual void getuShort(rownr_t rownr, unsigned short* dataPtr);
  virtual void getInt(rownr_t rownr, int* dataPtr);
  virtual void getuInt(rownr_t rownr, unsigned int* dataPtr);
  virtual void getfloat(rownr_t rownr, float* dataPtr);
  virtual void getdouble(rownr_t rownr, double* dataPtr);
  virtual void getComplex(rownr_t rownr, Complex* dataPtr);
  virtual void getDComplex(rownr_t rownr, DComplex* dataPtr);
  virtual void getString(rownr_t rownr, String* dataPtr);
  // </group>

  // Put the scalar value in the given row.
  // <group>
  virtual void putBool(rownr_t rownr, const bool* dataPtr);
  virtual void putuChar(rownr_t rownr, const unsigned char* dataPtr);
  virtual void putShort(rownr_t rownr, const short* dataPtr);
  virtual void putuShort(rownr_t rownr, const unsigned short* dataPtr);
  virtual void putInt(rownr_t rownr, const int* dataPtr);
  virtual void putuInt(rownr_t rownr, const unsigned int* dataPtr);
  virtual void putfloat(rownr_t rownr, const float* dataPtr);
  virtual void putdouble(rownr_t rownr, const double* dataPtr);
  virtual void putComplex(rownr_t rownr, const Complex* dataPtr);
  virtual void putDComplex(rownr_t rownr, const DComplex* dataPtr);
  virtual void putString(rownr_t rownr, const String* dataPtr);
  // </group>

  // Get all scalar values in the column.
  // The argument dataPtr is in fact a Vector<T>&, but an ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn getColumn function).
  // The default implementation calls the appropriate getScalarColumnXXV.
  virtual void getScalarColumnV(ArrayBase& dataPtr);

  // Put all scalar values in the column.
  // The argument dataPtr is in fact a const Vector<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn putColumn function).
  // The default implementation calls the appropriate putScalarColumnXXV.
  virtual void putScalarColumnV(const ArrayBase& dataPtr);

  // Get some scalar values in the column.
  // The argument dataPtr is in fact a Vector<T>&, but an ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn getColumn function).
  // The default implementation calls the appropriate getScalarColumnCellsXXV.
  virtual void getScalarColumnCellsV(const RefRows& rownrs, ArrayBase& dataPtr);

  // Put some scalar values in the column.
  // The argument dataPtr is in fact a const Vector<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn getColumn function).
  // The default implementation calls the appropriate putScalarColumnCellsXXV.
  virtual void putScalarColumnCellsV(const RefRows& rownrs, const ArrayBase& dataPtr);

  // Get the array value in the given row.
  // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn get function).
  // The default implementation calls the appropriate getArrayXXV.
  virtual void getArrayV(rownr_t rownr, ArrayBase& dataPtr);

  // Put the array value into the given row.
  // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn put function).
  // The default implementation calls the appropriate putArrayXXV.
  virtual void putArrayV(rownr_t rownr, const ArrayBase& dataPtr);

  // Get all array values in the column.
  // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementation calls the appropriate getArrayColumnXXV.
  virtual void getArrayColumnV(ArrayBase& dataPtr);

  // Put all array values in the column.
  // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putColumn function).
  // The default implementation calls the appropriate putArrayColumnXXV.
  virtual void putArrayColumnV(const ArrayBase& dataPtr);

  // Get some array values in the column.
  // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementation calls the appropriate getArrayColumnCellsXXV.
  virtual void getArrayColumnCellsV(const RefRows& rownrs, ArrayBase& dataPtr);

  // Put some array values in the column.
  // The argument dataPtr is in fact an const Array<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The vector pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementation calls the appropriate putArrayColumnCellsXXV.
  virtual void putArrayColumnCellsV(const RefRows& rownrs, const ArrayBase& dataPtr);

  // Get a section of the array in the given row.
  // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn getSlice function).
  // The default implementation calls the appropriate getSliceXXV.
  virtual void getSliceV(rownr_t rownr, const Slicer& slicer, ArrayBase& dataPtr);

  // Put into a section of the array in the given row.
  // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn putSlice function).
  // The default implementation calls the appropriate putSliceXXV.
  virtual void putSliceV(rownr_t rownr, const Slicer& slicer, const ArrayBase& dataPtr);

  // Get a section of all arrays in the column.
  // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementation calls the appropriate getColumnSliceXXV.
  virtual void getColumnSliceV(const Slicer& slicer, ArrayBase& dataPtr);

  // Put into a section of all arrays in the column.
  // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn putColumn function).
  // The default implementation calls the appropriate putColumnSliceXXV.
  virtual void putColumnSliceV(const Slicer& slicer, const ArrayBase& dataPtr);

  // Get a section of some arrays in the column.
  // The argument dataPtr is in fact an Array<T>&, but a ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementation calls the appropriate getColumnSliceCellsXXV.
  virtual void getColumnSliceCellsV(const RefRows& rownrs, const Slicer& slicer,
                                    ArrayBase& dataPtr);

  // Put into a section of some arrays in the column.
  // The argument dataPtr is in fact a const Array<T>&, but a const ArrayBase&
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn putColumn function).
  // The default implementation calls the appropriate putColumnSliceCellsXXV.
  virtual void putColumnSliceCellsV(const RefRows& rownrs, const Slicer& slicer,
                                    const ArrayBase& dataPtr);

 private:
  // Throw an "invalid operation" exception for the default
  // implementation of getArray.
  void throwInvalidOp(const String& op) const;

 protected:
  // Get the scalar value in the given row.
  // <group>
  virtual void getBoolV(unsigned int rownr, bool* dataPtr);
  virtual void getuCharV(unsigned int rownr, unsigned char* dataPtr);
  virtual void getShortV(unsigned int rownr, short* dataPtr);
  virtual void getuShortV(unsigned int rownr, unsigned short* dataPtr);
  virtual void getIntV(unsigned int rownr, int* dataPtr);
  virtual void getuIntV(unsigned int rownr, unsigned int* dataPtr);
  virtual void getFloatV(unsigned int rownr, float* dataPtr);
  virtual void getDoubleV(unsigned int rownr, double* dataPtr);
  virtual void getComplexV(unsigned int rownr, Complex* dataPtr);
  virtual void getDComplexV(unsigned int rownr, DComplex* dataPtr);
  virtual void getStringV(unsigned int rownr, String* dataPtr);
  // </group>

  // Put the scalar value in the given row.
  // <group>
  virtual void putBoolV(unsigned int rownr, const bool* dataPtr);
  virtual void putuCharV(unsigned int rownr, const unsigned char* dataPtr);
  virtual void putShortV(unsigned int rownr, const short* dataPtr);
  virtual void putuShortV(unsigned int rownr, const unsigned short* dataPtr);
  virtual void putIntV(unsigned int rownr, const int* dataPtr);
  virtual void putuIntV(unsigned int rownr, const unsigned int* dataPtr);
  virtual void putFloatV(unsigned int rownr, const float* dataPtr);
  virtual void putDoubleV(unsigned int rownr, const double* dataPtr);
  virtual void putComplexV(unsigned int rownr, const Complex* dataPtr);
  virtual void putDComplexV(unsigned int rownr, const DComplex* dataPtr);
  virtual void putStringV(unsigned int rownr, const String* dataPtr);
  // </group>

  // Get the scalar values in the entire column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ScalarColumn getColumn function).
  // The default implementations calls DataManagerColumn::getScalarColumnBase.
  // <group>
  virtual void getScalarColumnBoolV(Vector<bool>* dataPtr);
  virtual void getScalarColumnuCharV(Vector<unsigned char>* dataPtr);
  virtual void getScalarColumnShortV(Vector<short>* dataPtr);
  virtual void getScalarColumnuShortV(Vector<unsigned short>* dataPtr);
  virtual void getScalarColumnIntV(Vector<int>* dataPtr);
  virtual void getScalarColumnuIntV(Vector<unsigned int>* dataPtr);
  virtual void getScalarColumnInt64V(Vector<int64_t>* dataPtr);
  virtual void getScalarColumnfloatV(Vector<float>* dataPtr);
  virtual void getScalarColumndoubleV(Vector<double>* dataPtr);
  virtual void getScalarColumnComplexV(Vector<Complex>* dataPtr);
  virtual void getScalarColumnDComplexV(Vector<DComplex>* dataPtr);
  virtual void getScalarColumnStringV(Vector<String>* dataPtr);
  // </group>

  // Put the scalar values into the entire column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ScalarColumn putColumn function).
  // The default implementations calls DataManagerColumn::putScalarColumnBase.
  // <group>
  virtual void putScalarColumnBoolV(const Vector<bool>* dataPtr);
  virtual void putScalarColumnuCharV(const Vector<unsigned char>* dataPtr);
  virtual void putScalarColumnShortV(const Vector<short>* dataPtr);
  virtual void putScalarColumnuShortV(const Vector<unsigned short>* dataPtr);
  virtual void putScalarColumnIntV(const Vector<int>* dataPtr);
  virtual void putScalarColumnuIntV(const Vector<unsigned int>* dataPtr);
  virtual void putScalarColumnInt64V(const Vector<int64_t>* dataPtr);
  virtual void putScalarColumnfloatV(const Vector<float>* dataPtr);
  virtual void putScalarColumndoubleV(const Vector<double>* dataPtr);
  virtual void putScalarColumnComplexV(const Vector<Complex>* dataPtr);
  virtual void putScalarColumnDComplexV(const Vector<DComplex>* dataPtr);
  virtual void putScalarColumnStringV(const Vector<String>* dataPtr);
  // </group>

  // Get the scalar values in some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ScalarColumn getColumnCells function).
  // The default implementations call DataManagerColumn::getScalarColumnCellsBase.
  // <group>
  virtual void getScalarColumnCellsBoolV(const RefRows& rownrs, Vector<bool>* dataPtr);
  virtual void getScalarColumnCellsuCharV(const RefRows& rownrs, Vector<unsigned char>* dataPtr);
  virtual void getScalarColumnCellsShortV(const RefRows& rownrs, Vector<short>* dataPtr);
  virtual void getScalarColumnCellsuShortV(const RefRows& rownrs, Vector<unsigned short>* dataPtr);
  virtual void getScalarColumnCellsIntV(const RefRows& rownrs, Vector<int>* dataPtr);
  virtual void getScalarColumnCellsuIntV(const RefRows& rownrs, Vector<unsigned int>* dataPtr);
  virtual void getScalarColumnCellsInt64V(const RefRows& rownrs, Vector<int64_t>* dataPtr);
  virtual void getScalarColumnCellsfloatV(const RefRows& rownrs, Vector<float>* dataPtr);
  virtual void getScalarColumnCellsdoubleV(const RefRows& rownrs, Vector<double>* dataPtr);
  virtual void getScalarColumnCellsComplexV(const RefRows& rownrs, Vector<Complex>* dataPtr);
  virtual void getScalarColumnCellsDComplexV(const RefRows& rownrs, Vector<DComplex>* dataPtr);
  virtual void getScalarColumnCellsStringV(const RefRows& rownrs, Vector<String>* dataPtr);
  // </group>

  // Put the scalar values into some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ScalarColumn putColumnCells function).
  // The default implementations call DataManagerColumn::putScalarColumnCellsBase.
  // <group>
  virtual void putScalarColumnCellsBoolV(const RefRows& rownrs, const Vector<bool>* dataPtr);
  virtual void putScalarColumnCellsuCharV(const RefRows& rownrs,
                                          const Vector<unsigned char>* dataPtr);
  virtual void putScalarColumnCellsShortV(const RefRows& rownrs, const Vector<short>* dataPtr);
  virtual void putScalarColumnCellsuShortV(const RefRows& rownrs,
                                           const Vector<unsigned short>* dataPtr);
  virtual void putScalarColumnCellsIntV(const RefRows& rownrs, const Vector<int>* dataPtr);
  virtual void putScalarColumnCellsuIntV(const RefRows& rownrs,
                                         const Vector<unsigned int>* dataPtr);
  virtual void putScalarColumnCellsInt64V(const RefRows& rownrs, const Vector<int64_t>* dataPtr);
  virtual void putScalarColumnCellsfloatV(const RefRows& rownrs, const Vector<float>* dataPtr);
  virtual void putScalarColumnCellsdoubleV(const RefRows& rownrs, const Vector<double>* dataPtr);
  virtual void putScalarColumnCellsComplexV(const RefRows& rownrs, const Vector<Complex>* dataPtr);
  virtual void putScalarColumnCellsDComplexV(const RefRows& rownrs,
                                             const Vector<DComplex>* dataPtr);
  virtual void putScalarColumnCellsStringV(const RefRows& rownrs, const Vector<String>* dataPtr);
  // </group>

  // Get the array value in the given row.
  // The array pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  // The default implementations throw an exception.
  // <group>
  virtual void getArrayBoolV(unsigned int rownr, Array<bool>* dataPtr);
  virtual void getArrayuCharV(unsigned int rownr, Array<unsigned char>* dataPtr);
  virtual void getArrayShortV(unsigned int rownr, Array<short>* dataPtr);
  virtual void getArrayuShortV(unsigned int rownr, Array<unsigned short>* dataPtr);
  virtual void getArrayIntV(unsigned int rownr, Array<int>* dataPtr);
  virtual void getArrayuIntV(unsigned int rownr, Array<unsigned int>* dataPtr);
  virtual void getArrayInt64V(unsigned int rownr, Array<int64_t>* dataPtr);
  virtual void getArrayfloatV(unsigned int rownr, Array<float>* dataPtr);
  virtual void getArraydoubleV(unsigned int rownr, Array<double>* dataPtr);
  virtual void getArrayComplexV(unsigned int rownr, Array<Complex>* dataPtr);
  virtual void getArrayDComplexV(unsigned int rownr, Array<DComplex>* dataPtr);
  virtual void getArrayStringV(unsigned int rownr, Array<String>* dataPtr);
  // </group>

  // Put the array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  // The default implementations throw an exception.
  // <group>
  virtual void putArrayBoolV(unsigned int rownr, const Array<bool>* dataPtr);
  virtual void putArrayuCharV(unsigned int rownr, const Array<unsigned char>* dataPtr);
  virtual void putArrayShortV(unsigned int rownr, const Array<short>* dataPtr);
  virtual void putArrayuShortV(unsigned int rownr, const Array<unsigned short>* dataPtr);
  virtual void putArrayIntV(unsigned int rownr, const Array<int>* dataPtr);
  virtual void putArrayuIntV(unsigned int rownr, const Array<unsigned int>* dataPtr);
  virtual void putArrayInt64V(unsigned int rownr, const Array<int64_t>* dataPtr);
  virtual void putArrayfloatV(unsigned int rownr, const Array<float>* dataPtr);
  virtual void putArraydoubleV(unsigned int rownr, const Array<double>* dataPtr);
  virtual void putArrayComplexV(unsigned int rownr, const Array<Complex>* dataPtr);
  virtual void putArrayDComplexV(unsigned int rownr, const Array<DComplex>* dataPtr);
  virtual void putArrayStringV(unsigned int rownr, const Array<String>* dataPtr);
  // </group>

  // Get the array values in the entire column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementations call DataManagerColumn::getArrayColumnBase.
  // <group>
  virtual void getArrayColumnBoolV(Array<bool>* dataPtr);
  virtual void getArrayColumnuCharV(Array<unsigned char>* dataPtr);
  virtual void getArrayColumnShortV(Array<short>* dataPtr);
  virtual void getArrayColumnuShortV(Array<unsigned short>* dataPtr);
  virtual void getArrayColumnIntV(Array<int>* dataPtr);
  virtual void getArrayColumnuIntV(Array<unsigned int>* dataPtr);
  virtual void getArrayColumnInt64V(Array<int64_t>* dataPtr);
  virtual void getArrayColumnfloatV(Array<float>* dataPtr);
  virtual void getArrayColumndoubleV(Array<double>* dataPtr);
  virtual void getArrayColumnComplexV(Array<Complex>* dataPtr);
  virtual void getArrayColumnDComplexV(Array<DComplex>* dataPtr);
  virtual void getArrayColumnStringV(Array<String>* dataPtr);
  // </group>

  // Put the array values into the entire column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn putColumn function).
  // The default implementations call DataManagerColumn::putArrayColumnBase.
  // <group>
  virtual void putArrayColumnBoolV(const Array<bool>* dataPtr);
  virtual void putArrayColumnuCharV(const Array<unsigned char>* dataPtr);
  virtual void putArrayColumnShortV(const Array<short>* dataPtr);
  virtual void putArrayColumnuShortV(const Array<unsigned short>* dataPtr);
  virtual void putArrayColumnIntV(const Array<int>* dataPtr);
  virtual void putArrayColumnuIntV(const Array<unsigned int>* dataPtr);
  virtual void putArrayColumnInt64V(const Array<int64_t>* dataPtr);
  virtual void putArrayColumnfloatV(const Array<float>* dataPtr);
  virtual void putArrayColumndoubleV(const Array<double>* dataPtr);
  virtual void putArrayColumnComplexV(const Array<Complex>* dataPtr);
  virtual void putArrayColumnDComplexV(const Array<DComplex>* dataPtr);
  virtual void putArrayColumnStringV(const Array<String>* dataPtr);
  // </group>

  // Get the array values in some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn getColumnCells function).
  // The default implementations call DataManagerColumn::getArrayColumnCellsBase.
  // <group>
  virtual void getArrayColumnCellsBoolV(const RefRows& rownrs, Array<bool>* dataPtr);
  virtual void getArrayColumnCellsuCharV(const RefRows& rownrs, Array<unsigned char>* dataPtr);
  virtual void getArrayColumnCellsShortV(const RefRows& rownrs, Array<short>* dataPtr);
  virtual void getArrayColumnCellsuShortV(const RefRows& rownrs, Array<unsigned short>* dataPtr);
  virtual void getArrayColumnCellsIntV(const RefRows& rownrs, Array<int>* dataPtr);
  virtual void getArrayColumnCellsuIntV(const RefRows& rownrs, Array<unsigned int>* dataPtr);
  virtual void getArrayColumnCellsInt64V(const RefRows& rownrs, Array<int64_t>* dataPtr);
  virtual void getArrayColumnCellsfloatV(const RefRows& rownrs, Array<float>* dataPtr);
  virtual void getArrayColumnCellsdoubleV(const RefRows& rownrs, Array<double>* dataPtr);
  virtual void getArrayColumnCellsComplexV(const RefRows& rownrs, Array<Complex>* dataPtr);
  virtual void getArrayColumnCellsDComplexV(const RefRows& rownrs, Array<DComplex>* dataPtr);
  virtual void getArrayColumnCellsStringV(const RefRows& rownrs, Array<String>* dataPtr);
  // </group>

  // Put the array values into some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn putColumnCells function).
  // The default implementations call DataManagerColumn::putArrayColumnCellsBase.
  // <group>
  virtual void putArrayColumnCellsBoolV(const RefRows& rownrs, const Array<bool>* dataPtr);
  virtual void putArrayColumnCellsuCharV(const RefRows& rownrs,
                                         const Array<unsigned char>* dataPtr);
  virtual void putArrayColumnCellsShortV(const RefRows& rownrs, const Array<short>* dataPtr);
  virtual void putArrayColumnCellsuShortV(const RefRows& rownrs,
                                          const Array<unsigned short>* dataPtr);
  virtual void putArrayColumnCellsIntV(const RefRows& rownrs, const Array<int>* dataPtr);
  virtual void putArrayColumnCellsuIntV(const RefRows& rownrs, const Array<unsigned int>* dataPtr);
  virtual void putArrayColumnCellsInt64V(const RefRows& rownrs, const Array<int64_t>* dataPtr);
  virtual void putArrayColumnCellsfloatV(const RefRows& rownrs, const Array<float>* dataPtr);
  virtual void putArrayColumnCellsdoubleV(const RefRows& rownrs, const Array<double>* dataPtr);
  virtual void putArrayColumnCellsComplexV(const RefRows& rownrs, const Array<Complex>* dataPtr);
  virtual void putArrayColumnCellsDComplexV(const RefRows& rownrs, const Array<DComplex>* dataPtr);
  virtual void putArrayColumnCellsStringV(const RefRows& rownrs, const Array<String>* dataPtr);
  // </group>

  // Get the array value in the given row.
  // The array pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getSlice function).
  // The default implementations call DataManagerColumn::getSliceBase.
  // <group>
  virtual void getSliceBoolV(unsigned int rownr, const Slicer& ns, Array<bool>* dataPtr);
  virtual void getSliceuCharV(unsigned int rownr, const Slicer& ns, Array<unsigned char>* dataPtr);
  virtual void getSliceShortV(unsigned int rownr, const Slicer& ns, Array<short>* dataPtr);
  virtual void getSliceuShortV(unsigned int rownr, const Slicer& ns,
                               Array<unsigned short>* dataPtr);
  virtual void getSliceIntV(unsigned int rownr, const Slicer& ns, Array<int>* dataPtr);
  virtual void getSliceuIntV(unsigned int rownr, const Slicer& ns, Array<unsigned int>* dataPtr);
  virtual void getSliceInt64V(unsigned int rownr, const Slicer& ns, Array<int64_t>* dataPtr);
  virtual void getSlicefloatV(unsigned int rownr, const Slicer& ns, Array<float>* dataPtr);
  virtual void getSlicedoubleV(unsigned int rownr, const Slicer& ns, Array<double>* dataPtr);
  virtual void getSliceComplexV(unsigned int rownr, const Slicer& ns, Array<Complex>* dataPtr);
  virtual void getSliceDComplexV(unsigned int rownr, const Slicer& ns, Array<DComplex>* dataPtr);
  virtual void getSliceStringV(unsigned int rownr, const Slicer& ns, Array<String>* dataPtr);
  // </group>

  // Put the array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putSlice function).
  // The default implementations call DataManagerColumn::putSliceBase.
  // <group>
  virtual void putSliceBoolV(unsigned int rownr, const Slicer& ns, const Array<bool>* dataPtr);
  virtual void putSliceuCharV(unsigned int rownr, const Slicer& ns,
                              const Array<unsigned char>* dataPtr);
  virtual void putSliceShortV(unsigned int rownr, const Slicer& ns, const Array<short>* dataPtr);
  virtual void putSliceuShortV(unsigned int rownr, const Slicer& ns,
                               const Array<unsigned short>* dataPtr);
  virtual void putSliceIntV(unsigned int rownr, const Slicer& ns, const Array<int>* dataPtr);
  virtual void putSliceuIntV(unsigned int rownr, const Slicer& ns,
                             const Array<unsigned int>* dataPtr);
  virtual void putSliceInt64V(unsigned int rownr, const Slicer& ns, const Array<int64_t>* dataPtr);
  virtual void putSlicefloatV(unsigned int rownr, const Slicer& ns, const Array<float>* dataPtr);
  virtual void putSlicedoubleV(unsigned int rownr, const Slicer& ns, const Array<double>* dataPtr);
  virtual void putSliceComplexV(unsigned int rownr, const Slicer& ns,
                                const Array<Complex>* dataPtr);
  virtual void putSliceDComplexV(unsigned int rownr, const Slicer& ns,
                                 const Array<DComplex>* dataPtr);
  virtual void putSliceStringV(unsigned int rownr, const Slicer& ns, const Array<String>* dataPtr);
  // </group>

  // Get the array values in the entire column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn getColumn function).
  // The default implementations call DataManagerColumn::getColumnSliceBase.
  // <group>
  virtual void getColumnSliceBoolV(const Slicer& ns, Array<bool>* dataPtr);
  virtual void getColumnSliceuCharV(const Slicer& ns, Array<unsigned char>* dataPtr);
  virtual void getColumnSliceShortV(const Slicer& ns, Array<short>* dataPtr);
  virtual void getColumnSliceuShortV(const Slicer& ns, Array<unsigned short>* dataPtr);
  virtual void getColumnSliceIntV(const Slicer& ns, Array<int>* dataPtr);
  virtual void getColumnSliceuIntV(const Slicer& ns, Array<unsigned int>* dataPtr);
  virtual void getColumnSliceInt64V(const Slicer& ns, Array<int64_t>* dataPtr);
  virtual void getColumnSlicefloatV(const Slicer& ns, Array<float>* dataPtr);
  virtual void getColumnSlicedoubleV(const Slicer& ns, Array<double>* dataPtr);
  virtual void getColumnSliceComplexV(const Slicer& ns, Array<Complex>* dataPtr);
  virtual void getColumnSliceDComplexV(const Slicer& ns, Array<DComplex>* dataPtr);
  virtual void getColumnSliceStringV(const Slicer& ns, Array<String>* dataPtr);
  // </group>

  // Put the array values into the entire column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn putColumn function).
  // The default implementations call DataManagerColumn::putColumnSliceBase.
  // <group>
  virtual void putColumnSliceBoolV(const Slicer& ns, const Array<bool>* dataPtr);
  virtual void putColumnSliceuCharV(const Slicer& ns, const Array<unsigned char>* dataPtr);
  virtual void putColumnSliceShortV(const Slicer& ns, const Array<short>* dataPtr);
  virtual void putColumnSliceuShortV(const Slicer& ns, const Array<unsigned short>* dataPtr);
  virtual void putColumnSliceIntV(const Slicer& ns, const Array<int>* dataPtr);
  virtual void putColumnSliceuIntV(const Slicer& ns, const Array<unsigned int>* dataPtr);
  virtual void putColumnSliceInt64V(const Slicer& ns, const Array<int64_t>* dataPtr);
  virtual void putColumnSlicefloatV(const Slicer& ns, const Array<float>* dataPtr);
  virtual void putColumnSlicedoubleV(const Slicer& ns, const Array<double>* dataPtr);
  virtual void putColumnSliceComplexV(const Slicer& ns, const Array<Complex>* dataPtr);
  virtual void putColumnSliceDComplexV(const Slicer& ns, const Array<DComplex>* dataPtr);
  virtual void putColumnSliceStringV(const Slicer& ns, const Array<String>* dataPtr);
  // </group>

  // Get the array values in some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn getColumnCells function).
  // The default implementations call DataManagerColumn::getColumnSliceCellsBase.
  // <group>
  virtual void getColumnSliceCellsBoolV(const RefRows& rownrs, const Slicer& ns,
                                        Array<bool>* dataPtr);
  virtual void getColumnSliceCellsuCharV(const RefRows& rownrs, const Slicer& ns,
                                         Array<unsigned char>* dataPtr);
  virtual void getColumnSliceCellsShortV(const RefRows& rownrs, const Slicer& ns,
                                         Array<short>* dataPtr);
  virtual void getColumnSliceCellsuShortV(const RefRows& rownrs, const Slicer& ns,
                                          Array<unsigned short>* dataPtr);
  virtual void getColumnSliceCellsIntV(const RefRows& rownrs, const Slicer& ns,
                                       Array<int>* dataPtr);
  virtual void getColumnSliceCellsuIntV(const RefRows& rownrs, const Slicer& ns,
                                        Array<unsigned int>* dataPtr);
  virtual void getColumnSliceCellsInt64V(const RefRows& rownrs, const Slicer& ns,
                                         Array<int64_t>* dataPtr);
  virtual void getColumnSliceCellsfloatV(const RefRows& rownrs, const Slicer& ns,
                                         Array<float>* dataPtr);
  virtual void getColumnSliceCellsdoubleV(const RefRows& rownrs, const Slicer& ns,
                                          Array<double>* dataPtr);
  virtual void getColumnSliceCellsComplexV(const RefRows& rownrs, const Slicer& ns,
                                           Array<Complex>* dataPtr);
  virtual void getColumnSliceCellsDComplexV(const RefRows& rownrs, const Slicer& ns,
                                            Array<DComplex>* dataPtr);
  virtual void getColumnSliceCellsStringV(const RefRows& rownrs, const Slicer& ns,
                                          Array<String>* dataPtr);
  // </group>

  // Put the array values into some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ArrayColumn putColumnSlice function).
  // The default implementations call DataManagerColumn::putColumnSliceCellsBase.
  // <group>
  virtual void putColumnSliceCellsBoolV(const RefRows& rownrs, const Slicer& ns,
                                        const Array<bool>* dataPtr);
  virtual void putColumnSliceCellsuCharV(const RefRows& rownrs, const Slicer& ns,
                                         const Array<unsigned char>* dataPtr);
  virtual void putColumnSliceCellsShortV(const RefRows& rownrs, const Slicer& ns,
                                         const Array<short>* dataPtr);
  virtual void putColumnSliceCellsuShortV(const RefRows& rownrs, const Slicer& ns,
                                          const Array<unsigned short>* dataPtr);
  virtual void putColumnSliceCellsIntV(const RefRows& rownrs, const Slicer& ns,
                                       const Array<int>* dataPtr);
  virtual void putColumnSliceCellsuIntV(const RefRows& rownrs, const Slicer& ns,
                                        const Array<unsigned int>* dataPtr);
  virtual void putColumnSliceCellsInt64V(const RefRows& rownrs, const Slicer& ns,
                                         const Array<int64_t>* dataPtr);
  virtual void putColumnSliceCellsfloatV(const RefRows& rownrs, const Slicer& ns,
                                         const Array<float>* dataPtr);
  virtual void putColumnSliceCellsdoubleV(const RefRows& rownrs, const Slicer& ns,
                                          const Array<double>* dataPtr);
  virtual void putColumnSliceCellsComplexV(const RefRows& rownrs, const Slicer& ns,
                                           const Array<Complex>* dataPtr);
  virtual void putColumnSliceCellsDComplexV(const RefRows& rownrs, const Slicer& ns,
                                            const Array<DComplex>* dataPtr);
  virtual void putColumnSliceCellsStringV(const RefRows& rownrs, const Slicer& ns,
                                          const Array<String>* dataPtr);
  // </group>
};

}  // namespace casacore

#endif
