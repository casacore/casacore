//# tStManAll.cc: Test program for the various storage managers
//# Copyright (C) 2018
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/MemoryStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the various storage managers.
// It tests if all storage managers work well for all data types and for all
// possible get and put functions (defined in ArrayColumn).
// It also tests it for ForwardColumn, VirtualTaQLColumn, ConcatTable and RefTable.
// </summary>


// Define a macro to execute a function for all column types.
#define ExecFunc(funcName, tab, prefix)           \
  funcName <Bool> (tab, prefix+"b", BoolArrays); \
  funcName <uChar> (tab, prefix+"uc", uCharArrays); \
  funcName <Short> (tab, prefix+"s", ShortArrays); \
  funcName <uShort> (tab, prefix+"us", uShortArrays); \
  funcName <Int> (tab, prefix+"i", IntArrays); \
  funcName <uInt> (tab, prefix+"ui", uIntArrays); \
  funcName <Int64> (tab, prefix+"i64", Int64Arrays); \
  funcName <Float> (tab, prefix+"f", FloatArrays); \
  funcName <Double> (tab, prefix+"d", DoubleArrays); \
  funcName <Complex> (tab, prefix+"cx", ComplexArrays); \
  funcName <DComplex> (tab, prefix+"dcx", DComplexArrays); \
  funcName <String> (tab, prefix+"sf", FStringArrays); \
  funcName <String> (tab, prefix+"sv", VStringArrays);

#define updateTable(funcName) \
  { \
    Table tab(table); \
    if (! keepTable) { \
      tab = Table("tStMan_tmp.data", Table::Update); \
    } \
    ExecFunc(funcName, tab, String()); \
  } \
  checktab (table);


// Define globally all data arrays (one array per column; outer axis is row).
// They get changed in the same way as the table data are changed.
IPosition arrShapes[4];
Array<Bool> BoolArrays[4];
Array<uChar> uCharArrays[4];
Array<Short> ShortArrays[4];
Array<uShort> uShortArrays[4];
Array<Int> IntArrays[4];
Array<uInt> uIntArrays[4];
Array<Int64> Int64Arrays[4];
Array<Float> FloatArrays[4];
Array<Double> DoubleArrays[4];
Array<Complex> ComplexArrays[4];
Array<DComplex> DComplexArrays[4];
Array<String> FStringArrays[4];
Array<String> VStringArrays[4];


// Create an array and fill it using start value and increment.
// Note that a Bool andf String array are specialized below.
template<typename T> Array<T> makeArray (const IPosition& shape,
                                         T value, T incr)
{
  Array<T> arr(shape);
  indgen(arr, value, incr);
  return arr;
}
template<> Array<Bool> makeArray (const IPosition& shape,
                                  Bool, Bool)
{
  Array<Bool> arr(shape);
  for (uInt i=0; i<arr.size(); ++i) {
    arr.data()[i] = (i%3 == 1);
  }
  return arr;
}
template<> Array<String> makeArray (const IPosition& shape,
                                    String value, String)
{
  Array<String> arr(shape);
  for (uInt i=0; i<arr.size(); ++i) {
    arr.data()[i] = value + String::toString(i);
  }
  return arr;
}

template<typename T> void incrArray (Array<T>& arr, int incr, Bool)
{
  arr += T(incr);
}
template<> void incrArray (Array<Bool>& arr, int, Bool incr)
{
  // Shift one to the left and put new value at the end.
  if (arr.size() > 0) {
    Bool deleteIt;
    Bool* p = arr.getStorage (deleteIt);
    for (uInt i=0; i<arr.size()-1; ++i) {
      p[i] = p[i+1];
    }
    p[arr.size()-1] = incr;
    arr.putStorage (p, deleteIt);
  }
}
template<> void incrArray (Array<String>& arr, int incr, Bool)
{
  arr += String::toString(abs(incr)%10);
}

// Create all global arrays using the globally defined array shapes.
void createArrays (uInt nrow)
{
  for (int i=0; i<4; ++i) {
    IPosition shape = arrShapes[i].concatenate(IPosition(1,nrow));
    BoolArrays[i].reference (makeArray<Bool>(shape, True, True));
    uCharArrays[i].reference (makeArray<uChar>(shape, 0, 1));
    ShortArrays[i].reference (makeArray<Short>(shape, -32768, 10));
    uShortArrays[i].reference (makeArray<uShort>(shape, 0, 10));
    IntArrays[i].reference (makeArray<Int>(shape, -32768*65536, 100000));
    uIntArrays[i].reference (makeArray<uInt>(shape, 0, 100000));
    Int64Arrays[i].reference (makeArray<Int64>(shape, -6553600000L, 100000001));
    FloatArrays[i].reference (makeArray<Float>(shape, -10.5, 1));
    DoubleArrays[i].reference (makeArray<Double>(shape, -100.3, 22));
    ComplexArrays[i].reference (makeArray<Complex>(shape, Complex(-10.5,20), Complex(1,3.1)));
    DComplexArrays[i].reference (makeArray<DComplex>(shape, DComplex(-100.3,-5), DComplex(2,1.7)));
    FStringArrays[i].reference (makeArray<String>(shape, String("maxstr"), String()));
    VStringArrays[i].reference (makeArray<String>(shape, String(), String("varstr")));
  }
}

// Add 4 columns for the given data type to the table description.
// These are: Scalar, Direct Array, Indirect FixedShape Array, Indirect Array.
template<typename T> void addColDesc (TableDesc& td, const String& name,
                                      Bool addVirtual, uInt maxLength=0)
{
  // Give the scalars the group name 'scalar'.
  ScalarColumnDesc<T> s1(name+"s1", String(), String(), "scalar");
  if (maxLength > 0) s1.setMaxLength (maxLength);
  td.addColumn (s1);
  ArrayColumnDesc<T> a1(name+"a1", arrShapes[1], ColumnDesc::Direct);
  if (maxLength > 0) a1.setMaxLength (maxLength);
  td.addColumn (a1);
  ArrayColumnDesc<T> a2(name+"a2", 2, ColumnDesc::FixedShape);
  if (maxLength > 0) a2.setMaxLength (maxLength);
  td.addColumn (a2);
  ArrayColumnDesc<T> a3(name+"a3");
  if (maxLength > 0) a3.setMaxLength (maxLength);
  td.addColumn (a3);
  if (addVirtual) {
    td.addColumn (ScalarColumnDesc<T>("vt_" +name+"s1"));
    td.addColumn (ArrayColumnDesc<T>("vt_"+name+"a1"));
    td.addColumn (ArrayColumnDesc<T>("vt_"+name+"a2"));
    td.addColumn (ArrayColumnDesc<T>("vt_"+name+"a3"));
  }
}

// Check if the columns contain defined values and content.
// For TiledStMan arrays are always defined.
// Optionally check the value (used for strings).
template<typename T> void checkDefined (Table& tab, const String& name, Bool tiled,
                                        const IPosition& shape=IPosition(),
                                        Bool checkValue=False)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  for (uInt i=0; i<tab.nrow(); ++i) {
    AlwaysAssertExit (s1.isDefined(i));
    AlwaysAssertExit (a1.isDefined(i));
    AlwaysAssertExit (a2.isDefined(i));
    AlwaysAssertExit (a3.isDefined(i) == tiled);
    AlwaysAssertExit (s1.hasContent(i));
    AlwaysAssertExit (a1.hasContent(i));
    AlwaysAssertExit (a2.hasContent(i));
    AlwaysAssertExit (a3.hasContent(i) == tiled);
    if (checkValue) {
      AlwaysAssertExit (s1(i) == T());
      AlwaysAssertExit (allEQ (a1(i), Array<T>(shape)));
    }
  }
}

template<typename T> Bool testEQ (T v1, T v2)
  { return v1 == v2; }
template<> Bool testEQ (Float v1, Float v2)
  { return near (v1, v2); }
template<> Bool testEQ (Double v1, Double v2)
  { return near (v1, v2); }
template<typename T> Bool testEQ (std::complex<T> v1, std::complex<T> v2)
  { return near (v1, v2); }
template<typename T> Bool testEQ (Array<T> v1, Array<T> v2)
  { return allEQ (v1, v2); }
template<> Bool testEQ (Array<Float> v1, Array<Float> v2)
  { return allNear (v1, v2, 1e-5); }
template<> Bool testEQ (Array<Double> v1, Array<Double> v2)
  { return allNear (v1, v2, 1e-5); }
template<typename T> Bool testEQ (Array<std::complex<T>> v1, Array<std::complex<T>> v2)
  { return allNear (v1, v2, 1e-5); }

// Write per row the data into scalar and array columns of a data type.
template<typename T> void writeRows (Table& tab, const String& name,
                                     const Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  for (uInt i=0; i<tab.nrow(); ++i) {
    s1.put (i, values[0].data()[i]);
    a1.put (i, values[1][i]);
    a2.put (i, values[2][i]);
    a3.put (i, values[3][i]);
  }
}

// Check per row the data in scalar and array columns of a data type.
template<typename T> void checkRows (Table& tab, const String& name,
                                     const Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  for (uInt i=0; i<tab.nrow(); ++i) {
    AlwaysAssertExit (testEQ (s1.get(i), values[0].data()[i]));
    AlwaysAssertExit (testEQ (a1.get(i), values[1][i]));
    AlwaysAssertExit (testEQ (a2.get(i), values[2][i]));
    AlwaysAssertExit (testEQ (a3.get(i), values[3][i]));
  }
}

// Write the full column data into scalar and array columns of a data type.
template<typename T> void writeColumns (Table& tab, const String& name,
                                        Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  incrArray (values[0], -1, True);
  incrArray (values[1], 3, False);
  incrArray (values[2], -1, True);
  incrArray (values[3], 10, False);
  s1.putColumn (values[0]);
  a1.putColumn (values[1]);
  a2.putColumn (values[2]);
  a3.putColumn (values[3]);
}

// Check the full column data in scalar and array columns of a data type.
template<typename T> void checkColumns (Table& tab, const String& name,
                                        const Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  AlwaysAssertExit (testEQ (s1.getColumn(), values[0]));
  AlwaysAssertExit (testEQ (a1.getColumn(), values[1]));
  AlwaysAssertExit (testEQ (a2.getColumn(), values[2]));
  AlwaysAssertExit (testEQ (a3.getColumn(), values[3]));
}

// Write a column range data into scalar and array columns of a data type.
// The array values are updated.
template<typename T> void writeRange (Table& tab, const String& name,
                                      Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  uInt start = 1;
  uInt end   = tab.nrow() - 2;
  uInt incr  = 3;
  RefRows rows(start, end, incr);
  Slicer rowSlicer(IPosition(1,start), IPosition(1,end), IPosition(1,incr), Slicer::endIsLast);
  IPosition arrend1(values[1].shape() - 1);
  arrend1[2] = end;
  Slicer slicer1(IPosition(3,0,0,start), arrend1, IPosition(3,1,1,incr), Slicer::endIsLast);
  IPosition arrend2(values[2].shape() - 1);
  arrend2[2] = end;
  Slicer slicer2(IPosition(3,0,0,start), arrend2, IPosition(3,1,1,incr), Slicer::endIsLast);
  IPosition arrend3(values[3].shape() - 1);
  arrend3[2] = end;
  Slicer slicer3(IPosition(3,0,0,start), arrend3, IPosition(3,1,1,incr), Slicer::endIsLast);
  Array<T> arr0(values[0](rowSlicer));
  Array<T> arr1(values[1](slicer1));
  Array<T> arr2(values[2](slicer2));
  Array<T> arr3(values[3](slicer3));
  incrArray (arr0, 2, False);
  incrArray (arr1, 2, True);
  incrArray (arr2, 3, False);
  incrArray (arr3, 1, True);
  s1.putColumnRange (rowSlicer, arr0);
  a1.putColumnCells (rows, arr1);
  a2.putColumnCells (rows, arr2);
  a3.putColumnCells (rows, arr3);
}

// Check a column range data in scalar and array columns of a data type.
template<typename T> void checkRange (Table& tab, const String& name,
                                      const Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  uInt start = 1;
  uInt end   = tab.nrow() - 2;
  uInt incr  = 3;
  RefRows rows(start, end, incr);
  Slicer rowSlicer(IPosition(1,start), IPosition(1,end), IPosition(1,incr), Slicer::endIsLast);
  IPosition arrend1(values[1].shape() - 1);
  arrend1[2] = end;
  Slicer slicer1(IPosition(3,0,0,start), arrend1, IPosition(3,1,1,incr), Slicer::endIsLast);
  IPosition arrend2(values[2].shape() - 1);
  arrend2[2] = end;
  Slicer slicer2(IPosition(3,0,0,start), arrend2, IPosition(3,1,1,incr), Slicer::endIsLast);
  IPosition arrend3(values[3].shape() - 1);
  arrend3[2] = end;
  Slicer slicer3(IPosition(3,0,0,start), arrend3, IPosition(3,1,1,incr), Slicer::endIsLast);
  AlwaysAssertExit (testEQ (s1.getColumnRange(rowSlicer), values[0](rowSlicer)));
  AlwaysAssertExit (testEQ (a1.getColumnCells(rows), values[1](slicer1)));
  AlwaysAssertExit (testEQ (a2.getColumnCells(rows), values[2](slicer2)));
  AlwaysAssertExit (testEQ (a3.getColumnCells(rows), values[3](slicer3)));
}

// Write per row a data slice into array columns of a data type.
template<typename T> void writeRowSlice (Table& tab, const String& name,
                                         Array<T>* values)
{
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  Slicer slicer1(IPosition(2,0), (values[1].shape()-2).getFirst(2), IPosition(2,2,3),
                 Slicer::endIsLast);
  Slicer slicer2(IPosition(2,0), (values[2].shape()-2).getFirst(2), IPosition(2,1,2),
                 Slicer::endIsLast);
  Slicer slicer3(IPosition(2,0), (values[3].shape()-2).getFirst(2), IPosition(2,1,1),
                 Slicer::endIsLast);
  for (uInt i=0; i<tab.nrow(); ++i) {
    Array<T> arr1(values[1][i](slicer1));
    Array<T> arr2(values[2][i](slicer2));
    Array<T> arr3(values[3][i](slicer3));
    incrArray (arr1, 1, True);
    incrArray (arr2, 11, False);
    incrArray (arr3, 1, True);
    a1.putSlice (i, slicer1, arr1);
    a2.putSlice (i, slicer2, arr2);
    a3.putSlice (i, slicer3, arr3);
  }
}

// Check per row a data slice in scalar and array columns of a data type.
template<typename T> void checkRowSlice (Table& tab, const String& name,
                                         const Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  Slicer slicer1(IPosition(2,0), (values[1].shape()-2).getFirst(2), IPosition(2,2,3),
                 Slicer::endIsLast);
  Slicer slicer2(IPosition(2,0), (values[2].shape()-2).getFirst(2), IPosition(2,1,2),
                 Slicer::endIsLast);
  Slicer slicer3(IPosition(2,0), (values[3].shape()-2).getFirst(2), IPosition(2,1,1),
                 Slicer::endIsLast);
  for (uInt i=0; i<tab.nrow(); ++i) {
    AlwaysAssertExit (testEQ (s1(i), values[0].data()[i]));
    AlwaysAssertExit (testEQ (a1.getSlice(i, slicer1), values[1][i](slicer1)));
    AlwaysAssertExit (testEQ (a2.getSlice(i, slicer2), values[2][i](slicer2)));
    AlwaysAssertExit (testEQ (a3.getSlice(i, slicer3), values[3][i](slicer3)));
  }
}

// Write a column data slice into array columns of a data type.
template<typename T> void writeColumnSlice (Table& tab, const String& name,
                                            Array<T>* values)
{
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  Slicer slicer1(IPosition(2,0), (values[1].shape()-1).getFirst(2), IPosition(2,2,3),
                 Slicer::endIsLast);
  Slicer slicer2(IPosition(2,0), (values[2].shape()-1).getFirst(2), IPosition(2,1,2),
                 Slicer::endIsLast);
  Slicer slicer3(IPosition(2,0), (values[3].shape()-1).getFirst(2), IPosition(2,1,1),
                 Slicer::endIsLast);
  Slicer aslicer1(IPosition(3,0), values[1].shape()-1, IPosition(3,2,3,1),
                  Slicer::endIsLast);
  Slicer aslicer2(IPosition(3,0), values[2].shape()-1, IPosition(3,1,2,1),
                  Slicer::endIsLast);
  Slicer aslicer3(IPosition(3,0), values[3].shape()-1, IPosition(3,1,1,1),
                  Slicer::endIsLast);
  Array<T> arr1(values[1](aslicer1));
  Array<T> arr2(values[2](aslicer2));
  Array<T> arr3(values[3](aslicer3));
  incrArray (arr1, -2, True);
  incrArray (arr2, 5, False);
  incrArray (arr3, 3, True);
  a1.putColumn (slicer1, arr1);
  a2.putColumn (slicer2, arr2);
  a3.putColumn (slicer3, arr3);
}

// Check a column data slice in array columns of a data type.
template<typename T> void checkColumnSlice (Table& tab, const String& name,
                                            const Array<T>* values)
{
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  Slicer slicer1(IPosition(2,0), (values[1].shape()-1).getFirst(2), IPosition(2,2,3),
                 Slicer::endIsLast);
  Slicer slicer2(IPosition(2,0), (values[2].shape()-1).getFirst(2), IPosition(2,1,2),
                 Slicer::endIsLast);
  Slicer slicer3(IPosition(2,0), (values[3].shape()-1).getFirst(2), IPosition(2,1,1),
                 Slicer::endIsLast);
  Slicer aslicer1(IPosition(3,0), values[1].shape()-1, IPosition(3,2,3,1),
                  Slicer::endIsLast);
  Slicer aslicer2(IPosition(3,0), values[2].shape()-1, IPosition(3,1,2,1),
                  Slicer::endIsLast);
  Slicer aslicer3(IPosition(3,0), values[3].shape()-1, IPosition(3,1,1,1),
                  Slicer::endIsLast);
  AlwaysAssertExit (testEQ (a1.getColumn(slicer1), values[1](aslicer1)));
  AlwaysAssertExit (testEQ (a2.getColumn(slicer2), values[2](aslicer2)));
  AlwaysAssertExit (testEQ (a3.getColumn(slicer3), values[3](aslicer3)));
}

// Write a data slice range into array columns of a data type.
template<typename T> void writeRangeSlice (Table& tab, const String& name,
                                           const Array<T>* values)
{
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  uInt start = 1;
  uInt end   = tab.nrow() - 1;
  uInt incr  = 2;
  RefRows rows(start, end, incr);
  Slicer slicer1(IPosition(2,1), (values[1].shape()-1).getFirst(2), IPosition(2,1,2),
                 Slicer::endIsLast);
  Slicer slicer2(IPosition(2,2), (values[2].shape()-2).getFirst(2), IPosition(2,2,3),
                 Slicer::endIsLast);
  Slicer slicer3(IPosition(2,0), (values[3].shape()-3).getFirst(2), IPosition(2,1,1),
                 Slicer::endIsLast);
  IPosition arrend1(values[1].shape() - 1);
  arrend1[2] = end;
  IPosition arrend2(values[2].shape() - 2);
  arrend2[2] = end;
  IPosition arrend3(values[3].shape() - 3);
  arrend3[2] = end;
  Slicer aslicer1(IPosition(3,1,1,start), arrend1, IPosition(3,1,2,incr), Slicer::endIsLast);
  Slicer aslicer2(IPosition(3,2,2,start), arrend2, IPosition(3,2,3,incr), Slicer::endIsLast);
  Slicer aslicer3(IPosition(3,0,0,start), arrend3, IPosition(3,1,1,incr), Slicer::endIsLast);
  Array<T> arr1(values[1](aslicer1));
  Array<T> arr2(values[2](aslicer2));
  Array<T> arr3(values[3](aslicer3));
  incrArray (arr1, 3, True);
  incrArray (arr2, -11, False);
  incrArray (arr3, -4, True);
  a1.getColumnCells(rows, slicer1, arr1);
  a2.getColumnCells(rows, slicer2, arr2);
  a3.getColumnCells(rows, slicer3, arr3);
}

// Check a data slice range in array columns of a data type.
template<typename T> void checkRangeSlice (Table& tab, const String& name,
                                           const Array<T>* values)
{
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  uInt start = 1;
  uInt end   = tab.nrow() - 1;
  uInt incr  = 2;
  RefRows rows(start, end, incr);
  Slicer slicer1(IPosition(2,1), (values[1].shape()-1).getFirst(2), IPosition(2,1,2),
                 Slicer::endIsLast);
  Slicer slicer2(IPosition(2,2), (values[2].shape()-2).getFirst(2), IPosition(2,2,3),
                 Slicer::endIsLast);
  Slicer slicer3(IPosition(2,0), (values[3].shape()-3).getFirst(2), IPosition(2,1,1),
                 Slicer::endIsLast);
  IPosition arrend1(values[1].shape() - 1);
  arrend1[2] = end;
  IPosition arrend2(values[2].shape() - 2);
  arrend2[2] = end;
  IPosition arrend3(values[3].shape() - 3);
  arrend3[2] = end;
  Slicer aslicer1(IPosition(3,1,1,start), arrend1, IPosition(3,1,2,incr), Slicer::endIsLast);
  Slicer aslicer2(IPosition(3,2,2,start), arrend2, IPosition(3,2,3,incr), Slicer::endIsLast);
  Slicer aslicer3(IPosition(3,0,0,start), arrend3, IPosition(3,1,1,incr), Slicer::endIsLast);
  AlwaysAssertExit (testEQ (a1.getColumnCells(rows, slicer1), values[1](aslicer1)));
  AlwaysAssertExit (testEQ (a2.getColumnCells(rows, slicer2), values[2](aslicer2)));
  AlwaysAssertExit (testEQ (a3.getColumnCells(rows, slicer3), values[3](aslicer3)));
}

// Do a check in all kind of ways to test all get variants.
template<typename T> void checkAll (Table& tab, const String& name, const Array<T>* values)
{
  checkRows (tab, name, values);
  checkColumns (tab, name, values);
  checkRange (tab, name, values);
  checkRowSlice (tab, name, values);
  checkColumnSlice (tab, name, values);
  checkRangeSlice (tab, name, values);
}

// Bind a VirtualTaQLColumn.
void bindVirtual (SetupNewTable& newtab, const String& name)
{
  newtab.bindColumn ("vt_"+name+"s1", VirtualTaQLColumn(name+"s1"));
  newtab.bindColumn ("vt_"+name+"a1", VirtualTaQLColumn(name+"a1"));
  newtab.bindColumn ("vt_"+name+"a2", VirtualTaQLColumn(name+"a2"));
  newtab.bindColumn ("vt_"+name+"a3", VirtualTaQLColumn(name+"a3"));
}

// Create a new table and fill a few cells with an empty array.
// An empty table name defaults to tStMan_tmp.data.
Table maketab (uInt nrrow, const DataManager& stman, Bool tiled,
               const String& tabName=String(), Bool addVirtual=False)
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  addColDesc<Bool> (td, "b", addVirtual);
  addColDesc<uChar> (td, "uc", addVirtual);
  addColDesc<Short> (td, "s", addVirtual);
  addColDesc<uShort> (td, "us", addVirtual);
  addColDesc<Int> (td, "i", addVirtual);
  addColDesc<uInt> (td, "ui", addVirtual);
  addColDesc<Int64> (td, "i64", addVirtual);
  addColDesc<Float> (td, "f", addVirtual);
  addColDesc<Double> (td, "d", addVirtual);
  addColDesc<Complex> (td, "cx", addVirtual);
  addColDesc<DComplex> (td, "dcx", addVirtual);
  addColDesc<String> (td, "sv", addVirtual);           // variable length string
  addColDesc<String> (td, "sf", addVirtual, 40);       // fixed length string
  // Now create a new table from the description.
  String tname(tabName.empty() ? "tStMan_tmp.data" : tabName);
  SetupNewTable newtab(tname, td, Table::New);
  // Bind all columns to the storage manager and set the fixed shapes.
  newtab.bindAll (stman);
  // The tiled storage manager does not support scalars nor strings,
  // so use StandardStMan for them.
  if (tiled) {
    StandardStMan ssm(1000);
    newtab.bindGroup ("scalar", ssm, True);
    newtab.bindColumn ("sva1", ssm);
    newtab.bindColumn ("sva2", ssm);
    newtab.bindColumn ("sva3", ssm);
    newtab.bindColumn ("sfa1", ssm);
    newtab.bindColumn ("sfa2", ssm);
    newtab.bindColumn ("sfa3", ssm);
  }
  if (addVirtual) {
    bindVirtual (newtab, "b");
    bindVirtual (newtab, "uc");
    bindVirtual (newtab, "s");
    bindVirtual (newtab, "us");
    bindVirtual (newtab, "i");
    bindVirtual (newtab, "ui");
    bindVirtual (newtab, "i64");
    bindVirtual (newtab, "f");
    bindVirtual (newtab, "d");
    bindVirtual (newtab, "cx");
    bindVirtual (newtab, "dcx");
    bindVirtual (newtab, "sv");
    bindVirtual (newtab, "sf");
  }
  newtab.setShapeColumn("ba2", arrShapes[2]);
  newtab.setShapeColumn("uca2", arrShapes[2]);
  newtab.setShapeColumn("sa2", arrShapes[2]);
  newtab.setShapeColumn("usa2", arrShapes[2]);
  newtab.setShapeColumn("ia2", arrShapes[2]);
  newtab.setShapeColumn("uia2", arrShapes[2]);
  newtab.setShapeColumn("i64a2", arrShapes[2]);
  newtab.setShapeColumn("fa2", arrShapes[2]);
  newtab.setShapeColumn("da2", arrShapes[2]);
  newtab.setShapeColumn("cxa2", arrShapes[2]);
  newtab.setShapeColumn("dcxa2", arrShapes[2]);
  newtab.setShapeColumn("sva2", arrShapes[2]);
  newtab.setShapeColumn("sfa2", arrShapes[2]);
  Table tab(newtab, nrrow);
  // Check the columns for defined and content.
  checkDefined<Bool> (tab, "b", tiled);
  checkDefined<uChar> (tab, "uc", tiled);
  checkDefined<Short> (tab, "s", tiled);
  checkDefined<uShort> (tab, "us", tiled);
  checkDefined<Int> (tab, "i", tiled);
  checkDefined<uInt> (tab, "ui", tiled);
  checkDefined<Int64> (tab, "i64", tiled);
  checkDefined<Float> (tab, "f", tiled);
  checkDefined<Double> (tab, "d", tiled);
  checkDefined<Complex> (tab, "cx", tiled);
  checkDefined<DComplex> (tab, "dcx", tiled);
  checkDefined<String> (tab, "sv", False, arrShapes[1], True);
  checkDefined<String> (tab, "sf", False, arrShapes[1], True);
  // Write Bool arrays to avoid valgrind errors in Conversion::boolToBit.
  ScalarColumn<Bool> bs1(tab, "bs1");
  ArrayColumn<Bool>  ba1(tab, "ba1");
  ArrayColumn<Bool>  ba2(tab, "ba2");
  Array<Bool> boolArray1(arrShapes[1], False);
  Array<Bool> boolArray2(arrShapes[2], False);
  for (uInt i=0; i<nrrow; i++) {
    bs1.put (i, False);
    ba1.put (i, boolArray1);
    ba2.put (i, boolArray2);
  }
  // Put empty arrays in some columns cells and check them.
  // Do not do it for TiledStMan and if the table is used by ForwardColumn.
  if (!tiled  &&  tabName.empty()) {
    ArrayColumn<Float>    fa3(tab, "fa3");
    ArrayColumn<DComplex> dca3(tab, "dcxa3");
    fa3.put (nrrow-1, Array<Float>());
    dca3.put (nrrow-1, Array<DComplex>(IPosition(2,2,0)));
    AlwaysAssertExit (  fa3.isDefined(nrrow-1));
    AlwaysAssertExit (  dca3.isDefined(nrrow-1));
    AlwaysAssertExit (! fa3.hasContent(nrrow-1));
    AlwaysAssertExit (! dca3.hasContent(nrrow-1));
  }
  return tab;
}

// Reopen the table and check the contents again.
// This has to be done right after creating the table in function maketab.
void checknewtab (const Table& table, uInt nrrow, Bool tiled)
{
  Table tab(table);
  if (tab.isNull()) {
    tab = Table("tStMan_tmp.data");
  }
  AlwaysAssertExit (tab.nrow() == nrrow);
  // Make a subset of all except the last row, because the float and DComplex
  // column contain an empty array in the last row.
  Vector<rownr_t> rows(nrrow-1);
  indgen(rows);
  Table subtab(tab(rows));
  // Check the columns for defined and content.
  checkDefined<Bool> (tab, "b", tiled);
  checkDefined<uChar> (tab, "uc", tiled);
  checkDefined<Short> (tab, "s", tiled);
  checkDefined<uShort> (tab, "us", tiled);
  checkDefined<Int> (tab, "i", tiled);
  checkDefined<uInt> (tab, "ui", tiled);
  checkDefined<Int64> (tab, "i64", tiled);
  checkDefined<Float> (subtab, "f", tiled);
  checkDefined<Double> (tab, "d", tiled);
  checkDefined<Complex> (tab, "cx", tiled);
  checkDefined<DComplex> (subtab, "dcx", tiled);
  checkDefined<String> (tab, "sv", False, arrShapes[1], True);
  checkDefined<String> (tab, "sf", False, arrShapes[1], True);
  // Check the last row where an empty array has been put.
  ArrayColumn<Float>    fa3(tab, "fa3");
  ArrayColumn<DComplex> dca3(tab, "dcxa3");
  AlwaysAssertExit (fa3.isDefined(nrrow-1));
  AlwaysAssertExit (dca3.isDefined(nrrow-1));
  AlwaysAssertExit (fa3.hasContent(nrrow-1) == tiled);
  AlwaysAssertExit (dca3.hasContent(nrrow-1) == tiled);
}

void checktab (const Table& table)
{
  Table tab(table);
  if (tab.isNull()) {
    tab = Table("tStMan_tmp.data");
  }
  ExecFunc(checkAll, tab, String());
}


void doTest (uInt nrrow, const DataManager& stman,
             Bool keepTable, Bool tiled, const Table& refTab = Table())
{
  // Create the table (if not given).
  Table table(refTab);
  if (table.isNull()) {
    table = maketab (nrrow, stman, tiled);
  }
  if (! keepTable) {
    table = Table();        // will not be done for MemoryStMan
  }
  // Check for defined and content.
  checknewtab (table, nrrow, tiled);
  // Write the data row by row and check the result.
  cout << "  writeRows .." << endl;
  updateTable (writeRows);
  // Update the data by writing full columns.
  cout << "  writeColumns .." << endl;
  updateTable (writeColumns);
  // Update data row range.
  cout << "  writeRange .." << endl;
  updateTable (writeRange);
  // Update data slices row by row.
  cout << "  writeRowSlice .." << endl;
  updateTable (writeRowSlice);
  // Update data column slices.
  cout << "  writeColumnSlice .." << endl;
  updateTable (writeColumnSlice);
  // Update data slice ranges.
  cout << "  writeRangeSlice .." << endl;
  updateTable (writeRangeSlice);
}

void testTaQLColumns()
{
}

int main (int argc, const char* argv[])
{
  uInt nrrow = 10;
  uInt bucketSize = 1000;
  if (argc > 1) {
    istringstream istr(argv[1]);
    istr >> nrrow;
  }
  if (argc > 2) {
    istringstream istr(argv[2]);
    istr >> bucketSize;
  }
  try {
    // Define the shapes of the array columns; the first one is a scalar column.
    arrShapes[0] = IPosition();
    arrShapes[1] = IPosition(2,3,4);
    arrShapes[2] = IPosition(2,5,7);
    arrShapes[3] = IPosition(2,11,23);
    createArrays (nrrow);
    cout << "Testing StManAipsIO ..." << endl;
    StManAipsIO st1;
    doTest (nrrow, st1, False, False);
    cout << "Testing StandardStMan ..." << endl;
    StandardStMan st2(max(bucketSize,1000u));
    doTest (nrrow, st2, False, False);
    cout << "Testing IncrementalStMan ..." << endl;
    IncrementalStMan st3(max(bucketSize,5000u), False);
    doTest (nrrow, st3, False, False);
    cout << "Testing MemoryStMan ..." << endl;
    MemoryStMan st4;
    doTest (nrrow, st4, True, False);
    cout << "Testing TiledShapeStMan ..." << endl;
    // Need to be the same shape (because FixedShape columns are part of it).
    arrShapes[1] = IPosition(2,11,23);
    arrShapes[2] = IPosition(2,11,23);
    arrShapes[3] = IPosition(2,11,23);
    createArrays (nrrow);
    TiledShapeStMan st5("tiled", IPosition(3,20,20,20));
    doTest (nrrow, st5, False, True);
    {
      cout << "Testing ForwardColumnEngine ..." << endl;
      // Test ForwardColumn.
      StandardStMan stman(2000);
      Table tab = maketab (nrrow, stman, False, "tStMan_tmp.datafc");
      ForwardColumnEngine dataman(tab, "forwardcolumn");
      doTest (nrrow, dataman, True, False);
    }
    {
      cout << "Testing RefTable ..." << endl;
      StandardStMan stman(2000);
      Table tab = maketab (nrrow, stman, False);
      Vector<rownr_t> rows(tab.nrow());
      indgen (rows);
      // Note that the second argument does not matter.
      doTest (tab.nrow(), stman, True, False, tab(rows));
      // Test if underlying table is correct.
      ExecFunc(checkRows, tab, String());
    }
    {
      cout << "Testing ConcatTable ..." << endl;
      StandardStMan stman(2000);
      Table tab = maketab (nrrow, stman, False);
      Table ctab(Block<Table>(1, tab));
      // Note that the second argument does not matter.
      doTest (tab.nrow(), stman, True, False, ctab);
      // Test if underlying table is correct.
      ExecFunc(checkRows, tab, String());
    }
    {
      cout << "Testing VirtualTaQLColumn ..." << endl;
      StandardStMan stman(2000);
      Bool keepTable = False;
      Table table = maketab (nrrow, stman, keepTable, String(), True);
      updateTable (writeRows);
      ExecFunc(checkAll, table, String("vt_"));
    }
    
  } catch (const std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  }
  return 0;                           // exit with success status
}
