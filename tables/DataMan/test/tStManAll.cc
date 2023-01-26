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
  funcName <bool> (tab, prefix+"b", BoolArrays); \
  funcName <unsigned char> (tab, prefix+"uc", uCharArrays); \
  funcName <int16_t> (tab, prefix+"s", ShortArrays); \
  funcName <uint16_t> (tab, prefix+"us", uShortArrays); \
  funcName <int32_t> (tab, prefix+"i", IntArrays); \
  funcName <uint32_t> (tab, prefix+"ui", uIntArrays); \
  funcName <int64_t> (tab, prefix+"i64", Int64Arrays); \
  funcName <float> (tab, prefix+"f", FloatArrays); \
  funcName <double> (tab, prefix+"d", DoubleArrays); \
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
Array<bool> BoolArrays[4];
Array<unsigned char> uCharArrays[4];
Array<int16_t> ShortArrays[4];
Array<uint16_t> uShortArrays[4];
Array<int32_t> IntArrays[4];
Array<uint32_t> uIntArrays[4];
Array<int64_t> Int64Arrays[4];
Array<float> FloatArrays[4];
Array<double> DoubleArrays[4];
Array<Complex> ComplexArrays[4];
Array<DComplex> DComplexArrays[4];
Array<String> FStringArrays[4];
Array<String> VStringArrays[4];


// Create an array and fill it using start value and increment.
// Note that a bool andf String array are specialized below.
template<typename T> Array<T> makeArray (const IPosition& shape,
                                         T value, T incr)
{
  Array<T> arr(shape);
  indgen(arr, value, incr);
  return arr;
}
template<> Array<bool> makeArray (const IPosition& shape,
                                  bool, bool)
{
  Array<bool> arr(shape);
  for (uint32_t i=0; i<arr.size(); ++i) {
    arr.data()[i] = (i%3 == 1);
  }
  return arr;
}
template<> Array<String> makeArray (const IPosition& shape,
                                    String value, String)
{
  Array<String> arr(shape);
  for (uint32_t i=0; i<arr.size(); ++i) {
    arr.data()[i] = value + String::toString(i);
  }
  return arr;
}

template<typename T> void incrArray (Array<T>& arr, int incr, bool)
{
  arr += T(incr);
}
template<> void incrArray (Array<bool>& arr, int, bool incr)
{
  // Shift one to the left and put new value at the end.
  if (arr.size() > 0) {
    bool deleteIt;
    bool* p = arr.getStorage (deleteIt);
    for (uint32_t i=0; i<arr.size()-1; ++i) {
      p[i] = p[i+1];
    }
    p[arr.size()-1] = incr;
    arr.putStorage (p, deleteIt);
  }
}
template<> void incrArray (Array<String>& arr, int incr, bool)
{
  arr += String::toString(abs(incr)%10);
}

// Create all global arrays using the globally defined array shapes.
void createArrays (uint32_t nrow)
{
  for (int i=0; i<4; ++i) {
    IPosition shape = arrShapes[i].concatenate(IPosition(1,nrow));
    BoolArrays[i].reference (makeArray<bool>(shape, true, true));
    uCharArrays[i].reference (makeArray<unsigned char>(shape, 0, 1));
    ShortArrays[i].reference (makeArray<int16_t>(shape, -32768, 10));
    uShortArrays[i].reference (makeArray<uint16_t>(shape, 0, 10));
    IntArrays[i].reference (makeArray<int32_t>(shape, -32768*65536, 100000));
    uIntArrays[i].reference (makeArray<uint32_t>(shape, 0, 100000));
    Int64Arrays[i].reference (makeArray<int64_t>(shape, -6553600000L, 100000001));
    FloatArrays[i].reference (makeArray<float>(shape, -10.5, 1));
    DoubleArrays[i].reference (makeArray<double>(shape, -100.3, 22));
    ComplexArrays[i].reference (makeArray<Complex>(shape, Complex(-10.5,20), Complex(1,3.1)));
    DComplexArrays[i].reference (makeArray<DComplex>(shape, DComplex(-100.3,-5), DComplex(2,1.7)));
    FStringArrays[i].reference (makeArray<String>(shape, String("maxstr"), String()));
    VStringArrays[i].reference (makeArray<String>(shape, String(), String("varstr")));
  }
}

// Add 4 columns for the given data type to the table description.
// These are: Scalar, Direct Array, Indirect FixedShape Array, Indirect Array.
template<typename T> void addColDesc (TableDesc& td, const String& name,
                                      bool addVirtual, uint32_t maxLength=0)
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
template<typename T> void checkDefined (Table& tab, const String& name, bool tiled,
                                        const IPosition& shape=IPosition(),
                                        bool checkValue=false)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  for (uint32_t i=0; i<tab.nrow(); ++i) {
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

template<typename T> bool testEQ (T v1, T v2)
  { return v1 == v2; }
template<> bool testEQ (float v1, float v2)
  { return near (v1, v2); }
template<> bool testEQ (double v1, double v2)
  { return near (v1, v2); }
template<typename T> bool testEQ (std::complex<T> v1, std::complex<T> v2)
  { return near (v1, v2); }
template<typename T> bool testEQ (Array<T> v1, Array<T> v2)
  { return allEQ (v1, v2); }
template<> bool testEQ (Array<float> v1, Array<float> v2)
  { return allNear (v1, v2, 1e-5); }
template<> bool testEQ (Array<double> v1, Array<double> v2)
  { return allNear (v1, v2, 1e-5); }
template<typename T> bool testEQ (Array<std::complex<T>> v1, Array<std::complex<T>> v2)
  { return allNear (v1, v2, 1e-5); }

// Write per row the data into scalar and array columns of a data type.
template<typename T> void writeRows (Table& tab, const String& name,
                                     const Array<T>* values)
{
  ScalarColumn<T> s1(tab, name+"s1");
  ArrayColumn<T>  a1(tab, name+"a1");
  ArrayColumn<T>  a2(tab, name+"a2");
  ArrayColumn<T>  a3(tab, name+"a3");
  for (uint32_t i=0; i<tab.nrow(); ++i) {
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
  for (uint32_t i=0; i<tab.nrow(); ++i) {
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
  incrArray (values[0], -1, true);
  incrArray (values[1], 3, false);
  incrArray (values[2], -1, true);
  incrArray (values[3], 10, false);
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
  uint32_t start = 1;
  uint32_t end   = tab.nrow() - 2;
  uint32_t incr  = 3;
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
  incrArray (arr0, 2, false);
  incrArray (arr1, 2, true);
  incrArray (arr2, 3, false);
  incrArray (arr3, 1, true);
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
  uint32_t start = 1;
  uint32_t end   = tab.nrow() - 2;
  uint32_t incr  = 3;
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
  for (uint32_t i=0; i<tab.nrow(); ++i) {
    Array<T> arr1(values[1][i](slicer1));
    Array<T> arr2(values[2][i](slicer2));
    Array<T> arr3(values[3][i](slicer3));
    incrArray (arr1, 1, true);
    incrArray (arr2, 11, false);
    incrArray (arr3, 1, true);
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
  for (uint32_t i=0; i<tab.nrow(); ++i) {
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
  incrArray (arr1, -2, true);
  incrArray (arr2, 5, false);
  incrArray (arr3, 3, true);
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
  uint32_t start = 1;
  uint32_t end   = tab.nrow() - 1;
  uint32_t incr  = 2;
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
  incrArray (arr1, 3, true);
  incrArray (arr2, -11, false);
  incrArray (arr3, -4, true);
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
  uint32_t start = 1;
  uint32_t end   = tab.nrow() - 1;
  uint32_t incr  = 2;
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
Table maketab (uint32_t nrrow, const DataManager& stman, bool tiled,
               const String& tabName=String(), bool addVirtual=false)
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  addColDesc<bool> (td, "b", addVirtual);
  addColDesc<unsigned char> (td, "uc", addVirtual);
  addColDesc<int16_t> (td, "s", addVirtual);
  addColDesc<uint16_t> (td, "us", addVirtual);
  addColDesc<int32_t> (td, "i", addVirtual);
  addColDesc<uint32_t> (td, "ui", addVirtual);
  addColDesc<int64_t> (td, "i64", addVirtual);
  addColDesc<float> (td, "f", addVirtual);
  addColDesc<double> (td, "d", addVirtual);
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
    newtab.bindGroup ("scalar", ssm, true);
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
  checkDefined<bool> (tab, "b", tiled);
  checkDefined<unsigned char> (tab, "uc", tiled);
  checkDefined<int16_t> (tab, "s", tiled);
  checkDefined<uint16_t> (tab, "us", tiled);
  checkDefined<int32_t> (tab, "i", tiled);
  checkDefined<uint32_t> (tab, "ui", tiled);
  checkDefined<int64_t> (tab, "i64", tiled);
  checkDefined<float> (tab, "f", tiled);
  checkDefined<double> (tab, "d", tiled);
  checkDefined<Complex> (tab, "cx", tiled);
  checkDefined<DComplex> (tab, "dcx", tiled);
  checkDefined<String> (tab, "sv", false, arrShapes[1], true);
  checkDefined<String> (tab, "sf", false, arrShapes[1], true);
  // Write bool arrays to avoid valgrind errors in Conversion::boolToBit.
  ScalarColumn<bool> bs1(tab, "bs1");
  ArrayColumn<bool>  ba1(tab, "ba1");
  ArrayColumn<bool>  ba2(tab, "ba2");
  Array<bool> boolArray1(arrShapes[1], false);
  Array<bool> boolArray2(arrShapes[2], false);
  for (uint32_t i=0; i<nrrow; i++) {
    bs1.put (i, false);
    ba1.put (i, boolArray1);
    ba2.put (i, boolArray2);
  }
  // Put empty arrays in some columns cells and check them.
  // Do not do it for TiledStMan and if the table is used by ForwardColumn.
  if (!tiled  &&  tabName.empty()) {
    ArrayColumn<float>    fa3(tab, "fa3");
    ArrayColumn<DComplex> dca3(tab, "dcxa3");
    fa3.put (nrrow-1, Array<float>());
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
void checknewtab (const Table& table, uint32_t nrrow, bool tiled)
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
  checkDefined<bool> (tab, "b", tiled);
  checkDefined<unsigned char> (tab, "uc", tiled);
  checkDefined<int16_t> (tab, "s", tiled);
  checkDefined<uint16_t> (tab, "us", tiled);
  checkDefined<int32_t> (tab, "i", tiled);
  checkDefined<uint32_t> (tab, "ui", tiled);
  checkDefined<int64_t> (tab, "i64", tiled);
  checkDefined<float> (subtab, "f", tiled);
  checkDefined<double> (tab, "d", tiled);
  checkDefined<Complex> (tab, "cx", tiled);
  checkDefined<DComplex> (subtab, "dcx", tiled);
  checkDefined<String> (tab, "sv", false, arrShapes[1], true);
  checkDefined<String> (tab, "sf", false, arrShapes[1], true);
  // Check the last row where an empty array has been put.
  ArrayColumn<float>    fa3(tab, "fa3");
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


void doTest (uint32_t nrrow, const DataManager& stman,
             bool keepTable, bool tiled, const Table& refTab = Table())
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
  uint32_t nrrow = 10;
  uint32_t bucketSize = 1000;
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
    doTest (nrrow, st1, false, false);
    cout << "Testing StandardStMan ..." << endl;
    StandardStMan st2(max(bucketSize,1000u));
    doTest (nrrow, st2, false, false);
    cout << "Testing IncrementalStMan ..." << endl;
    IncrementalStMan st3(max(bucketSize,5000u), false);
    doTest (nrrow, st3, false, false);
    cout << "Testing MemoryStMan ..." << endl;
    MemoryStMan st4;
    doTest (nrrow, st4, true, false);
    cout << "Testing TiledShapeStMan ..." << endl;
    // Need to be the same shape (because FixedShape columns are part of it).
    arrShapes[1] = IPosition(2,11,23);
    arrShapes[2] = IPosition(2,11,23);
    arrShapes[3] = IPosition(2,11,23);
    createArrays (nrrow);
    TiledShapeStMan st5("tiled", IPosition(3,20,20,20));
    doTest (nrrow, st5, false, true);
    {
      cout << "Testing ForwardColumnEngine ..." << endl;
      // Test ForwardColumn.
      StandardStMan stman(2000);
      Table tab = maketab (nrrow, stman, false, "tStMan_tmp.datafc");
      ForwardColumnEngine dataman(tab, "forwardcolumn");
      doTest (nrrow, dataman, true, false);
    }
    {
      cout << "Testing RefTable ..." << endl;
      StandardStMan stman(2000);
      Table tab = maketab (nrrow, stman, false);
      Vector<rownr_t> rows(tab.nrow());
      indgen (rows);
      // Note that the second argument does not matter.
      doTest (tab.nrow(), stman, true, false, tab(rows));
      // Test if underlying table is correct.
      ExecFunc(checkRows, tab, String());
    }
    {
      cout << "Testing ConcatTable ..." << endl;
      StandardStMan stman(2000);
      Table tab = maketab (nrrow, stman, false);
      Table ctab(Block<Table>(1, tab));
      // Note that the second argument does not matter.
      doTest (tab.nrow(), stman, true, false, ctab);
      // Test if underlying table is correct.
      ExecFunc(checkRows, tab, String());
    }
    {
      cout << "Testing VirtualTaQLColumn ..." << endl;
      StandardStMan stman(2000);
      bool keepTable = false;
      Table table = maketab (nrrow, stman, keepTable, String(), true);
      updateTable (writeRows);
      ExecFunc(checkAll, table, String("vt_"));
    }
    
  } catch (const std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  }
  return 0;                           // exit with success status
}
