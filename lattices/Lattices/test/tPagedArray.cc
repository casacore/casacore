//# tPagedArray.cc:  tests the PagedArray class
//# Copyright (C) 1997,1999,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
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

#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>


// Remove the dirname from the table name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

void testTempClose()
{
  PagedArray<int32_t> scratch(IPosition(3,64,64,257), "tPagedArray_tmp.scr");
  scratch.tempClose();
  AlwaysAssertExit (scratch.ok());
  IPosition shape(3,1);    
  shape(2) = scratch.shape()(2);
  AlwaysAssertExit (scratch.ok());
  AlwaysAssertExit (scratch.isWritable());
  scratch.tempClose();
  LatticeIterator<int32_t> li(scratch, shape);
  scratch.tempClose();
  int32_t i = 0;
  for (li.reset(); !li.atEnd(); li++, i++) {
    li.woCursor() = i;
  }
  shape = scratch.shape();
  shape(2) = 1;
  COWPtr<Array<int32_t> > ptrM;
  scratch.tempClose();
  scratch.getSlice(ptrM, IPosition(3,0), shape, IPosition(3,1), false);
  scratch.reopen();
  AlwaysAssert(ptrM->shape().isEqual(shape), AipsError);
  Array<int32_t> expectedResult(shape);
  indgen(expectedResult);
  AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
  ptrM.rwRef() = 0;
  AlwaysAssert(allEQ(*ptrM, 0), AipsError);
  Slicer sl(IPosition(3,0,0,5), shape, IPosition(3,1));
  scratch.getSlice(ptrM, sl, false);
  AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
  scratch.set(0);
  scratch.putAt (7, IPosition(3,7));
  AlwaysAssert(scratch.getAt(IPosition(3,0)) == 0, AipsError);
  AlwaysAssert(scratch.getAt(IPosition(3,7)) == 7, AipsError);
}


int main() {
  try {
    {
      PagedArray<float> pa(IPosition(2,12), "tPagedArray_tmp.table");
      pa.set(10.0);
      Array<float> arr;
      pa.getSlice(arr, IPosition(2,0), IPosition(2,12), IPosition(2,1));
      AlwaysAssert(allNear(arr, 10.0f, 1E-5), AipsError);
      indgen(arr);
      Array<float> arr1(arr(IPosition(2,0), IPosition(2,0,11), 
			    IPosition(2,1,2)));
      pa.putSlice(arr1, IPosition(2,0), IPosition(2,1,2));
      Vector<float> vec(10);
      indgen(vec);
      pa.putSlice(vec(IPosition(1,0), IPosition(1,9), IPosition(1,2)), 
		  IPosition(2,1,1), IPosition(2,2,1));
    }
    {
      PagedArray<float> pa("tPagedArray_tmp.table");
      AlwaysAssert(pa.shape().isEqual(IPosition(2,12)), AipsError);
      Array<float> arr;
      Slicer sl(IPosition(2,0), IPosition(2,12));
      pa.getSlice(arr, sl);
      AlwaysAssert(near(pa(IPosition(2,0)), 0.0f), AipsError);
      AlwaysAssert(near(pa(IPosition(2,0,1)), 10.0f), AipsError);
      AlwaysAssert(near(pa.getAt(IPosition(2,0,2)), 24.0f), AipsError);
      AlwaysAssert(near(pa.getAt(IPosition(2,1,1)), 0.0f), AipsError);
      AlwaysAssert(near(pa(IPosition(2,2,1)), 10.0f), AipsError);
      AlwaysAssert(near(pa(IPosition(2,3,1)), 2.0f), AipsError);
      pa.putAt (99.0, IPosition(2,11));
      pa.putAt (98.0f, IPosition(2,11,10));
      AlwaysAssert(removeDir(pa.tableName()) == "tPagedArray_tmp.table", AipsError);
      AlwaysAssert(pa.name(true) == "tPagedArray_tmp.table", AipsError);
      AlwaysAssert(pa.isPersistent(), AipsError);
      AlwaysAssert(pa.isPaged(), AipsError);
      AlwaysAssert(pa.isWritable(), AipsError);
      AlwaysAssert(pa.columnName() == PagedArray<float>::defaultColumn(), 
		   AipsError);
      AlwaysAssert(pa.rowNumber() == PagedArray<float>::defaultRow(),
		   AipsError);
    }
    {
      Table pagedTable("tPagedArray_tmp.table");
      PagedArray<float> pa(pagedTable);
      AlwaysAssert(near(pa(IPosition(2,11)), 99.0f), AipsError);
      AlwaysAssert(near(pa(IPosition(2,11, 10)), 98.0f), AipsError);
    }
    {
      PagedArray<int32_t> scratch(IPosition(3,9));
      LatticeIterator<int32_t> li(scratch, IPosition(3,1,1,9));
      int32_t i = 0;
      for (li.reset(); !li.atEnd(); li++, i++) {
	li.woCursor() = i;
      }
      COWPtr<Array<int32_t> > ptrM;
      scratch.getSlice(ptrM, IPosition(3,0), IPosition(3,9,9,1), 
		       IPosition(3,1), true);
      AlwaysAssert(ptrM->shape().isEqual(IPosition(2,9)), AipsError);
      Array<int32_t> expectedResult(IPosition(2,9));
      indgen(expectedResult);
      AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
      ptrM.rwRef() = 0;
      AlwaysAssert(allEQ(*ptrM, 0), AipsError);
      Slicer sl(IPosition(3,0,0,5), IPosition(3,9,9,1), IPosition(3,1));

      scratch.getSlice(ptrM, sl, true);
      AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
      scratch.resize(IPosition(3,8));
      AlwaysAssert(scratch.shape().isEqual(IPosition(3,8)), AipsError);
      scratch.set(0);
      scratch.putAt (7, IPosition(3,7));
      AlwaysAssert(scratch.getAt(IPosition(3,0)) == 0, AipsError);
      AlwaysAssert(scratch.getAt(IPosition(3,7)) == 7, AipsError);
    }
    {
      SetupNewTable arraySetup("tPagedArray_tmp_1.table", 
			       TableDesc(), Table::Scratch);
      Table arrayTable(arraySetup);
      const IPosition latticeShape(4, 128, 128, 4, 32);
      PagedArray<float> pa(latticeShape, arrayTable);
      AlwaysAssert(pa.tileShape().isEqual(pa.niceCursorShape()),
		   AipsError);
      Array<float> arr(IPosition(4,1,1,4,32));
      Slicer sl(IPosition(4,0), IPosition(4,1,1,4,32));
      pa.clearCache();
      pa.setCacheSizeFromPath(arr.shape(), IPosition(4,0),
			      pa.tileShape() - 1, 
			      IPosition(4,0,1,2,3));
      pa.getSlice(arr, sl);
      pa.showCacheStatistics(cout);

      SetupNewTable array1Setup("tPagedArray_tmp.table", 
				TableDesc(), Table::New);
      Table array1Table(array1Setup);
      PagedArray<float> pa1(TiledShape(latticeShape,IPosition(4,16,16,4,32)),
			    array1Table);
      AlwaysAssert(pa1.tileShape().isEqual(IPosition(4,16,16,4,32)), 
		   AipsError);
      pa1.clearCache();
      pa1.setCacheSizeFromPath(arr.shape(), IPosition(4,0),
			      IPosition(4,16,16,4,32) - 1, 
			       IPosition(4,0,1,2,3));
      pa1.getSlice(arr, sl);
      pa1.showCacheStatistics(cout);
      pa = pa1;
      AlwaysAssert(pa.tileShape().isEqual(IPosition(4,16,16,4,32)), AipsError);
      arr = 9.0f;
      pa.putSlice(arr, IPosition(4,0));
      arr = 0.0f;
      pa1.getSlice(arr, sl);
      AlwaysAssert(allNear(arr, 9.0f, 1E-5), AipsError);

      IPosition lat2Shape = IPosition(4,16);
      PagedArray<float> pa2(lat2Shape, array1Table, 
			    PagedArray<float>::defaultColumn(), 2);
      arr.resize(lat2Shape);
      indgen(arr);
      pa2.putSlice(arr, IPosition(4,0));
      
      IPosition lat3Shape = IPosition(2,16);
      PagedArray<int32_t> pa3(TiledShape(lat3Shape,lat3Shape),
			  array1Table, "IntPagedArray", 1);
      Array<int32_t> iarr(lat3Shape);
      indgen(iarr);
      pa3.putSlice(iarr, IPosition(2,0));
    }
    {
      Table file("tPagedArray_tmp.table");
      PagedArray<float> pa1(file);
      AlwaysAssert(pa1.shape().isEqual(IPosition(4,128,128,4,32)), AipsError);
      PagedArray<float> pa2(file, PagedArray<float>::defaultColumn(), 2);
      AlwaysAssert(pa2.shape().isEqual(IPosition(4,16)), AipsError);
      PagedArray<int32_t> pa3(file, "IntPagedArray", 1);
      AlwaysAssert(pa3.shape().isEqual(IPosition(2,16)), AipsError);
      Array<int32_t> iarr(pa3.shape()), expected(pa3.shape());
      pa3.setMaximumCacheSize(256*256);
      indgen(expected);
      pa3.getSlice(iarr, IPosition(2,0), IPosition(2,16), IPosition(2,1));
      AlwaysAssert(allEQ(iarr, expected), AipsError);
      {
	PagedArray<int32_t> pa4(pa3);
	AlwaysAssert(pa4.shape().isEqual(IPosition(2,16)), AipsError);
	iarr = 0;
	pa4.getSlice(iarr, IPosition(2,0), IPosition(2,16), IPosition(2,1));
	AlwaysAssert(allEQ(iarr, expected), AipsError);
	AlwaysAssert(pa4.ok() == true, AipsError);
      }
      AlwaysAssert(pa3.maximumCacheSize() == 65536, AipsError);

      pa3.resize(IPosition(2,8));
      pa3.set(0);
      pa3.putAt (7, IPosition(2,7));
      AlwaysAssert(pa3.getAt(IPosition(2,7)) == 7, AipsError);
      AlwaysAssert(pa3.getAt(IPosition(2,0)) == 0, AipsError);
      AlwaysAssert(pa3.shape().isEqual(IPosition(2,8)), AipsError);
    }
    {
      SetupNewTable arraySetup("tPagedArray_tmp_1.table", 
			       TableDesc(), Table::New);
      Table arrayTable(arraySetup);
      const IPosition latticeShape(4, 4, 16, 15, 8);
      PagedArray<float> pa(TiledShape(latticeShape, IPosition(4,2,8,8,3)),
				      arrayTable);
      Array<float> arr(latticeShape);
      indgen(arr);
      pa.put (arr);
      AlwaysAssertExit (allEQ(pa.get(), arr));
      pa += pa;
      AlwaysAssertExit (allEQ(pa.get(), float(2)*arr));
    }
    {
      PagedArray<float> pa("tPagedArray_tmp_1.table");
      Array<float> arr(pa.shape());
      indgen(arr);
      AlwaysAssertExit (allEQ(pa.get(), float(2)*arr));
    }
    testTempClose();
  } catch (std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  } 
  cout<< "OK"<< endl;
  return 0;
}
