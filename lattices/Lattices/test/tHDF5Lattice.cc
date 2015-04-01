//# tHDF5Array.cc:  tests the HDF5Array class
//# Copyright (C) 2008
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
//#
//# $Id$

#include <casacore/lattices/Lattices/HDF5Lattice.h>
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
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/iostream.h>

using namespace casacore;

int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    {
      HDF5Lattice<Float> pa(IPosition(2,12), "tHDF5Lattice_tmp.dat");
      AlwaysAssert(pa.arrayName()=="array", AipsError);
      pa.set(10.0);
      Array<Float> arr;
      pa.getSlice(arr, IPosition(2,0), IPosition(2,12), IPosition(2,1));
      AlwaysAssert(allNear(arr, 10.0f, 1E-5), AipsError);
      indgen(arr);
      Array<Float> arr1(arr(IPosition(2,0), IPosition(2,0,11), 
			    IPosition(2,1,2)));
      pa.putSlice(arr1, IPosition(2,0), IPosition(2,1,2));
      Vector<Float> vec(10);
      indgen(vec);
      pa.putSlice(vec(IPosition(1,0), IPosition(1,9), IPosition(1,2)), 
		  IPosition(2,1,1), IPosition(2,2,1));
    }
    {
      HDF5Lattice<Float> pa("tHDF5Lattice_tmp.dat");
      AlwaysAssert(pa.shape().isEqual(IPosition(2,12)), AipsError);
      Array<Float> arr;
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
      AlwaysAssert(pa.name(True) == "tHDF5Lattice_tmp.dat", AipsError);
      AlwaysAssert(pa.isPersistent(), AipsError);
      AlwaysAssert(pa.isPaged(), AipsError);
      AlwaysAssert(pa.isWritable(), AipsError);
    }
    {
      HDF5Lattice<Int> scratch(IPosition(3,9));
      LatticeIterator<Int> li(scratch, IPosition(3,1,1,9));
      Int i = 0;
      for (li.reset(); !li.atEnd(); li++, i++) {
	li.woCursor() = i;
      }
      COWPtr<Array<Int> > ptrM;
      scratch.getSlice(ptrM, IPosition(3,0), IPosition(3,9,9,1), 
		       IPosition(3,1), True);
      AlwaysAssert(ptrM->shape().isEqual(IPosition(2,9)), AipsError);
      Array<Int> expectedResult(IPosition(2,9));
      indgen(expectedResult);
      AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
      ptrM.rwRef() = 0;
      AlwaysAssert(allEQ(*ptrM, 0), AipsError);
      Slicer sl(IPosition(3,0,0,5), IPosition(3,9,9,1), IPosition(3,1));

      scratch.getSlice(ptrM, sl, True);
      AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
    }
    {
      const IPosition latticeShape(4, 128, 128, 4, 32);
      HDF5Lattice<Float> pa(latticeShape, "tHDF5Lattice_tmp_1.dat");
      AlwaysAssert(pa.tileShape().isEqual(pa.niceCursorShape()),
		   AipsError);
      Array<Float> arr(IPosition(4,1,1,4,32));
      Slicer sl(IPosition(4,0), IPosition(4,1,1,4,32));
      pa.clearCache();
      pa.setCacheSizeFromPath(arr.shape(), IPosition(4,0),
			      pa.tileShape() - 1, 
			      IPosition(4,0,1,2,3));
      pa.getSlice(arr, sl);
      pa.showCacheStatistics(cout);

      HDF5Lattice<Float> pa1(TiledShape(latticeShape,IPosition(4,16,16,4,32)),
			     "tHDF5Lattice_tmp.dat");
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
      HDF5Lattice<Float> pa2(lat2Shape, pa1.file(), "array2");
      arr.resize(lat2Shape);
      indgen(arr);
      pa2.putSlice(arr, IPosition(4,0));
      
      IPosition lat3Shape = IPosition(2,16);
      HDF5Lattice<Int> pa3(TiledShape(lat3Shape,lat3Shape),
			   pa1.file(), "IntHDF5Lattice");
      Array<Int> iarr(lat3Shape);
      indgen(iarr);
      pa3.putSlice(iarr, IPosition(2,0));
    }
    {
      HDF5Lattice<Float> pa1("tHDF5Lattice_tmp.dat");
      AlwaysAssert(pa1.shape().isEqual(IPosition(4,128,128,4,32)), AipsError);
      HDF5Lattice<Float> pa2(pa1.file(), "array2");
      AlwaysAssert(pa2.shape().isEqual(IPosition(4,16)), AipsError);
      HDF5Lattice<Int> pa3(pa1.file(), "IntHDF5Lattice");
      AlwaysAssert(pa3.shape().isEqual(IPosition(2,16)), AipsError);
      Array<Int> iarr(pa3.shape()), expected(pa3.shape());
      pa3.setMaximumCacheSize(256*256);
      indgen(expected);
      pa3.getSlice(iarr, IPosition(2,0), IPosition(2,16), IPosition(2,1));
      AlwaysAssert(allEQ(iarr, expected), AipsError);
      {
	HDF5Lattice<Int> pa4(pa3);
	AlwaysAssert(pa4.shape().isEqual(IPosition(2,16)), AipsError);
	iarr = 0;
	pa4.getSlice(iarr, IPosition(2,0), IPosition(2,16), IPosition(2,1));
	AlwaysAssert(allEQ(iarr, expected), AipsError);
	AlwaysAssert(pa4.ok() == True, AipsError);
      }
    }
    {
      const IPosition latticeShape(4, 4, 16, 15, 8);
      HDF5Lattice<Float> pa(TiledShape(latticeShape, IPosition(4,2,8,8,3)),
			    "tHDF5Lattice_tmp_1.dat", "data", "group1");
      AlwaysAssertExit(pa.arrayName()=="data");
      Array<Float> arr(latticeShape);
      indgen(arr);
      pa.put (arr);
      AlwaysAssertExit (allEQ(pa.get(), arr));
      pa += pa;
      AlwaysAssertExit (allEQ(pa.get(), float(2)*arr));
    }
    {
      HDF5Lattice<Float> pa("tHDF5Lattice_tmp_1.dat", "data", "group1");
      AlwaysAssertExit(pa.arrayName()=="data");
      Array<Float> arr(pa.shape());
      indgen(arr);
      AlwaysAssertExit (allEQ(pa.get(), float(2)*arr));
    }
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout<< "OK"<< endl;
  return 0;
}
