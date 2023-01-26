//# tHDF5DataSet.cc: Test program for class HDF5DataSet
//# Copyright (C) 2008
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

#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
 
using namespace casacore;

// Make a compound of a Complex, int32_t and float[2].
struct Data {
  Complex f1;
  int32_t     f2;
  float   f3[2];
};

void testCompound()
{
  std::vector<String> names(3);
  std::vector<HDF5DataType> types(3);
  names[0] = "f1";
  names[1] = "f2";
  names[2] = "f3";
  types[0] = HDF5DataType((Complex*)0);
  types[1] = HDF5DataType((int32_t*)0);
  types[2] = HDF5DataType(HDF5DataType((float*)0), IPosition(1,2));
  HDF5DataType dtcom(names, types);
  IPosition shape(1,3);
  {
    // Create the file.
    HDF5File file("tHDF5DataSet_tmp", ByteIO::New);
    // Create a data set in it.
    IPosition ts(1,3);
    HDF5DataSet dset(file, "array", shape, ts, dtcom);
    AlwaysAssertExit (dset.getName() == "array");
    AlwaysAssertExit (dset.shape() == shape);
    AlwaysAssertExit (dset.tileShape() == shape);
    AlwaysAssertExit (HDF5DataSet::getDataType (file, "array") == TpRecord);
    // Put 3 rows.
    for (int i=0; i<3; ++i) {
      Vector<Data> data(1);;
      data[0].f1 = Complex(i-10., i+10.);
      data[0].f2 = i;
      data[0].f3[0] = i+100.;
      data[0].f3[1] = i-100.;
      dset.put (Slicer(IPosition(1,i), IPosition(1,1)), data);
    }
  }
  {
    // Open the file and data set.
    HDF5File file("tHDF5DataSet_tmp", ByteIO::Old);
    HDF5DataSet dset(file, "array", dtcom);
    AlwaysAssertExit (dset.getName() == "array");
    AlwaysAssertExit (dset.shape() == shape);
    AlwaysAssertExit (dset.tileShape() == shape);
    // Set the cache size in chunks.
    dset.setCacheSize (10);
    Array<Data> ires(shape);
    dset.get (Slicer(IPosition(1,0), shape), ires);
    AlwaysAssertExit (int32_t(ires.size()) == shape.product());
    for (uint32_t i=0; i<ires.size(); ++i) {
      AlwaysAssertExit (near(ires.data()[i].f1, Complex(i-10., i+10.)));
      AlwaysAssertExit (ires.data()[i].f2 == int32_t(i));
      AlwaysAssertExit (ires.data()[i].f3[0] == i+100.);
      AlwaysAssertExit (ires.data()[i].f3[1] == i-100.);
    }
  }
}

int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    IPosition shape(2,5,6);
    IPosition ts(shape);
    Array<int32_t> iarr(shape);
    indgen(iarr);
    {
      // Create the file.
      HDF5File file("tHDF5DataSet_tmp", ByteIO::New);
      // Create a data set in it.
      HDF5DataSet dset(file, "array", IPosition(2,0,shape[1]), ts, (int32_t*)0);
      AlwaysAssertExit (dset.getName() == "array");
      AlwaysAssertExit (dset.shape() == IPosition(2,0,shape[1]));
      AlwaysAssertExit (dset.tileShape() == shape);
      dset.extend (shape);
      AlwaysAssertExit (dset.shape() == shape);
      AlwaysAssertExit (dset.tileShape() == shape);
      dset.put (Slicer(IPosition(2,0), shape), iarr);
      AlwaysAssertExit (HDF5DataSet::getDataType (file, "array") == TpInt);
    }
    {
      // Open the file and data set.
      HDF5File file("tHDF5DataSet_tmp", ByteIO::Old);
      HDF5DataSet dset(file, "array", (int32_t*)0);
      AlwaysAssertExit (dset.getName() == "array");
      AlwaysAssertExit (dset.shape() == shape);
      AlwaysAssertExit (dset.tileShape() == shape);
      // Set the cache size in chunks.
      dset.setCacheSize (10);
      Array<int32_t> ires(shape);
      dset.get (Slicer(IPosition(2,0), shape), ires);
      AlwaysAssertExit (allEQ(iarr, ires));
      Slicer section(IPosition(2,1), IPosition(2,2), IPosition(2,2));
      Array<int32_t> ires2(section.length());
      dset.get (section, ires2.data());
      cout<< ires2<< iarr(section)<<endl;
      //      AlwaysAssertExit (allEQ(iarr(section), ires2));
    }
    {
      // Create another data set with another tile shape.
      HDF5File file("tHDF5DataSet_tmp", ByteIO::Update);
      IPosition ash(3,5,6,2);
      IPosition tsh(3,4,5,2);
      {
	HDF5DataSet dset(file, "carray", ash, tsh, (Complex*)0);
	AlwaysAssertExit (dset.shape() == ash);
	AlwaysAssertExit (dset.tileShape() == tsh);
      }
      {
	HDF5DataSet dset(file, "carray", (Complex*)0);
	AlwaysAssertExit (dset.shape() == ash);
	AlwaysAssertExit (dset.tileShape() == tsh);
      }
    }
    {
      // Yet another with boolean values.
      HDF5File file("tHDF5DataSet_tmp", ByteIO::Update);
      HDF5DataSet dset(file, "mask", shape, ts, (bool*)0);
      AlwaysAssertExit (dset.shape() == shape);
      AlwaysAssertExit (dset.tileShape() == shape);
    }
    // Test a compound data type.
    testCompound();

  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
