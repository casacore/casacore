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
//#
//# $Id$

#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
 
using namespace casacore;

int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    IPosition shape(2,5,6);
    IPosition ts(shape);
    Array<Int> iarr(shape);
    indgen(iarr);
    {
      // Create the file.
      HDF5File file("tHDF5DataSet_tmp", ByteIO::New);
      // Create a data set in it.
      HDF5DataSet dset(file, "array", shape, ts, (Int*)0);
      AlwaysAssertExit (dset.getName() == "array");
      AlwaysAssertExit (dset.shape() == shape);
      AlwaysAssertExit (dset.tileShape() == shape);
      dset.put (Slicer(IPosition(2,0), shape), iarr.data());
      AlwaysAssertExit (HDF5DataSet::getDataType (file, "array") == TpInt);
    }
    {
      // Open the file and data set.
      HDF5File file("tHDF5DataSet_tmp", ByteIO::Old);
      HDF5DataSet dset(file, "array", (Int*)0);
      AlwaysAssertExit (dset.getName() == "array");
      AlwaysAssertExit (dset.shape() == shape);
      AlwaysAssertExit (dset.tileShape() == shape);
      // Set the cache size in chunks.
      dset.setCacheSize (10);
      Array<Int> ires(shape);
      dset.get (Slicer(IPosition(2,0), shape), ires.data());
      AlwaysAssertExit (allEQ(iarr, ires));
      Slicer section(IPosition(2,1), IPosition(2,2), IPosition(2,2));
      Array<Int> ires2(section.length());
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
      HDF5DataSet dset(file, "mask", shape, ts, (Bool*)0);
      AlwaysAssertExit (dset.shape() == shape);
      AlwaysAssertExit (dset.tileShape() == shape);
    }

  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
