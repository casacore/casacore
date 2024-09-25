//# tHDF5DataType.cc: Test program for class HDF5DataType
//# Copyright (C) 2018
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/HDF5/HDF5DataType.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
 
using namespace casacore;

template<typename T>
void testDT (DataType dt, DataType arrdt, uInt sz, Bool isC=False)
{
  // Test scalar.
  HDF5DataType hdt((T*)0);
  AlwaysAssertExit (hdt.size() == sz);
  AlwaysAssertExit (HDF5DataType::getDataType(hdt.getHidMem()) == dt);
  AlwaysAssertExit (HDF5DataType::getDataType(hdt.getHidFile()) == dt);
  AlwaysAssertExit (HDF5DataType::isComplex(hdt.getHidMem()) == isC);
  AlwaysAssertExit (HDF5DataType::isComplex(hdt.getHidFile()) == isC);
  // Test array.
  HDF5DataType dtarr(hdt, IPosition(2,3,4));
  AlwaysAssertExit (dtarr.size() == 3*4*sz);
  AlwaysAssertExit (HDF5DataType::getDataType(dtarr.getHidMem()) == arrdt);
  AlwaysAssertExit (HDF5DataType::getShape(dtarr.getHidMem()) == IPosition(2,3,4));
  AlwaysAssertExit (HDF5DataType::getDataType(dtarr.getHidFile()) == arrdt);
  AlwaysAssertExit (HDF5DataType::getShape(dtarr.getHidFile()) == IPosition(2,3,4));
}

void testCompound()
{
  // First test with a few scalars.
  std::vector<String> names(3);
  std::vector<HDF5DataType> types(3);
  names[0] = "f1";
  names[1] = "f2";
  names[2] = "f3";
  types[0] = HDF5DataType((Complex*)0);
  types[1] = HDF5DataType((Int*)0);
  types[2] = HDF5DataType((Float*)0);
  HDF5DataType dtcom1(names, types);
  AlwaysAssertExit (dtcom1.size() == 16);
  AlwaysAssertExit (HDF5DataType::getDataType(dtcom1.getHidMem()) == TpRecord);
  // Now add a few arrays and the previous compound.
  names.push_back ("fa1");
  names.push_back ("fa2");
  names.push_back ("fc");
  types.push_back (HDF5DataType(HDF5DataType((Bool*)0), IPosition(1,8)));
  types.push_back (HDF5DataType(HDF5DataType((DComplex*)0), IPosition(2,1,2)));
  types.push_back (dtcom1);
  HDF5DataType dtcom2(names, types);
  AlwaysAssertExit (dtcom2.size() == 16 + 8 + 2*16 + 16);
  AlwaysAssertExit (HDF5DataType::getDataType(dtcom2.getHidMem()) == TpRecord);
}

int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    testDT<Bool> (TpBool, TpArrayBool, 1);
    testDT<uChar> (TpUChar, TpArrayUChar, 1);
    testDT<Short> (TpShort, TpArrayShort, 2);
    testDT<Int> (TpInt, TpArrayInt, 4);
    testDT<uInt> (TpUInt, TpArrayUInt, 4);
    testDT<Int64> (TpInt64, TpArrayInt64, 8);
    testDT<Float> (TpFloat, TpArrayFloat, 4);
    testDT<Double> (TpDouble, TpArrayDouble, 8);
    testDT<Complex> (TpComplex, TpArrayComplex, 8, True);
    testDT<DComplex> (TpDComplex, TpArrayDComplex, 16, True);
    testCompound();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
