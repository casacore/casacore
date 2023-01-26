//# tHDF5Record.cc: Test program for class HDF5Record
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

#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/vector.h>

using namespace casacore;

Array<bool> arrb()
{
  Array<bool> arrb(IPosition(4,1,1,4,1));
  arrb = false;
  arrb(IPosition(4,0,0,2,0)) = true;
  return arrb;
}
Array<unsigned char> arruc()
{
  Array<unsigned char> arruc(IPosition(1,1));
  indgen(arruc);
  return arruc;
}
Array<int16_t> arrs()
{
  Array<int16_t> arrs(IPosition(2,3,4));
  indgen(arrs);
  return arrs;
}
Array<int32_t> arri()
{
  Array<int32_t> arri(IPosition(3,2,2,2));
  indgen(arri);
  return arri;
}
Array<uint32_t> arrui()
{
  Array<uint32_t> arrui(IPosition(1,5));
  indgen(arrui, 32768u*65536u);
  return arrui;
}
Array<int64_t> arri64()
{
  Array<int64_t> arri(IPosition(3,2,2,2));
  indgen(arri, int64_t(2e10));
  return arri;
}
Array<float> arrf()
{
  Array<float> arrf(IPosition(1,3));
  indgen(arrf);
  return arrf;
}
Array<double> arrd()
{
  Array<double> arrd(IPosition(1,5));
  indgen(arrd);
  return arrd;
}
Array<Complex> arrc()
{
  Array<Complex> arrc(IPosition(1,10));
  indgen(arrc);
  return arrc;
}
Array<DComplex> arrdc()
{
  Array<DComplex> arrdc(IPosition(1,8));
  indgen(arrdc);
  return arrdc;
}
Array<String> arrstr()
{
  Array<String> arrstr(IPosition(3,2,1,2));
  arrstr(IPosition(3,0,0,0)) = "str000";
  arrstr(IPosition(3,1,0,0)) = "a horse without a name";
  arrstr(IPosition(3,0,0,1)) = "stray";
  arrstr(IPosition(3,1,0,1)) = "";
  return arrstr;
}
Array<String> arrstremp()
{
  Array<String> arrstr(IPosition(6,1,2,3,4,5,6));
  return arrstr;
}

Array<bool> emparrb()
{
  return Array<bool> (IPosition(4,0));
}
Array<unsigned char> emparruc()
{
  return Array<unsigned char> (IPosition(0,0));
}
Array<int16_t> emparrs()
{
  return Array<int16_t> (IPosition(0,0));
}
Array<int32_t> emparri()
{
  return Array<int32_t> (IPosition(0,0));
}
Array<uint32_t> emparrui()
{
  return Array<uint32_t> (IPosition(1,0));
}
Array<int64_t> emparri64()
{
  return Array<int64_t> (IPosition(2,0));
}
Array<float> emparrf()
{
  return Array<float> (IPosition(2,0));
}
Array<double> emparrd()
{
  return Array<double> (IPosition(1,0));
}
Array<Complex> emparrc()
{
  return Array<Complex> (IPosition(2,0,0));
}
Array<DComplex> emparrdc()
{
  return Array<DComplex> (IPosition(2,0,0));
}
Array<String> emparrstr()
{
  return Array<String> (IPosition(0,0));
}

void checkRecord (const RecordInterface& rec)
{
  AlwaysAssertExit (rec.nfields()==12);
  AlwaysAssertExit (rec.asBool("bool") == true);
  AlwaysAssertExit (rec.asuChar("uchar") == 1);
  AlwaysAssertExit (rec.asShort("short") == -2);
  AlwaysAssertExit (rec.asInt("int") == 2);
  AlwaysAssertExit (rec.asuInt("uint") == 21);
  AlwaysAssertExit (rec.asInt64("int64") == int64_t(1e10));
  AlwaysAssertExit (rec.asFloat("float") == 3.);
  AlwaysAssertExit (rec.asDouble("double") == -2.1);
  AlwaysAssertExit (rec.asComplex("complex") == Complex(-2.1,1.1));
  AlwaysAssertExit (rec.asDComplex("dcomplex") == DComplex(-2.2,1.2));
  AlwaysAssertExit (allEQ(rec.asArrayString("arrstring"), arrstr()));
  AlwaysAssertExit (rec.asString("string") == "abc");
}

void check (HDF5File& file)
{
  // Read the records back and check them.
  Record reca3 = HDF5Record::readRecord (file, "test");
  AlwaysAssertExit (reca3.nfields()==25);
  AlwaysAssertExit (reca3.asString("string") == "");
  AlwaysAssertExit (allEQ(reca3.asArrayString("arrstringemp"), arrstremp()));
  AlwaysAssertExit (allEQ(reca3.asArrayBool("arrbool"), arrb()));
  AlwaysAssertExit (allEQ(reca3.asArrayuChar("arruchar"), arruc()));
  AlwaysAssertExit (allEQ(reca3.asArrayShort("arrshort"), arrs()));
  AlwaysAssertExit (allEQ(reca3.asArrayInt("arrint"), arri()));
  AlwaysAssertExit (allEQ(reca3.asArrayuInt("arruint"), arrui()));
  AlwaysAssertExit (allEQ(reca3.asArrayInt64("arrint64"), arri64()));
  AlwaysAssertExit (allEQ(reca3.asArrayFloat("arrfloat"), arrf()));
  AlwaysAssertExit (allEQ(reca3.asArrayDouble("arrdouble"), arrd()));
  AlwaysAssertExit (allEQ(reca3.asArrayComplex("arrcomplex"), arrc()));
  AlwaysAssertExit (allEQ(reca3.asArrayDComplex("arrdcomplex"), arrdc()));
  AlwaysAssertExit (allEQ(reca3.asArrayString("arrstring"), arrstr()));
  AlwaysAssertExit (allEQ(reca3.asArrayBool("emparrbool"), emparrb()));
  AlwaysAssertExit (allEQ(reca3.asArrayuChar("emparruchar"), emparruc()));
  AlwaysAssertExit (allEQ(reca3.asArrayShort("emparrshort"), emparrs()));
  AlwaysAssertExit (allEQ(reca3.asArrayInt("emparrint"), emparri()));
  AlwaysAssertExit (allEQ(reca3.asArrayuInt("emparruint"), emparrui()));
  AlwaysAssertExit (allEQ(reca3.asArrayInt64("emparrint64"), emparri64()));
  AlwaysAssertExit (allEQ(reca3.asArrayFloat("emparrfloat"), emparrf()));
  AlwaysAssertExit (allEQ(reca3.asArrayDouble("emparrdouble"), emparrd()));
  AlwaysAssertExit (allEQ(reca3.asArrayComplex("emparrcomplex"), emparrc()));
  AlwaysAssertExit (allEQ(reca3.asArrayDComplex("emparrdcomplex"), emparrdc()));
  AlwaysAssertExit (allEQ(reca3.asArrayString("emparrstring"), emparrstr()));
  Record reca2 = reca3.asRecord("rec2");
  AlwaysAssertExit (reca2.nfields()==3);
  AlwaysAssertExit (reca2.asDouble("double") == 3.14);
  checkRecord (reca2.asRecord("rec1"));
  checkRecord (reca2.asRecord("rec1a"));
}

int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    // Create the file.
    HDF5File file("tHDF5Record_tmp", ByteIO::New);
    // Create a record and nested record.
    Record rec1;
    rec1.define ("bool", true);
    rec1.define ("uchar", (unsigned char)1);
    rec1.define ("short", (int16_t)-2);
    rec1.define ("int", (int32_t)2);
    rec1.define ("uint", (uint32_t)21);
    rec1.define ("int64", int64_t(1e10));
    rec1.define ("float", (float)3.);
    rec1.define ("double", (double)-2.1);
    rec1.define ("complex", Complex(-2.1,1.1));
    rec1.define ("dcomplex", DComplex(-2.2,1.2));
    rec1.define ("arrstring", arrstr());
    rec1.define ("string", "abc");
    Record rec2;
    rec2.defineRecord ("rec1", rec1);
    rec2.define ("double", (double)3.14);
    rec2.defineRecord ("rec1a",rec1);
    Record rec3;
    rec3.defineRecord ("rec2",rec2);
    rec3.define ("string", "");
    rec3.define ("arrstringemp", arrstremp());
    rec3.define ("arrbool", arrb());
    rec3.define ("arruchar", arruc());
    rec3.define ("arrshort", arrs());
    rec3.define ("arrint", arri());
    rec3.define ("arruint", arrui());
    rec3.define ("arrint64", arri64());
    rec3.define ("arrfloat", arrf());
    rec3.define ("arrdouble", arrd());
    rec3.define ("arrcomplex", arrc());
    rec3.define ("arrdcomplex", arrdc());
    rec3.define ("arrstring", arrstr());
    rec3.define ("emparrbool", emparrb());
    rec3.define ("emparruchar", emparruc());
    rec3.define ("emparrshort", emparrs());
    rec3.define ("emparrint", emparri());
    rec3.define ("emparruint", emparrui());
    rec3.define ("emparrint64", emparri64());
    rec3.define ("emparrfloat", emparrf());
    rec3.define ("emparrdouble", emparrd());
    rec3.define ("emparrcomplex", emparrc());
    rec3.define ("emparrdcomplex", emparrdc());
    rec3.define ("emparrstring", emparrstr());
    HDF5Record::writeRecord (file, "test", rec3);
    file.flush();
   
    // Read everything back and check.
    check (file);
    // Write again and check.
    HDF5Record::writeRecord (file, "test", rec3);
    check (file);

    // Check a group exists.
    AlwaysAssertExit (HDF5Group::exists (file, "test"));
    // Get the group names in the file.
    vector<String> names = HDF5Group::linkNames (file);
    AlwaysAssertExit (names.size() == 1  &&  names[0] == "test");
    // Delete the record.
    HDF5Record::remove (file, "test");
    // Read back and check it is empty.
    Record reca3 = HDF5Record::readRecord (file, "test");
    AlwaysAssertExit (reca3.nfields() == 0);
    // Do the same for a non-existing group.
    Record reca4 = HDF5Record::readRecord (file, "xxx");
    AlwaysAssertExit (reca4.nfields() == 0);

    // Write again and check.
    HDF5Record::writeRecord (file, "test", rec3);
    check (file);

  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
