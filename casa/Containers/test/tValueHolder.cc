//# tValueHolder.cc: Test the ValueHolder class
//# Copyright (C) 2009
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

#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>

using namespace casacore;

void doBool (bool v)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == TpBool);
  AlwaysAssertExit (vh.asBool() == v);
  Vector<bool> vec = vh.asArrayBool();
  AlwaysAssertExit (vec.size() == 1  &&  vec.data()[0] == v);
  cout << vh << endl;
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpBool);
  bool vc;
  vhc.getValue (vc);
  AlwaysAssertExit (vc == v);
  cout << vh << ' ' << vhc << endl;
}

template<typename T, typename U> void doPos(T v, U, DataType dt)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == dt);
  AlwaysAssertExit (vh.asBool());
  AlwaysAssertExit (vh.asuChar() == static_cast<unsigned char>(v));
  AlwaysAssertExit (vh.asShort() == int16_t(v));
  AlwaysAssertExit (vh.asuShort() == uint16_t(v));
  AlwaysAssertExit (vh.asInt() == int32_t(v));
  AlwaysAssertExit (vh.asuInt() == uint32_t(v));
  AlwaysAssertExit (vh.asInt64() == int64_t(v));
  AlwaysAssertExit (vh.asFloat() == float(v));
  AlwaysAssertExit (vh.asDouble() == double(v));
  AlwaysAssertExit (vh.asComplex() == Complex(float(v),0));
  AlwaysAssertExit (vh.asDComplex() == DComplex(double(v),0));
  Vector<bool> vecbool = vh.asArrayBool();
  AlwaysAssertExit (vecbool.size() == 1  &&  vecbool.data()[0] == true);
  Vector<unsigned char> vecuChar = vh.asArrayuChar();
  AlwaysAssertExit (vecuChar.size() == 1  &&  vecuChar.data()[0] == static_cast<unsigned char>(v));
  Vector<int16_t> vecShort = vh.asArrayShort();
  AlwaysAssertExit (vecShort.size() == 1  &&  vecShort.data()[0] == int16_t(v));
  Vector<uint16_t> vecUShort = vh.asArrayuShort();
  AlwaysAssertExit (vecUShort.size() == 1  &&  vecUShort.data()[0] == uint16_t(v));
  Vector<int32_t> vecInt = vh.asArrayInt();
  AlwaysAssertExit (vecInt.size() == 1  &&  vecInt.data()[0] == int32_t(v));
  Vector<uint32_t> vecuInt = vh.asArrayuInt();
  AlwaysAssertExit (vecuInt.size() == 1  &&  vecuInt.data()[0] == uint32_t(v));
  Vector<int64_t> vecInt64 = vh.asArrayInt64();
  AlwaysAssertExit (vecInt64.size() == 1  &&  vecInt64.data()[0] == int64_t(v));
  Vector<float> vecfloat = vh.asArrayFloat();
  AlwaysAssertExit (vecfloat.size() == 1  &&  vecfloat.data()[0] == float(v));
  Vector<double> vecdouble = vh.asArrayDouble();
  AlwaysAssertExit (vecdouble.size() == 1  &&  vecdouble.data()[0] == double(v));
  Vector<Complex> veccomplex = vh.asArrayComplex();
  AlwaysAssertExit (veccomplex.size() == 1  &&
                    veccomplex.data()[0] == Complex(float(v), 0));
  Vector<DComplex> vecdcomplex = vh.asArrayDComplex();
  AlwaysAssertExit (vecdcomplex.size() == 1  &&
                    vecdcomplex.data()[0] == DComplex(double(v), 0));
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  if (dt == TpUShort) {
    AlwaysAssertExit (vhc.dataType() == TpInt);
  } else {
    AlwaysAssertExit (vhc.dataType() == dt);
  }
  U vc;
  vhc.getValue (vc);
  AlwaysAssertExit (vc == U(v));
  cout << vh << ' ' << vhc << endl;
}

template<typename T, typename U> void doNeg(T v, U, DataType dt)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == dt);
  AlwaysAssertExit (vh.asBool());
  AlwaysAssertExit (vh.asShort() == int16_t(v));
  AlwaysAssertExit (vh.asInt() == int32_t(v));
  AlwaysAssertExit (vh.asInt64() == int64_t(v));
  AlwaysAssertExit (vh.asFloat() == float(v));
  AlwaysAssertExit (vh.asDouble() == double(v));
  AlwaysAssertExit (vh.asComplex() == Complex(float(v),0));
  AlwaysAssertExit (vh.asDComplex() == DComplex(double(v),0));
  Vector<bool> vecbool = vh.asArrayBool();
  AlwaysAssertExit (vecbool.size() == 1  &&  vecbool.data()[0] == true);
  Vector<int16_t> vecShort = vh.asArrayShort();
  AlwaysAssertExit (vecShort.size() == 1  &&  vecShort.data()[0] == int16_t(v));
  Vector<int32_t> vecInt = vh.asArrayInt();
  AlwaysAssertExit (vecInt.size() == 1  &&  vecInt.data()[0] == int32_t(v));
  Vector<int64_t> vecInt64 = vh.asArrayInt64();
  AlwaysAssertExit (vecInt64.size() == 1  &&  vecInt64.data()[0] == int64_t(v));
  Vector<float> vecfloat = vh.asArrayFloat();
  AlwaysAssertExit (vecfloat.size() == 1  &&  vecfloat.data()[0] == float(v));
  Vector<double> vecdouble = vh.asArrayDouble();
  AlwaysAssertExit (vecdouble.size() == 1  &&  vecdouble.data()[0] == double(v));
  Vector<Complex> veccomplex = vh.asArrayComplex();
  AlwaysAssertExit (veccomplex.size() == 1  &&
                    veccomplex.data()[0] == Complex(float(v), 0));
  Vector<DComplex> vecdcomplex = vh.asArrayDComplex();
  AlwaysAssertExit (vecdcomplex.size() == 1  &&
                    vecdcomplex.data()[0] == DComplex(double(v), 0));
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == dt);
  U vc;
  vhc.getValue (vc);
  AlwaysAssertExit (vc == v);
  cout << vh << ' ' << vhc << endl;
}

void doComplex (const Complex& v)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == TpComplex);
  AlwaysAssertExit (vh.asComplex() == v);
  AlwaysAssertExit (vh.asDComplex() == DComplex(v));
  Vector<Complex> vec = vh.asArrayComplex();
  AlwaysAssertExit (vec.size() == 1  &&  vec.data()[0] == v);
  Vector<DComplex> vecd = vh.asArrayDComplex();
  AlwaysAssertExit (vecd.size() == 1  &&  vecd.data()[0] == DComplex(v));
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpComplex);
  Complex vc;
  vhc.getValue (vc);
  AlwaysAssertExit (vc == v);
  cout << vh << ' ' << vhc << endl;
}

void doDComplex (const DComplex& v)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == TpDComplex);
  AlwaysAssertExit (vh.asComplex() == Complex(v));
  AlwaysAssertExit (vh.asDComplex() == v);
  Vector<Complex> vec = vh.asArrayComplex();
  AlwaysAssertExit (vec.size() == 1  &&  vec.data()[0] == Complex(v));
  Vector<DComplex> vecd = vh.asArrayDComplex();
  AlwaysAssertExit (vecd.size() == 1  &&  vecd.data()[0] == v);
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpDComplex);
  DComplex vc;
  vhc.getValue (vc);
  AlwaysAssertExit (vc == v);
  cout << vh << ' ' << vhc << endl;
}

void doString (const String& v)
{
  ValueHolder vh1(v);
  AlwaysAssertExit (!vh1.isNull());
  AlwaysAssertExit (vh1.dataType() == TpString);
  AlwaysAssertExit (vh1.asString() == v);
  ValueHolder vh2(v.c_str());
  AlwaysAssertExit (!vh2.isNull());
  AlwaysAssertExit (vh2.dataType() == TpString);
  AlwaysAssertExit (vh2.asString() == v);
  Vector<String> vec = vh1.asArrayString();
  AlwaysAssertExit (vec.size() == 1  &&  vec.data()[0] == v);
  Record rec;
  vh1.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpString);
  String vc;
  vhc.getValue (vc);
  AlwaysAssertExit (vc == v);
  ValueHolder vh3("123");
  ValueHolder vh4(vhc);
  vh3 = vhc;
  cout << vh1 << ' ' << vh2 << ' ' << vhc << ' ' << vh3 << ' ' << vh4 << endl;
}

void doArrayDComplex (const Array<DComplex>& v)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == TpArrayDComplex);
  Array<Complex> ac(v.shape());
  convertArray (ac, v);
  AlwaysAssertExit (allEQ (vh.asArrayComplex(), ac));
  AlwaysAssertExit (allEQ (vh.asArrayDComplex(), v));
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpArrayDComplex);
  Array<DComplex> vc;
  vhc.getValue (vc);
  AlwaysAssertExit (allEQ (vc, v));
  cout << vh << ' ' << vhc << endl;
}

void doArrayString (const Array<String>& v)
{
  ValueHolder vh1(v);
  AlwaysAssertExit (!vh1.isNull());
  AlwaysAssertExit (vh1.dataType() == TpArrayString);
  AlwaysAssertExit (allEQ(vh1.asArrayString(), v));
  Record rec;
  vh1.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpArrayString);
  Array<String> vc;
  vhc.getValue (vc);
  AlwaysAssertExit (allEQ(vc, v));
  cout << vh1 << ' ' << vhc << endl;
}

int main()
{
  try {
    doBool(true);
    doBool(false);
    doNeg(int16_t(-4), int16_t(0), TpShort);
    doNeg(int32_t(-7), int32_t(0), TpInt);
    doNeg(int64_t(-40), int64_t(0), TpInt64);
    doNeg(float(-4.1), float(0), TpFloat);
    doNeg(double(-4.7), double(0), TpDouble);
    doPos(static_cast<unsigned char>(13), static_cast<unsigned char>(0), TpUChar);
    doPos(int16_t(4), int16_t(0), TpShort);
    doPos(uint16_t(14), uint16_t(0), TpUShort);
    doPos(int32_t(17), int32_t(0), TpInt);
    doPos(uint32_t(10), uint32_t(0), TpUInt);
    doPos(int64_t(40), int64_t(0), TpInt64);
    doPos(float(4.1), float(0), TpFloat);
    doPos(double(4.7), double(0), TpDouble);
    doString(String());
    doString("");
    doString("astring");
    doComplex(Complex(3.1, -1.5));
    doDComplex(DComplex(-3.2, 11.5));

    Array<DComplex> dcomplexs(IPosition(2,1,2));
    dcomplexs(IPosition(2,0,0)) = DComplex(-1.311, 1.422);
    dcomplexs(IPosition(2,0,1)) = DComplex(-11.311, 21.422);
    doArrayDComplex (dcomplexs);

    Array<String> strings(IPosition(2,1,1));
    strings = "abc";
    doArrayString (strings);
  } catch (std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;                           // exit with success status
}
