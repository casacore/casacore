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
//#
//# $Id$

#include <casa/Containers/ValueHolder.h>
#include <casa/Containers/Record.h>
#include <casa/Utilities/Assert.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>

using namespace casa;

void doBool (Bool v)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == TpBool);
  AlwaysAssertExit (vh.asBool() == v);
  Vector<Bool> vec = vh.asArrayBool();
  AlwaysAssertExit (vec.size() == 1  &&  vec.data()[0] == v);
  cout << vh << endl;
  Record rec;
  vh.toRecord (rec, "a");
  ValueHolder vhc;
  AlwaysAssertExit (vhc.isNull());
  vhc = ValueHolder::fromRecord (rec, "a");
  AlwaysAssertExit (!vhc.isNull());
  AlwaysAssertExit (vhc.dataType() == TpBool);
  Bool vc;
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
  AlwaysAssertExit (vh.asuChar() == uChar(v));
  AlwaysAssertExit (vh.asShort() == Short(v));
  AlwaysAssertExit (vh.asuShort() == uShort(v));
  AlwaysAssertExit (vh.asInt() == Int(v));
  AlwaysAssertExit (vh.asuInt() == uInt(v));
  AlwaysAssertExit (vh.asFloat() == float(v));
  AlwaysAssertExit (vh.asDouble() == double(v));
  AlwaysAssertExit (vh.asComplex() == Complex(float(v),0));
  AlwaysAssertExit (vh.asDComplex() == DComplex(double(v),0));
  Vector<Bool> vecbool = vh.asArrayBool();
  AlwaysAssertExit (vecbool.size() == 1  &&  vecbool.data()[0] == True);
  Vector<uChar> vecuChar = vh.asArrayuChar();
  AlwaysAssertExit (vecuChar.size() == 1  &&  vecuChar.data()[0] == uChar(v));
  Vector<Short> vecShort = vh.asArrayShort();
  AlwaysAssertExit (vecShort.size() == 1  &&  vecShort.data()[0] == Short(v));
  Vector<uShort> vecUShort = vh.asArrayuShort();
  AlwaysAssertExit (vecUShort.size() == 1  &&  vecUShort.data()[0] == uShort(v));
  Vector<Int> vecInt = vh.asArrayInt();
  AlwaysAssertExit (vecInt.size() == 1  &&  vecInt.data()[0] == Int(v));
  Vector<uInt> vecuInt = vh.asArrayuInt();
  AlwaysAssertExit (vecuInt.size() == 1  &&  vecuInt.data()[0] == uInt(v));
  Vector<Float> vecfloat = vh.asArrayFloat();
  AlwaysAssertExit (vecfloat.size() == 1  &&  vecfloat.data()[0] == float(v));
  Vector<Double> vecdouble = vh.asArrayDouble();
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
  vh.write (cout);
  cout << ' ';
  vhc.write (cout);
  cout << ' ' << vh << ' ' << vhc << endl;
}

template<typename T, typename U> void doNeg(T v, U, DataType dt)
{
  ValueHolder vh(v);
  AlwaysAssertExit (!vh.isNull());
  AlwaysAssertExit (vh.dataType() == dt);
  AlwaysAssertExit (vh.asBool());
  AlwaysAssertExit (vh.asShort() == Short(v));
  AlwaysAssertExit (vh.asInt() == Int(v));
  AlwaysAssertExit (vh.asFloat() == float(v));
  AlwaysAssertExit (vh.asDouble() == double(v));
  AlwaysAssertExit (vh.asComplex() == Complex(float(v),0));
  AlwaysAssertExit (vh.asDComplex() == DComplex(double(v),0));
  Vector<Bool> vecbool = vh.asArrayBool();
  AlwaysAssertExit (vecbool.size() == 1  &&  vecbool.data()[0] == True);
  Vector<Short> vecShort = vh.asArrayShort();
  AlwaysAssertExit (vecShort.size() == 1  &&  vecShort.data()[0] == Short(v));
  Vector<Int> vecInt = vh.asArrayInt();
  AlwaysAssertExit (vecInt.size() == 1  &&  vecInt.data()[0] == Int(v));
  Vector<Float> vecfloat = vh.asArrayFloat();
  AlwaysAssertExit (vecfloat.size() == 1  &&  vecfloat.data()[0] == float(v));
  Vector<Double> vecdouble = vh.asArrayDouble();
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
  vh.write (cout);
  cout << ' ';
  vhc.write (cout);
  cout << ' ' << vh << ' ' << vhc << endl;
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
  vh.write (cout);
  cout << ' ';
  vhc.write (cout);
  cout << ' ' << vh << ' ' << vhc << endl;
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
  vh.write (cout);
  cout << ' ';
  vhc.write (cout);
  cout << ' ' << vh << ' ' << vhc << endl;
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
  vh.write (cout, ", ", 2);
  cout << ' ';
  vhc.write (cout, ":", 3);
  cout << ' ' << vh << ' ' << vhc << endl;
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
    doBool(True);
    doBool(False);
    doNeg(Short(-4), Short(0), TpShort);
    doNeg(Int(-7), Int(0), TpInt);
    doNeg(float(-4.1), float(0), TpFloat);
    doNeg(double(-4.7), double(0), TpDouble);
    doPos(uChar(13), uChar(0), TpUChar);
    doPos(Short(4), Short(0), TpShort);
    doPos(uShort(14), Int(0), TpInt);
    doPos(Int(17), Int(0), TpInt);
    doPos(uInt(10), Int(0), TpInt);
    doPos(float(4.1), Float(0), TpFloat);
    doPos(double(4.7), Double(0), TpDouble);
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
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;                           // exit with success status
}
