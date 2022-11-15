//# tJsonKVMap.cc: Program to test class JsonValue
//# Copyright (C) 2016
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

#include <casacore/casa/Json/JsonValue.h>
#include <casacore/casa/Json/JsonError.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <iostream>
#include <cstdlib>

using namespace casacore;
using namespace std;

#define AssertException(cmd) \
  { Bool tryFail = False; \
    try { cmd ; } catch (const JsonError&) { tryFail = True; } \
    AlwaysAssertExit (tryFail); \
  }

void doScalar()
{
  AlwaysAssertExit (JsonValue().isNull());
  AlwaysAssertExit (! JsonValue(True).isNull());
  AlwaysAssertExit (! JsonValue(1).isNull());
  AlwaysAssertExit (! JsonValue(1.).isNull());
  AlwaysAssertExit (! JsonValue(DComplex()).isNull());
  AlwaysAssertExit (! JsonValue(String()).isNull());
  AlwaysAssertExit (JsonValue(True).dataType() == TpBool);
  AlwaysAssertExit (JsonValue(1).dataType() == TpInt64);
  AlwaysAssertExit (JsonValue(1.).dataType() == TpDouble);
  AlwaysAssertExit (JsonValue(Complex()).dataType() == TpDComplex);
  AlwaysAssertExit (JsonValue("").dataType() == TpString);
  AlwaysAssertExit (JsonValue(True).arrayDataType() == TpBool);
  AlwaysAssertExit (JsonValue(1).arrayDataType() == TpInt64);
  AlwaysAssertExit (JsonValue(1.).arrayDataType() == TpDouble);
  AlwaysAssertExit (JsonValue(Complex()).arrayDataType() == TpDComplex);
  AlwaysAssertExit (JsonValue("").arrayDataType() == TpString);

  AlwaysAssertExit (JsonValue(True).size() == 1);
  AlwaysAssertExit (JsonValue(1).size() == 1);
  AlwaysAssertExit (JsonValue(1.).size() == 1);
  AlwaysAssertExit (JsonValue(Complex()).size() == 1);
  AlwaysAssertExit (JsonValue("").size() == 1);

  AlwaysAssertExit (JsonValue(True).shape() == IPosition(1,1));
  AlwaysAssertExit (JsonValue(1).shape() == IPosition(1,1));
  AlwaysAssertExit (JsonValue(1.).shape() == IPosition(1,1));
  AlwaysAssertExit (JsonValue(Complex()).shape() == IPosition(1,1));
  AlwaysAssertExit (JsonValue("").shape() == IPosition(1,1));

  AssertException  (JsonValue().getBool());
  AssertException  (JsonValue().getInt());
  AlwaysAssertExit (isNaN (JsonValue().getDouble()));
  AlwaysAssertExit (isNaN (JsonValue().getDComplex()));
  AssertException  (JsonValue().getString());
  AlwaysAssertExit (JsonValue(True).getBool() == True);
  AssertException  (JsonValue(True).getInt());
  AssertException  (JsonValue(True).getDouble());
  AssertException  (JsonValue(True).getDComplex());
  AssertException  (JsonValue(True).getString());
  AlwaysAssertExit (JsonValue(1).getBool() == True);
  AlwaysAssertExit (JsonValue(0).getBool() == False);
  AlwaysAssertExit (JsonValue(1).getInt() == 1);
  AlwaysAssertExit (JsonValue(1).getDouble() == 1.);
  AlwaysAssertExit (JsonValue(1).getDComplex() == DComplex(1,0));
  AssertException  (JsonValue(1).getString());
  AssertException  (JsonValue(-1.).getBool());
  AssertException  (JsonValue(-1.).getInt());
  AlwaysAssertExit (JsonValue(-1.).getDouble() == -1.);
  AlwaysAssertExit (JsonValue(-1.).getDComplex() == DComplex(-1,0));
  AssertException  (JsonValue(-1.).getString());
  AssertException  (JsonValue(DComplex(1,2)).getBool());
  AssertException  (JsonValue(DComplex(1,2)).getInt());
  AssertException  (JsonValue(DComplex(1,2)).getDouble());
  AlwaysAssertExit (JsonValue(DComplex(1,2)).getDComplex() == DComplex(1,2));
  AssertException  (JsonValue(DComplex(1,2)).getString());
  AssertException  (JsonValue("1,2").getBool());
  AssertException  (JsonValue("1,2").getInt());
  AssertException  (JsonValue("1,2").getDouble());
  AssertException  (JsonValue("1,2").getDComplex());
  AlwaysAssertExit (JsonValue("1,2").getString() == "1,2");
}

void doVector()
{
  vector<JsonValue> vec;
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).size() == 0);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,0));

  vec.push_back (JsonValue(10));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpInt64);
  AlwaysAssertExit (JsonValue(vec).size() == 1);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,1));
  AlwaysAssertExit (JsonValue(vec).getVector().size() == 1);
  AlwaysAssertExit (JsonValue(vec).getVecBool().size() == 1);
  AlwaysAssertExit (JsonValue(vec).getVecInt().size() == 1);
  AlwaysAssertExit (JsonValue(vec).getVecDouble().size() == 1);
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 1);
  AssertException  (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecInt()[0] == 10);

  vec.push_back (JsonValue(2));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpInt64);
  AlwaysAssertExit (JsonValue(vec).size() == 2);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,2));
  AlwaysAssertExit (JsonValue(vec).getVecBool().size() == 2);
  AlwaysAssertExit (JsonValue(vec).getVecInt().size() == 2);
  AlwaysAssertExit (JsonValue(vec).getVecDouble().size() == 2);
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 2);
  AssertException  (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecInt()[0] == 10);
  AlwaysAssertExit (JsonValue(vec).getVecInt()[1] == 2);

  vec.push_back (JsonValue(3.));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpDouble);
  AlwaysAssertExit (JsonValue(vec).size() == 3);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,3));
  AssertException  (JsonValue(vec).getVecBool());
  AssertException  (JsonValue(vec).getVecInt());
  AlwaysAssertExit (JsonValue(vec).getVecDouble().size() == 3);
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 3);
  AssertException  (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecDouble()[0] == 10);
  AlwaysAssertExit (JsonValue(vec).getVecDouble()[1] == 2);
  AlwaysAssertExit (JsonValue(vec).getVecDouble()[2] == 3);

  vec.push_back (JsonValue(DComplex(-1,-2)));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpDComplex);
  AlwaysAssertExit (JsonValue(vec).size() == 4);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,4));
  AssertException  (JsonValue(vec).getVecBool());
  AssertException  (JsonValue(vec).getVecInt());
  AssertException  (JsonValue(vec).getVecDouble());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 4);
  AssertException  (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[0] == DComplex(10,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[1] == DComplex(2,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[2] == DComplex(3,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[3] == DComplex(-1,-2));

  vec.push_back (JsonValue(3.));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpDComplex);
  AlwaysAssertExit (JsonValue(vec).size() == 5);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,5));
  AssertException  (JsonValue(vec).getVecBool());
  AssertException  (JsonValue(vec).getVecInt());
  AssertException  (JsonValue(vec).getVecDouble());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 5);
  AssertException  (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[0] == DComplex(10,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[1] == DComplex(2,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[2] == DComplex(3,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[3] == DComplex(-1,-2));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[4] == DComplex(3,0));
  AssertException  (JsonValue(vec).getArrayDouble());
  AlwaysAssertExit (JsonValue(vec).getArrayDComplex().shape() == IPosition(1,5));
  AlwaysAssertExit (JsonValue(vec).getArrayDComplex().data()[0] == DComplex(10,0));
  AlwaysAssertExit (JsonValue(vec).getArrayDComplex().data()[1] == DComplex(2,0));
  AlwaysAssertExit (JsonValue(vec).getArrayDComplex().data()[2] == DComplex(3,0));
  AlwaysAssertExit (JsonValue(vec).getArrayDComplex().data()[3] == DComplex(-1,-2));
  AlwaysAssertExit (JsonValue(vec).getArrayDComplex().data()[4] == DComplex(3,0));
}

void doArray()
{
  vector<JsonValue> zvec;
  Int v = 0;
  for (int i=0; i<4; ++i) {
    vector<JsonValue> yvec;
    for (int j=0; j<3; ++j) {
      vector<JsonValue> xvec;
      for (int k=0; k<5; ++k) {
        xvec.push_back (v);
        v++;
      }
      yvec.push_back (xvec);
    }
    zvec.push_back (yvec);
  }
  Array<Int64> arr = JsonValue(zvec).getArrayInt();
  Array<Int64> exp(IPosition(3,5,3,4));
  indgen(exp);
  AlwaysAssertExit (allEQ(arr,exp));
}

void doValueHolder()
{
  AlwaysAssertExit (JsonValue().getValueHolder().isNull());
  AlwaysAssertExit (! JsonValue(1).getValueHolder().isNull());
  AlwaysAssertExit (JsonValue(1).getValueHolder().dataType() == TpInt64);
  AlwaysAssertExit (JsonValue(1.).getValueHolder().dataType() == TpDouble);
  AlwaysAssertExit (JsonValue(Complex()).getValueHolder().dataType() == TpDComplex);
  AlwaysAssertExit (JsonValue("a").getValueHolder().dataType() == TpString);
  AlwaysAssertExit (JsonValue(1).getValueHolder().asInt() == 1);
  AlwaysAssertExit (JsonValue(1.).getValueHolder().asDouble() == 1.);
  AlwaysAssertExit (JsonValue(Complex()).getValueHolder().asDComplex() == DComplex());
  AlwaysAssertExit (JsonValue("a").getValueHolder().asString() == "a");
  AlwaysAssertExit (JsonValue(vector<JsonValue>()).getValueHolder().dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(True))).getValueHolder().dataType() == TpArrayBool);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(1))).getValueHolder().dataType() == TpArrayInt64);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(1.))).getValueHolder().dataType() == TpArrayDouble);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(Complex()))).getValueHolder().dataType() == TpArrayDComplex);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue("a"))).getValueHolder().dataType() == TpArrayString);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(True))).getValueHolder().asArrayBool().size() == 1);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(True))).getValueHolder().asArrayBool().data()[0] == True);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(-2))).getValueHolder().asArrayInt().data()[0] == -2);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(2.5))).getValueHolder().asArrayDouble().data()[0] == 2.5);
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue(DComplex(1,2)))).getValueHolder().asArrayDComplex().data()[0] == DComplex(1,2));
  AlwaysAssertExit (JsonValue(vector<JsonValue>(1, JsonValue("bc"))).getValueHolder().asArrayString().data()[0] == "bc");
}

int main()
{
  try {
    doScalar();
    doVector();
    doArray();
    doValueHolder();
  } catch (const std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    exit(1);
  }
  cout << "OK" << endl;
  exit(0);
}
