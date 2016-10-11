//# tJsonKVMap.cc: Program to test class JsonValue
//#
//# Copyright (C) 2016
//# ASTRON (Netherlands Institute for Radio Astronomy)
//# P.O.Box 2, 7990 AA Dwingeloo, The Netherlands
//#
//# This file is part of the LOFAR software suite.
//# The LOFAR software suite is free software: you can redistribute it and/or
//# modify it under the terms of the GNU General Public License as published
//# by the Free Software Foundation, either version 3 of the License, or
//# (at your option) any later version.
//#
//# The LOFAR software suite is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with the LOFAR software suite. If not, see <http://www.gnu.org/licenses/>.
//#
//# $Id: tJsonGram.cc 14057 2009-09-18 12:26:29Z diepen $

#include <casacore/casa/Json/JsonValue.h>
#include <casacore/casa/Json/JsonError.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>
#include <cstdlib>

using namespace casacore;
using namespace std;

#define AssertExcept(cmd) \
  { Bool tryFail = False; \
    try { cmd ; } catch (const JsonError&) { tryFail = True; } \
    AlwaysAssertExit (tryFail); \
  }

void doScalar()
{
  AlwaysAssertExit (JsonValue(True).dataType()  == TpBool);
  AlwaysAssertExit (JsonValue(1).dataType() == TpInt64);
  AlwaysAssertExit (JsonValue(1.).dataType() == TpDouble);
  AlwaysAssertExit (JsonValue(Complex()).dataType() == TpDComplex);
  AlwaysAssertExit (JsonValue("").dataType() == TpString);
  AlwaysAssertExit (JsonValue(True).arrayDataType()  == TpBool);
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

  AlwaysAssertExit (JsonValue(True).getBool() == True);
  AssertExcept     (JsonValue(True).getInt());
  AssertExcept     (JsonValue(True).getDouble());
  AssertExcept     (JsonValue(True).getDComplex());
  AssertExcept     (JsonValue(True).getString());
  AlwaysAssertExit (JsonValue(1).getBool() == True);
  AlwaysAssertExit (JsonValue(0).getBool() == False);
  AlwaysAssertExit (JsonValue(1).getInt() == 1);
  AlwaysAssertExit (JsonValue(1).getDouble() == 1.);
  AlwaysAssertExit (JsonValue(1).getDComplex() == DComplex(1,0));
  AssertExcept     (JsonValue(1).getString());
  AssertExcept     (JsonValue(-1.).getBool());
  AssertExcept     (JsonValue(-1.).getInt());
  AlwaysAssertExit (JsonValue(-1.).getDouble() == -1.);
  AlwaysAssertExit (JsonValue(-1.).getDComplex() == DComplex(-1,0));
  AssertExcept     (JsonValue(-1.).getString());
  AssertExcept     (JsonValue(DComplex(1,2)).getBool());
  AssertExcept     (JsonValue(DComplex(1,2)).getInt());
  AssertExcept     (JsonValue(DComplex(1,2)).getDouble());
  AlwaysAssertExit (JsonValue(DComplex(1,2)).getDComplex() == DComplex(1,2));
  AssertExcept     (JsonValue(DComplex(1,2)).getString());
  AssertExcept     (JsonValue("1,2").getBool());
  AssertExcept     (JsonValue("1,2").getInt());
  AssertExcept     (JsonValue("1,2").getDouble());
  AssertExcept     (JsonValue("1,2").getDComplex());
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
  AssertExcept     (JsonValue(vec).getVecString());
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
  AssertExcept     (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecInt()[0] == 10);
  AlwaysAssertExit (JsonValue(vec).getVecInt()[1] == 2);

  vec.push_back (JsonValue(3.));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpDouble);
  AlwaysAssertExit (JsonValue(vec).size() == 3);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,3));
  AssertExcept     (JsonValue(vec).getVecBool());
  AssertExcept     (JsonValue(vec).getVecInt());
  AlwaysAssertExit (JsonValue(vec).getVecDouble().size() == 3);
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 3);
  AssertExcept     (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecDouble()[0] == 10);
  AlwaysAssertExit (JsonValue(vec).getVecDouble()[1] == 2);
  AlwaysAssertExit (JsonValue(vec).getVecDouble()[2] == 3);

  vec.push_back (JsonValue(DComplex(-1,-2)));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpDComplex);
  AlwaysAssertExit (JsonValue(vec).size() == 4);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,4));
  AssertExcept     (JsonValue(vec).getVecBool());
  AssertExcept     (JsonValue(vec).getVecInt());
  AssertExcept     (JsonValue(vec).getVecDouble());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 4);
  AssertExcept     (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[0] == DComplex(10,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[1] == DComplex(2,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[2] == DComplex(3,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[3] == DComplex(-1,-2));

  vec.push_back (JsonValue(3.));
  AlwaysAssertExit (JsonValue(vec).dataType() == TpOther);
  AlwaysAssertExit (JsonValue(vec).arrayDataType() == TpDComplex);
  AlwaysAssertExit (JsonValue(vec).size() == 5);
  AlwaysAssertExit (JsonValue(vec).shape() == IPosition(1,5));
  AssertExcept     (JsonValue(vec).getVecBool());
  AssertExcept     (JsonValue(vec).getVecInt());
  AssertExcept     (JsonValue(vec).getVecDouble());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex().size() == 5);
  AssertExcept     (JsonValue(vec).getVecString());
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[0] == DComplex(10,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[1] == DComplex(2,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[2] == DComplex(3,0));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[3] == DComplex(-1,-2));
  AlwaysAssertExit (JsonValue(vec).getVecDComplex()[4] == DComplex(3,0));
  AssertExcept     (JsonValue(vec).getArrayDouble());
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

int main()
{
  try {
    doScalar();
    doVector();
    doArray();
  } catch (const std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    exit(1);
  }
  cout << "OK" << endl;
  exit(0);
}
