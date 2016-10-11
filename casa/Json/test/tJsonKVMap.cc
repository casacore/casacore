//# tJsonKVMap.cc: Program to test classes JsonKVMap and JsonValue
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

#include <casacore/casa/Json/JsonKVMap.h>
#include <casacore/casa/Json/JsonParser.h>
#include <casacore/casa/Json/JsonOut.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>
#include <cstdlib>

using namespace casacore;
using namespace std;

void showpar3 (JsonKVMap& par3)
{
  JsonOut jout;
  par3.show (cout);
  cout << par3["b1"].getBool() << ' ';
  jout.putArray (Vector<Bool>(par3["b1"].getVecBool()), String(), True);
  cout << endl;
  cout << par3["i1"].getInt() << ' ';
  jout.putArray (Vector<Int64>(par3["i1"].getVecInt()), String(), True);
  cout << endl;
  cout << par3["i1"].getDouble() << ' ';
  jout.putArray (Vector<Double>(par3["i1"].getVecDouble()), String(), True);
  cout << endl;
  cout << par3["i1"].getDComplex() << ' ';
  jout.putArray (Vector<DComplex>(par3["i1"].getVecDComplex()), String(), True);
  cout << endl;
  cout << par3["d1"].getDouble() << ' ';
  jout.putArray (Vector<Double>(par3["d1"].getVecDouble()), String(), True);
  cout << endl;
  cout << par3["d1"].getDComplex() << ' ';
  jout.putArray (Vector<DComplex>(par3["d1"].getVecDComplex()), String(), True);
  cout << endl;
  cout << par3["dc1"].getDComplex() << ' ';
  jout.putArray (Vector<DComplex>(par3["dc1"].getVecDComplex()), String(), True);
  cout << endl;
  cout << par3["s1"].getString() << ' ';
  jout.putArray (Vector<String>(par3["s1"].getVecString()), String(), True);
  cout << endl;
  cout << par3["b1"].dataType() << ' ' << par3["b1"].isVector() << ' '
       << par3["b1"].size() << endl;
  cout << par3["i1"].dataType() << ' ' << par3["i1"].isVector() << ' '
       << par3["i1"].size() << endl;
  cout << par3["d1"].dataType() << ' ' << par3["d1"].isVector() << ' '
       << par3["d1"].size() << endl;
  cout << par3["dc1"].dataType() << ' ' << par3["dc1"].isVector() << ' '
       << par3["dc1"].size() << endl;
  cout << par3["s1"].dataType() << ' ' << par3["s1"].isVector() << ' '
       << par3["s1"].size() << endl;
}

void showpar2 (JsonKVMap& par2)
{
  par2.show (cout);
  cout << par2["blk1"].dataType() << ' ' << par2["blk1"].isVector() << ' '
       << par2["blk1"].size() << endl;
  cout << par2["blk2"].dataType() << ' ' << par2["blk2"].isVector() << ' '
       << par2["blk2"].size() << endl;
  cout << par2["vec"].dataType() << ' ' << par2["vec"].isVector() << ' '
       << par2["vec"].size() << endl;
}

void doIt()
{
  JsonKVMap par;
  par["b1"] = true;
  par["i1"] = 10;
  par["d1"] = double(30);
  par["dc1"] = DComplex(60,70);
  par["s1"] = "abc";
  par["s2"] = String("defg");
  cout << "JsonKVMap par:" << endl;
  par.show (cout);
  JsonKVMap par1(par);
  cout << "JsonKVMap par1:" << endl;
  par1.show(cout);
  par["b2"] = false;
  par1 = par;
  cout << "JsonKVMap par1:" << endl;
  par1.show (cout);

  cout << "JsonKVMap par3:" << endl;
  JsonKVMap par3(par1);
  showpar3 (par3);

  JsonKVMap par2;
  par1 = par2;
  cout << "Empty JsonKVMap par1:" << endl;
  par1.show (cout);
  par1["blk1"] = par;
  par1["blk2"] = par2;
  vector<JsonValue> vec;
  vec.push_back (true);
  vec.push_back (100);
  vec.push_back (double(100.02));
  vec.push_back (DComplex (1.123456789, -3));
  vec.push_back ("a");
  vec.push_back ("");
  vec.push_back ("abc");
  par1["vec"] = vec;
  par2 = par1;
  cout << "JsonKVMap par2:" << endl;
  showpar2 (par2);
}

void doItParse()
{
  try {
    cout << JsonParser::parse ("{\"key1\":1}") << endl;
    cout << JsonParser::parse ("{\"key2\":\"abc\"}") << endl;
    cout << JsonParser::parse ("{\"key1\" : 1, \"key2\":\"abc\"}") << endl;
    cout << JsonParser::parse ("{\"key1\":[1,1.3], \"key2\":\"abc\"}") << endl;
    cout << JsonParser::parse ("{\"key1\":{\"a\":1,\"b\":2}, \"key2\":\"abc\"}") << endl;
    cout << JsonParser::parse ("{\"key1\":{\"r\":1,\"i\":2}, \"key2\":\"abc\"}") << endl;
    cout << JsonParser::parse ("{\"key1\":1 /* ,,, */ ,\"Key2\":true } ") << endl;

    JsonKVMap par = JsonParser::parseFile ("tJsonKVMap.in");
    cout << par << endl;
    Array<double> exp(IPosition(3,4,3,2));
    indgen(exp);
    AlwaysAssertExit (allNear(par["keyarr"].getArrayDouble(), exp, 1e-5));
  } catch (std::exception& x) {
    cout << x.what() << endl;
  }
}

int main()
{
  try {
    doIt();
    doItParse();
  } catch (...) {
    cout << "Unexpected exception" << endl;
    exit(1);
  }
  cout << "OK" << endl;
  exit(0);
}
