//# tJsonOut.cc: Test program for class JsonOut.cc
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

#include <casacore/casa/Json/JsonOut.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <iostream>
#include <stdexcept>

using namespace casacore;

int main()
{
  try {
    String indent("     ");
    JsonOut os("tJsonOut_tmp.txt");
    os.start("#");
    for (int ndim=0; ndim<4; ++ndim) {
      Array<int32_t> a(IPosition(ndim,3));
      indgen(a);
      os.write ("arr", a);
    }
    Array<String> as(IPosition(2,2,2));
    as.data()[0] = "s0";
    as.data()[1] = "s1";
    as.data()[2] = "s\n2";
    as.data()[3] = "s\"3";
    os.write ("arrs", as, "comment1");
    os.write ("key1", true);
    os.write ("key2", 1, "comment2");
    os.write ("key3", -10);
    os.write ("key4", 11.5f);
    os.write ("keyfa", -1e20f);
    os.write ("keyfb", -1e-20f);
    os.write ("keyfc", -1.f);
    os.write ("key5", -13.2345);
    os.write ("keyda", -1e20);
    os.write ("keydb", -1e-20);
    os.write ("keydc", -1.);
    os.write ("keydd", -123456789012345.123456789012345);
    os.write ("key6", Complex(-1,2));
    os.write ("key7", ValueHolder(DComplex(3,-4)));
    os.write ("key8", "string");
    os.write ("key9", "\t str\\\"ing\n "); // str\"ing
    os.write ("key10", String("ab") + char(1) + "cd");
    os.write ("null1", ValueHolder());     // null value
    os.write ("null2", floatNaN());        // null value
    os.write ("null3", doubleNaN());       // null value
    Array<float> arr(IPosition(2,5,3));
    indgen(arr);
    os.write ("arrf", ValueHolder(arr));
    Record rec;
    rec.define ("recfld", 1);
    rec.define ("recarr", arr);
    Record subrec;
    subrec.define("subf1", 2);
    subrec.define("subf2", arr+float(15));
    rec.defineRecord ("recsub", subrec);
    os.write ("rec", rec);
    os.end();
  } catch (std::exception& x) {
    std::cout << x.what() << std::endl;
    return 1;
  }
  return 0;
}
