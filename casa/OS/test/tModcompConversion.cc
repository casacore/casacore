//# tModcompConversion.h: Test program for class ModcompConversion
//# Copyright (C) 1999,2000,2001,2002
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

#include <casacore/casa/OS/ModcompConversion.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

#include <casacore/casa/namespace.h>
// This program tests the Modcomp conversion functions.

void compare(Int& error, Char exp, Char res) {
  // Compare the results. 
  if (res != exp) {
    error = 1;
    uChar byteResult[1];
    uChar byteExpected[1];
    memcpy (byteResult, &res, 1);
    memcpy (byteExpected, &exp, 1);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" << Int(byteExpected[0]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" << Int(byteResult[0]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, uChar exp, uChar res) {
  // Compare the results. 
  if (res != exp) {
    error = 1;
    uChar byteResult[1];
    uChar byteExpected[1];
    memcpy (byteResult, &res, 1);
    memcpy (byteExpected, &exp, 1);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" << Int(byteExpected[0]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" << Int(byteResult[0]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, Short exp, Short res) {
  // Compare the results. 
  if (res != exp) {
    error = 1;
    uChar byteResult[2];
    uChar byteExpected[2];
    memcpy (byteResult, &res, 2);
    memcpy (byteExpected, &exp, 2);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" 
	 <<Int(byteExpected[0]) << ":"
	 << Int(byteExpected[1]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" 
	 << Int(byteResult[0]) << ":"
	 << Int(byteResult[1]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, uShort exp, uShort res) {
  // Compare the results. 
  if (res != exp) {
    error = 1;
    uChar byteResult[2];
    uChar byteExpected[2];
    memcpy (byteResult, &res, 2);
    memcpy (byteExpected, &exp, 2);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" 
	 << Int(byteExpected[0]) << ":"
	 << Int(byteExpected[1]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" 
	 << Int(byteResult[0]) << ":"
	 << Int(byteResult[1]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, Int exp, Int res) {
  // Compare the results.
  if (res != exp) {
    error = 1;
    uChar byteResult[4];
    uChar byteExpected[4];
    memcpy (byteResult, &res, 4);
    memcpy (byteExpected, &exp, 4);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" 
	 << Int(byteExpected[0]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[3]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" 
	 << Int(byteResult[0]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[3]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, uInt exp, uInt res) {
  // Compare the results.
  if (res != exp) {
    error = 1;
    uChar byteResult[4];
    uChar byteExpected[4];
    memcpy (byteResult, &res, 4);
    memcpy (byteExpected, &exp, 4);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" 
	 << Int(byteExpected[0]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[3]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" 
	 << Int(byteResult[0]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[3]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, Int64 exp, Int64 res) {
  // Compare the results.
  if (res != exp) {
    error = 1;
    uChar byteResult[8];
    uChar byteExpected[8];
    memcpy (byteResult, &res, 8);
    memcpy (byteExpected, &exp, 8);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" 
	 << Int(byteExpected[0]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[3]) << ":"
	 << Int(byteExpected[4]) << ":"
	 << Int(byteExpected[5]) << ":"
	 << Int(byteExpected[6]) << ":"
	 << Int(byteExpected[7]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" 
	 << Int(byteResult[0]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[3]) << ":"
	 << Int(byteResult[4]) << ":"
	 << Int(byteResult[5]) << ":"
	 << Int(byteResult[6]) << ":"
	 << Int(byteResult[7]) << ")";
    cerr << setbase(10) << endl;
  }
}

void compare(Int& error, uInt64 exp, uInt64 res) {
  // Compare the results.
  if (res != exp) {
    error = 1;
    uChar byteResult[8];
    uChar byteExpected[8];
    memcpy (byteResult, &res, 8);
    memcpy (byteExpected, &exp, 8);
    cerr << "expected " << exp;
    cerr << setbase(16) << " (" 
	 << Int(byteExpected[0]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[3]) << ":"
	 << Int(byteExpected[4]) << ":"
	 << Int(byteExpected[5]) << ":"
	 << Int(byteExpected[6]) << ":"
	 << Int(byteExpected[7]) << ")";
    cerr << setbase(10) << " got " << res;
    cerr << setbase(16) << " (" 
	 << Int(byteResult[0]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[3]) << ":"
	 << Int(byteResult[4]) << ":"
	 << Int(byteResult[5]) << ":"
	 << Int(byteResult[6]) << ":"
	 << Int(byteResult[7]) << ")";
    cerr << setbase(10) << endl;
  }
}

void checkConversion (Int& error)
{
  {
    Char input[2];
    input[0] = 'A';
    input[1] = 'B';
    Char result[2];
    result[0] = 'Z';
    result[1] = 'Z';
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 1, AipsError);
    compare(error, 'A', result[0]);
    result[0] = 'Z';
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, 'A', result[0]);
    compare(error, 'B', result[1]);
    result[0] = 'a';
    result[1] = 'b';
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, 'a', input[0]);
    compare(error, 'b', input[1]);
    result[0] = 'Z';
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 1, AipsError);
    compare(error, 'Z', input[0]);
  }
  {
    uChar input[2];
    input[0] = 0xa5;
    input[1] = 0xff;
    uChar result[2];
    result[0] = 0x00;
    result[1] = 0x00;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 1, AipsError);
    compare(error, uChar(0xa5), result[0]);
    result[0] = 0x00;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, uChar(0xa5), result[0]);
    compare(error, uChar(0xff), result[1]);
    result[0] = 0xfe;
    result[1] = 0x5a;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, uChar(0xfe), input[0]);
    compare(error, uChar(0x5a), input[1]);
    result[0] = 0x00;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 1, AipsError);
    compare(error, uChar(0x00), input[0]);
  }
  {
    uChar input[4];
    input[0] = 0x7e;
    input[1] = 0xa5;
    input[2] = 0xff;
    input[3] = 0xef;
    Short result[2];
    result[0] = 0;
    result[1] = 0;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, Short(32421), result[0]);
    result[0] = 0;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, Short(32421), result[0]);
    compare(error, Short(-17), result[1]);
    result[0] = -2;
    result[1] = 30000;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uChar(0xff), input[0]);
    compare(error, uChar(0xfe), input[1]);
    compare(error, uChar(0x75), input[2]);
    compare(error, uChar(0x30), input[3]);
    result[0] = 1;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, uChar(0x00), input[0]);
    compare(error, uChar(0x01), input[1]);
  }
  {
    uChar input[4];
    input[0] = 0x7e;
    input[1] = 0xa5;
    input[2] = 0xff;
    input[3] = 0xef;
    uShort result[2];
    result[0] = 0;
    result[1] = 0;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, uShort(32421), result[0]);
    result[0] = 0;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uShort(32421), result[0]);
    compare(error, uShort(65519), result[1]);
    result[0] = 65534;
    result[1] = 30000;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uChar(0xff), input[0]);
    compare(error, uChar(0xfe), input[1]);
    compare(error, uChar(0x75), input[2]);
    compare(error, uChar(0x30), input[3]);
    result[0] = 1;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 2, AipsError);
    compare(error, uChar(0x00), input[0]);
    compare(error, uChar(0x01), input[1]);
  }
  {
    uChar input[8];
    input[0] = 0x7e;
    input[1] = 0xa5;
    input[2] = 0x43;
    input[3] = 0x21;
    input[4] = 0xff;
    input[5] = 0xef;
    input[6] = 0xab;
    input[7] = 0xcd;
    Int result[2];
    result[0] = 0;
    result[1] = 0;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, 2124759841, result[0]);
    result[0] = 0;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, 2124759841, result[0]);
    compare(error, -1070131, result[1]);
    result[0] = -19088744;
    result[1] = 305419896;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, uChar(0xfe), input[0]);
    compare(error, uChar(0xdc), input[1]);
    compare(error, uChar(0xba), input[2]);
    compare(error, uChar(0x98), input[3]);
    compare(error, uChar(0x12), input[4]);
    compare(error, uChar(0x34), input[5]);
    compare(error, uChar(0x56), input[6]);
    compare(error, uChar(0x78), input[7]);
    result[0] = 591751049;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uChar(0x23), input[0]);
    compare(error, uChar(0x45), input[1]);
    compare(error, uChar(0x67), input[2]);
    compare(error, uChar(0x89), input[3]);
  }
  {
    uChar input[8];
    input[0] = 0x7e;
    input[1] = 0xa5;
    input[2] = 0x43;
    input[3] = 0x21;
    input[4] = 0xff;
    input[5] = 0xef;
    input[6] = 0xab;
    input[7] = 0xcd;
    uInt result[2];
    result[0] = 0u;
    result[1] = 0u;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, 2124759841u, result[0]);
    result[0] = 0;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, 2124759841u, result[0]);
    compare(error, 4293897165u, result[1]);
    result[0] = 4275878552u;
    result[1] = 305419896u;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, uChar(0xfe), input[0]);
    compare(error, uChar(0xdc), input[1]);
    compare(error, uChar(0xba), input[2]);
    compare(error, uChar(0x98), input[3]);
    compare(error, uChar(0x12), input[4]);
    compare(error, uChar(0x34), input[5]);
    compare(error, uChar(0x56), input[6]);
    compare(error, uChar(0x78), input[7]);
    result[0] = 591751049;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uChar(0x23), input[0]);
    compare(error, uChar(0x45), input[1]);
    compare(error, uChar(0x67), input[2]);
    compare(error, uChar(0x89), input[3]);
  }
  {
    uChar input[8];
    input[0] = 0x7e;
    input[1] = 0xa5;
    input[2] = 0x43;
    input[3] = 0x21;
    input[4] = 0xff;
    input[5] = 0xef;
    input[6] = 0xab;
    input[7] = 0xcd;
    Int64 result[2];
    result[0] = 0;
    result[1] = 0;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, 2124759841L, result[0]);
    result[0] = 0;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, 2124759841L, result[0]);
    compare(error, -1070131L, result[1]);
    result[0] = -19088744;
    result[1] = 305419896;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, uChar(0xfe), input[0]);
    compare(error, uChar(0xdc), input[1]);
    compare(error, uChar(0xba), input[2]);
    compare(error, uChar(0x98), input[3]);
    compare(error, uChar(0x12), input[4]);
    compare(error, uChar(0x34), input[5]);
    compare(error, uChar(0x56), input[6]);
    compare(error, uChar(0x78), input[7]);
    result[0] = 591751049;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uChar(0x23), input[0]);
    compare(error, uChar(0x45), input[1]);
    compare(error, uChar(0x67), input[2]);
    compare(error, uChar(0x89), input[3]);
  }
  {
    uChar input[8];
    input[0] = 0x7e;
    input[1] = 0xa5;
    input[2] = 0x43;
    input[3] = 0x21;
    input[4] = 0xff;
    input[5] = 0xef;
    input[6] = 0xab;
    input[7] = 0xcd;
    uInt64 result[2];
    result[0] = 0u;
    result[1] = 0u;
    uInt nbytes = ModcompConversion::toLocal(result[0], input);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, 2124759841uL, result[0]);
    result[0] = 0;
    nbytes = ModcompConversion::toLocal(result, input, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, 2124759841uL, result[0]);
    compare(error, 4293897165uL, result[1]);
    result[0] = 4275878552u;
    result[1] = 305419896u;
    nbytes = ModcompConversion::fromLocal(input, result, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, uChar(0xfe), input[0]);
    compare(error, uChar(0xdc), input[1]);
    compare(error, uChar(0xba), input[2]);
    compare(error, uChar(0x98), input[3]);
    compare(error, uChar(0x12), input[4]);
    compare(error, uChar(0x34), input[5]);
    compare(error, uChar(0x56), input[6]);
    compare(error, uChar(0x78), input[7]);
    result[0] = 591751049;
    nbytes = ModcompConversion::fromLocal(input, result[0]);
    AlwaysAssert(nbytes == 4, AipsError);
    compare(error, uChar(0x23), input[0]);
    compare(error, uChar(0x45), input[1]);
    compare(error, uChar(0x67), input[2]);
    compare(error, uChar(0x89), input[3]);
  }
}

void compare(Int& error, Float exp, Float res) {
  // Compare the results. Allow the answers to differ in the LSB as the
  // conversion from Double to Float rounds to the nearest floating point
  // number whereas the conversion from ModComp to IEEE rounds down.
  if (std::abs(res - exp) > 1.4013e-45) {    // This number is the
                                             // smallest subnormal number
    error = 1;
    uChar byteResult[4];
    uChar byteExpected[4];
    memcpy (byteResult, &res, 4);
    memcpy (byteExpected, &exp, 4);
    cerr << "expected " << exp;
    cerr << setbase(16) << " ("
	 << Int(byteExpected[3]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[0]) << ")";
    cerr << " got " << res;
    cerr << setbase(16) << " ("
	 << Int(byteResult[3]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[0]) << ")";
    cerr << endl;
  }
}

void checkFloat (Int& error)
{
  Float result;
  Float expected;

  uChar data[4], cdata[4];
  data[2] = 0x00;
  data[3] = 0x01; // put a bit at the end to make sure it does not get lost
  for (uShort j = 0; j < 512; j++) { // exponent is nine bits
    data[0] = j >> 2;
    Double exponent;
    if (j > 256) {
      exponent = pow(2.0, Double(j - 256));
    } else {
      exponent = 1.0 / pow(2.0, Double(256 - j));
    }
    for (uInt k=0; k<63; k++) {// cannot cycle through all mantissa' it would
      // take too long. So just increment through the 5-MSB's
      // This tests both normalised and unnormailsed numbers & positive and
      // negative infinity.
      data[1] = (j & 0x0003) << 6 | k; 
      expected = exponent * (Double(k)/64.0 + 1.0/(256*256*64));
      uInt nbytes = ModcompConversion::toLocal(result, data);
      AlwaysAssert(nbytes == 4, AipsError);
      compare(error, expected, result);
      cdata[0] = ~data[0];
      cdata[1] = ~data[1];
      cdata[2] = ~data[2];
      cdata[3] = ~data[3];
      cdata[3]++; // There can be no carry as data[3] == 0x01;
      nbytes = ModcompConversion::toLocal(result, cdata);
      AlwaysAssert(nbytes == 4, AipsError);
      compare(error, -1.0*expected, result);
    }
  }
  // Check that the conversion for zero works. Try all possible representations
  // of zero ie., all numbers with a zero mantissa
  uChar data2[8];
  data2[0] = data2[1] = data2[2] = data2[3] = 0x00;
  data2[4] = data2[5] = data2[6] = data2[7] = 0x00;
  Float result2[2]; 
  result2[1] = result2[0] = 1.0f;
  const Float plusZero = 0.0f;
  const Float minusZero = -0.0f;
  for (uInt i = 0; i < 512; i++) {
    data2[0] = i >> 2;
    data2[4] = data2[0] | 0x80;
    data2[5] = data2[1] = (i & 0x03) << 6;
    uInt nbytes = ModcompConversion::toLocal(result2, data2, 2);
    AlwaysAssert(nbytes == 8, AipsError);
    compare(error, plusZero, result2[0]);
    compare(error, minusZero, result2[1]);
  }
}

void compare(Int& error, Double exp, Double res) {
  // Compare the results. Allow the answers to differ in the LSB as the
  // conversion from Double to Float rounds to the nearest floating point
  // number whereas the conversion from ModComp to IEEE rounds down.
  if (std::abs(res - exp) > 0) {
    error = 1;
    uChar byteResult[8];
    uChar byteExpected[8];
    memcpy (byteResult, &res, 8);
    memcpy (byteExpected, &exp, 8);
    cerr << "expected " << exp;
    cerr << setbase(16) << " ("
	 << Int(byteExpected[7]) << ":"
	 << Int(byteExpected[6]) << ":"
	 << Int(byteExpected[5]) << ":"
	 << Int(byteExpected[4]) << ":"
	 << Int(byteExpected[3]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[0]) << ")";
    cerr << " got " << res;
    cerr << setbase(16) << " ("
	 << Int(byteResult[7]) << ":"
	 << Int(byteResult[6]) << ":"
	 << Int(byteResult[5]) << ":"
	 << Int(byteResult[4]) << ":"
	 << Int(byteResult[3]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[0]) << ")";
    cerr << endl;
  }
}

void checkDouble (Int& error) {
  Double result;
  Double expected;

  uChar data[8], cdata[8];
  data[2] = data[3] = data[4] = data[5] = data[6] = 0x00;
  data[7] = 0x02; // put a bit nearly at the end to make sure it does 
                  // not get lost. The last bit IS lost.
  for (uShort j = 0; j < 512; j++) { // exponent is nine bits
    data[0] = j >> 2;
    Double exponent;
    if (j > 256) {
      exponent = pow(2.0, Double(j - 256));
    } else {
      exponent = 1.0 / pow(2.0, Double(256 - j));
    }
    for (uInt k=0; k<63; k++) {// cannot cycle through all mantissa' it would
      // take too long. So just increment through the 5-MSB's
      // This tests both normalised and unnormalised numbers
      data[1] = (j & 0x0003) << 6 | k; 
      expected = exponent * (Double(k)/64.0 + 1.0/65536/65536/65536/32);
      uInt nbytes = ModcompConversion::toLocal(result, data);
      AlwaysAssert(nbytes == 8, AipsError);
      compare(error, expected, result);
      cdata[0] = ~data[0];
      cdata[1] = ~data[1];
      cdata[2] = ~data[2];
      cdata[3] = ~data[3];
      cdata[4] = ~data[4];
      cdata[5] = ~data[5];
      cdata[6] = ~data[6];
      cdata[7] = ~data[7];
      cdata[7]++; // There can be no carry as data[7] == 0x02;
      nbytes = ModcompConversion::toLocal(result, cdata);
      AlwaysAssert(nbytes == 8, AipsError);
      compare(error, -1.0*expected, result);
    }
  }
  // Check that the conversion for zero works. Try all possible representations
  // of zero ie., all numbers with a zero mantissa
  uChar data2[16];
  data2[0] = data2[1] = data2[2] = data2[3] = 0x00;
  data2[4] = data2[5] = data2[6] = data2[7] = 0x00;
  data2[8] = data2[9] = data2[10] = data2[11] = 0x00;
  data2[12] = data2[13] = data2[14] = data2[15] = 0x00;
  Double result2[2]; 
  result2[1] = result2[0] = 1.0;
  const Double plusZero = 0.0;
  const Double minusZero = -0.0;
  for (uInt i = 0; i < 512; i++) {
    data2[0] = i >> 2;
    data2[8] = data2[0] | 0x80;
    data2[9] = data2[1] = (i & 0x03) << 6;
    uInt nbytes = ModcompConversion::toLocal(result2, data2, 2);
    AlwaysAssert(nbytes == 16, AipsError);
    compare(error, plusZero, result2[0]);
    compare(error, minusZero, result2[1]);
  }
}

int main()
{
  Int error = 0;
  try {
    checkConversion (error);
    checkFloat (error);
    checkDouble (error);
  } 
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  // Exit when errors found.
  if (error) {
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tModcompConversion"
// End: 
