//# tModcompConversion.h: Test program for class ModcompConversion
//# Copyright (C) 1999
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


#include <trial/OS/ModcompConversion.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <iostream.h>
#include <iomanip.h>

// This program tests the Modcomp conversion functions.

void checkConversion (Int& error)
{
  Char val[4];
  Char out[4];
  {
    val[0] = -2;
    Char result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != -2) {
      cout << "invalid char to conversion" << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]) {
      cout << "invalid char from conversion" << endl;
      error = 1;
    }
  }
  {
    val[0] = -2;
    uChar result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 254) {
      cout << "invalid unsigned char to conversion" << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out+1, &result, 1);
    if (out[0] != val[0]) {
      cout << "invalid unsigned char from conversion" << endl;
      error = 1;
    }
  }
  {
    val[0] = 2;
    val[1] = 1;
    Short result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 513) {
      cout << "invalid short to conversion 1: " << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]) {
      cout << "invalid short from conversion 1" << endl;
      error = 1;
    }
    val[0] = -2;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != -2*256 + 1) {
      cout << "invalid short to conversion 2: " << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]) {
      cout << "invalid short from conversion 2: " << endl;
      error = 1;
    }
  }
  {
    val[0] = 7;
    val[1] = 111;
    uShort result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 7*256 + 111) {
      cout << "invalid unsigned short to conversion " << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]) {
      cout << "invalid unsigned short from conversion" << endl;
      error = 1;
    }
  }
  {
    val[0] = 1;
    val[1] = 2;
    val[2] = 3;
    val[3] = 0;
    Int result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 1*256*256*256 + 2*256*256 + 3*256 + 0) {
      cout << "invalid int to conversion 1: " << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]
 	||  out[2] != val[2]  ||  out[3] != val[3]) {
      cout << "invalid int from conversion 1" << endl;
      error = 1;
    }
    val[0] = -127;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != -127*256*256*256 + 2*256*256 + 3*256) {
      cout << "invalid int to conversion 2" << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]
 	||  out[2] != val[2]  ||  out[3] != val[3]) {
      cout << "invalid int from conversion 2" << endl;
      error = 1;
    }
  }
  {
    val[0] = 11;
    val[1] = 23;
    val[2] = 35;
    val[3] = 7;
    uInt result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 11*256*256*256 + 23*256*256 + 35*256 + 7) {
      cout << "invalid unsigned int to conversion" << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]
 	||  out[2] != val[2]  ||  out[3] != val[3]) {
      cout << "invalid unsigned int from conversion" << endl;
      error = 1;
    }
  }
  {
    val[0] = 2;
    val[1] = 54;
    val[2] = 78;
    val[3] = 145;
    Long result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 2*256*256*256 + 54*256*256 + 78*256 + 145) {
      cout << "invalid long to conversion " << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]
 	||  out[2] != val[2]  ||  out[3] != val[3]) {
      cout << "invalid long from conversion" << endl;
      error = 1;
    }
    val[0] = -2;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != -(1*256*256*256 + 201*256*256 + 177*256 + 111)) {
      cout << "invalid long to conversion 2: " << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]
	||  out[2] != val[2]  ||  out[3] != val[3]) {
      cout << "invalid long from conversion 2" << endl;
      error = 1;
    }
  }
  {
    val[0] = 128;
    val[1] = 54;
    val[2] = 78;
    val[3] = 100;
    uLong result;
    ModcompConversion::toLocal (&result, val, 1);
    if (result != 128U*256U*256U*256U + 54U*256U*256U + 78U*256U + 100U) {
      cout << "invalid unsigned long to conversion" << result << endl;
      error = 1;
    }
    ModcompConversion::fromLocal (out, &result, 1);
    if (out[0] != val[0]  ||  out[1] != val[1]
	||  out[2] != val[2]  ||  out[3] != val[3]) {
      cout << "invalid unsigned long from conversion" << endl;
      error = 1;
    }
  }
}

void compareFloat (Int& error, Float exp, Float res) {
  union {
    Float result;
    uChar byteResult[4];
  };
  result = res;
  union {
    Float expected;
    uChar byteExpected[4];
  };
  expected = exp;
  // Compare the results. Allow the answers to differ in the LSB as the
  // conversion from Double to Float rounds to the nearest floating point
  // number whereas the conversion from ModComp to IEEE rounds down.
  if (abs(result - expected) > 1.4013e-45) { // This number is the
                                             // smallest subnormal number
    error = 1;
    cout << "expected " << expected;
    cout << setbase(16) << " ("
	 << Int(byteExpected[3]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[0]) << ")";
    cout << " got " << result;
    cout << setbase(16) << " ("
	 << Int(byteResult[3]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[0]) << ")";
    cout << endl;
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
      ModcompConversion::toLocal (&result, data, 1);
      compareFloat(error, expected, result);
      cdata[0] = ~data[0];
      cdata[1] = ~data[1];
      cdata[2] = ~data[2];
      cdata[3] = ~data[3];
      cdata[3]++; // There can be no carry as data[3] == 0x01;
      ModcompConversion::toLocal (&result, cdata, 1);
      compareFloat(error, -1.0*expected, result);
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
    ModcompConversion::toLocal (result2, data2, 2);
    compareFloat(error, plusZero, result2[0]);
    compareFloat(error, minusZero, result2[1]);
  }
}

void compareDouble (Int& error, Double exp, Double res) {
  union {
    Double result;
    uChar byteResult[8];
  };
  result = res;
  union {
    Double expected;
    uChar byteExpected[8];
  };
  expected = exp;
  // Compare the results. Allow the answers to differ in the LSB as the
  // conversion from Double to Float rounds to the nearest floating point
  // number whereas the conversion from ModComp to IEEE rounds down.
  if (abs(result - expected) > 0) { // This number is the
                                             // smallest subnormal number
    error = 1;
    cout << "expected " << expected;
    cout << setbase(16) << " ("
	 << Int(byteExpected[7]) << ":"
	 << Int(byteExpected[6]) << ":"
	 << Int(byteExpected[5]) << ":"
	 << Int(byteExpected[4]) << ":"
	 << Int(byteExpected[3]) << ":"
	 << Int(byteExpected[2]) << ":"
	 << Int(byteExpected[1]) << ":"
	 << Int(byteExpected[0]) << ")";
    cout << " got " << result;
    cout << setbase(16) << " ("
	 << Int(byteResult[7]) << ":"
	 << Int(byteResult[6]) << ":"
	 << Int(byteResult[5]) << ":"
	 << Int(byteResult[4]) << ":"
	 << Int(byteResult[3]) << ":"
	 << Int(byteResult[2]) << ":"
	 << Int(byteResult[1]) << ":"
	 << Int(byteResult[0]) << ")";
    cout << endl;
  }
}

void checkDouble (Int& error) {
}

main()
{
    Int error = 0;
    checkConversion (error);
    checkFloat (error);
    checkDouble (error);
    // Exit when errors found.
    if (error) {
	return 1;
    }
    cout << "OK" << endl;
    return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tModcompConversion"
// End: 
