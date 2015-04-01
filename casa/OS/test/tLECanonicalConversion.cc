//# tLECanonicalConversion.h: Test program for class LECanonicalConversion
//# Copyright (C) 2002
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


#include <casacore/casa/OS/LECanonicalConversion.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>


#include <casacore/casa/namespace.h>
// Check if all conversion definitions are set correctly.
// It write error messages to cout and exits (with status 1)
// when errors are found.
void checkFlags (int& error)
{
    int flag;

    // Check if byte order is correctly defined.
    // In AIPS_LITTLE_ENDIAN the first byte of an int==1 should be 1.
    // Otherwise it should be 0.
    int val = 1;
#if defined(AIPS_LITTLE_ENDIAN)
    if (((char*)(&val))[0] != 1) {
	cout << "AIPS_LITTLE_ENDIAN should not be defined"
	     << endl;
	error = 1;
    }
#else
    if (((char*)(&val))[0] != 0) {
	cout << "AIPS_LITTLE_ENDIAN should be defined"
	     << endl;
	error = 1;
    }
#endif

    // Check for (unsigned) char.
    flag = sizeof(char) != SIZE_LECAN_CHAR;
    if ((flag^CONVERT_LECAN_CHAR) != 0) {
	cout << "invalid CONVERT_LECAN_CHAR definition"
	     << endl;
	error = 1;
    }
    flag = sizeof(unsigned char) != SIZE_LECAN_UCHAR;
    if ((flag^CONVERT_LECAN_UCHAR) != 0) {
	cout << "invalid CONVERT_LECAN_UCHAR definition"
	     << endl;
	error = 1;
    }
    if (sizeof(char) != 1) {
	cout << "sizeof(char) must be 1" << endl;
	error = 1;
    }
    if (sizeof(unsigned char) != 1) {
	cout << "sizeof(char) must be 1" << endl;
	error = 1;
    }

    // When AIPS_LITTLE_ENDIAN is not defined, conversion is always needed.
#if !defined(AIPS_LITTLE_ENDIAN)
    flag = 1;
#endif

    // Check for (unsigned) short.
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(short) != SIZE_LECAN_SHORT;
#endif
    if ((flag^CONVERT_LECAN_SHORT) != 0) {
	cout << "invalid CONVERT_LECAN_SHORT definition"
	     << endl;
	error = 1;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(unsigned short) != SIZE_LECAN_USHORT;
#endif
    if ((flag^CONVERT_LECAN_USHORT) != 0) {
	cout << "invalid CONVERT_LECAN_USHORT definition"
	     << endl;
	error = 1;
    }
    if (sizeof(short) < 2) {
	cout << "sizeof(short) must be >=2" << endl;
	error = 1;
    }
    if (sizeof(unsigned short) < 2) {
	cout << "sizeof(unsigned short) must be >=2" << endl;
	error = 1;
    }

    // Check for (unsigned) int.
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(int) != SIZE_LECAN_INT;
#endif
    if ((flag^CONVERT_LECAN_INT) != 0) {
	cout << "invalid CONVERT_LECAN_INT definition"
	     << endl;
	error = 1;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(unsigned int) != SIZE_LECAN_UINT;
#endif
    if ((flag^CONVERT_LECAN_UINT) != 0) {
	cout << "invalid CONVERT_LECAN_UINT definition"
	     << endl;
	error = 1;
    }
    if (sizeof(int) < 4) {
	cout << "sizeof(int) must be >=4" << endl;
	error = 1;
    }
    if (sizeof(unsigned int) < 4) {
	cout << "sizeof(unsigned int) must be >=4" << endl;
	error = 1;
    }

    // Check for (unsigned) int64
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(Int64) != SIZE_LECAN_INT64;
#endif
    if ((flag^CONVERT_LECAN_INT64) != 0) {
	cout << "invalid CONVERT_LECAN_INT64 definition"
	     << endl;
	error = 1;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(uInt64) != SIZE_LECAN_UINT64;
#endif
    if ((flag^CONVERT_LECAN_UINT64) != 0) {
	cout << "invalid CONVERT_LECAN_UINT64 definition"
	     << endl;
	error = 1;
    }
    if (sizeof(Int64) < 8) {
	cout << "sizeof(Int64) must be >=8" << endl;
	error = 1;
    }
    if (sizeof(uInt64) < 8) {
	cout << "sizeof(uInt64) must >=8" << endl;
	error = 1;
    }

    // Check for float and double.
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(float) != SIZE_LECAN_FLOAT;
#endif
    if ((flag^CONVERT_LECAN_FLOAT) != 0) {
	cout << "invalid CONVERT_LECAN_FLOAT definition"
	     << endl;
	error = 1;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    flag = sizeof(double) != SIZE_LECAN_DOUBLE;
#endif
    if ((flag^CONVERT_LECAN_DOUBLE) != 0) {
	cout << "invalid CONVERT_LECAN_DOUBLE definition"
	     << endl;
	error = 1;
    }
    if (sizeof(float) != 4) {
	cout << "sizeof(float) must be 4" << endl;
	error = 1;
    }
    if (sizeof(double) != 8) {
	cout << "sizeof(double) must be 8" << endl;
	error = 1;
    }

}


void checkConversion (int& error)
{
    char val[9];
    char out[9];
    {
	val[1] = -1;
	char result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != -1) {
	    cout << "invalid char to conversion " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]) {
	    cout << "invalid char from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[1] = -2;
	unsigned char result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 254) {
	    cout << "invalid unsigned char to conversion " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]) {
	    cout << "invalid unsigned char from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[2] = 1;
	val[1] = 2;
	short result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 258) {
	    cout << "invalid short to conversion 1 " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid short from conversion 1" << endl;
	    error = 1;
	}
	val[2] = -2;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != -(1*256 + 254)) {
	    cout << "invalid short to conversion 2 " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid short from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[2] = 7;
	val[1] = 111;
	unsigned short result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 7*256 + 111) {
	    cout << "invalid unsigned short to conversion " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid unsigned short from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[4] = 1;
	val[3] = 2;
	val[2] = 3;
	val[1] = 0;
	int result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 1*256*256*256 + 2*256*256 + 3*256 + 0) {
	    cout << "invalid int to conversion 1 " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid int from conversion 1" << endl;
	    error = 1;
	}
	val[4] = -127;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != -(126*256*256*256 + 253*256*256 + 252*256 + 256)) {
	    cout << "invalid int to conversion 2 " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid int from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[4] = 11;
	val[3] = 23;
	val[2] = 35;
	val[1] = 7;
	unsigned int result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 11*256*256*256 + 23*256*256 + 35*256 + 07) {
	    cout << "invalid unsigned int to conversion " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid unsigned int from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[8] = 0;
	val[7] = 0;
	val[6] = 0;
	val[5] = 6;
	val[4] = 2;
	val[3] = 54;
	val[2] = 78;
	val[1] = 145-256;
	Int64 result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 2*256*256*256 + 54*256*256 + 78*256 + 145
	              + 6*(Int64)(256)*256*256*256) {
	    cout << "invalid Int64 to conversion 1 " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]
	||  out[5] != val[5]  ||  out[6] != val[6]
	||  out[7] != val[7]  ||  out[8] != val[8]) {
	    cout << "invalid Int64 from conversion 1" << endl;
	    error = 1;
	}
	val[8] = -1;
	val[7] = -1;
	val[6] = -1;
	val[5] = -2;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != -((Int64)(256)*256*256*256
			+ (Int64)(253)*256*256*256
			+ 201*256*256 + 177*256 + 111)) {
	    cout << "invalid Int64 to conversion 2 " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]
	||  out[5] != val[5]  ||  out[6] != val[6]
	||  out[7] != val[7]  ||  out[8] != val[8]) {
	    cout << "invalid Int64 from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[8] = 0;
	val[7] = 0;
	val[6] = 0;
	val[5] = 5;
	val[4] = 128-256;
	val[3] = 54;
	val[2] = 78;
	val[1] = 100;
	uInt64 result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	if (result != 128U*256U*256U*256U + 54U*256U*256U + 78U*256U + 100U
	              + 5U*(uInt64)(256)*256U*256U*256U) {
	    cout << "invalid uInt64 to conversion " << result << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]
	||  out[5] != val[5]  ||  out[6] != val[6]
	||  out[7] != val[7]  ||  out[8] != val[8]) {
	    cout << "invalid uInt64 from conversion" << endl;
	    error = 1;
	}
    }
    {
	// Try a float with a positive exponent and sign.
	// An IEEE float looks like SEEEEEEE EFFFFFFF FFFFFFFF FFFFFFFF.
	// The first bit of the fraction is hidden (is 1 and not stored).
	// Exponent-126 is the true exponent (base 2).
	val[4] = 63;
	val[3] = 192-256;
	val[2] = 1;
	val[1] = 7;
	float result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	double d = pow(double(2), double(127-126));
	d *= double(192)/256 + double(1)/(256*256) + double(7)/(256*256*256);
	float v = d;
	if (result != float(v)) {
	    cout << "invalid float to conversion " << result << " " <<v<<endl;
	    unsigned char* cv = (unsigned char*)&result;
	    cout << hex;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    cv = (unsigned char*)&v;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    cv = (unsigned char*)&result;
	    cout << dec;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    cv = (unsigned char*)&v;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid float from conversion" << endl;
	    error = 1;
	}
    }
    {
	// Try a float with a negative exponent and sign.
	val[4] = 128 + 54 - 256;
	val[3] = 64;
	val[2] = 23;
	val[1] = 34;
	float result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	double d = -1 / pow(double(2), double(126-54*2));
	d *= double(192)/256 + double(23)/(256*256) + double(34)/(256*256*256);
	float v = d;
	if (result != v) {
	    cout << "invalid float to conversion " << setprecision(20)
		 << result << " " << setprecision(20) << v <<endl;
	    unsigned char* cv = (unsigned char*)&result;
	    cout << hex;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    cv = (unsigned char*)&v;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    cv = (unsigned char*)&result;
	    cout << dec;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    cv = (unsigned char*)&v;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid float from conversion" << endl;
	    error = 1;
	}
    }
    {
	// Try a double with a positive exponent and sign.
	// An IEEE float looks like SEEEEEEE EEEEFFFF FFFFFFFF ... FFFFFFFF.
	// The first bit of the fraction is hidden (is 1 and not stored).
	// Exponent-1022 is the true exponent (base 2).
	val[8] = 63;
	val[7] = 64+32+16 + 15;
	val[6] = 1;
	val[5] = 2;
	val[4] = 3;
	val[3] = 4;
	val[2] = 5;
	val[1] = 0;
	double result;
	LECanonicalConversion::toLocal (&result, val+1, 1);
	float w = pow(double(2), double(1022 - 63*16 - (64+32+16)/16));
	double v = w;
	v = 1/v;
	v *= double(16+15)/32 + double(1)/(32*256) + double(2)/(32*256*256)
	    + double(3)/(32*256*256*256)
	    + double(4)/(32*double(256*256*256)*256)
	    + double(5)/(32*double(256*256*256)*256*256);
	if (result != v) {
	    cout << "invalid double to conversion " << result << " " <<v<<endl;
	    unsigned char* cv = (unsigned char*)&result;
	    cout << hex;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << ' '
		 << int(cv[4]) << ' ' << int(cv[5]) << ' '
		 << int(cv[6]) << ' ' << int(cv[7]) << endl;
	    cv = (unsigned char*)&v;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << ' '
		 << int(cv[4]) << ' ' << int(cv[5]) << ' '
		 << int(cv[6]) << ' ' << int(cv[7]) << endl;
	    cv = (unsigned char*)&result;
	    cout << dec;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << ' '
		 << int(cv[4]) << ' ' << int(cv[5]) << ' '
		 << int(cv[6]) << ' ' << int(cv[7]) << endl;
	    cv = (unsigned char*)&v;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << ' '
		 << int(cv[4]) << ' ' << int(cv[5]) << ' '
		 << int(cv[6]) << ' ' << int(cv[7]) << endl;
	    error = 1;
	}
	LECanonicalConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]
	||  out[5] != val[5]  ||  out[6] != val[6]
	||  out[7] != val[7]  ||  out[8] != val[8]) {
	    cout << "invalid double from conversion" << endl;
	    error = 1;
	}
    }
}


int main()
{
    int error = 0;
    checkFlags (error);
    if (!error) {
	checkConversion (error);
    }
    // Exit when errors found.
    if (error) {
	return 1;
    }
    cout << "OK" << endl;
    return 0;
}
