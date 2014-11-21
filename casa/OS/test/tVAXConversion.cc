//# tVAXConversion.h: Test program for class VAXConversion
//# Copyright (C) 1996,1997,2001,2002
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


#include <casacore/casa/OS/VAXConversion.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This program test the VAX conversion functions.


void checkConversion (int& error)
{
    char val[5];
    char out[9];
    {
	val[1] = -2;
	char result;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != -2) {
	    cout << "invalid char to conversion " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]) {
	    cout << "invalid char from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[1] = -2;
	unsigned char result;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 254) {
	    cout << "invalid unsigned char to conversion " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]) {
	    cout << "invalid unsigned char from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[2] = 1;
	val[1] = 2;
	short result;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 258) {
	    cout << "invalid short to conversion 1 " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid short from conversion 1" << endl;
	    error = 1;
	}
	val[2] = -2;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != -(1*256 + 254)) {
	    cout << "invalid short to conversion 2 " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid short from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[2] = 7;
	val[1] = 111;
	unsigned short result;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 7*256 + 111) {
	    cout << "invalid unsigned short to conversion " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
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
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 1*256*256*256 + 2*256*256 + 3*256 + 0) {
	    cout << "invalid int to conversion 1 " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid int from conversion 1" << endl;
	    error = 1;
	}
	val[4] = -127;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != -(126*256*256*256 + 253*256*256 + 252*256 + 256)) {
	    cout << "invalid int to conversion 2 " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
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
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 11*256*256*256 + 23*256*256 + 35*256 + 7) {
	    cout << "invalid unsigned int to conversion " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid unsigned int from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[4] = 2;
	val[3] = 54;
	val[2] = 78;
	val[1] = 145-256;
	Int64 result;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 2*256*256*256 + 54*256*256 + 78*256 + 145) {
	    cout << "invalid Int64 to conversion 1 " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid Int64 from conversion 1" << endl;
	    error = 1;
	}
	val[4] = -2;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != -(1*256*256*256 + 201*256*256 + 177*256 + 111)) {
	    cout << "invalid Int64 to conversion 2 " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid Int64 from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[4] = 128-256;
	val[3] = 54;
	val[2] = 78;
	val[1] = 100;
	uInt64 result;
	VAXConversion::toLocal (&result, val+1, 1);
	if (result != 128U*256U*256U*256U + 54U*256U*256U + 78U*256U + 100U) {
	    cout << "invalid uInt64 to conversion " << result << endl;
	    error = 1;
	}
	VAXConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid uInt64 from conversion" << endl;
	    error = 1;
	}
    }
}

void checkFloat (int& error)
{
    float f1;
    unsigned char out[5];
    unsigned char val[5];
    val[3] = 0;
    val[4] = 1;
    for (uInt i=0; i<2; i++) {
	for (uInt j=2; j<128; j++) {
	    val[2] = (i<<7) + j;
	    double v;
	    if (2*j>=128) {
		v = pow(double(2), double(2*j - 128));
	    }else{
		v = 1 / pow(double(2), double(128 - 2*j));
	    }
	    if (i==1) {
		v *= -1;
	    }
	    for (uInt k=0; k<128; k++) {
		val[1] = k;
		VAXConversion::toLocal (&f1, val+1, 1);
                float v1 = v * (double(128+k)/256 + double(1)/(256*256));
		if (f1 != v1) {
		    cout <<i<<" "<<j<<" "<<k<<" "<< f1 <<" " << v1
			 << " float to" << endl;
		    error = 1;
		}
		VAXConversion::fromLocal (out+1, &f1, 1);
		if (out[1] != val[1]  ||  out[2] != val[2]
		||  out[3] != val[3]  ||  out[4] != val[4]) {
		    cout <<i<<" "<<j<<" "<<k<<" "<< f1 <<" " << v1
			 << " float from" << endl;
		    error = 1;
		}
	    }
	}
    }
}

void checkDouble (int& error)
{
    double f1;
    unsigned char out[9];
    unsigned char val[9];
    val[3] = 2;
    val[4] = 1;
    val[5] = 4;
    val[6] = 3;
    val[7] = 0;
    val[8] = 5;
    for (uInt i=0; i<2; i++) {
	for (uInt j=1; j<128; j++) {
	    val[2] = (i<<7) + j;
	    double v;
	    if (2*j<128) {
		v = pow(double(2), double(128 - 2*j));
	    }else{
		v = pow(double(2), double(2*j - 128));
	    }
#if defined(__hpux__)
	    //# HP pow function is not exact, so clear last byte.
	    unsigned char* ucv = (unsigned char*)&v;
	    ucv[7] &= 0;
#endif
	    if (2*j<128) {
		v = 1/v;
	    }
	    if (i==1) {
		v *= -1;
	    }
	    for (uInt k=0; k<128; k++) {
		val[1] = k;
		VAXConversion::toLocal (&f1, val+1, 1);
                double v1 = v * (double(128+k)/256 + double(1)/(256*256)
		    + double(2)/(256*256*256)
		    + double(3)/(double(256*256)*256*256)
		    + double(4)/(double(256*256*256)*256*256)
		    + double(5)/(double(256*256*256)*256*256*256));
		if (f1 != v1) {
		    cout <<i<<" "<<j<<" "<<k<<" "<< f1 <<" " << v1
			 << " double to" << endl;
		    error = 1;
		}
		VAXConversion::fromLocal (out+1, &f1, 1);
		if (out[1] != val[1]  ||  out[2] != val[2]
		||  out[3] != val[3]  ||  out[4] != val[4]
		||  out[5] != val[5]  ||  out[6] != val[6]
		||  out[7] != val[7]  ||  out[8] != val[8]) {
		    cout <<i<<" "<<j<<" "<<k<<" "<< f1 <<" " << v1
			 << " double from" << endl;
		    error = 1;
		}
	    }
	}
    }
}

int main()
{
    int error = 0;
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
