//# tIBMConversion.h: Test program for class IBMConversion
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


#include <casacore/casa/OS/IBMConversion.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void showFloat (float f)
{
    uChar* c = (uChar*)(&f);
    int i0 = c[0];
    int i1 = c[1];
    int i2 = c[2];
    int i3 = c[3];
    cout <<f<<"= "<< i0 << " " << i1 << " " << i2 << " " << i3 << endl;
}

void checkConversion (int& error)
{
    char val[5];
    char out[5];
    {
	val[1] = 129-256;
	val[2] = 64;
	val[3] = 132-256;
	val[4] = 162-256;
	char result[4];
	IBMConversion::toLocal (result, val+1, 4);
	if (result[0] != 'a'  ||  result[1] != ' '
	||  result[2] != 'd'  ||  result[3] != 's') {
	    cout << "invalid char to conversion " << result[0]
		 << result[1] << result[2] << result[3] << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, result, 4);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid char from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[1] = -2;
	unsigned char result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 254) {
	    cout << "invalid unsigned char to conversion " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]) {
	    cout << "invalid unsigned char from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[1] = 1;
	val[2] = 2;
	short result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 258) {
	    cout << "invalid short to conversion 1 " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid short from conversion 1" << endl;
	    error = 1;
	}
	val[1] = -2;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != -(1*256 + 254)) {
	    cout << "invalid short to conversion 2 " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid short from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[1] = 7;
	val[2] = 111;
	unsigned short result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 7*256 + 111) {
	    cout << "invalid unsigned short to conversion " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]) {
	    cout << "invalid unsigned short from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[1] = 1;
	val[2] = 2;
	val[3] = 3;
	val[4] = 0;
	int result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 1*256*256*256 + 2*256*256 + 3*256 + 0) {
	    cout << "invalid int to conversion 1 " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid int from conversion 1" << endl;
	    error = 1;
	}
	val[1] = -127;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != -(126*256*256*256 + 253*256*256 + 252*256 + 256)) {
	    cout << "invalid int to conversion 2 " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid int from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[1] = 11;
	val[2] = 23;
	val[3] = 35;
	val[4] = 7;
	unsigned int result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 11*256*256*256 + 23*256*256 + 35*256 + 7) {
	    cout << "invalid unsigned int to conversion " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid unsigned int from conversion" << endl;
	    error = 1;
	}
    }
    {
	val[1] = 2;
	val[2] = 54;
	val[3] = 78;
	val[4] = 145-256;
	Int64 result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 2*256*256*256 + 54*256*256 + 78*256 + 145) {
	    cout << "invalid Int64 to conversion 1 " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid Int64 from conversion" << endl;
	    error = 1;
	}
	val[1] = -2;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != -(1*256*256*256 + 201*256*256 + 177*256 + 111)) {
	    cout << "invalid Int64 to conversion 2 " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
	if (out[1] != val[1]  ||  out[2] != val[2]
	||  out[3] != val[3]  ||  out[4] != val[4]) {
	    cout << "invalid Int64 from conversion 2" << endl;
	    error = 1;
	}
    }
    {
	val[1] = 128-256;
	val[2] = 54;
	val[3] = 78;
	val[4] = 100;
	uInt64 result;
	IBMConversion::toLocal (&result, val+1, 1);
	if (result != 128U*256U*256U*256U + 54U*256U*256U + 78U*256U + 100U) {
	    cout << "invalid uInt64 to conversion " << result << endl;
	    error = 1;
	}
	IBMConversion::fromLocal (out+1, &result, 1);
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
    unsigned char val[5];
    unsigned char out[5];
    val[3] = 0;
    val[4] = 0;
    for (uInt i=0; i<2; i++) {
	for (uInt j=35; j<97; j++) {
	    val[1] = (i<<7) + j;
	    double v;
	    if (j>=64) {
		v = pow(double(16), double(j-64));
	    }else{
		v = 1 / pow(double(16), double(64-j));
	    }
	    if (i==1) {
		v *= -1;
	    }
	    for (uInt k=1; k<256; k++) {
		val[2] = k;
		IBMConversion::toLocal (&f1, val+1, 1);
                float v1 = v * double(k)/256;
		if (f1 != v1) {
		    cout <<i<<" "<<j<<" "<<k<<" "<< f1 <<" " << v1
			 << " float to" << endl;
		    error = 1;
		}
		// When first 4 bits of mantissa are zero, the
		// result is slightly different; so do not test them.
		if (k > 15)  {
		    IBMConversion::fromLocal (out+1, &f1, 1);
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
}

void checkDouble (int& error)
{
    double f1;
    unsigned char val[9];
    unsigned char out[9];
    val[3] = 0;
    val[4] = 0;
    val[5] = 0;
    val[6] = 0;
    val[7] = 0;
    val[8] = 0;
    for (uInt i=0; i<2; i++) {
	for (uInt j=0; j<128; j++) {
	    val[1] = (i<<7) + j;
	    double v;
	    if (j<64) {
		v = pow(double(16), double(64-j));
	    }else{
		v = pow(double(16), double(j-64));
	    }
#if defined(__hpux__)
	    //# HP pow function is not exact, so clear last byte.
	    unsigned char* ucv = (unsigned char*)&v;
	    ucv[7] &= 0;
#endif
	    if (j<64) {
		v = 1/v;
	    }
	    if (i==1) {
		v *= -1;
	    }
	    for (uInt k=1; k<256; k++) {
		val[2] = k;
                double v1 = double(k)/256;
		for (uInt l=0; l<32; l++) {
		    val[5] = l;
		    IBMConversion::toLocal (&f1, val+1, 1);
		    double v2 = v * (v1 + double(l)/(double(256*256)*256*256));
		    if (f1 != v2) {
			cout <<i<<" "<<j<<" "<<k<<" "<<l<<" "
			     << f1 <<" " << v2 << " double to" << endl;
	    unsigned char* cv = (unsigned char*)&f1;
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
	    cv = (unsigned char*)&f1;
	    cout << dec;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << ' '
		 << int(cv[4]) << ' ' << int(cv[5]) << ' '
		 << int(cv[6]) << ' ' << int(cv[7]) << endl;
	    cv = (unsigned char*)&v2;
	    cout << int(cv[0]) << ' ' << int(cv[1]) << ' '
		 << int(cv[2]) << ' ' << int(cv[3]) << ' '
		 << int(cv[4]) << ' ' << int(cv[5]) << ' '
		 << int(cv[6]) << ' ' << int(cv[7]) << endl;
			error = 1;
			return;
		    }
		    // When first 4 bits of mantissa are zero, the
		    // result is slightly different; so do not test them.
		    // A zero exponent always results in a zero value.
		    if (k > 15) {
			IBMConversion::fromLocal (out+1, &f1, 1);
			if (j == 0) {
			    if (out[1] != 0  ||  out[2] != 0
			    ||  out[3] != 0  ||  out[4] != 0
			    ||  out[5] != 0  ||  out[6] != 0
			    ||  out[7] != 0  ||  out[8] != 0) {
				cout <<i<<" "<<j<<" "<<k<<" "<<l<<" "
				    << f1 <<" " << v2 << " double from" <<endl;
				error = 1;
			    }
			}else{
			    if (out[1] != val[1]  ||  out[2] != val[2]
			    ||  out[3] != val[3]  ||  out[4] != val[4]
			    ||  out[5] != val[5]  ||  out[6] != val[6]
			    ||  out[7] != val[7]  ||  out[8] != val[8]) {
				cout <<i<<" "<<j<<" "<<k<<" "<<l<<" "
				    << f1 <<" " << v2 << " double from" <<endl;
				error = 1;
			    }
			}
		    }
		}
	    }
	}
    }
}

int main()
{
///    showFloat (float(0));
///    showFloat (float(1));
///    showFloat (float(2));
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
