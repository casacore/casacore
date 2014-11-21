//# tConversion.cc: test program for class Conversion
//# Copyright (C) 1996,1997,2001
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


#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This program tests the class Conversion.


// Check optimized conversions.
void checkAll()
{
  cout << "checkAll ..." << endl;
  uChar bits[256];
  for (uInt i=0; i<256; ++i) {
    bits[i]= i;
  }
  Bool flagArr[8*260];
  // Make sure to use an aligned array.
  Bool* flags = flagArr;
  cout << "unaligned flag pointer " << flags << endl;
  flags = (Bool*)(8 * (((unsigned long long)flags-1)/8 + 1));
  cout << "  aligned flag pointer " << flags << endl;
  Conversion::bitToBool (flags, bits, 8*256);
  for (uInt i=0; i<256; ++i) {
    uInt val = 0;
    uInt tmp = 1;
    for (uInt j=0; j<8;++j) {
      if (flags[i*8 + j]) {
        val += tmp;
      }
      tmp *= 2;
    }
    AlwaysAssertExit (val == bits[i]);
  }
  uChar out[256];
  Conversion::boolToBit (out, flags, 8*256);
  for (uInt i=0; i<256; ++i) {
    AlwaysAssertExit (out[i] == bits[i]);
  }
}

int main()
{
    uInt nbool = 100;
    uInt nbyte = (100 + 7) / 8;
    Bool* data = new Bool[nbool];
    uChar* bits = new uChar[nbyte];
    uInt i;

    //# Initialize all bits and check if resulting Bools are all False.
    for (i=0; i<nbyte; i++) {
	bits[i] = 0;
    }
    AlwaysAssertExit (Conversion::bitToBool (data, bits, nbool) == nbyte);
    for (i=0; i<nbool; i++) {
	AlwaysAssertExit (data[i] == False);
    }

    //# Set some bools and check the flags.
    data[0] = True;
    data[10] = True;
    bits[0] = 2;
    bits[5] = 1;
    Conversion::boolToBit (bits, data, nbool);
    AlwaysAssertExit (bits[0] == 1);
    AlwaysAssertExit (bits[1] == 4);
    for (i=2; i<nbyte; i++) {
	AlwaysAssertExit (bits[i] == 0);
    }

    //# Check the partial conversion functions.
    data[2] = True;
    Conversion::boolToBit (bits, data+2, 2, 2);
    AlwaysAssertExit (bits[0] == 1+4);
    AlwaysAssertExit (bits[1] == 4);
    for (i=2; i<nbyte; i++) {
	AlwaysAssertExit (bits[i] == 0);
    }

    //# Check the partial conversion functions.
    data[2] = False;
    data[9] = True;
    data[20] = True;
    Conversion::boolToBit (bits, data+2, 2, 19);
    AlwaysAssertExit (bits[0] == 1);
    AlwaysAssertExit (bits[1] == 2+4);
    AlwaysAssertExit (bits[2] == 16);
    for (i=3; i<nbyte; i++) {
	AlwaysAssertExit (bits[i] == 0);
    }
    Conversion::bitToBool (data, bits, 1, 20);
    for (i=0; i<100; i++) {
	if (i==8 || i==9 || i==19 || i==20) {
	    AlwaysAssertExit (data[i] == True);
	}else{
	    AlwaysAssertExit (data[i] == False);
	}
    }

    data[0] = True;
    data[7] = True;
    data[8] = True;
    Conversion::boolToBit (bits, data, 0, 8);
    AlwaysAssertExit (bits[0] == 1+128);
    AlwaysAssertExit (bits[1] == 2+4);
    AlwaysAssertExit (bits[2] == 16);
    for (i=3; i<nbyte; i++) {
	AlwaysAssertExit (bits[i] == 0);
    }
    data[0] = False;
    data[7] = False;
    data[8] = False;
    Conversion::bitToBool (data, bits, 0, 64);
    for (i=0; i<100; i++) {
	if (i==0 || i==7 || i==9 || i==10 || i==20) {
	    AlwaysAssertExit (data[i] == True);
	}else{
	    AlwaysAssertExit (data[i] == False);
	}
    }
    
	 
    delete [] data;
    delete [] bits;

    checkAll();
    cout << "OK" << endl;
    return 0;
}
