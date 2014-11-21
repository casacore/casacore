//# tBitVector.cc: This program tests the BitVector class
//# Copyright (C) 1994,1995,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes
#include <casacore/casa/Utilities/BitVector.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void doIt()
{
    uInt maxbits=35;
    // Create bit vectors
    BitVector b;
    BitVector c(maxbits,True);
    BitVector d;
    uInt i;
    b.resize (maxbits);

    for (i=0; i<(maxbits/2); i++)  {
	b.setBit(i*2);
    }
    b[maxbits-2] = b[maxbits-3];
    cout << "b =    ";
    for (i=0; i<maxbits; i++) {
        if (b[i]) {                      // tests operator bool
            cout << "1";
        }else{
            cout << "0";
	}
    }
    cout << endl;

    for (i=0; i<(maxbits/2); i++) {
	c.clearBit(i*2);
    }
    c[maxbits-1] = False;
    cout << "c =   " << c;

    d = b^c; // Operator XOR with two bit vectors
    cout << "b^c = " << d;
    d.set (31, 2, False);
    cout << "d =   " << d;
    cout << "b|c = " << (b|c);
    d = b&c; // Operator AND with two bit vectors
    cout << "b&c = " << d;
    cout << "~b  = " << (~b);

    cout << "b==b = " << (b==b) << endl;
    cout << "b!=b = " << (b!=b) << endl;
    cout << "b==c = " << (b==c) << endl;
    cout << "b!=c = " << (b!=c) << endl;

    BitVector b1(b);
    cout << "b1 = " << b1;
    b1.resize (70);                          // resize longer with copy
    cout << "Size of b1 is "<< b1.nbits() << endl;
    cout << "b1 = " << b1;
    b1.resize (b.nbits());                   // resize shorter with copy
    cout << "b1 = " << b1;
    b1.resize (70, True);                    // resize longer with copy
    cout << "b1 = " << b1;
    b1 = b;
    cout << "b1 = " << b1;
    b1.resize (70, False, False);            // resize without copy
    cout << "b1 = " << b1;
    b1.resize (35, True, False);             // resize without copy
    cout << "b1 = " << b1;

    b1.resize (70, True, False);             // resize without copy
    b1.copy (30, 23, c, 8);
    cout << "b1 = " << b1;
    
    b1 = b;
    for (i=0; i<b.nbits(); i++) {
	b.toggleBit (i);
    }
    if (~b1 != b) {
	cout << "b and ~b1 should be equal" << endl;
    }

    b1.resize (128);
    b1 = False;
    b1.set (32, 64, True);
    for (i=0; i<32; i++) {
	if (b1[i] || !b1[i+32] || !b1[i+64] || b1[i=96]) {
	    cout << "error in b1.set(True,32,64)" << endl;
	}
    }

    BitVector bv1(4,False);
    bv1[0] = True;
    bv1[1] = True;
    BitVector bv2(4,False);
    bv2[0] = True;
    bv2[2] = True;
    BitVector bv3(bv2);
    bv2.reverse();
    if (bv2 != ~bv3  ||  bv2[0] || !bv2[1] || bv2[2] || !bv2[3]) {
	cout << "reverse is incorrect" << endl;
    }
    bv2 = bv3;
    bv2 &= bv1;
    if (bv2 != (bv1&bv3)  ||  !bv2[0] || bv2[1] || bv2[2] || bv2[3]) {
	cout << "=& is incorrect" << endl;
    }
    bv2 = bv3;
    bv2 |= bv1;
    if (bv2 != (bv1|bv3)  ||  !bv2[0] || !bv2[1] || !bv2[2] || bv2[3]) {
	cout << "=| is incorrect" << endl;
    }
    bv2 = bv3;
    bv2 ^= bv1;
    if (bv2 != (bv1^bv3)  ||  bv2[0] || !bv2[1] || !bv2[2] || bv2[3]) {
	cout << "=^ is incorrect" << endl;
    }
}


int main () {
    try {
	doIt();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
