//# tIPosition.cc: This program tests the IPosition class
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <assert.h>
#include <iostream.h>
#include <strstream.h>
#include <aips/Arrays/IPosition.h>
#include <aips/IO/AipsIO.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

main()
{
{
    IPosition ip(3, 0, 1, 2);
    
    assert(ip.nelements() == 3);
    assert(ip(0) == 0 && ip(1) == 1 && ip(2) == 2);
    ip(2) = 22;
    assert(ip(2) == 22);
    
    IPosition ip2;
    assert(ip2.nelements() == 0);

    ip2 = ip;
    assert(ip2 == ip);
    assert(ip2(0) == 0 && ip2(1) == 1 && ip2(2) == 22);

    ip2 += 2;
    ip2 = ip2 - 1;
    ip2 -= 1;
    assert(ip2(0) == 0 && ip2(1) == 1 && ip2(2) == 22);

    ip2 = 5;
    assert(ip2 == 5);
    assert(ip2 == ip2);
    assert(ip2 != ip);

    ip2.resize(10);
    ip2 = 10;
    ip.resize(0);
    ip = ip2;
    assert (ip == 10);
    assert(ip.nelements() == 10);
    ip = ip * ip2;
    assert(ip == 100);
    assert (ip > ip2);
    assert (ip >= ip2);
    assert (ip2 < ip);
    assert (ip2 <= ip);
    assert (ip2 != ip);

    IPosition ip3(5,0,1,2,3,4);

    AipsIO io("tIPosition_tmp.data", ByteIO::New);
    io << ip3;
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Old);
    IPosition ip4;
    io >> ip4;
    assert (ip4 == ip3);
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Delete);
    io.close();
}

{
    // Now test the IPosition class more exhaustively
    uInt i;

    IPosition ip1;                             // IPosition()
    assert(ip1.nelements() == 0);              // nelements()
    IPosition ip2(5);                          // IPosition(uInt);
    assert(ip2.nelements() == 5);
    IPosition ip3(5, 0, 1, 2, 3, 4);           // IPosition(uInt, Int, ...)
    assert(ip3.nelements() == 5);
    const IPosition &rip3 = ip3;
    for (i=0; i<5; i++) {
	assert(ip3(i) == i);                   // operator()(uInt)
	assert(rip3(i) == i);                  // operator()(uInt) const
    }
    IPosition ip4(ip3);                        // IPosition(const IPosition &)
    assert(ip4.nelements() == 5);
    for (i=0; i<5; i++) {
	assert(ip4(i) == i);
    }
    ip1 = ip4;                                 // operator=(const IPosition &);
    assert(ip1.nelements() == 5);
    for (i=0; i<5; i++) {
	assert(ip1(i) == i);
    }
    ip1 = ip1;
    for (i=0; i<5; i++) {
	assert(ip1(i) == i);
    }

    ip1 = 1;                                   // operator=(Int);
    assert(ip1.nelements() == 5);
    for (i=0; i<5; i++) {
	assert(ip1(i) == 1);
    }
    ip1 = ip4;
    assert(ip1.nelements() == 5);
    for (i=0; i<5; i++) {
	assert(ip1(i) == i);
    }

    ip1.resize(10);                            // resize(uInt)
    assert(ip1.nelements() == 10);

    assert(ip1.conform(ip4) == False);         // conform
    assert(ip4.conform(ip3) == True);

    IPosition ipf (2,3,4);
    IPosition ipl (3,14,15,16);
    IPosition ipr (5,3,4,14,15,16);
    IPosition ipp0 (ipf);
    ipp0.append (ipl);
    assert (ipp0 == ipr);
    IPosition ipp1 (ipl);
    ipp1.prepend (ipf);
    assert (ipp1 == ipr);
    assert (ipf.concatenate (ipl) == ipr);
    IPosition ipgf = ipr.getFirst (2);
    assert (ipgf == ipf);
    IPosition ipgl = ipr.getLast (3);
    assert (ipgl == ipl);
    ipp0.setFirst (ipl);
    assert (ipp0 == IPosition(5,14,15,16,15,16));
    ipp1.setLast (ipf);
    assert (ipp1 == IPosition(5,3,4,14,3,4));


                                               // Member fn arithmetic
    ip1 = 1;
    ip1 += 5;
    assert(ip1 == 6);
    ip1 -= 1;
    assert(ip1 == 5);
    ip1 *= 2;
    assert(ip1 == 10);
    ip1 /= 2;
    assert(ip1 == 5);
    ip2.resize(10);
    ip2 = 1;
    ip1 += ip2;
    assert(ip1 == 6);
    ip1 -= ip2;
    assert(ip1 == 5);
    ip2 = 2;
    ip1 *= ip2;
    assert(ip1 == 10);
    ip1 /= ip2;
    assert(ip1 == 5);
                                               // Global fn arithmetic
    ip1 = ip1 + ip2;
    assert(ip1 == 7);
    ip1 = ip1 - ip2;
    assert(ip1 == 5);
    ip1 = ip2 * ip1;
    assert(ip1 == 10);
    ip1 = ip1 / ip2;
    assert(ip1 == 5);
    ip1 = ip1 + 1;
    assert(ip1 == 6);
    ip1 = ip1 - 1;
    assert(ip1 == 5);
    ip1 = ip1 * 2;
    assert(ip1 == 10);
    ip1 = ip1 / 2;
    assert(ip1 == 5);
    ip1 = 1 + ip1;
    assert(ip1 == 6);
    ip1 = 1 - ip1;
    assert(ip1 == -5);
    ip1 = -2 * ip1;
    assert(ip1 == 10);
    ip1 = 50 / ip1;
    assert(ip1 == 5);
                                               // Global fn logicals
    ip2.resize(ip1.nelements());
    ip2 = 6;
    assert(ip1 == ip1);
    assert((ip1 == ip2) == False);
    assert (ip1.isEqual(ip1));
    assert (ip1.isEqual(ip2) == False);
    assert(ip1 != ip2);
    assert((ip1 != ip1) == False);
    assert(ip1 < ip2);
    assert((ip1 < ip1) == False);
    assert(ip1 <= ip2);
    assert((ip1 <= ip1));
    ip2(0) = 0;
    assert((ip1 <= ip2) == False);
    assert(ip1 + 1 > ip1);
    assert((ip1  > ip1) == False);
    assert((ip2 > ip1) == False);
    assert(ip1 + 1 >= ip1);
    assert(ip1 >= ip1);
    assert((ip2 >= ip1) == False);
    assert(ip1 == 5);
    assert((ip1 == 6) == False);
    assert(5 == ip1);
    assert((6 == ip1) == False);
    assert(ip1 != 6);
    assert((ip1 != 5) == False);
    assert(6 != ip1);
    assert((5 != ip1) == False);
    assert(ip1 < 6);
    assert((ip1 < 5) == False);
    assert(4 < ip1);
    assert((5 < ip1) == False);
    assert(ip1 <= 6);
    assert(ip1 <= 5);
    assert((ip1 <= 4) == False);
    assert(4 <= ip1);
    assert(5 <= ip1);
    assert((6 <= ip1) == False);
    assert(ip1 > 4);
    assert((ip1 > 5) == False);
    assert(ip1 >= 4);
    assert((ip1 >= 5));
    assert((ip1 >= 6) == False);
    assert(6 > ip1);
    assert(( 5 > ip1) == False);
    assert(5 >= ip1);
    assert((4 >= ip1) == False);

    IPosition ip7 (ip1.nelements());
    IPosition ip8 (ip1.nelements() + 1);
    ip7 = 0;
    ip8 = 0;
    for (Int ipindex=0; ipindex < ip1.nelements(); ipindex++) {
        ip7(ipindex) = ip1(ipindex);
        ip8(ipindex) = ip1(ipindex);
    }
    assert (ip7.isEqual(ip1));
    assert (ip7.isEqual(ip8) == False);

    assert (! IPosition(1,1).isEqual (IPosition(), False));
    assert (IPosition(1,1).isEqual (IPosition(), True));
    assert (IPosition(1,1).isEqual (IPosition(1,1), True));
    assert (IPosition(1,1).isEqual (IPosition(2,1,1), True));
    assert (IPosition(2,1,1).isEqual (IPosition(2,1,1), True));
    assert (IPosition(2,1,1).isEqual (IPosition(1,1), True));
    assert (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(3,2,3,4), True));
    assert (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(5,1,1,2,3,4), True));
    assert (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(5,2,3,4,1,1), True));
    assert (IPosition(3,2,3,4).isEqual (IPosition(5,2,1,3,1,4), True));

    assert (IPosition(3,2,3,4).nonDegenerate().isEqual (IPosition(3,2,3,4)));
    assert (IPosition(4,1,2,3,4).nonDegenerate().isEqual (IPosition(3,2,3,4)));
    assert (IPosition(5,1,1,2,3,4).nonDegenerate().isEqual
	                                             (IPosition(3,2,3,4)));
    assert (IPosition(6,1,1,2,3,1,4).nonDegenerate().isEqual
	                                             (IPosition(3,2,3,4)));
    assert (IPosition(6,1,1,2,3,1,4).nonDegenerate(2).isEqual
	                                             (IPosition(5,1,1,2,3,4)));

    AipsIO io("tIPosition_tmp.data", ByteIO::New);   // AipsIO << and >>
    IPosition iptmp1;
    io << ip1 << ip2 << iptmp1;
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Old);
    IPosition iptmp2;
    io >> iptmp2;
    assert (ip1 == iptmp2);
    io >> iptmp2;
    assert (ip2 == iptmp2);
    io >> iptmp2;
    assert (iptmp1 == iptmp2);
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Delete);
    io.close();

    // Use an ostrstream to convert to ASCII and reading it back.
    // Note that function str "freezes" the buffer in ostrstream,
    // which means we have to delete it ourselves.
    // Also note that no terminating 0 is stored by operator<<.
    ostrstream os;                            // ostream &operator<<
    os << ip1;
    char* osbuf = os.str();
    String string (osbuf, os.pcount());
    assert(string == "[5, 5, 5, 5, 5, 5, 5, 5, 5, 5]");
    delete [] osbuf;

{
                                               // Check out exceptions
    Bool caught = False;

    IPosition ip1(2);
    IPosition ip2(3);  // ip1.conform(ip2) == False
    ip1 = 5; ip2 = 6;
    caught = False;
    try {ip1+=ip2;} catch (AipsError x) {caught = True;}  assert(caught);
    caught = False;
    try {ip1-=ip2;} catch (AipsError x) {caught = True;}  assert(caught);
    caught = False;
    try {ip1*=ip2;} catch (AipsError x) {caught = True;}  assert(caught);
    caught = False;
    try {ip1/=ip2;} catch (AipsError x) {caught = True;}  assert(caught);
    caught = False;
    try {(void)(ip1+ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1-ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1*ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1/ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1==ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1!=ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1<ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1<=ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1>ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1>=ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
    caught = False;
    try {(void)(ip1=ip2);} catch (AipsError x) {caught = True;} 
    assert(caught);
}    
                                               // ~IPosition tested implicitly
                                               // at end of block
{
     IPosition x;
     IPosition y(5,1,2,3,4,5);
     assert(x.product() == 0 && y.product() == 120);
}
{
    Vector<Int> vi;
    IPosition ip(3, 1, 2, 3);
    IPosition ip2(6, 1, 2, 3, 4, 5, 6);
    vi = ip.asVector();
    AlwaysAssertExit(vi(0) == 1 && vi(1) == 2 && vi(2) == 3);
    vi.resize(6);
    vi = ip2.asVector();
    AlwaysAssertExit(vi(0) == 1 && vi(1) == 2 && vi(2) == 3 && vi(3) == 4 &&
		     vi(4) == 5 && vi(5) == 6);
    IPosition ip3(vi);
    AlwaysAssertExit(ip3(0) == 1 && ip3(1) == 2 && ip3(2) == 3 && ip3(3) == 4 &&
		     ip3(4) == 5 && ip3(5) == 6);
}

}

    cout << "OK\n";
    return 0;
}


