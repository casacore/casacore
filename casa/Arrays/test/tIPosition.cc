//# tIPosition.cc: This program tests the IPosition class
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001,2003
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

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <vector>

#include <casacore/casa/namespace.h>

void testKeepRemove()
{
  IPosition p1(4,2,3,4,5);
  IPosition p2 (p1.removeAxes(IPosition(2,0,3)));
  AlwaysAssertExit (p2.size()==2 && p2[0]==3 && p2[1]==4);
  IPosition p3 (p1.keepAxes(IPosition(2,2,1)));
  AlwaysAssertExit (p2.size()==2 && p2[0]==3 && p2[1]==4);
}


int main()
{
  {
    IPosition ip(3, 0, 1, 2);
    
    Int nrit = 0;
    for (IPosition::const_iterator iter=ip.begin(); iter!=ip.end(); iter++) {
      AlwaysAssertExit(*iter == ip[nrit++]);
    }
    AlwaysAssertExit(nrit == 3);
    AlwaysAssertExit(ip.nelements() == 3);
    AlwaysAssertExit(ip.size() == 3);
    AlwaysAssertExit(!ip.empty());
    AlwaysAssertExit(ip(0) == 0 && ip(1) == 1 && ip(2) == 2);
    AlwaysAssertExit(ip[0] == 0 && ip[1] == 1 && ip[2] == 2);
    AlwaysAssertExit (ip.last() == 2);
    AlwaysAssertExit (ip.last(1) == 1);
    AlwaysAssertExit (ip.last(2) == 0);

    std::vector<Int> vec(ip.begin(), ip.end());
    AlwaysAssertExit(vec.size() == 3);
    AlwaysAssertExit(vec[0] == 0 && vec[1] == 1 && vec[2] == 2);

    ip[2] = 21;
    AlwaysAssertExit(ip(2) == 21);
    ip(2) = 23;
    AlwaysAssertExit(ip(2) == 23);
    AlwaysAssertExit (ip.last() == 23);
    ip.last() = 22;
    AlwaysAssertExit(ip(2) == 22);

    IPosition ip2;
    AlwaysAssertExit(ip2.nelements() == 0);
    AlwaysAssertExit(ip2.size() == 0);
    AlwaysAssertExit(ip2.empty());

    ip2 = ip;
    AlwaysAssertExit(ip2 == ip);
    AlwaysAssertExit(ip2(0) == 0 && ip2(1) == 1 && ip2(2) == 22);

    ip2 += 2;
    ip2 = ip2 - 1;
    ip2 -= 1;
    AlwaysAssertExit(ip2(0) == 0 && ip2(1) == 1 && ip2(2) == 22);

    ip2 = 5;
    AlwaysAssertExit(ip2 == 5);
    AlwaysAssertExit(ip2 == ip2);
    AlwaysAssertExit(ip2 != ip);

    ip2.resize(10);
    ip2 = 10;
    ip.resize(0);
    ip = ip2;
    AlwaysAssertExit (ip == 10);
    AlwaysAssertExit(ip.nelements() == 10);
    ip = ip * ip2;
    AlwaysAssertExit(ip == 100);
    AlwaysAssertExit (ip > ip2);
    AlwaysAssertExit (ip >= ip2);
    AlwaysAssertExit (ip2 < ip);
    AlwaysAssertExit (ip2 <= ip);
    AlwaysAssertExit (ip2 != ip);

    IPosition ip3(5,0,1,2,3,4);

    nrit = 0;
    for (IPosition::const_iterator iter=ip3.begin(); iter!=ip3.end(); iter++) {
      AlwaysAssertExit(*iter == ip3[nrit++]);
    }
    
    AipsIO io("tIPosition_tmp.data", ByteIO::New);
    io << ip3;
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Old);
    IPosition ip4;
    io >> ip4;
    AlwaysAssertExit (ip4 == ip3);
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Delete);
    io.close();
  }

  {
    // Now test the IPosition class more exhaustively
    Int i;

    IPosition ip1;                             // IPosition()
    AlwaysAssertExit(ip1.nelements() == 0);              // nelements()
    IPosition ip2(5);                          // IPosition(uInt);
    AlwaysAssertExit(ip2.nelements() == 5);
    IPosition ip3(5, 0, 1, 2, 3, 4);           // IPosition(uInt, Int, ...)
    AlwaysAssertExit(ip3.nelements() == 5);
    const IPosition &rip3 = ip3;
    for (i=0; i<5; i++) {
	AlwaysAssertExit(ip3(i) == i);         // operator()(uInt)
	AlwaysAssertExit(rip3(i) == i);        // operator()(uInt) const
    }
    IPosition ip4(ip3);                        // IPosition(const IPosition &)
    AlwaysAssertExit(ip4.nelements() == 5);
    for (i=0; i<5; i++) {
	AlwaysAssertExit(ip4(i) == i);
    }
    ip1 = ip4;                                 // operator=(const IPosition &);
    AlwaysAssertExit(ip1.nelements() == 5);
    for (i=0; i<5; i++) {
	AlwaysAssertExit(ip1(i) == i);
    }
    ip1 = ip1;
    for (i=0; i<5; i++) {
	AlwaysAssertExit(ip1(i) == i);
    }

    ip1 = 1;                                   // operator=(Int);
    AlwaysAssertExit(ip1.nelements() == 5);
    for (i=0; i<5; i++) {
	AlwaysAssertExit(ip1(i) == 1);
    }
    ip1 = ip4;
    AlwaysAssertExit(ip1.nelements() == 5);
    for (i=0; i<5; i++) {
	AlwaysAssertExit(ip1(i) == i);
    }

    ip1.resize(10);                            // resize(uInt)
    AlwaysAssertExit(ip1.nelements() == 10);

    AlwaysAssertExit(ip1.conform(ip4) == False); // conform
    AlwaysAssertExit(ip4.conform(ip3) == True);

    IPosition ipf (2,3,4);
    IPosition ipl (3,14,15,16);
    IPosition ipr (5,3,4,14,15,16);
    IPosition ipp0 (ipf);
    ipp0.append (ipl);
    AlwaysAssertExit (ipp0 == ipr);
    IPosition ipp1 (ipl);
    ipp1.prepend (ipf);
    AlwaysAssertExit (ipp1 == ipr);
    AlwaysAssertExit (ipf.concatenate (ipl) == ipr);
    IPosition ipgf = ipr.getFirst (2);
    AlwaysAssertExit (ipgf == ipf);
    IPosition ipgl = ipr.getLast (3);
    AlwaysAssertExit (ipgl == ipl);
    ipp0.setFirst (ipl);
    AlwaysAssertExit (ipp0 == IPosition(5,14,15,16,15,16));
    ipp1.setLast (ipf);
    AlwaysAssertExit (ipp1 == IPosition(5,3,4,14,3,4));


                                               // Member fn arithmetic
    ip1 = 1;
    ip1 += 5;
    AlwaysAssertExit(ip1 == 6);
    ip1 -= 1;
    AlwaysAssertExit(ip1 == 5);
    ip1 *= 2;
    AlwaysAssertExit(ip1 == 10);
    ip1 /= 2;
    AlwaysAssertExit(ip1 == 5);
    ip2.resize(10);
    ip2 = 1;
    ip1 += ip2;
    AlwaysAssertExit(ip1 == 6);
    ip1 -= ip2;
    AlwaysAssertExit(ip1 == 5);
    ip2 = 2;
    ip1 *= ip2;
    AlwaysAssertExit(ip1 == 10);
    ip1 /= ip2;
    AlwaysAssertExit(ip1 == 5);
                                               // Global fn arithmetic
    ip1 = ip1 + ip2;
    AlwaysAssertExit(ip1 == 7);
    ip1 = ip1 - ip2;
    AlwaysAssertExit(ip1 == 5);
    ip1 = ip2 * ip1;
    AlwaysAssertExit(ip1 == 10);
    ip1 = ip1 / ip2;
    AlwaysAssertExit(ip1 == 5);
    ip1 = ip1 + 1;
    AlwaysAssertExit(ip1 == 6);
    ip1 = ip1 - 1;
    AlwaysAssertExit(ip1 == 5);
    ip1 = ip1 * 2;
    AlwaysAssertExit(ip1 == 10);
    ip1 = ip1 / 2;
    AlwaysAssertExit(ip1 == 5);
    ip1 = 1 + ip1;
    AlwaysAssertExit(ip1 == 6);
    ip1 = 1 - ip1;
    AlwaysAssertExit(ip1 == -5);
    ip1 = -2 * ip1;
    AlwaysAssertExit(ip1 == 10);
    ip1 = 50 / ip1;
    AlwaysAssertExit(ip1 == 5);
                                               // Global fn logicals
    ip2.resize(ip1.nelements());
    ip2 = 6;
    AlwaysAssertExit(ip1 == ip1);
    AlwaysAssertExit((ip1 == ip2) == False);
    AlwaysAssertExit (ip1.isEqual(ip1));
    AlwaysAssertExit (ip1.isEqual(ip2) == False);
    AlwaysAssertExit(ip1 != ip2);
    AlwaysAssertExit((ip1 != ip1) == False);
    AlwaysAssertExit(ip1 < ip2);
    AlwaysAssertExit((ip1 < ip1) == False);
    AlwaysAssertExit(ip1 <= ip2);
    AlwaysAssertExit((ip1 <= ip1));
    ip2(0) = 0;
    AlwaysAssertExit((ip1 <= ip2) == False);
    AlwaysAssertExit(ip1 + 1 > ip1);
    AlwaysAssertExit((ip1  > ip1) == False);
    AlwaysAssertExit((ip2 > ip1) == False);
    AlwaysAssertExit(ip1 + 1 >= ip1);
    AlwaysAssertExit(ip1 >= ip1);
    AlwaysAssertExit((ip2 >= ip1) == False);
    AlwaysAssertExit(ip1 == 5);
    AlwaysAssertExit((ip1 == 6) == False);
    AlwaysAssertExit(5 == ip1);
    AlwaysAssertExit((6 == ip1) == False);
    AlwaysAssertExit(ip1 != 6);
    AlwaysAssertExit((ip1 != 5) == False);
    AlwaysAssertExit(6 != ip1);
    AlwaysAssertExit((5 != ip1) == False);
    AlwaysAssertExit(ip1 < 6);
    AlwaysAssertExit((ip1 < 5) == False);
    AlwaysAssertExit(4 < ip1);
    AlwaysAssertExit((5 < ip1) == False);
    AlwaysAssertExit(ip1 <= 6);
    AlwaysAssertExit(ip1 <= 5);
    AlwaysAssertExit((ip1 <= 4) == False);
    AlwaysAssertExit(4 <= ip1);
    AlwaysAssertExit(5 <= ip1);
    AlwaysAssertExit((6 <= ip1) == False);
    AlwaysAssertExit(ip1 > 4);
    AlwaysAssertExit((ip1 > 5) == False);
    AlwaysAssertExit(ip1 >= 4);
    AlwaysAssertExit((ip1 >= 5));
    AlwaysAssertExit((ip1 >= 6) == False);
    AlwaysAssertExit(6 > ip1);
    AlwaysAssertExit(( 5 > ip1) == False);
    AlwaysAssertExit(5 >= ip1);
    AlwaysAssertExit((4 >= ip1) == False);

    IPosition ip7 (ip1.nelements());
    IPosition ip8 (ip1.nelements() + 1);
    ip7 = 0;
    ip8 = 0;
    for (uInt ipindex=0; ipindex < ip1.nelements(); ipindex++) {
        ip7(ipindex) = ip1(ipindex);
        ip8(ipindex) = ip1(ipindex);
    }
    AlwaysAssertExit (ip7.isEqual(ip1));
    AlwaysAssertExit (ip7.isEqual(ip8) == False);

    AlwaysAssertExit (! IPosition(1,1).isEqual (IPosition(), False));
    AlwaysAssertExit (IPosition(1,1).isEqual (IPosition(), True));
    AlwaysAssertExit (IPosition(1,1).isEqual (IPosition(1,1), True));
    AlwaysAssertExit (IPosition(1,1).isEqual (IPosition(2,1,1), True));
    AlwaysAssertExit (IPosition(2,1,1).isEqual (IPosition(2,1,1), True));
    AlwaysAssertExit (IPosition(2,1,1).isEqual (IPosition(1,1), True));
    AlwaysAssertExit (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(3,2,3,4),
                                                        True));
    AlwaysAssertExit (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(5,1,1,2,3,4),
                                                        True));
    AlwaysAssertExit (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(5,2,3,4,1,1),
                                                        True));
    AlwaysAssertExit (IPosition(3,2,3,4).isEqual (IPosition(5,2,1,3,1,4),
                                                  True));

    AlwaysAssertExit (IPosition(3,2,3,4).nonDegenerate().isEqual
                                                     (IPosition(3,2,3,4)));
    AlwaysAssertExit (IPosition(4,1,2,3,4).nonDegenerate().isEqual
                                                     (IPosition(3,2,3,4)));
    AlwaysAssertExit (IPosition(5,1,1,2,3,4).nonDegenerate().isEqual
	                                             (IPosition(3,2,3,4)));
    AlwaysAssertExit (IPosition(6,1,1,2,3,1,4).nonDegenerate().isEqual
	                                             (IPosition(3,2,3,4)));
    AlwaysAssertExit (IPosition(6,1,1,2,3,1,4).nonDegenerate(2).isEqual
	                                             (IPosition(5,1,1,2,3,4)));

    AipsIO io("tIPosition_tmp.data", ByteIO::New);   // AipsIO << and >>
    IPosition iptmp1;
    io << ip1 << ip2 << iptmp1;
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Old);
    IPosition iptmp2;
    io >> iptmp2;
    AlwaysAssertExit (ip1 == iptmp2);
    io >> iptmp2;
    AlwaysAssertExit (ip2 == iptmp2);
    io >> iptmp2;
    AlwaysAssertExit (iptmp1 == iptmp2);
    io.close();
    io.open("tIPosition_tmp.data", ByteIO::Delete);
    io.close();

    // Use an ostringstream to convert to ASCII and reading it back.
    // Note that function str "freezes" the buffer in ostringstream,
    // which means we have to delete it ourselves.
    // Also note that no terminating 0 is stored by operator<<.
    ostringstream os;                            // ostream &operator<<
    os << ip1;
    String string (os.str());
    AlwaysAssertExit(string == "[5, 5, 5, 5, 5, 5, 5, 5, 5, 5]");

    {
      // Check out exceptions
      Bool caught = False;

      IPosition ip1(2);
      IPosition ip2(3);  // ip1.conform(ip2) == False
      ip1 = 5; ip2 = 6;
      caught = False;
      try {ip1+=ip2;} catch (AipsError& x) {caught = True;}
      AlwaysAssertExit(caught);
      caught = False;
      try {ip1-=ip2;} catch (AipsError& x) {caught = True;}
      AlwaysAssertExit(caught);
      caught = False;
      try {ip1*=ip2;} catch (AipsError& x) {caught = True;}
      AlwaysAssertExit(caught);
      caught = False;
      try {ip1/=ip2;} catch (AipsError& x) {caught = True;}
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1+ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1-ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1*ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1/ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1==ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1!=ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1<ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1<=ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1>ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1>=ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
      caught = False;
      try {(void)(ip1=ip2);} catch (AipsError& x) {caught = True;} 
      AlwaysAssertExit(caught);
    }    
    // ~IPosition tested implicitly
    // at end of block
    {
      IPosition x;
      IPosition y(5,1,2,3,4,5);
      AlwaysAssertExit(x.product() == 0 && y.product() == 120);
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
      AlwaysAssertExit(ip3(0) == 1 && ip3(1) == 2 && ip3(2) == 3 &&
                       ip3(3) == 4 && ip3(4) == 5 && ip3(5) == 6);
      
      AlwaysAssertExit(IPosition(3,1).allOne());
      AlwaysAssertExit(! IPosition(3,0).allOne());
    }
    {
      std::vector<Int> vi;
      IPosition ip(3, 1, 2, 3);
      IPosition ip2(6, 1, 2, 3, 4, 5, 6);
      vi = ip.asStdVector();
      AlwaysAssertExit(vi[0] == 1 && vi[1] == 2 && vi[2] == 3);
      vi.resize(6);
      vi = ip2.asStdVector();
      AlwaysAssertExit(vi[0] == 1 && vi[1] == 2 && vi[2] == 3 && vi[3] == 4 &&
                       vi[4] == 5 && vi[5] == 6);
      IPosition ip3(vi);
      AlwaysAssertExit(ip3[0] == 1 && ip3[1] == 2 && ip3[2] == 3 &&
                       ip3[3] == 4 && ip3[4] == 5 && ip3[5] == 6);
    }
    
  }
  
  testKeepRemove();

  // operator()(IPostion) tests
  IPosition ipos(4,11,12,13,14);
  AlwaysAssertExit(ipos(IPosition(4, 0,2,1,3)) == IPosition(4, 11,13,12,14));
  AlwaysAssertExit(ipos(IPosition(3, 2,2,1)) == IPosition(3, 13,13,12));
  
  cout << "OK\n";
  return 0;
}
