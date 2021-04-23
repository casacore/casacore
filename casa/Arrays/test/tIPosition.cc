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

#include "../IPosition.h"
#include "../Vector.h"

#include <vector>
#include <map>

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(iposition)

BOOST_AUTO_TEST_CASE( keep_remove )
{
  IPosition p1(4,2,3,4,5);
  IPosition p2 (p1.removeAxes(IPosition(2,0,3)));
  BOOST_CHECK (p2.size()==2 && p2[0]==3 && p2[1]==4);
  IPosition p3 (p1.keepAxes(IPosition(2,2,1)));
  BOOST_CHECK (p2.size()==2 && p2[0]==3 && p2[1]==4);
}

BOOST_AUTO_TEST_CASE( common_operations )
{
  IPosition ip(3, 0, 1, 2);
  
  int nrit = 0;
  for (IPosition::const_iterator iter=ip.begin(); iter!=ip.end(); iter++) {
    BOOST_CHECK(*iter == ip[nrit++]);
  }
  BOOST_CHECK(nrit == 3);
  BOOST_CHECK(ip.nelements() == 3);
  BOOST_CHECK(ip.size() == 3);
  BOOST_CHECK(!ip.empty());
  BOOST_CHECK(ip(0) == 0 && ip(1) == 1 && ip(2) == 2);
  BOOST_CHECK(ip[0] == 0 && ip[1] == 1 && ip[2] == 2);
  BOOST_CHECK (ip.last() == 2);
  BOOST_CHECK (ip.last(1) == 1);
  BOOST_CHECK (ip.last(2) == 0);
  
  std::vector<int> vec(ip.begin(), ip.end());
  BOOST_CHECK(vec.size() == 3);
  BOOST_CHECK(vec[0] == 0 && vec[1] == 1 && vec[2] == 2);
  
  ip[2] = 21;
  BOOST_CHECK(ip(2) == 21);
  ip(2) = 23;
  BOOST_CHECK(ip(2) == 23);
  BOOST_CHECK (ip.last() == 23);
  ip.last() = 22;
  BOOST_CHECK(ip(2) == 22);
  
  IPosition ip2;
  BOOST_CHECK(ip2.nelements() == 0);
  BOOST_CHECK(ip2.size() == 0);
  BOOST_CHECK(ip2.empty());
  
  ip2 = ip;
  BOOST_CHECK(ip2 == ip);
  BOOST_CHECK(ip2(0) == 0 && ip2(1) == 1 && ip2(2) == 22);
  
  ip2 += 2;
  ip2 = ip2 - 1;
  ip2 -= 1;
  BOOST_CHECK(ip2(0) == 0 && ip2(1) == 1 && ip2(2) == 22);
  
  ip2 = 5;
  BOOST_CHECK(ip2 == 5);
  BOOST_CHECK(ip2 == ip2);
  BOOST_CHECK(ip2 != ip);
  
  ip2.resize(10);
  ip2 = 10;
  ip.resize(0);
  ip = ip2;
  BOOST_CHECK (ip == 10);
  BOOST_CHECK(ip.nelements() == 10);
  ip = ip * ip2;
  BOOST_CHECK(ip == 100);
  BOOST_CHECK (ip > ip2);
  BOOST_CHECK (ip >= ip2);
  BOOST_CHECK (ip2 < ip);
  BOOST_CHECK (ip2 <= ip);
  BOOST_CHECK (ip2 != ip);
  
  IPosition ip3(5,0,1,2,3,4);
  
  nrit = 0;
  for (IPosition::const_iterator iter=ip3.begin(); iter!=ip3.end(); iter++) {
    BOOST_CHECK(*iter == ip3[nrit++]);
  }
}

BOOST_AUTO_TEST_CASE( construct_from_int_array )
{
  Vector<int> emptyVec;
  IPosition emptyPos(emptyVec);
  BOOST_CHECK(emptyPos.nelements() == 0);

  Vector<int> filledVec{ 19, 82 };
  IPosition filledPos(filledVec);
  BOOST_CHECK(filledPos.nelements() == 2);
  BOOST_CHECK(filledPos[0] == 19);
  BOOST_CHECK(filledPos[1] == 82);

  Vector<int> bigVec{ 9, 8, 7, 6, 5, 4, 3 };
  IPosition bigPos(bigVec);
  BOOST_CHECK(bigPos.nelements() == 7);
  for(int i=0; i!=7; ++i)
    BOOST_CHECK(bigPos[i] == (9-i));
}

BOOST_AUTO_TEST_CASE( construct_from_ll_array )
{
  Vector<long long> emptyVec;
  IPosition emptyPos(emptyVec);
  BOOST_CHECK(emptyPos.nelements() == 0);

  Vector<long long> filledVec{ 19, 82 };
  IPosition filledPos(filledVec);
  BOOST_CHECK(filledPos.nelements() == 2);
  BOOST_CHECK(filledPos[0] == 19);
  BOOST_CHECK(filledPos[1] == 82);

  Vector<long long> bigVec{ 9, 8, 7, 6, 5, 4, 3 };
  IPosition bigPos(bigVec);
  BOOST_CHECK(bigPos.nelements() == 7);
  for(int i=0; i!=7; ++i)
    BOOST_CHECK(bigPos[i] == (9-i));
}

BOOST_AUTO_TEST_CASE(make)
{
  IPosition d = IPosition::Make(8, 5, 19, 82);
  BOOST_CHECK_EQUAL(d.nelements(), 4);
  BOOST_CHECK_EQUAL(d[0], 8);
  BOOST_CHECK_EQUAL(d[1], 5);
  BOOST_CHECK_EQUAL(d[2], 19);
  BOOST_CHECK_EQUAL(d[3], 82);
  
  IPosition l = IPosition::Make(9, 8, 7, 6, 5, 4, 3);
  BOOST_CHECK_EQUAL(l.nelements(), 7);
  for(int i=0; i!=7; ++i)
    BOOST_CHECK_EQUAL(l[i], 9-i);
}

BOOST_AUTO_TEST_CASE( move_exhaustively )
{
  int i;
  
  IPosition ip1;                             // IPosition()
  BOOST_CHECK(ip1.nelements() == 0);              // nelements()
  IPosition ip2(5);                          // IPosition(size_t);
  BOOST_CHECK(ip2.nelements() == 5);
  IPosition ip3(5, 0, 1, 2, 3, 4);           // IPosition(size_t, int, ...)
  BOOST_CHECK(ip3.nelements() == 5);
  const IPosition &rip3 = ip3;
  for (i=0; i<5; i++) {
    BOOST_CHECK(ip3(i) == i);         // operator()(size_t)
    BOOST_CHECK(rip3(i) == i);        // operator()(size_t) const
  }
  IPosition ip4(ip3);                        // IPosition(const IPosition &)
  BOOST_CHECK(ip4.nelements() == 5);
  for (i=0; i<5; i++) {
    BOOST_CHECK(ip4(i) == i);
  }
  ip1 = ip4;                                 // operator=(const IPosition &);
  BOOST_CHECK(ip1.nelements() == 5);
  for (i=0; i<5; i++) {
    BOOST_CHECK(ip1(i) == i);
  }
  ip1 = ip1;
  for (i=0; i<5; i++) {
    BOOST_CHECK(ip1(i) == i);
  }
  
  ip1 = 1;                                   // operator=(int);
  BOOST_CHECK(ip1.nelements() == 5);
  for (i=0; i<5; i++) {
    BOOST_CHECK(ip1(i) == 1);
  }
  ip1 = ip4;
  BOOST_CHECK(ip1.nelements() == 5);
  for (i=0; i<5; i++) {
    BOOST_CHECK(ip1(i) == i);
  }
  
  ip1.resize(10);                            // resize(size_t)
  BOOST_CHECK(ip1.nelements() == 10);
  
  BOOST_CHECK(ip1.conform(ip4) == false); // conform
  BOOST_CHECK(ip4.conform(ip3) == true);
  
  IPosition ipf (2,3,4);
  IPosition ipl (3,14,15,16);
  IPosition ipr (5,3,4,14,15,16);
  IPosition ipp0 (ipf);
  ipp0.append (ipl);
  BOOST_CHECK (ipp0 == ipr);
  IPosition ipp1 (ipl);
  ipp1.prepend (ipf);
  BOOST_CHECK (ipp1 == ipr);
  BOOST_CHECK (ipf.concatenate (ipl) == ipr);
  IPosition ipgf = ipr.getFirst (2);
  BOOST_CHECK (ipgf == ipf);
  IPosition ipgl = ipr.getLast (3);
  BOOST_CHECK (ipgl == ipl);
  ipp0.setFirst (ipl);
  BOOST_CHECK (ipp0 == IPosition(5,14,15,16,15,16));
  ipp1.setLast (ipf);
  BOOST_CHECK (ipp1 == IPosition(5,3,4,14,3,4));
  
  
  // Member fn arithmetic
  ip1 = 1;
  ip1 += 5;
  BOOST_CHECK(ip1 == 6);
  ip1 -= 1;
  BOOST_CHECK(ip1 == 5);
  ip1 *= 2;
  BOOST_CHECK(ip1 == 10);
  ip1 /= 2;
  BOOST_CHECK(ip1 == 5);
  ip2.resize(10);
  ip2 = 1;
  ip1 += ip2;
  BOOST_CHECK(ip1 == 6);
  ip1 -= ip2;
  BOOST_CHECK(ip1 == 5);
  ip2 = 2;
  ip1 *= ip2;
  BOOST_CHECK(ip1 == 10);
  ip1 /= ip2;
  BOOST_CHECK(ip1 == 5);
  // Global fn arithmetic
  ip1 = ip1 + ip2;
  BOOST_CHECK(ip1 == 7);
  ip1 = ip1 - ip2;
  BOOST_CHECK(ip1 == 5);
  ip1 = ip2 * ip1;
  BOOST_CHECK(ip1 == 10);
  ip1 = ip1 / ip2;
  BOOST_CHECK(ip1 == 5);
  ip1 = ip1 + 1;
  BOOST_CHECK(ip1 == 6);
  ip1 = ip1 - 1;
  BOOST_CHECK(ip1 == 5);
  ip1 = ip1 * 2;
  BOOST_CHECK(ip1 == 10);
  ip1 = ip1 / 2;
  BOOST_CHECK(ip1 == 5);
  ip1 = 1 + ip1;
  BOOST_CHECK(ip1 == 6);
  ip1 = 1 - ip1;
  BOOST_CHECK(ip1 == -5);
  ip1 = -2 * ip1;
  BOOST_CHECK(ip1 == 10);
  ip1 = 50 / ip1;
  BOOST_CHECK(ip1 == 5);
  // Global fn logicals
  ip2.resize(ip1.nelements());
  ip2 = 6;
  BOOST_CHECK(ip1 == ip1);
  BOOST_CHECK((ip1 == ip2) == false);
  BOOST_CHECK (ip1.isEqual(ip1));
  BOOST_CHECK (ip1.isEqual(ip2) == false);
  BOOST_CHECK(ip1 != ip2);
  BOOST_CHECK((ip1 != ip1) == false);
  BOOST_CHECK(ip1 < ip2);
  BOOST_CHECK((ip1 < ip1) == false);
  BOOST_CHECK(ip1 <= ip2);
  BOOST_CHECK((ip1 <= ip1));
  ip2(0) = 0;
  BOOST_CHECK((ip1 <= ip2) == false);
  BOOST_CHECK(ip1 + 1 > ip1);
  BOOST_CHECK((ip1  > ip1) == false);
  BOOST_CHECK((ip2 > ip1) == false);
  BOOST_CHECK(ip1 + 1 >= ip1);
  BOOST_CHECK(ip1 >= ip1);
  BOOST_CHECK((ip2 >= ip1) == false);
  BOOST_CHECK(ip1 == 5);
  BOOST_CHECK((ip1 == 6) == false);
  BOOST_CHECK(5 == ip1);
  BOOST_CHECK((6 == ip1) == false);
  BOOST_CHECK(ip1 != 6);
  BOOST_CHECK((ip1 != 5) == false);
  BOOST_CHECK(6 != ip1);
  BOOST_CHECK((5 != ip1) == false);
  BOOST_CHECK(ip1 < 6);
  BOOST_CHECK((ip1 < 5) == false);
  BOOST_CHECK(4 < ip1);
  BOOST_CHECK((5 < ip1) == false);
  BOOST_CHECK(ip1 <= 6);
  BOOST_CHECK(ip1 <= 5);
  BOOST_CHECK((ip1 <= 4) == false);
  BOOST_CHECK(4 <= ip1);
  BOOST_CHECK(5 <= ip1);
  BOOST_CHECK((6 <= ip1) == false);
  BOOST_CHECK(ip1 > 4);
  BOOST_CHECK((ip1 > 5) == false);
  BOOST_CHECK(ip1 >= 4);
  BOOST_CHECK((ip1 >= 5));
  BOOST_CHECK((ip1 >= 6) == false);
  BOOST_CHECK(6 > ip1);
  BOOST_CHECK(( 5 > ip1) == false);
  BOOST_CHECK(5 >= ip1);
  BOOST_CHECK((4 >= ip1) == false);
  
  IPosition ip7 (ip1.nelements());
  IPosition ip8 (ip1.nelements() + 1);
  ip7 = 0;
  ip8 = 0;
  for (size_t ipindex=0; ipindex < ip1.nelements(); ipindex++) {
    ip7(ipindex) = ip1(ipindex);
    ip8(ipindex) = ip1(ipindex);
  }
  BOOST_CHECK (ip7.isEqual(ip1));
  BOOST_CHECK (ip7.isEqual(ip8) == false);
  
  BOOST_CHECK (! IPosition(1,1).isEqual (IPosition(), false));
  BOOST_CHECK (IPosition(1,1).isEqual (IPosition(), true));
  BOOST_CHECK (IPosition(1,1).isEqual (IPosition(1,1), true));
  BOOST_CHECK (IPosition(1,1).isEqual (IPosition(2,1,1), true));
  BOOST_CHECK (IPosition(2,1,1).isEqual (IPosition(2,1,1), true));
  BOOST_CHECK (IPosition(2,1,1).isEqual (IPosition(1,1), true));
  BOOST_CHECK (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(3,2,3,4),
                                                 true));
  BOOST_CHECK (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(5,1,1,2,3,4),
                                                 true));
  BOOST_CHECK (IPosition(6,2,1,1,3,1,4).isEqual (IPosition(5,2,3,4,1,1),
                                                 true));
  BOOST_CHECK (IPosition(3,2,3,4).isEqual (IPosition(5,2,1,3,1,4),
                                           true));
  
  BOOST_CHECK (IPosition(3,2,3,4).nonDegenerate().isEqual
  (IPosition(3,2,3,4)));
  BOOST_CHECK (IPosition(4,1,2,3,4).nonDegenerate().isEqual
  (IPosition(3,2,3,4)));
  BOOST_CHECK (IPosition(5,1,1,2,3,4).nonDegenerate().isEqual
  (IPosition(3,2,3,4)));
  BOOST_CHECK (IPosition(6,1,1,2,3,1,4).nonDegenerate().isEqual
  (IPosition(3,2,3,4)));
  BOOST_CHECK (IPosition(6,1,1,2,3,1,4).nonDegenerate(2).isEqual
  (IPosition(5,1,1,2,3,4)));
}

BOOST_AUTO_TEST_CASE(strio)
{
  IPosition ip1{5, 5, 5, 5, 5, 5, 5, 5, 5, 5};
  // Use an ostringstream to convert to ASCII and reading it back.
  // Note that function str "freezes" the buffer in ostringstream,
  // which means we have to delete it ourselves.
  // Also note that no terminating 0 is stored by operator<<.
  std::ostringstream os;                            // ostream &operator<<
  os << ip1;
  std::string string (os.str());
  BOOST_CHECK(string == "[5, 5, 5, 5, 5, 5, 5, 5, 5, 5]");
  BOOST_CHECK(to_string(ip1) == "[5, 5, 5, 5, 5, 5, 5, 5, 5, 5]");
}

BOOST_AUTO_TEST_CASE(exceptions)
{
  // Check out exceptions
  IPosition ip1(2);
  IPosition ip2(3);  // ip1.conform(ip2) == false
  ip1 = 5; ip2 = 6;
  BOOST_CHECK_THROW(ip1+=ip2, std::runtime_error);
  BOOST_CHECK_THROW(ip1-=ip2, std::runtime_error);
  BOOST_CHECK_THROW(ip1*=ip2, std::runtime_error);
  BOOST_CHECK_THROW(ip1/=ip2, std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1+ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1-ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1*ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1/ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1==ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1!=ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1<ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1<=ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1>ip2), std::runtime_error);
  BOOST_CHECK_THROW((void)(ip1>=ip2), std::runtime_error);
  
  // Assignment no longer requires conformance
  // BOOST_CHECK_THROW((void)(ip1=ip2), std::runtime_error);
}    
// ~IPosition tested implicitly
// at end of block

BOOST_AUTO_TEST_CASE( product )
{
  IPosition x;
  IPosition y(5,1,2,3,4,5);
  BOOST_CHECK(x.product() == 0 && y.product() == 120);
}

BOOST_AUTO_TEST_CASE( as_vector )
{
  Vector<int> vi;
  IPosition ip(3, 1, 2, 3);
  IPosition ip2(6, 1, 2, 3, 4, 5, 6);
  vi = ip.asVector();
  BOOST_CHECK(vi(0) == 1 && vi(1) == 2 && vi(2) == 3);
  vi.resize(6);
  vi = ip2.asVector();
  BOOST_CHECK(vi(0) == 1 && vi(1) == 2 && vi(2) == 3 && vi(3) == 4 &&
  vi(4) == 5 && vi(5) == 6);
  IPosition ip3(vi);
  BOOST_CHECK(ip3(0) == 1 && ip3(1) == 2 && ip3(2) == 3 &&
  ip3(3) == 4 && ip3(4) == 5 && ip3(5) == 6);
  
  BOOST_CHECK(IPosition(3,1).allOne());
  BOOST_CHECK(! IPosition(3,0).allOne());
}
BOOST_AUTO_TEST_CASE( as_std_vector )
{
  std::vector<int> vi;
  IPosition ip(3, 1, 2, 3);
  IPosition ip2(6, 1, 2, 3, 4, 5, 6);
  vi = ip.asStdVector();
  BOOST_CHECK(vi[0] == 1 && vi[1] == 2 && vi[2] == 3);
  vi.resize(6);
  vi = ip2.asStdVector();
  BOOST_CHECK(vi[0] == 1 && vi[1] == 2 && vi[2] == 3 && vi[3] == 4 &&
  vi[4] == 5 && vi[5] == 6);
  IPosition ip3(vi);
  BOOST_CHECK(ip3[0] == 1 && ip3[1] == 2 && ip3[2] == 3 &&
  ip3[3] == 4 && ip3[4] == 5 && ip3[5] == 6);
}

BOOST_AUTO_TEST_CASE( index_operator )
{
  // operator()(IPostion) tests
  IPosition ipos(4,11,12,13,14);
  BOOST_CHECK(ipos(IPosition(4, 0,2,1,3)) == IPosition(4, 11,13,12,14));
  BOOST_CHECK(ipos(IPosition(3, 2,2,1)) == IPosition(3, 13,13,12));
  // test IPOsitionComparator
  IPosition ip0(2, 0);
  IPosition ip1(2, 1);
  IPosition ip2(3, 1);
  IPosition ip3(2, 0, 1);
  IPosition ip4(2, 1, 0);
  // creating a map with IPosition keys without the comparator
  // doesn't work as we would like
  std::map<IPosition, int> mymap;
  mymap[ip0] = 0;
  mymap[ip1] = 1;
  bool thrown = false;
  try {
    // throws exception because size not equal
    mymap[ip2] = 2;
  } catch (const std::runtime_error& x) {
    thrown = true;
  }
  BOOST_CHECK(thrown);
  mymap[ip3] = 3;
  mymap[ip4] = 4;
  // one would think the map now has four elements, but no, it only has two
  BOOST_CHECK(mymap.size() != 4);
  // so use the comparator and things make sense
  std::map<IPosition, int, IPositionComparator> goodmap;
  goodmap[ip0] = 0;
  goodmap[ip1] = 1;
  goodmap[ip2] = 2;
  goodmap[ip3] = 3;
  goodmap[ip4] = 4;
  BOOST_CHECK(goodmap.size() == 5);
}

BOOST_AUTO_TEST_SUITE_END()
