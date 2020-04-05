//# tArrayAccessor.cc: Test program for the ArrayAccessor class
//# Copyright (C) 2002
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
//#        internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes
#include "../ArrayAccessor.h"
#include "../ArrayMath.h"
#include "../Cube.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_accessor)

BOOST_AUTO_TEST_CASE( cube_access )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx(0);
  for (size_t i=0; i!=N2; ++i) {
    for (size_t j=0; j!=N1; ++j) {
      for (size_t k=0; k!=N0; ++k) {
        BOOST_CHECK_EQUAL(cub(k,j,i), inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_constructors )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx(0);
  for (ArrayAccessor<int, Axis<2> > i(cub); i!=i.end(); ++i) {
    for (ArrayAccessor<int, Axis<1> > j(i); j!=j.end(); ++j) {
      for (ArrayAccessor<int, Axis<0> > k(j); k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axisn_constructors )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx(0);
  for (ArrayAccessor<int, AxisN> i(cub, AxisN(2)); i!=i.end(); ++i) {
    for (ArrayAccessor<int, AxisN> j(i, AxisN(1)); j!=j.end(); ++j) {
      for (ArrayAccessor<int, AxisN> k(j, AxisN(0)); k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_axisn_constructors1 )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx(0);
  for (ArrayAccessor<int, Axis<2> > i(cub); i!=i.end(); ++i) {
    for (ArrayAccessor<int, AxisN> j(i, AxisN(1)); j!=j.end(); ++j) {
      for (ArrayAccessor<int, Axis<0> > k(j); k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_axisn_constructors2 )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx = 0;
  for (ArrayAccessor<int, AxisN> i(cub, AxisN(2)); i!=i.end(); ++i) {
    for (ArrayAccessor<int, Axis<1> > j(i); j!=j.end(); ++j) {
      for (ArrayAccessor<int, AxisN> k(j, AxisN(0)); k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }  
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_assignments )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, Axis<2> > i(cub);
  ArrayAccessor<int, Axis<1> > j;
  ArrayAccessor<int, Axis<0> > k;
  ArrayAccessor<int, Axis<0> > l;
  for (; i!=i.end(); ++i) {
    for (j = i; j!=j.end(); ++j) {
      for (k =j; k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        l = k;
        BOOST_CHECK_EQUAL(*l, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axisn_assignments )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, AxisN> i(cub, AxisN(2));
  ArrayAccessor<int, AxisN> j(AxisN(1));
  ArrayAccessor<int, AxisN> k(AxisN(0));
  ArrayAccessor<int, AxisN> l(AxisN(0));
  for (; i!=i.end(); ++i) {
    for (j = i; j!=j.end(); ++j) {
      for (k =j; k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        l = k;
        BOOST_CHECK_EQUAL(*l, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_axisn_assignments1 )
{
  Cube<int> cub(10, 20, 30);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, Axis<2> > i(cub);
  ArrayAccessor<int, AxisN> j(AxisN(1));
  ArrayAccessor<int, Axis<0> > k;
  ArrayAccessor<int, AxisN> l(AxisN(0));
  for (; i!=i.end(); ++i) {
    for (j = i; j!=j.end(); ++j) {
      for (k =j; k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        l = k;
        BOOST_CHECK_EQUAL(*l, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_axisn_assignments2 )
{
  Cube<int> cub(10, 20, 30);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, AxisN> i(cub, AxisN(2));
  ArrayAccessor<int, Axis<1> > j;
  ArrayAccessor<int, AxisN> k(AxisN(0));
  ArrayAccessor<int, Axis<0> > l;
  for (; i!=i.end(); ++i) {
    for (j = i; j!=j.end(); ++j) {
      for (k =j; k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        l = k;
        BOOST_CHECK_EQUAL(*l, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_init1 )
{
  Cube<int> cub(10, 20, 30);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, Axis<2> > i(cub);
  for (; i!=i.end(); ++i) {
    for (ArrayAccessor<int, Axis<1> > j(i); j!=j.end(); ++j) {
      for (ArrayAccessor<int, Axis<0> > k(j); k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_init2 )
{
  Cube<int> cub(10, 20, 30);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, Axis<2> > i(cub);
  while(i!=i.end()) ++i;
  for (i.init(cub); i!=i.end(); ++i) {
    for (ArrayAccessor<int, Axis<1> > j(i); j!=j.end(); ++j) {
      for (ArrayAccessor<int, Axis<0> > k(j); k!=k.end(); ++k) {
       BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axisn_init )
{
  Cube<int> cub(10, 20, 30);
  indgen(cub);
  
  int inx = 0;
  ArrayAccessor<int, AxisN> i(cub, AxisN(2));
  ArrayAccessor<int, AxisN> j(AxisN(1));
  ArrayAccessor<int, AxisN> k(AxisN(0));
  for (; i!=i.end(); ++i) {
    for (j = i; j!=j.end(); ++j) {
      for (k = j; k!=k.end(); ++k) {
        BOOST_CHECK_EQUAL(*k, inx);
        ++inx;
      }
    }
  }
  inx = 0;
  j.init(AxisN(0));
  k.init(AxisN(1));
  for (i.init(cub, AxisN(2)); i!=i.end(); ++i) {
    for (k = i; k!=k.end(); ++k) {
      for (j = k; j!=j.end(); ++j) {
        BOOST_CHECK_EQUAL(*j, inx);
        ++inx;
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( axis_access )
{
  const size_t N0=10;
  Cube<int> cub(N0, 20, 30);
  indgen(cub);
  
  int inx = 0;
  for (ArrayAccessor<int, Axis<2> > i(cub); i!=i.end(); ++i) {
    for (ArrayAccessor<int, Axis<1> > j(i); j!=j.end(); ++j) {
      BOOST_CHECK_EQUAL(*j, inx);
      BOOST_CHECK_EQUAL(j.next<Axis<0> >(), inx+1);
      BOOST_CHECK_EQUAL(j.index<Axis<0> >(2), inx+2);
      BOOST_CHECK_EQUAL(j.next(AxisN(0)), inx+1);
      BOOST_CHECK_EQUAL(j.index(2, AxisN(0)), inx+2);
      if (j == j.end()-1) {
        BOOST_CHECK_EQUAL(j.prev<Axis<0> >(), inx-1);
        BOOST_CHECK_EQUAL(j.prev(AxisN(0)), inx-1);
      }
      inx += N0;
    }
  }
}

BOOST_AUTO_TEST_CASE( operations )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  // Test += -= -- ++ --- 
  int inx = 0;
  int ln = N0*N1;
  for (ArrayAccessor<int, Axis<2> > i(cub); i!=i.end(-4); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    ++i; BOOST_CHECK_EQUAL(*i, inx+1*ln);
    i++; BOOST_CHECK_EQUAL(*i, inx+2*ln);
    --i; BOOST_CHECK_EQUAL(*i, inx+1*ln);
    i--; BOOST_CHECK_EQUAL(*i, inx);
    i += 3; BOOST_CHECK_EQUAL(*i, inx+3*ln);
    i -= 1; BOOST_CHECK_EQUAL(*i, inx+2*ln);
    i -= 2;
    inx += ln;
  }
}

BOOST_AUTO_TEST_CASE( dereferencing )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  int inx = 0;
  int ln = N0*N1;
  size_t cnt = 0;
  for (ArrayAccessor<int, Axis<2> > i(cub); i!=i.end(-2); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    BOOST_CHECK(i.data() == &cub(0,0,cnt));
    BOOST_CHECK(&(i.baseArray()) == &cub);
    BOOST_CHECK(i[2] == inx+2*ln);
    BOOST_CHECK(i.step() == size_t(ln));
    inx += ln;
    ++cnt;
  }
}

BOOST_AUTO_TEST_CASE( iterators )
{
  // test end(), begin(), rbegin(), rend()
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  int inx = 0;
  int ln = N0*N1;
  for (ArrayAccessor<int, Axis<2> > i(cub); i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    BOOST_CHECK_EQUAL(i.begin(), &cub(0,0,0));
    BOOST_CHECK_EQUAL(i.end(), i.begin()+N2*ln);
    BOOST_CHECK_EQUAL(i.rbegin(), i.end()-ln);
    BOOST_CHECK_EQUAL(i.rend(), i.begin()-ln);
    BOOST_CHECK_EQUAL(i.begin(-2), i.begin()-2*ln);
    BOOST_CHECK_EQUAL(i.end(-1), i.begin()+N2*ln-ln);
    BOOST_CHECK_EQUAL(i.rbegin(+5), i.end()-ln+5*ln);
    BOOST_CHECK_EQUAL(i.rend(1), i.begin());
    inx += ln;
  }
}

BOOST_AUTO_TEST_CASE( reset1 )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  int inx = 0;
  int ln = N0*N1;
  
  ArrayAccessor<int, Axis<2> > i(cub);
  for (; i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
  inx = 0;
  for (i.reset(); i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
  inx = 2*ln;
  for (i.reset(i.begin(2)); i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
}

BOOST_AUTO_TEST_CASE( reset2 )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  int inx = 0;
  int ln = N0*N1;
  
  ArrayAccessor<int, AxisN> i(cub, AxisN(2));
  for (; i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
  inx = 0;
  for (i.reset(); i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
  inx = 2*ln;
  for (i.reset(i.begin(2)); i!=i.end(); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
}

BOOST_AUTO_TEST_CASE( split_loops1 )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  int inx = 0;
  int ln = N0*N1;
  
  ArrayAccessor<int, Axis<2> > i(cub);
  ArrayAccessor<int, Axis<2> > j;
  for (; i!=i.end(-2); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
  for (j = i; j!=j.end(); ++j) {
    BOOST_CHECK_EQUAL(*j, inx);
    inx += ln;
  }
}

BOOST_AUTO_TEST_CASE( split_loops2 )
{
  const size_t N0=10, N1=20, N2=30;
  Cube<int> cub(N0, N1, N2);
  indgen(cub);
  int inx = 0;
  int ln = N0*N1;
 
  ArrayAccessor<int, AxisN> i(cub, AxisN(2));
  ArrayAccessor<int, AxisN> j(AxisN(2));
  for (; i!=i.end(-2); ++i) {
    BOOST_CHECK_EQUAL(*i, inx);
    inx += ln;
  }
  for (j = i; j!=j.end(); ++j) {
    BOOST_CHECK_EQUAL(*j, inx);
    inx += ln;
  }
}

BOOST_AUTO_TEST_SUITE_END()
