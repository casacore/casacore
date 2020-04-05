//# tArrayIter.cc: This program tests Array iteration
//# Copyright (C) 1993,1994,1995,1999,2001
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

#include "../Array.h"
#include "../ArrayIter.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
//#include "../ArrayIO.h"
#include "../IPosition.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_iter1)

BOOST_AUTO_TEST_CASE( arraypositer_zero_dimension )
{
  IPosition shape{5, 5};
  ArrayPositionIterator ai (shape, 0);
  IPosition index(2);

  for (int iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
    index = ai.pos();
    BOOST_CHECK_EQUAL(index (0), iloop%5);
    BOOST_CHECK_EQUAL(index (1), iloop/5);
  }
}

BOOST_AUTO_TEST_CASE( arraypositer_one_dimension )
{
  IPosition shape{5, 5};
  Array<int> arr (shape);
  ArrayPositionIterator ai (shape, 1);

  IPosition index (2);

  for (int iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
      index = ai.pos();
      arr (index) = 4 * iloop;
      BOOST_CHECK_EQUAL(index(0), 0);
      BOOST_CHECK_EQUAL(index(1), iloop);
      BOOST_CHECK_EQUAL(arr (index), iloop*4);
  }
  ai.set (IPosition(1,4));
  BOOST_CHECK_EQUAL (ai.pos(), IPosition(2,0,4));
  ai.set (IPosition(2,2,3));
  BOOST_CHECK_EQUAL (ai.pos(), IPosition(2,0,3));
  
  BOOST_CHECK(!ai.pastEnd());
  BOOST_CHECK_EQUAL(ai.pos(), IPosition(2,0,3));
  ai.next();
  BOOST_CHECK(!ai.pastEnd());
  BOOST_CHECK_EQUAL(ai.pos(), IPosition(2,0,4));
  ai.next();
  BOOST_CHECK(ai.pastEnd());
}

BOOST_AUTO_TEST_CASE( arrayiter_one_dimension )
{
  IPosition shape{5, 5};
  Array<int> arr (shape);
  ArrayIterator<int> arri (arr, 1);
  IPosition arriindex (1, 0);
  
  ArrayPositionIterator ai (shape, 1);
  for (int iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ )
    arr (ai.pos()) = 4 * iloop;
      
  for (int iloop = 0; ! arri.pastEnd(); arri.next(), iloop++ ) {
    BOOST_CHECK_EQUAL((arri.array ()) (arriindex), iloop*4);
  }
}

BOOST_AUTO_TEST_CASE( double_arraypositer )
{
  IPosition shape{5, 5};
  ArrayPositionIterator ai (shape, 1);
  Array<double> arr (shape);

  IPosition index (2);
  for (int  iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
    index = ai.pos();
    arr (index) = double (4 * iloop) + 0.5;
    BOOST_CHECK_EQUAL(index (0), 0);
    BOOST_CHECK_EQUAL(index (1), iloop);
    BOOST_CHECK_CLOSE_FRACTION(arr (index), double (4 * iloop) + 0.5, 1e-6);
  }
}

BOOST_AUTO_TEST_CASE( double_arrayiter )
{
  IPosition shape{5, 5};
  ArrayPositionIterator ai (shape, 1);
  Array<double> arr (shape);

  for (int  iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ )
    arr (ai.pos()) = double (4 * iloop) + 0.5;
    
  ArrayIterator<double> arri (arr, 1);
  IPosition arriindex (1, 0);
  for (int iloop = 0; ! arri.pastEnd(); arri.next(), iloop++ )
  {
    BOOST_CHECK_CLOSE_FRACTION((arri.array ()) (arriindex), double (4 * iloop) + 0.5, 1e-6);
  }
}

BOOST_AUTO_TEST_CASE( int_arrayiter )
{
  // Test that leading degenerate axes do not go away.
  // Check if each chunk matches.
  IPosition shape{2,3,4,5,6};
  Array<int> ai(shape);
  indgen(ai);

  // Test a regular iterator.
  ArrayIterator<int> iter(ai, 2);
  BOOST_CHECK(iter.array().shape() == IPosition(2,shape(0),shape(1)));
  while (!iter.pastEnd()) {
    Array<int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
    BOOST_CHECK(iter.array().data() == tmparr.data());
    BOOST_CHECK(allEQ (iter.array(), tmparr));
    iter.next();
  }
  {
      // Test that a cursor as large as the array works
      ArrayIterator<int> aiter(ai, 5);
      BOOST_CHECK(allEQ(aiter.array(), ai));
      aiter.next();
      BOOST_CHECK(aiter.pastEnd());
    }
    {
      // Test iterator with arbitrary axes.
      ReadOnlyArrayIterator<int> iter(ai, IPosition(3,4,1,3), false);
      BOOST_CHECK(iter.array().shape() == IPosition(2,shape(0),shape(2)));
      while (!iter.pastEnd()) {
	Array<int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
	BOOST_CHECK(allEQ (iter.array(), tmparr));
	iter.next();
      }
      iter.reset();
      // Check if iteration is in correct order.
      IPosition blc(5,0);
      IPosition trc(shape-1);
      for (int ax3=0; ax3<shape(3); ++ax3) {
	blc(3) = trc(3) = ax3;
	for (int ax1=0; ax1<shape(1); ++ax1) {
	  blc(1) = trc(1) = ax1;
	  for (int ax4=0; ax4<shape(4); ++ax4) {
	    blc(4) = trc(4) = ax4;
	    BOOST_CHECK(!iter.pastEnd());
	    BOOST_CHECK(blc==iter.pos()  &&  trc==iter.endPos());
	    iter.next();
	  }
	}
      }
      BOOST_CHECK(iter.pastEnd());
    }
}

BOOST_AUTO_TEST_CASE( int_arrayiter_part )
{
  // Test iterator with arbitrary axes on a part of an array.
  // Test that leading degenerate axes do not go away.
  // Check if each chunk matches.
  IPosition shape1(5, 10,20,16,20,15);
  Array<int> ai1(shape1);
  indgen(ai1);
  // Take a chunk from it.
  Array<int> ai(ai1(IPosition(5,1,2,1,4,3), IPosition(5,7,12,13,16,13),
                    IPosition(5,6,5,4,3,2)));
  IPosition shape(5, 2,3,4,5,6);
  BOOST_CHECK (ai.shape() == shape);
  {
    // Test a regular iterator.
    ArrayIterator<int> iter(ai, 2);
    BOOST_CHECK(iter.array().shape() == IPosition(2,shape(0),shape(1)));
    while (!iter.pastEnd()) {
      Array<int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
      BOOST_CHECK(iter.array().data() == tmparr.data());
      BOOST_CHECK(allEQ (iter.array(), tmparr));
      iter.next();
    }
  }
  {
    // Test that a cursor as large as the array works
    ArrayIterator<int> aiter(ai, 5);
    BOOST_CHECK(allEQ(aiter.array(), ai));
    aiter.next();
    BOOST_CHECK(aiter.pastEnd());
  }
  {
    // Test iterator with arbitrary axes.
    ReadOnlyArrayIterator<int> iter(ai, IPosition(3,4,1,3), false);
    BOOST_CHECK(iter.array().shape() == IPosition(2,shape(0),shape(2)));
    while (!iter.pastEnd()) {
      Array<int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
      BOOST_CHECK(allEQ (iter.array(), tmparr));
      iter.next();
    }
    iter.reset();
    // Check if iteration is in correct order.
    IPosition blc(5,0);
    IPosition trc(shape-1);
    for (int ax3=0; ax3<shape(3); ++ax3) {
      blc(3) = trc(3) = ax3;
      for (int ax1=0; ax1<shape(1); ++ax1) {
        blc(1) = trc(1) = ax1;
        for (int ax4=0; ax4<shape(4); ++ax4) {
          blc(4) = trc(4) = ax4;
          BOOST_CHECK(!iter.pastEnd());
          BOOST_CHECK(blc==iter.pos()  &&  trc==iter.endPos());
          iter.next();
        }
      }
    }
    BOOST_CHECK(iter.pastEnd());
    // Check if set works correctly.
    iter.set (IPosition(3,3,1,2));
    BOOST_CHECK(iter.pos() == IPosition(5,0,1,0,2,3));
    Array<int> tmparr1 (ai(iter.pos(), iter.endPos()).nonDegenerate());
    BOOST_CHECK(allEQ (iter.array(), tmparr1));
    iter.next();
    BOOST_CHECK(iter.pos() == IPosition(5,0,1,0,2,4));
    Array<int> tmparr2 (ai(iter.pos(), iter.endPos()).nonDegenerate());
    BOOST_CHECK(allEQ (iter.array(), tmparr2));
  }
  {
    ReadOnlyArrayIterator<int> iter(ai, IPosition(3,4,1,3));
    BOOST_CHECK(iter.array().shape() == IPosition(3,shape(1),shape(3),
                                                  shape(4)));
    while (!iter.pastEnd()) {
      Array<int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
      BOOST_CHECK(allEQ (iter.array(), tmparr));
      iter.next();
    }
    iter.reset();
    // Check if iteration is in correct order.
    IPosition blc(5,0);
    IPosition trc(shape-1);
    for (int ax2=0; ax2<shape(2); ++ax2) {
      blc(2) = trc(2) = ax2;
      for (int ax0=0; ax0<shape(0); ++ax0) {
        blc(0) = trc(0) = ax0;
        BOOST_CHECK(!iter.pastEnd());
        BOOST_CHECK(blc==iter.pos()  &&  trc==iter.endPos());
        iter.next();
      }
    }
    BOOST_CHECK(iter.pastEnd());
  }
}

BOOST_AUTO_TEST_CASE( iterate_empty_array )
{
  // Test iterating through an empty array.
  Vector<int> vec(0);
  {
    Array<int> arr(IPosition(2,0,5));
    ArrayIterator<int> iter(arr, 1);
    int nstep=0;
    while (!iter.pastEnd()) {
      Array<int>& darr = iter.array();
      darr.assign_conforming( vec );
      iter.next();
      nstep++;
    }
    BOOST_CHECK (nstep==5);
  }
  {
    Array<int> arr(IPosition(2,5,0));
    ArrayIterator<int> iter(arr, 1);
    int nstep=0;
    while (!iter.pastEnd()) {
      Array<int>& darr = iter.array();
      darr.assign_conforming( vec );
      iter.next();
      nstep++;
    }
    BOOST_CHECK (nstep==0);
  }
  {
    Array<int> arr(IPosition(1,2));
    ArrayIterator<int> iter(arr, 1);
    int nstep=0;
    while (!iter.pastEnd()) {
      Array<int>& darr = iter.array();
      BOOST_CHECK (darr.shape() == IPosition(1,2));
      iter.next();
      nstep++;
    }
    BOOST_CHECK (nstep==1);
  }
  {
    Array<int> arr(IPosition(1,0));
    ArrayIterator<int> iter(arr, 1);
    int nstep=0;
    while (!iter.pastEnd()) {
      iter.next();
      nstep++;
    }
    BOOST_CHECK (nstep==0);
  }
}

BOOST_AUTO_TEST_CASE( virtual_iteration_function )
{
  Array<int> arr(IPosition(2,4,5));
  indgen (arr);
  std::unique_ptr<ArrayPositionIterator> iter = arr.makeIterator(1);
  
  int count = 0;
  while (!iter->pastEnd()) {
    Array<int>& subarr = dynamic_cast<Array<int>&>(iter->getArray());
    int ref[4] = {count*4, count*4+1, count*4+2, count*4+3};
    BOOST_CHECK_EQUAL_COLLECTIONS(subarr.begin(), subarr.end(), std::begin(ref), std::end(ref));
    iter->next();
    ++count;
  }
  BOOST_CHECK_EQUAL(count, 5);
}

BOOST_AUTO_TEST_SUITE_END()
