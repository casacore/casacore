//# tArrayIter1.cc: This program test the Array-based iterators
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2001
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

//# Includes

#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../ArrayPosIter.h"
#include "../ArrayIter.h"
#include "../MatrixIter.h"
#include "../VectorIter.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_iter2)

// Test iterating through a subset of a 5D array.
void checkIter (const Array<int>& array, const IPosition& blc,
		const IPosition& trc, const IPosition& inc)
{
  Array<int> a(array);
  Array<int> a1 = a(blc, trc, inc);
  {
    IPosition st(blc);
    IPosition end(blc);
    end(0) = trc(0);
    IPosition shp = a1.shape();
    shp.resize (1);
    ReadOnlyArrayIterator<int> ai(a1,1);
    for (int i5=blc(4); i5<=trc(4); i5+=inc(4)) {
      for (int i4=blc(3); i4<=trc(3); i4+=inc(3)) {
	for (int i3=blc(2); i3<=trc(2); i3+=inc(2)) {
	  for (int i2=blc(1); i2<=trc(1); i2+=inc(1)) {
	    BOOST_CHECK(allEQ (ai.array(), a(st,end,inc).reform(shp)));
	    ai.next();
	    st(1)+=inc(1);
	    end(1)+=inc(1);
	  }
	  st(1) = blc(1);
	  end(1) = blc(1);
	  st(2)+=inc(2);
	  end(2)+=inc(2);
	}
	st(2) = blc(2);
	end(2) = blc(2);
	st(3)+=inc(3);
	end(3)+=inc(3);
      }
      st(3) = blc(3);
      end(3) = blc(3);
      st(4)+=inc(4);
      end(4)+=inc(4);
    }
    BOOST_CHECK (ai.pastEnd());
  }
  {
    IPosition st(blc);
    IPosition end(blc);
    end(0) = trc(0);
    end(1) = trc(1);
    IPosition shp = a1.shape();
    shp.resize (2);
    ReadOnlyArrayIterator<int> ai(a1,2);
    for (int i5=blc(4); i5<=trc(4); i5+=inc(4)) {
      for (int i4=blc(3); i4<=trc(3); i4+=inc(3)) {
	for (int i3=blc(2); i3<=trc(2); i3+=inc(2)) {
	  BOOST_CHECK(allEQ (ai.array(), a(st,end,inc).reform(shp)));
	  ai.next();
	  st(2)+=inc(2);
	  end(2)+=inc(2);
	}
	st(2) = blc(2);
	end(2) = blc(2);
	st(3)+=inc(3);
	end(3)+=inc(3);
      }
      st(3) = blc(3);
      end(3) = blc(3);
      st(4)+=inc(4);
      end(4)+=inc(4);
    }
    BOOST_CHECK (ai.pastEnd());
  }
  {
    IPosition st(blc);
    IPosition end(blc);
    end(0) = trc(0);
    end(1) = trc(1);
    end(2) = trc(2);
    IPosition shp = a1.shape();
    shp.resize (3);
    ReadOnlyArrayIterator<int> ai(a1,3);
    for (int i5=blc(4); i5<=trc(4); i5+=inc(4)) {
      for (int i4=blc(3); i4<=trc(3); i4+=inc(3)) {
	BOOST_CHECK(allEQ (ai.array(), a(st,end,inc).reform(shp)));
	ai.next();
	st(3)+=inc(3);
	end(3)+=inc(3);
      }
      st(3) = blc(3);
      end(3) = blc(3);
      st(4)+=inc(4);
      end(4)+=inc(4);
    }
    BOOST_CHECK (ai.pastEnd());
  }
  {
    IPosition st(blc);
    IPosition end(blc);
    end(0) = trc(0);
    end(1) = trc(1);
    end(2) = trc(2);
    end(3) = trc(3);
    IPosition shp = a1.shape();
    shp.resize (4);
    ReadOnlyArrayIterator<int> ai(a1,4);
    for (int i5=blc(4); i5<=trc(4); i5+=inc(4)) {
      BOOST_CHECK(allEQ (ai.array(), a(st,end,inc).reform(shp)));
      ai.next();
      st(4)+=inc(4);
      end(4)+=inc(4);
    }
    BOOST_CHECK (ai.pastEnd());
  }
  {
    IPosition st(blc);
    IPosition end(blc);
    end(0) = trc(0);
    end(1) = trc(1);
    end(2) = trc(2);
    end(3) = trc(3);
    end(4) = trc(4);
    IPosition shp = a1.shape();
    shp.resize (5);
    ReadOnlyArrayIterator<int> ai(a1,5);
    BOOST_CHECK(allEQ (ai.array(), a(st,end,inc).reform(shp)));
    ai.next();
    BOOST_CHECK (ai.pastEnd());
  }
}

BOOST_AUTO_TEST_CASE( iterate_5d_array )
{
    IPosition shape(3);
    shape=3;
    ArrayPositionIterator api(shape, 1);
    int count = 0;
    while (!api.pastEnd()) {
	count++;
	api.next();
    }
    BOOST_CHECK(count == 9);

    Cube<int> a(3,3,3);
    MatrixIterator<int> ai(a);
    int i;
    for (i = 0; i < 3; i++)
	a.xyPlane(i) = i;
    count = 0;
    while (!ai.pastEnd()) {
	BOOST_CHECK(allEQ (ai.array(), count));
	BOOST_CHECK(allEQ (ai.matrix(), count));
	count++;
	ai.next();
    }

    ReadOnlyMatrixIterator<int> roai(a);
    for (i = 0; i < 3; i++)
	a.xyPlane(i) = i;
    count = 0;
    while (!roai.pastEnd()) {
	BOOST_CHECK(allEQ (roai.array(), count));
	BOOST_CHECK(allEQ (roai.matrix(), count));
	count++;
	roai.next();
    }

    VectorIterator<int> ai2(a);
    count = 0;
    while (!ai2.pastEnd()) {
	ai2.vector().set(count);
	count++;
	ai2.next();
    }

    ai.origin();
    BOOST_CHECK(ai.atStart());

    count = 0;
    while (!ai.pastEnd()) {
	for (i=0; i<3; i++) {
	    BOOST_CHECK(allEQ (ai.matrix().column(i), count));
	    count++;
	}
	ai.next();
    }

    {
      // Test iterating by the same dimensionality as the array
      Vector<int> theArray(100);
      theArray = 0;
      ArrayPositionIterator api(theArray.shape(), 1);
      size_t count = 0;
      while (! api.pastEnd()) {
	count++;
	api.next();
      }
      BOOST_CHECK(count == 1);
      
      VectorIterator<int> vi(theArray);
      count = 0;
      while (! vi.pastEnd()) {
	count++;
	vi.next();
      }
      BOOST_CHECK(count == 1);
    }

    {
      Cube<int> acube(16,16,16);
      indgen(acube);
      {
	Vector<int> b(16);
	indgen(b);
	VectorIterator<int> ai(acube);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  b += 16;
	  ai.next();
	}
      }

      // Test iterating through a subset (bottomleft) of the cube.
      {
	Cube<int> a1 = acube(IPosition(3,0,0,0), IPosition(3,7,7,7));
	Vector<int> b(8);
	indgen(b);
	int count = 0;
	VectorIterator<int> ai(a1);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  count++;
	  b += 16;
	  if (count%8 == 0) {
	    b += 128;             // next plane in cube subset
	  }
	  ai.next();
	}
      }

      // Test iterating through a strided subset (bottomleft) of the cube.
      {
	Cube<int> a1 = acube(IPosition(3,0,0,0), IPosition(3,7,7,7),
			     IPosition(3,2,2,2));
	Vector<int> b(4);
	indgen(b, 0, 2);
	int count = 0;
	VectorIterator<int> ai(a1);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  count++;
	  b += 32;
	  if (count%4 == 0) {
	    b += 128 + 256;             // next plane in cube subset
	  }
	  ai.next();
	}
      }

      // Test iterating through a subset (middle) of the cube.
      {
	Cube<int> a1 = acube(IPosition(3,5,4,3), IPosition(3,12,11,13));
	Vector<int> b(8);
	indgen(b, 3*256+4*16+5);
	int count = 0;
	VectorIterator<int> ai(a1);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  count++;
	  b += 16;
	  if (count%8 == 0) {
	    b += 128;             // next plane in cube subset
	  }
	  ai.next();
	}
      }

      // Test iterating through a strided subset (middle) of the cube.
      {
	Cube<int> a1 = acube(IPosition(3,4,4,4), IPosition(3,11,11,11),
			     IPosition(3,2,2,2));
	Vector<int> b(4);
	indgen(b, 4*256+4*16+4, 2);
	int count = 0;
	VectorIterator<int> ai(a1);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  count++;
	  b += 32;
	  if (count%4 == 0) {
	    b += 128 + 256;             // next plane in cube subset
	  }
	  ai.next();
	}
      }

      // Test iterating through a strided subset (middle) of the cube
      // with an axis of length 1.
      {
	Cube<int> a1 = acube(IPosition(3,4,4,4), IPosition(3,11,4,11),
			     IPosition(3,2,2,2));
	Vector<int> b(4);
	indgen(b, 4*256+4*16+4, 2);
	int count = 0;
	VectorIterator<int> ai(a1);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  count++;
	  b += 512;
	  ai.next();
	}
      }
    }

    // Now test a 5D array.
    {
      Array<int> array(IPosition(5,8,10,12,14,16));
      indgen(array);
      {
	Array<int> b(IPosition(1,8));
	indgen(b);
	ArrayIterator<int> ai(array,1);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  b += 8;
	  ai.next();
	}
      }
      {
	Array<int> b(IPosition(2,8,10));
	indgen(b);
	ArrayIterator<int> ai(array,2);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  b += 8*10;
	  ai.next();
	}
      }
      {
	Array<int> b(IPosition(3,8,10,12));
	indgen(b);
	ArrayIterator<int> ai(array,3);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  b += 8*10*12;
	  ai.next();
	}
      }
      {
	Array<int> b(IPosition(4,8,10,12,14));
	indgen(b);
	ArrayIterator<int> ai(array,4);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  b += 8*10*12*14;
	  ai.next();
	}
      }
      {
	Array<int> b(IPosition(5,8,10,12,14,16));
	indgen(b);
	ArrayIterator<int> ai(array,5);
	while (!ai.pastEnd()) {
	  BOOST_CHECK(allEQ (ai.array(), b));
	  ai.next();
	}
      }

      // Test iterating through various subsets of the array.
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,9,11,13), IPosition(5,1,1,1,1,1));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,9,11,13), IPosition(5,1,1,1,1,1));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,9,11,13), IPosition(5,2,2,2,2,2));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,9,11,13), IPosition(5,2,1,3,1,2));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,3,11,13), IPosition(5,1,2,1,1,1));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,3,4,13), IPosition(5,1,1,1,1,1));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,7,3,11,5), IPosition(5,1,1,1,1,1));
      checkIter (array, IPosition(5,1,2,3,4,5),
		 IPosition(5,5,2,9,11,13), IPosition(5,1,1,1,1,1));
      checkIter (array, IPosition(5,5,2,3,4,5),
		 IPosition(5,5,2,9,11,13), IPosition(5,1,1,1,1,1));
    }
}

BOOST_AUTO_TEST_SUITE_END()
