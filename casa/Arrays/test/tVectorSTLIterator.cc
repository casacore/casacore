//# tVectorSTLIterator.cc: Test program for the VectroSTLIterator class
//# Copyright (C) 2010
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

#include "../VectorSTLIterator.h"
#include "../ArrayMath.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(vector_stl_iterator)

BOOST_AUTO_TEST_CASE(vector_forward)
{
  Vector<int> vec(10);
  indgen (vec);
  {
    VectorSTLIterator<int> iter(vec);
    for (int i=0; i<int(vec.size()); ++i) {
      BOOST_CHECK (*iter == i);
      BOOST_CHECK (iter[i] == i);
      ++iter;
    }
    iter -= 10;
    BOOST_CHECK (*iter == 0);
    iter += 4;
    BOOST_CHECK (*iter == 4);
    BOOST_CHECK (*(iter-4) == 0);
    BOOST_CHECK (*(iter+3) == 7);
    BOOST_CHECK (iter - VectorSTLIterator<int>(vec) == 4);
  }
}

BOOST_AUTO_TEST_CASE(vector_backward)
{
  Vector<int> vec(10);
  indgen (vec);
  VectorSTLIterator<int> iter(vec);
  iter += vec.size();
  for (int i=0; i<int(vec.size()); ++i) {
    --iter;
    BOOST_CHECK (*iter == int(vec.size()) - i - 1);
    BOOST_CHECK (iter[i] == i);
  }
}

BOOST_AUTO_TEST_CASE(array_forward)
{
  Vector<int> vec(10);
  indgen (vec);
  // Uses ArraySTLIterator, not VectorSTLIterator.
  int i=0;
  for (Vector<int>::const_iterator iter=vec.begin(); iter!=vec.end(); ++iter){
    BOOST_CHECK (*iter == i);
    ++i;
  }
}

BOOST_AUTO_TEST_SUITE_END()
