//# ArrayMath.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2001,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayPartMath.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(median_smooth)

BOOST_AUTO_TEST_CASE(test)
{
  const size_t specsize=50;
  const int width=1;
  IPosition box(1,width);
  Vector<float> input(specsize);
  for(size_t i=0;i<specsize;i++) input[i]=i%5+(i/5)*0.01;
  Vector<float> medians = slidingArrayMath(input, box, MedianFunc<float>(false,true,false));
  
  Vector<float> ref{
    0, 1, 2, 3, 3, 1.01, 1.01, 2.01, 3.01, 3.01, 
    1.02, 1.02, 2.02, 3.02, 3.02, 1.03, 1.03, 2.03, 3.03, 3.03,
    1.04, 1.04, 2.04, 3.04, 3.04, 1.05, 1.05, 2.05, 3.05, 3.05,
    1.06, 1.06, 2.06, 3.06, 3.06, 1.07, 1.07, 2.07, 3.07, 3.07,
    1.08, 1.08, 2.08, 3.08, 3.08, 1.09, 1.09, 2.09, 3.09, 0 };
  
  BOOST_CHECK_EQUAL(medians.shape(), ref.shape());
  for(size_t i=0; i!=ref.nelements(); ++i)
    BOOST_CHECK_CLOSE_FRACTION(medians[i], ref[i], 1e-4);
}

BOOST_AUTO_TEST_SUITE_END()
