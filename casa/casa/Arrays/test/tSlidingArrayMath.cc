//# tSlidingArrayMath.cc: Test program for function slidingArrayMath
//# Copyright (C) 2006
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

#include <casa/Arrays/MaskArrMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayIO.h>

#include <casa/Utilities/GenSort.h>
#include <casa/OS/Timer.h>
#include <iostream>


#ifdef AIPS_NO_TEMPLATE_SRC
#include <casa/Arrays/ArrayMath.tcc>
#include <casa/Arrays/MaskArrMath.tcc>
namespace casa {
  template Array<Float> slidingArrayMath (const Array<Float>&,
					  const IPosition&,
					  Float (*) (const Array<Float>&),
					  Bool);
  template Array<Float> slidingArrayMath (const MaskedArray<Float>&,
					  const IPosition&,
					  Float (*) (const MaskedArray<Float>&),
					  Bool);
}
#endif //# AIPS_NO_TEMPLATE_SRC


using namespace casa;
using namespace std;

Float smartMedian (const Array<Float>& arr)
{
  Block<Float> bbuf(arr.size());
  Float* buf = bbuf.storage();
  Int nl = 0;
  Int nr = arr.size();
  Float pivot = nr/2;
  Array<Float>::const_iterator iterEnd=arr.end();
  for (Array<Float>::const_iterator iter=arr.begin();
       iter!=iterEnd;
       ++iter) {
    Float val = *iter;
    if (val <= pivot) {
      buf[nl++] = val;
    } else {
      buf[--nr] = val;
    }
  }
  return GenSort<Float>::kthLargest (buf, nl, nl/2);
}

void doIt (Bool doTiming)
{
  {
    IPosition shape(2,5,5);
    Array<Float> arr(shape);
    indgen (arr);
    cout << slidingArrayMath(arr, IPosition(2,2), casa::sum, False) << endl;
    cout << slidingArrayMath(arr, IPosition(2,1), casa::sum, False) << endl;
    cout << slidingArrayMath(arr, IPosition(1,0), casa::sum, False) << endl;

    cout << slidingArrayMath(arr, IPosition(4,2), casa::sum, True) << endl;
    cout << slidingArrayMath(arr, IPosition(3,1), casa::sum, True) << endl;
    cout << slidingArrayMath(arr, IPosition(),    casa::sum, True) << endl;

    cout << slidingArrayMath(arr, IPosition(2,2), casa::median, False) << endl;
    cout << slidingArrayMath(arr, IPosition(2,1), casa::median, False) << endl;
    cout << slidingArrayMath(arr, IPosition(2,0), casa::median, False) << endl;
  }

  if (doTiming) {
    Array<Float> arr(IPosition(2,1000,1000));
    indgen(arr);
    Timer timer;
    Array<Float> res = slidingArrayMath(arr, IPosition(2,25), casa::median);
    timer.show();
    timer.mark();
    {
      IPosition blc(2,0);
      IPosition trc(2,30);
      for (Int i=0; i<100000; i++) {
	median(arr(blc,trc));
      }
    }
    timer.show();
    timer.mark();
    {
      IPosition blc(2,0);
      IPosition trc(2,30);
      for (Int i=0; i<100000; i++) {
	smartMedian(arr(blc,trc));
      }
    }
    timer.show();
    timer.mark();
    {
      IPosition blc(2,0);
      IPosition trc(2,1,31*31-1);
      for (Int i=0; i<100000; i++) {
	median(arr(blc,trc));
      }
    }
    timer.show();
    timer.mark();
    {
      IPosition blc(2,0);
      IPosition trc(2,1,31*31-1);
      for (Int i=0; i<100000; i++) {
	smartMedian(arr(blc,trc));
      }
    }
    timer.show();
    timer.mark();
    {
      IPosition blc(2,0);
      IPosition trc(2,31*31-1,1);
      for (Int i=0; i<100000; i++) {
	median(arr(blc,trc));
      }
    }
    timer.show();
    timer.mark();
    {
      IPosition blc(2,0);
      IPosition trc(2,31*31-1,1);
      for (Int i=0; i<100000; i++) {
	smartMedian(arr(blc,trc));
      }
    }
    timer.show();
  }
}

void doItMasked (Bool)
{
  {
    IPosition shape(2,5,5);
    Array<Float> darr(shape);
    indgen(darr);
    MaskedArray<Float> arr = darr(darr<=float(10) || darr>float(14));
    cout << arr.getMask() << endl;
    cout << sum(arr) << endl;
    cout << slidingArrayMath(arr, IPosition(2,2), casa::sum, False) << endl;
    cout << slidingArrayMath(arr, IPosition(2,1), casa::sum, False) << endl;

    cout << slidingArrayMath(arr, IPosition(4,2), casa::sum, True) << endl;
    cout << slidingArrayMath(arr, IPosition(3,1), casa::sum, True) << endl;

    cout << slidingArrayMath(arr, IPosition(2,2), casa::median, False) << endl;
    cout << slidingArrayMath(arr, IPosition(2,1), casa::median, False) << endl;
  }
}

int main(int argc, const char*[])
{
  try {
    doIt (argc>1);
    doItMasked (argc>1);
  } catch (exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
