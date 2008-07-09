//# tCompareBoxedPartial.cc: Compare performance of partialSums and boxedArrayMath
//# Copyright (C) 2008
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

#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/BasicSL/Complex.h>
#include <casa/OS/Timer.h>

using namespace casa;
using namespace std;

template<class T>
void doIt()
{
  Array<T> arr(IPosition(3,100,200,1000));
  arr=1;
  Timer timer;
  {
    timer.mark();
    Array<T> res = boxedArrayMath(arr, IPosition(3,100,1,1), &sum);
    timer.show ("boxed 0  ");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = partialSums (arr, IPosition(1,0));
    timer.show ("partial 0");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = boxedArrayMath(arr, IPosition(3,1,200,1), &sum);
    timer.show ("boxed 1  ");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = partialSums (arr, IPosition(1,1));
    timer.show ("partial 1");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = boxedArrayMath(arr, IPosition(3,1,1,1000), &sum);
    timer.show ("boxed 2  ");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = partialSums (arr, IPosition(1,2));
    timer.show ("partial 2");
    cout << res.shape() << endl;
  }
}

int main()
{
  std::cout << "test int ..." << std::endl;
  doIt<int>();
  std::cout << "test float ..." << std::endl;
  doIt<float>();
  std::cout << "test DComplex ..." << std::endl;
  doIt<DComplex>();
}

/*
Test remarks on MacBook OS-X Tiger g++-4.01. -O2:
1. partial is faster than boxed, especially for axis 0 (1.5x),
   axis 1 (3x), and axis 2 (4x).
*/
