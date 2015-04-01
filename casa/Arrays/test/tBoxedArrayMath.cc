//# tBoxedArrayMath.cc: Test program for function boxedArrayMath
//# Copyright (C) 2008
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

#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/MaskArrIO.h>

#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/OS/Timer.h>
#include <iostream>


using namespace casacore;
using namespace std;

void doIt (Bool /*doTiming*/)
{
  {
    IPosition shape(2,5,5);
    Array<Float> arr(shape);
    indgen (arr);
    cout << boxedArrayMath(arr, IPosition(2,2), SumFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(2,1), SumFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(1,0), SumFunc<Float>()) << endl;

    cout << boxedArrayMath(arr, IPosition(4,2), SumFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(3,1), SumFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(),    SumFunc<Float>()) << endl;

    cout << boxedArrayMath(arr, IPosition(2,2), MedianFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(2,1), MedianFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(2,0), MedianFunc<Float>()) << endl;
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
    cout << boxedArrayMath(arr, IPosition(2,2), MaskedSumFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(2,1), MaskedSumFunc<Float>()) << endl;

    cout << boxedArrayMath(arr, IPosition(4,2), MaskedSumFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(3,1), MaskedSumFunc<Float>()) << endl;

    cout << boxedArrayMath(arr, IPosition(2,2), MaskedMedianFunc<Float>()) << endl;
    cout << boxedArrayMath(arr, IPosition(2,1), MaskedMedianFunc<Float>()) << endl;
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
