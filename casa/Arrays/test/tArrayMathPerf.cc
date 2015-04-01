//# tArrayMathPerf.cc: Performance tests of various ArrayMath operations
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

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <iostream>

using namespace casacore;
using namespace std;


void doTest (int nloop, const Array<float>& arr1, const Array<float>& arr2)
{
  Array<float> res(arr1.shape());
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      res.reference (arr1+arr2);
    }
    timer.show(cout, "arr1+arr2");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      res += arr2;
    }
    timer.show(cout, "res+=arr2");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      res += float(2);
    }
    timer.show(cout, "res+=2   ");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      res.reference (arr1*arr2);
    }
    timer.show(cout, "arr1*arr2");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      res *= arr2;
    }
    timer.show(cout, "res*=arr2");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      res *= float(2);
    }
    timer.show(cout, "res*=2   ");
  }
  float tsum=0;
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      tsum += sum(arr1);
    }
    timer.show(cout, "sum(arr1)");
  }
}


int main (int argc, char* argv[])
{
  int nloop = 1;
  if (argc > 1) {
    istringstream istr(argv[1]);
    istr >> nloop;
  }
  Array<float> arr1(IPosition(3,100,1000,100));
  Array<float> arr2(IPosition(3,100,1000,100));
  indgen (arr1);
  indgen (arr2);
  // Form subsets.
  IPosition s(3,0,1,2);
  IPosition e(arr1.shape()-2);
  Array<float> arr1s (arr1(s,e));
  Array<float> arr2s (arr2(s,e));
  Array<float> arrs(arr1s.shape());
  indgen (arrs);
  // Test the full array.
  cout << "full arr1, full arr2" << endl;
  doTest (nloop, arr1, arr2);
  // Test an array subsets.
  cout << "full arr1, subset arr2" << endl;
  doTest (nloop, arrs, arr2s);
  cout << "subset arr1, full arr2" << endl;
  doTest (nloop, arr1s, arrs);
  cout << "subset arr1, subset arr2" << endl;
  doTest (nloop, arr1s, arr2s);
}
