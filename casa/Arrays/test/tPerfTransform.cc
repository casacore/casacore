//# tTransform.cc: Compare the performance of various ways to transform an array
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
#include <casacore/casa/BasicMath/Functors.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <numeric>
#include <functional>

using namespace casacore;



int main (int argc, char* argv[])
{
  int nloop = 1;
  if (argc > 1) {
    istringstream istr(argv[1]);
    istr >> nloop;
  }
  Array<float> arr1(IPosition(3,100,100,100));
  Array<float> arr2(IPosition(3,100,100,100));
  Array<float> res(arr1.shape());
  indgen (arr1);
  indgen (arr2);
  std::vector<float> vec1(arr1.cbegin(), arr1.cend());
  std::vector<float> vec2(arr2.cbegin(), arr2.cend());
  std::vector<float> resv(vec1.size());
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      Array<float> res1 (arrayTransformResult(arr1, arr2, std::plus<float>()));
    }
    timer.show(std::cout, "Arraya");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      Array<float> res1 = arrayTransformResult(arr1, float(2), std::plus<float>());
    }
    timer.show(std::cout, "Arrays");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      std::transform(arr1.cbegin(), arr1.cend(), arr2.cbegin(),
                     res.cbegin(), std::plus<float>());
    }
    timer.show(std::cout, "cbegin");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      std::transform(vec1.begin(), vec1.end(), vec2.begin(),
                     resv.begin(), std::plus<float>());
    }
    timer.show(std::cout, "vector");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      std::transform(arr1.begin(), arr1.end(), arr2.begin(),
                     res.cbegin(), std::plus<float>());
    }
    timer.show(std::cout, "begin ");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      std::transform(arr1.cbegin(), arr1.cend(), arr2.cbegin(),
                     arr1.cbegin(), std::plus<float>());
    }
    timer.show(std::cout, "inarrc");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      std::transform(vec1.begin(), vec1.end(), vec2.begin(),
                     vec1.begin(), std::plus<float>());
    }
    timer.show(std::cout, "invec ");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      arrayTransformInPlace(arr1, arr2, std::plus<float>());
    }
    timer.show(std::cout, "inplac");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      arr1 += arr2;
    }
    timer.show(std::cout, "arr+=a");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      arr1 += float(2);
    }
    timer.show(std::cout, "arr+=s");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      transformInPlace (arr1.cbegin(), arr1.cend(),
                        bind2nd(std::plus<float>(), float(2)));
    }
    timer.show(std::cout, "arr+=s");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      transformInPlace(arr1.begin(), arr1.end(), arr2.begin(),
                       std::plus<float>());
    }
    timer.show(std::cout, "inplan");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      std::transform(arr1.begin(), arr1.end(), arr2.begin(),
                     arr1.begin(), std::plus<float>());
    }
    timer.show(std::cout, "inarrn");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      float* p1 = arr1.data();
      float* p2 = arr2.data();
      float* r = res.data();
      uInt sz = arr1.size();
      for (uInt i=0; i<sz; ++i) {
        r[i] = p1[i] + p2[i];
      }
    }
    timer.show(std::cout, "index ");
  }
  {
    Timer timer;
    for (int j=0; j<nloop; ++j) {
      float* p1 = arr1.data();
      float* p2 = arr2.data();
      float* r = res.data();
      float* pend = p1 + arr1.size();
      while (p1 != pend) {
        *r++ = *p1++ + *p2++;
      }
    }
    timer.show(std::cout, "ptr   ");
  }
}
