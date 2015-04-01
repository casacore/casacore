//# tFunctors.cc:
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

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Functors.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <vector>
#include <algorithm>


using namespace casacore;
using namespace std;

#define TESTFUNCTOR1(NAME, FUNC) \
  { \
    std::transform (v1.begin(), v1.end(), res.begin(), NAME<double>()); \
    for (uInt i=0; i<res.size(); ++i) { \
      AlwaysAssertExit (near(res[i], FUNC(v1[i])));     \
    } \
    res = v1; \
    transformInPlace (res.begin(), res.end(), NAME<double>()); \
    for (uInt i=0; i<res.size(); ++i) { \
      AlwaysAssertExit (near(res[i], FUNC(v1[i])));     \
    } \
  }

#define TESTFUNCTOR2(NAME, FUNC) \
  { \
    std::transform (v1.begin(), v1.end(), v2.begin(), res.begin(), NAME<double>()); \
    for (uInt i=0; i<res.size(); ++i) { \
      AlwaysAssertExit (near(res[i], FUNC(v1[i], v2[i])));      \
    } \
    res = v1; \
    transformInPlace (res.begin(), res.end(), v2.begin(), NAME<double>()); \
    for (uInt i=0; i<res.size(); ++i) { \
      AlwaysAssertExit (near(res[i], FUNC(v1[i], v2[i])));      \
    } \
  }

#define TESTFUNCTORB1(NAME, FUNC) \
  { \
    std::transform (v1.begin(), v1.end(), reb.begin(), NAME<double>()); \
    for (uInt i=0; i<reb.size(); ++i) { \
      AlwaysAssertExit (reb[i] == FUNC(v1[i])); \
    } \
  }

#define TESTFUNCTORB2(NAME, FUNC) \
  { \
    std::transform (v1.begin(), v1.end(), v2.begin(), reb.begin(), NAME<double>()); \
    for (uInt i=0; i<reb.size(); ++i) { \
      AlwaysAssertExit (reb[i] == FUNC(v1[i], v2[i])); \
    } \
  }


double sqr (double r)
  { return r*r; }
double sumsqr (double l, double r)
  { return l + r*r; }


int main()
{
  try {
    vector<double> v1(10);
    vector<double> v2(10);
    vector<double> res(10);
    vector<bool>   reb(10);
    for (uInt i=0; i<v1.size(); ++i) {
      v1[i] = (i+1)*0.05;
      v2[i] = (i+3)*0.025;
    }
    TESTFUNCTOR1 (Sin, sin)
    TESTFUNCTOR1 (Sinh, sinh)
    TESTFUNCTOR1 (Asin, asin)
    TESTFUNCTOR1 (Cos, cos)
    TESTFUNCTOR1 (Cosh, cosh)
    TESTFUNCTOR1 (Acos, acos)
    TESTFUNCTOR1 (Tan, tan)
    TESTFUNCTOR1 (Tanh, tanh)
    TESTFUNCTOR1 (Atan, atan)
    TESTFUNCTOR2 (Atan2, atan2)
    TESTFUNCTOR1 (Sqr, sqr)
    TESTFUNCTOR1 (Sqrt, sqrt)
    TESTFUNCTOR1 (Exp, exp)
    TESTFUNCTOR1 (Log, log)
    TESTFUNCTOR1 (Log10, log10)
    TESTFUNCTOR1 (Abs, std::abs)
    TESTFUNCTOR1 (Floor, floor)
    TESTFUNCTOR1 (Ceil, ceil)
    TESTFUNCTOR2 (Pow, pow)
    TESTFUNCTOR2 (Fmod, fmod)
    TESTFUNCTORB1 (IsNaN, isNaN)
    TESTFUNCTORB1 (IsInf, isInf)
    TESTFUNCTORB2 (Near, near)
    TESTFUNCTORB2 (NearAbs, nearAbs)
    TESTFUNCTOR2 (SumSqr, sumsqr)
    TESTFUNCTOR2 (Min, std::min)
    TESTFUNCTOR2 (Max, std::max)

    { 
      std::transform (v1.begin(), v1.end(), v2.begin(), res.begin(), SumSqrDiff<double>(0.25));
      for (uInt i=0; i<res.size(); ++i) {
        AlwaysAssertExit (near(res[i], v1[i] + (v2[i]-0.25)*(v2[i]-0.25)));
      }
  }
  } catch (AipsError& x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
