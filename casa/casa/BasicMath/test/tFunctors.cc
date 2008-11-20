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

#include <casa/aips.h>
#include <casa/BasicMath/Functors.h>
#include <casa/OS/Timer.h>
#include <casa/Exceptions/Error.h>
#include <casa/Utilities/Assert.h>
#include <vector>


using namespace casa;
using namespace std;

int main()
{
  try {
    vector<double> v1(10, 1.);
    vector<double> v2(10, 0.5);
    vector<double> res(10);
    vector<bool>   reb(10);
    std::transform (v1.begin(), v1.end(), res.begin(), Sin<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Sinh<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Asin<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Cos<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Cosh<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Acos<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Tan<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Tanh<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Atan<double>());
    std::transform (v1.begin(), v1.end(), v2.begin(), res.begin(), Atan2<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Sqr<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Sqrt<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Exp<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Log<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Log10<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Abs<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Floor<double>());
    std::transform (v1.begin(), v1.end(), res.begin(), Ceil<double>());
    std::transform (v1.begin(), v1.end(), v2.begin(), res.begin(), Pow<double>());
    std::transform (v1.begin(), v1.end(), v2.begin(), res.begin(), Fmod<double>());
    std::transform (v1.begin(), v1.end(), reb.begin(), IsNaN<double>());
    std::transform (v1.begin(), v1.end(), reb.begin(), IsInf<double>());
    std::transform (v1.begin(), v1.end(), v2.begin(), reb.begin(), Near<double>());
    std::transform (v1.begin(), v1.end(), v2.begin(), reb.begin(), NearAbs<double>());
  }
  catch (AipsError& x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
