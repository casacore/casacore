//# tStdLogical.cc: Test program for functions in StdLogical.h
//# Copyright (C) 2012
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

#include <casa/BasicMath/StdLogical.h>

#include <casa/Utilities/Assert.h>

#include <set>

using namespace casa;

int main()
{
  try {
    std::set<Float> a;
    std::set<Float> b;

    Float a1[] = {0.1, 5.2, 6.3};
    Float b1[] = {0.5};

    a.insert(a1, a1+3);
    b.insert(b1, b1+1);
    AlwaysAssert(! allNearAbs(a, b, 0.1), AipsError);

    b.clear();
    Float b2[] = {0.2, 5.1, 6.4};
    b.insert(b2, b2+3);
    AlwaysAssert(! allNearAbs(a, b, 0.05), AipsError);
    AlwaysAssert(allNearAbs(a, b, 0.11), AipsError);

    AlwaysAssert(compareAll(a.begin(), a.end(), a1, NearAbs<Float>(0.1)),
                 AipsError);

    cout << "OK" << endl;
    return 0;
  }
  catch (const AipsError& exc) {
    cout << "FAIL" << endl;
    return 1;
  }

}
