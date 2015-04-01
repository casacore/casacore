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
//#
//# $Id$

#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>

using namespace casacore;

void test1()
{
  Vector<Int> vec(10);
  indgen (vec);
  {
    VectorSTLIterator<Int> iter(vec);
    for (int i=0; i<int(vec.size()); ++i) {
      AlwaysAssertExit (*iter == i);
      AlwaysAssertExit (iter[i] == i);
      ++iter;
    }
    iter -= 10;
    AlwaysAssertExit (*iter == 0);
    iter += 4;
    AlwaysAssertExit (*iter == 4);
    AlwaysAssertExit (*(iter-4) == 0);
    AlwaysAssertExit (*(iter+3) == 7);
    AlwaysAssertExit (iter - VectorSTLIterator<Int>(vec) == 4);
  }
  {
    VectorSTLIterator<Int> iter(vec);
    iter += vec.size();
    for (int i=0; i<int(vec.size()); ++i) {
      --iter;
      AlwaysAssertExit (*iter == int(vec.size()) - i - 1);
      AlwaysAssertExit (iter[i] == i);
    }
  }
  {
    // Uses ArraySTLIterator, not VectorSTLIterator.
    int i=0;
    for (Vector<Int>::const_iterator iter=vec.begin(); iter!=vec.end(); ++iter){
      AlwaysAssertExit (*iter == i);
      ++i;
    }
  }
}

int main()
{
  try {
    test1();
  } catch (AipsError x) {
    cout << "\nCaught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
