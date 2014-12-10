//# tStringDistance.cc: Test of class StringDistance
//# Copyright (C) 2009
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

#include <casacore/casa/Utilities/StringDistance.h>
#include <iostream>
#include <stdexcept>
#include <casacore/casa/Arrays/ArrayIO.h>

using namespace std;
using namespace casacore;

int main(int argc, char* argv[2])
{
  try {
    if (argc < 3) {
      cout << "Run as: tStringDistance source target [countSwap] "
           << "[ignoreBlanks] [caseInsensitive]"
           << endl;
      return 1;
    }
    Bool countSwap       = (argc > 3  &&  *argv[3] == '1');
    Bool ignoreBlanks    = (argc > 4  &&  *argv[4] == '1');
    Bool caseInsensitive = (argc > 5  &&  *argv[5] == '1');
    StringDistance dist(argv[1], -1, countSwap, ignoreBlanks, caseInsensitive);
    cout << dist.source() << ' ' << dist.maxDistance() << ": match="
         << dist.match(argv[2]) << " dist=" << dist.distance(argv[2])
         << " distlr=" << StringDistance::distance(argv[1], argv[2], countSwap)
         << " distrl=" << StringDistance::distance(argv[2], argv[1], countSwap)
         << endl;
    cout << dist.matrix();
  } catch (exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
