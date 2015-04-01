//# tSlice.cc: Test program for class Slice.
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

#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>

using namespace casacore;

int main()
{
  Slicer first;
  IPosition shape(3,100,110,120);
  {
    Vector<Vector<Slice> > slices;
    IPosition shp = Slice::checkSlices (slices, first, shape);
    AlwaysAssertExit (slices.size() == 3);
    AlwaysAssertExit (shp == shape);
    AlwaysAssertExit (first.start() == IPosition(3,0));
    AlwaysAssertExit (first.length() == shape);
    AlwaysAssertExit (first.stride() == IPosition(3,1));
  }
  {
    Vector<Vector<Slice> > slices(3);
    IPosition shp = Slice::checkSlices (slices, first, shape);
    AlwaysAssertExit (slices.size() == 3);
    AlwaysAssertExit (shp == shape);
    AlwaysAssertExit (first.start() == IPosition(3,0));
    AlwaysAssertExit (first.length() == shape);
    AlwaysAssertExit (first.stride() == IPosition(3,1));
  }
  {
    Vector<Vector<Slice> > slices(2);
    slices[0].resize(2);
    slices[0][0] = Slice(20,10);
    slices[0][1] = Slice(25,20);
    slices[1].resize(3);
    slices[1][0] = Slice(22,12,2);
    slices[1][1] = Slice(20,10);
    slices[1][2] = Slice(34,10,2);
    IPosition shp = Slice::checkSlices (slices, first, shape);
    AlwaysAssertExit (slices.size() == 3);
    AlwaysAssertExit (shp == IPosition(3,30,32,shape[2]));
    AlwaysAssertExit (first.start() == IPosition(3,20,22,0));
    AlwaysAssertExit (first.length() == IPosition(3,10,12,shape[2]));
    AlwaysAssertExit (first.stride() == IPosition(3,1,2,1));
  }
  {
    Slice slice(3,10,5);
    AlwaysAssertExit (slice.start() == 3);
    AlwaysAssertExit (slice.length() == 10);
    AlwaysAssertExit (slice.end() == 48);
    AlwaysAssertExit (slice.inc() == 5);
  }
  {
    Slice slice(3,48,5, False);
    AlwaysAssertExit (slice.start() == 3);
    AlwaysAssertExit (slice.length() == 10);
    AlwaysAssertExit (slice.end() == 48);
    AlwaysAssertExit (slice.inc() == 5);
  }
  {
    Slice slice(2,10,5, False);
    AlwaysAssertExit (slice.start() == 2);
    AlwaysAssertExit (slice.length() == 2);
    AlwaysAssertExit (slice.end() == 7);
    AlwaysAssertExit (slice.inc() == 5);
  }
}
