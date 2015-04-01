//# tExtendSpecifier.cc: Test program for class ExtendSpecifier
//# Copyright (C) 2001
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
//#
//# $Id$

#include <casacore/casa/Arrays/ExtendSpecifier.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void doIt()
{
  {
    IPosition oldShape(4,10,1,3,1);
    IPosition newShape(5,10,1,5,3,8);
    ExtendSpecifier spec (oldShape, newShape, IPosition(1,2), IPosition(1,4));
    cout << "oldShape    " << spec.oldShape() << endl;
    cout << "newShape    " << spec.newShape() << endl;
    cout << "newAxes     " << spec.newAxes() << endl;
    cout << "stretchAxes " << spec.stretchAxes() << endl;
    cout << "extendAxes  " << spec.extendAxes() << endl;
    cout << "oldOldAxes  " << spec.oldOldAxes() << endl;
    cout << "oldNewAxes  " << spec.oldNewAxes() << endl;

    IPosition sh(4,3,4,5,6);
    cout << sh << " -> " << spec.convertNew(sh) << endl;

    Slicer sl(IPosition(5,0,1,2,3,4), IPosition(5,1,2,3,4,5));
    cout << "Slicer " << sl.start() << ' ' << sl.length() << ' '
	 << sl.stride() << endl;
    IPosition shp;
    Slicer sln = spec.convert (shp, sl);
    cout << "    -> " << sln.start() << ' ' << sln.length() << ' '
	 << sln.stride() << "  shp: " << shp << endl;
  }
  {
    IPosition oldShape(4,10,1,1,3);
    IPosition newShape(5,10,1,5,3,8);
    ExtendSpecifier spec (oldShape, newShape, IPosition(1,4), IPosition(1,2));
    cout << "oldShape    " << spec.oldShape() << endl;
    cout << "newShape    " << spec.newShape() << endl;
    cout << "newAxes     " << spec.newAxes() << endl;
    cout << "stretchAxes " << spec.stretchAxes() << endl;
    cout << "extendAxes  " << spec.extendAxes() << endl;
    cout << "oldOldAxes  " << spec.oldOldAxes() << endl;
    cout << "oldNewAxes  " << spec.oldNewAxes() << endl;

    IPosition sh(4,3,4,5,6);
    cout << sh << " -> " << spec.convertNew(sh) << endl;

    Slicer sl(IPosition(5,0,1,2,3,4), IPosition(5,1,2,3,4,5));
    cout << "Slicer " << sl.start() << ' ' << sl.length() << ' '
	 << sl.stride() << endl;
    IPosition shp;
    Slicer sln = spec.convert (shp, sl);
    cout << "    -> " << sln.start() << ' ' << sln.length() << ' '
	 << sln.stride() << "  shp: " << shp << endl;
  }
  // Do some erronous constructions.
  try {
    ExtendSpecifier spec (IPosition(1,1), IPosition(1,1),
			  IPosition(), IPosition());
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // new nor stretch axes
  }
  try {
    ExtendSpecifier spec (IPosition(1,1), IPosition(1,1),
			  IPosition(), IPosition(1,0));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // no axes remaining
  }
  try {
    ExtendSpecifier spec (IPosition(2,2,1), IPosition(2,1,1),
			  IPosition(), IPosition(1,0));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // length stretch axis > 1
  }
  try {
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,3),
			  IPosition(), IPosition(1,0));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // lengths old axis mismatch
  }
  try {
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(), IPosition(2,0,0));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // axes multiply given
  }
  try {
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(1,0), IPosition(1,0));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // axes multiply given
  }
  try {
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(1,0), IPosition());
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // new shape 1 element too short
  }
  try {
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(), IPosition(1,-1));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // invalid axis
  }
  try {
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(), IPosition(1,2));
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;     // invalid axis
  }
}


int main()
{
  try {
    doIt();
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  return 0;
}
