//# tWCUnion.cc: Test program for class WCUnion
//# Copyright (C) 1998
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

#include <trial/Images/WCUnion.h>
#include <trial/Images/WCBox.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Lattices/LCRegion.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


void doIt()
{
    CoordinateSystem cSys (CoordinateUtil::defaultCoords3D());
    {
      Vector<Quantum<Double> > blc(2);
      Vector<Quantum<Double> > trc(2);
      blc(0) = Quantum<Double> (10.0, "deg");
      blc(1) = Quantum<Double> (1.0, "deg");
      trc(0) = Quantum<Double> (14.0, "deg");
      trc(1) = Quantum<Double> (3.0, "deg");
      WCBox box1(blc, trc, cSys);
      {
	LCRegion* regptr = box1.toLCRegion (cSys, IPosition(3,30,40,50));
	const Slicer& bbox = regptr->boundingBox();
	cout << bbox.start() << bbox.end() << endl;
	delete regptr;
      }
      Vector<uInt> axes(2);
      axes(0) = 1;
      axes(1) = 0;
      blc(1) = Quantum<Double> (2.0, "deg");
      trc(0) = Quantum<Double> (12.0, "deg");
      WCBox box2(blc, trc, axes, axes, cSys);
      {
	LCRegion* regptr = box2.toLCRegion (cSys, IPosition(3,30,40,50));
	const Slicer& bbox = regptr->boundingBox();
	cout << bbox.start() << bbox.end() << endl;
	delete regptr;
      }
      axes(0) = 2;
      axes(1) = 0;
      blc(0) = Quantum<Double> (10.0, "Hz");
      trc(0) = Quantum<Double> (14.0, "Hz");
      {
	WCBox box(blc, trc, axes, axes, cSys);
	LCRegion* regptr = box.toLCRegion (cSys, IPosition(3,30,40,50));
	const Slicer& bbox = regptr->boundingBox();
	cout << bbox.start() << bbox.end() << endl;
	delete regptr;
      }

      WCUnion union1(box1, box2);
      {
	LCRegion* regptr = union1.toLCRegion (cSys, IPosition(3,30,40,50));
	const Slicer& bbox = regptr->boundingBox();
	cout << bbox.start() << bbox.end() << endl;
	delete regptr;
      }
    }
    {
      Vector<Quantum<Double> > blc(1);
      Vector<Quantum<Double> > trc(1);
      blc(0) = Quantum<Double> (10.0, "deg");
      trc(0) = Quantum<Double> (14.0, "deg");
      {
	WCBox box1(blc, trc, cSys);
	Vector<uInt> axes(1);
	axes(0) = 1;
	WCBox box2(blc, trc, axes, axes, cSys);
	ImageRegion ir1(box1);
	ImageRegion ir2(box2);
	WCUnion union1 (&ir1, &ir2);
	LCRegion* regptr = union1.toLCRegion (cSys, IPosition(3,30,40,50));
	const Slicer& bbox = regptr->boundingBox();
	cout << bbox.start() << bbox.end() << endl;
	delete regptr;
      }
    }
}


main ()
{
  try {
    doIt();
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;

  cout << "OK" << endl;
  return 0;
}
