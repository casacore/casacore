//# tWCExtension.cc: Test program for class WCExtension
//# Copyright (C) 2001
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

#include <casacore/images/Regions/WCExtension.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/images/Regions/WCPolygon.h>

#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt()
{
  // Create a dummy box to make the special units known to UnitMap.
  WCBox dummy;
  CoordinateSystem cSys (CoordinateUtil::defaultCoords3D());

  // Test for region that only gets stretched.
  {
    Vector<Int> absRel(3);
    absRel = RegionType::Abs;
    Vector<Quantum<Double> > blc(3);
    Vector<Quantum<Double> > trc(3);
    IPosition axes(3);
    axes(0) = 1;
    axes(1) = 2;
    axes(2) = 0;
    blc(0) = Quantum<Double> (1.0, "pix");
    blc(1) = Quantum<Double> (1.0, "pix");
    blc(2) = Quantum<Double> (10.0, "pix");
    trc(0) = Quantum<Double> (1.0, "pix");
    trc(1) = Quantum<Double> (1.0, "pix");
    trc(2) = Quantum<Double> (14.0, "pix");
    WCBox box(blc, trc, axes, cSys, absRel);
    {
      LCRegion* regptr = box.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    Vector<Int> absRel2(2);
    absRel2 = RegionType::Abs;
    Vector<Quantum<Double> > blc2(2);
    Vector<Quantum<Double> > trc2(2);
    IPosition axes2(2);
    axes2(0) = 1;
    axes2(1) = 2;
    blc2(0) = Quantum<Double> (3.0, "pix");
    blc2(1) = Quantum<Double> (5.0, "pix");
    trc2(0) = Quantum<Double> (10.0, "pix");
    trc2(1) = Quantum<Double> (15.0, "pix");
    WCBox sbox(blc2, trc2, axes2, cSys, absRel2);
    {
      LCRegion* regptr = sbox.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    WCExtension wcs1(box, sbox);
    {
      LCRegion* regptr = wcs1.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
  }

  // Test for region that only gets extended.
  {
    Vector<Int> absRel(1);
    absRel = RegionType::Abs;
    Vector<Quantum<Double> > blc(1);
    Vector<Quantum<Double> > trc(1);
    IPosition axes(1);
    axes(0) = 1;
    blc(0) = Quantum<Double> (1.0, "pix");
    trc(0) = Quantum<Double> (5.0, "pix");
    WCBox box(blc, trc, axes, cSys, absRel);
    {
      LCRegion* regptr = box.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    Vector<Int> absRel2(2);
    absRel2 = RegionType::Abs;
    Vector<Quantum<Double> > blc2(2);
    Vector<Quantum<Double> > trc2(2);
    IPosition axes2(2);
    axes2(0) = 2;
    axes2(1) = 0;
    blc2(0) = Quantum<Double> (3.0, "pix");
    blc2(1) = Quantum<Double> (5.0, "pix");
    trc2(0) = Quantum<Double> (10.0, "pix");
    trc2(1) = Quantum<Double> (15.0, "pix");
    WCBox sbox(blc2, trc2, axes2, cSys, absRel2);
    {
      LCRegion* regptr = sbox.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    WCExtension wcs1(box, sbox);
    {
      LCRegion* regptr = wcs1.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
  }

  // Test for region that gets stretched and extended.
  {
    Vector<Int> absRel(2);
    absRel = RegionType::Abs;
    Vector<Quantum<Double> > blc(2);
    Vector<Quantum<Double> > trc(2);
    IPosition axes(2);
    axes(0) = 2;
    axes(1) = 0;
    blc(0) = Quantum<Double> (1.0, "pix");
    blc(1) = Quantum<Double> (2.0, "pix");
    trc(0) = Quantum<Double> (1.0, "pix");
    trc(1) = Quantum<Double> (8.0, "pix");
    WCBox box(blc, trc, axes, cSys, absRel);
    {
      LCRegion* regptr = box.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    Vector<Int> absRel2(2);
    absRel2 = RegionType::Abs;
    Vector<Quantum<Double> > blc2(2);
    Vector<Quantum<Double> > trc2(2);
    IPosition axes2(2);
    axes2(0) = 1;
    axes2(1) = 2;
    blc2(0) = Quantum<Double> (3.0, "pix");
    blc2(1) = Quantum<Double> (5.0, "pix");
    trc2(0) = Quantum<Double> (10.0, "pix");
    trc2(1) = Quantum<Double> (15.0, "pix");
    WCBox sbox(blc2, trc2, axes2, cSys, absRel2);
    {
      LCRegion* regptr = sbox.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    WCExtension wcs1(box, sbox);
    {
      LCRegion* regptr = wcs1.toLCRegion (cSys, IPosition(3,30,40,50));
      const Slicer& bbox = regptr->boundingBox();
      cout << bbox.start() << bbox.end() << endl;
      delete regptr;
    }
    Vector<Double> x(3);
    Vector<Double> y(3);
    x[0] = 3;
    x[1] = 3;
    x[2] = 8;
    y[0] = 3;
    y[1] = 8;
    y[2] = 3;
    Quantum<Vector<Double> > xq(x);
    xq.setUnit("pix");
    Quantum<Vector<Double> > yq(y);
    yq.setUnit("pix");
    IPosition pixelAxes(2);
    pixelAxes[0] = 0;
    pixelAxes[1] = 1;

    WCPolygon poly(xq, yq, pixelAxes, cSys);
    IPosition axes3(1,2);
    Vector<Quantum<Double> > blc3(1);
    Vector<Quantum<Double> > trc3(1);
    blc3(0) = Quantum<Double> (3.0, "pix");
    trc3(0) = Quantum<Double> (10.0, "pix");
    Vector<Int> absRel3(1, RegionType::Abs);
    WCBox ebox(blc3, trc3, axes3, cSys, absRel3);
    WCExtension wcspoly(poly, ebox);

    {
      LCRegion* regptr = wcspoly.toLCRegion (cSys, IPosition(3,30,40,50));
      Array<Bool> mask = regptr->get();
      IPosition shape = mask.shape();
      for (Int k=0; k<shape[2]; k++) {
    	  for (Int j=shape[1]-1; j>=0; j--) {
    		  for (Int i=0; i< shape[0]; i++) {
    			  cout << regptr->getAt(IPosition(3,i,j,k)) << " ";
    		  }
    		  cout << endl;
    	  }
    	  cout << endl;
      }
      delete regptr;

    }
  }
}

Int inIPos (Int val, const IPosition& ipos)
{
  for (uInt i=0; i<ipos.nelements(); i++) {
    if (val == ipos(i)) {
      return i;
    }
  }
  return -1;
}

void testRegionBox (const CoordinateSystem& cSys, 
		    const IPosition& regAxes, const IPosition& boxAxes)
{
  // Define: shape of the total lattice
  //         blc and trc for possible extend axes
  //         blc and trc for possible stretch axes
  IPosition latShape(4,30,40,4,60);
  IPosition regBlc(4, 1, 2, 2, 4);
  IPosition extBlc(4, 3, 4, 0, 6);
  IPosition extTrc(4, 5, 7, 1, 11);
  IPosition strBlc(4, 12, 13, 1, 15);
  IPosition strTrc(4, 14, 18, 3, 22);
  // Create a region (which is a box) for the given region axes.
  // All axes have length 1 in it (so they can all be stretched).
  // First create a temporary box with axes in normal order.
  // Create the final box from it with required axes in required order.
  Vector<Int> absRel(4);
  absRel = RegionType::Abs;
  Vector<Quantum<Double> > blc(4);
  Vector<Quantum<Double> > trc(4);
  blc(0) = Quantum<Double> (regBlc(0), "pix");
  blc(1) = Quantum<Double> (regBlc(1), "pix");
  blc(2) = Quantum<Double> (regBlc(2), "pix");
  blc(3) = Quantum<Double> (regBlc(3), "pix");
  trc(0) = blc(0);
  trc(1) = blc(1);
  trc(2) = blc(2);
  trc(3) = blc(3);
  WCBox reg1(blc, trc, cSys, absRel);
  WCBox region (reg1.splitBox (regAxes));
  // Set up blc and trc which get the extend box shape.
  uInt ndbox = boxAxes.nelements();
  IPosition boxBlc(ndbox);
  IPosition boxTrc(ndbox);
  for (uInt i=0; i<ndbox; i++) {
    Int axis = boxAxes(i);
    if (inIPos(axis, regAxes) >= 0) {
      boxBlc(i) = strBlc(axis);
      boxTrc(i) = strTrc(axis);
    } else {
      boxBlc(i) = extBlc(axis);
      boxTrc(i) = extTrc(axis);
    }
    blc(axis) = Quantum<Double> (boxBlc(i), "pix");
    trc(axis) = Quantum<Double> (boxTrc(i), "pix");
  }
  // Also fill in the expected blc and trc.
  IPosition resBlc(4, 0, 0, 0, 0);
  IPosition resTrc (latShape-1);
  for (Int i=0; i<4; i++) {
    Int axis = inIPos(i, boxAxes);
    if (axis >= 0) {
      resBlc(i) = boxBlc(axis);
      resTrc(i) = boxTrc(axis);
    } else if (inIPos(i, regAxes) >= 0) {
      resBlc(i) = regBlc(i);
      resTrc(i) = regBlc(i);
    }
  }
  // Make an extension of region and box.
  WCBox box1(blc, trc, cSys, absRel);
  WCBox box (box1.splitBox (boxAxes));
  WCExtension wcext(region, box);
  // Now convert to region to an LCRegion and check the boundary box.
  LCRegion* regptr = wcext.toLCRegion (cSys, latShape);
  const Slicer& bbox = regptr->boundingBox();
  if (! resBlc.isEqual (bbox.start())) {
    cout << regAxes << boxAxes
	 << " Expected blc " << resBlc << ", found " << bbox.start() << endl;
  }
  if (! resTrc.isEqual (bbox.end())) {
    cout << regAxes << boxAxes
	 << " Expected trc " << resTrc << ", found " << bbox.end() << endl;
  }
  delete regptr;
}

void testRegion (const CoordinateSystem& cSys, const IPosition& regAxes)
{
  // Now test the region with all possible extend boxes.
  for (Int i0=0; i0<4; i0++) {
    testRegionBox (cSys, regAxes, IPosition(1,i0));
    for (Int i1=0; i1<4; i1++) {
      if (i1 != i0) {
	testRegionBox (cSys, regAxes, IPosition(2,i0,i1));
	for (Int i2=0; i2<4; i2++) {
	  if (i2 != i0  &&  i2 != i1) {
	    testRegionBox (cSys, regAxes, IPosition(3,i0,i1,i2));
	    for (Int i3=0; i3<4; i3++) {
	      if (i3 != i0  &&  i3 != i1  &&  i3 != i2) {
		testRegionBox (cSys, regAxes, IPosition(4,i0,i1,i2,i3));
	      }
	    }
	  }
	}
      }
    }
  }
}

void testAll()
{
  CoordinateSystem cSys (CoordinateUtil::defaultCoords4D());
  // Test all possible 1,2,3,4D regions (with axes in all possible orders).
  for (Int i0=0; i0<4; i0++) {
    testRegion (cSys, IPosition(1,i0));
    for (Int i1=0; i1<4; i1++) {
      if (i1 != i0) {
	testRegion (cSys, IPosition(2,i0,i1));
	for (Int i2=0; i2<4; i2++) {
	  if (i2 != i0  &&  i2 != i1) {
	    testRegion (cSys, IPosition(3,i0,i1,i2));
	    for (Int i3=0; i3<4; i3++) {
	      if (i3 != i0  &&  i3 != i1  &&  i3 != i2) {
		testRegion (cSys, IPosition(4,i0,i1,i2,i3));
	      }
	    }
	  }
	}
      }
    }
  }
}

int main ()
{
  try {
    doIt();
    testAll();
  } catch (AipsError x) {
    cout << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
