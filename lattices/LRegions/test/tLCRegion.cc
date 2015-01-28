//# tLCRegion.cc: Test program for derived LCRegion classes.
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCEllipsoid.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (const IPosition& latticeShape,
	   const Vector<Float>& center,
	   const Vector<Float>& radii)
{
    LCEllipsoid cir (center, radii, latticeShape);
    cout << cir.hasMask() << ' ' << cir.maskArray() << endl;
    cout << cir.boundingBox().start() << cir.boundingBox().end()
	 << cir.boundingBox().length() << cir.latticeShape() << endl;
    cout << cir.center() << cir.radii() << endl;
}

void doIt (const IPosition& latticeShape,
	   const IPosition& start,
	   const IPosition& end,
	   const IPosition& center,
	   Float radius)
{
    LCBox box (start, end, latticeShape);
    box.setComment ("com1");
    cout << box.hasMask() << ' ' << box.maskArray() << endl;
    cout << box.boundingBox().start() << box.boundingBox().end()
	 << box.boundingBox().length() << box.latticeShape()
	 << box.comment() << endl;
    LCEllipsoid cir (center, radius, latticeShape);
    cout << cir.hasMask() << ' ' << cir.maskArray() << endl;
    cout << cir.boundingBox().start() << cir.boundingBox().end()
	 << cir.boundingBox().length() << cir.latticeShape() << endl;
    cout << cir.center() << cir.radii() << endl;
    {
	// Test cloning.
	LCRegionFixed* boxcop = (LCRegionFixed*)(box.cloneRegion());
	AlwaysAssertExit (box.hasMask() == boxcop->hasMask());
	AlwaysAssertExit (allEQ (box.maskArray(), boxcop->maskArray()));
	AlwaysAssertExit (box.boundingBox().start() ==
			  boxcop->boundingBox().start());
	AlwaysAssertExit (box.boundingBox().end() ==
			  boxcop->boundingBox().end());
	AlwaysAssertExit (box.boundingBox().stride() ==
			  boxcop->boundingBox().stride());
	AlwaysAssertExit (box.boundingBox().length() ==
			  boxcop->boundingBox().length());
	AlwaysAssertExit (box.comment() == boxcop->comment());
	delete boxcop;
	LCRegionFixed* circop = (LCRegionFixed*)(cir.cloneRegion());
	AlwaysAssertExit (cir.hasMask() == circop->hasMask());
	AlwaysAssertExit (allEQ (cir.maskArray(), circop->maskArray()));
	AlwaysAssertExit (cir.boundingBox().start() ==
			  circop->boundingBox().start());
	AlwaysAssertExit (cir.boundingBox().end() ==
			  circop->boundingBox().end());
	AlwaysAssertExit (cir.boundingBox().stride() ==
			  circop->boundingBox().stride());
	AlwaysAssertExit (cir.boundingBox().length() ==
			  circop->boundingBox().length());
	AlwaysAssertExit (cir.comment() == circop->comment());
	AlwaysAssertExit (allEQ (cir.center(),
				 ((LCEllipsoid*)circop)->center()));
	AlwaysAssertExit (allEQ (cir.radii(),
				 ((LCEllipsoid*)circop)->radii()));
	delete circop;
    }
    {
	// Test persistency.
	LCRegionFixed* boxcop = (LCRegionFixed*)
	                    (LCRegion::fromRecord (box.toRecord(""), ""));
	AlwaysAssertExit (box.hasMask() == boxcop->hasMask());
	AlwaysAssertExit (allEQ (box.maskArray(), boxcop->maskArray()));
	AlwaysAssertExit (box.boundingBox().start() ==
			  boxcop->boundingBox().start());
	AlwaysAssertExit (box.boundingBox().end() ==
			  boxcop->boundingBox().end());
	AlwaysAssertExit (box.boundingBox().stride() ==
			  boxcop->boundingBox().stride());
	AlwaysAssertExit (box.boundingBox().length() ==
			  boxcop->boundingBox().length());
	AlwaysAssertExit (box.comment() == boxcop->comment());
	delete boxcop;
	LCRegionFixed* circop = (LCRegionFixed*)
	                    (LCRegion::fromRecord (cir.toRecord(""), ""));
	AlwaysAssertExit (cir.hasMask() == circop->hasMask());
	AlwaysAssertExit (allEQ (cir.maskArray(), circop->maskArray()));
	AlwaysAssertExit (cir.boundingBox().start() ==
			  circop->boundingBox().start());
	AlwaysAssertExit (cir.boundingBox().end() ==
			  circop->boundingBox().end());
	AlwaysAssertExit (cir.boundingBox().stride() ==
			  circop->boundingBox().stride());
	AlwaysAssertExit (cir.boundingBox().length() ==
			  circop->boundingBox().length());
	AlwaysAssertExit (cir.comment() == circop->comment());
	AlwaysAssertExit (allEQ (cir.center(),
				 ((LCEllipsoid*)circop)->center()));
	AlwaysAssertExit (allEQ (cir.radii(),
				 ((LCEllipsoid*)circop)->radii()));
	delete circop;
    }
    {
        // test comparison
        LCBox box1(start, end, latticeShape);
        LCBox box2(box1);
        AlwaysAssertExit (box2 == box1);
        LCBox box3(start, end-2, latticeShape);
        AlwaysAssertExit (box3 != box1);
        LCEllipsoid cir1 (center, radius, latticeShape);
        LCEllipsoid cir2 (cir1);
        AlwaysAssertExit (cir2 == cir1);
        LCEllipsoid cir3 (center, radius-0.01, latticeShape);
        AlwaysAssertExit (cir3 != cir1);
        AlwaysAssertExit (cir1 != box1);
    }
}


int main()
{
    try {
	doIt (IPosition (2,11,20),
	      IPosition (2,3,4), IPosition (2,7,8),
	      IPosition (2,5,10), 5.);
	doIt (IPosition (2,10,20),
	      IPosition (2,3,4), IPosition (2,7,8),
	      IPosition (2,4,16), 5.);
	Vector<Float> center(2), radii(2);
	radii(0) = radii(1) = 5.01;
	center(0) = 5;
	center(1) = 10.5;
	doIt (IPosition (2,11,20), center, radii);
	radii(0) = 4;
	radii(1) = 8;
	center(1) = 10;
	doIt (IPosition (2,11,20), center, radii);
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
