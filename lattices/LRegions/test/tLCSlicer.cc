//# tLCSlicer.cc: Test program the LCSlicer class
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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

#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt()
{
    // Construct a simple LCSlicer.
    // Test comment functions, copy constructor, default constructor,
    // assignment, and to/fromRecord.
    {
	LCSlicer sl1 (IPosition(3,5,8,3), IPosition(3,10,20,30));
	sl1.setComment ("comm1");
	AlwaysAssertExit (sl1.isComplete());
	AlwaysAssertExit (sl1.isAbsolute());
	AlwaysAssertExit (! sl1.isFractional());
	AlwaysAssertExit (! sl1.isStrided());
	AlwaysAssertExit (! sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	AlwaysAssertExit (sl1.comment() == "comm1");
	Slicer sl (sl1.toSlicer (IPosition(3,0,0,0), IPosition(3,40,50,60)));
	cout << sl.start() << sl.end() << sl.stride() << endl;

	LCSlicer sl2 (sl1);
	AlwaysAssertExit (sl2 == sl1);
	AlwaysAssertExit (sl2.comment() == sl1.comment());

	LCSlicer sl3;
	AlwaysAssertExit (! sl3.isComplete());
	AlwaysAssertExit (sl3 != sl1);
	sl2 = sl3;
	AlwaysAssertExit (sl2 == sl3);
	AlwaysAssertExit (sl2.comment() == "");
	sl2 = sl1;
	AlwaysAssertExit (sl2 == sl1);
	AlwaysAssertExit (sl2.comment() == sl1.comment());

	TableRecord rec = sl1.toRecord ("");
	LCSlicer* sl4 = LCSlicer::fromRecord (rec, "");
	AlwaysAssertExit (! (*sl4 != sl1));
	AlwaysAssertExit (sl4->comment() == sl1.comment());
	delete sl4;
    }
    // Test if stride gets padded.
    // Also test if toSlicer works well (higher dimensionality and truncating
    // trc to shape).
    {
	LCSlicer sl1 (IPosition(3,5,8,3), IPosition(3,10,20,30),
		      IPosition(1,2));
	AlwaysAssertExit (sl1.isComplete());
	AlwaysAssertExit (sl1.isAbsolute());
	AlwaysAssertExit (! sl1.isFractional());
	AlwaysAssertExit (sl1.isStrided());
	AlwaysAssertExit (! sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	Slicer sl (sl1.toSlicer (IPosition(4,0,0,0,0), IPosition(4,8,50,60,7)));
	cout << sl.start() << sl.end() << sl.stride() << endl;
    }
    // Test if trc gets padded.
    {
	LCSlicer sl1 (IPosition(3,5,8,3), IPosition(1,10),
		      IPosition(1,2));
	AlwaysAssertExit (! sl1.isComplete());
	AlwaysAssertExit (sl1.isAbsolute());
	AlwaysAssertExit (! sl1.isFractional());
	AlwaysAssertExit (sl1.isStrided());
	AlwaysAssertExit (sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	Slicer sl (sl1.toSlicer (IPosition(3,0,0,0), IPosition(3,40,50,60)));
	cout << sl.start() << sl.end() << sl.stride() << endl;
    }
    // Test if pixel reference works fine.
    {
	LCSlicer sl1 (IPosition(3,5,8,3), IPosition(1,10),
		      IPosition(1,2), RegionType::RelRef);
	AlwaysAssertExit (! sl1.isComplete());
	AlwaysAssertExit (! sl1.isAbsolute());
	AlwaysAssertExit (! sl1.isFractional());
	AlwaysAssertExit (sl1.isStrided());
	AlwaysAssertExit (sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	Slicer sl (sl1.toSlicer (IPosition(3,10,11,12), IPosition(3,40,50,60)));
	cout << sl.start() << sl.end() << sl.stride() << endl;
    }
    // Test if center reference works fine.
    {
	LCSlicer sl1 (IPosition(3,5,8,3), IPosition(1,10),
		      IPosition(1,2), RegionType::RelCen);
	AlwaysAssertExit (! sl1.isComplete());
	AlwaysAssertExit (! sl1.isAbsolute());
	AlwaysAssertExit (! sl1.isFractional());
	AlwaysAssertExit (sl1.isStrided());
	AlwaysAssertExit (sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	Slicer sl (sl1.toSlicer (IPosition(3,10,11,12), IPosition(3,40,50,60)));
	cout << sl.start() << sl.end() << sl.stride() << endl;
    }
    // Test using vectors with combination of fractional and absolute/relative.
    Vector<Float> blc(3);
    Vector<Float> trc(3);
    Vector<Float> inc(3);
    Vector<Bool> fracblc(3);
    Vector<Bool> fractrc(3);
    Vector<Bool> fracinc(3);
    Vector<Int> relblc(3);
    Vector<Int> reltrc(3);
    blc(0) = 0.125;  blc(1) = 8;   blc(2) = 3;
    trc(0) = 10;     trc(1) = 0.4; trc(2) = 25;
    inc(0) = 1;      inc(1) = 1;   inc(2) = 0.1;
    fracblc = False;  fracblc(0) = True;
    fractrc = False;  fractrc(1) = True;
    fracinc = False;  fracinc(2) = True;
    relblc  = RegionType::Abs;  relblc(0) = RegionType::RelRef;
    reltrc  = RegionType::Abs;  reltrc(0) = RegionType::RelCen;
    {
	LCSlicer sl1 (blc, trc, inc, fracblc, fractrc, fracinc, relblc, reltrc);
	AlwaysAssertExit (! sl1.isComplete());
	AlwaysAssertExit (! sl1.isAbsolute());
	AlwaysAssertExit (sl1.isFractional());
	AlwaysAssertExit (sl1.isStrided());
	AlwaysAssertExit (! sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	Slicer sl (sl1.toSlicer (IPosition(3,10,11,12), IPosition(3,40,50,60)));
	cout << sl1.blc() << sl1.trc() << sl1.inc() << endl;
	cout << sl.start() << sl.end() << sl.stride() << endl;
    }
    {
	Vector<Double> blc(3);
	Vector<Double> trc(2);
	blc(0) = 0.4; blc(1) = 0.1; blc(2) = 0.3;
	trc(0) = 0.5; trc(1) = 0.6;
	LCSlicer sl1 (blc, trc, True);
	AlwaysAssertExit (! sl1.isComplete());
	AlwaysAssertExit (sl1.isAbsolute());
	AlwaysAssertExit (sl1.isFractional());
	AlwaysAssertExit (! sl1.isStrided());
	AlwaysAssertExit (sl1.isUnspecified());
	AlwaysAssertExit (sl1.ndim() == 3);
	Slicer sl (sl1.toSlicer (IPosition(3,10,11,12), IPosition(3,40,50,60)));
	cout << sl.start() << sl.end() << sl.stride() << endl;
    }
    {
	// Test if constructing from a record works fine.
	// Such a record is created by the quarter function in regionmanager.g.
	Vector<Float> vec(2);
	Vector<Bool> flags(2);
	flags = True;
	Vector<Int> absrel(2);
	absrel = RegionType::Abs;
	vec = 0.25;
	TableRecord rec;
	rec.define ("name", "LCSLicer");
	rec.define ("isRegion", Int(RegionType::ArrSlicer));
	vec = 0.25;
	rec.define ("blc", vec);
	vec = 0.75;
	rec.define ("trc", vec);
	rec.define ("inc", Vector<Float>());
	rec.define ("fracblc", flags);
	rec.define ("fractrc", flags);
	rec.define ("fracinc", Vector<Bool>());
	rec.define ("arblc", absrel);
	rec.define ("artrc", absrel);
	rec.define ("oneRel", True);
	rec.define ("comment", "");
	LCSlicer* lc = LCSlicer::fromRecord (rec, "");
	Slicer sl (lc->toSlicer (IPosition(3,0,0,0), IPosition(3,40,50,60)));
	cout << sl.start() << sl.end() << sl.stride() << endl;
	delete lc;
    }
    {
        Vector<Float> blc(4, 0), trc(4,0), refPix(4, 0);
        blc[3] = 23;
        trc[0] = 399;
        trc[1] = 399;
        trc[3] = 23;
        refPix[0] = 100;
        refPix[1] = 100;
        refPix[3] = -23;
        IPosition newLatticeShape(4, 400, 400, 1, 1);
        LCSlicer lcslicer(blc, trc, False, RegionType::RelRef);
        Slicer sl = lcslicer.toSlicer(refPix, newLatticeShape);
        AlwaysAssert(sl.start() == IPosition(4, 100, 100, 0, 0), AipsError);
        AlwaysAssert(sl.end() == IPosition(4, 399, 399, 0, 0), AipsError);
    }
}


int main()
{
    try {
	doIt();
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
