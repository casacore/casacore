//# tLatticeRegion.cc: Test program for LatticeRegion class
//# Copyright (C) 1998,1999,2000,2001
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

#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCEllipsoid.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (const IPosition& latticeShape,
	   const IPosition& start,
	   const IPosition& end,
	   const IPosition& center,
	   Int radius)
{
    uInt ndim = start.nelements();
    // Show output of simple circle.
    LCEllipsoid cir (center, radius, latticeShape);
    LatticeRegion reg1(cir);
    AlwaysAssertExit (reg1.hasMask());
    AlwaysAssertExit (cir.shape() == reg1.shape());
    AlwaysAssertExit (allEQ (cir.get(), reg1.get()));
    cout << "circle: " << reg1.get() << endl;

    // Be sure that a slicer gives 
    LatticeRegion reg2(Slicer(start,end,IPosition(2,2),Slicer::endIsLast),
		       latticeShape);
    AlwaysAssertExit (! reg2.hasMask());
    AlwaysAssertExit (reg2.get().shape() == 1+(end-start)/2);
    AlwaysAssertExit (allEQ (reg2.get(), True));
    cout << "slicer: " << reg2.get() << endl;

    // Take a slicer of the slicer.
    LatticeRegion reg2a((Slicer(IPosition(ndim,0), reg2.shape()-1,
			       IPosition(2,1,2), Slicer::endIsLast)),
			reg2.shape());
    AlwaysAssertExit (! reg2a.hasMask());
    AlwaysAssertExit (allEQ (reg2a.get(), True));
    cout << "strided slicer: " << reg2a.get() << endl;
}


int main()
{
    try {
	doIt (IPosition (2,11,20),
	      IPosition (2,3,4), IPosition (2,7,8),
	      IPosition (2,5,10), 5);
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
