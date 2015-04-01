//# tCurvedLattice2D.cc: Test program for class CurvedLattice
//# Copyright (C) 2003
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

#include <casacore/lattices/Lattices/CurvedLattice2D.h>
#include <casacore/lattices/LatticeMath/CLIPNearest2D.h>
#include <casacore/lattices/Lattices/PixelCurve1D.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (MaskedLattice<Int>& lat, uInt axis1, uInt axis2, uInt curveAxis)
{
  // Make a straight line from (1,0) to the trc.
  IPosition shp = lat.shape();
  Int xtop = shp(axis1);
  Int ytop = shp(axis2);
  Int nr = xtop-1;
  if (nr > ytop) nr = ytop;
  PixelCurve1D pc(1, 0, nr, nr-1, nr);
  CurvedLattice2D<Int> clat(lat, CLIPNearest2D<Int>(), pc,
			    axis1, axis2, curveAxis);
  // Compose expected output shape.
  IPosition outshp(shp.nelements() - 1);
  uInt axnr = 0;
  for (uInt i=0; i<shp.nelements(); i++) {
    if (axnr == curveAxis) outshp[axnr++] = nr;
    if (i != axis1  &&  i != axis2) {
      outshp[axnr++] = shp[i];
    }
    if (axnr == curveAxis) outshp[axnr++] = nr;
  }
  AlwaysAssertExit (clat.shape() == outshp);
  AlwaysAssertExit (!clat.isPersistent());
  AlwaysAssertExit (!clat.isWritable());
  AlwaysAssertExit (!clat.canReferenceArray());
  AlwaysAssertExit (clat.isMasked() == lat.isMasked());
  AlwaysAssertExit (clat.hasPixelMask() == lat.hasPixelMask());
  AlwaysAssertExit (clat.isPaged() == lat.isPaged());
  // Read all the data of the original and curved lattice.
  Array<Int> cdata = clat.get();
  Array<Int> alldata = lat.get();
  // Compare if they are equal.
  IPosition cblc(outshp.nelements(), 0);
  IPosition ctrc(outshp - 1);
  ctrc[curveAxis] = 0;
  IPosition ablc(alldata.ndim(), 0);
  IPosition atrc(alldata.shape() - 1);
  outshp[curveAxis] = 1;
  for (Int i=0; i<nr; i++) {
    cblc[curveAxis] = i;
    ctrc[curveAxis] = i;
    ablc[axis1] = i+1;
    atrc[axis1] = i+1;
    ablc[axis2] = i;
    atrc[axis2] = i;
    Array<Int> achunk = alldata(ablc, atrc);
    AlwaysAssert(allEQ(achunk.reform(outshp), cdata(cblc, ctrc)),
		 AipsError);
  }
  // Iterate through the curved lattice and check if the data match.
  RO_LatticeIterator<Int> iter(clat, outshp);
  Int i=0;
  for (iter.reset(); !iter.atEnd(); iter++){
    cblc[curveAxis] = i;
    ctrc[curveAxis] = i;
    AlwaysAssert(allEQ(iter.cursor(), cdata(cblc, ctrc)),
		 AipsError);
    i++;
  }  
}

void doIt2 (const Lattice<Int>& lattice)
{
  SubLattice<Int> mlat(lattice);
  doIt (mlat, 0, 1, 0);
  doIt (mlat, 1, 0, 0);
}

void doIt3 (const Lattice<Int>& lattice)
{
  SubLattice<Int> mlat(lattice);
  doIt (mlat, 0, 1, 1);
  doIt (mlat, 0, 1, 0);
  doIt (mlat, 0, 2, 1);
  doIt (mlat, 0, 2, 0);
  doIt (mlat, 1, 0, 1);
  doIt (mlat, 1, 0, 0);
  doIt (mlat, 1, 2, 1);
  doIt (mlat, 1, 2, 0);
  doIt (mlat, 2, 0, 1);
  doIt (mlat, 2, 0, 0);
  doIt (mlat, 2, 1, 1);
  doIt (mlat, 2, 1, 0);
}

int main (int argc, const char* argv[])
{
  try {
    {
      const IPosition latticeShape(2, 16, 12);
      Array<Int> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Int> lattice(arr);
      doIt2 (lattice);
      PagedArray<Int> pa(latticeShape, "tCurvedLattice2D_tmp.pa");
      pa.put (arr);
      doIt2 (pa);
    }
    {
      const IPosition latticeShape(3, 16, 12, 4);
      Array<Int> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Int> lattice(arr);
      doIt3 (lattice);
      PagedArray<Int> pa(latticeShape, "tCurvedLattice2D_tmp.pa");
      pa.put (arr);
      doIt3 (pa);
    }
    {
      const IPosition latticeShape(4, 16, 12, 4, 32);
      Array<Int> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Int> lattice(arr);
      doIt3 (lattice);
      PagedArray<Int> pa(latticeShape, "tCurvedLattice2D_tmp.pa");
      pa.put (arr);
      doIt3 (pa);
    }

    {
      // Test performance.
      Input inp(1);
      inp.version(" ");
      inp.create("nx", "64", "Number of pixels along the x-axis", "int");
      inp.create("ny", "64", "Number of pixels along the y-axis", "int");
      inp.create("nz", "64", "Number of pixels along the z-axis", "int");
      inp.readArguments(argc, argv);
      const uInt nx=inp.getInt("nx");
      const uInt ny=inp.getInt("ny");
      const uInt nz=inp.getInt("nz");
      IPosition latticeShape(3,nx,ny,nz);
      {
	PagedArray<Int> pa(latticeShape, "tCurvedLattice2D_tmp.pa");
	Array<Int> arr(IPosition(3,nx,ny,1));
	indgen(arr);
	LatticeIterator<Int> iter(pa, IPosition(3,nx,ny,1));
	for (iter.reset(); !iter.atEnd(); iter++) {
	  iter.woCursor() = arr;
	  arr += Int(arr.nelements());
	}
	cout << "Filled PagedArray with shape " << latticeShape << endl;
      }
      PagedArray<Int> pa("tCurvedLattice2D_tmp.pa");
      SubLattice<Int> mlat(pa);
      // Make a straight line from (0,0) to the trc.
      IPosition shp = pa.shape();
      Int xtop = shp(0);
      Int ytop = shp(1);
      Int nr = xtop;
      if (nr > ytop) nr = ytop;
      PixelCurve1D pc(0, 0, shp(0)-1, shp(1)-1, nr);
      cout << "nr=" << nr << endl;
      {
	CurvedLattice2D<Int> clat(mlat, CLIPNearest2D<Int>(), pc, 0, 1, 0);
	Timer timer;
	clat.get();
	timer.show("curved 0,1,0");
	pa.showCacheStatistics(cout);
      }
      {
	CurvedLattice2D<Int> clat(mlat, CLIPNearest2D<Int>(), pc, 0, 1, 1);
	Timer timer;
	clat.get();
	timer.show("curved 0,1,1");
	pa.showCacheStatistics(cout);
      }
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
