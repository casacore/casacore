//# LELFunction.cc:  this defines non-templated classes in LELFunction.h
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

#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[])
{
  try {
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "128", "Number of pixels along the x-axis", "int");
    inp.create("ny", "128", "Number of pixels along the y-axis", "int");
    inp.create("nz", "128", "Number of pixels along the z-axis", "int");
    inp.create("tx", "0", "Number of pixels along the x-axis tile", "int");
    inp.create("ty", "0", "Number of pixels along the y-axis tile", "int");
    inp.create("tz", "0", "Number of pixels along the z-axis tile", "int");
    inp.readArguments(argc, argv);

    const uInt nx=inp.getInt("nx");
    const uInt ny=inp.getInt("ny");
    const uInt nz=inp.getInt("nz");
    const uInt tx=inp.getInt("tx");
    const uInt ty=inp.getInt("ty");
    const uInt tz=inp.getInt("tz");
    const IPosition latticeShape(3, nx, ny, nz);
    IPosition tileShape(3, tx, ty, tz);
    if (tileShape.product() == 0) {
      tileShape = TiledShape(latticeShape).tileShape();
    }
    cout << "Data Type: Float";
    cout << "  Lattice shape:" << latticeShape;
    cout << "  Tile shape:" << tileShape << endl;

    {
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> lat(TiledShape(latticeShape, tileShape), paTable);
      Array<Float> arr(tileShape);
      indgen(arr);
      LatticeIterator<Float> iter(lat, tileShape);
      Timer timer;
      for (iter.reset(); !iter.atEnd(); iter++) {
 	iter.woCursor() = arr;
	arr += Float(tileShape.product());
      }
      timer.show ("filling         ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab2", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr(2*lat);
      latout.copyData (expr);
      timer.show ("2*lat           ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab2", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr((mean(lat)-lat.shape().product()/3)*lat);
      latout.copyData (expr);
      timer.show ("mean(lat)*lat   ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab2", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr(amp(lat,lat));
      latout.copyData (expr);
      timer.show ("amp(lat,lat)    ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab2", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr(lat+lat);
      latout.copyData (expr);
      timer.show ("lat+lat         ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab3", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr(2*min(lat)*2*lat);
      latout.copyData (expr);
      timer.show ("2*min(lat)*2*lat");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab3", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr(lat*2*min(lat)*2);
      latout.copyData (expr);
      timer.show ("lat*2*min(lat)*2");
    }
    {
      Table t1("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat1(t1);
      Table t2("tLatticeExpr2_tmp.tab2");
      PagedArray<Float> lat2(t2);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab3", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LatticeExpr<Float> expr(2*lat1 + 3*lat2);
      latout.copyData (expr);
      timer.show ("2*lat1 + 3*lat2 ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab3");
      PagedArray<Float> lat(t);
      Array<Float> arr(tileShape);
      indgen(arr);
      RO_LatticeIterator<Float> iter(lat, tileShape);
      Timer timer;
      for (iter.reset(); !iter.atEnd(); iter++) {
	if (! allEQ (iter.cursor(), float(8)*arr)) {
	  cout << "result mismatches" << endl;
	  return 1;
	}
	arr += Float(tileShape.product());
      }
      timer.show ("checking        ");
    }
    {
      Table t("tLatticeExpr2_tmp.tab");
      PagedArray<Float> lat(t);
      SetupNewTable paSetup("tLatticeExpr2_tmp.tab2", TableDesc(), Table::New);
      Table paTable(paSetup);
      PagedArray<Float> latout(TiledShape(latticeShape, tileShape), paTable);
      Timer timer;
      LCPagedMask mask (lat.shape(), lat.tableName() + "/mask");
      mask.set (True);
      timer.show ("filling mask    ");
      SubLattice<Float> sublat (lat, mask);
      timer.mark();
      LatticeExpr<Float> expr(sublat+sublat);
      latout.copyData (expr);
      timer.show ("sublat+sublat   ");
      timer.mark();
      LatticeExpr<Float> expr2((mean(sublat)-lat.shape().product()/3)*lat);
      latout.copyData (expr2);
      timer.show ("mean(sublat)*lat");
      timer.mark();
      LatticeExpr<Float> expr3(amp(sublat,lat));
      latout.copyData (expr3);
      timer.show ("amp(sublat,lat) ");
    }
    if (latticeShape.product() <= 1024*1024) {
      Array<Float> arr1(latticeShape);
      Array<Float> arr5(latticeShape);
      indgen(arr1);
      ArrayLattice<Float> al1(arr1);
      ArrayLattice<Float> al5(latticeShape);

      cout << "arr1+arr1" << endl;
      Timer timer;
      arr5 = arr1 + arr1;
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(al1+al1));
      timer.show ("as lattice");

      cout << "arr1+arr1+arr1" << endl;
      timer.mark();
      arr5 = arr1 + arr1 + arr1;
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(al1+al1+al1));
      timer.show ("as lattice");

      cout << "arr1+arr1+arr1+arr1" << endl;
      timer.mark();
      arr5 = arr1 + arr1 + arr1 + arr1;
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(al1+al1+al1+al1));
      timer.show ("as lattice");

      cout << "arr1+arr1+arr1+arr1+arr1" << endl;
      timer.mark();
      arr5 = arr1 + arr1 + arr1 + arr1 + arr1;
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(al1+al1+al1+al1+al1));
      timer.show ("as lattice");

      cout << "((((arr1+arr1)+arr1)+arr1)+arr1)+arr1" << endl;
      timer.mark();
      arr5 = ((((arr1 + arr1) + arr1) + arr1) + arr1) + arr1;
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(((((al1+al1)+al1)+al1)+al1)+al1));
      timer.show ("as lattice");

      cout << "arr1+(arr1+(arr1+(arr1+(arr1+arr1))))" << endl;
      timer.mark();
      arr5 = arr1 + (arr1 + (arr1 + (arr1 + (arr1 + arr1))));
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(al1+(al1+(al1+(al1+(al1+al1))))));
      timer.show ("as lattice");

      cout << "arr1+arr1+arr1+arr1+arr1+arr1+arr1+arr1+arr1+arr1" << endl;
      timer.mark();
      arr5 = arr1+arr1+arr1+arr1+arr1+arr1+arr1+arr1+arr1+arr1;
      timer.show ("as array  ");
      timer.mark();
      al5.copyData (LatticeExpr<Float>(al1+al1+al1+al1+al1+al1+al1+al1+al1+al1));
      timer.show ("as lattice");
    }

  } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    return 1;
  } 
 
  return 0;
}
