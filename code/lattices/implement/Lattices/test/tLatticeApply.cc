//# tLatticeApply.cc
//# Copyright (C) 1996,1997
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

#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/VectorCollapser.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/OS/Timer.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/String.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/PagedArrIter.h>
#include <iostream.h>


class MyCollapser : public VectorCollapser<Int>
{
public:
    MyCollapser() : itsVec(2) {};
    virtual Int collapse (const Vector<Int> &vector,
			  const IPosition& pos);
    virtual Vector<Int>& multiCollapse (const Vector<Int> &vector,
					const IPosition& pos);
private:
    Vector<Int> itsVec;
};
Int MyCollapser::collapse (const Vector<Int> &vector, const IPosition& pos)
{
    return sum(vector.ac());
}
Vector<Int>& MyCollapser::multiCollapse (const Vector<Int> &vector,
					 const IPosition&)
{
    itsVec(0) = sum(vector.ac());
    itsVec(1) = 0;
    return itsVec;
}

main (int argc, char *argv[])
{
    Input inp(1);
    inp.Version(" ");
    inp.Create("nx", "128", "Number of pixels along the x-axis", "int");
    inp.Create("ny", "128", "Number of pixels along the y-axis", "int");
    inp.Create("nz", "128", "Number of pixels along the z-axis", "int");
    inp.Create("tx", "32", "Number of pixels along the x-axis tile", "int");
    inp.Create("ty", "32", "Number of pixels along the y-axis tile", "int");
    inp.Create("tz", "32", "Number of pixels along the z-axis tile", "int");
    inp.ReadArguments(argc, argv);

    const uInt nx=inp.GetInt("nx");
    const uInt ny=inp.GetInt("ny");
    const uInt nz=inp.GetInt("nz");
    const uInt tx=inp.GetInt("tx");
    const uInt ty=inp.GetInt("ty");
    const uInt tz=inp.GetInt("tz");
    IPosition latticeShape(3, nx, ny, nz);
    IPosition tileShape(3, tx, ty, tz);
    if (tileShape.product() == 0) {
	tileShape = PagedArray<Int>::defaultTileShape(latticeShape);
    }
    cout << "Data Type: Int";
    cout << "  Lattice shape:" << latticeShape;
    cout << "  Tile shape:" << tileShape << endl;
    {
	SetupNewTable paSetup("tLatticeApply_tmp.array",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<Int> lat(latticeShape, paTable, tileShape);      
	Array<Int> arr(tileShape);
	indgen(arr);
	PagedArrIter<Int> iter(lat, tileShape);
	Timer tim;
	for (iter.reset(); !iter.atEnd(); iter++, arr += tileShape.product()) {
	    iter.cursor() = arr;
	}
	tim.show();
    }
    {
	Table t("tLatticeApply_tmp.array", Table::Update);
	PagedArray<Int> lat(t);
	latticeShape(2) = 1;
	tileShape(2) = 1;
	SetupNewTable paSetup2("tLatticeApply_tmp.array1",
			       TableDesc(), Table::New);
	Table paTable2(paSetup2);
	PagedArray<Int> latout(latticeShape, paTable2, tileShape);      
	MyCollapser collapser;
	Timer tim;
	LatticeApply<Int>::vectorApply (latout, lat, collapser,
					2, IPosition(3,0), lat.shape()-1,
					False, False, "");
	tim.show();
    }
    {
	Table t("tLatticeApply_tmp.array", Table::Update);
	PagedArray<Int> lat(t);
	latticeShape(2) = 1;
	tileShape(2) = 1;
	SetupNewTable paSetup0("tLatticeApply_tmp.array2a",
			       TableDesc(), Table::New);
	Table paTable0(paSetup0);
	PagedArray<Int> latout0(latticeShape, paTable0, tileShape);      
	SetupNewTable paSetup1("tLatticeApply_tmp.array2b",
			       TableDesc(), Table::New);
	Table paTable1(paSetup1);
	PagedArray<Int> latout1(latticeShape, paTable1, tileShape);      
	PtrBlock<Lattice<Int>*> blat(2);
	blat[0] = &latout0;
	blat[1] = &latout1;
	MyCollapser collapser;
	Timer tim;
	LatticeApply<Int>::vectorMultiApply (blat, lat, collapser,
					     2, IPosition(3,0), lat.shape()-1,
					     False, False, "");
	tim.show();
    }
}
