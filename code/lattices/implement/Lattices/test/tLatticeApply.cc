//# tLatticeApply.cc: Test program for class LatticeApply
//# Copyright (C) 1997
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
#include <trial/Lattices/LineCollapser.h>
#include <trial/Lattices/TiledCollapser.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/OS/Timer.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/String.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeProgress.h>
#include <trial/Lattices/LatticeRegion.h>
#include <trial/Tasking/ProgressMeter.h>
#include <iostream.h>


class MyLineCollapser : public LineCollapser<Int>
{
public:
    MyLineCollapser() : itsVec(2) {};
    virtual void init (uInt nOutPixelsPerCollapse);
    virtual Int process (const Vector<Int>& vector,
			 const IPosition& pos);
    virtual Vector<Int>& multiProcess (const Vector<Int>& vector,
				       const IPosition& pos);
private:
    Vector<Int> itsVec;
};
void MyLineCollapser::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == 1, AipsError);
}
Int MyLineCollapser::process (const Vector<Int>& vector, const IPosition&)
{
    return sum(vector.ac());
}
Vector<Int>& MyLineCollapser::multiProcess (const Vector<Int>& vector,
					    const IPosition&)
{
    itsVec(0) = sum(vector.ac());
    itsVec(1) = -itsVec(0);
    return itsVec;
}


class MyTiledCollapser : public TiledCollapser<Int>
{
public:
    MyTiledCollapser() {};
    virtual void init (uInt nOutPixelsPerCollapse);
    virtual void initAccumulator (uInt n1, uInt n3);
    virtual void process (uInt index1, uInt index3,
			  const Int* inData, uInt inIncr, uInt nrval);
    virtual Array<Int> endAccumulator (const IPosition& shape);
private:
    Matrix<uInt>* itsSum1;
    Block<Int>*   itsSum2;
    uInt          itsn1;
    uInt          itsn3;
};
void MyTiledCollapser::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == 2, AipsError);
}
void MyTiledCollapser::initAccumulator (uInt n1, uInt n3)
{
    itsSum1 = new Matrix<uInt> (n1, n3);
    itsSum2 = new Block<Int> (n1*n3);
    itsSum1->set (0);
    itsSum2->set (0);
    itsn1 = n1;
    itsn3 = n3;
}
void MyTiledCollapser::process (uInt index1, uInt index3,
				const Int* inData,
				uInt inIncr, uInt nrval)
{
    uInt& sum1 = (*itsSum1)(index1, index3);
    Int& sum2 = (*itsSum2)[index1 + index3*itsn1];
    for (uInt i=0; i<nrval; i++) {
	sum1 += *inData;
	sum2 -= *inData;
	inData += inIncr;
    }
}
Array<Int> MyTiledCollapser::endAccumulator (const IPosition& shape)
{
    Array<Int> result(shape);
    Bool deleteRes, deleteSum1;
    Int* res = result.getStorage (deleteRes);
    Int* resptr = res;
    const uInt* sum1 = itsSum1->getStorage (deleteSum1);
    const uInt* sum1ptr = sum1;
    const Int* sum2ptr = itsSum2->storage();
    for (uInt i=0; i<itsn3; i++) {
        for (uInt j=0; j<itsn1; j++) {
	    *resptr++ = Int(*sum1ptr++);
	}
	objcopy (resptr, sum2ptr, itsn1);
	resptr += itsn1;
	sum2ptr += itsn1;
    }
    itsSum1->freeStorage (sum1, deleteSum1);
    result.putStorage (res, deleteRes);
    delete itsSum1;
    delete itsSum2;
    return result;
}


class MyLatticeProgress : public LatticeProgress
{
public:
    MyLatticeProgress() : itsMeter(0) {};
    virtual ~MyLatticeProgress();
    virtual void initDerived();
    virtual void nstepsDone (uInt nsteps);
    virtual void done();
private:
    ProgressMeter* itsMeter;
};
MyLatticeProgress::~MyLatticeProgress()
{
    delete itsMeter;
}
void MyLatticeProgress::initDerived()
{
    delete itsMeter;
    itsMeter = new ProgressMeter(0.0, expectedNsteps(), "tLatticeApply",
				 "Vectors extracted", "", "",
				 True, max(1,Int(expectedNsteps()/100)));
}
void MyLatticeProgress::nstepsDone (uInt nsteps)
{
    itsMeter->update (nsteps);
}
void MyLatticeProgress::done()
{
    delete itsMeter;
    itsMeter = 0;
}


void doIt (int argc, char *argv[])
{
    Input inp(1);
    inp.Version(" ");
    inp.Create("nx", "128", "Number of pixels along the x-axis", "int");
    inp.Create("ny", "128", "Number of pixels along the y-axis", "int");
    inp.Create("nz", "128", "Number of pixels along the z-axis", "int");
    inp.Create("tx", "0", "Number of pixels along the x-axis tile", "int");
    inp.Create("ty", "0", "Number of pixels along the y-axis tile", "int");
    inp.Create("tz", "0", "Number of pixels along the z-axis tile", "int");
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
	tileShape = TiledShape(latticeShape).tileShape();
    }
    cout << "Data Type: Int";
    cout << "  Lattice shape:" << latticeShape;
    cout << "  Tile shape:" << tileShape << endl;

    MyLatticeProgress showProgress;
    {
	SetupNewTable paSetup("tLatticeApply_tmp.array",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<Int> lat(TiledShape(latticeShape, tileShape), paTable);    
	Array<Int> arr(IPosition(3,nx,ny,1));
	indgen(arr);
	LatticeIterator<Int> iter(lat, LatticeStepper(latticeShape,
						      IPosition(3,nx,ny,1)));
	Timer tim;
	for (iter.reset(); !iter.atEnd(); iter++, arr += Int(nx*ny)) {
	    iter.woCursor() = arr;
	}
	tim.show("fill       ");
    }
    IPosition l1Shape (latticeShape);
    IPosition t1Shape (tileShape);
    l1Shape(2) = 1;
    t1Shape(2) = 1;
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<Int> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array1",
			       TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<Int> latout(TiledShape(l1Shape,t1Shape), paTable);
	MyLineCollapser collapser;
	Timer tim;
	LatticeApply<Int>::lineApply (latout, lat, collapser, 2, &showProgress);
	tim.show("line 2     ");
    }
    {
	Table t("tLatticeApply_tmp.array1");
	PagedArray<Int> lat(t);
	Int sum = (nz-1)*nz/2*nx*ny;
	IPosition pos(3,0);
	Timer tim;
	for (Int i=0; i<l1Shape(1); i++) {
	    pos(1) = i;
	    for (Int j=0; j<l1Shape(0); j++) {
		pos(0) = j;
		Int value = lat.getAt (pos);
		if (value != sum) {
		    cout << "Value=" << value << ", expected " << sum
			 << "   at position " << pos << endl;
		}
		sum += nz;
	    }
	}
	tim.show("check      ");
    }
    IPosition l2Shape (latticeShape);
    IPosition t2Shape (tileShape);
    l2Shape(0) = 1;
    t2Shape(0) = 1;
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<Int> lat(t);
	SetupNewTable paSetup0("tLatticeApply_tmp.array2a",
			       TableDesc(), Table::New);
	Table paTable0(paSetup0);
	PagedArray<Int> latout0(TiledShape(l2Shape, t2Shape), paTable0);
	SetupNewTable paSetup1("tLatticeApply_tmp.array2b",
			       TableDesc(), Table::New);
	Table paTable1(paSetup1);
	PagedArray<Int> latout1(TiledShape(l2Shape, t2Shape), paTable1);
	PtrBlock<Lattice<Int>*> blat(2);
	blat[0] = &latout0;
	blat[1] = &latout1;
	MyLineCollapser collapser;
	Timer tim;
	LatticeApply<Int>::lineMultiApply (blat, lat, collapser, 0);
	tim.show("multiline 0");
    }
    {
	Table t("tLatticeApply_tmp.array2b");
	PagedArray<Int> lat(t);
	Int sum = (nx-1)*nx/2;
	IPosition pos(3,0);
	Timer tim;
	for (Int i=0; i<l2Shape(2); i++) {
	    pos(2) = i;
	    for (Int j=0; j<l2Shape(1); j++) {
		pos(1) = j;
		Int value = lat.getAt (pos);
		if (value != -sum) {
		    cout << "Value=" << value << ", expected " << -sum
			 << "   at position " << pos << endl;
		}
		sum += nx*nx;
	    }
	}
	tim.show("check      ");
    }
    l2Shape(0) = 2;
    t2Shape(0) = 2;
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<Int> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array2t",
			       TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<Int> latout(TiledShape(l2Shape,t2Shape), paTable);
	MyTiledCollapser collapser;
	Timer tim;
	LatticeApply<Int>::tiledApply (latout, lat, collapser,
				       IPosition(1,0));
	tim.show("tiled 0    ");
    }
    {
	Table t("tLatticeApply_tmp.array2t");
	PagedArray<Int> lat(t);
	Int sum = (nx-1)*nx/2;
	IPosition pos(3,0);
	Timer tim;
	for (Int i=0; i<l2Shape(2); i++) {
	    pos(2) = i;
	    for (Int j=0; j<l2Shape(1); j++) {
		pos(1) = j;
		pos(0) = 0;
		Int value = lat.getAt (pos);
		pos(0) = 1;
		Int value1 = lat.getAt (pos);
		if (value != sum  ||  value1 != -sum) {
		    cout << "Value=" << value << ',' << value1
			 << ", expected +-" << sum
			 << "   at position " << pos << endl;
		}
		sum += nx*nx;
	    }
	}
	tim.show("check      ");
    }
    l2Shape(2) = l2Shape(1);
    l2Shape(1) = 1;
    l2Shape(0) = 2;
    //# Do the tests below only when the lattice is small enough.
    //# Otherwise we get overflow errors (due to the summation).
    Double s1 = nx*nz;
    Double s2 = s1/2 * (nx-1);
    Double s3 = s1/2 * nx*ny*(nz-1);
    Double s4 = s1*s1;
    if (s2+s3+s4 < Double(32768)*65536) {
	{
	    Table t("tLatticeApply_tmp.array");
	    PagedArray<Int> lat(t);
	    SetupNewTable paSetup("tLatticeApply_tmp.array2tb",
				  TableDesc(), Table::New);
	    Table paTable(paSetup);
	    PagedArray<Int> latout(l2Shape, paTable);
	    MyTiledCollapser collapser;
	    Timer tim;
	    LatticeApply<Int>::tiledApply (latout, lat, collapser,
					   IPosition(2,0,2));
	    tim.show("tiled 0,2  ");
	}
	{
	    Table t("tLatticeApply_tmp.array2tb");
	    PagedArray<Int> lat(t);
	    Int sum = nz*(nx-1)*nx/2 + nx*nx*ny*(nz-1)*nz/2;
	    IPosition pos(3,0);
	    Timer tim;
	    for (Int j=0; j<l2Shape(2); j++) {
		pos(2) = j;
		pos(0) = 0;
		Int value = lat.getAt (pos);
		pos(0) = 1;
		Int value1 = lat.getAt (pos);
		if (value != sum  ||  value1 != -sum) {
		    cout << "Value=" << value << ',' << value1
			 << ", expected +-" << sum
			 << "   at position " << pos << endl;
		}
		sum += nx*nx*nz;
	    }
	    tim.show("check      ");
	}
    }
    l2Shape(2) = 1;
    if (nx*ny*nz <= 65536) {
	{
	    Table t("tLatticeApply_tmp.array");
	    PagedArray<Int> lat(t);
	    SetupNewTable paSetup("tLatticeApply_tmp.array2tc",
				  TableDesc(), Table::New);
	    Table paTable(paSetup);
	    PagedArray<Int> latout(l2Shape, paTable);
	    MyTiledCollapser collapser;
	    Timer tim;
	    LatticeApply<Int>::tiledApply (latout, lat, collapser,
					   IPosition(3,0,1,2));
	    tim.show("tiled 0,1,2");
	}
	{
	    Table t("tLatticeApply_tmp.array2tc");
	    PagedArray<Int> lat(t);
	    //# Use uInt to avoid possible overflow.
	    uInt s = nx*ny*nz;
	    s = s * (s-1) / 2;
	    Int sum = s;
	    IPosition pos(3,0);
	    Timer tim;
	    Int value = lat.getAt (pos);
	    pos(0) = 1;
	    Int value1 = lat.getAt (pos);
	    if (value != sum  ||  value1 != -sum) {
		cout << "Value=" << value << ',' << value1
		     << ", expected +-" << sum
		     << "   at position " << pos << endl;
	    }
	    tim.show("check      ");
	}
    }
    IPosition l3Shape(2);
    Slicer slicer3 (IPosition(3,1,2,3),
		    latticeShape-IPosition(3,8,7,6),
		    IPosition(3,2,5,3),
		    Slicer::endIsLast);
    l3Shape(0) = slicer3.length()(0);
    l3Shape(1) = slicer3.length()(2);
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<Int> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array3",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<Int> latout(l3Shape, paTable);
	MyLineCollapser collapser;
	LatticeRegion region (slicer3, lat.shape());
	Timer tim;
	LatticeApply<Int>::lineApply (latout, lat, region, collapser, 1);
	tim.show("lsliced 1  ");
    }
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<Int> lat(t);
	SetupNewTable paSetup0("tLatticeApply_tmp.array4a",
			       TableDesc(), Table::New);
	Table paTable0(paSetup0);
	PagedArray<Int> latout0(l3Shape, paTable0);
	SetupNewTable paSetup1("tLatticeApply_tmp.array4b",
			       TableDesc(), Table::New);
	Table paTable1(paSetup1);
	PagedArray<Int> latout1(l3Shape, paTable1);
	PtrBlock<Lattice<Int>*> blat(2);
	blat[0] = &latout0;
	blat[1] = &latout1;
	MyLineCollapser collapser;
	LatticeRegion region (slicer3, lat.shape());
	Timer tim;
	LatticeApply<Int>::lineMultiApply (blat, lat, region, collapser, 1);
	tim.show("msliced 1  ");
    }
    IPosition l5Shape(5,1);
    l5Shape(0) = slicer3.length()(0);
    l5Shape(2) = 2;
    l5Shape(4) = slicer3.length()(2);
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<Int> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array5t",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<Int> latout(l5Shape, paTable);
	MyTiledCollapser collapser;
	LatticeRegion region (slicer3, lat.shape());
	Timer tim;
	LatticeApply<Int>::tiledApply (latout, lat, region, collapser,
				       IPosition(1,1));
	tim.show("tsliced 1  ");
    }
    {
	Table t("tLatticeApply_tmp.array3");
	PagedArray<Int> lat(t);
	Table t1("tLatticeApply_tmp.array4a");
	PagedArray<Int> lat1(t1);
	Table t2("tLatticeApply_tmp.array5t");
	PagedArray<Int> lat2(t2);
	Int sx = slicer3.start()(0);
	Int sy = slicer3.start()(1);
	Int sz = slicer3.start()(2);
	Int mx = slicer3.length()(0);
	Int my = slicer3.length()(1);
	Int mz = slicer3.length()(2);
	Int ix = slicer3.stride()(0);
	Int iy = slicer3.stride()(1);
	Int iz = slicer3.stride()(2);
	Int iniSum = (my-1) * iy * my / 2 * nx + (sy*nx + sx) * my;
	IPosition pos(2,0);
	IPosition pos2(5,0);
	Timer tim;
	for (Int i=0; i<mz; i++) {
	    Int sum = (sz+i*iz)*nx*ny*my + iniSum;
	    pos(1) = i;
	    pos2(4) = i;
	    for (Int j=0; j<mx; j++) {
		pos(0) = j;
		pos2(0) = j;
		Int value = lat.getAt (pos);
		Int value1 = lat1.getAt (pos);
		pos2(2) = 0;
		Int value2p = lat2.getAt (pos2);
		pos2(2) = 1;
		Int value2m = lat2.getAt (pos2);
		if (value != sum  ||  value1 != sum  ||  value2p != sum
		||  value2m != -sum) {
		    cout << "Value=" << value << ',' << value1
			 << ',' << value2p << ',' << value2m
			 << ", expected " << sum
			 << "   at position " << pos << endl;
		}
		sum += ix*my;
	    }
	}
	tim.show("check      ");
    }
}


main (int argc, char *argv[])
{
    try {
	doIt (argc,argv);
	cout<< "OK"<< endl;
	return 0;
    } catch (AipsError x) {
	cerr << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    cout << "OK" << endl;
    return 0;
}
