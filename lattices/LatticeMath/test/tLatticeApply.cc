//# tLatticeApply.cc: Test program for class LatticeApply
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/lattices/LatticeMath/LatticeApply.h>
#include <casacore/lattices/LatticeMath/LineCollapser.h>
#include <casacore/lattices/LatticeMath/TiledCollapser.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/LatticeMath/LatticeProgress.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
class MyLineCollapser : public LineCollapser<int32_t>
{
public:
    MyLineCollapser() {}
    virtual void init (uint32_t nOutPixelsPerCollapse);
    virtual bool canHandleNullMask() const;
    virtual void process (int32_t& result, bool& resultMask,
			  const Vector<int32_t>& vector,
			  const Vector<bool>& arrayMask,
			  const IPosition& pos);
    virtual void multiProcess (Vector<int32_t>& result, Vector<bool>& resultMask,
			  const Vector<int32_t>& vector,
			  const Vector<bool>& arrayMask,
			  const IPosition& pos);
};
void MyLineCollapser::init (uint32_t nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == 1, AipsError);
}
bool MyLineCollapser::canHandleNullMask() const
{
    return false;
}
void MyLineCollapser::process (int32_t& result, bool& resultMask,
			       const Vector<int32_t>& vector,
			       const Vector<bool>& mask,
			       const IPosition&)
{
    DebugAssert (vector.nelements() == mask.nelements(), AipsError);
    int32_t sum = 0;
    bool fnd = false;
    uint32_t n = vector.nelements();
    for (uint32_t i=0; i<n; i++) {
	if (mask(i)) {
	    fnd = true;
	    sum += vector(i);
	}
    }
    result = sum;
    resultMask = fnd;
}
void MyLineCollapser::multiProcess (Vector<int32_t>& result,
				    Vector<bool>& resultMask,
				    const Vector<int32_t>& vector,
				    const Vector<bool>& mask,
				    const IPosition&)
{
    DebugAssert (vector.nelements() == mask.nelements(), AipsError);
    int32_t sum = 0;
    bool fnd = false;
    uint32_t n = vector.nelements();
    for (uint32_t i=0; i<n; i++) {
	if (mask(i)) {
	    fnd = true;
	    sum += vector(i);
	}
    }
    result.resize (2);
    resultMask.resize (2);
    result(0) = sum;
    result(1) = -result(0);
    resultMask(0) = resultMask(1) = fnd;
}


class MyTiledCollapser : public TiledCollapser<int32_t>
{
public:
    MyTiledCollapser() : itsSum1(0),itsSum2(0),itsNpts(0) {}
    virtual ~MyTiledCollapser();
    virtual void init (uint32_t nOutPixelsPerCollapse);
    virtual bool canHandleNullMask() const;
    virtual void initAccumulator (uint64_t n1, uint64_t n3);
    virtual void process (uint32_t index1, uint32_t index3,
			  const int32_t* inData, const bool* inMask,
			  uint32_t inDataIncr, uint32_t inMaskIncr, uint32_t nrval,
			  const IPosition& pos, const IPosition& shape);
    virtual void endAccumulator (Array<int32_t>& result,
				 Array<bool>& resultMask,
				 const IPosition& shape);
private:
    Matrix<uint32_t>* itsSum1;
    Block<int32_t>*   itsSum2;
    Matrix<uint32_t>* itsNpts;
    uint64_t        itsn1;
    uint64_t        itsn3;
};
MyTiledCollapser::~MyTiledCollapser()
{
    delete itsSum1;
    delete itsSum2;
    delete itsNpts;
}
void MyTiledCollapser::init (uint32_t nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == 2, AipsError);
}
void MyTiledCollapser::initAccumulator (uint64_t n1, uint64_t n3)
{
    itsSum1 = new Matrix<uint32_t> (n1, n3);
    itsSum2 = new Block<int32_t> (n1*n3);
    itsNpts = new Matrix<uint32_t> (n1, n3);
    itsSum1->set (0);
    itsSum2->set (0);
    itsNpts->set (0);
    itsn1 = n1;
    itsn3 = n3;
}
bool MyTiledCollapser::canHandleNullMask() const
{
    return false;
}
void MyTiledCollapser::process (uint32_t index1, uint32_t index3,
				const int32_t* inData, const bool* inMask,
				uint32_t inDataIncr, uint32_t inMaskIncr, uint32_t nrval,
				const IPosition&, const IPosition&)
{
    uint32_t& sum1 = (*itsSum1)(index1, index3);
    int32_t& sum2 = (*itsSum2)[index1 + index3*itsn1];
    uint32_t& npts = (*itsNpts)(index1, index3);
    for (uint32_t i=0; i<nrval; i++) {
	if (*inMask) {
	    sum1 += *inData;
	    sum2 -= *inData;
	    npts++;
	}
	inMask += inMaskIncr;
	inData += inDataIncr;
    }
}
void MyTiledCollapser::endAccumulator (Array<int32_t>& result,
				       Array<bool>& resultMask,
				       const IPosition& shape)
{
    result.resize (shape);
    resultMask.resize (shape);
    bool deleteRes, deleteSum1;
    bool deleteMask, deleteNpts;
    int32_t* res = result.getStorage (deleteRes);
    int32_t* resptr = res;
    bool* mask = resultMask.getStorage (deleteMask);
    bool* maskptr = mask;
    const uint32_t* sum1 = itsSum1->getStorage (deleteSum1);
    const uint32_t* sum1ptr = sum1;
    const int32_t* sum2ptr = itsSum2->storage();
    const uint32_t* npts = itsNpts->getStorage (deleteNpts);
    const uint32_t* nptsptr = npts;
    for (uint32_t i=0; i<itsn3; i++) {
	bool* maskptr2 = maskptr;
        for (uint32_t j=0; j<itsn1; j++) {
	    *resptr++ = int32_t(*sum1ptr++);
	    *maskptr++ = (*nptsptr++ != 0);
	}
	objcopy (resptr, sum2ptr, itsn1);
	resptr += itsn1;
	sum2ptr += itsn1;
	objcopy (maskptr, maskptr2, itsn1);
	maskptr += itsn1;
    }
    itsSum1->freeStorage (sum1, deleteSum1);
    itsNpts->freeStorage (npts, deleteNpts);
    result.putStorage (res, deleteRes);
    resultMask.putStorage (mask, deleteMask);
    delete itsSum1;
    itsSum1 = 0;
    delete itsSum2;
    itsSum2 = 0;
    delete itsNpts;
    itsNpts = 0;
}


class MyLatticeProgress : public LatticeProgress
{
public:
    MyLatticeProgress() : itsMeter(0) {}
    virtual ~MyLatticeProgress();
    virtual void initDerived();
    virtual void nstepsDone (uint32_t nsteps);
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
				 true, max(1,int32_t(expectedNsteps()/100)));
}
void MyLatticeProgress::nstepsDone (uint32_t nsteps)
{
    itsMeter->update (nsteps);
}
void MyLatticeProgress::done()
{
    delete itsMeter;
    itsMeter = 0;
}


void doIt (int argc, const char* argv[])
{
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "128", "Number of pixels along the x-axis", "int");
    inp.create("ny", "128", "Number of pixels along the y-axis", "int");
    inp.create("nz", "128", "Number of pixels along the z-axis", "int");
    inp.create("tx", "0", "Number of pixels along the x-axis tile", "int");
    inp.create("ty", "0", "Number of pixels along the y-axis tile", "int");
    inp.create("tz", "0", "Number of pixels along the z-axis tile", "int");
    inp.readArguments(argc, argv);

    const uint32_t nx=inp.getInt("nx");
    const uint32_t ny=inp.getInt("ny");
    const uint32_t nz=inp.getInt("nz");
    const uint32_t tx=inp.getInt("tx");
    const uint32_t ty=inp.getInt("ty");
    const uint32_t tz=inp.getInt("tz");
    IPosition latticeShape(3, nx, ny, nz);
    IPosition tileShape(3, tx, ty, tz);
    if (tileShape.product() == 0) {
	tileShape = TiledShape(latticeShape).tileShape();
    }
    cout << "Data Type: int32_t";
    cout << "  Lattice shape:" << latticeShape;
    cout << "  Tile shape:" << tileShape << endl;

    MyLatticeProgress showProgress;
    {
	SetupNewTable paSetup("tLatticeApply_tmp.array",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<int32_t> lat(TiledShape(latticeShape, tileShape), paTable);    
	Array<int32_t> arr(IPosition(3,nx,ny,1));
	indgen(arr);
	LatticeIterator<int32_t> iter(lat, LatticeStepper(latticeShape,
						      IPosition(3,nx,ny,1)));
	Timer tim;
	for (iter.reset(); !iter.atEnd(); iter++, arr += int32_t(nx*ny)) {
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
	PagedArray<int32_t> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array1",
			       TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<int32_t> arrout(TiledShape(l1Shape,t1Shape), paTable);
	SubLattice<int32_t> latout(arrout, true);
	MyLineCollapser collapser;
	Timer tim;
	LatticeApply<int32_t>::lineApply (latout, SubLattice<int32_t>(lat),
				      collapser, 2, &showProgress);
	tim.show("line 2     ");
    }
    {
	Table t("tLatticeApply_tmp.array1");
	PagedArray<int32_t> lat(t);
	int32_t sum = (nz-1)*nz/2*nx*ny;
	IPosition pos(3,0);
	Timer tim;
	for (int32_t i=0; i<l1Shape(1); i++) {
	    pos(1) = i;
	    for (int32_t j=0; j<l1Shape(0); j++) {
		pos(0) = j;
		int32_t value = lat.getAt (pos);
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
	PagedArray<int32_t> lat(t);
	SetupNewTable paSetup0("tLatticeApply_tmp.array2a",
			       TableDesc(), Table::New);
	Table paTable0(paSetup0);
	PagedArray<int32_t> arrout0(TiledShape(l2Shape, t2Shape), paTable0);
	SubLattice<int32_t> latout0(arrout0, true);
	SetupNewTable paSetup1("tLatticeApply_tmp.array2b",
			       TableDesc(), Table::New);
	Table paTable1(paSetup1);
	PagedArray<int32_t> arrout1(TiledShape(l2Shape, t2Shape), paTable1);
	SubLattice<int32_t> latout1(arrout1, true);
	PtrBlock<MaskedLattice<int32_t>*> blat(2);
	blat[0] = &latout0;
	blat[1] = &latout1;
	MyLineCollapser collapser;
	Timer tim;
	LatticeApply<int32_t>::lineMultiApply (blat, SubLattice<int32_t>(lat),
					   collapser, 0);
	tim.show("multiline 0");
    }
    {
	Table t("tLatticeApply_tmp.array2b");
	PagedArray<int32_t> lat(t);
	int32_t sum = (nx-1)*nx/2;
	IPosition pos(3,0);
	Timer tim;
	for (int32_t i=0; i<l2Shape(2); i++) {
	    pos(2) = i;
	    for (int32_t j=0; j<l2Shape(1); j++) {
		pos(1) = j;
		int32_t value = lat.getAt (pos);
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
	PagedArray<int32_t> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array2t",
			       TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<int32_t> arrout(TiledShape(l2Shape,t2Shape), paTable);
	SubLattice<int32_t> latout(arrout, true);
	MyTiledCollapser collapser;
	Timer tim;
	LatticeApply<int32_t>::tiledApply (latout, SubLattice<int32_t>(lat),
				       collapser, IPosition(1,0));
	tim.show("tiled 0    ");
    }
    {
	Table t("tLatticeApply_tmp.array2t");
	PagedArray<int32_t> lat(t);
	int32_t sum = (nx-1)*nx/2;
	IPosition pos(3,0);
	Timer tim;
	for (int32_t i=0; i<l2Shape(2); i++) {
	    pos(2) = i;
	    for (int32_t j=0; j<l2Shape(1); j++) {
		pos(1) = j;
		pos(0) = 0;
		int32_t value = lat.getAt (pos);
		pos(0) = 1;
		int32_t value1 = lat.getAt (pos);
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
    double s1 = nx*nz;
    double s2 = s1/2 * (nx-1);
    double s3 = s1/2 * nx*ny*(nz-1);
    double s4 = s1*s1;
    if (s2+s3+s4 < double(32768)*65536) {
	{
	    Table t("tLatticeApply_tmp.array");
	    PagedArray<int32_t> lat(t);
	    SetupNewTable paSetup("tLatticeApply_tmp.array2tb",
				  TableDesc(), Table::New);
	    Table paTable(paSetup);
	    PagedArray<int32_t> arrout(l2Shape, paTable);
	    SubLattice<int32_t> latout(arrout, true);
	    MyTiledCollapser collapser;
	    Timer tim;
	    LatticeApply<int32_t>::tiledApply (latout, SubLattice<int32_t>(lat),
					   collapser, IPosition(2,0,2));
	    tim.show("tiled 0,2  ");
	}
	{
	    Table t("tLatticeApply_tmp.array2tb");
	    PagedArray<int32_t> lat(t);
	    int32_t sum = nz*(nx-1)*nx/2 + nx*nx*ny*(nz-1)*nz/2;
	    IPosition pos(3,0);
	    Timer tim;
	    for (int32_t j=0; j<l2Shape(2); j++) {
		pos(2) = j;
		pos(0) = 0;
		int32_t value = lat.getAt (pos);
		pos(0) = 1;
		int32_t value1 = lat.getAt (pos);
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
	    PagedArray<int32_t> lat(t);
	    SetupNewTable paSetup("tLatticeApply_tmp.array2tc",
				  TableDesc(), Table::New);
	    Table paTable(paSetup);
	    PagedArray<int32_t> arrout(l2Shape, paTable);
	    SubLattice<int32_t> latout(arrout, true);
	    MyTiledCollapser collapser;
	    Timer tim;
	    LatticeApply<int32_t>::tiledApply (latout, SubLattice<int32_t>(lat),
					   collapser, IPosition(3,0,1,2));
	    tim.show("tiled 0,1,2");
	}
	{
	    Table t("tLatticeApply_tmp.array2tc");
	    PagedArray<int32_t> lat(t);
	    //# Use uint32_t to avoid possible overflow.
	    uint32_t s = nx*ny*nz;
	    s = s * (s-1) / 2;
	    int32_t sum = s;
	    IPosition pos(3,0);
	    Timer tim;
	    int32_t value = lat.getAt (pos);
	    pos(0) = 1;
	    int32_t value1 = lat.getAt (pos);
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
	PagedArray<int32_t> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array3",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<int32_t> arrout(l3Shape, paTable);
	SubLattice<int32_t> latout(arrout, true);
	MyLineCollapser collapser;
	LatticeRegion region (slicer3, lat.shape());
	Timer tim;
	LatticeApply<int32_t>::lineApply (latout, SubLattice<int32_t>(lat),
				      region, collapser, 1);
	tim.show("lsliced 1  ");
    }
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<int32_t> lat(t);
	SetupNewTable paSetup0("tLatticeApply_tmp.array4a",
			       TableDesc(), Table::New);
	Table paTable0(paSetup0);
	PagedArray<int32_t> arrout0(l3Shape, paTable0);
	SubLattice<int32_t> latout0(arrout0, true);
	SetupNewTable paSetup1("tLatticeApply_tmp.array4b",
			       TableDesc(), Table::New);
	Table paTable1(paSetup1);
	PagedArray<int32_t> arrout1(l3Shape, paTable1);
	SubLattice<int32_t> latout1(arrout1, true);
	PtrBlock<MaskedLattice<int32_t>*> blat(2);
	blat[0] = &latout0;
	blat[1] = &latout1;
	MyLineCollapser collapser;
	LatticeRegion region (slicer3, lat.shape());
	Timer tim;
	LatticeApply<int32_t>::lineMultiApply (blat, SubLattice<int32_t>(lat),
					   region, collapser, 1);
	tim.show("msliced 1  ");
    }
    IPosition l5Shape(5,1);
    l5Shape(0) = slicer3.length()(0);
    l5Shape(2) = 2;
    l5Shape(4) = slicer3.length()(2);
    {
	Table t("tLatticeApply_tmp.array");
	PagedArray<int32_t> lat(t);
	SetupNewTable paSetup("tLatticeApply_tmp.array5t",
			      TableDesc(), Table::New);
	Table paTable(paSetup);
	PagedArray<int32_t> arrout(l5Shape, paTable);
	SubLattice<int32_t> latout(arrout, true);
	MyTiledCollapser collapser;
	LatticeRegion region (slicer3, lat.shape());
	Timer tim;
	LatticeApply<int32_t>::tiledApply (latout, SubLattice<int32_t>(lat),
				       region, collapser, IPosition(1,1));
	tim.show("tsliced 1  ");
    }
    {
	Table t("tLatticeApply_tmp.array3");
	PagedArray<int32_t> lat(t);
	Table t1("tLatticeApply_tmp.array4a");
	PagedArray<int32_t> lat1(t1);
	Table t2("tLatticeApply_tmp.array5t");
	PagedArray<int32_t> lat2(t2);
	int32_t sx = slicer3.start()(0);
	int32_t sy = slicer3.start()(1);
	int32_t sz = slicer3.start()(2);
	int32_t mx = slicer3.length()(0);
	int32_t my = slicer3.length()(1);
	int32_t mz = slicer3.length()(2);
	int32_t ix = slicer3.stride()(0);
	int32_t iy = slicer3.stride()(1);
	int32_t iz = slicer3.stride()(2);
	int32_t iniSum = (my-1) * iy * my / 2 * nx + (sy*nx + sx) * my;
	IPosition pos(2,0);
	IPosition pos2(5,0);
	Timer tim;
	for (int32_t i=0; i<mz; i++) {
	    int32_t sum = (sz+i*iz)*nx*ny*my + iniSum;
	    pos(1) = i;
	    pos2(4) = i;
	    for (int32_t j=0; j<mx; j++) {
		pos(0) = j;
		pos2(0) = j;
		int32_t value = lat.getAt (pos);
		int32_t value1 = lat1.getAt (pos);
		pos2(2) = 0;
		int32_t value2p = lat2.getAt (pos2);
		pos2(2) = 1;
		int32_t value2m = lat2.getAt (pos2);
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


int main (int argc, const char* argv[])
{
    try {
	doIt (argc,argv);
	cout<< "OK"<< endl;
	return 0;
    } catch (std::exception& x) {
	cerr << "Caught exception: " << x.what() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
