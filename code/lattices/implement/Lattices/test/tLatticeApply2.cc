//# tLatticeApply2.cc: Test program for class LatticeApply
//# Copyright (C) 1997,1998,1999,2000
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

#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/LineCollapser.h>
#include <trial/Lattices/TiledCollapser.h>
#include <aips/Lattices/PagedArray.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCPagedMask.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeProgress.h>
#include <trial/Lattices/LatticeRegion.h>
#include <trial/Lattices/MaskedLattice.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/RegionHandler.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Tasking/ProgressMeter.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Inputs/Input.h>
#include <aips/OS/Timer.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


class MyLineCollapser : public LineCollapser<Float>
{
public:
    MyLineCollapser() {};
    virtual void init (uInt nOutPixelsPerCollapse);
    virtual Bool canHandleNullMask() const;
    virtual void process (Float& result, Bool& resultMask,
			  const Vector<Float>& vector,
			  const Vector<Bool>& arrayMask,
			  const IPosition& pos);
    virtual void multiProcess (Vector<Float>& result, Vector<Bool>& resultMask,
			  const Vector<Float>& vector,
			  const Vector<Bool>& arrayMask,
			  const IPosition& pos);
};
void MyLineCollapser::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == 1, AipsError);
}
Bool MyLineCollapser::canHandleNullMask() const
{
    return False;
}
void MyLineCollapser::process (Float& result, Bool& resultMask,
			       const Vector<Float>& vector,
			       const Vector<Bool>& mask,
			       const IPosition&)
{
    DebugAssert (vector.nelements() == mask.nelements(), AipsError);
    Float sum = 0;
    Bool fnd = False;
    uInt n = vector.nelements();
    for (uInt i=0; i<n; i++) {
	if (mask(i)) {
	    fnd = True;
	    sum += vector(i);
	}
    }
    result = sum;
    resultMask = fnd;
}
void MyLineCollapser::multiProcess (Vector<Float>& result,
				    Vector<Bool>& resultMask,
				    const Vector<Float>& vector,
				    const Vector<Bool>& mask,
				    const IPosition&)
{
    DebugAssert (vector.nelements() == mask.nelements(), AipsError);
    Float sum = 0;
    Bool fnd = False;
    uInt n = vector.nelements();
    for (uInt i=0; i<n; i++) {
	if (mask(i)) {
	    fnd = True;
	    sum += vector(i);
	}
    }
    result.resize (2);
    resultMask.resize (2);
    result(0) = sum;
    result(1) = -result(0);
    resultMask(0) = resultMask(1) = fnd;
}


class MyTiledCollapser : public TiledCollapser<Float>
{
public:
    MyTiledCollapser() : itsSum1(0),itsSum2(0),itsNpts(0) {};
    virtual ~MyTiledCollapser();
    virtual void init (uInt nOutPixelsPerCollapse);
    virtual Bool canHandleNullMask() const;
    virtual void initAccumulator (uInt n1, uInt n3);
    virtual void process (uInt index1, uInt index3,
			  const Float* inData, const Bool* inMask,
			  uInt inIncr, uInt nrval,
			  const IPosition& pos, const IPosition& shape);
    virtual void endAccumulator (Array<Float>& result,
				 Array<Bool>& resultMask,
				 const IPosition& shape);
private:
    Matrix<Float>* itsSum1;
    Block<Float>*   itsSum2;
    Matrix<uInt>* itsNpts;
    uInt          itsn1;
    uInt          itsn3;
};
MyTiledCollapser::~MyTiledCollapser()
{
    delete itsSum1;
    delete itsSum2;
    delete itsNpts;
}
void MyTiledCollapser::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == 2, AipsError);
}
void MyTiledCollapser::initAccumulator (uInt n1, uInt n3)
{
    itsSum1 = new Matrix<Float> (n1, n3);
    itsSum2 = new Block<Float> (n1*n3);
    itsNpts = new Matrix<uInt> (n1, n3);
    itsSum1->set (0.0);
    itsSum2->set (0.0);
    itsNpts->set (0);
    itsn1 = n1;
    itsn3 = n3;
}
Bool MyTiledCollapser::canHandleNullMask() const
{
    return False;
}
void MyTiledCollapser::process (uInt index1, uInt index3,
				const Float* inData, const Bool* inMask,
				uInt inIncr, uInt nrval,
				const IPosition&, const IPosition&)
{
    Float& sum1 = (*itsSum1)(index1, index3);
    Float& sum2 = (*itsSum2)[index1 + index3*itsn1];
    uInt& npts = (*itsNpts)(index1, index3);
    for (uInt i=0; i<nrval; i++) {
	if (*inMask) {
	    sum1 += *inData;
	    sum2 -= *inData;
	    npts++;
	}
	inMask += inIncr;
	inData += inIncr;
    }
}
void MyTiledCollapser::endAccumulator (Array<Float>& result,
				       Array<Bool>& resultMask,
				       const IPosition& shape)
{
    result.resize (shape);
    resultMask.resize (shape);
    Bool deleteRes, deleteSum1;
    Bool deleteMask, deleteNpts;
    Float* res = result.getStorage (deleteRes);
    Float* resptr = res;
    Bool* mask = resultMask.getStorage (deleteMask);
    Bool* maskptr = mask;
    const Float* sum1 = itsSum1->getStorage (deleteSum1);
    const Float* sum1ptr = sum1;
    const Float* sum2ptr = itsSum2->storage();
    const uInt* npts = itsNpts->getStorage (deleteNpts);
    const uInt* nptsptr = npts;
    for (uInt i=0; i<itsn3; i++) {
	Bool* maskptr2 = maskptr;
        for (uInt j=0; j<itsn1; j++) {
	    *resptr++ = *sum1ptr++;
	    *maskptr++ = ToBool(*nptsptr++ != 0);
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
    inp.version(" ");
    inp.create("nx", "32", "Number of pixels along the x-axis", "int");
    inp.create("ny", "32", "Number of pixels along the y-axis", "int");
    inp.create("nz", "64", "Number of pixels along the z-axis", "int");
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
    IPosition latticeShape(3, nx, ny, nz);
    IPosition tileShape(3, tx, ty, tz);
    if (tileShape.product() == 0) {
	tileShape = TiledShape(latticeShape).tileShape();
    }
    cout << "Data Type: Float";
    cout << "  Lattice shape:" << latticeShape;
    cout << "  Tile shape:" << tileShape << endl;

    MyLatticeProgress showProgress;
    {
        PagedImage<Float> image(TiledShape(latticeShape, tileShape), 
                                CoordinateUtil::defaultCoords3D(),
                                "tLatticeApply2_tmp.image");
//
// Make mask and make the corner x profiles all False
//
        LCPagedMask mask(RegionHandler::makeMask (image, "mask0"));
        mask.set(True);
//
        Array<Bool> slice(IPosition(3,nx,1,1));
        slice = False;
        mask.putSlice(slice, IPosition(3,0,0,0));
        mask.putSlice(slice, IPosition(3,0,0,nz-1));
        mask.putSlice(slice, IPosition(3,0,ny-1,nz-1));
        mask.putSlice(slice, IPosition(3,0,ny-1,0));
//
        image.defineRegion ("mask0", ImageRegion(mask), RegionHandler::Masks);
        image.setDefaultMask("mask0");
//
	Array<Float> arr(IPosition(3,nx,ny,1));
	indgen(arr);
	LatticeIterator<Float> iter(image, LatticeStepper(latticeShape,
                                    IPosition(3,nx,ny,1)));
	Timer tim;
	for (iter.reset(); !iter.atEnd(); iter++, arr += Float(nx*ny)) {
	    iter.woCursor() = arr;
	}
	tim.show("fill       ");
    }
    IPosition l2Shape (latticeShape);
    IPosition t2Shape (tileShape);
    l2Shape(0) = 1;
    t2Shape(0) = 1;
    {
	PagedImage<Float> lat("tLatticeApply2_tmp.image");
//
        PagedImage<Float> latout0(TiledShape(l2Shape, t2Shape), 
                                  CoordinateUtil::defaultCoords3D(),
                                "tLatticeApply2_tmp.image2a");
        LCPagedMask mask0(RegionHandler::makeMask (latout0, "mask0"));
        latout0.defineRegion ("mask0", ImageRegion(mask0),
			      RegionHandler::Masks);
        latout0.setDefaultMask("mask0");
//
        PagedImage<Float> latout1(TiledShape(l2Shape, t2Shape), 
                                  CoordinateUtil::defaultCoords3D(),
                                "tLatticeApply2_tmp.image2b");
        LCPagedMask mask1(RegionHandler::makeMask (latout1, "mask0"));
        latout1.defineRegion ("mask0", ImageRegion(mask1),
			      RegionHandler::Masks);
        latout1.setDefaultMask("mask0");
//
	PtrBlock<MaskedLattice<Float>*> blat(2);
	blat[0] = &latout0;
	blat[1] = &latout1;
	MyLineCollapser collapser;
	Timer tim;
	LatticeApply<Float>::lineMultiApply (blat, lat, collapser, 0);
	tim.show("multiline 0");
    }
    {
	PagedImage<Float> lat("tLatticeApply2_tmp.image2b");
	Float sum = (nx-1)*nx/2;
	IPosition pos(3,0);
	Timer tim;
	for (uInt i=0; i<nz; i++) {
	    pos(2) = i;
	    for (uInt j=0; j<ny; j++) {
		pos(1) = j;
		Float value = lat.getAt (pos);
		Float expval = -sum;
		if ((i==0 || i==nz-1)  &&  (j==0 || j==ny-1)) {
		    expval = 0;
		}
		if (value != expval) {
		    cout << "Value=" << value << ", expected " << expval
			 << "   at position " << pos << endl;
		}
		sum += Float(nx*nx);
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
    } 
    cout << "OK" << endl;
    return 0;
}
