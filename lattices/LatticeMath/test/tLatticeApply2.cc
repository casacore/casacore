// # tLatticeApply2.cc: Test program for class LatticeApply
// # Copyright (C) 1997,1998,1999,2000,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This program is free software; you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by the Free
// # Software Foundation; either version 2 of the License, or (at your option)
// # any later version.
// #
// # This program is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// # more details.
// #
// # You should have received a copy of the GNU General Public License along
// # with this program; if not, write to the Free Software Foundation, Inc.,
// # 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/lattices/LatticeMath/LatticeApply.h>

#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/TiledShape.h>
//
#include <casacore/lattices/LatticeMath/LineCollapser.h>
#include <casacore/lattices/LatticeMath/TiledCollapser.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/LatticeMath/LatticeProgress.h>
#include <casacore/casa/System/ProgressMeter.h>
//
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
class MyLineCollapser : public LineCollapser<float> {
 public:
  MyLineCollapser() {}
  virtual void init(unsigned int nOutPixelsPerCollapse);
  virtual bool canHandleNullMask() const;
  virtual void process(float& result, bool& resultMask, const Vector<float>& vector,
                       const Vector<bool>& arrayMask, const IPosition& pos);
  virtual void multiProcess(Vector<float>& result, Vector<bool>& resultMask,
                            const Vector<float>& vector, const Vector<bool>& arrayMask,
                            const IPosition& pos);
};
void MyLineCollapser::init(unsigned int nOutPixelsPerCollapse) {
  AlwaysAssert(nOutPixelsPerCollapse == 1, AipsError);
}
bool MyLineCollapser::canHandleNullMask() const { return false; }
void MyLineCollapser::process(float& result, bool& resultMask, const Vector<float>& vector,
                              const Vector<bool>& mask, const IPosition&) {
  DebugAssert(vector.nelements() == mask.nelements(), AipsError);
  float sum = 0;
  bool fnd = false;
  unsigned int n = vector.nelements();
  for (unsigned int i = 0; i < n; i++) {
    if (mask(i)) {
      fnd = true;
      sum += vector(i);
    }
  }
  result = sum;
  resultMask = fnd;
}
void MyLineCollapser::multiProcess(Vector<float>& result, Vector<bool>& resultMask,
                                   const Vector<float>& vector, const Vector<bool>& mask,
                                   const IPosition&) {
  DebugAssert(vector.nelements() == mask.nelements(), AipsError);
  float sum = 0;
  bool fnd = false;
  unsigned int n = vector.nelements();
  for (unsigned int i = 0; i < n; i++) {
    if (mask(i)) {
      fnd = true;
      sum += vector(i);
    }
  }
  result.resize(2);
  resultMask.resize(2);
  result(0) = sum;
  result(1) = -result(0);
  resultMask(0) = resultMask(1) = fnd;
}

class MyTiledCollapser : public TiledCollapser<float> {
 public:
  MyTiledCollapser() : itsSum1(nullptr), itsSum2(nullptr), itsNpts(nullptr) {}
  virtual ~MyTiledCollapser();
  virtual void init(unsigned int nOutPixelsPerCollapse);
  virtual bool canHandleNullMask() const;
  virtual void initAccumulator(uint64_t n1, uint64_t n3);
  virtual void process(unsigned int index1, unsigned int index3, const float* inData,
                       const bool* inMask, unsigned int inDataIncr, unsigned int inMaskIncr,
                       unsigned int nrval, const IPosition& pos, const IPosition& shape);
  virtual void endAccumulator(Array<float>& result, Array<bool>& resultMask,
                              const IPosition& shape);

 private:
  Matrix<float>* itsSum1;
  Block<float>* itsSum2;
  Matrix<unsigned int>* itsNpts;
  unsigned int itsn1;
  unsigned int itsn3;
};
MyTiledCollapser::~MyTiledCollapser() {
  delete itsSum1;
  delete itsSum2;
  delete itsNpts;
}
void MyTiledCollapser::init(unsigned int nOutPixelsPerCollapse) {
  AlwaysAssert(nOutPixelsPerCollapse == 2, AipsError);
}
void MyTiledCollapser::initAccumulator(uint64_t n1, uint64_t n3) {
  itsSum1 = new Matrix<float>(n1, n3);
  itsSum2 = new Block<float>(n1 * n3);
  itsNpts = new Matrix<unsigned int>(n1, n3);
  itsSum1->set(0.0);
  itsSum2->set(0.0);
  itsNpts->set(0);
  itsn1 = n1;
  itsn3 = n3;
}
bool MyTiledCollapser::canHandleNullMask() const { return false; }
void MyTiledCollapser::process(unsigned int index1, unsigned int index3, const float* inData,
                               const bool* inMask, unsigned int inDataIncr, unsigned int inMaskIncr,
                               unsigned int nrval, const IPosition&, const IPosition&) {
  float& sum1 = (*itsSum1)(index1, index3);
  float& sum2 = (*itsSum2)[index1 + index3 * itsn1];
  unsigned int& npts = (*itsNpts)(index1, index3);
  for (unsigned int i = 0; i < nrval; i++) {
    if (*inMask) {
      sum1 += *inData;
      sum2 -= *inData;
      npts++;
    }
    inMask += inMaskIncr;
    inData += inDataIncr;
  }
}
void MyTiledCollapser::endAccumulator(Array<float>& result, Array<bool>& resultMask,
                                      const IPosition& shape) {
  result.resize(shape);
  resultMask.resize(shape);
  bool deleteRes, deleteSum1;
  bool deleteMask, deleteNpts;
  float* res = result.getStorage(deleteRes);
  float* resptr = res;
  bool* mask = resultMask.getStorage(deleteMask);
  bool* maskptr = mask;
  const float* sum1 = itsSum1->getStorage(deleteSum1);
  const float* sum1ptr = sum1;
  const float* sum2ptr = itsSum2->storage();
  const unsigned int* npts = itsNpts->getStorage(deleteNpts);
  const unsigned int* nptsptr = npts;
  for (unsigned int i = 0; i < itsn3; i++) {
    bool* maskptr2 = maskptr;
    for (unsigned int j = 0; j < itsn1; j++) {
      *resptr++ = *sum1ptr++;
      *maskptr++ = (*nptsptr++ != 0);
    }
    objcopy(resptr, sum2ptr, itsn1);
    resptr += itsn1;
    sum2ptr += itsn1;
    objcopy(maskptr, maskptr2, itsn1);
    maskptr += itsn1;
  }
  itsSum1->freeStorage(sum1, deleteSum1);
  itsNpts->freeStorage(npts, deleteNpts);
  result.putStorage(res, deleteRes);
  resultMask.putStorage(mask, deleteMask);
  delete itsSum1;
  itsSum1 = nullptr;
  delete itsSum2;
  itsSum2 = nullptr;
  delete itsNpts;
  itsNpts = nullptr;
}

class MyLatticeProgress : public LatticeProgress {
 public:
  MyLatticeProgress() : itsMeter(nullptr) {}
  virtual ~MyLatticeProgress();
  virtual void initDerived();
  virtual void nstepsDone(unsigned int nsteps);
  virtual void done();

 private:
  ProgressMeter* itsMeter;
};
MyLatticeProgress::~MyLatticeProgress() { delete itsMeter; }
void MyLatticeProgress::initDerived() {
  delete itsMeter;
  itsMeter = new ProgressMeter(0.0, expectedNsteps(), "tLatticeApply", "Vectors extracted", "", "",
                               true, max(1, int(expectedNsteps() / 100)));
}
void MyLatticeProgress::nstepsDone(unsigned int nsteps) { itsMeter->update(nsteps); }
void MyLatticeProgress::done() {
  delete itsMeter;
  itsMeter = nullptr;
}

void doIt(int argc, const char* argv[]) {
  Input inp(1);
  inp.version(" ");
  inp.create("nx", "32", "Number of pixels along the x-axis", "int");
  inp.create("ny", "32", "Number of pixels along the y-axis", "int");
  inp.create("nz", "64", "Number of pixels along the z-axis", "int");
  inp.create("tx", "0", "Number of pixels along the x-axis tile", "int");
  inp.create("ty", "0", "Number of pixels along the y-axis tile", "int");
  inp.create("tz", "0", "Number of pixels along the z-axis tile", "int");
  inp.readArguments(argc, argv);

  const unsigned int nx = inp.getInt("nx");
  const unsigned int ny = inp.getInt("ny");
  const unsigned int nz = inp.getInt("nz");
  const unsigned int tx = inp.getInt("tx");
  const unsigned int ty = inp.getInt("ty");
  const unsigned int tz = inp.getInt("tz");
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
    //
    // Make a ML with the corner x profiles all false
    //
    ArrayLattice<float> lat(latticeShape);
    ArrayLattice<bool> mask(latticeShape);
    mask.set(true);
    //
    Array<bool> slice(IPosition(3, nx, 1, 1));
    slice = false;
    mask.putSlice(slice, IPosition(3, 0, 0, 0));
    mask.putSlice(slice, IPosition(3, 0, 0, nz - 1));
    mask.putSlice(slice, IPosition(3, 0, ny - 1, nz - 1));
    mask.putSlice(slice, IPosition(3, 0, ny - 1, 0));
    SubLattice<float> mLat(lat, true);
    mLat.setPixelMask(mask, false);
    //
    Array<float> arr(IPosition(3, nx, ny, 1));
    indgen(arr);
    LatticeIterator<float> iter(mLat, LatticeStepper(latticeShape, IPosition(3, nx, ny, 1)));
    Timer tim;
    for (iter.reset(); !iter.atEnd(); iter++, arr += float(nx * ny)) {
      iter.woCursor() = arr;
    }
    tim.show("fill       ");
    //
    IPosition l2Shape(latticeShape);
    IPosition t2Shape(tileShape);
    l2Shape(0) = 1;
    t2Shape(0) = 1;
    //
    ArrayLattice<float> lat0(l2Shape);
    SubLattice<float> mLatOut0(lat0, true);
    ArrayLattice<bool> mask0(l2Shape);
    mask0.set(true);
    mLatOut0.setPixelMask(mask0, false);
    //
    ArrayLattice<float> lat1(l2Shape);
    SubLattice<float> mLatOut1(lat1, true);
    ArrayLattice<bool> mask1(l2Shape);
    mask0.set(true);
    mLatOut1.setPixelMask(mask1, false);
    //
    Block<MaskedLattice<float>*> blat(2);
    blat[0] = &mLatOut0;
    blat[1] = &mLatOut1;
    MyLineCollapser collapser;
    tim.mark();
    LatticeApply<float>::lineMultiApply(blat, mLat, collapser, 0);
    tim.show("multiline 0");
    //
    float sum = (nx - 1) * nx / 2;
    IPosition pos(3, 0);
    tim.mark();
    for (unsigned int i = 0; i < nz; i++) {
      pos(2) = i;
      for (unsigned int j = 0; j < ny; j++) {
        pos(1) = j;
        float value = mLatOut1.getAt(pos);
        float expval = -sum;
        if ((i == 0 || i == nz - 1) && (j == 0 || j == ny - 1)) {
          expval = 0;
        }
        if (value != expval) {
          cout << "Value=" << value << ", expected " << expval << "   at position " << pos << endl;
        }
        sum += float(nx * nx);
      }
    }
    tim.show("check      ");
  }
}

int main(int argc, const char* argv[]) {
  try {
    doIt(argc, argv);
    cout << "OK" << endl;
    return 0;
  } catch (std::exception& x) {
    cerr << "Caught exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
