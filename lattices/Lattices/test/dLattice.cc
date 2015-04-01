//# dLattice.cc:  illustrates the functions discused in the docs of Lattice.h
//# Copyright (C) 1997,1998,1999,2000,2002
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/scimath/Mathematics/FFTServer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Complex latMean(const Lattice<Complex> & lat) {
  Complex currentSum = 0.0f;
  uInt nPixels = 0u;
  RO_LatticeIterator<Complex> iter(lat);
  for (iter.reset(); !iter.atEnd(); iter++){
    currentSum += sum(iter.cursor());    // 
    nPixels += iter.cursor().nelements();
  }
  return currentSum/Float(nPixels);
}

void FFT2DReal2Complex(Lattice<Complex> & result, 
		       const Lattice<Float> & input){
  AlwaysAssert(input.ndim() == 4, AipsError);
  const IPosition shape = input.shape();
  const uInt nx = shape(0);
  AlwaysAssert (nx > 1, AipsError);
  const uInt ny = shape(1);
  AlwaysAssert (ny > 1, AipsError);
  const uInt npol = shape(2);
  const uInt nchan = shape(3); const IPosition resultShape = result.shape();
  AlwaysAssert(resultShape.nelements() == 4, AipsError);
  AlwaysAssert(resultShape(3) == Int(nchan), AipsError);
  AlwaysAssert(resultShape(2) == Int(npol), AipsError);
  AlwaysAssert(resultShape(1) == Int(ny), AipsError);
  AlwaysAssert(resultShape(0) == Int(nx/2 + 1), AipsError);

  const IPosition inputSliceShape(4,nx,ny,1,1);
  const IPosition resultSliceShape(4,nx/2+1,ny,1,1);
  COWPtr<Array<Float> > 
    inputArrPtr(new Array<Float>(inputSliceShape.nonDegenerate()));
  Array<Complex> resultArray(resultSliceShape.nonDegenerate());
  FFTServer<Float, Complex> FFT2D(inputSliceShape.nonDegenerate());
  
  IPosition start(4,0);
  for (uInt c = 0; c < nchan; c++){
    for (uInt p = 0; p < npol; p++){
      input.getSlice(inputArrPtr, Slicer(start,inputSliceShape), True);
      FFT2D.fft(resultArray, *inputArrPtr);
      result.putSlice(resultArray, start);
      start(2) += 1;
    }
    start(2) = 0;
    start(3) += 1;
  }
}

void makePsf(Lattice<Float> & psf) {
  const IPosition centrePos = psf.shape()/2;
  psf.set(0.0f);       // this sets all the elements to zero
                       // As it uses a LatticeIterator it is efficient
  psf.putAt(1, centrePos);  // This sets just the centre element to one
  AlwaysAssert(near(psf(centrePos), 1.0f, 1E-6), AipsError);
  AlwaysAssert(near(psf(centrePos*0), 0.0f, 1E-6), AipsError);
}

int main() {
  try {
    const IPosition psfShape(4,4,4,2,3);
    ArrayLattice<Float> psf(psfShape) ;
    makePsf(psf);
    IPosition xfrShape(psfShape);
    xfrShape(0) = psfShape(0)/2 + 1;
    SetupNewTable xfrSetup("dLattice_tmp_xfr.array", TableDesc(), 
			   Table::Scratch);
    Table xfrTable(xfrSetup);
    PagedArray<Complex> xfr(xfrShape, xfrTable);
    FFT2DReal2Complex(xfr, psf);
    AlwaysAssert(near(latMean(xfr), 
		      Complex(1.0)/Float(psfShape(2)*psfShape(3)), 1E-6), 
		 AipsError);
  } catch (AipsError x) {
    cout << x.getMesg() << endl << "FAIL" << endl;		
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 dLattice"
// End: 
