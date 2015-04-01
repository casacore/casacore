// -*- C++ -*-
//# LatticeFFT.cc: functions for doing FFT's on Lattices.
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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
//#
//# $Id$

#include <casacore/lattices/LatticeMath/LatticeFFT.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Mathematics/FFTServer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/TiledLineStepper.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

void LatticeFFT::cfft2d(Lattice<Complex>& cLattice, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 1, AipsError);
  const IPosition& latticeShape = cLattice.shape();
  const uInt maxPixels = cLattice.advisedMaxPixels();
  IPosition slabShape = cLattice.niceCursorShape(maxPixels);
  const uInt nx = slabShape(0) = latticeShape(0);
  const uInt ny = slabShape(1) = latticeShape(1);
  // use 1/8 of memory for FFT of a plane at most 
  //Long cacheSize = (HostInfo::memoryTotal()/(sizeof(Complex)*8))*1024;
  //use memory Free  and use a quarter of that
  Long cacheSize = (HostInfo::memoryFree()/(sizeof(Complex)*4))*1024;

  // For small transforms, we do everything in one plane
  if (((Long)(nx)*(Long)(ny)) <= cacheSize) {
    const IPosition cursorShape(2, nx, ny);
    LatticeStepper ls(latticeShape, cursorShape);
    LatticeIterator<Complex> li(cLattice, ls);
    FFTServer<Float,Complex> ffts(cursorShape);
    for (li.reset(); !li.atEnd(); li++) {
      ffts.fft(li.rwMatrixCursor(), toFrequency);
    }
  } // For large transforms , we do line by line FFT's
  else {
    Vector<Bool> whichAxes(ndim, False);
    whichAxes(0) = whichAxes(1) = True;
    LatticeFFT::cfft(cLattice, whichAxes, toFrequency);
  }
}

void LatticeFFT::cfft2d(Lattice<DComplex>& cLattice, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 1, AipsError);
  const IPosition& latticeShape = cLattice.shape();
  const uInt maxPixels = cLattice.advisedMaxPixels();
  IPosition slabShape = cLattice.niceCursorShape(maxPixels);
  const uInt nx = slabShape(0) = latticeShape(0);
  const uInt ny = slabShape(1) = latticeShape(1);
  // use 1/8 of memory for FFT of a plane at most 
  //Long cacheSize = (HostInfo::memoryTotal()/(sizeof(Complex)*8))*1024;
  //use memory Free  and use a quarter of that
  Long cacheSize = (HostInfo::memoryFree()/(sizeof(DComplex)*4))*1024;


  // For small transforms, we do everything in one plane
  if (((Long)(nx)*(Long)(ny)) <= cacheSize) {
    const IPosition cursorShape(2, nx, ny);
    LatticeStepper ls(latticeShape, cursorShape);
    LatticeIterator<DComplex> li(cLattice, ls);
    FFTServer<Double,DComplex> ffts(cursorShape);
    for (li.reset(); !li.atEnd(); li++) {
      ffts.fft(li.rwMatrixCursor(), toFrequency);
    }
  } // For large transforms , we do line by line FFT's
  else {
    Vector<Bool> whichAxes(ndim, False);
    whichAxes(0) = whichAxes(1) = True;
    LatticeFFT::cfft(cLattice, whichAxes, toFrequency);
  }
}

void LatticeFFT::cfft(Lattice<Complex>& cLattice,
		     const Vector<Bool>& whichAxes, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(ndim == whichAxes.nelements(), AipsError);
  FFTServer<Float,Complex> ffts;
  const IPosition latticeShape = cLattice.shape();
  const IPosition tileShape = cLattice.niceCursorShape();

  for (uInt dim = 0; dim < ndim; dim++) {
    if (whichAxes(dim) == True) {
      TiledLineStepper ts(latticeShape, tileShape, dim);
      LatticeIterator<Complex> li(cLattice, ts);
      for (li.reset(); !li.atEnd(); li++) {
	ffts.fft(li.rwVectorCursor(), toFrequency);
      }
    }
  }
}

void LatticeFFT::cfft0(Lattice<Complex>& cLattice,
		       const Vector<Bool>& whichAxes, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(ndim == whichAxes.nelements(), AipsError);
  FFTServer<Float,Complex> ffts;
  const IPosition latticeShape = cLattice.shape();
  const IPosition tileShape = cLattice.niceCursorShape();

  for (uInt dim = 0; dim < ndim; dim++) {
    if (whichAxes(dim) == True) {
      TiledLineStepper ts(latticeShape, tileShape, dim);
      LatticeIterator<Complex> li(cLattice, ts);
      for (li.reset(); !li.atEnd(); li++) {
	ffts.fft0(li.rwVectorCursor(), toFrequency);
      }
    }
  }
}

void LatticeFFT::cfft(Lattice<DComplex>& cLattice,
		     const Vector<Bool>& whichAxes, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(ndim == whichAxes.nelements(), AipsError);
  FFTServer<Double,DComplex> ffts;
  const IPosition latticeShape = cLattice.shape();
  const IPosition tileShape = cLattice.niceCursorShape();

  for (uInt dim = 0; dim < ndim; dim++) {
    if (whichAxes(dim) == True) {
      TiledLineStepper ts(latticeShape, tileShape, dim);
      LatticeIterator<DComplex> li(cLattice, ts);
      for (li.reset(); !li.atEnd(); li++) {
	ffts.fft(li.rwVectorCursor(), toFrequency);
      }
    }
  }
}

void LatticeFFT::cfft(Lattice<Complex>& cLattice, const Bool toFrequency) {
  const Vector<Bool> whichAxes(cLattice.ndim(), True);
  LatticeFFT::cfft(cLattice, whichAxes, toFrequency);
}

void LatticeFFT::cfft(Lattice<DComplex>& cLattice, const Bool toFrequency) {
  const Vector<Bool> whichAxes(cLattice.ndim(), True);
  LatticeFFT::cfft(cLattice, whichAxes, toFrequency);
}

void LatticeFFT::rcfft(Lattice<Complex>& out, const Lattice<Float>& in, 
		       const Vector<Bool>& whichAxes, const Bool doShift,
		       Bool doFast){
  const uInt ndim = in.ndim();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(ndim == whichAxes.nelements(), AipsError);

  // find the required shape of the output Array
  const IPosition inShape = in.shape();
  IPosition outShape = in.shape();
  uInt i = 0, firstAxis = ndim;
  while (i < ndim && firstAxis == ndim) {
    if (whichAxes(i) == True) firstAxis = i;
    i++;
  }
  DebugAssert(firstAxis < ndim, AipsError); // At least one axis must be given
  outShape(firstAxis) = (outShape(firstAxis)+2)/2;
  DebugAssert(outShape.isEqual(out.shape()), AipsError);
//   if (outShape.product() == 1) {
//     const IPosition origin(ndim, 0);
//     Float val = in.getAt(origin);
//     out.set(Complex(val, 0));
//     return;
//   }

  const IPosition tileShape = out.niceCursorShape();
  TempLattice<Float> inlocal(TiledShape(in.shape(), tileShape));
  inlocal.put(in.get());
  FFTServer<Float,Complex> ffts;

    {
      for (uInt dim = 0; dim < ndim; dim++) {
	if (whichAxes(dim) == True) {
	  if (dim == firstAxis) { 
	    if (inShape(dim) != 1) { // Do real->complex Transforms
	      LatticeIterator<Float> inIter(inlocal, 
					       TiledLineStepper(inShape,
								tileShape,dim));
	      LatticeIterator<Complex> outIter(out,
					       TiledLineStepper(outShape,
								tileShape,dim));
	      for (inIter.reset(), outIter.reset();
		   !inIter.atEnd() && !outIter.atEnd(); inIter++, outIter++) {
		if (doShift) {
		  if(doFast){
		    // ffts.flip(inIter.rwVectorCursor(), True, False);
		    ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
		  }
		  else{
		    ffts.fft(outIter.woVectorCursor(), inIter.vectorCursor());
		  }
		    
		} else {
		  ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
		}
	      }
	    } else { // just copy the data
	      out.copyData(LatticeExpr<Complex>(in));
	    }
	  }
	  else { // Do complex->complex transforms
	    if (inShape(dim) != 1) { 
	      LatticeIterator<Complex> iter(out,
					    TiledLineStepper(outShape,
							     tileShape, dim));
	      for (iter.reset(); !iter.atEnd(); iter++) {
		if (doShift) {
		  if(doFast){
		    ffts.fft0(iter.rwVectorCursor(),True);
		  }
		  else{
		    ffts.fft(iter.rwVectorCursor(),True);
		  }
		} else {
		  ffts.fft0(iter.rwVectorCursor(), True);
		}
	      }
	    }
	  }
	}
      }
    }
}
//
// ----------------MYRCFFT--------------------------------------
//
void LatticeFFT::myrcfft(Lattice<Complex>& out, const Lattice<Float>& in, 
		     const Vector<Bool>& whichAxes, const Bool doShift){

  //  cerr << "####myrcfft" << endl;
  const uInt ndim = in.ndim();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(ndim == whichAxes.nelements(), AipsError);

  // find the required shape of the output Array
  const IPosition inShape = in.shape();
  IPosition outShape = in.shape();
  uInt i = 0, firstAxis = ndim;
  while (i < ndim && firstAxis == ndim) {
    if (whichAxes(i) == True) firstAxis = i;
    i++;
  }
  DebugAssert(firstAxis < ndim, AipsError); // At least one axis must be given
  outShape(firstAxis) = (outShape(firstAxis)+2)/2;
  DebugAssert(outShape.isEqual(out.shape()), AipsError);

  // outShape(0) = (inShape(0)+2)/2;
  //  outShape(1) = 2*(inShape(1)/2+1);

//   if (outShape.product() == 1) {
//     const IPosition origin(ndim, 0);
//     Float val = in.getAt(origin);
//     out.set(Complex(val, 0));
//     return;
//   }

  const IPosition tileShape = out.niceCursorShape();
  FFTServer<Float,Complex> ffts;

    {
      for (uInt dim = 0; dim < ndim; dim++) {
	if (whichAxes(dim) == True) {
	  if (dim == firstAxis) { 
	    if (inShape(dim) != 1) { // Do real->complex Transforms
	      RO_LatticeIterator<Float> inIter(in, 
					       TiledLineStepper(inShape,tileShape,dim));
	      LatticeIterator<Complex> outIter(out,
					       TiledLineStepper(outShape,tileShape,dim));
	      for (inIter.reset(), outIter.reset();
		   !inIter.atEnd() && !outIter.atEnd(); inIter++, outIter++) {
		if (doShift) {
		  //		  ffts.myfft(outIter.woVectorCursor(), inIter.vectorCursor());
		  ffts.flip((Vector<Float> &)inIter.vectorCursor(),True,False);
		  ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
		} else {
		  ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
		}
	      }
	    } else { // just copy the data
	      out.copyData(LatticeExpr<Complex>(in));
	    }
	  }
	  else { // Do complex->complex transforms
	    if (inShape(dim) != 1) { 
	      LatticeIterator<Complex> iter(out,
					    TiledLineStepper(outShape, tileShape, dim));
	      for (iter.reset(); !iter.atEnd(); iter++) {
		if (doShift) {
		  //		  ffts.fft(iter.rwVectorCursor(), 1, True);
		  ffts.flip(iter.rwVectorCursor(),True,False);
		  ffts.fft0(iter.rwVectorCursor(),True);
		} else {
		  ffts.fft0(iter.rwVectorCursor(), True);
		}
	      }
	    }
	  }
	}
      }
    }
}
//
//-------------------------------------------------------------------------
//
void LatticeFFT::rcfft(Lattice<Complex>& out, const Lattice<Float>& in, 
		     const Bool doShift, Bool doFast){
  const Vector<Bool> whichAxes(in.ndim(), True);
  LatticeFFT::rcfft(out, in, whichAxes, doShift, doFast);
}
void LatticeFFT::myrcfft(Lattice<Complex>& out, const Lattice<Float>& in, 
		     const Bool doShift){
  const Vector<Bool> whichAxes(in.ndim(), True);
  LatticeFFT::myrcfft(out, in, whichAxes, doShift);
}

void LatticeFFT::crfft(Lattice<Float>& out, Lattice<Complex>& in, 
		       const Vector<Bool>& whichAxes, const Bool doShift, 
		       Bool doFast){
  const uInt ndim = in.ndim();
  DebugAssert(ndim > 0, AipsError);
  DebugAssert(ndim == whichAxes.nelements(), AipsError);

  // find the required shape of the output Array
  const IPosition inShape = in.shape();
  IPosition outShape = in.shape();
  uInt i = 0, firstAxis = ndim;
  while (i < ndim && firstAxis == ndim) {
    if (whichAxes(i) == True) firstAxis = i;
    i++;
  }
  DebugAssert(firstAxis < ndim, AipsError); // At least one axis must be given
  outShape(firstAxis) = outShape(firstAxis)*2 - 2;
  if (!outShape.isEqual(out.shape())) outShape(firstAxis) += 1;
  DebugAssert(outShape.isEqual(out.shape()), AipsError);
//   if (outShape.product() == 1) {
//     const IPosition origin(ndim, 0);
//     const Complex val = in.getAt(origin);
//     out.set(val.re);
//     return;
//   }

  const IPosition tileShape = in.niceCursorShape();
  FFTServer<Float,Complex> ffts;

  uInt dim = ndim;
  while (dim != 0) {
    dim--;
    if (whichAxes(dim) == True) {
      if (dim != firstAxis) { // Do complex->complex Transforms
	if (inShape(dim) != 1) { // no need to do anything unless len > 1
	  LatticeIterator<Complex> iter(in, TiledLineStepper(inShape,
							     tileShape, dim));
	  for (iter.reset(); !iter.atEnd(); iter++) {
	    if (doShift) {
	      if(doFast){
		ffts.fft0(iter.rwVectorCursor(), False);
		ffts.flip(iter.rwVectorCursor(), False, False);
	      }
	      else{
	      //	      ffts.fft(iter.rwVectorCursor(), 2, False);
	      ffts.fft(iter.rwVectorCursor(),False);

	      }
	    } else {
	      ffts.fft0(iter.rwVectorCursor(), False);
	    }
	  }
	}
      } else { // the first axis is treated specially
	if (inShape(dim) != 1) { // Do complex->real transforms
	  RO_LatticeIterator<Complex> inIter(in, 
					     TiledLineStepper(inShape,
							      tileShape, dim));
	  LatticeIterator<Float> outIter(out, 
					 TiledLineStepper(outShape,
							  tileShape, dim));
	  for (inIter.reset(), outIter.reset(); 
	       !inIter.atEnd() && !outIter.atEnd(); inIter++, outIter++) {
	    if (doShift) {
	      if(doFast){
	       ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
	       ffts.flip(outIter.rwVectorCursor(), False, False);
	      }else{
		ffts.fft(outIter.woVectorCursor(), inIter.vectorCursor());
	      }
	    } else {
	      ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
	    }
	  }
	} else { // just copy the data truncating the imaginary parts.
	  out.copyData(LatticeExpr<Float>(real(in)));
	}
      }
    }
  }
}

void LatticeFFT::crfft(Lattice<Float>& out, Lattice<Complex>& in, 
		       const Bool doShift, Bool doFast){
  const Vector<Bool> whichAxes(in.ndim(), True);
  LatticeFFT::crfft(out, in, whichAxes, doShift, doFast);
}

void LatticeFFT::crfft(Lattice<Float>& out, const Lattice<Complex>& in,
		       const Bool doShift, Bool doFast){
 TempLattice<Complex> inCopy(in.shape());
 inCopy.copyData(in);
 LatticeFFT::crfft(out, inCopy, doShift, doFast);
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 LatticeFFT"
// End: 

} //# NAMESPACE CASACORE - END

