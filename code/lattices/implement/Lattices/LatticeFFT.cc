//# LatticeFFT.cc: functions for doing FFT's on Lattices.
//# Copyright (C) 1996,1997,1998
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#include <trial/Lattices/LatticeFFT.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/FFTServer.h>
#include <aips/Utilities/Assert.h>
#include <trial/Lattices/CopyLattice.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/TempLattice.h>
#include <trial/Lattices/TiledLineStepper.h>

void LatticeFFT::cfft2d(Lattice<Complex> & cLattice, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  AlwaysAssert(ndim > 1, AipsError);
  const IPosition & latticeShape = cLattice.shape();
  const uInt maxPixels = cLattice.maxPixels();
  IPosition slabShape = cLattice.niceCursorShape(maxPixels);
  const uInt nx = slabShape(0) = latticeShape(0);
  const uInt ny = slabShape(1) = latticeShape(1);
  // For small transforms, we do everything in one plane
  if ((uInt) slabShape.product() <= maxPixels) {
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

void LatticeFFT::cfft(Lattice<Complex> & cLattice,
		     const Vector<Bool> & whichAxes, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 0, AipsError);
  AlwaysAssert(ndim == whichAxes.nelements(), AipsError);
  FFTServer<Float,Complex> ffts;
  const IPosition latticeShape = cLattice.shape();
  const IPosition tileShape = cLattice.niceCursorShape(cLattice.maxPixels());

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

void LatticeFFT::cfft(Lattice<Complex> & cLattice, const Bool toFrequency) {
  const Vector<Bool> whichAxes(cLattice.ndim(), True);
  LatticeFFT::cfft(cLattice, whichAxes, toFrequency);
}

void LatticeFFT::rcfft(Lattice<Complex> & out, const Lattice<Float> & in, 
		     const Vector<Bool> & whichAxes, const Bool doShift){
  const uInt ndim = in.ndim();
  DebugAssert(ndim > 0, AipsError);
  AlwaysAssert(ndim == whichAxes.nelements(), AipsError);

  // find the required shape of the output Array
  const IPosition inShape = in.shape();
  IPosition outShape = in.shape();
  uInt i = 0, firstAxis = ndim;
  while (i < ndim && firstAxis == ndim) {
    if (whichAxes(i) == True)
      firstAxis = i;
    i++;
  }
  AlwaysAssert(firstAxis < ndim, AipsError); // At least one axis must be given
  outShape(firstAxis) = (outShape(firstAxis)+2)/2;
  AlwaysAssert(outShape.isEqual(out.shape()), AipsError);

  const IPosition tileShape = out.niceCursorShape(out.maxPixels());
  FFTServer<Float,Complex> ffts;

  for (uInt dim = 0; dim < ndim; dim++) {
    if (whichAxes(dim) == True) {
      if (dim == firstAxis) { // Do real->complex Transforms
	RO_LatticeIterator<Float> inIter(in, 
					 TiledLineStepper(inShape,
							  tileShape,dim));
	LatticeIterator<Complex> outIter(out, 
					 TiledLineStepper(outShape,
							  tileShape,dim));
	for (inIter.reset(), outIter.reset(); 
	     !inIter.atEnd() && !outIter.atEnd(); inIter++, outIter++) {
	  if (doShift) {
	    ffts.fft(outIter.woVectorCursor(), inIter.vectorCursor());
	  } else {
	    ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
	  }
	}
      }
      else { // Do complex->complex transforms
	LatticeIterator<Complex> iter(out,
				      TiledLineStepper(outShape,
						       tileShape, dim));
	for (iter.reset(); !iter.atEnd(); iter++) {
	  if (doShift) {
	    ffts.fft(iter.rwVectorCursor(), True);
	  } else {
	    ffts.fft0(iter.rwVectorCursor(), True);
	  }
	}
      }
    }
  }
}

void LatticeFFT::rcfft(Lattice<Complex> & out, const Lattice<Float> & in, 
		     const Bool doShift){
  const Vector<Bool> whichAxes(in.ndim(), True);
  LatticeFFT::rcfft(out, in, whichAxes, doShift);
}

void LatticeFFT::crfft(Lattice<Float> & out, Lattice<Complex> & in, 
		     const Vector<Bool> & whichAxes, const Bool doShift){
  const uInt ndim = in.ndim();
  DebugAssert(ndim > 0, AipsError);
  AlwaysAssert(ndim == whichAxes.nelements(), AipsError);

  // find the required shape of the output Array
  const IPosition inShape = in.shape();
  IPosition outShape = in.shape();
  uInt i = 0, firstAxis = ndim;
  while (i < ndim && firstAxis == ndim) {
    if (whichAxes(i) == True)
      firstAxis = i;
    i++;
  }
  AlwaysAssert(firstAxis < ndim, AipsError); // At least one axis must be given
  outShape(firstAxis) = outShape(firstAxis)*2 - 2;
  if (!outShape.isEqual(out.shape()))
    outShape(firstAxis) += 1;
  AlwaysAssert(outShape.isEqual(out.shape()), AipsError);

  const IPosition tileShape = in.niceCursorShape(in.maxPixels());
  FFTServer<Float,Complex> ffts;

  uInt dim = ndim;
  while (dim != 0) {
    dim--;
    if (whichAxes(dim) == True) {
      if (dim != firstAxis) { // Do complex->complex Transforms
 	LatticeIterator<Complex> iter(in, TiledLineStepper(inShape,
							   tileShape, dim));
 	for (iter.reset(); !iter.atEnd(); iter++) {
	  if (doShift) {
	    ffts.fft(iter.rwVectorCursor(), False);
	  } else {
	    ffts.fft0(iter.rwVectorCursor(), False);
	  }
 	}
      }
      else { // Do complex->real transforms
 	RO_LatticeIterator<Complex> inIter(in, 
					   TiledLineStepper(inShape,
							    tileShape, dim));
 	LatticeIterator<Float> outIter(out, 
				       TiledLineStepper(outShape,
							tileShape, dim));
 	for (inIter.reset(), outIter.reset(); 
 	     !inIter.atEnd() && !outIter.atEnd(); inIter++, outIter++) {
	  if (doShift) {
	    ffts.fft(outIter.woVectorCursor(), inIter.vectorCursor());
	  } else {
	    ffts.fft0(outIter.woVectorCursor(), inIter.vectorCursor());
	  }
	}
      }
    }
  }
}

void LatticeFFT::crfft(Lattice<Float> & out, Lattice<Complex> & in, 
		       const Bool doShift){
  const Vector<Bool> whichAxes(in.ndim(), True);
  LatticeFFT::crfft(out, in, whichAxes, doShift);
}

void LatticeFFT::crfft(Lattice<Float> & out, const Lattice<Complex> & in,
		       const Bool doShift){
 TempLattice<Complex> inCopy(in.shape());
 CopyLattice(inCopy.lc(), in);
 LatticeFFT::crfft(out, inCopy.lc(), doShift);
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 LatticeFFT"
// End: 
