//# LatticeFFT.cc: functions for doing FFT's on Lattices.
//# Copyright (C) 1996,1997
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
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/TiledStepper.h>


void LatticeFFT::fft2d(Lattice<Complex> & cLattice, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  AlwaysAssert(ndim > 1, AipsError);
  const IPosition latticeShape = cLattice.shape();
  const uInt nx = latticeShape(0);
  const uInt ny = latticeShape(1);
  const Complex cZero(0,0);
  // For small transforms, we do everything in one plane
  if (nx*ny <= cLattice.maxPixels()) {
    const IPosition cursorShape(2, nx, ny);
    LatticeStepper ls(latticeShape, cursorShape);
    LatticeIterator<Complex> li(cLattice, ls);
    FFTServer<Float,Complex> ffts(cursorShape);
    for (li.reset(); !li.atEnd(); li++)
      //       if (!allNear(li.cursor(), cZero, 1E-6))
	 ffts.fft(li.matrixCursor().ac(), toFrequency);
  } // For large transforms , we do line by line FFT's
  else {
    Vector<Bool> whichAxes(ndim, False);
    whichAxes(0) = whichAxes(1) = True;
    LatticeFFT::fft(cLattice, whichAxes, toFrequency);
  }
}

void LatticeFFT::fft(Lattice<Complex> & cLattice,
		     const Vector<Bool> & whichAxes, const Bool toFrequency) {
  const uInt ndim = cLattice.ndim();
  DebugAssert(ndim > 0, AipsError);
  AlwaysAssert(ndim == whichAxes.nelements(), AipsError);
  const IPosition latticeShape = cLattice.shape();
  const IPosition tileShape = cLattice.niceCursorShape(cLattice.maxPixels());
  FFTServer<Float,Complex> ffts;
  const Complex cZero(0,0);

  for (uInt dim = 0; dim < ndim; dim++) {
    if (whichAxes(dim) == True) {
      TiledStepper ts(latticeShape, tileShape, dim);
      LatticeIterator<Complex> li(cLattice, ts);
      for (li.reset(); !li.atEnd(); li++) {
	// 	if (!allNear(li.cursor(), cZero, 1E-6))
	  ffts.fft(li.vectorCursor(), toFrequency);
      }
    }
  }
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 LatticeFFT"
// End: 
