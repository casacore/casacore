//# tLatticeConvolver.cc:
//# Copyright (C) 1997
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

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
//#include <aips/Lattices/Slicer.h>
#include <aips/Mathematics/NumericTraits.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeConvolver.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/TempLattice.h>
#include <trial/Lattices/PagedArray.h>
#include <iostream.h>

int main() {
  try {
    LatticeConvolver<Float> d;

    TempLattice<Float> psf(IPosition(3,160,160,160));
    psf.set(0.0f);
    psf.putAt(1.0f, psf.shape()/2);
    LatticeConvolver<Float> c(psf);
    TempLattice<Float> extractedPsf(psf.shape());
    c.getPsf(extractedPsf);
    AlwaysAssert(near(extractedPsf.getAt(psf.shape()/2), 1.0f, 
		      NumericTraits<Float>::epsilon), AipsError);
    extractedPsf.putAt(0.0f, psf.shape()/2);
    RO_LatticeIterator<Float> iter(extractedPsf, 
				   extractedPsf.niceCursorShape(extractedPsf.maxPixels()));
    for (iter.reset(); !iter.atEnd(); iter++) {
      AlwaysAssert(allNearAbs(iter.cursor(), 0.0f, 
			      NumericTraits<Float>::epsilon), AipsError);
    }
    
    
  } catch (AipsError x) {
    cout<< "FAIL"<< endl;
    cerr << x.getMesg() << endl;
  } end_try;
  cout<< "OK"<< endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tLatticeConvolver"
// End: 
