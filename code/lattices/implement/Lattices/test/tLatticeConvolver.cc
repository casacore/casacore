//# tLatticeConvolver.cc:
//# Copyright (C) 1997,1998
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

    TempLattice<Float> psf(IPosition(4,16,5,1,9));
    psf.set(0.0f);
    psf.putAt(1.0f, psf.shape()/2);
    {
      LatticeConvolver<Float> c(psf);
      TempLattice<Float> extractedPsf(psf.shape());
      // test the getPsf function
      c.getPsf(extractedPsf);
      AlwaysAssert(near(extractedPsf.getAt(psf.shape()/2), 1.0f, 
			NumericTraits<Float>::epsilon), AipsError);
      extractedPsf.putAt(0.0f, psf.shape()/2);
      RO_LatticeIterator<Float> iter(extractedPsf, 
				     extractedPsf.niceCursorShape());
      for (iter.reset(); !iter.atEnd(); iter++) {
	AlwaysAssert(allNearAbs(iter.cursor(), 0.0f,
				NumericTraits<Float>::epsilon), AipsError);
      }
   // test circular convolution
 }
 {
      // test linear convolution
   {
     const IPosition imageShape = psf.shape();
     LatticeConvolver<Float> c(psf, imageShape);
   }
   {
     const IPosition imageShape(4,1);
     LatticeConvolver<Float> c(psf, imageShape);
   }
   {
     const IPosition imageShape(4,2);
     LatticeConvolver<Float> c(psf, imageShape);
   }
   {
     const IPosition imageShape(4,32);
     LatticeConvolver<Float> c(psf, imageShape);
   }
   {
     TempLattice<Float> psf1D(IPosition(1,3));
     psf1D.set(0.0f);
     psf1D.putAt(1.0f, psf1D.shape()/2);
     psf1D.putAt(0.5f, psf1D.shape()/2-1);
     psf1D.putAt(0.3f, psf1D.shape()/2+1);
     Array<Float> psfArray;
     psf1D.getSlice(psfArray, IPosition(1,0), psf1D.shape());
     cout << "psf = " << psfArray.ac() << endl;

     TempLattice<Float> model(IPosition(1,7));
     model.set(0.0);
     model.putAt(2.0, IPosition(1,0));
     model.putAt(5.0, model.shape()-1);
     Array<Float> modelArray;
     model.getSlice(modelArray, IPosition(1,0), model.shape());
     cout << "model = " << modelArray.ac() << endl;
     
     const IPosition imageShape = model.shape();
     LatticeConvolver<Float> c(psf1D, imageShape);

     TempLattice<Float> result(model.shape());
     c.linear(result, model);

     Array<Float> resultArray;
     result.getSlice(resultArray, IPosition(1,0), result.shape());
     cout << "result = " << resultArray.ac() << endl;
   }
   {
     TempLattice<Float> psf2D(IPosition(2,3,3));
     psf2D.set(0.0f);
     IPosition centre = psf2D.shape()/2;
     psf2D.putAt(1.0f, centre);
     centre(0) -= 1;
     psf2D.putAt(0.5f, centre);
     centre(0) += 2;
     psf2D.putAt(0.4f, centre);
     centre = psf2D.shape()/2; centre(1) -= 1;
     psf2D.putAt(0.2f, centre);
     centre(1) += 2;
     psf2D.putAt(0.1f, centre);
     Array<Float> psfArray;
     psf2D.getSlice(psfArray, IPosition(2,0), psf2D.shape());
     cout << "psf = " << psfArray.ac() << endl;

     TempLattice<Float> model(IPosition(2,7,6));
     model.set(0.0);
     model.putAt(2.0, IPosition(2,0));
     model.putAt(5.0, model.shape()-1);
     Array<Float> modelArray;
     model.getSlice(modelArray, IPosition(2,0), model.shape());
     cout << "model = " << modelArray.ac() << endl;
     
     const IPosition imageShape = model.shape();
     LatticeConvolver<Float> c(psf2D, imageShape);

     TempLattice<Float> result(model.shape());
     c.linear(result, model);

     Array<Float> resultArray;
     result.getSlice(resultArray, IPosition(2,0), result.shape());
     cout << "result = " << resultArray.ac() << endl;
   }
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
