//# ClassFileName.cc:  this defines ClassName, which ...
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
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Complex.h>
#include <trial/Lattices/LatticeFFT.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/PagedArray.h>
#include <iostream.h>

Int main() {
  try {
    {
      const uInt n = 64;
      const uInt nPlanes = 3;
      const IPosition shape(3,n,n,nPlanes);
      PagedArray<Complex> arr(shape);
      IPosition centre(3, n/2, n/2, 0);
      { // test the fft2d function
	arr.set(Complex(1,0));
	LatticeFFT::fft2d(arr);
	uInt i;
	for (i = 0; i < nPlanes; i++) {
	  centre(2) = i;
	  AlwaysAssert(near(arr.getAt(centre), Complex(n*n,0), 1E-5),
		       AipsError);
	  arr.putAt(Complex(0,0), centre);
	}
	
	RO_LatticeIterator<Complex> iter(arr, 
					 arr.niceCursorShape(arr.maxPixels()));
	for (iter.reset(); !iter.atEnd(); iter++) {
	  AlwaysAssert(allNearAbs(iter.cursor(), Complex(0,0), 1E-5),
		       AipsError);
	}
	for (i = 0; i < nPlanes; i++) {
	  centre(2) = i;
	  arr.putAt(Complex(n*n,0), centre);
	}
	LatticeFFT::fft2d(arr, False);
	for (iter.reset(); !iter.atEnd(); iter++) {
	  AlwaysAssert(allNearAbs(iter.cursor(), Complex(1,0), 1E-5),
		       AipsError);
	}
      }
      { // test the fft function
	Vector<Bool> whichAxes(3, True);
	arr.set(Complex(1,0));
	LatticeFFT::fft(arr, whichAxes);
	centre(2) = nPlanes/2;
 	AlwaysAssert(near(arr.getAt(centre), Complex(n*n*nPlanes,0), 1E-5),
 		     AipsError);
 	arr.putAt(Complex(0,0), centre);
	const IPosition tileShape(arr.niceCursorShape(arr.maxPixels()));
	{
	  RO_LatticeIterator<Complex> iter(arr, tileShape);
	  for (iter.reset(); !iter.atEnd(); iter++) {
	    AlwaysAssert(allNearAbs(iter.cursor(), Complex(0,0), 1E-5),
			 AipsError);
	  }
	}
	whichAxes(2) = False;
	arr.putAt(Complex(n*n,0), centre);
	LatticeFFT::fft(arr, whichAxes, False);
	IPosition planeShape = tileShape;
	planeShape(2) = 1;
	{
	  RO_LatticeIterator<Complex> planeIter(arr,planeShape);
	  Complex cValue;
	  for (planeIter.reset(); !planeIter.atEnd(); planeIter++) {
	    if (planeIter.position()(2) == centre(2))
	      cValue = Complex(1,0);
	    else
	      cValue = Complex(0,0);
	    AlwaysAssert(allNearAbs(planeIter.cursor(), cValue, 1E-5),
			 AipsError);
	  }
	}
      }
    }
    cout<< "OK"<< endl;
    return 0;
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout<< "FAIL"<< endl;
  } end_try;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tLatticeFFT"
// End: 
