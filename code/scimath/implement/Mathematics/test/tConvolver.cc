//# tConvolver.cc:  this tests the Convolver class
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
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <aips/aips.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Mathematics/Convolver.h>

int main() {
  Bool anyFailures = False;
  {
    Bool failed = False;
    // Test the double precision constructor
    Array<Double> psf(IPosition(1,4)); 
    psf = 0.;
    psf(IPosition(1,1)) = 0.1;
    psf(IPosition(1,2)) = 1.;
    psf(IPosition(1,3)) = 0.5;
    Convolver<Double> conv(psf);
    // Now test circular Convolution (1 - Dimensional)
    Vector<Double> mod(4);
    mod = 0;
    mod(3) = 1;
    mod(0) = 2;
    Vector<Double> result;
    conv.circularConv(result, mod);
    Array<Double> expectedResult(IPosition(1,4));
    expectedResult(IPosition(1,0)) = 2.5;
    expectedResult(IPosition(1,1)) = 1.0;
    expectedResult(IPosition(1,2)) = 0.1;
    expectedResult(IPosition(1,3)) = 1.2;
    if (!allNearAbs(expectedResult, result.arrayCast(), 1.E-10))
      failed = True;

    if (failed) 
      cout << "Failed";
    else
      cout << "Passed";
    cout << " the Circular Convolution in Double Precision Test"
	 << endl;
    mod.resize(IPosition(1,6));
    mod = 0;
    mod(5) = 1;
    mod(0) = 2;
    if (!failed){
      result.resize(IPosition(1,0));
      conv.circularConv(result, mod);
      expectedResult.resize(IPosition(1,6));
      expectedResult = 0.;
      expectedResult(IPosition(1,0)) = 2.5;
      expectedResult(IPosition(1,1)) = 1.0;
      expectedResult(IPosition(1,4)) = 0.1;
      expectedResult(IPosition(1,5)) = 1.2;
      if (!allNearAbs(expectedResult, result.arrayCast(), 1.E-10)){
	failed = True;
	cout << "Failed";
      }
      else
	cout << "Passed";
      cout << " the Circular Convolution Resize Test"
	   << endl;
    }
    if (failed) anyFailures = True;
  }
  {
    Bool failed = False;
    // Test the single precision constructor
    Matrix<Float> psf(2,2); 
    psf = 0.;
    psf(1,1) = 1;
    psf(0,1) = .5;
    psf(1,0) = .1;
    //    cout << "Psf:" << psf << endl;
    Convolver<Float> conv(psf);
    // And test single precision circular convolution
    Matrix<Float> mod(6,6);
    mod = 0;
    mod(0,0) = 1;
    mod(5,5) = 2;
    mod(2,0) = 3;
    //    cout << "Model:" << mod << endl;
    Matrix<Float> result;
    conv.circularConv(result, mod);
    //    cout << "Result:" << result << endl;
    Matrix<Float> expectedResult(6,6);
    expectedResult = mod;
    expectedResult(5,0) = 0.5;
    expectedResult(0,5) = 0.1;
    expectedResult(5,4) = 0.2;
    expectedResult(4,5) = 1.0;
    expectedResult(2,5) = 0.3;
    expectedResult(1,0) = 1.5;
    if (!allNearAbs(expectedResult.arrayCast(), result.arrayCast(), 1.E-5)){
      failed = True;
      cout << "Failed";
    }
    
    else
      cout << "Passed";
    cout << " the Floating Point 2-D Circular Convolution Test"
	 << endl;
    if (failed) anyFailures = True;
  }
  {
    Bool failed = False;
    // Test the double precision constructor with supplied image size
    Array<Double> psf(IPosition(1,2)); 
    psf = 0.;
    psf(IPosition(1,0)) = .5;
    psf(IPosition(1,1)) = 1.;
    Convolver<Double> conv(psf, IPosition(1,4));
    // And test linear convolution
    Array<Double> mod(IPosition(1,4)); 
    mod = 0.;
    mod(IPosition(1,0)) = 1.;
    mod(IPosition(1,3)) = 2.;
    Array<Double> result;
    conv.linearConv(result, mod, False);
    Array<Double> expectedResult(IPosition(1,4));
    expectedResult(IPosition(1,0)) = 1.;
    expectedResult(IPosition(1,1)) = 0.;
    expectedResult(IPosition(1,2)) = 1.;
    expectedResult(IPosition(1,3)) = 2.;
    if (!allNearAbs(expectedResult, result, 1.E-10)){
      failed = True;
      cout << "Failed";
    }
    else
      cout << "Passed";
    cout << " the Linear Convolution in Double Precision Test"
	 << endl;
    if (!failed){
      // see if the convolver can automatically resize if given a bigger image
      Vector<Double> bigMod(8), bigResult; 
      bigMod = 0; bigMod(0) = 1; bigMod(7) = 2;
      conv.linearConv(bigResult, bigMod, True);
      Vector<Double> expectedBigResult(9);
      expectedBigResult = 0;
      expectedBigResult(0) = 0.5;
      expectedBigResult(1) = 1.0;
      expectedBigResult(7) = 1.0;
      expectedBigResult(8) = 2.0; 
      if (!allNearAbs(expectedBigResult.arrayCast(), bigResult.arrayCast(), 1.E-10)){
	failed = True;
	cout << "Failed";
      }
      else
	cout << "Passed";
      cout << " the   array resize test"
	   << endl;
    }
    if (!failed){
      // Set the psf to something new (different size)
      psf.resize(IPosition(1,4));
      psf(IPosition(1,0)) = .5;
      psf(IPosition(1,1)) = 1.;
      psf(IPosition(1,2)) = .3;
      psf(IPosition(1,3)) = .1;
      conv.setPsf(psf); 
      result.resize(IPosition(1,0));
      conv.linearConv(result, mod);
      expectedResult.resize(IPosition(1,4));
      expectedResult(IPosition(1,0)) = 0.3;
      expectedResult(IPosition(1,1)) = 1.1;
      expectedResult(IPosition(1,2)) = 2.0;
      expectedResult(IPosition(1,3)) = 0.6;
      if (!allNearAbs(expectedResult, result, 1.E-10)){
	failed = True;
	cout << "Failed";
      }
      else
	cout << "Passed";
      cout << " the   new psf test"
	   << endl;
    }
    if (!failed){
      mod.resize(IPosition(1,2));
      mod(IPosition(1,0)) = 0;
      mod(IPosition(1,1)) = 1;
      result.resize(IPosition(1,0));
      conv.linearConv(result, mod);
      expectedResult.resize(IPosition(1,2));
      expectedResult(IPosition(1,0)) = 1;
      expectedResult(IPosition(1,1)) = 0.3;
      if (!allNearAbs(expectedResult, result, 1.E-10)){
	failed = True;
	cout << "Failed";
      }
      else
	cout << "Passed";
      cout << " the   small model test"
	   << endl;
    }
    if (failed) anyFailures = True;
  }
  {
    Bool failed = False;
    //    Test the linear convolution with Single precision 2-D functions
    Matrix<Float> psf(2,4); 
    psf = 0.;
    psf(1,2) = 1.;
    psf(1,3) = .1;
    Cube<Float> mod(2,4,3); 
    Convolver<Float> conv(psf, mod.shape());
    mod = 0.;
    mod(1,2,0) = 1.;
    mod(0,0,1) = 2.;
    mod(1,3,1) = 4.;
    for (uInt i = 0; i < 4; i++)
      for (uInt j = 0; j < 2; j++)
	mod(j,i,2) = 1.;
    Cube<Float> result;
    conv.linearConv(result, mod, False);
    Cube<Float> expectedResult(2,4,3);
    expectedResult = mod;
    expectedResult(1,3,0) = 0.1;
    expectedResult(0,1,1) = 0.2;
    expectedResult(0,0,2) = 1;
    expectedResult(1,0,2) = 1;
    expectedResult(0,1,2) = 1.1;
    expectedResult(1,1,2) = 1.1;
    expectedResult(0,2,2) = 1.1;
    expectedResult(1,2,2) = 1.1;
    expectedResult(0,3,2) = 1.1;
    expectedResult(1,3,2) = 1.1;
    if (!allNearAbs(expectedResult.arrayCast(), result.arrayCast(), 1.E-5)){
      failed = True;
      cout << "Failed";
    }
    
    else
      cout << "Passed";
    cout << " the Multiple Floating Point 2-D Linear Convolution Test"
	 << endl;
    if (failed) anyFailures = True;
  }
  if (anyFailures) {
    cout << "FAIL" << endl;
    return 1;
  }
  else {
    cout << "OK" << endl;
    return 0;
  }
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tConvolver"
// End:
