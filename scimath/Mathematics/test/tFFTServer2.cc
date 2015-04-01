//# tFFTServer: This program tests the FFTServer and FourierTool classes
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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
//# Includes

#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/FFTServer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
// #include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  try {
    FFTServer<Float, Complex> server(IPosition(1,8));
    { // 1-D real->complex FFT's on an even length
      Vector<Float> input(8);
      input = 0.0f;
      input(0) = 1.0f;
      Vector<Complex> result, expectedResult(5);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input(2) = -1.0f;
      input(4) = 1.0f;
      input(6) = -1.0f;
      expectedResult = Complex(0,0);
      expectedResult(2) = Complex(4,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 0.0f;
      input(1) = 1.0f;
      input(3) = -1.0f;
      input(5) = 1.0f;
      input(7) = -1.0f;
      expectedResult = Complex(0,0);
      expectedResult(2) = Complex(0,-4);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 0.0f;
      input(1) = 1.0f;
      input(3) = 1.0f;
      input(5) = 1.0f;
      input(7) = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(4,0);
      expectedResult(4) = Complex(-4,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
    }
    { // 1-D real->complex FFT's on an odd length
      Vector<Float> input(9);
      input = 0.0f;
      input(0) = 1.0f;
      Vector<Complex> result, expectedResult(5);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(9,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input(1) = 0.0f;
      input(3) = 0.0f;
      input(5) = 0.0f;
      input(7) = 0.0f;
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(5,0);
      server.fft0(result, input);
      AlwaysAssert(near(result(0), Complex(5,0), FLT_EPSILON), AipsError);
      AlwaysAssert(!near(result(4).imag(), 0.0f, FLT_EPSILON), AipsError);
    }
    { // 2-D real->complex FFT's on an even/even length
      Matrix<Float> input(4,6);
      input = 0.0f;
      input(0,0) = 1.0f;
      Matrix<Complex> result, expectedResult(3,6);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(24,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 0.0f;
      input(1,1) = 1.0f;
      input(1,3) = 1.0f;
      input(1,5) = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0) = expectedResult(2,3) = Complex(3,0);
      expectedResult(0,3) = expectedResult(2,0) = Complex(-3,0);
      expectedResult(1,3) = Complex(0,3);
      expectedResult(1,0) = Complex(0,-3);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, 2.0f*FLT_EPSILON),
		   AipsError);
    }
    { // 2-D real->complex FFT's on an even/odd length
      Matrix<Float> input(4,5);
      input = 0.0f;
      input(0,0) = 1.0f;
      Matrix<Complex> result, expectedResult(3,5);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(20,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
    }
    { // 2-D real->complex FFT's on an odd/even length
      Matrix<Float> input(3,6);
      input = 0.0f;
      input(0,0) = 1.0f;
      Matrix<Complex> result, expectedResult(2,6);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(18,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
    }
    { // 2-D real->complex FFT's on an odd/odd length
      Matrix<Float> input(3,5);
      input = 0.0f;
      input(0,0) = 1.0f;
      Matrix<Complex> result, expectedResult(2,5);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(15,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
    }
    { // 3-D real->complex FFT's on an even/even/even length
      Cube<Float> input(4,6,8);
      input = 0.0f;
      input(0,0,0) = 1.0f;
      Cube<Complex> result, expectedResult(3,6,8);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0,0) = Complex(4*6*8,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
    }
    { // 3-D real->complex FFT's on an odd/odd/odd length
      Cube<Float> input(3,5,7);
      input = 0.0f;
      input(0,0,0) = 1.0f;
      Cube<Complex> result, expectedResult(2,5,7);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,0,0) = Complex(3*5*7,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, 
			      100*FLT_EPSILON), AipsError);
    }
    { // 4-D real->complex FFT's on an odd/odd/odd/even length
      Array<Float> input(IPosition(4,3,5,7,4));
      input = 0.0f;
      input(IPosition(4,0)) = 1.0f;
      Array<Complex> result, expectedResult(IPosition(4,2,5,7,4));
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(IPosition(4,0)) = Complex(3*5*7*4,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      500*FLT_EPSILON), AipsError);
    }
    { // 1-D complex->real FFT's on an even length
      Vector<Complex> input(5);
      input = Complex(0.0f, 0.0f);
      input(0) = Complex(8.0f, 0.0f);
      Vector<Float> result, expectedResult(8);
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input(0) = Complex(0.0f, 0.0f);
      input(0) = Complex(16.0f, 0.0f);
      input(2) = Complex(8.0f, 0.0f);
      expectedResult = 2.0f;
      expectedResult(0) = 4.0f;
      expectedResult(2) = 0.0f;
      expectedResult(4) = 4.0f;
      expectedResult(6) = 0.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input(0) = Complex(0.0f, 0.0f);
      input(2) = Complex(0.0f, 4.0f);
      expectedResult = 0.0f;
      expectedResult(1) = -1.0f;
      expectedResult(3) = 1.0f;
      expectedResult(5) = -1.0f;
      expectedResult(7) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input = Complex(1.0f, 0.0f);
      expectedResult = 0.0f;
      expectedResult(0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input(1) = Complex(0,0);
      input(3) = Complex(0,0);
      expectedResult = 0.0f;
      expectedResult(0) = 0.5f;
      expectedResult(4) = 0.5f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
    }
    { // 1-D complex->real FFT's on an odd length
      Vector<Complex> input(5);
      input = Complex(0,0);
      input(0) = Complex(9,0);
      Vector<Float> result(9), expectedResult(9);
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      result.resize(0);
      server.fft0(result, input);
      AlwaysAssert(result.nelements() == 9, AipsError);
      result.resize(0);
      server.resize(IPosition(1,8));
      server.fft0(result, input);
      AlwaysAssert(result.nelements() == 8, AipsError);
      result.resize(0);
      server.resize(IPosition(1,7));
      server.fft0(result, input);
      AlwaysAssert(result.nelements() == 8, AipsError);
      result.resize(0);
      input(4) = Complex(1,1);
      server.fft0(result, input);
      AlwaysAssert(result.nelements() == 9, AipsError);
    }
    { // 2-D complex->real FFT's on an even/even length
      Matrix<Complex> input(3,6);
      input = Complex(0,0);
      input(0,0) = Complex(4*6,0);
      Matrix<Float> result(4,6), expectedResult(4,6);
      server.fft0(result, input);
      expectedResult =1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0,0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      input = Complex(0,0);
      input(0,0) = Complex(24,0);
      input(2,0) = Complex(-24,0);
      input(0,1) = Complex(-24,0);
      input(0,2) = Complex( 24,0);
      input(0,3) = Complex(-24,0);
      input(0,4) = Complex( 24,0);
      input(0,5) = Complex(-24,0);
      expectedResult = 0.0f;
      expectedResult(0,0) = expectedResult(0,1) = expectedResult(0,2) 
	= expectedResult(0,4) = expectedResult(0,5) = -1.0f;
      expectedResult(0,3) = 5.0f;
      expectedResult(1,0) = expectedResult(1,1) = expectedResult(1,2) 
	= expectedResult(1,4) = expectedResult(1,5) = 1.0f;
      expectedResult(1,3) = 7.0f;
      expectedResult(2,0) = expectedResult(2,1) = expectedResult(2,2) 
	= expectedResult(2,4) = expectedResult(2,5) = -1.0f;
      expectedResult(2,3) = 5.0f;
      expectedResult(3,0) = expectedResult(3,1) = expectedResult(3,2) 
	= expectedResult(3,4) = expectedResult(3,5) = 1.0f;
      expectedResult(3,3) = 7.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, 4.0f*FLT_EPSILON),
 		   AipsError);
      input = 0.0f;
      input(2,5) = Complex(0,24);
      result.resize(0,0);
      server.fft0(result, input);
      AlwaysAssert(result.shape().isEqual(IPosition(2,5,6)), AipsError);
    }
    { // 2-D complex->real FFT's on an odd/odd length
      Matrix<Complex> input(2,5);
      input = Complex(0,0);
      input(0,0) = Complex(3*5,0);
      Matrix<Float> result(3,5), expectedResult(3,5);
      server.fft0(result, input);
      expectedResult =1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0,0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(0,0);
      input(1,0) = Complex(0,45);
      input(1,1) = Complex(0,45);
      input(1,2) = Complex(0,45);
      input(1,3) = Complex(0,45);
      input(1,4) = Complex(0,45);
      expectedResult = 0.0f;
      expectedResult(1,0) = -25.9808f;
      expectedResult(2,0) = 25.9808f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      500*FLT_EPSILON), AipsError);
    }
    { // 2-D complex->real FFT's on an even/odd length
      Matrix<Complex> input(3,5);
      input = Complex(0,0);
      input(0,0) = Complex(4*5,0);
      Matrix<Float> result(4,5), expectedResult(4,5);
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0,0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->real FFT's on an odd/even length
      Matrix<Complex> input(2,6);
      input = Complex(0,0);
      input(0,0) = Complex(3*6,0);
      Matrix<Float> result(3,6), expectedResult(3,6);
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0,0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 3-D complex->real FFT's on an even/even/even length
      Cube<Complex> input(3,6,2);
      input = Complex(0,0);
      input(0,0,0) = Complex(4*6*2,0);
      Cube<Float> result(4,6,2), expectedResult(4,6,2);
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0,0,0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 3-D complex->real FFT's on an odd/odd/odd length
      Cube<Complex> input(2,5,7);
      input = Complex(0,0);
      input(0,0,0) = Complex(3*5*7,0);
      Cube<Float> result(3,5,7), expectedResult(3,5,7);
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(0,0,0) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 4-D complex->real FFT's on an odd/odd/odd/even length
      Array<Complex> input(IPosition(4,2,5,7,2));
      input = Complex(0,0);
      input(IPosition(4,0)) = Complex(3*5*7*2,0);
      Array<Float> result(IPosition(4,3,5,7,2));
      Array<Float> expectedResult(IPosition(4,3,5,7,2));
      server.fft0(result, input);
      expectedResult = 1.0f;
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      input = Complex(1,0);
      expectedResult = 0.0f;
      expectedResult(IPosition(4,0)) = 1.0f;
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 1-D complex->complex FFT's on an even length
      Vector<Complex> input(8);
      input = Complex(0, 0);
      input(0) = Complex(1.0f, 0.0f);
      Vector<Complex> result, expectedResult(8);
      server.fft0(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
 		   AipsError);
      
      input = Complex(1, 0);
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(8,0);
      server.fft0(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
 		   AipsError);
      input = Complex(-1, 0);
      input(0) = Complex(1, 0);
      input(2) = Complex(1, 0);
      input(4) = Complex(1, 0);
      input(6) = Complex(1, 0);
      expectedResult = Complex(0,0);
      expectedResult(4) = Complex(8,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
 		   AipsError);

      input = Complex(0, 0);
      input(1) = Complex(1, 0);
      input(3) = Complex(-1,0);
      input(5) = Complex(1, 0);
      input(7) = Complex(-1,0);
      expectedResult = Complex(0,0);
      expectedResult(2) = Complex(0,-4);
      expectedResult(6) = Complex(0,4);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
 		   AipsError);
    }
    { // 1-D complex->complex FFT's on an odd length
      Vector<Complex> input(7);
      input = Complex(0, 0);
      input(0) = Complex(1.0f, 0.0f);
      Vector<Complex> result, expectedResult(7);
      server.fft0(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1, 0);
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(7,0);
      server.fft0(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult,5*FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 5*FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an even/even length
      Matrix<Complex> input(4,6);
      input = Complex(0,0);
      input(0,0) = Complex(1,0);
      Matrix<Complex> result, expectedResult(4,6);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(24,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(0,0);
      input(1,1) = Complex(1,1);
      input(1,3) = Complex(1,1);
      input(1,5) = Complex(1,1);
      expectedResult = Complex(0,0);
      expectedResult(0,0) = expectedResult(2,3) = Complex(3,3);
      expectedResult(0,3) = expectedResult(2,0) = Complex(-3,-3);
      expectedResult(1,3) = expectedResult(3,0) = Complex(-3,3);
      expectedResult(1,0) = expectedResult(3,3) = Complex(3,-3);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, 5*FLT_EPSILON),
 		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an odd/odd length
      Matrix<Complex> input(3,5);
      input = Complex(0,0);
      input(0,0) = Complex(1,0);
      Matrix<Complex> result, expectedResult(3,5);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(15,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an even/odd length
      Matrix<Complex> input(4,5);
      input = Complex(0,0);
      input(0,0) = Complex(1,0);
      Matrix<Complex> result, expectedResult(4,5);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(20,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an odd/even length
      Matrix<Complex> input(3,6);
      input = Complex(0,0);
      input(0,0) = Complex(1,0);
      Matrix<Complex> result, expectedResult(3,6);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(0,0) = Complex(18,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 3-D complex->complex FFT's on an even/even/even length
      Cube<Complex> input(4,6,8);
      input = Complex(0,0);
      input(0,0,0) = Complex(1,0);
      Cube<Complex> result, expectedResult(4,6,8);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(0,0,0) = Complex(4*6*8,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 3-D complex->complex FFT's on an odd/odd/odd length
      Cube<Complex> input(3,5,7);
      input = Complex(0,0);
      input(0,0,0) = Complex(1,0);
      Cube<Complex> result, expectedResult(3,5,7);
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(0,0,0) = Complex(3*5*7,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, 
			      100*FLT_EPSILON), AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);
    }
    { // 4-D complex->complex FFT's on an odd/odd/odd/even length
      Array<Complex> input(IPosition(4,3,5,7,4));
      input = Complex(0,0);
      input(IPosition(4,0)) = Complex(1,0);
      Array<Complex> result, expectedResult(IPosition(4,3,5,7,4));
      server.fft0(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(IPosition(4,0)) = Complex(3*5*7*4,0);
      server.fft0(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      500*FLT_EPSILON), AipsError);
      expectedResult = input;
      server.fft0(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);
    }
    { // 1-D complex->complex FFT's on an even length (origin at the centre)
      Vector<Complex> input(8);
      input = Complex(0, 0);
      input(4) = Complex(1.0f, 0.0f);
      Vector<Complex> result, expectedResult(8);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
      
      input = Complex(1, 0);
      expectedResult = Complex(0,0);
      expectedResult(4) = Complex(8,0);
      server.fft(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(-1, 0);
      input(0) = Complex(1, 0);
      input(2) = Complex(1, 0);
      input(4) = Complex(1, 0);
      input(6) = Complex(1, 0);
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(8,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
 		   AipsError);

      input = Complex(0, 0);
      input(1) = Complex(1, 0);
      input(3) = Complex(-1,0);
      input(5) = Complex(1, 0);
      input(7) = Complex(-1,0);
      expectedResult = Complex(0,0);
      expectedResult(2) = Complex(0,4);
      expectedResult(6) = Complex(0,-4);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
   		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 1-D complex->complex FFT's on an odd length (origin at the centre)
      Vector<Complex> input(7);
      input = Complex(0, 0);
      input(3) = Complex(1.0f, 0.0f);
      Vector<Complex> result, expectedResult(7);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1, 0);
      expectedResult = Complex(0,0);
      expectedResult(3) = Complex(7,0);
      server.fft(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult, 5*FLT_EPSILON),
  		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 5*FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an even/even length (origin at centre)
      Matrix<Complex> input(4,6);
      input = Complex(0,0);
      input(2,3) = Complex(1,0);
      Matrix<Complex> result, expectedResult(4,6);
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(2,3) = Complex(24,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(0,0);
      input(3,1) = Complex(1,1);
      input(3,3) = Complex(1,1);
      input(3,5) = Complex(1,1);
      expectedResult = Complex(0,0);
      expectedResult(2,3) = expectedResult(2,0) = Complex(3,3);
      expectedResult(0,0) = expectedResult(0,3) = Complex(-3,-3);
      expectedResult(1,0) = expectedResult(1,3) = Complex(-3,3);
      expectedResult(3,3) = expectedResult(3,0) = Complex(3,-3);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, 5*FLT_EPSILON),
 		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an odd/odd length (origin at centre)
      Matrix<Complex> input(3,5);
      input = Complex(0,0);
      input(1,2) = Complex(1,0);
      Matrix<Complex> result, expectedResult(3,5);
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(1,2) = Complex(15,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an even/odd length (origin at centre)
      Matrix<Complex> input(4,5);
      input = Complex(0,0);
      input(2,2) = Complex(1,0);
      Matrix<Complex> result, expectedResult(4,5);
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(2,2) = Complex(20,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D complex->complex FFT's on an odd/even length (origin at centre)
      Matrix<Complex> input(3,6);
      input = Complex(0,0);
      input(1,3) = Complex(1,0);
      Matrix<Complex> result, expectedResult(3,6);
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(1,3) = Complex(18,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 3-D complex->complex FFT's on an even/even/even len (origin at centre)
      Cube<Complex> input(4,6,8);
      input = Complex(0,0);
      input(2,3,4) = Complex(1,0);
      Cube<Complex> result, expectedResult(4,6,8);
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(2,3,4) = Complex(4*6*8,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, FLT_EPSILON),
  		   AipsError);
    }
    { // 3-D complex->complex FFT's on an odd/odd/odd length (origin at centre)
      Cube<Complex> input(3,5,7);
      input = Complex(0,0);
      input(1,2,3) = Complex(1,0);
      Cube<Complex> result, expectedResult(3,5,7);
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(1,2,3) = Complex(3*5*7,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      100*FLT_EPSILON), AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);
    }
    { // 4-D complex->complex FFT's on an odd/odd/odd/even len (orig at centre)
      Array<Complex> input(IPosition(4,3,5,7,4));
      input = Complex(0,0);
      input(IPosition(4,1,2,3,2)) = Complex(1,0);
      Array<Complex> result, expectedResult(IPosition(4,3,5,7,4));
      server.fft(result, input);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);

      input = Complex(1,0);
      expectedResult = Complex(0,0);
      expectedResult(IPosition(4,1,2,3,2)) = Complex(3*5*7*4,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      500*FLT_EPSILON), AipsError);
      expectedResult = input;
      server.fft(input, result, False);
      AlwaysAssert(allNearAbs(input, expectedResult, 2*FLT_EPSILON),
  		   AipsError);
    }
    { // 1-D real<->complex FFT's on an even length (orig at centre)
      Vector<Float> input(8);
      input = 0.0f;
      input(4) = 1.0f;
      Vector<Complex> result, expectedResult(5);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Vector<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);

      input(2) = -1.0f;
      input(0) = 1.0f;
      input(6) = -1.0f;
      expectedResult = Complex(0,0);
      expectedResult(2) = Complex(4,0);
      server.fft(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);

      input = 0.0f;
      input(1) = 1.0f;
      input(3) = -1.0f;
      input(5) = 1.0f;
      input(7) = -1.0f;
      expectedResult = Complex(0,0);
      expectedResult(2) = Complex(0,-4);
      server.fft(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);

      input = 0.0f;
      input(1) = 1.0f;
      input(3) = 1.0f;
      input(5) = 1.0f;
      input(7) = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(4,0);
      expectedResult(4) = Complex(-4,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);
    }
    { // 1-D real<->complex FFT's on an odd length (orig at centre)
      Vector<Float> input(9);
      input = 0.0f;
      input(4) = 1.0f;
      Vector<Complex> result, expectedResult(5);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Vector<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(9,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);

      input(1) = 0.0f;
      input(3) = 0.0f;
      input(5) = 0.0f;
      input(7) = 0.0f;
      expectedResult = Complex(0,0);
      expectedResult(0) = Complex(5,0);
      server.fft(result, input, True);
      AlwaysAssert(near(result(0), Complex(5,0), FLT_EPSILON), AipsError);
      AlwaysAssert(!near(result(4).imag(), 0.0f, FLT_EPSILON), AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);
    }
    { // 2-D real<->complex FFT's on an even/even length (orig at centre)
      Matrix<Float> input(4,6);
      input = 0.0f;
      input(2,3) = 1.0f;
      Matrix<Complex> result, expectedResult(3,6);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Matrix<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,3) = Complex(24,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
  		   AipsError);

      input = 0.0f;
      input(3,4) = 1.0f;
      input(3,0) = 1.0f;
      input(3,2) = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,3) = expectedResult(2,0) = Complex(3,0);
      expectedResult(0,0) = expectedResult(2,3) = Complex(-3,0);
      expectedResult(1,0) = Complex(0,3);
      expectedResult(1,3) = Complex(0,-3);
      server.fft(result, input, True);
      AlwaysAssert(allNearAbs(result, expectedResult, 2.0f*FLT_EPSILON),
 		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);
    }
    { // 2-D real<->complex FFT's on an even/odd length (orig at centre)
      Matrix<Float> input(4,5);
      input = 0.0f;
      input(2,2) = 1.0f;
      Matrix<Complex> result, expectedResult(3,5);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      Matrix<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,2) = Complex(20,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);
    }
    { // 2-D real<->complex FFT's on an odd/even length (orig at centre)
      Matrix<Float> input(3,6);
      input = 0.0f;
      input(1,3) = 1.0f;
      Matrix<Complex> result, expectedResult(2,6);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      Matrix<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,3) = Complex(18,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
 		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);
    }
    { // 2-D real<->complex FFT's on an odd/odd length (orig at centre)
      Matrix<Float> input(3,5);
      input = 0.0f;
      input(1,2) = 1.0f;
      Matrix<Complex> result, expectedResult(2,5);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Matrix<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,2) = Complex(15,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);
    }
    { // 3-D real<->complex FFT's on an even/even/even length (orig at centre)
      Cube<Float> input(4,6,8);
      input = 0.0f;
      input(2,3,4) = 1.0f;
      Cube<Complex> result, expectedResult(3,6,8);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Cube<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,3,4) = Complex(4*6*8,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);
    }
    { // 3-D real<->complex FFT's on an odd/odd/odd length (orig at centre)
      Cube<Float> input(3,5,7);
      input = 0.0f;
      input(1,2,3) = 1.0f;
      Cube<Complex> result, expectedResult(2,5,7);
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Cube<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(0,2,3) = Complex(3*5*7,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      100*FLT_EPSILON), AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform,
			      5*FLT_EPSILON), AipsError);
    }
    { // 4-D real<->complex FFT's on an odd/odd/odd/even len. (orig at centre)
      Array<Float> input(IPosition(4,3,5,7,4));
      input = 0.0f;
      input(IPosition(4,1,2,3,2)) = 1.0f;
      Array<Complex> result, expectedResult(IPosition(4,2,5,7,4));
      server.fft(result, input, True);
      expectedResult = Complex(1,0);
      AlwaysAssert(allNearAbs(result, expectedResult, FLT_EPSILON),
		   AipsError);
      Array<Float> reverseTransform;
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, FLT_EPSILON),
   		   AipsError);

      input = 1.0f;
      expectedResult = Complex(0,0);
      expectedResult(IPosition(4,0,2,3,2)) = Complex(3*5*7*4,0);
      server.fft(result, input);
      AlwaysAssert(allNearAbs(result, expectedResult,
			      500*FLT_EPSILON), AipsError);
      server.fft(reverseTransform, result);
      AlwaysAssert(allNearAbs(input, reverseTransform, 
			      5*FLT_EPSILON), AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tFFTServer"
// End: 
