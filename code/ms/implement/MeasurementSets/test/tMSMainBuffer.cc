//# tMSMainBuffer.cc:
//# Copyright (C) 1999
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
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#include <trial/MeasurementSets/MSMainBuffer.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Array.h>
#include <aips/Mathematics/Constants.h>

int main() {
  try {
    MSMainBuffer mainBuffer;
    { // test the correlation length functions
      AlwaysAssert(mainBuffer.numCorrelations() == 0, AipsError);
      mainBuffer.setCorrelations(4);
      AlwaysAssert(mainBuffer.numCorrelations() == 4, AipsError);
    }
    { // test the channel size functions
      AlwaysAssert(mainBuffer.numChannels() == 0, AipsError);
      mainBuffer.setChannels(32);
      AlwaysAssert(mainBuffer.numChannels() == 32, AipsError);
    }
    { // test the rows functions
      AlwaysAssert(mainBuffer.rows() == 0, AipsError);
      mainBuffer.addRows(5);
      AlwaysAssert(mainBuffer.rows() == 5, AipsError);
    }
    { // test the antenna1 functions
      for (uInt i = 0; i < mainBuffer.rows(); i++) {
	mainBuffer.antenna1(i) = Int(i) + 1;
      }
      Vector<Int> expectedResult(5);
      for (uInt i = 0; i < mainBuffer.rows(); i++) {
	AlwaysAssert(mainBuffer.antenna1(i) == Int(i+1), AipsError);
	expectedResult(i) = Int(i+1);
      }
      AlwaysAssert(allEQ(mainBuffer.antenna1(), expectedResult), 
		   AipsError);
      expectedResult += 2;
      Vector<Int> test(mainBuffer.antenna1());
      test = expectedResult;
      AlwaysAssert(allEQ(mainBuffer.antenna1(), expectedResult), 
		   AipsError);
    }
    { // test the complex data access functions
      AlwaysAssert(mainBuffer.data().shape().isEqual(IPosition(3,4,32,5)),
		   AipsError);
      Cube<Complex> data(mainBuffer.data());
      data = Complex(0.5, 1.1);
      AlwaysAssert(allNear(mainBuffer.data(),Complex(0.5,1.1), C::flt_epsilon),
		  AipsError);
      mainBuffer.data(0,0,0) = Complex(1.2,1.3);
      AlwaysAssert(allNear(mainBuffer.data(0,0,0),Complex(1.2,1.3), 
 			   C::flt_epsilon), AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tMSMainBuffer"
// End: 
