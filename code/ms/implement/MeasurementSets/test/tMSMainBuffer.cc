//# tMSMainBuffer.cc:
//# Copyright (C) 1999,2000
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
#include <aips/Tables/Table.h>
// #include <aips/Arrays/Vector.h>
// #include <aips/Arrays/ArrayLogical.h>
// #include <aips/Arrays/ArrayMath.h>
// #include <aips/Arrays/Array.h>
// #include <aips/Mathematics/Constants.h>

int main() {
  try {
    // Check the default constructor
    MSMainBuffer newBuffer;
    // test the ok function.
    AlwaysAssert(newBuffer.ok(), AipsError);
    // test the addRow & nrow functions.
    AlwaysAssert(newBuffer.nrow() == 0, AipsError);
    newBuffer.addRow(20);
    AlwaysAssert(newBuffer.ok(), AipsError);
    AlwaysAssert(newBuffer.nrow() == 20, AipsError);
    {
      MSMainBuffer buffer(1);
      { // test the addRow & nrow functions.
    	AlwaysAssert(buffer.nrow() == 1, AipsError);
    	buffer.addRow(4);
    	AlwaysAssert(buffer.nrow() == 5, AipsError);
      }
      { // test the antenna1 functions.
//     	AlwaysAssert(buffer.antenna1()(0) == -1, AipsError);
//    	AlwaysAssert(buffer.antenna1()(4) == -1, AipsError);
//     	buffer.antenna1().put(0, 0);
//     	buffer.antenna1().put(4, 1);
      }
//       { // test the polarizationId functions.
//  	AlwaysAssert(buffer.polarizationId()(0) == -1, AipsError);
//  	AlwaysAssert(buffer.polarizationId()(4) == -1, AipsError);
//  	buffer.polarizationId().put(0, 1);
//  	buffer.polarizationId().put(4, 3);
//       }
//       { // test the flagRow functions.
//   	AlwaysAssert(buffer.flagRow()(0) == False, AipsError);
//   	AlwaysAssert(buffer.flagRow()(4) == False, AipsError);
//   	buffer.flagRow().put(4, True);
//       }
//       { // Check the assignment operator & copy constructor
//    	MSDataDescBuffer otherBuffer(buffer);
//    	AlwaysAssert(otherBuffer.ok(), AipsError);
//    	AlwaysAssert(otherBuffer.nrow() == 5, AipsError);
// 	newBuffer = otherBuffer;
//    	AlwaysAssert(newBuffer.ok(), AipsError);
// 	// Check the reference semantics by adding data here and seeing if it
// 	// is mirrored into the newBuffer object.
// 	buffer.spectralWindowId().put(1, 100);
// 	buffer.polarizationId().put(1, 101);
// 	buffer.flagRow().put(1, True);
//       }
    }
//     { // check the data has not been lost.
//       AlwaysAssert(newBuffer.nrow() == 5, AipsError);
//       AlwaysAssert(newBuffer.spectralWindowId()(0) ==  0, AipsError);
//       AlwaysAssert(newBuffer.spectralWindowId()(4) ==  1, AipsError);
//       AlwaysAssert(newBuffer.polarizationId()(0) ==  1, AipsError);
//       AlwaysAssert(newBuffer.polarizationId()(4) ==  3, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(0) == False, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(4) == True, AipsError);
//       // check the reference semantics
//       AlwaysAssert(newBuffer.spectralWindowId()(1) ==  100, AipsError);
//       AlwaysAssert(newBuffer.polarizationId()(1) ==  101, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(1) == True, AipsError);
//     }
//     { // Check the isValid functions
//       AlwaysAssert(newBuffer.isValid(True) == False, AipsError);
//       AlwaysAssert(newBuffer.isValid(3u) == False, AipsError);
//       AlwaysAssert(newBuffer.isValid(4u) == True, AipsError);
//       AlwaysAssert(newBuffer.isValid() == False, AipsError);
//     }
//     { // Check the match functions
//     }

//     MSMainBuffer mainBuffer;
//     { // test the addRows, rows, nCorrelations & nChannels functions.
//       AlwaysAssert(mainBuffer.rows() == 0, AipsError);
//       mainBuffer.addRows(5);
//       AlwaysAssert(mainBuffer.rows() == 5, AipsError);
//       Matrix<Complex> data(4, 32);
//       data = Complex(0.5,1.1);
//       mainBuffer.data(0, data); 
//       AlwaysAssert(mainBuffer.nCorrelations(0) == 4, AipsError);
//       AlwaysAssert(mainBuffer.nChannels(0) == 32, AipsError);
//     }
//     { // test the antenna1 functions
//       for (uInt i = 0; i < mainBuffer.rows(); i++) {
//  	mainBuffer.antenna1(i, i+1);
//       }
//       Vector<Int> expectedResult(5);
//       for (uInt i = 0; i < mainBuffer.rows(); i++) {
//  	const Int ip1 =  static_cast<Int>(i) + 1;
//   	AlwaysAssert(mainBuffer.antenna1(i) == ip1, AipsError);
//   	expectedResult(i) = ip1;
//       }
//       AlwaysAssert(allEQ(mainBuffer.antenna1(), expectedResult), AipsError);
//     }
//     { // test the complex data access functions
//       AlwaysAssert(mainBuffer.data(0).shape().isEqual(IPosition(2,4,32)),
//  		   AipsError);
//       Matrix<Complex> data(4, 32);
//       data = Complex(0.5, 1.1);
//       AlwaysAssert(allNear(mainBuffer.data(0), Complex(0.5,1.1), 
// 			   C::flt_epsilon),
// 		   AipsError);
//     }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSMainBuffer"
// End: 
