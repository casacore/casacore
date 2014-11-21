//# tMSPolBuffer.cc:
//# Copyright (C) 2000
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

#include <casacore/casa/aips.h>
#include <casacore/casa/namespace.h>
// #include <casacore/casa/Exceptions/Error.h>
// #include <aips/Exceptions/Excp.h>
// #include <casacore/casa/Utilities/Assert.h>
// #include <casacore/casa/BasicSL/String.h>
// #include <casacore/casa/iostream.h>

// #include <trial/MeasurementSets/MSPolBuffer.h>
// #include <casacore/casa/Arrays/ArrayLogical.h>
// #include <casacore/casa/Arrays/IPosition.h>
// #include <casacore/casa/Arrays/Matrix.h>
// #include <casacore/casa/Arrays/Vector.h>
// // #include <casacore/casa/BasicSL/Constants.h>
// // #include <casacore/casa/BasicMath/Math.h>
// #include <casacore/measures/Measures/Stokes.h>

int main() {
//   const String filename = "tMSPolBuffer_tmp.table";
//   try {
//     // Check the default constructor
//     MSPolarizationBuffer newBuffer;
//     // test the ok function.
//     AlwaysAssert(newBuffer.ok(), AipsError);
//     // test the addRow & nrow functions.
//     AlwaysAssert(newBuffer.nrow() == 0, AipsError);
//     newBuffer.addRow(20, 1);
//     AlwaysAssert(newBuffer.ok(), AipsError);
//     AlwaysAssert(newBuffer.nrow() == 20, AipsError);
//     {
//       MSPolarizationBuffer polBuffer;
//       { // test the addRow & nrow functions.
//   	AlwaysAssert(polBuffer.nrow() == 0, AipsError);
//   	polBuffer.addRow(1, 1);
//   	AlwaysAssert(polBuffer.nrow() == 1, AipsError);
//   	polBuffer.addRow(4, 4);
//   	AlwaysAssert(polBuffer.nrow() == 5, AipsError);
//       }
//       { // test the numCorr functions.
//  	AlwaysAssert(polBuffer.numCorr()(0) == 1, AipsError);
//  	AlwaysAssert(polBuffer.numCorr()(4) == 4, AipsError);
//       }
//       { // test the corrType functions.
//   	AlwaysAssert(polBuffer.corrType()(0).shape() == IPosition(1, 1),
//   		     AipsError);
//   	AlwaysAssert(allEQ(polBuffer.corrType()(0), 
// 			   static_cast<Int>(Stokes::Undefined)), AipsError);
//   	AlwaysAssert(polBuffer.corrType()(4).shape() == IPosition(1, 4),
//   		     AipsError);
//   	AlwaysAssert(allEQ(polBuffer.corrType()(4), 
// 			   static_cast<Int>(Stokes::Undefined)), AipsError);
//   	polBuffer.corrType()
// 	  .put(0, Vector<Int>(1, static_cast<Int>(Stokes::RR)));
// 	Vector<Int> ct(4);
// 	ct(0) =  static_cast<Int>(Stokes::XX);
// 	ct(1) =  static_cast<Int>(Stokes::XY);
// 	ct(2) =  static_cast<Int>(Stokes::YY);
// 	ct(3) =  static_cast<Int>(Stokes::YX);
//   	polBuffer.corrType().put(4, ct);
//       }
//       { // test the corrProduct functions.
//   	AlwaysAssert(polBuffer.corrProduct()(0).shape() == IPosition(2, 2, 1),
//   		     AipsError);
//   	AlwaysAssert(allEQ(polBuffer.corrProduct()(0), 0), AipsError);
//   	AlwaysAssert(polBuffer.corrProduct()(4).shape() == IPosition(2, 2, 4),
//   		     AipsError);
//   	AlwaysAssert(allEQ(polBuffer.corrProduct()(4), 0), AipsError);
//    	polBuffer.corrProduct().put(0, Matrix<Int>(2, 1, 1));
//  	Matrix<Int> pr(2, 4, 0);
// 	pr(1,1) = pr(2,0) = pr(2,1) = pr(3,0) = 1;
//    	polBuffer.corrProduct().put(4, pr);
//       }
//       { // test the flagRow functions.
//  	AlwaysAssert(polBuffer.flagRow()(0) == False, AipsError);
//  	AlwaysAssert(polBuffer.flagRow()(4) == False, AipsError);
//  	polBuffer.flagRow().put(3, True);
//       }
//       { // Check the assignment operator & copy constructor
//  	MSPolarizationBuffer otherBuffer(polBuffer);
//  	AlwaysAssert(otherBuffer.ok(), AipsError);
//  	AlwaysAssert(otherBuffer.nrow() == 5, AipsError);
//  	newBuffer = otherBuffer;
//  	AlwaysAssert(newBuffer.ok(), AipsError);
//  	// Check the reference semantics by adding data here and seeing if it
//  	// is mirrored into the newBuffer object.
//   	polBuffer.corrType()
// 	  .put(1, Vector<Int>(4, static_cast<Int>(Stokes::I)));
//    	polBuffer.corrProduct().put(1, Matrix<Int>(2, 4, 1));
//   	polBuffer.flagRow().put(1, True);
//  	// Save the buffer to disk
//  	polBuffer.save(filename, True);
//       }
//     }
//     { // check the data has not been lost.
//       AlwaysAssert(newBuffer.nrow() == 5, AipsError);
//       AlwaysAssert(newBuffer.numCorr()(0) == 1, AipsError);
//       AlwaysAssert(newBuffer.numCorr()(4) == 4, AipsError);
//       AlwaysAssert(newBuffer.corrType()(0).shape() == IPosition(1, 1),
// 		   AipsError);
//       AlwaysAssert(allEQ(newBuffer.corrType()(0), 
// 			 static_cast<Int>(Stokes::RR)), AipsError);
//       AlwaysAssert(newBuffer.corrType()(4).shape() == IPosition(1, 4),
// 		   AipsError);
//       Vector<Int> ct(4);
//       ct(0) =  static_cast<Int>(Stokes::XX);
//       ct(1) =  static_cast<Int>(Stokes::XY);
//       ct(2) =  static_cast<Int>(Stokes::YY);
//       ct(3) =  static_cast<Int>(Stokes::YX);
//       AlwaysAssert(allEQ(newBuffer.corrType()(4), ct), AipsError);
//       AlwaysAssert(newBuffer.corrProduct()(0).shape() == IPosition(2, 2, 1),
// 		   AipsError);
//       AlwaysAssert(allEQ(newBuffer.corrProduct()(0), 1), AipsError);
//       AlwaysAssert(newBuffer.corrProduct()(4).shape() == IPosition(2, 2, 4),
// 		   AipsError);
//       Matrix<Int> pr(2, 4, 0);
//       pr(1,1) = pr(2,0) = pr(2,1) = pr(3,0) = 1;
//       AlwaysAssert(allEQ(newBuffer.corrProduct()(4), pr), AipsError);
//       AlwaysAssert(newBuffer.flagRow()(0) == False, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(3) == True, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(4) == False, AipsError);
//       // check the reference semantics
//       AlwaysAssert(allEQ(newBuffer.corrType()(1), 
// 			 static_cast<Int>(Stokes::I)), AipsError);
//       AlwaysAssert(allEQ(newBuffer.corrProduct()(1), 1), AipsError);
//       AlwaysAssert(newBuffer.flagRow()(1) == True, AipsError);
//     }
//     { // Check the isValid functions
//       AlwaysAssert(newBuffer.isValid(True) == False, AipsError);
//       AlwaysAssert(newBuffer.isValid(4u) == True, AipsError);
//       AlwaysAssert(newBuffer.isValid(3u) == False, AipsError);
//       AlwaysAssert(newBuffer.isValid(2u) == False, AipsError);
//       AlwaysAssert(newBuffer.isValid() == False, AipsError);
//     }
//     { // Check the match functions
//     }
//   }
//   catch (AipsError x) {
//     cerr << x.getMesg() << endl;
//     cout << "FAIL" << endl;
//     return 1;
//   }
//   try {
//     // Check that the Table ended up on disk (after the save function).
//     MSPolarization ms(filename);
//     ms.markForDelete();
//   }
//   catch (AipsError x) {
//     cerr << x.getMesg() << endl;
//     cout << "FAIL" << endl;
//     return 1;
//   }
//   cout << "OK" << endl;
  return 3;   //untested
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSPolBuffer"
// End: 
