//# tMSFieldBuffer.cc:
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

// #include <trial/MeasurementSets/MSFieldBuffer.h>
// #include <casacore/casa/Arrays/ArrayLogical.h>
// #include <casacore/casa/Arrays/IPosition.h>
// #include <casacore/casa/Arrays/Matrix.h>
// #include <casacore/casa/BasicSL/Constants.h>
// #include <casacore/casa/BasicMath/Math.h>
// #include <casacore/measures/Measures/MDirection.h>
// #include <casacore/measures/Measures/MEpoch.h>

int main() {
//   const String filename = "tMSFieldBuffer_tmp.table";
//   try {
//     // Check the default constructor
//     MSFieldBuffer newBuffer;
//     // test the ok function.
//     AlwaysAssert(newBuffer.ok(), AipsError);
//     // test the addRow & nrow functions.
//     AlwaysAssert(newBuffer.nrow() == 0, AipsError);
//     newBuffer.addRow(20);
//     AlwaysAssert(newBuffer.ok(), AipsError);
//     AlwaysAssert(newBuffer.nrow() == 20, AipsError);
//     {
//       MSFieldBuffer fieldBuffer;
//       { // test the addRow & nrow functions.
//  	AlwaysAssert(fieldBuffer.nrow() == 0, AipsError);
//  	fieldBuffer.addRow(1);
//  	AlwaysAssert(fieldBuffer.nrow() == 1, AipsError);
//  	fieldBuffer.addRow(4, 1);
//  	AlwaysAssert(fieldBuffer.nrow() == 5, AipsError);
//       }
//       { // test the name functions.
// 	AlwaysAssert(fieldBuffer.name()(0) == String(""), AipsError);
// 	AlwaysAssert(fieldBuffer.name()(4) == String(""), AipsError);
// 	fieldBuffer.name().put(0, "row 0");
// 	fieldBuffer.name().put(4, "row 4");
//       }
//       { // test the code functions.
// 	AlwaysAssert(fieldBuffer.code()(0) == String(""), AipsError);
// 	AlwaysAssert(fieldBuffer.code()(4) == String(""), AipsError);
// 	fieldBuffer.code().put(0, "code 0");
// 	fieldBuffer.code().put(4, "code 4");
//       }
//       { // test the numPoly functions.
// 	AlwaysAssert(fieldBuffer.numPoly()(0) == 0, AipsError);
// 	AlwaysAssert(fieldBuffer.numPoly()(4) == 1, AipsError);
//       }
//       { // test the time functions.
//  	AlwaysAssert(near(fieldBuffer.time()(0), 0.0), AipsError);
//  	AlwaysAssert(near(fieldBuffer.time()(4), 0.0), AipsError);
// 	AlwaysAssert(fieldBuffer.timeMeas().getMeasRef().getType() == 
// 		     MEpoch::UTC, AipsError);
//   	fieldBuffer.time().put(0, 1.0);
//   	fieldBuffer.time().put(4, 2.0);
//       }
//       { // test the delayDir & delayFrame functions.
//  	AlwaysAssert(fieldBuffer.delayDir()(0).shape() == IPosition(2, 2, 1),
//  		     AipsError);
//  	AlwaysAssert(allNear(fieldBuffer.delayDir()(0), 0.0, C::dbl_epsilon),
//  		     AipsError);
//  	AlwaysAssert(fieldBuffer.delayDir()(4).shape() == IPosition(2, 2, 2),
//  		     AipsError);
//  	AlwaysAssert(allNear(fieldBuffer.delayDir()(4), 0.0, C::dbl_epsilon),
//  		     AipsError);
//  	AlwaysAssert(fieldBuffer.delayDirMeas(4, 0.0).getRef().getType()
// 		     == MDirection::J2000, AipsError);
//  	fieldBuffer.delayDir().put(0, Matrix<Double>(2, 1, 1.0));
//  	fieldBuffer.delayDir().put(4, Matrix<Double>(2, 2, 2.0));
//       }
//       { // test the phaseDir & phaseFrame functions.
// 	AlwaysAssert(fieldBuffer.phaseDir()(0).shape() == IPosition(2, 2, 1),
// 		     AipsError);
// 	AlwaysAssert(allNear(fieldBuffer.phaseDir()(0), 0.0, C::dbl_epsilon),
// 		     AipsError);
// 	AlwaysAssert(fieldBuffer.phaseDir()(4).shape() == IPosition(2, 2, 2),
// 		     AipsError);
// 	AlwaysAssert(allNear(fieldBuffer.phaseDir()(4), 0.0, C::dbl_epsilon),
// 		     AipsError);
//  	AlwaysAssert(fieldBuffer.phaseDirMeas(4, 0.0).getRef().getType()
// 		     == MDirection::J2000, AipsError);
// 	fieldBuffer.phaseDir().put(0, Matrix<Double>(2, 1, 20.0));
// 	fieldBuffer.phaseDir().put(4, Matrix<Double>(2, 2, 30.0));
//       }
//       { // test the referenceDir & referenceFrame functions.
// 	AlwaysAssert(fieldBuffer.referenceDir()(0).shape() == 
// 		     IPosition(2, 2, 1), AipsError);
// 	AlwaysAssert(allNear(fieldBuffer.referenceDir()(0), 0.0,
// 			     C::dbl_epsilon), AipsError);
// 	AlwaysAssert(fieldBuffer.referenceDir()(4).shape() ==
// 		     IPosition(2, 2, 2), AipsError);
// 	AlwaysAssert(allNear(fieldBuffer.referenceDir()(4), 0.0,
// 			     C::dbl_epsilon), AipsError);
//  	AlwaysAssert(fieldBuffer.referenceDirMeas(4, 0.0).getRef().getType()
// 		     == MDirection::J2000, AipsError);
// 	fieldBuffer.referenceDir().put(0, Matrix<Double>(2, 1, 10.0));
// 	fieldBuffer.referenceDir().put(4, Matrix<Double>(2, 2, 15.0));
//       }
//       { // test the sourceID functions.
// 	AlwaysAssert(fieldBuffer.sourceId()(0) == -1, AipsError);
// 	AlwaysAssert(fieldBuffer.sourceId()(4) == -1, AipsError);
// 	fieldBuffer.sourceId().put(0, 10);
// 	fieldBuffer.sourceId().put(4, 20);
//       }
//       { // test the flagRow functions.
// 	AlwaysAssert(fieldBuffer.flagRow()(0) == False, AipsError);
// 	AlwaysAssert(fieldBuffer.flagRow()(4) == False, AipsError);
// 	fieldBuffer.flagRow().put(3, True);
//       }
//       { // Check the assignment operator & copy constructor
// 	MSFieldBuffer otherBuffer(fieldBuffer);
// 	AlwaysAssert(otherBuffer.ok(), AipsError);
// 	AlwaysAssert(otherBuffer.nrow() == 5, AipsError);
// 	newBuffer = otherBuffer;
// 	AlwaysAssert(newBuffer.ok(), AipsError);
// 	// Check the reference semantics by adding data here and seeing if it
// 	// is mirrored into the newBuffer object.
// 	fieldBuffer.delayDir().put(1, Matrix<Double>(2, 2, 1.2));
// 	fieldBuffer.name().put(1, "name 1");
// 	fieldBuffer.flagRow().put(1, True);
// 	// Save the buffer to disk
// 	fieldBuffer.save(filename, True);
//       }
//     }
//     { // check the data has not been lost.
//       AlwaysAssert(newBuffer.nrow() == 5, AipsError);
//       AlwaysAssert(newBuffer.name()(0) == String("row 0"), AipsError);
//       AlwaysAssert(newBuffer.name()(4) == String("row 4"), AipsError);
//       AlwaysAssert(newBuffer.code()(0) == String("code 0"), AipsError);
//       AlwaysAssert(newBuffer.code()(4) == String("code 4"), AipsError);
//       AlwaysAssert(newBuffer.numPoly()(0) == 0, AipsError);
//       AlwaysAssert(newBuffer.numPoly()(4) == 1, AipsError);
//       AlwaysAssert(near(newBuffer.time()(0), 1.0), AipsError);
//       AlwaysAssert(near(newBuffer.time()(4), 2.0), AipsError);
//       AlwaysAssert(newBuffer.delayDir()(0).shape() == IPosition(2, 2, 1),
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.delayDir()(0), 1.0, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.delayDir()(4).shape() == IPosition(2, 2, 2),
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.delayDir()(4), 2.0, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.phaseDir()(0).shape() == IPosition(2, 2, 1),
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.phaseDir()(0), 20.0, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.phaseDir()(4).shape() == IPosition(2, 2, 2),
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.phaseDir()(4), 30.0, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.referenceDir()(0).shape() == IPosition(2, 2, 1),
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.referenceDir()(0), 10.0, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.referenceDir()(4).shape() == IPosition(2, 2, 2),
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.referenceDir()(4), 15.0, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.sourceId()(0) == 10, AipsError);
//       AlwaysAssert(newBuffer.sourceId()(4) == 20, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(0) == False, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(3) == True, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(4) == False, AipsError);
//       // check the reference semantics
//       AlwaysAssert(allNear(newBuffer.delayDir()(1), 1.2, C::dbl_epsilon),
//  		   AipsError);
//       AlwaysAssert(newBuffer.name()(1) == "name 1", AipsError);
//       AlwaysAssert(newBuffer.flagRow()(1) == True, AipsError);
//     }
//     { // Check the isValid functions
//       AlwaysAssert(newBuffer.isValid(True) == True, AipsError);
//       AlwaysAssert(newBuffer.isValid(3u) == True, AipsError);
//       AlwaysAssert(newBuffer.isValid() == True, AipsError);
//     }
//     { // Check the match functions
//       AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 1, 0.0)) == -1,
//  		   AipsError);
//       AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 1, 10.0), False)==0,
//  		   AipsError);
//       AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 2, 0.0)) == 2,
//  		   AipsError);
//       AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 2, 0.0), False)==3,
//  		   AipsError);
//     }
//   }
//   catch (AipsError x) {
//     cerr << x.getMesg() << endl;
//     cout << "FAIL" << endl;
//     return 1;
//   }
//   try {
//     // Check that the Table ended up on disk (after the save function).
//     MSField ms(filename);
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
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSFieldBuffer"
// End: 
