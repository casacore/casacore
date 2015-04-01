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

#include <casacore/casa/aips.h>
#include <casacore/casa/namespace.h>
// #include <trial/MeasurementSets/MSMainBuffer.h>
// #include <casacore/ms/MeasurementSets/MeasurementSet.h>
// #include <casacore/casa/Arrays/ArrayLogical.h>
// #include <casacore/casa/Arrays/Vector.h>
// #include <casacore/casa/Arrays/Cube.h>
// #include <casacore/casa/Arrays/Matrix.h>
// #include <casacore/casa/Exceptions/Error.h>
// #include <casacore/casa/BasicSL/Constants.h>
// #include <casacore/casa/BasicMath/Math.h>
// #include <aips/Exceptions/Excp.h>
// #include <casacore/tables/Tables/Table.h>
// #include <casacore/casa/Utilities/Assert.h>
// #include <casacore/casa/BasicSL/String.h>
// #include <casacore/casa/iostream.h>

int main() {
//   const String filename = "tMSMainBuffer_tmp.table";
//   try {
//     const IPosition row1DataShape(2, 2, 8);
//     const uInt nCat = 2;
//     const IPosition row1CatShape(3, row1DataShape(0), row1DataShape(1), nCat);
//     const IPosition row1SigmaShape(1, row1DataShape(0));
//     const IPosition row4DataShape(2, 4, 7);
//     const IPosition row4CatShape(3, row4DataShape(0), row4DataShape(1), nCat);
//     const IPosition row4SigmaShape(1, row4DataShape(0));
//     // Check the default constructor
//     MSMainBuffer newBuffer;
//     // test the ok function.
//     AlwaysAssert(newBuffer.ok(), AipsError);
//     // test the addRow & nrow functions.
//     AlwaysAssert(newBuffer.nrow() == 0, AipsError);
//     newBuffer.addRow(20, 4, 16);
//     AlwaysAssert(newBuffer.ok(), AipsError);
//     AlwaysAssert(newBuffer.nrow() == 20, AipsError);
//     {
//       MSMainBuffer buffer(nCat);
//       { // test the addRow & nrow functions.
//     	AlwaysAssert(buffer.nrow() == 0, AipsError);
//     	buffer.addRow(1, row1DataShape(0), row1DataShape(1));
//     	AlwaysAssert(buffer.nrow() == 1, AipsError);
//     	buffer.addRow(4, row4DataShape(0), row4DataShape(1));
//     	AlwaysAssert(buffer.nrow() == 5, AipsError);
//       }
//       { // test the antenna1 functions.
// 	AlwaysAssert(buffer.antenna1()(0) == -1, AipsError);
//     	AlwaysAssert(buffer.antenna1()(4) == -1, AipsError);
//      	buffer.antenna1().put(0, 0);
//      	buffer.antenna1().put(4, 1);
//       }
//       { // test the antenna2 functions.
// 	AlwaysAssert(buffer.antenna2()(0) == -1, AipsError);
//     	AlwaysAssert(buffer.antenna2()(4) == -1, AipsError);
//      	buffer.antenna2().put(0, 2);
//      	buffer.antenna2().put(4, 3);
//       }
//       { // test the arrayId functions.
// 	AlwaysAssert(buffer.arrayId()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.arrayId()(4) == 1, AipsError);
//      	buffer.arrayId().put(0, 4);
//      	buffer.arrayId().put(4, 2);
//       }
//       { // test the dataDescId functions.
// 	AlwaysAssert(buffer.dataDescId()(0) == -1, AipsError);
//     	AlwaysAssert(buffer.dataDescId()(4) == -1, AipsError);
//      	buffer.dataDescId().put(0, 10);
//      	buffer.dataDescId().put(4, 11);
//       }
//       { // test the exposure functions.
// 	AlwaysAssert(buffer.exposure()(0) < 0.0, AipsError);
//     	AlwaysAssert(buffer.exposure()(4) < 0.0, AipsError);
//      	buffer.exposure().put(0, 10.0);
//      	buffer.exposure().put(4, 20.0);
//       }
//       { // test the feed1 functions.
// 	AlwaysAssert(buffer.feed1()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.feed1()(4) == 1, AipsError);
//      	buffer.feed1().put(0, 4);
//      	buffer.feed1().put(4, 5);
//       }
//       { // test the feed2 functions.
// 	AlwaysAssert(buffer.feed2()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.feed2()(4) == 1, AipsError);
//      	buffer.feed2().put(0, 2);
//      	buffer.feed2().put(4, 3);
//       }
//       { // test the fieldId functions.
// 	AlwaysAssert(buffer.fieldId()(0) == -1, AipsError);
//     	AlwaysAssert(buffer.fieldId()(4) == -1, AipsError);
//      	buffer.fieldId().put(0, 13);
//      	buffer.fieldId().put(4, 14);
//       }
//       { // test the flag functions.
//  	AlwaysAssert(buffer.flag()(0).shape().isEqual(row1DataShape), 
//  		     AipsError);
//  	AlwaysAssert(allEQ(buffer.flag()(0), False), AipsError);
//  	AlwaysAssert(buffer.flag()(4).shape().isEqual(row4DataShape), 
//  		     AipsError);
//  	AlwaysAssert(allEQ(buffer.flag()(4), False), AipsError);
//       	buffer.flag().put(0, Matrix<Bool>(row1DataShape, True));
//  	Matrix<Bool> flag(row4DataShape, True);
//  	flag(0, 0) = False;
//       	buffer.flag().put(4, flag);
//       }
//       { // test the flagCategory functions.
//   	AlwaysAssert(buffer.flagCategory()(0).shape().isEqual(row1CatShape), 
//   		     AipsError);
//   	AlwaysAssert(allEQ(buffer.flagCategory()(0), False), AipsError);
//   	AlwaysAssert(buffer.flagCategory()(4).shape().isEqual(row4CatShape), 
//   		     AipsError);
//   	AlwaysAssert(allEQ(buffer.flagCategory()(4), False), AipsError);
//       	buffer.flagCategory().put(0, Cube<Bool>(row1CatShape, True));
//  	Cube<Bool> flag(row4CatShape, True);
//  	flag(0, 0, 0) = False;
//       	buffer.flagCategory().put(3, flag);
//       	buffer.flagCategory().put(4, flag);
//       }
//       { // test the flagRow functions.
//    	AlwaysAssert(buffer.flagRow()(0) == False, AipsError);
//    	AlwaysAssert(buffer.flagRow()(4) == False, AipsError);
//    	buffer.flagRow().put(2, True);
//    	buffer.flagRow().put(3, True);
//       }
//       { // test the interval functions.
// 	AlwaysAssert(buffer.interval()(0) < 0.0, AipsError);
//     	AlwaysAssert(buffer.interval()(4) < 0.0, AipsError);
//      	buffer.interval().put(0, 9.0);
//      	buffer.interval().put(4, 19.0);
//       }
//       { // test the observationId functions.
// 	AlwaysAssert(buffer.observationId()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.observationId()(4) == 1, AipsError);
//      	buffer.observationId().put(0, 21);
//      	buffer.observationId().put(4, 22);
//       }
//       { // test the processorId functions.
// 	AlwaysAssert(buffer.processorId()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.processorId()(4) == 1, AipsError);
//      	buffer.processorId().put(0, 23);
//      	buffer.processorId().put(4, 24);
//       }
//       { // test the scanNumber functions.
// 	AlwaysAssert(buffer.scanNumber()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.scanNumber()(4) == 1, AipsError);
//      	buffer.scanNumber().put(0, 25);
//      	buffer.scanNumber().put(4, 26);
//       }
//       { // test the sigma functions.
//   	AlwaysAssert(buffer.sigma()(0).shape().isEqual(row1SigmaShape), 
//   		     AipsError);
//   	AlwaysAssert(allNear(buffer.sigma()(0), 1.0f, C::flt_epsilon), 
// 		     AipsError);
//   	AlwaysAssert(buffer.sigma()(4).shape().isEqual(row4SigmaShape), 
//   		     AipsError);
//   	AlwaysAssert(allNear(buffer.sigma()(4), 1.0f, C::flt_epsilon),
// 		     AipsError);
//       	buffer.sigma().put(0, Vector<Float>(row1SigmaShape, 2.0));
//  	Vector<Float> sigma(row4SigmaShape, 3.0);
//  	sigma(0) = 4.0;
//       	buffer.sigma().put(4, sigma);
//       }
//       { // test the state_id functions.
// 	AlwaysAssert(buffer.stateId()(0) == 1, AipsError);
//     	AlwaysAssert(buffer.stateId()(4) == 1, AipsError);
//      	buffer.stateId().put(0, 32);
//      	buffer.stateId().put(4, 33);
//       }
//       { // test the time functions.
// 	AlwaysAssert(near(buffer.time()(0), 0.0f), AipsError);
// 	AlwaysAssert(near(buffer.time()(4), 0.0f), AipsError);
//      	buffer.time().put(0, 990.0);
//      	buffer.time().put(4, 991.0);
//       }
//       { // test the time_centroid functions.
// 	AlwaysAssert(near(buffer.timeCentroid()(0), 0.0f), AipsError);
// 	AlwaysAssert(near(buffer.timeCentroid()(4), 0.0f), AipsError);
//      	buffer.timeCentroid().put(0, 992.0);
//      	buffer.timeCentroid().put(4, 993.0);
//       }
//       { // test the uvw functions.
// 	Vector<Double> uvw(3, 1E100);
//  	AlwaysAssert(buffer.uvw()(0).shape().isEqual(IPosition(1,3)), 
//  		     AipsError);
//  	AlwaysAssert(allNear(buffer.uvw()(0), uvw, C::dbl_epsilon), AipsError);
//  	AlwaysAssert(buffer.uvw()(4).shape().isEqual(IPosition(1,3)), 
//  		     AipsError);
//  	AlwaysAssert(allNear(buffer.uvw()(4), uvw, C::dbl_epsilon), AipsError);
// 	uvw = 0.0;
//       	buffer.uvw().put(0, uvw);
// 	uvw(0) = 10.0;
//       	buffer.uvw().put(4, uvw);
//       }
//       { // test the weight functions.
// 	AlwaysAssert(allNear(buffer.weight()(0), 1.0f, C::flt_epsilon), 
// 		     AipsError);
// 	AlwaysAssert(allNear(buffer.weight()(4), 1.0f, C::flt_epsilon), 
// 		     AipsError);
//       	buffer.weight().put(0, Vector<Float>(row1SigmaShape, 0.1));
//  	Vector<Float> weight(row4SigmaShape, 0.2);
//  	weight(0) = 0.4;
//       	buffer.weight().put(4, weight);
//       }
//       { // Check the assignment operator & copy constructor
//     	MSMainBuffer otherBuffer(buffer);
//     	AlwaysAssert(otherBuffer.ok(), AipsError);
//     	AlwaysAssert(otherBuffer.nrow() == 5, AipsError);
//  	newBuffer = otherBuffer;
//     	AlwaysAssert(newBuffer.ok(), AipsError);
//  	// Check the reference semantics by adding data here and seeing if it
//  	// is mirrored into the newBuffer object.
//    	buffer.antenna1().put(1, 100);
//  	buffer.fieldId().put(1, 101);
// 	buffer.flagRow().put(1, True);
// 	// Save the buffer to disk
// 	buffer.save(filename, True);
//       }
//     }
//     { // check the data has not been lost.
//       AlwaysAssert(newBuffer.nrow() == 5, AipsError);
//       AlwaysAssert(newBuffer.antenna1()(0) ==  0, AipsError);
//       AlwaysAssert(newBuffer.antenna1()(4) ==  1, AipsError);
//       AlwaysAssert(newBuffer.antenna2()(0) ==  2, AipsError);
//       AlwaysAssert(newBuffer.antenna2()(4) ==  3, AipsError);
//       AlwaysAssert(newBuffer.arrayId()(0) ==  4, AipsError);
//       AlwaysAssert(newBuffer.arrayId()(4) ==  2, AipsError);
//       AlwaysAssert(newBuffer.dataDescId()(0) ==  10, AipsError);
//       AlwaysAssert(newBuffer.dataDescId()(4) ==  11, AipsError);
//       AlwaysAssert(near(newBuffer.exposure()(0), 10.0f), AipsError);
//       AlwaysAssert(near(newBuffer.exposure()(4), 20.0f), AipsError);
//       AlwaysAssert(newBuffer.feed1()(0) ==  4, AipsError);
//       AlwaysAssert(newBuffer.feed1()(4) ==  5, AipsError);
//       AlwaysAssert(newBuffer.feed2()(0) ==  2, AipsError);
//       AlwaysAssert(newBuffer.feed2()(4) ==  3, AipsError);
//       AlwaysAssert(newBuffer.fieldId()(0) ==  13, AipsError);
//       AlwaysAssert(newBuffer.fieldId()(4) ==  14, AipsError);
//       AlwaysAssert(newBuffer.flag()(0).shape().isEqual(row1DataShape), 
//  		   AipsError);
//       AlwaysAssert(allEQ(newBuffer.flag()(0), True), AipsError);
//       {
//  	Matrix<Bool> flag(row4DataShape, True);
//  	flag(0,0) = False;
//  	AlwaysAssert(newBuffer.flag()(4).shape().isEqual(row4DataShape), 
//  		     AipsError);
//  	AlwaysAssert(allEQ(newBuffer.flag()(4), flag), AipsError);
//       }
//       AlwaysAssert(newBuffer.flagCategory()(0).shape().isEqual(row1CatShape), 
//  		   AipsError);
//       AlwaysAssert(allEQ(newBuffer.flagCategory()(0), True), AipsError);
//       {
//  	Cube<Bool> flag(row4CatShape, True);
//  	flag(0,0, 0) = False;
//  	AlwaysAssert(newBuffer.flagCategory()(4).shape().isEqual(row4CatShape), 
//  		     AipsError);
//  	AlwaysAssert(allEQ(newBuffer.flagCategory()(4), flag), AipsError);
//       }
//       AlwaysAssert(newBuffer.flagRow()(2) == True, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(3) == True, AipsError);
//       AlwaysAssert(near(newBuffer.interval()(0), 9.0f), AipsError);
//       AlwaysAssert(near(newBuffer.interval()(4), 19.0f), AipsError);
//       AlwaysAssert(newBuffer.observationId()(0) == 21, AipsError);
//       AlwaysAssert(newBuffer.observationId()(4) == 22, AipsError);
//       AlwaysAssert(newBuffer.processorId()(0) == 23, AipsError);
//       AlwaysAssert(newBuffer.processorId()(4) == 24, AipsError);
//       AlwaysAssert(newBuffer.scanNumber()(0) == 25, AipsError);
//       AlwaysAssert(newBuffer.scanNumber()(4) == 26, AipsError);
//       AlwaysAssert(newBuffer.sigma()(0).shape().isEqual(row1SigmaShape), 
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.sigma()(0), 2.0f, C::flt_epsilon),
// 		   AipsError);
//       {
//   	Vector<Float> sigma(row4SigmaShape, 3.0);
//   	sigma(0) = 4.0;
//   	AlwaysAssert(newBuffer.sigma()(4).shape().isEqual(row4SigmaShape), 
//   		     AipsError);
//   	AlwaysAssert(allNear(newBuffer.sigma()(4), sigma, C::flt_epsilon),
// 		     AipsError);
//       }
//       AlwaysAssert(newBuffer.stateId()(0) == 32, AipsError);
//       AlwaysAssert(newBuffer.stateId()(4) == 33, AipsError);
//       AlwaysAssert(near(newBuffer.time()(0), 990.0f), AipsError);
//       AlwaysAssert(near(newBuffer.time()(4), 991.0f), AipsError);
//       AlwaysAssert(near(newBuffer.timeCentroid()(0), 992.0f), AipsError);
//       AlwaysAssert(near(newBuffer.timeCentroid()(4), 993.0f), AipsError);
//       {
// 	Vector<Double> uvw(3, 0.0);
//  	AlwaysAssert(newBuffer.uvw()(0).shape().isEqual(IPosition(1,3)), 
//  		     AipsError);
//  	AlwaysAssert(allNear(newBuffer.uvw()(0), uvw, C::dbl_epsilon), 
// 		     AipsError);
//  	AlwaysAssert(newBuffer.uvw()(4).shape().isEqual(IPosition(1,3)), 
//  		     AipsError);
// 	uvw(0) = 10.0;
//  	AlwaysAssert(allNear(newBuffer.uvw()(4), uvw, C::dbl_epsilon),
// 		     AipsError);
//       }
//       AlwaysAssert(newBuffer.weight()(0).shape().isEqual(row1SigmaShape), 
//  		   AipsError);
//       AlwaysAssert(allNear(newBuffer.weight()(0), 0.1f, C::flt_epsilon),
// 		   AipsError);
//       {
//   	Vector<Float> weight(row4SigmaShape, 0.2);
//   	weight(0) = 0.4;
//   	AlwaysAssert(newBuffer.weight()(4).shape().isEqual(row4SigmaShape), 
//   		     AipsError);
//   	AlwaysAssert(allNear(newBuffer.weight()(4), weight, C::flt_epsilon),
// 		     AipsError);
//       }
//       // check the reference semantics
//       AlwaysAssert(newBuffer.antenna1()(1) ==  100, AipsError);
//       AlwaysAssert(newBuffer.fieldId()(1) ==  101, AipsError);
//       AlwaysAssert(newBuffer.flagRow()(1) == True, AipsError);
//     }
//     { // Check the isValid functions
//       AlwaysAssert(newBuffer.isValid(True) == True, AipsError);
//       AlwaysAssert(newBuffer.isValid(3u) == False, AipsError);
//       AlwaysAssert(newBuffer.isValid(4u) == True, AipsError);
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
//     // Check that the Table ended up on disk (after the save function). This
//     // line may fail if the MS ctr checks for the existance of subtables (it
//     // doesn't at the moment).
//     MS ms(filename);
//     ms.markForDelete();
//   }
//   catch (AipsError x) {
//     cerr << x.getMesg() << endl;
//     cout << "FAIL" << endl;
//     return 1;
//   }
//   cout << "OK" << endl;
  return 3;  //untested
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSMainBuffer"
// End: 
