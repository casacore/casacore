//# tPagedArrIter.cc:  mechanical test of the {RO_,}PagedArrIter class
//# Copyright (C) 1995,1996,1997
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

#include <aips/aips.h>
#include <trial/Lattices/PagedArrIter.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/OS/Timer.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>

int main ()
{
  try {
    {
      cout << "Creating a Paged Array on disk" << endl;
      const IPosition latticeShape(4, 16, 12, 4, 32);
      PagedArray<Int> pagedArr(latticeShape, "tPagedArrIter_saved_tmp.table");
      Array<Int> arr;
      pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
 			latticeShape, IPosition(latticeShape.nelements(), 1));
      indgen(arr);
      pagedArr.putSlice(arr, IPosition(latticeShape.nelements(), 0));
    }
    //++++++++++++++++++++ Test RO_PagedArrIter ++++++++++++++++++++
    // Check the Iterator with a Vector cursor. 
    // Eventually I will be able to open the Table for read only access
    cout << "Testing the RO iterator" << endl;
    {
      cout << "Testing using a Vector cursor" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      LatticeStepper step(latticeShape, cursorShape);
      RO_PagedArrIter<Int>  iter(pagedArr, step);
      Vector<Int> expectedResult(latticeShape(0)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
		   == True, AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
  		   == True, AipsError);
      try {
  	Matrix<Int> temp(iter.matrixCursor());
  	throw(AipsError("tPagedArrIter - "
  			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
 	if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
 	  cerr << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
  	}
      } end_try;
      try {
 	Cube<Int> temp(iter.cubeCursor());
 	throw(AipsError("tPagedArrIter - "
 			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
 	if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
 	  cerr << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
 	}
      } end_try;

      AlwaysAssert(latticeShape == iter.latticeShape(), AipsError);
      AlwaysAssert(cursorShape == iter.cursorShape().nonDegenerate(),
		   AipsError);
      Timer clock;
      for (iter.reset(); !iter.atEnd(); iter++){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		     == True, AipsError);
 	expectedResult.ac() += cursorShape.product();
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product()/latticeShape(0) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= cursorShape.product();
      for (; !iter.atStart(); iter--){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
		     == True, AipsError);
 	expectedResult.ac() -= cursorShape.product();
      }
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape.product()/latticeShape(0)-1),
 		   AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos(0) = latticeShape(0) - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      cout << "Testing using a Matrix cursor" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
      RO_PagedArrIter<Int>  iter(pagedArr, cursorShape);
      Matrix<Int> expectedResult(latticeShape(0), latticeShape(1)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac())
		   == True, AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
  		   == True, AipsError);
      try {
 	Vector<Int> temp(iter.vectorCursor());
 	throw(AipsError("tPagedArrIter - "
 			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
	if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
	  cerr << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      try {
 	Cube<Int> temp(iter.cubeCursor());
 	throw(AipsError("tPagedArrIter - "
 			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
	if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
	  cerr << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); iter++){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac()) 
 		     == True, AipsError);
 	expectedResult.ac() += cursorShape.product();
      }
      AlwaysAssert(iter.nsteps() == latticeShape(2)*latticeShape(3) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= cursorShape.product();
      for (; !iter.atStart(); --iter){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
 		     == True, AipsError);
 	expectedResult.ac() -= cursorShape.product();
      }
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape(2)*latticeShape(3)-1),
 		   AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos(0) = latticeShape(0) - 1;
      expectedPos(1) = latticeShape(1) - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with a Cube cursor. 
    {
      cout << "Testing using a Cube cursor" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				  latticeShape(2));
      LatticeStepper step(latticeShape, cursorShape);
      RO_LatticeIterator<Int>  iter(pagedArr, step);
      Cube<Int> expectedResult(latticeShape(0), latticeShape(1), 
			       latticeShape(2)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac()) == True,
		   AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
 		   == True, AipsError);
      try {
	Vector<Int> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Int> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); iter++){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac())
		     == True, AipsError);
	expectedResult.ac() += cursorShape.product();
      }
      AlwaysAssert(iter.nsteps() == latticeShape(3) - 1, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0; expectedPos(2) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= cursorShape.product();
      for (; !iter.atStart(); iter--){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
		     == True, AipsError);
	expectedResult.ac() -= cursorShape.product();
      }
      clock.show();
      AlwaysAssert(iter.nsteps() == 2 * (latticeShape(3) - 1), AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos(0) = latticeShape(0) - 1;
      expectedPos(1) = latticeShape(1) - 1;
      expectedPos(2) = latticeShape(2) - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with an Array cursor. 
    {
      cout << "Testing using an Array (4-D) cursor" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(latticeShape);
      RO_LatticeIterator<Int>  iter(pagedArr, cursorShape);
      Array<Int> expectedResult(latticeShape); 
      indgen(expectedResult);
      AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
      try {
	Vector<Int> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Int> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Int> temp(iter.cubeCursor());
	throw(AipsError("tPagedArrIter - "
			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); ++iter){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True, 
		     AipsError);
	expectedResult.ac() += cursorShape.product();
      }
      AlwaysAssert(iter.nsteps() == 0, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= cursorShape.product();
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True,
		     AipsError);
	expectedResult.ac() -= cursorShape.product();
      }
      clock.show();
      AlwaysAssert(iter.nsteps() == 0, AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos = latticeShape - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with a single element cursor (this is very slow)
    {
      cout << "Testing using a single element cursor" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,1);
      LatticeStepper step(latticeShape, cursorShape);
      RO_PagedArrIter<Int>  iter(pagedArr, step);
      Array<Int> expectedResult(cursorShape); 
      indgen(expectedResult);
      AlwaysAssert(allEQ(expectedResult, iter.vectorCursor().ac()) == True,
		   AipsError);
      AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
		   == True, AipsError);
      try {
	Matrix<Int> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Int> temp(iter.cubeCursor());
	throw(AipsError("tPagedArrIter - "
			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); ++iter){
	AlwaysAssert(allEQ(expectedResult, iter.vectorCursor().ac()) == True,
		     AipsError);
	expectedResult += cursorShape.product();
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product() - 1, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult -= cursorShape.product();
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
		     == True, AipsError);
	expectedResult -= cursorShape.product();
      }
      clock.show();
      AlwaysAssert(iter.nsteps() == 2*(latticeShape.product() - 1), AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the copy constructor and assignment operator
    {
      cout << "Testing the copy constructor and assignment operator" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      RO_PagedArrIter<Int> iter(pagedArr, 
				LatticeStepper(latticeShape, cursorShape));
      iter++;
      Vector<Int> expectedResult(latticeShape(0)); 
      indgen(expectedResult.ac()); 
      expectedResult.ac() += cursorShape.product();
      AlwaysAssert(allEQ(expectedResult.ac(),iter.vectorCursor().ac()) == True,
		   AipsError);

      RO_PagedArrIter<Int> iterCopy(iter);
      Vector<Int> expectedCopy(expectedResult.copy());
      AlwaysAssert(allEQ(expectedCopy.ac(), iterCopy.vectorCursor().ac())
		   == True, AipsError);
      iter++; expectedResult.ac() += cursorShape.product();
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		   == True, AipsError);
      AlwaysAssert(allEQ(expectedCopy.ac(), iterCopy.vectorCursor().ac()) 
 		   == True, AipsError);
      iterCopy--; 
      expectedCopy.ac() -= cursorShape.product();
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		   == True, AipsError);
      AlwaysAssert(allEQ(expectedCopy.ac(), iterCopy.vectorCursor().ac()) 
 		   == True, AipsError);
      AlwaysAssert(allEQ(iter.vectorCursor().ac(),iterCopy.vectorCursor().ac())
 		   == False, AipsError);
      iterCopy = iter;
      expectedCopy = expectedResult;
      AlwaysAssert(allEQ(iter.vectorCursor().ac(),iterCopy.vectorCursor().ac())
 		   == True, AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		   == True, AipsError);
      iterCopy++;
      expectedCopy.ac() += cursorShape.product();
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		   == True, AipsError);
      AlwaysAssert(allEQ(expectedCopy.ac(), iterCopy.vectorCursor().ac()) 
 		   == True, AipsError);
      AlwaysAssert(allEQ(iter.vectorCursor().ac(),iterCopy.vectorCursor().ac())
 		   == False, AipsError);
    }
    // Test the non-congruent cursor handling
    {
      cout << "Testing using a non-congruent cursor" << endl;
      const PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      IPosition cursorShape(2,9);
      LatticeStepper step(latticeShape, cursorShape);
      RO_PagedArrIter<Int>  iter(pagedArr, step);
      Matrix<Int> expectedResult(cursorShape); 
      Vector<Int> oneRow(cursorShape(0));
      indgen(oneRow.ac());
      uInt i;
      for (i = 0; i < cursorShape(1); i++) {
	expectedResult.column(i) = oneRow;
	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()),
		   AipsError);
      iter++;
      indgen(oneRow.ac(), cursorShape(0)); 
      for (i = 0; i < cursorShape(1); i++) {
	oneRow(7) = 0;
	oneRow(8) = 0;
	expectedResult.column(i) = oneRow;
	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()),
		   AipsError);
      iter++;
      expectedResult = 0;
      indgen(oneRow.ac(), cursorShape(0)*latticeShape(0)); 
      for (i = 0; i < 3; i++) {
	expectedResult.column(i) = oneRow;
	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()),
		   AipsError);
      iter++; 
      expectedResult = 0;
      indgen(oneRow.ac(), cursorShape(0)*(latticeShape(0)+1)); 
      for (i = 0; i < 3; i++) {
	oneRow(7) = 0;
	oneRow(8) = 0;
	expectedResult.column(i) = oneRow;
	oneRow.ac() += latticeShape(0);
      }
      cursorShape = 5;
      step.setCursorShape(cursorShape);
      step.subSection(IPosition(4, 3,0,0,0), latticeShape-1, 
 		      IPosition(4, 2,2,1,1));
      RO_PagedArrIter<Int>  subIter(pagedArr, step);

      oneRow.resize(5);
      Matrix<Int> expectedResult1(5,5);
      expectedResult1 = 0;
      indgen(oneRow.ac(), 3, 2); 
      for (i = 0; i < 5; i++) {
 	expectedResult1.column(i) = oneRow;
 	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResult1.ac(),
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter++;
      Matrix<Int> expectedResult2(5,5);
      expectedResult2 = 0;
      indgen(oneRow.ac(), 13, 2); 
      for (i = 0; i < 5; i++) {
 	oneRow(2) = 0;
 	oneRow(3) = 0;
 	oneRow(4) = 0;
 	expectedResult2.column(i) = oneRow;
 	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResult2.ac(),
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter++;
      Matrix<Int> expectedResult3(5,5);
      expectedResult3 = 0;
      indgen(oneRow.ac(), 163, 2); 
      for (i = 0; i < 1; i++) {
 	expectedResult3.column(i) = oneRow;
 	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResult3.ac(),
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter++;
      Matrix<Int> expectedResult4(5,5);
      expectedResult4 = 0;
      indgen(oneRow.ac(), 173, 2); 
       for (i = 0; i < 1; i++) {
	 oneRow(2) = 0;
	 oneRow(3) = 0;
	 oneRow(4) = 0;
	 expectedResult4.column(i) = oneRow;
	 oneRow.ac() += 32;
       }
       AlwaysAssert(allEQ(expectedResult4.ac(),
			  subIter.cursor().nonDegenerate()), AipsError);
       subIter--;
       AlwaysAssert(allEQ(expectedResult3.ac(),
			  subIter.cursor().nonDegenerate()), AipsError);
       subIter--;
       AlwaysAssert(allEQ(expectedResult2.ac(),
			  subIter.cursor().nonDegenerate()), AipsError);
       subIter--;
       AlwaysAssert(allEQ(expectedResult1.ac(),
			  subIter.cursor().nonDegenerate()), AipsError);
    }
    //++++++++++++++++++++ Test PagedArrIter ++++++++++++++++++++
    cout << "Testing the RW iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      cout << "Testing using a Vector cursor" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      LatticeStepper step(latticeShape, cursorShape);
      PagedArrIter<Int>  iter(pagedArr, step);
      Vector<Int> expectedResult(latticeShape(0)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allEQ(expectedResult.ac(),iter.vectorCursor().ac()) == True,
		   AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
  		   == True, AipsError);
      try {
  	Matrix<Int> temp(iter.matrixCursor());
  	throw(AipsError("tPagedArrIter - "
  			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
 	if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
 	  cerr << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
  	}
      } end_try;
      try {
 	Cube<Int> temp(iter.cubeCursor());
 	throw(AipsError("tPagedArrIter - "
 			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
 	if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
 	  cerr << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
 	}
      } end_try;

      AlwaysAssert(latticeShape == iter.latticeShape(), AipsError);
      AlwaysAssert(cursorShape == iter.cursorShape().nonDegenerate(),
		   AipsError);
      Timer clock;
      for (iter.reset(); !iter.atEnd(); iter++){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		     == True, AipsError);
	iter.vectorCursor()(0) -= expectedResult(0);
 	expectedResult.ac() += cursorShape.product();
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product()/latticeShape(0) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= cursorShape.product();
      expectedResult(0) = 0;
      for (; !iter.atStart(); iter--){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		     == True, AipsError);
	iter.cursor() = 1;
	expectedResult.ac() -= cursorShape.product();
	expectedResult(0) = 0;
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		   == True, AipsError);
      iter.cursor() = 1;
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape.product()/latticeShape(0)-1),
 		   AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos(0) = latticeShape(0) - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      cout << "Testing using a Matrix cursor" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
      PagedArrIter<Int>  iter(pagedArr, cursorShape);
      Matrix<Int> expectedResult(latticeShape(0), latticeShape(1)); 
      expectedResult = 1.0;
      AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac())
		   == True, AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
  		   == True, AipsError);
      try {
 	Vector<Int> temp(iter.vectorCursor());
 	throw(AipsError("tPagedArrIter - "
 			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
	if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
	  cerr << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      try {
 	Cube<Int> temp(iter.cubeCursor());
 	throw(AipsError("tPagedArrIter - "
 			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
	if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
	  cerr << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); iter++){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac()) 
 		     == True, AipsError);
	iter.matrixCursor()(0,0) = 2;
      }
      AlwaysAssert(iter.nsteps() == latticeShape(2)*latticeShape(3) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult(0,0) = 2;
      for (; !iter.atStart(); --iter){
 	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
 		     == True, AipsError);
	iter.cursor() = 3;
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		   == True, AipsError);
      iter.cursor() = 3;
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape(2)*latticeShape(3)-1),
 		   AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos(0) = latticeShape(0) - 1;
      expectedPos(1) = latticeShape(1) - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with a Cube cursor. 
    {
      cout << "Testing using a Cube cursor" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				  latticeShape(2));
      LatticeStepper step(latticeShape, cursorShape);
      LatticeIterator<Int>  iter(pagedArr, step);
      Cube<Int> expectedResult(latticeShape(0), latticeShape(1),
			       latticeShape(2)); 
      expectedResult.ac() = 3;
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac()) == True,
		   AipsError);
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
 		   == True, AipsError);
      try {
	Vector<Int> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Int> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); iter++){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac())
		     == True, AipsError);
        iter.cubeCursor()(0,0,0) = 4;
      }
      AlwaysAssert(iter.nsteps() == latticeShape(3) - 1, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0; expectedPos(2) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult(0,0,0) = 4;
      for (; !iter.atStart(); iter--){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
		     == True, AipsError);
	iter.cursor() = 5;
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		   == True, AipsError);
      iter.cursor() = 5;
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape(3)-1), AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos(0) = latticeShape(0) - 1;
      expectedPos(1) = latticeShape(1) - 1;
      expectedPos(2) = latticeShape(2) - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with an Array cursor. 
    {
      cout << "Testing using an Array (4-D) cursor" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(latticeShape);
      LatticeIterator<Int>  iter(pagedArr, cursorShape);
      Array<Int> expectedResult(latticeShape); 
      expectedResult = 5;
      AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
      try {
	Vector<Int> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Int> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Int> temp(iter.cubeCursor());
	throw(AipsError("tPagedArrIter - "
			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); ++iter){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True,
		     AipsError);
	iter.cursor()(IPosition(4,0)) = 6;
      }
      AlwaysAssert(iter.nsteps() == 0, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult(IPosition(4,0)) = 6;
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True,
		     AipsError);
	iter.cursor() = 7;
      }
      AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True,
		   AipsError);
      iter.cursor() = 7;
      clock.show();
      AlwaysAssert(iter.nsteps() == 0, AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedPos = latticeShape - 1;
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the Iterator with a single element cursor (this is very slow)
    {
      cout << "Testing using a single element cursor" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,1);
      LatticeStepper step(latticeShape, cursorShape);
      PagedArrIter<Int>  iter(pagedArr, step);
      Array<Int> expectedResult(cursorShape); 
      expectedResult = 7;
      AlwaysAssert(allEQ(expectedResult, iter.vectorCursor().ac()) == True, 
		   AipsError);
      AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
		   == True, AipsError);
      try {
	Matrix<Int> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Int> temp(iter.cubeCursor());
	throw(AipsError("tPagedArrIter - "
			"cubeCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 3-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      Timer clock;
      for (iter.reset(); !iter.atEnd(); ++iter){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
		     == True, AipsError);
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product() - 1, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		     == True, AipsError);
      }
      clock.show();
      AlwaysAssert(iter.nsteps() == 2*(latticeShape.product() - 1), AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    }
    // Check the copy constructor and assignment operator
    {
      cout << "Testing the copy constructor and assignment operator" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      PagedArrIter<Int> iter(pagedArr, 
			     LatticeStepper(latticeShape, cursorShape));
      iter++;
      Vector<Int> expectedResult(latticeShape(0)); 
      expectedResult = 7;
      AlwaysAssert(allEQ(expectedResult.ac(),iter.vectorCursor().ac()) == True,
		   AipsError);

      PagedArrIter<Int> iterCopy(iter);
      AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
 		   == True, AipsError);
      iter++; iter.cursor() = 2;
      expectedResult.ac() = 2;
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
 		   == True, AipsError);
      expectedResult.ac() = 7;
      AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
  		   == True, AipsError);
      iterCopy--; iterCopy.cursor() = 0;
      expectedResult.ac() = 2;
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
   		   == True, AipsError);
      expectedResult.ac() = 0;
      AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
   		   == True, AipsError);

      iterCopy = iter;
      AlwaysAssert(allEQ(iter.vectorCursor().ac(),
			 iterCopy.vectorCursor().ac()) == True, AipsError);
      expectedResult.ac() = 2;
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
 		   == True, AipsError);
      iter++;
      expectedResult.ac() = 7;
      AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
 		   == True, AipsError);
      expectedResult.ac() = 2;
      AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
 		   == True, AipsError);
      --iterCopy; iterCopy--;
      expectedResult.ac() = 0;
      AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
 		   == True, AipsError);
    }
    // Test the non-congruent cursor handling
    {
      cout << "Testing using a non-congruent cursor" << endl;
      PagedArray<Int> pagedArr("tPagedArrIter_saved_tmp.table");
      const IPosition latticeShape(pagedArr.shape());
      {
	Array<Int> arr;
	pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			  latticeShape, IPosition(latticeShape.nelements(),1));
	indgen(arr);
	pagedArr.putSlice(arr, IPosition(latticeShape.nelements(), 0));
      }
      IPosition cursorShape(2,9);
      LatticeStepper step(latticeShape, cursorShape);
      PagedArrIter<Int> iter(pagedArr, step);
      Matrix<Int> expectedResult1(cursorShape); 
      Vector<Int> oneRow(cursorShape(0));
      indgen(oneRow.ac());
      uInt i;
      for (i = 0; i < cursorShape(1); i++) {
 	expectedResult1.column(i) = oneRow;
 	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult1.ac(), iter.cursor().nonDegenerate()),
		   AipsError);
      iter.cursor() = -1 * iter.cursor() - 1;
      iter++;
      Matrix<Int> expectedResult2(cursorShape); 
      indgen(oneRow.ac(), cursorShape(0)); 
      for (i = 0; i < cursorShape(1); i++) {
  	oneRow(7) = 0;
  	oneRow(8) = 0;
  	expectedResult2.column(i) = oneRow;
  	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult2.ac(), iter.cursor().nonDegenerate()),
			 AipsError);
      iter.cursor() = -1 * iter.cursor() - 1;
      iter++;
      Matrix<Int> expectedResult3(cursorShape); 
      expectedResult3 = 0;
      indgen(oneRow.ac(), cursorShape(0)*latticeShape(0)); 
      for (i = 0; i < 3; i++) {
  	expectedResult3.column(i) = oneRow;
  	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult3.ac(), iter.cursor().nonDegenerate()),
		   AipsError);
      iter.cursor() = -1 * iter.cursor() - 1;
      iter++; 
      Matrix<Int> expectedResult4(cursorShape); 
      expectedResult4 = 0;
      indgen(oneRow.ac(), cursorShape(0)*(latticeShape(0)+1)); 
      for (i = 0; i < 3; i++) {
  	oneRow(7) = 0;
 	oneRow(8) = 0;
  	expectedResult4.column(i) = oneRow;
  	oneRow.ac() += latticeShape(0);
      }
      AlwaysAssert(allEQ(expectedResult4.ac(), iter.cursor().nonDegenerate()),
		   AipsError);
      iter.cursor() = -1 * iter.cursor() - 1;
      iter--; iter++;
      iter.matrixCursor().ac() += expectedResult4.ac();
      {
	Array<Int> m(iter.matrixCursor()(IPosition(2,0),IPosition(2,6,2)));
	m += 1;
	AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
      }
      iter--;
      iter.matrixCursor().ac() += expectedResult3.ac();
      {
	Array<Int> m(iter.matrixCursor()(IPosition(2,0),IPosition(2,8,2)));
	m += 1;
	AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
      }
      iter--;
      iter.matrixCursor().ac() += expectedResult2.ac();
      {
	Array<Int> m(iter.matrixCursor()(IPosition(2,0),IPosition(2,6,8)));
	m += 1;
	AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
      }
      iter--;
      iter.matrixCursor().ac() += expectedResult1.ac();
      iter.cursor() += 1;
      AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
      {
	Array<Int> arr;
	pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			  latticeShape, IPosition(latticeShape.nelements(),1));
	indgen(arr);
	pagedArr.putSlice(arr, IPosition(latticeShape.nelements(), 0));
      }

      cursorShape = 5;
      step.setCursorShape(cursorShape);
      step.subSection(IPosition(4, 3,0,0,0), latticeShape-1,
		      IPosition(4, 2,2,1,1));
      PagedArrIter<Int> subIter(pagedArr, step);

      oneRow.resize(5);
      Matrix<Int> expectedResulta(5,5);
      expectedResulta = 0;
      indgen(oneRow.ac(), 3, 2); 
      for (i = 0; i < 5; i++) {
  	expectedResulta.column(i) = oneRow;
  	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResulta.ac(), 
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter.cursor() = -1 * subIter.cursor() - 1;
      subIter++;
      Matrix<Int> expectedResultb(5,5);
      expectedResultb = 0;
      indgen(oneRow.ac(), 13, 2); 
      for (i = 0; i < 5; i++) {
  	oneRow(2) = 0;
  	oneRow(3) = 0;
  	oneRow(4) = 0;
  	expectedResultb.column(i) = oneRow;
  	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResultb.ac(), 
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter.cursor() = -1 * subIter.cursor() - 1;
      subIter++;
      Matrix<Int> expectedResultc(5,5);
      expectedResultc = 0;
      indgen(oneRow.ac(), 163, 2); 
      for (i = 0; i < 1; i++) {
  	expectedResultc.column(i) = oneRow;
  	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResultc.ac(),
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter.cursor() = -1 * subIter.cursor() - 1;
      subIter++;
      Matrix<Int> expectedResultd(5,5);
      expectedResultd = 0;
      indgen(oneRow.ac(), 173, 2); 
      for (i = 0; i < 1; i++) {
	oneRow(2) = 0;
	oneRow(3) = 0;
	oneRow(4) = 0;
	expectedResultd.column(i) = oneRow;
	oneRow.ac() += 32;
      }
      AlwaysAssert(allEQ(expectedResultd.ac(),
			 subIter.cursor().nonDegenerate()), AipsError);
      subIter.cursor() = -1 * subIter.cursor() - 1;
      subIter--; subIter++;
      Array<Int> arr;
      pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			IPosition(4,16,12,1,1),
			IPosition(latticeShape.nelements(), 1));
      AlwaysAssert(arr(IPosition(4,0)) == 0, AipsError);
      AlwaysAssert(arr(IPosition(4,1,0,0,0)) == 1, AipsError);
      AlwaysAssert(arr(IPosition(4,1,0,0,0)) == 1, AipsError);
      AlwaysAssert(arr(IPosition(4,3,0,0,0)) == -4, AipsError);
      AlwaysAssert(arr(IPosition(4,13,0,0,0)) == -14, AipsError);
      AlwaysAssert(arr(IPosition(4,14,0,0,0)) == 14, AipsError);
      AlwaysAssert(arr(IPosition(4,2,10,0,0)) == 162, AipsError);
      AlwaysAssert(arr(IPosition(4,3,10,0,0)) == -164, AipsError);
      AlwaysAssert(arr(IPosition(4,3,11,0,0)) == 179, AipsError);
      AlwaysAssert(arr(IPosition(4,15,10,0,0)) == -176, AipsError);
      AlwaysAssert(arr(IPosition(4,15,11,0,0)) == 191, AipsError);
    }
    cout << "OK" << endl;
    return 0;
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tPagedArrIter"
// End: 
