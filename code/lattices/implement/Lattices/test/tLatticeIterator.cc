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
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>

int main ()
{
  try {
    {
      cout << "Creating a Paged Array on disk" << endl;
      const IPosition latticeShape(4, 16, 12, 4, 32);
      SetupNewTable saved("tPagedArrIter_saved_tmp.table", 
 			  TableDesc(), Table::New);
      Table standardTable(saved);
      PagedArray<Float> pagedArr(latticeShape, standardTable);
      Array<Float> arr;
      pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
 			latticeShape, IPosition(latticeShape.nelements(), 1));
      indgen(arr);
      pagedArr.putSlice(arr, IPosition(latticeShape.nelements(), 0),
 			IPosition(latticeShape.nelements(), 1));
    }
    //++++++++++++++++++++ Test RO_PagedArrIter ++++++++++++++++++++
    // Check the Iterator with a Vector cursor. 
    // Eventually I will be able to open the Table for read only access
    cout << "Testing the RO iterator" << endl;
    {
      cout << "Testing using a Vector cursor" << endl;
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      LatticeStepper step(latticeShape, cursorShape);
      RO_PagedArrIter<Float>  iter(pagedArr, step);
      Vector<Float> expectedResult(latticeShape(0)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allNear(expectedResult.ac(),iter.vectorCursor().ac(), 1E-6) 
  		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6) 
  		   == True, AipsError);
      try {
  	Matrix<Float> temp(iter.matrixCursor());
  	throw(AipsError("tPagedArrIter - "
  			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
 	if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
 	  cerr << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
  	}
      } end_try;
      try {
 	Cube<Float> temp(iter.cubeCursor());
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
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.vectorCursor().ac(), 1E-6) 
 		     == True, AipsError);
 	expectedResult.ac() += Float(cursorShape.product());
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product()/latticeShape(0) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= Float(cursorShape.product());
      for (; !iter.atStart(); iter--){
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.cursor(), 1E-6) 
 		     == True, AipsError);
 	expectedResult.ac() -= Float(cursorShape.product());
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(2,latticeShape(0), latticeShape(1));
      RO_PagedArrIter<Float>  iter(pagedArr, cursorShape);
      Matrix<Float> expectedResult(latticeShape(0), latticeShape(1)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allNear(expectedResult.ac(),
  			   iter.matrixCursor().ac(), 1E-6) 
  		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(),
  			   iter.cursor().ac(), 1E-6) 
  		   == True, AipsError);
      try {
 	Vector<Float> temp(iter.vectorCursor());
 	throw(AipsError("tPagedArrIter - "
 			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
	if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
	  cerr << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      try {
 	Cube<Float> temp(iter.cubeCursor());
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
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.matrixCursor().ac(), 1E-6) 
 		     == True, AipsError);
 	expectedResult.ac() += Float(cursorShape.product());
      }
      AlwaysAssert(iter.nsteps() == latticeShape(2)*latticeShape(3) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= Float(cursorShape.product());
      for (; !iter.atStart(); --iter){
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.cursor(), 1E-6) 
 		     == True, AipsError);
 	expectedResult.ac() -= Float(cursorShape.product());
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				  latticeShape(2));
      LatticeStepper step(latticeShape, cursorShape);
      RO_LatticeIterator<Float>  iter(pagedArr, step);
      Cube<Float> expectedResult(latticeShape(0), latticeShape(1),
				 latticeShape(2)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allNear(expectedResult.ac(),
 			   iter.cubeCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor().ac(), 1E-6) 
 		   == True, AipsError);
      try {
	Vector<Float> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Float> temp(iter.matrixCursor());
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
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cubeCursor().ac(), 1E-6) 
		     == True, AipsError);
	expectedResult.ac() += Float(cursorShape.product());
      }
      AlwaysAssert(iter.nsteps() == latticeShape(3) - 1,
		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0; expectedPos(2) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= Float(cursorShape.product());
      for (; !iter.atStart(); iter--){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
		     == True, AipsError);
	expectedResult.ac() -= Float(cursorShape.product());
      }
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape(3)-1),
		   AipsError);
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(latticeShape);
      RO_LatticeIterator<Float>  iter(pagedArr, cursorShape);
      Array<Float> expectedResult(latticeShape); 
      indgen(expectedResult);
      AlwaysAssert(allNear(expectedResult,
 			   iter.cursor(), 1E-6) 
 		   == True, AipsError);
      try {
	Vector<Float> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Float> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Float> temp(iter.cubeCursor());
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
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor().ac(), 1E-6) 
		     == True, AipsError);
	expectedResult.ac() += Float(cursorShape.product());
      }
      AlwaysAssert(iter.nsteps() == 0, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= Float(cursorShape.product());
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
		     == True, AipsError);
	expectedResult.ac() -= Float(cursorShape.product());
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,1);
      LatticeStepper step(latticeShape, cursorShape);
      RO_PagedArrIter<Float>  iter(pagedArr, step);
      Array<Float> expectedResult(cursorShape); 
      indgen(expectedResult);
      AlwaysAssert(allNear(expectedResult,
 			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult,
 			   iter.cursor(), 1E-6) 
 		   == True, AipsError);
      try {
	Matrix<Float> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Float> temp(iter.cubeCursor());
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
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.vectorCursor().ac(), 1E-6) 
		     == True, AipsError);
	expectedResult.ac() += Float(cursorShape.product());
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product() - 1, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= Float(cursorShape.product());
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
		     == True, AipsError);
	expectedResult.ac() -= Float(cursorShape.product());
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      RO_PagedArrIter<Float> iter(pagedArr, 
				  LatticeStepper(latticeShape, cursorShape));
      iter++;
      Vector<Float> expectedResult(latticeShape(0)); 
      indgen(expectedResult.ac()); 
      expectedResult.ac() += Float(cursorShape.product());
      AlwaysAssert(allNear(expectedResult.ac(),iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);

      RO_PagedArrIter<Float> iterCopy(iter);
      Vector<Float> expectedCopy(expectedResult.copy());
      AlwaysAssert(allNear(expectedCopy.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      iter++; expectedResult.ac() += Float(cursorShape.product());
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedCopy.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      iterCopy--; 
      expectedCopy.ac() -= Float(cursorShape.product());
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedCopy.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(iter.vectorCursor().ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == False, AipsError);
      iterCopy = iter;
      expectedCopy = expectedResult;
      AlwaysAssert(allNear(iter.vectorCursor().ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      iterCopy++;
      expectedCopy.ac() += Float(cursorShape.product());
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedCopy.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(iter.vectorCursor().ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == False, AipsError);
    }
    // Test the non-congruent cursor handling
    {
      cout << "Testing using a non-congruent cursor" << endl;
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      const PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      IPosition cursorShape(2,9);
      LatticeStepper step(latticeShape, cursorShape);
      RO_PagedArrIter<Float>  iter(pagedArr, step);
      Matrix<Float> expectedResult(cursorShape); 
      Vector<Float> oneRow(cursorShape(0));
      indgen(oneRow.ac());
      uInt i;
      for (i = 0; i < cursorShape(1); i++) {
	expectedResult.column(i) = oneRow;
	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6), 
		   AipsError);
      iter++;
      indgen(oneRow.ac(), Float(cursorShape(0))); 
      for (i = 0; i < cursorShape(1); i++) {
	oneRow(7) = 0.0;
	oneRow(8) = 0.0;
	expectedResult.column(i) = oneRow;
	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6), 
		   AipsError);
      iter++;
      expectedResult = 0.0f;
      indgen(oneRow.ac(), Float(cursorShape(0)*latticeShape(0))); 
      for (i = 0; i < 3; i++) {
	expectedResult.column(i) = oneRow;
	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6), 
		   AipsError);
      iter++; 
      expectedResult = 0.0f;
      indgen(oneRow.ac(), Float(cursorShape(0)*(latticeShape(0)+1))); 
      for (i = 0; i < 3; i++) {
	oneRow(7) = 0.0;
	oneRow(8) = 0.0;
	expectedResult.column(i) = oneRow;
	oneRow.ac() += Float(latticeShape(0));
      }
      cursorShape = 5;
      step.setCursorShape(cursorShape);
      step.subSection(IPosition(4, 3,0,0,0), latticeShape-1, 
 		      IPosition(4, 2,2,1,1));
      RO_PagedArrIter<Float>  subIter(pagedArr, step);

      oneRow.resize(5);
      Matrix<Float> expectedResult1(5,5);
      expectedResult1 = 0.0f;
      indgen(oneRow.ac(), 3.0f, 2.0f); 
      for (i = 0; i < 5; i++) {
 	expectedResult1.column(i) = oneRow;
 	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResult1.ac(), subIter.cursor(), 1E-6), 
 		   AipsError);
      subIter++;
      Matrix<Float> expectedResult2(5,5);
      expectedResult2 = 0.0f;
      indgen(oneRow.ac(), 13.0f, 2.0f); 
      for (i = 0; i < 5; i++) {
 	oneRow(2) = 0.0f;
 	oneRow(3) = 0.0f;
 	oneRow(4) = 0.0f;
 	expectedResult2.column(i) = oneRow;
 	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResult2.ac(), subIter.cursor(), 1E-6), 
 		   AipsError);
      subIter++;
      Matrix<Float> expectedResult3(5,5);
      expectedResult3 = 0.0f;
      indgen(oneRow.ac(), 163.0f, 2.0f); 
      for (i = 0; i < 1; i++) {
 	expectedResult3.column(i) = oneRow;
 	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResult3.ac(), subIter.cursor(), 1E-6), 
 		   AipsError);
      subIter++;
      Matrix<Float> expectedResult4(5,5);
      expectedResult4 = 0.0f;
      indgen(oneRow.ac(), 173.0f, 2.0f); 
       for (i = 0; i < 1; i++) {
	 oneRow(2) = 0.0f;
	 oneRow(3) = 0.0f;
	 oneRow(4) = 0.0f;
	 expectedResult4.column(i) = oneRow;
	 oneRow.ac() += 32.0f;
       }
       AlwaysAssert(allNear(expectedResult4.ac(), subIter.cursor(), 1E-6), 
		    AipsError);
       subIter--;
       AlwaysAssert(allNear(expectedResult3.ac(), subIter.cursor(), 1E-6), 
		    AipsError);
       subIter--;
       AlwaysAssert(allNear(expectedResult2.ac(), subIter.cursor(), 1E-6), 
		    AipsError);
       subIter--;
       AlwaysAssert(allNear(expectedResult1.ac(), subIter.cursor(), 1E-6), 
		    AipsError);
    }
    //++++++++++++++++++++ Test PagedArrIter ++++++++++++++++++++
    cout << "Testing the RW iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      cout << "Testing using a Vector cursor" << endl;
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      LatticeStepper step(latticeShape, cursorShape);
      PagedArrIter<Float>  iter(pagedArr, step);
      Vector<Float> expectedResult(latticeShape(0)); 
      indgen(expectedResult.ac());
      AlwaysAssert(allNear(expectedResult.ac(),iter.vectorCursor().ac(), 1E-6) 
  		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6) 
  		   == True, AipsError);
      try {
  	Matrix<Float> temp(iter.matrixCursor());
  	throw(AipsError("tPagedArrIter - "
  			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
 	if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
 	  cerr << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
  	}
      } end_try;
      try {
 	Cube<Float> temp(iter.cubeCursor());
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
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.vectorCursor().ac(), 1E-6) 
 		     == True, AipsError);
	iter.vectorCursor()(0) -= expectedResult(0);
 	expectedResult.ac() += Float(cursorShape.product());
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product()/latticeShape(0) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult.ac() -= Float(cursorShape.product());
      expectedResult(0) = 0.0f;
      for (; !iter.atStart(); iter--){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
		     == True, AipsError);
	iter.cursor() = 1.0f;
	expectedResult.ac() -= Float(cursorShape.product());
	expectedResult(0) = 0.0f;
      }
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.cursor(), 1E-6) 
		   == True, AipsError);
      iter.cursor() = 1.0f;
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
      PagedArrIter<Float>  iter(pagedArr, cursorShape);
      Matrix<Float> expectedResult(latticeShape(0), latticeShape(1)); 
      expectedResult = 1.0;
      AlwaysAssert(allNear(expectedResult.ac(),
  			   iter.matrixCursor().ac(), 1E-6) 
  		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(),
  			   iter.cursor().ac(), 1E-6) 
  		   == True, AipsError);
      try {
 	Vector<Float> temp(iter.vectorCursor());
 	throw(AipsError("tPagedArrIter - "
 			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
	if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
	  cerr << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      try {
 	Cube<Float> temp(iter.cubeCursor());
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
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.matrixCursor().ac(), 1E-6) 
 		     == True, AipsError);
	iter.matrixCursor()(0,0) = 2.0f;
      }
      AlwaysAssert(iter.nsteps() == latticeShape(2)*latticeShape(3) - 1,
 		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult(0,0) = 2.0f;
      for (; !iter.atStart(); --iter){
 	AlwaysAssert(allNear(expectedResult.ac(),
 			     iter.cursor(), 1E-6) 
 		     == True, AipsError);
	iter.cursor() = 3.0f;
      }
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6) 
		   == True, AipsError);
      iter.cursor() = 3.0f;
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				  latticeShape(2));
      LatticeStepper step(latticeShape, cursorShape);
      LatticeIterator<Float>  iter(pagedArr, step);
      Cube<Float> expectedResult(latticeShape(0), latticeShape(1),
				 latticeShape(2)); 
      expectedResult.ac() = 3.0;
      AlwaysAssert(allNear(expectedResult.ac(),
 			   iter.cubeCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor().ac(), 1E-6) 
 		   == True, AipsError);
      try {
	Vector<Float> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Float> temp(iter.matrixCursor());
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
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cubeCursor().ac(), 1E-6) 
		     == True, AipsError);
        iter.cubeCursor()(0,0,0) = 4.0f;
      }
      AlwaysAssert(iter.nsteps() == latticeShape(3) - 1,
		   AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos(0) = 0; expectedPos(1) = 0; expectedPos(2) = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult(0,0,0) = 4.0f;
      for (; !iter.atStart(); iter--){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
		     == True, AipsError);
	iter.cursor() = 5.0f;
      }
      AlwaysAssert(allNear(expectedResult.ac(), iter.cursor(), 1E-6) 
		   == True, AipsError);
      iter.cursor() = 5.0f;
      clock.show();
      AlwaysAssert(iter.nsteps()==2*(latticeShape(3)-1),
		   AipsError);
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(latticeShape);
      LatticeIterator<Float>  iter(pagedArr, cursorShape);
      Array<Float> expectedResult(latticeShape); 
      expectedResult = 5.0;
      AlwaysAssert(allNear(expectedResult,
 			   iter.cursor(), 1E-6) 
 		   == True, AipsError);
      try {
	Vector<Float> temp(iter.vectorCursor());
	throw(AipsError("tPagedArrIter - "
			"vectorCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 1-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Matrix<Float> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Float> temp(iter.cubeCursor());
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
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor().ac(), 1E-6) 
		     == True, AipsError);
	iter.cursor()(IPosition(4,0)) = 6.0f;
      }
      AlwaysAssert(iter.nsteps() == 0, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      expectedPos = 0;
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      expectedResult(IPosition(4,0)) = 6.0f;
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
		     == True, AipsError);
	iter.cursor() = 7.0f;
      }
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.cursor(), 1E-6) 
		   == True, AipsError);
      iter.cursor() = 7.0f;
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,1);
      LatticeStepper step(latticeShape, cursorShape);
      PagedArrIter<Float>  iter(pagedArr, step);
      Array<Float> expectedResult(cursorShape); 
      expectedResult = 7.0f;
      AlwaysAssert(allNear(expectedResult,
 			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      AlwaysAssert(allNear(expectedResult,
 			   iter.cursor(), 1E-6) 
 		   == True, AipsError);
      try {
	Matrix<Float> temp(iter.matrixCursor());
	throw(AipsError("tPagedArrIter - "
			"matrixCursor worked where it should not have"));
      } catch (AipsError x) {
        if (!x.getMesg().contains("check the cursor shape is 2-dimensional")) {
          cerr << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      try {
	Cube<Float> temp(iter.cubeCursor());
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
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.vectorCursor().ac(), 1E-6) 
		     == True, AipsError);
      }
      AlwaysAssert(iter.nsteps() == latticeShape.product() - 1, AipsError);
      IPosition expectedPos(latticeShape-1);
      AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
      AlwaysAssert(iter.position() == expectedPos, AipsError);
      for (; !iter.atStart(); --iter){
	AlwaysAssert(allNear(expectedResult.ac(),
			     iter.cursor(), 1E-6) 
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
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      const IPosition cursorShape(1,latticeShape(0));
      PagedArrIter<Float> iter(pagedArr, 
			       LatticeStepper(latticeShape, cursorShape));
      iter++;
      Vector<Float> expectedResult(latticeShape(0)); 
      expectedResult = 7.0f;
      AlwaysAssert(allNear(expectedResult.ac(),iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);

      PagedArrIter<Float> iterCopy(iter);
      AlwaysAssert(allNear(expectedResult.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      iter++; iter.cursor() = 2.0f;
      expectedResult.ac() = 2.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      expectedResult.ac() = 7.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
 			   iterCopy.vectorCursor().ac(), 1E-6) 
  		   == True, AipsError);
      iterCopy--; iterCopy.cursor() = 0.0f;
      expectedResult.ac() = 2.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
  			   iter.vectorCursor().ac(), 1E-6) 
   		   == True, AipsError);
      expectedResult.ac() = 0.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
   		   == True, AipsError);

      iterCopy = iter;
      AlwaysAssert(allNear(iter.vectorCursor().ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      expectedResult.ac() = 2.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      iter++;
      expectedResult.ac() = 7.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
			   iter.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      expectedResult.ac() = 2.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
      --iterCopy; iterCopy--;
      expectedResult.ac() = 0.0f;
      AlwaysAssert(allNear(expectedResult.ac(),
			   iterCopy.vectorCursor().ac(), 1E-6) 
 		   == True, AipsError);
    }
    // Test the non-congruent cursor handling
    {
      cout << "Testing using a non-congruent cursor" << endl;
      Table tData("tPagedArrIter_saved_tmp.table", Table::Update);
      PagedArray<Float> pagedArr(tData);
      const IPosition latticeShape(pagedArr.shape());
      {
	Array<Float> arr;
	pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			  latticeShape, IPosition(latticeShape.nelements(), 1));
	indgen(arr);
	pagedArr.putSlice(arr, IPosition(latticeShape.nelements(), 0),
			  IPosition(latticeShape.nelements(), 1));
      }
      IPosition cursorShape(2,9);
      LatticeStepper step(latticeShape, cursorShape);
      PagedArrIter<Float>  iter(pagedArr, step);
      Matrix<Float> expectedResult1(cursorShape); 
      Vector<Float> oneRow(cursorShape(0));
      indgen(oneRow.ac());
      uInt i;
      for (i = 0; i < cursorShape(1); i++) {
 	expectedResult1.column(i) = oneRow;
 	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult1.ac(), iter.cursor(), 1E-6), 
 		   AipsError);
      iter.cursor() = -1.0f * iter.cursor() - 1.0f;
      iter++;
      Matrix<Float> expectedResult2(cursorShape); 
      indgen(oneRow.ac(), Float(cursorShape(0))); 
      for (i = 0; i < cursorShape(1); i++) {
  	oneRow(7) = 0.0;
  	oneRow(8) = 0.0;
  	expectedResult2.column(i) = oneRow;
  	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult2.ac(), iter.cursor(), 1E-6), 
  		   AipsError);
      iter.cursor() = -1.0f * iter.cursor() - 1.0f;
      iter++;
      Matrix<Float> expectedResult3(cursorShape); 
      expectedResult3 = 0.0f;
      indgen(oneRow.ac(), Float(cursorShape(0)*latticeShape(0))); 
      for (i = 0; i < 3; i++) {
  	expectedResult3.column(i) = oneRow;
  	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult3.ac(), iter.cursor(), 1E-6), 
		   AipsError);
      iter.cursor() = -1.0f * iter.cursor() - 1.0f;
      iter++; 
      Matrix<Float> expectedResult4(cursorShape); 
      expectedResult4 = 0.0f;
      indgen(oneRow.ac(), Float(cursorShape(0)*(latticeShape(0)+1))); 
      for (i = 0; i < 3; i++) {
  	oneRow(7) = 0.0;
 	oneRow(8) = 0.0;
  	expectedResult4.column(i) = oneRow;
  	oneRow.ac() += Float(latticeShape(0));
      }
      AlwaysAssert(allNear(expectedResult4.ac(), iter.cursor(), 1E-6), 
		   AipsError);
      iter.cursor() = -1.0f * iter.cursor() - 1.0f;
      iter--; iter++;
      iter.cursor() += expectedResult4.ac();
      {
	Array<Float> m(iter.cursor()(IPosition(2,0),IPosition(2,6,2)));
	m += 1.0f;
	AlwaysAssert(allNear(iter.cursor(), 0.0f, 1E-6), AipsError);
      }
      iter--;
      iter.cursor() += expectedResult3.ac();
      {
	Array<Float> m(iter.cursor()(IPosition(2,0),IPosition(2,8,2)));
	m += 1.0f;
	AlwaysAssert(allNear(iter.cursor(), 0.0f, 1E-6), AipsError);
      }
      iter--;
      iter.cursor() += expectedResult2.ac();
      {
	Array<Float> m(iter.cursor()(IPosition(2,0),IPosition(2,6,8)));
	m += 1.0f;
	AlwaysAssert(allNear(iter.cursor(), 0.0f, 1E-6), AipsError);
      }
      iter--;
      iter.cursor() += expectedResult1.ac();
      iter.cursor() += 1.0f;
      AlwaysAssert(allNear(iter.cursor(), 0.0f, 1E-6), AipsError);

      {
	Array<Float> arr;
	pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			  latticeShape, IPosition(latticeShape.nelements(), 1));
	indgen(arr);
	pagedArr.putSlice(arr, IPosition(latticeShape.nelements(), 0),
			  IPosition(latticeShape.nelements(), 1));
      }

      cursorShape = 5;
      step.setCursorShape(cursorShape);
      step.subSection(IPosition(4, 3,0,0,0), latticeShape-1, 
  		      IPosition(4, 2,2,1,1));
      PagedArrIter<Float> subIter(pagedArr, step);

      oneRow.resize(5);
      Matrix<Float> expectedResulta(5,5);
      expectedResulta = 0.0f;
      indgen(oneRow.ac(), 3.0f, 2.0f); 
      for (i = 0; i < 5; i++) {
  	expectedResulta.column(i) = oneRow;
  	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResulta.ac(), subIter.cursor(), 1E-6), 
  		   AipsError);
      subIter.cursor() = -1.0f * subIter.cursor() - 1.0f;
      subIter++;
      Matrix<Float> expectedResultb(5,5);
      expectedResultb = 0.0f;
      indgen(oneRow.ac(), 13.0f, 2.0f); 
      for (i = 0; i < 5; i++) {
  	oneRow(2) = 0.0f;
  	oneRow(3) = 0.0f;
  	oneRow(4) = 0.0f;
  	expectedResultb.column(i) = oneRow;
  	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResultb.ac(), subIter.cursor(), 1E-6), 
		   AipsError);
      subIter.cursor() = -1.0f * subIter.cursor() - 1.0f;
      subIter++;
      Matrix<Float> expectedResultc(5,5);
      expectedResultc = 0.0f;
      indgen(oneRow.ac(), 163.0f, 2.0f); 
      for (i = 0; i < 1; i++) {
  	expectedResultc.column(i) = oneRow;
  	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResultc.ac(), subIter.cursor(), 1E-6), 
		   AipsError);
      subIter.cursor() = -1.0f * subIter.cursor() - 1.0f;
      subIter++;
      Matrix<Float> expectedResultd(5,5);
      expectedResultd = 0.0f;
      indgen(oneRow.ac(), 173.0f, 2.0f); 
      for (i = 0; i < 1; i++) {
	oneRow(2) = 0.0f;
	oneRow(3) = 0.0f;
	oneRow(4) = 0.0f;
	expectedResultd.column(i) = oneRow;
	oneRow.ac() += 32.0f;
      }
      AlwaysAssert(allNear(expectedResultd.ac(), subIter.cursor(), 1E-6), 
		   AipsError);
      subIter.cursor() = -1.0f * subIter.cursor() - 1.0f;
      subIter--; subIter++;
      Array<Float> arr;
      pagedArr.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			IPosition(4,16,12,1,1), 
			IPosition(latticeShape.nelements(), 1));
      AlwaysAssert(near(arr(IPosition(4,0)), 0.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,1,0,0,0)), 1.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,1,0,0,0)), 1.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,3,0,0,0)), -4.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,13,0,0,0)), -14.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,14,0,0,0)), 14.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,2,10,0,0)), 162.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,3,10,0,0)), -164.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,3,11,0,0)), 179.0f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,15,10,0,0)), -176.f, 1E-6), AipsError);
      AlwaysAssert(near(arr(IPosition(4,15,11,0,0)), 191.0f, 1E-6), AipsError);
    }
    cout << "OK" << endl;
    return 0;
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  
}
