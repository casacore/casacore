//# tHDF5Iterator.cc:  Test of HDF5 iterator performance
//# Copyright (C) 2008
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

#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/HDF5Lattice.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TiledLineStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/iostream.h>

using namespace casacore;


void testVectorROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a Vector cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step, useRef);
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) 
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    AlwaysAssert(latticeShape == iter.latticeShape(), AipsError);
    AlwaysAssert(cursorShape == iter.cursorShape().nonDegenerate(), AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
                     == True, AipsError);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= Int(cursorShape.product());
    Int ns=0;
    for (; !iter.atStart(); iter--){
	AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
		     == True, AipsError);
	expectedResult -= Int(cursorShape.product());
	ns++;
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape.product()/latticeShape(0)),
		 AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = latticeShape(0) - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testMatrixROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a Matrix cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
    RO_LatticeIterator<Int>  iter(lattice, cursorShape, useRef);
    Matrix<Int> expectedResult(latticeShape(0), latticeShape(1));
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.matrixCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tHDF5Iterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult, iter.matrixCursor()) 
                     == True, AipsError);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape(2)*latticeShape(3),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    expectedPos(1) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= Int(cursorShape.product());
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
                     == True, AipsError);
        expectedResult -= Int(cursorShape.product());
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape(2)*latticeShape(3)),
		 AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = latticeShape(0) - 1;
    expectedPos(1) = latticeShape(1) - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testCubeROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a Cube cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				latticeShape(2));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step, useRef);
    Cube<Int> expectedResult(latticeShape(0), latticeShape(1),
			     latticeShape(2));
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.cubeCursor()) == True,
		 AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tHDF5Iterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult, iter.cubeCursor())
                     == True, AipsError);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape(3), AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    expectedPos(1) = 0;
    expectedPos(2) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= Int(cursorShape.product());
    for (; !iter.atStart(); iter--){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
                     == True, AipsError);
        expectedResult -= Int(cursorShape.product());
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2 * (latticeShape(3)), AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = latticeShape(0) - 1;
    expectedPos(1) = latticeShape(1) - 1;
    expectedPos(2) = latticeShape(2) - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testArrayROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using an Array (4-D) cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(latticeShape);
    RO_LatticeIterator<Int>  iter(lattice, cursorShape, useRef);
    Array<Int> expectedResult(latticeShape);
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tHDF5Iterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, 
                     AipsError);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 1, AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= Int(cursorShape.product());
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True,
                     AipsError);
        expectedResult -= Int(cursorShape.product());
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2, AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos = latticeShape - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void test8ElemROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using an 8 element cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,8);
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step, useRef);
    Array<Int> expectedResult(cursorShape);
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) == True,
		 AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
		 == True, AipsError);
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) == True,
                     AipsError);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/8, AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 8;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= Int(cursorShape.product());
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
                     == True, AipsError);
        expectedResult -= Int(cursorShape.product());
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape.product()/8), AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = 8-1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testTileROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a tile cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(lattice.niceCursorShape());
    RO_LatticeIterator<Int>  iter(lattice, useRef);
    Array<Int> expectedResult(cursorShape);
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/cursorShape.product(),
		 AipsError);
    for (; !iter.atStart(); --iter){
        expectedResult -= Int(cursorShape.product());
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*latticeShape.product()/cursorShape.product(),
		 AipsError);
}

void testTiledLineROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a tiled line cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(lattice.niceCursorShape());
    TiledLineStepper step(latticeShape, cursorShape, 0);
    RO_LatticeIterator<Int>  iter(lattice, step, useRef);
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) == True,
		 AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		     == True, AipsError);
        expectedResult += Int(latticeShape(0));
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    for (; !iter.atStart(); --iter){
        expectedResult -= Int(latticeShape(0));
        AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		     == True, AipsError);
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*latticeShape.product()/latticeShape(0),
		 AipsError);
}

void testCopyAssignROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing the copy constructor and assignment operator" << endl;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));

    {
      // Test default ctor and its handling in copy and assignment.
      RO_LatticeIterator<Int> iter;
      AlwaysAssert(iter.isNull(), AipsError);
      RO_LatticeIterator<Int> iter1 = iter.copy();
      AlwaysAssert(iter1.isNull(), AipsError);
      iter = RO_LatticeIterator<Int> (lattice, useRef);
      AlwaysAssert(!iter.isNull(), AipsError);
      iter = iter1;
      AlwaysAssert(iter.isNull(), AipsError);
      iter = RO_LatticeIterator<Int> (lattice, useRef);
      AlwaysAssert(!iter.isNull(), AipsError);
      iter1 = iter;
      AlwaysAssert(!iter1.isNull(), AipsError);
      iter = RO_LatticeIterator<Int>();
      AlwaysAssert(iter.isNull(), AipsError);
      AlwaysAssert(!iter1.isNull(), AipsError);
      RO_LatticeIterator<Int> iterc(iter);
      AlwaysAssert(iterc.isNull(), AipsError);
      RO_LatticeIterator<Int> iterc1(iter1);
      AlwaysAssert(!iterc1.isNull(), AipsError);
      iterc1 = iterc;
      AlwaysAssert(iterc1.isNull(), AipsError);
      AlwaysAssert(!iter1.isNull(), AipsError);
    }

    RO_LatticeIterator<Int> iter(lattice, 
				 LatticeStepper(latticeShape, cursorShape),
				 useRef);
    AlwaysAssert(!iter.isNull(), AipsError);
    iter++;
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult);
    expectedResult += Int(cursorShape.product());
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		 == True, AipsError);
    
    RO_LatticeIterator<Int> iterCopy(iter.copy());
    Vector<Int> expectedCopy(expectedResult.copy());
    AlwaysAssert(allEQ(expectedCopy, iterCopy.vectorCursor())
		 == True, AipsError);
    iter++;
    expectedResult += Int(cursorShape.product());
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedCopy, iterCopy.vectorCursor()) 
		 == True, AipsError);
    iterCopy--;
    expectedCopy -= Int(cursorShape.product());
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedCopy, iterCopy.vectorCursor()) 
		 == True, AipsError);
    AlwaysAssert(allEQ(iter.vectorCursor(),iterCopy.vectorCursor())
		 == False, AipsError);
    iterCopy = iter.copy();
    expectedCopy = expectedResult;
    AlwaysAssert(allEQ(iter.vectorCursor(),iterCopy.vectorCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		 == True, AipsError);
    iterCopy++;
    expectedCopy += Int(cursorShape.product());
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedCopy, iterCopy.vectorCursor()) 
		 == True, AipsError);
    AlwaysAssert(allEQ(iter.vectorCursor(),iterCopy.vectorCursor())
		 == False, AipsError);
}

void testNonCongruentROIter (const Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a non-congruent cursor" << endl;
    const IPosition latticeShape(lattice.shape());
    IPosition cursorShape(2,9);
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step, useRef);
    Matrix<Int> expectedResult(cursorShape);
    Vector<Int> oneRow(cursorShape(0));
    indgen(oneRow);
    uInt i;
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        expectedResult.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()),
		 AipsError);
    iter++;
    indgen(oneRow, Int(cursorShape(0)));
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        oneRow(7) = 0;
        oneRow(8) = 0;
        expectedResult.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()),
		 AipsError);
    iter++;
    expectedResult = 0;
    indgen(oneRow, Int(cursorShape(0)*latticeShape(0)));
    for (i = 0; i < 3; i++) {
        expectedResult.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()),
		 AipsError);
    iter++;
    expectedResult = 0;
    indgen(oneRow, Int(cursorShape(0)*(latticeShape(0)+1)));
    for (i = 0; i < 3; i++) {
        oneRow(7) = 0;
        oneRow(8) = 0;
        expectedResult.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    cursorShape = 5;
    step.setCursorShape(cursorShape);
    step.subSection(IPosition(4, 3,0,0,0), latticeShape-1,
		    IPosition(4, 2,2,1,1));
    RO_LatticeIterator<Int>  subIter(lattice, step, useRef);
    
    oneRow.resize(5);
    Matrix<Int> expectedResult1(5,5);
    expectedResult1 = 0;
    indgen(oneRow, 3, 2);
    for (i = 0; i < 5; i++) {
        expectedResult1.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResult1,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter++;
    Matrix<Int> expectedResult2(5,5);
    expectedResult2 = 0;
    indgen(oneRow, 13, 2);
    for (i = 0; i < 5; i++) {
        oneRow(2) = 0;
        oneRow(3) = 0;
        oneRow(4) = 0;
        expectedResult2.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResult2,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter++;
    Matrix<Int> expectedResult3(5,5);
    expectedResult3 = 0;
    indgen(oneRow, 163, 2);
    for (i = 0; i < 1; i++) {
        expectedResult3.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResult3,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter++;
    Matrix<Int> expectedResult4(5,5);
    expectedResult4 = 0;
    indgen(oneRow, 173, 2);
    for (i = 0; i < 1; i++) {
	oneRow(2) = 0;
	oneRow(3) = 0;
	oneRow(4) = 0;
	expectedResult4.column(i) = oneRow;
	oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResult4,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter--;
    AlwaysAssert(allEQ(expectedResult3,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter--;
    AlwaysAssert(allEQ(expectedResult2,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter--;
    AlwaysAssert(allEQ(expectedResult1,
		       subIter.cursor().nonDegenerate()), AipsError);
}

void testVectorRWIter (Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a Vector cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    LatticeIterator<Int>  iter(lattice, step, useRef);
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult,iter.vectorCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    
    AlwaysAssert(latticeShape == iter.latticeShape(), AipsError);
    AlwaysAssert(cursorShape == iter.cursorShape().nonDegenerate(),
		 AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult, iter.vectorCursor())
                     == True, AipsError);
        iter.rwVectorCursor()(0) -= expectedResult(0);
        expectedResult += Int(cursorShape.product());
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= Int(cursorShape.product());
    expectedResult(0) = 0;
    for (; !iter.atStart(); iter--){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
                     == True, AipsError);
        iter.woCursor() = 1;
        expectedResult -= Int(cursorShape.product());
        expectedResult(0) = 0;
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape.product()/latticeShape(0)),
		 AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = latticeShape(0) - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testMatrixRWIter (Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a Matrix cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
    LatticeIterator<Int>  iter(lattice, cursorShape, useRef);
    Matrix<Int> expectedResult(latticeShape(0), latticeShape(1));
    expectedResult = 1;
    AlwaysAssert(allEQ(expectedResult, iter.matrixCursor()) 
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tHDF5Iterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult, iter.matrixCursor()) 
                     == True, AipsError);
        iter.rwMatrixCursor()(0,0) = 2;
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape(2)*latticeShape(3),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    expectedPos(1) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult(0,0) = 2;
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
                     == True, AipsError);
        iter.woCursor() = 3;
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape(2)*latticeShape(3)),
		 AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = latticeShape(0) - 1;
    expectedPos(1) = latticeShape(1) - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testCubeRWIter (Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a Cube cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				latticeShape(2));
    LatticeStepper step(latticeShape, cursorShape);
    LatticeIterator<Int>  iter(lattice, step, useRef);
    Cube<Int> expectedResult(latticeShape(0), latticeShape(1),
			     latticeShape(2));
    expectedResult = 3;
    AlwaysAssert(allEQ(expectedResult, iter.cubeCursor())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tHDF5Iterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult, iter.cubeCursor())
                     == True, AipsError);
        iter.rwCubeCursor()(0,0,0) = 4;
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape(3), AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    expectedPos(1) = 0;
    expectedPos(2) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult(0,0,0) = 4;
    for (; !iter.atStart(); iter--){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
                     == True, AipsError);
        iter.woCursor() = 5;
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape(3)), AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = latticeShape(0) - 1;
    expectedPos(1) = latticeShape(1) - 1;
    expectedPos(2) = latticeShape(2) - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testArrayRWIter (Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using an Array (4-D) cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(latticeShape);
    LatticeIterator<Int>  iter(lattice, cursorShape, useRef);
    Array<Int> expectedResult(latticeShape);
    expectedResult = 5;
    AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tHDF5Iterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tHDF5Iterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tHDF5Iterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } 
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True,
                     AipsError);
        iter.rwCursor()(IPosition(4,0)) = 6;
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 1, AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult(IPosition(4,0)) = 6;
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True,
                     AipsError);
        iter.woCursor() = 7;
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2, AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos = latticeShape - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testCopyAssignRWIter (Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing the copy constructor and assignment operator" << endl;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeIterator<Int> iter(lattice, 
			      LatticeStepper(latticeShape, cursorShape),
			      useRef);
    iter++;
    Vector<Int> expectedResult(latticeShape(0));
    expectedResult = 7;
    AlwaysAssert(allEQ(expectedResult,iter.vectorCursor()) == True,
		 AipsError);
    
    LatticeIterator<Int> iterCopy(iter.copy());
    AlwaysAssert(allEQ(expectedResult, iterCopy.vectorCursor()) 
		 == True, AipsError);
    iter++;
    iter.woCursor() = 2;
    expectedResult = 2;
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) 
		 == True, AipsError);
    expectedResult = 7;
    AlwaysAssert(allEQ(expectedResult, iterCopy.vectorCursor()) 
		 == True, AipsError);
    iterCopy--;
    iterCopy.woCursor() = 0;
    expectedResult = 2;
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) 
		 == True, AipsError);
    expectedResult = 0;
    AlwaysAssert(allEQ(expectedResult, iterCopy.vectorCursor()) 
		 == True, AipsError);
    
    iterCopy = iter.copy();
    AlwaysAssert(allEQ(iter.vectorCursor(),
		       iterCopy.vectorCursor()) == True, AipsError);
    expectedResult = 2;
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) 
		 == True, AipsError);
    iter++;
    expectedResult = 7;
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor()) 
		 == True, AipsError);
    expectedResult = 2;
    AlwaysAssert(allEQ(expectedResult, iterCopy.vectorCursor()) 
		 == True, AipsError);
    --iterCopy;
    iterCopy--;
    expectedResult = 0;
    AlwaysAssert(allEQ(expectedResult, iterCopy.vectorCursor()) 
		 == True, AipsError);
}

void testNonCongruentRWIter (Lattice<Int>& lattice, Bool useRef)
{
    cout << "  Testing using a non-congruent cursor" << endl;
    const IPosition latticeShape(lattice.shape());
    {
        Array<Int> arr;
        lattice.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			 latticeShape,
			 IPosition(latticeShape.nelements(), 1));
        indgen(arr);
        lattice.putSlice(arr, IPosition(latticeShape.nelements(), 0));
    }
    IPosition cursorShape(2,9);
    LatticeStepper step(latticeShape, cursorShape);
    LatticeIterator<Int> iter(lattice, step, useRef);
    Matrix<Int> expectedResult1(cursorShape);
    Vector<Int> oneRow(cursorShape(0));
    indgen(oneRow);
    uInt i;
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        expectedResult1.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult1, iter.cursor().nonDegenerate()),
		 AipsError);
    iter.woCursor() = (-1 * iter.cursor() - 1);
    iter++;
    Matrix<Int> expectedResult2(cursorShape);
    indgen(oneRow, Int(cursorShape(0)));
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        oneRow(7) = 0;
        oneRow(8) = 0;
        expectedResult2.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult2, iter.cursor().nonDegenerate()),
		 AipsError);
    iter.woCursor() = (-1 * iter.cursor() - 1);
    iter++;
    Matrix<Int> expectedResult3(cursorShape);
    expectedResult3 = 0;
    indgen(oneRow, Int(cursorShape(0)*latticeShape(0)));
    for (i = 0; i < 3; i++) {
        expectedResult3.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult3, iter.cursor().nonDegenerate()),
		 AipsError);
    iter.woCursor() = (-1 * iter.cursor() - 1);
    iter++;
    Matrix<Int> expectedResult4(cursorShape);
    expectedResult4 = 0;
    indgen(oneRow, Int(cursorShape(0)*(latticeShape(0)+1)));
    for (i = 0; i < 3; i++) {
        oneRow(7) = 0;
        oneRow(8) = 0;
        expectedResult4.column(i) = oneRow;
        oneRow += Int(latticeShape(0));
    }
    AlwaysAssert(allEQ(expectedResult4, iter.cursor().nonDegenerate()),
		 AipsError);
    iter.woCursor() = (-1 * iter.cursor() - 1);
    iter--;
    iter++;
    iter.rwMatrixCursor() += expectedResult4;
    {
        Array<Int> m(iter.rwMatrixCursor()(IPosition(2,0),IPosition(2,6,2)));
        m += 1;
        AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    }
    iter--;
    iter.rwMatrixCursor() += expectedResult3;
    {
        Array<Int> m(iter.rwMatrixCursor()(IPosition(2,0),IPosition(2,8,2)));
        m += 1;
        AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    }
    iter--;
    iter.rwMatrixCursor() += expectedResult2;
    {
        Array<Int> m(iter.rwMatrixCursor()(IPosition(2,0),IPosition(2,6,8)));
        m += 1;
        AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    }
    iter--;
    iter.rwMatrixCursor() += expectedResult1;
    iter.rwCursor() += 1;
    AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    {
        Array<Int> arr;
        lattice.getSlice(arr, IPosition(latticeShape.nelements(), 0),
			 latticeShape,
			 IPosition(latticeShape.nelements(), 1));
        indgen(arr);
        lattice.putSlice(arr, IPosition(latticeShape.nelements(), 0));
    }
    cursorShape = 5;
    step.setCursorShape(cursorShape);
    step.subSection(IPosition(4, 3,0,0,0), latticeShape-1,
		    IPosition(4, 2,2,1,1));
    LatticeIterator<Int> subIter(lattice, step, useRef);
    
    oneRow.resize(5);
    Matrix<Int> expectedResulta(5,5);
    expectedResulta = 0;
    indgen(oneRow, 3, 2);
    for (i = 0; i < 5; i++) {
        expectedResulta.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResulta,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
    subIter++;
    Matrix<Int> expectedResultb(5,5);
    expectedResultb = 0;
    indgen(oneRow, 13, 2);
    for (i = 0; i < 5; i++) {
        oneRow(2) = 0;
        oneRow(3) = 0;
        oneRow(4) = 0;
        expectedResultb.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResultb, 
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
    subIter++;
    Matrix<Int> expectedResultc(5,5);
    expectedResultc = 0;
    indgen(oneRow, 163, 2);
    for (i = 0; i < 1; i++) {
        expectedResultc.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResultc,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
    subIter++;
    Matrix<Int> expectedResultd(5,5);
    expectedResultd = 0;
    indgen(oneRow, 173, 2);
    for (i = 0; i < 1; i++) {
        oneRow(2) = 0;
        oneRow(3) = 0;
        oneRow(4) = 0;
        expectedResultd.column(i) = oneRow;
        oneRow += 32;
    }
    AlwaysAssert(allEQ(expectedResultd,
		       subIter.cursor().nonDegenerate()), AipsError);
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
    subIter--;
    subIter++;
    Array<Int> arr;
    lattice.getSlice(arr, IPosition(latticeShape.nelements(), 0),
		     IPosition(4,16,12,1,1),
		     IPosition(latticeShape.nelements(), 1));
    AlwaysAssert(arr(IPosition(4,0)) == 0, AipsError);
    AlwaysAssert(arr(IPosition(4,1,0,0,0)) == 1, AipsError);
    AlwaysAssert(arr(IPosition(4,2,0,0,0)) == 2, AipsError);
    AlwaysAssert(arr(IPosition(4,3,0,0,0)) == -4, AipsError);
    AlwaysAssert(arr(IPosition(4,13,0,0,0)) == -14, AipsError);
    AlwaysAssert(arr(IPosition(4,14,0,0,0)) == 14, AipsError);
    AlwaysAssert(arr(IPosition(4,2,10,0,0)) == 162, AipsError);
    AlwaysAssert(arr(IPosition(4,3,10,0,0)) == -164, AipsError);
    AlwaysAssert(arr(IPosition(4,3,11,0,0)) == 179, AipsError);
    AlwaysAssert(arr(IPosition(4,15,10,0,0)) == -176, AipsError);
    AlwaysAssert(arr(IPosition(4,15,11,0,0)) == 191, AipsError);
}

void testAdd (Lattice<Int>& lat1, Lattice<Int>& lat2, Bool useRef)
{
  {
    HDF5Lattice<Int>* pa1 = dynamic_cast<HDF5Lattice<Int>*> (&lat1);
    if (pa1) pa1->clearCache();
    HDF5Lattice<Int>* pa2 = dynamic_cast<HDF5Lattice<Int>*> (&lat2);
    if (pa2) pa2->clearCache();
    Timer timer;
    LatticeIterator<Int> lat1Iter (lat1, useRef);
    // Create dummy lat2Iter to setup cache correctly.
    // It may not be necessary, because the Table getSlice function
    // will setup the cache on its first access.
    RO_LatticeIterator<Int> lat2Iter (lat2, lat1.niceCursorShape(), useRef);
    Array<Int> lat2Buffer;
    while (! lat1Iter.atEnd()) {
      // Do separate getSlice to use reference semantics if
      // lat2 is an ArrayLattice.
      // Note that it requires lat2 to be non-const.
      lat2.getSlice (lat2Buffer, lat1Iter.position(),
		     lat1Iter.cursorShape());
      lat1Iter.rwCursor() += lat2Buffer;
      lat1Iter++;
    }
    timer.show ("  iter-get ");
    ///if (pa1) pa1->showCacheStatistics (cout);
    ///if (pa2) pa2->showCacheStatistics (cout);
  }
  {
    Timer timer;
    // This iterator uses the TileStepper.
    LatticeIterator<Int> lat1Iter (lat1, useRef);
    // Use tile shape of lat1, because they have to be iterated
    // in the same way. The cursor has to be resized if needed.
    RO_LatticeIterator<Int> lat2Iter (lat2, LatticeStepper
				       (lat1.shape(), lat1.niceCursorShape(),
					LatticeStepper::RESIZE),
				      useRef);
    while (! lat1Iter.atEnd()) {
      lat1Iter.rwCursor() += lat2Iter.cursor();
      lat1Iter++;
      lat2Iter++;
    }
    timer.show ("  iter-iter");
  }
  {
    Timer timer;
    LatticeIterator<Int> lat1Iter (lat1, useRef);
    Array<Int> lat2Buffer;
    while (! lat1Iter.atEnd()) {
      // Do separate getSlice to use reference semantics if
      // lat2 is an ArrayLattice.
      // Note that it requires lat2 to be non-const.
      lat2.getSlice (lat2Buffer, lat1Iter.position(),
		     lat1Iter.cursorShape());
      lat1Iter.rwCursor() += lat2Buffer;
      lat1Iter++;
    }
    timer.show ("  iter-get ");
  }
}


int main (int argc, const char *argv[])
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    {
      cout << "Creating a HDF5Lattice on disk" << endl;
      const TiledShape latticeShape(IPosition(4, 16, 12, 4, 32),
				    IPosition(4, 16, 12, 2,  1));
      HDF5Lattice<Int> pagedArr(latticeShape, "tHDF5Iterator_tmp.dat");
      Array<Int> arr(latticeShape.shape());
      indgen(arr);
      pagedArr.putSlice(arr, IPosition(latticeShape.shape().nelements(), 0));
    }
    //++++++++++++++++++++ Test HDF5ArrIter ++++++++++++++++++++
    cout << " Testing the RO iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testVectorROIter (pagedArr, False);
      testVectorROIter (pagedArr, True);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testMatrixROIter (pagedArr, False);
      testMatrixROIter (pagedArr, True);
    }
    // Check the Iterator with a Cube cursor. 
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testCubeROIter (pagedArr, False);
      testCubeROIter (pagedArr, True);
    }
    // Check the Iterator with an Array cursor. 
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testArrayROIter (pagedArr, False);
      testArrayROIter (pagedArr, True);
    }
    // Check the Iterator with an 8 element element cursor.
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      test8ElemROIter (pagedArr, False);
      test8ElemROIter (pagedArr, True);
    }
    // Check the Iterator with a tile cursor.
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testTileROIter (pagedArr, False);
      testTileROIter (pagedArr, True);
    }
    // Check the Iterator with a tiled line cursor.
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testTiledLineROIter (pagedArr, False);
      testTiledLineROIter (pagedArr, True);
    }
    // Check the copy constructor and assignment operator
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testCopyAssignROIter (pagedArr, False);
      testCopyAssignROIter (pagedArr, True);
    }
    // Test the non-congruent cursor handling
    {
      const HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      testNonCongruentROIter (pagedArr, False);
      testNonCongruentROIter (pagedArr, True);
    }

    cout << " Testing the RW iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      Array<Int> savarr = pagedArr.get();
      testVectorRWIter (pagedArr, False);
      pagedArr.put (savarr);
      testVectorRWIter (pagedArr, True);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      Array<Int> savarr = pagedArr.get();
      testMatrixRWIter (pagedArr, False);
      pagedArr.put (savarr);
      testMatrixRWIter (pagedArr, True);
    }
    // Check the Iterator with a Cube cursor. 
    {
      HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      Array<Int> savarr = pagedArr.get();
      testCubeRWIter (pagedArr, False);
      pagedArr.put (savarr);
      testCubeRWIter (pagedArr, True);
    }
    // Check the Iterator with an Array cursor. 
    {
      HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      Array<Int> savarr = pagedArr.get();
      testArrayRWIter (pagedArr, False);
      pagedArr.put (savarr);
      testArrayRWIter (pagedArr, True);
    }
    // Check the copy constructor and assignment operator
    {
      HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      Array<Int> savarr = pagedArr.get();
      testCopyAssignRWIter (pagedArr, False);
      pagedArr.put (savarr);
      testCopyAssignRWIter (pagedArr, True);
    }
    // Test the non-congruent cursor handling
    {
      HDF5Lattice<Int> pagedArr("tHDF5Iterator_tmp.dat");
      Array<Int> savarr = pagedArr.get();
      testNonCongruentRWIter (pagedArr, False);
      pagedArr.put (savarr);
      testNonCongruentRWIter (pagedArr, True);
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  try {
    cout << "Creating an ArrayLattice" << endl;
    const IPosition latticeShape(4, 16, 12, 2, 32);
    ArrayLattice<Int> refLattice(latticeShape);
    {
      Array<Int> arr(latticeShape);
      indgen(arr);
      refLattice.putSlice(arr, IPosition(latticeShape.nelements(), 0));
    }
    //++++++++++++++++++++ Test ArrLatticeIter ++++++++++++++++++++
    // Check the Iterator with a Vector cursor. 
    cout << " Testing the RO iterator" << endl;
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testVectorROIter (arrLattice, False);
      testVectorROIter (arrLattice, True);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testMatrixROIter (arrLattice, False);
      testMatrixROIter (arrLattice, True);
    }
    // Check the Iterator with a Cube cursor. 
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testCubeROIter (arrLattice, False);
      testCubeROIter (arrLattice, True);
    }
    // Check the Iterator with an Array cursor. 
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testArrayROIter (arrLattice, False);
      testArrayROIter (arrLattice, True);
    }
    // Check the Iterator with an 8 element element cursor.
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      test8ElemROIter (arrLattice, False);
      test8ElemROIter (arrLattice, True);
    }
    // Check the Iterator with a tile cursor.
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testTileROIter (arrLattice, False);
      testTileROIter (arrLattice, True);
    }
    // Check the Iterator with a tiled line cursor.
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testTiledLineROIter (arrLattice, False);
      testTiledLineROIter (arrLattice, True);
    }
    // Check the copy constructor and assignment operator
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testCopyAssignROIter (arrLattice, False);
      testCopyAssignROIter (arrLattice, True);
    }
    // Test the non-congruent cursor handling
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testNonCongruentROIter (arrLattice, False);
      testNonCongruentROIter (arrLattice, True);
    }

    cout << " Testing the RW iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      Array<Int> savarr = arrLattice.get();
      testVectorRWIter (arrLattice, False);
      arrLattice.put (savarr);
      testVectorRWIter (arrLattice, True);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      Array<Int> savarr = arrLattice.get();
      testMatrixRWIter (arrLattice, False);
      arrLattice.put (savarr);
      testMatrixRWIter (arrLattice, True);
    }
    // Check the Iterator with a Cube cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      Array<Int> savarr = arrLattice.get();
      testCubeRWIter (arrLattice, False);
      arrLattice.put (savarr);
      testCubeRWIter (arrLattice, True);
    }
    // Check the Iterator with an Array cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      Array<Int> savarr = arrLattice.get();
      testArrayRWIter (arrLattice, False);
      arrLattice.put (savarr);
      testArrayRWIter (arrLattice, True);
    }
    // Check the copy constructor and assignment operator
    {
      ArrayLattice<Int> arrLattice(refLattice);
      Array<Int> savarr = arrLattice.get();
      testCopyAssignRWIter (arrLattice, False);
      arrLattice.put (savarr);
      testCopyAssignRWIter (arrLattice, True);
    }
    // Test the non-congruent cursor handling
    {
      ArrayLattice<Int> arrLattice(refLattice);
      Array<Int> savarr = arrLattice.get();
      testNonCongruentRWIter (arrLattice, False);
      arrLattice.put (savarr);
      testNonCongruentRWIter (arrLattice, True);
    }
    // Test some performance aspects.
    {
      Input inp(1);
      inp.version(" ");
      inp.create("nx", "512", "Number of pixels along the x-axis", "int");
      inp.create("ny", "512", "Number of pixels along the y-axis", "int");
      inp.readArguments(argc, argv);
      const uInt nx=inp.getInt("nx");
      const uInt ny=inp.getInt("ny");
      IPosition shape(2,nx,ny);
      TiledShape tshape(shape, IPosition(2,nx,1));
      HDF5Lattice<Int> pagedArr1(tshape, "tHDF5Iterator_tmp.dat1");
      HDF5Lattice<Int> pagedArr2(tshape, "tHDF5Iterator_tmp.dat2");
      ArrayLattice<Int> latArr1(shape);
      ArrayLattice<Int> latArr2(shape);

      Array<Int> arr(latArr1.shape());
      indgen(arr);
      pagedArr1.put (arr);
      pagedArr2.put (arr);
      latArr1.put (arr);
      latArr2.put (arr);

      cout << "Shape " << shape << endl;
      cout << "paged+=paged useRef=False" << endl;
      testAdd (pagedArr1, pagedArr2, False);
      AlwaysAssert (allEQ(pagedArr1.get(), 4*arr), AipsError);
      cout << "paged+=paged useRef=True" << endl;
      testAdd (pagedArr1, pagedArr2, True);
      AlwaysAssert (allEQ(pagedArr1.get(), 7*arr), AipsError);
      cout << "array+=array useRef=False" << endl;
      testAdd (latArr1, latArr2, False);
      AlwaysAssert (allEQ(latArr1.get(), 4*arr), AipsError);
      cout << "array+=array useRef=True" << endl;
      testAdd (latArr1, latArr2, True);
      AlwaysAssert (allEQ(latArr1.get(), 7*arr), AipsError);
      cout << "paged+=array useRef=False" << endl;
      testAdd (pagedArr1, latArr2, False);
      AlwaysAssert (allEQ(pagedArr1.get(), 10*arr), AipsError);
      cout << "paged+=array useRef=True" << endl;
      testAdd (pagedArr1, latArr2, True);
      AlwaysAssert (allEQ(pagedArr1.get(), 13*arr), AipsError);
      cout << "lat+=paged useRef=False" << endl;
      testAdd (latArr1, pagedArr2, False);
      AlwaysAssert (allEQ(latArr1.get(), 10*arr), AipsError);
      cout << "lat+=paged useRef=True" << endl;
      testAdd (latArr1, pagedArr2, True);
      AlwaysAssert (allEQ(latArr1.get(), 13*arr), AipsError);
    }      
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
    cout << "OK" << endl;
    return 0;
}
