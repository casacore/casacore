//# tLatticeIterator.cc:  mechanical test of the LatticeIterator class
//# Copyright (C) 1995,1996,1997,1998
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
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/TiledLineStepper.h>
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

void testVectorROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using a Vector cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step);
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult.ac());
    AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    AlwaysAssert(latticeShape == iter.latticeShape(), AipsError);
    AlwaysAssert(cursorShape == iter.cursorShape().nonDegenerate(), AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
                     == True, AipsError);
        expectedResult.ac() += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult.ac() -= cursorShape.product();
    Int ns=0;
    for (; !iter.atStart(); iter--){
	AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
		     == True, AipsError);
	expectedResult.ac() -= cursorShape.product();
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

void testMatrixROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using a Matrix cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
    RO_LatticeIterator<Int>  iter(lattice, cursorShape);
    Matrix<Int> expectedResult(latticeShape(0), latticeShape(1));
    indgen(expectedResult.ac());
    AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tLatticeIterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac()) 
                     == True, AipsError);
        expectedResult.ac() += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape(2)*latticeShape(3),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    expectedPos(1) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult.ac() -= cursorShape.product();
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
                     == True, AipsError);
        expectedResult.ac() -= cursorShape.product();
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

void testCubeROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using a Cube cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				latticeShape(2));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step);
    Cube<Int> expectedResult(latticeShape(0), latticeShape(1),
			     latticeShape(2));
    indgen(expectedResult.ac());
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac()) == True,
		 AipsError);
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tLatticeIterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac())
                     == True, AipsError);
        expectedResult.ac() += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape(3), AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    expectedPos(1) = 0;
    expectedPos(2) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult.ac() -= cursorShape.product();
    for (; !iter.atStart(); iter--){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
                     == True, AipsError);
        expectedResult.ac() -= cursorShape.product();
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

void testArrayROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using an Array (4-D) cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(latticeShape);
    RO_LatticeIterator<Int>  iter(lattice, cursorShape);
    Array<Int> expectedResult(latticeShape);
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tLatticeIterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True, 
                     AipsError);
        expectedResult.ac() += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 1, AipsError);
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
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2, AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos = latticeShape - 1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void test8ElemROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using an 8 element cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,8);
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step);
    Array<Int> expectedResult(cursorShape);
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.vectorCursor().ac()) == True,
		 AipsError);
    AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate())
		 == True, AipsError);
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.vectorCursor().ac()) == True,
                     AipsError);
        expectedResult += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/8, AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 8;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedResult -= cursorShape.product();
    for (; !iter.atStart(); --iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor().nonDegenerate()) 
                     == True, AipsError);
        expectedResult -= cursorShape.product();
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*(latticeShape.product()/8), AipsError);
    expectedPos = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
    expectedPos(0) = 8-1;
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
}

void testTileROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using a tile cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(lattice.niceCursorShape());
    RO_LatticeIterator<Int>  iter(lattice);
    Array<Int> expectedResult(cursorShape);
    indgen(expectedResult);
    AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
        expectedResult += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/cursorShape.product(),
		 AipsError);
    for (; !iter.atStart(); --iter){
        expectedResult -= cursorShape.product();
        AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*latticeShape.product()/cursorShape.product(),
		 AipsError);
}

void testTiledLineROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using a tiled line cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(lattice.niceCursorShape());
    TiledLineStepper step(latticeShape, cursorShape, 0);
    RO_LatticeIterator<Int>  iter(lattice, step);
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult.ac());
    AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) == True,
		 AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		     == True, AipsError);
        expectedResult.ac() += latticeShape(0);
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    for (; !iter.atStart(); --iter){
        expectedResult.ac() -= latticeShape(0);
        AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		     == True, AipsError);
    }
    clock.show();
    nstep = iter.nsteps();
    AlwaysAssert(nstep == 2*latticeShape.product()/latticeShape(0),
		 AipsError);
}

void testCopyAssignROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing the copy constructor and assignment operator" << endl;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    RO_LatticeIterator<Int> iter(lattice, 
				 LatticeStepper(latticeShape, cursorShape));
    iter++;
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult.ac());
    expectedResult.ac() += cursorShape.product();
    AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
		 == True, AipsError);
    
    RO_LatticeIterator<Int> iterCopy(iter.copy());
    Vector<Int> expectedCopy(expectedResult.copy());
    AlwaysAssert(allEQ(expectedCopy.ac(), iterCopy.vectorCursor().ac())
		 == True, AipsError);
    iter++;
    expectedResult.ac() += cursorShape.product();
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
    iterCopy = iter.copy();
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

void testNonCongruentROIter (const Lattice<Int>& lattice)
{
    cout << "  Testing using a non-congruent cursor" << endl;
    const IPosition latticeShape(lattice.shape());
    IPosition cursorShape(2,9);
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(lattice, step);
    Matrix<Int> expectedResult(cursorShape);
    Vector<Int> oneRow(cursorShape(0));
    indgen(oneRow.ac());
    uInt i;
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        expectedResult.column(i) = oneRow;
        oneRow.ac() += latticeShape(0);
    }
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()),
		 AipsError);
    iter++;
    indgen(oneRow.ac(), cursorShape(0));
    for (i = 0; i < uInt(cursorShape(1)); i++) {
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
    RO_LatticeIterator<Int>  subIter(lattice, step);
    
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

void testVectorRWIter (Lattice<Int>& lattice)
{
    cout << "  Testing using a Vector cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    LatticeIterator<Int>  iter(lattice, step);
    Vector<Int> expectedResult(latticeShape(0));
    indgen(expectedResult.ac());
    AlwaysAssert(allEQ(expectedResult.ac(),iter.vectorCursor().ac())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    
    AlwaysAssert(latticeShape == iter.latticeShape(), AipsError);
    AlwaysAssert(cursorShape == iter.cursorShape().nonDegenerate(),
		 AipsError);
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac())
                     == True, AipsError);
        iter.rwVectorCursor()(0) -= expectedResult(0);
        expectedResult.ac() += cursorShape.product();
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
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
        iter.woCursor() = 1;
        expectedResult.ac() -= cursorShape.product();
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

void testMatrixRWIter (Lattice<Int>& lattice)
{
    cout << "  Testing using a Matrix cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(2, latticeShape(0), latticeShape(1));
    LatticeIterator<Int>  iter(lattice, cursorShape);
    Matrix<Int> expectedResult(latticeShape(0), latticeShape(1));
    expectedResult = 1;
    AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac()) 
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tLatticeIterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.matrixCursor().ac()) 
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
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
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

void testCubeRWIter (Lattice<Int>& lattice)
{
    cout << "  Testing using a Cube cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(3, latticeShape(0), latticeShape(1),
				latticeShape(2));
    LatticeStepper step(latticeShape, cursorShape);
    LatticeIterator<Int>  iter(lattice, step);
    Cube<Int> expectedResult(latticeShape(0), latticeShape(1),
			     latticeShape(2));
    expectedResult.ac() = 3;
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac())
		 == True, AipsError);
    AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate()) 
		 == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tLatticeIterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cubeCursor().ac())
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
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor().nonDegenerate())
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

void testArrayRWIter (Lattice<Int>& lattice)
{
    cout << "  Testing using an Array (4-D) cursor" << endl;
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(latticeShape);
    LatticeIterator<Int>  iter(lattice, cursorShape);
    Array<Int> expectedResult(latticeShape);
    expectedResult = 5;
    AlwaysAssert(allEQ(expectedResult, iter.cursor()) == True, AipsError);
    try {
        Vector<Int> temp(iter.vectorCursor());
        throw(AipsError("tLatticeIterator - "
                        "vectorCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("one non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Matrix<Int> temp(iter.matrixCursor());
        throw(AipsError("tLatticeIterator - "
                        "matrixCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("two non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    try {
        Cube<Int> temp(iter.cubeCursor());
        throw(AipsError("tLatticeIterator - "
                        "cubeCursor worked where it should not have"));
    } catch (AipsError x) {
        if (!x.getMesg().contains("three non-degenerate")) {
	    throw (AipsError (x.getMesg()));
        }
    } end_try;
    Timer clock;
    for (iter.reset(); !iter.atEnd(); ++iter){
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True,
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
        AlwaysAssert(allEQ(expectedResult.ac(), iter.cursor()) == True,
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

void testCopyAssignRWIter (Lattice<Int>& lattice)
{
    cout << "  Testing the copy constructor and assignment operator" << endl;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeIterator<Int> iter(lattice, 
			      LatticeStepper(latticeShape, cursorShape));
    iter++;
    Vector<Int> expectedResult(latticeShape(0));
    expectedResult = 7;
    AlwaysAssert(allEQ(expectedResult.ac(),iter.vectorCursor().ac()) == True,
		 AipsError);
    
    LatticeIterator<Int> iterCopy(iter.copy());
    AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
		 == True, AipsError);
    iter++;
    iter.woCursor() = 2;
    expectedResult.ac() = 2;
    AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
		 == True, AipsError);
    expectedResult.ac() = 7;
    AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
		 == True, AipsError);
    iterCopy--;
    iterCopy.woCursor() = 0;
    expectedResult.ac() = 2;
    AlwaysAssert(allEQ(expectedResult.ac(), iter.vectorCursor().ac()) 
		 == True, AipsError);
    expectedResult.ac() = 0;
    AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
		 == True, AipsError);
    
    iterCopy = iter.copy();
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
    --iterCopy;
    iterCopy--;
    expectedResult.ac() = 0;
    AlwaysAssert(allEQ(expectedResult.ac(), iterCopy.vectorCursor().ac()) 
		 == True, AipsError);
}

void testNonCongruentRWIter (Lattice<Int>& lattice)
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
    LatticeIterator<Int> iter(lattice, step);
    Matrix<Int> expectedResult1(cursorShape);
    Vector<Int> oneRow(cursorShape(0));
    indgen(oneRow.ac());
    uInt i;
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        expectedResult1.column(i) = oneRow;
        oneRow.ac() += latticeShape(0);
    }
    AlwaysAssert(allEQ(expectedResult1.ac(), iter.cursor().nonDegenerate()),
		 AipsError);
    iter.woCursor() = (-1 * iter.cursor() - 1);
    iter++;
    Matrix<Int> expectedResult2(cursorShape);
    indgen(oneRow.ac(), cursorShape(0));
    for (i = 0; i < uInt(cursorShape(1)); i++) {
        oneRow(7) = 0;
        oneRow(8) = 0;
        expectedResult2.column(i) = oneRow;
        oneRow.ac() += latticeShape(0);
    }
    AlwaysAssert(allEQ(expectedResult2.ac(), iter.cursor().nonDegenerate()),
		 AipsError);
    iter.woCursor() = (-1 * iter.cursor() - 1);
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
    iter.woCursor() = (-1 * iter.cursor() - 1);
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
    iter.woCursor() = (-1 * iter.cursor() - 1);
    iter--;
    iter++;
    iter.rwMatrixCursor().ac() += expectedResult4.ac();
    {
        Array<Int> m(iter.rwMatrixCursor()(IPosition(2,0),IPosition(2,6,2)));
        m += 1;
        AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    }
    iter--;
    iter.rwMatrixCursor().ac() += expectedResult3.ac();
    {
        Array<Int> m(iter.rwMatrixCursor()(IPosition(2,0),IPosition(2,8,2)));
        m += 1;
        AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    }
    iter--;
    iter.rwMatrixCursor().ac() += expectedResult2.ac();
    {
        Array<Int> m(iter.rwMatrixCursor()(IPosition(2,0),IPosition(2,6,8)));
        m += 1;
        AlwaysAssert(allEQ(iter.cursor(), 0), AipsError);
    }
    iter--;
    iter.rwMatrixCursor().ac() += expectedResult1.ac();
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
    LatticeIterator<Int> subIter(lattice, step);
    
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
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
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
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
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
    subIter.woCursor() = (-1 * subIter.cursor() - 1);
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


main ()
{
  try {
    {
      cout << "Creating a PagedArray on disk" << endl;
      const TiledShape latticeShape(IPosition(4, 16, 12, 4, 32),
				    IPosition(4, 16, 12, 2,  1));
      PagedArray<Int> pagedArr(latticeShape, "tLatticeIterator_tmp.table");
      Array<Int> arr(latticeShape.shape());
      indgen(arr);
      pagedArr.putSlice(arr, IPosition(latticeShape.shape().nelements(), 0));
    }
    //++++++++++++++++++++ Test PagedArrIter ++++++++++++++++++++
    cout << " Testing the RO iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testVectorROIter (pagedArr);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testMatrixROIter (pagedArr);
    }
    // Check the Iterator with a Cube cursor. 
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testCubeROIter (pagedArr);
    }
    // Check the Iterator with an Array cursor. 
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testArrayROIter (pagedArr);
    }
    // Check the Iterator with an 8 element element cursor.
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      test8ElemROIter (pagedArr);
    }
    // Check the Iterator with a tile cursor.
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testTileROIter (pagedArr);
    }
    // Check the Iterator with a tiled line cursor.
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testTiledLineROIter (pagedArr);
    }
    // Check the copy constructor and assignment operator
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testCopyAssignROIter (pagedArr);
    }
    // Test the non-congruent cursor handling
    {
      const PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testNonCongruentROIter (pagedArr);
    }

    cout << " Testing the RW iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testVectorRWIter (pagedArr);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testMatrixRWIter (pagedArr);
    }
    // Check the Iterator with a Cube cursor. 
    {
      PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testCubeRWIter (pagedArr);
    }
    // Check the Iterator with an Array cursor. 
    {
      PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testArrayRWIter (pagedArr);
    }
    // Check the copy constructor and assignment operator
    {
      PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testCopyAssignRWIter (pagedArr);
    }
    // Test the non-congruent cursor handling
    {
      PagedArray<Int> pagedArr("tLatticeIterator_tmp.table");
      testNonCongruentRWIter (pagedArr);
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;

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
      testVectorROIter (arrLattice);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testMatrixROIter (arrLattice);
    }
    // Check the Iterator with a Cube cursor. 
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testCubeROIter (arrLattice);
    }
    // Check the Iterator with an Array cursor. 
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testArrayROIter (arrLattice);
    }
    // Check the Iterator with an 8 element element cursor.
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      test8ElemROIter (arrLattice);
    }
    // Check the Iterator with a tile cursor.
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testTileROIter (arrLattice);
    }
    // Check the Iterator with a tiled line cursor.
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testTiledLineROIter (arrLattice);
    }
    // Check the copy constructor and assignment operator
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testCopyAssignROIter (arrLattice);
    }
    // Test the non-congruent cursor handling
    {
      const ArrayLattice<Int> arrLattice(refLattice);
      testNonCongruentROIter (arrLattice);
    }

    cout << " Testing the RW iterator" << endl;
    // Check the Iterator with a Vector cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      testVectorRWIter (arrLattice);
    }
    // Check the Iterator with a Matrix cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      testMatrixRWIter (arrLattice);
    }
    // Check the Iterator with a Cube cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      testCubeRWIter (arrLattice);
    }
    // Check the Iterator with an Array cursor. 
    {
      ArrayLattice<Int> arrLattice(refLattice);
      testArrayRWIter (arrLattice);
    }
    // Check the copy constructor and assignment operator
    {
      ArrayLattice<Int> arrLattice(refLattice);
      testCopyAssignRWIter (arrLattice);
    }
    // Test the non-congruent cursor handling
    {
      ArrayLattice<Int> arrLattice(refLattice);
      testNonCongruentRWIter (arrLattice);
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
    cout << "OK" << endl;
    return 0;
}
