//# tArrayLattice.cc: test ArrayLattices and ArrayLatticeIterators.
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/lattices/Lattices/ArrayLattice.h> 
#include <casacore/lattices/Lattices/LatticeIterator.h> 
#include <casacore/lattices/Lattices/LatticeStepper.h> 

#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Int const_arg_func(const Int &val)
{
  return 3*val;
}

Int func(Int val)
{
  return 2*val*val;
}

int main()
{
  try{
    // make an array
    Array<Float> array1(IPosition(1,256));
    Int i;
    for (i=0; i<256; i++) {
        array1(IPosition(1,i)) = i;
    }
      
    // make another array
    Array<Float> array2(IPosition(2,256,128));
    for (i=0; i<256; i++) {
        for (Int j=0; j<128; j++) {
	    array2(IPosition(2,i,j)) = i+j;
	}
    }
      
    // default ctor, useless (though legal) until assigned to
    ArrayLattice<Float> al0;

    // construct a new ArrayLattice, with 'array' as contents, in
    ArrayLattice<Float> al1(IPosition(1,256));
      
    // construct a new ArrayLattice, with 'array' as contents, with a 
    const ArrayLattice<Float> al2(IPosition(2,256,128));
    
    // reconstruct from a pre-existing ArrayLattice in the Table, 
    ArrayLattice<Float> al3(array1);
    
    // reconstruct from a pre-existing ArrayLattice in the Table, with 
    // TableColumn name, and row number (defaults to row zero)
    const ArrayLattice<Float> al4(array2);
    
    // the copy constructor (reference semantics):  passing by value
    // doesn't make sense, because it would require the creation of a
    // temporary (but possibly huge) file on disk
    ArrayLattice<Float> al5(al3);
    
    // test reference nature
    AlwaysAssert(near(al3(IPosition(1,0)), 0.0f, 1E-6), AipsError);
    al5.putAt (33.0, IPosition(1,0));
    AlwaysAssert(near(al3(IPosition(1,0)), 33.0f, 1E-6), AipsError);

    // the assignment operator.  typical use would be to create a new
    // ArrayLattice that (at least initially) is a copy of another one:
    al0 = al4;

    // test copy nature
    AlwaysAssert(near(al0(IPosition(2,0)), 0.0f, 1E-6), AipsError);
    al0.putAt (33.0, IPosition(2,0));
    AlwaysAssert(near(al4(IPosition(2,0)), 33.0f, 1E-6) == False, AipsError);

    ArrayLattice<Int> al6(IPosition(4,5,6,7,8));
    // returns the shape of the ArrayLattice.
    AlwaysAssert(al6.shape() == IPosition(4,5,6,7,8), AipsError);
    AlwaysAssert(al4.shape() == IPosition(2,256,128), AipsError);
    
    // function which extracts an Array of values from a Lattice - a read-only 
    // operation. 
    COWPtr<Array<Float> > buffer1;
    IPosition start(2, 0, 0), shape(2, 128, 64), stride(2, 2, 2);
    AlwaysAssert(!al4.getSlice(buffer1, start, shape, stride), AipsError);
    AlwaysAssert(near(buffer1.ref()(IPosition(2,0,0)),0.0f, 1E-6),AipsError);
    AlwaysAssert(near(buffer1.ref()(IPosition(2,127,0)),254.f,1E-6),AipsError);
    AlwaysAssert(near(buffer1.ref()(IPosition(2,0,63)),126.0f,1E-6),AipsError);
    AlwaysAssert(near(buffer1.ref()(IPosition(2,127,63)),380.f,1E-6),AipsError);
    COWPtr<Array<Float> > buffer2;
    Slicer theSlice(start, shape, stride);
    AlwaysAssert(!al4.getSlice(buffer2, theSlice), AipsError);
    AlwaysAssert(near(buffer2.ref()(IPosition(2,0,0)),0.0f, 1E-6), AipsError);
    AlwaysAssert(near(buffer2.ref()(IPosition(2,127,0)),254.f,1E-6),AipsError);
    AlwaysAssert(near(buffer2.ref()(IPosition(2,0,63)),126.f,1E-6),AipsError);
    AlwaysAssert(near(buffer2.ref()(IPosition(2,127,63)),380.f,1E-6),AipsError);

    Array<Float> buffer3;
    AlwaysAssert(al0.getSlice(buffer3, start, shape, stride), AipsError);
    AlwaysAssert(near(buffer3(IPosition(2,0,0)),33.0f,1E-6), AipsError);
    AlwaysAssert(near(buffer3(IPosition(2,127,0)),254.0f,1E-6), AipsError);
    AlwaysAssert(near(buffer3(IPosition(2,0,63)),126.0f,1E-6), AipsError);
    AlwaysAssert(near(buffer3(IPosition(2,127,63)),380.0f,1E-6), AipsError);
    
    Array<Float> buffer4;
    AlwaysAssert(al0.getSlice(buffer4, theSlice), AipsError);
    AlwaysAssert(near(buffer4(IPosition(2,0,0)),33.0f,1E-6), AipsError);
    AlwaysAssert(near(buffer4(IPosition(2,127,0)),254.0f,1E-6), AipsError);
    AlwaysAssert(near(buffer4(IPosition(2,0,63 )),126.0f,1E-6), AipsError);
    AlwaysAssert(near(buffer4(IPosition(2,127,63)),380.0f,1E-6), AipsError);

    // test reference nature of slicer
    buffer3.set(99.0);
    AlwaysAssert(near(al0(IPosition(2,0)), 99.0f), AipsError);   
    
    // put 'value' at every element of the ArrayLattice
    al6.set(42);
    
    // pick a couple of locations at random
    AlwaysAssert(al6.getAt(IPosition(4,3)) == 42, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,1,2,3,4)) == 42, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,4,5,6,7)) == 42, AipsError);
    
    Array<Int> sourceBuffer(IPosition(4,4));
    sourceBuffer = 6;
    // function which places an Array of values within the lattice
    al6.putSlice(sourceBuffer,IPosition(4,1,2,3,4), IPosition(4,1));
    
    // check the same spots again
    AlwaysAssert(al6.getAt(IPosition(4,3)) == 42, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,1,2,3,4)) == 6, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,4,5,6,7)) == 6, AipsError);
    
    // function which returns an Array of the data within this Lattice.
    // <group>
    AlwaysAssert(allEQ(al3.asArray(), array1), AipsError);
    array2(IPosition(2,0)) = 33.0;
    AlwaysAssert(allEQ(al4.asArray(), array2), AipsError);
    // </group>

    // a handy place to check for internal consistency
    AlwaysAssert(al4.ok(), AipsError);
    
// -------------------inherited from Lattice-----------------------------

    // returns the value of the single element located at the argument
    // IPosition.  
    al6.putAt(99, IPosition(4,0));
    AlwaysAssert(al6.getAt(IPosition(4,0)) == 99, AipsError);
    
    // returns the number of axes in this Lattice.
    AlwaysAssert(al6.ndim() == 4, AipsError);
    
    // returns the total number of elements in this Lattice.
    AlwaysAssert(al6.nelements() == 1680, AipsError);
    
    // returns a value of "True" if this instance of Lattice and 'other' have 
    // the same shape, otherwise returns a value of "False".
    AlwaysAssert(al0.conform(al4), AipsError);

    // replace every element, x, of the lattice with the result of f(x).
    // You must pass in the address of the function -- so the function
    // must be declared and defined in the scope of your program.  
    // All versions of apply require a function that accepts a single 
    // argument of type T (the Lattice template actual type) and returns
    // a result of the same type.  The first apply expects a function with
    // an argument passed by value; the second expects the argument to
    // be passed by const reference; the third requires an instance of the 
    // class Functional<T,T>.  The first form ought to run faster
    // for the built-in types, which may be an issue for large Lattices
    // stored in memory, where disk access is not an issue.

    al6.set(2);
    // check a couple of random spots
    AlwaysAssert(al6.getAt(IPosition(4,4))==2, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,2,3,4,5))==2, AipsError);

    // func = arg*arg*2
    al6.apply(&func);
    // check a couple of random spots
    AlwaysAssert(al6.getAt(IPosition(4,4))==8, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,2,3,4,5))==8, AipsError);

    // const_arg_func = arg*3
    al6.apply(&const_arg_func);
    AlwaysAssert(al6.getAt(IPosition(4,4))==24, AipsError);
    AlwaysAssert(al6.getAt(IPosition(4,2,3,4,5))==24, AipsError);

    Polynomial<Float> poly(3);
    poly.setCoefficient(1, 0.5);
    poly.setCoefficient(2, 0.75);
    poly.setCoefficient(3, 1.0);

    al3.apply(poly);
    AlwaysAssert(near(al3(IPosition(1,0)), poly(33), 1E-6), AipsError);
    AlwaysAssert(near(al3(IPosition(1,127)), poly(127), 1E-6), AipsError);


// ----------------------RO_LatticeIterator----------------------------------

    IPosition zvector(4,1,1,7,1);
    LatticeStepper method(al6.shape(), zvector);
    // Lattice and LatticeNavigator constructor
    RO_LatticeIterator<Int> al6ROIter(al6, method);

    // LatticeNavigator default "BLC to TRC" constructor 
    RO_LatticeIterator<Float> al3ROIter(al3,IPosition(1,8));  

    // copy ctor (uses reference sematics)
    RO_LatticeIterator<Int> al6ROItercopy(al6ROIter);

    // destructor (cleans up dangling references)
    //virtual ~RO_LatticeIterator();

    // assignment operator (uses reference semantics)
    //RO_LatticeIterator<T> &operator=(const RO_LatticeIterator<T> &other);

    // Function which returns a value of "True" if the cursor is at the start.
    AlwaysAssert(al6ROIter.atStart(), AipsError);
    
    // Increment operator - increment the cursor to the next position. 
    // <group>
    al6ROIter++;
    ++al6ROIter;
    // </group>
    AlwaysAssert(!al6ROIter.atStart(), AipsError);
    
    // Decrement operator - decrement the cursor to the next position.
    // <group>
    al6ROIter--;
    --al6ROIter;
    --al6ROIter;
    // </group>
    AlwaysAssert(al6ROIter.atStart(), AipsError);
    
    // Function which resets the cursor to the beginning of the Lattice 
    // (also sets the number of steps taken to zero.)
    al6ROIter++;
    al6ROIter.reset();
    AlwaysAssert(al6ROIter.atStart(), AipsError);

    // Function which returns a value of "True" if the cursor is at the end.
    Int I;
    for (I=0; I<240; I++) {
        al6ROIter++;
    }
    AlwaysAssert(al6ROIter.atEnd(), AipsError);
  
    // Function which returns the number of steps taken since construction
    // or since reset().  <note> This is a running count 
    // of all cursor movement (operator++ or operator--) since doing x iter++
    // followed by x iter-- does not necessarily put the cursor back to the
    // origin of the Lattice </note> 
    AlwaysAssert(al6ROIter.nsteps() == 240, AipsError);
  
    // Function which returns the position of the beginning of the cursor 
    // within the lattice.
    al6ROIter.reset();
    AlwaysAssert(al6ROIter.position() == IPosition(4,0), AipsError);

    // Function which returns the end of the cursor (i.e. the cursor position
    // plus the cursor shape.)
    AlwaysAssert(al6ROIter.endPosition() == IPosition(4,0,0,6,0), 
		 AipsError);

    // Function which returns the shape of the Lattice being iterated through.
    AlwaysAssert(al6ROIter.latticeShape() == al6.shape(), AipsError);

    // Function which returns the shape of the cursor as set by the 
    // LatticeNavigator method.
    AlwaysAssert(al6ROIter.cursorShape() == zvector, AipsError);

    // Function which returns a reference to the data in the Lattice.  
    // <note> The cursor array may have fewer dimensions than the
    // Lattice. A call of the function whose return value is
    // inappropriate with reference to the cursor shape as defined by
    // the LatticeNavigator will throw an exception. </note> 
    Vector<Int> zvectdata(al6ROIter.vectorCursor());
    AlwaysAssert(allEQ(zvectdata, 24), AipsError); 
    AlwaysAssert(zvectdata.ndim() == 1, AipsError); 
    AlwaysAssert(zvectdata.shape() == IPosition(1,7), AipsError); 

    Array<Int> zarray(al6ROIter.cursor());
    AlwaysAssert(allEQ(zarray, 24), AipsError); 
    AlwaysAssert(zarray.ndim() == 4, AipsError); 
    AlwaysAssert(zarray.shape() == IPosition(4,1,1,7,1), AipsError); 

    // test functions which should throw exceptions
    Bool caught = False;
    try {
      al6ROIter.matrixCursor();
    } catch (AipsError x) {
      caught = True;
    } 
    AlwaysAssert(caught, AipsError);

    caught = False;
    try {
      al6ROIter.cubeCursor();
    } catch (AipsError x) {
      caught = True;
    } 
    AlwaysAssert(caught, AipsError);

    // check internals for sensibility
//    AlwaysAssert(al6ROIter.ok(), AipsError);

    IPosition xymatrix(2,5,6);
    LatticeStepper newMethod(al6.shape(), xymatrix);

// -------------------Read&Write LatticeIterator--------------------

    // Lattice and LatticeNavigator ctor
    LatticeIterator<Int> al6Iter(al6, newMethod);
    
    // LatticeNavigator default "BLC to TRC" constructor
    LatticeIterator<Float> al3Iter(al3, IPosition(1,8));
    
    // copy ctor (uses reference sematics)
    LatticeIterator<Int> copyal6Iter(al6Iter);
    
    // destructor (cleans up dangling references)
    //~LatticeIterator();
    
    // assignment operator (uses reference semantics)
    //LatticeIterator<T> &operator=(const LatticeIterator<T> &other);
    
    // Function which returns a reference to the data in the Lattice.  <note>
    // The cursor array may have fewer dimensions than the Lattice. A call of 
    // the function whose return value is inappropriate with reference to the 
    // cursor shape as defined by the LatticeNavigator will throw an 
    // exception</note>
    Matrix<Int> xymatdata(al6Iter.matrixCursor());
    AlwaysAssert(allEQ(xymatdata, 24), AipsError); 
    AlwaysAssert(xymatdata.ndim() == 2, AipsError); 
    AlwaysAssert(xymatdata.shape() == xymatrix, AipsError); 
    
    Array<Int> xyarray(al6Iter.cursor());
    AlwaysAssert(allEQ(xyarray, 24), AipsError); 
    AlwaysAssert(xyarray.ndim() == 4, AipsError); 
    AlwaysAssert(xyarray.shape() == IPosition(4,5,6,1,1), AipsError); 
    // test functions which should throw exceptions
    caught = False;
    try {
      al6Iter.vectorCursor();
    } catch (AipsError x) {
      caught = True;
    } 
    AlwaysAssert(caught, AipsError);

    caught = False;
    try {
      al6Iter.cubeCursor();
    } catch (AipsError x) {
      caught = True;
    } 
    AlwaysAssert(caught, AipsError);
    

// --------------------- inherited from RO_LatticeIterator -----------    

    // Function which returns a value of "True" if the cursor is at the start.
    AlwaysAssert(al6Iter.atStart(), AipsError);
    
    // Increment operator - increment the cursor to the next position. 
    // <group>
    al6Iter++;
    ++al6Iter;
    // </group>
    AlwaysAssert(!al6Iter.atStart(), AipsError);
    
    // Decrement operator - decrement the cursor to the next position.
    // <group>
    al6Iter--;
    --al6Iter;
    --al6Iter;
    // </group>
    AlwaysAssert(al6Iter.atStart(), AipsError);
    
    // Function which resets the cursor to the beginning of the Lattice 
    // (also sets the number of steps taken to zero.)
    al6Iter++;
    al6Iter.reset();
    AlwaysAssert(al6Iter.atStart(), AipsError);

    // Function which returns a value of "True" if the cursor is at the end.
    for (I=0; I<56; I++) {
        al6Iter++;
    }
    AlwaysAssert(al6Iter.atEnd(), AipsError);
  
    // Function which returns the number of steps taken since construction
    // or since reset().  <note> This is a running count 
    // of all cursor movement (operator++ or operator--) since doing x iter++
    // followed by x iter-- does not necessarily put the cursor back to the
    // origin of the Lattice </note> 
    AlwaysAssert(al6Iter.nsteps() == 56, AipsError);
  
    // Function which returns the position of the beginning of the cursor 
    // within the lattice.
    al6Iter.reset();
    AlwaysAssert(al6Iter.position() == IPosition(4,0), AipsError);

    // Function which returns the end of the cursor (i.e. the cursor position
    // plus the cursor shape.)
    AlwaysAssert(al6Iter.endPosition() == IPosition(4,4,5,0,0), 
		 AipsError);

    // Function which returns the shape of the Lattice being iterated through.
    AlwaysAssert(al6Iter.latticeShape() == al6.shape(), AipsError);

    // Function which returns the shape of the cursor as set by the 
    // LatticeNavigator method.
    AlwaysAssert(al6Iter.cursorShape() == IPosition(4,5,6,1,1), AipsError);
    
// -------------------- test Iterator very hard ---------------------
    IPosition orientation;
    Int j, k, l;
    for (i=0;i<4;i++) {
      for (j=0;j<4; j++) {
	for (k=0; k<4; k++) {
	  for (l=0; l<4; l++) {
	    if (l!=k && l!=j && l!=i) {
	      if (k!=j && k!=i) {
		if (j!=i) {
		  orientation = IPosition(4,i,j,k,l);

		  // ------------------- integral shaped vectors ----------
		  IPosition xvector(1,5);
		  LatticeStepper xvectorstepper(al6.shape(), xvector,
						orientation);
		  LatticeIterator<Int> xiter(al6, xvectorstepper);
		  for (;!xiter.atEnd();xiter++) {}
		  AlwaysAssert(xiter.nsteps() == 336, AipsError);
		  AlwaysAssert(allEQ(xiter.vectorCursor(), 24),AipsError);

		  IPosition yvector(2,1,6);
		  LatticeStepper yvectorstepper(al6.shape(), yvector, 
						orientation);
		  LatticeIterator<Int> yiter(al6, yvectorstepper);
		  for (;!yiter.atEnd();yiter++) {}
		  AlwaysAssert(yiter.nsteps() == 280, AipsError);
		  AlwaysAssert(allEQ(yiter.vectorCursor(), 24),AipsError);

		  LatticeStepper zvectorstepper(al6.shape(), zvector,
						orientation);
		  LatticeIterator<Int> ziter(al6, zvectorstepper);
		  for (;!ziter.atEnd();ziter++) {}
		  AlwaysAssert(ziter.nsteps() == 240, AipsError);
		  AlwaysAssert(allEQ(ziter.vectorCursor(), 24),AipsError);

		  IPosition tvector(4,1,1,1,8);
		  LatticeStepper tvectorstepper(al6.shape(), tvector, 
						orientation);
		  LatticeIterator<Int> titer(al6, tvectorstepper);
		  for (;!titer.atEnd();titer++) {}
		  AlwaysAssert(titer.nsteps() == 210, AipsError);
		  AlwaysAssert(allEQ(titer.vectorCursor(),24), AipsError);

// ----------------------non integral vectors------------------------
// use the algorithm: shape = ceiling(axis length / 2)
		  IPosition xnonIntgrlvector(1,3);
		  LatticeStepper xnonIntgrlvectorstepper(al6.shape(), 
							 xnonIntgrlvector,
							 orientation);
		  LatticeIterator<Int> nixiter(al6, xnonIntgrlvectorstepper);
		  for (;!nixiter.atEnd();nixiter++) {}
		  AlwaysAssert(nixiter.nsteps() == 672, AipsError);
		  Vector<Int> tester(3);
		  tester.set(24);
		  tester(2) = 0;
		  AlwaysAssert(allEQ(nixiter.vectorCursor(), tester),
			       AipsError);
		  
		  IPosition ynonIntgrlvector(2,1,4);
		  LatticeStepper ynonIntgrlvectorstepper(al6.shape(), 
							 ynonIntgrlvector,
							 orientation);
		  LatticeIterator<Int> niyiter(al6, ynonIntgrlvectorstepper);
		  for (;!niyiter.atEnd();niyiter++) {}
		  AlwaysAssert(niyiter.nsteps() == 560, AipsError);
		  tester.resize(4);
		  tester.set(24);
		  tester(2) = 0;
		  tester(3) = 0;
		  AlwaysAssert(allEQ(niyiter.vectorCursor(), 
				     tester), AipsError);
		  
		  IPosition znonIntgrlvector(3,1,1,4);
		  LatticeStepper znonIntgrlvectorstepper(al6.shape(),
							 znonIntgrlvector,
							 orientation);
		  LatticeIterator<Int> niziter(al6, znonIntgrlvectorstepper);
		  for (;!niziter.atEnd();niziter++) {}
		  AlwaysAssert(niziter.nsteps() == 480, AipsError);
		  tester(2) = 24;
		  AlwaysAssert(allEQ(niziter.vectorCursor(),
				     tester), AipsError);
		  
		  IPosition tnonIntgrlvector(4,1,1,1,5);
		  LatticeStepper tnonIntgrlvectorstepper(al6.shape(),
							 tnonIntgrlvector,
							 orientation);
		  LatticeIterator<Int> nititer(al6, tnonIntgrlvectorstepper);
		  for (;!nititer.atEnd();nititer++) {}
		  AlwaysAssert(nititer.nsteps() == 420, AipsError);
		  tester.resize(5);
		  tester.set(24);
		  tester(3) = 0;
		  tester(4) = 0;
		  AlwaysAssert(allEQ(nititer.vectorCursor(),
				     tester), AipsError);    

// -------------------------integral matrices----------------------------

		  LatticeStepper xymatrixstepper(al6.shape(), xymatrix, 
						 orientation);
		  LatticeIterator<Int> xyiter(al6, xymatrixstepper);
		  for (;!xyiter.atEnd();xyiter++) {}
		  AlwaysAssert(xyiter.nsteps() == 56, AipsError);
		  AlwaysAssert(allEQ(xyiter.matrixCursor(), 24), 
			       AipsError);

		  IPosition xzmatrix(3,5,1,7);
		  LatticeStepper xzmatrixstepper(al6.shape(), xzmatrix,
						 orientation);
		  LatticeIterator<Int> xziter(al6, xzmatrixstepper);
		  for (;!xziter.atEnd();xziter++) {}
		  AlwaysAssert(xziter.nsteps() == 48, AipsError);
		  AlwaysAssert(allEQ(xziter.matrixCursor(), 24), 
			       AipsError);

		  IPosition xtmatrix(4,5,1,1,8);
		  LatticeStepper xtmatrixstepper(al6.shape(), xtmatrix,
						 orientation);
		  LatticeIterator<Int> xtiter(al6, xtmatrixstepper);
		  for (;!xtiter.atEnd();xtiter++) {}
		  AlwaysAssert(xtiter.nsteps() == 42, AipsError);
		  AlwaysAssert(allEQ(xtiter.matrixCursor(), 24), 
			       AipsError);

		  IPosition yzmatrix(3,1,6,7);
		  LatticeStepper yzmatrixstepper(al6.shape(), yzmatrix,
						 orientation);
		  LatticeIterator<Int> yziter(al6, yzmatrixstepper);
		  for (;!yziter.atEnd();yziter++) {}
		  AlwaysAssert(yziter.nsteps() == 40, AipsError);
		  AlwaysAssert(allEQ(yziter.matrixCursor(), 24),
			       AipsError);

		  IPosition ytmatrix(4,1,6,1,8);
		  LatticeStepper ytmatrixstepper(al6.shape(), ytmatrix,
						 orientation);
		  LatticeIterator<Int> ytiter(al6, ytmatrixstepper);
		  for (;!ytiter.atEnd();ytiter++) {}
		  AlwaysAssert(ytiter.nsteps() == 35, AipsError);
		  AlwaysAssert(allEQ(ytiter.matrixCursor(), 24),
			       AipsError);

		  IPosition ztmatrix(4,1,1,7,8);
		  LatticeStepper ztmatrixstepper(al6.shape(), ztmatrix, 
						 orientation);
		  LatticeIterator<Int> ztiter(al6, ztmatrixstepper);
		  for (;!ztiter.atEnd();ztiter++) {}
		  AlwaysAssert(ztiter.nsteps() == 30, AipsError);
		  AlwaysAssert(allEQ(ztiter.matrixCursor(), 24),
			       AipsError);

// -----------------------non integral matrices----------------------------

		  IPosition xyNonItgrlmatrix1(2,3,6);
		  LatticeStepper xyNonItgrlmatrix1stepper(al6.shape(), 
							  xyNonItgrlmatrix1);
		  LatticeIterator<Int> nixyiter(al6, xyNonItgrlmatrix1stepper);
		  for (;!nixyiter.atEnd();nixyiter++) {}
		  AlwaysAssert(nixyiter.nsteps() == 112, AipsError);
		  Matrix<Int> test(xyNonItgrlmatrix1);
		  test.set(24);
		  test.row(2) = 0;
		  AlwaysAssert(allEQ(nixyiter.matrixCursor(), test),
			       AipsError);

		  IPosition xyNonItgrlmatrix2(2,5,4);
		  LatticeStepper xyNonItgrlmatrix2stepper(al6.shape(), 
							  xyNonItgrlmatrix2, 
							  orientation);
		  LatticeIterator<Int> ni2xyiter(al6,xyNonItgrlmatrix2stepper);
		  for (;!ni2xyiter.atEnd();ni2xyiter++) {}
		  AlwaysAssert(ni2xyiter.nsteps() == 112, AipsError);
		  test.resize(xyNonItgrlmatrix2);
		  test.set(24);
		  test.column(2) = 0;
		  test.column(3) = 0;
		  AlwaysAssert(allEQ(ni2xyiter.matrixCursor(), test),
			       AipsError);

		  IPosition xyNonItgrlmatrix3(2,3,4);
		  LatticeStepper xyNonItgrlmatrix3stepper(al6.shape(),
							  xyNonItgrlmatrix3,
							  orientation);
		  LatticeIterator<Int> ni3xyiter(al6,xyNonItgrlmatrix3stepper);
		  for (;!ni3xyiter.atEnd();ni3xyiter++) {}
		  AlwaysAssert(ni3xyiter.nsteps() == 224, AipsError);

		  test.resize(xyNonItgrlmatrix3);
		  test.set(24);
		  test.row(2) = 0;
		  test.column(2) = 0;
		  test.column(3) = 0;
		  AlwaysAssert(allEQ(ni3xyiter.matrixCursor(), test),
			       AipsError);

		  IPosition xzNonItgrlmatrix1(3,3,1,7);
		  LatticeStepper xzNonItgrlmatrix1stepper(al6.shape(),
							  xzNonItgrlmatrix1, 
							  orientation);
		  LatticeIterator<Int> nixziter(al6, xzNonItgrlmatrix1stepper);
		  for (;!nixziter.atEnd();nixziter++) {}
		  AlwaysAssert(nixziter.nsteps() == 96, AipsError);
		  test.resize(IPosition(2,3,7));
		  test.set(24);
		  test.row(2) = 0;
		  AlwaysAssert(allEQ(nixziter.matrixCursor(), test),
			       AipsError);

		  IPosition xzNonItgrlmatrix2(3,5,1,4);
		  LatticeStepper xzNonItgrlmatrix2stepper(al6.shape(),
							  xzNonItgrlmatrix2,
							  orientation);
		  LatticeIterator<Int> ni2xziter(al6,xzNonItgrlmatrix2stepper);
		  for (;!ni2xziter.atEnd();ni2xziter++) {}
		  AlwaysAssert(ni2xziter.nsteps() == 96, AipsError);
		  test.resize(IPosition(2,5,4));
		  test.set(24);
		  test.column(3) = 0;
		  AlwaysAssert(allEQ(ni2xziter.matrixCursor(), test),
			       AipsError);
		  
		  IPosition xzNonItgrlmatrix3(3,3,1,4);
		  LatticeStepper xzNonItgrlmatrix3stepper(al6.shape(),
							  xzNonItgrlmatrix3, 
							  orientation);
		  LatticeIterator<Int> ni3xziter(al6,xzNonItgrlmatrix3stepper);
		  for (;!ni3xziter.atEnd();ni3xziter++) {}
		  AlwaysAssert(ni3xziter.nsteps() == 192, AipsError);
		  test.resize(IPosition(2,3,4));
		  test.set(24);
		  test.row(2) = 0;
		  test.column(3) = 0;
		  AlwaysAssert(allEQ(ni3xziter.matrixCursor(), test),
			       AipsError);

		  IPosition xtNonItgrlmatrix1(4,3,1,1,8);
		  LatticeStepper xtNonItgrlmatrix1stepper(al6.shape(),
							  xtNonItgrlmatrix1, 
							  orientation);
		  LatticeIterator<Int> nixtiter(al6, xtNonItgrlmatrix1stepper);
		  for (;!nixtiter.atEnd();nixtiter++) {}
		  AlwaysAssert(nixtiter.nsteps() == 84, AipsError);
		  test.resize(IPosition(2,3,8));
		  test.set(24);
		  test.row(2) = 0;
		  AlwaysAssert(allEQ(nixtiter.matrixCursor(), test),
			       AipsError);
		  
		  IPosition xtNonItgrlmatrix2(4,5,1,1,5);
		  LatticeStepper xtNonItgrlmatrix2stepper(al6.shape(), 
							  xtNonItgrlmatrix2, 
							  orientation);
		  LatticeIterator<Int> ni2xtiter(al6,xtNonItgrlmatrix2stepper);
		  for (;!ni2xtiter.atEnd();ni2xtiter++) {}
		  AlwaysAssert(ni2xtiter.nsteps() == 84, AipsError);
		  test.resize(IPosition(2,5,5));
		  test.set(24);
		  test.column(3) = 0;
		  test.column(4) = 0;
		  AlwaysAssert(allEQ(ni2xtiter.matrixCursor(), test),
			       AipsError);

		  IPosition xtNonItgrlmatrix3(4,3,1,1,5);
		  LatticeStepper xtNonItgrlmatrix3stepper(al6.shape(),
							  xtNonItgrlmatrix3, 
							  orientation);
		  LatticeIterator<Int> ni3xtiter(al6,xtNonItgrlmatrix3stepper);
		  for (;!ni3xtiter.atEnd();ni3xtiter++) {}
		  AlwaysAssert(ni3xtiter.nsteps() == 168, AipsError);
		  test.resize(IPosition(2,3,5));
		  test.set(24);
		  test.row(2) = 0;
		  test.column(3) = 0;
		  test.column(4) = 0;
		  AlwaysAssert(allEQ(ni3xtiter.matrixCursor(), test),
			       AipsError);

		  IPosition yzNonItgrlmatrix1(3,1,4,7);
		  LatticeStepper yzNonItgrlmatrix1stepper(al6.shape(),
							  yzNonItgrlmatrix1, 
							  orientation);
		  LatticeIterator<Int> niyziter(al6, yzNonItgrlmatrix1stepper);
		  for (;!niyziter.atEnd();niyziter++) {}
		  AlwaysAssert(niyziter.nsteps() == 80, AipsError);
		  test.resize(IPosition(2,4,7));
		  test.set(24);
		  test.row(2) = 0;
		  test.row(3) = 0;
		  AlwaysAssert(allEQ(niyziter.matrixCursor(), test),
			       AipsError);

		  IPosition yzNonItgrlmatrix2(3,1,6,4);
		  LatticeStepper yzNonItgrlmatrix2stepper(al6.shape(),
							  yzNonItgrlmatrix2, 
							  orientation);
		  LatticeIterator<Int> ni2yziter(al6,yzNonItgrlmatrix2stepper);
		  for (;!ni2yziter.atEnd();ni2yziter++) {}
		  AlwaysAssert(ni2yziter.nsteps() == 80, AipsError);
		  test.resize(IPosition(2,6,4));
		  test.set(24);
		  test.column(3) = 0;
		  AlwaysAssert(allEQ(ni2yziter.matrixCursor(), test),
			       AipsError);

		  IPosition yzNonItgrlmatrix3(3,1,4,4);
		  LatticeStepper yzNonItgrlmatrix3stepper(al6.shape(), 
							  yzNonItgrlmatrix3, 
							  orientation);
		  LatticeIterator<Int> ni3yziter(al6,yzNonItgrlmatrix3stepper);
		  for (;!ni3yziter.atEnd();ni3yziter++) {}
		  AlwaysAssert(ni3yziter.nsteps() == 160, AipsError);
		  test.resize(IPosition(2,4,4));
		  test.set(24);
		  test.row(2) = 0;
		  test.row(3) = 0;
		  test.column(3) = 0;
		  AlwaysAssert(allEQ(ni3yziter.matrixCursor(), test),
			       AipsError);

		  IPosition ytNonItgrlmatrix1(4,1,4,1,8);
		  LatticeStepper ytNonItgrlmatrix1stepper(al6.shape(),
							  ytNonItgrlmatrix1, 
							  orientation);
		  LatticeIterator<Int> niytiter(al6, ytNonItgrlmatrix1stepper);
		  for (;!niytiter.atEnd();niytiter++) {}
		  AlwaysAssert(niytiter.nsteps() == 70, AipsError);
		  test.resize(IPosition(2,4,8));
		  test.set(24);
		  test.row(2) = 0;
		  test.row(3) = 0;
		  AlwaysAssert(allEQ(niytiter.matrixCursor(), test),
			       AipsError);

		  IPosition ytNonItgrlmatrix2(4,1,6,1,5);
		  LatticeStepper ytNonItgrlmatrix2stepper(al6.shape(), 
							  ytNonItgrlmatrix2, 
							  orientation);
		  LatticeIterator<Int> ni2ytiter(al6,ytNonItgrlmatrix2stepper);
		  for (;!ni2ytiter.atEnd();ni2ytiter++) {}
		  AlwaysAssert(ni2ytiter.nsteps() == 70, AipsError);
		  test.resize(IPosition(2,6,5));
		  test.set(24);
		  test.column(3) = 0;
		  test.column(4) = 0;
		  AlwaysAssert(allEQ(ni2ytiter.matrixCursor(), test),
			       AipsError);

		  IPosition ytNonItgrlmatrix3(4,1,4,1,5);
		  LatticeStepper ytNonItgrlmatrix3stepper(al6.shape(), 
							  ytNonItgrlmatrix3, 
							  orientation);
		  LatticeIterator<Int> ni3ytiter(al6,ytNonItgrlmatrix3stepper);
		  for (;!ni3ytiter.atEnd();ni3ytiter++) {}
		  AlwaysAssert(ni3ytiter.nsteps() == 140, AipsError);
		  test.resize(IPosition(2,4,5));
		  test.set(24);
		  test.row(2) = 0;
		  test.row(3) = 0;
		  test.column(3) = 0;
		  test.column(4) = 0;
		  AlwaysAssert(allEQ(ni3ytiter.matrixCursor(), test),
			       AipsError);

		  IPosition ztNonItgrlmatrix1(4,1,1,4,8);
		  LatticeStepper ztNonItgrlmatrix1stepper(al6.shape(),
							  ztNonItgrlmatrix1, 
							  orientation);
		  LatticeIterator<Int> niztiter(al6, ztNonItgrlmatrix1stepper);
		  for (;!niztiter.atEnd();niztiter++) {}
		  AlwaysAssert(niztiter.nsteps() == 60, AipsError);
		  test.resize(IPosition(2,4,8));
		  test.set(24);
		  test.row(3) = 0;
		  AlwaysAssert(allEQ(niztiter.matrixCursor(), test),
			       AipsError);

		  IPosition ztNonItgrlmatrix2(4,1,1,7,5);
		  LatticeStepper ztNonItgrlmatrix2stepper(al6.shape(), 
							  ztNonItgrlmatrix2, 
							  orientation);
		  LatticeIterator<Int> ni2ztiter(al6,ztNonItgrlmatrix2stepper);
		  for (;!ni2ztiter.atEnd();ni2ztiter++) {}
		  AlwaysAssert(ni2ztiter.nsteps() == 60, AipsError);
		  test.resize(IPosition(2,7,5));
		  test.set(24);
		  test.column(3) = 0;
		  test.column(4) = 0;
		  AlwaysAssert(allEQ(ni2ztiter.matrixCursor(), test),
			       AipsError);

		  IPosition ztNonItgrlmatrix3(4,1,1,4,5);
		  LatticeStepper ztNonItgrlmatrix3stepper(al6.shape(), 
							  ztNonItgrlmatrix3, 
							  orientation);
		  LatticeIterator<Int> ni3ztiter(al6,ztNonItgrlmatrix3stepper);
		  for (;!ni3ztiter.atEnd();ni3ztiter++) {}
		  AlwaysAssert(ni3ztiter.nsteps() == 120, AipsError);
		  test.resize(IPosition(2,4,5));
		  test.set(24);
		  test.row(3) = 0;
		  test.column(3) = 0;
		  test.column(4) = 0;
		  AlwaysAssert(allEQ(ni3ztiter.matrixCursor(), test),
			       AipsError);
		}
	      }
	    }
	  }
	}
      }
    }
    // Test of operator+, etc.
    {
      const IPosition latticeShape(4, 4, 16, 15, 8);
      ArrayLattice<Float> pa(latticeShape);
      Array<Float> arr(latticeShape);
      indgen(arr);
      pa.put (arr);
      AlwaysAssertExit (allEQ(pa.get(), arr));
      pa += pa;
      AlwaysAssertExit (allEQ(pa.get(), float(2)*arr));
      pa -= ArrayLattice<Float>(arr);
      AlwaysAssertExit (allEQ(pa.get(), arr));
    }
    // Test of copyData
    {
      const IPosition latticeShape(4, 4, 16, 15, 8);
      Array<Float> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Float> from(arr.copy());
      ArrayLattice<Float> to(latticeShape);
      to.copyData (from);
      AlwaysAssertExit (to.asArray()(IPosition(4,0,0,0,1)) == 960);
      AlwaysAssertExit (allEQ(arr, to.asArray()));
    }
  } catch (AipsError x) {
    cerr << x.getMesg () << endl;
    cout << "FAIL" << endl; 
    return 1;
  } 
  cout << "OK" << endl; 
  return 0;
}
