//# tArrayLattice.cc: test ArrayLattices and ArrayLatticeIterators.
//# Copyright (C) 1995, 1996
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

#include <trial/Lattices/ArrayLattice.h> 
#include <trial/Lattices/LatticeIterator.h> 
#include <trial/Lattices/LatticeStepper.h> 

#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/COWPtr.h>

#include <iostream.h>

Int const_arg_func(const Int &val)
{
  return 3*val;
};

Int func(Int val)
{
  return 2*val*val;
};

main()
{
  try{
    // make an array
    Array<Float> array1(IPosition(1,256));
    for(int i=0;i<256;i++) array1(IPosition(1,i)) = i;
      
    // make another array
    Array<Float> array2(IPosition(2,256,128));
    for(i=0;i<256;i++) 
      for(int j=0;j<128;j++) array2(IPosition(2,i,j)) = i+j;
      
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
    AlwaysAssert(al3(IPosition(1,0)) == 0, AipsError);
    al5(IPosition(1,0)) = 33.0;
    AlwaysAssert(al3(IPosition(1,0)) == 33.0, AipsError);

    // the assignment operator.  typical use would be to create a new
    // ArrayLattice that (at least initially) is a copy of another one:
    al0 = al4;

    // test reference nature
    AlwaysAssert(al0(IPosition(2,0)) == 0, AipsError);
    al0(IPosition(2,0)) = 33.0;
    AlwaysAssert(al4(IPosition(2,0)) == 33.0, AipsError);

    ArrayLattice<Int> al6(IPosition(4,5,6,7,8));
    // returns the shape of the ArrayLattice.
    AlwaysAssert(al6.shape() == IPosition(4,5,6,7,8), AipsError);
    AlwaysAssert(al4.shape() == IPosition(2,256,128), AipsError);
    
    // function which extracts an Array of values from a Lattice - a read-only 
    // operation. 
    // getSlice parameters:
    // <ul>
    // <li> buffer: an Array<T> with a shape that is unimportant.  The 
    //      sub-class implementation should always call Array::resize(uInt) 
    //      in order to match the Array<T> shape to the "shape" argument.
    // <li> start: an IPosition which must have the same number of axes
    //      as the underlying Lattice, otherwise, throw an exception.
    // <li> shape: an IPosition which must have equal or fewer axes than the 
    //      true shape od the Lattice, otherwise, throw an exception
    // <li> stride: an IPosition which must have the same number of axes
    //      as the underlying Lattice, otherwise, throw an exception.
    // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
    //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
    //      from an (n+1)-dimensional will fill 'buffer' with an array that 
    //      has a degenerate axis (i.e. one axis will have a length = 1.))
    // </ul>
    // 
    // The sub-class implementation of these functions return
    // 'True' if Array <T> buffer contains a reference when this function
    // returns, and 'False' if it contains a copy.
    // <note role=tip> 
    // In most cases, it will be more efficient in execution, if you
    // use a LatticeIterator class to move through the Lattice. 
    // LatticeIterators are optimized for that purpose.  If you are doing 
    // unsystematic traversal, or random gets and puts, then getSlice and 
    // putSlice or operator() may be the right tools to use.
    // </note>
    // <group>   
    COWPtr<Array<Float> > buffer1;
    IPosition start(2, 0, 0), shape(2, 128, 64), stride(2, 2, 2);
    AlwaysAssert(!al4.getSlice(buffer1, start, shape, stride), AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(2,0,0)) == 33, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(2,127,0)) == 254, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(2,0,63 )) == 126, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(2,127,63)) == 380, AipsError);

    COWPtr<Array<Float> > buffer2;
    Slicer theSlice(start, shape, stride);
    AlwaysAssert(!al4.getSlice(buffer2, theSlice), AipsError);
    AlwaysAssert(buffer2.ref()(IPosition(2,0,0)) == 33, AipsError);
    AlwaysAssert(buffer2.ref()(IPosition(2,127,0)) == 254, AipsError);
    AlwaysAssert(buffer2.ref()(IPosition(2,0,63 )) == 126, AipsError);
    AlwaysAssert(buffer2.ref()(IPosition(2,127,63)) == 380, AipsError);

    Array<Float> buffer3;
    AlwaysAssert(al0.getSlice(buffer3, start, shape, stride), AipsError);
    AlwaysAssert(buffer3(IPosition(2,0,0)) == 33, AipsError);
    AlwaysAssert(buffer3(IPosition(2,127,0)) == 254, AipsError);
    AlwaysAssert(buffer3(IPosition(2,0,63 )) == 126, AipsError);
    AlwaysAssert(buffer3(IPosition(2,127,63)) == 380, AipsError);
    
    Array<Float> buffer4;
    AlwaysAssert(al0.getSlice(buffer4, theSlice), AipsError);
    AlwaysAssert(buffer4(IPosition(2,0,0)) == 33, AipsError);
    AlwaysAssert(buffer4(IPosition(2,127,0)) == 254, AipsError);
    AlwaysAssert(buffer4(IPosition(2,0,63 )) == 126, AipsError);
    AlwaysAssert(buffer4(IPosition(2,127,63)) == 380, AipsError);

    // test reference nature of slicer
    buffer3.set(99.0);
    AlwaysAssert(al0(IPosition(2,0)) == 99.0, AipsError);   
    
    // put 'value' at every element of the ArrayLattice
    al6.set(42);
    
    // pick a couple of locations at random
    AlwaysAssert(al6(IPosition(4,3)) == 42, AipsError);
    AlwaysAssert(al6(IPosition(4,1,2,3,4)) == 42, AipsError);
    AlwaysAssert(al6(IPosition(4,4,5,6,7)) == 42, AipsError);
    
    Array<Int> sourceBuffer(IPosition(4,4));
    sourceBuffer = 6;
    // function which places an Array of values within the lattice
    al6.putSlice(sourceBuffer,IPosition(4,1,2,3,4), IPosition(4,1));
    
    // check the same spots again
    AlwaysAssert(al6(IPosition(4,3)) == 42, AipsError);
    AlwaysAssert(al6(IPosition(4,1,2,3,4)) == 6, AipsError);
    AlwaysAssert(al6(IPosition(4,4,5,6,7)) == 6, AipsError);
    
    // function which returns an Array of the data within this Lattice.
    // <group>
    AlwaysAssert(allEQ(al3.asArray(), array1), AipsError);
    array2(IPosition(2,0)) = 33.0;
    AlwaysAssert(allEQ(al4.asArray(), array2), AipsError);
    // </group>

    // a handy place to check for internal consistency
//    AlwaysAssert(al4.ok(), AipsError);
    
// -------------------inherited from Lattice-----------------------------

    // returns the value of the single element located at the argument 
    // IPosition.  The return type should be assumed to be of the template 
    // <class T>.  The actual return type (LatticeValueRef<T>) may be ignored.
    // For details, see "Advanced C++" by James O. Coplien, pp 49-52.
    al6(IPosition(4,0)) = 99;
    
    // returns the value of the single element located at the argument
    // IPosition.  
    AlwaysAssert(al6(IPosition(4,0)) == 99, AipsError);
    
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
    AlwaysAssert(al6(IPosition(4,4))==2, AipsError);
    AlwaysAssert(al6(IPosition(4,2,3,4,5))==2, AipsError);

    // func = arg*arg*2
    al6.apply(&func);
    // check a couple of random spots
    AlwaysAssert(al6(IPosition(4,4))==8, AipsError);
    AlwaysAssert(al6(IPosition(4,2,3,4,5))==8, AipsError);

    // const_arg_func = arg*3
    al6.apply(&const_arg_func);
    AlwaysAssert(al6(IPosition(4,4))==24, AipsError);
    AlwaysAssert(al6(IPosition(4,2,3,4,5))==24, AipsError);

    Polynomial<Float> poly(3);
    poly.setCoefficient(1, 0.5);
    poly.setCoefficient(2, 0.75);
    poly.setCoefficient(3, 1.0);

    al3.apply(poly);
    AlwaysAssert(al3(IPosition(1,0)) == poly(33), AipsError);
    AlwaysAssert(al3(IPosition(1,127)) == poly(127), AipsError);


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
    // </group>
    AlwaysAssert(al6ROIter.atStart(), AipsError);
    
    // Function which resets the cursor to the beginning of the Lattice 
    // (also sets the number of steps taken to zero.)
    al6ROIter++;
    al6ROIter.reset();
    AlwaysAssert(al6ROIter.atStart(), AipsError);

    // Function which returns a value of "True" if the cursor is at the end.
    for (int I=0;I<240;I++) al6ROIter++;
    AlwaysAssert(al6ROIter.atEnd(), AipsError);
  
    // Function which returns the number of steps taken since construction
    // or since reset().  <note> This is a running count 
    // of all cursor movement (operator++ or operator--) since doing x iter++
    // followed by x iter-- does not necessarily put the cursor back to the
    // origin of the Lattice </note> 
    AlwaysAssert(al6ROIter.nsteps() == 239, AipsError);
  
    // Function which returns the position of the beginning of the cursor 
    // within the lattice.
    al6ROIter.reset();
    AlwaysAssert(al6ROIter.position() == IPosition(4,0), AipsError);

    // Function which returns the end of the cursor (i.e. the cursor position
    // plus the cursor shape.)
    AlwaysAssert(al6ROIter.cursorEndPosition() == IPosition(4,0,0,6,0), 
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
    AlwaysAssert(zarray.ndim() == 1, AipsError); 
    AlwaysAssert(zarray.shape() == IPosition(1,7), AipsError); 

    // test functions which should throw exceptions
    Bool caught = False;
    try {
      al6ROIter.matrixCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);

    caught = False;
    try {
      al6ROIter.cubeCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);

    // check internals for sensibility
//    AlwaysAssert(al6ROIter.ok(), AipsError);

    IPosition xymatrix(2,5,6);
    LatticeStepper newMethod(al6.shape(), xymatrix);
    // Function which allows clients to replace a navigator, done by 
    // passing an object that derives from the LatticeNavigator.  
    al6ROIter.replaceNavigator(newMethod);
    AlwaysAssert(al6ROIter.cursorShape() == xymatrix, AipsError);
    AlwaysAssert(al6ROIter.position() == IPosition(4,0), AipsError);

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
    AlwaysAssert(xyarray.ndim() == 2, AipsError); 
    AlwaysAssert(xyarray.shape() == xymatrix, AipsError); 
    
    // test functions which should throw exceptions
    caught = False;
    try {
      al6Iter.vectorCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);

    caught = False;
    try {
      al6Iter.cubeCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
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
    // </group>
    AlwaysAssert(al6Iter.atStart(), AipsError);
    
    // Function which resets the cursor to the beginning of the Lattice 
    // (also sets the number of steps taken to zero.)
    al6Iter++;
    al6Iter.reset();
    AlwaysAssert(al6Iter.atStart(), AipsError);

    // Function which returns a value of "True" if the cursor is at the end.
    for (I=0;I<56;I++) al6Iter++;
    AlwaysAssert(al6Iter.atEnd(), AipsError);
  
    // Function which returns the number of steps taken since construction
    // or since reset().  <note> This is a running count 
    // of all cursor movement (operator++ or operator--) since doing x iter++
    // followed by x iter-- does not necessarily put the cursor back to the
    // origin of the Lattice </note> 
    AlwaysAssert(al6Iter.nsteps() == 55, AipsError);
  
    // Function which returns the position of the beginning of the cursor 
    // within the lattice.
    al6Iter.reset();
    AlwaysAssert(al6Iter.position() == IPosition(4,0), AipsError);

    // Function which returns the end of the cursor (i.e. the cursor position
    // plus the cursor shape.)
    AlwaysAssert(al6Iter.cursorEndPosition() == IPosition(4,4,5,0,0), 
		 AipsError);

    // Function which returns the shape of the Lattice being iterated through.
    AlwaysAssert(al6Iter.latticeShape() == al6.shape(), AipsError);

    // Function which returns the shape of the cursor as set by the 
    // LatticeNavigator method.
    AlwaysAssert(al6Iter.cursorShape() == xymatrix, AipsError);
/*
// -------------------- test Iterator very hard ---------------------
    IPosition orientation;
    int j, k, l;
    for(i=0;i<4;i++)
      for(j=0;j<4; j++)
	for(k=0; k<4; k++)
	  for(l=0; l<4; l++)
	    if (l!=k && l!=j && l!=i)
	      if(k!=j && k!=i)
		if(j!=i) {
		  orientation = IPosition(4,i,j,k,l);

// ------------------- integral shaped vectors ---------------------------

    IPosition xvector(1,5);
    LatticeStepper xvectorstepper(al6.shape(), xvector, orientation);
    al6Iter.replaceNavigator(xvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 335, AipsError);
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), 24), AipsError);

    IPosition yvector(2,1,6);
    LatticeStepper yvectorstepper(al6.shape(), yvector, orientation);
    al6Iter.replaceNavigator(yvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 279, AipsError);
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), 24), AipsError);

    LatticeStepper zvectorstepper(al6.shape(), zvector, orientation);
    al6Iter.replaceNavigator(zvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 239, AipsError);
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), 24), AipsError);

    IPosition tvector(4,1,1,1,8);
    LatticeStepper tvectorstepper(al6.shape(), tvector, orientation);
    al6Iter.replaceNavigator(tvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 209, AipsError);
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), 24), AipsError);

// ----------------------non integral vectors------------------------
// use the algorithm: shape = ceiling(axis length / 2)
    IPosition xnonIntgrlvector(1,3);
    LatticeStepper xnonIntgrlvectorstepper(al6.shape(), xnonIntgrlvector,
					   orientation);
    al6Iter.replaceNavigator(xnonIntgrlvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 671, AipsError);
    Vector<Int> tester(3);
    tester.set(24);
    tester(2) = 0;
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), tester), AipsError);

    IPosition ynonIntgrlvector(2,1,4);
    LatticeStepper ynonIntgrlvectorstepper(al6.shape(), ynonIntgrlvector,
					   orientation);
    al6Iter.replaceNavigator(ynonIntgrlvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 559, AipsError);
    tester.resize(4);
    tester.set(24);
    tester(2) = 0;
    tester(3) = 0;
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), tester), AipsError);

    IPosition znonIntgrlvector(3,1,1,4);
    LatticeStepper znonIntgrlvectorstepper(al6.shape(), znonIntgrlvector,
					   orientation);
    al6Iter.replaceNavigator(znonIntgrlvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 479, AipsError);
    tester(2) = 24;
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), tester), AipsError);

    IPosition tnonIntgrlvector(4,1,1,1,5);
    LatticeStepper tnonIntgrlvectorstepper(al6.shape(), tnonIntgrlvector,
					   orientation);
    al6Iter.replaceNavigator(tnonIntgrlvectorstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 419, AipsError);
    tester.resize(5);
    tester.set(24);
    tester(3) = 0;
    tester(4) = 0;
    AlwaysAssert(allEQ(al6Iter.vectorCursor(), tester), AipsError);    
   
// -------------------------integral matrices----------------------------

    LatticeStepper xymatrixstepper(al6.shape(), xymatrix, orientation);
    al6Iter.replaceNavigator(xymatrixstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 55, AipsError);
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), 24), AipsError);

    IPosition xzmatrix(3,5,1,7);
    LatticeStepper xzmatrixstepper(al6.shape(), xzmatrix, orientation);
    al6Iter.replaceNavigator(xzmatrixstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 47, AipsError);
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), 24), AipsError);

    IPosition xtmatrix(4,5,1,1,8);
    LatticeStepper xtmatrixstepper(al6.shape(), xtmatrix, orientation);
    al6Iter.replaceNavigator(xtmatrixstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 41, AipsError);
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), 24), AipsError);

    IPosition yzmatrix(3,1,6,7);
    LatticeStepper yzmatrixstepper(al6.shape(), yzmatrix, orientation);
    al6Iter.replaceNavigator(yzmatrixstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 39, AipsError);
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), 24), AipsError);

    IPosition ytmatrix(4,1,6,1,8);
    LatticeStepper ytmatrixstepper(al6.shape(), ytmatrix, orientation);
    al6Iter.replaceNavigator(ytmatrixstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 34, AipsError);
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), 24), AipsError);

    IPosition ztmatrix(4,1,1,7,8);
    LatticeStepper ztmatrixstepper(al6.shape(), ztmatrix, orientation);
    al6Iter.replaceNavigator(ztmatrixstepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 29, AipsError);
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), 24), AipsError);

// -----------------------non integral matrices----------------------------

    IPosition xyNonItgrlmatrix1(2,3,6);
    LatticeStepper xyNonItgrlmatrix1stepper(al6.shape(), xyNonItgrlmatrix1);
    al6Iter.replaceNavigator(xyNonItgrlmatrix1stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 111, AipsError);
    Matrix<Int> test(xyNonItgrlmatrix1);
    test.set(24);
    test.row(2) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xyNonItgrlmatrix2(2,5,4);
    LatticeStepper xyNonItgrlmatrix2stepper(al6.shape(), xyNonItgrlmatrix2, 
					    orientation);
    al6Iter.replaceNavigator(xyNonItgrlmatrix2stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 111, AipsError);
    test.resize(xyNonItgrlmatrix2);
    test.set(24);
    test.column(2) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xyNonItgrlmatrix3(2,3,4);
    LatticeStepper xyNonItgrlmatrix3stepper(al6.shape(), xyNonItgrlmatrix3,
					    orientation);
    al6Iter.replaceNavigator(xyNonItgrlmatrix3stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 223, AipsError);
    test.resize(xyNonItgrlmatrix3);
    test.set(24);
    test.row(2) = 0;
    test.column(2) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xzNonItgrlmatrix1(3,3,1,7);
    LatticeStepper xzNonItgrlmatrix1stepper(al6.shape(), xzNonItgrlmatrix1, 
					    orientation);
    al6Iter.replaceNavigator(xzNonItgrlmatrix1stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 95, AipsError);
    test.resize(IPosition(2,3,7));
    test.set(24);
    test.row(2) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xzNonItgrlmatrix2(3,5,1,4);
    LatticeStepper xzNonItgrlmatrix2stepper(al6.shape(), xzNonItgrlmatrix2,
					    orientation);
    al6Iter.replaceNavigator(xzNonItgrlmatrix2stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 95, AipsError);
    test.resize(IPosition(2,5,4));
    test.set(24);
    test.column(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xzNonItgrlmatrix3(3,3,1,4);
    LatticeStepper xzNonItgrlmatrix3stepper(al6.shape(), xzNonItgrlmatrix3, 
					    orientation);
    al6Iter.replaceNavigator(xzNonItgrlmatrix3stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 191, AipsError);
    test.resize(IPosition(2,3,4));
    test.set(24);
    test.row(2) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xtNonItgrlmatrix1(4,3,1,1,8);
    LatticeStepper xtNonItgrlmatrix1stepper(al6.shape(), xtNonItgrlmatrix1, 
					    orientation);
    al6Iter.replaceNavigator(xtNonItgrlmatrix1stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 83, AipsError);
    test.resize(IPosition(2,3,8));
    test.set(24);
    test.row(2) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xtNonItgrlmatrix2(4,5,1,1,5);
    LatticeStepper xtNonItgrlmatrix2stepper(al6.shape(), xtNonItgrlmatrix2, 
					    orientation);
    al6Iter.replaceNavigator(xtNonItgrlmatrix2stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 83, AipsError);
    test.resize(IPosition(2,5,5));
    test.set(24);
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition xtNonItgrlmatrix3(4,3,1,1,5);
    LatticeStepper xtNonItgrlmatrix3stepper(al6.shape(), xtNonItgrlmatrix3, 
					    orientation);
    al6Iter.replaceNavigator(xtNonItgrlmatrix3stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 167, AipsError);
    test.resize(IPosition(2,3,5));
    test.set(24);
    test.row(2) = 0;
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition yzNonItgrlmatrix1(3,1,4,7);
    LatticeStepper yzNonItgrlmatrix1stepper(al6.shape(), yzNonItgrlmatrix1, 
					    orientation);
    al6Iter.replaceNavigator(yzNonItgrlmatrix1stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 79, AipsError);
    test.resize(IPosition(2,4,7));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition yzNonItgrlmatrix2(3,1,6,4);
    LatticeStepper yzNonItgrlmatrix2stepper(al6.shape(), yzNonItgrlmatrix2, 
					    orientation);
    al6Iter.replaceNavigator(yzNonItgrlmatrix2stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 79, AipsError);
    test.resize(IPosition(2,6,4));
    test.set(24);
    test.column(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition yzNonItgrlmatrix3(3,1,4,4);
    LatticeStepper yzNonItgrlmatrix3stepper(al6.shape(), yzNonItgrlmatrix3, 
					    orientation);
    al6Iter.replaceNavigator(yzNonItgrlmatrix3stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 159, AipsError);
    test.resize(IPosition(2,4,4));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition ytNonItgrlmatrix1(4,1,4,1,8);
    LatticeStepper ytNonItgrlmatrix1stepper(al6.shape(), ytNonItgrlmatrix1, 
					    orientation);
    al6Iter.replaceNavigator(ytNonItgrlmatrix1stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 69, AipsError);
    test.resize(IPosition(2,4,8));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition ytNonItgrlmatrix2(4,1,6,1,5);
    LatticeStepper ytNonItgrlmatrix2stepper(al6.shape(), ytNonItgrlmatrix2, 
					    orientation);
    al6Iter.replaceNavigator(ytNonItgrlmatrix2stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 69, AipsError);
    test.resize(IPosition(2,6,5));
    test.set(24);
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition ytNonItgrlmatrix3(4,1,4,1,5);
    LatticeStepper ytNonItgrlmatrix3stepper(al6.shape(), ytNonItgrlmatrix3, 
					    orientation);
    al6Iter.replaceNavigator(ytNonItgrlmatrix3stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 139, AipsError);
    test.resize(IPosition(2,4,5));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition ztNonItgrlmatrix1(4,1,1,4,8);
    LatticeStepper ztNonItgrlmatrix1stepper(al6.shape(), ztNonItgrlmatrix1, 
					    orientation);
    al6Iter.replaceNavigator(ztNonItgrlmatrix1stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 59, AipsError);
    test.resize(IPosition(2,4,8));
    test.set(24);
    test.row(3) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition ztNonItgrlmatrix2(4,1,1,7,5);
    LatticeStepper ztNonItgrlmatrix2stepper(al6.shape(), ztNonItgrlmatrix2, 
					    orientation);
    al6Iter.replaceNavigator(ztNonItgrlmatrix2stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 59, AipsError);
    test.resize(IPosition(2,7,5));
    test.set(24);
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);

    IPosition ztNonItgrlmatrix3(4,1,1,4,5);
    LatticeStepper ztNonItgrlmatrix3stepper(al6.shape(), ztNonItgrlmatrix3, 
					    orientation);
    al6Iter.replaceNavigator(ztNonItgrlmatrix3stepper);
    for (;!al6Iter.atEnd();al6Iter++);
    AlwaysAssert(al6Iter.nsteps() == 119, AipsError);
    test.resize(IPosition(2,4,5));
    test.set(24);
    test.row(3) = 0;
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(al6Iter.matrixCursor(), test), AipsError);
  } */    
    cout << "OK" << endl;
  } catch (AipsError x) {
    cerr << x.getMesg () << endl;
  } end_try;
  
return 0;
};









    
    
