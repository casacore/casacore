//# tPagedArray.cc:  test the PagedArray class
//# Copyright (C) 1994,1995,1998
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
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

#include <trial/Images/PagedImage.h>
#include <trial/ImgCrdSys/ImageCoordinate.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/String.h>

#include <stdlib.h>
#include <iostream.h>

Int const_arg_func(const Int &val)
{
  return 3*val;
};

Int func(Int val)
{
  return 2*val*val;
};

int main()
{
  try {

    //build an ImageCoordinate
    ImageCoordinate ImgCoord1;
    ImgCoord1.addAxis(LinearAxis(MeasuredValue::RADIO_VELOCITY, 26.00826, 
				 ReferenceValue(ReferenceValue::VELOCITY, 
						25.007),
				 0.045, 55.03));
    
    Table table1(SetupNewTable("tPagedImage_tmp.img1",
			       TableDesc("", TableDesc::Scratch), 
			       Table::New));

    { // use scoping to destruct

      IPosition map1shape(1,256);
      // construct a new PagedImage, with 'array' as contents, in
      // the given Table.
      PagedImage<Float> pi1(map1shape, ImgCoord1, table1, True);
      LatticeIterator<Float> pi1iter(pi1, pi1.shape());
      Vector<float> &cursor1(pi1iter.vectorCursor());
      for(int i=0;i<256;i++) cursor1(IPosition(1,i)) = i;
      pi1.mask().set(False);
      pi1.mask()(IPosition(1,128)) = True;
      
      // construct a new Image from an array and coordinate information. Table
      // will be stored in the named file. NO MASKING
      PagedImage<Float> pi2(map1shape, ImgCoord1, "tPagedImage_tmp.img2");
      LatticeIterator<Float> pi2iter(pi2, pi2.shape());
      Vector<float> &cursor2(pi2iter.vectorCursor());
      for(i=0;i<256;i++) cursor2(IPosition(1,i)) = i;
    }
    
    // reconstruct from a pre-existing PagedImage in the Table
    PagedImage<Float> pi3(table1);
    
    // reconstruct from a pre-existing PagedImage in the file 
    PagedImage<Float> pi4("tPagedImage_tmp.img2");
    
    // reconstruct from a pre-existing PagedImage in the file
    // and row number (defaults to row zero)
    PagedImage<Float> pi5("tPagedImage_tmp.img1");
    PagedImage<Float> pi6("tPagedImage_tmp.img2");

    // the copy constructor (reference semantics):  passing by value
    // doesn't make sense, because it would require the creation of a
    // temporary (but possibly huge) file on disk
    PagedImage<Float> pi7(pi5);
    
    // test reference nature
    AlwaysAssert(pi5(IPosition(1,0)) == 0, AipsError);
    pi7(IPosition(1,0)) = 33.0;
    AlwaysAssert(pi5(IPosition(1,0)) == 33.0, AipsError);

    // the assignment operator.
    pi7 = pi6;

    // test reference nature
    AlwaysAssert(pi6(IPosition(1,0)) == 0, AipsError);
    pi7(IPosition(1,0)) = 33.0;
    AlwaysAssert(pi6(IPosition(1,0)) == 33.0, AipsError);

    // we need to know a projection method - here it is Global Sinusoid
    ProjectedPosition::Type myMethod(ProjectedPosition::GLS);
    // we need to know what form the coordinates are in - here it is RA & Dec
    SkyPosition::Type myVectorType(SkyPosition::EQUATORIAL);
    // we need to know the epoch of the coordinates
    SkyPosition::Epoch myEpoch(SkyPosition::J2000);
    // we need to know the vector itself.
    Vector<Double> myCoords(2);
    myCoords(0) = 122.35;
    myCoords(1) = -33.7764;
    // we need to know the position of the observer.
    // Let's create an EarthPosition with full description of all parameters.
    // We need a type - GEOCENTRIC seems good.
    EarthPosition::Type theType(EarthPosition::GEOCENTRIC);
    // We need the time and date of the observation... 
    Double julianDate = 2449376.0;
    // and we can add on the UT.
    julianDate += 16.52/24.0;
    // we need the coordinates of our position.
    Vector<Double> ourPosition(3);
    // geocentric longitude (in degrees) goes in the first field of the vector.
    ourPosition(0) = 107.2334666;
    // geocentric latitude (in degrees) goes in the second field.
    ourPosition(1) = 34.1562394;
    // geocentric radius (in meters) goes in the last field.
    ourPosition(2) = 6372139.592;
    // then use these to build our EarthPosition. 
    EarthPosition myObs(theType, julianDate, ourPosition);
    // we need to know a rotation - here it is zero.
    Double myRot = 0;
    // we need to know where the spherical position is to be on our 2-d 
    // projection i.e. what pixel is associated with my object's position?
    Vector<Double> thePixel(2);
    thePixel(0) = 55.0;
    thePixel(1) = 526.3;
    // finally, we need to know the number of spherical units per integer on
    // our 2-d projection (i.e. binning per pixel).
    Vector<Double> theBinning(2);
    theBinning(0) = 3.04e-03;
    theBinning(1) = 3.6255e-03;
    // Now we can make fruit of our labor - the ProjectedPosition itself.
    ProjectedPosition myMapping(myMethod, myVectorType, myEpoch, myCoords,
				myObs, myRot, thePixel, theBinning);

    // we need to know what the units are of the measured value
    MeasuredValue::Type myValueUnit(MeasuredValue::RADIO_VELOCITY);
    // we need to know what the above units are in reference 
    // here it is the velocity of the earth.
    ReferenceValue myRefValue(ReferenceValue::VELOCITY, 9.56e+03);
    // we need to know the value of the measurement
    Double myValue(9.5688823e+03);
    // we need to know the binning per "pixel"
    Double myBin(5.63e-04);
    // we need to know the position on the "number line" of our value
    Double myValuePos(27.3);
    // Now we may construct the LinearAxis itself.
    LinearAxis myLinearAxis(myValueUnit,myValue,myRefValue,myBin,myValuePos);

    ImageCoordinate ImgCoord2;
    ImgCoord2.addAxis(myMapping);
    ImgCoord2.addAxis(myLinearAxis);
    ImgCoord2.addAxis(LinearAxis(MeasuredValue::TIME, 
				 200.01,
				 ReferenceValue(ReferenceValue::TIME, 0),
				 1.5, 4.0));
    // make another array
    IPosition map2shape(4,5,6,7,8);
      
    Table table3(SetupNewTable("tPagedImage_tmp.img3",
			       TableDesc("", TableDesc::Scratch), 
			       Table::New));

    PagedImage<Int> pi8(map2shape, ImgCoord2, table3, True);
    pi8.mask().set(False);
    {
      LatticeIterator<Int> foo(pi8, IPosition(2,5,6));
      Matrix<Int> &cursor(foo.matrixCursor());
      for (; !foo.atEnd();foo++)
	for(int i=0;i<5;i++) 
	  for(int j=0;j<6;j++) cursor(IPosition(2,i,j)) = i+j+foo.nsteps();
    }

    // rename the table to something else
    pi8.rename("tPagedImage_tmp.imgNew");

    // returns the current Table name
    AlwaysAssert(pi8.name() == String("tPagedImage_tmp.imgNew"), AipsError);

    // return the Table this instance is stored in.
    AlwaysAssert(pi8.table().tableName() == String("tPagedImage_tmp.imgNew"),
		 AipsError);
    
    // a default constructed PagedImage is automatically given a temporary
    // TableColumn row number.  You may change it here.
    pi8.changeRowNumber(6);
    
    // returns the current TableColumn row number 
    AlwaysAssert(pi8.rowNumber() == 0, AipsError);
    AlwaysAssert(pi6.rowNumber() == 0, AipsError);
    
    // returns the shape of the PagedImage.
    AlwaysAssert(pi8.shape() == map2shape, AipsError);
    AlwaysAssert(pi5.shape() == IPosition(1,256), AipsError);
    
    // function to set the shape of this instance.
    // !!!! should work when Table::removeColumn is implemented !!!!
//  IPosition zip(1,100);
//  pi5.resize(zip);
//  AlwaysAssert(pi5.shape() == zip, AipsError);

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
    COWPtr<Array<Int> > buffer1;
    IPosition start(4,0), shape(4,2,3,3,4), stride(4, 2);
    AlwaysAssert(!pi8.getSlice(buffer1, start, shape, stride), AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(4,0)) == 0, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(4,1,0,0,0)) == 2, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(4,0,2,0,0)) == 4, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(4,0,0,2,0)) == 4, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(4,0,0,0,3)) == 42, AipsError);
    AlwaysAssert(buffer1.ref()(IPosition(4,1,2,2,3)) == 52, AipsError);

    COWPtr<Array<Int> > buffer2;
    Slicer theSlice(start, shape, stride);
    AlwaysAssert(!pi8.getSlice(buffer2, theSlice), AipsError);
    AlwaysAssert(allEQ(buffer1.ref(), buffer2.ref()), AipsError);

    Array<Int> buffer3;
    AlwaysAssert(!pi8.getSlice(buffer3, start, shape, stride), AipsError);
    AlwaysAssert(allEQ(buffer1.ref(), buffer3), AipsError);
    
    Array<Int> buffer4;
    AlwaysAssert(!pi8.getSlice(buffer4, theSlice), AipsError);
    AlwaysAssert(allEQ(buffer1.ref(), buffer4), AipsError);
    
    // put 'value' at every element of the PagedImage
    pi8.set(42);
    
    // pick a couple of locations at random
    AlwaysAssert(pi8(IPosition(4,3)) == 42, AipsError);
    AlwaysAssert(pi8(IPosition(4,1,2,3,4)) == 42, AipsError);
    AlwaysAssert(pi8(IPosition(4,4,5,6,7)) == 42, AipsError);
    
    Array<Int> sourceBuffer(IPosition(4,4));
    sourceBuffer = 6;
    // function which places an Array of values within the lattice
    pi8.putSlice(sourceBuffer,IPosition(4,1,2,3,4), IPosition(4,1));
    
    // check the same spots again
    AlwaysAssert(pi8(IPosition(4,3)) == 42, AipsError);
    AlwaysAssert(pi8(IPosition(4,1,2,3,4)) == 6, AipsError);
    AlwaysAssert(pi8(IPosition(4,4,5,6,7)) == 6, AipsError);
    
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

    pi8.set(2);
    // check a couple of random spots
    AlwaysAssert(pi8(IPosition(4,4))==2, AipsError);
    AlwaysAssert(pi8(IPosition(4,2,3,4,5))==2, AipsError);

    // func = arg*arg*2
    pi8.apply(&func);
    // check a couple of random spots
    AlwaysAssert(pi8(IPosition(4,4))==8, AipsError);
    AlwaysAssert(pi8(IPosition(4,2,3,4,5))==8, AipsError);

    // const_arg_func = arg*3
    pi8.apply(&const_arg_func);
    AlwaysAssert(pi8(IPosition(4,4))==24, AipsError);
    AlwaysAssert(pi8(IPosition(4,2,3,4,5))==24, AipsError);

    Polynomial<Float> poly(3);
    poly.setCoefficient(1, 0.5);
    poly.setCoefficient(2, 0.75);
    poly.setCoefficient(3, 1.0);

    pi5.apply(poly);
    AlwaysAssert(pi5(IPosition(1,0)) == poly(33), AipsError);
    AlwaysAssert(pi5(IPosition(1,127)) == poly(127), AipsError);

    // return the whole mask Lattice to allow iteration or Lattice functions.
    // <group>    
    AlwaysAssert(pi8.mask()(IPosition(4,3)) == False, AipsError);
    AlwaysAssert(pi8.mask()(IPosition(4,1,2,3,4)) == False, AipsError);
    AlwaysAssert(pi8.mask()(IPosition(4,4,5,6,7)) == False, AipsError);
    pi8.mask().set(True);
    AlwaysAssert(pi8.mask()(IPosition(4,3)) == True, AipsError);
    AlwaysAssert(pi8.mask()(IPosition(4,1,2,3,4)) == True, AipsError);
    AlwaysAssert(pi8.mask()(IPosition(4,4,5,6,7)) == True, AipsError);
    // </group>
  
    // function to set the default value.  This value will be returned when 
    // access to a masked element is attempted.
    pi8.setDefaultValue(99);
    
    // function to return the value of the stored default value.
    AlwaysAssert(pi8.defaultValue()==99,AipsError);
    AlwaysAssert(pi8(IPosition(4,3)) == 99, AipsError);
    AlwaysAssert(pi8(IPosition(4,1,2,3,4)) == 99, AipsError);
    AlwaysAssert(pi8(IPosition(4,4,5,6,7)) == 99, AipsError);
    pi8.mask().set(False);
    
    // return the table holding the data
//    Table table();
    
    // a handy place to check for internal consistency
//    AlwaysAssert(pi6.ok(), AipsError);
    
// -------------------inherited from Lattice-----------------------------

    // returns the value of the single element located at the argument 
    // IPosition.  The return type should be assumed to be of the template 
    // <class T>.  The actual return type (LatticeValueRef<T>) may be ignored.
    // For details, see "Advanced C++" by James O. Coplien, pp 49-52.
    pi8(IPosition(4,0)) = 15;
    
    // returns the value of the single element located at the argument
    // IPosition.  
    AlwaysAssert(pi8(IPosition(4,0)) == 15, AipsError);
    
    // returns the number of axes in this Lattice.
    AlwaysAssert(pi8.ndim() == 4, AipsError);
    
    // returns the total number of elements in this Lattice.
    AlwaysAssert(pi8.nelements() == 1680, AipsError);
    
    // returns a value of "True" if this instance of Lattice and 'other' have 
    // the same shape, otherwise returns a value of "False".
    AlwaysAssert(pi7.conform(pi6), AipsError);

// ----------------------RO_LatticeIterator----------------------------------
    pi8.set(24);

    IPosition zvector(4,1,1,7,1);
    LatticeStepper method(pi8.shape(), zvector);
    // Lattice and LatticeNavigator constructor
    RO_LatticeIterator<Int> pi8ROIter(pi8, method);

    // LatticeNavigator default "BLC to TRC" constructor 
    RO_LatticeIterator<Float> pi5ROIter(pi5,IPosition(1,8));  

    // copy ctor (uses reference sematics)
    RO_LatticeIterator<Int> pi8ROItercopy(pi8ROIter);

    // destructor (cleans up dangling references)
    //virtual ~RO_LatticeIterator();

    // assignment operator (uses reference semantics)
    //RO_LatticeIterator<T> &operator=(const RO_LatticeIterator<T> &other);

    // Function which returns a value of "True" if the cursor is at the start.
    AlwaysAssert(pi8ROIter.atStart(), AipsError);
    
    // Increment operator - increment the cursor to the next position. 
    // <group>
    pi8ROIter++;
    ++pi8ROIter;
    // </group>
    AlwaysAssert(!pi8ROIter.atStart(), AipsError);
    
    // Decrement operator - decrement the cursor to the next position.
    // <group>
    pi8ROIter--;
    --pi8ROIter;
    // </group>
    AlwaysAssert(pi8ROIter.atStart(), AipsError);
    
    // Function which resets the cursor to the beginning of the Lattice 
    // (also sets the number of steps taken to zero.)
    pi8ROIter++;
    pi8ROIter.reset();
    AlwaysAssert(pi8ROIter.atStart(), AipsError);

    // Function which returns a value of "True" if the cursor is at the end.
    for (int I=0;I<240;I++) pi8ROIter++;
    AlwaysAssert(pi8ROIter.atEnd(), AipsError);
  
    // Function which returns the number of steps taken since construction
    // or since reset().  <note> This is a running count 
    // of all cursor movement (operator++ or operator--) since doing x iter++
    // followed by x iter-- does not necessarily put the cursor back to the
    // origin of the Lattice </note> 
    AlwaysAssert(pi8ROIter.nsteps() == 240, AipsError);
  
    // Function which returns the position of the beginning of the cursor 
    // within the lattice.
    pi8ROIter.reset();
    AlwaysAssert(pi8ROIter.position() == IPosition(4,0), AipsError);

    // Function which returns the end of the cursor (i.e. the cursor position
    // plus the cursor shape.)
    AlwaysAssert(pi8ROIter.cursorEndPosition() == IPosition(4,0,0,6,0), 
		 AipsError);

    // Function which returns the shape of the Lattice being iterated through.
    AlwaysAssert(pi8ROIter.latticeShape() == pi8.shape(), AipsError);

    // Function which returns the shape of the cursor as set by the 
    // LatticeNavigator method.
    AlwaysAssert(pi8ROIter.cursorShape() == zvector, AipsError);

    // Function which returns a reference to the data in the Lattice.  
    // <note> The cursor array may have fewer dimensions than the
    // Lattice. A call of the function whose return value is
    // inappropriate with reference to the cursor shape as defined by
    // the LatticeNavigator will throw an exception. </note> 
    const Vector<Int> &zvectdata(pi8ROIter.vectorCursor());
    AlwaysAssert(allEQ(zvectdata, 24), AipsError); 
    AlwaysAssert(zvectdata.ndim() == 1, AipsError); 
    AlwaysAssert(zvectdata.shape() == IPosition(1,7), AipsError); 

    const Array<Int> &zarray(pi8ROIter.cursor());
    AlwaysAssert(allEQ(zarray, 24), AipsError); 
    AlwaysAssert(zarray.ndim() == 1, AipsError); 
    AlwaysAssert(zarray.shape() == IPosition(1,7), AipsError); 

    // test functions which should throw exceptions
    Bool caught = False;
    try {
      pi8ROIter.matrixCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);

    caught = False;
    try {
      pi8ROIter.cubeCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);

    // check internals for sensibility
//    AlwaysAssert(pi8ROIter.ok(), AipsError);

    IPosition xymatrix(2,5,6);
    LatticeStepper newMethod(pi8.shape(), xymatrix);
    // Function which allows clients to replace a navigator, done by 
    // passing an object that derives from the LatticeNavigator.  
    pi8ROIter.replaceNavigator(newMethod);
    AlwaysAssert(pi8ROIter.cursorShape() == xymatrix, AipsError);
    AlwaysAssert(pi8ROIter.position() == IPosition(4,0), AipsError);

// -------------------Read&Write LatticeIterator--------------------

    // Lattice and LatticeNavigator ctor
    LatticeIterator<Int> pi8Iter(pi8, newMethod);
    
    // LatticeNavigator default "BLC to TRC" constructor
    LatticeIterator<Float> pi5Iter(pi5, IPosition(1,8));
    
    // copy ctor (uses reference sematics)
    LatticeIterator<Int> copypi8Iter(pi8Iter);
    
    // destructor (cleans up dangling references)
    //~LatticeIterator();
    
    // assignment operator (uses reference semantics)
    //LatticeIterator<T> &operator=(const LatticeIterator<T> &other);
    
    // Function which returns a reference to the data in the Lattice.  <note>
    // The cursor array may have fewer dimensions than the Lattice. A call of 
    // the function whose return value is inappropriate with reference to the 
    // cursor shape as defined by the LatticeNavigator will throw an 
    // exception</note>
    Matrix<Int> xymatdata(pi8Iter.matrixCursor());
    AlwaysAssert(allEQ(xymatdata, 24), AipsError); 
    AlwaysAssert(xymatdata.ndim() == 2, AipsError); 
    AlwaysAssert(xymatdata.shape() == xymatrix, AipsError); 
    
    Array<Int> xyarray(pi8Iter.cursor());
    AlwaysAssert(allEQ(xyarray, 24), AipsError); 
    AlwaysAssert(xyarray.ndim() == 2, AipsError); 
    AlwaysAssert(xyarray.shape() == xymatrix, AipsError); 
    
    // test functions which should throw exceptions
    caught = False;
    try {
      pi8Iter.vectorCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);

    caught = False;
    try {
      pi8Iter.cubeCursor();
    } catch (AipsError x) {
      caught = True;
    } end_try;
    AlwaysAssert(caught, AipsError);
    

// --------------------- inherited from RO_LatticeIterator -----------    

    // Function which returns a value of "True" if the cursor is at the start.
    AlwaysAssert(pi8Iter.atStart(), AipsError);
    
    // Increment operator - increment the cursor to the next position. 
    // <group>
    pi8Iter++;
    ++pi8Iter;
    // </group>
    AlwaysAssert(!pi8Iter.atStart(), AipsError);
    
    // Decrement operator - decrement the cursor to the next position.
    // <group>
    pi8Iter--;
    --pi8Iter;
    // </group>
    AlwaysAssert(pi8Iter.atStart(), AipsError);
    
    // Function which resets the cursor to the beginning of the Lattice 
    // (also sets the number of steps taken to zero.)
    pi8Iter++;
    pi8Iter.reset();
    AlwaysAssert(pi8Iter.atStart(), AipsError);

    // Function which returns a value of "True" if the cursor is at the end.
    for (I=0;I<56;I++) pi8Iter++;
    AlwaysAssert(pi8Iter.atEnd(), AipsError);
  
    // Function which returns the number of steps taken since construction
    // or since reset().  <note> This is a running count 
    // of all cursor movement (operator++ or operator--) since doing x iter++
    // followed by x iter-- does not necessarily put the cursor back to the
    // origin of the Lattice </note> 
    AlwaysAssert(pi8Iter.nsteps() == 56, AipsError);
  
    // Function which returns the position of the beginning of the cursor 
    // within the lattice.
    pi8Iter.reset();
    AlwaysAssert(pi8Iter.position() == IPosition(4,0), AipsError);

    // Function which returns the end of the cursor (i.e. the cursor position
    // plus the cursor shape.)
    AlwaysAssert(pi8Iter.cursorEndPosition() == IPosition(4,4,5,0,0), 
		 AipsError);

    // Function which returns the shape of the Lattice being iterated through.
    AlwaysAssert(pi8Iter.latticeShape() == pi8.shape(), AipsError);

    // Function which returns the shape of the cursor as set by the 
    // LatticeNavigator method.
    AlwaysAssert(pi8Iter.cursorShape() == xymatrix, AipsError);
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
    LatticeStepper xvectorstepper(pi8.shape(), xvector,	orientation);
    pi8Iter.replaceNavigator(xvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 336, AipsError);
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), 24), AipsError);

    IPosition yvector(2,1,6);
    LatticeStepper yvectorstepper(pi8.shape(), yvector, orientation);
    pi8Iter.replaceNavigator(yvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 280, AipsError);
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), 24), AipsError);

    LatticeStepper zvectorstepper(pi8.shape(), zvector, orientation);
    pi8Iter.replaceNavigator(zvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 240, AipsError);
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), 24), AipsError);

    IPosition tvector(4,1,1,1,8);
    LatticeStepper tvectorstepper(pi8.shape(), tvector, orientation);
    pi8Iter.replaceNavigator(tvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 210, AipsError);
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), 24), AipsError);

// ----------------------non integral vectors------------------------
// use the algorithm: shape = ceiling(axis length / 2)
    IPosition xnonIntgrlvector(1,3);
    LatticeStepper xnonIntgrlvectorstepper(pi8.shape(), xnonIntgrlvector,
					   orientation);
    pi8Iter.replaceNavigator(xnonIntgrlvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 672, AipsError);
    Vector<Int> tester(3);
    tester.set(24);
    tester(2) = 0;
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), tester), AipsError);

    IPosition ynonIntgrlvector(2,1,4);
    LatticeStepper ynonIntgrlvectorstepper(pi8.shape(), ynonIntgrlvector,
					   orientation);
    pi8Iter.replaceNavigator(ynonIntgrlvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 560, AipsError);
    tester.resize(4);
    tester.set(24);
    tester(2) = 0;
    tester(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), tester), AipsError);

    IPosition znonIntgrlvector(3,1,1,4);
    LatticeStepper znonIntgrlvectorstepper(pi8.shape(), znonIntgrlvector,
					   orientation);
    pi8Iter.replaceNavigator(znonIntgrlvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 480, AipsError);
    tester(2) = 24;
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), tester), AipsError);

    IPosition tnonIntgrlvector(4,1,1,1,5);
    LatticeStepper tnonIntgrlvectorstepper(pi8.shape(), tnonIntgrlvector,
					   orientation);
    pi8Iter.replaceNavigator(tnonIntgrlvectorstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 420, AipsError);
    tester.resize(5);
    tester.set(24);
    tester(3) = 0;
    tester(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.vectorCursor(), tester), AipsError);    
   
// -------------------------integral matrices----------------------------

    LatticeStepper xymatrixstepper(pi8.shape(), xymatrix, orientation);
    pi8Iter.replaceNavigator(xymatrixstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 56, AipsError);
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), 24), AipsError);

    IPosition xzmatrix(3,5,1,7);
    LatticeStepper xzmatrixstepper(pi8.shape(), xzmatrix, orientation);
    pi8Iter.replaceNavigator(xzmatrixstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 48, AipsError);
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), 24), AipsError);

    IPosition xtmatrix(4,5,1,1,8);
    LatticeStepper xtmatrixstepper(pi8.shape(), xtmatrix, orientation);
    pi8Iter.replaceNavigator(xtmatrixstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 42, AipsError);
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), 24), AipsError);

    IPosition yzmatrix(3,1,6,7);
    LatticeStepper yzmatrixstepper(pi8.shape(), yzmatrix, orientation);
    pi8Iter.replaceNavigator(yzmatrixstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 40, AipsError);
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), 24), AipsError);

    IPosition ytmatrix(4,1,6,1,8);
    LatticeStepper ytmatrixstepper(pi8.shape(), ytmatrix, orientation);
    pi8Iter.replaceNavigator(ytmatrixstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 35, AipsError);
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), 24), AipsError);

    IPosition ztmatrix(4,1,1,7,8);
    LatticeStepper ztmatrixstepper(pi8.shape(), ztmatrix, orientation);
    pi8Iter.replaceNavigator(ztmatrixstepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 30, AipsError);
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), 24), AipsError);

// -----------------------non integral matrices----------------------------

    IPosition xyNonItgrlmatrix1(2,3,6);
    LatticeStepper xyNonItgrlmatrix1stepper(pi8.shape(), xyNonItgrlmatrix1,
					    orientation);
    pi8Iter.replaceNavigator(xyNonItgrlmatrix1stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 112, AipsError);
    Matrix<Int> test(xyNonItgrlmatrix1);
    test.set(24);
    test.row(2) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);
    
    IPosition xyNonItgrlmatrix2(2,5,4);
    LatticeStepper xyNonItgrlmatrix2stepper(pi8.shape(), xyNonItgrlmatrix2,
					    orientation);
    pi8Iter.replaceNavigator(xyNonItgrlmatrix2stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 112, AipsError);
    test.resize(xyNonItgrlmatrix2);
    test.set(24);
    test.column(2) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xyNonItgrlmatrix3(2,3,4);
    LatticeStepper xyNonItgrlmatrix3stepper(pi8.shape(), xyNonItgrlmatrix3,
					    orientation);
    pi8Iter.replaceNavigator(xyNonItgrlmatrix3stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 224, AipsError);
    test.resize(xyNonItgrlmatrix3);
    test.set(24);
    test.row(2) = 0;
    test.column(2) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xzNonItgrlmatrix1(3,3,1,7);
    LatticeStepper xzNonItgrlmatrix1stepper(pi8.shape(), xzNonItgrlmatrix1,
					    orientation);
    pi8Iter.replaceNavigator(xzNonItgrlmatrix1stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 96, AipsError);
    test.resize(IPosition(2,3,7));
    test.set(24);
    test.row(2) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xzNonItgrlmatrix2(3,5,1,4);
    LatticeStepper xzNonItgrlmatrix2stepper(pi8.shape(), xzNonItgrlmatrix2,
					    orientation);
    pi8Iter.replaceNavigator(xzNonItgrlmatrix2stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 96, AipsError);
    test.resize(IPosition(2,5,4));
    test.set(24);
    test.column(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xzNonItgrlmatrix3(3,3,1,4);
    LatticeStepper xzNonItgrlmatrix3stepper(pi8.shape(), xzNonItgrlmatrix3,
					    orientation);
    pi8Iter.replaceNavigator(xzNonItgrlmatrix3stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 192, AipsError);
    test.resize(IPosition(2,3,4));
    test.set(24);
    test.row(2) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xtNonItgrlmatrix1(4,3,1,1,8);
    LatticeStepper xtNonItgrlmatrix1stepper(pi8.shape(), xtNonItgrlmatrix1,
					    orientation);
    pi8Iter.replaceNavigator(xtNonItgrlmatrix1stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 84, AipsError);
    test.resize(IPosition(2,3,8));
    test.set(24);
    test.row(2) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xtNonItgrlmatrix2(4,5,1,1,5);
    LatticeStepper xtNonItgrlmatrix2stepper(pi8.shape(), xtNonItgrlmatrix2,
					    orientation);
    pi8Iter.replaceNavigator(xtNonItgrlmatrix2stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 84, AipsError);
    test.resize(IPosition(2,5,5));
    test.set(24);
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition xtNonItgrlmatrix3(4,3,1,1,5);
    LatticeStepper xtNonItgrlmatrix3stepper(pi8.shape(), xtNonItgrlmatrix3,
					    orientation);
    pi8Iter.replaceNavigator(xtNonItgrlmatrix3stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 168, AipsError);
    test.resize(IPosition(2,3,5));
    test.set(24);
    test.row(2) = 0;
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition yzNonItgrlmatrix1(3,1,4,7);
    LatticeStepper yzNonItgrlmatrix1stepper(pi8.shape(), yzNonItgrlmatrix1,
					    orientation);
    pi8Iter.replaceNavigator(yzNonItgrlmatrix1stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 80, AipsError);
    test.resize(IPosition(2,4,7));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition yzNonItgrlmatrix2(3,1,6,4);
    LatticeStepper yzNonItgrlmatrix2stepper(pi8.shape(), yzNonItgrlmatrix2,
					    orientation);
    pi8Iter.replaceNavigator(yzNonItgrlmatrix2stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 80, AipsError);
    test.resize(IPosition(2,6,4));
    test.set(24);
    test.column(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition yzNonItgrlmatrix3(3,1,4,4);
    LatticeStepper yzNonItgrlmatrix3stepper(pi8.shape(), yzNonItgrlmatrix3,
					    orientation);
    pi8Iter.replaceNavigator(yzNonItgrlmatrix3stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 160, AipsError);
    test.resize(IPosition(2,4,4));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    test.column(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition ytNonItgrlmatrix1(4,1,4,1,8);
    LatticeStepper ytNonItgrlmatrix1stepper(pi8.shape(), ytNonItgrlmatrix1,
					    orientation);
    pi8Iter.replaceNavigator(ytNonItgrlmatrix1stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 70, AipsError);
    test.resize(IPosition(2,4,8));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition ytNonItgrlmatrix2(4,1,6,1,5);
    LatticeStepper ytNonItgrlmatrix2stepper(pi8.shape(), ytNonItgrlmatrix2,
					    orientation);
    pi8Iter.replaceNavigator(ytNonItgrlmatrix2stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 70, AipsError);
    test.resize(IPosition(2,6,5));
    test.set(24);
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition ytNonItgrlmatrix3(4,1,4,1,5);
    LatticeStepper ytNonItgrlmatrix3stepper(pi8.shape(), ytNonItgrlmatrix3,
					    orientation);
    pi8Iter.replaceNavigator(ytNonItgrlmatrix3stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 140, AipsError);
    test.resize(IPosition(2,4,5));
    test.set(24);
    test.row(2) = 0;
    test.row(3) = 0;
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition ztNonItgrlmatrix1(4,1,1,4,8);
    LatticeStepper ztNonItgrlmatrix1stepper(pi8.shape(), ztNonItgrlmatrix1,
					    orientation);
    pi8Iter.replaceNavigator(ztNonItgrlmatrix1stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 60, AipsError);
    test.resize(IPosition(2,4,8));
    test.set(24);
    test.row(3) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition ztNonItgrlmatrix2(4,1,1,7,5);
    LatticeStepper ztNonItgrlmatrix2stepper(pi8.shape(), ztNonItgrlmatrix2,
					    orientation);
    pi8Iter.replaceNavigator(ztNonItgrlmatrix2stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 60, AipsError);
    test.resize(IPosition(2,7,5));
    test.set(24);
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);

    IPosition ztNonItgrlmatrix3(4,1,1,4,5);
    LatticeStepper ztNonItgrlmatrix3stepper(pi8.shape(), ztNonItgrlmatrix3,
					    orientation);
    pi8Iter.replaceNavigator(ztNonItgrlmatrix3stepper);
    for (;!pi8Iter.atEnd();pi8Iter++);
    AlwaysAssert(pi8Iter.nsteps() == 120, AipsError);
    test.resize(IPosition(2,4,5));
    test.set(24);
    test.row(3) = 0;
    test.column(3) = 0;
    test.column(4) = 0;
    AlwaysAssert(allEQ(pi8Iter.matrixCursor(), test), AipsError);
  }
*/
    cout<< "OK"<< endl;
  } catch (AipsError x) {
    cerr << "Exception caught: " << x.getMesg() << endl;
  } end_try;

  return 0;
}
