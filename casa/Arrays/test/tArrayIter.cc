//# tArrayIter.cc: This program tests Array iteration
//# Copyright (C) 1993,1994,1995,1999,2001
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

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main()
{
  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  0 dim. ......\n";
    IPosition shape(2);
    shape(0) = shape(1) = 5;
    ArrayPositionIterator ai (shape, 0);
    IPosition index (2);
    Int iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] \n";
    }
    cout << "END.  Testing ArrayPositionIterator.  0 dim. ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  1 dim. ......\n";
    IPosition shape(2);
    shape(0) = shape(1) = 5;
    ArrayPositionIterator ai (shape, 1);
    Array<Int> arr (shape);

    IPosition index (2);
    Int iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        arr (index) = 4 * iloop;
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] "
             << arr (index) << "\n";
    }
    ai.set (IPosition(1,4));
    AlwaysAssertExit (ai.pos() == IPosition(2,0,4));
    ai.set (IPosition(2,2,3));
    AlwaysAssertExit (ai.pos() == IPosition(2,0,3));
    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        cout << ai.pos() << endl;
    }
    cout << "END.  Testing ArrayPositionIterator.  1 dim. ......\n";

    cout << "\nBEGIN.  Testing ArrayIterator.  1 dim. ......\n";
    ArrayIterator<Int> arri (arr, 1);
    IPosition arriindex (1, 0);
    for ( iloop = 0; ! arri.pastEnd(); arri.next(), iloop++ ) {
        cout << iloop << "  " << (arri.array ()) (arriindex) << "\n";
    }
    cout << "END.  Testing ArrayIterator.  1 dim. ......\n";
  }

  {   
    cout << "\nBEGIN.  Testing double ArrayPositionIterator.  1 dim. ......\n";
    IPosition shape(2);
    shape(0) = shape(1) = 5;
    ArrayPositionIterator ai (shape, 1);
    Array<double> arr (shape);

    IPosition index (2);
    Int iloop;
    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        arr (index) = double (4 * iloop) + 0.5;
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] "
             << arr (index) << "\n";
    }
    cout << "END.  Testing double ArrayPositionIterator.  1 dim. ......\n";

    cout << "\nBEGIN.  Testing double ArrayIterator.  1 dim. ......\n";
    ArrayIterator<double> arri (arr, 1);
    IPosition arriindex (1, 0);
    for ( iloop = 0; ! arri.pastEnd(); arri.next(), iloop++ ) {
        cout << iloop << "  " << (arri.array ()) (arriindex) << "\n";
    }
    cout << "END.  Testing double ArrayIterator.  1 dim. ......\n";
  }

  {
    cout << "\nBEGIN.  Testing int ArrayIterator.  5 dim. ......\n";
    // Test that leading degenerate axes do not go away.
    // Check if each chunk matches.
    IPosition shape(5, 2,3,4,5,6);
    Array<Int> ai(shape);
    indgen(ai);
    {
      // Test a regular iterator.
      ArrayIterator<Int> iter(ai, 2);
      AlwaysAssertExit(iter.array().shape() == IPosition(2,shape(0),shape(1)));
      while (!iter.pastEnd()) {
	Array<Int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
	AlwaysAssertExit(iter.array().data() == tmparr.data());
	AlwaysAssertExit(allEQ (iter.array(), tmparr));
	iter.next();
      }
    }
    {
      // Test that a cursor as large as the array works
      ArrayIterator<Int> aiter(ai, 5);
      AlwaysAssertExit(allEQ(aiter.array(), ai));
      aiter.next();
      AlwaysAssertExit(aiter.pastEnd());
    }
    {
      // Test iterator with arbitrary axes.
      ReadOnlyArrayIterator<Int> iter(ai, IPosition(3,4,1,3), False);
      AlwaysAssertExit(iter.array().shape() == IPosition(2,shape(0),shape(2)));
      while (!iter.pastEnd()) {
	Array<Int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
	AlwaysAssertExit(allEQ (iter.array(), tmparr));
	iter.next();
      }
      iter.reset();
      // Check if iteration is in correct order.
      IPosition blc(5,0);
      IPosition trc(shape-1);
      for (Int ax3=0; ax3<shape(3); ++ax3) {
	blc(3) = trc(3) = ax3;
	for (Int ax1=0; ax1<shape(1); ++ax1) {
	  blc(1) = trc(1) = ax1;
	  for (Int ax4=0; ax4<shape(4); ++ax4) {
	    blc(4) = trc(4) = ax4;
	    AlwaysAssertExit(!iter.pastEnd());
	    AlwaysAssertExit(blc==iter.pos()  &&  trc==iter.endPos());
	    iter.next();
	  }
	}
      }
      AlwaysAssertExit(iter.pastEnd());
    }
    cout << "END.  Testing int ArrayIterator.  5 dim. ......\n";
  }
  {
    cout << "\nBEGIN.  Testing int ArrayIterator part.  5 dim. ......\n";
    // Test iterator with arbitrary axes on a part of an array.
    // Test that leading degenerate axes do not go away.
    // Check if each chunk matches.
    IPosition shape1(5, 10,20,16,20,15);
    Array<Int> ai1(shape1);
    indgen(ai1);
    // Take a chunk from it.
    Array<Int> ai(ai1(IPosition(5,1,2,1,4,3), IPosition(5,7,12,13,16,13),
		      IPosition(5,6,5,4,3,2)));
    IPosition shape(5, 2,3,4,5,6);
    AlwaysAssertExit (ai.shape() == shape);
    {
      // Test a regular iterator.
      ArrayIterator<Int> iter(ai, 2);
      AlwaysAssertExit(iter.array().shape() == IPosition(2,shape(0),shape(1)));
      while (!iter.pastEnd()) {
	Array<Int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
	AlwaysAssertExit(iter.array().data() == tmparr.data());
	AlwaysAssertExit(allEQ (iter.array(), tmparr));
	iter.next();
      }
    }
    {
      // Test that a cursor as large as the array works
      ArrayIterator<Int> aiter(ai, 5);
      AlwaysAssertExit(allEQ(aiter.array(), ai));
      aiter.next();
      AlwaysAssertExit(aiter.pastEnd());
    }
    {
      // Test iterator with arbitrary axes.
      ReadOnlyArrayIterator<Int> iter(ai, IPosition(3,4,1,3), False);
      AlwaysAssertExit(iter.array().shape() == IPosition(2,shape(0),shape(2)));
      while (!iter.pastEnd()) {
	Array<Int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
	AlwaysAssertExit(allEQ (iter.array(), tmparr));
	iter.next();
      }
      iter.reset();
      // Check if iteration is in correct order.
      IPosition blc(5,0);
      IPosition trc(shape-1);
      for (Int ax3=0; ax3<shape(3); ++ax3) {
	blc(3) = trc(3) = ax3;
	for (Int ax1=0; ax1<shape(1); ++ax1) {
	  blc(1) = trc(1) = ax1;
	  for (Int ax4=0; ax4<shape(4); ++ax4) {
	    blc(4) = trc(4) = ax4;
	    AlwaysAssertExit(!iter.pastEnd());
	    AlwaysAssertExit(blc==iter.pos()  &&  trc==iter.endPos());
	    iter.next();
	  }
	}
      }
      AlwaysAssertExit(iter.pastEnd());
      // Check if set works correctly.
      iter.set (IPosition(3,3,1,2));
      AlwaysAssertExit(iter.pos() == IPosition(5,0,1,0,2,3));
      Array<Int> tmparr1 (ai(iter.pos(), iter.endPos()).nonDegenerate());
      AlwaysAssertExit(allEQ (iter.array(), tmparr1));
      iter.next();
      AlwaysAssertExit(iter.pos() == IPosition(5,0,1,0,2,4));
      Array<Int> tmparr2 (ai(iter.pos(), iter.endPos()).nonDegenerate());
      AlwaysAssertExit(allEQ (iter.array(), tmparr2));
    }
    {
      ReadOnlyArrayIterator<Int> iter(ai, IPosition(3,4,1,3));
      AlwaysAssertExit(iter.array().shape() == IPosition(3,shape(1),shape(3),
							 shape(4)));
      while (!iter.pastEnd()) {
	Array<Int> tmparr (ai(iter.pos(), iter.endPos()).nonDegenerate());
	AlwaysAssertExit(allEQ (iter.array(), tmparr));
	iter.next();
      }
      iter.reset();
      // Check if iteration is in correct order.
      IPosition blc(5,0);
      IPosition trc(shape-1);
      for (Int ax2=0; ax2<shape(2); ++ax2) {
	blc(2) = trc(2) = ax2;
	for (Int ax0=0; ax0<shape(0); ++ax0) {
	  blc(0) = trc(0) = ax0;
	  AlwaysAssertExit(!iter.pastEnd());
	  AlwaysAssertExit(blc==iter.pos()  &&  trc==iter.endPos());
	  iter.next();
	}
      }
      AlwaysAssertExit(iter.pastEnd());
    }
    cout << "END.  Testing int ArrayIterator part.  5 dim. ......\n";
  }
  // Test iterating through an empty array.
  {
    Vector<Int> vec(0);
    {
      Array<Int> arr(IPosition(2,0,5));
      ArrayIterator<Int> iter(arr, 1);
      int nstep=0;
      while (!iter.pastEnd()) {
	Array<Int>& darr = iter.array();
	darr = vec;
	iter.next();
	nstep++;
      }
      AlwaysAssertExit (nstep==5);
    }
    {
      Array<Int> arr(IPosition(2,5,0));
      ArrayIterator<Int> iter(arr, 1);
      int nstep=0;
      while (!iter.pastEnd()) {
	Array<Int>& darr = iter.array();
	darr = vec;
	iter.next();
	nstep++;
      }
      AlwaysAssertExit (nstep==0);
    }
    {
      Array<Int> arr(IPosition(1,2));
      ArrayIterator<Int> iter(arr, 1);
      int nstep=0;
      while (!iter.pastEnd()) {
	Array<Int>& darr = iter.array();
        AlwaysAssertExit (darr.shape() == IPosition(1,2));
	iter.next();
	nstep++;
      }
      AlwaysAssertExit (nstep==1);
    }
    {
      Array<Int> arr(IPosition(1,0));
      ArrayIterator<Int> iter(arr, 1);
      int nstep=0;
      while (!iter.pastEnd()) {
	iter.next();
	nstep++;
      }
      AlwaysAssertExit (nstep==0);
    }
  }
  {
    // Test the virtual iteration function.
    cout << "\nBEGIN.  Testing makeIterator.  1 dim. ......\n";
    Array<Int> arr(IPosition(2,4,5));
    indgen (arr);
    CountedPtr<ArrayPositionIterator> iter = arr.makeIterator(1);
    while (!iter->pastEnd()) {
      ArrayBase& subarrb = iter->getArray();
      Array<Int>& subarr = dynamic_cast<Array<Int>&>(subarrb);
      cout << subarr << endl;
      iter->next();
    }
    cout << "END.  Testing makeIterator.  1 dim. ......\n";
  }

  return 0;
}
