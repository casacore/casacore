//# tArrayUtilPerf.cc: Test program for performance of functions in ArrayUtil.h
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002
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
//# $Id: tArrayUtil.cc 21285 2012-11-14 15:36:59Z gervandiepen $

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// Test performance of some functions in ArrayUtil.h.

void testReorderArray()
{
  {
    cout << "arrayReorder 10 timings on [30,40,50,60] ..." << endl;
    IPosition shape(4,30,40,50,60);
    Array<Int> arr(shape);
    indgen(arr);
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reorderArray (arr, IPosition(1,0));
      }
      tim.show ("0,1,2,3");
    }
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reorderArray (arr, IPosition(4,0,1,3,2));
      }
      tim.show ("0,1,3,2");
    }
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reorderArray (arr, IPosition(4,0,2,1,3));
      }
      tim.show ("0,2,1,3");
    }
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reorderArray (arr, IPosition(4,1,3,2,0));
      }
      tim.show ("1,3,2,0");
    }
  }
}

void testReverseArray()
{
  cout << "reverseArray..." << endl;
  {
    cout << "arrayReverse 10 timings on [30,40,50,60] ..." << endl;
    IPosition shape(4,30,40,50,60);
    Array<Int> arr(shape);
    indgen(arr);
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reverseArray (arr, 0);
      }
      tim.show ("0");
    }
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reverseArray (arr, 1);
      }
      tim.show ("1");
    }
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reverseArray (arr, 2);
      }
      tim.show ("2");
    }
    {
      Timer tim;
      for (Int i=0; i<10; i++) {
	reverseArray (arr, 3);
      }
      tim.show ("3");
    }
  }
}

int main()
{
  try {
    testReorderArray();
    testReverseArray();
  } catch (const AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;               // successfully executed
}
