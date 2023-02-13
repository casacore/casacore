//# dCOWPtr.cc:  how to use COWPtr's (copy-on-write pointers)
//# Copyright (C) 1996,1999,2001
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

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/iostream.h> 

#include <casacore/casa/namespace.h>
int main()
{
  // Create an array of dimension 1, length 4 and initialize it.
  Array<Int> arr(IPosition(1,4));
  indgen(arr);

  // Create a second array referencing the first array
  // and put it in a COWPtr (which takes over the pointer).
  // Mark it as readonly, so a copy is made when non-const access is done.
  COWPtr<Array<Int> > arrptr(new Array<Int>(arr), True);
  cout << "The original array is: " << arr << endl;
  cout << "Array 1 is a reference to it: " << *arrptr << endl;

  // Change the original array, which will also change the array in the
  // COWPtr because it is a reference.
  arr(IPosition(1,0)) = 10;
  cout << "The first element of the array is modified: "
       << arr << endl;
  cout << "Accessing the array in the COWptr gives the same answer: " 
       << *arrptr << endl;

  // Change the array in the COWPtr which needs the rwRef() function to
  // get non-const access.
  // Because the COWPtr array was declared readonly, COWPtr makes a copy
  // of the array leaving the original array untouched.
  arrptr.rwRef().operator()(IPosition(1,1)) = 11;
  cout << "The array in the COWPtr has been copied and modified: "
       << *arrptr << endl;
  cout << "But the original array is unchanged: " 
       << arr << endl;
  cout << "as the COWptr has made a copy" << endl;
}
