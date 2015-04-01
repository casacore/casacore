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
//#
//# $Id$

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/iostream.h> 

#include <casacore/casa/namespace.h>
int main()
{
  // create an array of dimension 1, length 4
  Array<Int> arr(IPosition(1,4));

  // create a second array from the first array.  note that neither
  // array has any values yet assigned to them
  Array<Int> arr1(arr); 

  // assign the elements of the first array
  indgen(arr);

  Bool deleteIt = False;
  Bool readOnly = True;

  COWPtr<Array<Int> > arrptr(&arr1, deleteIt, readOnly);

  //COWPtr< Array<Int> > arrptr(&arr1, False, True); 
  // The COWptr does not have exclusive control of arr1 as I will also
  // access it through normal array functions. It is Readonly so that when
  // I modify it, it is forced to make a copy. Otherwise it will only make
  // a copy when its internal number of references (increased using the
  // assignment operator and copy constructor) is greater than one and a
  // write operation is performed. However the COWptr does not not know
  // how many external references (through normal array functions) there
  // are so it is advisable to always make a copy if other methods of
  // accessing the data are used. If COWptr functions were the only
  // mechanism for accessing the data then I would not need to make the
  // data Readonly.
  cout << "The original array is:" << arr << endl;
  cout << "Array 1 is a reference to it:" << arr1 << endl;
  arr1(IPosition(1,0)) = 10;
  cout << "Modifying array 1 modifies the original array also:" 
       << arr1<< endl;
  cout << "Or accessing it via the COWptr gives the same answer:" 
       << *arrptr << endl;

  arrptr.rwRef().operator()(IPosition(1,1)) = 11;
  // I need the rwRef function as both the operator-> and operator*
  // functions return constant references and I need a non-const one to be
  // able to modify the data
  cout << "Array 1 has been modified using COWptr functions:" 
       << *arrptr << endl;
  cout << "But the normally accessed Array 1 is unchanged:"
       << arr1 << endl;
  cout << "As is the original array:" 
       << arr << endl;
  cout << "as the COWptr has made a copy" << endl;
}
