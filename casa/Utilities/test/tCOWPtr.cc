//# tCOWPtr.cc: This program tests the Copy-On-Write-Pointer class
//# Copyright (C) 1995,1996,1997,1999,2000,2001
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

#include <casacore/casa/Utilities/COWPtr.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
static Bool testFunc(Array<Float> *ptr, const Array<Float> &array, 
		     Bool deleteIt, Bool constant) 
{
  COWPtr<Array<float> > COW(ptr, deleteIt, constant);

  // only const T functions may be used through the pointer. 
  AlwaysAssert(COW->nelements() == 128, AipsError);

  // only const T functions may be used through the dereferenced object.
  AlwaysAssert(allEQ(*COW, array), AipsError);

  // return a reference to this instance.
  AlwaysAssert(allEQ(COW.ref(), array),AipsError);
  
  // fill this instance. The pointer must be dynamically allocated. Default 
  // behavior is to delete the pointer when this instance's destructer is 
  // called.  "deleteIt = False" implies the pointer is being maintained by 
  // another object,(i.e. this is a copy - do not delete.) The
  // Boolean "readOnly" argument forces the COWPtr to treat the templated
  // data as const.  This allows non-const data operations be used to fill a
  // const acting version of COWPtr.  
  // note:  this deletes the old ptr and resets it.
  Array<Float> *foobar = new Array<float>(IPosition(2,5,5));
  *foobar = 0.0;
  COW.set(foobar, deleteIt, constant);

  // return a readable and writable reference to this instance.
  COW.rwRef().set(22.0);
  AlwaysAssert(allEQ(COW.rwRef(), 22.0f), AipsError);
  
  // returns False if this contains a non-null ptr. Otherwise, True.
  AlwaysAssert(COW.isNull() == False, AipsError);
    
  // make this a copy if more than one exist.
  if (COW.isReadOnly()){
    AlwaysAssert(COW.makeUnique(), AipsError); 
  } else {
    AlwaysAssert(!COW.makeUnique(), AipsError);
  }

  // assignment operator with reference semantics
  Array<Float> *fooAlso = new Array<Float>(array); 
  COW = COWPtr<Array<Float> >(fooAlso, deleteIt, constant);
  AlwaysAssert(allEQ(*COW, array), AipsError);

  //-------------------- test default ctor---------------------------

  // default ctor
  COWPtr<Array<float> > deflt;

  // returns False if this contains a non-null ptr. Otherwise, True.
  AlwaysAssert(deflt.isNull() == True, AipsError);
    
  // assignment operator with reference semantics
  Array<Float> *fooAgain = new Array<Float>(array);
  deflt = COWPtr<Array<Float> >(fooAgain, deleteIt, constant);

  // only const T functions may be used through the pointer. 
  AlwaysAssert(deflt->nelements() == 128, AipsError);

  // only const T functions may be used through the dereferenced object.
  AlwaysAssert(allEQ(*deflt, array),AipsError);

  // return a reference to this instance.
  AlwaysAssert(allEQ(deflt.ref(), array),AipsError);
  
  // fill this instance. The pointer must be dynamically allocated. Default 
  // behavior is to delete the pointer when this instance's destructer is 
  // called.  "deleteIt = False" implies the pointer is being maintained by 
  // another object,(i.e. this is a copy - do not delete.) The
  // Boolean "readOnly" argument forces the COWPtr to treat the templated
  // data as const.  This allows non-const data operations be used to fill a
  // const acting version of COWPtr.  
  Array<float> *bar = new Array<float>(IPosition(2,5,5));
  *bar = 0.0;
  deflt.set(bar, deleteIt, constant);

  // return a readable and writable reference to this instance.
  deflt.rwRef().set(22.0);
  AlwaysAssert(allEQ(deflt.rwRef(), 22.0f), AipsError);
  
  // make this a copy if more than one exist.
  if (deflt.isReadOnly()){
    AlwaysAssert(deflt.makeUnique(), AipsError); 
  } else {
    AlwaysAssert(!deflt.makeUnique(), AipsError);
  }

  // ------------------test copy ctor-----------------------
    
  // copy ctor with reference semantics
  COWPtr<Array<Float> > copy(COW);

  // only const T functions may be used through the pointer. 
  AlwaysAssert(copy->nelements() == 128, AipsError);

  // only const T functions may be used through the dereferenced object.
  AlwaysAssert(allEQ(*copy, array),AipsError);

  // return a reference to this instance.
  AlwaysAssert(allEQ(copy.ref(), array),AipsError);
  
  // fill this instance. The pointer must be dynamically allocated. Default 
  // behavior is to delete the pointer when this instance's destructer is 
  // called.  "deleteIt = False" implies the pointer is being maintained by 
  // another object,(i.e. this is a copy - do not delete.) The
  // Boolean "readOnly" argument forces the COWPtr to treat the templated
  // data as const.  This allows non-const data operations be used to fill a
  // const acting version of COWPtr.  
  Array<float> *foo = new Array<float>(IPosition(2,5,5));
  *foo = 0.0;
  copy.set(foo, deleteIt, constant);

  // return a readable and writable reference to this instance.
  copy.rwRef().set(22.0);
  AlwaysAssert(allEQ(copy.rwRef(), 22.0f), AipsError);
  
  // returns False if this contains a non-null ptr. Otherwise, True.
  AlwaysAssert(copy.isNull() == False, AipsError);
    
  // make this a copy if more than one exist.
  if (copy.isReadOnly()){
    AlwaysAssert(copy.makeUnique(), AipsError);
  } else {
    AlwaysAssert(!copy.makeUnique(), AipsError);
  }

  //assignment
  copy = COW;
  AlwaysAssert(allEQ(*copy, array), AipsError);

  // test setReadOnly
  if (!deleteIt) {
    copy.setReadOnly(foo);
    AlwaysAssert(allEQ(*copy, *foo), AipsError);
    AlwaysAssert(copy.isReadOnly(), AipsError);
  }

  if (!deleteIt) {
    delete foo;
    delete bar;
    delete foobar;
    delete fooAlso;
    delete fooAgain;
  }

  return True;
}

int main()
{
  try {

    Array<float> array(IPosition(3,2,4,16));
    for (int i=0; i<2; i++)
      for (int j=0; j<4; j++)
	for (int k=0; k<16; k++) array(IPosition(3,i,j,k)) = i+j+k;

    // we have four permutations

    // Case 0: a const which controls the ptr.
    Array<Float> *ptr = new Array<float>(array.copy());
    AlwaysAssert(ptr, AipsError); 
    AlwaysAssert(testFunc(ptr, array, True, True), AipsError);
// the following can be uncommented when the CountedPtr class
// is made to set the deleted pointer to NULL.
//    AlwaysAssert(!ptr, AipsError); 

    // Case 1: a non-const which controls the ptr.
    ptr = new Array<float>(array.copy());
    AlwaysAssert(ptr, AipsError); 
    AlwaysAssert(testFunc(ptr, array, True, False), AipsError);
// the following can be uncommented when the CountedPtr class
// is made to set the deleted pointer to NULL.
//    AlwaysAssert(!ptr, AipsError); 

    // Case 2: a const which doesn't control the pointer
    ptr = new Array<float>(array.copy());
    AlwaysAssert(ptr, AipsError); 
    AlwaysAssert(testFunc(ptr, array, False, True), AipsError);
    AlwaysAssert(ptr, AipsError); 
    delete ptr;    

    // Case 3: a non-const which doesn't control the pointer
    ptr = new Array<float>(array.copy());
    AlwaysAssert(ptr, AipsError);
    AlwaysAssert(testFunc(ptr, array, False, False), AipsError);
    AlwaysAssert(ptr, AipsError); 
    delete ptr;    

    cout << "OK" << endl;

  } catch (AipsError x) {
    cerr << x.getMesg () << endl;
  } 
  
  return 0;
}
