//# tArrayIteratorSTL.cc: Test program for the Array Iterator member class
//# Copyright (C) 2002,2003
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

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <vector>
#include <iterator>

// If an argument is given, some performance tests will also be done.

using namespace casacore;

void testSub (Array<Int>& arr1, const IPosition& blc,
	      const IPosition& trc, const IPosition& inc)
{
  Array<Int> arr = arr1(blc,trc,inc);
  Array<Int> arrs;
  arrs = arr;
  std::vector<Int> vec(arr.begin(), arr.end());
  Array<Int>::const_iterator iters = arrs.begin();
  uInt i=0;
  Array<Int>::const_iterator enditer=arr.end();
  for (Array<Int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
    if (*iter != *iters) {
      cout << "error in iter: " << *iter << ' ' << *iters << endl;
    }
    if (vec[i] != *iters) {
      cout << "error in vec: " << vec[i] << ' ' << *iters << endl;
    }
    if (arrs.data()[i] != *iters) {
      cout << "error in data(): " << arrs.data()[i] << ' ' << *iters << endl;
    }
    iters++;
    i++;
  }
  AlwaysAssert (i == arr.size(), AipsError);
  AlwaysAssert (std::distance(arr.begin(), arr.end()) == Int(arr.size()),
		AipsError);
}


void testIt()
{
  Array<Int> arr(IPosition(3,4,5,6));
  indgen(arr);
  {
    Int i=0;
    Array<Int>::const_iterator enditer=arr.end();
    for (Array<Int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
      if (*iter != i) {
	cout << "error: " << *iter << ' ' << i << endl;
      }
      ++i;
    }
    if (uInt(i) != arr.nelements()) {
      cout << "error: i!=" << arr.nelements() << endl;
    }
  }
  testSub (arr, IPosition(3,0,0,0), IPosition(3,3,4,5), IPosition(3,1,1,1));
  testSub (arr, IPosition(3,0,0,0), IPosition(3,3,4,5), IPosition(3,2,1,1));
  testSub (arr, IPosition(3,0,0,0), IPosition(3,3,4,5), IPosition(3,1,1,2));
  testSub (arr, IPosition(3,3,0,0), IPosition(3,3,4,5), IPosition(3,1,1,1));
  testSub (arr, IPosition(3,3,4,1), IPosition(3,3,4,4), IPosition(3,1,1,1));
  testSub (arr, IPosition(3,1,2,1), IPosition(3,3,3,4), IPosition(3,2,1,1));

  // Test an empty array.
  {
    Array<Int> earr;
    for (Array<Int>::iterator itera1=earr.begin();
	 itera1!=earr.end(); itera1++) {
      cout << "error (itera1 should be empty): " << itera1 << ' ' << endl;
    }
    for (Array<Int>::iterator itera2=earr.begin();
	 itera2!=earr.end(); itera2++) {
      cout << "error (itera2 should be empty): " << itera2 << ' ' << endl;
    }
    for (Array<Int>::contiter itera3=earr.cbegin(); 
	 itera3!=earr.cend(); itera3++) {
      cout << "error (itera3 should be empty): " << itera3 << ' ' << endl;
    }
    for (Array<Int>::contiter itera4=earr.cbegin();
	 itera4!=earr.cend(); itera4++) {
      cout << "error (itera4 should be empty): " << itera4 << ' ' << endl;
    }
  }
  {
    IPosition eshp;
    Array<Int> earr(eshp);
    for (Array<Int>::iterator iterb1=earr.begin();
	 iterb1!=earr.end(); iterb1++) {
      cout << "error (iterb1 should be empty): " << iterb1 << ' ' << endl;
    }
    for (Array<Int>::iterator iterb2=earr.begin();
	 iterb2!=earr.end(); iterb2++) {
      cout << "error (iterb2 should be empty): " << iterb2 << ' ' << endl;
    }
    for (Array<Int>::contiter iterb3=earr.cbegin(); 
	 iterb3!=earr.cend(); iterb3++) {
      cout << "error (iterb3 should be empty): " << iterb3 << ' ' << endl;
    }
    for (Array<Int>::contiter iterb4=earr.cbegin();
	 iterb4!=earr.cend(); iterb4++) {
      cout << "error (iterb4 should be empty): " << iterb4 << ' ' << endl;
    }
  }
}

int main (int argc, char* [])
{
  testIt();
  if (argc < 2) {
    return 0;
  }
  // Do performance tests.
  const Int nelem = 1000000;
  const Int nstep = 100;
  //const Int nstep = 1;
  {
    Array<Int> arr(IPosition(1,nelem));
    indgen(arr);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      for (Array<Int>::const_iterator iter=arr.begin(); iter!=arr.end(); ++iter) {
	if (*iter != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  full, end()     ");
  }
  {
    Array<Int> arr(IPosition(1,nelem));
    indgen(arr);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::const_iterator enditer=arr.end();
      for (Array<Int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
	if (*iter != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  full; enditer   ");
  }
  {
    Array<Int> arr(IPosition(1,nelem));
    indgen(arr);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      for (Array<Int>::const_contiter iter=arr.cbegin(); iter!=arr.cend(); ++iter) {
	if (*iter != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  full, contiter()");
  }
  {
    Array<Int> bl(IPosition(1,nelem));
    indgen(bl);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      const Int* str = bl.getStorage(deleteIt);
      for (Int i=0; i<nelem; ++i) {
	if (str[i] != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
      bl.freeStorage(str, deleteIt);
    }
    tim.show("read  full, getStorage");
  }
  {
    Array<Int> bl(IPosition(1,nelem));
    indgen(bl);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Int* ptr = bl.data();
      for (Int i=0; i<nelem; ++i) {
	if (ptr[i] != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  full, data()[i] ");
  }
  {
    Array<Int> bl(IPosition(1,nelem));
    indgen(bl);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      const Int* str = bl.getStorage(deleteIt);
      const Bool contig = bl.contiguousStorage();
      for (Int i=0; i<nelem; ++i) {
	if (str[i] != inx) {
	  cout << "err" << endl;
	}
	inx++;
	// This test is always false. It is there to mimic the
	// contig test in ArraySTLIterator.
	if (!contig) {
	  inx++;
	}
      }
      bl.freeStorage(str, deleteIt);
    }
    tim.show("read  full, getSt+test");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> bl = bl1(IPosition(2,50,50), bl1.shape()-50);
    indgen(bl);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      uInt n = bl.nelements();
      const Int* str = bl.getStorage(deleteIt);
      for (uInt i=0; i<n; ++i) {
	if (str[i] != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
      bl.freeStorage(str, deleteIt);
    }
    tim.show("read  part, getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> arr = bl1(IPosition(2,50,50), bl1.shape()-50);
    indgen(arr);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::const_iterator enditer=arr.end();
      for (Array<Int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
	if (*iter != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  part, enditer   ");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> bl = bl1(IPosition(2,50,50), bl1.shape()-50, IPosition(2,2,2));
    indgen(bl);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      uInt n = bl.nelements();
      const Int* str = bl.getStorage(deleteIt);
      for (uInt i=0; i<n; ++i) {
	if (str[i] != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
      bl.freeStorage(str, deleteIt);
    }
    tim.show("read  incr, getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> arr = bl1(IPosition(2,50,50), bl1.shape()-50, IPosition(2,2,2));
    indgen(arr);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::const_iterator enditer=arr.end();
      for (Array<Int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
	if (*iter != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  incr, enditer   ");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> bl = bl1(IPosition(2,50,0), IPosition(2,50,999));
    indgen(bl);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      uInt n = bl.nelements();
      const Int* str = bl.getStorage(deleteIt);
      for (uInt i=0; i<n; ++i) {
	if (str[i] != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
      bl.freeStorage(str, deleteIt);
    }
    tim.show("read  small getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> arr = bl1(IPosition(2,50,0), IPosition(2,50,999));
    indgen(arr);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::const_iterator enditer=arr.end();
      for (Array<Int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
	if (*iter != inx) {
	  cout << "err" << endl;
	}
	inx++;
      }
    }
    tim.show("read  small enditer   ");
  }

  {
    Array<Int> arr(IPosition(1,nelem));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      for (Array<Int>::iterator iter=arr.begin(); iter!=arr.end(); ++iter) {
	*iter = inx;
	inx++;
      }
    }
    tim.show("write full; end()     ");
  }
  {
    Array<Int> arr(IPosition(1,nelem));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::iterator enditer;
      enditer = arr.end();
      for (Array<Int>::iterator iter=arr.begin(); iter!=enditer; ++iter) {
	*iter = inx;
	inx++;
      }
    }
    tim.show("write full, enditer   ");
  }
  {
    Array<Int> arr(IPosition(1,nelem));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      for (Array<Int>::contiter iter=arr.cbegin(); iter!=arr.cend(); ++iter) {
	*iter = inx;
	inx++;
      }
    }
    tim.show("write full, contiter()");
  }
  {
    Array<Int> bl(IPosition(1,nelem));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      Int* str = bl.getStorage(deleteIt);
      for (Int i=0; i<nelem; ++i) {
	str[i] = inx;
	inx++;
      }
      bl.putStorage(str, deleteIt);
    }
    tim.show("write full, getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> bl = bl1(IPosition(2,50,50), bl1.shape()-50);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      uInt n = bl.nelements();
      Int* str = bl.getStorage(deleteIt);
      for (uInt i=0; i<n; ++i) {
	str[i] = inx;
	inx++;
      }
      bl.putStorage(str, deleteIt);
    }
    tim.show("write part, getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> arr = bl1(IPosition(2,50,50), bl1.shape()-50);
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::iterator enditer=arr.end();
      for (Array<Int>::iterator iter=arr.begin(); iter!=enditer; ++iter) {
	*iter = inx;
	inx++;
      }
    }
    tim.show("write part, enditer   ");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> bl = bl1(IPosition(2,50,50), bl1.shape()-50, IPosition(2,2,2));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      uInt n = bl.nelements();
      Int* str = bl.getStorage(deleteIt);
      for (uInt i=0; i<n; ++i) {
	str[i] = inx;
	inx++;
      }
      bl.putStorage(str, deleteIt);
    }
    tim.show("write incr, getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> arr = bl1(IPosition(2,50,50), bl1.shape()-50, IPosition(2,2,2));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::iterator enditer=arr.end();
      for (Array<Int>::iterator iter=arr.begin(); iter!=enditer; ++iter) {
	*iter = inx;
	inx++;
      }
    }
    tim.show("write incr, enditer   ");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> bl = bl1(IPosition(2,50,0), IPosition(2,50,999));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Bool deleteIt;
      uInt n = bl.nelements();
      Int* str = bl.getStorage(deleteIt);
      for (uInt i=0; i<n; ++i) {
	str[i] = inx;
	inx++;
      }
      bl.putStorage(str, deleteIt);
    }
    tim.show("write small getStorage");
  }
  {
    Array<Int> bl1(IPosition(2,1000,1000));
    Array<Int> arr = bl1(IPosition(2,50,0), IPosition(2,50,999));
    Timer tim;
    for (Int j=0; j<nstep; j++) {
      Int inx=0;
      Array<Int>::iterator enditer=arr.end();
      for (Array<Int>::iterator iter=arr.begin(); iter!=enditer; ++iter) {
	*iter = inx;
	inx++;
      }
    }
    tim.show("write small enditer   ");
  }
  return 0;
}
