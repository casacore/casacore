//# dArrayAccessor.cc: Demonstrator for the ArrayAccessor 1D access class
//# Copyright (C) 2002
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
#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main() {
  // Loop number
  const uInt Ncnt=100;
  try {
    Cube<Double> cub(5,2,4);
    for (uInt i=0; i<5; i++) {
      for (uInt j=0; j<2; j++) {
	for (uInt k=0; k<4; k++) {
	  cub(i,j,k) = 100*i + 10*j + k + 10000;
	}
      }
    }
    cout << "--------- Test ArrayAccessor ---------------------" << endl;
    cout << "------------------ Loop in cube ------------------" << endl;
    cout << "Cube: " << cub << endl;
    cout << "With accessor over axes 2-0-1: " << endl;
    for (ArrayAccessor<Double, Axis<2> > i(cub); i != i.end() ; ++i) {
      for (ArrayAccessor<Double, Axis<0> > j(i);
	   j != j.end(); ++j) {
	cout << *j << ", " << j.index<Axis<1> >(1) << endl;
      }
    }
    ArrayAccessor<Double, Axis<2> > aa(cub);
    ArrayAccessor<Double, Axis<0> > ab(cub);
        cout << "t1: " << *aa << ", " << *aa.begin() << ", " <<
	  *aa.begin(3) << endl;;;
	  ab.reset(aa.begin(3));
	  cout << "t2: " << *ab << endl;;
	  ab++;
	  cout << "t3: " << *ab << endl;;
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  }

  try {
    Cube<Double> cube(10,6,16);
    for (uInt i=0; i<10; i++) {
      for (uInt j=0; j<6; j++) {
	for (uInt k=0; k<16; k++) {
	  cube(i,j,k) = 100*i + 10*j + k + 10000;
	}
      }
    }
    Cube<Double> cub = cube(Slice(1,5,2), Slice(0,2,3), Slice(2,4,4));
    cout << "-------------------- Loop in slice of cube --------" << endl;
    cout << "Cube: " << cub << endl;
    cout << "With accessor over axes 2-0-1: " << endl;
    for (ArrayAccessor<Double, Axis<2> > i(cub); i != i.end() ; ++i) {
      for (ArrayAccessor<Double, Axis<0> > j(i);
	   j != j.end(); ++j) {
	cout << *j << ", " << j.index<Axis<1> >(1) << endl;
      }
    }
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  }

  try {
    Cube<Int> cub(100,100,100);
    indgen (cub);
    cout << "-- Various timings (*" << Ncnt << "(Cube: *" << Ncnt/10 <<
      ")) -------------" << endl;
    Timer timer;
    for (uInt cnt=0; cnt<Ncnt/10; cnt++) {
      Int inx=0;
      for (uInt i=0; i<100; i++) {
	for (uInt j=0; j<100; j++) {
	  for (uInt k=0; k<100; k++) {
	    if (cub(k,j,i) != inx) {
	      cout << inx << ' ' << cub(k,j,i) << endl;
	    }
	    inx++;
	  }
	}
      }
      if (inx != Int(cub.nelements())) {
	cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
      }
    }
    timer.show("Cube access   full ");

    {
      timer.mark();
      for (uInt cnt=0; cnt<Ncnt; cnt++) {
	Int inx=0;
	for (ArrayAccessor<Int, Axis<2> > i(cub); i != i.end() ; ++i) {
	  for (ArrayAccessor<Int, Axis<1> > j(i); j != j.end() ; ++j) {
	    for (ArrayAccessor<Int, Axis<0> > k(j); k != k.end() ; ++k) {
	      if (*k != inx) {
		cout << inx << ' ' << *k << endl;
	      }
	      inx++;
	    }
	  }
	}
	if (inx != Int(cub.nelements())) {
	  cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
	}
      }
      timer.show("ArrayAccessor full ");
    }

    {
      timer.mark();
      for (uInt cnt=0; cnt<Ncnt; cnt++) {
	Int inx=0;
	for (ArrayAccessor<Int, Axis<2> > i(cub); i != i.end() ; ++i) {
	  for (ArrayAccessor<Int, AxisN > j(i, AxisN(1)); j != j.end() ; ++j) {
	    for (ArrayAccessor<Int, Axis<0> > k(j); k != k.end() ; ++k) {
	      if (*k != inx) {
		cout << inx << ' ' << *k << endl;
	      }
	      inx++;
	    }
	  }
	}
	if (inx != Int(cub.nelements())) {
	  cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
	}
      }
      timer.show("ArrayAccessor  run ");
    }

    {
      timer.mark();
      for (uInt cnt=0; cnt<Ncnt; cnt++) {
	Int inx=0;
	for (ArrayAccessor<Int, Axis<2> > i(cub); i != i.end() ; ++i) {
	  for (ArrayAccessor<Int, AxisN > j(i, AxisN(1)); j != j.end() ; ++j) {
	    for (ArrayAccessor<Int, AxisN> k(j, AxisN(0)); k != k.end() ;
		 ++k) {
	      if (*k != inx) {
		cout << inx << ' ' << *k << endl;
	      }
	      inx++;
	    }
	  }
	}
	if (inx != Int(cub.nelements())) {
	  cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
	}
      }
      timer.show("ArrayAccessor run2 ");
    }

    {
      timer.mark();
      for (uInt cnt=0; cnt<Ncnt; cnt++) {
	Int inx=0;
	ArrayAccessor<Int, Axis<2> > i;
	ArrayAccessor<Int, Axis<1> > j;
	ArrayAccessor<Int, Axis<0> > k;
	for (i = ArrayAccessor<Int, Axis<2> >(cub); i != i.end() ; ++i) {
	  for (j = i; j != j.end() ; ++j) {
	    for (k = j; k != k.end() ; ++k) {
	      if (*k != inx) {
		cout << inx << ' ' << *k << endl;
	      }
	      inx++;
	    }
	  }
	}
	if (inx != Int(cub.nelements())) {
	  cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
	}
      }
      timer.show("ArrayAccessor nome ");
    }

    {
      timer.mark();
      for (uInt cnt=0; cnt<Ncnt; cnt++) {
	Int inx=0;
	ArrayAccessor<Int, Axis<2> > i;
	ArrayAccessor<Int, Axis<1> > j;
	ArrayAccessor<Int, Axis<0> > k;
	for (i = ArrayAccessor<Int, Axis<2> >(cub); i != i.end() ; ++i) {
	  for (j = i; j != j.end() ; ++j) {
	    for (k = j; k != k.end() ; ++k) {
	      *k = inx;
	      inx++;
	    }
	  }
	}
	if (inx != Int(cub.nelements())) {
	  cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
	}
      }
      timer.show("Accessor writenome ");
    }

    {
      timer.mark();
      for (uInt cnt=0; cnt<Ncnt; cnt++) {
	Int inx=0;
	ArrayAccessor<Int, Axis<2> > i(cub);
	ArrayAccessor<Int, Axis<1> > j;
	ArrayAccessor<Int, Axis<0> > k;
	for (; i != i.end() ; ++i) {
	  for (j = i; j != j.end() ; ++j) {
	    for (k = j; k != k.end() ; ++k) {
	      inx = *k;
	      inx++;
	    }
	  }
	}
	if (inx != Int(cub.nelements())) {
	  cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
	}
      }
      timer.show("Accessor read nome ");
    }

    timer.mark();
    for (uInt cnt=0; cnt<Ncnt; cnt++) {
      Int inx=0;
      Bool deleteIt;
      const Int* ptr = cub.getStorage(deleteIt);
      for (uInt i=0; i<cub.nelements(); i++) {
	if (ptr[i] != inx) {
	  cout << inx << ' ' << ptr[i] << endl;
	}
	inx++;
      }
      cub.freeStorage(ptr, deleteIt);
      if (inx != Int(cub.nelements())) {
	cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
      }
    }
    timer.show("getStorage    full ");

    timer.mark();
    for (uInt cnt=0; cnt<Ncnt; cnt++) {
      Int inx=0;
      uInt ix=0;
      Bool deleteIt;
      const Int* ptr = cub.getStorage(deleteIt);
      for (uInt i=0; i<100; i++) {
	for (uInt j=0; j<100; j++) {
	  for (uInt k=0; k<100; k++) {
	    if (ptr[ix] != inx) {
	      cout << inx << ' ' << ptr[i] << endl;
	    }
	    inx++; ix++;
	  }
	}
      }
      cub.freeStorage(ptr, deleteIt);
      if (inx != Int(cub.nelements())) {
	cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
      }
    }
    timer.show("getStorage   loops ");

    Cube<Int> cubs = cub(Slice(0,50,2), Slice(0,100,1), Slice(0,100,1));
    timer.mark();
    for (uInt cnt=0; cnt<Ncnt/10; cnt++) {
      Int inx=0;
      for (uInt i=0; i<100; i++) {
	for (uInt j=0; j<100; j++) {
	  for (uInt k=0; k<50; k++) {
	    if (cubs(k,j,i) != inx) {
	      cout << inx << ' ' << cubs(k,j,i) << endl;
	    }
	    inx+=2;
	  }
	}
      }
      if (inx != Int(cub.nelements())) {
	cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
      }
    }
    timer.show("Cube access   part ");

    timer.mark();
    for (uInt cnt=0; cnt<Ncnt; cnt++) {
      Int inx=0;
      for (ArrayAccessor<Int, Axis<2> > i(cubs); i != i.end() ; ++i) {
	for (ArrayAccessor<Int, Axis<1> > j(i); j != j.end() ; ++j) {
	  for (ArrayAccessor<Int, Axis<0> > k(j); k != k.end() ; ++k) {
	    if (*k != inx) {
	      cout << inx << ' ' << *k << endl;
	    }
	    inx+=2;
	  }
	}
      }
      if (inx != Int(cub.nelements())) {
	cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
      }
    }
    timer.show("ArrayAccessor part ");

    timer.mark();
    for (uInt cnt=0; cnt<Ncnt; cnt++) {
      Int inx=0;
      Bool deleteIt;
      const Int *ptr = cubs.getStorage(deleteIt);
      for (uInt i=0; i<cubs.nelements(); i++) {
	if (ptr[i] != inx) {
	  cout << inx << ' ' << ptr[i] << endl;
	}
	inx+=2;
      }
      cubs.freeStorage(ptr, deleteIt);
      if (inx != Int(cub.nelements())) {
	cout << "Inx: " << inx << ' ' << cub.nelements() << endl;
      }
    }
    timer.show("getStorage    part ");

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    return 1;
  }

  return 0;

}
