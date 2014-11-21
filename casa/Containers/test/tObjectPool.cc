//# tObjectPool.cc -- test ObjectPool
//# Copyright (C) 2001
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
#include <casacore/casa/Containers/ObjectPool.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main() {

  Bool ok(True);
  try {
    cout << "Test ObjectPool" << endl;
    cout << "---------------------------------------------------" << endl;

    // Create pool
    ObjectPool<Vector<Double>, uInt> pool;
    // Some poolobjects
    Vector<Double> *list3[10];
    Vector<Double> *list5[10];
    
    for (uInt i=0; i<1; i++) {
      //for (uInt i=0; i<10000; i++) {
      for (uInt j=0; j<10; j++) {
	list3[j] = pool.get(3);
	list5[j] = pool.get(5);
      }
      if ((list3[4])->nelements() != 3) {
	if (ok) cout << "Illegal pool3 length" << endl;
	ok = False;
      }
      if ((list5[7])->nelements() != 5) {
	if (ok) cout << "Illegal pool5 length" << endl;
	ok = False;
      }
      for (uInt j=0; j<10; j++) {
	pool.release(list3[9-j], 3);
	pool.release(list5[9-j], 5);
      }
    }
    
    if (pool.nelements() != 2) {
      cout << pool.nelements() << " elements in pool" << endl;
      ok = False;
    }
    list5[0] = pool.get(5);
    if (list5[0]->nelements() != 5) {
      cout << "Incorrectly added elements" << endl;
      ok = False;
    }
    pool.release(list5[0], 5);
    PoolStack<Vector<Double>, uInt> &mstack = pool.getStack(5);
    if (mstack.key() != 5) {
      cout << "Illegal key found" << endl;
      ok = False;
    }
    pool.clear();
    if (pool.nelements() != 1) {
      cout << pool.nelements() << " elements in pool after clearing" << endl;
      ok = False;
    }

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    ok = False;
  }
  
  if (!ok) return 1;
  cout << "Tests ok" << endl;
  cout << "---------------------------------------------------" << endl;

  return 0;
}

