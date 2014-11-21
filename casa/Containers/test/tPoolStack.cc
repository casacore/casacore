//# tPoolStack.cc -- test PoolStack
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
#include <casacore/casa/Containers/PoolStack.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main() {

  Bool ok(True);
  try {
    cout << "Test PoolStack" << endl;
    cout << "---------------------------------------------------" << endl;

    // Create two pools
    PoolStack<Vector<Double>, uInt> pool3(3);
    PoolStack<Vector<Double>, uInt> pool5(5);
    // Some poolobjects
    Vector<Double> *list3[10];
    Vector<Double> *list5[10];
    
    for (uInt i=0; i<10000; i++) {
      for (uInt j=0; j<10; j++) {
	list3[j] = pool3.get();
	list5[j] = pool5.get();
      }
      if (pool3.key() != 3) {
	if (ok) cout << "Illegal pool3 id" << endl;
	ok = False;
      }
      if (pool5.key() != 5) {
	if (ok) cout << "Illegal pool5 id" << endl;
	ok = False;
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
	pool3.release(list3[9-j]);
	pool5.release(list5[9-j]);
      }
    }
    
    if (pool3.nelements() != 16) {
      cout << pool3.nelements() << " elements in pool3" << endl;
      ok = False;
    }
    pool5.addElements(2);
    list5[0] = pool5.get();
    if (list5[0]->nelements() != 5) {
      cout << "Incorrectly added elements" << endl;
      ok = False;
    }
    pool5.release(list5[0]);
    if (pool5.nelements() != pool3.nelements() + 2) {
      cout << pool5.nelements() << " elements in pool5" << endl;
      ok = False;
    }
    pool5.clear();
    if (pool5.nelements() != 0) {
      cout << pool5.nelements() << " elements in pool5 after clearing" << endl;
      ok = False;
    }
    pool3.clear();
    if (!pool5.empty() || !pool3.empty()) {
      cout << "Incorrect stack pointer " << endl;
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

