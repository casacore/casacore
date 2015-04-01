//# tCountedPtr.cc: This program tests the Counted Pointer class
//# Copyright (C) 1993,1994,1995,2001
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

#include <casacore/casa/Utilities/test/tCountedPtr.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// class myobj is defined in tCountedPtr.h

String prt(CountedPtr<myobj> &obj) {
  return obj->name();
}

void testDerived()
{
  cout << "start testDerived" << endl;
  {
    CountedPtr<myobj>  v0 (new myobj("v0"));
    CountedPtr<myobj>  v1 (new myobj1("v1"));
    CountedPtr<myobj1> v2 (new myobj1("v2"));
    CountedPtr<myobj>  v3(v1);
    CountedPtr<myobj>  v4(v2);
    CountedPtr<myobj1> v5(dynamic_pointer_cast<myobj1>(v1));
    cout << v0->name() << ' ' << v1->name() << ' ' << v2->name() << ' '
         << v3->name() << ' ' << v4->name() << ' ' << v5->name() << endl;
    v0 = v1;
    cout << v0->name() << ' ' << v1->name() << ' ' << v2->name() << ' '
         << v3->name() << ' ' << v4->name() << ' ' << v5->name() << endl;
    v0 = v2;
    cout << v0->name() << ' ' << v1->name() << ' ' << v2->name() << ' '
         << v3->name() << ' ' << v4->name() << ' ' << v5->name() << endl;
    v2 = v5;
    cout << v0->name() << ' ' << v1->name() << ' ' << v2->name() << ' '
         << v3->name() << ' ' << v4->name() << ' ' << v5->name() << endl;
  }
  cout << "end testDerived" << endl;
}

int main() {

  cout << ">>>" << endl;
  if (countedPtrShared()) {
    cout << "Using shared_ptr" << endl;
  } else {
    cout << "Not using shared_ptr" << endl;
  }
  cout << "<<<" << endl;

  CountedPtr<myobj> var = new myobj("fred");
  CountedPtr<myobj> var2 = var;
  CountedPtr<myobj> var3 = var;
  CountedPtr<myobj> var4 (var);
  AlwaysAssertExit (var != 0);

  cout << (*var).name() << ".." <<
    (*var2).name() << ".." <<
    (*var3).name() << ".." <<
    (*var4).name() << ".." << endl;

  var = new myobj("barney");

  cout << (*var).name() << ".." <<
    (*var2).name() << ".." <<
    (*var3).name() << ".." <<
    (*var4).name() << ".." << endl;

  // Check to see if the no-delete feature is honored
  // by the copy constructor and the assignment operator

  myobj * doNotDelete = new myobj ("Don't delete me!");

  {
      CountedPtr<myobj> p1 (doNotDelete, False);
      CountedPtr<myobj> p2 (p1);
      CountedPtr<myobj> p3;

      p3 = p1;
  }

  cout << "Now explicitly delete object" << endl;

  delete doNotDelete;

  // Test reset.
  var2.reset(new myobj("betty"));
  cout << var->name() << ".." <<
    var2->name() << ".." <<
    prt(var3) << ".." <<
    var4->name() << ".." << endl;

  // Test with a derived class.
  testDerived();

  return 0;
}
