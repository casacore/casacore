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

#include <casa/Utilities/test/tCountedPtr.h>
#include <casa/Utilities/CountedPtr.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// class myobj is defined in tCountedPtr.h

const char *prt(CountedPtr<myobj> &obj) {
  return obj->name();
}

int main() {

#if defined (USE_BOOST_SHARED_PTR)
#warning "Using boost::shared_ptr"
#else
#warning "Not using boost::shared_ptr"
#endif

  CountedPtr<myobj> var = new myobj("fred");
  CountedPtr<myobj> var2 = var;
  CountedPtr<myobj> var3 = var;
  CountedPtr<myobj> var4 (var);

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

// Replace is deprecated and not possible when
// using a boost or c++0x smart pointer implementation
//
//  var2.replace(new myobj("betty"));
//
//  cout << var->name() << ".." <<
//    var2->name() << ".." <<
//    prt(var3) << ".." <<
//    var4->name() << ".." << endl;

  return 0;
}
