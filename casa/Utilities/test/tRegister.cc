//# tRegister.cc: This program tests the Register class
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

#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Utilities/test/tRegister.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main()
{
  foo *ap = 0;
  bar *bp = 0;
  foobar *cp = 0;
  foo2 *dp = 0;
  bar2 *ep = 0;
  foo2bar *fp = 0;
  bar2foo *gp = 0;
  foobar2 *hp = 0;
  barfoo2 *ip = 0;
  foo2bar2 *jp = 0;
  bar2foo2 *kp = 0;
  mytmp<int> *t1ap = 0;
  mytmp<float> *t1bp = 0;
  mytmp<short> *t1cp = 0;
  mytmp<long> *t1dp = 0;
  mytmp2<int> *t2ap = 0;
  mytmp2<float> *t2bp = 0;
  mytmp2<short> *t2cp = 0;
  mytmp2<long> *t2dp = 0;

  cout << "foo:      " << Register(ap) << endl;
  cout << "bar:      " << Register(bp) << endl;
  cout << "foobar:   " << Register(cp) << endl;
  cout << "foo2:     " << Register(dp) << endl;
  cout << "bar2:     " << Register(ep) << endl;
  cout << "foo2bar:  " << Register(fp) << endl;
  cout << "bar2foo:  " << Register(gp) << endl;
  cout << "foobar2:  " << Register(hp) << endl;
  cout << "barfoo2:  " << Register(ip) << endl;
  cout << "foo2bar2: " << Register(jp) << endl;
  cout << "bar2foo2: " << Register(kp) << endl;

  cout << "-- -- -- -- -- -- -- -- -- -- -- -- -- --" << endl;

  cout << "foo:      " << Register(ap) << endl;
  cout << "bar:      " << Register(bp) << endl;
  cout << "foobar:   " << Register(cp) << endl;
  cout << "foo2:     " << Register(dp) << endl;
  cout << "bar2:     " << Register(ep) << endl;
  cout << "foo2bar:  " << Register(fp) << endl;
  cout << "bar2foo:  " << Register(gp) << endl;
  cout << "foobar2:  " << Register(hp) << endl;
  cout << "barfoo2:  " << Register(ip) << endl;
  cout << "foo2bar2: " << Register(jp) << endl;
  cout << "bar2foo2: " << Register(kp) << endl;

  cout << "-- -- -- -- -- -- -- -- -- -- -- -- -- --" << endl;

  cout << "mytmp<int>:    " << Register(t1ap) << endl;
  cout << "mytmp<float>:  " << Register(t1bp) << endl;
  cout << "mytmp<short>:  " << Register(t1cp) << endl;
  cout << "mytmp<long>:   " << Register(t1dp) << endl;
  cout << "mytmp2<int>:   " << Register(t2ap) << endl;
  cout << "mytmp2<float>: " << Register(t2bp) << endl;
  cout << "mytmp2<short>: " << Register(t2cp) << endl;
  cout << "mytmp2<long>:  " << Register(t2dp) << endl;

  cout << "-- -- -- -- -- -- -- -- -- -- -- -- -- --" << endl;

  cout << "mytmp<int>:    " << Register(t1ap) << endl;
  cout << "mytmp<float>:  " << Register(t1bp) << endl;
  cout << "mytmp<short>:  " << Register(t1cp) << endl;
  cout << "mytmp<long>:   " << Register(t1dp) << endl;
  cout << "mytmp2<int>:   " << Register(t2ap) << endl;
  cout << "mytmp2<float>: " << Register(t2bp) << endl;
  cout << "mytmp2<short>: " << Register(t2cp) << endl;
  cout << "mytmp2<long>:  " << Register(t2dp) << endl;

  return 0;

}
