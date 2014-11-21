//# tComplex.cc: This program tests the Complex class
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2002
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
//# $Id: tComplex.cc 21130 2011-10-18 07:39:05Z gervandiepen $

//# Includes

#include <casacore/casa/BasicSL/STLMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

void testStdVectorPlus()
{
  std::vector<Int> a(3);
  std::vector<Int> b(3);
  a[0] = 0; a[1] = 1; a[2] = 2;
  b[0] = 5; b[1] = 6; b[2] = 7;
  std::vector<Int> c = a + b;
  AlwaysAssertExit(c.size() == 3  &&  c[0] == 5  &&
                   c[1] == 7  &&  c[2] == 9);
  std::vector<int> d(2);
  Bool caught = False;
  try {
    std::vector<int> e = a + d;
    // exception should be thrown, shouldn't get here.
    AlwaysAssertExit(False);
  }
  catch (const AipsError& exc) {
    caught = True;
  }
  AlwaysAssertExit(caught);
}

void testStdVectorDivide()
{
  std::vector<Int> a(3);
  a[0] = 0; a[1] = 2; a[2] = 4;
  std::vector<Int> b = a/2;
  AlwaysAssertExit(b.size() == 3  &&  b[0] == 0  &&
                   b[1] == 1  &&  b[2] == 2);
}


int main()
{
  try {
    testStdVectorPlus();
    testStdVectorDivide();
  } catch (const AipsError& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
