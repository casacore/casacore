//# tSTLIO.cc: This program tests STL IO
//# Copyright (C) 2011
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
//# $Id: tSTLIO.cc 21315 2013-02-13 12:24:02Z gervandiepen $

//# Includes

#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdmap.h>

using namespace casacore;

int main()
{
  // Test with various delimiters.
  Vector<Int> vec2(3);
  indgen(vec2, 1);
  {
    ostringstream oss;
    showContainer (oss, vec2);
    AlwaysAssertExit (oss.str() == "[1,2,3]");
  }
  {
    ostringstream oss;
    showContainer (oss, vec2, ", ");
    AlwaysAssertExit (oss.str() == "[1, 2, 3]");
  }
  {
    ostringstream oss;
    showDataIter (oss, vec2.data(), vec2.data()+vec2.size(), " ", "(", ")");
    AlwaysAssertExit (oss.str() == "(1 2 3)");
  }

  // Test a map (and pair).
  map<Int,String> map1;
  map1[-1] = "str-1";
  map1[3] = "str3";
  {
    ostringstream oss;
    oss << map1;
    AlwaysAssertExit (oss.str() == "{<-1,str-1>, <3,str3>}");
  }

  // Test empty container and vector.
  {
    ostringstream oss;
    oss << vector<int>();
    AlwaysAssertExit (oss.str() == "[]");
  }
  {
    ostringstream oss;
    oss << vector<int>(1,3);
    AlwaysAssertExit (oss.str() == "[3]");
  }
  {
    ostringstream oss;
    oss << vector<int>(3,4);
    AlwaysAssertExit (oss.str() == "[4,4,4]");
  }

  cout << "OK\n";
  return 0;
}
