//# tFunctionOrder.cc: Test the one-dimensional scaled polynomial class
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

#include <casacore/scimath/Functionals/FunctionOrder.h>

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  cout << "---------------- test FunctionOrder ---------------" << endl;
  {
    FunctionOrder<Double> x;
    FunctionOrder<Double> z;
    String errormsg;
    Record y;
    cout << "x: " << x << endl;
    cout << "To: " << x.toRecord(errormsg, y) << endl;
    cout << "From: ";
    cout << z.fromRecord(errormsg, y) << ": " << z << endl;
    cout << "-----------------------------------------------------" << endl;
    x.getInt(0) = 0;
    x.getInt(1) = 5;
    cout << "x: " << x << endl;
    cout << "To: " << x.toRecord(errormsg, y) << endl;
    cout << "From: " << z.fromRecord(errormsg, y) << ": " << z << endl;
    cout << "-----------------------------------------------------" << endl;

  }
  cout << "-----------------------------------------------------" << endl;
  cout << "OK" << endl;
  return 0;
}
