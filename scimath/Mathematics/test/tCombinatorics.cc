//# tMathFunc.cc: This program tests MathFunc objects
//# Copyright (C) 1993,1994,1995,1997,1998,1999,2000,2001
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

#include <casacore/scimath/Mathematics/Combinatorics.h>

#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>

int main() {
    try {
        AlwaysAssert(Combinatorics::factorial(0) == 1, AipsError);
        AlwaysAssert(Combinatorics::factorial(1) == 1, AipsError);
        AlwaysAssert(Combinatorics::factorial(5) == 120, AipsError);
        AlwaysAssert(Combinatorics::factorial(4) == 24, AipsError);
        AlwaysAssert(Combinatorics::factorial(6) == 720, AipsError);
        
        AlwaysAssert(Combinatorics::choose(6,3) == 20, AipsError);
        AlwaysAssert(Combinatorics::choose(5,3) == 10, AipsError);
       
        {
        	// test exception is thrown if k > n
            Bool res = False;
            try {
                Combinatorics::choose(3,5);
            }
            catch (AipsError) {
                res = True;
            }
            AlwaysAssert(res, AipsError);
        }


        cout << "ok" << endl;
    }
    catch (AipsError x) {
        cerr << x.getMesg() << endl;
        return 1;
    }
    return 0;
}
