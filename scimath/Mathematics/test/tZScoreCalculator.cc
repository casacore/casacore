//# tStatAcc.cc: Test program for class StatAcc
//# Copyright (C) 1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: tStatAcc.cc 20329 2008-06-06 07:59:22Z gervandiepen $

#include <casacore/scimath/Mathematics/ZScoreCalculator.h>

#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>

int main() {
    try {
    	for (Double z=0; z<=7; z+=0.5) {
    		cout << z << " "  << ZScoreCalculator::zscoreToNpts(z) << endl;
    	}
    	uInt count = 0;
    	uInt64 x = 10;
    	while (count < 15) {
    		cout << "log(npts) " << log10(x) << " zscore " << ZScoreCalculator::getMaxZScore(x) << endl;
    		++count;
    		x *= 10;
    	}
    }
    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}






