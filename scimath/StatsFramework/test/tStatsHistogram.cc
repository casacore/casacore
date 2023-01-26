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

#include <casacore/scimath/StatsFramework/StatsHistogram.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>

int main() {
    try {
        // CAS-11828 for very small binwidth
    	StatsHistogram<double> sh(
    	    -0.0014156261459125514795, -0.0014156261458961459827, 1937361
    	);
    	auto idx = sh.getIndex(-0.0014156261458992958069);
    	AlwaysAssert(idx == 1565404, AipsError);
    }
    catch (const std::exception& x) {
        cout << x.what() << endl;
        return 1;
    } 
    return 0;
}






