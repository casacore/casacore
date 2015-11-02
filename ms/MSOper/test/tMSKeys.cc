//# tMSKeys.cc
//# Copyright (C) 2013
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
//# $Id: tMSMetaData.cc 21578 2015-03-18 15:01:43Z gervandiepen $

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/ms/MSOper/MSKeys.h>

#include <casacore/casa/namespace.h>

#include <iostream>

using namespace std;

int main() {
    try {
        {
            cout << "*** test uniqueArrayKeys()" << endl;
            ScanKey s;
            s.arrayID = 0;
            s.obsID = 2;
            s.scan = 4;
            set<ScanKey> skeys;
            skeys.insert(s);
            s.scan = 5;
            skeys.insert(s);
            s.arrayID = 1;
            skeys.insert(s);
            set<ArrayKey> aKeys = uniqueArrayKeys(skeys);
            AlwaysAssert(aKeys.size() == 2, AipsError);
            set<ArrayKey>::const_iterator iter = aKeys.begin();
            AlwaysAssert(iter->arrayID == 0 && iter->obsID == 2, AipsError);
            ++iter;
            AlwaysAssert(iter->arrayID == 1 && iter->obsID == 2, AipsError);
        }
    	cout << "OK" << endl;
    } 
    catch (const AipsError& x) {
    	cerr << "Exception : " << x.getMesg() << endl;
    	return 1;
    }
    return 0;
}
