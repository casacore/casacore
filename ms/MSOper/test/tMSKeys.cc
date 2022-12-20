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
        {
            cout << "*** test ArrayKey==" << endl;
            ArrayKey first, second;
            first.arrayID = 4;
            first.obsID = 6;
            second.arrayID = 4;
            second.obsID = 6;
            AlwaysAssert(first == second, AipsError);
            second.obsID = 5;
            AlwaysAssert(first != second, AipsError);
            second.obsID = 6;
            second.arrayID = 3;
            AlwaysAssert(first != second, AipsError);
        }
        {
            cout << "*** test filter" << endl;
            std::set<ScanKey> scanKeys;
            ScanKey sk;
            sk.obsID = 0;
            sk.arrayID = 0;
            sk.scan = 9;
            scanKeys.insert(sk);
            sk.obsID = 1;
            scanKeys.insert(sk);
            sk.arrayID = 1;
            scanKeys.insert(sk);
            sk.obsID = 0;
            scanKeys.insert(sk);
            std::set<ScanKey>::const_iterator iter = scanKeys.begin();
            std::set<ScanKey>::const_iterator end = scanKeys.end();
            ArrayKey x;
            uInt i = 0;
            ScanKey expec;
            expec.scan = 9;
            for (; iter!=end; ++iter, ++i) {
                x.obsID = i % 2;
                x.obsID = i/2;
                x.arrayID = iter->arrayID;
                std::set<ScanKey> filtered = filter(scanKeys, x);
                AlwaysAssert(filtered.size() == 1, AipsError);
                expec.obsID = x.obsID;
                expec.arrayID = x.arrayID;
                AlwaysAssert(*filtered.begin() == expec, AipsError);
            }
        }
        cout << "OK" << endl;
    } 
    catch (const std::exception& x) {
    	cerr << "Exception : " << x.what() << endl;
    	return 1;
    }
    return 0;
}
