//# tInterfaceThreadUnsafe.cc: This program tests the ManagedObjectPool class
//# Copyright (C) 2022
//# NRF South African Radio Astronomy Observatory and
//# Benjamin Hugo
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

#include <casacore/casa/Utilities/InterfaceThreadUnsafe.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <iostream>
#include <thread>

using namespace casacore;
using namespace std;

class Dummy : InterfaceThreadUnsafe {
public:
    Dummy() {}
    int foobar() {
        verifyProcessIdentifier();
        return 42;
    }
protected:
    virtual void onMultithreadedAccess(pid_t fromPid, pthread_t fromTid) const{
        throw NotThreadSafeError();
    }
};

int main() {
    Dummy dummy;
    bool doraise=false;
    try {
        dummy.foobar();
    } catch (NotThreadSafeError& x) {
        doraise=true;
    }
    doraise=false;
    AlwaysAssert(!doraise, AipsError);
    
    auto t = thread([&](){ 
        bool doraise=false;
        try {
            dummy.foobar(); 
        } catch (NotThreadSafeError& x) {
            doraise=true;
        }
        AlwaysAssert(doraise, AipsError);
    });
    t.join();
    cout << "OK" << endl;
    return 0;
}