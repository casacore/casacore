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
#include <sys/wait.h>
#include <unistd.h>

using namespace casacore;
using namespace std;

class DummyThreadUnsafe : InterfaceThreadUnsafe {
public:
    DummyThreadUnsafe() {}
    int foobar() {
        verifyProcessIdentifier();
        return 42;
    }
protected:
    virtual void onMultithreadedAccess() const{
        throw NotThreadSafeError();
    }
};

class DummyProcessUnsafe : InterfaceThreadUnsafe {
public:
    DummyProcessUnsafe() : InterfaceThreadUnsafe(true, true) {}
    int foobar() {
        verifyProcessIdentifier();
        return 42;
    }
protected:
    virtual void onMultithreadedAccess() const{
        throw NotThreadSafeError();
    }
};

class DummySafe : InterfaceThreadUnsafe {
public:
    DummySafe() : InterfaceThreadUnsafe(false, false) {}
    int foobar() {
        verifyProcessIdentifier();
        return 42;
    }
protected:
    virtual void onMultithreadedAccess() const{
        throw NotThreadSafeError();
    }
};

int main() {
    // check threading
    {
        DummyThreadUnsafe dummy;
        bool doraise=false;
        try {
            dummy.foobar();
        } catch (NotThreadSafeError& x) {
            doraise=true;
        }
        AlwaysAssert(!doraise, AipsError);
        doraise=false;
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
    }
    // check process
    {
        DummyProcessUnsafe dummy;
        pid_t c_pid = fork();
        // fork should not fail
        AlwaysAssert(c_pid != -1, AipsError);
        if (c_pid > 0) { // parent
            bool doraise=false;
            try {
                dummy.foobar();
            } catch (NotThreadSafeError& x) {
                doraise=true;
            }
            int child_doraise;
            wait(&child_doraise);
            // parent must not raise
            AlwaysAssert(!doraise, AipsError);
            AlwaysAssert(child_doraise, AipsError);
        } else { // child process
            bool doraise=false;
            try {
                dummy.foobar();
            } catch (NotThreadSafeError& x) {
                doraise=true;
            }
            _exit(doraise);
        }
    }
    // check process safe thread unsafe
    // (this is generally allowed -- OS will copy all pages upon touch)
    {
        DummyThreadUnsafe dummy; 
        pid_t c_pid = fork();
        // fork should not fail, thread should
        AlwaysAssert(c_pid != -1, AipsError);
        if (c_pid > 0) { // parent
            bool doraise=false;
            try {
                dummy.foobar();
            } catch (NotThreadSafeError& x) {
                doraise=true;
            }
            int child_doraise;
            wait(&child_doraise);
            // parent must not raise
            AlwaysAssert(!doraise, AipsError);
            AlwaysAssert(!child_doraise, AipsError);
        } else { // child process
            bool doraiseProcess=false;
            try {
                dummy.foobar();
            } catch (NotThreadSafeError& x) {
                doraiseProcess=true;
            }
            doraiseProcess=false;
            bool doraiseThread=false;
            auto t = thread([&](){ 
                try {
                    dummy.foobar(); 
                } catch (NotThreadSafeError& x) {
                    doraiseThread=true;
                }
            });
            t.join();
            _exit(doraiseProcess || doraiseThread);
        }
    }
    // check process safe thread safe
    {
        DummySafe dummy; 
        pid_t c_pid = fork();
        // fork should not fail
        AlwaysAssert(c_pid != -1, AipsError);
        if (c_pid > 0) { // parent
            bool doraise=false;
            try {
                dummy.foobar();
            } catch (NotThreadSafeError& x) {
                doraise=true;
            }
            int child_doraise;
            wait(&child_doraise);
            // parent must not raise
            AlwaysAssert(!doraise, AipsError);
            AlwaysAssert(!child_doraise, AipsError);
        } else { // child process
            bool doraiseProcess=false;
            try {
                dummy.foobar();
            } catch (NotThreadSafeError& x) {
                doraiseProcess=true;
            }
            bool doraiseThread=false;
            auto t = thread([&](){ 
                try {
                    dummy.foobar(); 
                } catch (NotThreadSafeError& x) {
                    doraiseThread=true;
                }
            });
            t.join();
            _exit(doraiseProcess || doraiseThread);
        }
    }
    cout << "OK" << endl;
    return 0;
}
