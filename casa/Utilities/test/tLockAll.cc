//# tLockAll.cc: Test program for general purpose simultaneous locking pattern
//# WARNING:: mostly a problem reproducer at the moment
//# Table system is NOT threadsafe
//# Copyright (C) 2019
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

#include <iostream>
#include <casacore/casa/Utilities/LockAll.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <thread>

class Dummy : public casacore::LockableObject<std::recursive_mutex> {
public: 
    Dummy() : 
    casacore::LockableObject<std::recursive_mutex>() {}
    Dummy& operator=(const Dummy& other) { return *this; }
    Dummy(Dummy&& other) {}
    std::recursive_mutex& object_mutex() const {
        return casacore::LockableObject<std::recursive_mutex>::object_mutex();
    }
};

int main() {
    using namespace std;
    using namespace casacore;
    size_t num_hardware_threads = std::thread::hardware_concurrency(); 

    // must be run on multithreaded machine
    AlwaysAssert(num_hardware_threads > 1, AipsError);
    {
        // check succeed lock all
        {
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                });
                t2.join();
                std::thread t3([&d3](){
                    AlwaysAssert(!d3.object_mutex().try_lock(), AipsError);
                });
                t3.join();
            }
            //all must be lockable again after the guard exits scope
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
            });
            t2.join();
            std::thread t3([&d3](){
                AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
            });
            t3.join();
        }
        // check move construction
        {
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                LockAll<std::recursive_mutex> lg2(std::move(lg));
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                });
                t2.join();
                std::thread t3([&d3](){
                    AlwaysAssert(!d3.object_mutex().try_lock(), AipsError);
                });
                t3.join();
            }
            //all must be lockable again after the guard exits scope
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
            });
            t2.join();
            std::thread t3([&d3](){
                AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
            });
            t3.join();
        }
        // check move assignment
        {
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                vector<const LockableObject<std::recursive_mutex>*> vecDummy2;
                LockAll<std::recursive_mutex> lg2(vecDummy2, 0);
                lg2 = std::move(lg);
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                });
                t2.join();
                std::thread t3([&d3](){
                    AlwaysAssert(!d3.object_mutex().try_lock(), AipsError);
                });
                t3.join();
            }
            //all must be lockable again after the guard exits scope
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
            });
            t2.join();
            std::thread t3([&d3](){
                AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
            });
            t3.join();
        }
        // check fail not lock all
        {
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                std::thread t([&d2](){
                    std::lock_guard<std::recursive_mutex> lg(d2.object_mutex());
                    sleep(6); // give more time for parent to try lock
                });
                sleep(3); // give some time to spin up
                bool dothrow=false;
                try {
                    LockAll<std::recursive_mutex> lg(vecDummy, 1);
                } catch (casacore::LockAllAttemptsExceeded& x) {
                    dothrow=true;
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                    AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
                    d3.object_mutex().unlock();
                }   
                AlwaysAssert(dothrow, AipsError);
                t.join();
            }
        }
        
    }

    cout << "OK" << endl;
    return 0;
}