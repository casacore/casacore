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
#include <condition_variable>

class Dummy : public casacore::LockableObject<std::recursive_mutex> {
public: 
    Dummy() : 
    casacore::LockableObject<std::recursive_mutex>() {}
    Dummy& operator=(const Dummy& other) { 
        if (&other != this) {}
        return *this; 
    }
    Dummy(Dummy&& other) {}
    std::recursive_mutex& object_mutex() const {
        return casacore::LockableObject<std::recursive_mutex>::object_mutex();
    }
};

int main() {
    using namespace std;
    using namespace casacore;
    {
        // check succeed lock all
        {
            cout<< "--- LockAll variable number object constructor test ---"<<endl;
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                cout<<"parent aquired locks"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                    cout<<"thread2 failed to lock"<<endl;
                });
                t2.join();
                std::thread t3([&d3](){
                    AlwaysAssert(!d3.object_mutex().try_lock(), AipsError);
                    cout<<"thread3 failed to lock"<<endl;
                });
                t3.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
                d2.object_mutex().unlock();
                cout<<"thread2 locked and unlocked success"<<endl;
            });
            t2.join();
            std::thread t3([&d3](){
                AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
                d3.object_mutex().unlock();
                cout<<"thread3 locked and unlocked success"<<endl;
            });
            t3.join();
        }
        // check succeed lock all
        {
            cout<< "--- LockAll variable number object constructor test (single obj) ---"<<endl;
            Dummy d1;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                cout<<"parent aquired locks"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
        }
        // check succeed lock all
        {
            cout<< "--- LockAll variable number object constructor test (two obj) ---"<<endl;
            Dummy d1;
            Dummy d2;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                cout<<"parent aquired locks"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                    cout<<"thread2 failed to lock"<<endl;
                });
                t2.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
                d2.object_mutex().unlock();
                cout<<"thread2 locked and unlocked success"<<endl;
            });
            t2.join();
        }
        // single object constructor lock successful test
        {
            cout<< "--- LockAll single object constructor test ---"<<endl;
            Dummy d1;
            {
                LockAll<std::recursive_mutex> lg(d1, 3);
                cout<<"parent aquired locks"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
        }
        // two object constructor lock successful test
        {
            cout<< "--- LockAll two object constructor test ---"<<endl;
            Dummy d1;
            Dummy d2;
            {
                LockAll<std::recursive_mutex> lg(d1, d2, 3);
                cout<<"parent aquired locks"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                    cout<<"thread2 failed to lock"<<endl;
                });
                t2.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
                d2.object_mutex().unlock();
                cout<<"thread2 locked and unlocked success"<<endl;
            });
            t2.join();
        }
        // check move construction
        {
            cout<< "--- LockAll move constructor test ---"<<endl;
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                cout<<"parent aquired locks"<<endl;
                LockAll<std::recursive_mutex> lg2(std::move(lg));
                cout<<"parent handed off locks to partner"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                    cout<<"thread2 failed to lock"<<endl;
                });
                t2.join();
                std::thread t3([&d3](){
                    AlwaysAssert(!d3.object_mutex().try_lock(), AipsError);
                    cout<<"thread3 failed to lock"<<endl;
                });
                t3.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
                d2.object_mutex().unlock();
                cout<<"thread2 locked and unlocked success"<<endl;
            });
            t2.join();
            std::thread t3([&d3](){
                AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
                d3.object_mutex().unlock();
                cout<<"thread3 locked and unlocked success"<<endl;
            });
            t3.join();
        }
        // check move assignment
        {
            cout<< "--- LockAll move test ---"<<endl;
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                LockAll<std::recursive_mutex> lg(vecDummy, 3);
                cout<<"parent aquired locks"<<endl;
                vector<const LockableObject<std::recursive_mutex>*> vecDummy2;
                LockAll<std::recursive_mutex> lg2(vecDummy2, 0);
                cout<<"parent handed off locks to partner"<<endl;
                std::thread t1([&d1](){
                    AlwaysAssert(!d1.object_mutex().try_lock(), AipsError);
                    cout<<"thread1 failed to lock"<<endl;
                });
                t1.join();
                std::thread t2([&d2](){
                    AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                    cout<<"thread2 failed to lock"<<endl;
                });
                t2.join();
                std::thread t3([&d3](){
                    AlwaysAssert(!d3.object_mutex().try_lock(), AipsError);
                    cout<<"thread3 failed to lock"<<endl;
                });
                t3.join();
            }
            //all must be lockable again after the guard exits scope
            cout<<"parent released all locks"<<endl;
            std::thread t1([&d1](){
                    AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                    d1.object_mutex().unlock();
                    cout<<"thread1 locked and unlocked success"<<endl;
                });
            t1.join();
            std::thread t2([&d2](){
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
                d2.object_mutex().unlock();
                cout<<"thread2 locked and unlocked success"<<endl;
            });
            t2.join();
            std::thread t3([&d3](){
                AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
                d3.object_mutex().unlock();
                cout<<"thread3 locked and unlocked success"<<endl;
            });
            t3.join();
        }
        // check fail not lock all
        {
            cout<< "--- LockAll deadlock avoidance test ---"<<endl;
            Dummy d1;
            Dummy d2;
            Dummy d3;
            vector<const LockableObject<std::recursive_mutex>*> vecDummy;
            vecDummy.push_back(&d1);
            vecDummy.push_back(&d2);
            vecDummy.push_back(&d3);
            {
                bool has_locked = false;
                bool has_checked = false;
                std::condition_variable cv;
                std::mutex cv_lock;
                std::thread t([&d2, &cv_lock, &has_locked, &cv, &has_checked](){
                    // lock d2 then notify parent to continue trying to aquire a lock on d2
                    {
                        std::lock_guard<std::recursive_mutex> lg(d2.object_mutex());
                        {
                            std::unique_lock<std::mutex> lk(cv_lock);
                            cout << "thread set lock snare" << endl;
                            has_locked = true;
                            lk.unlock();
                            cv.notify_one();
                        }
                        // now wait for the parent to check that it fails to aquire a lock on d2
                        {
                            std::unique_lock<std::mutex> lk(cv_lock);
                            cv.wait(lk, [&has_checked]{ return has_checked; });    
                        }
                        // lg release lock on d2 upon termination of scope
                    }
                    cout << "thread removed lock snare" << endl;
                });
                {
                    std::unique_lock<std::mutex> lk(cv_lock);
                    // wait for thread to spin up and aquire d2 before continuing
                    cv.wait(lk, [&has_locked]{ return has_locked; });
                    has_locked=false; // will only notify thread to unlock d2 when we checked
                    // we can't aquire the lock on d2 and (d1 and d3 remain unlocked)
                }
                {
                    cout << "parent walking into lock snare" << endl;
                    bool dothrow=false;
                    try {
                        LockAll<std::recursive_mutex> lg(vecDummy, 1);
                    } catch (casacore::LockAllAttemptsExceeded& x) {
                        dothrow=true;
                        cout << "parent caught in lock snare" << endl;
                        AlwaysAssert(d1.object_mutex().try_lock(), AipsError);
                        d1.object_mutex().unlock();
                        AlwaysAssert(!d2.object_mutex().try_lock(), AipsError);
                        AlwaysAssert(d3.object_mutex().try_lock(), AipsError);
                        d3.object_mutex().unlock();
                        cout << "parent not aquired any other locks" << endl;
                    }   
                    AlwaysAssert(dothrow, AipsError);
                    // notify t to continue and release the lock on d2, we still have cv through lk at this point
                    std::unique_lock<std::mutex> lk(cv_lock);
                    has_checked=true;
                    lk.unlock();
                    cv.notify_one();
                }
                // finally check lg in t has released lock upon scope end
                t.join();
                AlwaysAssert(d2.object_mutex().try_lock(), AipsError);
                d2.object_mutex().unlock();
            }
        }
        
    }
    cout<< "--- LockAll test suite complete ---"<<endl;
    cout << "OK" << endl;
    return 0;
}