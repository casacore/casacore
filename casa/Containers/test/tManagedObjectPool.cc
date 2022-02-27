//# tMultiton.cc: This program tests the ManagedObjectPool class
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

#include <casacore/casa/Containers/ManagedObjectPool.h>
#include <string>
#include <iostream>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

class dummy {
private:
    static size_t alive;
    static size_t objidcntr;
    int a;
    bool b;
    std::string c;
    size_t objid;
public:
    dummy(int a = 5, bool b = true, std::string c = "ok") : 
        a(a), b(b), c(c), objid(dummy::objidcntr++) { ++dummy::alive; }
    dummy(const dummy & rhs) = delete;
    ~dummy() { --dummy::alive; }
    dummy& operator=(const dummy & rhs) = delete;
    static bool alldead() { return dummy::alive == 0; }
    static size_t size () { return dummy::alive; } 
    std::string to_string() { return std::to_string(a) + (b ? "_yes_" : "_no_") + c; }
    bool operator==(const dummy & rhs) { return this->objid == rhs.objid; }
};
size_t dummy::alive = 0;
size_t dummy::objidcntr = 0;

int main() {
    using namespace std;
    using namespace casacore;
    using MOP = ManagedObjectPool<string, dummy>;
    {
        MOP pool;
        pool.constructObject("1", 1,false,"bla");
        AlwaysAssert(dummy::size() == 1, AipsError);
        AlwaysAssert(pool["1"].to_string() == "1_no_bla", AipsError);
        AlwaysAssert(pool.contains("1"), AipsError);
        AlwaysAssert(!pool.contains("NOTEXIST"), AipsError);
        bool raises = false;
        try { pool.erase("3"); }
        catch (std::invalid_argument&) {raises = true; }
        AlwaysAssert(raises, AipsError);
        raises = false;
        try { pool.constructObject("1"); }
        catch (std::invalid_argument&) {raises = true; }
        AlwaysAssert(raises, AipsError);
        AlwaysAssert(pool.size() == 1, AipsError);
        AlwaysAssert(!pool.empty(), AipsError);
        // test move operators
        MOP pool2(std::move(pool));
        AlwaysAssert(pool.empty(), AipsError);
        AlwaysAssert(pool2.size() == 1, AipsError);
        AlwaysAssert(pool2["1"].to_string() == "1_no_bla", AipsError);
        MOP pool3 = std::move(pool2);
        AlwaysAssert(pool2.empty(), AipsError);
        AlwaysAssert(pool3.size() == 1, AipsError);
        AlwaysAssert(pool3["1"].to_string() == "1_no_bla", AipsError);
        pool3.constructObject("2");
        AlwaysAssert(pool3.size() == 2, AipsError);
        AlwaysAssert(pool3["2"].to_string() == "5_yes_ok", AipsError);
        pool3.clear();
        AlwaysAssert(dummy::size() == 0, AipsError);
        pool3.constructObject("3", 6);
        AlwaysAssert(pool3["3"].to_string() == "6_yes_ok", AipsError);
        pool3.erase("3");
        AlwaysAssert(pool3.empty(), AipsError);
        dummy& v4 = pool3.constructObject("4");
        AlwaysAssert(pool3.contains("4"), AipsError);
        dummy& v4ref = pool3.checkConstructObject("4");
        AlwaysAssert(v4 == v4ref, AipsError);
        AlwaysAssert(&v4 == &v4ref, AipsError);
        pool3.clear();
        AlwaysAssert(pool3.empty(), AipsError);
    }
    // test don't leak
    {
        AlwaysAssert(dummy::size() == 0, AipsError);
        {
            MOP pool;
            pool.constructObject("1");
            pool.constructObject("2");
            pool.constructObject("3");
            AlwaysAssert(dummy::size() == 3, AipsError);
        }
        AlwaysAssert(dummy::size() == 0, AipsError);
    }
    cout<<"OK"<<endl;
    return 0;
}