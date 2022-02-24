//# tMultiton.cc: This program tests the WeakptrProcessGuard
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

#include <casacore/casa/Containers/WeakptrProcessGuard.h>
#include <thread>
#include <cassert>
#include <iostream>
#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

void assignval(casacore::WeakptrProcessGuard<int>& ag, std::size_t indx, int newval) {
    ag[indx] = newval;
}

bool assignvalth(casacore::WeakptrProcessGuard<int>& ag, std::size_t indx, int newval) {
    bool hasexcept = false;
    try {
        ag[indx] = newval;
    }
    catch (std::runtime_error &e) {
        hasexcept = true;
    }
    assert(hasexcept);
    return hasexcept;
}

bool assignvalpr(casacore::WeakptrProcessGuard<int>& ag, std::size_t indx, int newval) {
    bool hasexcept = false;
    try {
        ag[indx] = newval;
    }
    catch (std::runtime_error &e) {
        hasexcept = true;
    }
    return hasexcept;
}

int main()
{
    using namespace casacore;
    using namespace std;
    int* a = new int[1];
    
    //test use within parent successful
    WeakptrProcessGuard<int> ag(a, "int");
    assignval(ag, 1, 5);
    assert(ag[1] == 5); 
    int* b = ag.rawptr();
    assert(b==a);

    //test pass to thread
    thread athread(assignvalth, std::ref(ag), 1, 5);
    athread.join();

    // test pass to fork 
    pid_t cpid = fork();
    if (cpid == -1) { // fail
        throw std::runtime_error("Failed to fork");
    } else if (cpid == 0) { // child
        _exit(assignvalpr(ag, 1, 5));
    } else {
        int forkassignstatus;
        waitpid(cpid, &forkassignstatus, 0);
        assert(forkassignstatus != 0);
    }

    delete a;
    cout << "OK" << endl;
}