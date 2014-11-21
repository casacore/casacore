//# tQueue.cc: Test program for the Queue class
//# Copyright (C) 1995,2000,2001
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

#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

#include <casacore/casa/Containers/Queue.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    Queue<Int> qi;                // Queue::Queue()
    AlwaysAssertExit(qi.nelements() == 0);  // Queue::nelements()
    qi.enqueue(1);                // Queue::enqueue()
    qi(2);                        // Queue::operator()(const T &)
    qi(3);
    qi.enqueue(4);
    qi.enqueue(5);
    AlwaysAssertExit(qi.nelements() == 5);
    AlwaysAssertExit(qi() == 1);            // Queue::operator();
    Int val;
    qi.dequeue(val);              // Queue::dequeue();
    AlwaysAssertExit(val == 2);
    AlwaysAssertExit(qi.nelements() == 3);
    qi = qi;
    AlwaysAssertExit(qi.nelements() == 3);
    Queue<Int> qi2(qi);           // Queue::Queue(const Queue<T> &other);
    AlwaysAssertExit(qi2.nelements() == 3 && qi2() == 3 && qi2.nelements() == 2);
    qi2 = qi;                     // Queue::operator=
    AlwaysAssertExit(qi2.nelements() == 3 && qi2() == 3 && qi2.nelements() == 2);
    qi2.clear();                  // Queue::clear()
    AlwaysAssertExit(qi2.nelements() == 0);
    qi.compress();                // Queue::compress()
    AlwaysAssertExit(qi.nelements() == 3 && qi() == 3 && qi.nelements() == 2);
    AlwaysAssertExit(qi() == 4 && qi() == 5 && qi.nelements() == 0);
    Bool caught = False;
    try {
	(void) qi(); // Should cause an exception - queue is empty
    } catch (AipsError x) {
	caught = True;
    } 
    AlwaysAssertExit(caught);
				  // Queue::~Queue() implicit

    cout << "OK" << endl;
    return 0;
}
