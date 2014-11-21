//# tSimOrdMap.cc: This program tests the SimOrdMap class
//# Copyright (C) 1993,1994,1995,1996,2000,2001
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

//# Includes

#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Containers/SimOrdMapIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
//# Forward Declarations

void doit();

// This test program creates an OrderedMap, which is written to
// disk and read back. It outputs to stdout. A script executing
// this test program makes a diff of the output and a reference output.

int main () {
    try {
       doit();
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } 
    return 0;          // successfully executed
}

void doit()
{
    AipsIO io("tSimOrdMap_tmp.data", ByteIO::New);
    Int i;
    i=-32768;
    SimpleOrderedMap<Int,Int> name(i);
    SimpleOrderedMap<String,Int> nams(32767,4);
    SimpleOrderedMap<String,Int> nam1(32767,2);
    i=1;
    cout << name.ndefined() << "end\n";
    name.define(2,10);
    name.define(-10,-2);
    name.define(100,108);
    name.define(30,38);
    name.define(31,39);
    name.define(32,40);
    name.define(33,41);
    name.define(34,42);
    name.define(35,43);
    name.define(36,44);
    AlwaysAssert(name.ok(),AipsError);
    cout << name.ndefined() << "end\n";
    i = name(2);
    cout << 2 << " " << i << "\n";
    i = name(-10);
    cout << -10 << " " << i << "\n";
    i = name(37);
    cout << 37 << " " << i << "\n";
    i = name(100);
    cout << 100 << " " << i << "\n";
    cout << name;
    i = name(33);
    cout << 33 << " " << i << "\n";
    i = name(101);
    cout << 101 << " " << i << "\n";
    i = name(-100);
    cout << -100 << " " << i << "\n";
    i = name(20);
    cout << 20 << " " << i << "\n";
    AlwaysAssert(name.ok(),AipsError);

    name(37) = 45;
    name.rename (-100,-10);
    name.rename (38,30);
    name.rename (30,36);
    try {
	name.rename (30,36);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    AlwaysAssert(name.ok(),AipsError);
    cout << name;

    nams.define("Probeer",10);
    nams.define("Nog eens",20);
    cout << "Probeer " << nams("Probeer") << endl;
    nams.define("Ger",1);
    nams.define("Brian",2);
    nams.define("Darrell",3);
    AlwaysAssert(nams.ok(),AipsError);
    cout << "Ger " << nams("Ger") << " Darrell " << nams("Darrell") << " Zomaar " << nams("Zomaar") << " Brian " << nams("Brian") << endl;
    cout << nams;
    io << nams;                             // write the map
    SimpleOrderedMap<String,Int> nam2 = nams;     // copy the map
    AlwaysAssert(nam2.ok(),AipsError);
    cout << nam2;                           // and show it
    nam2.remove("Ger");
    AlwaysAssert(nam2.ok(),AipsError);
    cout << nam2;
    nam2.define("ger",100);
    AlwaysAssert(nam2.ok(),AipsError);
    cout << nam2;
    cout << nams;
    nam1.define("A",1);
    nam1.define("B",2);
    AlwaysAssert(nam1.ok(),AipsError);
    cout << nam1;
    io << nam1;                             // write the changed map
    nam1 = nams;                            // get original again
    io << nam1;                             // and write that
    io.setpos (0);                          // reposition to BOF
    cout << nam1;
    io >> nam1;                             // read first map
    AlwaysAssert(nam1.ok(),AipsError);
    cout << nam1;
    io >> nam1;                             // read 2nd map
    AlwaysAssert(nam1.ok(),AipsError);
    cout << nam1;
    io >> nam1;                             // read 3rd map
    AlwaysAssert(nam1.ok(),AipsError);
    cout << nam1;
}
