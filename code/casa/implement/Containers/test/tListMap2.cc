//# tListMap2.cc: This program tests the ListMap class
//# Copyright (C) 1993,1994,1995,1996,1998
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

#include <iostream.h>
#include <aips/Containers/ListMap.h>
#include <aips/Containers/ListMapIO.h>
#include <aips/IO/AipsIO.h>

// This test program creates an OrderedMap, which is written to
// disk and read back. It outputs to stdout. A script executing
// this test program makes a diff of the output and a reference output.

main () {
    AipsIO io("tListMap2_tmp", ByteIO::New);
    Int i;
    i=-32768;
    ListMap<Int,Int> Name(i);
    MapIter<Int,Int> name = Name;
    ListMap<String,Int> Nams(32767);
    MapIter<String,Int> nams = Nams;
    ListMap<String,Int> Nam1(32767);
    MapIter<String,Int> nam1 = Nam1;

    i=1;
    cout << i << name.atEnd() << "end\n";
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
    cout << name.atEnd() << "end\n";
    i = name(2);
    cout << 2 << " " << i << "\n";
    i = name(-10);
    cout << -10 << " " << i << "\n";
    i = name(37);
    cout << 37 << " " << i << "\n";
    i = name(100);
    cout << 100 << " " << i << "\n";
#if ! defined(__GNUG__)
    cout << Name << endl;
#else
    cout << *((Map<Int,Int>*) &Name) << endl;
#endif
    i = name(33);
    cout << 33 << " " << i << "\n";
    i = name(101);
    cout << 101 << " " << i << "\n";
    i = name(-100);
    cout << -100 << " " << i << "\n";
    i = name(20);
    cout << 20 << " " << i << "\n";

//  name.define(37,45);
    name(37) = 45;

    nams.define("Probeer",10);
    nams.define("Nog eens",20);
    cout << "Probeer " << nams("Probeer") << endl;
    nams.define("Ger",1);
    nams.define("Brian",2);
    nams.define("Darrell",3);
    cout << "Ger " << nams("Ger") << " Darrell " << nams("Darrell") << " Zomaar " << nams("Zomaar") << " Brian " << nams("Brian") << endl;
#if ! defined(__GNUG__)
    cout << Nams << endl;
#else
    cout << *((Map<String,Int>*) &Nams) << endl;
#endif
#if ! defined(__GNUG__)
    io << nams;                             // write the map
#else
    io << *((ConstMapIter<String,Int>*) &nams);
#endif
    ListMap<String,Int> Nam2 = Nams;        // copy the map
    MapIter<String,Int> nam2 = Nam2;        // copy the map
#if ! defined(__GNUG__)
    cout << Nam2 << endl;                   // and show it
#else
    cout << *((Map<String,Int>*) &Nam2) << endl;
#endif
    nam2.remove("Ger");
#if ! defined(__GNUG__)
    cout << Nam2 << endl;
#else
    cout << *((Map<String,Int>*) &Nam2) << endl;
#endif
    nam2.define("ger",100);
#if ! defined(__GNUG__)
    cout << nam2 << endl;
#else
    cout << *((ConstMapIter<String,Int>*) &nam2) << endl;
#endif
#if ! defined(__GNUG__)
    cout << Nams << endl;
#else
    cout << *((Map<String,Int>*) &Nams) << endl;
#endif
    nam1.define("A",1);
    nam1.define("B",2);
#if ! defined(__GNUG__)
    cout << Nam1 << endl;
#else
    cout << *((Map<String,Int>*) &Nam1) << endl;
#endif
#if ! defined(__GNUG__)
    io << nam1;                             // write the changed map
#else
    io << *((ConstMapIter<String,Int>*) &nam1);
#endif
    nam1 = nams;                            // get original again
#if ! defined(__GNUG__)
    io << nam1;                             // and write that
#else
    io << *((ConstMapIter<String,Int>*) &nam1);
#endif
    io.setpos (0);                          // reposition to BOF
#if ! defined(__GNUG__)
    cout << Nams << endl;
#else
    cout << *((Map<String,Int>*) &Nams) << endl;
#endif
    io >> nam1;                             // read first map
#if ! defined(__GNUG__)
    cout << Nams << endl;
#else
    cout << *((Map<String,Int>*) &Nams) << endl;
#endif
    io >> nam1;                             // read 2nd map
#if ! defined(__GNUG__)
    cout << Nams << endl;
#else
    cout << *((Map<String,Int>*) &Nams) << endl;
#endif
    io >> nam1;                             // read 3rd map
#if ! defined(__GNUG__)
    cout << Nams << endl;
#else
    cout << *((Map<String,Int>*) &Nams) << endl;
#endif
    return 0;                               // exit with success status
}
