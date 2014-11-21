//# tHashMapIter.cc: test of HashMapIter functionality
//# Copyright (C) 1996,2001,2002
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

#include <casacore/casa/Containers/HashMapIter.h>
#include <casacore/casa/Containers/HashMapIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/fstream.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdlib.h>
#include <cstring>              //# for strdup

#include <casacore/casa/namespace.h>
//# for GNU compile in trial:
//#
//#	 gmake tHashMapIter 'EXTRA_PGMRLIBS=-llapack -lblas -L/opt/SUNWspro/SC3.0.1/lib -lF77 -lsunmath'
//#

const char *word_file = "./tHashMap_tmp.words";  // "/usr/dict/words";

int main() {

    ifstream inFile( word_file );

    if ( !inFile ) {
	cerr << "Can't open \"" << word_file << "\"." << endl;
	exit(1);
    }

    HashMap<String,Int> hash(-1);

    Int x = 0;
    String str;
    int i = 0;
    while ( i++ < 350 && inFile >> str ) {

	hash.define(str,x++);

    }

    HashMapIter<String,Int> it1(hash);
    HashMapIter<String,Int> it2(hash);
    HashMapIter<String,Int> it3(hash);
    HashMapIter<String,Int> it4(hash);

    HashMapIter<String,Int> *it[4] = { &it1, &it2, &it3, &it4 };

    for ( int xx=0; xx < 100; xx++ )
        {
	it1++;it2++;it3++;it4++;
	}

    int dcnt;
#define DUMP_CURRENT 								\
    for ( dcnt=0; dcnt < 4; dcnt++ )						\
        cout << "(" << it[dcnt]->getKey() << "," << it[dcnt]->getVal() << ")";	\
    cout << endl;

#define RUN_THROUGH_ITERATORS									\
    {												\
    DUMP_CURRENT										\
    cout << "---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----" << endl;		\
												\
    for ( int yy=0; yy < 20; yy++ )								\
        {											\
	for ( int zz=0; zz < 4; zz++ )								\
	    {											\
	    it[zz]->remove(it[zz]->getKey());							\
	    DUMP_CURRENT									\
	    cout << "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --" << endl;	\
	    }											\
												\
	cout << "===========================================================" << endl;		\
	}											\
    }
    RUN_THROUGH_ITERATORS
    cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    DUMP_CURRENT
    cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    char *key = strdup(it1.getKey().chars());
    it1.remove(it1.getKey());
    DUMP_CURRENT
    cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    hash.define(key,x++);
    DUMP_CURRENT
    cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    while ( i++ < 1000 && inFile >> str ) {

	hash.define(str,x++);

    }
    RUN_THROUGH_ITERATORS
    cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;

    cout << hash << endl;
}
