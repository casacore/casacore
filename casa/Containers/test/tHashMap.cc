//# tHashMap.cc: general test of the HashMap class
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
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/fstream.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdlib.h>

#include <casacore/casa/namespace.h>
#define DIAGNOSTICS 1
#define OUTPUT 1
#define COMPLETE 0

//# for GNU compile in trial:
//#
//#	 gmake tHashMap 'EXTRA_PGMRLIBS=-llapack -lblas -L/opt/SUNWspro/SC3.0.1/lib -lF77 -lsunmath'
//#

const char *word_file = "./tHashMap_tmp.words";  // "/usr/dict/words";

int main() {

    ifstream inFile( word_file );

    if ( !inFile ) {
	cerr << "Can't open \"" << word_file << "\"." << endl;
	exit(1);
    }

    HashMap<String,Int> hash(-1);

#if DIAGNOSTICS
    Block<String> block(25150);
#endif
    Int x = 0;
    String str;
    while ( inFile >> str ) {
#if DIAGNOSTICS
	block[x] = str;
#endif

#if COMPLETE
	if ( x % 2 )
	    hash.define(str,x++);
	else
	    hash(str) = x++;
#else
	hash.define(str,x++);
#endif

    }

#if DIAGNOSTICS
    if (! x ) {
	cerr << "No words..." << endl;
	exit(1);
    }

    srand((unsigned long) &x);
    for (int y=0; y < 500; y++) {
	Int index = rand() % x;
	if ( index != hash(block[index]) )
	    cout << "ERROR -- index(" << index << "), key(" << 
		block[index] << "), value(" << hash(block[index]) << ")" << endl;
    }

#if OUTPUT
    cout << "--- --- --- --- --- --- --- --- --- ---" << endl;
    cout << "number:            " << hash.ndefined() << endl;
    cout << "buckets used:      " << hash.usedBuckets() << endl;
    cout << "buckets available: " << hash.availableBuckets() << endl;
    cout << "buckets allocated: " << hash.allocBuckets() << endl;
    cout << "loading:           " << hash.loading() << endl;
    cout << "size:              " << hash.totalSize() << endl;
    cout << "--- --- --- --- --- --- --- --- --- ---" << endl;
    cout << "bucket utilization:" << endl;
    Block<uInt> b = hash.distribution();
    for (uInt i = 0; i < b.nelements();) {
	cout << "[" << i << "]:\t";
	for (int j = 0; j < 24 && i < b.nelements(); j++)
	    cout << b[i++] << " ";
	cout << endl;
    }
    cout << "--- --- --- --- --- --- --- --- --- ---" << endl;
#if COMPLETE
    int CNT = 1;
#endif
    ConstHashMapIter<String,Int> iter(hash);
    for (iter.toStart(); ! iter.atEnd(); iter++) {
#if COMPLETE
	if ( !(CNT % 100) ) {
	    CNT++;
	    for (ConstHashMapIter<String,Int> other_iter(iter);
		   ! other_iter.atEnd() && CNT % 100; 
		   other_iter++) {
		cout << other_iter.getKey() << " (" << other_iter.getVal() << ")" << endl;
		CNT++;
	    }
	    iter = other_iter;
	    if (iter.atEnd()) break;
	}
	cout << iter.getKey() << " (" << iter.getVal() << ")" << endl;
	CNT++;
#else
	cout << iter.getKey() << " (" << iter.getVal() << ")" << endl;
#endif
    }
    cout << "--- --- --- --- --- --- --- --- --- ---" << endl;
    
#endif

#endif
}
