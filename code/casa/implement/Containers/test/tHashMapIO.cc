//# tHashMapIO.cc: test of HashMap IO
//# Copyright (C) 1996,1999
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

#include <trial/Containers/HashMapIO.h>
#include <aips/Utilities/String.h>
#include <fstream.h>
#include <stdlib.h>
#include <aips/IO/AipsIO.h>

#if MAPIO_COMPATIBILITY
#include <aips/Containers/OrderedMap.h>
#include <aips/Containers/MapIO.h>
#include <aips/Exceptions/Error.h>
#endif
#if defined(AIPS_STUPID_SUN)
#undef AIPS_STUPID_SUN
#endif

//# for GNU compile in trial:
//#
//#	 gmake tHashMapIO 'EXTRA_PGMRLIBS=-llapack -lblas -L/opt/SUNWspro/SC3.0.1/lib -lF77 -lsunmath'
//#

const char *word_file = "./words"; // "/usr/dict/words";

main() {

    ifstream inFile( word_file );
    ofstream hmos1( "tHashMapIO_hm.1st" );
    ofstream hmos2( "tHashMapIO_hm.2nd" );
#if MAPIO_COMPATIBILITY
    ofstream omos1( "tHashMapIO_om.1st" );
    ofstream hmos3( "tHashMapIO_hm.3rd" );
#endif

    AipsIO hmo("tHashMapIO_hm.out", ByteIO::New);
#if MAPIO_COMPATIBILITY
    AipsIO omo("tHashMapIO_om.out", ByteIO::New);
#endif

    if ( !inFile ) {
	cerr << "Can't open \"" << word_file << "\"." << endl;
	exit(1);
    }

    HashMap<String,Int> hash(-1);
#if MAPIO_COMPATIBILITY
    OrderedMap<String,Int> om(-1);
#endif

    Int x = 0;
    String str;
    int i = 0;
    while ( i++ < 350 && inFile >> str ) {

#if MAPIO_COMPATIBILITY
	om.define(str,x);
#endif
	hash.define(str,x++);

    }

    hmos1 << hash << endl;
    hmo << hash;
    hmo.close();
    HashMap<String,Int> hash2(-1);
    AipsIO ai("tHashMapIO_hm.out", ByteIO::Old);
    ai >> hash2;
    hmos2 << hash2 << endl;;
#if MAPIO_COMPATIBILITY
    omo << om;
    omo.close();
    HashMap<String,Int> hash3(-1);
    AipsIO ai2("tHashMapIO_om.out", ByteIO::Old);
    ai2 >> hash3;
    hmos3 << hash3 << endl;
#endif

}
