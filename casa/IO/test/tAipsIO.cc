//# tAipsIO.cc: This program tests the AipsIO class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,2000,2001,2002
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

#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/RawIO.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This test program tests the AipsIO class.
// It writes all kind of stuff, reads it back and writes it to stdout.
// A script compares this output with a reference output file.
// It also tries to open/create in all kind of combinations and
// catches the exceptions thrown.
//
// If an argument is given to tAipsIO, no statements throwing exceptions
// will be executed. In this way no memory leaks should occur and a
// tool like TestCenter can be used to check for that.
// On the other hand using TestCenter for test coverage should include
// the exceptions.

//# Forward declaration.
void doit (Bool doExcp);
void doIO (Bool doExcp, Bool out, AipsIO&);
void doTry (AipsIO&);

int main (int argc, const char*[])
{
    try {
	doit (argc<2);
    } catch (AipsError& x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } 
    cout << "end" << endl;
    return 0;                       // successfully executed
}

void doit (Bool doExcp)
{
  {
    cout << "Test using normal files ..." << endl;
    AipsIO io("tAipsIO_tmp.data", ByteIO::New);      // open output file
    doIO (doExcp, True, io);
    io.close();
    io.open("tAipsIO_tmp.data");
    doIO (doExcp, False, io);
    // Now do some open calls; some of them are erronous which are caught.
    // Delete the file in case it exists.
    if (doExcp) {
	doTry (io);
    }
  }
  {
    cout << endl << "Test using MultiFile files ..." << endl;
    MultiFile mfile ("tAipsIO_tmp.mf", ByteIO::New);
    AipsIO io("tAipsIO_tmp.data", ByteIO::New, 1024, &mfile); // open output file
    doIO (doExcp, True, io);
    io.close();
    io.open("tAipsIO_tmp.data", ByteIO::Old, 1024, &mfile);
    doIO (doExcp, False, io);
    // Now do some open calls; some of them are erronous which are caught.
    // Delete the file in case it exists.
    if (doExcp) {
	doTry (io);
    }
  }

  cout << endl << "Test using MemoryIO ..." << endl;
  MemoryIO membuf;
  {
    RawIO rawio(&membuf);
    AipsIO io2(&rawio);
    doIO (doExcp, True, io2);
  }
  const uChar* iobuf = membuf.getBuffer();
  uInt bufleng = membuf.length();
  MemoryIO membuf2(iobuf, bufleng);
  {
    RawIO rawio(&membuf2);
    AipsIO io2(&rawio);
    doIO (doExcp, False, io2);
  }
}


void doIO (Bool doExcp, Bool out, AipsIO& io)
{
    Bool tbi,tbo;
    tbi = True;
    Char tci,tco;
    tci = -1;
    uChar tuci,tuco;
    tuci = 2;
    short tsi,tso;
    tsi = -3;
    unsigned short tusi,tuso;
    tusi = 4;
    int tii,tio;
    tii = -5;
    unsigned int tuii,tuio;
    tuii = 6;
    Int64 tli,tlo;
    tli = -7;
    uInt64 tuli,tulo;
    tuli = 8;
    float tfi,tfo;
    tfi = 3.15;
    double tdi,tdo;
    tdi = 6.897;
    Complex toi (1.98,-1000.45);
    Complex too;
    DComplex tdoi (93.7,-11.5);
    DComplex tdoo;
    
    int i,j;
    unsigned int len;
    String str("aa van Diepenaa");
    String a("bcdefg");
    String cp;
    int* ip;
    Int* lp;
    Int lo;
    String* cptr;
    String* sptr;
    String sap[6];
    String cap[6];
    static char s0[10] = "string000";
    static char s1[10] = "str1";
    static char s2[10] = "strin2";
    static char s3[10] = "stri3";
    static char s4[10] = "string45";
    static char s5[10] = "s";
    String ca[6];
    ca[0] = s0;
    ca[1] = s1;
    ca[2] = s2;
    ca[3] = s3;
    ca[4] = s4;
    ca[5] = s5;
    String sa[6];
    sa[0] = s1;
    sa[1] = s2;
    sa[2] = s3;
    sa[3] = s4;
    sa[4] = s5;
    sa[5] = s5;
    sa[5] += "abc";
    cout << sa[5] << endl;
    Bool barr[100];
    for (i=0; i<100; i++) {
	barr[i] = False;
	if (i%5 == 1) {
	    barr[i] = True;
	}
    }
    Bool barri[100];
    Int arr[250001];
    for (i=0; i<250001; i++) {
        arr[i] = i;
    }
    i=-32768;

    if (out) {
	cout << io.putstart ("abcdefghij",20) << endl;
	io << tbi << tci << tuci << tsi << tusi << tii << tuii;
	io << tli << tuli << tfi << tdi << toi << tdoi;
	io << 3 << i;
	io.put (1, &i);
	cout << io.putend() << endl;
	
	cout << io.putstart ("abcdefghij",20) << endl;
	io << "Ger";
	io << str(2,11);                                 // put a substring
	io.put (1, &i);
	cout << io.putend() << endl;
	
	cout << io.putstart ("abcdefghij",20) << endl;
	io << 1;
	cout << io.putstart ("klm",21) << endl;
	io << 2;
        cout << io.putstart ("nopq",22) << endl;
        io << 3;
        cout << io.putend() << endl;
	io << 4;
        cout << io.putstart ("r",23) << endl;
        io << 5;
        cout << io.putend() << endl;
	io << 6;
	cout << io.putend() << endl;
	io << 7;
	cout << io.putend() << endl;
	
	cout << io.putstart ("abcdefghij",20) << endl;
	io << 3 << i;
	io.put (1, &i);
	for (i=0; i<250000; i++) {
	    io << (arr[i]);
	}
	io.put (250000, arr);
	io.put (100, barr);
	io.put (250000, arr);
	io.put (100, barr);
	io.put (5, ca);
	io.put (5, ca);
	io.put (5, sa);
	io.put (5, sa);
	cout << io.putend() << endl;
	cout << "Length=" << io.getpos() << endl;      // total length
	return;
    }
	
    cout << io.getNextType() << " " << io.getNextType() << " ";
    cout << "Version=" << io.getstart ("abcdefghij") << endl;
    // Check if all data types are read back correctly.
    io>>tbo>>tco>>tuco>>tso>>tuso>>tio>>tuio;
    io>>tlo>>tulo;
    io>>tfo>>tdo;
    io>>too;
    io>>tdoo;
    if (tbo!=tbi) cout << "Bool "<<tbi<<" "<<tbo<<endl;
    if (tco!=tci) cout << "Char "<<tci<<" "<<tco<<endl;
    if (tuco!=tuci) cout << "uChar "<<tuci<<" "<<tuco<<endl;
    if (tso!=tsi) cout << "short "<<tsi<<" "<<tso<<endl;
    if (tuso!=tusi) cout << "ushort "<<tusi<<" "<<tuso<<endl;
    if (tio!=tii) cout << "int "<<tii<<" "<<tio<<endl;
    if (tuio!=tuii) cout << "uint "<<tuii<<" "<<tuio<<endl;
    if (tlo!=tli) cout << "long "<<tli<<" "<<tlo<<endl;
    if (tulo!=tuli) cout << "ulong "<<tuli<<" "<<tulo<<endl;
    if (tfo!=tfi) cout << "float "<<tfi<<" "<<tfo<<endl;
    if (tdo!=tdi) cout << "double "<<tdi<<" "<<tdo<<endl;
    if (too!=toi) cout << "Complex "<<toi<<" "<<too<<endl;
    if (tdoo!=tdoi) cout << "DComplex "<<tdoi<<" "<<tdoo<<endl;
    io >> i >> j;
    cout << i << " " << j << endl;
    io >> len;
    io.get (len, &i);
    cout << len << " " << i << endl;
    cout << io.getend() << endl;

    cout << "Version=" << io.getstart ("abcdefghij") << endl;
    io >> a;
    io >> cp;
    cout << a << cp << endl;
    io.getnew (len, ip);
    cout << len << " " << ip[0] << endl;
    cout << io.getend() << endl;
    delete [] ip;

    cout << "Version=" << io.getstart ("abcdefghij") << endl;
    io >> i;
    cout << i << endl;
      cout << io.getNextType() << " " << io.getNextType() << " ";
      cout << "Version=" << io.getstart ("klm") << endl;
      io >> i;
      cout << i << endl;
        cout << "Version=" << io.getstart ("nopq") << endl;
        io >> i;
        cout << i << endl;
        cout << io.getend() << endl;
      io >> i;
      cout << i << endl;
        cout << io.getNextType() << " ";
        cout << "Version=" << io.getstart ("r") << endl;
        io >> i;
        cout << i << endl;
        cout << io.getend() << endl;
      io >> i;
      cout << i << endl;
      cout << io.getend() << endl;
    io >> i;
    cout << i << endl;
    cout << io.getend() << endl;

    cout << "Version=" << io.getstart ("abcdefghij") << endl;
    io >> i >> j;
    cout << i << " " << j << endl;
    io >> len;
    io.get (len, &i);
    cout << len << " " << i << endl;
    for (i=0; i<250000; i++) {
	io >> lo;
        if (lo != i) {
	    cout << lo << " " << i << endl;
        }
    }
    io >> len;
    cout << len << endl;
    io.get (len, &arr[1]);
    for (i=0; i<250000; i++) {
	if (arr[i+1] != i) {
	    cout << i << " not equal" << endl;
	}
    }
    io >> len;
    cout << len << endl;
    io.get (len, barri);
    for (i=0; i<100; i++) {
	if (barri[i] != barr[i]) {
	    cout << i << " barri not equal" << endl;
	}
    }
    len = 0;
    io.getnew (len, lp);
    cout << len << endl;
    for (i=0; i<250000; i++) {
	if (lp[i] != i) {
	    cout << i << " not equal" << endl;
	}
    }
    delete [] lp;
    Bool* barrp;
    io.getnew (len, barrp);
    cout << len << endl;
    for (i=0; i<100; i++) {
	if (barrp[i] != barr[i]) {
	    cout << i << " barrp not equal" << endl;
	}
    }
    delete [] barrp;
    len = 0;
    io >> len;
    cout << len << endl;
    io.get (len, &cap[1]);
    len = 0;
    io >> len;
    cout << len << endl;
    io.get (len, &sap[1]);
    len = 0;
    io.getnew (len, sptr);
    cout << len << endl;
    len = 0;
    io.getnew (len, cptr);
    cout << len << endl;
    for (i=0; i<int(len); i++) {
	cout << cap[i+1] << " " << sap[i+1] << " " << sptr[i]
	     << " " << cptr[i] << endl;
    }
    delete [] sptr;
    delete [] cptr;
    if (doExcp) {
	try {
	    io >> len;                            // read beyond object
	} catch (AipsError& x) {
	    cout << x.getMesg() << endl;
	} 
    }
    cout << io.getend() << endl;
    if (doExcp) {
	try {
	    io.getstart ("aa");                   // read-error (past EOF)
	} catch (AipsError& x) {
	    cout << x.getMesg() << endl;
	} 
    }

// So, the entire file has been read back.
    cout << "Length=" << io.getpos() << endl;
    io.setpos(1);
    if (doExcp) {
	try {
	    io.getstart ("aa");                   // no magic number
	} catch (AipsError& x) {
	    cout << x.getMesg() << endl;
	} 
    }
    io.setpos(0);
    if (doExcp) {
	try {
	    io.getstart ("aa");                   // invalid object type
	} catch (AipsError& x) {
	    cout << x.getMesg() << endl;
	} 
    }
}


void doTry (AipsIO& io)
{
    cout << "TryErrOpen" << endl;
    try {
	io.open ("tAipsIO_tmp.aa");                   // still open
    } catch (AipsError& x) {
	cout << x.getMesg() << endl;
    } 
    io.close ();                                 // now close the file

    cout << "TryAlrCls" << endl;
    io.close ();

    try {
	io.open ("tAipsIO_tmp.aa", ByteIO::Delete);
	io.close ();
    } catch (AipsError& x) {
    } 
    
    cout << "TryErrIn" << endl;
    try {
	io.open ("tAipsIO_tmp.aa");
	io.close ();
    } catch (AipsError& x) {
	cout << x.getMesg() << endl;
    } 

    cout << "TryScr" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Scratch);
    io.close ();

    cout << "TryErrDel" << endl;
    try {
	io.open ("tAipsIO_tmp.aa", ByteIO::Delete);
	io.close ();
    } catch (AipsError& x) {
	cout << x.getMesg() << endl;
    } 

    cout << "TryOut" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::New);
    io.close();

    cout << "TryApp" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Append);
    io.close();

    cout << "TryUpd" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Update);
    io.close();

    cout << "TryErrNorep" << endl;
    try {
	io.open ("tAipsIO_tmp.aa", ByteIO::NewNoReplace);
	io.close ();
    } catch (AipsError& x) {
	cout << x.getMesg() << endl;
    } 

    cout << "TryDel" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Delete);
    io.close();

    cout << "TryErrUpd" << endl;
    try {
	io.open ("tAipsIO_tmp.aa", ByteIO::Update);
	io.close ();
    } catch (AipsError& x) {
	cout << x.getMesg() << endl;
    } 

    cout << "TryNew" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::New);
    io.close();

    cout << "TryIn" << endl;
    io.open ("tAipsIO_tmp.aa",ByteIO::Old);
    io.close();

    cout << "TryScr" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Scratch);
    io.close();

    cout << "TryOut" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::New);
    io.close();

    cout << "TryApp" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Append);
    io.close();

    cout << "TryDel" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Delete);
    io.close();

    cout << "TryNorep" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::NewNoReplace);
    io.close();

    cout << "TryUpd" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::Update);
    io.close();

    cout << "TryNew" << endl;
    io.open ("tAipsIO_tmp.aa", ByteIO::New);
    io.close();
}
