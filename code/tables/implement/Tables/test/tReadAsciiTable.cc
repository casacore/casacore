//# tAsciiFileIO.cc: Test program for the AsciiFileIO functions
//# Copyright (C) 1994,1995,1996
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

#include <aips/Tables/AsciiFileIO.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>

// <summary> Test program for the AsciiFileIO functions </summary>

// This program tests the functions in AsciiFileIO.h.
// It uses some files in the test directory. The directory of those
// files is given in argv[1].
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a (const String& dir);
void b (const String& dir);

main (int argc, char** argv) {
    try {
	String dir;
	if (argc > 1) {
	    dir = argv[1];
	}
	a (dir);
	b (dir);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    return 0;                           // exit with success status
}

void a (const String& dir)
{
    readAsciiTable (dir + "tAsciiFileIO.in_tab", "",
		    "tAsciiFileIO_tmp.data_tab");
    Table tab("tAsciiFileIO_tmp.data_tab");
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ROScalarColumn<Int>     coli (tab,"COLI");
    ROScalarColumn<float>   colf (tab,"COLF");
    ROScalarColumn<double>  cold (tab,"COLD");
    ROScalarColumn<Complex> colx (tab,"COLX");
    ROScalarColumn<Complex> colz (tab,"COLZ");
    ROScalarColumn<String>  cols (tab,"COLS");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << coli(i) << " " << colf(i) << " " << cold(i) << " "
	     << colx(i) << " " << colz(i) << " " << cols(i) << endl;
    }
}

void b (const String& dir)
{
    readAsciiTable (dir + "tAsciiFileIO.in_tkh", dir + "tAsciiFileIO.in_tkd",
		    "tAsciiFileIO_tmp", "tAsciiFileIO_tmp.data_tk");
    cout << endl;
    TableDesc tabdesc("tAsciiFileIO_tmp");
    tabdesc.show();
    cout << endl;
    Table tab("tAsciiFileIO_tmp.data_tk");
    const TableRecord& keys = tab.keywordSet();
    cout << keys.description();
    cout << "KEYI " << keys.asInt ("KEYI") << endl;
    cout << "KEYF " << keys.asfloat ("KEYF") << endl;
    cout << "KEYD " << keys.asdouble ("KEYD") << endl;
    cout << "KEYX " << keys.asComplex ("KEYX") << endl;
    cout << "KEYZ " << keys.asDComplex ("KEYZ") << endl;
    cout << "KEYS " << keys.asString ("KEYS") << endl;
    cout << "KEYIV " << keys.asArrayInt ("KEYIV") << endl;
    cout << "KEYFV " << keys.asArrayfloat ("KEYFV") << endl;
    cout << "KEYDV " << keys.asArraydouble ("KEYDV") << endl;
    cout << "KEYXC " << keys.asArrayComplex ("KEYXC") << endl;
    cout << "KEYZV " << keys.asArrayComplex ("KEYZV") << endl;
    cout << "KEYSV " << keys.asArrayString ("KEYSV") << endl;
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ROScalarColumn<Int>     coli (tab,"COLI");
    ROScalarColumn<float>   colf (tab,"COLF");
    ROScalarColumn<double>  cold (tab,"COLD");
    ROScalarColumn<Complex> colx (tab,"COLX");
    ROScalarColumn<Complex> colz (tab,"COLZ");
    ROScalarColumn<String>  cols (tab,"COLS");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << coli(i) << " " << colf(i) << " " << cold(i) << " "
	     << colx(i) << " " << colz(i) << " " << cols(i) << endl;
    }
}
