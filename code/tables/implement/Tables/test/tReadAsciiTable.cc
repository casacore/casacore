//# tReadAsciiTable.cc: Test program for the ReadAsciiTable functions
//# Copyright (C) 1994,1995,1996,1999,2000
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

#include <aips/Tables/ReadAsciiTable.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>

// <summary> Test program for the ReadAsciiTable functions </summary>

// This program tests the functions in ReadAsciiTable.h.
// It uses some files in the test directory. The directory of those
// files is given in argv[1].
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a (const String& dir);
void a1 (const String& dir);
void b (const String& dir);

main (int argc, char** argv) {
    try {
	String dir;
	if (argc > 1) {
	    dir = argv[1];
	}
	a (dir);
	a1 (dir);
	b (dir);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void a (const String& dir)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tab", "",
				     "tReadAsciiTable_tmp.data_tab");
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    Table tab("tReadAsciiTable_tmp.data_tab");
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

void a1 (const String& dir)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tah", "",
				     "tReadAsciiTable_tmp.data_tah", True);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    Table tab("tReadAsciiTable_tmp.data_tah");
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ROScalarColumn<Int>      col1 (tab,"Column1");
    ROScalarColumn<double>   col2 (tab,"Column2");
    ROScalarColumn<double>   col3 (tab,"Column3");
    ROScalarColumn<double>   col4 (tab,"Column4");
    ROScalarColumn<double>   col5 (tab,"Column5");
    ROScalarColumn<double>   col6 (tab,"Column6");
    ROScalarColumn<double>   col7 (tab,"Column7");
    ROScalarColumn<String>   col8 (tab,"Column8");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << col1(i) << " " << col2(i) << " " << col3(i) << " "
	     << col4(i) << " " << col5(i) << " " << col6(i) << " "
	     << col7(i) << " " << col8(i) << endl;
    }
}

void b (const String& dir)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tkh",
				     dir + "tReadAsciiTable.in_tkd",
				     "tReadAsciiTable_tmp",
				     "tReadAsciiTable_tmp.data_tk");
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    cout << endl;
    TableDesc tabdesc("tReadAsciiTable_tmp");
    tabdesc.show();
    cout << endl;
    Table tab("tReadAsciiTable_tmp.data_tk");
    const TableRecord& keys = tab.keywordSet();
    cout << keys.description();
    cout << "KEYS " << keys.asShort ("KEYS") << endl;
    cout << "KEYI " << keys.asInt ("KEYI") << endl;
    cout << "KEYF " << keys.asfloat ("KEYF") << endl;
    cout << "KEYD " << keys.asdouble ("KEYD") << endl;
    cout << "KEYX " << keys.asComplex ("KEYX") << endl;
    cout << "KEYZ " << keys.asComplex ("KEYZ") << endl;
    cout << "KEYDX " << keys.asDComplex ("KEYDX") << endl;
    cout << "KEYDZ " << keys.asDComplex ("KEYDZ") << endl;
    cout << "KEYA " << keys.asString ("KEYA") << endl;
    cout << "KEYB " << keys.asBool ("KEYB") << endl;
    cout << "KEYSV " << keys.asArrayShort ("KEYSV") << endl;
    cout << "KEYIV " << keys.asArrayInt ("KEYIV") << endl;
    cout << "KEYFV " << keys.asArrayfloat ("KEYFV") << endl;
    cout << "KEYDV " << keys.asArraydouble ("KEYDV") << endl;
    cout << "KEYXC " << keys.asArrayComplex ("KEYXC") << endl;
    cout << "KEYZV " << keys.asArrayComplex ("KEYZV") << endl;
    cout << "KEYDXC " << keys.asArrayDComplex ("KEYDXC") << endl;
    cout << "KEYDZV " << keys.asArrayDComplex ("KEYDZV") << endl;
    cout << "KEYAV " << keys.asArrayString ("KEYAV") << endl;
    cout << "KEYBV " << keys.asArrayBool ("KEYBV") << endl;
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ROScalarColumn<Short>    cols (tab,"COLS");
    ROScalarColumn<Int>      coli (tab,"COLI");
    ROScalarColumn<float>    colf (tab,"COLF");
    ROScalarColumn<double>   cold (tab,"COLD");
    ROScalarColumn<Complex>  colx (tab,"COLX");
    ROScalarColumn<Complex>  colz (tab,"COLZ");
    ROScalarColumn<DComplex> coldx (tab,"COLDX");
    ROScalarColumn<DComplex> coldz (tab,"COLDZ");
    ROScalarColumn<String>   cola (tab,"COLA");
    ROScalarColumn<Bool>     colb (tab,"COLB");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << cols(i) << " " << coli(i) << " " << colf(i) << " "
	     << cold(i) << " " << colx(i) << " " << colz(i) << " "
	     << coldx(i) << " " << coldz(i) << " " << cola(i) << " "
	     << colb(i) << endl;
    }
}
