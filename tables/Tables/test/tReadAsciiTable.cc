//# tReadAsciiTable.cc: Test program for the ReadAsciiTable functions
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2002
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

#include <casacore/tables/Tables/ReadAsciiTable.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for the ReadAsciiTable functions </summary>

// This program tests the functions in ReadAsciiTable.h.
// It uses some files in the test directory. The directory of those
// files is given in argv[1].
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a (const String& dir);
void aa (const String& dir);
void ab (const String& dir);
void a1 (const String& dir, const String& commentMarker,
	 Int firstLine, Int lastLine);
void a2 (const String& dir, const String& commentMarker,
	 Int firstLine, Int lastLine);
void b (const String& dir, const String& suffix, Char separator,
	const String& commentMarker,
	Int firstLine, Int lastLine);
void b1 (const String& dir);
void b2 (const String& dir);
void b3 (const String& dir, const IPosition& autoShape);
void erronous();

int main (int argc, const char* argv[])
{
    try {
	String dir;
	if (argc > 1) {
	    dir = argv[1];
	}
	a (dir);
	aa (dir);
	ab (dir);
	a1 (dir, "", -1, -1);
	a1 (dir, "1 ", -1, -1);
	a1 (dir, "", 1, 2);
	a1 (dir, "", 2, -1);
	a2 (dir, "", -1, -1);
	a2 (dir, "1 ", -1, -1);
	a2 (dir, "", 1, 2);
	a2 (dir, "", 2, -1);
	b (dir, "", ' ', " *#", 1, -1);
	b (dir, "", ' ', " #", 2, 3);
	b (dir, "c", ',', "", -1, -1);
	b (dir, "c", ',', "K", -1, -1);
	b1 (dir);
	b2 (dir);
	b3 (dir, IPosition(1,0));
	b3 (dir, IPosition(2,1,10));
	b3 (dir, IPosition(1,10));
	b3 (dir, IPosition(1,5));
	b3 (dir, IPosition(1,15));
	b3 (dir, IPosition(2,2,5));
	b3 (dir, IPosition(2,3,5));
	b3 (dir, IPosition(2,0,5));
	erronous();
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
    ScalarColumn<Int>     coli (tab,"COLI");
    ScalarColumn<float>   colf (tab,"COLF");
    ScalarColumn<double>  cold (tab,"COLD");
    ScalarColumn<Complex> colx (tab,"COLX");
    ScalarColumn<Complex> colz (tab,"COLZ");
    ScalarColumn<String>  cols (tab,"COLS");
    ScalarColumn<Double>  colra (tab,"COLRA");
    ScalarColumn<Double>  coldec (tab,"COLDEC");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << coli(i) << " " << colf(i) << " " << cold(i) << " "
	     << colx(i) << " " << colz(i) << " " << cols(i) << " "
	     << colra(i) << " " << coldec(i) << endl;
    }
}

void aa (const String& dir)
{
    cout << ">>>" << endl;
    String formStr;
    Table tab = readAsciiTable (formStr, Table::Plain,
				dir + "tReadAsciiTable.in_tab", "",
				"tReadAsciiTable_tmp.data_tab1");
    AlwaysAssertExit (tab.tableType() == Table::Plain);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ScalarColumn<Int>     coli (tab,"COLI");
    ScalarColumn<float>   colf (tab,"COLF");
    ScalarColumn<double>  cold (tab,"COLD");
    ScalarColumn<Complex> colx (tab,"COLX");
    ScalarColumn<Complex> colz (tab,"COLZ");
    ScalarColumn<String>  cols (tab,"COLS");
    ScalarColumn<Double>  colra (tab,"COLRA");
    ScalarColumn<Double>  coldec (tab,"COLDEC");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << coli(i) << " " << colf(i) << " " << cold(i) << " "
	     << colx(i) << " " << colz(i) << " " << cols(i) << " "
	     << colra(i) << " " << coldec(i) << endl;
    }
}

void ab (const String& dir)
{
    cout << ">>>" << endl;
    String formStr;
    Table tab = readAsciiTable (formStr, Table::Memory,
				dir +  "tReadAsciiTable.in_tab", "",
				"");
    AlwaysAssertExit (tab.tableType() == Table::Memory);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ScalarColumn<Int>     coli (tab,"COLI");
    ScalarColumn<float>   colf (tab,"COLF");
    ScalarColumn<double>  cold (tab,"COLD");
    ScalarColumn<Complex> colx (tab,"COLX");
    ScalarColumn<Complex> colz (tab,"COLZ");
    ScalarColumn<String>  cols (tab,"COLS");
    ScalarColumn<Double>  colra (tab,"COLRA");
    ScalarColumn<Double>  coldec (tab,"COLDEC");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << coli(i) << " " << colf(i) << " " << cold(i) << " "
	     << colx(i) << " " << colz(i) << " " << cols(i) << " "
	     << colra(i) << " " << coldec(i) << endl;
    }
}

void a1 (const String& dir, const String& commentMarker,
	 Int firstLine, Int lastLine)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tah", "",
				     "tReadAsciiTable_tmp.data_tah", True,
				     ' ', commentMarker, firstLine, lastLine);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    Table tab("tReadAsciiTable_tmp.data_tah");
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ScalarColumn<Int>      col1 (tab,"Column1");
    ScalarColumn<double>   col2 (tab,"Column2");
    ScalarColumn<double>   col3 (tab,"Column3");
    ScalarColumn<double>   col4 (tab,"Column4");
    ScalarColumn<double>   col5 (tab,"Column5");
    ScalarColumn<double>   col6 (tab,"Column6");
    ScalarColumn<double>   col7 (tab,"Column7");
    ScalarColumn<String>   col8 (tab,"Column8");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << col1(i) << " " << col2(i) << " " << col3(i) << " "
	     << col4(i) << " " << col5(i) << " " << col6(i) << " "
	     << col7(i) << " " << col8(i) << endl;
    }
}

void a2 (const String& dir, const String& commentMarker,
	 Int firstLine, Int lastLine)
{
    cout << ">>>" << endl;
    Vector<String> names(7);
    Vector<String> types(7);
    names[0] = "COLI";
    names[1] = "COLF";
    names[2] = "COLD";
    names[3] = "COLX";
    names[4] = "COLZ1";
    names[5] = "COLZ2";
    names[6] = "COLS";
    types[0] = "I";  
    types[1] = "R";
    types[2] = "D"; 
    types[3] = "X"; 
    types[4] = "R";
    types[5] = "D";
    types[6] = "A";
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tah", "",
				     "tReadAsciiTable_tmp.data_tah",
				     names, types,
				     ' ', commentMarker, firstLine, lastLine);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    Table tab("tReadAsciiTable_tmp.data_tah");
    cout << endl;
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ScalarColumn<Int>     coli (tab,"COLI");
    ScalarColumn<float>   colf (tab,"COLF");
    ScalarColumn<double>  cold (tab,"COLD");
    ScalarColumn<Complex> colx (tab,"COLX");
    ScalarColumn<float>  colz1 (tab,"COLZ1");
    ScalarColumn<double> colz2 (tab,"COLZ2");
    ScalarColumn<String>  cols (tab,"COLS");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << coli(i) << " " << colf(i) << " " << cold(i) << " "
	     << colx(i) << " " << colz1(i) << " " << colz2(i) << " "
	     << cols(i) << endl;
    }
}

void b (const String& dir, const String& suffix, Char separator,
	const String& commentMarker, Int firstLine, Int lastLine)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tkh" + suffix,
				     dir + "tReadAsciiTable.in_tkd" + suffix,
				     "tReadAsciiTable_tmp",
				     "tReadAsciiTable_tmp.data_tk",
				     separator, commentMarker,
				     firstLine, lastLine);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    cout << endl;
    TableDesc tabdesc("tReadAsciiTable_tmp");
    tabdesc.show();
    cout << endl;
    Table tab("tReadAsciiTable_tmp.data_tk");
    const TableRecord& keys = tab.keywordSet();
    cout << keys.description();
    if (commentMarker != "K") {
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
    }
    {
      TableColumn tabcol (tab, "COLI");
      const TableRecord& keycol = tabcol.keywordSet();
      cout << keycol.description();
      cout << "IKEYS " << keycol.asString ("IKEYS") << endl;
    }
    {
      TableColumn tabcol (tab, "COLDX");
      const TableRecord& keycol = tabcol.keywordSet();
      cout << keycol.description();
      cout << "IKEYS " << keycol.asString ("IKEYS") << endl;
      cout << "DKEYS " << keycol.asString ("DKEYS") << endl;
    }
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ScalarColumn<Short>    cols (tab,"COLS");
    ScalarColumn<Int>      coli (tab,"COLI");
    ScalarColumn<float>    colf (tab,"COLF");
    ScalarColumn<double>   cold (tab,"COLD");
    ScalarColumn<Complex>  colx (tab,"COLX");
    ScalarColumn<Complex>  colz (tab,"COLZ");
    ScalarColumn<DComplex> coldx (tab,"COLDX");
    ScalarColumn<DComplex> coldz (tab,"COLDZ");
    ScalarColumn<String>   cola (tab,"COLA");
    ScalarColumn<Bool>     colb (tab,"COLB");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << cols(i) << " " << coli(i) << " " << colf(i) << " "
	     << cold(i) << " " << colx(i) << " " << coldx(i) << " "
	     << colz(i) << " " << coldz(i) << " " << cola(i) << " "
	     << colb(i) << endl;
    }
}

void b1 (const String& dir)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tkh", "",
				     "tReadAsciiTable_tmp.data_tk", False,
				     ' ', " #");
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
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
    {
      TableColumn tabcol (tab, "COLI");
      const TableRecord& keycol = tabcol.keywordSet();
      cout << keycol.description();
      cout << "IKEYS " << keycol.asString ("IKEYS") << endl;
    }
    {
      TableColumn tabcol (tab, "COLDX");
      const TableRecord& keycol = tabcol.keywordSet();
      cout << keycol.description();
      cout << "IKEYS " << keycol.asString ("IKEYS") << endl;
      cout << "DKEYS " << keycol.asString ("DKEYS") << endl;
    }
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ScalarColumn<Short>    cols (tab,"COLS");
    ScalarColumn<Int>      coli (tab,"COLI");
    ScalarColumn<float>    colf (tab,"COLF");
    ScalarColumn<double>   cold (tab,"COLD");
    ScalarColumn<Complex>  colx (tab,"COLX");
    ScalarColumn<Complex>  colz (tab,"COLZ");
    ScalarColumn<DComplex> coldx (tab,"COLDX");
    ScalarColumn<DComplex> coldz (tab,"COLDZ");
    ScalarColumn<String>   cola (tab,"COLA");
    ScalarColumn<Bool>     colb (tab,"COLB");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << cols(i) << " " << coli(i) << " " << colf(i) << " "
	     << cold(i) << " " << colx(i) << " " << colz(i) << " "
	     << coldx(i) << " " << coldz(i) << " " << cola(i) << " "
	     << colb(i) << endl;
    }
}

void b2 (const String& dir)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tkh", "",
				     "tReadAsciiTable_tmp.data_tk", True,
				     ' ', " #");
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
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
    ScalarColumn<String> col1 (tab,"Column1");
    ScalarColumn<String> col2 (tab,"Column2");
    ScalarColumn<String> col3 (tab,"Column3");
    ScalarColumn<String> col4 (tab,"Column4");
    ScalarColumn<String> col5 (tab,"Column5");
    ScalarColumn<String> col6 (tab,"Column6");
    ScalarColumn<String> col7 (tab,"Column7");
    ScalarColumn<String> col8 (tab,"Column8");
    ScalarColumn<String> col9 (tab,"Column9");
    ScalarColumn<String> col10 (tab,"Column10");
    for (uInt i=0; i<tab.nrow(); i++) {
	cout << col1(i) << " " << col2(i) << " " << col3(i) << " "
	     << col4(i) << " " << col5(i) << " " << col6(i) << " "
	     << col7(i) << " " << col8(i) << " " << col9(i) << " "
	     << col10(i) << endl;
    }
}

void b3 (const String& dir, const IPosition& autoShape)
{
    cout << ">>>" << endl;
    String formStr = readAsciiTable (dir + "tReadAsciiTable.in_tkh", "",
				     "tReadAsciiTable_tmp.data_tk", True,
				     ' ', " #", 1, -1, autoShape);
    cout << "<<<" << endl;
    cout << "Input format: [" << formStr << ']' << endl;
    cout << "shape=" << autoShape << endl;;
    Table tab("tReadAsciiTable_tmp.data_tk");
    cout << tab.nrow() << " rows, " << tab.tableDesc().ncolumn()
	 << " columns" << endl;
    ArrayColumn<String> col1 (tab,"Column1");
    for (uInt i=0; i<tab.nrow(); i++) {
        cout << col1(i) << endl;
    }
}


void tryerror()
{
  Bool ok = True;
  try {
    readAsciiTable ("tReadAsciiTable_tmp.header", "",
		    "tReadAsciiTable_tmp.data_try");
  } catch (AipsError& x) {
    cout << x.getMesg() << endl;
    ok = False;
  }
  AlwaysAssertExit (ok==False);
}

void erronous()
{
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2 COL3" << endl;
    ofile << "I D" << endl;
  }
  tryerror();      // mismatching header lines
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I D R" << endl;
  }
  tryerror();      // mismatching header lines
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I F" << endl;
  }
  tryerror();      // invalid datatype
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I0 I" << endl;
  }
  tryerror();      // variable length not last column
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I I0,0" << endl;
  }
  tryerror();      // more than one variable length axis
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I I,1" << endl;
  }
  tryerror();      // first axis not given
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I I1," << endl;
  }
  tryerror();      // second axis not given
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I I1." << endl;
  }
  tryerror();      // invalid axis length
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
    ofile << "I 1" << endl;
  }
  tryerror();      // no column datatype
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
  }
  tryerror();      // missing NAMES line
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << "COL1 COL2" << endl;
  }
  tryerror();      // missing TYPES line
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << ".key" << endl;
  }
  tryerror();      // missing .endkey
  {
    ofstream ofile("tReadAsciiTable_tmp.header");
    ofile << ".key" << endl;
    ofile << "KEYNAME" << endl;
  }
  tryerror();      // missing keyword datatype
}
