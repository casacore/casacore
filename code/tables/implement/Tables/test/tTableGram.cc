//# tTableGram.cc: This program tests table commands using TableGram/Parse
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

#include <aips/Tables/TableParse.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Exceptions/Error.h>

// <summary>
// Test program for table commands from user interface
// </summary>

// This program tests table commands with an SQL-like grammar.
// The grammar is scanned and parsed using the flex/bison file TableGram.l/y
// and with the help of the class TableParse.
// It ask for commands until a "q" is given.
// When columns are selected, it will show their contents.


void seltab (const String&);
void docomm ();
main (int argc, char** argv) {
    try {
	if (argc > 1) {
	    seltab(argv[1]);
	}else{
	    docomm();
	}
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } end_try;
    return 0;               // successfully executed
}


// Ask and execute command till empty string is given.
void docomm()
{
    char comm[1025];
    while (True) {
	cout << "Table command (q=quit): ";
	cin.getline (comm, 1024);
	String str(comm);
	if (str == "q") 
	    break;
	try {
	    seltab (str);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} end_try;
    }
}


// Show the required columns.
// First test if they exist and contain scalars or arrays.
void showtab (const Table& tab, const Vector<String>& colnam)
{
    uInt nrcol = 0;
    PtrBlock<ROTableColumn*> tableColumns(colnam.nelements());
    for (uInt i=0; i<colnam.nelements(); i++) {
	if (! tab.tableDesc().isColumn (colnam(i))) {
	    cout << "Column " << colnam(i) << " does not exist" << endl;
	}else{
	    tableColumns[nrcol] = new ROTableColumn (tab, colnam(i));
	    if (! tableColumns[nrcol]->columnDesc().isScalar()
	    &&  ! tableColumns[nrcol]->columnDesc().isArray()) {
		cout << "Column " << colnam(i)
		     << " contains scalars nor arrays"
		     << endl;
		delete tableColumns[nrcol];
	    }else{
		nrcol++;
	    }
	}
    }
    if (nrcol == 0) {
	return;
    }

    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<nrcol; j++) {
	    if (tableColumns[j]->columnDesc().isArray()) {
		cout << " " << tableColumns[j]->shape (i);
	    }else{
		switch (tableColumns[j]->columnDesc().dataType()) {
		case TpBool:
		    cout << " " << tableColumns[j]->asBool (i);
		    break;
		case TpString:
		    cout << " " << tableColumns[j]->asString (i);
		    break;
		case TpComplex:
		case TpDComplex:
		    cout << " " << tableColumns[j]->asDComplex (i);
		    break;
		default:
		    cout << " " << tableColumns[j]->asdouble (i);
		}
	    }
	}
	cout << endl;
    }

    for (i=0; i<nrcol; i++) {
	delete tableColumns[i];
    }
}


// Sort and select data.
void seltab (const String& str)
{
    uInt i;
    Table tab;
    Vector<String> vecstr;
    cout << str << endl;
    // Select rows from the table.
    tab = tableCommand (str, vecstr);
    cout << "    has been executed" << endl;
    cout << "    " << tab.nrow() << " rows selected" << endl;
    // Show the selected column names.
    cout << vecstr.nelements() << " selected columns: ";
    for (i=0; i<vecstr.nelements(); i++) {
	cout << " " << vecstr(i);
    }
    cout << endl;

    // Show the contents of the columns.
    if (vecstr.nelements() > 0) {
	showtab (tab, vecstr);
    }
}
