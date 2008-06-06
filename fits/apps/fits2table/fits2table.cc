//# fits2table - convert a FITS file into an casacore table
//# Copyright (C) 1995,1996,1997,1999,2000,2001,2007
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
//# $Id: fits2table.cc,v 19.3 2004/11/30 17:50:07 ddebonis Exp $

//# Includes

#include <fits/FITS/FITSTable.h>
#include <fits/FITS/SDFITSTable.h>
#include <casa/Inputs/Input.h>
#include <casa/Exceptions/Error.h>
#include <casa/OS/File.h>

#include <tables/Tables.h>

#include <casa/iostream.h>
#include <casa/stdio.h>

#include <casa/namespace.h>
int main(int argc, const char* argv[])
{
    try {
	Input inputs(1);

	inputs.create("input",
		      "",
		      "The input FITS file",
		      "String");
	inputs.create("output",
		      "",
		      "The ouput aips++ Table name",
		      "String");
	inputs.create("which_hdu",
		      "1",
		      "0-relative to primary HDU, i.e. 1 is the smallest valid value.",

		      "Integer");
	inputs.create("storage",
		      "standard",
		      "The storage manager to use - standard or incremental "
		      "(memory)",
		      "String");
	inputs.create("sdfits",
		      "True",
		      "Interpret keywords as virtual columns as in the SD-FITS convention",
		      "Bool");

	inputs.readArguments(argc, argv);

	String inputFilename = inputs.getString("input");
	String outputFilename = inputs.getString("output");
	String storageManagerType = inputs.getString("storage");
	Int whichHDU = inputs.getInt("which_hdu");
	Bool sdfits = inputs.getBool("sdfits");

	storageManagerType.downcase();

	Bool useIncrSM;
	if (storageManagerType == "incremental") {
	    useIncrSM = True;
	} else 	if (storageManagerType == "standard") {
	    useIncrSM = False;
	} else {
	    cerr << storageManagerType << " is not a valid storage manager" << 
	        endl;
	    return 1;
	}

	if (whichHDU < 1) {
	    cerr << "whichHDU is not valid, must be >= 1" << endl;
	    return 1;
	}

	File inputFile(inputFilename);
	if (! inputFile.isReadable()) {
	    cerr << inputFilename << " is not readable - exiting" << endl;
	    return 1;
	}

	// construct the FITS table of the appropriate type
	FITSTable *infits = 0;
	if (sdfits) {
	    infits = new SDFITSTable(inputFilename, whichHDU);
	} else {
	    infits = new FITSTable(inputFilename, whichHDU);
	}
	AlwaysAssert(infits, AipsError);
	if (!infits->isValid()) {
	    cerr << "The indicated FITS file does not have a valid binary table at HDU=" 
		 << whichHDU << endl;
	    return 1;
	}

	TableDesc td(FITSTabular::tableDesc(*infits));
	// if sdfits, remove any TDIM columns from td, FITSTable takes care of interpreting them
	// and if sdfits is true, that most likely means we don't want to see them
	
	if (sdfits) {
	    Vector<String> cols(td.columnNames());
	    for (uInt i=0;i<cols.nelements();i++) {
		if (cols(i).matches(Regex("^TDIM.*"))) {
		    td.removeColumn(cols(i));
		}
	    }
	}
	    
	SetupNewTable newtab(outputFilename, td, Table::NewNoReplace);
	if (useIncrSM) {
	    IncrementalStMan stman("ISM");
	    newtab.bindAll(stman);
	}
	Table tab(newtab, TableLock::PermanentLocking, infits->nrow());
	TableRow row(tab);
	uInt rownr = 0;

	while (rownr < tab.nrow()) {
	    row.putMatchingFields(rownr, TableRecord(infits->currentRow()));
	    infits->next();
	    rownr++;
	}

	cout << "done." << endl;
    } catch (AipsError x) {
	cout << "Exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;
}
