//# tBinTable.cc - this program tests the BinTable class
//# Copyright (C) 1995,1996,1999,2000,2001,2002
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

#include <casacore/fits/FITS/BinTable.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/File.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[])
{
    try {
	Input inputs(1);

	inputs.create("inputFile",
		      "",
		      "The input FITS file",
		      "String");
	inputs.create("baseName",
		      "",
		      "The root name for all created files",
		      "String");
	inputs.create("storageManager",
		      "miriad",
		      "The storage manager to use - miriad (RLE) or aipsio "
		      "(memory)",
		      "String");
	inputs.create("sdfits",
		      "False",
		      "Interpret keywords as virtual columns as in the SD-FITS convention",
		      "Bool");

	inputs.readArguments(argc, argv);

	String inputFilename = inputs.getString("inputFile");
	String baseName = inputs.getString("baseName");
	String storageManagerType = inputs.getString("storageManager");
	Bool sdfits = inputs.getBool("sdfits");

	storageManagerType.downcase();

	Bool useMiriadSM;
	if (storageManagerType == "miriad") {
	    useMiriadSM = True;
	} else 	if (storageManagerType == "aipsio") {
	    useMiriadSM = False;
	} else {
	    cout << storageManagerType << " is not a valid storage manager" << 
	        endl;
	    return 1;
	}

	File inputFile(inputFilename);
	if (! inputFile.isReadable()) {
	    cout << inputFilename << " is not readable - exiting" << endl;
	    return 1;
	}

	Int tabCount = 0;
	// This allows for constructed names of the form baseName.table.xx
	char *tabName = new char[baseName.length() + 10];
	// construct the FITS input
	FitsInput infits(inputFilename.chars(), FITS::Disk);
	if (infits.err() != FitsIO::OK) {
	    cout << "Problem instantiating FITS input " << infits.err() << endl;
	    return 1;
	}

	while (!infits.eof()) {
	    switch (infits.hdutype()) {
	    case FITS::BinaryTableHDU:
	    {
		sprintf(tabName,"%s.table.%i",baseName.chars(),tabCount++);
		String tabNameString(tabName);
		cout << "BinaryTableHDU : " << tabNameString << " ... " ;
		BinaryTable bintab(infits, FITSError::defaultHandler, 
				   useMiriadSM, sdfits);
		if (infits.err() != FitsIO::OK) {
		    cout << "Problem in infits while instantiating binary table " <<
			infits.err() << endl;
		    return 1;
		}
		Table tab = bintab.fullTable(tabNameString, Table::NewNoReplace,
					     useMiriadSM);
		if (infits.err() != FitsIO::OK) {
		    cout << "Problem in infits while making the table " <<
			infits.err() << endl;
		    return 1;
		}
		cout << "done." << endl;
	    }
	    break;
	    default:
		cout << "Unable to do anything but skip this hdutype : " << 
		    Int(infits.hdutype()) << endl;
		infits.skip_hdu();
		if (infits.err() != FitsIO::OK) {
		    cout << "Problem in infits while skipping the hdu" <<
			infits.err() << endl;
		    return 1;
		}
		break;
	    }
	}
	cout << "At end of file" << endl;
    } catch (AipsError x) {
	cout << "Unexpected exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;
}
