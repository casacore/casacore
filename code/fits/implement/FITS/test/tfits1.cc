//# tfits1.cc: FITS test program to read and display values from a FITS file
//# Copyright (C) 1993,1994,1996
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

# include <aips/FITS/fits.h>
# include <aips/FITS/fitsio.h>
# include <aips/FITS/hdu.h>

# include <iostream.h>
# include <stdlib.h>

// Display basic info and the keyword list
void show(HeaderDataUnit *h);

// Read and display a binary table
void do_binary_table(BinaryTableExtension &x);

// Read the data in a Primary Group and display the first few groups
template <class TYPE>
void do_primary_group(PrimaryGroup<TYPE> &x);

// Read the data in a Primary Array and display the first few data points
template <class TYPE>
void do_primary_array(PrimaryArray<TYPE> &x);

# if defined(MSDOS)
void do_primary_array(PrimaryArray<unsigned char> &x);
void do_primary_array(PrimaryArray<short> &x);
void do_primary_array(PrimaryArray<FitsLong> &x);
void do_primary_array(PrimaryArray<float> &x);
void do_primary_array(PrimaryArray<double> &x);
void do_primary_group(PrimaryGroup<unsigned char> &x);
void do_primary_group(PrimaryGroup<short> &x);
void do_primary_group(PrimaryGroup<FitsLong> &x);
void do_primary_group(PrimaryGroup<float> &x);
void do_primary_group(PrimaryGroup<double> &x);
# endif


// Read the data in a Primary Group and display the first few groups
template <class TYPE>
void do_primary_group(PrimaryGroup<TYPE> &x) {
	int i, j;
	int number_to_display = 10;
	show(&x);
	if (x.err() != HeaderDataUnit::OK) {
		cout << "Error occured during construction process -- exiting\n";
		exit(0);
	}
	for (i = 0; i < x.gcount(); ++i) {
	    x.read();
	    if (i < number_to_display) {
		cout << "Group " << i << " parms: " << "\n";
		for (j = 0; j < x.pcount(); ++j)
		    cout << " " << x.parm(j);
		cout << "\n";
		cout << "Group " << i << " data: " << "\n";
		for (j = 0; j < 4; ++j)
		    cout << " " << x(j);
		cout << "\n";

	    }
	}
	delete &x;
}

// Read the data in a Primary Array and display the first few data points
template <class TYPE>
void do_primary_array(PrimaryArray<TYPE> &x) {
	int i, j, n0, n1;
	if (x.fitsdatasize())
	    x.read(); // read the entire array
	show(&x);
	if (x.err() != HeaderDataUnit::OK) {
		cout << "Error occured during construction process -- exiting\n";
		exit(0);
	}
	if (x.dims() == 2) {
	    n0 = x.dim(0) > 6 ? 6 : x.dim(0);
	    n1 = x.dim(1) > 6 ? 6 : x.dim(1);
	    for (i = 0; i < n0; ++i)
	    for (j = 0; j < n1; ++j)
		cout << "(" << i << "," << j << ") = " 
		     << x(i,j) << "\n";
	}
	delete &x;
}

// Display basic info and the keyword list
void show(HeaderDataUnit *h) {
	cout << "Data type   " << h->datatype() << "\n"
	     << "Data size   " << h->fitsdatasize() << "\n"
	     << "Dimensions  " << h->dims() << "\n";
	for (int n = 0; n < h->dims(); n++)
		cout << "Axis " << (n + 1) << " size "
		     << h->dim(n) << "\n";
	cout << "----- Keyword List -----\n" << *h << "\n";
}


// Read and display a binary table
void do_binary_table(BinaryTableExtension &x) {
	if (x.err() != 0) {
	    cout << "BT ERROR! " << x.err() << endl;
	    return;
	}
	cout << x.nrows() << " rows   "
	     << x.ncols() << " cols   "
	     << x.fitsdatasize() << " bytes total\n" << endl;
	cout << "----- Keyword List -----\n" << x << endl;

	cout << "\nTable Data\n\n";

	for (int i = 0; i < x.ncols(); ++i) {
	    cout << "Col " << i << ": " 
		 << x.field(i).nelements() << " "
		 << x.field(i).fieldtype() << " "
		 << x.ttype(i) << " "
		 << x.tunit(i) << "\n";		 
	}
	cout << endl;

        x.read(); // read the whole table
	for (int n = 0; n < 50 && n < x.nrows(); ++n) { // display first 50 rows
		cout << x.currrow() << ": | ";
		if (x.field(0).nelements() != 0)
	            cout << x.field(0);
	        for (i = 1; i < x.ncols(); ++i) {
		    cout << " | ";
		    if (x.field(i).nelements() != 0)
			cout << x.field(i);
		}
	        cout << " |" << endl;
	    	++x; // increment the current row
	}
	delete &x;
}

int main(int argc, char **argv) {
	HeaderDataUnit *h;
	PrimaryArray<unsigned char> *paB;
	PrimaryArray<short> *paS;
	PrimaryArray<FitsLong> *paL;
	PrimaryArray<float> *paF;
	PrimaryArray<double> *paD;
	PrimaryGroup<unsigned char> *pgB;
	PrimaryGroup<short> *pgS;
	PrimaryGroup<FitsLong> *pgL;
	PrimaryGroup<float> *pgF;
	PrimaryGroup<double> *pgD;
	BinaryTableExtension *bt;
	AsciiTableExtension *at;

	cout << "Test of reading fits files" << endl;
	if (argc != 2) {
	    cout << "ex1 <filename>" << "\n";
	    exit(0);
	}
	FitsInput fin(argv[1],FITS::Disk);
	if (fin.err() == FitsIO::IOERR) {
	    cout << "Error opening FITS input.\n";
	    exit(0);
	} else if (fin.err()) {
	    cout << "Error reading initial record -- exiting.\n";
	    exit(0);
	}
	const int NMAXERRS = 100;

	for(int nerrs = 0;
		nerrs < NMAXERRS && fin.rectype() != FITS::EndOfFile; ) {
	    if (fin.rectype() == FITS::HDURecord) {
		switch (fin.hdutype()) {
		    case FITS::PrimaryArrayHDU:
			cout << "----- Primary Array -----\n";
			switch (fin.datatype()) {
			    case FITS::BYTE:
				paB = new PrimaryArray<unsigned char>(fin);
				do_primary_array(*paB);
				break;
			    case FITS::SHORT:
				paS = new PrimaryArray<short>(fin);
				do_primary_array(*paS);
				break;
			    case FITS::LONG:
				paL = new PrimaryArray<FitsLong>(fin);
				do_primary_array(*paL);
				break;
			    case FITS::FLOAT:
				paF = new PrimaryArray<float>(fin);
				do_primary_array(*paF);
				break;
			    case FITS::DOUBLE:
				paD = new PrimaryArray<double>(fin);
				do_primary_array(*paD);
				break;
			}
			break;
		    case FITS::PrimaryGroupHDU:
			cout << "----- Primary Group -----\n";
			switch (fin.datatype()) {
			    case FITS::BYTE:
				pgB = new PrimaryGroup<unsigned char>(fin);
				do_primary_group(*pgB);
				break;
			    case FITS::SHORT:
				pgS = new PrimaryGroup<short>(fin);
				do_primary_group(*pgS);
				break;
			    case FITS::LONG:
				pgL = new PrimaryGroup<FitsLong>(fin);
				do_primary_group(*pgL);
				break;
			    case FITS::FLOAT:
				pgF = new PrimaryGroup<float>(fin);
				do_primary_group(*pgF);
				break;
			    case FITS::DOUBLE:
				pgD = new PrimaryGroup<double>(fin);
				do_primary_group(*pgD);
				break;
			}
			break;
		    case FITS::AsciiTableHDU:
			at = new AsciiTableExtension(fin);
			cout << "\n\nASCII Table   ";
			do_binary_table(*at);
			break;
		    case FITS::BinaryTableHDU:
			bt = new BinaryTableExtension(fin);
			cout << "\n\nBinary Table   ";
			do_binary_table(*bt);
			break;
		    case FITS::ImageExtensionHDU:
			cout << "----- Image Extension -----\n";
			switch (fin.datatype()) {
			    case FITS::BYTE:
				paB = new ImageExtension<unsigned char>(fin);
				do_primary_array(*paB);
				break;
			    case FITS::SHORT:
				paS = new ImageExtension<short>(fin);
				do_primary_array(*paS);
				break;
			    case FITS::LONG:
				paL = new ImageExtension<FitsLong>(fin);
				do_primary_array(*paL);
				break;
			    case FITS::FLOAT:
				paF = new ImageExtension<float>(fin);
				do_primary_array(*paF);
				break;
			    case FITS::DOUBLE:
				paD = new ImageExtension<double>(fin);
				do_primary_array(*paD);
				break;
			}
			break;
		    case FITS::UnknownExtensionHDU:
			h = new ExtensionHeaderDataUnit(fin);
			cout << "----- Unknown conforming extension -----\n";
			h->skip();
			delete h;
			break;
		    default:
			cout << "This isn't supposed to happen\n";
			break;
		}
	    } else if (fin.rectype() == FITS::BadBeginningRecord ||
		       fin.rectype() == FITS::UnrecognizableRecord) {
		cout << "Bad Record encountered\n";
		exit(0);
	    }
	    else if (fin.rectype() == FITS::SpecialRecord) {
		cout << "Special Record encountered\n";
		exit(0);
	    }
	    if (fin.err())
		++nerrs;
	}

	if (nerrs == NMAXERRS)
	    cout << "Too many errors.  Processing terminated.\n";
	else
	    cout << "End of Header-Data Units.\n";
	cout << endl;

	return 0;
}

#if defined(__GNUG__)
// Manually instantiate the templates for g++ 2.7.x -fno-implicit-templates
template do_primary_array(PrimaryArray<unsigned char> &x);
template do_primary_array(PrimaryArray<short> &x);
template do_primary_array(PrimaryArray<FitsLong> &x);
template do_primary_array(PrimaryArray<float> &x);
template do_primary_array(PrimaryArray<double> &x);
template do_primary_group(PrimaryGroup<unsigned char> &x);
template do_primary_group(PrimaryGroup<short> &x);
template do_primary_group(PrimaryGroup<FitsLong> &x);
template do_primary_group(PrimaryGroup<float> &x);
template do_primary_group(PrimaryGroup<double> &x);
#endif
