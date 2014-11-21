//# tfits5.cc: FITS test program to read and display values from a FITS file
//# Copyright (C) 1993,1994,1998,2001
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

# include <casacore/fits/FITS/fits.h>
# include <casacore/fits/FITS/hdu.h>
# include <casacore/fits/FITS/fitsio.h>

# include <casacore/casa/iostream.h>
# include <casacore/casa/stdlib.h>
# include <casacore/casa/stdio.h>
# include <string.h>

#include <casacore/casa/namespace.h>
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
void cvt_table(AsciiTableExtension &x, FitsOutput &fout) {
	if (x.err() != 0) {
	    cout << "BT ERROR! " << x.err() << endl;
	    return;
	}
	cout << x.nrows() << " rows   "
	     << x.ncols() << " cols   "
	     << x.fitsdatasize() << " bytes total\n" << endl;
	cout << "----- Keyword List -----\n" << x << endl;

	cout << "\nTable Data\n\n";

	int i;
	for (i = 0; i < x.ncols(); ++i) {
	    cout << "Col " << i << ": " 
		 << x.field(i).nelements() << " "
		 << x.field(i).fieldtype() << " "
		 << x.ttype(i) << " "
		 << x.tunit(i) << "\n";		 
	}
	cout << endl;

        x.read(); // read the whole ascii table
	int n;
	for (n = 0; n < 50 && n < x.nrows(); ++n) { // display first 50 rows
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

	// We must first build a new keyword list using the old one
	FitsKeywordList bk = x.kwlist();
	bk.first();
	n = 0;
	char p[24];
	int rs = 0; // compute the size of a row in the binary table
	for (i = 0; i < x.ncols(); ++i)
	    rs += x.field(i).fitsfieldsize();
	while (bk.next()) {
	    // change the EXTENSION keyword
	    if (bk.curr()->isreserved() && 
		bk.curr()->kw().name() == FITS::XTENSION) {
		bk.del();
		bk.mk(FITS::XTENSION,"BINTABLE");
	    }
	    if (bk.curr()->isreserved() && bk.curr()->isindexed()) {
		// delete TBCOL and TNULL keywords
		if (bk.curr()->kw().name() == FITS::TBCOL ||
		    bk.curr()->kw().name() == FITS::TNULL)
		    bk.del();
		// and convert the TFORM keyword
		if (bk.curr()->kw().name() == FITS::TFORM) {
		    bk.del();
		    switch (x.field(n).fieldtype()) {
			case FITS::CHAR:
			    sprintf(p,"%dA",x.field(n).fitsfieldsize());
			    bk.mk((n + 1),FITS::TFORM,p);
			    break;
			case FITS::LONG:
			    bk.mk((n + 1),FITS::TFORM,"J");
			    break;
			case FITS::FLOAT:
			    bk.mk((n + 1),FITS::TFORM,"E");
			    break;
			case FITS::DOUBLE:
			    bk.mk((n + 1),FITS::TFORM,"D");
			    break;
		        default:
			    break;
		    }
		    ++n;		
		}
		// and convert NAXIS1
		if (bk.curr()->kw().name() == FITS::NAXIS &&
		    bk.curr()->index() == 1) {
		    bk.del();
		    bk.mk(1,FITS::NAXIS,rs);
		}
	    }
	}
	cout << "New keyword list:\n" << bk << endl;

	BinaryTableExtension bt(bk); // create the binary table
	cout << "the new binary table\n";
	show(&bt);	
	bt.write_hdr(fout);

/**********************************************************************
This is one implementation
***********************************************************************
	// get ready to transfer the data
	bt.set_next(x.nrows());
	x(0); // reset the ascii row pointer to the first row
	for (n = 0; n < x.nrows(); ++n) {
	    for (i = 0; i < x.ncols(); ++i)
	        bt.field(i) = x.field(i);
	    ++x;	// increment the ascii table row
	    ++bt;	// increment the binary table row
	}
	bt.write(fout);
**********************************************************************/


/**********************************************************************
This is another implementation that accomplishes the same thing.
**********************************************************************/
	FitsField<char> *target_c;
	FitsField<char> *source_c;
	FitsField<FitsLong> *target_l;
	FitsField<FitsLong> *source_l;
	FitsField<float> *target_f;
	FitsField<float> *source_f;
	FitsField<double> *target_d;
	FitsField<double> *source_d;
	FitsBase **fb;

	// allocate an array of pointers to the fields
	fb = new FitsBase * [bt.ncols()];
	// and make a FitsField for each field
	for (i = 0; i < bt.ncols(); ++i) {
		// this makes a field exactly like the table field
		fb[i] = FitsBase::make(bt.field(i));
		if (fb[i] == 0)
			exit(-1);
		// and bind the FitsField to the column
		bt.bind(i,*fb[i]);
	}

	// Now, assign data values and write the table
	bt.set_next(bt.nrows());
	x(0); // reset the ascii row pointer to the first row
	for (n = 0; n < x.nrows(); ++n) {
	    for (i = 0; i < x.ncols(); ++i) {
		switch (fb[i]->fieldtype()) {
		    case FITS::CHAR:
			target_c = (FitsField<char> *)fb[i];
			source_c = &((FitsField<char> &)x.field(i));
			strncpy(&((*target_c)(0)),&((*source_c)(0)),
					fb[i]->fitsfieldsize());
			break;
		    case FITS::LONG:
			target_l = (FitsField<FitsLong> *)fb[i];
			source_l = &((FitsField<FitsLong> &)x.field(i));
			(*target_l)(0) = (*source_l)(0);
			break;
		    case FITS::FLOAT:
			target_f = (FitsField<float> *)fb[i];
			source_f = &((FitsField<float> &)x.field(i));
			(*target_f)(0) = (*source_f)(0);
			break;
		    case FITS::DOUBLE:
			target_d = (FitsField<double> *)fb[i];
			source_d = &((FitsField<double> &)x.field(i));
			(*target_d)(0) = (*source_d)(0);
			break;
		    default:
			break;
	 	}
	    }
	    ++x;	// increment the ascii table row
	    ++bt;	// increment the binary table row
	}
	bt.write(fout);
/**********************************************************************/

	delete &x;
}

int main(int argc, const char* argv[])
{
	AsciiTableExtension *at;

	cout << "Test5 -- convert an ASCII table to a binary table" << endl;
	if (argc != 2) {
	    cout << "ex1 <filename>" << "\n";
	    exit(0);
	}
	FitsInput fin(argv[1],FITS::Disk);
	if (fin.err() == FitsIO::IOERR) {
	    cout << "Could not open FITS input.\n";
	    exit(0);
	} else if (fin.err())
	    exit(0);
	const int NMAXERRS = 100;

	FitsOutput fout("tfits5.dat",FITS::Disk);
	if (fout.err() == FitsIO::IOERR) {
		cout << "Could not open FITS output.\n";
		exit(0);
	}

	// Create the initial keyword list
	FitsKeywordList st;
	st.mk(FITS::SIMPLE,True,"Standard FITS format");
	st.mk(FITS::BITPIX,8,"Character Information");
	st.mk(FITS::NAXIS,0,"No image data array present");
	st.mk(FITS::EXTEND,True,"Extension exists");
	st.end();

	// Create and write the initial HDU
	PrimaryArray<unsigned char> hdu1(st);
	if (hdu1.err())
		exit(0);
	cout << "Initial HDU constructed\n";
	// Display the keyword list
	cout << hdu1;
	hdu1.write_hdr(fout); if (hdu1.err()) exit(0);

	int nerrs;
	for(nerrs = 0;
		nerrs < NMAXERRS && fin.rectype() != FITS::EndOfFile; ) {
	    if (fin.rectype() == FITS::HDURecord) {
		switch (fin.hdutype()) {
		    case FITS::PrimaryArrayHDU:
		    case FITS::PrimaryGroupHDU:
		    case FITS::BinaryTableHDU:
		    case FITS::ImageExtensionHDU:
		    case FITS::UnknownExtensionHDU:
			fin.skip_hdu();
			cout << "Skipping HDU\n";
			break;
		    case FITS::AsciiTableHDU:
			at = new AsciiTableExtension(fin);
			if (at->err()) exit(0);
			cout << "\n\nASCII Table   ";
			cvt_table(*at,fout);
			break;
		    default:
			cout << "This isn't supposed to happen\n";
			break;
		}
	    } else if (fin.rectype() == FITS::BadBeginningRecord ||
		       fin.rectype() == FITS::UnrecognizableRecord)
		fin.read_sp();
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
