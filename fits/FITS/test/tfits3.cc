//# tfits3.cc: FITS test program to create a random group
//# Copyright (C) 1993,1994,2001
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

#include <casacore/casa/namespace.h>
int main()
{
	cout << "Test to create a random group\n";

	FitsOutput fout("tfits3.dat",FITS::Disk);
	if (fout.err() == FitsIO::IOERR) {
		cout << "Could not open FITS output.\n";
		exit(0);
	}

	int no_parms = 3;  // we will create 3 parms in each group
	int no_groups = 6; // and 6 groups
	int no_data = 4;   // and 4 data points in each group

	// Create a keyword list for a random group
	FitsKeywordList kw;
	kw.mk(FITS::SIMPLE,True,"Standard FITS format");
	kw.mk(FITS::BITPIX,32,"Integer data");
	kw.mk(FITS::NAXIS,2);
	kw.mk(1,FITS::NAXIS,0);
	kw.mk(2,FITS::NAXIS,no_data);
	kw.mk(FITS::GROUPS,True,"Random Group structure");
	kw.mk(FITS::PCOUNT,no_parms);
	kw.mk(FITS::GCOUNT,no_groups);
	kw.spaces();
	kw.comment("This is test 3.");
	kw.spaces();
	kw.end();

	PrimaryGroup<FitsLong> pg(kw);
	if (pg.err())
		exit(0);

	cout << "PrimaryGroup constructed\n";
	cout << "Data type   " << pg.datatype() << "\n"
	     << "Data size   " << pg.fitsdatasize() << "\n"
	     << "Dimensions  " << pg.dims() << "\n";
	for (int n = 0; n < pg.dims(); n++)
		cout << "Axis " << (n + 1) << " size "
		     << pg.dim(n) << endl;
	cout << pg << endl; // Display the keyword list
	pg.write_hdr(fout);
	int i, j;
	for (i = 0; i < no_groups; ++i) {
	    for (j = 0; j < no_parms; ++j)
		pg.rawparm(j) = i * 10 + j;  	// assign the parms
	    for (j = 0; j < no_data; ++j)
		pg.data(j) = i * 10 + j + 5;   	// assign the data
	    pg.write(fout);			// write the group
	}
	cout << "PrimaryGroup data written\n";

	return 0;
}
