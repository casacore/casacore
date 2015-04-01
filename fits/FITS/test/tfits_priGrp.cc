//# tfits_priGrp.cc: FITS test program to create a random group by using
//# PrimaryGroup::write_priGrp_hdr().
//# Modified form Copyright (C) 1993,1994,2001
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
# include <casacore/casa/namespace.h>


# include <casacore/casa/Arrays/Vector.h>

int main()
{
	cout << "Test to create a random group\n";

	FitsOutput fout("tfits_priGrp.dat",FITS::Disk);
	if (fout.err() == FitsIO::IOERR) {
		cout << "Could not open FITS output.\n";
		exit(0);
	}

   int naxis = 2;  // number of axes + 1.
	int no_parms = 4;  // we will create 4 parms in each group
	int no_groups = 6; // and 6 groups
	int no_data = 8;   // and 4 data points in each group
	long naxes[2];
	naxes[0]= 0; // must be zero for random group
	naxes[1] = no_data;
   
	// Create a keyword list for a random group
	/*
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
	*/
	PrimaryGroup<FitsLong> pg;
	if (pg.err())
		exit(0);

	pg.write_priGrp_hdr(fout,True, 32, naxis,naxes, no_parms, no_groups );
	cout << "PrimaryGroup constructed\n";
	cout << "Data type   " << pg.datatype() << "\n"
	     << "Data size   " << pg.fitsdatasize() << "\n"
	     << "Dimensions  " << pg.dims() << "\n";
	for (int n = 0; n < pg.dims(); n++)
		cout << "Axis " << (n + 1) << " size "
		     << pg.dim(n) << endl;
	cout << pg << endl; // Display the keyword list
	
	
	int i, j;
	for (i = 0; i < no_groups; ++i) {
	   for (j = 0; j < no_parms; ++j){
	 	   pg.rawparm(j) = (FitsLong)( i * 10 + j);
		}  	// assign the parms
	   for (j = 0; j < no_data; ++j)
		{ pg.data(j) = (FitsLong)(i * 10 + j + 5); }   	// assign the data
	   pg.write(fout);			// write the group
	}
	cout << "PrimaryGroup data written\n";
   //fout.~FitsOutput();
	return 0;
}
