//# tfits_imgExt2.cc: FITS test program to create a primary array and an image extension
//# using the PrimaryArray<TYPE>::write_hdr() and ImageExtension<TYPE>::write_imgExt_hdr()
//# ( mixture test between write_hdr() and write_***_hdr()).
//# Modified from Copyright (C) 1993,1994,2001
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
	cout << "Test to create a primary array and an image extension\n";

	FitsOutput fout("tfits_imgExt2.dat",FITS::Disk);
	if (fout.err() == FitsIO::IOERR) {
		cout << "Could not open FITS output.\n";
		exit(0);
	}

	// Create the initial keyword list
	// We will create an array with 10 rows and 10 columns, as one
	// would normally do in C.
	const int row = 10;
	const int col = 10;
	FitsLong data[col][row];
	// And, we will populate it with data
	int i, j;
	for (i = 0; i < col; ++i)
	  for(j = 0; j < row; ++j)
	    data[i][j] = j * 10 + i;

	// Create the initial keyword list
	FitsKeywordList st;
	st.mk(FITS::SIMPLE,True,"Standard FITS format");
	st.mk(FITS::BITPIX,32,"Integer data");
	st.mk(FITS::NAXIS,2,"This is a primary array");
	st.mk(1,FITS::NAXIS,row);
	st.mk(2,FITS::NAXIS,col);
	st.mk(FITS::EXTEND,True,"Extension exists");
	//st.mk(FITS::EXTEND,False,"Extension exists");
	st.spaces();
	st.comment("This is test tfits_binTbl1.cc.");
	st.spaces();
	st.end();

	// Create and write the initial HDU
	PrimaryArray<FitsLong> hdu1(st);
	if (hdu1.err())
		exit(0);
	cout << "Initial HDU constructed\n";
	// Display the keyword list
	cout << hdu1;
	if( !hdu1.write_hdr(fout) ){
	    cout << "Primary Header wrote ok!"<<endl;
		 //return 0;
	}else{
	    cout<< "Prinary Header wrote error!" << endl;
		 exit(0);
	}
	hdu1.store(&data[0][0],FITS::CtoF);
	hdu1.write(fout);
		
	// Create an image extension
	ImageExtension<FitsLong> ie;
	if (ie.err())
		exit(0);
	cout << "ImageExtension constructed\n";
	// Display the keyword list
	cout << ie;
	// parameters for write_imgExt_hdr()
	int bitpix=32;
	int naxis=2;
	long naxes_img[2];
	naxes_img[0] = row;
	naxes_img[1] = col;

	if( !ie.write_imgExt_hdr(fout,bitpix, naxis, naxes_img) ){
      cout << "ImageExtension wrote header  ok!"<< endl;
		//return 0;
	}else{
	   cout<< "ImageExtension wrote header  failed!" << endl;
		exit( 0 );	
	}
	ie.set_next(row * col);		// setup to write the whole array
	cout<<"set_next() done." << endl;

	for (i = 0; i < row; ++i){
	  for(j = 0; j < col; ++j)
	  {
	  	  ie.data(i,j) = i * 10 + j; // assign the data
	     //cout<< "ie.data(i,j) = " << ie.data(i,j) << endl;
	  }	
   }
	cout<< "data(i,j) assignment ok."<< endl;
	ie.write(fout);			// write the data
   cout << "ImageExtension data wrote!"<< endl;
	return 0;
}
