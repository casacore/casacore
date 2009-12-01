//# t_priArr_imgExt.cc: FITS test program to create a primary array and an image extension
//# using the PrimaryArray<TYPE>::write_priArr_hdr() and
//# ImageExtension<TYPE>::write_imgExt_hdr()
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

# include <fits/FITS/fits.h>
# include <fits/FITS/hdu.h>
# include <fits/FITS/fitsio.h>

# include <casa/iostream.h>
# include <casa/stdlib.h>
# include <casa/namespace.h>


int main()
{
	cout << "Test to create a primary array and an image extension\n";

	FitsOutput fout("t_priArr_imgExt.dat",FITS::Disk);
	if (fout.err() == FitsIO::IOERR) {
		cout << "Could not open FITS output.\n";
		exit(0);
	}

	// We will create an array with 3 rows and 6 columns, as one
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
	/*FitsKeywordList st;
	st.mk(FITS::SIMPLE,True,"Standard FITS format");
	st.mk(FITS::BITPIX,32,"Integer data");
	st.mk(FITS::NAXIS,2,"This is a primary array");
	st.mk(1,FITS::NAXIS,row);
	st.mk(2,FITS::NAXIS,col);
	st.mk(FITS::EXTEND,True,"Extension exists");
	//st.mk(FITS::EXTEND,False,"Extension exists");
	st.spaces();
	st.comment("This is test tfits_priArr.");
	st.spaces();
	st.end();
	*/
	// Create and write the initial HDU
	PrimaryArray<FitsLong> hdu1;
	if (hdu1.err()){
	   cout<<"Error during construction of PrimaryArray."<<endl;
		exit(0);
	}
	cout << "Initial HDU constructed\n";
	// Display the keyword list
	cout << hdu1;
	int bitpix=32, naxis=2;
	long naxes[2];
	naxes[0] = row;
	naxes[1] = col;
	
	if( !hdu1.write_priArr_hdr(fout,True, bitpix, naxis, naxes, True ) ){
	    cout << "Primary Header wrote ok!"<<endl;
	}else{
	    cout<< "Prinary Header wrote error!" << endl;
		 exit(0);
	}
	hdu1.store(&data[0][0],FITS::CtoF);
	cout<<" After hdu1.store()." << endl;
	hdu1.write(fout);
	cout<<" After hdu1.write()." << endl;
	// Create a keyword list for an image extension
	/*
	FitsKeywordList kw;
	kw.mk(FITS::XTENSION,"IMAGE   ","Image extension");
	kw.mk(FITS::BITPIX,32,"Integer data");
	kw.mk(FITS::NAXIS,2,"This is an image");
	kw.mk(1,FITS::NAXIS,row);
	kw.mk(2,FITS::NAXIS,col);
	kw.mk(FITS::PCOUNT,0);
	kw.mk(FITS::GCOUNT,1);
	kw.spaces();
	kw.comment("This is test tfits_priArr.");
	kw.spaces();
	kw.end();
	*/
	ImageExtension<FitsLong> ie;
	if (ie.err())
		exit(0);
	cout << "ImageExtension constructed\n";
	// Display the keyword list
	cout << ie;
	// parameters for write_imgExt_hdr()
	bitpix=32;
	naxis=2;
	long naxes_img[2];
	naxes_img[0] = row;
	naxes_img[1] = col;

	if( !ie.write_imgExt_hdr(fout,bitpix, naxis, naxes_img) ){
      cout << "ImageExtension header wrote ok!"<< endl;
		//return 0;
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
