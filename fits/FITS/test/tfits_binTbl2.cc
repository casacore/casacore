//# tfits_binTbl1.cc: FITS test program to create a binary table by using
//# BinaryTableExtension::write_binTbl_hdr(). This program also tests the mixture use of
//# write_hdr() and write_binTbl_hdr() methods.
//# Modified from Copyright (C) 1993,1994,1998,2001
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


int main()
{
	cout << "Test to create a binary table\n";

	FitsOutput fout("tfits_binTbl2.dat",FITS::Disk);
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
	
	// Create and write the binary table HDU
	long naxis2 = 50;     // I - number of rows in the table 
	long pcount= 0;       // I - size of the variable length heap area            
   int tfields = 10;      // I - number of columns in the table          
      
	
	 // define the name, datatype, and physical units for the columns 
         const char *ttype[] = { "GSC_ID", "RA_DEG", "DEC_DEG","POS_ERR", "MAG" ,"MAG_ERR", "MAG_BAND", "CLASS", "PLATE_ID","MULTIPLE" };  // I - name of each column 
         const char *tform[] = { "J",     "E",        "E",       "E",      "E",    "E",       "I", "I",   "4A", "1A" };    // I - value of TFORMn keyword for each column.
         const char *tunit[] = { "\0",      "deg",    "deg",    "deg",     "cm",  "cm",     "cm", "cm", "\0","\0" }; // I - value of TUNITn keyword for each column
	 const char *extname = "GSC_REGION_00100";   // I - value of EXTNAME keyword, if any 
       
      
	BinaryTableExtension bt;
	if (bt.err())
		exit(0);
	cout << "Binary Table HDU constructed.\n";
	// Display the keyword list
	
	if( !bt.write_binTbl_hdr(fout, naxis2,tfields,ttype, tform, tunit, extname, pcount ) ){  
	   cout << "Binary Table HDU header has been written.\n";
	}else{
	   cout<< "Binary Table HDU wrote header failed!" << endl;
		exit( 0 );
	}
	cout << bt;

	// Construct variables that correspond to the fields
	FitsField<FitsLong> id;
	FitsField<float> ra;
	FitsField<float> dec;
	FitsField<float> pos_err;
	FitsField<float> mag;
	FitsField<float> mag_err;
	FitsField<short> mag_band;
	FitsField<short> classification;
	FitsField<char> plate_id(4);
	FitsField<char> multiple;
	// Bind the variables to the fields in the table
	bt.bind(0,id);
	bt.bind(1,ra);
	bt.bind(2,dec);
	bt.bind(3,pos_err);
	bt.bind(4,mag);
	bt.bind(5,mag_err);
	bt.bind(6,mag_band);
	bt.bind(7,classification);
	bt.bind(8,plate_id);
	bt.bind(9,multiple);

   // write the first batch of rows one at a time
	//int i;
	for (i = 0; i < 25; ++i) {
		bt.set_next(1);
		id = i;
		ra = 79.8185 + i;
		dec = 0.08173 + i;
		pos_err	= 0.3;
		mag = 12.19;
		mag_err = 0.4;
		mag_band = 1;
		classification = 3;
		plate_id(0) = '0';
		plate_id(1) = '4';
		plate_id(2) = 'Y';
		plate_id(3) = 'P';
		multiple = 'T';
		cout << bt.currrow() << ": " 
		     << id << " " << ra << " " << dec << "\n";
		bt.write(fout);
	}


	// write the remaining rows 5 at a time
	for (i = 0; i < 5; ++i) {
	    bt.set_next(5); // set_next resets the current row
	    for (int j = 0; j < 5; ++j) { // assign values to the next 5 rows
		id = i*10 + j;
		ra = 79.8185 + i*10 + j;
		dec = 0.08173 + i*10 + j;
		pos_err	= 0.3;
		mag = 12.19;
		mag_err = 0.4;
		mag_band = 1;
		classification = 3;
		plate_id(0) = '0';
		plate_id(1) = '4';
		plate_id(2) = 'Y';
		plate_id(3) = 'P';
		multiple = 'T';
		cout << bt.currrow() << ": "
		     << id << " " << ra << " " << dec << "\n";
		++bt; // advance the current row
	    }
	    bt.write(fout); // write writes all rows designated in set_next
	}

	return 0;
}
