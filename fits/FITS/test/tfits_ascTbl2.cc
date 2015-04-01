//# tfits_ascTbl2.cc: FITS test program to create a ascii table by using
//# AsciiTableExtension::write_ascTbl_hdr() and PrimaryArray<TYPE>::write_hdr()
//# ( mixture test between write_hdr() and write_***_hdr()).
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

	FitsOutput fout("tfits_ascTbl2.dat",FITS::Disk);
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
	long naxis1 = 0;   // rowlength in bytes( default value will be calculated when passing in 0)
	long naxis2 = 50;  // number of rows in the table        
   int tfields = 8;   // number of columns in the table          
   long tbcol = 0;    // byte offset in row to each column( default value will be calculated when passing in 0) 
	
	 // define the name, datatype, and physical units for the columns 
	 // I - name of each column
         const char *ttype[] = { "GSC_ID", "RA_DEG", "DEC_DEG","POS_ERR", "MAG" ,"MAG_ERR", "MAG_BAND", "PLATE_ID" };
	 // I - value of TFORMn keyword for each column.   
         const char *tform[] = { "I8",     "E4.2",        "E4.2",       "E4.2",      "E4.2",    "E4.2",       "I8",   "A4" };
	 // I - value of TUNITn keyword for each column    
	 const char *tunit[] = { "\0",      "deg",    "deg",    "deg",     "cm",  "cm",     "cm",  "\0" }; 	 
	 // I - value of EXTNAME keyword, if any
	 const char *extname = "GSC_REGION_00100";    
       
      
	AsciiTableExtension at;
	if (at.err()){  exit(0);  }
	cout << "Ascii Table HDU constructed.\n";
	// Write the required keywords for AsciiTableExtension	
	if(!at.write_ascTbl_hdr(fout, naxis1,naxis2,tfields, ttype, &tbcol, tform, tunit, extname )){   
	    cout << "Ascii Table HDU header written.\n";
	}else{
	    cout << "Ascii Table HDU wrote header failded!" << endl;
	    exit( 0 );
	}
	cout << at;
	
	// Construct variables that correspond to the fields
	FitsField<FitsLong> id; 
	FitsField<float> ra;    
	FitsField<float> dec;  
	FitsField<float> pos_err;  
	FitsField<float> mag;      
	FitsField<float> mag_err; 
	FitsField<FitsLong> mag_band; 
	//FitsField<short> classification;
	FitsField<char> plate_id(4); 
	//FitsField<char> multiple;
	// Bind the variables to the fields in the table
	at.bind(0,id);        
	at.bind(1,ra);        
	at.bind(2,dec);       
	at.bind(3,pos_err);  
	at.bind(4,mag);      
	at.bind(5,mag_err);   
	at.bind(6,mag_band);  
	//bt.bind(7,classification);
	at.bind(7,plate_id);   
	//bt.bind(9,multiple);

   cout<<"write the first batch of rows one at a time."<<endl;
	//int i;
	for (i = 0; i < 25; ++i) {
		at.set_next(1);
		id = i;
		ra = 79.8185 + i;
		dec = 0.08173 + i;
		pos_err	= 0.3;
		mag = 12.19;
		mag_err = 0.4;
		mag_band = 1;
		//classification = 3;
		plate_id(0) = '0';
		plate_id(1) = '4';
		plate_id(2) = 'Y';
		plate_id(3) = 'P';
		//multiple = 'T';
		cout << at.currrow() << ": " 
		     << id << " " << ra << " " << dec << "\n";
		at.write(fout);
	}


	cout<<" write the remaining rows 5 at a time."<<endl;
	for (i = 0; i < 5; ++i) {
	    at.set_next(5); // set_next resets the current row
	    for (int j = 0; j < 5; ++j) { // assign values to the next 5 rows
		   id = i*10 + j;
		   ra = 79.8185 + i*10 + j;
		   dec = 0.08173 + i*10 + j;
		   pos_err	= 0.3;
		   mag = 12.19;
		   mag_err = 0.4;
		   mag_band = 1;
		  //classification = 3;
		   plate_id(0) = '0';
		   plate_id(1) = '4';
	  	   plate_id(2) = 'Y';
		   plate_id(3) = 'P';
		   //multiple = 'T';
		   cout << at.currrow() << ": "
		     << id << " " << ra << " " << dec << "\n";
		   ++at; // advance the current row
	    }
	    at.write(fout); // write writes all rows designated in set_next
	}


	return 0;
}
