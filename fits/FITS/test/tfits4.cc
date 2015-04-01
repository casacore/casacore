//# tfits4.cc: FITS test program to create a binary table
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

#include <casacore/casa/namespace.h>
int main()
{
	cout << "Test to create a binary table\n";

	FitsOutput fout("tfits4.dat",FITS::Disk);
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
	st.mk("FILETYPE","GSC_REGION","Indicates file type");
	st.mk(FITS::ORIGIN,"ST ScI","Space Telescope Science Institute");
	st.mk(FITS::DATE,"01/06/89","Date of issue (dd/mm/yy)");
	st.spaces();
	st.comment("                         THE GUIDE STAR CATALOG");
	st.comment("");
	st.comment("             An all-sky astrometric and photometric catalog");
	st.comment("             prepared for the operation of the Hubble Space");
	st.comment("                               Telescope.");
	st.comment("");
	st.comment("             Copyright,  1989,  Association of Universities");
	st.comment("                     for Research in Astronomy, Inc.");
	st.comment("");
	st.comment("This file contains data for one of the  9537  regions  constituting  the");
	st.comment("Guide Star Catalog (GSC). Additional information on the GSC may be found");
	st.comment("in  accompanying  scientific  publications  as  well  as in comments and");
	st.comment("tables elsewhere on this set of volumes.");
	st.comment("");
	st.comment("The Guide Star Catalog (GSC) was prepared by the Space Telescope Science");
	st.comment("Institute (ST ScI),  3700 San Martin Drive,  Baltimore,  MD 21218,  USA.");
	st.comment("ST ScI  is  operated  by the Association of Universities for Research in");
	st.comment("Astronomy, Inc. (AURA), under contract with the National Aeronautics and");
	st.comment("Space Administration (NASA).");
	st.spaces();
	st.end();

	// Create and write the initial HDU
	PrimaryArray<unsigned char> hdu1(st);
	if (hdu1.err())
		exit(0);
	cout << "Initial HDU constructed\n";
	// Display the keyword list
	cout << hdu1;
	hdu1.write_hdr(fout);

	// Create the keyword list for the binary table
	FitsKeywordList kw;
	kw.mk(FITS::XTENSION,"BINTABLE","Binary Table Extension");
	kw.mk(FITS::BITPIX,8,"Character Information");
	kw.mk(FITS::NAXIS,2,"Two-dimensional table");
	kw.mk(1,FITS::NAXIS,33,"Number of bytes per row");
	kw.mk(2,FITS::NAXIS,50,"Number of rows");
	kw.mk(FITS::PCOUNT,0,"No random parameters");
	kw.mk(FITS::GCOUNT,1,"Only one group");
	kw.mk(FITS::TFIELDS,10,"Ten fields per row");
	kw.spaces();
	kw.mk(FITS::EXTNAME,"GSC_REGION_00100","GSC Region No. 00100");
	kw.mk(FITS::EXTVER,1,"Integer Version Number");
	kw.spaces();
	kw.mk(1,FITS::TTYPE,"GSC_ID","ID within Region");
	kw.mk(1,FITS::TFORM,"J","Integer, 5 character field (I5.5 Style)");
	kw.spaces();
	kw.mk(2,FITS::TTYPE,"RA_DEG","Right Ascension - Decimal Degrees (0 to 360)");
	kw.mk(2,FITS::TFORM,"E","Floating, 9 character field");
	kw.spaces();
	kw.mk(3,FITS::TTYPE,"DEC_DEG","Declination - Decimal Degrees (-90 to +90)");
	kw.mk(3,FITS::TFORM,"E","Floating, 9 character field");
	kw.spaces();
	kw.mk(4,FITS::TTYPE,"POS_ERR","Position Error in Arc Seconds");
	kw.mk(4,FITS::TFORM,"E","Floating, 5 character field");
	kw.spaces();
	kw.mk(5,FITS::TTYPE,"MAG","Magnitude");
	kw.mk(5,FITS::TFORM,"E","Floating, 5 character field");
	kw.spaces();
	kw.mk(6,FITS::TTYPE,"MAG_ERR","Magnitude error");
	kw.mk(6,FITS::TFORM,"E","Floating, 4 character field");
	kw.spaces();
	kw.mk(7,FITS::TTYPE,"MAG_BAND","Magnitude Band");
	kw.mk(7,FITS::TFORM,"I","Integer, 2 character field (I2.2 Style)");
	kw.spaces();
	kw.mk(8,FITS::TTYPE,"CLASS","Classification");
	kw.mk(8,FITS::TFORM,"I","Integer, 1 character field");
	kw.spaces();
	kw.mk(9,FITS::TTYPE,"PLATE_ID","GSSS Internal Plate Number");
	kw.mk(9,FITS::TFORM,"4A","4 character field");
	kw.spaces();
	kw.mk(10,FITS::TTYPE,"MULTIPLE","(T/F) Flag for additional entries");
	kw.mk(10,FITS::TFORM,"1A","Logical flag, 1 character field");
	kw.spaces();
	kw.end();

	// Create and write the binary table HDU
	BinaryTableExtension bt(kw);
	if (bt.err())
		exit(0);
	cout << "Binary Table HDU constructed\n";
	// Display the keyword list
	cout << bt;
	bt.write_hdr(fout);

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
	int i;
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
