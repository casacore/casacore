//# tFITSImgParser.cc:  test the FITSImgParser class
//# Copyright (C) 1994,1995,1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
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

#include <images/Images/FITSImgParser.h>

#include <casa/Inputs/Input.h>
#include <casa/OS/Path.h>
#include <casa/Logging/LogIO.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

int main (int argc, const char* argv[])
{
	try {

	   LogIO os(LogOrigin("tFITSImgParser", "main()", WHERE));

	   Bool isdefinput=True;

	   // Get inputs
	   Input inputs(1);
	   inputs.create("in", "", "Input FITS file");

	   inputs.readArguments(argc, argv);
	   String in = inputs.getString("in");
	   if (in.empty()) {
		 in = "mexinputtest.fits";
	   }
	   else {
		   isdefinput = False;
	   }
	   Path p(in);

	   // create the parser
	   FITSImgParser fitsImg(in);

	   // process the default image
	   if (isdefinput){

		   // check the name information
		   AlwaysAssert(fitsImg.fitsname(True) == String("mexinputtest.fits"), AipsError);

		   // check the number of extensions
		   AlwaysAssert(fitsImg.get_numhdu() == 7, AipsError);

		   // check the first extension with data
		   AlwaysAssert(fitsImg.get_firstdata_index() == 1, AipsError);

		   // check the index of "mexinputtest.fits[sci, 1]"
		   FITSExtInfo extinfo("mexinputtest.fits", 0, "SCI", 1, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == 1, AipsError);

		   // check the index of "mexinputtest.fits[DQ]"
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "DQ", -1, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == 3, AipsError);

		   // check the index of "mexinputtest.fits[dq]"
		   // capitalization should not matter
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "dq", -1, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == 3, AipsError);

		   // check the non-existing index of "mexinputtest.fits[dq, 3]"
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "dq", 3, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == -1, AipsError);

		   // check the non-existing index of "mexinputtest.fits[dq, 3]"
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "dq", 3, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == -1, AipsError);
	   }
	   else {
		   cerr << "Fits file: " << fitsImg.fitsname(True) << "\n";
		   cerr << "Number of extensions:  " << fitsImg.get_numhdu() << "\n";
		   cerr << "First index with data: " << fitsImg.get_firstdata_index() << "\n";
		   cerr << "String representation of all extensions with data:\n" << fitsImg.get_ext_list(String("<A>")) << endl;
	   }
	} catch (AipsError x) {
	   cerr << "aipserror: error " << x.getMesg() << endl;
	   return 1;
	}

	cout<< "[tFITSImgParser.cc] End of tFITSImgParser.cc."<< endl;
	cout << "ok " << endl;
	return 0;
}


