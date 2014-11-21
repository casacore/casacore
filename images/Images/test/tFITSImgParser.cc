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

#include <casacore/images/Images/FITSImgParser.h>

#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main (int argc, const char* argv[])
{
	try {

	   LogIO os(LogOrigin("tFITSImgParser", "main()", WHERE));

	   Bool isdefaultinput;

	   // Get inputs
	   Input inputs(1);
	   inputs.create("in", "", "Input FITS file");

	   inputs.readArguments(argc, argv);
	   String in1 = inputs.getString("in");
	   String in2;
	   if (in1.empty()) {
		 in1 = "mexinputtest.fits";
		 in2 = "qualityimage.fits";
		 isdefaultinput=True;
	   }
	   else {
	   	isdefaultinput = False;
	   }

	   // create the parser
	   FITSImgParser fitsImg(in1);

	   // process the default image
	   if (isdefaultinput){

		   // check the name information
		   AlwaysAssert(fitsImg.fitsname(True) == String("mexinputtest.fits"), AipsError);

		   // check the number of extensions
		   AlwaysAssert(fitsImg.get_numhdu() == 7, AipsError);

		   // check the first extension with data
		   AlwaysAssert(fitsImg.get_firstdata_index() == 1, AipsError);

		   // check the index of "mexinputtest.fits[sci, 1]"
		   FITSExtInfo extinfo("mexinputtest.fits", 0, "SCI", 1, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == 1, AipsError);
		   AlwaysAssert(fitsImg.find_extension("SCI", 1) == 1, AipsError);

		   // check the index of "mexinputtest.fits[DQ]"
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "DQ", -1, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == 3, AipsError);
		   AlwaysAssert(fitsImg.find_extension("DQ") == 3, AipsError);

		   // check the index of "mexinputtest.fits[dq]"
		   // capitalization should not matter
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "dq", -1, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == 3, AipsError);
		   AlwaysAssert(fitsImg.find_extension("dq") == 3, AipsError);

		   // check the non-existing index of "mexinputtest.fits[dq, 3]"
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "dq", 3, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == -1, AipsError);
		   AlwaysAssert(fitsImg.find_extension("DQ", 3) == -1, AipsError);

		   // check the non-existing index of "mexinputtest.fits[dq, 3]"
		   extinfo = FITSExtInfo("mexinputtest.fits", 0, "dq", 3, True);
		   AlwaysAssert(fitsImg.get_index(extinfo) == -1, AipsError);
		   AlwaysAssert(fitsImg.find_extension("dq", 3) == -1, AipsError);

		   // open the second image;
		   FITSImgParser fitsImg2(in2);

		   // check the name information
		   AlwaysAssert(fitsImg2.fitsname(True) == in2, AipsError);

		   // check the number of extensions
		   AlwaysAssert(fitsImg2.get_numhdu() == 9, AipsError);

		   // check the first extension with data
		   AlwaysAssert(fitsImg2.get_firstdata_index() == 1, AipsError);

		   // check the index of "qualityimage.fits[IFU1.SCI]"
		   AlwaysAssert(fitsImg2.find_extension("IFU1.SCI") == 1, AipsError);

		   // check the index of "qualityimage.fits[IFU1.DQ]"
		   AlwaysAssert(fitsImg2.find_extension("IFU1.DQ") == 3, AipsError);

		   // check the index of "qualityimage.fits[IFU1.dq]"
		   // capitalization should not matter
		   AlwaysAssert(fitsImg2.find_extension("IFU1.dq") == 3, AipsError);

		   // check the non-existing index of "qualityimage.fits[dq, 3]"
		   AlwaysAssert(fitsImg2.find_extension("dq", 3) == -1, AipsError);

		   // check the non-existing index of "qualityimage.fits[dq]"
		   AlwaysAssert(fitsImg2.find_extension("dq") == -1, AipsError);

		   // check whether there exists a quality image
		   AlwaysAssert(fitsImg2.has_qualityimg(), AipsError);

		   // including mask image use this:
		   //AlwaysAssert(fitsImg2.is_qualityimg("[IFU1.SCI,IFU1.ERR,IFU1.DQ]"), AipsError)

		   // this should be a quality image
		   AlwaysAssert(fitsImg2.is_qualityimg("[IFU1.SCI,IFU1.ERR]"), AipsError)

		   // capitalization is not important
		   AlwaysAssert(fitsImg2.is_qualityimg("[ifu1.sci,IFU1.ERR]"), AipsError)

		   // capitalization is not important
		   AlwaysAssert(fitsImg2.is_qualityimg("[ifu1.sci,ifu1.err]"), AipsError)

		   // using not all extensions should not matter
		   AlwaysAssert(fitsImg2.is_qualityimg("[IFU1.SCI,IFU1.ERR]"), AipsError)

		   // including mask image use this:
		   //AlwaysAssert(fitsImg2.is_qualityimg("[IFU2.SCI,IFU2.ERR,IFU2.DQ]"), AipsError)

		   // check the next quality image
		   // note in the data extension, the keywords pointing
		   // to the error extension are filled in small letters
		   AlwaysAssert(fitsImg2.is_qualityimg("[IFU2.SCI,IFU2.ERR]"), AipsError)

		   // including mask image test this:
		   // neglect braces, add whitespaces
		   // AlwaysAssert(fitsImg2.is_qualityimg(" IFU3.SCI, IFU3.DQ "), AipsError)

		   // the extension order does not matter
		   AlwaysAssert(fitsImg2.is_qualityimg("[IFU1.ERR, IFU1.SCI]"), AipsError)

		   // make sure the two string representations are NOT equal
		   AlwaysAssert(fitsImg2.get_extlist_string(String("<A>"), "", "", False) != fitsImg2.get_extlist_string(String("<A>"), "", "", True), AipsError);

		   /*
		   // this is the full call including a mask image
		   // retrieve all information on an extension expression
		   Int data_HDU, error_HDU, mask_HDU, mask_value;
		   String error_type, mask_type;
		   fitsImg2.get_quality_data("[IFU3.SCI, IFU3.DQ]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==7, AipsError);
		   AlwaysAssert(error_HDU==-1, AipsError);
		   AlwaysAssert(error_type=="", AipsError);
		   AlwaysAssert(mask_HDU==8, AipsError);
		   AlwaysAssert(mask_type=="MASKONE", AipsError);
		   AlwaysAssert(mask_value==0, AipsError);
			*/

		   // retrieve all information on an extension expression
		   Int data_HDU, error_HDU, mask_HDU, mask_value;
		   String error_type, mask_type;
		   fitsImg2.get_quality_data("[IFU2.SCI, IFU2.ERR]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==4, AipsError);
		   AlwaysAssert(error_HDU==5, AipsError);
		   AlwaysAssert(error_type=="RMSE", AipsError);
		   AlwaysAssert(mask_HDU==-1, AipsError);
		   AlwaysAssert(mask_type=="", AipsError);
		   AlwaysAssert(mask_value==0, AipsError);

		   /*
		   // this is the full call including a mask image
		   fitsImg2.get_quality_data("[IFU1.SCI,IFU1.ERR,IFU1.DQ]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);
		   // check all return values
		   AlwaysAssert(data_HDU==1, AipsError);
		   AlwaysAssert(error_HDU==2, AipsError);
		   AlwaysAssert(error_type=="MSE", AipsError);
		   AlwaysAssert(mask_HDU==3, AipsError);
		   AlwaysAssert(mask_type=="FLAG16BIT", AipsError);
		   AlwaysAssert(mask_value==16383, AipsError);
			*/

		   // retrieve all information on an extension expression
		   fitsImg2.get_quality_data("[IFU1.SCI,IFU1.ERR]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==1, AipsError);
		   AlwaysAssert(error_HDU==2, AipsError);
		   AlwaysAssert(error_type=="MSE", AipsError);
		   AlwaysAssert(mask_HDU==-1, AipsError);
		   AlwaysAssert(mask_type=="", AipsError);
		   AlwaysAssert(mask_value==0, AipsError);

		   // retrieve all information on an extension expression
		   fitsImg2.get_quality_data("[SCIEXT]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values;
		   // they must be the default ones
		   AlwaysAssert(data_HDU<0, AipsError);
		   AlwaysAssert(error_HDU < 0, AipsError);
		   AlwaysAssert(error_type.size()==0, AipsError);
		   AlwaysAssert(mask_HDU<0, AipsError);
		   AlwaysAssert(mask_type.size()==0, AipsError);
		   AlwaysAssert(mask_value==0, AipsError);

		   // test the copy constructor
		   FITSImgParser fitsImg3(fitsImg2);

		   // test the number of extensions
		   AlwaysAssert(fitsImg3.get_numhdu()==fitsImg2.get_numhdu(), AipsError);

		   /* this would be the full call, including mask images
		   // retrieve all information on an extension expression
		   fitsImg3.get_quality_data("[IFU1.SCI,IFU1.ERR,IFU1.DQ]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==1, AipsError);
		   AlwaysAssert(error_HDU==2, AipsError);
		   AlwaysAssert(error_type=="MSE", AipsError);
		   AlwaysAssert(mask_HDU==3, AipsError);
		   AlwaysAssert(mask_type=="FLAG16BIT", AipsError);
		   AlwaysAssert(mask_value==16383, AipsError);
			*/

		   // retrieve all information on an extension expression
		   fitsImg3.get_quality_data("[IFU1.SCI,IFU1.ERR,IFU1.DQ]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==1, AipsError);
		   AlwaysAssert(error_HDU==2, AipsError);
		   AlwaysAssert(error_type=="MSE", AipsError);
		   AlwaysAssert(mask_HDU==-1, AipsError);
		   AlwaysAssert(mask_type=="", AipsError);
		   AlwaysAssert(mask_value==0, AipsError);

		   // test the assignment
		   FITSImgParser fitsImg4=fitsImg2;

		   // test the number of extensions
		   AlwaysAssert(fitsImg4.get_numhdu()==fitsImg2.get_numhdu(), AipsError);

		   /* this would be the full call, including mask images
		   // retrieve all information on an extension expression
		   fitsImg4.get_quality_data("[IFU1.SCI,IFU1.ERR,IFU1.DQ]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==1, AipsError);
		   AlwaysAssert(error_HDU==2, AipsError);
		   AlwaysAssert(error_type=="MSE", AipsError);
		   AlwaysAssert(mask_HDU==3, AipsError);
		   AlwaysAssert(mask_type=="FLAG16BIT", AipsError);
		   AlwaysAssert(mask_value==16383, AipsError);
			*/

		   // retrieve all information on an extension expression
		   fitsImg4.get_quality_data("[IFU1.SCI,IFU1.ERR,IFU1.DQ]", data_HDU, error_HDU, error_type, mask_HDU, mask_type, mask_value);

		   // check all return values
		   AlwaysAssert(data_HDU==1, AipsError);
		   AlwaysAssert(error_HDU==2, AipsError);
		   AlwaysAssert(error_type=="MSE", AipsError);
		   AlwaysAssert(mask_HDU==-1, AipsError);
		   AlwaysAssert(mask_type=="", AipsError);
		   AlwaysAssert(mask_value==0, AipsError);

	   }
	   else {
		   cerr << "Fits file: " << fitsImg.fitsname(True) << "\n";
		   cerr << "Number of extensions:  " << fitsImg.get_numhdu() << "\n";
		   cerr << "First index with data: " << fitsImg.get_firstdata_index() << "\n";
		   cerr << "File contains quality image: " << fitsImg.has_qualityimg() << "\n";
		   cerr << "String representation of all extensions with data:\n" << fitsImg.get_extlist_string(String("<A>")) << endl;
	   }
	} catch (AipsError x) {
	   cerr << "aipserror: error " << x.getMesg() << endl;
	   return 1;
	}

	cout<< "[tFITSImgParser.cc] End of tFITSImgParser.cc."<< endl;
	cout << "ok " << endl;
	return 0;
}


