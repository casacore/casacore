//# image2fits.cc: conversion from aips++ native tables to FITS
//# Copyright (C) 1994,1995,1997,1999
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
//-----------------------------------------------------------------------------
//# usage:  image2fits <image ename> <new FITS file name>
//#
//-----------------------------------------------------------------------------
#include <aips/aips.h>
#include <aips/Tasking/Aipsrc.h>

#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageFITSConverter.h>

#include <aips/Arrays/Array.h>
#include <aips/Containers/MapIO.h>
#include <aips/Containers/OrderedMap.h>
#include <aips/Exceptions/Error.h>
#include <aips/FITS/FITS.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>

#include <iostream.h>

Int main(int argc, char *argv[])
{
    try {
	// Inputs
	Input inp(1);
	inp.Version(""); // By setting to null, we turn of the announcement

        String root = Aipsrc::aipsRoot();
        String name = root + "/code/trial/implement/Images/test/test_image";
	inp.Create("in", name, "Input AIPS++ Image name", "string");
	inp.Create("out", "out.fits", "Output FITS file name", "string");
	inp.Create("overwrite", "False", "Allow output to be overwritten?",
		   "Bool");
	inp.Create("verbose", "False", "Verbose?", "Bool");
	inp.ReadArguments(argc, argv);
    
	Bool verbose=inp.GetBool("verbose");
	Bool overwrite=inp.GetBool("overwrite");
    
	File inputFile(inp.GetString("in"));
	if (!inputFile.isReadable()) 
	    throw (AipsError ("input file unreadable"));
    
	String out(inp.GetString("out"));
	if (out == "") out = "out.fits";
    
	// Get the image from disk
	PagedImage<Float> image(inputFile.path().originalName());
	IPosition imageShape(image.shape());
	if(verbose) {
	    cout << "Read input image " << inputFile.path().originalName()
		 << " successfully" << endl;
	    cout << "Shape is " << imageShape << endl;
	}

	String error;
	Bool ok = ImageFITSConverter::ImageToFITS(error, image, out,
						  64, True, True, -32, 1, -1,
						  overwrite);
	if (!ok) {
	    cout << "Error writing FITS file: " << error << endl;
	    return 1;
	}

    } catch (AipsError x) {
	cout << "Exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    
    return 0;  
}
