//# image2fits.cc: conversion from aips++ native tables to FITS
//# Copyright (C) 1994,1995,1997,1999,2000,2001
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

#include <casa/aips.h>

#include <images/Images/PagedImage.h>
#include <images/Images/ImageFITSConverter.h>
#include <images/Regions/RegionHandler.h>

#include <casa/Arrays/Array.h>
#include <casa/Containers/MapIO.h>
#include <casa/Containers/OrderedMap.h>
#include <casa/Exceptions/Error.h>
#include <fits/FITS/BasicFITS.h>
#include <casa/Inputs/Input.h>
#include <casa/Arrays/IPosition.h>
#include <casa/BasicSL/Constants.h>
#include <casa/OS/File.h>
#include <casa/OS/Path.h>

#include <casa/iostream.h>


#include <casa/namespace.h>
int main(int argc, const char *argv[])
{
    try {
	// Inputs
	Input inp(1);
	inp.version(""); // By setting to null, we turn of the announcement

        String name = "test_image.im";
	inp.create("in", name, "Input AIPS++ Image name", "string");
	inp.create("mask", "default", "Mask to apply", "string");
	inp.create("out", "image2fits_tmp.out", "Output FITS file name",
		   "string");
	inp.create("overwrite", "True", "Allow output to be overwritten?",
		   "Bool");
	inp.create("verbose", "True", "Verbose?", "Bool");
	inp.create("do16", "False", "16 bit integer or 32 bit floating point?", "Bool");
	inp.readArguments(argc, argv);
    
	Bool verbose=inp.getBool("verbose");
	Bool overwrite=inp.getBool("overwrite");
	Bool do16=inp.getBool("do16");
    
	File inputFile(inp.getString("in"));
	if (!inputFile.isReadable()) 
	    throw (AipsError ("input file unreadable"));
    
	String out(inp.getString("out"));
	if (out == "") out = "out.fits";
    
        String mask = inp.getString("mask");

	// Get the image from disk
	PagedImage<Float> image(inputFile.path().originalName(), True);
        if (mask!=String("default")) image.setDefaultMask(mask);
	IPosition imageShape(image.shape());
	if(verbose) {
	    cout << "Read input image " << inputFile.path().originalName()
		 << " successfully" << endl;
	    cout << "Shape is " << imageShape << endl;
	    cout << "Applying mask " << mask << endl;
	}

	String error;
        Int bits = -32;
        if (do16) bits = 16;
        Bool degLast = False;
	Bool ok = ImageFITSConverter::ImageToFITS(error, image, out,
						  64, True, True, bits, 1, -1,
						  overwrite, degLast, verbose);
	if (!ok) {
	    cout << "Error writing FITS file: " << error << endl;
	    return 1;
	}

    } catch (AipsError x) {
	cout << "Exception: " << x.getMesg() << endl;
	return 1;
    } 
    
    return 0;  
}
