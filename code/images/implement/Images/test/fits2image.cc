//# fits2image.cc: conversion to aips++ native tables
//# Copyright (C) 1996,1997,1999,2000
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

#include <aips/aips.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Exceptions/Error.h>
#include <aips/FITS/FITS.h>
#include <aips/Inputs/Input.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>

#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageFITSConverter.h>
#include <aips/Tasking/AppInfo.h>

#include <iostream.h>

int main(int argc, char **argv)
{
  try {
    // Inputs
    Input inp(1);
    inp.version("2.0: PagedImage with coordinate conversion");

    String root = Aipsrc::aipsRoot();
    String name = root + "/data/demo/Images/test_image.fits";
    inp.create("in", name, "Input FITS file name", "string");
    inp.create("out", "out.image", "Output AIPS++ Image name", "string");
    inp.create("overwrite", "True", "Allow output to be overwritten?",
                "Bool");
    inp.create("zero", "False", "Zero blanks?", "Bool");
    inp.readArguments(argc, argv);

    Bool overwrite=inp.getBool("overwrite");
    Bool zero =inp.getBool("zero");
    String fitsFile = inp.getString("in");
    String outFile = inp.getString("out");
    if(outFile.empty() ) {
       File inFile(fitsFile);
       outFile=inFile.path().expandedName()+".image";
    }

    String error;
    ImageInterface<Float>* pOutImage;
    Bool ok = ImageFITSConverter::FITSToImage(pOutImage, error, outFile,
					      fitsFile, 0,
					      AppInfo::memoryInMB(),
					      overwrite, zero);
    LogIO os(LogOrigin("fits2image", "main()", WHERE));
    if (!ok) {
        os << LogIO::SEVERE << error << LogIO::EXCEPTION;
    } else {
       os << "table " << outFile << " write complete " << endl;
    }
    delete pOutImage;    


  } catch (AipsError x) {
    cout << "exception: " << x.getMesg() << endl;
    return 1;
  } 
  
  return 0;  
}

