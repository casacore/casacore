//# fits2image.cc: conversion to aips++ native tables
//# Copyright (C) 1996,1997,1999,2000,2001
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

#include <casa/aips.h>
#include <casa/System/Aipsrc.h>
#include <casa/Exceptions/Error.h>
#include <fits/FITS/BasicFITS.h>
#include <casa/Inputs/Input.h>
#include <casa/OS/File.h>
#include <casa/OS/Path.h>

#include <images/Images/PagedImage.h>
#include <images/Images/ImageFITSConverter.h>
#include <casa/OS/HostInfo.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
int main(int argc, char **argv)
{
  try {
    // Inputs
    Input inp(1);
    inp.version("2.0: PagedImage with coordinate conversion");

    String root = Aipsrc::aipsRoot();
    String name = root + "/data/demo/Images/test_image.fits";
    inp.create("in", name, "Input FITS file name", "string");
    inp.create("out", "fits2image_tmp.out", "Output AIPS++ Image name",
	       "string");
    inp.create("overwrite", "True", "Allow output to be overwritten?",
                "Bool");
    inp.create("zero", "False", "Zero blanks?", "Bool");
    inp.readArguments(argc, argv);

    Bool overwrite=inp.getBool("overwrite");
    Bool zeroBlanks =inp.getBool("zero");
    //    Bool oldParser =inp.getBool("old");
    String fitsFile = inp.getString("in");
    String outFile = inp.getString("out");
    if(outFile.empty() ) {
       File inFile(fitsFile);
       outFile=inFile.path().expandedName()+".image";
    }

    String error;
    ImageInterface<Float>* pOutImage;
    Bool ok=False;
    uInt whichRep = 0;
    uInt whichHDU = 0;
    ok = ImageFITSConverter::FITSToImage(pOutImage, error, outFile,
					 fitsFile, whichRep, whichHDU, 
					 HostInfo::memoryFree()/1024,
					 overwrite, zeroBlanks);
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

