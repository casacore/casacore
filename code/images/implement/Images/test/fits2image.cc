//# fits2image.cc: conversion to aips++ native tables
//# Copyright (C) 1996,1997,1999
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
    inp.Version("2.0: PagedImage with coordinate conversion");

    String root = Aipsrc::aipsRoot();
    String name = root + "/code/trial/implement/Images/test/test_image.FITS";
    inp.Create("in", name, "Input FITS file name", "string");
    inp.Create("out", "out.image", "Output AIPS++ Image name", "string");
    inp.ReadArguments(argc, argv);

    String fitsFile = inp.GetString("in");
    String outFile = inp.GetString("out");
    if(outFile.empty() ) {
       File inFile(fitsFile);
       outFile=inFile.path().expandedName()+".image";
    }

    String error;
    PagedImage<Float>* pOutImage;
    ImageFITSConverter::FITSToImage(pOutImage, error, outFile, fitsFile, 0,
                                    AppInfo::memoryInMB());
    LogIO os(LogOrigin("fits2image", "main()", WHERE));
    if (!pOutImage) {
        os << LogIO::SEVERE << error << LogIO::EXCEPTION;
    }

    delete pOutImage;    
    os << "table " << outFile << " write complete " << endl;

  } catch (AipsError x) {
    cout << "exception: " << x.getMesg() << endl;
    return 1;
  } end_try;
  
  return 0;  
}

