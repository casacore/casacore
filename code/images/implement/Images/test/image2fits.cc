//# image2fits.cc: conversion from aips++ native tables to FITS
//# Copyright (C) 1994,1995
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

#include <trial/ImgCrdSys/ImageCoordinate.h>
#include <trial/Images/PagedImage.h>

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
    inp.Version(" ");
    inp.Create("in", " ", "Input AIPS++ Image name", "string");
    inp.Create("out", " ", "Output FITS file name", "string");
    inp.Create("verbose", "False", "Verbose?", "Bool");
    inp.ReadArguments(argc, argv);
    
    Bool verbose=inp.GetBool("verbose");
    
    File inputFile(inp.GetString("in"));
    if (!inputFile.isReadable()) 
      throw (AipsError ("input file unreadable"));
    
    
    // Get the image from disk
    PagedImage<Float> image(inputFile.path().originalName());
    IPosition imageShape(image.shape());
    if(verbose) {
      cout << "Read input image " << inputFile.path().originalName()
	  << " successfully" << endl;
      cout << "Shape is " << imageShape << endl;
    };

    // Use getSlice to get the whole thing at once
    Array<Float> limage(image.shape());
    uInt numberOfAxes = imageShape.nelements();
    IPosition ipStart(numberOfAxes,0);
    IPosition ipStride(numberOfAxes,1);
    image.getSlice(limage,ipStart,imageShape,ipStride);

    // Units are not defined in Image yet
    String units = image.units().getName();;

    // Fix once axis names are tranlated properly
    Block<String> axisNames = image.coordinates().axisNames();
    Vector<Double> referenceValuesD = image.coordinates().referenceValues();
    Vector<Double> referencePixelsD = image.coordinates().referencePixels();
    Vector<Double> deltasD = image.coordinates().deltas();
    Vector<String> names(numberOfAxes);
    Vector<Float> referenceValues(numberOfAxes);
    Vector<Float> referencePixels(numberOfAxes);
    Vector<Float> deltas(numberOfAxes);

    // Copy values and output if required
    for (Int axis=0;axis<numberOfAxes;axis++) {
      names(axis) = axisNames[axis];
      referenceValues(axis)=referenceValuesD(axis);
      referencePixels(axis)=referencePixelsD(axis);
      deltas(axis)=deltasD(axis);
      if(names(axis)=="SIN Right Ascension") {
         names(axis)="RA---SIN";
         referenceValues(axis)=referenceValuesD(axis)/C::degree;
         deltas(axis)=deltasD(axis)/C::degree;
      }
      if(names(axis)=="SIN Declination") {
         names(axis)="DEC--SIN";
         referenceValues(axis)=referenceValuesD(axis)/C::degree;
         deltas(axis)=deltasD(axis)/C::degree;
      }
      if(names(axis)=="Frequency") {
         names(axis)="FREQ";
      }
      if(verbose) {
	cout<<"Output Axis "<<axis<<endl;
	cout<<"   name    = "<<names(axis)<<endl;
	cout<<"   ref val = "<<referenceValues(axis)<<endl;
	cout<<"   ref pix = "<<referencePixels(axis)<<endl;
	cout<<"   delta= "<<deltas(axis)<<endl;
      };
    }
    //OrderedMap <String, Double> keywords (0.0);

    String outputFilename = inp.GetString("out");
    if(outputFilename==" ") {
       outputFilename=inputFile.path().originalName()+".FITS";
    };
    String errorMessage;
    Bool success = WriteFITS(outputFilename, limage, errorMessage,
			     units.chars(), &names, &referencePixels,
			     &referenceValues, &deltas);
    if (!success) {
      cout << "  WriteFITS error message: " << errorMessage << endl;
      return 1;
    }
  
    cout << "Write complete of " << outputFilename << " complete" << endl;
    } // try
  catch (AipsError x) {
    cout << "Exception: " << x.getMesg() << endl;
    return 1;
  } end_try;

  return 0;  
}





