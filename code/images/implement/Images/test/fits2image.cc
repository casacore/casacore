//# fits2image.cc: conversion to aips++ native tables
//# Copyright (C) 1996
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

#include <trial/ImgCrdSys/ImageCoordinate.h>
#include <trial/Images/PagedImage.h>

#include <aips/Arrays/Array.h>
#include <aips/Containers/MapIO.h>
#include <aips/Containers/OrderedMap.h>
#include <aips/Exceptions/Error.h>
#include <aips/FITS/FITS.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/IPosition.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>

#include <iostream.h>

int main(int argc, char **argv)
{
  try {
    // Inputs
    Input inp(1);
    inp.Version("2.0: PagedImage with coordinate conversion");
    inp.Create("in", " ", "Input FITS file name", "string");
    inp.Create("out", " ", "Output AIPS++ Image name", "string");
    inp.Create("verbose", "False", "Verbose?", "Bool");
    inp.ReadArguments(argc, argv);

    Bool verbose=inp.GetBool("verbose");

    File inputFilename = inp.GetString("in");
    if (!inputFilename.isReadable()) 
      throw (AipsError ("input FITS file unreadable"));

    String outputFilename = inp.GetString("out");
    if(outputFilename==" ") 
	outputFilename=inputFilename.path().originalName()+".image";

    Array <Float> fitsArray;
    Bool ok;
    String errorMessage;
    String unitName;
    Vector <String> axisNames (10);
    Vector <Float> referenceValues (10);
    Vector <Float> referencePixels (10);
    Vector <Float> deltas (10);
    OrderedMap <String, Double> keywords (0.0);

    if (verbose) 
      cout << "ReadFITS: " << inputFilename.path().originalName() << endl;

    fitsArray = ReadFITS (inputFilename.path().originalName(), 
                          ok, errorMessage, &unitName,
                          &axisNames,  &referencePixels, &referenceValues,
			  &deltas, &keywords);

    if (verbose){
      cout << "============= back from ReadFITS" << endl;
      cout << "number of axes:     " << axisNames.nelements() << endl;
      cout << "axisNames:          " << axisNames.ac() << endl;
      cout << "unit name:          " << unitName << endl;
      cout << "reference pixel:    " << referencePixels.ac() << endl;
      cout << "reference location: " << referenceValues.ac() << endl;
      cout << "delta:              " << deltas.ac() << endl;
      cout << "keywords:           " << (Map<String,Double>&) keywords << endl;
      cout << "number of keywords: " << keywords.ntot () << endl;
      cout << "array shape:        " << fitsArray.shape () << endl;
    }

    Int numberOfAxes = axisNames.nelements();
    ImageCoordinate coords;
    for (int i=0;i<numberOfAxes;i++){
      ReferenceValue ref(ReferenceValue::UNKNOWN, 0);
      LinearAxis tmp(MeasuredValue::UNKNOWN, referenceValues(i), ref,
		     deltas(i), referencePixels(i));
      tmp.setAxisName(axisNames(i));
      coords.addAxis(tmp);
    }
    
    // Copy values and output if required. For the moment we
    // don't give any coordinates
    PagedImage<Float> image2(fitsArray.shape(), coords, outputFilename);
    image2.putSlice(fitsArray, IPosition(fitsArray.ndim(), 0),
		    IPosition(fitsArray.ndim(), 1));
    
    if (verbose)
      cout << "table " << outputFilename << " write complete " << endl;

  } catch (AipsError x) {
    cout << "exception: " << x.getMesg() << endl;
    return 1;
  } end_try;
  
  return 0;  
}

