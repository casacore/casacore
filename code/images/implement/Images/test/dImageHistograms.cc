//# imhist.cc: This program generates histograms from images
//#
//# Copyright (C) 1996,1997
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
//
// IMHIST iterates through an image and creates and displays histograms from
//    data chunks specified by the axes keyword. 
//
//
//   in       The name on the input aips++ image.  Currently must be of type <Float>
//            and can be of any dimension.
//
//            There is no default.
//
//   axes     An array of integers (1 relative) specifying which axes are to be 
//            histogrammed and which ones the histograms will be displayed as 
//            a function of.  For example,  axes=3  would cause imhist to work
//            out histograms of the third (z) axis and display them as a function of
//              location along the first (x) and second (y) axes.   axes=1,3 would cause
//            histograms of 1-3 (x-z) planes to be made and then to be displayed as
//            a function of the second axis (y) location.  
// 
//            The default is to make a histogram of the entire image.
//    
// 
//   nbins    This specifies the number of bins, which is the same for each 
//            histogram.  Note that the bin width is worked out for each histogram 
//            separately from the data minimum and maximum for that data chunk
//            (unless you set the range keyword).
//          
//            The default is 25
//         
//   include  This specifies a range of pixel values to *include* in generating
//            the histograms.  If two values are given then include pixels in
//            the range include(1) to include(2).  If one value is given, then
//            include pixels in the range -abs(include) to abs(include)
//            This range applies to all histograms.
//             
//            The default is to include all data.
//
//   gauss    If True, a Gaussian overlay with the same mean, sigma
//            (of the pixels that were binned) and integral (of the histogram)
//            will be drawn on each histogram.
//
//            The default is True.
//
//   cumu     If True, a cumulative histogram is drawn.
//
//            The defaults is False.
//
//   log      If True, the log of the histogram values is drawn.
//
//            The default is False.
//
//   list     If True list some statistical information about each histogram
//
//            The default is False.
//
//   device   The PGPLOT device.
//
//            The default is /xs
//
//   nxy      The number of subplots to put on each page in x and y.  Each 
//            histogram takes one subplot.
//
//            The default is 1,1
//
// To do:
//   add keyword exclude ?  Bit hard as requires restucturing of min/max
//   storage image
//
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Utilities/String.h>

#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageHistograms.h>

#include <iostream.h>


enum defaults {AXES, RANGE, NDEFAULTS};


main (int argc, char **argv)
{
try {

   Input inputs(1);
   inputs.Version ("$Revision$");


// Get inputs

   inputs.Create("in", "", "Input image name");
   inputs.Create("axes", "-10", "Cursor axes");
   inputs.Create("nbins", "25", "Number of bins");
   inputs.Create("include", "0.0", "Pixel range to include");
   inputs.Create("gauss", "True", "Plot Gaussian equivalent ?");
   inputs.Create("cumu", "False", "Plot cumulative histogram ?");
   inputs.Create("log", "False", "Take log of y axis ?");
   inputs.Create("list", "False", "List statistics for each histogram");
   inputs.Create("device", "/xs", "Plot device");
   inputs.Create("nxy", "1,1", "Number of subplots in x & y");
   inputs.ReadArguments(argc, argv);

   const String in = inputs.GetString("in");
   const Block<Int> cursorAxesB(inputs.GetIntArray("axes"));
   const Int nBins = inputs.GetInt("nbins");
   const Bool doGauss = inputs.GetBool("gauss");
   const Bool doCumu = inputs.GetBool("cumu");
   const Bool doLog = inputs.GetBool("log");
   const Block<Double> includeB = inputs.GetDoubleArray("include");
   const Bool doList = inputs.GetBool("list");
   const Block<Int> nxyB(inputs.GetIntArray("nxy"));
   const String device = inputs.GetString("device");


// Create defaults array
 
   Vector<Bool> validInputs(NDEFAULTS);
   validInputs = False;

// Check inputs

   if (in.empty()) {
     cout << endl << "You must give an input image" << endl << endl;
     return 1;
   }
   if (device.empty()) {
     cout << endl << "You must give a plotting device" << endl << endl;
     return 1;
   }

// Convert cursor axes array to a vector (0 relative)
  
   Vector<Int> cursorAxes(cursorAxesB);
   if (cursorAxes.nelements() == 1 && cursorAxes(0) == -10) {
      cursorAxes.resize(0);   
   } else {
      for (Int i=0; i<cursorAxes.nelements(); i++) cursorAxes(i)--;
      validInputs(AXES) = True;
   }

// Convert inclusion range to vector
   
   Vector<Double> include(includeB);
   if (include.nelements() == 1 && include(0)==0) {
      include.resize(0);
   } else {
      validInputs(RANGE) = True;
   }


// Plotting

   Vector<Int> nxy(nxyB);


// Do the work 
    
   DataType imageType = imagePixelType(in);
   if (imageType==TpFloat) {
   
// Construct image
     
      PagedImage<Float> inImage(in);
  
// Construct histogram object
  
      LogOrigin or("imhist", "main()", WHERE);
      LogIO os(or);
      ImageHistograms<Float> histo(inImage, os);
 
   
// Set state
      
      if (validInputs(AXES)) {
         if (!histo.setAxes(cursorAxes)) return 1;
      }
      if (!histo.setNBins(nBins)) return 1;
      if (validInputs(RANGE)) {
         if (!histo.setIncludeRange(include)) return 1;
      }
      if (!histo.setGaussian (doGauss)) return 1;
      if (!histo.setForm(doLog, doCumu)) return 1;
      if (!histo.setStatsList(doList)) return 1;
      if (!histo.setPlotting(device, nxy)) return 1;

// Display histogram

      if (!histo.display()) return 1;

   } else {
      cout << "images of type " << imageType << " not yet supported" << endl;
      return 1;
   }
}
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return 1;
  }end_try;

  return 0;

}
