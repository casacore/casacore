//# dImageHistograms.cc: This program generates histograms from images
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
//   blc,trc  Region (1 relative)
//   inc      Increment to step through image
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
//   plotter  The PGPLOT device.
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
#include <aips/Arrays.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Utilities/String.h>

#include <trial/Images/PagedImage.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/ImageHistograms.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Tasking/PGPlotter.h>

#include <iostream.h>


enum defaults {AXES, REGION, RANGE, NDEFAULTS};


main (int argc, char **argv)
{
try {

   Input inputs(1);
   inputs.Version ("$Revision$");


// Get inputs

   inputs.Create("in", "", "Input image name");
   inputs.Create("axes", "-10", "Cursor axes");
   inputs.Create("blc", "-10", "blc");
   inputs.Create("trc", "-10", "trc");
   inputs.Create("inc", "-10", "inc");
   inputs.Create("nbins", "25", "Number of bins");
   inputs.Create("include", "0.0", "Pixel range to include");
   inputs.Create("gauss", "True", "Plot Gaussian equivalent ?");
   inputs.Create("cumu", "False", "Plot cumulative histogram ?");
   inputs.Create("log", "False", "Take log of y axis ?");
   inputs.Create("list", "False", "List statistics for each histogram");
   inputs.Create("plotter", "/xs", "Plot device");
   inputs.Create("nxy", "1,1", "Number of subplots in x & y");
   inputs.ReadArguments(argc, argv);

   const String in = inputs.GetString("in");
   const Block<Int> cursorAxesB(inputs.GetIntArray("axes"));
   const Block<Int> blcB(inputs.GetIntArray("blc"));
   const Block<Int> trcB(inputs.GetIntArray("trc"));
   const Block<Int> incB(inputs.GetIntArray("inc"));
   const Int nBins = inputs.GetInt("nbins");
   const Bool doGauss = inputs.GetBool("gauss");
   const Bool doCumu = inputs.GetBool("cumu");
   const Bool doLog = inputs.GetBool("log");
   const Block<Double> includeB = inputs.GetDoubleArray("include");
   const Bool doList = inputs.GetBool("list");
   const Block<Int> nxyB(inputs.GetIntArray("nxy"));
   const String device = inputs.GetString("plotter");


// Create defaults array
 
   Vector<Bool> validInputs(NDEFAULTS);
   validInputs = False;
   LogOrigin or("imhist", "main()", WHERE);
   LogIO os(or);

// Check inputs

   if (in.empty()) {
     os << LogIO::SEVERE << "You must give an input image" << LogIO::POST;
     return 1;
   }
   if (device.empty()) {
     os << LogIO::SEVERE << "You must give a plotting device" << LogIO::POST;
     return 1;
   }

// Convert cursor axes array to a vector (0 relative)
  
   Vector<Int> cursorAxes;
   if (cursorAxesB.nelements() == 1 && cursorAxesB[0] == -10) {
      cursorAxes.resize(0);   
   } else {
      cursorAxes.resize(cursorAxesB.nelements());
      for (uInt i=0; i<cursorAxes.nelements(); i++) cursorAxes(i) = cursorAxesB[i] - 1;
      validInputs(AXES) = True;
   }

// Convert region things to IPositions (0 relative)
   
   IPosition blc;
   IPosition trc;
   IPosition inc;
   if (blcB.nelements() == 1 && blcB[0] == -10) {
      blc.resize(0);
   } else {
      blc.resize(blcB.nelements());
      for (uInt i=0; i<blcB.nelements(); i++) blc(i) = blcB[i] - 1;
      validInputs(REGION) = True;
   }
   if (trcB.nelements() == 1 && trcB[0] == -10) {
      trc.resize(0);
   } else {
      trc.resize(trcB.nelements());
      for (uInt i=0; i<trcB.nelements(); i++) trc(i) = trcB[i] - 1;
      validInputs(REGION) = True;
   }
   if (incB.nelements() == 1 && incB[0] == -10) {
      inc.resize(0);
   } else {
      inc.resize(incB.nelements());
      for (uInt i=0; i<incB.nelements(); i++) inc(i) = incB[i];
      validInputs(REGION) = True;
   }

// Convert inclusion range to vector
   
   Vector<Float> include(includeB.nelements());
   for (uInt i=0;i<include.nelements(); i++) {
     include(i) = includeB[i]; 
   }
   if (include.nelements() == 1 && include(0)==0) {
      include.resize(0);
   } else {
      validInputs(RANGE) = True;
   }


// Plotting things
 
   Vector<Int> nxy;
   if (nxyB.nelements() == 1 && nxyB[0] == -1) {
      nxy.resize(0);
   } else {
      nxy.resize(nxyB.nelements());
      for (i=0;i<nxy.nelements(); i++) nxy(i) = nxyB[i];
   }


// Do the work 
    
   DataType imageType = imagePixelType(in);
   if (imageType==TpFloat) {
   
// Construct image
     
      PagedImage<Float> inImage(in);
      SubImage<Float>* pSubImage = 0;
  
      if (validInputs(REGION)) {   
         ImageUtilities::verifyRegion(blc, trc, inc, inImage.shape());  
         cout << "Selected region : " << blc+1<< " to "
              << trc+1 << endl;
         const LCBox region(blc, trc, inImage.shape());
   
         pSubImage = new SubImage<Float>(inImage, ImageRegion(region));
      } else {
         pSubImage = new SubImage<Float>(inImage);
      }

  
// Construct histogram object
  
      ImageHistograms<Float> histo(*pSubImage, os, True);

   
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
      PGPlotter plotter(device);
      if (!histo.setPlotting(plotter, nxy)) return 1;

// Display histograms

      Bool ok = histo.display();


// Get histograms   

      Array<Float> values, counts;
      os << LogIO::NORMAL << "Recovering histogram arrays" << LogIO::POST;
      ok = histo.getHistograms(values,counts);
      if (!ok) {
        os << LogIO::SEVERE << "Error recovering histograms" << LogIO::POST;
        return 1;
      }
//      cout << "values=" << values.ac() << endl;
//      cout << "counts=" << counts.ac() << endl;

 
      os << LogIO::NORMAL << "Recovering individual histogram arrays" << LogIO::POST;
      Vector<Float> valuesV, countsV;
      IPosition pos(histo.displayAxes().nelements(),0);
      ok = histo.getHistogram(valuesV,countsV,pos,False);
      if (!ok) {
        os <<  LogIO::SEVERE <<  "Error recovering individual histograms" << LogIO::POST;
        return 1;
      }
//      cout << "values=" << valuesV.ac() << endl;
//      cout << "counts=" << countsV.ac() << endl;



// Test copy constructor

      os << LogIO::NORMAL << "Applying copy constructor" << endl;
      ImageHistograms<Float> histo2(histo);

// Test assignment operator

      os << "Applying assignment operator" << LogIO::POST;
      histo = histo2;

   } else {
      os << "images of type " << imageType << " not yet supported" << endl;
      exit(1);
   }
}
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      exit(1);
  }end_try;

  exit(0)

;

}
