//# dImageStatistics.cc: image statistics program
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
//
// dImageStatistics iterates through an image accumulating and displaying statistics
//    from data chunks specified by the axes keyword.   The statistics
//    are displayed as a function of location of the display axes
//    (the axes not specified by keyword axes) with line plots.
//
//   in       The name on the input aips++ image.  Currently must be of 
//            type <Float> and can be of any dimension.
//
//            There is no default.
//
//   axes     An array of integers (1 relative) specifying which axes are to
//            have the statistics accumulated for, and which ones the statistics
//	      will be displayed as a function of.   For example,  axes=3  would 
//            cause imstat to work out statistics along the third (z) axis and 
//            display them as a function of the first (x) and second (y) axes.   
//            axes=1,3 would cause statistics of 1-3 (x-z) planes to be made 
//            and then to be displayed as a function of the second axis (y) 
//            location.  
// 
//            The default is to evaluate the statistics over the entire image.
//    
//   blc,trc  Region (1 relative)
//   inc      Increment to step through image
//   stats    This specifies which statistics you would like to see plotted.
//            Give a list choosing from 
//
//                 npts        The number of selected points 
//                 min         The minimum value
//                 max         The maximum value     
//                 sum         The sum of the values 
//                               sum_N(x_i)
//                 mean        The mean value 
//                               (1/N) * sum_N(x_i)  (<x>)
//                 sigma       The standard deviation about the mean 
//                               (1/[N-1]) * sum_N(x_i - <x>)**2
//                 rms         The root mean square value
//                               (1/N) * sum_N(x_i**2)
//
//            The default is mean,sigma
//
//   include  This specifies a range of pixel values to *include* for the
//            statistics. If two values are given then include pixels in
//            the range include(1) to include(2).  If one value is give, then
//            include pixels in the range -abs(include) to abs(include)
//             
//            The default is to include all data.
//             
//   exclude  This specifies a range of pixel values to *exclude* for the
//            statistics.  If two values are given then exlude pixels in 
//            the range exclude(1) to exclude(2).  If one value is give, then 
//            exclude pixels in the range -abs(exclude) to abs(exclude)
//            
//            The default is to exclude no data.
//
//   list     Only active if making a plot.  If True, write the statistics to the 
//            standard output.
//
//            Default is True.
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
//
//
#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Utilities/String.h>
  
#include <trial/Images/ImageStatistics.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Tasking/PGPlotter.h>

#include <iostream.h>

enum defaults {AXES, REGION, STATS, RANGE, PLOTTING, NDEFAULTS=5};


main (int argc, char **argv)
{
try {

   Input inputs(1);
   inputs.Version ("$Revision$");


// Get inputs

   inputs.Create("in", "", "Input file name");
   inputs.Create("axes", "-10", "Cursor axes");
   inputs.Create("blc", "-10", "blc");
   inputs.Create("trc", "-10", "trc");
   inputs.Create("inc", "-10", "inc");
   inputs.Create("stats", "mean,sigma", "Statistics to plot");
   inputs.Create("include", "0.0", "Pixel range to include");
   inputs.Create("exclude", "0.0", "Pixel range to exclude");
   inputs.Create("list", "True", "List statistics as well as plot ?");
   inputs.Create("plotter", "none", "PGPlot device");
   inputs.Create("nxy", "-1", "Number of subplots in x & y");
   inputs.ReadArguments(argc, argv);

   const String in = inputs.GetString("in");
   const Block<Int> cursorAxesB(inputs.GetIntArray("axes"));
   const Block<Int> blcB(inputs.GetIntArray("blc"));
   const Block<Int> trcB(inputs.GetIntArray("trc"));
   const Block<Int> incB(inputs.GetIntArray("inc"));
   const String statsToPlot = inputs.GetString("stats");
   const Block<Double> includeB = inputs.GetDoubleArray("include");
   const Block<Double> excludeB = inputs.GetDoubleArray("exclude");
   const Bool doList = inputs.GetBool("list");
   const Block<Int> nxyB(inputs.GetIntArray("nxy"));
   String device = inputs.GetString("plotter");


// Create defaults array

   Vector<Bool> validInputs(NDEFAULTS);
   validInputs = False;
   LogOrigin or("dImageStatistics", "main()", WHERE);
   LogIO os(or);
 

// Check image name and get image data type. 

   if (in.empty()) {
      os << LogIO::NORMAL << "You must specify the image file name" << LogIO::POST;
      return 1;
   }


// Convert cursor axes array to a vector (0 relative)
   
   Vector<Int> cursorAxes(cursorAxesB);
   if (cursorAxes.nelements() == 1 && cursorAxes(0) == -10) {
      cursorAxes.resize(0);
   } else {
      for (uInt i=0; i<cursorAxes.nelements(); i++) cursorAxes(i)--;
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


// Convert inclusion and exclusion ranges to vectors.

   Vector<Float> include(includeB.nelements());
   for (uInt i=0;i<include.nelements(); i++) {
     include(i) = includeB[i];
   }
   if (include.nelements() == 1 && include(0)==0) {
      include.resize(0);
   } else {
      validInputs(RANGE) = True;
   }
   Vector<Float> exclude(excludeB.nelements());
   for (i=0;i<exclude.nelements(); i++) {
     exclude(i) = excludeB[i];
   }
   if (exclude.nelements() == 1 && exclude(0)==0) {
      exclude.resize(0);
   } else {
      validInputs(RANGE) = True;
   } 


// Plotting things

   Vector<Int> statisticTypes = ImageStatsBase::toStatisticTypes(statsToPlot);
   Vector<Int> nxy(nxyB);
   if (nxy.nelements() == 1 && nxy(0) == -1) nxy.resize(0);
    if (device != "none" && 
       (statisticTypes.nelements()!=0 || !device.empty() || 
        nxy.nelements()!=0)) validInputs(PLOTTING) = True;


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



// Construct statistics object
   
      ImageStatistics<Float> stats(*pSubImage, os);


// Set state

      if (validInputs(AXES)) {
         if (!stats.setAxes(cursorAxes)) return 1;
      }
      if (validInputs(RANGE)) {
         if (!stats.setInExCludeRange(include, exclude, True)) return 1;
      }
      if (!stats.setList(doList)) return 1;
      if (validInputs(PLOTTING)) {
         PGPlotter plotter(device);
         if (!stats.setPlotting(plotter, statisticTypes, nxy)) return 1;
      }

// Recover things

     os.post();

     os << "Recovering display axes" << endl;
     Vector<Int> displayAxes = stats.displayAxes();

     Array<Float> data;
     os << LogIO::NORMAL << "Recovering npts array"  << endl;
     Bool ok = stats.getNPts(data);
     if (!ok) os << "Error recovering npts array" << endl;

     os << "Recovering sum array" << endl;
     ok = stats.getSum(data);
     if (!ok) os << "Error recovering sum array" << endl;

     os << "Recovering sum squared array" << endl;
     ok = stats.getSumSquared(data);
     if (!ok) os << "Error recovering sum squared array" << endl;

     os << "Recovering min array" << endl;
     ok = stats.getMin(data);
     if (!ok) os << "Error recovering min array" << endl;

     os << "Recovering max array" << endl;
     ok = stats.getMax(data);
     if (!ok) os << "Error recovering max array" << endl;

     os << "Recovering mean array" << endl;
     ok = stats.getMean(data);
     if (!ok) os << "Error recovering mean array" << endl;

     os << "Recovering sigma array " << endl;
     ok = stats.getSigma(data);
     if (!ok) os << "Error recovering sigma array" << endl;

     os << "Recovering variance array " << endl;
     ok = stats.getVariance(data);
     if (!ok) os << "Error recovering variance array" << endl;
     
     os << "Recovering rms array" << endl;
     ok = stats.getRms(data);
     if (!ok) os << "Error recovering rms array" << endl;


     os << "Recovering statistics slice from origin" << endl;
     IPosition pos(stats.displayAxes().nelements(),0);
     IPosition pos2(pSubImage->ndim(),0);
     Vector<Float> dataV;
     ok = stats.getStats(dataV, pos, False);
     if (!ok) os << "Error recovering statistics slice " << endl;
     ok = stats.getStats(dataV, pos2, True);
     if (!ok) os << "Error recovering statistics slice " << endl;
     os.post();

// Display statistics

     if (!stats.display()) return 1;


// Test copy constructor
     
     os << LogIO::NORMAL << "Applying copy constructor" << endl;
     ImageStatistics<Float> stats2(stats);

// Test assignment operator

     os << "Applying assignment" << LogIO::POST;
     stats = stats2;


   } else {
      os << LogIO::NORMAL << "images of type " << imageType << " not yet supported" << LogIO::POST;
      return 1;
   }

}

  catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
  } end_try;

  return 0;
}

