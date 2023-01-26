//# dImageStatistics.cc: image statistics program
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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
// 
//
// dImageStatistics iterates through an image accumulating and displaying statistics
//    from data chunks specified by the axes keyword.   The statistics
//    are displayed as a function of location of the display axes
//    (the axes not specified by keyword axes) with line plots.
//
//   in       The name on the input Casacore image.  Currently must be of 
//            type <float> and can be of any dimension.
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
//   list     Only active if making a plot.  If true, write the statistics to the 
//            standard output.
//
//            Default is true.
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
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Logging.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
  
#include <casacore/images/Images/ImageStatistics.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/System/PGPlotter.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
enum defaults {AXES, REGION, STATS, RANGE, PLOTTING, NDEFAULTS=5};


int main (int argc, const char* argv[])
{
try {

   Input inputs(1);
   inputs.version ("$Revision$");


// Get inputs

   String name = "test_image.im";
   inputs.create("in", name, "Input file name");
   inputs.create("axes", "-10", "Cursor axes");
   inputs.create("blc", "-10", "blc");
   inputs.create("trc", "-10", "trc");
   inputs.create("inc", "-10", "inc");
   inputs.create("stats", "mean,sigma", "Statistics to plot");
   inputs.create("include", "0.0", "Pixel range to include");
   inputs.create("exclude", "0.0", "Pixel range to exclude");
   inputs.create("list", "True", "List statistics as well as plot ?");
   inputs.create("plotter", "none", "PGPlot device");
   inputs.create("nxy", "-1", "Number of subplots in x & y");
   inputs.create("disk", "F", "Force storage image to be disk based");
   inputs.readArguments(argc, argv);

   const String in = inputs.getString("in");
   const Block<int32_t> cursorAxesB(inputs.getIntArray("axes"));
   const Block<int32_t> blcB(inputs.getIntArray("blc"));
   const Block<int32_t> trcB(inputs.getIntArray("trc"));
   const Block<int32_t> incB(inputs.getIntArray("inc"));
   const String statsToPlot = inputs.getString("stats");
   const Block<double> includeB = inputs.getDoubleArray("include");
   const Block<double> excludeB = inputs.getDoubleArray("exclude");
   const bool doList = inputs.getBool("list");
   const Block<int32_t> nxyB(inputs.getIntArray("nxy"));
   String device = inputs.getString("plotter");
   const bool forceDisk = inputs.getBool("disk");


// Create defaults array

   Vector<bool> validInputs(NDEFAULTS);
   validInputs = false;
   LogOrigin lor("dImageStatistics", "main()", WHERE);
   LogIO os(lor);
 

// Check image name and get image data type. 

   if (in.empty()) {
      os << LogIO::NORMAL << "You must specify the image file name" << LogIO::POST;
      return 1;
   }


// Convert cursor axes array to a vector (0 relative)
   
   Vector<int32_t> cursorAxes(cursorAxesB.begin(), cursorAxesB.end());
   if (cursorAxes.nelements() == 1 && cursorAxes(0) == -10) {
      cursorAxes.resize(0);
   } else {
      for (uint32_t i=0; i<cursorAxes.nelements(); i++) cursorAxes(i)--;
      validInputs(AXES) = true;
   }
   

// Convert region things to IPositions (0 relative)
   
   IPosition blc;
   IPosition trc;
   IPosition inc;
   if (blcB.nelements() == 1 && blcB[0] == -10) {
      blc.resize(0);
   } else {
      blc.resize(blcB.nelements());
      for (uint32_t i=0; i<blcB.nelements(); i++) blc(i) = blcB[i] - 1;
      validInputs(REGION) = true;
   }
   if (trcB.nelements() == 1 && trcB[0] == -10) {
      trc.resize(0);
   } else {
      trc.resize(trcB.nelements());
      for (uint32_t i=0; i<trcB.nelements(); i++) trc(i) = trcB[i] - 1;
      validInputs(REGION) = true;
   }
   if (incB.nelements() == 1 && incB[0] == -10) {
      inc.resize(0);
   } else {
      inc.resize(incB.nelements());
      for (uint32_t i=0; i<incB.nelements(); i++) inc(i) = incB[i];
      validInputs(REGION) = true;
   }


// Convert inclusion and exclusion ranges to vectors.

   Vector<float> include(includeB.nelements());
   uint32_t i;
   for (i=0;i<include.nelements(); i++) {
     include(i) = includeB[i];
   }
   if (include.nelements() == 1 && include(0)==0) {
      include.resize(0);
   } else {
      validInputs(RANGE) = true;
   }
   Vector<float> exclude(excludeB.nelements());
   for (i=0;i<exclude.nelements(); i++) {
     exclude(i) = excludeB[i];
   }
   if (exclude.nelements() == 1 && exclude(0)==0) {
      exclude.resize(0);
   } else {
      validInputs(RANGE) = true;
   } 


// Plotting things

   std::regex re("[ \n\t\r\v\f,]+");
   Vector<int32_t> statisticTypes = LatticeStatsBase::toStatisticTypes(statsToPlot, re);
   Vector<int32_t> nxy(nxyB.begin(), nxyB.end());
   if (nxy.nelements() == 1 && nxy(0) == -1) nxy.resize(0);
    if (device != "none" && 
       (statisticTypes.nelements()!=0 || !device.empty() || 
        nxy.nelements()!=0)) validInputs(PLOTTING) = true;


// Do the work

   DataType imageType = imagePixelType(in);
//
   if (imageType==TpFloat) {
      
// Construct image
   
      PagedImage<float> inImage(in, true);
      SubImage<float>* pSubImage2 = 0;

      if (validInputs(REGION)) {
         LCBox::verify(blc, trc, inc, inImage.shape());
         cout << "Selected region : " << blc+1<< " to "
              << trc+1 << endl;
         const LCSlicer region(blc, trc);
//
         SubImage<float>* pSubImage = 0;
         if (inImage.isMasked()) {
            ImageRegion mask = 
              inImage.getRegion(inImage.getDefaultMask(),
                                RegionHandler::Masks);            
            pSubImage = new SubImage<float>(inImage, mask);
         } else {
            pSubImage = new SubImage<float>(inImage);
         }
         pSubImage2 = new SubImage<float>(*pSubImage, ImageRegion(region));
         delete pSubImage;
      } else {
         if (inImage.isMasked()) {
            ImageRegion mask = 
              inImage.getRegion(inImage.getDefaultMask(),
                                RegionHandler::Masks);            
            pSubImage2 = new SubImage<float>(inImage, mask);
         } else {
            pSubImage2 = new SubImage<float>(inImage);
         }
      }

// Construct statistics object
   
      ImageStatistics<float> stats(*pSubImage2, os, true, forceDisk);
  
// Clean up SUbImage pointers

      int32_t nDim = pSubImage2->ndim();
      if (pSubImage2!=0) delete pSubImage2;


// Set state
      if (validInputs(AXES)) {
         if (!stats.setAxes(cursorAxes)) {
            os << stats.errorMessage() << LogIO::POST;
            return 1;
         }
      }
      if (validInputs(RANGE)) {
         if (!stats.setInExCludeRange(include, exclude, true)) {
            os << stats.errorMessage() << LogIO::POST;
            return 1;
         }
      }
      if (!stats.setList(doList)) {
         os << stats.errorMessage() << LogIO::POST;
         return 1;
      }
      if (validInputs(PLOTTING)) {
         PGPlotter plotter(device);
            /*
         if (!stats.setPlotting(plotter, statisticTypes, nxy)) {
            os << stats.errorMessage() << LogIO::POST;
            return 1;
         }
         */
      }

// Recover things

     os.post();

     os << "Recovering display axes" << endl;
     Vector<int32_t> displayAxes = stats.displayAxes();
//
     os << endl << endl;
     os << "Recover array for each statistics type " << endl;
     const int32_t nStats = LatticeStatsBase::NSTATS;
     for (int32_t i=0; i<nStats; i++) {
       os << "Statistic " << LatticeStatsBase::toStatisticName(i) << LogIO::POST;
       Array<double> a;
       LatticeStatsBase::StatisticsTypes t = static_cast<LatticeStatsBase::StatisticsTypes>(i);
       stats.getStatistic (a, t, true);
     }
//
     os << "Recovering statistics slice from origin" << endl;
     IPosition pos(stats.displayAxes().nelements(),0);
     IPosition pos2(nDim,0);
     Vector<double> dataV;
     if (!stats.getStats(dataV, pos, false)) {
        os << stats.errorMessage() << LogIO::POST;
     }
     if (!stats.getStats(dataV, pos2, true)) {
        os << stats.errorMessage() << LogIO::POST;
     }

// Display statistics

     if (!stats.display()) {
        os << stats.errorMessage() << LogIO::POST;
        return 1;
     }

// Test copy constructor
     
     os << LogIO::NORMAL << "Applying copy constructor" << endl;
     ImageStatistics<float> stats2(stats);

// Test assignment operator

     os << "Applying assignment" << LogIO::POST;
     stats = stats2;

// Test setNewImage

     os << "Test setNewImage" << LogIO::POST; 
     stats.setNewImage(inImage);
     if (!stats.display()) {
        os << stats.errorMessage() << LogIO::POST;
     }
   } else if (imageType==TpComplex) {

// COnstruct image
   
      PagedImage<Complex> inImage(in, true);
      SubImage<Complex>* pSubImage2 = 0;

      if (validInputs(REGION)) {
         LCBox::verify(blc, trc, inc, inImage.shape());
         cout << "Selected region : " << blc+1<< " to "
              << trc+1 << endl;
         const LCSlicer region(blc, trc);
//
         SubImage<Complex>* pSubImage = 0;
         if (inImage.isMasked()) {
            ImageRegion mask = 
              inImage.getRegion(inImage.getDefaultMask(),
                                RegionHandler::Masks);            
            pSubImage = new SubImage<Complex>(inImage, mask);
         } else {
            pSubImage = new SubImage<Complex>(inImage);
         }
         pSubImage2 = new SubImage<Complex>(*pSubImage, ImageRegion(region));
         delete pSubImage;
      } else {
         if (inImage.isMasked()) {
            ImageRegion mask = 
              inImage.getRegion(inImage.getDefaultMask(),
                                RegionHandler::Masks);            
            pSubImage2 = new SubImage<Complex>(inImage, mask);
         } else {
            pSubImage2 = new SubImage<Complex>(inImage);
         }
      }

// Construct statistics object
   
      ImageStatistics<Complex> stats(*pSubImage2, os, true, forceDisk);
  
// Clean up SUbImage pointers

      if (pSubImage2!=0) delete pSubImage2;


// Set state

      if (validInputs(AXES)) {
         if (!stats.setAxes(cursorAxes)) {
            os << stats.errorMessage() << LogIO::POST;
            return 1;
         }
      }
      if (!stats.setList(doList)) {
         os << stats.errorMessage() << LogIO::POST;
         return 1;
      }
      /*
      if (validInputs(PLOTTING)) {
         PGPlotter plotter(device);
         if (!stats.setPlotting(plotter, statisticTypes, nxy)) {
            os << stats.errorMessage() << LogIO::POST;
            return 1;
         }
      }
      */

// Display statistics

     if (!stats.display()) {
        os << stats.errorMessage() << LogIO::POST;
        return 1;
     }
   } else {
      os << LogIO::NORMAL << "images of type " << int32_t(imageType)
	 << " not yet supported" << LogIO::POST;
      return 1;
   }
} catch (std::exception& x) {
     cerr << "exception: error " << x.what() << endl;
     return 1;
  }

  return 0;
}

