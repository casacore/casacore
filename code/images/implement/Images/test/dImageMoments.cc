//# imoment.cc: generate image moments
//# Copyright (C) 1996,1997,1998
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
// IMOMENT generate moments from an image.  The use may specify the moment
// axis and the moments to output.  The term "moment" is used loosely
// here and refers to any quantity computed from the pixel values along
// the designated "moment" axis.  A variety of moments and methods
// are offered.
//
//
//   in      The name on the input aips++ image.  Currently must be of 
//           type <Float> and can be of any dimension.
//
//           There is no default.
//
//   axis    Specify the moment axis number (1 relative).
//
//           The default is the first spectral axis in the image.
//
//   blc,trc  Region (1 relative)
//
//   moments This integer array specifies which moments will be output.
//
//           Give a list chosen from:
//
//              -1      The average intensity (same units as input)
//                        sum_N(I) / N      ( = <I>)
//               0      The integrated intensity 
//                        sum_N(I)
//               1      The intensity weighted mean velocity
//                        sum_N(I*v) / sum(I)
//               2      The intensity weighted velocity dispersion
//                        sum_N(I*(v-M1)**2) / sum_N(I)   where M1 = sum_N(I*v)/sum_N(I)
//               3      The median of I
//               4      The median coordinate 
//               5      The standard deviation about the mean of I
//                        sqrt(sum_N(I - <I>)**2 / (N-1))
//               6      The rms of I 
//                        sqrt(sum_N(I**2) / N)
//               7      The absolute mean deviation of I
//                        sum_N(|I - <I>|) / N
//               8      The maximum value of I
//               9      The coordinate of maximum value of I
//              10      The minimum value of I
//              11      The coordinate of minimum value of I
//
//            where N is the number of pixels, I is the pixel value, and v is 
//            the coordinate along the moment axis.   If the moment axis is 
//            spectral (some velocity, frequency or wavelength), the coordinate 
//            will be converted to velocity before computing the moment.
//
//            Default is 0
//
//   method   Specifies, together with keyword "smaxes" the method.  Choose from:
//
//                "window" for the spectral windowing method
//                "fit"    to fit Gaussians
//                "inter"  for interactive 
//            
//            Both the windowing and fitting method have ineractive modes.
//            The windowing method also has a fitting flavour, so if you set
//            "window,fit" you would be invoking the windowing method but
//            determining the window by fitting Gaussians automatically.
//
//            The default is not to invoke windowing or fitting.
//
//   smaxes   Specifies which axes (1 relative) you want to smooth to invoke
//            the smooth and clip method.  You must also specify keywords
//            "smwidth", "smtype", "include" or "exclude"
//
//            Default is to smooth no axes.
//
//   smtype   Specifies the type of smoothing kernel for each axis.   Choices are
//            from "gaussian", "boxcar", and "hanning". 
//
//            There is no default (if axes set).
//            
//   smwidth  Specifies the width (in pixels) of the smoothing kernel for each 
//            axis given by the "axes" keyword.   For Hanning kernels,       
//            FWHM is always set to 3, as triangular kernels of greater width have 
//            no mathematical basis.  For boxcar kernels, you must give an
//            odd integer full width.  For Gaussians, give any value as long
//            as its greater than about 1.5 pixels.  
//
//            There is no default (if axes set).
//
//   include  This specifies a range of pixel values to *include* from the moment
//            calculation.  If two values are given then include pixels in the
//            range include(1) to include(2).  If one value is give, then include
//            pixels in the range -abs(include) to abs(include)
//    
//            The default is to include all data.
//
//   exclude  This specifies a range of pixel values to *exclude* from the moment
//            calculation.  If two values are given then exlude pixels in the
//            range exclude(1) to exclude(2).  If one value is give, then exclude
//            pixels in the range -abs(exclude) to abs(exclude)
//    
//            The default is to exclude no data.
//
//   snr      This keyword is for the automatic window and fit methods.  It specifies two
//            things.  First, the signal-to-noise ratio below which the spectrum will be 
//            rejected as pure noise.   For each spectrum, the SNR of the absolute
//            absolute value is calculated and compared with this value.  Second,
//            if you think you know the standard deviation of the noise signal
//            better than the program can work it then you can enter it here.
//            If you don't enter it, the program will generate a histogram from
//            all the data and then fit a Gaussian to the bins above 25% of the peak.
//            If the plot device keyword is set, this procedure is displayed for you.
//
//            Default is 3 and automatic sigma determination.
//
//   device   The PGPLOT device to plot on when required
//
//            The default is /xs
//
//   nxy      The number of subplots in x and y to put on the plotting device
//
//            The default is 1,1
//
//   out      Each moment is written to a separate image.  This keyword specifies 
//            the name of the moment image if you ask for only one moment.  If
//            you specify multiple moments, then this is the root name of the 
//            output images, and the suffixes are constructed automatically.
//
//            The default is the input image name with the suffix appended (whether
//            you ask for one or more moments).
//
//   psfout   If you invoke the smooth and clip method, you can save the
//            point spread function image in this file.
//
//            The default is to not save the point spread function.
//
//   smout    If you invoke the smooth and clip method, you can save the
//            smoothed image in this file.  It might be useful to look
//            at this to get the clip limits right.
//
//            The default is to not save the smoothed image.
//
//
// To do:
//
//  . Eventually decent memory management will cause me to move the
//    smoothedImage from an ArrayLattice to something like a PagedImage.  
//
//  . Output masking is not handled yet.  The moments are just set to zero
//
//  . deal with absorption in M2
//
//  . input velocity field to work out moments relative to
//    instead off just using internally generated M1
//  
//  . coordinates, uses linear approximation for spectral axis 
//    coordinate value. Must replace when new image 
//    coordinates/measures come into place
//
//    The coordinate descriptors for the collapsed axis
//    are wrong as it is imposisble to change them with the
//    mentally damaged old coordinate class
//
//  . The routines aips/code/fortran/fftpak.f and 
//    aips/code/implement/Mathematics/extern_fft.cc  contain bugs that
//    cause the convolution to go wrong unless unoptimized versions
//    are used.  If they are built unoptimized and put in the optimized 
//    libaips_f.a and libaips.a it works, but still s-l-o-w-l-y
//
//
//
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>

#include <trial/Images/ImageMoments.h>
#include <trial/Images/PagedImage.h>
#include <trial/Tasking/PGPlotter.h>

#include <iostream.h>

enum defaults {IN, MOMENTS, AXIS, REGION, METHOD, SMOOTH, RANGE, SNR, OUT, PSFOUT, SMOUT, 
               PLOTTING, NDEFAULTS};

main (int argc, char **argv)
{

try {

   Input inputs(1);
   inputs.Version ("$Revision$");


// Get inputs

   inputs.Create("in", "", "Input image name");
   inputs.Create("moments", "0", "Moments to output");
   inputs.Create("axis", "-100", "Moment axis");
   inputs.Create("blc", "-10", "blc");
   inputs.Create("trc", "-10", "trc");
   inputs.Create("inc", "-10", "inc");
   inputs.Create("method","","Method (window,fit,inter)");
   inputs.Create("smaxes","-100","Smoothing axes");
   inputs.Create("smwidth", "-100.0", "Smoothing width along axes");
   inputs.Create("smtype","","Smoothing kernel types");
   inputs.Create("include", "0.0", "Pixel range to include");
   inputs.Create("exclude", "0.0", "Pixel range to exclude");
   inputs.Create("snr", "3.0,0.0", "SNR cutoff and sigma for automatic window method");
   inputs.Create("out", "", "Output root image name");
   inputs.Create("psfout", "", "Output PSF image name");
   inputs.Create("smout", "", "Output smoothed image name");
   inputs.Create("device", "none", "PGPLOT device");
   inputs.Create("nxy", "-1", "Number of subplots in x and y");
   inputs.Create("yind","False","Y scale independent ?");
   inputs.ReadArguments(argc, argv);

   const String in = inputs.GetString("in");
   const Block<Int> momentsB = inputs.GetIntArray("moments");
   Int momentAxis = inputs.GetInt("axis");
   const Block<Int> blcB(inputs.GetIntArray("blc"));
   const Block<Int> trcB(inputs.GetIntArray("trc"));
   const Block<Int> incB(inputs.GetIntArray("inc"));
   const String method = inputs.GetString("method");
   const Block<Int> smoothAxesB = inputs.GetIntArray("smaxes");
   const String kernels = inputs.GetString("smtype");
   const Block<Double> kernelWidthsB = inputs.GetDoubleArray("smwidth");
   const Block<Double> includeB = inputs.GetDoubleArray("include");
   const Block<Double> excludeB = inputs.GetDoubleArray("exclude");
   const Block<Double> snrB = inputs.GetDoubleArray("snr");
   const String out = inputs.GetString("out");
   const String psfOut = inputs.GetString("psfout");
   const String smOut = inputs.GetString("smout");
   String device = inputs.GetString("device");
   const Block<Int> nxyB = inputs.GetIntArray("nxy");
   const Bool yInd = inputs.GetBool("yind");

// Create defaults array

   Vector<Bool> validInputs(NDEFAULTS);
   validInputs = False;
   

// Check image name and get image data type. 
// Default is blank.

   if (in.empty()) {
      cout << "You must specify the image file name" << endl;
      return 1;
   }
   validInputs(IN) = True;
   

// Convert moments array to a vector. Add one because in the class
// interface the first allowed moment is 0.

   Vector<Int> moments(momentsB);
   for (Int i=0; i<moments.nelements(); i++) moments(i)++;
   validInputs(MOMENTS) = True;

   
// Make moment axis 0 relative if not defaulted

   if (momentAxis != -100) {
      momentAxis--;
      validInputs(AXIS) = True;
   } 

// Convert region things to IPositions (0 relative)
   
   IPosition blc;
   IPosition trc;
   IPosition inc;
   if (blcB.nelements() == 1 && blcB[0] == -10) {
      blc.resize(0);
   } else {
      blc.resize(blcB.nelements());
      for (Int i=0; i<blcB.nelements(); i++) blc(i) = blcB[i] - 1;
      validInputs(REGION) = True;
   }
   if (trcB.nelements() == 1 && trcB[0] == -10) {
      trc.resize(0);
   } else {
      trc.resize(trcB.nelements());
      for (Int i=0; i<trcB.nelements(); i++) trc(i) = trcB[i] - 1;
      validInputs(REGION) = True;
   }
   if (incB.nelements() == 1 && incB[0] == -10) {
      inc.resize(0);
   } else {
      inc.resize(incB.nelements());
      for (Int i=0; i<incB.nelements(); i++) inc(i) = incB[i];
      validInputs(REGION) = True;
   }


// Method.

   Vector<Int> winFitMethods;  
   winFitMethods = ImageMoments<Float>::toMethodTypes(method);
   if (winFitMethods.nelements() != 0) validInputs(METHOD) = True;


// Convert kernel types to an Int vector

   Vector<Int> kernelTypes;
   kernelTypes = ImageMoments<Float>::toKernelTypes(kernels);
   if (kernelTypes.nelements() != 0) validInputs(SMOOTH) = True;


// Convert smoothing axes to a vector.  

   Vector<Int> smoothAxes(smoothAxesB);
   if (smoothAxes.nelements() == 1 && smoothAxes(0) == -100) {
      smoothAxes.resize(0);
   } else {
      for (Int i=0; i<smoothAxes.nelements(); i++) smoothAxes(i)--;
      validInputs(SMOOTH) = True;
   }


// Convert smoothing widths to a vector

   Vector<Double> kernelWidths(kernelWidthsB);
   if (kernelWidths.nelements() == 1 && kernelWidths(0) == -100.0) {
      kernelWidths.resize(0);
   } else {
      validInputs(SMOOTH) = True;
   }



// Convert inclusion and exclusion ranges to vectors.

   Vector<Double> include(includeB);
   if (include.nelements() == 1 && include(0)==0) {
      include.resize(0);
   } else {
      validInputs(RANGE) = True;
   }
   Vector<Double> exclude(excludeB);
   if (exclude.nelements() == 1 && exclude(0)==0) {
      exclude.resize(0);
   } else {
      validInputs(RANGE) = True;
   }


// Deal with SNR parameters to a vector.   

   Double peakSNR = snrB[0];
   Double stdDeviation = snrB[1];
   validInputs(SNR) = True;


// Output file names

   if (!out.empty()) validInputs(OUT) = True;
   if (!psfOut.empty()) validInputs(PSFOUT) = True;
   if (!smOut.empty()) validInputs(SMOUT) = True;


// Plotting things

   Vector<Int> nxy(nxyB);
   if (device == "none") device = "";
   if (nxy.nelements() == 1 && nxy(0) == -1) nxy.resize(0);
   if (!device.empty() || nxy.nelements()!=0) validInputs(PLOTTING) = True;


// Construct moment object and do the work   

   DataType imageType = imagePixelType(in);
   if (imageType==TpFloat) {

// Construct image

      PagedImage<Float> inImage(in);

// Construct moment class

      LogOrigin or("imoment", "main()", WHERE);
      LogIO os(or);
      ImageMoments<Float> moment(inImage, os);

// Set inputs.  

      if (validInputs(MOMENTS)) {if (!moment.setMoments(moments)) return 1;}
      if (validInputs(REGION)) {if (!moment.setRegion(blc, trc, inc)) return 1;}
      if (validInputs(AXIS)) {if (!moment.setMomentAxis(momentAxis)) return 1;}
      if (validInputs(METHOD)) {
         if (!moment.setWinFitMethod(winFitMethods)) return 1;
      }
      if (validInputs(SMOOTH)) {
         if (!moment.setSmoothMethod(smoothAxes, kernelTypes, kernelWidths)) return 1;
      }
      if (validInputs(RANGE)) {if (!moment.setInExCludeRange(include, exclude)) return 1;}
      if (validInputs(SNR)) {if (!moment.setSnr(peakSNR, stdDeviation)) return 1;}
      if (validInputs(OUT)) moment.setOutName(out);
      if (validInputs(PSFOUT)) moment.setPsfOutName(psfOut);
      if (validInputs(SMOUT)) moment.setSmoothOutName(smOut);
      if (validInputs(PLOTTING)) {
         PGPlotter plotter(device);
         if (!moment.setPlotting (plotter, nxy, yInd)) return 1;
      }


// Do work

      if (!moment.createMoments()) return 1;

// Test copy constructor// Test assignment operator

      os << "Testing copy constructor" << endl;
      ImageMoments<Float> moment2(moment);


// Test assignment operator

      os << "Testing assignment operator" << endl;
      moment = moment2;

   } else {
      cout << "images of type " << imageType << " not yet supported" << endl;
      return 1;
   }

}

  catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
  } end_try;

return 0;
}
   

