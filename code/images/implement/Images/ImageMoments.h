//# ImageMoments.h: generate moments from images
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
#if !defined(AIPS_IMAGEMOMENTS_H)
#define AIPS_IMAGEMOMENTS_H

#if defined(_AIX)
#pragma implementation ("ImageMoments.cc")
#endif

#include <aips/aips.h>


template <class T> class Array;
template <class T> class Matrix;
template <class T> class Vector;
template <class T> class MaskedLattice;
template <class T> class MomentCalcBase;
template <class T> class SubImage;
template <class T> class PagedImage;
template <class T> class ImageInterface;
class CoordinateSystem;
class IPosition;
class LogIO;
class PGPlotter;
class String;
class Unit;


//
// <summary> This class generates moments from an image. </summary>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="SubImage">SubImage</linkto>
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>   
//   <li> <linkto class="MomentCalculator">MomentCalculator</linkto>
// </prerequisite>
//
// <etymology>
//   This class computes moments from images
// </etymology>
//
// <synopsis>
//  The primary goal of this class is to help spectral-line astronomers analyze 
//  their multi-dimensional images by generating moments of a specified axis.
//  The word "moment" is used loosely here.  It refers to collapsing an axis
//  to one pixel and putting the value of that pixel (for all of the other 
//  non-collapsed axes) to something computed from the data values along
//  the moment axis.  For example, take an RA-DEC-Velocity cube, collapse
//  the velocity axis by computing the mean intensity at each RA-DEC
//  pixel.  This class offers many different moments and a variety of
//  interactive and automatic ways to compute them.
//
//  This class only accepts images of type <src>Float</src> and <src>Double</src>.
//  This restriction is because of the plotting capabilities which are a
//  bit awkward for other types.
//
//  This class makes a distinction between a "moment" and a "method". This
//  boundary is a little blurred, but it claims to refer to the distinction 
//  of what you are computing, as to how the pixels that were included in the 
//  computation were selected.  For example,  a "moment" would be the average value 
//  of some pixels.  A method for selecting those pixels would be a simple range 
//  specifying  a range for which pixels should be included.
//
//  The default state of this class is to do nothing.  If you specify an image via
//  the <src>setImage</src> function then invoking the <src>createMoments</src>
//  function will cause it to compute the integrated itensity along the 
//  spectral axis of the image (if it can find one).  You can change the 
//  computational state of the class from this basic form via the remaining
//  <src>set</src> functions.  You can call any number of these functions to 
//  suit your needs.
//
//  Because there are a wide variety of methods available, if you specify an
//  invalid combination, a table showing the available methods is listed. It
//  is reproduced below for convenience.  
//
//  The basic method is to just compute moments directly from the pixel intensities.  
//  This can be modified by applying pixel intensity inclusion or exclusion ranges.  
//  You can then also smooth the image and compute a mask based on the inclusion or 
//  exclusion ranges applied to the smoothed image.  This mask is then applied to 
//  the unsmoothed data for moment computation.
//
//  The window method does no pixel intensity range selection.  Instead a spectral
//  window is found (hopefully surrounding the spectral line feature) and only the 
//  pixels in that window are used for computation.  This window can be found from the 
//  smoothed or unsmoothed data.  The moments are always computed from the unsmoothed 
//  data.  For each spectrum, the window can be found interactively or automatically.
//  There are two interactive methods.  Either you just mark the window with the
//  cursor, or you interactively fit a Gaussian to the profile and the +/- 3-sigma
//  window is returned.  There are two automatic methods.  Either Bosma's converging
//  mean algorithm is used, or an automatically  fit Gaussian +/- 3-sigma window
//  is returned.
//
//  The fitting method fits Gaussians to spectral features either automatically
//  or interactively.  The moments are then computed from the Gaussian fits
//  (not the data themselves).
//
//  When an output image is created, it will have N-1 axes, where the input image
//  has N axes.  In the output image, the physical axis corresponding to the moment
//  axis will have been removed, but the coordinate information will be retained 
//  for future coordinate transformations. For example, if you have a RA-DEC-VELOCITY 
//  image and you collapsed axis 2 (the DEC axis) the output images would be 
//  RA-VELOCITY with the coordinate information retained for the DEC axis so that 
//  the coupled nature of RA/DEC coordinates is preserved.    
//
//  When making plots, the order in which the spectra are  displayed is determined
//  by the tiling sequence of the image (for optimum speed of access).  
//
//
// <srcblock>
//                   Allowed Methods
//                   ---------------
//
//   Smooth    Window      Fit   in/exclude   Interactive 
//   -----------------------------------------------------
//     N          N         N        N            N       
//     Y/N        N         N        Y            N       
// 
//     Y/N        Y         N        N            Y       
//     Y/N        Y         N        N            N       
//     Y/N        Y         Y        N            Y/N     
//
//     N          N         Y        N            Y/N     
//   ----------------------------------------------------
// </srcblock>
//
// </synopsis>
//
// <example>
// <srcBlock>
//// Set state function argument values
//
//      Vector<Int> moments(2);
//      moments(0) = ImageMoments<Float>::AVERAGE;
//      moments(1) = ImageMoments<Float>::WEIGHTED_MEAN_COORDINATE;
//      Vector<int> methods(2);
//      methods(0) = ImageMoments<Float>::WINDOW;
//      methods(1) = ImageMoments<Float>::INTERACTIVE;
//      Vector<Int> nxy(2);
//      nxy(0) = 3;
//      nxy(1) = 3;
//
//// Open paged image
//     
//      PagedImage<Float> inImage(inName);  
//
//// Construct moment helper object
//
//      LogOrigin or("myClass", "myFunction(...)", WHERE);
//      LogIO os(or);
//      ImageMoments<Float> moment(inName, os);
//
//// Specify state via control functions
//
//      if (!moment.setMoments(moments)) return 1;
//      if (!moment.setWinFitMethod(methods)) return 1;
//      if (!moment.setMomentAxis(3)) return 1;
//      if (!moment.setPlotting("/xs", nxy)) return 1;
//
//// Create the moments
//
//      if (!moment.createMoments()) return 1;
// </srcBlock>
// In this example, we generate two moments (average intensity and intensity
// weighted mean coordinate -- usually the velocity field) of axis 3 by the 
// interactive window method.  The interactive plotting is done on the PGPLOT 
// device called <src>/xs</src>.   We put 9 subplots on each page.  The output 
// file names are constructed by the class from the input file name plus some 
// internally generated suffixes.
// </example>
//
// <note role=caution>
// Note that the <src>MEDIAN_COORDINATE</src> moment is not very robust.
// It is very useful for generating quickly a velocity field in a way
// that is not sensitive to noise.    However, it will only give sensible
// results under certain conditions.   It treats the spectrum as a
// probability distribution, generates the cumulative distribution for
// the selected pixels (via an <src>include</src> or <src>exclude</src>
// pixel range, and finds the (interpolated) coordinate coresponding to 
// the 50% value.  The generation of the cumulative distribution and the
// finding of the 50% level really only makes sense if the cumulative
// distribution is monotonically increasing.  This essentially means only
// selecting pixels which are positive or negative.  For this reason, this
// moment type is *only* supported with the basic method (i.e. no smoothing,
// no windowing, no fitting) with a pixel selection range that is either
// all positive, or all negative.
// </note>
//
// <note role=caution>
// Note that if the <src>ImageInterface</src> object goes out of scope, this
// class will retrieve and generate rubbish as it just maintains a pointer
// to the image.
// </note>
//
// <note role=tip>
// If you ignore return error statuses from the functions that set the
// state of the class, the internal status of the class is set to bad.
// This means it will just  keep on returning error conditions until you
// explicitly recover the situation.
// </note>
//
// <motivation>
//  This is a fundamental and traditional piece of spectral-line image analysis.
// </motivation>
//
// <todo asof="1996/11/26">
//   <li> more control over histogram of image noise at start (pixel
//        range and number of bins)
//   <li> better algorithm for seeing if spectrum is pure noise
//   <li> Make this class extensible so users could add their own method.
// </todo>
 

template <class T> class ImageMoments
{
public:

// Note that if I don't put MomentCalcBase as a forward declaration
// and use instead  "friend class MomentCalcBase<T>"  The gnu compiler
// fails with a typedef problem.  But I can't solve it with say
// "typedef MomentCalcBase<T> gpp_type;"  because you can only do a 
// typedef with an actual type, not a <T> !
   friend MomentCalcBase<T>;

// Constructor takes an image and a <src>LogIO</src> object for logging purposes.
   ImageMoments (SubImage<T>& image, LogIO &os);

// Copy constructor.  Uses copy semantics.
   ImageMoments(const ImageMoments<T> &other);

// Copy constructor.  Uses copy semantics.
   ImageMoments(ImageMoments<T> &other);

// Destructor
  ~ImageMoments();

// Assignment operator. USes copy semantics.
   ImageMoments<T> &operator=(const ImageMoments<T> &other);

// This <src>enum MomentTypes</src> is provided for use with the
// <src>setMoments</src> function.  It gives the allowed moment
// types that you can ask for. 

enum MomentTypes {

// The average intensity
   AVERAGE,

// The integrated intensity
   INTEGRATED,

// The intensity weighted mean coordinate (usually velocity)
   WEIGHTED_MEAN_COORDINATE,

// The intensity weighted coordinate (usually velocity) dispersion
   WEIGHTED_DISPERSION_COORDINATE,

// The median intensity
   MEDIAN,

// The median coordinate (usually velocity). Treat the spectrum as
// a probability distribution, generate the cumulative distribution, 
// and find the coordinate corresponding to the 50% value.
   MEDIAN_COORDINATE,

// The standard deviation about the mean of the intensity
   STANDARD_DEVIATION,

// The rms of the intensity
   RMS,

// The absolute mean deviation of the intensity
   ABS_MEAN_DEVIATION,

// The maximum value of the intensity
   MAXIMUM,

// The coordinate (usually velocity) of the maximum value of the intensity
   MAXIMUM_COORDINATE,

// The minimum value of the intensity
   MINIMUM,

// The coordinate (usually velocity) of the minimum value of the intensity
   MINIMUM_COORDINATE,

// Total number
   NMOMENTS,

// Default value is the integrated intensity
   DEFAULT = INTEGRATED};

// Set the desired moments via an <src>Int</src> array.  Each <src>Int</src>
// specifies a different moment; the allowed values and their meanings
// are given by the <src>enum MomentTypes</src>.   A return value
// of <src>False</src> indicates you asked for an out of range 
// moment.  If you don't call this function, the default state of the 
// class is to request the integrated intensity.
   Bool setMoments (const Vector<Int>& moments);

// Set the moment axis (0 relative).  A return value of <src>False</src> 
// indicates that the axis was not contained in the image. If you don't
// call this function, the default state of the class is to set the 
// moment axis to the spectral axis if it can find one.  Otherwise 
// an error will result.
   Bool setMomentAxis (const Int& momentAxis);

// The <src>enum MethodTypes</src> is provided for use with the
// <src>setWinFitMethod</src> function.  It gives the allowed moment
// methods which are available with this function.  The use of these
// methods is described further with the description of this function
// as well as in the general documentation earlier.
enum MethodTypes {

// Invokes the spectral windowing method
   WINDOW,

// Invokes Gaussian fitting
   FIT,

// Invokes interactive methods
   INTERACTIVE,

   NMETHODS};


// The method by which you compute the moments is specified by calling
// (or not calling) the <src>setWinFitMethod</src> and
// <src>setSmoothMethod</src> functions.  The default state of the class 
// is to compute directly on all (or some according to <src>setInExClude</src>) 
// of the pixels in the spectrum.  Calling these functions modifies the 
// computational state to something more complicated. 
// 
// The <src>setWinMethod</src> function requires an <src>Int</src> array
// as its argument.  Each <src>Int</src> specifies a different method
// that you can invoke (either separately or in combination).  The
// allowed values and their meanings are given by the 
// <src>enum MethodTypes</src>.
//
// Both the windowing and fitting methods have interactive modes. The
// windowing method also has a fitting flavour, so if you set both 
// ImageMoments::WINDOW and ImageMoments::FIT, you would be invoking the 
// windowing method but determining the window by fitting Gaussians 
// automatically (as ImageMoments::INTERACTIVE) was not given.
//               
// If you don't call this function, then neither the windowing nor fitting
// methods are invoked.  A return value of <src>False</src> indicates
// that you asked for an illegal method.
   Bool setWinFitMethod(const Vector<Int>& method);



// The <src>enum KernelTypes</src> is provided for use with the
// <src>setSmoothMethod</src> function.  It gives the allowed smoothing
// kernel types which are available with this function.  
enum KernelTypes {

// Box-car smoothing kernel
   BOXCAR,

// Gaussian smoothing kernel
   GAUSSIAN,

// Hanning smoothing kernel
   HANNING,

   NKERNELS};

// This function invokes smoothing of the input image.  Give <src>Int</src> 
// arrays for the axes (0 relative) to be smoothed and the smoothing kernel 
// types (use the <src>enum KernelTypes</src>) for each axis.  Give a
// <src>Double</src> array for the widths (full width for BOXCAR and full 
// width at half maximum for GAUSSIAN) in pixels of the smoothing kernels for
// each axis.  For HANNING smoothing, you always get the quarter-half-quarter
// kernel (no matter what you might ask for).  A return value of <src>False</src>
// indicates that you have given an inconsistent or invalid set of smoothing 
// parameters.  If you don't call this function the default state of the
// class is to do no smoothing.
   Bool setSmoothMethod(const Vector<Int>& smoothAxes,
                        const Vector<Int>& kernelTypes,
                        const Vector<Double>& kernelWidths);


// You may specify a pixel intensity range as either one for which
// all pixels in that range are included in the moment calculation,
// or one for which all pixels in that range are excluded from the moment
// calculations.  One or the other of <src>include</src> and <src>exclude</src>
// must therefore be a zero length vector if you call this function.
// A return value of <src>False</src> indicates that you have given both
// an <src>include</src> and an <src>exclude</src> range.  If you don't call
// this function, the default state of the class is to include all pixels.
   Bool setInExCludeRange(const Vector<Double>& include,
                          const Vector<Double>& exclude);

// This function is used to help assess whether a spectrum along the moment 
// axis is all noise or not.  If it is all noise, there is not a lot of point
// to trying to computing the moment.  This is only needed for the automatic
// window or fit methods.  If you are using an interactive nethod, you assess
// yourself whether the spectrum contains signal or not.
// 
// <src>peakSNR</src> is the signal-to-noise ratio of the peak value in the
// spectrum below which the spectrum is considered pure noise.  
// <src>stdDeviation</src> is the standard deviation of the noise for the
// input image.  
//
// Default values for one or the other parameter are indicated by giving zero.
//
// The default state of the class then is to set <src>peakSNR=3</src>
// and/or to work out the noise level from a Gaussian fit to a histogram
// (above 25%) of the entire image (it is very hard to get an accurate 
// estimate of the noise a single spectrum).  If you have specified a 
// plotting device (see <src>setPlotting</src>) then you get to interact with 
// the fitting procedure if you want to.  A return value of <src>False</src> 
// indicates you have set invalid values.  
   Bool setSnr(const Double& peakSNR, 
               const Double& stdDeviation);

// Set the name of the output file.  If you create more than one moment, 
// this string is the root name for the output files.  Suffixes will
// be made up internally to append to this root.  If you only ask for one moment,
// this will be the actual name of the output file.  If you don't call this
// function, the default state of the class is to set the output name root to 
// the name of the input file.
   Bool setOutName(const String& outU);

// This is the output file name for the convolving function generated for the
// smoothing.  If you don't call this function, the default state of the class 
// is to not output the PSF.
   Bool setPsfOutName(const String& psfOut);

// This is the output file name for the smoothed image.   It can be useful
// to have access this to this image when trying to get the pixel
// <src>include</src> or <src>exclude</src> range correct for the smooth-clip
// method.  The default state of the class is to not output the smoothed image. 
   Bool setSmoothOutName(const String& smOut);

// This sets the name of the PGPLOT plotting device, the number of
// subplots in x and y per page and whether each spectrum plot is 
// autoscaled individually (<src>yInd=False</src>) or they are 
// plotted with the same range automatically determined from the image.
// Plotting is not invoked for all states of the class.  It is only
// needed for the interactive methods.  If you ask for a method that
// needs to determine the noise from the image, and you set the
// plotting device, then this will be done interactively.  Similarly,
// if you invoke the automatic window or fit methods, but set the
// plotting device, then you will see plots of the spectra and
// the selected windows and fits, respectively.
//
// The default state of the class is that no plotting characteristics are set.
// However, if you set <src>device</src> but offer a zero length array for
// <src>nxy</src> then the latter is set to [1,1].   A return value
// of <src>False</src> indicates that you gave roo many values in the
// <src>nxy</src> vector.
   Bool setPlotting(const PGPlotter& device,
                    const Vector<Int>& nxy,
                    const Bool yInd=False);

// Reset argument error condition.  If you specify invalid arguments to
// one of the above functions, an internal flag will be set which will
// prevent the <src>createMoments</src> function from doing anything
// (should you have chosen to igmore the Boolean return values of the functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True;};


// This is the function that does all the computational work.  It should be called
// after the </src>set</src> functions.  A return value of  <src>False</src>
// indicates that additional checking of the combined methods that you
// have requested has shown that you have not given consistent state to the class.
   Bool createMoments();


// Set a new image.  A return value of <src>False</src> indicates the 
// image had an invalid type (this class only accepts Float or Double images).
   Bool setNewImage (SubImage<T>& image);

// Helper function to convert a string containing a list of desired methods to
// the correct <src>Vector<Int></src> required for the <src>setWinFitMethod</src> function.
// This may be usful if your user interface involves strings rather than integers.
// A new value is added to the output vector (which is resized appropriately) if any of the 
// substrings "window", "fit" or "interactive" (actually "win", "box" and 
// "inter" will do) is present.
   static Vector<Int> toMethodTypes (const String& methods);

// Helper function to convert a string containing a list of desired smoothed kernel types
// to the correct <src>Vector<Int></src> required for the <src>setSmooth</src> function.
// This may be usful if your user interface involves strings rather than integers.
// A new value is added to the output vector (which is resized appropriately) if any of the 
// substrings "boxcar", "gaussian" or "hanning" (actually "box", "gauss", and "hann"
// will do) is present.
   static Vector<Int> toKernelTypes (const String& kernels);


private:

   LogIO& os_p;
   SubImage<T>* pInImage_p;

   Int momentAxis_p;
   Int momentAxisDefault_p;

   Vector<Int> kernelTypes_p;
   Vector<Double> kernelWidths_p;   
   Vector<Int> nxy_p;
   Vector<Int> moments_p;
   Vector<Float> range_p;
   Vector<Int> smoothAxes_p;

   Double peakSNR_p;
   Double stdDeviation_p;
   Float yMin_p, yMax_p;

   PGPlotter plotter_p;

   String out_p;
   String psfOut_p;
   String smoothOut_p;

   Bool goodParameterStatus_p;
   Bool doWindow_p, doFit_p, doAuto_p, doSmooth_p, noInclude_p, noExclude_p;
   Bool fixedYLimits_p;




// Check that the combination of methods that the user has requested is valid
// List a handy dandy table if not.
   Bool checkMethod    ();

// Plot a histogram                     
   static void drawHistogram  (const Vector<Float>& values,
                               const Vector<Float>& counts,
                               PGPlotter& plotter);

// Plot a line 
   static void drawLine       (const Vector<T>& x,
                               const Vector<T>& y,
                               PGPlotter& plotter);
                     
// Draw a vertical line of the given length at a given abcissa 
   static void drawVertical   (const T& x,
                               const T& yMin,
                               const T& yMax,
                               PGPlotter& plotter);

// Read the cursor and return its coordinates 
   static Bool getLoc (T& x,
                       T& y,
                       PGPlotter& plotter);

// Increase an integer to the next odd integer
   Bool makeOdd        (Int& i);
                     
// Generate the PSF
   void makePSF        (Array<T>& psf,
                        Matrix<T>& psfSep);

// Fish out cursor values
   static Bool readCursor (PGPlotter& plotter, Float& x,
                           Float& y, String& ch);

// Set the output image suffixes and units
   Bool setOutThings(String& suffix,
                     Unit& momentUnits,
                     const Unit& imageUnits,
                     const String& momentAxisUnits,
                     const Int moment);

// Smooth an image   
  Bool smoothImage (String& smoothName,
                    PagedImage<T>*& pSmoothedImage);

// Smooth one row
  void smoothProfiles (Lattice<T>* pIn,
                       const Int& row,
                       const Vector<T>& psf);

// Determine the noise by fitting a Gaussian to a histogram 
// of the entire image above the 25% levels.  If a plotting
// device is set, the user can interact with this process.
   Bool whatIsTheNoise (Double& noise,
                        SubImage<T>& image);

};




#endif

