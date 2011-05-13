//# MomentsBase.h: base class for moment generator 
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef IMAGES_MOMENTSBASE_H
#define IMAGES_MOMENTSBASE_H


//# Includes
#include <casa/aips.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <casa/Quanta/Quantum.h>
#include <measures/Measures/MDoppler.h>
#include <casa/System/PGPlotter.h>
#include <casa/Logging/LogIO.h>
#include <casa/Arrays/Vector.h>
#include <casa/iosfwd.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class MomentCalcBase;
class IPosition;
class String;
class Unit;

// <summary>
// This class is a base class for generating moments from an image or a spectral data.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageMoments">ImageMoments</linkto>   
//   <li> <linkto class="MSMoments">MSMoments</linkto>   
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>   
//   <li> <linkto class="MomentCalcBase">MomentCalcBase</linkto>
// </prerequisite>

// <etymology>
//   This class is an abstract class to compute moments from images or spectral data.
// </etymology>

// <synopsis>
//  The primary goal of MSMoments, ImageMoments, and MSMoments are to help 
//  spectral-line astronomers analyze their multi-dimensional images or 
//  spectral data (in the form of MeasurementSet) by generating moments of 
//  a specified axis.  ImageMoments is a specialized class to generate moments 
//  from images, while MSMoments is designed properly for MeasurementSet input.
//  MSMoments class is an abstract class that is inherited by the above two 
//  concrete classes.
//  MomentsBase provides interface layer to the MomentCalculators so that 
//  functionalities in MomentCalculators can be shared with ImageMoments and 
//  MSMoments.
//  The word "moment" is used loosely here.  It refers to collapsing an axis
//  to one pixel and putting the value of that pixel (for all of the other 
//  non-collapsed axes) to something computed from the data values along
//  the moment axis.  For example, take an RA-DEC-Velocity cube, collapse
//  the velocity axis by computing the mean intensity at each RA-DEC
//  pixel.  This class and its inheritances offer many different moments and a variety of
//  interactive and automatic ways to compute them. 
//

// <motivation>
//  MSMoments is defined so that moments can be generated from both images 
//  and spectral data (in the form of MeasurementSet).  
// </motivation>
 

template <class T> class MomentsBase
{
public:

// Note that if I don't put MomentCalcBase as a forward declaration
// and use instead  "friend class MomentCalcBase<T>"  The gnu compiler
// fails with a typedef problem.  But I can't solve it with say
// <src>typedef MomentCalcBase<T> gpp_type;</src>  because you can only do a 
// typedef with an actual type, not a <tt>T</tt> !
   friend class MomentCalcBase<T>;

// Constructor takes an image and a <src>LogIO</src> object for logging purposes.
// You specify whether output images are  automatically overwritten if pre-existing,
// or whether an intercative choice dialog widget appears (overWriteOutput=F)
// You may also determine whether a progress meter is displayed or not.
   MomentsBase( LogIO &os,
                Bool overWriteOutput=False,
                Bool showProgress=True );

// Destructor
   virtual ~MomentsBase();

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
   virtual Bool setMomentAxis (const Int&) ;

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
// MomentsBase::WINDOW and MomentsBase::FIT, you would be invoking the 
// windowing method but determining the window by fitting Gaussians 
// automatically (as MomentsBase::INTERACTIVE) was not given.
//               
// If you don't call this function, then neither the windowing nor fitting
// methods are invoked.  A return value of <src>False</src> indicates
// that you asked for an illegal method.
   Bool setWinFitMethod(const Vector<Int>& method);


// This function invokes smoothing of the input image.  Give <src>Int</src> 
// arrays for the axes (0 relative) to be smoothed and the smoothing kernel 
// types (use the <src>enum KernelTypes</src>) for each axis.  Give a
// <src>Double</src> array for the widths (full width for BOXCAR and full 
// width at half maximum for GAUSSIAN) in pixels of the smoothing kernels for
// each axis.  For HANNING smoothing, you always get the quarter-half-quarter
// kernel (no matter what you might ask for).  A return value of <src>False</src>
// indicates that you have given an inconsistent or invalid set of smoothing 
// parameters.  If you don't call this function the default state of the
// class is to do no smoothing.  The kernel types are specified with
// the VectorKernel::KernelTypes enum
// <group>
   virtual Bool setSmoothMethod(const Vector<Int>&,
                                const Vector<Int>&,
                                const Vector<Quantum<Double> >&);
   Bool setSmoothMethod(const Vector<Int>& smoothAxes,
                        const Vector<Int>& kernelTypes,
                        const Vector<Double>& kernelWidths);
// </group>

// You may specify a pixel intensity range as either one for which
// all pixels in that range are included in the moment calculation,
// or one for which all pixels in that range are excluded from the moment
// calculations.  One or the other of <src>include</src> and <src>exclude</src>
// must therefore be a zero length vector if you call this function.
// A return value of <src>False</src> indicates that you have given both
// an <src>include</src> and an <src>exclude</src> range.  If you don't call
// this function, the default state of the class is to include all pixels.
   Bool setInExCludeRange(const Vector<T>& include,
                          const Vector<T>& exclude);

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
   Bool setSnr(const T& peakSNR, 
               const T& stdDeviation);

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
   Bool setPlotting(PGPlotter& device,
                    const Vector<Int>& nxy,
                    const Bool yInd=False);

// Set Velocity type.  This is used for moments for which the moment axis is
// a spectral axis for which the coordinate is traditionally presented in
// km/s   You can select the velocity definition. The default state of the
// class is to use the radio definition.
   void setVelocityType (MDoppler::Types type);

// CLose plotter
   void closePlotting();

// Reset argument error condition.  If you specify invalid arguments to
// one of the above functions, an internal flag will be set which will
// prevent the <src>createMoments</src> function, which is defined in its inheritances,
// from doing anything
// (should you have chosen to igmore the Boolean return values of the functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True; error_p = "";};

// Recover last error message
   String errorMessage() const {return error_p;};

// Get CoordinateSystem
   virtual CoordinateSystem coordinates() ;

// Helper function to convert a string containing a list of desired methods to
// the correct <src>Vector<Int></src> required for the <src>setWinFitMethod</src> function.
// This may be usful if your user interface involves strings rather than integers.
// A new value is added to the output vector (which is resized appropriately) if any of the 
// substrings "window", "fit" or "interactive" (actually "win", "box" and 
// "inter" will do) is present.
   static Vector<Int> toMethodTypes (const String& methods);


   virtual IPosition getShape() { return IPosition() ; } ;

protected:

   LogIO os_p;
   Bool showProgress_p;
   Int momentAxisDefault_p;
   T peakSNR_p;
   T stdDeviation_p;
   T yMin_p, yMax_p;
   String out_p;
   String smoothOut_p;
   Bool goodParameterStatus_p;
   Bool doWindow_p, doFit_p, doAuto_p, doSmooth_p, noInclude_p, noExclude_p;
   Bool fixedYLimits_p;

   Int momentAxis_p;
   Int worldMomentAxis_p;
   Vector<Int> kernelTypes_p;
   Vector<Quantum<Double> > kernelWidths_p;   
   Vector<Int> nxy_p;
   Vector<Int> moments_p;
   Vector<T> selectRange_p;
   Vector<Int> smoothAxes_p;
   PGPlotter plotter_p;
   Bool overWriteOutput_p;
   String error_p;
   Bool convertToVelocity_p;
   MDoppler::Types velocityType_p;

// Check that the combination of methods that the user has requested is valid
// List a handy dandy table if not.
   Bool checkMethod();

// Plot a histogram                     
   static void drawHistogram  (const Vector<T>& values,
                               const Vector<T>& counts,
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

// Convert a <tt>T</tt> to a <tt>Float</tt> for plotting
   static Float convertT (const T value) {return Float(real(value));};

// Convert a <tt>Float</tt> (from plotting) to a <tt>T</tt> 
   static T convertF (const Float value) {return T(value);};

// Fish out cursor values
   static Bool readCursor (PGPlotter& plotter, 
                           Float& x,
                           Float& y, 
                           String& ch);

// Take the user's data inclusion and exclusion data ranges and
// generate the range and Booleans to say what sort it is
   Bool setIncludeExclude (Vector<T>& range,
                           Bool& noInclude,
                           Bool& noExclude,
                           const Vector<T>& include,
                           const Vector<T>& exclude,
                           ostream& os);

// Set the output image suffixes and units
   Bool setOutThings(String& suffix,
                     Unit& momentUnits,
                     const Unit& imageUnits,
                     const String& momentAxisUnits,
                     const Int moment, Bool convertToVelocity);

// Make output Coordinates
   CoordinateSystem makeOutputCoordinates (IPosition& outShape,
                                           const CoordinateSystem& cSysIn,
                                           const IPosition& inShape,
                                           Int momentAxis, Bool removeAxis);
};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/MomentsBase.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
