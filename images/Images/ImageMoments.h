//# ImageMoments.h: generate moments from images
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

#ifndef IMAGES_IMAGEMOMENTS_H
#define IMAGES_IMAGEMOMENTS_H


//# Includes
#include <casa/aips.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <casa/Quanta/QMath.h>
#include <casa/Quanta/Quantum.h>
#include <measures/Measures/MDoppler.h>
#include <casa/System/PGPlotter.h>
#include <casa/Logging/LogIO.h>
#include <casa/Arrays/Vector.h>
#include <casa/iosfwd.h>
#include <images/Images/MomentsBase.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class Matrix;
template <class T> class MomentCalcBase;
template <class T> class SubImage;
template <class T> class ImageInterface;
template <class T> class MaskedLattice;
template <class T> class Lattice;
template <class T> class PtrHolder;
class IPosition;
class String;
class Unit;
//template <class T> class MomentsBase;

// <summary>
// This class generates moments from an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="MomentsBase">MomentsBase</linkto>
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>   
//   <li> <linkto class="MomentCalcBase">MomentCalcBase</linkto>
// </prerequisite>

// <etymology>
//   This class computes moments from images
// </etymology>

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
//  Output images are created with an all True (good) mask.  If, for a given
//  pixel, the moment calculation fails, then the mask is set to F.
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
// <note role=tip>
// If you ignore return error statuses from the functions that set the
// state of the class, the internal status of the class is set to bad.
// This means it will just  keep on returning error conditions until you
// explicitly recover the situation.  A message describing the last
// error condition can be recovered with function errorMessage.
// </note>
// </synopsis>

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
//      ImageMoments<Float> moment(SubImage<Float>(inName), os);
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

// <motivation>
//  This is a fundamental and traditional piece of spectral-line image analysis.
// </motivation>

// <todo asof="1996/11/26">
//   <li> more control over histogram of image noise at start (pixel
//        range and number of bins)
//   <li> better algorithm for seeing if spectrum is pure noise
//   <li> Make this class extensible so users could add their own method.
// </todo>
 

template <class T> class ImageMoments : public MomentsBase<T>
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
   ImageMoments (ImageInterface<T>& image, 
                 LogIO &os,
                 Bool overWriteOutput=False,
                 Bool showProgress=True);

// Copy constructor.  Uses copy semantics.
   ImageMoments(const ImageMoments<T> &other);

// Copy constructor.  Uses copy semantics.
   ImageMoments(ImageMoments<T> &other);

// Destructor
  ~ImageMoments();

// Assignment operator. USes copy semantics.
   ImageMoments<T> &operator=(const ImageMoments<T> &other);

// Set the moment axis (0 relative).  A return value of <src>False</src> 
// indicates that the axis was not contained in the image. If you don't
// call this function, the default state of the class is to set the 
// moment axis to the spectral axis if it can find one.  Otherwise 
// an error will result.
   Bool setMomentAxis (const Int& momentAxis);

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
   Bool setSmoothMethod(const Vector<Int>& smoothAxes,
                        const Vector<Int>& kernelTypes,
                        const Vector<Quantum<Double> >& kernelWidths);
   Bool setSmoothMethod(const Vector<Int>& smoothAxes,
			const Vector<Int>& kernelTypes,
			const Vector<Double>& kernelWidths);

// This is the function that does all the computational work.  It should be called
// after the <src>set</src> functions.  A return value of  <src>False</src>
// indicates that additional checking of the combined methods that you
// have requested has shown that you have not given consistent state to the class.
// If the axis being collapsed comes from a coordinate with one axis only,
// the axis and its coordinate are physically removed from the output image.  Otherwise,
// if <src>removeAxes=True</src> then the output axis is logically removed from the
// the output CoordinateSystem.  If <src>removeAxes=False</src> then the axis
// is retained with shape=1 and with its original coordinate information (which
// is probably meaningless).
//
// The output PtrBlock will hold PagedImages or TempImages (doTemp=True).
// It is your responsibility to delete the pointers.  
// If doTemp is True, the outFileName is irrelevant.
//
// If you create PagedImages, outFileName is the root name for 
// the output files.  Suffixes will be made up internally to append 
// to this root.  If you only ask for one moment,
// this will be the actual name of the output file.  If you don't set this
// variable, the default state of the class is to set the output name root to 
// the name of the input file.  
   Bool createMoments(PtrBlock<MaskedLattice<T>* >& images,
                      Bool doTemp, const String& outFileName, 
                      Bool removeAxes=True);

// Set a new image.  A return value of <src>False</src> indicates the 
// image had an invalid type (this class only accepts Float or Double images).
   Bool setNewImage (ImageInterface<T>& image);

// Get CoordinateSystem
   CoordinateSystem coordinates() {return pInImage_p->coordinates();};


// Get shape 
   IPosition getShape() { return pInImage_p->shape() ; } ;

private:

   ImageInterface<T>* pInImage_p;

// Smooth an image   
   Bool smoothImage (PtrHolder<ImageInterface<T> >& pSmoothedImage,
                     String& smoothName);

// Determine the noise by fitting a Gaussian to a histogram 
// of the entire image above the 25% levels.  If a plotting
// device is set, the user can interact with this process.
   Bool whatIsTheNoise (T& noise,
                        ImageInterface<T>& image);

protected:
  using MomentsBase<T>::os_p;
  using MomentsBase<T>::showProgress_p;
  using MomentsBase<T>::momentAxisDefault_p;
  using MomentsBase<T>::peakSNR_p;
  using MomentsBase<T>::stdDeviation_p;
  using MomentsBase<T>::yMin_p;
  using MomentsBase<T>::yMax_p;
  using MomentsBase<T>::out_p;
  using MomentsBase<T>::smoothOut_p;
  using MomentsBase<T>::goodParameterStatus_p;
  using MomentsBase<T>::doWindow_p; 
  using MomentsBase<T>::doFit_p;
  using MomentsBase<T>::doAuto_p;
  using MomentsBase<T>::doSmooth_p;
  using MomentsBase<T>::noInclude_p;
  using MomentsBase<T>::noExclude_p;
  using MomentsBase<T>::fixedYLimits_p;
  using MomentsBase<T>::momentAxis_p;
  using MomentsBase<T>::worldMomentAxis_p;
  using MomentsBase<T>::kernelTypes_p;
  using MomentsBase<T>::kernelWidths_p;   
  using MomentsBase<T>::nxy_p;
  using MomentsBase<T>::moments_p;
  using MomentsBase<T>::selectRange_p;
  using MomentsBase<T>::smoothAxes_p;
  using MomentsBase<T>::plotter_p;
  using MomentsBase<T>::overWriteOutput_p;
  using MomentsBase<T>::error_p;
  using MomentsBase<T>::convertToVelocity_p;
  using MomentsBase<T>::velocityType_p;
  using MomentsBase<T>::checkMethod;
};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageMoments.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
