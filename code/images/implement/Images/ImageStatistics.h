//# ImageStatistics.h: generate statistics from an image
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
#if !defined(AIPS_IMAGESTATISTICS_H)
#define AIPS_IMAGESTATISTICS_H

#if defined(_AIX)
#pragma implementation ("ImageStatistics.cc")
#endif

#include <aips/aips.h>
#include <trial/Images/ImageStatsBase.h>
template <class T> class ImageInterface;
template <class T> class PagedArray;
template <class T> class Vector;
class IPosition;
class LogIO;
class CoordinateSystem;

// <summary> Displays various statistics from an image</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> ImageStatsBase (base class)
//   <li> ImageInterface
// </prerequisite>
//
// <etymology>
// This is a class designed to retrieve and display statistics from images
// </etymology>
//
// <synopsis>
// This class enable you to display and/or retrieve statistics evaluated over 
// specified regions from an image.  The dimension of the region is arbitrary, but 
// the size of each dimension is always the size of the corresponding image axis.
// The statistics are displayed as a function of location of the axes not
// used to evaluate the statistics over.  The axes which you evaluate the statistics
// over are called the cursor axes, the others are called the display axwes.
//
// For example, consider an image cube (call the axes xyz or [0,1,2]).  You could 
// display statistics from xy planes (cursor axes [0,1]) as a function of z (display
// axes [2]).   Or  you could retrieve statistics from the z axis (cursor axes [2])
// for each [x,y] location (display axes [0,1]).
//
// This class inherits from  <linkto class="ImageStatsBase">ImageStatsBase</linkto> 
// This base class provides an <src>enum</src> defining allowed statistics types and a 
// helper function to convert between a <src>String</src> and a <src>Vector<Int></src> describing 
// the desired statistics to plot.  An example is shown below.
//
// This class can list, plots and retrieve statistics.  When it lists statistics,
// it always lists all the available statistics.  When you plot statistics,
// you must specify which ones you would like to see.
//
// Note that this class cannot handle complex images yet.
//
// This class generates a "storage image" into which it writes the accumulated
// statistical sums.  It is from this storage image that the listing and
// plotting is drawn.  The dimension of the storage image is the number of
// display axes (i.e. the axes not given as the cursor axes) plus 1.
// The storage image is actually put in a PagedArray.  This is a disk based
// storage medium.   The storage image is deleted when the ImageStatistics class 
// object destructs.  
//
// </synopsis>
//
// <example>
// <srcBlock>
//// Construct PagedImage from file name
//
//      PagedImage<Float> inImage(inName);
//   
//// Construct statistics object
//      
//      LogOrigin or("myClass", "myFunction(...)", WHERE);
//      LogIO os(or);
//      ImageStatistics<Float> stats(inImage, os);
//      
//// Set cursor axes to see statistics of yz planes (0 relative)
//
//      Vector<Int> cursorAxes(2)
//      cursorAxes(0) = 1;
//      cursorAxes(1) = 2;
//      if (!stats.setAxes(cursorAxes)) return 1;
//
//// Set to list and plot mean, sigma and rms
//
//      if (!stats.setList(True)) return 1;
//      String device = "/xs";
//      Vector<Int> nxy(2);
//      nxy(0) = 1;
//      nxy(1) = 1;
//      Vector<Int> statsToPlot = ImageStatsBase::toStatisticTypes("mean,rms,sigma");
//      if (!stats.setPlotting(statsToPlot, device, nxy)) return 1;
// 
//// Now activate actual listing and plotting
// 
//      if (!stats.display ()) return 1;
//
//// Retrieve statistics into array
//
//      Array<Float> sum;
//      if (!stats.getSum()) return 1;
//
// </srcBlock>
// In this example, a <src>PagedImage</src> is constructed.  We set the cursor axes 
// to be the y and z axes, we specify to list the statistics if we plot them,
// and we ask to plot the mean, standard deviation, and root mean square of each 
// yz plane as a function of x location on the PGPLOT device "/xs" with
// 1 subplot per page (there will be only one in this case).  After the
// plotting and listing, we also retrieve the sum of the selected pixels
// as a function of x location into an array.
// </example>
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
// The generation of statistical information from an image is a basic 
// and necessary capability.
// </motivation>
//
// <todo asof="1996/11/26">
//   <li> Deal with complex images at least for statistics retrieval if not plotting.
//   <li> Retrieve statistics at specified location of display axes
//   <li> Standard errors on statistical quantities
//   <li> Median, other more exotic statistics.  Life made difficult by accumulation
//        image approach
//   <li> Memory model is poor
// </todo>
//

template <class T> class ImageStatistics : public ImageStatsBase
{
public:

// Constructor takes the image and a <src>LogIO</src> object for logging.
   ImageStatistics (const ImageInterface<T>& image, 
                    LogIO& os);

// Copy constructor
   ImageStatistics(const ImageStatistics<T> &other);

// Destructor
  ~ImageStatistics ();

// Assignment operator
   ImageStatistics<T> &operator=(const ImageStatistics<T> &other);

// Set the cursor axes (0 relative).  A return value of <src>False</src>
//  indicates you have asked for an invalid axis.  The default state of the class
// is to set the cursor axes to all axes in the image.
   Bool setAxes (const Vector<Int>& cursorAxes);

// You may specify a pixel intensity range as either one for which 
// all pixels in that range are included or one for which all pixels 
// in that range are excluded.   One or the other of <src>include</src> 
// and <src>exclude</src> must therefore be a zero length vector if you 
// call this function.  A return value of <src>False</src> indicates that 
// you have given both an <src>include</src> and an <src>exclude</src> 
// range.  A vector of length 1 for <src>include</src> and/or <src>exclude</src>
// means that the range will be set to (say for <src>include</src>)
// <src>-abs(include(0))<src> to <src>abs(include(0))</src>.  A return value
// of <src>False</src> indicates that both an inclusion and exclusion 
// range were given or that the internal state of the class is bad.   If you don't
// call this function, the default state of the class  is to include all pixels.
   Bool setInExCludeRange(const Vector<Double>& include,
                          const Vector<Double>& exclude);

// This function allows you to control whether the statistics are written to
// the output stream if you are also making a plot.  A return value of 
// <src>False</src> indicates that the internal state of the class is bad.
// The default state of the class is to not list the output when making a plot. 
   Bool setList(const Bool& doList);

// This functions enable you to specify which statistics you would like to
// plot, sets the name of the PGPLOT plotting device and the number of
// subplots in x and y per page.   If you set <src>device</src> 
// but offer a zero length array for <src>nxy</src> then <src>nxy</src> is 
// set to [1,1].  Similarly, the default for <src>statsToPlot</src> is
// to plot the mean and standard deviation. Use the helper function
// <src>ImageStatsBase::toStatisticTypes(String& stats)</src> to convert 
// a <src>String</src> to the desired <src>Vector<Int> statsToPlot</src>.  
// A return value of <src>False</src> indicates invalid plotting arguments
// or that the internal state of the class is bad.  If you don't call this function,
// the default state of the class is to not make plots.
   Bool setPlotting(const Vector<Int>& statsToPlot,
                    const String& device,
                    const Vector<Int>& nxy);

// Display the statistics by listing and/or plotting them.  If you don't call
// this function then you won't see anything !  A return value of <src>False</src>
// indicates an invalid plotting device, or that the internal state of the class is bad.
   Bool display ();

// These functions retrieve the designated statistics into an array.  The shape of the
// array is the shape of the display axes (e.g. if the shape of the image is
// [nx,ny,nz] and you ask for the mean of the y axis the shape of the returned
// array would be [nx,nz].    You should use the  <src>enum</src> provided by
// <src>ImageStatsBase</src> to select the argument <src>statToPlot</src>.
// A returned array of size 0 indicates that there were no good values. A return 
// value of <src>False</src> indicates that the internal state of the class is bad.
// <group>
   Bool getNPts (Array<T>&);
   Bool getSum (Array<T>&);
   Bool getSumSquared (Array<T>&);
   Bool getMin (Array<T>&);
   Bool getMax (Array<T>&);
   Bool getMean (Array<T>&);
   Bool getSigma (Array<T>&);
   Bool getRms (Array<T>&);
// </group>   


// Reset argument error condition.  If you specify invalid arguments to
// one of the above <src>set</src> functions, an internal flag will be set which will
// prevent the work functions from doing anything (should you have chosen 
// to ignore the Boolean return values of the <src>set</src> functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True;};

// Set a new ImageInterface object.  A return value of <src>False</src> indicates the 
// image had an invalid type or that the internal state of the class is bad.
   Bool setNewImage (const ImageInterface<T>& image);

private:


// Data

   LogIO &os_p;
   const ImageInterface<T>* pInImage_p;
   Vector<Int> cursorAxes_p, displayAxes_p;
   Vector<Int> nxy_p, statsToPlot_p;
   Vector<Float> range_p;
   String device_p; 
   Bool doList_p;
   Bool noInclude_p, noExclude_p;
   IPosition cursorShape_p, minPos_p, maxPos_p;

   Bool goodParameterStatus_p;
   Bool needStorageImage_p;
   Bool doneSomeGoodPoints_p, someGoodPointsValue_p;
   Int nVirCursorIter_p;   

   PagedArray<Double>* pStoreImage_p;


// Functions

// Accumulate statistics from this cursor chunk
   void accumulate       (Int& nIter,
                          const IPosition& cursorPos,
                          const Array<Float>& cursor);

// Update accumulation sums from this datum
   void accumulate2      (Double& sum,
                          Double& sumsq,
                          Double& sMin,
                          Double& sMax,
                          Int& nPts,
                          IPosition& tMinPos,
                          IPosition& tMaxPos,
                          const Int& i,
                          const IPosition& pos,
                          const Double& datum);

// Calculate statistic from accumulation image and return in an array
   void calculateStatistic (Array<T>& slice, const Int& ISTAT);

// Copy the cursor into the output array
   void copyArray        (Array<T>& slice, 
                          const Array<Double>& cursor);

// Create a new storage image
   void generateStorageImage
                          ();

// List the statistics
   void listStats         (const IPosition& dPos, 
                           const Int& n1,
                           const Matrix<Float>& ord);

// Plot the statistics
   Bool plotStats         (const IPosition& dPos,
                           const Int& n,
                           const Matrix<Float>& ord);

// Find the next good or bad point in an array
   Bool findNextDatum     (Int& iFound,
                           const Int& n,
                           const float* pn,
                           const Int& iStart,
                           const Bool& findGood);

// Find the next label in a list of comma delimitered labels
   Bool findNextLabel     (String& subLabel,
                           Int& iLab,
                           String& label);

// Examine an array and determine how many segments of good points it consists 
// of.    A good point occurs if the array value is greater than zero.
   void lineSegments      (Int& nSeg, 
                           Vector<Int>& start,
                           Vector<Int>& nPts,
                           const float* pn,
                           const Int& n);

// Draw each Y-axis sublabel in a string with a different colour
   void multiColourYLabel (const String& LRLoc,
                           String& label,
                           const Vector<Int>& colours,
                           const Int& nLabs);

// Plot an array which may have some blanked points.
// Thus we plot it in segments         
   void multiPlot        (const Int& n1,
                          const Vector<Float>& x,
                          const Vector<Float>& y,
                          const Vector<Float>& n);

// Find min and max of good data in arrays specified by pointers
   void minMax            (Bool& none,
                           Float& dMin,
                           Float& dMax,
                           const Vector<Float>& d,
                           const Vector<Float>& n,
                           const Int& n1);

// Find the next nice PGPLOT colour index 
   Int niceColour         (Bool& initColours);

// Find the world axis of the given pixel axis
   Int pixelAxisToWorldAxis(const CoordinateSystem& cSys,
                            const Int& pixelAxis);

// Convert pixel coordinate to world coordinate string
   void pix2World         (Vector<String>& sWorld,
                           const Int& worldAxis,
                           const Vector<Double>& pixel,
                           const Int& prec);

// Retrieve a statistic from the accumulation image and return in an array
   Bool retrieveStorageStatistic
                          (Array<Double>& slice, const Int& ISTAT);

// See if there were some valid points found in the accumulation
   Bool someGoodPoints ();

// Summarize the statistics found over the entire image
   void summStats         ();

};

#endif

