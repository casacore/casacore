//# ImageHistograms.h: generate histograms from an image
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
#if !defined(AIPS_IMAGEHISTOGRAMS_H)
#define AIPS_IMAGEHISTOGRAMS_H

#if defined(_AIX)
#pragma implementation ("ImageHistograms.cc")
#endif

#include <aips/aips.h>
template <class T> class ImageInterface;
template <class T> class PagedArray;
template <class T> class Vector;
template <class T> class RO_LatticeIterator;
class IPosition;
class LogIO;
class CoordinateSystem;

// <summary> Displays histograms of regions from an image</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> ImageInterface
// </prerequisite>
//
// <etymology>
// This is a class designed to display histograms from images
// </etymology>
//
// <synopsis>
// This class enable you to display and/or retrieve histograms evaluated over 
// specified regions from an image.  The dimension of the region is arbitrary, but 
// the size of each dimension is always the size of the corresponding image axis.
// The histograms are displayed as a function of location of the axes not
// used to evaluate the histograms over.  The axes which you evaluate the histograms 
// over are called the cursor axes, the others are called the display axwes.
//
// For example, consider an image cube (call the axes xyz or [0,1,2]).  You could 
// display histograms from xy planes (cursor axes [0,1]) as a function of z (display
// axes [2]).   Or  you could retrieve histograms from the z axis (cursor axes [2])
// for each [x,y] location (display axes [0,1]).
//
// Currently complex images are not handled by this class.
//
// This class generates three "storage images" into which it writes the histograms,
// and some statistical values.  It is from the histogram se storage image that the 
// plotting and retrieval arrays are drawn.  The storage images are actually put in 
// a PagedArray.  This is a disk based storage medium.   The storage images are deleted 
// when the ImageHistograms class object destructs.    However, currently, if the process 
// is terminated ungracefully, the storage images will be left over.  They have a name 
// starting with the string "ImageHistograms_Hist", "ImageHistograms_MinMax_",
// and "ImageHistograms_Sums_"  and then a unique number. You can 
// safely delete them in this case.
//
// </synopsis>
//
// <example>
// <srcBlock>
//// Construct PagedImage from file name
//
//      PagedImage<Float> inImage(inName);
//   
//// Construct histogram object
//      
//      LogOrigin or("myClass", "myFunction(...)", WHERE);
//      LogIO os(or);
//      ImageHistograms<Float> histo(inImage, os);
//      
//// Set cursor axes to see statistics of yz planes (0 relative)
//
//      Vector<Int> cursorAxes(2)
//      cursorAxes(0) = 1;
//      cursorAxes(1) = 2;
//      if (!histo.setAxes(cursorAxes)) return 1;
//
//// Set to list and plot mean, sigma and rms
//
//      if (!histo.setList(True)) return 1;
//      String device = "/xs";
//      Vector<Int> nxy(2);
//      nxy(0) = 3;
//      nxy(1) = 3;
//      if (!histo.setPlotting(device, nxy)) return 1;
// 
//// Now activate actual listing and plotting
// 
//      if (!histo.display ()) return 1;
//
//// Retrieve histograms into array
//
//      Array<Float> values, counts;
//      if (!histo.getHistograms(values, counts)) return 1;
//
// </srcBlock>
// In this example, a <src>PagedImage</src> is constructed.  We set the cursor axes 
// to be the y and z axes so we make a histogram of each yz plane as a function 
// of x location on the PGPLOT device "/xs" with 9 subplots per page.
// After the plotting we also retrieve the histograms into an array.
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
//
// <motivation>
// The generation of histograms from an image is a basic and necessary capability.
// </motivation>
//
// <todo asof="1996/11/26">
//   <li> Deal with complex images 
//   <li> Make ascii listing of histograms as well as plots if desired
//   <li> retrieve histograms at specified location of display axes
// </todo>
//

template <class T> class ImageHistograms 
{
public:

// Constructor takes the image and a <src>LogIO</src> object for logging.
   ImageHistograms(const ImageInterface<T>& image, 
                   LogIO& os);

// Copy constructor.  Copy semantics are followed so any storage images 
// that have already been created for <src>other</src> are copied
// to <src>*this</src>
   ImageHistograms(const ImageHistograms<T> &other);

// Destructor
  ~ImageHistograms ();

// Assignment operator.  Deletes any storage images associated with
// the object being assigned to and copies any storage images that have
// already been created for "other".  
   ImageHistograms<T> &operator=(const ImageHistograms<T> &other);

// Set the cursor axes (0 relative).  A return value of <src>False</src>
// indicates you have asked for an invalid axis or that the internal
// status of the class is bad.  The default state of the class is to set 
// the cursor axes to all axes in the image.
   Bool setAxes (const Vector<Int>& cursorAxes);

// Set the region of interest of the image.    Currently, just a blc and trc
// are available (the increment is always set to unity at present),  In the
// future, a more sophisticated selection method will be available.  The default
// state of the class is to use the entire image.
   Bool setRegion (const IPosition &blc,
                   const IPosition &trc,
                   const IPosition &inc,
                   const Bool& listRegion=True);

// Set the number of bins for the histogram.  Note that the bin width is
// worked out for each histogram separately from the data minimum and maximum.
// The default state of the class is to set 25 bins.  A return value of <src>False</src>
// indicates you gave a non-positive bin width or  that the internal status of the 
// class is bad. 
   Bool setNBins (const Int& nBins);

// Specify a pixel intensity range for which all pixels in that range are 
// included.  A vector of length 1 for <src>include</src> means that the 
// range will be set to <src>-abs(include(0))<src> to <src>abs(include(0))</src>.
// A return value of <src>False</src> indicates that the internal
// status of the class is bad. If you don't call this function, the default 
// state of the class is to include all pixels.
   Bool setIncludeRange (const Vector<Double>& include);

// Specify that a Gaussian overlay should be plotted on the histogram. This
// Gaussian has the same mean and standard deviation as the data that were
// binned, and the same integral as the histogram.   A return value of <src>False</src>
// indicates that the internal status of the class is bad. The default state of
// the class is to not draw a Gaussian overlay.  
   Bool setGaussian (const Bool& doGauss);

// Specify the form of the histogram.   It can be plotted linearly or
// logarithmically, and cumulatively or non-cumulatively.   A return value 
// of <src>False</src> indicates that the internal status of the class is bad.
// The default state of the class is to draw the histograms linearly and
// non-cumulatively.
   Bool setForm (const Bool& doLog, const Bool& doCumu);

// This function allows you to control whether some statistics of the
// data that contributed to the histogram are written to  the output 
// stream.   A return value of <src>False</src> indicates that the internal
// status of the class is bad. The default state of the class is to not 
// list statistics.
   Bool setStatsList(const Bool& doList);

// This function sets the name of the PGPLOT plotting device and the number of
// subplots in x and y per page.   If you set <src>device</src> but offer
// a zero length array for <src>nxy</src> then <src>nxy</src> is set
// to [1,1].  A return value of <src>False</src> indicates invalid
// plotting arguments or that the internal status of the class is bad. If you
// don't call this function, the default state of the class is to not set
// a plotting device.
   Bool setPlotting(const String& device,
                    const Vector<Int>& nxy);

// Display the histograms by plotting them.  A return value of <src>False</src> 
// indicates an invalid plotting device, or that the internal status of the class is bad.
// If you don't call this function you won't see any histograms.
   Bool display ();

// This function retrieves the histograms into an array.  The shape of the first
// dimension of this array is the number of bins.  The rest of the shape of the
// array is the shape of the display axes (e.g. if the shape of the image is
// [nx,ny,nz] and you ask for histograms of the y axis the shape of the returned
// array would be [nbins,nx,nz].    The histograms are retrieved in the form 
// specified by the <src>setForm</src> function. The arrays are resized internally
// and are always of type <src>Float</src>.  A returned array of size 0
// indicates that there were no good values. A return value of <src>False</src> indicates 
// that the internal status of the class is bad. 
   Bool getHistograms (Array<Float>& values, Array<Float>& counts);

// Reset argument error condition.  If you specify invalid arguments to
// one of the above <src>set</src> functions, an internal flag will be set which will
// prevent the work functions from doing anything (should you have chosen 
// to ignore the Boolean return values of the <src>set</src> functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True;};

// Set a new image.  A return value of <src>False</src> indicates the 
// image had an invalid type or that the internal status of the class is bad.
   Bool setNewImage (const ImageInterface<T>& image);

private:

   PagedArray<Int>* pHistImage_p;
   PagedArray<T>* pMinMaxImage_p;
   PagedArray<Double>* pStatsImage_p;
   Bool binAll_p, goodParameterStatus_p, needStorageImage_p;
   Bool doCumu_p, doGauss_p, doList_p, doLog_p;
   const ImageInterface<T>* pInImage_p;
   Int nBins_p;
   IPosition  blc_p, trc_p, inc_p;
   LogIO &os_p;
   String device_p; 
   Vector<Int> cursorAxes_p;
   Vector<Int> displayAxes_p;
   Vector<Int> nxy_p;
   Vector<Float> range_p;


// Accumulate the histograms and statistical sums 
   void accumulate (const IPosition& imageCursorPos,
                    const Array<T>& cursor);

// Copy storage images from other to *this
   void copyStorageImages(const ImageHistograms<T>& other);

// Display histograms as a function of display axis
   Bool displayHistograms ();

// Display one histogram
   Bool displayOneHistogram (const Float &linearSum,
                             const Float &linearYMax,
                             const IPosition& histPos,
                             const Vector<T>& range,
                             const Vector<Double> &stats,
                             const Vector<Float>& values,
                             const Vector<Float>& counts);


// Fish out and convert to the appropriate form one histogram from the
// storage image
   void extractOneHistogram (Float &linearSum,
                             Float &linearYMax,
                             Vector<Float>& values,
                             Vector<Float>& counts,
                             const Vector<T>& range,
                             const Vector<Int>& intCounts);

// Find minimum and maximum for each cursor chunk
// and fill minmax storage image
   void fillMinMax (RO_LatticeIterator<T>* imageIterator, 
                    const Int& nVirCursorIter);

// Iterate through the image and generate the histogram accumulation image
   Bool generateStorageImage();

// Get the min and max from the min/max storage image for the current
// location of either the input image, or the histogram storage image
   void getMinMax (Vector<T> &range, 
                   const IPosition &pos,
                   const Bool &posInImage);

// Get the statsitics from the statistics storage image for the current
// location of either the input image, or the histogram storage image
   void getStats(Vector<Double> &stats,
                 const IPosition &pos,
                 const Bool &posInImage);

// Determine the histogram bin that this datum falls into and
// increment the histogram storage vector
   void histAccum (Vector<Int>& counts,
                   const T& datum,
                   const T& dMin,
                   const T& binWidth,
                   const Int& nBins);

// Find start location in histogram storage image for given location
// in the input image
   IPosition locInHist (const IPosition& imageCursorPos);

// Find start location in min/max storage image for given location
// in the input image
   IPosition locInMinMax (const IPosition& imageCursorPos);

// Find start location in Statistics storage image for given location
// in the input image
   IPosition locInStats (const IPosition& imageCursorPos);

// Given a location in the histogram storage image, convert those locations on the
// non-histogram axis (the first one) to account for the lattice subsectioning
   IPosition locHistInImage (const IPosition& histPosition);

// Make histogram cumulative
   void makeCumulative (Vector<Float>& counts,
                        Float& yMax,
                        const Int& nBins,
                        const Float& scale);

// Make array with Gaussian
   void makeGauss (Int& nGPts,
                   Float& gMax,
                   Vector<Float>& gX,
                   Vector<Float>& gY,
                   const Double& dMean,
                   const Double& dSigma,
                   const Float& dSum,
                   const Float& xMin,
                   const Float& xMax,
                   const Float& binWidth);

// Make the histogram logarithmic
   void makeLogarithmic (Vector<Float>& counts,
                         Float& yMax,
                         const Int& nBins);

// Plot one histogram
   void plotHist  (const Int& n,
                   const float* const px,
                   const float* const py);

// Update the histogram storage image with the histogram
// accumulated from the current chunk
   void putInHist (const IPosition& imageCursorPos,
                   const Vector<Int>& newCounts);


// Increment the statistics storage image.  
   void putInStats (const IPosition& imageCursorPos,
                    const Vector<Double>& stats);


// Set the bin width for the current histogram
   T setBinWidth (const Vector<T>& clip,
                  const Int& nBins);


// Return the shape of a min/max storage image slice.  
   IPosition minMaxSliceShape ();

// Return the shape of a min/max storage image slice.  
   IPosition statsSliceShape ();

// Accumulate statistical sums
   void statsAccum (Vector<Double>& stats,
                    const T& datum);

// Write values of display axes on plots
   Bool writeDispAxesValues (const IPosition& startPos,
                             const Float& xMin,
                             const Float& yMax);

};

#endif
