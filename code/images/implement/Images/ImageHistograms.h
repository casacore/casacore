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
#include <trial/Lattices/TiledCollapser.h>
#include <trial/Images/ImageStatistics.h>
#include <trial/Lattices/LatticeProgress.h>
#include <aips/Mathematics/NumericTraits.h>
#include <trial/Tasking/ProgressMeter.h>

template <class T> class MaskedImage;
template <class T> class PagedArray;
template <class T> class Vector;
template <class T> class RO_LatticeIterator;
class IPosition;
class LogIO;
class CoordinateSystem;
class PGPlotter;

// <summary> Displays histograms of regions from an image </summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> ImageInterface
//   <li> MaskedImage
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
// This class generates one "storage image" into which it writes the histograms.
// It is from the histogram storage image that the plotting and retrieval arrays 
// are drawn.  The storage images is actually put in a <src>PagedArray</src>.  
// This is a disk based storage medium.   The storage image is deleted 
// when the <src>ImageHistograms</src> class object destructs.    However, currently, if 
// the process is terminated ungracefully, the storage images will be left over.  
// They have a name starting with the string "ImageHistograms::",
// and then a unique number. You can safely delete them in this case.
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
// Note that if the <src>MaskedImage</src> object goes out of scope, this
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
// </todo>
//

template <class T>
class ImageHistograms 
{
public:

// Constructor takes the image and a <src>LogIO</src> object for logging.
// You can also specify whether you want to see progress meters or not.
   ImageHistograms(const MaskedImage<T>& image, 
                   LogIO& os,
                   Bool showProgress=True);

// Constructor takes the image only. In the absence of a logger you get no messages.
// This includes error messages and potential listing of statistics.
// You can also specify whether you want to see progress meters or not.
   ImageHistograms(const MaskedImage<T>& image, 
                   Bool showProgress=True);

// Copy constructor (copy semantics)
   ImageHistograms(const ImageHistograms<T> &other);

// Destructor
  ~ImageHistograms ();

// Assignment operator (copy semantics)
   ImageHistograms<T> &operator=(const ImageHistograms<T> &other);

// Set the cursor axes (0 relative).  A return value of <src>False</src>
// indicates you have asked for an invalid axis or that the internal
// status of the class is bad.  The default state of the class is to set 
// the cursor axes to all axes in the image.
   Bool setAxes (const Vector<Int>& cursorAxes);

// Set the number of bins for the histogram.  Note that the bin width is
// worked out for each histogram separately from the data minimum and maximum.
// The default state of the class is to set 25 bins.  A return value of <src>False</src>
// indicates you gave a non-positive bin width or  that the internal status of the 
// class is bad. 
   Bool setNBins (const uInt& nBins);

// Specify a pixel intensity range for which all pixels in that range are 
// included.  A vector of length 1 for <src>include</src> means that the 
// range will be set to <src>-abs(include(0))<src> to <src>abs(include(0))</src>.
// A return value of <src>False</src> indicates that the internal
// status of the class is bad. If you don't call this function, the default 
// state of the class is to include all pixels.
   Bool setIncludeRange (const Vector<T>& include);

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
// subplots in x and y per page.   If you set <src>plotter</src> but offer
// a zero length array for <src>nxy</src> then <src>nxy</src> is set
// to [1,1].  A return value of <src>False</src> indicates invalid
// plotting arguments or that the internal status of the class is bad. If you
// don't call this function, the default state of the class is to not set
// a plotting device.
   Bool setPlotting(PGPlotter& plotter,
                    const Vector<Int>& nxy);

// Display the histograms by plotting them.  A return value of <src>False</src> 
// indicates an invalid plotting device, or that the internal status of the class is bad.
// If you don't call this function you won't see any histograms.
   Bool display ();

// CLose the plotter
   void closePlotting();

// Return the display axes
   Vector<Int> displayAxes() const {return displayAxes_p;}

// This function retrieves the histograms into <src>Array</src>.  The shape of the first
// dimension of this array is the number of bins.  The rest of the shape of the
// array is the shape of the display axes (e.g. if the shape of the image is
// [nx,ny,nz] and you ask for histograms of the y axis the shape of the returned
// array would be [nbins,nx,nz].    The histograms are retrieved in the form 
// specified by the <src>setForm</src> function. The arrays are resized internally.
// A return value of <src>False</src> indicates  that the internal status of the class is bad. 
   Bool getHistograms (Array<T>& values, Array<T>& counts);

// This function retrieves the histogram at the specified location 
// into <src>Vectors</src>.  The histogram is retrieved in the form 
// specified by the <src>setForm</src> function. The vectors are resized 
// internally. If <src>posInImage=True</src> then the location is a 
// location in the input image.  Any positions on the display axes 
// are ignored.  Otherwise, you should just give locations for 
// the display axes only. A return  value of <src>False</src> indicates  that 
// the internal status  of the class is bad. 
   Bool getHistogram (Vector<T>& values, 
                      Vector<T>& counts, 
                      const IPosition& pos,
                      const Bool posInImage=False);

// Reset argument error condition.  If you specify invalid arguments to
// one of the above <src>set</src> functions, an internal flag will be set which will
// prevent the work functions from doing anything (should you have chosen 
// to ignore the Boolean return values of the <src>set</src> functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True;};

// Set a new image.  A return value of <src>False</src> indicates the 
// image had an invalid type or that the internal status of the class is bad.
   Bool setNewImage (const MaskedImage<T>& image);

private:

   LogIO os_p;
   PagedArray<T>* pStoreImage_p;
   ImageStatistics<T>* pStats_p;
   Bool binAll_p, goodParameterStatus_p, needStorageImage_p;
   Bool doCumu_p, doGauss_p, doList_p, doLog_p;
   Bool haveLogger_p, showProgress_p;
   uInt nBins_p;
   const MaskedImage<T>* pInImage_p;
   PGPlotter plotter_p;
   Vector<Int> cursorAxes_p, displayAxes_p, nxy_p;
   Vector<T> range_p;
   IPosition blcParent_p;

// Display histograms as a function of display axis
   Bool displayHistograms ();

// Display one histogram
   Bool displayOneHistogram (const T&linearSum,
                             const T&linearYMax,
                             const IPosition& histPos,
                             const Vector<T> &stats,
                             const Vector<T>& values,
                             const Vector<T>& counts,
                             PGPlotter& plotter);


// Fish out and convert to the appropriate form one histogram from the
// storage image
   void extractOneHistogram (T& linearSum,
                             T& linearYMax,
                             Vector<T>& values,
                             Vector<T>& counts,
                             const Vector<T>& stats,
                             const Vector<T>& intCounts);

// Iterate through the image and generate the histogram accumulation image
   Bool generateStorageImage();

// Get the statistics from the statistics object for the current
// location of either the input image, or the histogram storage image
   void getStatistics (Vector<T> &stats,
                       const IPosition &pos) const;

// Given a location in the histogram storage image, convert those locations on the
// non-histogram axis (the first one) to account for the lattice subsectioning
   IPosition locHistInImage (const IPosition& histPosition) const;

// Make histogram cumulative
   void makeCumulative (Vector<T>& counts,
                        T& yMax,
                        const uInt nBins,
                        const T scale) const;

// Make array with Gaussian
   void makeGauss (Int& nGPts,
                   T& gMax,
                   Vector<T>& gX,
                   Vector<T>& gY,
                   const T dMean,
                   const T dSigma,
                   const T dSum,
                   const T xMin,
                   const T xMax,
                   const T binWidth) const;

// Make the histogram logarithmic
   void makeLogarithmic (Vector<T>& counts,
                         T& yMax,
                         const uInt nBins) const;

// Fill histograms storage image
   void makeHistograms();

// Create and fill statistics object
   Bool makeStatistics();

// Plot one histogram
   void plotHist  (const Vector<T>& x,
                   const Vector<T>& y,
                   PGPlotter& plotter) const;

// Check/set include pixel range
   Bool setInclude (Vector<T>& range,
                    Bool& noInclude,  
                    const Vector<T>& include,
                    ostream& os);

// Write values of display axes on plots
   Bool writeDispAxesValues (const IPosition& startPos,
                             const Float xMin,
                             const Float yMax,
                             PGPlotter& plotter) const;
};



// <summary> Generate histograms, tile by tile, from a masked lattice </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=LatticeApply>LatticeApply</linkto>
//   <li> <linkto class=TiledCollapser>TiledCollapser</linkto>
// </prerequisite>
//
// <etymology>
// This class is used by <src>ImageHistograms</src> to generate
// histograms from an input <src>MaskedLattice</src>.
// The input lattice is iterated through in tile-sized chunks
// and fed to an object of this class.
// </etymology>
//
// <synopsis>
// <src>HistTiledCollapser</src> is derived from <src>TiledCollapser</src> which
// is a base class used to define methods.  Objects of this base class are
// used by <src>LatticeApply</src> functions.  In this particular case,
// we are interested in <src>LatticeApply::tiledApply</src>.  This  function iterates
// through a <src>MaskedLattice</src> and allows you to collapse one or more
// axes, computing some values from it, and placing those values into
// an output <src>MaskedLattice</src>.  It iterates through the input
// lattice in optimal tile-sized chunks.    <src>ImageHistograms</src> 
// uses a <src>HistTiledCollapser</src> object which it gives to 
// <src>LatticeApply::tiledApply</src> for digestion.  After it has
// done its work, <src>ImageHistograms</src> then accesses the output
// <src>Lattice</src> that it made.
// </synopsis>
//
// <example>
// <srcblock>
//// Created collapser. Control information is passed in via the constructor.
//
//   HistTiledCollapser<T> collapser(pStats, nBins_p);
// 
//// This is the first output axis getting  collapsed values. In ImageHistograms
//// this is the first axis of the output lattice
// 
//   Int newOutAxis = 0;
//
//// tiledApply does the work by passing the collapser data in chunks
//// and by writing the results into the output lattice 
//
//   LatticeApply<T>::tiledApply(outLattice, inLattice,
//                               collapser, collapseAxes,
//                               newOutAxis);
//
// </srcblock>
// In this example, a collapser is made and passed to LatticeApply.
// Afterwards, the output Lattice is available for use.
// The Lattices must all be the correct shapes on input to tiledApply
// </example>
//
// <motivation>
// The LatticeApply classes enable the ugly details of optimal
// Lattice iteration to be hidden from the user.
// </motivation>
//
// <todo asof="1998/05/10">   
//   <li> 
// </todo>

template <class T>
class HistTiledCollapser : public TiledCollapser<T>
{
 
public:
// Constructor
    HistTiledCollapser(ImageStatistics<T>* pStats,
                       const uInt nBins);

// Initialize process, making some checks
    virtual void init (uInt nOutPixelsPerCollapse);

// Initialize the accumulator
    virtual void initAccumulator (uInt n1, uInt n3);

// Process the data in the current chunk.
    virtual void process (uInt accumIndex1,
                          uInt accumIndex3,
                          const T* inData,
                          const Bool* inMask,
                          uInt inIncr,
                          uInt nrval,
                          const IPosition& startPos,
                          const IPosition& shape);
 
// End the accumulation process and return the result arrays 
    virtual void endAccumulator(Array<T>& result,  
                                Array<Bool>& resultMask,
                                const IPosition& shape); 


// Set bin width.  STatic so ImageHistograms can use it too
    static T setBinWidth (const Vector<T>& clip,
                          uInt nBins);

private:
    ImageStatistics<T>* pStats_p;
    Block<uInt>* pHist_p;
    uInt nBins_p;
    uInt n1_p;
    uInt n3_p;
};
 
  
// <summary> Provides a progress meter for the <src>ImageHistograms</src> class </summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto module=Lattices>LatticeProgress</linkto>
// </prerequisite>
//
// <etymology>
// Display a progress meter for the class  <src>ImageHistograms</src>
// </etymology>
//
// <synopsis>
//   Progress meters can be displayed by the <src>LatticeApply</src> class
//   which is used by <src>ImageHistograms</src> in order to optimally iterate
//   through the image.  To do this,  one must derive a
//   class from <src>LatticeProgress</src>. <src>LatticeApply</src> calls 
//   methods declared in <src>LatticeProgress</src> and  implemented in 
//   the derived class.
// </synopsis>
//
// <motivation>
//  I like progress meters !
// </motivation>
//
// <todo asof="1998/01/10">
// </todo>
  
class ImageHistProgress : public LatticeProgress
{   
public:
 
// Constructor makes a null object
    ImageHistProgress() : itsMeter(0) {};
 
// Destructor deletes the ProgressMeter pointer
    virtual ~ImageHistProgress();
 
// Initialize this object.  Here we create the ProgressMeter
// This function is called by the <src>init</src> in LatticeProgress
    virtual void initDerived();
  
// Tell the number of steps done so far.
    virtual void nstepsDone (uInt nsteps);
 
// The process has ended so clean things up.
    virtual void done();
 
private:
    ProgressMeter* itsMeter;
};



#endif


