//# LatticeHistograms.h: generate histograms from a lattice
//# Copyright (C) 1996,1997,1999,2000,2001
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

#ifndef LATTICES_LATTICEHISTOGRAMS_H
#define LATTICES_LATTICEHISTOGRAMS_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LatticeMath/TiledCollapser.h>
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/LatticeMath/LatticeProgress.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/System/PGPlotter.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class MaskedLattice;
template <class T> class TempLattice;
template <class T> class Vector;
class IPosition;
class PGPlotter;

// <summary>
// Displays histograms of regions from a lattice.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MaskedLattice>MaskedLattice</linkto>
// </prerequisite>

// <etymology>
// This is a class designed to display histograms from MaskedLattices
// </etymology>

// <synopsis>
// This class enable you to display and/or retrieve histograms evaluated over 
// specified regions from a MaskedLattice.  The dimension of the region is arbitrary, but 
// the size of each dimension is always the size of the corresponding lattice axis.
// The histograms are displayed as a function of location of the axes not
// used to evaluate the histograms over.  The axes which you evaluate the histograms 
// over are called the cursor axes, the others are called the display axes.
//
// For example, consider a lattice cube (call the axes xyz or [0,1,2]).  You could 
// display histograms from xy planes (cursor axes [0,1]) as a function of z (display
// axes [2]).   Or  you could retrieve histograms from the z axis (cursor axes [2])
// for each [x,y] location (display axes [0,1]).
//
// This class generates a "storage lattice" into which it writes the histograms.
// It is from this storage lattice that the plotting and retrieval
// arrays are drawn.  The storage lattice is either in core or on disk
// depending upon its size (if > 10% of memory given by .aipsrc system.resources.memory
// then it goes into a disk-based PagedArray).  If on disk,  the
// storage lattice is deleted when the <src>LatticeHistograms</src> 
// object destructs.    
//
//
// <note role=tip>
// Note that for complex lattices, real and imaginary are treated independently.
// They are binned and plotted separately.
// </note>
//
// <note role=tip>
// If you ignore return error statuses from the functions that set the
// state of the class, the internal status of the class is set to bad.
// This means it will just  keep on returning error conditions until you
// explicitly recover the situation.   A message describing the last   
// error condition can be recovered with function errorMessage.

// </note>
// </synopsis>

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

// <motivation>
// The generation of histograms from an image is a basic and necessary capability.
// </motivation>
//
// <todo asof="2000/04/04">
//   <li> Make ascii listing of histograms as well as plots if desired
// </todo>
//


template <class T> class LatticeHistograms 
{
public:

// Constructor takes the MaskedLattice and a <src>LogIO</src> object for logging.
// You can also specify whether you want to see progress meters or not.
// You can force the storage lattice to be disk based, otherwise
// the decision for core or disk is taken for you.
   LatticeHistograms(const MaskedLattice<T>& lattice, 
                     LogIO& os,
                     Bool showProgress=True,
                     Bool forceDisk=False);

// Constructor takes the MaskedLattice only. In the absence of a logger you get no messages.
// This includes error messages and potential listing of statistics.
// You can specify whether you want to see progress meters or not.
// You can force the storage lattice to be disk based, otherwise
// the decision for core or disk is taken for you.
   LatticeHistograms(const MaskedLattice<T>& lattice, 
                   Bool showProgress=True,
                   Bool forceDisk=False);

// Copy constructor (copy semantics)
   LatticeHistograms(const LatticeHistograms<T> &other);

// Destructor
   virtual ~LatticeHistograms ();

// Assignment operator (copy semantics)
   LatticeHistograms<T> &operator=(const LatticeHistograms<T> &other);

// Set the cursor axes (0 relative).  A return value of <src>False</src>
// indicates you have asked for an invalid axis or that the internal
// status of the class is bad.  The default state of the class is to set 
// the cursor axes to all axes in the lattice.
   Bool setAxes (const Vector<Int>& cursorAxes);

// Set the number of bins for the histogram.  Note that the bin width is
// worked out for each histogram separately from the data minimum and maximum.
// The default state of the class is to set 25 bins.  A return value of <src>False</src>
// indicates you gave a non-positive bin width or  that the internal status of the 
// class is bad. 
   Bool setNBins (const uInt& nBins);

// Specify a pixel intensity range for which all pixels in that range are 
// included.  A vector of length 1 for <src>include</src> means that the 
// range will be set to <src>-abs(include(0))</src> to <src>abs(include(0))</src>.
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
// array is the shape of the display axes (e.g. if the shape of the lattice is
// [nx,ny,nz] and you ask for histograms of the y axis the shape of the returned
// array would be [nbins,nx,nz].    The histograms are retrieved in the form 
// specified by the <src>setForm</src> function. The arrays are resized internally.
// A return value of <src>False</src> indicates  that the internal status of the class is bad. 
   Bool getHistograms (Array<T>& values, Array<T>& counts);

// This function retrieves the histogram at the specified location 
// into <src>Vectors</src>.  The histogram is retrieved in the form 
// specified by the <src>setForm</src> function. The vectors are resized 
// internally. If <src>posInLattice=True</src> then the location is a 
// location in the input lattice.  Any positions on the display axes 
// are ignored.  Otherwise, you should just give locations for 
// the display axes only. A return  value of <src>False</src> indicates  that 
// the internal status  of the class is bad. 
   Bool getHistogram (Vector<T>& values, 
                      Vector<T>& counts, 
                      const IPosition& pos,
                      const Bool posInLattice=False);

// Reset argument error condition.  If you specify invalid arguments to
// one of the above <src>set</src> functions, an internal flag will be set which will
// prevent the work functions from doing anything (should you have chosen 
// to ignore the Boolean return values of the <src>set</src> functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True;};

// Recover last error message
   String errorMessage() const {return error_p;};

// Set a MaskedLattice.  A return value of <src>False</src> indicates the 
// lattice had an invalid type or that the internal status of the class is bad.
   Bool setNewLattice (const MaskedLattice<T>& lattice);

// These things are protected only so that they are available to ImageHistograms
// which inherits from LatticeHistograms

protected:
   LogIO os_p;
   Bool goodParameterStatus_p; 
   Vector<Int> cursorAxes_p, displayAxes_p;
   String error_p;

// Given a location in the histogram storage lattice, convert those locations on the
// non-histogram axis (the first one) relative to the parent or current lattice
   IPosition locHistInLattice (const IPosition& histPosition, 
                               Bool relativeToParent=True) const;  

private:

// A useful typedef
   typedef typename NumericTraits<T>::PrecisionType AccumType;

   const MaskedLattice<T>* pInLattice_p;
   TempLattice<T>* pStoreLattice_p;
   LatticeStatistics<T>* pStats_p;
   Bool binAll_p, needStorageLattice_p;
   Bool doCumu_p, doGauss_p, doList_p, doLog_p;
   Bool haveLogger_p, showProgress_p, forceDisk_p;
   uInt nBins_p;
   PGPlotter plotter_p;
   Vector<Int> nxy_p;
   Vector<T> range_p;
   IPosition blcParent_p;


// Convert a <tt>T</tt> to a <tt>Float</tt> for plotting
   static Float convertT (const T value) {return Float(std::real(value));};   
 
// Convert a <tt>Float</tt> (from plotting) to a <tt>T</tt>
   static T convertF (const Float value) {return T(value);};

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
// storage lattice
   void extractOneHistogram (T& linearSum,
                             T& linearYMax,
                             Vector<T>& values,
                             Vector<T>& counts,
                             const Vector<T>& stats,
                             const Vector<T>& intCounts);

// Iterate through the lattice and generate the histogram accumulation lattice
   Bool generateStorageLattice();

// Get the statistics from the statistics object for the current
// location of either the input lattice, or the histogram storage lattice
   void getStatistics (Vector<T> &stats,
                       const IPosition &pos) const;

// List statistics
   void listStatistics(LogIO& os, const Vector<T>& stats, T binWidth);


// Fill histograms storage lattice
   void makeHistograms();

// Create and fill statistics object
   Bool makeStatistics();

// Check/set include pixel range
   Bool setInclude (Vector<T>& range,
                    Bool& noInclude,  
                    const Vector<T>& include,
                    ostream& os);

// Set stream attributes
   void setStream (ostream& os, Int oPrec);

// Make a string with pixel coordinates of display axes.  This function
// is over-ridden by ImageHistograms which inherits from LatticeHistograms.
   virtual String writeCoordinates(const IPosition& histPos) const;

// Write values of display axes on plots
   Bool writeDispAxesValues (const String& coords,
                             PGPlotter& plotter,
                             Float nchar) const;
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
// This class is used by <src>LatticeHistograms</src> to generate
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
// lattice in optimal tile-sized chunks.    <src>LatticeHistograms</src> 
// uses a <src>HistTiledCollapser</src> object which it gives to 
// <src>LatticeApply::tiledApply</src> for digestion.  After it has
// done its work, <src>LatticeHistograms</src> then accesses the output
// <src>Lattice</src> that it made.
// </synopsis>
//
// <example>
// <srcblock>
//// Created collapser. Control information is passed in via the constructor.
//
//   HistTiledCollapser<T> collapser(pStats, nBins_p);
// 
//// This is the first output axis getting  collapsed values. In LatticeHistograms
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
class HistTiledCollapser : public TiledCollapser<T,T>
{
 
public:
// Constructor
    HistTiledCollapser(LatticeStatistics<T>* pStats, uInt nBins);

    virtual ~HistTiledCollapser();

// Initialize process, making some checks
    virtual void init (uInt nOutPixelsPerCollapse);

// Initialize the accumulator
    virtual void initAccumulator (uInt n1, uInt n3);

// Process the data in the current chunk.
    virtual void process (
    	uInt accumIndex1,
    	uInt accumIndex3,
    	const T* inData,
    	const Bool* inMask,
    	uInt inDataIncr,
    	uInt inMaskIncr,
    	uInt nrval,
    	const IPosition& startPos,
    	const IPosition& shape
    );
 
// End the accumulation process and return the result arrays 
    virtual void endAccumulator(Array<T>& result,  
                                Array<Bool>& resultMask,
                                const IPosition& shape); 

// Can handle null mask
   virtual Bool canHandleNullMask() const {return True;};

private:
    LatticeStatistics<T>* pStats_p;
    Block<T>* pHist_p;
    uInt nBins_p;
    uInt n1_p;
    uInt n3_p;
};
 


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeHistograms.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
